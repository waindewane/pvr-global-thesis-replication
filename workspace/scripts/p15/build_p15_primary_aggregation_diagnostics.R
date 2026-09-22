#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(data.table)
  library(digest)
  library(dplyr)
  library(tibble)
})

source("R/p15_observed_all_years.R")

root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
derived_dir <- file.path(
  root, "data-derived", "p15_observed_markets_2012_2024_v1"
)
governance_dir <- file.path(root, "docs", "governance")
paths <- list(
  primary_issues = file.path(
    derived_dir, "p15_primary_issue_evidence_2012_2024.csv.gz"
  ),
  primary_candidates = file.path(
    derived_dir, "p15_primary_country_year_candidate_variants_2012_2024.csv.gz"
  )
)
stopifnot(all(file.exists(unlist(paths))))

primary_issues <- data.table::fread(paths$primary_issues) |>
  tibble::as_tibble()
primary_candidates <- data.table::fread(paths$primary_candidates) |>
  tibble::as_tibble()
diagnostics <- p15_build_primary_aggregation_diagnostics(
  primary_issues, primary_candidates
)

decision_register <- tibble::tribble(
  ~decision_id, ~decision_question, ~evidence_result, ~recommended_direction, ~decision_state,
  "PRI-05-A", "What is the primary rate object?", "Original Yield Maturity is directly available and already preserves exact P13 precedence.", "Use Original Yield Maturity; keep price fields descriptive unless a separate price-implied method is approved.", "not_evaluated",
  "PRI-05-B", "How should duplicate identifiers and economic issues be handled?", paste0(nrow(diagnostics$reopening_families), " potential multi-identifier or reopening families are explicitly classified."), "Use one economic issue for identical terms while retaining identifier lineage; keep uncertain tranches or reopenings separate pending review.", "not_evaluated",
  "PRI-05-C", "Which amount should weight primary issues?", "The predecessor mixed original-currency amounts; P15 distinguishes positive USD face-issued weights from source-currency fallbacks.", "Use positive Face Issued USD for mixed USD/EUR aggregation; never treat original-currency fallback amounts as USD.", "not_evaluated",
  "PRI-05-D", "How should thin or concentrated issuance be treated?", paste0(nrow(diagnostics$country_casebook), " standard candidate country-years have at least one thin concentration range or currency review flag."), "Retain observed primary evidence but label single-issue dominant-issue and wide-range country-years rather than automatically deleting them.", "not_evaluated",
  "PRI-05-E", "Should the USD 50m materiality rule remain?", "The all-issue and USD 50m standard variants differ in only one country-year of coverage under the current source-object screens.", "Retain USD 50m as a labelled main candidate and preserve the all-issue-count variant as sensitivity until approval.", "not_evaluated",
  "PRI-05-F", "Should 1-30 percent be a hard primary screen?", "Valid low and negative euro-area issue yields fail the inherited lower bound while high or conflicting rows need source context.", "Do not use one universal deletion screen; apply the separately approved source-aware sanity rule.", "not_evaluated"
) |>
  dplyr::mutate(
    automatic_consequence = "none",
    build_id = "BUILD-P15-PRIMARY-DIAGNOSTICS-20260721-V1"
  )

summary <- tibble::tibble(
  metric = c(
    "potential_reopening_or_multi_identifier_families",
    "primary_issue_review_rows",
    "primary_country_year_aggregation_review_rows",
    "single_issue_standard_50m_country_years",
    "dominant_issue_standard_50m_country_years",
    "wide_rate_range_standard_50m_country_years",
    "mixed_currency_standard_50m_country_years",
    "approved_primary_decisions",
    "automatic_consequences_set"
  ),
  value = c(
    nrow(diagnostics$reopening_families),
    nrow(diagnostics$issue_casebook),
    nrow(diagnostics$country_casebook),
    sum(diagnostics$country_casebook$single_issue_flag),
    sum(diagnostics$country_casebook$dominant_issue_flag),
    sum(diagnostics$country_casebook$wide_issue_rate_range_flag),
    sum(diagnostics$country_casebook$mixed_currency_flag),
    0L,
    0L
  )
)

output_paths <- list(
  reopening = file.path(
    derived_dir, "p15_primary_reopening_tap_family_review_2012_2024.csv.gz"
  ),
  issue_casebook = file.path(
    derived_dir, "p15_primary_issue_outlier_casebook_2012_2024.csv.gz"
  ),
  country_casebook = file.path(
    derived_dir,
    "p15_primary_country_year_aggregation_casebook_2012_2024.csv.gz"
  ),
  decision_register = file.path(
    governance_dir, "p15_primary_aggregation_decision_register.csv"
  ),
  summary = file.path(
    governance_dir, "p15_primary_aggregation_diagnostic_summary.csv"
  ),
  manifest = file.path(
    governance_dir, "p15_primary_aggregation_diagnostic_manifest.csv"
  )
)
objects <- list(
  diagnostics$reopening_families, diagnostics$issue_casebook,
  diagnostics$country_casebook, decision_register, summary
)
for (i in seq_along(objects)) {
  data.table::fwrite(
    data.table::as.data.table(objects[[i]]), output_paths[[i]], na = ""
  )
}

input_paths <- unname(unlist(paths))
generated_paths <- unname(unlist(
  output_paths[names(output_paths) != "manifest"]
))
script_path <- file.path(
  root, "scripts", "p15", "build_p15_primary_aggregation_diagnostics.R"
)
manifest <- data.table::rbindlist(lapply(
  c(input_paths, generated_paths, file.path(root, "R", "p15_observed_all_years.R"),
    script_path),
  function(path) data.table::data.table(
    artifact_path = sub(paste0("^", root, "/"), "", path),
    artifact_role = if (path %in% input_paths) "parent_input" else
      "generated_output_or_code",
    bytes = file.info(path)$size,
    sha256 = digest::digest(file = path, algo = "sha256"),
    producing_script = sub(paste0("^", root, "/"), "", script_path),
    producing_script_sha256 = digest::digest(
      file = script_path, algo = "sha256"
    ),
    build_id = "BUILD-P15-PRIMARY-DIAGNOSTICS-20260721-V1",
    schema_version = p15_observed_all_years_schema_version(),
    build_date = "2026-07-21"
  )
), use.names = TRUE, fill = TRUE)
data.table::fwrite(manifest, output_paths$manifest, na = "")

cat("P15 primary aggregation diagnostics: PASS\n")
cat("Reopening or multi-identifier families:",
    nrow(diagnostics$reopening_families), "\n")
cat("Issue review rows:", nrow(diagnostics$issue_casebook), "\n")
cat("Country-year aggregation review rows:",
    nrow(diagnostics$country_casebook), "\n")
cat("Approved primary decisions: 0\n")
