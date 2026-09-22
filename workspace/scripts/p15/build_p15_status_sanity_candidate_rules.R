#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(data.table)
  library(digest)
  library(dplyr)
  library(tibble)
})

source("R/p15_status_sanity_candidate_rules.R")

root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
output_dir <- file.path(
  root, "data-derived", "p15_status_sanity_candidate_2012_2024_v1"
)
governance_dir <- file.path(root, "docs", "governance")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

paths <- list(
  primary = file.path(
    root, "data-derived/p15_observed_markets_2012_2024_v1",
    "p15_primary_issue_evidence_2012_2024.csv.gz"
  ),
  secondary = file.path(
    root, "data-derived/p15_observed_markets_2012_2024_v1",
    "p15_secondary_issue_year_evidence_2012_2024.csv.gz"
  ),
  status_recommendations = file.path(
    root, "docs/governance",
    "p15_status_case_review_recommendations_2012_2024.csv.gz"
  ),
  dgs5 = file.path(
    root, "data-raw/fred_treasury_tenor_sensitivity_2026-07-21",
    "fred_dgs5_2012_2024_downloaded_2026-07-21.csv"
  ),
  dgs7 = file.path(
    root, "data-raw/fred_treasury_tenor_sensitivity_2026-07-21",
    "fred_dgs7_2012_2024_downloaded_2026-07-21.csv"
  ),
  dgs10 = file.path(
    root, "data-raw/fred_treasury_tenor_sensitivity_2026-07-21",
    "fred_dgs10_2012_2024_downloaded_2026-07-21.csv"
  )
)
missing <- names(paths)[!file.exists(unlist(paths))]
if (length(missing)) {
  stop("Missing status/sanity candidate inputs: ", paste(missing, collapse = ", "),
       call. = FALSE)
}

primary <- fread(paths$primary) |> as_tibble()
secondary <- fread(paths$secondary) |> as_tibble()
status <- fread(paths$status_recommendations) |> as_tibble()
treasury <- p15_bind_fred_treasury_curve(paths$dgs5, paths$dgs7, paths$dgs10)

issue_review <- p15_observed_issue_review_universe(primary, secondary, treasury)
spread_review <- p15_build_imf_spread_review_flags(issue_review)
combined <- p15_integrate_status_sanity_candidate_actions(spread_review, status)

summary <- combined |>
  dplyr::group_by(
    .data$evidence_object, .data$historical_lmic_reporting_scope,
    .data$candidate_combined_action
  ) |>
  dplyr::summarise(
    country_years = dplyr::n(),
    imf_review_flags = sum(.data$imf_500bps_plus_doubling_review_flag),
    technical_review_flags = sum(.data$technical_data_error_review_flag),
    low_rate_review_flags = sum(.data$low_rate_source_review_flag),
    above_30_review_flags = sum(.data$above_30_context_review_flag),
    status_cases = sum(.data$status_case_present),
    .groups = "drop"
  )

outputs <- list(
  p15_observed_issue_rate_sanity_checks_2012_2024 = issue_review,
  p15_imf_spread_review_flags_2012_2024 = spread_review,
  p15_status_sanity_candidate_actions_2012_2024 = combined
)
for (name in names(outputs)) {
  fwrite(
    as.data.table(outputs[[name]]),
    file.path(output_dir, paste0(name, ".csv.gz")), na = ""
  )
}
summary_path <- file.path(
  governance_dir, "p15_status_sanity_candidate_summary.csv"
)
fwrite(as.data.table(summary), summary_path, na = "")

generated_paths <- c(
  file.path(output_dir, paste0(names(outputs), ".csv.gz")), summary_path
)
manifest <- data.table(
  artifact_path = sub(paste0("^", root, "/"), "", c(unlist(paths), generated_paths)),
  artifact_role = c(rep("source_input", length(paths)),
                    rep("generated_output", length(generated_paths)))
)
manifest[, `:=`(
  bytes = file.info(file.path(root, artifact_path))$size,
  sha256 = vapply(
    file.path(root, artifact_path), digest, character(1),
    algo = "sha256", file = TRUE
  ),
  producing_script = "scripts/p15/build_p15_status_sanity_candidate_rules.R",
  build_id = p15_status_sanity_build_id(),
  schema_version = p15_status_sanity_schema_version(),
  build_date = "2026-08-14"
)]
fwrite(
  manifest,
  file.path(governance_dir, "p15_status_sanity_candidate_manifest.csv"),
  na = ""
)

cat("Built P15 status/sanity candidate checks.\n")
print(as.data.table(summary))
