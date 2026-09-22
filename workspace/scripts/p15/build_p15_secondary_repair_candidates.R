#!/usr/bin/env Rscript

# Build the strict, lower-ranked P15 price-derived-secondary candidate tier.
# This implements SEC-10 but deliberately does not select a ladder rate; final
# secondary hierarchy and promotion remain under SEC-18.

suppressPackageStartupMessages({
  library(data.table)
  library(digest)
  library(dplyr)
  library(tibble)
  library(tidyr)
})

source("R/p15_observed_all_years.R")
source("R/p15_observed_secondary.R")
source("R/p15_secondary_repair_admissibility.R")

root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
derived_dir <- file.path(
  root, "data-derived", "p15_secondary_repair_candidate_2012_2024_v1"
)
governance_dir <- file.path(root, "docs", "governance")
dir.create(derived_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(governance_dir, recursive = TRUE, showWarnings = FALSE)

paths <- list(
  identifier_evidence = file.path(
    root, "data-derived", "p15_observed_markets_2012_2024_v1",
    "p15_secondary_identifier_year_evidence_2012_2024.csv.gz"
  ),
  secondary_history = file.path(
    root, "data-derived", "p15_unified_lseg_source_2012_2024_v1",
    "p15_secondary_history_long_2012_2024.csv.gz"
  ),
  direct_issue_evidence = file.path(
    root, "data-derived", "p15_observed_markets_2012_2024_v1",
    "p15_secondary_issue_year_evidence_2012_2024.csv.gz"
  ),
  direct_country_candidates = file.path(
    root, "data-derived", "p15_observed_markets_2012_2024_v1",
    "p15_secondary_country_year_candidate_variants_2012_2024.csv.gz"
  ),
  status_recommendations = file.path(
    governance_dir,
    "p15_status_case_review_recommendations_2012_2024.csv.gz"
  )
)
missing <- names(paths)[!file.exists(unlist(paths))]
if (length(missing)) {
  stop(
    "Missing strict secondary-repair inputs: ",
    paste(missing, collapse = ", "),
    call. = FALSE
  )
}

identifier_evidence <- data.table::fread(paths$identifier_evidence) |>
  tibble::as_tibble()
secondary_history <- data.table::fread(paths$secondary_history) |>
  tibble::as_tibble()
direct_issue_evidence <- data.table::fread(paths$direct_issue_evidence) |>
  tibble::as_tibble()
direct_country_candidates <- data.table::fread(
  paths$direct_country_candidates
) |>
  tibble::as_tibble()
status_recommendations <- data.table::fread(paths$status_recommendations) |>
  tibble::as_tibble()

result <- p15_build_strict_secondary_repair_candidates(
  identifier_evidence,
  secondary_history,
  status_recommendations
)
identifier_audit <- result$identifier_audit
issue_audit <- result$issue_audit
country_candidates <- result$country_candidates
issue_disposition_catalogue <-
  p15_build_secondary_issue_disposition_catalogue(
    direct_issue_evidence, issue_audit
  )

country_candidate_catalogue <- bind_rows(
  direct_country_candidates |>
    transmute(
      analysis_year, iso3, country, country_year_id, period,
      historical_income_level, historical_lmic_reporting_scope,
      market_rate_pct, market_maturity_years, issue_count,
      identifier_count, total_weight_usd, currency_basis,
      candidate_variant_id,
      candidate_evidence_tier = "direct_secondary",
      method_id = paste0("P15_COMMON_", .data$candidate_variant_id),
      source_candidate_state = .data$candidate_decision_state,
      selection_state = "not_selected_pending_SEC_18",
      selected_for_ladder = FALSE,
      candidate_schema_version =
        p15_secondary_repair_candidate_schema_version(),
      candidate_build_id = p15_secondary_repair_candidate_build_id()
    ),
  country_candidates |>
    transmute(
      analysis_year, iso3, country, country_year_id, period,
      historical_income_level, historical_lmic_reporting_scope,
      market_rate_pct, market_maturity_years, issue_count,
      identifier_count, total_weight_usd,
      currency_basis = .data$currency,
      candidate_variant_id,
      candidate_evidence_tier = .data$repaired_evidence_tier,
      method_id,
      source_candidate_state = "strict_candidate_admitted",
      selection_state, selected_for_ladder,
      candidate_schema_version, candidate_build_id
    )
) |>
  arrange(analysis_year, iso3, candidate_evidence_tier, candidate_variant_id)

direct_country_year_keys <- unique(paste(
  direct_country_candidates$analysis_year,
  direct_country_candidates$iso3,
  sep = "::"
))
repair_country_year_keys <- unique(paste(
  country_candidates$analysis_year,
  country_candidates$iso3,
  sep = "::"
))
direct_lmic_country_year_keys <- unique(paste(
  direct_country_candidates$analysis_year[
    direct_country_candidates$historical_lmic_reporting_scope %in% TRUE
  ],
  direct_country_candidates$iso3[
    direct_country_candidates$historical_lmic_reporting_scope %in% TRUE
  ],
  sep = "::"
))
repair_lmic_country_year_keys <- unique(paste(
  country_candidates$analysis_year[
    country_candidates$historical_lmic_reporting_scope %in% TRUE
  ],
  country_candidates$iso3[
    country_candidates$historical_lmic_reporting_scope %in% TRUE
  ],
  sep = "::"
))

stopifnot(
  nrow(identifier_audit) > 0L,
  nrow(issue_audit) > 0L,
  nrow(country_candidates) > 0L,
  nrow(issue_disposition_catalogue) == nrow(direct_issue_evidence),
  nrow(country_candidate_catalogue) ==
    nrow(direct_country_candidates) + nrow(country_candidates),
  !anyDuplicated(issue_audit[c(
    "analysis_year", "iso3", "repair_issue_key"
  )]),
  !anyDuplicated(country_candidates[c(
    "analysis_year", "iso3", "currency", "candidate_variant_id"
  )]),
  all(!is.na(issue_audit$strict_candidate_admitted)),
  all(!issue_audit$selected_for_ladder),
  all(!country_candidates$selected_for_ladder),
  all(!issue_disposition_catalogue$selected_for_ladder),
  all(!country_candidate_catalogue$selected_for_ladder),
  all(issue_audit$selection_state == "not_selected_pending_SEC_18"),
  all(country_candidates$selection_state ==
        "not_selected_pending_SEC_18"),
  all(issue_disposition_catalogue$selection_state ==
        "not_selected_pending_SEC_18"),
  all(country_candidate_catalogue$selection_state ==
        "not_selected_pending_SEC_18"),
  all(country_candidates$currency %in% c("USD", "EUR")),
  !any(issue_audit$strict_candidate_admitted &
         issue_audit$issue_any_direct_yield),
  all(issue_audit$issue_technical_pass[
    issue_audit$strict_candidate_admitted
  ]),
  all(issue_audit$relative_outlier_gate[
    issue_audit$strict_candidate_admitted
  ]),
  all(issue_audit$status_gate[
    issue_audit$strict_candidate_admitted
  ]),
  all(identifier_audit$candidate_build_id ==
        p15_secondary_repair_candidate_build_id()),
  all(issue_audit$candidate_build_id ==
        p15_secondary_repair_candidate_build_id()),
  all(country_candidates$candidate_build_id ==
        p15_secondary_repair_candidate_build_id())
)

parameter_register <- tibble(
  parameter = names(result$parameters),
  value = as.character(unlist(result$parameters, use.names = FALSE)),
  rule_role = c(
    "preferred_quote_window", "preferred_quote_window",
    "settlement_sensitivity_test", "candidate_maturity_scope",
    "candidate_maturity_scope", "technical_stability_gate",
    "technical_stability_gate", "technical_stability_gate",
    "technical_data_error_gate", "technical_data_error_gate",
    "issuer_year_relative_data_error_gate",
    "issuer_year_relative_data_error_gate"
  ),
  decision_authority = c(
    rep("SEC-10_implemented_candidate_rule_pending_SEC-18", 10),
    rep("OECD_style_relative_data_error_adaptation", 2)
  ),
  automatic_ladder_selection_effect = "none"
)

gate_columns <- names(identifier_audit)[grepl("_gate$", names(identifier_audit))]
identifier_gate_summary <- identifier_audit |>
  summarise(across(
    all_of(gate_columns),
    ~ sum(!coalesce(.x, FALSE))
  )) |>
  pivot_longer(
    everything(), names_to = "gate", values_to = "failed_identifier_rows"
  ) |>
  mutate(summary_level = "identifier_gate")

admission_summary <- issue_audit |>
  count(admission_state, name = "issue_rows") |>
  transmute(
    gate = admission_state,
    failed_identifier_rows = issue_rows,
    summary_level = "issue_admission_state"
  )
gate_summary <- bind_rows(identifier_gate_summary, admission_summary) |>
  arrange(summary_level, desc(failed_identifier_rows), gate)

build_summary <- tibble(
  metric = c(
    "price_bearing_identifier_rows_audited",
    "identifier_rows_passing_every_technical_gate",
    "issue_rows_audited",
    "strict_candidate_issues_admitted",
    "strict_candidate_issues_admitted_with_status_warning",
    "issues_held_pending_STAT_05",
    "issues_excluded_by_relative_outlier_gate",
    "same_date_direct_repair_comparison_issues",
    "same_date_direct_repair_median_absolute_gap_bps",
    "currency_specific_country_year_candidates",
    "distinct_candidate_country_years",
    "direct_candidate_country_years_any_variant",
    "repair_incremental_country_years_beyond_any_direct_variant",
    "combined_direct_or_repair_country_years",
    "repair_incremental_LMIC_country_years_beyond_any_direct_variant",
    "integrated_issue_disposition_rows",
    "integrated_country_candidate_rows",
    "feature_rich_issue_rows_visible",
    "blocked_or_diagnostic_issue_rows_visible",
    "selected_ladder_rows",
    "candidate_schema_version",
    "candidate_build_id"
  ),
  value = c(
    nrow(identifier_audit),
    sum(identifier_audit$identifier_technical_pass),
    nrow(issue_audit),
    sum(issue_audit$strict_candidate_admitted),
    sum(issue_audit$admission_state ==
          "admitted_candidate_with_status_warning"),
    sum(issue_audit$admission_state ==
          "held_pending_STAT_05_case_decision"),
    sum(issue_audit$admission_state ==
          "excluded_issuer_year_relative_outlier_gate"),
    sum(issue_audit$direct_repair_comparison_identifier_count > 0),
    stats::median(
      issue_audit$repaired_minus_direct_abs_bps[
        issue_audit$direct_repair_comparison_identifier_count > 0
      ],
      na.rm = TRUE
    ),
    nrow(country_candidates),
    nrow(distinct(country_candidates, analysis_year, iso3)),
    length(direct_country_year_keys),
    sum(!repair_country_year_keys %in% direct_country_year_keys),
    length(unique(c(direct_country_year_keys, repair_country_year_keys))),
    sum(!repair_lmic_country_year_keys %in% direct_lmic_country_year_keys),
    nrow(issue_disposition_catalogue),
    nrow(country_candidate_catalogue),
    sum(issue_disposition_catalogue$nonstandard_feature_flag),
    sum(!issue_disposition_catalogue$candidate_for_secondary_method_review),
    sum(country_candidate_catalogue$selected_for_ladder),
    p15_secondary_repair_candidate_schema_version(),
    p15_secondary_repair_candidate_build_id()
  )
) |>
  mutate(value = as.character(value))

output_paths <- list(
  identifier_audit = file.path(
    derived_dir,
    "p15_secondary_repair_identifier_audit_2012_2024.csv.gz"
  ),
  issue_audit = file.path(
    derived_dir,
    "p15_secondary_repair_issue_audit_2012_2024.csv.gz"
  ),
  country_candidates = file.path(
    derived_dir,
    "p15_secondary_repair_country_candidates_2012_2024.csv.gz"
  ),
  issue_disposition_catalogue = file.path(
    derived_dir,
    "p15_secondary_issue_disposition_catalogue_2012_2024.csv.gz"
  ),
  country_candidate_catalogue = file.path(
    derived_dir,
    "p15_secondary_country_candidate_catalogue_2012_2024.csv.gz"
  ),
  parameter_register = file.path(
    governance_dir,
    "p15_secondary_repair_candidate_parameters_2026-08-09.csv"
  ),
  gate_summary = file.path(
    governance_dir,
    "p15_secondary_repair_candidate_gate_summary_2026-08-09.csv"
  ),
  build_summary = file.path(
    governance_dir,
    "p15_secondary_repair_candidate_build_summary_2026-08-09.csv"
  ),
  manifest = file.path(
    governance_dir,
    "p15_secondary_repair_candidate_manifest_2026-08-09.csv"
  )
)

data.table::fwrite(as.data.table(identifier_audit), output_paths$identifier_audit, na = "")
data.table::fwrite(as.data.table(issue_audit), output_paths$issue_audit, na = "")
data.table::fwrite(as.data.table(country_candidates), output_paths$country_candidates, na = "")
data.table::fwrite(as.data.table(issue_disposition_catalogue), output_paths$issue_disposition_catalogue, na = "")
data.table::fwrite(as.data.table(country_candidate_catalogue), output_paths$country_candidate_catalogue, na = "")
data.table::fwrite(as.data.table(parameter_register), output_paths$parameter_register, na = "")
data.table::fwrite(as.data.table(gate_summary), output_paths$gate_summary, na = "")
data.table::fwrite(as.data.table(build_summary), output_paths$build_summary, na = "")

script_path <- file.path(
  root, "scripts", "p15", "build_p15_secondary_repair_candidates.R"
)
function_paths <- c(
  file.path(root, "R", "p15_observed_all_years.R"),
  file.path(root, "R", "p15_observed_secondary.R"),
  file.path(root, "R", "p15_secondary_repair_admissibility.R")
)
input_paths <- c(unname(unlist(paths)), function_paths)
generated_paths <- unname(unlist(output_paths[names(output_paths) != "manifest"]))
manifest <- rbindlist(lapply(c(input_paths, generated_paths), function(path) {
  data.table(
    artifact_path = sub(paste0("^", root, "/"), "", path),
    artifact_role = if (path %in% input_paths) {
      if (path %in% function_paths) "processor_code" else "parent_input"
    } else {
      "generated_output"
    },
    bytes = file.info(path)$size,
    sha256 = digest::digest(file = path, algo = "sha256"),
    producing_script = sub(paste0("^", root, "/"), "", script_path),
    producing_script_sha256 = digest::digest(
      file = script_path, algo = "sha256"
    ),
    build_id = p15_secondary_repair_candidate_build_id(),
    schema_version = p15_secondary_repair_candidate_schema_version(),
    build_date = "2026-08-09"
  )
}), use.names = TRUE, fill = TRUE)
data.table::fwrite(manifest, output_paths$manifest, na = "")

cat("P15 strict secondary-repair candidate build: PASS\n")
cat("Issue rows audited:", nrow(issue_audit), "\n")
cat("Strict candidate issues admitted:",
    sum(issue_audit$strict_candidate_admitted), "\n")
cat("Currency-specific country-year candidates:",
    nrow(country_candidates), "\n")
cat("Selected ladder rows: 0\n")
