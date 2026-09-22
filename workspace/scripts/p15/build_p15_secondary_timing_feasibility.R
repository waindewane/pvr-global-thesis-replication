#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(data.table)
  library(digest)
})

source("R/p15_secondary_timing_feasibility.R")

root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
output_dir <- file.path(
  root, "data-derived", "p15_secondary_timing_feasibility_2012_2024_v1"
)
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

paths <- list(
  history = file.path(
    root, "data-derived/p15_unified_lseg_source_2012_2024_v1",
    "p15_secondary_history_long_2012_2024.csv.gz"
  ),
  identifiers = file.path(
    root, "data-derived/p15_observed_markets_2012_2024_v1",
    "p15_secondary_identifier_year_evidence_2012_2024.csv.gz"
  ),
  primary = file.path(
    root, "data-derived/p15_observed_markets_2012_2024_v1",
    "p15_primary_issue_evidence_2012_2024.csv.gz"
  )
)
missing <- names(paths)[!file.exists(unlist(paths))]
if (length(missing)) {
  stop("Missing timing-feasibility inputs: ", paste(missing, collapse = ", "),
       call. = FALSE)
}

history <- fread(paths$history)
identifiers <- fread(paths$identifiers)
primary <- fread(paths$primary)

output_register <- p15_secondary_timing_output_register()
history_coverage <- p15_secondary_history_date_coverage(history)
issuance_matches <- p15_primary_issuance_match_feasibility(
  primary, history, identifiers, lookback_days = 7L
)
issuance_summary <- p15_primary_issuance_match_summary(issuance_matches)

outputs <- list(
  p15_secondary_timing_output_register_2012_2024 = output_register,
  p15_secondary_history_date_coverage_2012_2024 = history_coverage,
  p15_primary_issuance_secondary_date_match_feasibility_2012_2024 =
    issuance_matches,
  p15_primary_issuance_secondary_date_match_summary_2012_2024 =
    issuance_summary
)

for (name in names(outputs)) {
  fwrite(outputs[[name]], file.path(output_dir, paste0(name, ".csv")))
}

manifest <- data.table(
  build_id = p15_secondary_timing_build_id(),
  schema_version = p15_secondary_timing_schema_version(),
  file_name = paste0(names(outputs), ".csv")
)
manifest[, sha256 := vapply(
  file.path(output_dir, file_name),
  digest, character(1), algo = "sha256", file = TRUE
)]
manifest[, row_count := vapply(outputs, nrow, integer(1))]
fwrite(
  manifest,
  file.path(output_dir, "p15_secondary_timing_feasibility_manifest.csv")
)

cat("Built P15 secondary timing feasibility package at", output_dir, "\n")
print(output_register[, .(
  timing_output_id, current_local_source_status, current_method_status
)])
print(issuance_summary[, .(
  primary_issues = sum(primary_issues),
  issues_with_quote = sum(issues_with_any_eligible_secondary_quote),
  issues_with_bracket = sum(issues_with_maturity_bracket)
), by = candidate_scope])
