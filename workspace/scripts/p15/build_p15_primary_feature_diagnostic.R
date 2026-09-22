#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(data.table)
  library(digest)
  library(dplyr)
  library(stringr)
  library(tibble)
  library(tidyr)
})

source("R/p15_observed_all_years.R")
source("R/p15_primary_feature_diagnostic.R")

root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
output_dir <- file.path(
  root, "data-derived", "p15_primary_feature_diagnostic_2012_2024_v1"
)
governance_dir <- file.path(root, "docs", "governance")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

paths <- list(
  issues = file.path(
    root, "data-derived", "p15_observed_markets_2012_2024_v1",
    "p15_primary_issue_evidence_2012_2024.csv.gz"
  ),
  instruments = file.path(
    root, "data-derived", "p15_unified_lseg_source_2012_2024_v1",
    "p15_instrument_year_universe_2012_2024.csv.gz"
  )
)
missing <- names(paths)[!file.exists(unlist(paths))]
if (length(missing)) {
  stop("Missing primary feature inputs: ", paste(missing, collapse = ", "),
       call. = FALSE)
}

issues <- fread(paths$issues) |> as_tibble()
instruments <- fread(paths$instruments) |> as_tibble()
audit <- p15_primary_issue_feature_diagnostic(issues, instruments)
summary <- p15_summarise_primary_feature_diagnostic(audit)

outputs <- list(
  p15_primary_feature_issue_audit_2012_2024 = audit,
  p15_primary_feature_diagnostic_summary = summary$overall,
  p15_primary_feature_diagnostic_by_feature = summary$by_feature,
  p15_primary_feature_country_year_coverage = summary$country_year_coverage,
  p15_primary_fixed_call_sink_country_year_comparison =
    summary$fixed_call_sink_comparison
)
for (name in names(outputs)) {
  path <- if (name == "p15_primary_feature_issue_audit_2012_2024") {
    file.path(output_dir, paste0(name, ".csv.gz"))
  } else {
    file.path(governance_dir, paste0(name, ".csv"))
  }
  fwrite(as.data.table(outputs[[name]]), path, na = "")
}

generated_paths <- c(
  file.path(output_dir, "p15_primary_feature_issue_audit_2012_2024.csv.gz"),
  file.path(governance_dir, paste0(names(outputs)[2:5], ".csv"))
)
manifest <- data.table(
  artifact_path = sub(
    paste0("^", root, "/"), "", c(unlist(paths), generated_paths)
  ),
  artifact_role = c(rep("source_input", length(paths)),
                    rep("generated_output", length(generated_paths)))
)
manifest[, `:=`(
  bytes = file.info(file.path(root, artifact_path))$size,
  sha256 = vapply(
    file.path(root, artifact_path), digest, character(1),
    algo = "sha256", file = TRUE
  ),
  producing_script = "scripts/p15/build_p15_primary_feature_diagnostic.R",
  build_id = p15_primary_feature_diagnostic_build(),
  schema_version = p15_primary_feature_diagnostic_schema(),
  build_date = "2026-08-14"
)]
fwrite(
  manifest,
  file.path(governance_dir, "p15_primary_feature_diagnostic_manifest.csv"),
  na = ""
)

cat("Built P15 primary feature diagnostic.\n")
print(as.data.table(summary$overall))
print(as.data.table(summary$by_feature))
