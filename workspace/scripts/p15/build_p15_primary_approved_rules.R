#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(data.table)
  library(digest)
  library(dplyr)
  library(tibble)
  library(tidyr)
})

source("R/p15_observed_all_years.R")
source("R/p15_primary_feature_diagnostic.R")
source("R/p15_primary_approved_rules.R")

root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
output_dir <- file.path(
  root, "data-derived", "p15_primary_approved_candidate_2012_2024_v1"
)
governance_dir <- file.path(root, "docs", "governance")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

paths <- list(
  issues = file.path(
    root, "data-derived/p15_observed_markets_2012_2024_v1",
    "p15_primary_issue_evidence_2012_2024.csv.gz"
  ),
  instruments = file.path(
    root, "data-derived/p15_unified_lseg_source_2012_2024_v1",
    "p15_instrument_year_universe_2012_2024.csv.gz"
  )
)
missing <- names(paths)[!file.exists(unlist(paths))]
if (length(missing)) {
  stop("Missing approved primary inputs: ", paste(missing, collapse = ", "),
       call. = FALSE)
}

issues <- fread(paths$issues) |> as_tibble()
instruments <- fread(paths$instruments) |> as_tibble()
disposition <- p15_apply_approved_primary_rules(issues, instruments)
candidates <- p15_aggregate_approved_primary_candidates(disposition)
register <- p15_primary_approved_variant_register()
summary <- p15_summarise_approved_primary_rules(disposition, candidates)

output_paths <- list(
  issue_disposition = file.path(
    output_dir, "p15_primary_approved_issue_disposition_2012_2024.csv.gz"
  ),
  candidates = file.path(
    output_dir, "p15_primary_approved_country_year_candidates_2012_2024.csv.gz"
  ),
  register = file.path(
    governance_dir, "p15_primary_approved_method_register.csv"
  ),
  issue_summary = file.path(
    governance_dir, "p15_primary_approved_issue_summary.csv"
  ),
  candidate_summary = file.path(
    governance_dir, "p15_primary_approved_candidate_summary.csv"
  )
)
fwrite(as.data.table(disposition), output_paths$issue_disposition, na = "")
fwrite(as.data.table(candidates), output_paths$candidates, na = "")
fwrite(as.data.table(register), output_paths$register, na = "")
fwrite(as.data.table(summary$issue_summary), output_paths$issue_summary, na = "")
fwrite(
  as.data.table(summary$candidate_summary), output_paths$candidate_summary,
  na = ""
)

manifest <- data.table(
  artifact_path = sub(
    paste0("^", root, "/"), "", c(unlist(paths), unlist(output_paths))
  ),
  artifact_role = c(
    rep("source_input", length(paths)),
    rep("generated_output", length(output_paths))
  )
)
manifest[, `:=`(
  bytes = file.info(file.path(root, artifact_path))$size,
  sha256 = vapply(
    file.path(root, artifact_path), digest, character(1),
    algo = "sha256", file = TRUE
  ),
  producing_script = "scripts/p15/build_p15_primary_approved_rules.R",
  build_id = p15_primary_approved_build_id(),
  schema_version = p15_primary_approved_schema_version(),
  build_date = "2026-08-15"
)]
fwrite(
  manifest,
  file.path(governance_dir, "p15_primary_approved_manifest.csv"),
  na = ""
)

cat("Built approved P15 primary candidate layer.\n")
print(as.data.table(summary$candidate_summary))
