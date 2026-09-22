#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(data.table)
  library(digest)
  library(dplyr)
  library(tibble)
})

source("R/p15_observed_all_years.R")
source("R/p15_status_sanity_candidate_rules.R")
source("R/p15_status_sanity_approved_rules.R")

root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
output_dir <- file.path(
  root, "data-derived", "p15_status_sanity_approved_2012_2024_v1"
)
governance_dir <- file.path(root, "docs", "governance")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

paths <- list(
  primary = file.path(
    root, "data-derived/p15_primary_approved_candidate_2012_2024_v1",
    "p15_primary_approved_issue_disposition_2012_2024.csv.gz"
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
  stop("Missing approved status/sanity inputs: ", paste(missing, collapse = ", "),
       call. = FALSE)
}

primary <- fread(paths$primary) |> as_tibble()
secondary <- fread(paths$secondary) |> as_tibble()
status_recommendations <- fread(paths$status_recommendations) |> as_tibble()
treasury <- p15_bind_fred_treasury_curve(paths$dgs5, paths$dgs7, paths$dgs10)

issue_sanity <- p15_build_approved_sanity_issue_universe(
  primary, secondary, treasury
)
imf_context <- p15_build_imf_inspired_context_flags(issue_sanity)
status_contract <- p15_build_approved_status_rule_contract(
  status_recommendations
)
country_actions <- p15_integrate_approved_status_sanity_actions(
  issue_sanity, status_contract, imf_context
)
summary <- p15_summarise_approved_status_sanity(
  issue_sanity, status_contract, country_actions
)

output_paths <- list(
  issue_sanity = file.path(
    output_dir, "p15_approved_issue_rate_sanity_2012_2024.csv.gz"
  ),
  imf_context = file.path(
    output_dir, "p15_imf_inspired_spread_context_2012_2024.csv.gz"
  ),
  status_contract = file.path(
    output_dir, "p15_approved_status_rule_contract_2012_2024.csv.gz"
  ),
  country_actions = file.path(
    output_dir, "p15_approved_status_sanity_country_actions_2012_2024.csv.gz"
  ),
  summary = file.path(
    governance_dir, "p15_status_sanity_approved_summary.csv"
  )
)
fwrite(as.data.table(issue_sanity), output_paths$issue_sanity, na = "")
fwrite(as.data.table(imf_context), output_paths$imf_context, na = "")
fwrite(as.data.table(status_contract), output_paths$status_contract, na = "")
fwrite(as.data.table(country_actions), output_paths$country_actions, na = "")
fwrite(as.data.table(summary), output_paths$summary, na = "")

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
  producing_script = "scripts/p15/build_p15_status_sanity_approved_rules.R",
  build_id = p15_status_sanity_approved_build_id(),
  schema_version = p15_status_sanity_approved_schema_version(),
  build_date = "2026-08-15"
)]
fwrite(
  manifest,
  file.path(governance_dir, "p15_status_sanity_approved_manifest.csv"),
  na = ""
)

cat("Built approved P15 status/sanity rule layer.\n")
print(as.data.table(summary))
