#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(data.table)
  library(digest)
  library(dplyr)
  library(tibble)
  library(tidyr)
})

source("R/p15_secondary_maturity_diagnostic.R")

root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
output_dir <- file.path(
  root, "data-derived", "p15_secondary_maturity_diagnostic_2012_2024_v1"
)
governance_dir <- file.path(root, "docs", "governance")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

paths <- list(
  issues = file.path(
    root, "data-derived/p15_observed_markets_2012_2024_v1",
    "p15_secondary_issue_year_evidence_2012_2024.csv.gz"
  ),
  status = file.path(
    root, "data-derived/p15_status_expansion_2012_2024_v1",
    "p15_status_context_expanded_2012_2024.csv.gz"
  )
)
missing <- names(paths)[!file.exists(unlist(paths))]
if (length(missing)) {
  stop("Missing maturity-diagnostic inputs: ", paste(missing, collapse = ", "),
       call. = FALSE)
}

issues <- fread(paths$issues) |> as_tibble()
status <- fread(paths$status) |> as_tibble()

rates <- p15_build_secondary_maturity_scenario_rates_detailed(issues)
comparisons <- p15_compare_secondary_maturity_scenarios_detailed(rates)
membership <- p15_build_secondary_maturity_membership_changes(issues, rates)
summary <- p15_summarise_secondary_maturity_diagnostic(
  rates, comparisons, membership, status
)
worst_cases <- summary$changed_cases |>
  dplyr::filter(.data$coverage_state == "present_in_both") |>
  dplyr::arrange(dplyr::desc(.data$absolute_rate_difference_pp)) |>
  dplyr::group_by(.data$scenario_id) |>
  dplyr::slice_head(n = 50L) |>
  dplyr::ungroup()

outputs <- list(
  p15_secondary_maturity_scenario_rates_2012_2024 = rates,
  p15_secondary_maturity_country_year_differences_2012_2024 = comparisons,
  p15_secondary_maturity_issue_membership_changes_2012_2024 = membership,
  p15_secondary_maturity_changed_country_years_2012_2024 =
    summary$changed_cases,
  p15_secondary_maturity_diagnostic_summary = summary$overall,
  p15_secondary_maturity_membership_by_bin = summary$by_bin,
  p15_secondary_maturity_worst_cases = worst_cases
)

for (name in names(outputs)) {
  path <- if (name %in% c(
    "p15_secondary_maturity_diagnostic_summary",
    "p15_secondary_maturity_membership_by_bin",
    "p15_secondary_maturity_worst_cases"
  )) file.path(governance_dir, paste0(name, ".csv")) else
    file.path(output_dir, paste0(name, ".csv.gz"))
  fwrite(as.data.table(outputs[[name]]), path, na = "")
}

generated_paths <- c(
  file.path(output_dir, paste0(names(outputs)[1:4], ".csv.gz")),
  file.path(governance_dir, paste0(names(outputs)[5:7], ".csv"))
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
  producing_script = "scripts/p15/build_p15_secondary_maturity_diagnostic.R",
  build_id = p15_secondary_maturity_diagnostic_build(),
  schema_version = p15_secondary_maturity_diagnostic_schema(),
  build_date = "2026-08-14"
)]
fwrite(
  manifest,
  file.path(governance_dir, "p15_secondary_maturity_diagnostic_manifest.csv"),
  na = ""
)

cat("Built detailed secondary maturity diagnostic.\n")
print(as.data.table(summary$overall))
