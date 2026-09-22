#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(data.table)
  library(digest)
  library(dplyr)
  library(tibble)
})

source("R/p15_primary_closure_evidence.R")

root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
governance_dir <- file.path(root, "docs", "governance")
input_path <- file.path(
  root, "data-derived", "p15_observed_markets_2012_2024_v1",
  "p15_primary_country_year_candidate_variants_2012_2024.csv.gz"
)
if (!file.exists(input_path)) {
  stop("Missing primary candidate-variant input: ", input_path, call. = FALSE)
}

variants <- fread(input_path) |> as_tibble()
comparison <- p15_compare_primary_materiality_variants(variants)

outputs <- list(
  p15_primary_materiality_closure_summary = comparison$summary,
  p15_primary_materiality_coverage_differences = comparison$coverage_differences,
  p15_primary_materiality_rate_differences = comparison$rate_differences
)

for (name in names(outputs)) {
  fwrite(
    as.data.table(outputs[[name]]),
    file.path(governance_dir, paste0(name, ".csv")),
    na = ""
  )
}

generated_paths <- file.path(
  governance_dir, paste0(names(outputs), ".csv")
)
manifest <- data.table(
  artifact_path = sub(
    paste0("^", root, "/"), "", c(input_path, generated_paths)
  ),
  artifact_role = c("source_input", rep("generated_output", length(outputs)))
)
manifest[, `:=`(
  bytes = file.info(file.path(root, artifact_path))$size,
  sha256 = vapply(
    file.path(root, artifact_path), digest, character(1),
    algo = "sha256", file = TRUE
  ),
  producing_script = "scripts/p15/build_p15_primary_closure_evidence.R",
  build_id = p15_primary_closure_build(),
  schema_version = p15_primary_closure_schema(),
  build_date = "2026-08-14"
)]
fwrite(
  manifest,
  file.path(governance_dir, "p15_primary_materiality_closure_manifest.csv"),
  na = ""
)

cat("Built P15 primary materiality closure evidence.\n")
print(as.data.table(comparison$summary))
