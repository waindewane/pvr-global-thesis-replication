#!/usr/bin/env Rscript

# Build the neutral 2012-2024 P15 country-year foundation and publish the schema
# dictionary/taxonomy. This stage contains no market rates or selection decisions.

suppressPackageStartupMessages({
  library(data.table)
  library(dplyr)
  library(readr)
  library(tibble)
})

source("R/p15_schema.R")

root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
derived_dir <- file.path(root, "data-derived", "p15_platform_2012_2024_v1")
governance_dir <- file.path(root, "docs", "governance")
dir.create(derived_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(governance_dir, recursive = TRUE, showWarnings = FALSE)

classification_rel <- paste0(
  "experiments/p14_historical_validation_and_extension_2026-06-22/outputs/",
  "p14_historical_quality_equalization_2012_2023/",
  "p14_historical_country_classification_ledger_2012_2024.csv"
)
p13_rel <- paste0(
  "experiments/full_ladder_ratings_integrated_status_rebuild_2026-06-01/",
  "outputs/p13_best_available_benchmark_rate_2024.csv"
)
classification_path <- file.path(root, classification_rel)
p13_path <- file.path(root, p13_rel)
stopifnot(file.exists(classification_path), file.exists(p13_path))

classification <- readr::read_csv(
  classification_path, show_col_types = FALSE, guess_max = 100000
)
p13_2024 <- readr::read_csv(
  p13_path, show_col_types = FALSE, guess_max = 100000
) 
grid <- p15_build_country_year_grid(
  classification,
  years = 2012:2024,
  canonical_country_lookup = p13_2024 |> dplyr::select(iso3, country)
)

# P13's 2024 cross-section remains the exact geography/classification anchor. The
# historical source-backed labels may differ in earlier years but must match in 2024.
p13_2024 <- p13_2024 |>
  dplyr::select(
    analysis_year, iso3,
    p13_country = country,
    p13_income_level = income_level,
    p13_lmic_scope = included_in_lmic_reporting_scope
  )
reconciliation_2024 <- grid |>
  dplyr::filter(.data$analysis_year == 2024L) |>
  dplyr::left_join(p13_2024, by = c("analysis_year", "iso3")) |>
  dplyr::mutate(
    country_match = .data$country == .data$p13_country,
    income_level_match =
      .data$historical_income_level == .data$p13_income_level,
    lmic_scope_match =
      .data$historical_lmic_reporting_scope == .data$p13_lmic_scope,
    anchor_match = .data$country_match & .data$income_level_match &
      .data$lmic_scope_match
  )

stopifnot(
  nrow(grid) == 2743L,
  nrow(grid |> dplyr::distinct(.data$iso3)) == 211L,
  nrow(grid |> dplyr::filter(.data$iso3 == "XKX")) == 13L,
  sum(
    grid$classification_review_state ==
      "source_missing_not_classified"
  ) == 12L,
  sum(
    grid$static_vs_historical_classification_conflict %in% c(
      "static_income_label_differs_from_historical_source",
      "static_lmic_scope_differs_from_historical_source"
    )
  ) == 331L,
  sum(
    grid$analysis_year == 2024L &
      grid$historical_lmic_reporting_scope,
    na.rm = TRUE
  ) == 128L,
  nrow(reconciliation_2024) == 211L,
  all(reconciliation_2024$anchor_match)
)

grid_path <- file.path(
  derived_dir, "p15_country_year_grid_2012_2024.csv"
)
reconciliation_path <- file.path(
  governance_dir, "p15_country_year_grid_p13_anchor_reconciliation_2024.csv"
)
schema_path <- file.path(
  governance_dir, "p15_neutral_ladder_schema_dictionary.csv"
)
taxonomy_path <- file.path(
  governance_dir, "p15_neutral_ladder_taxonomy.csv"
)
summary_path <- file.path(
  governance_dir, "p15_country_year_grid_summary.csv"
)
manifest_path <- file.path(
  governance_dir, "p15_country_year_grid_manifest.csv"
)

data.table::fwrite(data.table::as.data.table(grid), grid_path, na = "")
data.table::fwrite(
  data.table::as.data.table(reconciliation_2024),
  reconciliation_path,
  na = ""
)
data.table::fwrite(
  data.table::as.data.table(p15_neutral_schema_dictionary()),
  schema_path,
  na = ""
)
data.table::fwrite(
  data.table::as.data.table(p15_neutral_taxonomy()),
  taxonomy_path,
  na = ""
)

summary <- data.table::data.table(
  metric = c(
    "country_year_rows_2012_2024",
    "countries_per_year",
    "analysis_year_count",
    "kosovo_xkx_rows",
    "source_backed_classification_rows",
    "classification_source_missing_rows",
    "static_vs_historical_conflict_rows",
    "historical_lmic_rows_2024",
    "p13_2024_anchor_mismatch_rows",
    "lending_type_time_basis",
    "country_year_grid_schema_version",
    "neutral_ladder_schema_version"
  ),
  value = c(
    as.character(nrow(grid)),
    "211",
    as.character(length(unique(grid$analysis_year))),
    as.character(sum(grid$iso3 == "XKX")),
    as.character(sum(!is.na(grid$income_classification_source_package_ids))),
    as.character(sum(
      grid$classification_review_state == "source_missing_not_classified"
    )),
    as.character(sum(
      grid$static_vs_historical_classification_conflict %in% c(
        "static_income_label_differs_from_historical_source",
        "static_lmic_scope_differs_from_historical_source"
      )
    )),
    as.character(sum(
      grid$analysis_year == 2024L & grid$historical_lmic_reporting_scope,
      na.rm = TRUE
    )),
    as.character(sum(!reconciliation_2024$anchor_match)),
    "static_project_snapshot_descriptive_only",
    p15_country_year_grid_schema_version(),
    p15_ladder_schema_version()
  )
)
data.table::fwrite(summary, summary_path, na = "")

script_path <- file.path(root, "scripts/p15/build_p15_neutral_country_year_grid.R")
manifest <- data.table::rbindlist(lapply(
  c(
    classification_path, grid_path, reconciliation_path, schema_path,
    taxonomy_path, summary_path
  ),
  function(path) {
    contents <- data.table::fread(path, showProgress = FALSE)
    data.table::data.table(
      artifact_path = sub(paste0("^", root, "/"), "", path),
      artifact_role = if (path == classification_path) {
        "classification_input"
      } else {
        "generated_output"
      },
      rows = nrow(contents),
      columns = ncol(contents),
      bytes = file.info(path)$size,
      sha256 = digest::digest(file = path, algo = "sha256"),
      producing_script = sub(paste0("^", root, "/"), "", script_path),
      producing_script_sha256 = digest::digest(
        file = script_path, algo = "sha256"
      ),
      build_id = "BUILD-P15-COUNTRY-YEAR-GRID-20260721-V1",
      schema_version = p15_country_year_grid_schema_version(),
      build_date = "2026-07-21"
    )
  }
), use.names = TRUE, fill = TRUE)
data.table::fwrite(manifest, manifest_path, na = "")

cat("P15 neutral country-year grid: PASS\n")
cat("Rows:", nrow(grid), "(211 countries x 13 years)\n")
cat("P13 2024 classification mismatches:", sum(!reconciliation_2024$anchor_match), "\n")
cat("Static-versus-historical classification conflicts retained:", 331, "\n")
cat("Source-missing/not-classified rows retained:", 12, "\n")
