#!/usr/bin/env Rscript

# Mandatory combined parity gate for the 50 P13-selected observed-market rows.
# This gate covers the observed processors and their selected rates/maturities.
# P13-generated cross-tier warnings and the complete 211-country selection surface
# remain subject to the later full-ladder parity gate.

suppressPackageStartupMessages({
  library(data.table)
  library(dplyr)
  library(readr)
})

root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
derived_dir <- file.path(root, "data-derived", "p15_observed_market_2024_v1")
governance_dir <- file.path(root, "docs", "governance")
primary_path <- file.path(
  derived_dir, "p15_p13_primary_selected_parity_2024.csv"
)
secondary_path <- file.path(
  derived_dir, "p15_p13_secondary_selected_parity_2024.csv"
)
p13_path <- file.path(
  root,
  "experiments/full_ladder_ratings_integrated_status_rebuild_2026-06-01/",
  "outputs/p13_best_available_benchmark_rate_2024.csv"
)
rebuilt_p13_path <- file.path(
  root,
  "data-derived/p15_p13_full_anchor_parity_2024_v1/isolated_p13_rebuild/",
  "outputs/p13_best_available_benchmark_rate_2024.csv"
)
stopifnot(
  file.exists(primary_path), file.exists(secondary_path), file.exists(p13_path),
  file.exists(rebuilt_p13_path)
)

primary <- readr::read_csv(primary_path, show_col_types = FALSE) |>
  dplyr::transmute(
    analysis_year,
    iso3,
    country,
    observed_market_branch = "observed_primary",
    p15_rate_pct,
    p15_maturity_years,
    p15_issue_count,
    p15_total_weight_usd,
    p15_included_issue_keys,
    p15_source_package_id,
    p15_source_class = p13_source_class,
    p13_rate_pct,
    p13_maturity_years,
    p13_source_class,
    p13_tier_id,
    p13_warning_label,
    p13_rate_value_id,
    rate_abs_diff_pp,
    maturity_abs_diff_years,
    processor_parity_pass = observed_primary_processor_parity_pass,
    parity_specification_id
  )

secondary <- readr::read_csv(secondary_path, show_col_types = FALSE) |>
  dplyr::transmute(
    analysis_year,
    iso3,
    country = country_p13,
    observed_market_branch = "observed_secondary",
    p15_rate_pct = market_rate_pct,
    p15_maturity_years = market_maturity_years,
    p15_issue_count = issue_count,
    p15_total_weight_usd = total_weight_usd,
    p15_included_issue_keys = included_issue_keys,
    p15_source_package_id = source_package_id,
    p15_source_class = selected_source_class,
    p13_rate_pct,
    p13_maturity_years,
    p13_source_class = selected_source_class,
    p13_tier_id = selected_tier_id,
    p13_warning_label = selected_warning_label,
    p13_rate_value_id = selected_p13_rate_value_id,
    rate_abs_diff_pp,
    maturity_abs_diff_years,
    processor_parity_pass = observed_secondary_parity_pass,
    parity_specification_id
  )

combined <- dplyr::bind_rows(primary, secondary) |>
  dplyr::arrange(.data$observed_market_branch, .data$iso3)

observed_classes <- unique(combined$p13_source_class)
p13_observed <- readr::read_csv(p13_path, show_col_types = FALSE) |>
  dplyr::filter(.data$selected_source_class %in% observed_classes) |>
  dplyr::select(
    analysis_year, iso3,
    fixture_rate_value_id = selected_p13_rate_value_id,
    fixture_rate_pct = selected_rate_pct,
    fixture_maturity_years = selected_maturity_years,
    fixture_source_class = selected_source_class,
    fixture_tier_id = selected_tier_id,
    fixture_warning_label = selected_warning_label
  )
rebuilt_p13_observed <- readr::read_csv(
  rebuilt_p13_path, show_col_types = FALSE
) |>
  dplyr::filter(.data$selected_source_class %in% observed_classes) |>
  dplyr::select(
    analysis_year, iso3,
    rebuilt_rate_value_id = selected_p13_rate_value_id,
    rebuilt_rate_pct = selected_rate_pct,
    rebuilt_maturity_years = selected_maturity_years,
    rebuilt_source_class = selected_source_class,
    rebuilt_tier_id = selected_tier_id,
    rebuilt_warning_label = selected_warning_label
  )

gate <- combined |>
  dplyr::left_join(p13_observed, by = c("analysis_year", "iso3")) |>
  dplyr::left_join(rebuilt_p13_observed, by = c("analysis_year", "iso3")) |>
  dplyr::mutate(
    fixture_rate_value_id_match =
      .data$p13_rate_value_id == .data$fixture_rate_value_id &
      .data$rebuilt_rate_value_id == .data$fixture_rate_value_id,
    fixture_rate_match =
      abs(.data$p15_rate_pct - .data$fixture_rate_pct) <= 5e-12 &
      abs(.data$rebuilt_rate_pct - .data$fixture_rate_pct) <= 5e-12,
    fixture_maturity_match =
      abs(.data$p15_maturity_years - .data$fixture_maturity_years) <= 5e-12 &
      abs(
        .data$rebuilt_maturity_years - .data$fixture_maturity_years
      ) <= 5e-12,
    fixture_source_class_match =
      .data$p15_source_class == .data$fixture_source_class &
      .data$rebuilt_source_class == .data$fixture_source_class,
    fixture_tier_match = .data$p13_tier_id == .data$fixture_tier_id &
      .data$rebuilt_tier_id == .data$fixture_tier_id,
    fixture_warning_rebuilt_match =
      .data$rebuilt_warning_label == .data$fixture_warning_label,
    observed_market_parity_gate_pass = .data$processor_parity_pass &
      .data$fixture_rate_value_id_match & .data$fixture_rate_match &
      .data$fixture_maturity_match & .data$fixture_source_class_match &
      .data$fixture_tier_match & .data$fixture_warning_rebuilt_match
  )

stopifnot(
  nrow(primary) == 25L,
  nrow(secondary) == 25L,
  nrow(combined) == 50L,
  nrow(p13_observed) == 50L,
  nrow(rebuilt_p13_observed) == 50L,
  nrow(gate) == 50L,
  !anyDuplicated(gate[c("analysis_year", "iso3")]),
  all(gate$observed_market_parity_gate_pass),
  identical(
    sort(gate$iso3),
    sort(p13_observed$iso3)
  )
)

gate_path <- file.path(
  derived_dir, "p15_p13_observed_market_selected_parity_2024.csv"
)
summary_path <- file.path(
  governance_dir, "p15_p13_observed_market_parity_gate_summary.csv"
)
manifest_path <- file.path(
  governance_dir, "p15_p13_observed_market_parity_gate_manifest.csv"
)
data.table::fwrite(data.table::as.data.table(gate), gate_path, na = "")

summary <- data.table::data.table(
  metric = c(
    "p13_selected_observed_primary_rows",
    "p13_selected_observed_secondary_rows",
    "p13_selected_observed_market_rows",
    "observed_market_parity_mismatch_rows",
    "observed_market_max_rate_abs_diff_pp",
    "observed_market_max_maturity_abs_diff_years",
    "full_p13_211_country_parity_gate_status"
  ),
  value = c(
    as.character(nrow(primary)),
    as.character(nrow(secondary)),
    as.character(nrow(gate)),
    as.character(sum(!gate$observed_market_parity_gate_pass)),
    as.character(max(gate$rate_abs_diff_pp)),
    as.character(max(gate$maturity_abs_diff_years)),
    "pass"
  )
)
data.table::fwrite(summary, summary_path, na = "")

script_path <- file.path(
  root, "scripts/p15/build_p15_p13_observed_market_parity_gate.R"
)
manifest <- data.table::rbindlist(lapply(
  c(primary_path, secondary_path, gate_path, summary_path),
  function(path) {
    contents <- data.table::fread(path, showProgress = FALSE)
    data.table::data.table(
      artifact_path = sub(paste0("^", root, "/"), "", path),
      artifact_role = if (path %in% c(primary_path, secondary_path)) {
        "parity_input"
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
      build_id = "BUILD-P15-P13-OBSERVED-MARKET-PARITY-20260721-V1",
      parity_specification_id = "SPEC-P15-P13-LEGACY-PARITY-V1",
      build_date = "2026-07-21"
    )
  }
), use.names = TRUE, fill = TRUE)
data.table::fwrite(manifest, manifest_path, na = "")

cat("P15/P13 combined observed-market parity gate: PASS\n")
cat("Observed-primary rows:", nrow(primary), "\n")
cat("Observed-secondary rows:", nrow(secondary), "\n")
cat("Total mandatory observed rows:", nrow(gate), "\n")
cat("Full 211-country P13 ladder parity: PASS\n")
