#!/usr/bin/env Rscript

# Reconstruct all P13-selected 2024 observed-secondary results through common P15
# functions. Preserved issue-level and targeted companion evidence are inputs; P13
# country rates are comparison fixtures only and never feed the calculations.

suppressPackageStartupMessages({
  library(data.table)
  library(dplyr)
  library(readr)
  library(stringr)
})

source("R/p15_observed_secondary.R")

root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
experiment_rel <- "experiments/full_ladder_ratings_integrated_status_rebuild_2026-06-01"
experiment_out <- file.path(root, experiment_rel, "outputs")
derived_dir <- file.path(root, "data-derived", "p15_observed_market_2024_v1")
governance_dir <- file.path(root, "docs", "governance")
dir.create(derived_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(governance_dir, recursive = TRUE, showWarnings = FALSE)

path_out <- function(file_name) file.path(experiment_out, file_name)
read_csv_local <- function(path) {
  readr::read_csv(path, show_col_types = FALSE, guess_max = 100000)
}

secondary_source_rel <- paste0(
  "experiments/full_ladder_database_all_years_2026-05-20/outputs/",
  "secondary_issue_level_all_years.csv"
)
required_paths <- c(
  file.path(root, secondary_source_rel),
  path_out("p12a_secondary_direct_country_scenarios_2024.csv"),
  path_out("secondary_terminal_post_audit_review_classes_2024.csv"),
  path_out("secondary_price_to_yield_identifier_price_snapshot_2024.csv"),
  path_out("secondary_price_to_yield_row_classification_2024.csv"),
  path_out("secondary_price_to_yield_trial_results_2024.csv"),
  path_out("p12a_feature_rich_secondary_accepted_issue_layer_2024.csv"),
  path_out("p12a_feature_rich_secondary_country_layer_2024.csv"),
  path_out("p13_best_available_benchmark_rate_2024.csv"),
  file.path(
    root,
    "experiments/paper_candidate_2024_clean_v1_2026-05-27/outputs/",
    "paper_candidate_benchmark_evidence_2024.csv"
  )
)
stopifnot(all(file.exists(required_paths)))

secondary_source <- read_csv_local(file.path(root, secondary_source_rel))
terminal_classes <- read_csv_local(
  path_out("secondary_terminal_post_audit_review_classes_2024.csv")
)
price_snapshots <- read_csv_local(
  path_out("secondary_price_to_yield_identifier_price_snapshot_2024.csv")
)
price_eligibility <- read_csv_local(
  path_out("secondary_price_to_yield_row_classification_2024.csv")
)
feature_issue_layer <- read_csv_local(
  path_out("p12a_feature_rich_secondary_accepted_issue_layer_2024.csv")
)

direct_issues <- p15_prepare_secondary_direct_issues(
  secondary_source,
  target_year = 2024L
)
standard_direct <- p15_aggregate_secondary_direct(
  direct_issues,
  candidate_field = "p12a_standard_usd_2_15_candidate",
  source_class = "p12a_secondary_direct_yield_rebuild",
  source_package_id = "SRC-P12A-SECONDARY-DIRECT-2024",
  method_id = "P12A_secondary_direct_ytm_usd_2_15_v1"
)
legacy_direct <- p15_aggregate_secondary_direct(
  direct_issues,
  candidate_field = "p13_legacy_direct_candidate",
  source_class = "lseg_research_grade_secondary_direct_ytm",
  source_package_id = "SRC-LSEG-SECONDARY-ALL-YEARS-20260520",
  method_id = "paper_candidate_secondary_direct_ytm_v1",
  issue_key_separator = " || "
)
terminal_direct <- p15_build_terminal_direct_country(terminal_classes)
price_to_yield <- p15_build_terminal_price_to_yield_country(
  price_snapshots,
  price_eligibility,
  terminal_classes
)
feature_rich <- p15_build_feature_rich_secondary_country(feature_issue_layer)

# Branch-level fixture checks establish that the common P15 functions reproduce
# the preserved intermediate processors, not only the final selected rates.
standard_reference <- read_csv_local(
  path_out("p12a_secondary_direct_country_scenarios_2024.csv")
) |>
  dplyr::filter(
    .data$secondary_scenario_id ==
      "secondary_direct_ytm_usd_only_2_15_standard_outstanding_weighted"
  ) |>
  dplyr::select(
    analysis_year, iso3,
    reference_rate_pct = scenario_rate_pct,
    reference_issue_count = scenario_issue_count,
    reference_total_weight_usd = scenario_total_weight_usd,
    reference_issue_keys = scenario_issue_keys
  )
standard_check <- standard_direct |>
  dplyr::left_join(standard_reference, by = c("analysis_year", "iso3")) |>
  dplyr::mutate(
    rate_diff = abs(.data$market_rate_pct - .data$reference_rate_pct),
    issue_count_match = .data$issue_count == .data$reference_issue_count,
    weight_diff = abs(.data$total_weight_usd - .data$reference_total_weight_usd),
    issue_keys_match = .data$included_issue_keys == .data$reference_issue_keys,
    branch_parity_pass = .data$rate_diff <= 5e-12 &
      .data$issue_count_match & .data$weight_diff <= 0.01 &
      .data$issue_keys_match
  )

legacy_reference <- read_csv_local(file.path(
  root,
  "experiments/paper_candidate_2024_clean_v1_2026-05-27/outputs/",
  "paper_candidate_benchmark_evidence_2024.csv"
)) |>
  dplyr::filter(.data$benchmark_source_tier == "secondary_market_direct_ytm") |>
  dplyr::select(
    analysis_year, iso3,
    reference_rate_pct = market_rate_pct,
    reference_maturity_years = market_maturity_years,
    reference_issue_count = issue_count,
    reference_total_weight_usd = total_weight_usd,
    reference_issue_keys = included_issue_keys
  )
legacy_check <- legacy_direct |>
  dplyr::left_join(legacy_reference, by = c("analysis_year", "iso3")) |>
  dplyr::mutate(
    rate_diff = abs(.data$market_rate_pct - .data$reference_rate_pct),
    maturity_diff = abs(
      .data$market_maturity_years - .data$reference_maturity_years
    ),
    issue_count_match = .data$issue_count == .data$reference_issue_count,
    weight_diff = abs(.data$total_weight_usd - .data$reference_total_weight_usd),
    issue_keys_match = .data$included_issue_keys == .data$reference_issue_keys,
    branch_parity_pass = .data$rate_diff <= 5e-12 &
      .data$maturity_diff <= 5e-12 & .data$issue_count_match &
      .data$weight_diff <= 0.01 & .data$issue_keys_match
  )

price_reference <- read_csv_local(
  path_out("secondary_price_to_yield_trial_results_2024.csv")
) |>
  dplyr::filter(
    .data$price_to_yield_trial_status ==
      "passes_p7n_trial_quarantine_for_p8_review"
  ) |>
  dplyr::select(
    p7b_target_id,
    reference_rate_pct = trial_repaired_yield_mid_pct,
    reference_identifier_count = identifier_count,
    reference_dispersion_bps = identifier_dispersion_bps,
    reference_settlement_bps = max_settlement_sensitivity_bps,
    reference_bid_ask_bps = max_bid_ask_yield_range_bps
  )
price_check <- price_to_yield |>
  dplyr::left_join(
    price_reference,
    by = c("included_target_ids" = "p7b_target_id")
  ) |>
  dplyr::mutate(
    rate_diff = abs(.data$market_rate_pct - .data$reference_rate_pct),
    identifier_count_match =
      .data$identifier_count == .data$reference_identifier_count,
    dispersion_diff = abs(
      .data$identifier_dispersion_bps - .data$reference_dispersion_bps
    ),
    settlement_diff = abs(
      .data$max_settlement_sensitivity_bps - .data$reference_settlement_bps
    ),
    bid_ask_diff = abs(
      .data$max_bid_ask_yield_range_bps - .data$reference_bid_ask_bps
    ),
    branch_parity_pass = .data$rate_diff <= 5e-12 &
      .data$identifier_count_match & .data$dispersion_diff <= 5e-10 &
      .data$settlement_diff <= 5e-10 & .data$bid_ask_diff <= 5e-10
  )

feature_reference <- read_csv_local(
  path_out("p12a_feature_rich_secondary_country_layer_2024.csv")
) |>
  dplyr::filter(
    .data$accepted_scope == paste0(
      "full_ladder_labelled_secondary_feature_rich_vendor_yield_",
      "usd_2_15_standard"
    )
  ) |>
  dplyr::select(
    analysis_year, iso3,
    reference_rate_pct = feature_rich_country_rate_pct,
    reference_issue_count = feature_rich_issue_count,
    reference_identifier_count = feature_rich_identifier_count,
    reference_total_weight_usd = feature_rich_total_weight_usd,
    reference_issue_keys = feature_rich_issue_layer_ids
  )
feature_check <- feature_rich |>
  dplyr::left_join(feature_reference, by = c("analysis_year", "iso3")) |>
  dplyr::mutate(
    rate_diff = abs(.data$market_rate_pct - .data$reference_rate_pct),
    issue_count_match = .data$issue_count == .data$reference_issue_count,
    identifier_count_match =
      .data$identifier_count == .data$reference_identifier_count,
    weight_diff = abs(.data$total_weight_usd - .data$reference_total_weight_usd),
    issue_keys_match = .data$included_issue_keys == .data$reference_issue_keys,
    branch_parity_pass = .data$rate_diff <= 5e-12 &
      .data$issue_count_match & .data$identifier_count_match &
      .data$weight_diff <= 0.01 & .data$issue_keys_match
  )

p13_selected <- read_csv_local(
  path_out("p13_best_available_benchmark_rate_2024.csv")
) |>
  dplyr::filter(.data$selected_source_class %in% c(
    "p12a_secondary_direct_yield_rebuild",
    "lseg_research_grade_secondary_direct_ytm",
    "lseg_terminal_secondary_direct_ytm",
    "lseg_terminal_secondary_price_to_yield",
    "p12a_feature_rich_vendor_yield_review"
  ))

all_secondary_candidates <- dplyr::bind_rows(
  standard_direct,
  legacy_direct,
  terminal_direct,
  price_to_yield,
  feature_rich
)
selected <- p13_selected |>
  dplyr::select(
    analysis_year, iso3, country,
    p13_rate_pct = selected_rate_pct,
    p13_maturity_years = selected_maturity_years,
    selected_source_class,
    selected_tier_id,
    selected_warning_label,
    selected_source_pointer,
    selected_p13_rate_value_id
  ) |>
  dplyr::inner_join(
    all_secondary_candidates,
    by = c("analysis_year", "iso3", "selected_source_class"),
    suffix = c("_p13", "_p15")
  ) |>
  dplyr::mutate(
    rate_abs_diff_pp = abs(.data$market_rate_pct - .data$p13_rate_pct),
    maturity_abs_diff_years = abs(
      .data$market_maturity_years - .data$p13_maturity_years
    ),
    observed_secondary_parity_pass = .data$rate_abs_diff_pp <= 5e-12 &
      .data$maturity_abs_diff_years <= 5e-12
  ) |>
  dplyr::arrange(.data$iso3)

stopifnot(
  nrow(direct_issues) == 1088L,
  sum(direct_issues$p12a_standard_usd_2_15_candidate) == 123L,
  nrow(standard_direct) == 36L,
  nrow(standard_check) == 36L,
  all(standard_check$branch_parity_pass),
  sum(direct_issues$p13_legacy_direct_candidate) == 172L,
  nrow(legacy_direct) == 35L,
  nrow(legacy_check) == 35L,
  all(legacy_check$branch_parity_pass),
  nrow(terminal_direct) == 1L,
  terminal_direct$iso3 == "ALB",
  nrow(price_to_yield) == 3L,
  all(price_check$branch_parity_pass),
  nrow(feature_rich) == 3L,
  all(feature_check$branch_parity_pass),
  nrow(p13_selected) == 25L,
  nrow(selected) == 25L,
  !anyDuplicated(selected[c("analysis_year", "iso3")]),
  all(selected$observed_secondary_parity_pass)
)

paths <- c(
  direct_issue_universe = file.path(
    derived_dir, "p15_p13_secondary_direct_issue_universe_2024.csv.gz"
  ),
  standard_direct = file.path(
    derived_dir, "p15_p13_secondary_standard_direct_country_2024.csv"
  ),
  legacy_direct = file.path(
    derived_dir, "p15_p13_secondary_legacy_direct_country_2024.csv"
  ),
  terminal_direct = file.path(
    derived_dir, "p15_p13_secondary_terminal_direct_country_2024.csv"
  ),
  price_to_yield = file.path(
    derived_dir, "p15_p13_secondary_price_to_yield_country_2024.csv"
  ),
  feature_rich = file.path(
    derived_dir, "p15_p13_secondary_feature_rich_country_2024.csv"
  ),
  selected = file.path(
    derived_dir, "p15_p13_secondary_selected_parity_2024.csv"
  )
)
data.table::fwrite(data.table::as.data.table(direct_issues), paths[[1]], na = "")
data.table::fwrite(data.table::as.data.table(standard_direct), paths[[2]], na = "")
data.table::fwrite(data.table::as.data.table(legacy_direct), paths[[3]], na = "")
data.table::fwrite(data.table::as.data.table(terminal_direct), paths[[4]], na = "")
data.table::fwrite(data.table::as.data.table(price_to_yield), paths[[5]], na = "")
data.table::fwrite(data.table::as.data.table(feature_rich), paths[[6]], na = "")
data.table::fwrite(data.table::as.data.table(selected), paths[[7]], na = "")

summary <- data.table::data.table(
  metric = c(
    "secondary_issue_rows_2024",
    "p12a_standard_candidate_issue_rows",
    "p12a_standard_country_rows",
    "p12a_standard_branch_mismatch_rows",
    "legacy_direct_candidate_issue_rows",
    "legacy_direct_country_rows",
    "legacy_direct_branch_mismatch_rows",
    "terminal_direct_country_rows",
    "price_to_yield_country_rows",
    "price_to_yield_branch_mismatch_rows",
    "feature_rich_standard_country_rows",
    "feature_rich_branch_mismatch_rows",
    "p13_selected_secondary_rows",
    "p13_selected_secondary_mismatch_rows",
    "p13_selected_secondary_max_rate_abs_diff_pp",
    "p13_selected_secondary_max_maturity_abs_diff_years"
  ),
  value = as.character(c(
    nrow(direct_issues),
    sum(direct_issues$p12a_standard_usd_2_15_candidate),
    nrow(standard_direct),
    sum(!standard_check$branch_parity_pass),
    sum(direct_issues$p13_legacy_direct_candidate),
    nrow(legacy_direct),
    sum(!legacy_check$branch_parity_pass),
    nrow(terminal_direct),
    nrow(price_to_yield),
    sum(!price_check$branch_parity_pass),
    nrow(feature_rich),
    sum(!feature_check$branch_parity_pass),
    nrow(selected),
    sum(!selected$observed_secondary_parity_pass),
    max(selected$rate_abs_diff_pp),
    max(selected$maturity_abs_diff_years)
  ))
)
summary_path <- file.path(
  governance_dir, "p15_p13_observed_secondary_parity_summary.csv"
)
data.table::fwrite(summary, summary_path, na = "")

script_path <- file.path(
  root, "scripts/p15/build_p15_p13_observed_secondary_parity.R"
)
manifest_paths <- c(unname(paths), summary_path)
manifest <- data.table::rbindlist(lapply(manifest_paths, function(path) {
  contents <- data.table::fread(path, showProgress = FALSE)
  data.table::data.table(
    artifact_path = sub(paste0("^", root, "/"), "", path),
    rows = nrow(contents),
    columns = ncol(contents),
    bytes = file.info(path)$size,
    sha256 = digest::digest(file = path, algo = "sha256"),
    producing_script = sub(paste0("^", root, "/"), "", script_path),
    producing_script_sha256 = digest::digest(
      file = script_path, algo = "sha256"
    ),
    build_id = "BUILD-P15-P13-SECONDARY-PARITY-20260721-V1",
    parity_specification_id = "SPEC-P15-P13-LEGACY-PARITY-V1",
    build_date = "2026-07-21"
  )
}), use.names = TRUE, fill = TRUE)
data.table::fwrite(
  manifest,
  file.path(
    governance_dir, "p15_p13_observed_secondary_parity_manifest.csv"
  ),
  na = ""
)

cat("P15/P13 observed-secondary parity: PASS\n")
cat("P13-selected secondary rows reconstructed:", nrow(selected), "\n")
cat("Maximum selected-rate difference:", max(selected$rate_abs_diff_pp), "pp\n")
cat(
  "Maximum selected-maturity difference:",
  max(selected$maturity_abs_diff_years), "years\n"
)
