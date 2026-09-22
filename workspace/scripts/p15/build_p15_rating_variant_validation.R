#!/usr/bin/env Rscript

# Build controlled rating-implied mapping, agency, vintage, timing, and
# risk-free variants. Validation uses clearly labelled predecessor observed
# anchors because the common all-years P15 observed processors are not complete.
# The result is decision preparation, not a final estimator choice.

suppressPackageStartupMessages({
  library(data.table)
  library(digest)
  library(dplyr)
  library(readr)
  library(tibble)
})

source("R/p15_rating_components.R")
source("R/p15_rating_validation.R")

root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
derived_dir <- file.path(root, "data-derived", "p15_rating_validation_2012_2024_v1")
governance_dir <- file.path(root, "docs", "governance")
dir.create(derived_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(governance_dir, recursive = TRUE, showWarnings = FALSE)

input_rel <- c(
  ledger = "data-derived/p15_rating_components_2012_2024_v1/p15_rating_component_ledger_2012_2024.csv.gz",
  status_evidence = "data-derived/p15_status_context_2012_2024_v1/p15_status_context_evidence_2012_2024.csv.gz",
  damodaran = "experiments/full_ladder_database_all_years_2026-05-20/outputs/damodaran_archive_country_spreads_2000_2024.csv",
  ratings = "data-raw/ratings/bloomberg_sovereign_rating_panel_2026-05-28/bloomberg_boy_ratings_single_precedence_country_year_2000_2025.csv",
  current_master = "experiments/full_ladder_database_all_years_2026-05-20/outputs/master_benchmark_scenarios_all_years.csv",
  combined_permissible = "experiments/p14_historical_validation_and_extension_2026-06-22/outputs/p14_p13_combined_benchmark_panel_2012_2024/p14_p13_combined_permissible_market_rate_ladder_2012_2024.csv",
  issue_alignment = "experiments/p14_historical_validation_and_extension_2026-06-22/outputs/p14_rating_issue_date_alignment_panel_2015_2024.csv",
  risk_free_history = "experiments/full_ladder_database_all_years_2026-05-20/data/lseg_v14/pvr-global-lseg/output/tables/lseg_risk_free_curve_history_oecd_base_2000-01-01_2025-12-31.csv",
  fred_dgs5 = "data-raw/fred_treasury_tenor_sensitivity_2026-07-21/fred_dgs5_2012_2024_downloaded_2026-07-21.csv",
  fred_dgs7_refresh = "data-raw/fred_treasury_tenor_sensitivity_2026-07-21/fred_dgs7_2012_2024_downloaded_2026-07-21.csv",
  fred_dgs10 = "data-raw/fred_treasury_tenor_sensitivity_2026-07-21/fred_dgs10_2012_2024_downloaded_2026-07-21.csv",
  prior_calibration = "experiments/p14_historical_validation_and_extension_2026-06-22/outputs/p14_rating_implied_calibration_candidate_summary_2015_2024.csv"
)
input_paths <- stats::setNames(file.path(root, unname(input_rel)), names(input_rel))
stopifnot(all(file.exists(input_paths)))

ledger <- readr::read_csv(
  input_paths[["ledger"]], show_col_types = FALSE, guess_max = 100000
)
status_evidence <- readr::read_csv(
  input_paths[["status_evidence"]], show_col_types = FALSE, guess_max = 100000
)
damodaran <- readr::read_csv(
  input_paths[["damodaran"]], show_col_types = FALSE, guess_max = 100000
)
ratings <- readr::read_csv(
  input_paths[["ratings"]], show_col_types = FALSE, guess_max = 100000
)
current_master <- readr::read_csv(
  input_paths[["current_master"]], show_col_types = FALSE, guess_max = 100000
)
combined_permissible <- readr::read_csv(
  input_paths[["combined_permissible"]],
  show_col_types = FALSE,
  guess_max = 100000
)
issue_alignment <- readr::read_csv(
  input_paths[["issue_alignment"]], show_col_types = FALSE, guess_max = 100000
)
risk_free_history <- readr::read_csv(
  input_paths[["risk_free_history"]], show_col_types = FALSE, guess_max = 100000
)
fred_dgs5 <- readr::read_csv(
  input_paths[["fred_dgs5"]], comment = "#", show_col_types = FALSE
)
fred_dgs7_refresh <- readr::read_csv(
  input_paths[["fred_dgs7_refresh"]], comment = "#", show_col_types = FALSE
)
fred_dgs10 <- readr::read_csv(
  input_paths[["fred_dgs10"]], comment = "#", show_col_types = FALSE
)
prior_calibration <- readr::read_csv(
  input_paths[["prior_calibration"]], show_col_types = FALSE
)

spread_lookup <- p15_build_damodaran_rating_spread_lookup(damodaran) |>
  dplyr::filter(.data$analysis_year >= 2012L, .data$analysis_year <= 2024L)

context_cols <- c(
  "analysis_year", "iso3", "country", "country_year_id",
  "historical_income_level", "historical_lmic_reporting_scope",
  "rating_source_region",
  "selected_rating_age_band", "agency_disagreement_state",
  "same_day_conflict_event_rows", "risk_free_7y_pct"
)
context <- ledger |>
  dplyr::select(dplyr::all_of(context_cols))

make_mapped_variant <- function(
    input, variant_id, variant_label, agency_rule, timing_rule,
    spread_choice = "median", variant_role = "candidate_comparison") {
  joined <- input |>
    dplyr::left_join(
      spread_lookup,
      by = c("analysis_year", "variant_rating_normalized" = "normalized_rating")
    )
  spread_col <- switch(
    spread_choice,
    min = "damodaran_rating_mapped_spread_min_pct",
    max = "damodaran_rating_mapped_spread_max_pct",
    "damodaran_rating_mapped_spread_pct"
  )
  joined |>
    dplyr::mutate(
      variant_id = variant_id,
      variant_label = variant_label,
      variant_family = "rating_group_mapping",
      agency_rule = agency_rule,
      timing_rule = timing_rule,
      spread_rule = paste0("same_year_rating_group_", spread_choice, "_spread"),
      variant_spread_pct = .data[[spread_col]],
      variant_rate_pct = .data$risk_free_7y_pct + .data$variant_spread_pct,
      variant_available = !is.na(.data$variant_rate_pct),
      variant_uncomputed_reason = dplyr::case_when(
        is.na(.data$variant_rating_normalized) ~ "rating_input_missing_or_unmapped",
        is.na(.data$variant_spread_pct) ~ "rating_year_spread_missing",
        is.na(.data$risk_free_7y_pct) ~ "risk_free_rate_missing",
        TRUE ~ NA_character_
      ),
      variant_role = variant_role,
      variant_decision_state = "not_selected_or_approved"
    ) |>
    dplyr::select(
      dplyr::all_of(context_cols),
      "variant_id", "variant_label", "variant_family", "variant_role",
      "variant_rating", "variant_rating_normalized", "variant_agency",
      "agency_rule", "timing_rule", "spread_rule", "variant_spread_pct",
      "risk_free_7y_pct", "variant_rate_pct", "variant_available",
      "variant_uncomputed_reason", "variant_decision_state"
    )
}

selected_input <- ledger |>
  dplyr::transmute(
    dplyr::across(dplyr::all_of(context_cols)),
    variant_rating = .data$selected_rating,
    variant_rating_normalized = .data$selected_rating_normalized,
    variant_agency = .data$selected_agency
  )
moodys_input <- ledger |>
  dplyr::transmute(
    dplyr::across(dplyr::all_of(context_cols)),
    variant_rating = .data$moodys_rating,
    variant_rating_normalized = .data$moodys_rating_normalized,
    variant_agency = "Moodys"
  )
fitch_input <- ledger |>
  dplyr::transmute(
    dplyr::across(dplyr::all_of(context_cols)),
    variant_rating = .data$fitch_rating,
    variant_rating_normalized = .data$fitch_rating_normalized,
    variant_agency = "Fitch"
  )
sp_input <- ledger |>
  dplyr::transmute(
    dplyr::across(dplyr::all_of(context_cols)),
    variant_rating = .data$sp_rating,
    variant_rating_normalized = .data$sp_rating_normalized,
    variant_agency = "SP"
  )

eoy_input <- ratings |>
  dplyr::filter(
    .data$year >= 2013L, .data$year <= 2025L,
    !is.na(.data$iso3), .data$iso3 != ""
  ) |>
  dplyr::transmute(
    analysis_year = as.integer(.data$year) - 1L,
    iso3 = as.character(.data$iso3),
    variant_rating = as.character(.data$selected_rating),
    variant_rating_normalized = p15_normalize_rating_to_moodys(
      .data$selected_rating
    ),
    variant_agency = as.character(.data$selected_agency)
  ) |>
  dplyr::filter(.data$analysis_year >= 2012L, .data$analysis_year <= 2024L)
stopifnot(!anyDuplicated(eoy_input[c("analysis_year", "iso3")]))
eoy_input <- context |>
  dplyr::left_join(eoy_input, by = c("analysis_year", "iso3"))

mapped_variants <- dplyr::bind_rows(
  make_mapped_variant(
    selected_input,
    "boy_selected_precedence_rating_group_median_dgs7",
    "Beginning-of-year selected agency; rating-group median spread; DGS7",
    "Moodys then Fitch then SP documented precedence",
    "rating active on January 1",
    "median", "exact_p13_reproduction_candidate"
  ),
  make_mapped_variant(
    selected_input,
    "boy_selected_precedence_rating_group_min_dgs7",
    "Beginning-of-year selected agency; rating-group minimum spread; DGS7",
    "Moodys then Fitch then SP documented precedence",
    "rating active on January 1", "min"
  ),
  make_mapped_variant(
    selected_input,
    "boy_selected_precedence_rating_group_max_dgs7",
    "Beginning-of-year selected agency; rating-group maximum spread; DGS7",
    "Moodys then Fitch then SP documented precedence",
    "rating active on January 1", "max"
  ),
  make_mapped_variant(
    moodys_input, "boy_moodys_only_rating_group_median_dgs7",
    "Beginning-of-year Moody's only; rating-group median spread; DGS7",
    "Moodys only; no agency fallback", "rating active on January 1"
  ),
  make_mapped_variant(
    fitch_input, "boy_fitch_only_rating_group_median_dgs7",
    "Beginning-of-year Fitch only; rating-group median spread; DGS7",
    "Fitch only; no agency fallback", "rating active on January 1"
  ),
  make_mapped_variant(
    sp_input, "boy_sp_only_rating_group_median_dgs7",
    "Beginning-of-year S&P only; rating-group median spread; DGS7",
    "SP only; no agency fallback", "rating active on January 1"
  ),
  make_mapped_variant(
    eoy_input, "end_year_next_jan1_selected_rating_group_median_dgs7",
    "End-year proxy from next January 1 selected rating; median spread; DGS7",
    "Moodys then Fitch then SP documented precedence",
    "next January 1 rating used as end-year proxy"
  )
)

agency_long <- dplyr::bind_rows(
  moodys_input |> dplyr::mutate(agency_member = "Moodys"),
  fitch_input |> dplyr::mutate(agency_member = "Fitch"),
  sp_input |> dplyr::mutate(agency_member = "SP")
) |>
  dplyr::filter(!is.na(.data$variant_rating_normalized)) |>
  dplyr::left_join(
    spread_lookup,
    by = c("analysis_year", "variant_rating_normalized" = "normalized_rating")
  ) |>
  dplyr::filter(!is.na(.data$damodaran_rating_mapped_spread_pct))

agency_aggregates <- agency_long |>
  dplyr::group_by(.data$analysis_year, .data$iso3) |>
  dplyr::summarise(
    agency_mapped_spread_median_pct = stats::median(
      .data$damodaran_rating_mapped_spread_pct
    ),
    agency_mapped_spread_max_pct = max(
      .data$damodaran_rating_mapped_spread_pct
    ),
    agency_rating_count = dplyr::n(),
    agency_members = paste(sort(unique(.data$agency_member)), collapse = ";"),
    agency_ratings_normalized = paste(
      sort(unique(.data$variant_rating_normalized)), collapse = ";"
    ),
    .groups = "drop"
  )

make_agency_aggregate_variant <- function(spread_field, variant_id, label, rule) {
  context |>
    dplyr::left_join(agency_aggregates, by = c("analysis_year", "iso3")) |>
    dplyr::mutate(
      variant_id = variant_id,
      variant_label = label,
      variant_family = "multi_agency_spread_aggregation",
      variant_role = "candidate_comparison",
      variant_rating = .data$agency_ratings_normalized,
      variant_rating_normalized = NA_character_,
      variant_agency = .data$agency_members,
      agency_rule = rule,
      timing_rule = "ratings active on January 1",
      spread_rule = spread_field,
      variant_spread_pct = .data[[spread_field]],
      variant_rate_pct = .data$risk_free_7y_pct + .data$variant_spread_pct,
      variant_available = !is.na(.data$variant_rate_pct),
      variant_uncomputed_reason = dplyr::if_else(
        .data$variant_available, NA_character_,
        "no_mapped_preferred_agency_spread"
      ),
      variant_decision_state = "not_selected_or_approved"
    ) |>
    dplyr::select(
      dplyr::all_of(context_cols),
      "variant_id", "variant_label", "variant_family", "variant_role",
      "variant_rating", "variant_rating_normalized", "variant_agency",
      "agency_rule", "timing_rule", "spread_rule", "variant_spread_pct",
      "risk_free_7y_pct", "variant_rate_pct", "variant_available",
      "variant_uncomputed_reason", "variant_decision_state"
    )
}

agency_variants <- dplyr::bind_rows(
  make_agency_aggregate_variant(
    "agency_mapped_spread_median_pct",
    "boy_available_agencies_median_mapped_spread_dgs7",
    "Beginning-of-year available-agency median mapped spread; DGS7",
    "map every available preferred agency then take the median spread"
  ),
  make_agency_aggregate_variant(
    "agency_mapped_spread_max_pct",
    "boy_available_agencies_max_mapped_spread_dgs7",
    "Beginning-of-year available-agency maximum mapped spread; DGS7",
    "map every available preferred agency then take the maximum spread"
  )
)

damodaran_country <- damodaran |>
  dplyr::filter(.data$analysis_year >= 2012L, .data$analysis_year <= 2024L) |>
  dplyr::transmute(
    analysis_year = as.integer(.data$analysis_year),
    iso3 = as.character(.data$iso3),
    variant_rating = as.character(.data$damodaran_rating),
    variant_rating_normalized = p15_normalize_rating_to_moodys(
      .data$damodaran_rating
    ),
    variant_spread_pct = as.numeric(.data$default_spread_pct)
  )
stopifnot(!anyDuplicated(damodaran_country[c("analysis_year", "iso3")]))

current_country <- current_master |>
  dplyr::filter(
    .data$scenario_id == "rating_implied_damodaran_current_vintage_dgs7",
    .data$analysis_year >= 2012L, .data$analysis_year <= 2024L
  ) |>
  dplyr::transmute(
    analysis_year = as.integer(.data$analysis_year),
    iso3 = as.character(.data$iso3),
    current_variant_rate_pct = as.numeric(.data$market_rate_pct),
    current_variant_rating = sub(".*rating=", "", as.character(.data$yield_source))
  )
stopifnot(!anyDuplicated(current_country[c("analysis_year", "iso3")]))

make_direct_spread_variant <- function(
    direct, variant_id, label, role, timing_rule, vintage_state,
    rate_field = NULL) {
  out <- context |>
    dplyr::left_join(direct, by = c("analysis_year", "iso3"))
  if (is.null(rate_field)) {
    out <- out |>
      dplyr::mutate(
        variant_rate_pct = .data$risk_free_7y_pct + .data$variant_spread_pct
      )
  } else {
    out <- out |>
      dplyr::mutate(
        variant_rate_pct = .data[[rate_field]],
        variant_spread_pct = .data$variant_rate_pct - .data$risk_free_7y_pct,
        variant_rating = .data$current_variant_rating,
        variant_rating_normalized = p15_normalize_rating_to_moodys(
          .data$current_variant_rating
        )
      )
  }
  out |>
    dplyr::mutate(
      variant_id = variant_id,
      variant_label = label,
      variant_family = "country_specific_spread",
      variant_role = role,
      variant_agency = "Damodaran country table",
      agency_rule = "Damodaran country-year or current-vintage country rating",
      timing_rule = timing_rule,
      spread_rule = vintage_state,
      variant_available = !is.na(.data$variant_rate_pct),
      variant_uncomputed_reason = dplyr::if_else(
        .data$variant_available, NA_character_, "country_spread_not_available"
      ),
      variant_decision_state = "not_selected_or_approved"
    ) |>
    dplyr::select(
      dplyr::all_of(context_cols),
      "variant_id", "variant_label", "variant_family", "variant_role",
      "variant_rating", "variant_rating_normalized", "variant_agency",
      "agency_rule", "timing_rule", "spread_rule", "variant_spread_pct",
      "risk_free_7y_pct", "variant_rate_pct", "variant_available",
      "variant_uncomputed_reason", "variant_decision_state"
    )
}

direct_variants <- dplyr::bind_rows(
  make_direct_spread_variant(
    damodaran_country,
    "damodaran_archive_country_year_spread_dgs7_p14_legacy",
    "Damodaran archive country-year spread plus DGS7",
    "exact_p14_legacy_reproduction_comparator",
    "same analysis-year archive and risk-free inputs",
    "historical_archive_country_specific_spread"
  ),
  make_direct_spread_variant(
    current_country,
    "damodaran_current_vintage_country_spread_dgs7_blocked",
    "Current-vintage Damodaran country spread repeated over history plus DGS7",
    "invalid_historical_vintage_diagnostic_only",
    "current spread vintage repeated over historical years",
    "current_vintage_substitution_blocked",
    "current_variant_rate_pct"
  )
)

rating_variants <- dplyr::bind_rows(
  mapped_variants, agency_variants, direct_variants
) |>
  dplyr::mutate(
    period = p15_rating_validation_period(.data$analysis_year),
    rating_variant_schema_version = p15_rating_variant_schema_version()
  ) |>
  dplyr::arrange(.data$variant_id, .data$analysis_year, .data$iso3)

stopifnot(
  nrow(rating_variants) == 11L * nrow(ledger),
  !anyDuplicated(rating_variants[c("variant_id", "analysis_year", "iso3")]),
  all(rating_variants$variant_decision_state == "not_selected_or_approved")
)
base_variant <- rating_variants |>
  dplyr::filter(
    .data$variant_id ==
      "boy_selected_precedence_rating_group_median_dgs7"
  ) |>
  dplyr::left_join(
    ledger |>
      dplyr::select(
        analysis_year, iso3,
        ledger_rate_pct = rating_implied_candidate_rate_pct
      ),
    by = c("analysis_year", "iso3")
  )
stopifnot(all(
  is.na(base_variant$variant_rate_pct) == is.na(base_variant$ledger_rate_pct)
))
stopifnot(all(
  abs(
    base_variant$variant_rate_pct - base_variant$ledger_rate_pct
  ) <= 1e-12,
  na.rm = TRUE
))

variant_path <- file.path(
  derived_dir, "p15_rating_candidate_variants_2012_2024.csv.gz"
)
data.table::fwrite(
  data.table::as.data.table(rating_variants), variant_path, na = ""
)

variant_availability <- rating_variants |>
  dplyr::count(
    .data$variant_id, .data$variant_label, .data$variant_role,
    .data$variant_available, .data$variant_uncomputed_reason,
    name = "country_year_rows"
  ) |>
  dplyr::arrange(.data$variant_id, dplyr::desc(.data$variant_available))
variant_availability_path <- file.path(
  governance_dir, "p15_rating_variant_availability_summary.csv"
)
data.table::fwrite(
  data.table::as.data.table(variant_availability),
  variant_availability_path,
  na = ""
)

# Candidate anchor set: clean predecessor observed-primary and direct-secondary
# country-year aggregates. These are suitable for diagnostic comparison but are not
# yet the user-approved final validation authority under VAL-02.
anchors <- combined_permissible |>
  dplyr::filter(
    (.data$analysis_year <= 2023L &
       .data$detailed_ladder_tier_id %in% c(
         "primary_standard_issue50m_ge2",
         "secondary_standard_usd_2_15_direct"
       )) |
      (.data$analysis_year == 2024L &
         .data$detailed_ladder_tier_id %in% c(
           "primary_standard", "secondary_prior_surface_direct_ytm"
         )),
    !is.na(.data$rate_pct), .data$rate_pct >= 1, .data$rate_pct <= 30
  ) |>
  dplyr::transmute(
    analysis_year = as.integer(.data$analysis_year),
    iso3 = as.character(.data$iso3),
    country = as.character(.data$country),
    anchor_family = dplyr::if_else(
      grepl("primary", .data$detailed_ladder_tier_id),
      "clean_observed_primary_candidate",
      "clean_direct_secondary_candidate"
    ),
    anchor_tier_id = as.character(.data$detailed_ladder_tier_id),
    anchor_rate_pct = as.numeric(.data$rate_pct),
    anchor_maturity_years = as.numeric(.data$maturity_years),
    anchor_currency_basis = as.character(.data$currency_basis),
    anchor_issue_count = as.numeric(.data$issue_count),
    anchor_reliability_label = as.character(.data$reliability_label),
    anchor_warning_label = as.character(.data$warning_label),
    anchor_status_review_state = as.character(.data$status_review_state),
    anchor_source_pointer = as.character(.data$source_pointer),
    anchor_authority_state =
      "predecessor_clean_anchor_candidate_pending_VAL_02_authority_decision"
  )
stopifnot(!anyDuplicated(anchors[c("analysis_year", "iso3", "anchor_family")]))

validation_detail <- anchors |>
  dplyr::inner_join(
    rating_variants |>
      dplyr::filter(.data$variant_available) |>
      dplyr::select(
        analysis_year, iso3, historical_income_level,
        historical_lmic_reporting_scope, rating_source_region,
        selected_rating_age_band,
        agency_disagreement_state, same_day_conflict_event_rows,
        variant_id, variant_label, variant_family, variant_role,
        variant_rating, variant_rating_normalized, variant_agency,
        agency_rule, timing_rule, spread_rule, variant_spread_pct,
        risk_free_7y_pct, variant_rate_pct
      ),
    by = c("analysis_year", "iso3"),
    relationship = "many-to-many"
  ) |>
  dplyr::left_join(
    status_evidence |>
      dplyr::select(
        analysis_year, iso3, positive_status_evidence_present,
        boc_default_context_flag, boc_private_or_bond_default_context_flag,
        boc_fiscal_arrears_context_flag,
        paris_club_same_year_agreement_flag,
        systematic_market_access_panel_state,
        status_consequence_state
      ),
    by = c("analysis_year", "iso3")
  ) |>
  dplyr::mutate(
    period = p15_rating_validation_period(.data$analysis_year),
    signed_gap_pp = .data$variant_rate_pct - .data$anchor_rate_pct,
    abs_gap_pp = abs(.data$signed_gap_pp),
    maturity_distance_from_7y = abs(.data$anchor_maturity_years - 7),
    validation_state = "diagnostic_predecessor_anchor_not_final_acceptance_test"
  )
validation_detail_path <- file.path(
  derived_dir, "p15_rating_variant_anchor_gap_detail_2012_2024.csv.gz"
)
data.table::fwrite(
  data.table::as.data.table(validation_detail), validation_detail_path, na = ""
)

validation_samples <- p15_add_rating_validation_sample_cuts(validation_detail)
summarise_with_breakdown <- function(data, dimension, field) {
  data |>
    dplyr::mutate(breakdown_value = as.character(.data[[field]])) |>
    p15_summarise_rating_gaps(
      c("sample_cut", "anchor_family", "variant_id", "breakdown_value")
    ) |>
    dplyr::mutate(breakdown_dimension = dimension, .before = "breakdown_value")
}
validation_summary <- dplyr::bind_rows(
  validation_samples |>
    dplyr::mutate(breakdown_value = "overall") |>
    p15_summarise_rating_gaps(
      c("sample_cut", "anchor_family", "variant_id", "breakdown_value")
    ) |>
    dplyr::mutate(breakdown_dimension = "overall", .before = "breakdown_value"),
  summarise_with_breakdown(validation_samples, "period", "period"),
  summarise_with_breakdown(
    validation_samples, "anchor_currency_basis", "anchor_currency_basis"
  ),
  summarise_with_breakdown(
    validation_samples, "rating_age_band", "selected_rating_age_band"
  ),
  summarise_with_breakdown(
    validation_samples, "agency_disagreement", "agency_disagreement_state"
  ),
  summarise_with_breakdown(
    validation_samples, "region", "rating_source_region"
  ),
  summarise_with_breakdown(
    validation_samples, "positive_status_evidence",
    "positive_status_evidence_present"
  ),
  summarise_with_breakdown(
    validation_samples, "private_or_bond_default_context",
    "boc_private_or_bond_default_context_flag"
  ),
  summarise_with_breakdown(
    validation_samples, "market_access_source_state",
    "systematic_market_access_panel_state"
  )
) |>
  dplyr::arrange(
    .data$breakdown_dimension, .data$sample_cut, .data$anchor_family,
    .data$variant_id, .data$breakdown_value
  )
validation_summary_path <- file.path(
  governance_dir, "p15_rating_variant_anchor_validation_summary.csv"
)
data.table::fwrite(
  data.table::as.data.table(validation_summary),
  validation_summary_path,
  na = ""
)

# Issue-date timing comparison on the predecessor clean primary issue panel.
issue_base <- issue_alignment |>
  dplyr::filter(
    .data$p14_primary_issue_state == "included_standard_primary_candidate",
    !is.na(.data$yield_final_preference),
    .data$yield_final_preference >= 1,
    .data$yield_final_preference <= 30
  ) |>
  dplyr::left_join(
    ledger |>
      dplyr::select(
        analysis_year, iso3, historical_income_level,
        historical_lmic_reporting_scope,
        boy_rating = selected_rating,
        boy_rating_normalized = selected_rating_normalized,
        boy_rating_agency = selected_agency,
        boy_rate_pct = rating_implied_candidate_rate_pct,
        damodaran_rating_mapped_spread_pct,
        risk_free_7y_pct
      ),
    by = c("analysis_year", "iso3")
  ) |>
  dplyr::left_join(
    rating_variants |>
      dplyr::filter(
        .data$variant_id ==
          "end_year_next_jan1_selected_rating_group_median_dgs7"
      ) |>
      dplyr::select(
        analysis_year, iso3,
        eoy_rating = variant_rating,
        eoy_rating_normalized = variant_rating_normalized,
        eoy_rating_agency = variant_agency,
        eoy_rate_pct = variant_rate_pct
      ),
    by = c("analysis_year", "iso3")
  ) |>
  dplyr::mutate(
    issue_rating_normalized = p15_normalize_rating_to_moodys(
      .data$issue_aligned_rating
    )
  ) |>
  dplyr::left_join(
    spread_lookup |>
      dplyr::select(
        analysis_year, normalized_rating,
        issue_rating_spread_pct = damodaran_rating_mapped_spread_pct
      ),
    by = c("analysis_year", "issue_rating_normalized" = "normalized_rating")
  ) |>
  dplyr::mutate(
    issue_aligned_rate_pct =
      .data$risk_free_7y_pct + .data$issue_rating_spread_pct
  )

make_timing_rows <- function(
    data, timing_variant_id, model_rate, model_rating, model_agency,
    source_rule, availability_rule = rep(TRUE, nrow(data))) {
  data |>
    dplyr::transmute(
      issue_row_id, analysis_year, iso3, country,
      historical_income_level, historical_lmic_reporting_scope,
      economic_issue_key, issue_date, maturity_date, currency,
      face_issued_usd, maturity_years,
      anchor_issue_yield_pct = yield_final_preference,
      timing_variant_id = timing_variant_id,
      model_rate_pct = ifelse(availability_rule, model_rate, NA_real_),
      model_rating = ifelse(availability_rule, model_rating, NA_character_),
      model_agency = ifelse(availability_rule, model_agency, NA_character_),
      timing_source_rule = source_rule,
      issue_rating_timing_warning,
      rating_timing_alignment_state,
      model_rate_available = !is.na(.data$model_rate_pct),
      signed_gap_pp = .data$model_rate_pct - .data$anchor_issue_yield_pct,
      period = p15_rating_validation_period(.data$analysis_year),
      timing_decision_state = "diagnostic_not_selected_or_approved"
    )
}

timing_detail <- dplyr::bind_rows(
  make_timing_rows(
    issue_base, "beginning_of_year_selected_precedence",
    issue_base$boy_rate_pct, issue_base$boy_rating,
    issue_base$boy_rating_agency,
    "rating active on January 1"
  ),
  make_timing_rows(
    issue_base, "issue_date_aligned_any_local_rule",
    issue_base$issue_aligned_rate_pct, issue_base$issue_aligned_rating,
    issue_base$issue_selected_agency,
    issue_base$issue_rating_source_rule
  ),
  make_timing_rows(
    issue_base, "issue_date_strict_latest_prior_only",
    issue_base$issue_aligned_rate_pct, issue_base$issue_aligned_rating,
    issue_base$issue_selected_agency,
    issue_base$issue_rating_source_rule,
    issue_base$issue_rating_source_rule ==
      "latest_event_on_or_before_issue_current_rating"
  ),
  make_timing_rows(
    issue_base, "end_year_next_jan1_proxy",
    issue_base$eoy_rate_pct, issue_base$eoy_rating,
    issue_base$eoy_rating_agency,
    "next January 1 rating used as end-year proxy"
  )
)
timing_detail_path <- file.path(
  derived_dir, "p15_rating_timing_issue_validation_detail_2015_2024.csv.gz"
)
data.table::fwrite(
  data.table::as.data.table(timing_detail), timing_detail_path, na = ""
)

timing_samples <- dplyr::bind_rows(
  timing_detail |> dplyr::mutate(sample_cut = "all_standard_primary_issues"),
  timing_detail |>
    dplyr::filter(.data$currency == "USD") |>
    dplyr::mutate(sample_cut = "usd_standard_primary_issues"),
  timing_detail |>
    dplyr::filter(.data$currency == "EUR") |>
    dplyr::mutate(sample_cut = "eur_standard_primary_issues"),
  timing_detail |>
    dplyr::filter(
      .data$currency == "USD", .data$maturity_years >= 5,
      .data$maturity_years <= 10
    ) |>
    dplyr::mutate(sample_cut = "usd_5_10y_standard_primary_issues")
)
timing_summary <- p15_summarise_rating_gaps(
  timing_samples,
  c("sample_cut", "timing_variant_id"),
  weight_col = "face_issued_usd"
)
timing_summary_path <- file.path(
  governance_dir, "p15_rating_timing_issue_validation_summary.csv"
)
data.table::fwrite(
  data.table::as.data.table(timing_summary), timing_summary_path, na = ""
)

# Risk-free curve/tenor/currency source audit. The local LSEG file is labelled as a
# curve history, but its requested field is MID_PRICE and most tenors are near 100.
# It is therefore a mixed quote-object table, not a validated yield curve. Preserve
# and quarantine it rather than using price/index levels as rates.
risk_free_annual <- p15_build_lseg_risk_free_annual(risk_free_history) |>
  dplyr::filter(.data$analysis_year >= 2020L, .data$analysis_year <= 2024L)
risk_free_annual_path <- file.path(
  governance_dir, "p15_rating_risk_free_annual_components_2020_2024.csv"
)
data.table::fwrite(
  data.table::as.data.table(risk_free_annual), risk_free_annual_path, na = ""
)

fred_tenor_annual <- dplyr::bind_rows(
  p15_build_fred_tenor_annual(fred_dgs5, "DGS5", 5),
  p15_build_fred_tenor_annual(fred_dgs7_refresh, "DGS7", 7),
  p15_build_fred_tenor_annual(fred_dgs10, "DGS10", 10)
) |>
  dplyr::filter(.data$analysis_year >= 2012L, .data$analysis_year <= 2024L) |>
  dplyr::arrange(.data$series_id, .data$analysis_year)
fred_tenor_annual_path <- file.path(
  governance_dir, "p15_rating_fred_tenor_annual_components_2012_2024.csv"
)
data.table::fwrite(
  data.table::as.data.table(fred_tenor_annual),
  fred_tenor_annual_path,
  na = ""
)

fred_dgs7_refresh_check <- fred_tenor_annual |>
  dplyr::filter(.data$series_id == "DGS7") |>
  dplyr::left_join(
    ledger |>
      dplyr::distinct(
        .data$analysis_year,
        frozen_p13_dgs7_pct = .data$risk_free_7y_pct
      ),
    by = "analysis_year"
  ) |>
  dplyr::mutate(
    refresh_minus_frozen_pp =
      .data$risk_free_annual_mean_pct - .data$frozen_p13_dgs7_pct,
    exact_within_numeric_tolerance =
      abs(.data$refresh_minus_frozen_pp) <= 1e-12
  )
stopifnot(all(fred_dgs7_refresh_check$exact_within_numeric_tolerance))
fred_dgs7_refresh_check_path <- file.path(
  governance_dir, "p15_rating_fred_dgs7_refresh_parity_2012_2024.csv"
)
data.table::fwrite(
  data.table::as.data.table(fred_dgs7_refresh_check),
  fred_dgs7_refresh_check_path,
  na = ""
)

fred_issue_rows <- issue_base |>
  dplyr::inner_join(
    fred_tenor_annual,
    by = "analysis_year",
    relationship = "many-to-many"
  ) |>
  dplyr::transmute(
    issue_row_id, analysis_year, iso3, country,
    historical_income_level, historical_lmic_reporting_scope,
    issue_date, currency, face_issued_usd, maturity_years,
    anchor_issue_yield_pct = yield_final_preference,
    rating_spread_pct = damodaran_rating_mapped_spread_pct,
    frozen_p13_dgs7_annual_mean_pct = risk_free_7y_pct,
    curve = "fred_usd_treasury",
    series_id, tenor_years,
    ric = .data$series_id,
    risk_free_annual_mean_pct,
    observation_count,
    first_observation_date,
    last_observation_date,
    risk_free_variant_id = paste0(
      "fred_usd_treasury_", .data$tenor_years, "y"
    ),
    model_rate_pct = .data$rating_spread_pct + .data$risk_free_annual_mean_pct,
    signed_gap_pp = .data$model_rate_pct - .data$anchor_issue_yield_pct,
    currency_match = .data$currency == "USD",
    tenor_distance_years = abs(.data$maturity_years - 7),
    tenor_distance_band = dplyr::case_when(
      .data$tenor_distance_years <= 1 ~ "within_1y",
      .data$tenor_distance_years <= 3 ~ "within_3y",
      TRUE ~ "more_than_3y"
    ),
    risk_free_decision_state = dplyr::if_else(
      .data$series_id == "DGS7",
      "inherited_p13_component_exact_refresh_parity",
      "validated_public_tenor_sensitivity_not_selected"
    )
  )
risk_free_issue_detail <- fred_issue_rows
risk_free_issue_detail_path <- file.path(
  derived_dir, "p15_rating_risk_free_issue_validation_detail_2015_2024.csv.gz"
)
data.table::fwrite(
  data.table::as.data.table(risk_free_issue_detail),
  risk_free_issue_detail_path,
  na = ""
)

risk_free_samples <- dplyr::bind_rows(
  risk_free_issue_detail |>
    dplyr::mutate(sample_cut = "all_standard_primary_issues"),
  risk_free_issue_detail |>
    dplyr::filter(.data$currency_match) |>
    dplyr::mutate(sample_cut = "usd_currency_matched"),
  risk_free_issue_detail |>
    dplyr::filter(.data$currency_match, .data$tenor_distance_years <= 3) |>
    dplyr::mutate(sample_cut = "usd_currency_matched_within_3y_tenor")
)
risk_free_summary <- p15_summarise_rating_gaps(
  risk_free_samples,
  c("sample_cut", "risk_free_variant_id", "curve", "tenor_years"),
  weight_col = "face_issued_usd"
)
risk_free_summary_path <- file.path(
  governance_dir, "p15_rating_risk_free_issue_validation_summary.csv"
)
data.table::fwrite(
  data.table::as.data.table(risk_free_summary), risk_free_summary_path, na = ""
)

fred_lseg_7y_comparison <- risk_free_annual |>
  dplyr::filter(.data$curve == "usd_treasury", .data$tenor_years == 7) |>
  dplyr::left_join(
    ledger |>
      dplyr::distinct(.data$analysis_year, .data$risk_free_7y_pct),
    by = "analysis_year"
  ) |>
  dplyr::mutate(
    lseg_minus_fred_7y_pp = NA_real_,
    comparison_state =
      "not_comparable_lseg_mid_price_is_not_validated_as_a_yield_rate",
    p15_use_state = "lseg_series_quarantined_fred_dgs7_retained"
  )
fred_lseg_7y_comparison_path <- file.path(
  governance_dir, "p15_rating_fred_lseg_7y_comparison_2020_2024.csv"
)
data.table::fwrite(
  data.table::as.data.table(fred_lseg_7y_comparison),
  fred_lseg_7y_comparison_path,
  na = ""
)

calibration_register <- prior_calibration |>
  dplyr::mutate(
    p15_evidence_state =
      "predecessor_in_sample_candidate_evidence_not_reused_as_accepted_calibration",
    p15_next_requirement =
      "rebuild after final candidate with past_only_or_grouped_out_of_sample_design",
    p15_decision_state = "not_selected_or_approved"
  )
calibration_register_path <- file.path(
  governance_dir, "p15_rating_prior_calibration_evidence_register.csv"
)
data.table::fwrite(
  data.table::as.data.table(calibration_register),
  calibration_register_path,
  na = ""
)

summary <- tibble::tibble(
  metric = c(
    "country_year_rating_variants",
    "country_year_variant_rows",
    "predecessor_anchor_rows",
    "country_year_variant_anchor_gap_rows",
    "clean_primary_anchor_rows",
    "clean_direct_secondary_anchor_rows",
    "standard_primary_issue_rows_in_timing_test",
    "timing_variants",
    "risk_free_component_years",
    "validated_risk_free_curve_tenor_variants",
    "lseg_mid_price_curve_series_quarantined",
    "p13_base_variant_reproduction_mismatches",
    "rating_candidate_decisions_made",
    "headline_role_decisions_made",
    "rating_variant_schema_version"
  ),
  value = c(
    as.character(dplyr::n_distinct(rating_variants$variant_id)),
    as.character(nrow(rating_variants)),
    as.character(nrow(anchors)),
    as.character(nrow(validation_detail)),
    as.character(sum(anchors$anchor_family == "clean_observed_primary_candidate")),
    as.character(sum(anchors$anchor_family == "clean_direct_secondary_candidate")),
    as.character(dplyr::n_distinct(timing_detail$issue_row_id)),
    as.character(dplyr::n_distinct(timing_detail$timing_variant_id)),
    as.character(dplyr::n_distinct(fred_tenor_annual$analysis_year)),
    as.character(dplyr::n_distinct(risk_free_issue_detail$risk_free_variant_id)),
    as.character(dplyr::n_distinct(paste(
      risk_free_annual$curve, risk_free_annual$tenor_years
    ))),
    "0", "0", "0", p15_rating_variant_schema_version()
  )
)
summary_path <- file.path(
  governance_dir, "p15_rating_variant_validation_build_summary.csv"
)
data.table::fwrite(data.table::as.data.table(summary), summary_path, na = "")

script_path <- file.path(root, "scripts", "p15", "build_p15_rating_variant_validation.R")
helper_paths <- file.path(root, "R", c(
  "p15_rating_components.R", "p15_rating_validation.R"
))
manifest_source_inputs <- unname(input_paths)
manifest_inputs <- c(manifest_source_inputs, helper_paths)
manifest_outputs <- c(
  variant_path, variant_availability_path, validation_detail_path,
  validation_summary_path, timing_detail_path, timing_summary_path,
  risk_free_annual_path, risk_free_issue_detail_path, risk_free_summary_path,
  fred_tenor_annual_path, fred_dgs7_refresh_check_path,
  fred_lseg_7y_comparison_path, calibration_register_path, summary_path
)
manifest <- data.table::rbindlist(lapply(
  c(manifest_inputs, manifest_outputs),
  function(path) {
    data.table::data.table(
      artifact_path = sub(paste0("^", root, "/"), "", path),
      artifact_role = if (path %in% helper_paths) {
        "code_dependency"
      } else if (path %in% manifest_source_inputs) {
        "source_or_predecessor_input"
      } else {
        "generated_output"
      },
      bytes = file.info(path)$size,
      sha256 = digest::digest(file = path, algo = "sha256"),
      producing_script = sub(paste0("^", root, "/"), "", script_path),
      producing_script_sha256 = digest::digest(
        file = script_path, algo = "sha256"
      ),
      build_id = "BUILD-P15-RATING-VARIANT-VALIDATION-20260721-V1",
      schema_version = p15_rating_variant_schema_version(),
      build_date = "2026-07-21"
    )
  }
), use.names = TRUE, fill = TRUE)
manifest_path <- file.path(
  governance_dir, "p15_rating_variant_validation_manifest.csv"
)
data.table::fwrite(manifest, manifest_path, na = "")

cat("P15 rating variant validation: PASS\n")
cat("Country-year variants:", dplyr::n_distinct(rating_variants$variant_id), "\n")
cat("Predecessor clean-anchor rows:", nrow(anchors), "\n")
cat("Timing issue rows:", dplyr::n_distinct(timing_detail$issue_row_id), "\n")
cat("Validated FRED risk-free years:",
    dplyr::n_distinct(fred_tenor_annual$analysis_year), "\n")
cat("Candidate or headline decisions made: 0\n")
