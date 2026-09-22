# Integrated terminal-independent P15 observed-market candidate.
#
# This layer combines already-approved primary and status/sanity rules with the
# supported secondary core. It produces validation-anchor candidates only. It
# does not choose between primary and secondary, select a ladder row, close
# SEC-14/SEC-18, or create a canonical benchmark.

p15_integrated_observed_schema_version <- function() {
  "SCHEMA-P15-INTEGRATED-OBSERVED-CANDIDATE-V1"
}

p15_integrated_observed_build_id <- function() {
  "BUILD-P15-INTEGRATED-OBSERVED-20260817-V1"
}

p15_integrated_observed_method_id <- function() {
  "ADM-P15-INTEGRATED-OBSERVED-TERMINAL-INDEPENDENT-V1"
}

p15_integrated_observed_parity_specification_id <- function() {
  "SPEC-P15-INTEGRATED-OBSERVED-2024-REGRESSION-V1"
}

p15_integrated_date_min_or_na <- function(value) {
  value <- as.Date(value)
  value <- value[!is.na(value)]
  if (!length(value)) as.Date(NA) else min(value)
}

p15_integrated_date_max_or_na <- function(value) {
  value <- as.Date(value)
  value <- value[!is.na(value)]
  if (!length(value)) as.Date(NA) else max(value)
}

p15_build_repair_issue_sanity <- function(repair_issue_audit, treasury_curve) {
  p15_observed_assert_columns(
    repair_issue_audit,
    c(
      "analysis_year", "iso3", "country", "country_year_id", "period",
      "historical_lmic_reporting_scope", "repair_issue_key", "currency",
      "selected_quote_date", "remaining_maturity_years",
      "repaired_yield_pct", "face_outstanding_usd",
      "strict_candidate_admitted", "source_package_ids",
      "source_record_locators"
    ),
    "P15 strict repair issue audit"
  )
  issues <- repair_issue_audit |>
    dplyr::filter(
      .data$strict_candidate_admitted,
      .data$currency %in% c("USD", "EUR"),
      is.finite(.data$repaired_yield_pct),
      is.finite(.data$face_outstanding_usd),
      .data$face_outstanding_usd > 0
    ) |>
    dplyr::transmute(
      evidence_object = paste0(
        "observed_secondary_repair_", tolower(.data$currency), "_2_15"
      ),
      evidence_family = "observed_secondary_price_derived_yield",
      analysis_year = as.integer(.data$analysis_year),
      .data$iso3, .data$country, .data$country_year_id,
      .data$historical_lmic_reporting_scope,
      economic_issue_key = .data$repair_issue_key,
      .data$currency,
      rate_date = as.Date(.data$selected_quote_date),
      maturity_years = as.numeric(.data$remaining_maturity_years),
      rate_pct = as.numeric(.data$repaired_yield_pct),
      weight_usd = as.numeric(.data$face_outstanding_usd),
      preferred_candidate_issue = TRUE,
      all_issue_sensitivity_issue = TRUE,
      source_object = "lseg_secondary_price_derived_yield",
      source_package_ids = as.character(.data$source_package_ids),
      source_record_locators = as.character(.data$source_record_locators),
      timing_object = "year_end_window_candidate_pending_SEC_18"
    )
  treasury <- p15_match_prior_treasury_rate(
    issues$rate_date, issues$maturity_years, treasury_curve
  )
  dplyr::bind_cols(issues, treasury) |>
    dplyr::mutate(
      risk_free_observation_date = dplyr::if_else(
        .data$currency == "USD", .data$risk_free_observation_date,
        as.Date(NA)
      ),
      risk_free_rate_pct = dplyr::if_else(
        .data$currency == "USD", .data$risk_free_rate_pct, NA_real_
      ),
      risk_free_series_id = dplyr::if_else(
        .data$currency == "USD", .data$risk_free_series_id, NA_character_
      ),
      risk_free_lag_days = dplyr::if_else(
        .data$currency == "USD", .data$risk_free_lag_days, NA_integer_
      )
    ) |>
    dplyr::group_by(
      .data$evidence_object, .data$analysis_year, .data$iso3, .data$currency
    ) |>
    dplyr::mutate(
      positive_country_year_median_rate_pct = p15_observed_median_or_na(
        .data$rate_pct[.data$rate_pct > 0]
      ),
      technical_range_flag = .data$rate_pct < -5 | .data$rate_pct > 100,
      ten_times_positive_median_flag =
        is.finite(.data$positive_country_year_median_rate_pct) &
        .data$positive_country_year_median_rate_pct > 0 &
        .data$rate_pct > 10 * .data$positive_country_year_median_rate_pct,
      technical_source_failure_flag =
        .data$technical_range_flag | .data$ten_times_positive_median_flag,
      below_one_percent_context_flag = .data$rate_pct < 1,
      above_thirty_percent_context_flag = .data$rate_pct > 30,
      technical_quarantine_from_preferred_aggregate =
        .data$technical_source_failure_flag,
      quantitative_issue_use_after_sanity =
        !.data$technical_source_failure_flag,
      raw_evidence_retained = TRUE,
      sovereign_spread_pct = dplyr::if_else(
        .data$currency == "USD" & is.finite(.data$risk_free_rate_pct),
        .data$rate_pct - .data$risk_free_rate_pct, NA_real_
      ),
      risk_free_match_available = is.finite(.data$risk_free_rate_pct),
      automatic_distress_classification = FALSE,
      automatic_status_classification = FALSE,
      automatic_pvr_consequence = FALSE,
      sanity_decision_state = "approved_rule_implemented",
      method_id = p15_status_sanity_approved_method_id(),
      schema_version = p15_status_sanity_approved_schema_version(),
      build_id = p15_status_sanity_approved_build_id()
    ) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      rate_review_id = paste(
        "RATE-REVIEW-APPROVED", .data$evidence_object,
        .data$analysis_year, .data$iso3, .data$economic_issue_key,
        sep = "::"
      )
    )
}

p15_integrated_issue_details <- function(issue_sanity) {
  issue_sanity |>
    dplyr::filter(.data$preferred_candidate_issue) |>
    dplyr::group_by(
      .data$evidence_object, .data$evidence_family,
      .data$analysis_year, .data$iso3, .data$country,
      .data$country_year_id, .data$historical_lmic_reporting_scope,
      .data$currency, .data$timing_object
    ) |>
    dplyr::summarise(
      market_maturity_years = p15_observed_weighted_mean(
        .data$maturity_years[.data$quantitative_issue_use_after_sanity],
        .data$weight_usd[.data$quantitative_issue_use_after_sanity]
      ),
      total_weight_usd = sum(
        .data$weight_usd[.data$quantitative_issue_use_after_sanity]
      ),
      largest_issue_weight_share = p15_observed_largest_weight_share(
        .data$weight_usd[.data$quantitative_issue_use_after_sanity]
      ),
      min_retained_issue_rate_pct = p15_observed_min_or_na(
        .data$rate_pct[.data$quantitative_issue_use_after_sanity]
      ),
      max_retained_issue_rate_pct = p15_observed_max_or_na(
        .data$rate_pct[.data$quantitative_issue_use_after_sanity]
      ),
      included_issue_keys = p15_observed_collapse(
        .data$economic_issue_key[.data$quantitative_issue_use_after_sanity]
      ),
      included_source_package_ids = p15_observed_collapse(
        .data$source_package_ids[.data$quantitative_issue_use_after_sanity]
      ),
      first_rate_date = p15_integrated_date_min_or_na(
        .data$rate_date[.data$quantitative_issue_use_after_sanity]
      ),
      last_rate_date = p15_integrated_date_max_or_na(
        .data$rate_date[.data$quantitative_issue_use_after_sanity]
      ),
      .groups = "drop"
    )
}

p15_integrated_evidence_register <- function() {
  tibble::tribble(
    ~evidence_object, ~observed_market_branch, ~candidate_variant_id,
    ~candidate_evidence_tier, ~evidence_priority_within_branch_currency,
    ~terminal_dependency_state,
    "observed_primary_forward_usd", "observed_primary",
    "primary_forward_usd_50m_preferred", "direct_original_issue_yield", 1L,
    "none_for_approved_primary_core",
    "observed_primary_forward_eur", "observed_primary",
    "primary_forward_eur_50m_evidence", "direct_original_issue_yield", 1L,
    "none_for_approved_primary_core",
    "observed_secondary_direct_usd_2_15", "observed_secondary",
    "secondary_direct_usd_2_15_year_end", "direct_yield_to_maturity", 1L,
    "year_end_core_available_full_year_history_parked_TODO_038",
    "observed_secondary_direct_eur_2_15", "observed_secondary",
    "secondary_direct_eur_2_15_year_end", "direct_yield_to_maturity", 1L,
    "year_end_core_available_full_year_history_parked_TODO_038",
    "observed_secondary_repair_usd_2_15", "observed_secondary",
    "secondary_repair_usd_2_15_year_end", "price_derived_secondary", 3L,
    "candidate_core_available_final_SEC_18_and_full_year_TODO_038_pending",
    "observed_secondary_repair_eur_2_15", "observed_secondary",
    "secondary_repair_eur_2_15_year_end", "price_derived_secondary", 3L,
    "candidate_core_available_final_SEC_18_and_full_year_TODO_038_pending"
  )
}

p15_build_targeted_secondary_companion_evidence <- function(
    standard_direct, terminal_direct, price_derived,
    common_issue_catalogue, country_year_grid, status_contract) {
  p15_observed_assert_columns(
    standard_direct,
    c(
      "analysis_year", "iso3", "country", "market_rate_pct",
      "market_maturity_years", "issue_count", "total_weight_usd",
      "included_issue_keys", "currency_basis", "selected_source_class",
      "source_package_id"
    ),
    "P15 targeted standard-direct country evidence"
  )
  p15_observed_assert_columns(
    common_issue_catalogue,
    c(
      "analysis_year", "iso3", "currency", "nonstandard_feature_flag",
      "candidate_secondary_standard", "strict_candidate_admitted",
      "direct_yield_present", "identifiers_with_price"
    ),
    "P15 common secondary issue catalogue"
  )
  metadata <- country_year_grid |>
    dplyr::filter(.data$analysis_year == 2024L) |>
    dplyr::transmute(
      analysis_year, iso3,
      historical_lmic_reporting_scope,
      historical_income_level
    ) |>
    dplyr::distinct(.data$analysis_year, .data$iso3, .keep_all = TRUE)
  diagnostics <- common_issue_catalogue |>
    dplyr::filter(.data$analysis_year == 2024L) |>
    dplyr::group_by(.data$analysis_year, .data$iso3, .data$currency) |>
    dplyr::summarise(
      common_issue_present = TRUE,
      common_direct_candidate_present = any(
        .data$candidate_secondary_standard
      ),
      common_repair_candidate_present = any(.data$strict_candidate_admitted),
      common_feature_evidence_present = any(
        .data$nonstandard_feature_flag &
          (.data$direct_yield_present | .data$identifiers_with_price > 0)
      ),
      .groups = "drop"
    )
  direct_columns <- function(x) x |>
    dplyr::transmute(
      analysis_year = as.integer(.data$analysis_year), .data$iso3,
      .data$country, market_rate_pct = as.numeric(.data$market_rate_pct),
      market_maturity_years = as.numeric(.data$market_maturity_years),
      retained_quantitative_issue_count = as.integer(.data$issue_count),
      total_weight_usd = as.numeric(.data$total_weight_usd),
      included_issue_keys = as.character(.data$included_issue_keys),
      currency = as.character(.data$currency_basis),
      source_class = as.character(.data$selected_source_class),
      source_package_ids = as.character(.data$source_package_id),
      candidate_evidence_tier = "targeted_direct_yield_to_maturity",
      evidence_priority_within_branch_currency = 2L,
      targeted_measure_class = "direct"
    )
  direct <- dplyr::bind_rows(
    direct_columns(standard_direct), direct_columns(terminal_direct)
  )
  repair <- price_derived |>
    dplyr::transmute(
      analysis_year = as.integer(.data$analysis_year), .data$iso3,
      .data$country, market_rate_pct = as.numeric(.data$market_rate_pct),
      market_maturity_years = as.numeric(.data$market_maturity_years),
      retained_quantitative_issue_count = as.integer(.data$issue_count),
      total_weight_usd = as.numeric(.data$total_weight_usd),
      included_issue_keys = as.character(.data$included_issue_keys),
      currency = as.character(.data$currency_basis),
      source_class = as.character(.data$selected_source_class),
      source_package_ids = as.character(.data$source_package_id),
      candidate_evidence_tier = "targeted_price_derived_secondary",
      evidence_priority_within_branch_currency = 4L,
      targeted_measure_class = "repair"
    )
  status <- status_contract |>
    dplyr::select(
      "analysis_year", "iso3", "approved_status_rule_class",
      "observed_rate_use_state", "observed_benchmark_selection_permitted",
      "raw_evidence_retained"
    )
  dplyr::bind_rows(direct, repair) |>
    dplyr::left_join(
      diagnostics, by = c("analysis_year", "iso3", "currency")
    ) |>
    dplyr::mutate(
      dplyr::across(
        c(
          "common_issue_present", "common_direct_candidate_present",
          "common_repair_candidate_present",
          "common_feature_evidence_present"
        ),
        ~ dplyr::coalesce(.x, FALSE)
      ),
      targeted_source_gap_eligible = dplyr::case_when(
        .data$targeted_measure_class == "direct" ~
          !.data$common_direct_candidate_present &
          !.data$common_feature_evidence_present,
        TRUE ~ !.data$common_direct_candidate_present &
          !.data$common_repair_candidate_present &
          !.data$common_feature_evidence_present
      )
    ) |>
    dplyr::filter(.data$targeted_source_gap_eligible) |>
    dplyr::left_join(metadata, by = c("analysis_year", "iso3")) |>
    dplyr::left_join(status, by = c("analysis_year", "iso3")) |>
    dplyr::mutate(
      country_year_id = paste(.data$analysis_year, .data$iso3, sep = "::"),
      period = as.character(.data$analysis_year),
      evidence_object = paste0(
        "observed_secondary_targeted_", .data$targeted_measure_class, "_",
        tolower(.data$currency), "_2_15"
      ),
      evidence_family = dplyr::if_else(
        .data$targeted_measure_class == "direct",
        "observed_secondary_targeted_direct_yield",
        "observed_secondary_targeted_price_derived_yield"
      ),
      observed_market_branch = "observed_secondary",
      candidate_variant_id = paste0(
        "secondary_targeted_", .data$targeted_measure_class, "_",
        tolower(.data$currency), "_2_15_2024_source_gap"
      ),
      raw_market_rate_pct = .data$market_rate_pct,
      market_rate_pct_after_technical_quarantine = dplyr::if_else(
        .data$market_rate_pct >= -5 & .data$market_rate_pct <= 100,
        .data$market_rate_pct, NA_real_
      ),
      technical_quarantine_issue_count = dplyr::if_else(
        is.finite(.data$market_rate_pct_after_technical_quarantine), 0L,
        .data$retained_quantitative_issue_count
      ),
      quantitative_rate_available_after_sanity = is.finite(
        .data$market_rate_pct_after_technical_quarantine
      ),
      low_rate_context_present = .data$market_rate_pct < 1,
      high_rate_context_present = .data$market_rate_pct > 30,
      approved_status_rule_class = dplyr::coalesce(
        .data$approved_status_rule_class, "no_registered_status_case"
      ),
      observed_rate_use_state = dplyr::coalesce(
        .data$observed_rate_use_state,
        "permitted_no_registered_status_case"
      ),
      observed_benchmark_selection_permitted = dplyr::coalesce(
        .data$observed_benchmark_selection_permitted, TRUE
      ),
      raw_evidence_retained = dplyr::coalesce(
        .data$raw_evidence_retained, TRUE
      ),
      approved_ordinary_benchmark_candidate =
        .data$quantitative_rate_available_after_sanity &
        .data$observed_benchmark_selection_permitted,
      candidate_for_validation_anchor =
        .data$approved_ordinary_benchmark_candidate,
      candidate_use_state = dplyr::if_else(
        .data$candidate_for_validation_anchor,
        "ordinary_validation_anchor_candidate",
        "retained_context_not_ordinary_validation_anchor"
      ),
      imf_inspired_500bps_plus_doubling_context_flag = FALSE,
      exact_imf_operational_replication = FALSE,
      interpretation =
        "not_testable_from_one_year_targeted_companion_alone",
      raw_issue_count = .data$retained_quantitative_issue_count,
      largest_issue_weight_share = dplyr::if_else(
        .data$retained_quantitative_issue_count == 1L, 1, NA_real_
      ),
      thin_evidence = .data$retained_quantitative_issue_count == 1L,
      high_concentration = dplyr::if_else(
        is.finite(.data$largest_issue_weight_share),
        .data$largest_issue_weight_share >= 0.8, NA
      ),
      evidence_strength_label = dplyr::if_else(
        .data$thin_evidence, "thin_single_issue_observed_evidence",
        "multiple_issue_targeted_companion_evidence"
      ),
      timing_object = "year_end_window_candidate_pending_SEC_18",
      terminal_dependency_state =
        "preserved_2024_companion_source_gap_fill_final_SEC_18_pending",
      source_evidence_row_id = paste(
        "P15-INTEGRATED-TARGETED", .data$evidence_object,
        .data$analysis_year, .data$iso3, sep = "::"
      ),
      terminal_independent_candidate = TRUE,
      selected_for_ladder = FALSE,
      canonical_benchmark = FALSE,
      method_id = p15_integrated_observed_method_id(),
      schema_version = p15_integrated_observed_schema_version(),
      build_id = p15_integrated_observed_build_id()
    ) |>
    dplyr::arrange(
      .data$analysis_year, .data$iso3,
      .data$evidence_priority_within_branch_currency
    )
}

p15_normalize_integrated_observed_evidence <- function(
    issue_sanity, country_actions) {
  p15_observed_assert_columns(
    country_actions,
    c(
      "evidence_object", "analysis_year", "iso3",
      "raw_market_rate_pct", "market_rate_pct_after_technical_quarantine",
      "raw_issue_count", "technical_quarantine_issue_count",
      "retained_quantitative_issue_count", "low_rate_context_present",
      "high_rate_context_present", "approved_status_rule_class",
      "observed_rate_use_state", "observed_benchmark_selection_permitted",
      "imf_inspired_500bps_plus_doubling_context_flag",
      "quantitative_rate_available_after_sanity",
      "approved_ordinary_benchmark_candidate", "raw_evidence_retained"
    ),
    "P15 approved status/sanity country actions"
  )
  register <- p15_integrated_evidence_register()
  details <- p15_integrated_issue_details(issue_sanity)
  out <- country_actions |>
    dplyr::inner_join(register, by = "evidence_object") |>
    dplyr::left_join(
      details,
      by = c(
        "evidence_object", "evidence_family", "analysis_year", "iso3",
        "country", "country_year_id", "historical_lmic_reporting_scope",
        "currency", "timing_object"
      )
    ) |>
    dplyr::mutate(
      period = as.character(.data$analysis_year),
      market_rate_pct =
        .data$market_rate_pct_after_technical_quarantine,
      thin_evidence = .data$retained_quantitative_issue_count == 1L,
      high_concentration = .data$largest_issue_weight_share >= 0.8,
      evidence_strength_label = dplyr::case_when(
        !.data$quantitative_rate_available_after_sanity ~
          "no_quantitative_rate_after_sanity",
        .data$thin_evidence ~ "thin_single_issue_observed_evidence",
        .data$high_concentration ~ "multiple_issue_high_concentration",
        TRUE ~ "multiple_issue_diversified_evidence"
      ),
      candidate_for_validation_anchor =
        .data$approved_ordinary_benchmark_candidate,
      candidate_use_state = dplyr::case_when(
        !.data$quantitative_rate_available_after_sanity ~
          "unavailable_after_technical_sanity",
        !.data$observed_benchmark_selection_permitted ~
          "retained_context_not_ordinary_validation_anchor",
        TRUE ~ "ordinary_validation_anchor_candidate"
      ),
      source_evidence_row_id = paste(
        "P15-INTEGRATED-OBSERVED", .data$evidence_object,
        .data$analysis_year, .data$iso3, sep = "::"
      ),
      terminal_independent_candidate = TRUE,
      selected_for_ladder = FALSE,
      canonical_benchmark = FALSE,
      method_id = p15_integrated_observed_method_id(),
      schema_version = p15_integrated_observed_schema_version(),
      build_id = p15_integrated_observed_build_id()
    ) |>
    dplyr::arrange(
      .data$analysis_year, .data$iso3, .data$observed_market_branch,
      .data$currency, .data$evidence_priority_within_branch_currency
    )
  if (any(!out$raw_evidence_retained)) {
    stop("Integrated evidence cannot violate raw-retention rules.", call. = FALSE)
  }
  if (anyDuplicated(out[c("evidence_object", "analysis_year", "iso3")])) {
    stop("Integrated evidence is not unique by evidence object/country-year.",
         call. = FALSE)
  }
  out
}

p15_resolve_terminal_independent_validation_anchors <- function(evidence) {
  primary <- evidence |>
    dplyr::filter(
      .data$observed_market_branch == "observed_primary",
      .data$candidate_for_validation_anchor
    ) |>
    dplyr::mutate(
      within_branch_resolution =
        "approved_primary_direct_issue_yield_candidate"
    )
  secondary <- evidence |>
    dplyr::filter(
      .data$observed_market_branch == "observed_secondary",
      .data$candidate_for_validation_anchor
    ) |>
    dplyr::arrange(
      .data$analysis_year, .data$iso3, .data$currency,
      .data$evidence_priority_within_branch_currency,
      .data$candidate_variant_id
    ) |>
    dplyr::group_by(.data$analysis_year, .data$iso3, .data$currency) |>
    dplyr::slice_head(n = 1L) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      within_branch_resolution = dplyr::case_when(
        .data$candidate_evidence_tier == "direct_yield_to_maturity" ~
          "common_direct_secondary_yield_available",
        .data$candidate_evidence_tier ==
          "targeted_direct_yield_to_maturity" ~
          "targeted_2024_direct_companion_fills_common_source_gap",
        .data$candidate_evidence_tier == "price_derived_secondary" ~
          "strict_common_price_derived_fallback_no_direct_yield",
        TRUE ~
          "targeted_2024_price_derived_companion_fills_common_source_gap"
      )
    )
  anchors <- dplyr::bind_rows(primary, secondary) |>
    dplyr::mutate(
      validation_anchor_state =
        "terminal_independent_candidate_not_final_ladder_selection",
      selected_for_ladder = FALSE,
      canonical_benchmark = FALSE
    ) |>
    dplyr::arrange(
      .data$analysis_year, .data$iso3, .data$observed_market_branch,
      .data$currency
    )
  keys <- c(
    "analysis_year", "iso3", "observed_market_branch", "currency"
  )
  if (anyDuplicated(anchors[keys])) {
    stop("Validation anchors are not unique by branch/currency country-year.",
         call. = FALSE)
  }
  direct_keys <- evidence |>
    dplyr::filter(
      .data$observed_market_branch == "observed_secondary",
      grepl("direct_yield_to_maturity", .data$candidate_evidence_tier),
      .data$candidate_for_validation_anchor
    ) |>
    dplyr::transmute(
      key = paste(.data$analysis_year, .data$iso3, .data$currency, sep = "::")
    ) |>
    dplyr::pull(.data$key)
  repair_selected_keys <- anchors |>
    dplyr::filter(.data$candidate_evidence_tier == "price_derived_secondary") |>
    dplyr::transmute(
      key = paste(.data$analysis_year, .data$iso3, .data$currency, sep = "::")
    ) |>
    dplyr::pull(.data$key)
  if (length(intersect(direct_keys, repair_selected_keys))) {
    stop("A repair fallback outranked an available direct secondary yield.",
         call. = FALSE)
  }
  anchors
}

p15_build_integrated_2024_regression <- function(anchors, p13_parity) {
  p15_observed_assert_columns(
    p13_parity,
    c(
      "analysis_year", "iso3", "country", "observed_market_branch",
      "p13_rate_pct", "p13_maturity_years", "p13_source_class",
      "p13_tier_id", "p13_currency_basis", "p13_mixed_currency_basis",
      "common_feature_evidence_present",
      "observed_market_parity_gate_pass"
    ),
    "P13 2024 observed-market parity"
  )
  forward <- anchors |>
    dplyr::filter(.data$analysis_year == 2024L) |>
    dplyr::select(
      "analysis_year", "iso3", "observed_market_branch", "currency",
      forward_rate_pct = "market_rate_pct",
      forward_maturity_years = "market_maturity_years",
      forward_evidence_tier = "candidate_evidence_tier",
      forward_candidate_variant_id = "candidate_variant_id",
      "within_branch_resolution", "approved_status_rule_class",
      "candidate_use_state", "source_evidence_row_id"
    )
  if (anyDuplicated(forward[c(
    "analysis_year", "iso3", "observed_market_branch", "currency"
  )])) {
    stop("Forward 2024 USD anchors are not unique by observed branch.",
         call. = FALSE)
  }
  out <- p13_parity |>
    dplyr::filter(.data$analysis_year == 2024L) |>
    dplyr::mutate(
      regression_currency_basis = dplyr::if_else(
        .data$observed_market_branch == "observed_primary", "USD",
        dplyr::if_else(
          grepl("USD", .data$p13_currency_basis), "USD", "EUR"
        )
      )
    ) |>
    dplyr::left_join(
      forward,
      by = c(
        "analysis_year", "iso3", "observed_market_branch",
        "regression_currency_basis" = "currency"
      )
    ) |>
    dplyr::mutate(
      legacy_p13_parity_still_passes =
        .data$observed_market_parity_gate_pass,
      forward_candidate_present = is.finite(.data$forward_rate_pct),
      forward_rate_abs_diff_pp = abs(
        .data$forward_rate_pct - .data$p13_rate_pct
      ),
      forward_maturity_abs_diff_years = abs(
        .data$forward_maturity_years - .data$p13_maturity_years
      ),
      exact_forward_rate_match = .data$forward_candidate_present &
        .data$forward_rate_abs_diff_pp <= 1e-8,
      exact_forward_maturity_match = .data$forward_candidate_present &
        .data$forward_maturity_abs_diff_years <= 1e-8,
      forward_regression_class = dplyr::case_when(
        .data$exact_forward_rate_match & .data$exact_forward_maturity_match ~
          "exact_forward_rate_and_maturity_match",
        !.data$forward_candidate_present &
          (grepl("feature_rich", .data$p13_source_class) |
             .data$common_feature_evidence_present) ~
          "forward_feature_rich_secondary_pending_TODO_036",
        !.data$forward_candidate_present &
          .data$observed_market_branch == "observed_secondary" ~
          "forward_secondary_unavailable_in_terminal_independent_core",
        !.data$forward_candidate_present ~
          "forward_primary_unavailable_under_approved_rule",
        .data$observed_market_branch == "observed_primary" &
          .data$p13_mixed_currency_basis ~
          "approved_primary_currency_separation_from_p13_mixed_nominal_pool",
        .data$observed_market_branch == "observed_primary" ~
          "approved_primary_reported_yield_only_or_issue_scope_difference",
        .data$p13_mixed_currency_basis ~
          "approved_secondary_currency_separation_from_p13_mixed_nominal_pool",
        TRUE ~ "forward_secondary_quote_vintage_or_issue_scope_difference"
      ),
      difference_requires_explanation =
        .data$forward_regression_class !=
        "exact_forward_rate_and_maturity_match",
      parity_specification_id =
        p15_integrated_observed_parity_specification_id(),
      schema_version = p15_integrated_observed_schema_version(),
      build_id = p15_integrated_observed_build_id()
    ) |>
    dplyr::arrange(.data$observed_market_branch, .data$iso3)
  if (!all(out$legacy_p13_parity_still_passes)) {
    stop("The immutable P13 2024 parity branch no longer passes.", call. = FALSE)
  }
  out
}

p15_summarise_integrated_observed <- function(evidence, anchors, regression) {
  tibble::tibble(
    metric = c(
      "integrated_evidence_rows",
      "validation_anchor_rows",
      "distinct_validation_anchor_country_years",
      "lmic_validation_anchor_rows",
      "primary_validation_anchor_rows",
      "secondary_direct_validation_anchor_rows",
      "secondary_repair_fallback_anchor_rows",
      "status_or_sanity_blocked_evidence_rows",
      "technical_quarantine_issue_rows",
      "p13_2024_regression_rows",
      "p13_2024_legacy_parity_pass_rows",
      "p13_2024_forward_candidate_present_rows",
      "p13_2024_exact_forward_rate_and_maturity_rows",
      "p13_2024_forward_difference_or_unavailable_rows",
      "selected_ladder_rows"
    ),
    value = c(
      nrow(evidence),
      nrow(anchors),
      length(unique(paste(anchors$analysis_year, anchors$iso3, sep = "::"))),
      sum(anchors$historical_lmic_reporting_scope %in% TRUE),
      sum(anchors$observed_market_branch == "observed_primary"),
      sum(grepl("direct_yield_to_maturity", anchors$candidate_evidence_tier)),
      sum(grepl("price_derived_secondary", anchors$candidate_evidence_tier)),
      sum(!evidence$candidate_for_validation_anchor),
      sum(evidence$technical_quarantine_issue_count),
      nrow(regression),
      sum(regression$legacy_p13_parity_still_passes),
      sum(regression$forward_candidate_present),
      sum(
        regression$forward_regression_class ==
          "exact_forward_rate_and_maturity_match"
      ),
      sum(regression$difference_requires_explanation),
      sum(anchors$selected_for_ladder)
    ),
    method_id = p15_integrated_observed_method_id(),
    schema_version = p15_integrated_observed_schema_version(),
    build_id = p15_integrated_observed_build_id()
  )
}
