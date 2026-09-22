# Approved P15 status and rate-sanity rule contract.
#
# Raw observations are always retained. Technical source failures are quarantined
# only from preferred quantitative aggregates. Low, high, and IMF-inspired market-
# spread signals remain contextual. Status rules distinguish ordinary benchmark use
# from retained crisis/status evidence.

p15_status_sanity_approved_schema_version <- function() {
  "SCHEMA-P15-STATUS-SANITY-APPROVED-V1"
}

p15_status_sanity_approved_build_id <- function() {
  "BUILD-P15-STATUS-SANITY-APPROVED-20260815-V1"
}

p15_status_sanity_approved_method_id <- function() {
  "ADM-P15-STATUS-SANITY-FORWARD-V1"
}

p15_build_approved_sanity_issue_universe <- function(
    primary_disposition, secondary_issues, treasury_curve) {
  primary <- primary_disposition |>
    dplyr::filter(.data$approved_primary_structural_issue) |>
    dplyr::transmute(
      evidence_object = paste0("observed_primary_forward_", tolower(.data$currency)),
      evidence_family = "observed_primary_issuance",
      analysis_year = as.integer(.data$analysis_year),
      .data$iso3, .data$country, .data$country_year_id,
      .data$historical_lmic_reporting_scope,
      .data$economic_issue_key, .data$currency,
      rate_date = as.Date(.data$issue_date),
      maturity_years = as.numeric(.data$original_maturity_years),
      rate_pct = as.numeric(.data$original_issue_yield_pct),
      weight_usd = as.numeric(.data$approved_weight_usd),
      preferred_candidate_issue =
        .data$approved_primary_preferred_issue_before_sanity,
      all_issue_sensitivity_issue =
        .data$approved_primary_all_issue_sensitivity_before_sanity,
      source_object = as.character(.data$source_object),
      source_package_ids = as.character(.data$source_package_ids),
      source_record_locators = as.character(.data$source_record_locators),
      timing_object = "primary_issue_date"
    )
  secondary <- secondary_issues |>
    dplyr::filter(
      .data$candidate_secondary_standard,
      .data$residual_2_15,
      .data$currency %in% c("USD", "EUR"),
      is.finite(.data$direct_yield_pct),
      is.finite(.data$face_outstanding_usd),
      .data$face_outstanding_usd > 0
    ) |>
    dplyr::transmute(
      evidence_object = paste0("observed_secondary_direct_", tolower(.data$currency),
                               "_2_15"),
      evidence_family = "observed_secondary_direct_yield",
      analysis_year = as.integer(.data$analysis_year),
      .data$iso3, .data$country, .data$country_year_id,
      .data$historical_lmic_reporting_scope,
      .data$economic_issue_key, .data$currency,
      rate_date = as.Date(.data$direct_quote_date),
      maturity_years = as.numeric(.data$remaining_maturity_years),
      rate_pct = as.numeric(.data$direct_yield_pct),
      weight_usd = as.numeric(.data$face_outstanding_usd),
      preferred_candidate_issue = TRUE,
      all_issue_sensitivity_issue = TRUE,
      source_object = "lseg_secondary_direct_yield_to_maturity",
      source_package_ids = as.character(.data$direct_source_package_ids),
      source_record_locators = as.character(
        .data$direct_source_record_locators
      ),
      timing_object = "year_end_window_candidate_pending_SEC_18"
    )
  issues <- dplyr::bind_rows(primary, secondary) |>
    dplyr::filter(
      is.finite(.data$rate_pct), is.finite(.data$weight_usd),
      .data$weight_usd > 0
    )
  treasury <- p15_match_prior_treasury_rate(
    issues$rate_date, issues$maturity_years, treasury_curve
  )
  out <- dplyr::bind_cols(issues, treasury) |>
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
        .data$analysis_year, .data$iso3, .data$economic_issue_key, sep = "::"
      )
    )
  if (any(!out$raw_evidence_retained)) {
    stop("Approved sanity rules may not delete raw evidence.", call. = FALSE)
  }
  out
}

p15_build_imf_inspired_context_flags <- function(issue_sanity) {
  issue_sanity |>
    dplyr::filter(
      .data$currency == "USD",
      .data$preferred_candidate_issue,
      .data$quantitative_issue_use_after_sanity,
      .data$risk_free_match_available
    ) |>
    dplyr::group_by(
      .data$evidence_object, .data$evidence_family,
      .data$analysis_year, .data$iso3, .data$country,
      .data$country_year_id, .data$historical_lmic_reporting_scope
    ) |>
    dplyr::summarise(
      market_rate_pct = stats::weighted.mean(.data$rate_pct, .data$weight_usd),
      market_maturity_years = stats::weighted.mean(
        .data$maturity_years, .data$weight_usd
      ),
      risk_free_rate_pct = stats::weighted.mean(
        .data$risk_free_rate_pct, .data$weight_usd
      ),
      sovereign_spread_pct = stats::weighted.mean(
        .data$sovereign_spread_pct, .data$weight_usd
      ),
      issue_count = dplyr::n(),
      reference_series_ids = p15_observed_collapse(
        .data$risk_free_series_id
      ),
      .groups = "drop"
    ) |>
    dplyr::arrange(.data$evidence_object, .data$iso3, .data$analysis_year) |>
    dplyr::group_by(.data$evidence_object, .data$iso3) |>
    dplyr::mutate(
      prior_analysis_year = dplyr::lag(.data$analysis_year),
      prior_sovereign_spread_pct = dplyr::lag(.data$sovereign_spread_pct),
      prior_market_maturity_years = dplyr::lag(.data$market_maturity_years),
      consecutive_prior_year_available =
        .data$prior_analysis_year == .data$analysis_year - 1L,
      maturity_comparable_to_prior =
        abs(.data$market_maturity_years -
              .data$prior_market_maturity_years) <= 3,
      comparable_prior_year_available =
        .data$consecutive_prior_year_available &
        .data$maturity_comparable_to_prior &
        is.finite(.data$prior_sovereign_spread_pct) &
        .data$prior_sovereign_spread_pct > 0,
      spread_at_least_500bps = .data$sovereign_spread_pct >= 5,
      spread_at_least_double_prior =
        .data$comparable_prior_year_available &
        .data$sovereign_spread_pct >= 2 * .data$prior_sovereign_spread_pct,
      imf_inspired_500bps_plus_doubling_context_flag =
        .data$spread_at_least_500bps &
        .data$spread_at_least_double_prior,
      contextual_flag_only = TRUE,
      exact_imf_operational_replication = FALSE,
      imf_operational_indicator = "EMBIG spread",
      project_indicator = paste(
        "issue-weighted observed USD bond yield minus nearest available",
        "5/7/10-year US Treasury rate"
      ),
      interpretation = dplyr::case_when(
        .data$imf_inspired_500bps_plus_doubling_context_flag ~
          "contextual_review_only_not_distress_verdict_or_quantitative_block",
        !.data$comparable_prior_year_available ~
          "not_testable_without_comparable_positive_prior_year_spread",
        TRUE ~ "condition_not_met"
      ),
      automatic_consequence = "none",
      source_method_url = paste0(
        "https://www.imf.org/-/media/files/publications/pp/2022/english/",
        "ppea2022039.pdf"
      )
    ) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      method_id = p15_status_sanity_approved_method_id(),
      schema_version = p15_status_sanity_approved_schema_version(),
      build_id = p15_status_sanity_approved_build_id()
    )
}

p15_build_approved_status_rule_contract <- function(status_recommendations) {
  required <- c("analysis_year", "iso3", "recommended_treatment_class", "case_level_note")
  if (!all(required %in% names(status_recommendations))) {
    stop("Status recommendations lack required case fields.", call. = FALSE)
  }
  for (field in required) {
    value <- status_recommendations[[field]]
    if (any(is.na(value) | !nzchar(trimws(as.character(value))))) {
      stop("Status recommendations contain missing case field: ", field, call. = FALSE)
    }
  }
  if (anyDuplicated(status_recommendations[c("analysis_year", "iso3")])) {
    stop("Duplicate country-year status judgments require explicit reconciliation.", call. = FALSE)
  }
  allowed <- c(
    "provisional_context_warning_no_automatic_block_pending_STAT_05",
    "provisional_manual_review_before_observed_use_pending_STAT_05",
    "provisional_block_ordinary_fallback_pending_STAT_05"
  )
  if (any(!status_recommendations$recommended_treatment_class %in% allowed)) {
    stop("Unknown status treatment must not default to permission.", call. = FALSE)
  }
  status_recommendations |>
    dplyr::mutate(
      approved_status_rule_class = dplyr::case_when(
        grepl("provisional_block_ordinary_fallback",
              .data$recommended_treatment_class) ~
          "block_ordinary_fallback_retain_status_evidence",
        grepl("provisional_manual_review",
              .data$recommended_treatment_class) ~
          "manual_review_before_observed_benchmark_use",
        TRUE ~ "context_warning_no_automatic_block"
      ),
      observed_rate_use_state = dplyr::case_when(
        .data$approved_status_rule_class ==
          "block_ordinary_fallback_retain_status_evidence" ~
          "retained_as_status_evidence_not_ordinary_benchmark",
        .data$approved_status_rule_class ==
          "manual_review_before_observed_benchmark_use" ~
          "manual_review_required_before_observed_benchmark_use",
        TRUE ~ "permitted_with_status_context"
      ),
      ordinary_fallback_use_state = dplyr::case_when(
        .data$approved_status_rule_class ==
          "block_ordinary_fallback_retain_status_evidence" ~ "blocked",
        .data$approved_status_rule_class ==
          "manual_review_before_observed_benchmark_use" ~
          "manual_review_required",
        TRUE ~ "not_blocked_by_status_context_alone"
      ),
      observed_benchmark_selection_permitted =
        .data$approved_status_rule_class ==
        "context_warning_no_automatic_block",
      ordinary_fallback_selection_permitted =
        .data$approved_status_rule_class ==
        "context_warning_no_automatic_block",
      raw_evidence_retained = TRUE,
      display_warning_required = TRUE,
      automatic_deletion = FALSE,
      status_decision_state = "approved_rule_implemented",
      method_id = p15_status_sanity_approved_method_id(),
      schema_version = p15_status_sanity_approved_schema_version(),
      build_id = p15_status_sanity_approved_build_id()
    )
}

p15_integrate_approved_status_sanity_actions <- function(
    issue_sanity, status_contract, imf_context) {
  preferred <- issue_sanity |>
    dplyr::filter(.data$preferred_candidate_issue) |>
    dplyr::group_by(
      .data$evidence_object, .data$evidence_family,
      .data$analysis_year, .data$iso3, .data$country,
      .data$country_year_id, .data$historical_lmic_reporting_scope,
      .data$currency, .data$timing_object
    ) |>
    dplyr::summarise(
      raw_issue_count = dplyr::n(),
      technical_quarantine_issue_count = sum(
        .data$technical_quarantine_from_preferred_aggregate
      ),
      retained_quantitative_issue_count = sum(
        .data$quantitative_issue_use_after_sanity
      ),
      raw_market_rate_pct = p15_observed_weighted_mean(
        .data$rate_pct, .data$weight_usd
      ),
      market_rate_pct_after_technical_quarantine = p15_observed_weighted_mean(
        .data$rate_pct[.data$quantitative_issue_use_after_sanity],
        .data$weight_usd[.data$quantitative_issue_use_after_sanity]
      ),
      low_rate_context_present = any(.data$below_one_percent_context_flag),
      high_rate_context_present = any(.data$above_thirty_percent_context_flag),
      retained_issue_keys = p15_observed_collapse(.data$economic_issue_key),
      quarantined_issue_keys = p15_observed_collapse(
        .data$economic_issue_key[
          .data$technical_quarantine_from_preferred_aggregate
        ]
      ),
      source_package_ids = p15_observed_collapse(.data$source_package_ids),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      technical_quarantine_changed_rate =
        .data$technical_quarantine_issue_count > 0 &
        abs(.data$market_rate_pct_after_technical_quarantine -
              .data$raw_market_rate_pct) > 1e-12,
      raw_evidence_retained = TRUE
    )

  status <- status_contract |>
    dplyr::select(
      "analysis_year", "iso3", "approved_status_rule_class",
      "observed_rate_use_state", "ordinary_fallback_use_state",
      "observed_benchmark_selection_permitted",
      "ordinary_fallback_selection_permitted",
      "recommended_display_action", "case_level_note"
    )
  imf <- imf_context |>
    dplyr::select(
      "evidence_object", "analysis_year", "iso3",
      "sovereign_spread_pct", "prior_sovereign_spread_pct",
      "imf_inspired_500bps_plus_doubling_context_flag",
      "exact_imf_operational_replication", "interpretation"
    )

  preferred |>
    dplyr::left_join(status, by = c("analysis_year", "iso3")) |>
    dplyr::left_join(
      imf, by = c("evidence_object", "analysis_year", "iso3")
    ) |>
    dplyr::mutate(
      approved_status_rule_class = dplyr::coalesce(
        .data$approved_status_rule_class, "no_registered_status_case"
      ),
      observed_rate_use_state = dplyr::coalesce(
        .data$observed_rate_use_state, "permitted_no_registered_status_case"
      ),
      ordinary_fallback_use_state = dplyr::coalesce(
        .data$ordinary_fallback_use_state,
        "not_blocked_no_registered_status_case"
      ),
      observed_benchmark_selection_permitted = dplyr::coalesce(
        .data$observed_benchmark_selection_permitted, TRUE
      ),
      ordinary_fallback_selection_permitted = dplyr::coalesce(
        .data$ordinary_fallback_selection_permitted, TRUE
      ),
      imf_inspired_500bps_plus_doubling_context_flag = dplyr::coalesce(
        .data$imf_inspired_500bps_plus_doubling_context_flag, FALSE
      ),
      exact_imf_operational_replication = dplyr::coalesce(
        .data$exact_imf_operational_replication, FALSE
      ),
      quantitative_rate_available_after_sanity = is.finite(
        .data$market_rate_pct_after_technical_quarantine
      ),
      approved_ordinary_benchmark_candidate =
        .data$quantitative_rate_available_after_sanity &
        .data$observed_benchmark_selection_permitted,
      contextual_flags_do_not_change_rate = TRUE,
      selected_for_ladder = FALSE,
      automatic_pvr_consequence = FALSE,
      decision_state =
        "approved_rule_implemented_pending_ladder_propagation",
      method_id = p15_status_sanity_approved_method_id(),
      schema_version = p15_status_sanity_approved_schema_version(),
      build_id = p15_status_sanity_approved_build_id()
    ) |>
    dplyr::arrange(.data$analysis_year, .data$iso3, .data$evidence_object)
}

p15_summarise_approved_status_sanity <- function(
    issue_sanity, status_contract, country_actions) {
  tibble::tibble(
    metric = c(
      "raw_issue_rows", "technical_quarantine_issue_rows",
      "low_rate_context_issue_rows", "high_rate_context_issue_rows",
      "status_context_warning_country_years",
      "status_manual_review_country_years",
      "status_ordinary_fallback_block_country_years",
      "imf_inspired_context_country_years",
      "country_rates_changed_by_technical_quarantine"
    ),
    value = c(
      nrow(issue_sanity),
      sum(issue_sanity$technical_quarantine_from_preferred_aggregate),
      sum(issue_sanity$below_one_percent_context_flag),
      sum(issue_sanity$above_thirty_percent_context_flag),
      sum(status_contract$approved_status_rule_class ==
            "context_warning_no_automatic_block"),
      sum(status_contract$approved_status_rule_class ==
            "manual_review_before_observed_benchmark_use"),
      sum(status_contract$approved_status_rule_class ==
            "block_ordinary_fallback_retain_status_evidence"),
      sum(country_actions$imf_inspired_500bps_plus_doubling_context_flag),
      sum(country_actions$technical_quarantine_changed_rate, na.rm = TRUE)
    ),
    selected_for_ladder = FALSE,
    method_id = p15_status_sanity_approved_method_id(),
    schema_version = p15_status_sanity_approved_schema_version(),
    build_id = p15_status_sanity_approved_build_id()
  )
}
