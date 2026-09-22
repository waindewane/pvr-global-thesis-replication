# Strict forward P15 price-derived-secondary candidate processor.
#
# This implements SEC-10 without selecting a ladder rate. It is intentionally
# fail-closed: every required field and technical check must pass. Country-year
# status cases that have a provisional block/manual-review recommendation are held
# for STAT-05 rather than silently admitted or deleted.

p15_secondary_repair_candidate_schema_version <- function() {
  "SCHEMA-P15-SECONDARY-REPAIR-CANDIDATE-V1"
}

p15_secondary_repair_candidate_build_id <- function() {
  "BUILD-P15-SECONDARY-REPAIR-CANDIDATE-20260809-V1"
}

p15_secondary_repair_candidate_parameters <- function() {
  list(
    quote_pre_year_end_days = 31,
    quote_post_year_end_days = 7,
    settlement_lag_calendar_days = 2,
    residual_maturity_min_years = 2,
    residual_maturity_max_years = 15,
    max_settlement_sensitivity_bps = 25,
    max_bid_ask_yield_range_bps = 50,
    max_identifier_yield_dispersion_bps = 25,
    technical_rate_min_pct = -5,
    technical_rate_max_pct = 100,
    relative_outlier_multiple = 10,
    relative_outlier_min_issue_count = 3L
  )
}

p15_secondary_repair_frequency <- function(x) {
  value <- tolower(trimws(as.character(x)))
  dplyr::case_when(
    value %in% c("annually", "annual", "yearly") ~ 1,
    value %in% c("semiannually", "semi-annually", "semiannual") ~ 2,
    value %in% c("quarterly", "quarterly in arrears") ~ 4,
    value %in% c("monthly") ~ 12,
    TRUE ~ NA_real_
  )
}

p15_secondary_repair_safe_ytm <- function(
    price, settle_date, maturity_date, coupon_rate_pct, frequency) {
  if (!is.finite(price) || price <= 0 || is.na(settle_date) ||
      is.na(maturity_date) || maturity_date <= settle_date ||
      !is.finite(coupon_rate_pct) || coupon_rate_pct < 0 ||
      !is.finite(frequency) || frequency <= 0) {
    return(NA_real_)
  }
  p15_secondary_ytm_from_clean_price(
    price, settle_date, maturity_date, coupon_rate_pct, frequency
  )
}

p15_secondary_repair_failed_gates <- function(data, gate_columns) {
  gate_values <- as.data.frame(data[gate_columns])
  vapply(seq_len(nrow(gate_values)), function(i) {
    passed <- vapply(gate_values[i, , drop = FALSE], function(x) {
      isTRUE(x[[1]])
    }, logical(1))
    failed <- sub("_gate$", "", gate_columns[!passed])
    if (!length(failed)) "none" else paste(failed, collapse = ";")
  }, character(1))
}

p15_secondary_repair_quote_summary <- function(
    identifier_evidence, history,
    parameters = p15_secondary_repair_candidate_parameters()) {
  p15_observed_assert_columns(
    identifier_evidence,
    c("analysis_year", "snapshot_date", "ric"),
    "P15 secondary identifier evidence"
  )
  p15_observed_assert_columns(
    history,
    c(
      "analysis_year", "history_date", "RIC", "mid_price", "bid", "ask",
      "yield_to_maturity", "source_package_id", "source_role",
      "source_record_locator"
    ),
    "P15 secondary history"
  )

  identifiers <- identifier_evidence |>
    dplyr::select("analysis_year", "snapshot_date", "ric") |>
    dplyr::distinct()
  if (anyDuplicated(identifiers[c("analysis_year", "ric")])) {
    stop("Secondary identifier evidence is not unique by year and RIC.",
         call. = FALSE)
  }

  normalized <- history |>
    dplyr::transmute(
      analysis_year = as.integer(.data$analysis_year),
      ric = as.character(.data$RIC),
      history_date = as.Date(.data$history_date),
      mid_price = p15_observed_num(.data$mid_price),
      bid_price = p15_observed_num(.data$bid),
      ask_price = p15_observed_num(.data$ask),
      direct_yield_pct = p15_observed_num(.data$yield_to_maturity),
      quote_source_package_id = as.character(.data$source_package_id),
      quote_source_role = as.character(.data$source_role),
      quote_source_record_locator = as.character(
        .data$source_record_locator
      )
    ) |>
    dplyr::inner_join(identifiers, by = c("analysis_year", "ric")) |>
    dplyr::mutate(
      quote_offset_days = as.numeric(.data$history_date - .data$snapshot_date),
      complete_price_triple = is.finite(.data$mid_price) &
        is.finite(.data$bid_price) & is.finite(.data$ask_price),
      preferred_quote_window = .data$quote_offset_days >=
        -parameters$quote_pre_year_end_days &
        .data$quote_offset_days <= parameters$quote_post_year_end_days,
      complete_preferred_quote = .data$complete_price_triple &
        .data$preferred_quote_window
    )

  coverage <- normalized |>
    dplyr::group_by(.data$analysis_year, .data$ric) |>
    dplyr::summarise(
      history_row_count_repair = dplyr::n(),
      any_price_row_count_repair = sum(
        is.finite(.data$mid_price) | is.finite(.data$bid_price) |
          is.finite(.data$ask_price)
      ),
      complete_price_triple_row_count = sum(.data$complete_price_triple),
      preferred_complete_price_row_count = sum(
        .data$complete_preferred_quote
      ),
      direct_yield_row_count_repair = sum(
        is.finite(.data$direct_yield_pct)
      ),
      .groups = "drop"
    )

  selected_dates <- normalized |>
    dplyr::filter(.data$complete_preferred_quote) |>
    dplyr::group_by(.data$analysis_year, .data$ric) |>
    dplyr::group_modify(~ p15_secondary_date_subset(.x, "history_date", "snapshot_date",
      if (is.null(parameters$quote_selection_rule)) "latest" else parameters$quote_selection_rule,
      parameters$quote_pre_year_end_days, parameters$quote_post_year_end_days)) |>
    dplyr::summarise(
      selected_quote_date = max(.data$history_date),
      .groups = "drop"
    )

  selected <- normalized |>
    dplyr::inner_join(
      selected_dates,
      by = c("analysis_year", "ric")
    ) |>
    dplyr::filter(.data$history_date == .data$selected_quote_date) |>
    dplyr::mutate(
      quote_triple_key = paste(
        format(.data$mid_price, digits = 15, scientific = FALSE),
        format(.data$bid_price, digits = 15, scientific = FALSE),
        format(.data$ask_price, digits = 15, scientific = FALSE),
        sep = "|"
      )
    ) |>
    dplyr::group_by(.data$analysis_year, .data$ric) |>
    dplyr::summarise(
      selected_quote_date = dplyr::first(.data$selected_quote_date),
      selected_quote_offset_days = dplyr::first(.data$quote_offset_days),
      selected_quote_row_count = dplyr::n(),
      selected_quote_distinct_triple_count = dplyr::n_distinct(
        .data$quote_triple_key
      ),
      selected_mid_price = stats::median(.data$mid_price),
      selected_bid_price = stats::median(.data$bid_price),
      selected_ask_price = stats::median(.data$ask_price),
      quote_source_package_ids = p15_observed_collapse(
        .data$quote_source_package_id
      ),
      quote_source_roles = p15_observed_collapse(.data$quote_source_role),
      quote_source_record_locators = p15_observed_collapse(
        .data$quote_source_record_locator
      ),
      .groups = "drop"
    )

  identifiers |>
    dplyr::left_join(coverage, by = c("analysis_year", "ric")) |>
    dplyr::left_join(selected, by = c("analysis_year", "ric"))
}

p15_build_strict_secondary_repair_candidates <- function(
    identifier_evidence, history, status_recommendations,
    parameters = p15_secondary_repair_candidate_parameters()) {
  required_identifier <- c(
    "analysis_year", "snapshot_date", "ric", "isin", "iso3", "country",
    "country_year_id", "period", "historical_income_level",
    "historical_lmic_reporting_scope", "issue_date", "maturity_date",
    "currency", "coupon_rate_pct", "coupon_frequency_description",
    "coupon_type_leaf", "coupon_type_description", "face_outstanding_usd",
    "is_callable", "is_putable", "is_sinkable", "is_convertible",
    "flag_short_bill_cp_like", "flag_central_bank_like",
    "nonstandard_feature_flag", "any_price_available",
    "direct_yield_available", "direct_quote_date", "direct_yield_pct",
    "universe_source_package_id",
    "universe_source_record_locator", "economic_issue_key"
  )
  p15_observed_assert_columns(
    identifier_evidence, required_identifier,
    "P15 secondary identifier evidence"
  )
  p15_observed_assert_columns(
    status_recommendations,
    c(
      "analysis_year", "iso3", "recommended_treatment_class",
      "recommended_admissibility_action", "recommended_display_action",
      "expanded_status_evidence_ids", "recommendation_state"
    ),
    "P15 status case recommendations"
  )

  # Normalize data.table::IDate and character inputs to base Date before any
  # subtraction or row-wise yield calculation.
  identifier_evidence <- identifier_evidence |>
    dplyr::mutate(
      snapshot_date = as.Date(.data$snapshot_date),
      issue_date = as.Date(.data$issue_date),
      maturity_date = as.Date(.data$maturity_date),
      direct_quote_date = as.Date(.data$direct_quote_date)
    )

  quote_summary <- p15_secondary_repair_quote_summary(
    identifier_evidence, history, parameters
  )

  issue_context <- identifier_evidence |>
    dplyr::mutate(
      repair_issue_key = dplyr::if_else(
        !is.na(.data$economic_issue_key) & nzchar(.data$economic_issue_key),
        paste(
          .data$analysis_year, .data$iso3, .data$economic_issue_key,
          sep = "::"
        ),
        NA_character_
      )
    ) |>
    dplyr::filter(!is.na(.data$repair_issue_key)) |>
    dplyr::group_by(
      .data$analysis_year, .data$iso3, .data$repair_issue_key
    ) |>
    dplyr::summarise(
      issue_total_identifier_count = dplyr::n_distinct(.data$ric),
      issue_any_direct_yield = any(.data$direct_yield_available, na.rm = TRUE),
      issue_identifier_country_count = dplyr::n_distinct(.data$country),
      issue_identifier_currency_count = dplyr::n_distinct(.data$currency),
      issue_identifier_maturity_count = dplyr::n_distinct(
        as.character(.data$maturity_date)
      ),
      .groups = "drop"
    )

  identifier_audit <- identifier_evidence |>
    dplyr::filter(
      dplyr::coalesce(.data$any_price_available, FALSE)
    ) |>
    dplyr::mutate(
      repair_issue_key = dplyr::if_else(
        !is.na(.data$economic_issue_key) & nzchar(.data$economic_issue_key),
        paste(
          .data$analysis_year, .data$iso3, .data$economic_issue_key,
          sep = "::"
        ),
        NA_character_
      )
    ) |>
    dplyr::left_join(
      quote_summary,
      by = c("analysis_year", "snapshot_date", "ric")
    ) |>
    dplyr::left_join(
      issue_context,
      by = c("analysis_year", "iso3", "repair_issue_key")
    ) |>
    dplyr::mutate(
      coupon_frequency = p15_secondary_repair_frequency(
        .data$coupon_frequency_description
      ),
      explicit_plain_fixed_coupon = stringr::str_detect(
        dplyr::coalesce(.data$coupon_type_leaf, ""),
        stringr::fixed("Plain Vanilla Fixed Coupon", ignore_case = TRUE)
      ) | stringr::str_detect(
        dplyr::coalesce(.data$coupon_type_description, ""),
        stringr::fixed("Fixed:Plain Vanilla Fixed Coupon", ignore_case = TRUE)
      ),
      instrument_plain_vanilla_gate = .data$explicit_plain_fixed_coupon &
        !dplyr::coalesce(.data$is_callable, FALSE) &
        !dplyr::coalesce(.data$is_putable, FALSE) &
        !dplyr::coalesce(.data$is_sinkable, FALSE) &
        !dplyr::coalesce(.data$is_convertible, FALSE) &
        !dplyr::coalesce(.data$flag_short_bill_cp_like, FALSE) &
        !dplyr::coalesce(.data$flag_central_bank_like, FALSE) &
        !dplyr::coalesce(.data$nonstandard_feature_flag, FALSE),
      identifier_gate = !is.na(.data$ric) & nzchar(.data$ric) &
        !is.na(.data$isin) & nzchar(.data$isin) &
        !is.na(.data$repair_issue_key),
      static_terms_gate = !is.na(.data$maturity_date) &
        is.finite(.data$coupon_rate_pct) & .data$coupon_rate_pct >= 0 &
        is.finite(.data$coupon_frequency) & .data$coupon_frequency > 0,
      currency_gate = .data$currency %in% c("USD", "EUR"),
      positive_weight_gate = is.finite(.data$face_outstanding_usd) &
        .data$face_outstanding_usd > 0,
      complete_price_quote_gate = dplyr::coalesce(
        .data$preferred_complete_price_row_count > 0, FALSE
      ) & is.finite(.data$selected_mid_price) &
        is.finite(.data$selected_bid_price) &
        is.finite(.data$selected_ask_price),
      quote_window_gate = !is.na(.data$selected_quote_offset_days) &
        .data$selected_quote_offset_days >=
          -parameters$quote_pre_year_end_days &
        .data$selected_quote_offset_days <=
          parameters$quote_post_year_end_days,
      quote_conflict_gate = dplyr::coalesce(
        .data$selected_quote_distinct_triple_count == 1, FALSE
      ),
      price_order_gate = .data$complete_price_quote_gate &
        .data$selected_bid_price > 0 & .data$selected_mid_price > 0 &
        .data$selected_ask_price > 0 &
        .data$selected_bid_price <= .data$selected_mid_price &
        .data$selected_mid_price <= .data$selected_ask_price,
      provenance_gate = !is.na(.data$universe_source_package_id) &
        nzchar(.data$universe_source_package_id) &
        !is.na(.data$universe_source_record_locator) &
        nzchar(.data$universe_source_record_locator) &
        !is.na(.data$quote_source_package_ids) &
        nzchar(.data$quote_source_package_ids) &
        !is.na(.data$quote_source_record_locators) &
        nzchar(.data$quote_source_record_locators),
      issue_resolution_gate = dplyr::coalesce(
        .data$issue_identifier_country_count == 1 &
          .data$issue_identifier_currency_count == 1 &
          .data$issue_identifier_maturity_count == 1,
        FALSE
      ),
      direct_ytm_absent_gate = !dplyr::coalesce(
        .data$issue_any_direct_yield, TRUE
      ),
      residual_maturity_years_at_quote = as.numeric(
        .data$maturity_date - .data$selected_quote_date
      ) / 365.25,
      residual_maturity_gate = is.finite(
        .data$residual_maturity_years_at_quote
      ) & .data$residual_maturity_years_at_quote >=
        parameters$residual_maturity_min_years &
        .data$residual_maturity_years_at_quote <=
          parameters$residual_maturity_max_years,
      yield_input_gate = .data$static_terms_gate &
        .data$instrument_plain_vanilla_gate & .data$identifier_gate &
        .data$currency_gate & .data$positive_weight_gate &
        .data$complete_price_quote_gate & .data$quote_window_gate &
        .data$quote_conflict_gate & .data$price_order_gate &
        .data$provenance_gate & .data$issue_resolution_gate &
        .data$residual_maturity_gate &
        !is.na(.data$selected_quote_date) &
        .data$maturity_date >
          (.data$selected_quote_date +
             parameters$settlement_lag_calendar_days),
      admission_yield_input_gate = .data$yield_input_gate &
        .data$direct_ytm_absent_gate,
      repaired_yield_mid_pct = mapply(
        p15_secondary_repair_safe_ytm,
        dplyr::if_else(
          .data$yield_input_gate, .data$selected_mid_price, NA_real_
        ),
        .data$selected_quote_date, .data$maturity_date,
        .data$coupon_rate_pct, .data$coupon_frequency
      ),
      repaired_yield_bid_pct = mapply(
        p15_secondary_repair_safe_ytm,
        dplyr::if_else(
          .data$admission_yield_input_gate,
          .data$selected_bid_price, NA_real_
        ),
        .data$selected_quote_date, .data$maturity_date,
        .data$coupon_rate_pct, .data$coupon_frequency
      ),
      repaired_yield_ask_pct = mapply(
        p15_secondary_repair_safe_ytm,
        dplyr::if_else(
          .data$admission_yield_input_gate,
          .data$selected_ask_price, NA_real_
        ),
        .data$selected_quote_date, .data$maturity_date,
        .data$coupon_rate_pct, .data$coupon_frequency
      ),
      repaired_yield_mid_tplus2_pct = mapply(
        p15_secondary_repair_safe_ytm,
        dplyr::if_else(
          .data$admission_yield_input_gate,
          .data$selected_mid_price, NA_real_
        ),
        .data$selected_quote_date +
          parameters$settlement_lag_calendar_days,
        .data$maturity_date, .data$coupon_rate_pct,
        .data$coupon_frequency
      ),
      yield_computation_gate = .data$admission_yield_input_gate &
        is.finite(.data$repaired_yield_mid_pct) &
        is.finite(.data$repaired_yield_bid_pct) &
        is.finite(.data$repaired_yield_ask_pct) &
        is.finite(.data$repaired_yield_mid_tplus2_pct),
      settlement_sensitivity_bps = abs(
        .data$repaired_yield_mid_tplus2_pct -
          .data$repaired_yield_mid_pct
      ) * 100,
      bid_ask_yield_range_bps = abs(
        .data$repaired_yield_bid_pct - .data$repaired_yield_ask_pct
      ) * 100,
      settlement_sensitivity_gate = .data$yield_computation_gate &
        .data$settlement_sensitivity_bps <=
          parameters$max_settlement_sensitivity_bps,
      bid_ask_yield_range_gate = .data$yield_computation_gate &
        .data$bid_ask_yield_range_bps <=
          parameters$max_bid_ask_yield_range_bps,
      technical_rate_range_gate = .data$yield_computation_gate &
        .data$repaired_yield_mid_pct > parameters$technical_rate_min_pct &
        .data$repaired_yield_mid_pct <= parameters$technical_rate_max_pct,
      direct_repair_comparison_applicable =
        dplyr::coalesce(.data$direct_yield_available, FALSE) &
        is.finite(.data$direct_yield_pct) &
        is.finite(.data$repaired_yield_mid_pct) &
        !is.na(.data$direct_quote_date) &
        .data$direct_quote_date == .data$selected_quote_date,
      repaired_minus_direct_pp = dplyr::if_else(
        .data$direct_repair_comparison_applicable,
        .data$repaired_yield_mid_pct - .data$direct_yield_pct,
        NA_real_
      ),
      repaired_minus_direct_abs_bps = abs(
        .data$repaired_minus_direct_pp
      ) * 100
    )

  identifier_gate_columns <- c(
    "instrument_plain_vanilla_gate", "identifier_gate",
    "static_terms_gate", "currency_gate", "positive_weight_gate",
    "complete_price_quote_gate", "quote_window_gate",
    "quote_conflict_gate", "price_order_gate", "provenance_gate",
    "issue_resolution_gate", "direct_ytm_absent_gate",
    "residual_maturity_gate", "yield_computation_gate",
    "settlement_sensitivity_gate", "bid_ask_yield_range_gate",
    "technical_rate_range_gate"
  )
  identifier_audit$identifier_failed_gates <-
    p15_secondary_repair_failed_gates(
      identifier_audit, identifier_gate_columns
    )
  identifier_audit$identifier_technical_pass <-
    identifier_audit$identifier_failed_gates == "none"
  identifier_audit <- identifier_audit |>
    dplyr::mutate(
      price_convention_state =
        "clean_price_working_assumption_official_definition_not_archived",
      repaired_evidence_tier = "price_derived_secondary",
      candidate_schema_version =
        p15_secondary_repair_candidate_schema_version(),
      candidate_build_id = p15_secondary_repair_candidate_build_id()
    )

  issue_audit <- identifier_audit |>
    dplyr::group_by(
      .data$analysis_year, .data$iso3, .data$country,
      .data$country_year_id, .data$period,
      .data$historical_income_level,
      .data$historical_lmic_reporting_scope, .data$repair_issue_key,
      .data$currency
    ) |>
    dplyr::summarise(
      issue_price_identifier_count = dplyr::n_distinct(.data$ric),
      issue_total_identifier_count = max(
        .data$issue_total_identifier_count, na.rm = TRUE
      ),
      issue_any_direct_yield = any(
        dplyr::coalesce(.data$issue_any_direct_yield, FALSE)
      ),
      direct_ytm_absent_gate = all(.data$direct_ytm_absent_gate),
      representative_rics = p15_observed_collapse(.data$ric),
      representative_isins = p15_observed_collapse(.data$isin),
      issue_date = p15_observed_first(.data$issue_date),
      maturity_date = p15_observed_first(.data$maturity_date),
      selected_quote_date = p15_observed_first(.data$selected_quote_date),
      coupon_rate_pct = p15_observed_median_or_na(.data$coupon_rate_pct),
      coupon_frequency = p15_observed_median_or_na(.data$coupon_frequency),
      face_outstanding_usd = p15_observed_max_or_na(
        .data$face_outstanding_usd
      ),
      remaining_maturity_years = p15_observed_median_or_na(
        .data$residual_maturity_years_at_quote
      ),
      repaired_yield_pct = p15_observed_median_or_na(
        .data$repaired_yield_mid_pct
      ),
      direct_yield_pct_comparator = p15_observed_median_or_na(
        .data$direct_yield_pct
      ),
      direct_repair_comparison_identifier_count = sum(
        .data$direct_repair_comparison_applicable
      ),
      repaired_minus_direct_pp = p15_observed_median_or_na(
        .data$repaired_minus_direct_pp
      ),
      repaired_minus_direct_abs_bps = p15_observed_median_or_na(
        .data$repaired_minus_direct_abs_bps
      ),
      identifier_yield_dispersion_bps = (
        p15_observed_max_or_na(.data$repaired_yield_mid_pct) -
          p15_observed_min_or_na(.data$repaired_yield_mid_pct)
      ) * 100,
      max_settlement_sensitivity_bps = p15_observed_max_or_na(
        .data$settlement_sensitivity_bps
      ),
      max_bid_ask_yield_range_bps = p15_observed_max_or_na(
        .data$bid_ask_yield_range_bps
      ),
      all_identifiers_technical_pass = all(
        .data$identifier_technical_pass
      ),
      identifier_failure_reasons = p15_observed_collapse(
        .data$identifier_failed_gates
      ),
      source_package_ids = p15_observed_collapse(
        c(.data$universe_source_package_id,
          .data$quote_source_package_ids)
      ),
      source_record_locators = p15_observed_collapse(
        c(.data$universe_source_record_locator,
          .data$quote_source_record_locators)
      ),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      identifier_dispersion_gate = is.finite(
        .data$identifier_yield_dispersion_bps
      ) & .data$identifier_yield_dispersion_bps <=
        parameters$max_identifier_yield_dispersion_bps,
      issue_technical_pass = .data$all_identifiers_technical_pass &
        .data$identifier_dispersion_gate,
      issue_technical_failure_reasons = dplyr::case_when(
        .data$issue_technical_pass ~ "none",
        !.data$all_identifiers_technical_pass &
          !.data$identifier_dispersion_gate ~ paste0(
            "identifier_failures:", .data$identifier_failure_reasons,
            ";identifier_dispersion"
          ),
        !.data$all_identifiers_technical_pass ~ paste0(
          "identifier_failures:", .data$identifier_failure_reasons
        ),
        TRUE ~ "identifier_dispersion"
      )
    )

  relative_context <- issue_audit |>
    dplyr::filter(
      .data$issue_technical_pass,
      is.finite(.data$repaired_yield_pct)
    ) |>
    dplyr::group_by(.data$analysis_year, .data$iso3) |>
    dplyr::summarise(
      technically_valid_issue_count = dplyr::n(),
      issuer_year_median_repaired_yield_pct = stats::median(
        .data$repaired_yield_pct
      ),
      .groups = "drop"
    )

  status <- status_recommendations |>
    dplyr::select(
      "analysis_year", "iso3", "recommended_treatment_class",
      "recommended_admissibility_action", "recommended_display_action",
      "expanded_status_evidence_ids", "recommendation_state"
    ) |>
    dplyr::distinct(.data$analysis_year, .data$iso3, .keep_all = TRUE)

  issue_audit <- issue_audit |>
    dplyr::left_join(
      relative_context, by = c("analysis_year", "iso3")
    ) |>
    dplyr::left_join(status, by = c("analysis_year", "iso3")) |>
    dplyr::mutate(
      relative_outlier_check_applicable =
        dplyr::coalesce(
          .data$technically_valid_issue_count >=
            parameters$relative_outlier_min_issue_count &
          is.finite(.data$issuer_year_median_repaired_yield_pct) &
          .data$issuer_year_median_repaired_yield_pct > 0,
          FALSE
        ),
      relative_outlier_gate = !.data$relative_outlier_check_applicable |
        dplyr::coalesce(
          .data$repaired_yield_pct <=
            parameters$relative_outlier_multiple *
              .data$issuer_year_median_repaired_yield_pct,
          FALSE
        ),
      status_review_state = dplyr::case_when(
        stringr::str_detect(
          dplyr::coalesce(.data$recommended_treatment_class, ""),
          "provisional_block|provisional_manual_review"
        ) ~ "held_pending_STAT_05_case_decision",
        stringr::str_detect(
          dplyr::coalesce(.data$recommended_treatment_class, ""),
          "context_warning"
        ) ~ "admissible_with_provisional_status_warning",
        TRUE ~ "no_bounded_status_review_trigger"
      ),
      status_gate = .data$status_review_state !=
        "held_pending_STAT_05_case_decision",
      strict_candidate_admitted = .data$issue_technical_pass &
        .data$relative_outlier_gate & .data$status_gate,
      admission_state = dplyr::case_when(
        !.data$issue_technical_pass ~ "excluded_failed_technical_gate",
        !.data$relative_outlier_gate ~
          "excluded_issuer_year_relative_outlier_gate",
        !.data$status_gate ~ "held_pending_STAT_05_case_decision",
        .data$status_review_state ==
          "admissible_with_provisional_status_warning" ~
          "admitted_candidate_with_status_warning",
        TRUE ~ "admitted_candidate"
      ),
      repaired_evidence_tier = "price_derived_secondary",
      method_id = "P15_PRICE_DERIVED_SECONDARY_STRICT_2_15_V1",
      selection_state = "not_selected_pending_SEC_18",
      selected_for_ladder = FALSE,
      candidate_schema_version =
        p15_secondary_repair_candidate_schema_version(),
      candidate_build_id = p15_secondary_repair_candidate_build_id()
    ) |>
    dplyr::arrange(.data$analysis_year, .data$iso3, .data$repair_issue_key)

  country_candidates <- issue_audit |>
    dplyr::filter(.data$strict_candidate_admitted) |>
    dplyr::group_by(
      .data$analysis_year, .data$iso3, .data$country,
      .data$country_year_id, .data$period,
      .data$historical_income_level,
      .data$historical_lmic_reporting_scope, .data$currency
    ) |>
    dplyr::summarise(
      market_rate_pct = p15_observed_weighted_mean(
        .data$repaired_yield_pct, .data$face_outstanding_usd
      ),
      market_maturity_years = p15_observed_weighted_mean(
        .data$remaining_maturity_years, .data$face_outstanding_usd
      ),
      issue_count = dplyr::n(),
      identifier_count = sum(.data$issue_price_identifier_count),
      total_weight_usd = sum(.data$face_outstanding_usd),
      included_issue_keys = p15_observed_collapse(.data$repair_issue_key),
      included_isins = p15_observed_collapse(.data$representative_isins),
      included_rics = p15_observed_collapse(.data$representative_rics),
      source_package_ids = p15_observed_collapse(.data$source_package_ids),
      status_warning_present = any(
        .data$admission_state ==
          "admitted_candidate_with_status_warning"
      ),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      candidate_variant_id = dplyr::case_when(
        .data$currency == "USD" ~
          "price_derived_secondary_strict_usd_2_15",
        .data$currency == "EUR" ~
          "price_derived_secondary_strict_eur_2_15",
        TRUE ~ "price_derived_secondary_strict_other_2_15"
      ),
      repaired_evidence_tier = "price_derived_secondary",
      method_id = "P15_PRICE_DERIVED_SECONDARY_STRICT_2_15_V1",
      selection_state = "not_selected_pending_SEC_18",
      selected_for_ladder = FALSE,
      candidate_schema_version =
        p15_secondary_repair_candidate_schema_version(),
      candidate_build_id = p15_secondary_repair_candidate_build_id()
    ) |>
    dplyr::arrange(
      .data$analysis_year, .data$iso3, .data$candidate_variant_id
    )

  list(
    identifier_audit = identifier_audit,
    issue_audit = issue_audit,
    country_candidates = country_candidates,
    parameters = parameters
  )
}

p15_build_secondary_issue_disposition_catalogue <- function(
    direct_issue_evidence, repair_issue_audit) {
  p15_observed_assert_columns(
    direct_issue_evidence,
    c(
      "analysis_year", "iso3", "economic_issue_key", "direct_yield_pct",
      "identifiers_with_price", "candidate_secondary_standard",
      "candidate_secondary_usd_eur_2_15", "nonstandard_feature_flag"
    ),
    "P15 direct secondary issue evidence"
  )
  p15_observed_assert_columns(
    repair_issue_audit,
    c(
      "analysis_year", "iso3", "repair_issue_key", "repaired_yield_pct",
      "strict_candidate_admitted", "admission_state",
      "issue_technical_failure_reasons", "status_review_state",
      "direct_repair_comparison_identifier_count",
      "repaired_minus_direct_pp", "repaired_minus_direct_abs_bps"
    ),
    "P15 repair issue audit"
  )

  repair <- repair_issue_audit |>
    dplyr::select(
      "analysis_year", "iso3", "repair_issue_key",
      repaired_yield_pct_candidate = "repaired_yield_pct",
      "strict_candidate_admitted", "admission_state",
      "issue_technical_failure_reasons", "status_review_state",
      "direct_repair_comparison_identifier_count",
      "repaired_minus_direct_pp", "repaired_minus_direct_abs_bps"
    )
  if (anyDuplicated(repair[c(
    "analysis_year", "iso3", "repair_issue_key"
  )])) {
    stop("Repair issue audit is not unique by resolved issue.", call. = FALSE)
  }

  catalogue <- direct_issue_evidence |>
    dplyr::mutate(
      repair_issue_key = dplyr::if_else(
        !is.na(.data$economic_issue_key) & nzchar(.data$economic_issue_key),
        paste(
          .data$analysis_year, .data$iso3, .data$economic_issue_key,
          sep = "::"
        ),
        NA_character_
      )
    ) |>
    dplyr::left_join(
      repair,
      by = c("analysis_year", "iso3", "repair_issue_key")
    ) |>
    dplyr::mutate(
      direct_yield_present = is.finite(.data$direct_yield_pct),
      strict_candidate_admitted = dplyr::coalesce(
        .data$strict_candidate_admitted, FALSE
      ),
      secondary_evidence_disposition = dplyr::case_when(
        .data$candidate_secondary_standard ~
          "direct_secondary_standard_candidate",
        .data$candidate_secondary_usd_eur_2_15 ~
          "direct_secondary_broader_currency_candidate",
        .data$direct_yield_present & .data$nonstandard_feature_flag ~
          "direct_secondary_feature_rich_diagnostic",
        .data$direct_yield_present ~ "direct_secondary_other_diagnostic",
        .data$strict_candidate_admitted ~
          "price_derived_secondary_strict_candidate",
        .data$nonstandard_feature_flag &
          .data$identifiers_with_price > 0 ~
          "feature_rich_price_evidence_blocked",
        !is.na(.data$admission_state) ~ paste0(
          "price_derived_secondary_", .data$admission_state
        ),
        .data$identifiers_with_price > 0 ~
          "price_evidence_not_repair_eligible",
        TRUE ~ "no_usable_secondary_measure"
      ),
      candidate_rate_pct = dplyr::case_when(
        .data$candidate_secondary_standard |
          .data$candidate_secondary_usd_eur_2_15 ~
          .data$direct_yield_pct,
        .data$strict_candidate_admitted ~
          .data$repaired_yield_pct_candidate,
        TRUE ~ NA_real_
      ),
      candidate_evidence_tier = dplyr::case_when(
        .data$candidate_secondary_standard ~ "direct_secondary_standard",
        .data$candidate_secondary_usd_eur_2_15 ~
          "direct_secondary_broader_currency",
        .data$strict_candidate_admitted ~ "price_derived_secondary",
        TRUE ~ "diagnostic_or_blocked_secondary_evidence"
      ),
      candidate_for_secondary_method_review =
        .data$candidate_secondary_standard |
        .data$candidate_secondary_usd_eur_2_15 |
        .data$strict_candidate_admitted,
      selection_state = "not_selected_pending_SEC_18",
      selected_for_ladder = FALSE,
      candidate_schema_version =
        p15_secondary_repair_candidate_schema_version(),
      candidate_build_id = p15_secondary_repair_candidate_build_id()
    ) |>
    dplyr::arrange(
      .data$analysis_year, .data$iso3, .data$economic_issue_key
    )

  if (any(catalogue$strict_candidate_admitted &
          catalogue$direct_yield_present)) {
    stop("A repaired issue was admitted despite a direct YTM.", call. = FALSE)
  }
  catalogue
}
