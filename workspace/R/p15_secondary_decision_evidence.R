# Evidence builders for the remaining P15 secondary-method decision gates.
#
# These functions are deliberately non-selecting.  They compare feature, maturity,
# quote-window, and currency alternatives without promoting a ladder rule.

p15_secondary_decision_schema_version <- function() {
  "SCHEMA-P15-SECONDARY-DECISION-EVIDENCE-V1"
}

p15_secondary_decision_build_id <- function() {
  "BUILD-P15-SECONDARY-DECISION-EVIDENCE-20260809-V1"
}

p15_secondary_feature_label <- function(
    is_callable, is_putable, is_sinkable, is_convertible,
    flag_short_bill_cp_like, coupon_type_leaf, coupon_type_description) {
  values <- c(
    callable = isTRUE(is_callable),
    putable = isTRUE(is_putable),
    sinkable = isTRUE(is_sinkable),
    convertible = isTRUE(is_convertible),
    short_bill_or_cp = isTRUE(flag_short_bill_cp_like)
  )
  text <- tolower(paste(
    dplyr::coalesce(as.character(coupon_type_leaf), ""),
    dplyr::coalesce(as.character(coupon_type_description), "")
  ))
  text_values <- c(
    floating_or_variable = stringr::str_detect(text, "floating|variable"),
    zero_or_strip = stringr::str_detect(text, "zero|strip|discount"),
    step_rate = stringr::str_detect(text, "step"),
    indexed_or_linked = stringr::str_detect(text, "index|linked"),
    sukuk = stringr::str_detect(text, "sukuk")
  )
  labels <- names(c(values, text_values))[c(values, text_values)]
  if (!length(labels)) "other_nonstandard" else paste(labels, collapse = ";")
}

p15_build_secondary_feature_evidence <- function(
    identifier_evidence, targeted_validation = NULL,
    targeted_review = NULL) {
  p15_observed_assert_columns(
    identifier_evidence,
    c(
      "analysis_year", "ric", "isin", "iso3", "country",
      "country_year_id", "historical_lmic_reporting_scope",
      "economic_issue_key", "is_callable", "is_putable", "is_sinkable",
      "is_convertible", "flag_short_bill_cp_like", "coupon_type_leaf",
      "coupon_type_description", "nonstandard_feature_flag",
      "direct_yield_available", "any_price_available",
      "direct_yield_pct", "remaining_maturity_years", "currency"
    ),
    "P15 secondary identifier evidence"
  )

  companion <- tibble::tibble(
    analysis_year = integer(), ric = character(),
    companion_targeted = logical(), companion_cashflow_match = logical(),
    companion_worst_redemption_event = character(),
    companion_pricing_mid_yield = double(), companion_ytm = double(),
    companion_review_class = character()
  )
  if (!is.null(targeted_validation) && nrow(targeted_validation)) {
    p15_observed_assert_columns(
      targeted_validation,
      c(
        "analysis_year", "terminal_identifier",
        "matched_to_v12_cashflow_enrichment", "v12_worst_redem_event",
        "v12_pricing_mid_yield", "v12_yield_to_maturity_1"
      ),
      "Targeted 2024 feature-rich validation"
    )
    companion <- targeted_validation |>
      dplyr::transmute(
        analysis_year = as.integer(.data$analysis_year),
        ric = as.character(.data$terminal_identifier),
        companion_targeted = TRUE,
        companion_cashflow_match = dplyr::coalesce(
          as.logical(.data$matched_to_v12_cashflow_enrichment), FALSE
        ),
        companion_worst_redemption_event = as.character(
          .data$v12_worst_redem_event
        ),
        companion_pricing_mid_yield = p15_observed_num(
          .data$v12_pricing_mid_yield
        ),
        companion_ytm = p15_observed_num(.data$v12_yield_to_maturity_1)
      ) |>
      dplyr::distinct(.data$analysis_year, .data$ric, .keep_all = TRUE)
  }
  if (!is.null(targeted_review) && nrow(targeted_review)) {
    p15_observed_assert_columns(
      targeted_review,
      c("analysis_year", "terminal_identifier", "review_class"),
      "Targeted 2024 feature-rich review"
    )
    review <- targeted_review |>
      dplyr::transmute(
        analysis_year = as.integer(.data$analysis_year),
        ric = as.character(.data$terminal_identifier),
        companion_review_class = as.character(.data$review_class)
      ) |>
      dplyr::distinct(.data$analysis_year, .data$ric, .keep_all = TRUE)
    companion <- dplyr::full_join(
      companion, review, by = c("analysis_year", "ric")
    )
  }
  if (!"companion_review_class" %in% names(companion)) {
    companion$companion_review_class <- NA_character_
  }

  identifier_audit <- identifier_evidence |>
    dplyr::filter(dplyr::coalesce(.data$nonstandard_feature_flag, FALSE)) |>
    dplyr::rowwise() |>
    dplyr::mutate(
      feature_types = p15_secondary_feature_label(
        .data$is_callable, .data$is_putable, .data$is_sinkable,
        .data$is_convertible, .data$flag_short_bill_cp_like,
        .data$coupon_type_leaf, .data$coupon_type_description
      )
    ) |>
    dplyr::ungroup() |>
    dplyr::left_join(companion, by = c("analysis_year", "ric")) |>
    dplyr::mutate(
      companion_targeted = dplyr::coalesce(
        .data$companion_targeted, FALSE
      ),
      companion_cashflow_match = dplyr::coalesce(
        .data$companion_cashflow_match, FALSE
      ),
      common_export_feature_schedule_state = dplyr::case_when(
        .data$is_callable %in% TRUE | .data$is_sinkable %in% TRUE |
          .data$is_putable %in% TRUE ~
          "feature_flag_present_complete_schedule_not_in_common_export",
        stringr::str_detect(
          .data$feature_types, "step_rate|floating_or_variable|indexed_or_linked"
        ) ~ "coupon_feature_label_present_complete_schedule_not_in_common_export",
        TRUE ~ "nonstandard_label_present_no_extra_schedule_required_or_available"
      ),
      feature_evidence_state = dplyr::case_when(
        .data$companion_cashflow_match &
          .data$companion_worst_redemption_event == "CALL" ~
          "targeted_2024_call_event_needs_validated_ytw_or_ytc_rule",
        .data$companion_cashflow_match &
          .data$companion_worst_redemption_event == "MAT" ~
          "targeted_2024_mat_event_and_vendor_yield_fields_available",
        .data$companion_targeted & !.data$companion_cashflow_match ~
          "targeted_2024_without_cashflow_enrichment_match",
        .data$direct_yield_available ~
          "ordinary_direct_ytm_available_without_complete_feature_schedule",
        .data$any_price_available ~
          "price_available_but_feature_rich_price_repair_blocked",
        TRUE ~ "no_observed_yield_or_complete_feature_schedule"
      ),
      feature_method_state = "not_admitted_pending_SEC_14",
      selected_for_ladder = FALSE,
      schema_version = p15_secondary_decision_schema_version(),
      build_id = p15_secondary_decision_build_id()
    )

  issue_audit <- identifier_audit |>
    dplyr::group_by(
      .data$analysis_year, .data$iso3, .data$country,
      .data$country_year_id, .data$historical_lmic_reporting_scope,
      .data$economic_issue_key
    ) |>
    dplyr::summarise(
      currency = p15_observed_first(.data$currency),
      remaining_maturity_years = p15_observed_median_or_na(
        .data$remaining_maturity_years
      ),
      feature_types = p15_observed_collapse(.data$feature_types),
      identifier_count = dplyr::n(),
      callable_identifier_count = sum(.data$is_callable %in% TRUE),
      sinkable_identifier_count = sum(.data$is_sinkable %in% TRUE),
      putable_identifier_count = sum(.data$is_putable %in% TRUE),
      identifiers_with_direct_ytm = sum(.data$direct_yield_available),
      identifiers_with_price = sum(.data$any_price_available, na.rm = TRUE),
      targeted_identifier_count = sum(.data$companion_targeted),
      cashflow_matched_identifier_count = sum(.data$companion_cashflow_match),
      companion_worst_redemption_events = p15_observed_collapse(
        .data$companion_worst_redemption_event
      ),
      companion_review_classes = p15_observed_collapse(
        .data$companion_review_class
      ),
      feature_evidence_states = p15_observed_collapse(
        .data$feature_evidence_state
      ),
      common_export_schedule_states = p15_observed_collapse(
        .data$common_export_feature_schedule_state
      ),
      representative_rics = p15_observed_collapse(.data$ric),
      representative_isins = p15_observed_collapse(.data$isin),
      feature_method_state = "not_admitted_pending_SEC_14",
      selected_for_ladder = FALSE,
      schema_version = p15_secondary_decision_schema_version(),
      build_id = p15_secondary_decision_build_id(),
      .groups = "drop"
    )

  list(identifier_audit = identifier_audit, issue_audit = issue_audit)
}

p15_build_secondary_callable_audit <- function(feature_identifier_audit) {
  feature_identifier_audit |>
    dplyr::filter(.data$is_callable %in% TRUE) |>
    dplyr::mutate(
      explicit_ytw_or_ytc_field_available = FALSE,
      callable_yield_validation_state = dplyr::case_when(
        .data$companion_cashflow_match &
          .data$companion_worst_redemption_event == "CALL" ~
          "call_event_observed_but_explicit_ytw_ytc_semantics_unvalidated",
        .data$companion_cashflow_match &
          .data$companion_worst_redemption_event == "MAT" ~
          "mat_is_vendor_worst_event_in_targeted_2024_snapshot",
        .data$companion_targeted & !.data$companion_cashflow_match ~
          "targeted_row_without_cashflow_enrichment_match",
        TRUE ~ "no_targeted_worst_redemption_or_call_yield_evidence"
      ),
      sec13_disposition = "remain_blocked_pending_SEC_14_decision",
      selected_for_ladder = FALSE
    )
}

p15_secondary_maturity_scenario_register <- function() {
  tibble::tribble(
    ~scenario_id, ~min_years, ~max_years, ~scenario_role,
    "usd_2_15_current_candidate", 2, 15, "current_candidate",
    "usd_ge1_inherited_sensitivity", 1, Inf, "preexisting_sensitivity",
    "usd_1_15_lower_bound_diagnostic", 1, 15, "bound_decomposition",
    "usd_ge2_upper_bound_diagnostic", 2, Inf, "bound_decomposition"
  )
}

p15_build_secondary_maturity_sensitivity <- function(issue_evidence) {
  p15_observed_assert_columns(
    issue_evidence,
    c(
      "analysis_year", "iso3", "country", "country_year_id", "period",
      "historical_income_level", "historical_lmic_reporting_scope",
      "economic_issue_key", "currency", "remaining_maturity_years",
      "face_outstanding_usd", "direct_yield_pct",
      "candidate_secondary_standard", "identifier_count"
    ),
    "P15 secondary issue evidence"
  )
  register <- p15_secondary_maturity_scenario_register()
  dplyr::bind_rows(lapply(seq_len(nrow(register)), function(i) {
    rule <- register[i, ]
    issue_evidence |>
      dplyr::filter(
        .data$candidate_secondary_standard,
        .data$currency == "USD",
        .data$remaining_maturity_years >= rule$min_years,
        .data$remaining_maturity_years <= rule$max_years
      ) |>
      dplyr::group_by(
        .data$analysis_year, .data$iso3, .data$country,
        .data$country_year_id, .data$period,
        .data$historical_income_level,
        .data$historical_lmic_reporting_scope
      ) |>
      dplyr::summarise(
        market_rate_pct = p15_observed_weighted_mean(
          .data$direct_yield_pct, .data$face_outstanding_usd
        ),
        market_maturity_years = p15_observed_weighted_mean(
          .data$remaining_maturity_years, .data$face_outstanding_usd
        ),
        issue_count = dplyr::n(),
        identifier_count = sum(.data$identifier_count),
        total_weight_usd = sum(.data$face_outstanding_usd, na.rm = TRUE),
        included_issue_keys = p15_observed_collapse(.data$economic_issue_key),
        .groups = "drop"
      ) |>
      dplyr::mutate(
        scenario_id = rule$scenario_id,
        scenario_role = rule$scenario_role,
        selected_for_ladder = FALSE,
        schema_version = p15_secondary_decision_schema_version(),
        build_id = p15_secondary_decision_build_id()
      )
  }))
}

p15_secondary_quote_scenario_register <- function() {
  tibble::tribble(
    ~scenario_id, ~pre_days, ~post_days, ~selection_rule, ~scenario_role,
    "latest_available_current_common", Inf, Inf, "latest",
    "current_common_processor",
    "preferred_31_pre_7_post", 31, 7, "latest",
    "legacy_around_year_end_window",
    "pre_year_end_31_only", 31, 0, "latest",
    "strict_no_lookahead_sensitivity",
    "closest_to_year_end_31_pre_7_post", 31, 7, "closest",
    "closest_date_sensitivity"
  )
}

p15_select_secondary_direct_quotes <- function(
    history, identifier_evidence, pre_days = Inf, post_days = Inf,
    selection_rule = c("latest", "closest")) {
  selection_rule <- match.arg(selection_rule)
  p15_observed_assert_columns(
    history,
    c(
      "analysis_year", "history_date", "RIC", "yield_to_maturity",
      "source_package_id", "source_role", "source_record_locator"
    ),
    "P15 secondary history"
  )
  p15_observed_assert_columns(
    identifier_evidence,
    c(
      "analysis_year", "snapshot_date", "ric", "iso3", "country",
      "country_year_id", "period", "historical_income_level",
      "historical_lmic_reporting_scope", "economic_issue_key", "currency",
      "remaining_maturity_years", "face_outstanding_usd",
      "positive_outstanding", "nonstandard_feature_flag",
      "flag_central_bank_like"
    ),
    "P15 secondary identifier evidence"
  )
  keys <- identifier_evidence |>
    dplyr::mutate(
      snapshot_date = as.Date(as.character(.data$snapshot_date))
    ) |>
    dplyr::select(
      "analysis_year", "snapshot_date", "ric", "iso3", "country",
      "country_year_id", "period", "historical_income_level",
      "historical_lmic_reporting_scope", "economic_issue_key", "currency",
      "remaining_maturity_years", "face_outstanding_usd",
      "positive_outstanding", "nonstandard_feature_flag",
      "flag_central_bank_like"
    ) |>
    dplyr::distinct(.data$analysis_year, .data$ric, .keep_all = TRUE)
  quotes <- history |>
    dplyr::transmute(
      analysis_year = as.integer(.data$analysis_year),
      ric = as.character(.data$RIC),
      history_date = as.Date(as.character(.data$history_date)),
      direct_yield_pct = p15_observed_num(.data$yield_to_maturity),
      source_package_id = as.character(.data$source_package_id),
      source_role = as.character(.data$source_role),
      source_record_locator = as.character(.data$source_record_locator)
    ) |>
    dplyr::filter(is.finite(.data$direct_yield_pct)) |>
    dplyr::inner_join(keys, by = c("analysis_year", "ric")) |>
    dplyr::mutate(
      quote_offset_days = as.numeric(.data$history_date - .data$snapshot_date)
    ) |>
    dplyr::filter(
      .data$quote_offset_days >= -pre_days,
      .data$quote_offset_days <= post_days
    )
  if (selection_rule == "latest") {
    quotes <- quotes |>
      dplyr::group_by(.data$analysis_year, .data$ric) |>
      dplyr::filter(.data$history_date == max(.data$history_date))
  } else {
    quotes <- quotes |>
      dplyr::group_by(.data$analysis_year, .data$ric) |>
      dplyr::filter(
        abs(.data$quote_offset_days) == min(abs(.data$quote_offset_days))
      ) |>
      dplyr::filter(
        .data$quote_offset_days == min(.data$quote_offset_days)
      )
  }
  quotes <- quotes |>
    dplyr::summarise(
      direct_quote_date = dplyr::first(.data$history_date),
      quote_offset_days = dplyr::first(.data$quote_offset_days),
      direct_yield_pct = stats::median(.data$direct_yield_pct),
      same_date_yield_range_bps =
        (max(.data$direct_yield_pct) - min(.data$direct_yield_pct)) * 100,
      source_package_ids = p15_observed_collapse(.data$source_package_id),
      source_roles = p15_observed_collapse(.data$source_role),
      source_record_locators = p15_observed_collapse(
        .data$source_record_locator
      ),
      .groups = "drop"
    )
  dplyr::left_join(keys, quotes, by = c("analysis_year", "ric")) |>
    dplyr::mutate(
      base_standard = .data$currency %in% c("USD", "EUR") &
        dplyr::coalesce(.data$positive_outstanding, FALSE) &
        !dplyr::coalesce(.data$nonstandard_feature_flag, FALSE) &
        !dplyr::coalesce(.data$flag_central_bank_like, FALSE),
      quote_value_conflict = is.finite(.data$same_date_yield_range_bps) &
        .data$same_date_yield_range_bps > 5
    )
}

p15_build_secondary_quote_sensitivity <- function(history, identifier_evidence) {
  register <- p15_secondary_quote_scenario_register()
  dplyr::bind_rows(lapply(seq_len(nrow(register)), function(i) {
    rule <- register[i, ]
    identifiers <- p15_select_secondary_direct_quotes(
      history, identifier_evidence, rule$pre_days, rule$post_days,
      rule$selection_rule
    )
    issues <- identifiers |>
      dplyr::filter(
        .data$base_standard, !.data$quote_value_conflict,
        is.finite(.data$direct_yield_pct)
      ) |>
      dplyr::group_by(
        .data$analysis_year, .data$iso3, .data$country,
        .data$country_year_id, .data$period,
        .data$historical_income_level,
        .data$historical_lmic_reporting_scope, .data$economic_issue_key
      ) |>
      dplyr::summarise(
        currency = p15_observed_first(.data$currency),
        remaining_maturity_years = p15_observed_median_or_na(
          .data$remaining_maturity_years
        ),
        face_outstanding_usd = p15_observed_max_or_na(
          .data$face_outstanding_usd
        ),
        direct_yield_pct = stats::median(.data$direct_yield_pct),
        direct_quote_date = p15_observed_first(.data$direct_quote_date),
        quote_offset_days = p15_observed_first(.data$quote_offset_days),
        identifier_count = dplyr::n(),
        identifier_yield_range_bps =
          (max(.data$direct_yield_pct) - min(.data$direct_yield_pct)) * 100,
        .groups = "drop"
      ) |>
      dplyr::filter(
        .data$identifier_yield_range_bps <= 25,
        .data$currency == "USD",
        .data$remaining_maturity_years >= 2,
        .data$remaining_maturity_years <= 15
      )
    issues |>
      dplyr::group_by(
        .data$analysis_year, .data$iso3, .data$country,
        .data$country_year_id, .data$period,
        .data$historical_income_level,
        .data$historical_lmic_reporting_scope
      ) |>
      dplyr::summarise(
        market_rate_pct = p15_observed_weighted_mean(
          .data$direct_yield_pct, .data$face_outstanding_usd
        ),
        market_maturity_years = p15_observed_weighted_mean(
          .data$remaining_maturity_years, .data$face_outstanding_usd
        ),
        issue_count = dplyr::n(),
        identifier_count = sum(.data$identifier_count),
        total_weight_usd = sum(.data$face_outstanding_usd, na.rm = TRUE),
        earliest_quote_offset_days = min(.data$quote_offset_days),
        latest_quote_offset_days = max(.data$quote_offset_days),
        max_quote_recency_abs_days = max(abs(.data$quote_offset_days)),
        included_issue_keys = p15_observed_collapse(.data$economic_issue_key),
        .groups = "drop"
      ) |>
      dplyr::mutate(
        scenario_id = rule$scenario_id,
        scenario_role = rule$scenario_role,
        selected_for_ladder = FALSE,
        schema_version = p15_secondary_decision_schema_version(),
        build_id = p15_secondary_decision_build_id()
      )
  }))
}

p15_compare_secondary_scenarios <- function(
    scenario_data, baseline_id, scenario_dimension) {
  baseline <- scenario_data |>
    dplyr::filter(.data$scenario_id == baseline_id) |>
    dplyr::select(
      "analysis_year", "iso3",
      baseline_rate_pct = "market_rate_pct",
      baseline_maturity_years = "market_maturity_years",
      baseline_issue_count = "issue_count"
    )
  alternative_ids <- setdiff(unique(scenario_data$scenario_id), baseline_id)
  dplyr::bind_rows(lapply(alternative_ids, function(alternative_id) {
    scenario_data |>
      dplyr::filter(.data$scenario_id == alternative_id) |>
      dplyr::full_join(baseline, by = c("analysis_year", "iso3")) |>
      dplyr::mutate(scenario_id = dplyr::coalesce(
        .data$scenario_id, alternative_id
      ))
  })) |>
    dplyr::mutate(
      scenario_dimension = scenario_dimension,
      baseline_scenario_id = baseline_id,
      coverage_state = dplyr::case_when(
        is.finite(.data$market_rate_pct) & is.finite(.data$baseline_rate_pct) ~
          "present_in_both",
        is.finite(.data$market_rate_pct) ~ "scenario_only",
        is.finite(.data$baseline_rate_pct) ~ "baseline_only",
        TRUE ~ "absent_in_both"
      ),
      rate_difference_pp = .data$market_rate_pct - .data$baseline_rate_pct,
      absolute_rate_difference_pp = abs(.data$rate_difference_pp),
      maturity_difference_years =
        .data$market_maturity_years - .data$baseline_maturity_years,
      selection_state = "not_selected_pending_SEC_18",
      selected_for_ladder = FALSE,
      schema_version = p15_secondary_decision_schema_version(),
      build_id = p15_secondary_decision_build_id()
    )
}

p15_build_currency_overlap_feasibility <- function(
    primary_issues, secondary_issues) {
  primary <- primary_issues |>
    dplyr::filter(
      .data$candidate_primary_standard,
      .data$historical_lmic_reporting_scope,
      .data$currency %in% c("USD", "EUR")
    )
  secondary <- secondary_issues |>
    dplyr::filter(
      .data$candidate_secondary_usd_eur_2_15,
      .data$historical_lmic_reporting_scope,
      .data$currency %in% c("USD", "EUR")
    )

  currency_counts <- dplyr::bind_rows(
    primary |>
      dplyr::distinct(.data$analysis_year, .data$iso3, .data$currency) |>
      dplyr::count(.data$analysis_year, .data$iso3, name = "currency_count") |>
      dplyr::mutate(market = "primary"),
    secondary |>
      dplyr::distinct(.data$analysis_year, .data$iso3, .data$currency) |>
      dplyr::count(.data$analysis_year, .data$iso3, name = "currency_count") |>
      dplyr::mutate(market = "secondary")
  )

  primary_pairs <- primary |>
    dplyr::mutate(issue_date = as.Date(as.character(.data$issue_date))) |>
    dplyr::filter(.data$currency == "USD") |>
    dplyr::select(
      "analysis_year", "iso3", usd_issue_date = "issue_date",
      usd_maturity = "original_maturity_years"
    ) |>
    dplyr::inner_join(
      primary |>
        dplyr::mutate(issue_date = as.Date(as.character(.data$issue_date))) |>
        dplyr::filter(.data$currency == "EUR") |>
        dplyr::select(
          "analysis_year", "iso3", eur_issue_date = "issue_date",
          eur_maturity = "original_maturity_years"
        ),
      by = c("analysis_year", "iso3"),
      relationship = "many-to-many"
    ) |>
    dplyr::mutate(
      date_gap_days = abs(as.numeric(.data$usd_issue_date - .data$eur_issue_date)),
      maturity_gap_years = abs(.data$usd_maturity - .data$eur_maturity)
    )
  secondary_pairs <- secondary |>
    dplyr::filter(.data$currency == "USD") |>
    dplyr::select(
      "analysis_year", "iso3",
      usd_maturity = "remaining_maturity_years"
    ) |>
    dplyr::inner_join(
      secondary |>
        dplyr::filter(.data$currency == "EUR") |>
        dplyr::select(
          "analysis_year", "iso3",
          eur_maturity = "remaining_maturity_years"
        ),
      by = c("analysis_year", "iso3"),
      relationship = "many-to-many"
    ) |>
    dplyr::mutate(
      maturity_gap_years = abs(.data$usd_maturity - .data$eur_maturity)
    )

  mixed <- currency_counts |>
    dplyr::filter(.data$currency_count == 2) |>
    dplyr::select("market", "analysis_year", "iso3")
  primary_overlap <- primary_pairs |>
    dplyr::group_by(.data$analysis_year, .data$iso3) |>
    dplyr::summarise(
      pair_90d_3y = any(.data$date_gap_days <= 90 &
                          .data$maturity_gap_years <= 3),
      pair_365d_5y = any(.data$date_gap_days <= 365 &
                           .data$maturity_gap_years <= 5),
      minimum_date_gap_days = min(.data$date_gap_days),
      minimum_maturity_gap_years = min(.data$maturity_gap_years),
      .groups = "drop"
    ) |>
    dplyr::mutate(market = "primary")
  secondary_overlap <- secondary_pairs |>
    dplyr::group_by(.data$analysis_year, .data$iso3) |>
    dplyr::summarise(
      pair_90d_3y = any(.data$maturity_gap_years <= 3),
      pair_365d_5y = any(.data$maturity_gap_years <= 5),
      minimum_date_gap_days = 0,
      minimum_maturity_gap_years = min(.data$maturity_gap_years),
      .groups = "drop"
    ) |>
    dplyr::mutate(market = "secondary")

  mixed |>
    dplyr::left_join(
      dplyr::bind_rows(primary_overlap, secondary_overlap),
      by = c("market", "analysis_year", "iso3")
    ) |>
    dplyr::mutate(
      pair_90d_3y = dplyr::coalesce(.data$pair_90d_3y, FALSE),
      pair_365d_5y = dplyr::coalesce(.data$pair_365d_5y, FALSE),
      normalization_state =
        "overlap_validation_feasible_swap_curve_input_not_available",
      selected_for_ladder = FALSE,
      schema_version = p15_secondary_decision_schema_version(),
      build_id = p15_secondary_decision_build_id()
    )
}
