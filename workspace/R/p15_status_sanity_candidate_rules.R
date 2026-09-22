# Executable candidate checks for the agreed P15 status and rate-sanity direction.
#
# These checks create review evidence. They do not approve deletions, benchmark
# blocks, PVR blocks, or final status consequences.

p15_status_sanity_schema_version <- function() {
  "SCHEMA-P15-STATUS-SANITY-CANDIDATE-V1"
}

p15_status_sanity_build_id <- function() {
  "BUILD-P15-STATUS-SANITY-CANDIDATE-20260814-V1"
}

p15_read_fred_treasury_series <- function(path, tenor_years, series_id) {
  if (!file.exists(path)) stop("Missing FRED input: ", path, call. = FALSE)
  data.table::fread(path, skip = "date,value") |>
    tibble::as_tibble() |>
    dplyr::transmute(
      observation_date = as.Date(.data$date),
      risk_free_rate_pct = suppressWarnings(as.numeric(.data$value)),
      reference_tenor_years = as.numeric(tenor_years),
      reference_series_id = as.character(series_id)
    ) |>
    dplyr::filter(is.finite(.data$risk_free_rate_pct))
}

p15_bind_fred_treasury_curve <- function(dgs5, dgs7, dgs10) {
  dplyr::bind_rows(
    p15_read_fred_treasury_series(dgs5, 5, "DGS5"),
    p15_read_fred_treasury_series(dgs7, 7, "DGS7"),
    p15_read_fred_treasury_series(dgs10, 10, "DGS10")
  ) |>
    dplyr::arrange(.data$reference_tenor_years, .data$observation_date)
}

p15_nearest_treasury_tenor <- function(maturity_years) {
  tenors <- c(5, 7, 10)
  vapply(maturity_years, function(x) {
    if (!is.finite(x)) return(NA_real_)
    tenors[which.min(abs(tenors - x))]
  }, numeric(1))
}

p15_match_prior_treasury_rate <- function(
    dates, maturities, treasury_curve, maximum_lag_days = 7L) {
  curve <- split(treasury_curve, treasury_curve$reference_tenor_years)
  target_tenor <- p15_nearest_treasury_tenor(maturities)
  match_one <- function(date, tenor) {
    if (is.na(date) || !is.finite(tenor)) {
      return(list(date = as.Date(NA), rate = NA_real_, series = NA_character_))
    }
    candidates <- curve[[as.character(tenor)]]
    candidates <- candidates[
      candidates$observation_date <= date &
        candidates$observation_date >= date - as.integer(maximum_lag_days),
    ]
    if (!nrow(candidates)) {
      return(list(date = as.Date(NA), rate = NA_real_, series = NA_character_))
    }
    row <- candidates[which.max(candidates$observation_date), ]
    list(
      date = row$observation_date[[1]],
      rate = row$risk_free_rate_pct[[1]],
      series = row$reference_series_id[[1]]
    )
  }
  matches <- Map(match_one, as.Date(dates), target_tenor)
  tibble::tibble(
    reference_tenor_years = target_tenor,
    risk_free_observation_date = as.Date(vapply(
      matches, function(x) as.character(x$date), character(1)
    )),
    risk_free_rate_pct = vapply(matches, function(x) x$rate, numeric(1)),
    risk_free_series_id = vapply(matches, function(x) x$series, character(1)),
    risk_free_lag_days = as.integer(
      as.Date(dates) - as.Date(vapply(
        matches, function(x) as.character(x$date), character(1)
      ))
    )
  )
}

p15_observed_issue_review_universe <- function(
    primary_issues, secondary_issues, treasury_curve) {
  primary <- primary_issues |>
    dplyr::filter(
      .data$candidate_primary_standard,
      .data$currency == "USD",
      is.finite(.data$original_issue_yield_pct),
      is.finite(.data$aggregation_weight_value),
      .data$aggregation_weight_value > 0
    ) |>
    dplyr::transmute(
      evidence_object = "observed_primary_standard_usd",
      analysis_year = as.integer(.data$analysis_year),
      .data$iso3, .data$country, .data$country_year_id,
      .data$historical_lmic_reporting_scope,
      .data$economic_issue_key,
      rate_date = as.Date(.data$issue_date),
      maturity_years = as.numeric(.data$original_maturity_years),
      rate_pct = as.numeric(.data$original_issue_yield_pct),
      weight_usd = as.numeric(.data$aggregation_weight_value),
      source_object = as.character(.data$source_object),
      source_package_ids = as.character(.data$source_package_ids),
      source_record_locators = as.character(.data$source_record_locators)
    )
  secondary <- secondary_issues |>
    dplyr::filter(
      .data$candidate_secondary_usd_2_15,
      is.finite(.data$direct_yield_pct),
      is.finite(.data$face_outstanding_usd),
      .data$face_outstanding_usd > 0
    ) |>
    dplyr::transmute(
      evidence_object = "observed_secondary_direct_usd_2_15",
      analysis_year = as.integer(.data$analysis_year),
      .data$iso3, .data$country, .data$country_year_id,
      .data$historical_lmic_reporting_scope,
      .data$economic_issue_key,
      rate_date = as.Date(.data$direct_quote_date),
      maturity_years = as.numeric(.data$remaining_maturity_years),
      rate_pct = as.numeric(.data$direct_yield_pct),
      weight_usd = as.numeric(.data$face_outstanding_usd),
      source_object = as.character(.data$source_object_standard),
      source_package_ids = as.character(.data$direct_source_package_ids),
      source_record_locators = as.character(.data$direct_source_record_locators)
    )
  issues <- dplyr::bind_rows(primary, secondary)
  treasury <- p15_match_prior_treasury_rate(
    issues$rate_date, issues$maturity_years, treasury_curve
  )
  dplyr::bind_cols(issues, treasury) |>
    dplyr::group_by(
      .data$evidence_object, .data$analysis_year, .data$iso3
    ) |>
    dplyr::mutate(
      positive_country_year_median_rate_pct = stats::median(
        .data$rate_pct[.data$rate_pct > 0], na.rm = TRUE
      ),
      technical_range_flag = .data$rate_pct < -5 | .data$rate_pct > 100,
      ten_times_positive_median_flag =
        is.finite(.data$positive_country_year_median_rate_pct) &
        .data$positive_country_year_median_rate_pct > 0 &
        .data$rate_pct > 10 * .data$positive_country_year_median_rate_pct,
      technical_data_error_review_flag =
        .data$technical_range_flag | .data$ten_times_positive_median_flag,
      low_rate_source_review_flag = .data$rate_pct < 1,
      above_30_context_review_flag = .data$rate_pct > 30,
      sovereign_spread_pct = .data$rate_pct - .data$risk_free_rate_pct,
      risk_free_match_available = is.finite(.data$risk_free_rate_pct),
      automatic_consequence = "none",
      check_decision_state = "candidate_not_approved"
    ) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      rate_review_id = paste(
        "RATE-REVIEW", .data$evidence_object, .data$analysis_year,
        .data$iso3, .data$economic_issue_key, sep = "::"
      ),
      schema_version = p15_status_sanity_schema_version(),
      build_id = p15_status_sanity_build_id()
    )
}

p15_build_imf_spread_review_flags <- function(issue_review) {
  country <- issue_review |>
    dplyr::filter(.data$risk_free_match_available) |>
    dplyr::group_by(
      .data$evidence_object, .data$analysis_year, .data$iso3, .data$country,
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
      technical_data_error_review_flag = any(
        .data$technical_data_error_review_flag
      ),
      low_rate_source_review_flag = any(.data$low_rate_source_review_flag),
      above_30_context_review_flag = any(.data$above_30_context_review_flag),
      reference_series_ids = paste(
        sort(unique(.data$risk_free_series_id)), collapse = ";"
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
        abs(.data$market_maturity_years - .data$prior_market_maturity_years) <= 3,
      comparable_prior_year_available =
        .data$consecutive_prior_year_available &
        .data$maturity_comparable_to_prior &
        is.finite(.data$prior_sovereign_spread_pct) &
        .data$prior_sovereign_spread_pct > 0,
      spread_at_least_500bps = .data$sovereign_spread_pct >= 5,
      spread_at_least_double_prior =
        .data$comparable_prior_year_available &
        .data$sovereign_spread_pct >= 2 * .data$prior_sovereign_spread_pct,
      imf_500bps_plus_doubling_review_flag =
        .data$spread_at_least_500bps & .data$spread_at_least_double_prior,
      imf_flag_interpretation = dplyr::case_when(
        .data$imf_500bps_plus_doubling_review_flag ~
          "contextual_review_flag_not_distress_verdict_or_automatic_block",
        !.data$comparable_prior_year_available ~
          "not_testable_without_comparable_positive_prior_year_spread",
        TRUE ~ "condition_not_met"
      )
    ) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      automatic_consequence = "none",
      review_decision_state = "candidate_not_approved",
      schema_version = p15_status_sanity_schema_version(),
      build_id = p15_status_sanity_build_id()
    )
  country
}

p15_integrate_status_sanity_candidate_actions <- function(
    spread_review, status_recommendations) {
  status <- status_recommendations |>
    dplyr::select(
      "analysis_year", "iso3", "review_basis",
      "recommended_treatment_class", "recommended_admissibility_action",
      "recommended_display_action", "case_level_note", "recommendation_state"
    ) |>
    dplyr::distinct(.data$analysis_year, .data$iso3, .keep_all = TRUE)
  spread_review |>
    dplyr::left_join(status, by = c("analysis_year", "iso3")) |>
    dplyr::mutate(
      status_case_present = !is.na(.data$recommended_treatment_class),
      candidate_combined_action = dplyr::case_when(
        .data$technical_data_error_review_flag ~
          "candidate_quarantine_pending_source_review",
        grepl("provisional_block", dplyr::coalesce(
          .data$recommended_treatment_class, ""
        )) ~ "candidate_block_ordinary_fallback_pending_owner",
        grepl("manual_review", dplyr::coalesce(
          .data$recommended_treatment_class, ""
        )) ~ "candidate_manual_review_before_observed_use",
        .data$imf_500bps_plus_doubling_review_flag ~
          "candidate_contextual_market_stress_review",
        .data$above_30_context_review_flag | .data$low_rate_source_review_flag ~
          "candidate_source_specific_rate_review",
        .data$status_case_present ~ "candidate_context_warning",
        TRUE ~ "candidate_no_special_review"
      ),
      candidate_display_action = dplyr::case_when(
        .data$technical_data_error_review_flag & .data$status_case_present ~
          paste(
            .data$recommended_display_action,
            "Also display a technical source-review warning; do not silently drop the raw value."
          ),
        .data$technical_data_error_review_flag ~
          "Display a technical source-review warning; do not silently drop the raw value.",
        .data$imf_500bps_plus_doubling_review_flag & .data$status_case_present ~
          paste(
            .data$recommended_display_action,
            "Also display a market-spread review flag with current and prior spread values."
          ),
        .data$imf_500bps_plus_doubling_review_flag ~
          "Display a market-spread review flag with current and prior spread values.",
        (.data$above_30_context_review_flag |
           .data$low_rate_source_review_flag) & .data$status_case_present ~
          paste(
            .data$recommended_display_action,
            "Also display the source-specific numerical review flag."
          ),
        .data$above_30_context_review_flag | .data$low_rate_source_review_flag ~
          "Display the source-specific numerical review flag.",
        .data$status_case_present ~ .data$recommended_display_action,
        TRUE ~ "No special status or sanity display action proposed."
      ),
      automatic_selection_block = FALSE,
      automatic_pvr_block = FALSE,
      final_consequence_decided = FALSE,
      combined_decision_state = "candidate_for_STAT_05_SANE_04_owner_review",
      schema_version = p15_status_sanity_schema_version(),
      build_id = p15_status_sanity_build_id()
    )
}
