# Feasibility diagnostics for alternative P15 secondary-market timing objects.
#
# These functions do not select a preferred timing rule. They establish which
# candidate outputs can be built from the currently archived source window.

p15_secondary_timing_schema_version <- function() {
  "SCHEMA-P15-SECONDARY-TIMING-FEASIBILITY-V1"
}

p15_secondary_timing_build_id <- function() {
  "BUILD-P15-SECONDARY-TIMING-FEASIBILITY-20260810-V1"
}

p15_secondary_timing_assert_columns <- function(data, required, label) {
  missing <- setdiff(required, names(data))
  if (length(missing)) {
    stop(label, " is missing required columns: ", paste(missing, collapse = ", "),
         call. = FALSE)
  }
  invisible(TRUE)
}

p15_secondary_timing_output_register <- function() {
  data.table::data.table(
    timing_output_id = c(
      "closest_to_31_december_year_end_state",
      "annual_time_weighted_secondary_state",
      "issuance_date_matched_secondary_comparator"
    ),
    research_object = c(
      "Outstanding-stock-weighted market state at year-end",
      "Outstanding-stock-weighted secondary-market state averaged through the year",
      "Secondary-market pricing immediately before each primary issue"
    ),
    uniform_construction = c(
      paste(
        "For each bond, choose the quote with the smallest absolute distance from",
        "31 December inside the approved window; prefer the pre-year-end quote on",
        "an exact tie; aggregate using eligible amount outstanding."
      ),
      paste(
        "Calculate eligible amount-outstanding-weighted country rates on regular",
        "dates, then time-weight those country-date rates across the calendar year",
        "under explicit staleness and minimum-coverage rules."
      ),
      paste(
        "For each primary issue, use same-country and same-currency secondary quotes",
        "from the nearest available date at least one day before issuance; construct",
        "a maturity-comparable curve estimate, then aggregate with primary issue weights."
      )
    ),
    current_local_source_status = c(
      "buildable_from_current_archive",
      "not_buildable_current_archive_is_year_end_window_only",
      "only_partial_year_end_issuance_feasibility_test_is_possible"
    ),
    current_method_status = c(
      "candidate_already_implemented_not_promoted",
      "tentative_specification_pending_full_year_source",
      "tentative_diagnostic_specification_pending_full_year_source"
    ),
    core_platform_blocked = c(FALSE, FALSE, FALSE),
    blocked_decision = c(
      "Preferred P15 year-end quote-window convention",
      "Whether this should be the preferred annual secondary fallback",
      "Final primary-versus-secondary timing diagnostic and its empirical results"
    ),
    schema_version = p15_secondary_timing_schema_version(),
    build_id = p15_secondary_timing_build_id()
  )
}

p15_secondary_history_date_coverage <- function(history) {
  p15_secondary_timing_assert_columns(
    history,
    c("analysis_year", "history_date", "RIC", "yield_to_maturity",
      "any_observed_measure"),
    "P15 normalized secondary history"
  )
  x <- data.table::as.data.table(data.table::copy(history))
  x[, history_date := data.table::as.IDate(history_date)]
  x[, target_date := data.table::as.IDate(sprintf("%d-12-31", analysis_year))]
  x[, quote_offset_days := as.integer(history_date - target_date)]

  result <- x[, .(
    earliest_history_date = min(history_date, na.rm = TRUE),
    latest_history_date = max(history_date, na.rm = TRUE),
    earliest_quote_offset_days = min(quote_offset_days, na.rm = TRUE),
    latest_quote_offset_days = max(quote_offset_days, na.rm = TRUE),
    distinct_quote_dates = data.table::uniqueN(history_date),
    distinct_calendar_months = data.table::uniqueN(format(history_date, "%Y-%m")),
    history_rows = .N,
    observed_measure_rows = sum(any_observed_measure %in% TRUE, na.rm = TRUE),
    direct_yield_rows = sum(!is.na(yield_to_maturity)),
    identifiers_with_direct_yield = data.table::uniqueN(
      RIC[!is.na(yield_to_maturity)]
    )
  ), by = analysis_year]
  result[, `:=`(
    full_calendar_year_covered =
      earliest_history_date <= data.table::as.IDate(sprintf("%d-01-01", analysis_year)) &
      latest_history_date >= data.table::as.IDate(sprintf("%d-12-31", analysis_year)),
    archive_scope = "around_year_end_only",
    schema_version = p15_secondary_timing_schema_version(),
    build_id = p15_secondary_timing_build_id()
  )]
  result[]
}

p15_secondary_plain_vanilla_history <- function(history, identifiers) {
  p15_secondary_timing_assert_columns(
    identifiers,
    c(
      "analysis_year", "ric", "iso3", "currency", "maturity_date",
      "source_object_standard", "nonstandard_feature_flag",
      "positive_outstanding", "hard_currency_usd_eur"
    ),
    "P15 secondary identifier evidence"
  )
  h <- data.table::as.data.table(data.table::copy(history))[
    !is.na(yield_to_maturity),
    .(analysis_year, history_date = data.table::as.IDate(history_date),
      ric = RIC, yield_to_maturity)
  ]
  a <- data.table::as.data.table(data.table::copy(identifiers))[, .(
    analysis_year, ric, iso3, currency,
    maturity_date = data.table::as.IDate(maturity_date),
    source_object_standard, nonstandard_feature_flag,
    positive_outstanding, hard_currency_usd_eur
  )]
  x <- merge(h, a, by = c("analysis_year", "ric"), allow.cartesian = TRUE)
  x <- unique(
    x,
    by = c(
      "analysis_year", "history_date", "ric", "iso3", "currency",
      "maturity_date", "yield_to_maturity"
    )
  )
  x[
    source_object_standard %in% TRUE &
      !nonstandard_feature_flag %in% TRUE &
      positive_outstanding %in% TRUE &
      hard_currency_usd_eur %in% TRUE
  ]
}

p15_primary_issuance_match_feasibility <- function(
    primary, history, identifiers, lookback_days = 7L) {
  p15_secondary_timing_assert_columns(
    primary,
    c(
      "analysis_year", "iso3", "currency", "issue_date",
      "economic_issue_key", "original_maturity_years",
      "historical_lmic_reporting_scope", "candidate_primary_standard",
      "candidate_primary_predecessor_trial"
    ),
    "P15 primary issue evidence"
  )
  p <- data.table::as.data.table(data.table::copy(primary))[
    historical_lmic_reporting_scope %in% TRUE & currency %chin% c("USD", "EUR")
  ]
  p[, issue_date := data.table::as.IDate(issue_date)]
  p[, primary_row_id := .I]
  h <- p15_secondary_plain_vanilla_history(history, identifiers)
  data.table::setkey(h, analysis_year, iso3, currency)

  inspect_one <- function(year, country, curr, issue_day, target_maturity) {
    candidates <- h[.(year, country, curr)]
    if (!nrow(candidates) || is.na(issue_day)) {
      return(list(
        matched_quote_date = as.Date(NA), calendar_days_before_issue = NA_integer_,
        eligible_secondary_bonds = 0L, shorter_maturity_bonds = 0L,
        longer_maturity_bonds = 0L, maturity_bracket_available = FALSE
      ))
    }
    candidates <- candidates[
      history_date <= issue_day - 1L &
        history_date >= issue_day - as.integer(lookback_days)
    ]
    if (!nrow(candidates)) {
      return(list(
        matched_quote_date = as.Date(NA), calendar_days_before_issue = NA_integer_,
        eligible_secondary_bonds = 0L, shorter_maturity_bonds = 0L,
        longer_maturity_bonds = 0L, maturity_bracket_available = FALSE
      ))
    }
    matched_date <- max(candidates$history_date)
    candidates <- candidates[history_date == matched_date]
    candidates[, residual_maturity_years := as.numeric(
      maturity_date - matched_date
    ) / 365.25]
    candidates <- candidates[
      residual_maturity_years >= 2 & residual_maturity_years <= 15
    ]
    shorter <- if (is.na(target_maturity)) 0L else
      data.table::uniqueN(candidates[residual_maturity_years <= target_maturity, ric])
    longer <- if (is.na(target_maturity)) 0L else
      data.table::uniqueN(candidates[residual_maturity_years >= target_maturity, ric])
    list(
      matched_quote_date = as.Date(matched_date),
      calendar_days_before_issue = as.integer(issue_day - matched_date),
      eligible_secondary_bonds = data.table::uniqueN(candidates$ric),
      shorter_maturity_bonds = as.integer(shorter),
      longer_maturity_bonds = as.integer(longer),
      maturity_bracket_available = shorter > 0L && longer > 0L
    )
  }

  matches <- Map(
    inspect_one,
    p$analysis_year, p$iso3, p$currency, p$issue_date,
    p$original_maturity_years
  )
  result <- cbind(
    p[, .(
      primary_row_id, analysis_year, iso3, currency, issue_date,
      economic_issue_key, original_maturity_years,
      candidate_primary_standard, candidate_primary_predecessor_trial
    )],
    data.table::rbindlist(matches)
  )
  result[, `:=`(
    quote_available_within_lookback = eligible_secondary_bonds > 0L,
    lookback_days = as.integer(lookback_days),
    archive_scope = "around_year_end_only",
    result_interpretation =
      "partial_local_feasibility_only_not_a_full_year_issuance_comparator",
    schema_version = p15_secondary_timing_schema_version(),
    build_id = p15_secondary_timing_build_id()
  )]
  result[]
}

p15_primary_issuance_match_summary <- function(issue_results) {
  scopes <- data.table::rbindlist(list(
    issue_results[candidate_primary_standard %in% TRUE][, candidate_scope :=
      "primary_standard"],
    issue_results[candidate_primary_predecessor_trial %in% TRUE][,
      candidate_scope := "primary_predecessor_trial"]
  ), use.names = TRUE, fill = TRUE)
  scopes[, .(
    primary_issues = .N,
    issues_with_any_eligible_secondary_quote = sum(
      quote_available_within_lookback, na.rm = TRUE
    ),
    issues_with_maturity_bracket = sum(maturity_bracket_available, na.rm = TRUE),
    share_with_any_eligible_secondary_quote = mean(
      quote_available_within_lookback, na.rm = TRUE
    ),
    share_with_maturity_bracket = mean(maturity_bracket_available, na.rm = TRUE),
    full_year_test_possible = FALSE,
    interpretation =
      "Current archive can test only issues falling inside its year-end quote window",
    schema_version = p15_secondary_timing_schema_version(),
    build_id = p15_secondary_timing_build_id()
  ), by = .(candidate_scope, analysis_year)][]
}
