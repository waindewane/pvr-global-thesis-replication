suppressPackageStartupMessages({
  library(data.table)
  library(testthat)
})

source("../../R/p15_secondary_timing_feasibility.R")

test_that("timing output register keeps all alternatives unpromoted", {
  register <- p15_secondary_timing_output_register()
  expect_equal(nrow(register), 3L)
  expect_setequal(
    register$timing_output_id,
    c(
      "closest_to_31_december_year_end_state",
      "annual_time_weighted_secondary_state",
      "issuance_date_matched_secondary_comparator"
    )
  )
  expect_false(any(grepl("approved|selected", register$current_method_status)))
  expect_false(any(register$core_platform_blocked))
})

test_that("history coverage distinguishes a year-end window from a full year", {
  history <- data.table(
    analysis_year = c(2024L, 2024L, 2024L),
    history_date = as.IDate(c("2024-11-18", "2024-12-31", "2025-01-07")),
    RIC = c("A=", "A=", "A="),
    yield_to_maturity = c(5, 5.1, 5.2),
    any_observed_measure = TRUE
  )
  result <- p15_secondary_history_date_coverage(history)
  expect_equal(result$earliest_quote_offset_days, -43L)
  expect_equal(result$latest_quote_offset_days, 7L)
  expect_false(result$full_calendar_year_covered)
  expect_equal(result$archive_scope, "around_year_end_only")
})

test_that("issuance matching requires a prior quote and a maturity bracket", {
  history <- data.table(
    analysis_year = rep(2024L, 4),
    history_date = as.IDate(c(
      "2024-12-27", "2024-12-30", "2024-12-30", "2025-01-02"
    )),
    RIC = c("SHORT=", "SHORT=", "LONG=", "LONG="),
    yield_to_maturity = c(5, 5.1, 6, 6.1),
    any_observed_measure = TRUE
  )
  identifiers <- data.table(
    analysis_year = c(2024L, 2024L), ric = c("SHORT=", "LONG="),
    iso3 = c("AAA", "AAA"), currency = c("USD", "USD"),
    maturity_date = as.IDate(c("2029-12-31", "2037-12-31")),
    source_object_standard = TRUE, nonstandard_feature_flag = FALSE,
    positive_outstanding = TRUE, hard_currency_usd_eur = TRUE
  )
  primary <- data.table(
    analysis_year = 2024L, iso3 = "AAA", currency = "USD",
    issue_date = as.IDate("2024-12-31"), economic_issue_key = "issue",
    original_maturity_years = 8, historical_lmic_reporting_scope = TRUE,
    candidate_primary_standard = TRUE,
    candidate_primary_predecessor_trial = TRUE
  )
  result <- p15_primary_issuance_match_feasibility(
    primary, history, identifiers, lookback_days = 7L
  )
  expect_equal(result$matched_quote_date, as.Date("2024-12-30"))
  expect_equal(result$eligible_secondary_bonds, 2L)
  expect_true(result$maturity_bracket_available)
  expect_equal(result$calendar_days_before_issue, 1L)
})
