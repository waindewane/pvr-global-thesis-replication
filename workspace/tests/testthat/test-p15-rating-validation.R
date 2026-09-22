suppressPackageStartupMessages({
  library(dplyr)
  library(testthat)
})

source("../../R/p15_rating_validation.R")

test_that("risk-free history is reduced to one annual curve-tenor component", {
  x <- tibble::tibble(
    Date = as.Date(c("2024-01-02", "2024-01-03", "2024-01-02")),
    MID_PRICE = c(4, 6, 100),
    curve = "usd_treasury",
    tenor_years = 7,
    ric = "US7YT=RR",
    history_field = c("MID_PRICE", "MID_PRICE", "BID")
  )
  out <- p15_build_lseg_risk_free_annual(x)
  expect_equal(nrow(out), 1L)
  expect_equal(out$source_value_annual_mean, 5)
  expect_equal(out$observation_count, 2L)
  expect_equal(
    out$risk_free_admissibility_state,
    "quarantined_pending_field_definition_and_rate_semantics"
  )
})

test_that("gap summaries report unweighted and weighted diagnostics", {
  x <- tibble::tibble(
    group = "A",
    signed_gap_pp = c(-1, 3),
    iso3 = c("AAA", "BBB"),
    analysis_year = c(2023L, 2024L),
    weight = c(3, 1)
  )
  out <- p15_summarise_rating_gaps(x, "group", "weight")
  expect_equal(out$mean_signed_gap_pp, 1)
  expect_equal(out$mean_abs_gap_pp, 2)
  expect_equal(out$weighted_mean_signed_gap_pp, 0)
  expect_equal(out$weighted_mean_abs_gap_pp, 1.5)
  expect_equal(out$countries, 2L)
})

test_that("FRED tenor captures produce annual market-yield components", {
  x <- tibble::tibble(
    date = as.Date(c("2024-01-02", "2024-01-03", "2024-01-04")),
    value = c(3, NA, 5)
  )
  out <- p15_build_fred_tenor_annual(x, "DGS5", 5)
  expect_equal(out$risk_free_annual_mean_pct, 4)
  expect_equal(out$observation_count, 2L)
  expect_equal(out$series_id, "DGS5")
  expect_equal(out$tenor_years, 5)
})

test_that("sample cuts retain all rows and add scope-specific copies", {
  x <- tibble::tibble(
    historical_lmic_reporting_scope = c(TRUE, FALSE),
    historical_income_level = c("Low income", "High income")
  )
  out <- p15_add_rating_validation_sample_cuts(x)
  expect_equal(nrow(out), 4L)
  expect_setequal(
    out$sample_cut,
    c(
      "all_country_years", "historical_lmic_scope",
      "historical_high_income_comparison"
    )
  )
})
