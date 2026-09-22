source("../../R/p15_observed_secondary.R")

p15_secondary_test_row <- function(
    key,
    yield,
    maturity,
    outstanding,
    currency = "USD",
    nonstandard = FALSE) {
  data.frame(
    analysis_year = 2024,
    iso3 = "TST",
    country = "Testland",
    economic_issue_key = key,
    issue_date = "2020-01-01",
    maturity_date = "2030-01-01",
    currency = currency,
    face_outstanding_usd = outstanding,
    remaining_maturity_years = maturity,
    secondary_yield_pct = yield,
    secondary_quote_date = "2024-12-31",
    baseline_status = "included_standard_secondary",
    nonstandard_issue = nonstandard,
    representative_isins = paste0("ISIN-", key),
    representative_rics = paste0("RIC-", key),
    stringsAsFactors = FALSE
  )
}

testthat::test_that("P15 secondary standard rule filters before amount weighting", {
  rows <- rbind(
    p15_secondary_test_row("A", 5, 5, 100),
    p15_secondary_test_row("B", 10, 10, 200),
    p15_secondary_test_row("EUR", 20, 8, 500, currency = "EUR"),
    p15_secondary_test_row("LONG", 20, 20, 500),
    p15_secondary_test_row("COMPLEX", 20, 8, 500, nonstandard = TRUE)
  )
  issues <- p15_prepare_secondary_direct_issues(rows)
  result <- p15_aggregate_secondary_direct(
    issues,
    "p12a_standard_usd_2_15_candidate",
    "test_source",
    "TEST",
    "TEST-METHOD"
  )

  testthat::expect_equal(sum(issues$p12a_standard_usd_2_15_candidate), 2)
  testthat::expect_equal(result$market_rate_pct, (5 * 100 + 10 * 200) / 300)
  testthat::expect_equal(result$market_maturity_years, (5 * 100 + 10 * 200) / 300)
  testthat::expect_equal(result$issue_count, 2)
  testthat::expect_equal(result$total_weight_usd, 300)
  testthat::expect_equal(result$included_issue_keys, "A;B")
})

testthat::test_that("P15 clean-price solver reproduces par yield on a coupon date", {
  result <- p15_secondary_ytm_from_clean_price(
    clean_price = 100,
    settle_date = as.Date("2024-12-31"),
    maturity_date = as.Date("2029-12-31"),
    coupon_rate_pct = 5,
    frequency = 1
  )
  # The parity method uses actual calendar days divided by 365.25, so a nominal
  # annual coupon-date example is within a basis point rather than algebraically
  # identical to the coupon rate.
  testthat::expect_equal(result, 5, tolerance = 0.001)
})

testthat::test_that("P15 terminal direct rule collapses paired identifiers by issue", {
  rows <- data.frame(
    p7b_target_id = c("P7B-SEC-001", "P7B-SEC-002"),
    analysis_year = 2024,
    iso3 = "TST",
    country = "Testland",
    candidate_issue_key = c("Issue A", "Issue B"),
    issue_currency = "USD",
    residual_maturity_years = c(5, 10),
    face_outstanding_usd = c(100, 300),
    latest_direct_yield_values_pct = c(
      "A1@2024-12-31=4;A2@2024-12-31=6",
      "B1@2024-12-31=7;B2@2024-12-31=9"
    ),
    has_numeric_latest_direct_yield = TRUE,
    p2_status_gate_overlap = FALSE,
    stringsAsFactors = FALSE
  )
  result <- p15_build_terminal_direct_country(rows)

  testthat::expect_equal(result$market_rate_pct, (5 * 100 + 8 * 300) / 400)
  testthat::expect_equal(result$market_maturity_years, (5 * 100 + 10 * 300) / 400)
  testthat::expect_equal(result$issue_count, 2)
  testthat::expect_equal(result$total_weight_usd, 400)
})
