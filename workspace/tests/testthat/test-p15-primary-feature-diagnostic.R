suppressPackageStartupMessages({
  library(dplyr)
  library(stringr)
  library(testthat)
  library(tibble)
  library(tidyr)
})

source("../../R/p15_observed_all_years.R")
source("../../R/p15_primary_feature_diagnostic.R")

test_that("feature classifier preserves separate feature categories", {
  instruments <- tibble(
    snapshot_year = c(2024L, 2024L), issue_year = c(2024L, 2024L),
    canonical_iso3 = c("AAA", "AAA"), ISIN = c("IA", "IB"),
    IsCallable = c(TRUE, FALSE), IsPutable = FALSE,
    IsSinkable = c(FALSE, TRUE), IsConvertible = FALSE,
    RCSCouponTypeLeaf = c("Fixed", "Floating Rate"),
    CouponTypeDescription = "", DebtTypeDescription = "",
    InstrumentTypeDescription = "Bond", DocumentTitle = c("Callable", "Sukuk")
  )
  result <- p15_classify_primary_instrument_features(instruments)
  expect_true(result$feature_callable[[1]])
  expect_true(result$feature_sinkable[[2]])
  expect_true(result$feature_floating_or_variable[[2]])
  expect_true(result$feature_sukuk[[2]])
})

test_that("feature-rich rows are diagnostic and never selected automatically", {
  issues <- tibble(
    analysis_year = 2024L, iso3 = "AAA", country = "Alpha",
    country_year_id = "2024::AAA", historical_lmic_reporting_scope = TRUE,
    economic_issue_key = "issue", representative_isins = "IA", currency = "USD",
    original_maturity_years = 10, original_issue_yield_pct = 7,
    face_issued_usd = 100000000, usd_weight_available = TRUE,
    identifier_yield_conflict = FALSE, bill_or_strip_flag = FALSE,
    central_bank_flag = FALSE, restructuring_flag = FALSE,
    nonstandard_feature_flag = TRUE, candidate_primary_standard = FALSE,
    candidate_primary_standard_50m = FALSE
  )
  instruments <- tibble(
    snapshot_year = 2024L, issue_year = 2024L, canonical_iso3 = "AAA",
    ISIN = "IA", IsCallable = TRUE, IsPutable = FALSE, IsSinkable = FALSE,
    IsConvertible = FALSE, RCSCouponTypeLeaf = "Fixed",
    CouponTypeDescription = "", DebtTypeDescription = "",
    InstrumentTypeDescription = "Bond", DocumentTitle = "Callable bond"
  )
  result <- p15_primary_issue_feature_diagnostic(issues, instruments)
  expect_true(result$feature_rich_nonrestructuring_review)
  expect_true(result$candidate_primary_fixed_call_sink_review)
  expect_true(result$candidate_primary_fixed_call_sink_50m_review)
  expect_match(result$feature_profile, "callable")
  expect_false(result$selected_for_ladder)
  expect_equal(result$automatic_consequence, "none")
})

test_that("fixed call-sink expansion remains a non-selecting comparison", {
  audit <- tibble(
    analysis_year = c(2024L, 2024L), iso3 = "AAA", country = "Alpha",
    country_year_id = "2024::AAA", historical_lmic_reporting_scope = TRUE,
    original_issue_yield_pct = c(5, 7), face_issued_usd = c(100, 100),
    candidate_primary_standard_50m = c(TRUE, FALSE),
    candidate_primary_fixed_call_sink_50m_review = c(FALSE, TRUE)
  )
  result <- p15_compare_primary_fixed_call_sink_candidate(audit)
  expect_equal(result$baseline_rate_pct, 5)
  expect_equal(result$expanded_rate_pct, 6)
  expect_equal(result$rate_difference_pp, 1)
  expect_false(result$selected_for_ladder)
})
