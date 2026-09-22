suppressPackageStartupMessages({
  library(dplyr)
  library(testthat)
  library(tibble)
})

source("../../R/p15_observed_all_years.R")
source("../../R/p15_secondary_decision_evidence.R")

test_that("feature-rich evidence remains non-selecting and CALL rows block", {
  identifiers <- tibble(
    analysis_year = 2024L,
    ric = c("CALL1", "STEP1"),
    isin = c("ISIN1", "ISIN2"),
    iso3 = "AAA",
    country = "Alpha",
    country_year_id = "2024::AAA",
    historical_lmic_reporting_scope = TRUE,
    economic_issue_key = c("ISSUE1", "ISSUE2"),
    is_callable = c(TRUE, FALSE),
    is_putable = FALSE,
    is_sinkable = FALSE,
    is_convertible = FALSE,
    flag_short_bill_cp_like = FALSE,
    coupon_type_leaf = c("Fixed", "Step Up"),
    coupon_type_description = c("Fixed", "Step Up"),
    nonstandard_feature_flag = TRUE,
    direct_yield_available = c(TRUE, FALSE),
    any_price_available = TRUE,
    direct_yield_pct = c(6, NA_real_),
    remaining_maturity_years = c(7, 8),
    currency = "USD"
  )
  validation <- tibble(
    analysis_year = 2024L,
    terminal_identifier = "CALL1",
    matched_to_v12_cashflow_enrichment = TRUE,
    v12_worst_redem_event = "CALL",
    v12_pricing_mid_yield = 5.8,
    v12_yield_to_maturity_1 = 6
  )
  review <- tibble(
    analysis_year = 2024L,
    terminal_identifier = "CALL1",
    review_class = "call_event_needs_yield_to_worst_rule"
  )

  evidence <- p15_build_secondary_feature_evidence(
    identifiers, validation, review
  )
  callable <- p15_build_secondary_callable_audit(
    evidence$identifier_audit
  )

  expect_equal(nrow(evidence$identifier_audit), 2L)
  expect_match(evidence$identifier_audit$feature_types[[2]], "step_rate")
  expect_equal(
    callable$callable_yield_validation_state,
    "call_event_observed_but_explicit_ytw_ytc_semantics_unvalidated"
  )
  expect_false(callable$explicit_ytw_or_ytc_field_available)
  expect_true(all(!evidence$identifier_audit$selected_for_ladder))
})

test_that("maturity scenarios separate lower and upper bound effects", {
  issues <- tibble(
    analysis_year = 2024L,
    iso3 = "AAA",
    country = "Alpha",
    country_year_id = "2024::AAA",
    period = "2024",
    historical_income_level = "Lower middle income",
    historical_lmic_reporting_scope = TRUE,
    economic_issue_key = c("SHORT", "CORE", "LONG"),
    currency = "USD",
    remaining_maturity_years = c(1.5, 7, 20),
    face_outstanding_usd = c(100, 100, 100),
    direct_yield_pct = c(12, 6, 8),
    candidate_secondary_standard = TRUE,
    identifier_count = 1L
  )

  result <- p15_build_secondary_maturity_sensitivity(issues)
  rates <- setNames(result$market_rate_pct, result$scenario_id)

  expect_equal(rates[["usd_2_15_current_candidate"]], 6)
  expect_equal(rates[["usd_1_15_lower_bound_diagnostic"]], 9)
  expect_equal(rates[["usd_ge2_upper_bound_diagnostic"]], 7)
  expect_equal(rates[["usd_ge1_inherited_sensitivity"]], 26 / 3)
  expect_true(all(!result$selected_for_ladder))
})

test_that("quote scenarios use the latest observation inside each window", {
  identifiers <- tibble(
    analysis_year = 2024L,
    snapshot_date = as.Date("2024-12-31"),
    ric = "RIC1",
    iso3 = "AAA",
    country = "Alpha",
    country_year_id = "2024::AAA",
    period = "2024",
    historical_income_level = "Lower middle income",
    historical_lmic_reporting_scope = TRUE,
    economic_issue_key = "ISSUE1",
    currency = "USD",
    remaining_maturity_years = 7,
    face_outstanding_usd = 100,
    positive_outstanding = TRUE,
    nonstandard_feature_flag = FALSE,
    flag_central_bank_like = FALSE
  )
  history <- tibble(
    analysis_year = 2024L,
    history_date = as.Date(c("2024-12-20", "2024-12-31", "2025-01-07")),
    RIC = "RIC1",
    yield_to_maturity = c(7, 6.5, 6),
    source_package_id = "SRC",
    source_role = "base",
    source_record_locator = c("ROW1", "ROW2", "ROW3")
  )

  result <- p15_build_secondary_quote_sensitivity(history, identifiers)
  rates <- setNames(result$market_rate_pct, result$scenario_id)

  expect_equal(rates[["latest_available_current_common"]], 6)
  expect_equal(rates[["preferred_31_pre_7_post"]], 6)
  expect_equal(rates[["pre_year_end_31_only"]], 6.5)
  expect_equal(rates[["closest_to_year_end_31_pre_7_post"]], 6.5)
  expect_true(all(!result$selected_for_ladder))
})

test_that("scenario comparison repeats baseline-only rows for every alternative", {
  scenarios <- tibble(
    analysis_year = c(2024L, 2024L),
    iso3 = c("AAA", "AAA"),
    market_rate_pct = c(6, 7),
    market_maturity_years = c(7, 8),
    issue_count = 1L,
    scenario_id = c("base", "alternative_one")
  )
  scenarios <- bind_rows(
    scenarios,
    tibble(
      analysis_year = 2025L,
      iso3 = "BBB",
      market_rate_pct = 5,
      market_maturity_years = 6,
      issue_count = 1L,
      scenario_id = "base"
    )
  )
  compared <- p15_compare_secondary_scenarios(
    scenarios, "base", "test"
  )

  expect_equal(sum(compared$coverage_state == "baseline_only"), 1L)
  expect_equal(
    compared$scenario_id[compared$coverage_state == "baseline_only"],
    "alternative_one"
  )
})
