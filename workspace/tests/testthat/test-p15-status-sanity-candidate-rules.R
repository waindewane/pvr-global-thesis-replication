suppressPackageStartupMessages({
  library(dplyr)
  library(testthat)
  library(tibble)
})

source("../../R/p15_status_sanity_candidate_rules.R")

test_that("nearest available Treasury tenor and prior date are deterministic", {
  curve <- tibble(
    observation_date = as.Date(c("2024-01-02", "2024-01-02", "2024-01-02")),
    risk_free_rate_pct = c(4, 4.1, 4.2),
    reference_tenor_years = c(5, 7, 10),
    reference_series_id = c("DGS5", "DGS7", "DGS10")
  )
  result <- p15_match_prior_treasury_rate(
    as.Date(c("2024-01-03", "2024-01-03", "2024-01-03")),
    c(4, 7, 12), curve
  )
  expect_equal(result$reference_tenor_years, c(5, 7, 10))
  expect_equal(result$risk_free_lag_days, c(1L, 1L, 1L))
  expect_equal(result$risk_free_rate_pct, c(4, 4.1, 4.2))
})

test_that("IMF flag requires level doubling and comparable prior year", {
  issues <- tibble(
    evidence_object = "observed_secondary_direct_usd_2_15",
    analysis_year = c(2022L, 2023L, 2024L), iso3 = "AAA", country = "Alpha",
    country_year_id = paste0(2022:2024, "::AAA"),
    historical_lmic_reporting_scope = TRUE,
    economic_issue_key = paste0("issue", 1:3), rate_date = as.Date(c(
      "2022-12-30", "2023-12-29", "2024-12-30"
    )),
    maturity_years = c(7, 7.5, 7.5), rate_pct = c(5, 8, 15),
    weight_usd = 100, source_object = "direct", source_package_ids = "source",
    source_record_locators = "row", reference_tenor_years = 7,
    risk_free_observation_date = as.Date(c(
      "2022-12-30", "2023-12-29", "2024-12-30"
    )),
    risk_free_rate_pct = c(2, 2, 2), risk_free_series_id = "DGS7",
    risk_free_lag_days = 0L, positive_country_year_median_rate_pct = c(5, 8, 15),
    technical_range_flag = FALSE, ten_times_positive_median_flag = FALSE,
    technical_data_error_review_flag = FALSE, low_rate_source_review_flag = FALSE,
    above_30_context_review_flag = FALSE,
    sovereign_spread_pct = c(3, 6, 13), risk_free_match_available = TRUE,
    automatic_consequence = "none", check_decision_state = "candidate_not_approved",
    rate_review_id = paste0("id", 1:3), schema_version = "v", build_id = "b"
  )
  result <- p15_build_imf_spread_review_flags(issues)
  expect_false(result$imf_500bps_plus_doubling_review_flag[[1]])
  expect_true(result$imf_500bps_plus_doubling_review_flag[[2]])
  expect_true(result$imf_500bps_plus_doubling_review_flag[[3]])
  expect_true(all(result$automatic_consequence == "none"))
})

test_that("candidate actions never create automatic selection or PVR blocks", {
  spread <- tibble(
    evidence_object = "observed_primary_standard_usd", analysis_year = 2024L,
    iso3 = "AAA", country = "Alpha", country_year_id = "2024::AAA",
    historical_lmic_reporting_scope = TRUE, market_rate_pct = 40,
    market_maturity_years = 7, risk_free_rate_pct = 4,
    sovereign_spread_pct = 36, issue_count = 1L,
    technical_data_error_review_flag = FALSE, low_rate_source_review_flag = FALSE,
    above_30_context_review_flag = TRUE, reference_series_ids = "DGS7",
    prior_analysis_year = 2023L, prior_sovereign_spread_pct = 10,
    prior_market_maturity_years = 7, consecutive_prior_year_available = TRUE,
    maturity_comparable_to_prior = TRUE, comparable_prior_year_available = TRUE,
    spread_at_least_500bps = TRUE, spread_at_least_double_prior = TRUE,
    imf_500bps_plus_doubling_review_flag = TRUE,
    imf_flag_interpretation = "review", automatic_consequence = "none",
    review_decision_state = "candidate_not_approved", schema_version = "v",
    build_id = "b"
  )
  status <- tibble(
    analysis_year = integer(), iso3 = character(), review_basis = character(),
    recommended_treatment_class = character(),
    recommended_admissibility_action = character(),
    recommended_display_action = character(), case_level_note = character(),
    recommendation_state = character()
  )
  result <- p15_integrate_status_sanity_candidate_actions(spread, status)
  expect_equal(
    result$candidate_combined_action,
    "candidate_contextual_market_stress_review"
  )
  expect_false(result$automatic_selection_block)
  expect_false(result$automatic_pvr_block)
  expect_false(result$final_consequence_decided)
})

test_that("display evidence retains status and numerical warnings together", {
  spread <- tibble(
    evidence_object = "observed_secondary_direct_usd_2_15",
    analysis_year = 2024L, iso3 = "AAA", country = "Alpha",
    country_year_id = "2024::AAA", historical_lmic_reporting_scope = TRUE,
    market_rate_pct = 120, market_maturity_years = 7,
    risk_free_rate_pct = 4, sovereign_spread_pct = 116, issue_count = 1L,
    technical_data_error_review_flag = TRUE,
    low_rate_source_review_flag = FALSE, above_30_context_review_flag = TRUE,
    reference_series_ids = "DGS7", prior_analysis_year = 2023L,
    prior_sovereign_spread_pct = 10, prior_market_maturity_years = 7,
    consecutive_prior_year_available = TRUE,
    maturity_comparable_to_prior = TRUE, comparable_prior_year_available = TRUE,
    spread_at_least_500bps = TRUE, spread_at_least_double_prior = TRUE,
    imf_500bps_plus_doubling_review_flag = TRUE,
    imf_flag_interpretation = "review", automatic_consequence = "none",
    review_decision_state = "candidate_not_approved", schema_version = "v",
    build_id = "b"
  )
  status <- tibble(
    analysis_year = 2024L, iso3 = "AAA", review_basis = "status",
    recommended_treatment_class = "context",
    recommended_admissibility_action = "review",
    recommended_display_action = "Display the status context.",
    case_level_note = "note", recommendation_state = "candidate"
  )
  result <- p15_integrate_status_sanity_candidate_actions(spread, status)
  expect_match(result$candidate_display_action, "status context")
  expect_match(result$candidate_display_action, "technical source-review")
})
