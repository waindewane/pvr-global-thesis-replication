suppressPackageStartupMessages({
  library(dplyr)
  library(testthat)
  library(tibble)
  library(tidyr)
})

source("../../R/p15_secondary_maturity_diagnostic.R")

test_that("detailed maturity scenarios separate near and long extensions", {
  register <- p15_secondary_maturity_scenarios_detailed()
  expect_equal(nrow(register), 8L)
  expect_true(all(c(
    "usd_2_16_near_cutoff", "usd_2_17_near_cutoff",
    "usd_2_20_medium_extension", "usd_2_30_long_extension"
  ) %in% register$scenario_id))
  expect_equal(
    p15_secondary_maturity_bin(c(15.5, 16.5, 18, 25, 31)),
    c("over_15_to_16", "over_16_to_17", "over_17_to_20",
      "over_20_to_30", "over_30")
  )
})

test_that("scenario rates use outstanding amount weights", {
  issues <- tibble(
    analysis_year = 2024L, iso3 = "AAA", country = "Alpha",
    country_year_id = "2024::AAA", period = "2024",
    historical_income_level = "Lower middle income",
    historical_lmic_reporting_scope = TRUE,
    economic_issue_key = c("base", "near", "long"), currency = "USD",
    remaining_maturity_years = c(10, 15.5, 25),
    face_outstanding_usd = c(100, 100, 300),
    direct_yield_pct = c(5, 7, 9), candidate_secondary_standard = TRUE,
    identifier_count = 1L, representative_rics = c("A", "B", "C"),
    representative_isins = c("IA", "IB", "IC"), asset_statuses = "ISS",
    direct_quote_date = as.Date("2024-12-31")
  )
  rates <- p15_build_secondary_maturity_scenario_rates_detailed(issues)
  baseline <- rates |> filter(scenario_id == "usd_2_15_baseline_candidate")
  near <- rates |> filter(scenario_id == "usd_2_16_near_cutoff")
  long <- rates |> filter(scenario_id == "usd_2_30_long_extension")
  expect_equal(baseline$market_rate_pct, 5)
  expect_equal(near$market_rate_pct, 6)
  expect_equal(long$market_rate_pct, 7.8)
})

test_that("membership audit identifies the issue causing the change", {
  issues <- tibble(
    analysis_year = 2024L, iso3 = "AAA", country = "Alpha",
    country_year_id = "2024::AAA", period = "2024",
    historical_income_level = "Lower middle income",
    historical_lmic_reporting_scope = TRUE,
    economic_issue_key = c("base", "near"), currency = "USD",
    remaining_maturity_years = c(10, 15.5),
    face_outstanding_usd = c(100, 100), direct_yield_pct = c(5, 7),
    candidate_secondary_standard = TRUE, identifier_count = 1L,
    representative_rics = c("A", "B"), representative_isins = c("IA", "IB"),
    asset_statuses = "ISS", direct_quote_date = as.Date("2024-12-31")
  )
  rates <- p15_build_secondary_maturity_scenario_rates_detailed(issues)
  changes <- p15_build_secondary_maturity_membership_changes(issues, rates)
  row <- changes |>
    filter(scenario_id == "usd_2_16_near_cutoff")
  expect_equal(nrow(row), 1L)
  expect_equal(row$economic_issue_key, "near")
  expect_equal(row$membership_change, "entered")
  expect_equal(row$issue_weight_share_in_scenario, 0.5)
})
