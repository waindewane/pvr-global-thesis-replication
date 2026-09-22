suppressPackageStartupMessages({
  library(dplyr)
  library(testthat)
})

source("../../R/p15_rating_residual_diagnostics.R")

test_that("residual sample uses rating minus observed convention", {
  x <- tibble::tibble(
    validation_question_id = "Q-USD-NEW-BORROWING",
    variant_id = "boy_available_agencies_median_mapped_spread_dgs7",
    historical_lmic_reporting_scope = TRUE,
    accuracy_summary_eligible = TRUE,
    analysis_year = 2024L, iso3 = "AAA", country = "A",
    anchor_rate_pct = 5, variant_rate_pct = 6, signed_gap_pp = 1,
    anchor_thin_evidence = FALSE,
    historical_income_level = "Low income",
    anchor_status_rule_class = "no_registered_status_case"
  )
  out <- p15_prepare_rating_residual_sample(x)
  expect_equal(out$residual_pp, 1)
  expect_equal(out$residual_direction, "Rating-implied rate higher")
})

test_that("country summaries classify repeated directional patterns", {
  x <- tibble::tibble(
    analysis_year = rep(2022:2024, 2),
    iso3 = rep(c("AAA", "BBB"), each = 3),
    country = rep(c("A", "B"), each = 3),
    residual_pp = c(1, 2, 3, -1, -0.5, -1)
  )
  out <- p15_summarise_country_residuals(x)
  expect_equal(out$directional_pattern[out$iso3 == "AAA"],
               "predominantly_rating_implied_higher")
  expect_true(out$all_positive[out$iso3 == "AAA"])
  expect_equal(out$directional_pattern[out$iso3 == "BBB"],
               "predominantly_rating_implied_lower")
})

test_that("country permutation test is reproducible", {
  x <- tidyr::expand_grid(
    analysis_year = 2022:2024,
    iso3 = c("AAA", "BBB", "CCC")
  ) |>
    mutate(
      residual_pp = rep(c(-1, 0, 1), each = 3),
      country = iso3
    )
  a <- p15_country_effect_permutation_test(x, simulations = 99L, seed = 42L)
  b <- p15_country_effect_permutation_test(x, simulations = 99L, seed = 42L)
  expect_equal(a, b)
  expect_true(a$p_value >= 0 && a$p_value <= 1)
})
