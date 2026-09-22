suppressPackageStartupMessages({
  library(dplyr)
  library(testthat)
  library(tibble)
})

source("../../R/p15_primary_closure_evidence.R")

test_that("primary materiality comparison reports coverage and rate effects", {
  variants <- tibble(
    analysis_year = c(2024L, 2024L, 2024L),
    iso3 = c("AAA", "AAA", "BBB"),
    country = c("Alpha", "Alpha", "Beta"),
    country_year_id = c("2024::AAA", "2024::AAA", "2024::BBB"),
    historical_lmic_reporting_scope = TRUE,
    market_rate_pct = c(5, 5.1, 7),
    issue_count = c(1L, 2L, 1L),
    total_weight_value = c(100, 110, 10),
    candidate_variant_id = c(
      "primary_standard_usd_eur_50m",
      "primary_standard_usd_eur_all_issue_counts",
      "primary_standard_usd_eur_all_issue_counts"
    )
  )
  result <- p15_compare_primary_materiality_variants(variants)
  expect_equal(result$summary$main_country_years, 1L)
  expect_equal(result$summary$all_issue_country_years, 2L)
  expect_equal(result$summary$all_issue_only, 1L)
  expect_equal(result$summary$maximum_absolute_change_pp, 0.1)
  expect_equal(result$coverage_differences$iso3, "BBB")
  expect_equal(result$rate_differences$iso3, "AAA")
  expect_false(any(result$rate_differences$selected_for_ladder))
})

test_that("primary materiality comparison fails on incomplete inputs", {
  expect_error(
    p15_compare_primary_materiality_variants(tibble(analysis_year = 2024L)),
    "missing columns"
  )
})
