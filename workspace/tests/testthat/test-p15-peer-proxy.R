suppressPackageStartupMessages({
  library(dplyr)
  library(testthat)
})

source("../../R/p15_peer_proxy.R")

test_that("peer candidates exclude the target and proxy-derived seeds", {
  grid <- tibble::tibble(
    analysis_year = rep(2024L, 4),
    iso3 = c("AAA", "BBB", "CCC", "DDD"),
    country = c("A", "B", "C", "D"),
    country_year_id = paste0("2024::", c("AAA", "BBB", "CCC", "DDD")),
    historical_income_level = c("Low", "Low", "Low", "High"),
    historical_lmic_reporting_scope = c(TRUE, TRUE, TRUE, FALSE),
    positive_status_context = c(FALSE, FALSE, TRUE, FALSE)
  )
  seeds <- grid |>
    transmute(
      analysis_year, iso3, country, historical_income_level,
      positive_status_context,
      seed_pool_class = "strict_observed",
      seed_priority = 10L,
      seed_rate_pct = c(2, 4, 8, 6),
      seed_maturity_years = 7,
      seed_source_family = "observed_primary",
      seed_source_tier = "test_primary",
      seed_source_package_id = "TEST",
      seed_record_locator = paste0("row::", iso3),
      seed_is_model_or_proxy = FALSE,
      seed_dependency_state = "current_test"
    )
  variants <- tibble::tibble(
    peer_variant_id = "TEST-INC3",
    seed_rule = "strict_observed",
    minimum_same_income_count = 3L,
    exclude_positive_status_context = FALSE,
    calculation_state = "computed_candidate"
  )
  out <- p15_build_peer_candidates(grid, seeds, variants)
  expect_equal(nrow(out$candidates), 4L)
  expect_true(all(out$candidates$target_excluded))
  expect_true(all(out$candidates$nonrecursive_seed_pool))
  expect_false(any(out$members$target_iso3 == out$members$peer_iso3))
  expect_false(any(out$members$recursive_seed_flag))
})

test_that("status-excluded variants remove flagged peer members", {
  grid <- tibble::tibble(
    analysis_year = rep(2024L, 3), iso3 = c("AAA", "BBB", "CCC"),
    country = c("A", "B", "C"),
    country_year_id = paste0("2024::", c("AAA", "BBB", "CCC")),
    historical_income_level = "Low",
    historical_lmic_reporting_scope = TRUE,
    positive_status_context = c(FALSE, TRUE, FALSE)
  )
  seeds <- grid |>
    transmute(
      analysis_year, iso3, country, historical_income_level,
      positive_status_context,
      seed_pool_class = "strict_observed", seed_priority = 10L,
      seed_rate_pct = c(2, 20, 8), seed_maturity_years = 7,
      seed_source_family = "observed_primary", seed_source_tier = "test",
      seed_source_package_id = "TEST", seed_record_locator = iso3,
      seed_is_model_or_proxy = FALSE, seed_dependency_state = "current_test"
    )
  variants <- tibble::tibble(
    peer_variant_id = "TEST-STATUS-EXCL", seed_rule = "strict_observed",
    minimum_same_income_count = 2L,
    exclude_positive_status_context = TRUE,
    calculation_state = "computed_candidate"
  )
  out <- p15_build_peer_candidates(grid, seeds, variants)
  expect_false(any(out$members$peer_positive_status_context))
})

test_that("model or proxy seeds fail closed", {
  seed <- tibble::tibble(
    analysis_year = 2024L, iso3 = "AAA", country = "A",
    historical_income_level = "Low", positive_status_context = FALSE,
    seed_pool_class = "strict_observed", seed_priority = 10L,
    seed_rate_pct = 5, seed_maturity_years = 7,
    seed_source_family = "peer_proxy", seed_source_tier = "bad",
    seed_source_package_id = "TEST", seed_record_locator = "bad",
    seed_is_model_or_proxy = TRUE, seed_dependency_state = "test"
  )
  expect_error(
    p15_peer_validate_seed_ledger(seed),
    "model or proxy-derived seed"
  )
})

test_that("validation requires unique target-anchor rows", {
  candidates <- tibble::tibble(
    analysis_year = 2024L, iso3 = "AAA", peer_rate_median_pct = 6,
    target_excluded = TRUE, nonrecursive_seed_pool = TRUE
  )
  anchors <- tibble::tibble(
    analysis_year = c(2024L, 2024L), iso3 = c("AAA", "AAA"),
    anchor_family = c("primary", "primary"), anchor_rate_pct = c(5, 5),
    anchor_maturity_years = c(7, 7), anchor_quality_state = "test"
  )
  expect_error(p15_build_peer_validation(candidates, anchors), "not unique")
})
