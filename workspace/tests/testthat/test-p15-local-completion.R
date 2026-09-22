suppressPackageStartupMessages({library(testthat); library(dplyr); library(tibble)})
source(testthat::test_path("..", "..", "R", "p15_bounded_fallback_comparison.R"))
source(testthat::test_path("..", "..", "R", "p15_local_completion.R"))

test_that("historical joins restore blanks and refuse conflicting populated labels", {
  grid <- tibble(analysis_year = c(2012L, 2013L), iso3 = "AAA", country = "A",
    country_year_id = c("2012::AAA", "2013::AAA"),
    historical_income_level = c("Low income", "Lower middle income"),
    historical_lmic_reporting_scope = TRUE)
  x <- grid[c("analysis_year", "iso3")]
  x$historical_income_level <- NA_character_
  x$rate <- c(4, 5)
  result <- p15_local_classify(x, grid)
  expect_identical(result$historical_income_level, grid$historical_income_level)
  expect_identical(result$rate, x$rate)
  x$historical_income_level <- "High income"
  expect_error(p15_local_classify(x, grid), "classification conflict")
  expect_error(p15_local_classify(x, bind_rows(grid, grid[1, ])), "duplicate keys")
  x$iso3 <- "ZZZ"
  expect_error(p15_local_classify(x, grid), "outside master grid")
})

test_that("IDS low positive rates survive separately from term validity", {
  ids <- tibble(analysis_year = 2012L, iso3 = c("AAA", "BBB", "CCC"),
    official_rate_pct = c(.5, 0, 4), official_maturity_years = c(5, 0, 3),
    official_grace_years = c(5, 0, 4),
    bondholder_term_validity_state = "legacy", ids_missingness_state = "present",
    ids_evidence_id = c("a", "b", "c"), ids_source_package_id = "source")
  x <- p15_local_ids_fields(ids)
  expect_identical(x$ids_rate_pct, c(.5, 0, 4))
  expect_identical(x$ids_legacy_low_rate_rule_conflict, c(TRUE, FALSE, FALSE))
  expect_identical(x$ids_term_order_valid, c(TRUE, FALSE, FALSE))
  expect_true(x$ids_zero_rate_review[2])
})

test_that("peer target audit excludes own outcomes and handles unrated targets", {
  context <- tibble(analysis_year = 2020L, iso3 = c("AAA", "BBB", "CCC", "DDD"),
    moodys_rating_normalized = c("Ba2", "Ba2", "Ba2", NA_character_), rating_source_region = "R")
  anchors <- tibble(analysis_year = 2020L, iso3 = c("AAA", "BBB", "CCC"),
    observed_market_branch = "observed_primary", currency = "USD",
    market_rate_pct = c(5, 6, 7), sovereign_spread_pct = c(3, 4, 5),
    historical_income_level = "Lower middle income", thin_evidence = c(TRUE, FALSE, FALSE),
    source_evidence_row_id = c("a", "b", "c"), source_package_ids = "source",
    included_issue_keys = c("issue_a", "issue_b", "issue_c"))
  panel <- context[c("analysis_year", "iso3")] |>
    mutate(historical_income_level = "Lower middle income", historical_lmic_reporting_scope = TRUE,
      fallback_audit_cohort = "test", ordinary_fallback_selection_permitted = TRUE)
  result <- p15_audit_local_peer_targets(panel, anchors, context)
  expect_equal(result$detail$peer_rate_pct, c(6.5, 6, 5.5, 6))
  expect_identical(result$detail$peer_minimum_met, c(FALSE, FALSE, FALSE, TRUE))
  expect_true(result$detail$target_moodys_missing[4])
  expect_false(result$detail$rating_proximity_used[4])
  expect_false(any(result$membership$target_iso3 == result$membership$peer_iso3))
  anchors$market_rate_pct[1] <- 99
  again <- p15_audit_local_peer_targets(panel, anchors, context)
  expect_equal(again$detail$peer_rate_pct[1], result$detail$peer_rate_pct[1])
})
