suppressPackageStartupMessages({library(testthat); library(tibble)})
source(testthat::test_path("..", "..", "R", "p15_local_completion.R"))
source(testthat::test_path("..", "..", "R", "p15_ladder_preview.R"))
test_that("preview order, review switches and explicit status blocks work", {
  x <- tibble(analysis_year = 2024L, iso3 = c("AAA", "BBB", "CCC", "DDD"), country = "test",
    historical_income_level = "Low income", historical_lmic_reporting_scope = TRUE,
    status_review_coverage = "case_record", peer_pool_rule = "global", peer_country_count = 5L,
    peer_iqr_pp = 2, observed_benchmark_selection_permitted = c(TRUE, TRUE, FALSE, TRUE),
    ordinary_fallback_selection_permitted = c(TRUE, TRUE, FALSE, TRUE),
    primary_usd_market_rate_pct = c(5, NA, 7, NA), ids_rate_pct = c(4, .5, 5, NA),
    secondary_usd_market_rate_pct = c(6, 8, 9, NA), rating_moodys_rate_pct = c(5, 6, 7, NA),
    peer_rate_pct = 6, ids_positive_rate_observed = c(TRUE, TRUE, TRUE, FALSE),
    ids_legacy_low_rate_rule_conflict = c(FALSE, TRUE, FALSE, FALSE),
    rating_moodys_available = c(TRUE, TRUE, TRUE, FALSE), peer_minimum_met = TRUE,
    primary_usd_source_evidence_row_id = "primary", secondary_usd_source_evidence_row_id = "secondary",
    ids_evidence_id = "ids", rating_moodys_variant_id = "moodys", peer_evidence_id = "peer")
  held <- p15_local_ladder_preview(x)
  expect_equal(held$preview_source, c("primary", "secondary", "no_eligible_rate", "no_eligible_rate"))
  open <- p15_local_ladder_preview(x, hold_low_ids_for_review = FALSE, include_broad_peer = TRUE)
  expect_equal(open$preview_source, c("primary", "ids", "no_eligible_rate", "peer"))
  expect_equal(open$preview_rate_pct[2], .5)
  expect_equal(open$preview_currency_basis[2], "IDS_currency_composition_unknown")
  swapped <- p15_local_ladder_preview(x, ids_before_secondary = FALSE, hold_low_ids_for_review = FALSE)
  expect_equal(swapped$preview_source[2], "secondary")
  expect_false(any(open$approved_selection))
})
