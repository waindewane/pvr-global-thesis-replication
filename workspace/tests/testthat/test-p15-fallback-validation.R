suppressPackageStartupMessages({
  library(dplyr)
  library(testthat)
})

source("../../R/p15_fallback_validation.R")

test_that("anchor authority keeps empirical objects and currencies separate", {
  anchors <- tibble::tibble(
    analysis_year = rep(2024L, 4),
    iso3 = c("AAA", "BBB", "CCC", "DDD"),
    country = c("A", "B", "C", "D"),
    country_year_id = paste0("2024::", iso3),
    currency = c("USD", "EUR", "USD", "USD"),
    observed_market_branch = c(
      "observed_primary", "observed_primary",
      "observed_secondary", "observed_secondary"
    ),
    candidate_evidence_tier = c(
      "direct_original_issue_yield", "direct_original_issue_yield",
      "direct_yield_to_maturity", "price_derived_secondary"
    ),
    market_rate_pct = c(5, 4, 6, 7),
    market_maturity_years = 7,
    thin_evidence = FALSE,
    evidence_strength_label = "test",
    source_evidence_row_id = paste0("ROW-", iso3),
    historical_lmic_reporting_scope = TRUE,
    approved_status_rule_class = "no_registered_status_case",
    timing_object = c(
      "primary_issue_date", "primary_issue_date",
      "year_end_window", "year_end_window"
    )
  )
  out <- p15_expand_validation_anchor_authority(anchors)
  expect_equal(nrow(out), 16L)

  new_cost <- out |>
    filter(validation_question_id == "Q-USD-NEW-BORROWING")
  expect_equal(
    new_cost$authority_class[new_cost$iso3 == "AAA"],
    "preferred_provisional_accuracy_anchor"
  )
  expect_false(new_cost$use_in_provisional_summary[new_cost$iso3 == "BBB"])
  expect_false(new_cost$use_in_provisional_summary[new_cost$iso3 == "CCC"])

  year_end <- out |>
    filter(validation_question_id == "Q-USD-YEAR-END-MARKET")
  expect_equal(
    year_end$authority_class[year_end$iso3 == "CCC"],
    "preferred_provisional_accuracy_anchor"
  )
  expect_equal(
    year_end$authority_class[year_end$iso3 == "DDD"],
    "supporting_reconstructed_secondary_anchor"
  )
  expect_true(all(year_end$use_in_provisional_summary[year_end$iso3 %in% c(
    "CCC", "DDD"
  )]))
})

test_that("IDS authority is descriptive rather than an accuracy claim", {
  anchors <- tibble::tibble(
    analysis_year = 2024L, iso3 = "AAA", country = "A",
    country_year_id = "2024::AAA", currency = "USD",
    observed_market_branch = "observed_primary",
    candidate_evidence_tier = "direct_original_issue_yield",
    market_rate_pct = 5, market_maturity_years = 7,
    thin_evidence = FALSE, evidence_strength_label = "test",
    source_evidence_row_id = "ROW-AAA",
    historical_lmic_reporting_scope = TRUE,
    approved_status_rule_class = "no_registered_status_case",
    timing_object = "primary_issue_date"
  )
  ids <- p15_expand_validation_anchor_authority(anchors) |>
    filter(validation_question_id == "Q-IDS-SOURCE-CONSISTENCY")
  expect_equal(
    ids$authority_class, "descriptive_source_object_overlap_only"
  )
  expect_match(ids$authority_reason, "not treated as ground truth")
})

test_that("fallback summaries report coverage and error distribution", {
  x <- tibble::tibble(
    group = "A", signed_gap_pp = c(-1, 3),
    analysis_year = c(2023L, 2024L), iso3 = c("AAA", "BBB"),
    historical_lmic_reporting_scope = c(TRUE, FALSE)
  )
  out <- p15_summarise_fallback_gaps(x, "group")
  expect_equal(out$matched_rows, 2L)
  expect_equal(out$lmic_rows, 1L)
  expect_equal(out$mean_signed_gap_pp, 1)
  expect_equal(out$mean_abs_gap_pp, 2)
  expect_equal(out$rmse_gap_pp, sqrt(5))
  expect_equal(out$within_1pp_share, 0.5)
})
