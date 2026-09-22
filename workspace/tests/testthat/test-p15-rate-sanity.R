suppressPackageStartupMessages({
  library(dplyr)
  library(testthat)
  library(tibble)
  library(tidyr)
})

source("../../R/p15_rate_sanity.R")

test_that("rate-sanity thresholds are parameterized and decision-neutral", {
  parameters <- p15_rate_sanity_parameters()
  expect_equal(dplyr::n_distinct(parameters$sanity_variant_id), 6L)
  expect_true(all(c(
    "baseline_1_30", "zero_30", "zero_40", "negative_1_to_30",
    "hyperinflation_1_100", "source_specific"
  ) %in% parameters$sanity_variant_id))
  expect_true(all(parameters$consequence_decision_state == "not_evaluated"))
  expect_false(any(is.na(parameters$lower_bound_pct)))
  expect_false(any(is.na(parameters$upper_bound_pct)))
})

test_that("rate-sanity variants classify boundaries and source objects", {
  universe <- tibble::tibble(
    rate_evidence_id = c("A", "B", "C", "D"),
    analysis_year = c(2020L, 2024L, 2024L, 2024L),
    iso3 = c("AAA", "BBB", "CCC", "DDD"),
    country = c("Alpha", "Beta", "Gamma", "Delta"),
    evidence_family = c(
      "observed_primary_issuance", "ids_bondholders_public_proxy",
      "peer_proxy", "observed_secondary_market_evidence"
    ),
    source_object = c("primary_yield", "bondholder_terms", "peer", "secondary"),
    rate_pct = c(-0.1, 0.5, 35, 101)
  )
  out <- p15_apply_rate_sanity_variants(universe)

  expect_equal(nrow(out), 24L)
  expect_false(anyDuplicated(out[c("rate_evidence_id", "sanity_variant_id")]) > 0L)
  baseline <- out |>
    dplyr::filter(.data$sanity_variant_id == "baseline_1_30")
  expect_equal(
    baseline$sanity_state,
    c("below_lower_bound", "below_lower_bound", "above_upper_bound", "above_upper_bound")
  )
  source_specific <- out |>
    dplyr::filter(.data$sanity_variant_id == "source_specific")
  expect_true(source_specific$threshold_pass[source_specific$rate_evidence_id == "A"])
  expect_true(source_specific$threshold_pass[source_specific$rate_evidence_id == "B"])
  expect_true(source_specific$threshold_pass[source_specific$rate_evidence_id == "C"])
  expect_false(source_specific$threshold_pass[source_specific$rate_evidence_id == "D"])
  expect_true(all(out$sanity_decision_state == "not_evaluated"))
})

test_that("duplicate evidence IDs fail closed", {
  duplicated_universe <- tibble::tibble(
    rate_evidence_id = c("A", "A"),
    analysis_year = c(2024L, 2024L),
    iso3 = c("AAA", "AAA"),
    country = c("Alpha", "Alpha"),
    evidence_family = c("peer_proxy", "peer_proxy"),
    source_object = c("peer", "peer"),
    rate_pct = c(5, 6)
  )
  expect_error(
    p15_apply_rate_sanity_variants(duplicated_universe),
    "duplicate evidence IDs"
  )
})
