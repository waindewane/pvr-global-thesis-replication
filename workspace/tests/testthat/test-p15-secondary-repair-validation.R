suppressPackageStartupMessages({
  library(dplyr)
  library(testthat)
  library(tibble)
})

source("../../R/p15_observed_secondary.R")
source("../../R/p15_secondary_repair_validation.R")

test_that("clean-price yield solver reproduces a par fixed-coupon bond", {
  ytm <- p15_secondary_ytm_from_clean_price(
    clean_price = 100,
    settle_date = as.Date("2025-01-01"),
    maturity_date = as.Date("2030-01-01"),
    coupon_rate_pct = 5,
    frequency = 1
  )
  expect_equal(ytm, 5, tolerance = 0.02)
})

test_that("P15 formula reproduces preserved same-instrument calculations", {
  root <- normalizePath(file.path("..", ".."), winslash = "/", mustWork = TRUE)
  detail <- data.table::fread(file.path(
    root,
    "experiments/full_ladder_ratings_integrated_status_rebuild_2026-06-01",
    "outputs/secondary_price_field_semantics_crosscheck_detail_2024.csv"
  )) |>
    tibble::as_tibble()
  rebuilt <- p15_recompute_secondary_price_semantics(detail)
  expect_equal(nrow(rebuilt), 2243L)
  expect_true(all(rebuilt$formula_reproduction_pass))
  expect_lte(max(rebuilt$formula_reproduction_abs_diff_bps), 1e-8)
  expect_true(all(rebuilt$method_decision_state == "not_evaluated"))
})

test_that("semantics summary keeps strict and broad comparisons separate", {
  detail <- tibble::tibble(
    evidence_surface = c("A", "A"),
    coupon_frequency_assumption = c(1L, 1L),
    strict_plainish_subset = c(TRUE, FALSE),
    ric = c("RIC1", "RIC2"),
    formula_reproduction_pass = c(TRUE, TRUE),
    direct_ytm_abs_error_recomputed_bps = c(2, 100),
    direct_in_clean_bid_ask_bracket_5bps = c(TRUE, FALSE)
  )
  summary <- p15_secondary_repair_validation_summary(detail)
  expect_equal(nrow(summary), 2L)
  expect_equal(
    summary$median_abs_error_clean_bps[summary$strict_plainish_subset],
    2
  )
  expect_true(all(summary$repair_decision_state == "not_evaluated"))
})
