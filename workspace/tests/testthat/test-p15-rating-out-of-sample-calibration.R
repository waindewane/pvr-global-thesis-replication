suppressPackageStartupMessages({library(dplyr); library(testthat)})
source("../../R/p15_rating_out_of_sample_calibration.R")

test_that("calibration predictions use training coefficients", {
  train <- tibble::tibble(
    observed_rate_pct = c(3, 5, 7), rating_rate_pct = c(2, 4, 6),
    risk_free_rate_pct = 1, rating_spread_pct = c(1, 3, 5)
  )
  test <- tibble::tibble(
    observed_rate_pct = 100, rating_rate_pct = 8,
    risk_free_rate_pct = 1, rating_spread_pct = 7
  )
  out <- p15_fit_rating_calibration(train, test, "affine_rate")
  expect_equal(out$prediction, 9, tolerance = 1e-10)
  expect_equal(out$intercept, 1, tolerance = 1e-10)
  expect_equal(out$slope, 1, tolerance = 1e-10)
})

test_that("OOS builder creates all three leakage-labelled designs", {
  x <- tidyr::expand_grid(
    analysis_year = 2012:2024, iso3 = c("AAA", "BBB", "CCC")
  ) |>
    mutate(
      country = iso3,
      rating_rate_pct = 3 + (analysis_year - 2012) / 10 + as.numeric(factor(iso3)),
      observed_rate_pct = 1 + 0.8 * rating_rate_pct,
      risk_free_rate_pct = 1,
      rating_spread_pct = rating_rate_pct - 1
    )
  out <- p15_build_rating_oos_predictions(x)
  expect_setequal(unique(out$validation_design), c(
    "fixed_future_2020_2024", "expanding_past_only", "leave_one_country_out"
  ))
  expect_setequal(unique(out$model_id), c("raw", "affine_rate", "spread_component"))
  expect_true(all(out$training_last_year[out$validation_design == "expanding_past_only"] <
                    out$analysis_year[out$validation_design == "expanding_past_only"]))
})

test_that("paired inference defines improvement as a negative change", {
  x <- tidyr::expand_grid(
    validation_design = "x", analysis_year = 2021:2024,
    iso3 = c("AAA", "BBB"), model_id = c("raw", "affine_rate")
  ) |>
    mutate(
      absolute_error_pp = if_else(
        model_id == "raw", 2,
        0.7 + 0.1 * (analysis_year - 2021) + if_else(iso3 == "BBB", 0.05, 0)
      )
    )
  out <- p15_rating_oos_paired_inference(x)
  expect_lt(out$mean_absolute_error_change_pp, 0)
})
