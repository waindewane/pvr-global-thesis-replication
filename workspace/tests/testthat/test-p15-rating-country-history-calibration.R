suppressPackageStartupMessages({
  library(dplyr); library(readr); library(testthat)
})
source("../../R/p15_rating_country_history_calibration.R")

synthetic_history_sample <- function() {
  tidyr::expand_grid(
    analysis_year = 2012:2024,
    iso3 = c("AAA", "BBB", "CCC", "DDD")
  ) |>
    dplyr::mutate(
      country = .data$iso3,
      rating_rate_pct = 4 + 0.15 * (.data$analysis_year - 2012) +
        as.numeric(factor(.data$iso3)) / 5,
      country_effect = dplyr::case_when(
        .data$iso3 == "AAA" ~ -0.6,
        .data$iso3 == "BBB" ~ 0.4,
        .data$iso3 == "CCC" ~ 0.1,
        TRUE ~ -0.2
      ),
      observed_rate_pct = 1.5 + 0.7 * .data$rating_rate_pct +
        .data$country_effect +
        0.04 * ((.data$analysis_year %% 3) - 1) +
        0.03 * sin(
          .data$analysis_year * as.numeric(factor(.data$iso3))
        )
    ) |>
    dplyr::select(-"country_effect")
}

test_that("test outcomes cannot enter country-history predictions", {
  sample <- synthetic_history_sample()
  train <- dplyr::filter(sample, .data$analysis_year < 2020)
  test <- dplyr::filter(sample, .data$analysis_year == 2020)
  altered_test <- dplyr::mutate(
    test, observed_rate_pct = .data$observed_rate_pct + 1000
  )
  original <- p15_fit_rating_country_history(train, test)$scored
  altered <- p15_fit_rating_country_history(train, altered_test)$scored
  expect_equal(
    original$history_prediction_pct,
    altered$history_prediction_pct,
    tolerance = 1e-12
  )
  expect_equal(
    original$country_history_adjustment_pp,
    altered$country_history_adjustment_pp,
    tolerance = 1e-12
  )
})

test_that("countries without prior outcomes fall back exactly to affine", {
  sample <- synthetic_history_sample()
  train <- dplyr::filter(
    sample, .data$analysis_year < 2020, .data$iso3 != "DDD"
  )
  test <- dplyr::filter(
    sample, .data$analysis_year == 2020, .data$iso3 == "DDD"
  )
  out <- p15_fit_rating_country_history(train, test)$scored
  expect_equal(out$country_history_rows, 0L)
  expect_equal(out$country_history_adjustment_pp, 0)
  expect_equal(
    out$history_prediction_pct, out$affine_prediction_pct,
    tolerance = 1e-12
  )
})

test_that("past-only and fixed designs preserve their time boundaries", {
  predictions <- p15_build_rating_country_history_predictions(
    synthetic_history_sample()
  )
  expanding <- dplyr::filter(
    predictions, .data$validation_design == "expanding_past_only"
  )
  fixed <- dplyr::filter(
    predictions, .data$validation_design == "fixed_future_2020_2024",
    .data$model_id == "affine_year_demeaned_country_eb"
  )
  expect_true(all(expanding$training_last_year < expanding$analysis_year))
  expect_equal(unique(fixed$training_last_year), 2019L)
  expect_length(unique(fixed$affine_intercept), 1L)
  expect_length(unique(fixed$affine_slope), 1L)
  expect_length(unique(fixed$shrinkage_k), 1L)
  expect_true(all(
    fixed |>
      dplyr::distinct(.data$iso3, .data$country_history_adjustment_pp) |>
      dplyr::count(.data$iso3) |>
      dplyr::pull(.data$n) == 1L
  ))
})

test_that("stored validation sample reproduces candidate counts and metrics", {
  input_path <- paste0(
    "../../data-derived/p15_fallback_validation_2012_2024_v1/",
    "p15_rating_integrated_anchor_validation_detail_2012_2024.csv.gz"
  )
  detail <- readr::read_csv(
    input_path, show_col_types = FALSE, guess_max = 100000
  )
  sample <- p15_prepare_rating_country_history_sample(detail)
  predictions <- p15_build_rating_country_history_predictions(sample)
  metrics <- p15_summarise_rating_country_history(predictions)
  candidate <- dplyr::filter(
    metrics, .data$model_id == "affine_year_demeaned_country_eb"
  )
  expanding <- dplyr::filter(
    candidate, .data$validation_design == "expanding_past_only"
  )
  fixed <- dplyr::filter(
    candidate, .data$validation_design == "fixed_future_2020_2024"
  )
  expect_equal(nrow(sample), 210L)
  expect_equal(expanding$test_rows, 176L)
  expect_equal(fixed$test_rows, 100L)
  expect_equal(expanding$mean_absolute_error_pp, 0.8274, tolerance = 5e-4)
  expect_equal(expanding$rmse_pp, 1.10, tolerance = 5e-3)
  expect_equal(fixed$mean_absolute_error_pp, 0.8024, tolerance = 5e-4)
  expect_equal(fixed$rmse_pp, 1.07, tolerance = 5e-3)
  expect_equal(expanding$prior_country_history_share, 151 / 176)
  expect_equal(fixed$prior_country_history_share, 85 / 100)
})
