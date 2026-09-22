suppressPackageStartupMessages({
  library(dplyr)
  library(testthat)
})

source("../../R/p15_rating_mean_error_inference.R")

test_that("mean-error inference separates ordinary and clustered tests", {
  x <- tibble::tibble(
    iso3 = rep(c("AAA", "BBB", "CCC"), each = 3),
    analysis_year = rep(2022:2024, times = 3),
    signed_gap_pp = c(-1, 0, 1, 0, 1, 2, -2, -1, 0)
  )
  out <- p15_build_rating_mean_error_inference(x)
  expect_equal(nrow(out), 5L)
  expect_setequal(
    out$inference_method,
    c(
      "ordinary_paired_t", "country_clustered", "year_clustered",
      "two_way_country_year_clustered",
      "equal_country_weight_sensitivity"
    )
  )
  expect_equal(sum(out$preferred_panel_inference), 1L)
  expect_true(all(out$p_value_two_sided >= 0 & out$p_value_two_sided <= 1))
  expect_true(all(out$confidence_interval_95_lower_pp <=
                    out$mean_signed_error_pp))
  expect_true(all(out$confidence_interval_95_upper_pp >=
                    out$mean_signed_error_pp))
})
