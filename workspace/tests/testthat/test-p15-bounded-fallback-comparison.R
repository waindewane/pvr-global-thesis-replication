suppressPackageStartupMessages({
  library(dplyr)
  library(testthat)
  library(tibble)
})

source(testthat::test_path("..", "..", "R", "p15_bounded_fallback_comparison.R"))

test_that("rating notches follow credit quality", {
  expect_equal(p15_rating_notch_number(c("Aaa", "Baa3", "C")), c(1L, 10L, 21L))
  expect_true(is.na(p15_rating_notch_number("NA")))
})

test_that("observable peer pools are target excluding and prefer all matches", {
  eligible <- tibble(
    iso3 = c("BBB", "CCC", "DDD", "EEE"),
    historical_income_level = c("Lower middle income", "Lower middle income",
                                "Lower middle income", "High income"),
    rating_source_region = c("Region 1", "Region 1", "Region 1", "Region 2"),
    moodys_rating_normalized = c("Ba2", "Ba3", "B1", "Aaa")
  )
  target <- tibble(
    historical_income_level = "Lower middle income",
    rating_source_region = "Region 1",
    moodys_rating_normalized = "Ba3"
  )
  out <- p15_peer_similarity_pool(eligible, target, minimum_count = 3L)
  expect_equal(out$rule, "same_income_region_rating3_min3")
  expect_equal(sort(out$pool$iso3), c("BBB", "CCC", "DDD"))
})

test_that("clustered paired comparison reports an interpretable difference", {
  x <- tidyr::expand_grid(iso3 = c("AAA", "BBB"), analysis_year = 2020:2022) |>
    mutate(delta = c(-1, -1, -1, 1, 1, 1))
  out <- p15_clustered_mean_difference(x, "delta")
  expect_equal(out$matched_rows, 6L)
  expect_equal(out$mean_difference_pp, 0, tolerance = 1e-12)
})
