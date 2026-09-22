source("../../R/p15_rating_country_history_calibration.R")
source("../../R/p15_rating_nested_check.R")

testthat::test_that("candidate support failures preserve common test support", {
  train <- data.frame(analysis_year = 2012L, iso3 = "AAA", country = "A",
    observed_rate_pct = 4, rating_rate_pct = 5)
  test <- transform(train, analysis_year = 2018L, iso3 = "BBB", country = "B")
  r <- p15_nested_candidates(train, test)
  testthat::expect_equal(nrow(r$scored), 3L)
  testthat::expect_true(all(r$scored$prediction_pct == 5))
  testthat::expect_true(all(r$scored$natural_history_rows == 0))
})

testthat::test_that("selection ties favour the simpler frozen candidate", {
  inner <- data.table::data.table(model = c("history", "affine", "raw"),
    absolute_error_pp = c(1, 1, 1 + 1e-12))
  testthat::expect_identical(p15_nested_select(inner)$model, "raw")
})

testthat::test_that("later outcomes cannot enter candidate fitting", {
  train <- data.frame(analysis_year = 2018L, iso3 = "AAA", country = "A",
    observed_rate_pct = 4, rating_rate_pct = 5)
  test <- transform(train, iso3 = "BBB")
  testthat::expect_error(p15_nested_candidates(train, test))
})

testthat::test_that("summary rejects mixing two branch populations", {
  d <- data.table::data.table(branch = c("temporal", "country_history_withheld"))
  testthat::expect_error(p15_nested_summary(d))
})
