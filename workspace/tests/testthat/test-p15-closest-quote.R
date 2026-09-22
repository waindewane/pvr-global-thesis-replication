testthat::test_that("closest quote preserves ties and enforces inclusive window", {
  source(file.path("..", "..", "R", "p15_observed_all_years.R"), local = TRUE)
  x <- data.frame(date = as.Date(c("2024-11-29", "2024-11-30", "2024-12-29",
    "2025-01-02", "2025-01-02", "2025-01-07", "2025-01-08")),
    snapshot = as.Date("2024-12-31"), value = seq_len(7))
  pick <- function(z, rule = "closest") p15_secondary_date_subset(z, "date", "snapshot", rule, 31, 7)
  testthat::expect_equal(pick(x)$value, 3L)
  testthat::expect_equal(pick(x[-3, ])$value, c(4L,5L))
  testthat::expect_equal(pick(x, "latest")$value, 6L)
  testthat::expect_equal(pick(x[c(1,2), ])$value, 2L)
  testthat::expect_equal(nrow(pick(x[c(1,7), ])), 0L)
  testthat::expect_equal(pick(x[c(6,7), ])$value, 6L)
  testthat::expect_equal(nrow(pick(x[FALSE, ])), 0L)
  testthat::expect_error(pick(x, "unknown"))
})

testthat::test_that("direct selector uses nearest date without yield shopping", {
  source("../../R/p15_observed_all_years.R", local=TRUE)
  x <- tibble::tibble(analysis_year=2024L,
    history_date=as.Date(c("2024-12-29","2025-01-02","2025-01-07","2025-01-08")),
    source_subrow=1:4,RIC="TEST",mid_price=100,bid=99,ask=101,
    yield_to_maturity=c(150,5,6,7),any_observed_measure=TRUE,
    source_package_id="TEST",source_role="fixture",source_record_locator=paste0("row:",1:4))
  closest <- p15_latest_secondary_direct_quotes(x,"closest",31,7)
  testthat::expect_equal(closest$direct_quote_date,as.Date("2024-12-29"))
  testthat::expect_equal(closest$direct_yield_pct,150)
  testthat::expect_equal(closest$direct_source_record_locators,"row:1")
  testthat::expect_equal(p15_latest_secondary_direct_quotes(x)$direct_yield_pct,7)
  testthat::expect_equal(p15_latest_secondary_direct_quotes(x,"latest",31,7)$direct_yield_pct,6)
  x$yield_to_maturity[1] <- NA_real_
  testthat::expect_equal(p15_latest_secondary_direct_quotes(x,"closest",31,7)$direct_quote_date,as.Date("2025-01-02"))
})
