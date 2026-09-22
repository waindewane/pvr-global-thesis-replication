source("../../R/pvr.R")

testthat::test_that("grace period delays principal repayment", {
  flows <- make_amortizing_cash_flows(100, annual_rate_percent = 5, maturity_years = 5, grace_years = 2)
  testthat::expect_equal(flows$principal[1:2], c(0, 0))
  testthat::expect_equal(sum(flows$principal), 100)
  testthat::expect_equal(flows$principal[3:5], rep(100 / 3, 3), tolerance = 1e-8)
})

testthat::test_that("bullet bonds repay principal at maturity", {
  flows <- make_bullet_cash_flows(100, annual_rate_percent = 10, maturity_years = 3)
  testthat::expect_equal(flows$principal, c(0, 0, 100))
  testthat::expect_equal(flows$interest, c(10, 10, 10))
})

testthat::test_that("market benchmark PVR is 100 when coupon equals discount rate", {
  pvr <- calculate_market_pvr(market_rate_percent = 9.75, market_maturity_years = 7)
  testthat::expect_equal(round(pvr, 8), 100)
})

testthat::test_that("Kenya-style screenshot values are reproduced by rounded terms", {
  ibrd <- calculate_official_pvr(
    annual_rate_percent = 6.57,
    maturity_years = 21,
    grace_years = 8,
    discount_rate_percent = 9.75
  )
  ida <- calculate_official_pvr(
    annual_rate_percent = 1.46,
    maturity_years = 30,
    grace_years = 5,
    discount_rate_percent = 9.75
  )
  testthat::expect_equal(round(ibrd), 76)
  testthat::expect_equal(round(ida), 35)
})

testthat::test_that("fractional IDS terms change the schedule instead of being rounded away", {
  rounded <- calculate_official_pvr(
    annual_rate_percent = 6.57,
    maturity_years = 21,
    grace_years = 8,
    discount_rate_percent = 9.75
  )
  fractional <- calculate_official_pvr(
    annual_rate_percent = 6.57,
    maturity_years = 21.3333,
    grace_years = 8.3333,
    discount_rate_percent = 9.75
  )
  testthat::expect_false(isTRUE(all.equal(rounded, fractional)))
})
