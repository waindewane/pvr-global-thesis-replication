source(file.path("..","..","scripts","p15","loan_extension","loan_valuation.R"))

testthat::test_that("historical headline policy changes at 2018 without borrowing the category column", {
  rates<-loan_policy_discount_rates(c(2013,2017,2018,2024,2017),c(10,10,6,9,10),c(TRUE,TRUE,TRUE,TRUE,FALSE))
  testthat::expect_equal(rates,c(10,10,6,9,NA_real_))
  testthat::expect_equal(loan_ge(0,10,10,rates[2]),100*(1-1.10^-10))
  testthat::expect_equal(loan_ge(0,10,10,rates[3]),100*(1-1.06^-10))
  testthat::expect_error(loan_policy_discount_rates(2017,6,TRUE),"historical 10%")
})

testthat::test_that("source-style formula equals independently built complete semiannual cash flows", {
  for(coupon in c(0,2,7))for(r in c(0,1,5,12))for(p in list(c(20,7),c(10,.5),c(12,12))) {
    f<-loan_explicit_schedule(coupon,p[1],p[2])
    testthat::expect_equal(sum(f$principal),100,tolerance=1e-10)
    testthat::expect_equal(loan_ge(coupon,p[1],p[2],r),loan_explicit_ge(f,r),tolerance=1e-8)
  }
})
testthat::test_that("zero-coupon bullet and negative grants retain their economic meaning", {
  testthat::expect_equal(loan_ge(0,10,10,5),100*(1-1.05^-10))
  testthat::expect_lt(loan_ge(9,10,.5,5),0)
  testthat::expect_equal(loan_ge(2,20,7,5),28.1503,tolerance=.0001)
})
testthat::test_that("fractional endpoints conserve principal and increasing discount lowers PV", {
  for(p in list(c(17.43,5.21),c(12.001,6.001),c(.2,.2))) {
    f<-loan_explicit_schedule(3,p[1],p[2])
    testthat::expect_equal(sum(f$principal),100,tolerance=1e-10)
    testthat::expect_equal(max(f$payment_time_years),p[1])
    testthat::expect_true(all(f$opening_principal>=-1e-8))
    testthat::expect_gt(loan_explicit_ge(f,7),loan_explicit_ge(f,5))
    testthat::expect_gt(loan_ge(3,p[1],p[2],7),loan_ge(3,p[1],p[2],5))
  }
})
testthat::test_that("invalid schedules cannot silently become values", {
  testthat::expect_true(is.na(loan_ge(2,7,13,5)))
  testthat::expect_true(is.na(loan_ge(NA,7,3,5)))
  testthat::expect_true(is.na(loan_ge(2,7,3,-100)))
})

testthat::test_that("unidentified loan events are not collapsed into one invented event", {
  z<-data.table::data.table(loan_event_id=c(NA_character_,NA_character_),delta_ge_pp=c(2,4),
    market_ge_pct=c(20,30),reference_ge_pct=c(18,26),amount_usd=c(1,3),iso3=c("AAA","BBB"),commitment_year=c(2020,2021))
  s<-loan_summary(z)
  testthat::expect_true(is.na(s$events))
  testthat::expect_true(is.na(s$equal_event_mean_of_record_deltas_pp))
  testthat::expect_equal(s$mean_delta_ge_pp,3)
  testthat::expect_equal(s$amount_weighted_mean_delta_ge_pp,3.5)
  z[,loan_event_id:="shared_event"];s<-loan_summary(z)
  testthat::expect_equal(s$events,1)
  testthat::expect_equal(s$equal_event_mean_of_amount_aggregated_deltas_pp,3.5)
})
