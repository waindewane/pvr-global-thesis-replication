source(file.path("..","..","R","p15_dataset_assessment.R"))

testthat::test_that("signed errors and absolute distances stay distinct", {
  d <- data.frame(iso3=c("A","A","B"),analysis_year=c(1,2,1),
    anchor_rate=c(3,4,5),comparison_rate=c(4,3,8),gap_pp=c(1,-1,3))
  m <- p15_assessment_metrics(d)
  testthat::expect_equal(m$mean_gap_pp,1)
  testthat::expect_equal(m$mae_pp,5/3)
  testthat::expect_equal(m$equal_country_mean_gap_pp,1.5)
  testthat::expect_equal(m$equal_country_mae_pp,2)
  testthat::expect_equal(m$within_1,2/3)
  testthat::expect_equal(m$countries,2)
})
testthat::test_that("source bridges preserve arithmetic without filling missing sources", {
  b <- p15_assessment_bridges(c(4,5),c(8,7),c(6,NA),c(5,NA))
  testthat::expect_equal(b$total_change_pp,c(4,2))
  testthat::expect_equal(b$old_source_within_change_pp[1]+b$source_difference_current_year_pp[1],4)
  testthat::expect_equal(b$new_source_within_change_pp[1]+b$source_difference_previous_year_pp[1],4)
  testthat::expect_false(b$old_source_bridge_available[2])
  testthat::expect_true(is.na(b$source_difference_current_year_pp[2]))
})
testthat::test_that("duplicate or missing keys fail", {
  testthat::expect_error(p15_assessment_keys(data.frame(id=c(1,1)),"id","test"),"duplicate")
  testthat::expect_error(p15_assessment_keys(data.frame(id=c(1,NA)),"id","test"),"missing")
  testthat::expect_true(p15_assessment_keys(data.frame(id=1:2),"id","test"))
})
testthat::test_that("cluster inference preserves signed mean and symmetric intervals", {
  d <- expand.grid(iso3=LETTERS[1:10],analysis_year=2012:2024)
  set.seed(31)
  d$gap_pp <- rep(seq(-1,1,length.out=10),13)+rnorm(130)+.5
  x <- p15_assessment_inference(d)
  testthat::expect_equal(x$mean_gap_pp,rep(mean(d$gap_pp),2))
  testthat::expect_equal(x$df,c(9,9))
  testthat::expect_true(all(x$ci_low_pp<x$mean_gap_pp & x$ci_high_pp>x$mean_gap_pp))
  testthat::expect_equal(x$ci_high_pp-x$mean_gap_pp,x$mean_gap_pp-x$ci_low_pp)
})
