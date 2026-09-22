source(file.path("..","..","R","p15_bounded_fallback_comparison.R"))
source(file.path("..","..","R","p15_peer_history_matching.R"))

testthat::test_that("persistence requires observed prior evidence not missing matches", {
  testthat::expect_true(p15_persistent_match(c(TRUE,TRUE,FALSE),c(TRUE,TRUE,TRUE)))
  testthat::expect_false(p15_persistent_match(c(TRUE,FALSE,FALSE),c(TRUE,TRUE,TRUE)))
  testthat::expect_false(p15_persistent_match(TRUE,TRUE))
  testthat::expect_false(p15_persistent_match(c(TRUE,NA),c(TRUE,FALSE)))
  testthat::expect_true(p15_persistent_match(c(TRUE,TRUE,NA),c(TRUE,TRUE,FALSE)))
})

testthat::test_that("future rows cannot change historical matching", {
  c <- expand.grid(analysis_year=2017:2021,iso3=c("T","A","B","C"),stringsAsFactors=FALSE)
  c$historical_income_level <- "Lower middle income";c$rating_source_region <- "Region"
  c$moodys_rating_normalized <- "B1"
  target <- c[c$analysis_year==2020 & c$iso3=="T",]
  eligible <- c[c$analysis_year==2020 & c$iso3!="T",]
  eligible$market_rate_pct <- c(4,5,6)
  one <- p15_peer_history_pool(eligible,target,c)
  c[c$analysis_year>=2020,c("historical_income_level","moodys_rating_normalized")] <- "changed"
  two <- p15_peer_history_pool(eligible,target,c)
  testthat::expect_identical(one,two)
  testthat::expect_equal(one$rule,"same_income_region_rating3_min3")
  testthat::expect_true(one$target_history_available)
})
