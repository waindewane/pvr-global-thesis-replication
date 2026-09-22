source(file.path("..","..","R","p15_analysis_dataset.R"))

testthat::test_that("independent eligibility retains explicit restrictions and missingness", {
  p <- data.frame(analysis_year=rep(2022,4),iso3=c("A","B","C","D"),
    primary_usd_market_rate_pct=c(5,NA,5,0),secondary_usd_market_rate_pct=c(6,6,6,NA),
    ids_rate_pct=c(4,4,4,NA),rating_moodys_rate_pct=rep(7,4),peer_rate_pct=rep(8,4),
    observed_benchmark_selection_permitted=c(NA,TRUE,FALSE,TRUE),
    ordinary_fallback_selection_permitted=c(NA,TRUE,FALSE,TRUE),
    ids_benchmark_proxy_candidate_permitted=c(TRUE,FALSE,TRUE,FALSE),
    rating_moodys_available=TRUE,peer_minimum_met=c(TRUE,TRUE,FALSE,TRUE))
  e <- p15_analysis_eligibility(p)
  testthat::expect_true(e$eligible[e$iso3=="A" & e$tier=="primary"])
  testthat::expect_false(e$eligible[e$iso3=="B" & e$tier=="ids"])
  testthat::expect_false(any(e$eligible[e$iso3=="C"]))
  testthat::expect_true(e$eligible[e$iso3=="D" & e$tier=="primary"])
  testthat::expect_equal(e$exclusion_reason[e$iso3=="B" & e$tier=="primary"],"no_finite_source_value")
})
