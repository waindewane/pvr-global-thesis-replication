source(file.path("..","..","R","p15_bounded_fallback_comparison.R"))
source(file.path("..","..","R","p15_peer_geography_validation.R"))

testthat::test_that("geography alternatives preserve minimum and target exclusion",{
  p<-data.frame(analysis_year=2024L,iso3=c("A","B","C","D","E"),
    historical_income_level="Low income",rating_source_region=c("Africa","Africa","Africa","Africa","Asia"),
    moodys_rating_normalized=NA_character_,primary_eligible=TRUE,
    primary_usd_market_rate_pct=c(100,8,9,10,1))
  z<-p15_geography_predictions(p)
  testthat::expect_equal(z$estimate[z$iso3=="A"&z$method=="region_only"],9)
  testthat::expect_equal(z$estimate[z$iso3=="A"&z$method=="worldwide_only"],8.5)
  testthat::expect_true(is.na(z$estimate[z$iso3=="E"&z$method=="region_only"]))
  testthat::expect_true(all(!mapply(function(id,s)id %in% strsplit(s,";")[[1]],z$iso3,z$seed_ids)))
  p$primary_usd_market_rate_pct[1]<-200
  zz<-p15_geography_predictions(p)
  testthat::expect_equal(z$estimate[z$iso3=="A"],zz$estimate[zz$iso3=="A"])
  # Future-year seed cannot enter an earlier prediction.
  q<-p[1,];q$analysis_year<-2025L;q$iso3<-"F";q$primary_usd_market_rate_pct<-500
  zzz<-p15_geography_predictions(rbind(p,q))
  testthat::expect_equal(zz$estimate[zz$iso3=="A"],zzz$estimate[zzz$iso3=="A"])
})

testthat::test_that("paired scores use intersection rather than unequal samples",{
  x<-data.frame(iso3=c("A","B","A","C"),analysis_year=2024,
    information="available_rating",method=c("current","current","no_geography","no_geography"),
    estimate=c(6,7,9,2),actual=c(5,8,5,3))
  z<-p15_geography_paired(x,"current","no_geography")
  testthat::expect_equal(z$iso3,"A")
  testthat::expect_equal(z$benefit,3)
  testthat::expect_true(is.na(p15_geography_inference(z)$p_two_sided))
})
