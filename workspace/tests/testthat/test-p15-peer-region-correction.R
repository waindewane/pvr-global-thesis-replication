source(file.path("..","..","R","p15_peer_region_correction.R"))
testthat::test_that("region fill is keyed, missing-only, and never invents a region",{
 c<-data.frame(analysis_year=2012L,iso3=c("AAA","BBB","CCC","DDD"),
   rating_source_region=c("Preserved","",NA," "))
 g<-data.frame(iso3=c("BBB","AAA","CCC"),wb_region=c(" Fill ","Different","Aggregates"))
 x<-p15_fill_peer_regions(c,g)
 testthat::expect_equal(x$rating_source_region,c("Preserved","Fill",NA," "))
 testthat::expect_equal(x$peer_region_filled,c(FALSE,TRUE,FALSE,FALSE))
 testthat::expect_equal(x$rating_source_region_before,c$rating_source_region)
 testthat::expect_error(p15_fill_peer_regions(c,rbind(g,g[1,])))
 testthat::expect_error(p15_fill_peer_regions(rbind(c,c[1,]),g))
})
