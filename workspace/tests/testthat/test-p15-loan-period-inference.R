source(testthat::test_path("..","..","scripts","p15","loan_extension","period_inference.R"))

period_fixture<-function() {
 data.frame(iso3=rep(LETTERS[1:6],each=4),commitment_year=rep(2018:2021,6),
   late=rep(c(FALSE,FALSE,TRUE,TRUE),6),
   delta_ge_pp=c(0,1,3,5,2,1,2,4,-2,0,4,3,0,1,2,4,1,3,6,7,2,2,6,8))
}

testthat::test_that("period contrast compares gaps and is invariant to row replication",{
 d<-period_fixture();j<-loan_period_country_jackknife(d)
 testthat::expect_equal(j$summary$estimate_pp,mean(d$delta_ge_pp[d$late])-mean(d$delta_ge_pp[!d$late]))
 repeated<-d[rep(seq_len(nrow(d)),each=7),]
 r<-loan_period_country_jackknife(repeated)
 testthat::expect_equal(r$summary$estimate_pp,j$summary$estimate_pp)
 testthat::expect_equal(r$summary$se_pp,j$summary$se_pp)
 testthat::expect_equal(r$summary$p_two_sided,j$summary$p_two_sided)
 testthat::expect_equal(j$summary$df,5L)
})

testthat::test_that("jackknife removes complete country histories",{
 d<-period_fixture();z<-loan_period_delete_cluster(d,"iso3")
 testthat::expect_equal(nrow(z),6L)
 testthat::expect_true(all(z$remaining_records==20L))
 testthat::expect_true(all(z$early_records==10L&z$late_records==10L))
})

testthat::test_that("too few represented countries do not trigger a row-level test",{
 d<-period_fixture();d$late<-d$iso3=="A"
 r<-loan_period_country_jackknife(d)
 testthat::expect_true(is.na(r$summary$p_two_sided))
 testthat::expect_true(is.na(r$summary$se_pp))
})

testthat::test_that("cluster score formula agrees with independent sandwich package",{
 testthat::skip_if_not_installed("sandwich")
 d<-period_fixture();fit<-lm(delta_ge_pp~late,data=d)
 expected<-sandwich::vcovCL(fit,cluster=d$iso3,type="HC1",cadjust=TRUE)[2,2]
 testthat::expect_equal(loan_period_cr1_variance(d$delta_ge_pp,d$late,d$iso3),unname(expected))
 s<-loan_period_shared_year(d)
 testthat::expect_gte(s$variance_pp2[s$method=="shared_year_variance_envelope"],
   max(s$variance_pp2[s$method%in%c("country_CR1","year_CR1")]))
})
