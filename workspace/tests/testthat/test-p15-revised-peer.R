source('../../R/p15_revised_peer_scorecard.R')
source('../../R/p15_revised_peer_matching.R')
source('../../R/p15_analysis_dataset.R')

p15_test_peer_row <- function(ids, notch=10, shadow=NA_real_, lower=shadow, upper=shadow,
  income='Low income',region='Africa',rate=4) {
  data.table::data.table(iso3=ids,analysis_year=2020L,notch=notch,shadow=shadow,
    shadow_lower=lower,shadow_upper=upper,shadow_complete=FALSE,shadow_status='conditional',
    historical_income_level=income,rating_source_region=region,seed_rate=rate,seed_source='primary')
}
p15_test_peer_design <- function()data.table::data.table(method='preferred',scenario='wgi_bounded',
  input_policy='conditional_points',role='test',distance='guaranteed_interval',rules='1;2;3;4;5',caliper=3L)

testthat::test_that('matching excludes the target and responds to changed donor rates',{
  t<-p15_test_peer_row('AAA');p<-p15_test_peer_row(c('AAA','BBB','CCC','DDD'),rate=c(1000,3,4,5))
  a<-p15_revised_peer_match_one(t,p,p15_test_peer_design());testthat::expect_equal(a$estimate,4)
  testthat::expect_equal(a$n_peers,3);testthat::expect_false(grepl('AAA',a$member_ids))
  p[iso3=='CCC',seed_rate:=10]
  testthat::expect_equal(p15_revised_peer_match_one(t,p,p15_test_peer_design())$estimate,5)
  testthat::expect_error(p15_revised_peer_match_one(t,rbind(p,p[iso3=='BBB']),p15_test_peer_design()))
})

testthat::test_that('the ladder stops after Rule5 even with three unmatched global donors',{
  t<-p15_test_peer_row('AAA',notch=NA_real_)
  p<-p15_test_peer_row(c('BBB','CCC','DDD'),notch=NA_real_,income='High income',region='Europe')
  z<-p15_revised_peer_match_one(t,p,p15_test_peer_design())
  testthat::expect_true(is.na(z$estimate));testthat::expect_true(is.na(z$rule));testthat::expect_equal(z$n_peers,0)
  p[,`:=`(historical_income_level='Low income',rating_source_region='Africa')]
  z<-p15_revised_peer_match_one(t,p,p15_test_peer_design())
  testthat::expect_equal(z$rule,5L);testthat::expect_false(z$estimate_depends_on_incomplete_input)
})

testthat::test_that('actual ratings take precedence and every interval endpoint must fit three notches',{
  t<-p15_test_peer_row('AAA',notch=10,shadow=20,lower=19,upper=21)
  p<-p15_test_peer_row(c('BBB','CCC','DDD'),notch=10)
  z<-p15_revised_peer_match_one(t,p,p15_test_peer_design())
  testthat::expect_equal(z$rule,1L);testthat::expect_false(z$target_shadow_used)
  t[,`:=`(notch=NA_real_,shadow=10.5,shadow_lower=9,shadow_upper=12)]
  p[,notch:=9]
  testthat::expect_equal(p15_revised_peer_match_one(t,p,p15_test_peer_design())$rule,1L)
  t[,shadow_upper:=13]
  z<-p15_revised_peer_match_one(t,p,p15_test_peer_design())
  testthat::expect_equal(z$rule,5L);testthat::expect_false(z$estimate_depends_on_incomplete_input)
})

testthat::test_that('donor source priority preserves eligibility and secondary ordinary-cost holds',{
  x<-data.table::data.table(iso3=c('AAA','BBB','LBN','DDD'),analysis_year=2021L,
    primary_usd_market_rate_pct=c(5,NA,NA,NA),ids_rate_pct=c(6,4,NA,NA),
    secondary_usd_market_rate_pct=c(7,5,8,9),rating_moodys_rate_pct=10,peer_rate_pct=99,
    observed_benchmark_selection_permitted=c(TRUE,TRUE,TRUE,FALSE),
    ordinary_fallback_selection_permitted=c(TRUE,TRUE,TRUE,FALSE),
    ids_benchmark_proxy_candidate_permitted=TRUE,rating_moodys_available=TRUE,peer_minimum_met=TRUE)
  cfg<-list(donor_priority=c('primary','ids','secondary','moodys'),
    secondary_ordinary_holds=data.frame(iso3='LBN',years=I(list(2020:2023))))
  z<-p15_revised_peer_donor_seeds(x,cfg)
  testthat::expect_equal(z$iso3,c('AAA','BBB','LBN'));testthat::expect_equal(z$seed_source,c('primary','ids','moodys'))
  testthat::expect_equal(z$seed_rate,c(5,4,10))
})

testthat::test_that('gross-interest uncertainty broadens intervals but zero-weight interest cannot affect IDA scores',{
  root<-normalizePath('../..');withr::local_dir(root)
  cfg<-p15_revised_peer_config();m<-p15_peer_read_exact_matrices(cfg$paths)
  u<-p15_revised_peer_score_inputs(cfg,data.table::data.table(iso3=c('MAR','BDI'),analysis_year=2013L))
  u[,`:=`(interest_source='WEO_net_proxy',interest_gdp=2,interest_revenue=7)]
  a<-p15_revised_peer_bounded_scores(u,m,FALSE);b<-p15_revised_peer_bounded_scores(u,m,TRUE)
  testthat::expect_true(all(b$shadow_notch_lower<=a$shadow_notch_lower,na.rm=TRUE))
  testthat::expect_true(all(b$shadow_notch_upper>=a$shadow_notch_upper,na.rm=TRUE))
  u[,concessional_weight_exception:=TRUE]
  a<-p15_revised_peer_bounded_scores(u,m,TRUE)
  u[,`:=`(interest_gdp=1000,interest_revenue=1000,interest_source='GFS_general_government_revised_gross')]
  b<-p15_revised_peer_bounded_scores(u,m,TRUE)
  testthat::expect_equal(a$shadow_notch_lower,b$shadow_notch_lower)
  testthat::expect_equal(a$shadow_notch_upper,b$shadow_notch_upper)
})
