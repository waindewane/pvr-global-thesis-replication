# Isolated, deterministic peer candidate based on externally built scorecards.
# Run from repository root. No regression is estimated and no production file is written.
source('scripts/p15/activate_p15_environment.R')
suppressPackageStartupMessages({library(data.table);library(jsonlite);library(digest)})
source('scripts/p15/loan_extension/loan_valuation.R')
source('scripts/p15/loan_extension/benchmark_matching.R')
source('R/p15_peer_geography_validation.R')
exp_dir <- 'experiments/p15_wb_scorecard_20260912'
score_path <- file.path(v2,'results/scorecard_country_year.csv')
out <- file.path(v2,'results')
dir.create(out,recursive=TRUE,showWarnings=FALSE)
stopifnot(startsWith(normalizePath(out),normalizePath(exp_dir)))
ptr_path <- file.path(v2,'snapshot/current_run.json')
ptr <- fromJSON(ptr_path)
inputs <- c(ptr_path,file.path(ptr$candidate,c('core_evidence.csv','selected_reference.csv',
  'tier_eligibility.csv','peer_region_context.csv','peer_membership.csv')),
  'data-derived/p15_peer_options_20260912_v2/seed_country_years.csv',
  file.path(ptr$stages$crs_application$dir,c('loan_valuations.csv','cash_flows.csv')),
  file.path(ptr$stages$loan_comparisons$dir,'loan_valuations.csv'))
manifest <- function(paths,role) data.table(path=paths,role=role,
  sha256=vapply(paths,digest,character(1),file=TRUE,algo='sha256'),bytes=file.info(paths)$size)
before <- manifest(inputs,'immutable_production_or_seed_input')
prior <- file.path(out,'production_input_manifest.csv')
if(file.exists(prior)){
  prior_manifest<-fread(prior);stopifnot(identical(before$path,prior_manifest$path),
    identical(before$sha256,prior_manifest$sha256),all(before$bytes==prior_manifest$bytes))
}else fwrite(before,prior)
core <- fread(inputs[2]);sel <- fread(inputs[3]);ctx <- fread(inputs[5]);saved_members <- fread(inputs[6])
seeds <- fread(inputs[7])[sources%in%c('P','PI','PIS')]
key <- c('iso3','analysis_year')
stopifnot(!anyDuplicated(core[,..key]),!anyDuplicated(sel[,..key]),!anyDuplicated(ctx[,..key]),
  !anyDuplicated(seeds[,.(sources,iso3,analysis_year)]),all(seeds$seed_source%in%c('primary','ids','secondary')),
  all(is.finite(seeds$seed_rate)),all(seeds$seed_rate > -100))
x <- merge(core[,.(iso3,analysis_year,country,historical_income_level,historical_lmic_reporting_scope,
  ordinary_fallback_selection_permitted)],sel[,.(iso3,analysis_year,selected_tier,selected_rate_pct,peer_pool_rule)],by=key)
x <- merge(x,ctx[,.(iso3,analysis_year,rating_source_region,moodys_rating_normalized)],by=key)
rating_scale <- c('Aaa','Aa1','Aa2','Aa3','A1','A2','A3','Baa1','Baa2','Baa3','Ba1','Ba2','Ba3',
  'B1','B2','B3','Caa1','Caa2','Caa3','Ca','C')
x[,notch:=match(moodys_rating_normalized,rating_scale)]
core_grade<-core$rating_moodys_rating[match(paste(x$iso3,x$analysis_year),paste(core$iso3,core$analysis_year))]
stopifnot(identical(x$notch,match(core_grade,rating_scale)))
fwrite(x[,.(country_years=.N,actual_moodys_available=sum(is.finite(notch)),
  actual_moodys_missing=sum(!is.finite(notch))),by=.(selected_tier,historical_income_level,historical_lmic_reporting_scope)],
  file.path(out,'current_rating_coverage.csv'))
# Independently reconcile archived donor seeds to the current-run eligibility
# table, including ordinary-cost holds and source-priority order.
seed_ref<-p15_loan_benchmark_reference(sel,fread(inputs[4]),core)$tiers
seed_orders<-list(P='primary',PI=c('primary','ids'),PIS=c('primary','ids','secondary'))
reconstructed_seeds<-rbindlist(lapply(names(seed_orders),function(sm){
  ord<-seed_orders[[sm]]
  z<-copy(seed_ref[ordinary_cost_tier_usable%in%TRUE & tier%in%ord & is.finite(benchmark_tier_rate_pct)])
  z[,priority:=match(tier,ord)];setorder(z,iso3,analysis_year,priority)
  z[,.(sources=sm,seed_source=first(tier),seed_rate=first(benchmark_tier_rate_pct)),by=.(iso3,analysis_year)]
}))
seed_compare<-merge(seeds[,.(sources,iso3,analysis_year,seed_source,seed_rate)],reconstructed_seeds,
  by=c('sources','iso3','analysis_year'),all=TRUE,suffixes=c('_saved','_current'))
stopifnot(nrow(seed_compare)==nrow(seeds),all(!is.na(seed_compare$seed_source_current)),
  all(seed_compare$seed_source_saved==seed_compare$seed_source_current),
  all(abs(seed_compare$seed_rate_saved-seed_compare$seed_rate_current)<1e-10))
fwrite(seed_compare,file.path(out,'seed_eligibility_reconciliation.csv'))
# Owner-authorized donor-source sensitivity: existing usable Moody's-implied
# rates only. Shadow grades do not manufacture new donor rates.
ord<-c('primary','ids','secondary','moodys')
z<-copy(seed_ref[ordinary_cost_tier_usable%in%TRUE & tier%in%ord & is.finite(benchmark_tier_rate_pct)])
z[,priority:=match(tier,ord)];setorder(z,iso3,analysis_year,priority)
extra<-z[,.(sources='PISR',seed_source=first(tier),seed_rate=first(benchmark_tier_rate_pct)),by=.(iso3,analysis_year)]
seeds<-rbind(seeds,extra,fill=TRUE)
fwrite(seeds,file.path(out,'all_donor_seeds.csv'))
x <- merge(x,seeds[sources=='P',.(iso3,analysis_year,actual=seed_rate)],by=key,all.x=TRUE)
rule_map <- data.table(rule=1:8,current_label=c('same_income_region_rating3_min3','same_income_rating3_min3',
  'same_region_rating3_min3','rating3_min3','same_income_region_min3','same_income_min3','same_region_min3','global_min3'),
  meaning=c('Same income, region, and rating within 3 notches','Same income and rating within 3 notches',
  'Same region and rating within 3 notches','Rating within 3 notches','Same income and region',
  'Same income','Same region','Any eligible donor'))
fwrite(rule_map,file.path(out,'rule_mapping.csv'))
# Known production grades always win over shadow grades. Interval bounds only
# affect a shadow-filled country and cannot replace an observed grade.
sc <- data.table(iso3=character(),analysis_year=integer(),scenario=character(),shadow_notch=numeric(),
  scorecard_complete=logical(),status=character(),shadow_notch_lower=numeric(),shadow_notch_upper=numeric())
if(file.exists(score_path)) {
  sc <- fread(score_path)
  req <- c('iso3','analysis_year','scenario','shadow_notch','scorecard_complete','status')
  stopifnot(all(req%in%names(sc)),!anyDuplicated(sc[,.(iso3,analysis_year,scenario)]))
  if(!is.logical(sc$scorecard_complete)) {
    stopifnot(all(tolower(as.character(sc$scorecard_complete))%in%c('true','false','1','0')))
    sc[,scorecard_complete:=tolower(as.character(scorecard_complete))%in%c('true','1')]
  }
  for(nm in c('shadow_notch_lower','shadow_notch_upper'))if(!nm%in%names(sc))sc[,(nm):=shadow_notch]
  stopifnot(all(sc$analysis_year%in%2012:2024),all(nchar(sc$iso3)==3),all(!is.na(sc$scenario)&nzchar(sc$scenario)),
    all(!is.na(sc$status)&nzchar(sc$status)),all(!is.na(sc$scorecard_complete)),
    all(paste(sc$iso3,sc$analysis_year)%in%paste(x$iso3,x$analysis_year)),
    all(!sc$scorecard_complete|is.finite(sc$shadow_notch)),
    all(!is.finite(sc$shadow_notch)|(sc$shadow_notch>=1 & sc$shadow_notch<=21)),
    all(!is.finite(sc$shadow_notch_lower)|(sc$shadow_notch_lower>=1 & sc$shadow_notch_lower<=21)),
    all(!is.finite(sc$shadow_notch_upper)|(sc$shadow_notch_upper>=1 & sc$shadow_notch_upper<=21)))
  has_bounds <- is.finite(sc$shadow_notch_lower)&is.finite(sc$shadow_notch_upper)
  stopifnot(all(sc$shadow_notch_lower[has_bounds]<=sc$shadow_notch[has_bounds]),
    all(sc$shadow_notch_upper[has_bounds]>=sc$shadow_notch[has_bounds]))
  score_before<-manifest(score_path,'scorecard_input')
  fwrite(score_before,file.path(out,'scorecard_input_manifest.csv'))
  fwrite(sc[,.(rows=.N,finite_points=sum(is.finite(shadow_notch)),complete_points=sum(scorecard_complete & is.finite(shadow_notch)),
    incomplete_finite_points=sum(!scorecard_complete & is.finite(shadow_notch)),
    interval_rows=sum(is.finite(shadow_notch_lower)&is.finite(shadow_notch_upper)&shadow_notch_lower<shadow_notch_upper)),
    by=.(scenario,status)],file.path(out,'scorecard_input_coverage.csv'))
}
