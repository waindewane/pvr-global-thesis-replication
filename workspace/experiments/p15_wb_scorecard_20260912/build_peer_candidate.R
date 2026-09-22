# Isolated, deterministic peer candidate based on externally built scorecards.
# Run from repository root. No regression is estimated and no production file is written.
source('scripts/p15/activate_p15_environment.R')
suppressPackageStartupMessages({library(data.table);library(jsonlite);library(digest)})
source('scripts/p15/loan_extension/loan_valuation.R')
source('scripts/p15/loan_extension/benchmark_matching.R')
source('R/p15_peer_geography_validation.R')
exp_dir <- 'experiments/p15_wb_scorecard_20260912'
args <- commandArgs(trailingOnly=TRUE)
score_path <- if(length(args))args[[1]] else file.path(exp_dir,'scorecard_country_year.csv')
out <- if(length(args)>1L)args[[2]] else file.path(exp_dir,'peer')
dir.create(out,recursive=TRUE,showWarnings=FALSE)
stopifnot(startsWith(normalizePath(out),normalizePath(exp_dir)))
ptr_path <- 'data-derived/p15_master/current_run.json'
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
designs <- data.table(method=c('current','P_stop5','PIS_stop5','PIS_strict'),sources=c('P','P','PIS','PIS'),
  scenario='observed_only',input_policy='observed_only',rules=c('1;2;3;4;5;6;7;8','1;2;3;4;5','1;2;3;4;5','1;2;3;5'),
  distance='point',role=c('production_replay','primary_only_cutoff','expanded_sources_cutoff','expanded_sources_strict'))
for(scenario_name in unique(sc$scenario)) {
  policies <- 'complete_only'
  if(any(sc[scenario==scenario_name,!scorecard_complete&is.finite(shadow_notch)]))policies<-c(policies,'conditional_points')
  has_range<-any(sc[scenario==scenario_name,is.finite(shadow_notch_lower)&is.finite(shadow_notch_upper)&shadow_notch_lower<shadow_notch_upper])
  for(pol in policies)for(src in c('P','PI','PIS'))for(rule_set in c('1;2;3;5','1;2;3;4;5')) {
    nm <- paste(src,scenario_name,pol,if(rule_set=='1;2;3;5')'strict' else 'stop5',sep='__')
    designs <- rbind(designs,data.table(method=nm,sources=src,scenario=scenario_name,input_policy=pol,rules=rule_set,
      distance='point',role=if(pol=='complete_only')'scorecard_candidate' else 'conditional_input_sensitivity'))
    if(has_range && src=='PIS')for(dist in c('guaranteed_interval','possible_interval'))
      designs <- rbind(designs,data.table(method=paste(nm,dist,sep='__'),sources=src,scenario=scenario_name,input_policy=pol,
        rules=rule_set,distance=dist,role='interval_sensitivity'))
  }
}
stopifnot(!anyDuplicated(designs$method))
designs[,`:=`(scenario_complete_points=0L,scenario_finite_points=0L,scorecard_build_state='observed_rating_control')]
for(sn in unique(sc$scenario)) {
  finite_n<-sc[scenario==sn,sum(is.finite(shadow_notch))]
  complete_n<-sc[scenario==sn,sum(is.finite(shadow_notch)&scorecard_complete)]
  designs[scenario==sn,`:=`(scenario_complete_points=complete_n,scenario_finite_points=finite_n,
    scorecard_build_state=if(complete_n>0L)'named_scenario_has_complete_points' else if(finite_n>0L)'conditional_points_only' else 'blocked_no_shadow_grades')]
  if(complete_n==0L)designs[scenario==sn & input_policy=='complete_only',role:='blocked_scorecard_control']
}
fwrite(designs,file.path(out,'designs.csv'))
select_shadow <- function(panel,scenario_name,input_policy) {
  panel <- copy(panel)
  panel[,`:=`(shadow=NA_real_,shadow_lower=NA_real_,shadow_upper=NA_real_,shadow_complete=FALSE,shadow_status='not_supplied')]
  if(input_policy=='observed_only')return(panel)
  z <- sc[scenario==scenario_name]
  at <- match(paste(panel$iso3,panel$analysis_year),paste(z$iso3,z$analysis_year))
  allowed <- !is.na(at)&is.finite(z$shadow_notch[at])
  if(input_policy=='complete_only')allowed <- allowed & (z$scorecard_complete[at]%in%TRUE)
  panel[allowed,`:=`(shadow=z$shadow_notch[at[allowed]],shadow_lower=z$shadow_notch_lower[at[allowed]],
    shadow_upper=z$shadow_notch_upper[at[allowed]],shadow_complete=z$scorecard_complete[at[allowed]],shadow_status=z$status[at[allowed]])]
  panel
}
one <- function(t,p,de,hidden=FALSE) {
  p <- p[iso3!=t$iso3]
  stopifnot(!anyDuplicated(p$iso3))
  tn <- if(hidden)NA_real_ else t$notch
  target_shadow <- !is.finite(tn)&is.finite(t$shadow)
  if(target_shadow)tn <- t$shadow
  pn <- p$notch;ps <- !is.finite(pn)&is.finite(p$shadow);pn[ps]<-p$shadow[ps]
  tl <- tu <- tn;pl <- pu <- pn
  if(target_shadow){tl<-t$shadow_lower;tu<-t$shadow_upper}
  pl[ps]<-p$shadow_lower[ps];pu[ps]<-p$shadow_upper[ps]
  dist <- switch(de$distance,point=abs(pn-tn),
    guaranteed_interval=pmax(abs(pl-tu),abs(pu-tl)),possible_interval=pmax(pl-tu,tl-pu,0))
  rat <- is.finite(dist)&dist<=3
  inc <- p$historical_income_level==t$historical_income_level;inc[is.na(inc)]<-FALSE
  reg <- p$rating_source_region==t$rating_source_region;reg[is.na(reg)]<-FALSE
  masks <- list(inc&reg&rat,inc&rat,reg&rat,rat,inc&reg,inc,reg,rep(TRUE,nrow(p)))
  matching_notches<-rep(tn,8L)
  if(de$distance=='possible_interval' && all(is.finite(c(tl,tu)))) {
    # Pairwise possibility alone need not imply a common feasible target grade.
    # Search all interval endpoints where membership can change, maximizing donor
    # count within each rule; ties use proximity to the supplied midpoint, then
    # the lower grade position. This is an optimistic, conditional sensitivity.
    qs<-sort(unique(c(tl,tu,tn,pl-3,pu+3)))
    qs<-qs[is.finite(qs)&qs>=tl&qs<=tu]
    restrictions<-list(inc&reg,inc,reg,rep(TRUE,nrow(p)))
    for(ri in 1:4) {
      candidates<-lapply(qs,function(q)is.finite(pl)&is.finite(pu)&pl<=q+3&pu>=q-3&restrictions[[ri]])
      nc<-vapply(candidates,sum,integer(1))
      win<-order(-nc,abs(qs-tn),qs)[[1]]
      masks[[ri]]<-candidates[[win]];matching_notches[[ri]]<-qs[[win]]
    }
  }
  counts <- vapply(masks,sum,integer(1));search<-as.integer(strsplit(de$rules,';',fixed=TRUE)[[1]])
  eligible <- search[counts[search]>=3L];rule<-if(length(eligible))eligible[[1]] else NA_integer_
  use <- if(is.finite(rule))masks[[rule]] else rep(FALSE,nrow(p))
  if(is.finite(rule))stopifnot(sum(use)>=3L,!any(counts[search[seq_len(match(rule,search)-1L)]]>=3L))
  incomplete_members <- ps & !p$shadow_complete
  target_conditional <- target_shadow && !t$shadow_complete
  # Ratings only enter Rules 1–4. Rule 5 does not depend on either shadow grade.
  rating_rule <- is.finite(rule) && rule<=4
  data.table(iso3=t$iso3,analysis_year=t$analysis_year,method=de$method,scenario=de$scenario,
    input_policy=de$input_policy,design_role=de$role,distance=de$distance,mode=if(hidden)'target_rating_hidden' else 'deployment',
    rule=rule,estimate=if(sum(use)>=3L)median(p$seed_rate[use]) else NA_real_,n_peers=sum(use),
    target_notch=tn,matching_target_notch=if(is.finite(rule))matching_notches[[rule]] else tn,target_shadow_used=target_shadow,target_shadow_status=t$shadow_status,
    target_shadow_complete=if(target_shadow)t$shadow_complete else NA,
    donor_shadow_members=sum(ps[use]),incomplete_shadow_donor_members=sum(incomplete_members[use]),
    estimate_depends_on_incomplete_input=rating_rule&&(target_conditional||any(incomplete_members[use])),
    primary_members=sum(p$seed_source[use]=='primary'),ids_members=sum(p$seed_source[use]=='ids'),
    secondary_members=sum(p$seed_source[use]=='secondary'),
    member_ids=paste(sort(p$iso3[use]),collapse=';'),
    member_sources=paste(paste(p$iso3[use],p$seed_source[use],sep=':'),collapse=';'),
    member_rates=paste(paste(p$iso3[use],sprintf('%.12g',p$seed_rate[use]),sep=':'),collapse=';'),
    member_rating_basis=paste(paste(p$iso3[use],ifelse(ps[use],p$shadow_status[use],'observed_or_missing'),sep=':'),collapse=';'),
    member_notches=paste(paste(p$iso3[use],sprintf('%.8g',pn[use]),sep=':'),collapse=';'),
    candidate_counts=paste(counts,collapse=';'))
}
# Each scorecard is a fixed scoring rule. Validation hides the target grade and
# removes the entire target from donors; no estimated rating regression exists.
pp <- list();k<-0L;donor_supply<-list()
for(j in seq_len(nrow(designs))) {
  de<-designs[j];cat('Matching',de$method,'\n');flush.console()
  xx<-select_shadow(x,de$scenario,de$input_policy)
  dp<-merge(seeds[sources==de$sources,.(iso3,analysis_year,seed_source)],xx,by=key)
  dp[,method:=de$method]
  donor_supply[[j]]<-dp[,.(donor_country_years=.N,actual_grade=sum(is.finite(notch)),
    shadow_grade_fill=sum(!is.finite(notch)&is.finite(shadow)),
    still_missing_grade=sum(!is.finite(notch)&!is.finite(shadow))),
    by=.(method,analysis_year,seed_source,historical_income_level)]
  for(y in 2012:2024) {
    pool<-merge(seeds[sources==de$sources & analysis_year==y,.(iso3,analysis_year,seed_rate,seed_source)],
      xx[analysis_year==y],by=key)
    targets<-xx[analysis_year==y & (selected_tier%in%c('peer','no_eligible_rate')|
      (historical_lmic_reporting_scope & is.finite(actual)))]
    for(i in seq_len(nrow(targets))) {
      t<-targets[i]
      if(t$selected_tier%in%c('peer','no_eligible_rate')){k<-k+1L;pp[[k]]<-one(t,pool,de,FALSE)}
      if(t$historical_lmic_reporting_scope && is.finite(t$actual)){k<-k+1L;pp[[k]]<-one(t,pool,de,TRUE)}
    }
  }
}
p<-rbindlist(pp)
p<-merge(p,x[,.(iso3,analysis_year,country,historical_income_level,historical_lmic_reporting_scope,
  selected_tier,selected_rate_pct,peer_pool_rule,ordinary_fallback_selection_permitted,actual)],by=key)
p[,selected_peer_new:=mode=='deployment' & is.finite(estimate) & !(ordinary_fallback_selection_permitted%in%FALSE)]
stopifnot(!anyDuplicated(p[,.(iso3,analysis_year,method,mode)]))
base<-p[method=='current' & mode=='deployment' & selected_tier=='peer' & historical_lmic_reporting_scope]
stopifnot(nrow(base)==778L,all(abs(base$estimate-base$selected_rate_pct)<1e-10),
  identical(as.integer(tabulate(base$rule,nbins=8)),c(0L,0L,0L,1L,72L,339L,146L,220L)),
  all(rule_map$current_label[base$rule]==base$peer_pool_rule))
mb<-saved_members[used_for_estimate%in%TRUE,.(saved=paste(sort(peer_iso3),collapse=';')),by=.(iso3=target_iso3,analysis_year)]
bm<-merge(base,mb,by=key)
stopifnot(nrow(bm)==778L,all(bm$member_ids==bm$saved),all(p$n_peers[is.finite(p$estimate)]>=3L),
  all(p$primary_members+p$ids_members+p$secondary_members==p$n_peers))
for(i in seq_len(nrow(p))){ids<-strsplit(p$member_ids[[i]],';',fixed=TRUE)[[1]]
  stopifnot(!p$iso3[[i]]%in%ids,!anyDuplicated(ids))}
valn<-p[mode=='target_rating_hidden' & method=='current']
stopifnot(nrow(valn)==215L,uniqueN(valn$iso3)==47L)
fwrite(p,file.path(out,'all_peer_predictions.csv'))
fwrite(rbindlist(donor_supply),file.path(out,'donor_grade_coverage.csv'))
dep<-p[mode=='deployment'];lmic<-dep[historical_lmic_reporting_scope==TRUE]
coverage<-lmic[,.(current_peer_country_years=sum(selected_tier=='peer'),
  retained_current_peers=sum(selected_tier=='peer'&selected_peer_new),
  lost_current_peers=sum(selected_tier=='peer'&!selected_peer_new),
  gained_peer_country_years=sum(selected_tier!='peer'&selected_peer_new),
  countries_retaining_peers=uniqueN(iso3[selected_peer_new]),
  changed_retained=sum(selected_tier=='peer'&selected_peer_new&abs(estimate-selected_rate_pct)>1e-10),
  mean_absolute_rate_change=mean(abs(estimate-selected_rate_pct)[selected_tier=='peer'&selected_peer_new]),
  conditional_estimates=sum(selected_peer_new&estimate_depends_on_incomplete_input),
  current_peer_targets_with_shadow_grade=sum(selected_tier=='peer'&target_shadow_used),
  retained_peers_using_shadow_matching=sum(selected_peer_new & rule<=4 & (target_shadow_used|donor_shadow_members>0L)),
  median_donor_count=as.numeric(median(n_peers[selected_peer_new]))),
  by=.(method,scenario,input_policy,design_role,distance)]
fwrite(coverage,file.path(out,'coverage_summary.csv'))
rule_counts<-merge(CJ(method=designs$method,rule=1:8),lmic[selected_peer_new==TRUE,.(country_years=.N,countries=uniqueN(iso3)),by=.(method,rule)],
  by=c('method','rule'),all.x=TRUE)
rule_counts[is.na(country_years),`:=`(country_years=0L,countries=0L)]
fwrite(rule_counts,file.path(out,'rule_counts.csv'))
fwrite(lmic[,.(country_years=.N,retained=sum(selected_peer_new),lost_current=sum(selected_tier=='peer'&!selected_peer_new)),
  by=.(method,analysis_year)],file.path(out,'coverage_year.csv'))
fwrite(lmic[,.(country_years=.N,retained=sum(selected_peer_new),lost_current=sum(selected_tier=='peer'&!selected_peer_new)),
  by=.(method,historical_income_level)],file.path(out,'coverage_income.csv'))
fwrite(lmic[selected_tier=='peer' & !selected_peer_new],file.path(out,'lost_peer_country_years.csv'))
fwrite(lmic[selected_tier=='peer' & !selected_peer_new,.(country_years=.N,years=paste(sort(analysis_year),collapse=';')),
  by=.(method,iso3,country)],file.path(out,'lost_peers_by_country.csv'))
fwrite(lmic[,.(country_years=.N),by=.(method,old_rule=peer_pool_rule,new_rule=rule,selected_peer_new)],file.path(out,'rule_transitions.csv'))
fwrite(lmic[selected_peer_new==TRUE,.(groups=.N,donor_memberships=sum(n_peers),primary=sum(primary_members),ids=sum(ids_members),
  secondary=sum(secondary_members),shadow_matched_members=sum(donor_shadow_members),
  groups_without_primary=sum(primary_members==0L)),by=.(method,rule)],file.path(out,'donor_composition.csv'))
# Full selection candidate preserves all original fields for inspection. Candidate
# columns, rather than relabelling the original source, identify changed selection.
candidates<-lapply(designs$method,function(nm){
  z<-merge(copy(sel),dep[method==nm,.(iso3,analysis_year,method,scenario,input_policy,design_role,
    candidate_peer_rate_pct=estimate,candidate_peer_rule=rule,candidate_peer_count=n_peers,selected_peer_new,
    estimate_depends_on_incomplete_input)],by=key,all.x=TRUE)
  z[is.na(method),method:=nm]
  z[,`:=`(candidate_selected_tier=selected_tier,candidate_selected_rate_pct=selected_rate_pct)]
  change<-z$selected_tier%in%c('peer','no_eligible_rate')
  z[change & selected_peer_new%in%TRUE,`:=`(candidate_selected_tier='peer',candidate_selected_rate_pct=candidate_peer_rate_pct)]
  z[change & !(selected_peer_new%in%TRUE),`:=`(candidate_selected_tier='no_eligible_rate',candidate_selected_rate_pct=NA_real_)]
  z[,candidate_authority:='isolated_owner_requested_build_not_production']
  z
})
cs<-rbindlist(candidates)
hi<-!cs$selected_tier%in%c('peer','no_eligible_rate')
stopifnot(all(cs$candidate_selected_tier[hi]==cs$selected_tier[hi]),
  identical(cs$candidate_selected_rate_pct[hi],cs$selected_rate_pct[hi]))
fwrite(cs,file.path(out,'selected_reference_candidate.csv'))
fwrite(cs[historical_lmic_reporting_scope==TRUE,.(country_years=.N,countries=uniqueN(iso3)),
  by=.(method,candidate_selected_tier)],file.path(out,'benchmark_tier_counts.csv'))
# Same-case comparison against the production peer design; cases lacking a new
# peer remain in the coverage table and are not counted as prediction successes.
val<-p[mode=='target_rating_hidden' & is.finite(estimate)]
val<-merge(val,val[method=='current',.(iso3,analysis_year,baseline=estimate)],by=key)
val[,`:=`(benefit=abs(baseline-actual)-abs(estimate-actual),estimate_a=estimate,actual_a=actual,
  estimate_b=baseline,actual_b=actual,period=ifelse(analysis_year>=2018,'2018-2024','2012-2017'))]
fwrite(val,file.path(out,'peer_rate_validation_cases.csv'))
fwrite(val[,as.data.table(p15_geography_inference(as.data.frame(.SD))),by=method],file.path(out,'peer_rate_validation.csv'))
fwrite(val[,.(n=.N,countries=uniqueN(iso3),baseline_mae=mean(abs(baseline-actual)),variant_mae=mean(abs(estimate-actual)),
  bias=mean(estimate-actual),rmse=sqrt(mean((estimate-actual)^2))),by=.(method,historical_income_level)],file.path(out,'peer_rate_validation_income.csv'))
fwrite(val[,.(n=.N,countries=uniqueN(iso3),baseline_mae=mean(abs(baseline-actual)),variant_mae=mean(abs(estimate-actual))),
  by=.(method,period)],file.path(out,'peer_rate_validation_period.csv'))
# Rating validation is separate from rate validation. An observed target rating
# is a comparison outcome and never a model input in this script.
if(nrow(sc)){
 fwrite(sc[,.(input_country_years=.N,finite_shadow_grades=sum(is.finite(shadow_notch)),
   complete_shadow_grades=sum(is.finite(shadow_notch)&scorecard_complete),
   conclusion=if(any(is.finite(shadow_notch)&scorecard_complete))'Numerically usable grades for this named scenario; source adaptations remain separate' else 'No complete shadow grades: peer figures are observed-rating controls, not new scorecard estimates'),
   by=.(scenario,status)],file.path(out,'scorecard_build_status.csv'))
 rv<-merge(sc,x[,.(iso3,analysis_year,country,historical_income_level,historical_lmic_reporting_scope,notch)],by=key)
 rv<-rv[is.finite(shadow_notch)&is.finite(notch)]
 rv[,error:=shadow_notch-notch]
 fwrite(rv,file.path(out,'rating_validation_cases.csv'))
 fwrite(rv[,.(n=.N,countries=uniqueN(iso3),mae=mean(abs(error)),rmse=sqrt(mean(error^2)),bias=mean(error),
   within_two=mean(abs(error)<=2),within_three=mean(abs(error)<=3)),
   by=.(scenario,scorecard_complete,historical_lmic_reporting_scope)],file.path(out,'rating_validation.csv'))
 fwrite(rv[historical_lmic_reporting_scope==TRUE,.(n=.N,countries=uniqueN(iso3),mae=mean(abs(error)),within_two=mean(abs(error)<=2)),
   by=.(scenario,scorecard_complete,historical_income_level)],file.path(out,'rating_validation_income.csv'))
 fwrite(rv[historical_lmic_reporting_scope==TRUE,.(n=.N,countries=uniqueN(iso3),mae=mean(abs(error)),
   rmse=sqrt(mean(error^2)),bias=mean(error),within_two=mean(abs(error)<=2),within_three=mean(abs(error)<=3),
   mean_interval_width=mean(shadow_notch_upper-shadow_notch_lower,na.rm=TRUE),
   actual_within_interval=mean(notch>=shadow_notch_lower&notch<=shadow_notch_upper,na.rm=TRUE)),
   by=.(scenario,scorecard_complete,analysis_year)],file.path(out,'rating_validation_year.csv'))
}

# Reuse the saved cash flows and the already accepted broader-loan formula.
crs<-fread(inputs[8]);flows<-fread(inputs[9]);loans<-fread(inputs[10])
cp<-crs[benchmark_selected_tier=='peer' & is.finite(ge_market_pct) & historical_lmic_reporting_scope==TRUE]
fs<-split(flows,flows$loan_id)
value_crs<-function(id,rate){if(!is.finite(rate))return(NA_real_);f<-fs[[id]];stopifnot(nrow(f)>0)
  100-sum(f$payment/(1+rate/100)^f$time_years)}
stopifnot(all(abs(mapply(value_crs,cp$loan_id,cp$benchmark_selected_rate_pct)-cp$ge_market_pct)<1e-9))
crsv<-merge(dep[selected_tier=='peer',.(method,iso3,commitment_year=analysis_year,
  estimate=ifelse(selected_peer_new,estimate,NA_real_),rule,estimate_depends_on_incomplete_input)],
  cp[,.(iso3,commitment_year,loan_id,ge_market_pct,ge_standardized_pct,amount_usd)],by=c('iso3','commitment_year'),allow.cartesian=TRUE)
crsv[,new_ge:=mapply(value_crs,loan_id,estimate)]
crsv[,`:=`(ge_change=new_ge-ge_market_pct,period=ifelse(commitment_year>=2018,'2018-2024','2012-2017'))]
fwrite(crsv,file.path(out,'crs_valuation_cases.csv'))
fwrite(crsv[,.(current_records=.N,retained=sum(is.finite(new_ge)),lost=sum(!is.finite(new_ge)),
  mean_ge_change_retained=mean(ge_change,na.rm=TRUE),mean_abs_ge_change_retained=mean(abs(ge_change),na.rm=TRUE),
  weighted_ge_change_retained=weighted.mean(ge_change,amount_usd,na.rm=TRUE)),by=.(method,period)],file.path(out,'crs_valuation_summary.csv'))
lp<-loans[benchmark_selected_tier=='peer' & is.finite(ge_market_pct) & historical_lmic_reporting_scope==TRUE]
stopifnot(all(abs(mapply(loan_ge,lp$interest_rate_pct,lp$maturity_years,lp$first_principal_payment_years,
  lp$benchmark_selected_rate_pct)-lp$ge_market_pct)<1e-9))
lv<-merge(dep[selected_tier=='peer',.(method,iso3,commitment_year=analysis_year,
  estimate=ifelse(selected_peer_new,estimate,NA_real_),rule,estimate_depends_on_incomplete_input)],
  lp[,.(iso3,commitment_year,loan_id,dataset,cohort,ge_market_pct,interest_rate_pct,maturity_years,
    first_principal_payment_years,ge_standardized_category_pct)],by=c('iso3','commitment_year'),allow.cartesian=TRUE)
lv[,new_ge:=mapply(loan_ge,interest_rate_pct,maturity_years,first_principal_payment_years,estimate)]
lv[,`:=`(ge_change=new_ge-ge_market_pct,period=ifelse(commitment_year>=2018,'2018-2024','2012-2017'))]
fwrite(lv,file.path(out,'other_loan_valuation_cases.csv'))
fwrite(lv[,.(current_records=.N,retained=sum(is.finite(new_ge)),lost=sum(!is.finite(new_ge)),
  mean_ge_change_retained=mean(ge_change,na.rm=TRUE),mean_abs_ge_change_retained=mean(abs(ge_change),na.rm=TRUE)),
  by=.(method,dataset,cohort,period)],file.path(out,'other_loan_valuation_summary.csv'))
valuation_scope<-rbindlist(lapply(c('all_countries','historical_LMIC'),function(scope){
  cx<-if(scope=='historical_LMIC')crs[historical_lmic_reporting_scope==TRUE] else crs
  lx<-if(scope=='historical_LMIC')loans[historical_lmic_reporting_scope==TRUE] else loans
  data.table(application=c('CRS','CRS modern','other loans'),reporting_scope=scope,
    current_finite_peer_records=c(cx[benchmark_selected_tier=='peer'&is.finite(ge_market_pct),.N],
      cx[commitment_year>=2018&benchmark_selected_tier=='peer'&is.finite(ge_market_pct),.N],
      lx[benchmark_selected_tier=='peer'&is.finite(ge_market_pct),.N]),
    unchanged_finite_nonpeer_records=c(cx[benchmark_selected_tier!='peer'&is.finite(ge_market_pct),.N],
      cx[commitment_year>=2018&benchmark_selected_tier!='peer'&is.finite(ge_market_pct),.N],
      lx[benchmark_selected_tier!='peer'&is.finite(ge_market_pct),.N]))
}))
fwrite(valuation_scope,file.path(out,'valuation_scope.csv'))
after<-manifest(inputs,'immutable_production_or_seed_input');stopifnot(identical(before,after))
if(nrow(sc))stopifnot(identical(score_before,manifest(score_path,'scorecard_input')))
fwrite(data.table(path=before$path,sha256_before=before$sha256,sha256_after=after$sha256,unchanged=before$sha256==after$sha256),
  file.path(out,'production_preservation_check.csv'))
checks<-data.table(check=c('production_peer_778_rates','production_rule_counts','production_peer_membership','actual_moodys_grades_match_current_core',
  'minimum_three_distinct_donors','target_excluded_all_sources','one_source_per_country_year',
  'rating_implied_donors_excluded','current_tier_eligibility_and_source_priority_reconciled','first_qualifying_rule','stronger_selected_tiers_and_rates_unchanged',
  'hidden_target_validation_215_cases_47_countries','original_crs_cashflows_replay','original_other_loan_formula_replay',
  'production_inputs_preserved','incomplete_inputs_not_used_by_complete_only_designs'),passed=TRUE)
stopifnot(!any(p[input_policy=='complete_only',estimate_depends_on_incomplete_input]))
if(nrow(sc) && !any(is.finite(sc$shadow_notch))){
  stopifnot(!any(p$target_shadow_used),all(p$donor_shadow_members==0L),
    all(designs[scenario!='observed_only',role]=='blocked_scorecard_control'))
  checks<-rbind(checks,data.table(check='all_missing_scorecard_does_not_claim_new_shadow_estimates',passed=TRUE))
}
fwrite(checks,file.path(out,'checks.csv'))
fwrite(manifest(c('experiments/p15_wb_scorecard_20260912/build_peer_candidate.R',
  'experiments/p15_wb_scorecard_20260912/peer/test_matching.R',
  'scripts/p15/loan_extension/loan_valuation.R','scripts/p15/loan_extension/benchmark_matching.R',
  'R/p15_peer_geography_validation.R','renv.lock'),'code_or_environment'),
  file.path(out,'code_manifest.csv'))
writeLines(capture.output(sessionInfo()),file.path(out,'r_session_info.txt'))
outputs<-list.files(out,full.names=TRUE,pattern='\\.(csv|txt|md)$')
outputs<-outputs[basename(outputs)!='output_manifest.csv']
fwrite(manifest(outputs,'isolated_peer_candidate_output'),file.path(out,'output_manifest.csv'))
print(coverage);cat('Checks:',nrow(checks),'passed. Scorecard scenarios:',uniqueN(sc$scenario),'\n')
