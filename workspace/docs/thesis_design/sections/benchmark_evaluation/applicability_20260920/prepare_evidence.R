library(data.table)
library(jsonlite)
library(digest)
j <- fromJSON('data-derived/p15_master/current_run.json')
out <- 'docs/thesis_design/sections/benchmark_evaluation/applicability_20260920'
used <- 'data-derived/p15_master/current_run.json'
read_stage <- function(stage,name) { p<-file.path(j$stages[[stage]]$dir,name);used<<-c(used,p);fread(p) }
p <- read_stage('rating_nested','outer_predictions.csv')
saved <- read_stage('rating_nested','summary.csv')
sample <- read_stage('rating_nested','sample.csv')
audit <- read_stage('rating_nested','fit_audit.csv')
choices <- read_stage('rating_nested','selections.csv')
stopifnot(!anyDuplicated(p[,.(branch,model,iso3,analysis_year)]), all(audit$training_last_year<audit$test_year),all(audit[phase=='inner',test_year<outer_year]))
held <- audit[branch=='country_history_withheld']
stopifnot(all(mapply(function(ids,id) !id %in% strsplit(ids,';',fixed=TRUE)[[1]], held$training_country_ids,held$held_outer_country)))
z <- p[,.(rows=.N,countries=uniqueN(iso3),mae_pp=mean(abs(prediction_pct-observed_rate_pct)),rmse_pp=sqrt(mean((prediction_pct-observed_rate_pct)^2))),by=.(branch,model)]
a<-merge(z,saved,by=c('branch','model'))
stopifnot(nrow(a)==7,all(a$rows.x==a$rows.y),all(abs(a$mae_pp.x-a$mae_pp.y)<1e-10),all(abs(a$rmse_pp.x-a$rmse_pp.y)<1e-10))
fwrite(z,file.path(out,'calibration_summary.csv'))
nat<-p[branch=='temporal',.(rows=.N,countries=uniqueN(iso3),mae_pp=mean(abs(prediction_pct-observed_rate_pct))),by=.(model,has_prior_country_history=natural_history_rows>0)]
a<-merge(nat,read_stage('rating_nested','natural_history_summary.csv'),by=c('model','has_prior_country_history'))
stopifnot(all(a$rows.x==a$rows.y),all(abs(a$mae_pp.x-a$mae_pp.y)<1e-10))
fwrite(nat,file.path(out,'natural_history_groups.csv'))
selpath<-file.path(j$candidate,'selected_reference.csv'); used<-c(used,selpath)
sel<-fread(selpath)[historical_lmic_reporting_scope==TRUE & selected_tier=='moodys']
sel[,earlier_matched_observations:=vapply(seq_len(.N),function(i)sample[iso3==sel$iso3[i]&analysis_year<sel$analysis_year[i],.N],integer(1))]
target<-rbindlist(lapply(c(2012L,2018L),function(start)sel[analysis_year>=start,.(period=paste0(start,'_2024'),rows=.N,countries=uniqueN(iso3),rows_with_prior_history=sum(earlier_matched_observations>0),rows_without_prior_history=sum(earlier_matched_observations==0))]))
a<-merge(target,read_stage('rating_nested','actual_target_summary.csv'),by='period');stopifnot(all(a$rows.x==a$rows.y),all(a$rows_without_prior_history.x==a$rows_without_prior_history.y))
fwrite(target,file.path(out,'rating_use_history.csv'))
annual<-p[,.(mae=mean(abs(prediction_pct-observed_rate_pct))),by=.(branch,model,analysis_year)]
fwrite(annual,file.path(out,'calibration_annual.csv'))
composition<-read_stage('assessment','deployment_by_income.csv')
composition[,total:=sum(N),by=.(method,cohort)];composition[,share:=N/total]
fwrite(composition,file.path(out,'evaluation_and_use_income.csv'))
peer<-read_stage('peer_geography_validation','validation_rows.csv')[method=='current' & is.finite(estimate)]
ps<-peer[,.(n=.N,countries=uniqueN(iso3),mae=mean(abs(estimate-actual)),low_income=sum(historical_income_level=='Low income')),by=information]
a<-merge(ps,read_stage('peer_geography_validation','all_available_scores.csv')[method=='current'],by='information');stopifnot(all(a$n.x==a$n.y),all(abs(a$mae.x-a$mae.y)<1e-10))
common<-merge(peer[information=='available_rating',.(iso3,analysis_year,actual,available=estimate)],peer[information=='target_rating_hidden',.(iso3,analysis_year,hidden=estimate)],by=c('iso3','analysis_year'))
fwrite(ps,file.path(out,'peer_information_summaries.csv'))
fwrite(common[,.(n=.N,countries=uniqueN(iso3),available_mae=mean(abs(available-actual)),hidden_mae=mean(abs(hidden-actual)))],file.path(out,'peer_information_common_cases.csv'))
ppath<-file.path(j$candidate,'all_peer_predictions.csv');used<-c(used,ppath)
ap<-fread(ppath)[method=='preferred' & mode=='deployment' & selected_peer_new==TRUE & historical_lmic_reporting_scope==TRUE]
stopifnot(nrow(ap)==731,all(vapply(seq_len(nrow(ap)),function(i)!ap$iso3[i]%in%strsplit(ap$member_ids[i],';',fixed=TRUE)[[1]],logical(1))))
fwrite(ap[,.(selected_country_years=.N,year_specific_groups=uniqueN(paste(analysis_year,member_ids)),selected_estimates_with_rating_implied_members=sum(rating_implied_members>0))],file.path(out,'peer_shared_inputs.csv'))
used<-c(used,'R/p15_rating_nested_check.R','R/p15_rating_country_history_calibration.R','scripts/p15/build_rating_nested_check.R','R/p15_revised_peer_matching.R','R/p15_revised_peer_validation.R',file.path(out,'prepare_evidence.R'))
manifest<-data.table(path=unique(used));manifest[,sha256:=vapply(path,digest,character(1),file=TRUE,algo='sha256')];fwrite(manifest,file.path(out,'input_manifest.csv'))
fwrite(data.table(check=c('nested summary reproduced from predictions','earlier-year and held-country restrictions checked','natural-history summary reproduced','actual selected-rating histories recounted','peer scores reproduced and common cases calculated','selected peer groups and target exclusion recounted'),passed=TRUE),file.path(out,'verification.csv'))
print(target);print(ps);print(fread(file.path(out,'peer_information_common_cases.csv')));print(fread(file.path(out,'peer_shared_inputs.csv')))
core_path<-file.path(j$candidate,'core_evidence.csv');elig_path<-file.path(j$candidate,'tier_eligibility.csv')
core<-fread(core_path)[historical_lmic_reporting_scope==TRUE,.(iso3,analysis_year,historical_income_level)]
elig<-fread(elig_path)[eligible==TRUE]
primary<-elig[tier=='primary',.(iso3,analysis_year)]
selected<-fread(selpath)[historical_lmic_reporting_scope==TRUE]
comps<-rbindlist(lapply(c('moodys','peer'),function(m){
 v<-merge(primary,elig[tier==m,.(iso3,analysis_year)],by=c('iso3','analysis_year'))
 v<-merge(v,core,by=c('iso3','analysis_year'));v[,cohort:='validation_primary_overlap']
 u<-merge(selected[selected_tier==m,.(iso3,analysis_year)],core,by=c('iso3','analysis_year'));u[,cohort:='actual_selected_fallback']
 rbind(v,u)[,method:=m][,.(N=.N),by=.(method,cohort,historical_income_level)]
}))
a<-merge(comps,composition,by=c('method','cohort','historical_income_level'))
stopifnot(nrow(a)==nrow(composition),all(a$N.x==a$N.y))
extra<-data.table(path=c(core_path,elig_path));extra[,sha256:=vapply(path,digest,character(1),file=TRUE,algo='sha256')]
fwrite(unique(rbind(manifest,extra)),file.path(out,'input_manifest.csv'))
checks<-fread(file.path(out,'verification.csv'));fwrite(rbind(checks,data.table(check='evaluation and selected-use income counts reproduced from current core/eligibility/selection',passed=TRUE)),file.path(out,'verification.csv'))
