#!/usr/bin/env Rscript
source("scripts/p15/activate_p15_environment.R")
suppressPackageStartupMessages({library(data.table);library(digest);library(jsonlite)})
args<-commandArgs(TRUE)
base<-if(length(args))args[1] else "data-derived/p15_statistical_annotation_review_20260911_v1"
out<-if(length(args)>1)args[2] else "data-derived/p15_statistical_annotation_review_verification_20260911_v1"
stopifnot(!dir.exists(out))
manifest<-fread(file.path(base,"input_manifest.csv"))
getsource<-function(name) {
 paths<-manifest[basename(path)==name,path];stopifnot(length(paths)==1L);fread(paths,na.strings="")
}
r<-function(name)fread(file.path(base,paste0(name,".csv")),na.strings="")
results<-list();check<-function(name,value) {results[[length(results)+1L]]<<-data.table(check=name,passed=isTRUE(value));if(!isTRUE(value))stop(name)}
for(file in c("input_manifest.csv","code_manifest.csv","output_manifest.csv")) {
 m<-fread(file.path(base,file));check(paste0(file,"_all_hashes"),all(vapply(m$path,function(p)digest(file=p,algo="sha256"),character(1))==m$sha256))
}

original<-getsource("comparison_inputs.csv");metrics<-r("benchmark_error_metrics")
criteria<-list(modern2018_2024=list(2018:2024,"dac_modern_2018"),standardized2012_2017=list(2012:2017,"dac_group976"),
 standardized2012_2024=list(2012:2024,"dac_group976"),transition2015_2017=list(2015:2017,"dac_ge_from2015"),
 historical10_2012_2017=list(2012:2017,"dac_headline_new"))
errors<-vapply(seq_len(nrow(metrics)),function(j) {
 z<-metrics[j];s<-criteria[[z$scenario]]
 d<-original[analysis_year%in%s[[1]]&is.finite(primary)&is.finite(get(z$tier))&is.finite(get(s[[2]]))]
 a<-d[[z$tier]]-d$primary;b<-d[[s[[2]]]]-d$primary
 max(abs(c(mean(a)-z$focal_bias_pp,sqrt(sum(a*a)/length(a))-z$focal_rmse_pp,
 mean(abs(a))-z$focal_mae_pp,mean(abs(b))-z$comparator_mae_pp,nrow(d)-z$records)))
},numeric(1))
check("all_twenty_metric_rows_from_original_inputs",max(errors)<1e-10)
check("rmse_squared_bias_variance_identity",all(abs(metrics$focal_rmse_pp^2-
 (metrics$focal_bias_pp^2+(metrics$records-1)/metrics$records*metrics$focal_error_sd_pp^2))<1e-10))

selected<-getsource("selected_reference.csv");tiers<-getsource("tier_eligibility.csv")
st<-r("tier_stability_country")
stable_error<-vapply(seq_len(nrow(st)),function(i) {
 z<-st[i];a<-tiers[iso3==z$iso3&tier==z$tier&analysis_year%in%2018:2024]
 hold<-(a$iso3=="LBN"&a$analysis_year%in%2020:2023)|(a$iso3=="BLR"&a$analysis_year%in%2022:2024)|(a$iso3=="RUS"&a$analysis_year==2022)
 allowed<-a$eligible&is.finite(a$rate_pct)&!(a$tier=="secondary"&hold)
 valid<-original[iso3==z$iso3&analysis_year%in%2018:2024&is.finite(primary)&is.finite(dac_modern_2018),analysis_year]
 identical(z$available_years,sum(allowed))&&identical(z$tier_and_primary_validation_years,sum(allowed&a$analysis_year%in%valid))
},logical(1))
check("all_country_tier_availability_and_validation_counts_from_raw_eligibility",all(stable_error))

lv<-getsource("loan_valuations.csv");tv<-getsource("same_loan_all_tier_valuations.csv")
fixed<-r("fixed_method_loan_details");fs<-r("fixed_method_period_contrasts")
at<-match(fixed$loan_id,lv$loan_id);tt<-match(paste(fixed$loan_id,fixed$tier),paste(tv$loan_id,tv$tier))
check("fixed_method_support_exists_in_original_loan_and_tier_tables",!anyNA(at)&&!anyNA(tt))
check("fixed_method_original_values_and_unchanged_terms",all(abs(fixed$fixed_gap_pp-(tv$tier_ge_pct[tt]-lv$ge_policy_pct[at]))<1e-10)&
 all(abs(fixed$selected_gap_pp-(lv$ge_market_pct[at]-lv$ge_policy_pct[at]))<1e-10)&
 all(fixed$interest_rate_pct==lv$interest_rate_pct[at])&all(fixed$maturity_years==lv$maturity_years[at])&
 all(fixed$first_principal_payment_years==lv$first_principal_payment_years[at]))
contrast_error<-vapply(seq_len(nrow(fs)),function(i) {
 z<-fs[i];d<-fixed[dataset==z$dataset&support==z$support&tier==z$tier]
 e<-d$commitment_year<=2021;l<-!e
 if(!any(e)||!any(l))return(0)
 max(abs(c(mean(d$fixed_gap_pp[l])-mean(d$fixed_gap_pp[e])-z$fixed_contrast_pp,
 mean(d$selected_gap_pp[l])-mean(d$selected_gap_pp[e])-z$selected_contrast_pp)))
},numeric(1))
check("fixed_method_period_contrasts_independent_means",max(contrast_error)<1e-10)

old<-getsource("analysis_rows.csv");cluster<-r("loan_cluster_design")
old_primary<-r("existing_loan_primary_inference");old_shared<-r("existing_loan_shared_year_inference")
for(k in c("aiddata","add")) {
 d<-old[dataset==k];fit<-lm(delta_ge_pp~late,data=d)
 check(paste0(k,"_period_coefficient_is_difference_of_record_means"),abs(coef(fit)[2]-old_primary[dataset==k,estimate_pp])<1e-10)
 deletions<-vapply(unique(d$iso3),function(g)coef(lm(delta_ge_pp~late,data=d[iso3!=g]))[2],numeric(1))
 variance<-(length(deletions)-1)/length(deletions)*sum((deletions-mean(deletions))^2)
 check(paste0(k,"_existing_jackknife_se_from_independent_OLS"),abs(sqrt(variance)-old_primary[dataset==k,se_pp])<1e-10)
 country_var<-sandwich::vcovCL(fit,cluster=d$iso3,type="HC1")[2,2]
 two_way<-sandwich::vcovCL(fit,cluster=list(d$iso3,d$commitment_year),type="HC1",multi0=FALSE)[2,2]
 check(paste0(k,"_existing_CR1_country_and_two_way_variance"),
 abs(country_var-old_shared[dataset==k&method=="country_CR1",variance_pp2])<1e-10 &&
 abs(two_way-old_shared[dataset==k&method=="two_way_country_year_CR1",variance_pp2])<1e-10)
 check(paste0(k,"_cluster_variance_contribution_reconciles"),abs(sum(cluster[dataset==k&cluster_dimension=="iso3",centered_jackknife_ss])*
 (uniqueN(d$iso3)-1)/uniqueN(d$iso3)-variance)<1e-10)
}

pairs<-getsource("matched_creditor_pairs.csv");summary<-r("creditor_reversal_summary")
check("reversal_19_of_209_is_observed_frequency_not_independent_sample",nrow(pairs)==209L&&sum(pairs$interest_ranking_reversed)==19L&&
 summary[grouping=="all",reversal_countries]==14L&&abs(summary[grouping=="all",reversal_share]-19/209)<1e-12)
check("all_producer_checks_pass",all(r("checks")$passed))
dir.create(out,recursive=TRUE);checks<-rbindlist(results);fwrite(checks,file.path(out,"checks.csv"))
hashes<-function(ps)data.table(path=ps,sha256=vapply(ps,function(p)digest(file=p,algo="sha256"),character(1)),bytes=file.info(ps)$size)
fwrite(hashes(file.path(base,list.files(base))),file.path(out,"input_manifest.csv"))
fwrite(hashes(c("scripts/p15/annotation_followup_20260911/verify_statistical_review.R","renv.lock")),file.path(out,"code_manifest.csv"))
write_json(list(build_id=basename(out),lifecycle_status="diagnostic",diagnostic_parent=base,checks=nrow(checks)),file.path(out,"version_bundle.json"),auto_unbox=TRUE,pretty=TRUE)
writeLines(capture.output(sessionInfo()),file.path(out,"environment.txt"));fwrite(hashes(list.files(out,full.names=TRUE)),file.path(out,"output_manifest.csv"))
cat(nrow(checks),"independent statistical-review checks passed\n")
