#!/usr/bin/env Rscript
suppressPackageStartupMessages({library(data.table);library(dplyr);library(sandwich)})
source("R/research_governance.R")
source("R/p15_bounded_fallback_comparison.R")
source("R/p15_peer_geography_validation.R")
args<-commandArgs(TRUE)
out<-if(length(args))args[1] else "data-derived/p15_peer_geography_validation_20260909_v1"
base<-Sys.getenv("P15_ANALYSIS_BASE","data-derived/p15_analysis_candidate_20260909_region_v1")
region_path<-Sys.getenv("P15_REGION_MAP","data-derived/p15_region_correction_research_20260909_v2/regional/country_region_map.csv")
stopifnot(!dir.exists(out))
inputs<-c(file.path(base,c("core_evidence.csv","peer_region_context.csv","tier_eligibility.csv",
  "selected_reference.csv","output_manifest.csv")),region_path,"renv.lock")
initial<-vapply(inputs,pvr_sha256_file,character(1))
m<-fread(file.path(base,"output_manifest.csv"))
stopifnot(all(vapply(m$artifact_path,pvr_sha256_file,character(1))==m$sha256))
p<-as.data.frame(fread(file.path(base,"core_evidence.csv")))
ctx<-as.data.frame(fread(file.path(base,"peer_region_context.csv")))
e<-fread(file.path(base,"tier_eligibility.csv"))[tier=="primary"]
key<-function(x)paste(x$analysis_year,x$iso3)
stopifnot(!anyDuplicated(key(p)),!anyDuplicated(key(ctx)),!anyDuplicated(key(e)))
ci<-match(key(p),key(ctx));ei<-match(key(p),key(e));stopifnot(!anyNA(ci),!anyNA(ei))
p$rating_source_region<-ctx$rating_source_region[ci]
p$moodys_rating_normalized<-ctx$moodys_rating_normalized[ci]
p$primary_eligible<-e$eligible[ei]
geo<-fread(region_path)
# Use the same declared reporting partition as the regional research tables.
print(names(geo))
region_col<-if("region_pre2025" %in% names(geo))"region_pre2025" else if("region" %in% names(geo))"region" else stop("Inspect region map schema")
stopifnot(!anyDuplicated(geo$iso3))
p$reporting_region<-geo[[region_col]][match(p$iso3,geo$iso3)]
message("Computing fixed peer methods, target excluded in every case")
pred<-as.data.table(rbind(p15_geography_predictions(p,FALSE),p15_geography_predictions(p,TRUE)))
meta<-as.data.table(p)[,.(analysis_year,iso3,country,reporting_region,historical_income_level,
  historical_lmic_reporting_scope,primary_eligible,actual=primary_usd_market_rate_pct,
  genuinely_unrated=is.na(moodys_rating_normalized)|!nzchar(moodys_rating_normalized))]
pred<-merge(pred,meta,by=c("analysis_year","iso3"))
v<-pred[historical_lmic_reporting_scope & primary_eligible & is.finite(actual)]
v[,error:=estimate-actual]
ref<-pred[information=="available_rating"&method=="current"]
at<-match(key(p),key(ref))
checks<-data.table(check="reproduce_current_all_country_peer_values",passed=
  identical(is.na(p$peer_rate_pct),is.na(ref$estimate[at]))&&all(abs(p$peer_rate_pct-ref$estimate[at])<1e-10,na.rm=TRUE))
addcheck<-function(name,ok){checks<<-rbind(checks,data.table(check=name,passed=isTRUE(ok)));stopifnot(isTRUE(ok))}
addcheck("current_reproduction",checks$passed[1])
addcheck("no_self_seeds",all(!mapply(function(id,s)id %in% strsplit(s,";",fixed=TRUE)[[1]],pred$iso3,pred$seed_ids)))
addcheck("minimum_three_for_finite",all(pred$seed_count[is.finite(pred$estimate)]>=3))
addcheck("unique_predictions",!anyDuplicated(pred[,.(analysis_year,iso3,method,information)]))
addcheck("mask_has_no_effect_on_simple_methods",{
  a<-pred[information=="available_rating"&method %in% c("region_only","worldwide_only")]
  b<-pred[information=="target_rating_hidden"&method %in% c("region_only","worldwide_only")]
  identical(a$estimate,b$estimate)
})
coverage<-v[,.(target_rows=.N,available=sum(is.finite(estimate)),countries=uniqueN(iso3),
  available_countries=uniqueN(iso3[is.finite(estimate)])),by=.(information,method)]
scores<-v[is.finite(estimate),p15_geography_metrics(.SD),by=.(information,method)]
paired<-rbindlist(lapply(unique(v$information),function(info){
  q<-as.data.frame(v[information==info])
  rbind(p15_geography_paired(q,"current","no_geography"),
        p15_geography_paired(q,"region_only","worldwide_only"))
}),fill=TRUE)
sample_rows<-function(x) {
  parts<-list(full=x,modern=x[analysis_year>=2018],early=x[analysis_year<=2021],late=x[analysis_year>=2022],
    geography_changes_estimate=x[abs(estimate_a-estimate_b)>1e-10],
    genuinely_unrated=x[genuinely_unrated_a==TRUE],
    SSA=x[reporting_region_a=="Sub-Saharan Africa"])
  for(r in sort(unique(x$reporting_region_a)))if(!is.na(r))parts[[paste0("region:",r)]]<-x[reporting_region_a==r]
  parts
}
summaries<-list();influence<-list();k<-0L;j<-0L
for(info in unique(paired$information))for(a in unique(paired$method_a)) {
  x<-paired[information==info&method_a==a]
  for(label in names(sample_rows(x))) {
    z<-sample_rows(x)[[label]];if(!nrow(z))next
    k<-k+1L;summaries[[k]]<-cbind(data.table(information=info,method_a=a,method_b=z$method_b[1],sample=label),
      as.data.table(p15_geography_inference(as.data.frame(z))))
  }
  for(dimension in c("iso3","analysis_year","reporting_region_a"))for(value in unique(x[[dimension]])) {
    z<-x[x[[dimension]]!=value];if(!nrow(z))next
    j<-j+1L;influence[[j]]<-data.table(information=info,method_a=a,omission_dimension=dimension,
      omitted=as.character(value),n=nrow(z),benefit=mean(z$benefit))
  }
}
summaries<-rbindlist(summaries);influence<-rbindlist(influence)
annual_scores<-paired[,.(n=.N,benefit=mean(benefit),mae_a=mean(abs(estimate_a-actual_a)),
  mae_b=mean(abs(estimate_b-actual_b))),by=.(information,method_a,analysis_year)]
setorder(v,information,method,iso3,analysis_year)
v[,`:=`(prior_year=shift(analysis_year),prior_actual=shift(actual),prior_estimate=shift(estimate),
  prior_rule=shift(pool_rule),prior_seed_ids=shift(seed_ids)),by=.(information,method,iso3)]
chg<-v[analysis_year-prior_year==1 & is.finite(estimate)&is.finite(prior_estimate)]
chg[,`:=`(level_actual=actual,level_estimate=estimate,actual=actual-prior_actual,estimate=estimate-prior_estimate,
  rule_changed=pool_rule!=prior_rule,membership_changed=seed_ids!=prior_seed_ids)]
change_scores<-chg[,p15_geography_metrics(.SD),by=.(information,method)]
change_paired<-rbindlist(lapply(unique(chg$information),function(info){
  q<-as.data.frame(chg[information==info])
  rbind(p15_geography_paired(q,"current","no_geography"),p15_geography_paired(q,"region_only","worldwide_only"))
}),fill=TRUE)
change_summary<-change_paired[,as.data.table(p15_geography_inference(as.data.frame(.SD))),by=.(information,method_a,method_b)]
nochange<-chg[,.(n=.N,mae_method=mean(abs(estimate-actual)),mae_no_change=mean(abs(actual)),
  rule_switches=sum(rule_changed),membership_switches=sum(membership_changed)),by=.(information,method)]
# More relevant fallback population, for coverage and consequences, not accuracy.
s<-fread(file.path(base,"selected_reference.csv"))[historical_lmic_reporting_scope&selected_tier=="peer"]
deployment<-merge(pred[information=="available_rating"],s[,.(analysis_year,iso3,selected_rate_pct)],by=c("analysis_year","iso3"))
deployment[,delta_from_current:=estimate-selected_rate_pct]
deploy_summary<-deployment[,.(n=.N,available=sum(is.finite(estimate)),changed=sum(abs(delta_from_current)>1e-10,na.rm=TRUE),
  mean_abs_change=mean(abs(delta_from_current),na.rm=TRUE)),by=method]
addcheck("all_primary_targets_scored",nrow(unique(v[,.(analysis_year,iso3)]))==sum(p$historical_lmic_reporting_scope&p$primary_eligible))
addcheck("region_only_strict",all(pred$same_region_share[pred$method=="region_only" & is.finite(pred$estimate)]==1))
addcheck("no_2024_ssa_primary_change_test",!nrow(chg[analysis_year==2024&reporting_region=="Sub-Saharan Africa"]))
addcheck("source_inputs_unchanged",identical(initial,vapply(inputs,pvr_sha256_file,character(1))))
stopifnot(all(checks$passed))
bundle<-list(build_id=basename(out),schema_id="SCHEMA-P15-PEER-GEOGRAPHY-VALIDATION-V1",
  estimator_id="EST-P15-PEER-GEOGRAPHY-DIAGNOSTIC-V1",admissibility_id="ADM-P15-SOURCE-CLOSURE-V1",
  selection_id="SEL-P15-REFERENCE-REGION-CORRECTED-V1",source_package_ids="SRC-P15-RAW-CLOSURE-20260906;SRC-P15-SOURCE-CLOSURE-20260907;SRC-WB-COUNTRIES-LOCAL-PEER-20260909")
tables<-list(predictions=pred,validation_rows=v,coverage=coverage,all_available_scores=scores,
  paired_rows=paired,paired_summaries=summaries,influence=influence,annual_scores=annual_scores,
  change_rows=chg,change_scores=change_scores,change_paired_rows=change_paired,
  change_paired_summaries=change_summary,change_vs_no_change=nochange,
  deployment_rows=deployment,deployment_summary=deploy_summary,checks=checks)
dir.create(out,recursive=TRUE)
for(name in names(tables)){d<-copy(tables[[name]]);for(b in names(bundle))set(d,j=b,value=bundle[[b]]);fwrite(d,file.path(out,paste0(name,".csv")),na="")}
manifest<-function(paths,role)do.call(pvr_manifest_rows,c(list(paths=paths,artifact_role=role),bundle))
fwrite(manifest(inputs,"input"),file.path(out,"input_manifest.csv"))
code<-c("R/p15_peer_geography_validation.R","R/p15_bounded_fallback_comparison.R","R/research_governance.R",
  "scripts/p15/build_p15_peer_geography_validation.R","docs/governance/P15_PEER_GEOGRAPHY_COMPARISON_DESIGN_2026-09-09.md")
fwrite(manifest(code,"code_and_design"),file.path(out,"code_manifest.csv"))
fwrite(manifest(file.path(out,paste0(names(tables),".csv")),"diagnostic_output"),file.path(out,"output_manifest.csv"))
capture.output(sessionInfo(),file=file.path(out,"environment.txt"))
print(summaries[sample %in% c("full","modern","SSA","genuinely_unrated")]);print(change_summary);print(deploy_summary);print(checks)
