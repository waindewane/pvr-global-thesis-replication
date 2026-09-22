source("R/p15_current_inputs.R")
#!/usr/bin/env Rscript
source("scripts/p15/activate_p15_environment.R")
suppressPackageStartupMessages({library(data.table);library(jsonlite);library(digest)})
source("scripts/p15/annotation_followup_20260911/statistical_helpers.R")
source("scripts/p15/loan_extension/benchmark_matching.R")
source("R/research_governance.R")
args<-commandArgs(TRUE)
out<-if(length(args))args[1] else file.path("data-derived",paste0("p15_statistical_review_",format(Sys.time(),"%Y%m%d_%H%M%S")))
stopifnot(!dir.exists(out))
pvr_assert_write_allowed(out,"diagnostic")
current<-fromJSON("data-derived/p15_master/current_run.json")
parent<-function(k,env) {v<-Sys.getenv(env);if(nzchar(v))v else if(k=="candidate")current$candidate else current$stages[[k]]$dir}
base<-parent("candidate","P15_ANALYSIS_BASE")
official<-parent("official_policy","P15_OFFICIAL_POLICY_BASE")
bench<-parent("benchmark_inference","P15_BENCHMARK_INFERENCE_BASE")
loan<-parent("loan_comparisons","P15_LOAN_BASE")
period<-parent("loan_period_inference","P15_LOAN_PERIOD_BASE")
pv<-parent("pv_interpretation","P15_PV_INTERPRETATION_BASE")
followup<-parent("pv_followup","P15_PV_FOLLOWUP_BASE")
consumed<-character()
read_input<-function(folder,f) {p<-file.path(folder,f);consumed<<-unique(c(consumed,p));fread(p,na.strings="")}
sel<-read_input(base,"selected_reference.csv")
elig<-read_input(base,"tier_eligibility.csv")
core<-read_input(base,"core_evidence.csv")
ref<-p15_loan_benchmark_reference(sel,elig,core)
d<-read_input(official,"comparison_inputs.csv")
saved_loss<-read_input(bench,"paired_loss_details.csv")
saved_inference<-read_input(bench,"paired_loss_inference.csv")
saved_two_way<-read_input(bench,"paired_loss_country_year_sensitivity.csv")
consecutive<-read_input(bench,"consecutive_details.csv")

# Same-observation error summaries; no new tests are fitted.
scenarios<-list(modern2018_2024=list(years=2018:2024,rate="dac_modern_2018"),
 standardized2012_2017=list(years=2012:2017,rate="dac_group976"),
 standardized2012_2024=list(years=2012:2024,rate="dac_group976"),
 transition2015_2017=list(years=2015:2017,rate="dac_ge_from2015"),
 historical10_2012_2017=list(years=2012:2017,rate="dac_headline_new"))
loss<-rbindlist(lapply(names(scenarios),function(k)rbindlist(lapply(c("ids","secondary","moodys","peer"),function(tier_name) {
 s<-scenarios[[k]];z<-d[analysis_year%in%s$years&is.finite(primary)&is.finite(get(tier_name))&is.finite(get(s$rate))]
 z[,.(scenario=k,tier=tier_name,iso3,country,analysis_year,primary,focal_rate=get(tier_name),comparator_rate=get(s$rate),
   focal_error=get(tier_name)-primary,comparator_error=get(s$rate)-primary)]
}))))
metric<-loss[,stat_error_metrics(.SD),by=.(scenario,tier)]
metric_year<-loss[,stat_error_metrics(.SD),by=.(scenario,tier,analysis_year)]
modern_loss<-loss[scenario=="modern2018_2024"]
benchmark_year_design<-modern_loss[,.(records=.N,countries=uniqueN(iso3),
 mean_improvement_pp=mean(abs(comparator_error)-abs(focal_error))),by=.(tier,analysis_year)]
benchmark_year_design[,`:=`(year_record_share=records/sum(records),
 leave_year_out_improvement_pp=(sum(records*mean_improvement_pp)-records*mean_improvement_pp)/(sum(records)-records)),by=tier]

# Every-year availability, not just the hierarchy's selected tier.
cy<-ref$country_year[analysis_year%in%2018:2024]
valid<-d[analysis_year%in%2018:2024&is.finite(primary)&is.finite(dac_modern_2018),.(iso3,analysis_year)]
cy[,primary_policy_validation_available:=paste(iso3,analysis_year)%in%paste(valid$iso3,valid$analysis_year)]
t<-merge(ref$tiers[analysis_year%in%2018:2024&tier!="peer"],
 cy[,.(iso3,analysis_year,benchmark_selected_tier,benchmark_selected_ordinary_usable,primary_policy_validation_available)],
 by=c("iso3","analysis_year"))
t[,usable:=ordinary_cost_tier_usable==TRUE&is.finite(benchmark_tier_rate_pct)]
t[is.na(usable),usable:=FALSE]
t[,selected_usable:=benchmark_selected_tier==tier&benchmark_selected_ordinary_usable==TRUE]
t[is.na(selected_usable),selected_usable:=FALSE]
stable<-t[,.(panel_years=uniqueN(analysis_year),available_years=sum(usable),selected_years=sum(selected_usable),
 historical_lmic_years=sum(historical_lmic_reporting_scope==TRUE),
 primary_validation_years=sum(primary_policy_validation_available),
 tier_and_primary_validation_years=sum(usable&primary_policy_validation_available)),by=.(iso3,tier)]
stable[,`:=`(available_all_seven=panel_years==7L&available_years==7L,
 selected_all_seven=panel_years==7L&selected_years==7L,
 available_and_primary_validation_all_seven=panel_years==7L&tier_and_primary_validation_years==7L)]
stable_summary<-rbindlist(lapply(c("all_panel_countries","historical_LMIC_every_year"),function(sc) {
 z<-if(sc=="all_panel_countries")stable else stable[historical_lmic_years==7L]
 z[,.(countries_in_scope=.N,available_all_seven=sum(available_all_seven),selected_all_seven=sum(selected_all_seven),
 primary_validation_all_seven=sum(available_and_primary_validation_all_seven),
 validation_country_years_within_stable_tier=sum(tier_and_primary_validation_years[available_all_seven]),
 stable_tier_countries_with_any_primary_validation=sum(available_all_seven&tier_and_primary_validation_years>0)),by=tier][,scope:=sc]
}))
any_tier<-t[,.(any_nonpeer_available=any(usable),same_year_primary_validation=first(primary_policy_validation_available),
 historical_lmic_reporting_scope=first(historical_lmic_reporting_scope)),by=.(iso3,analysis_year)]
any_country<-any_tier[,.(panel_years=.N,available_years=sum(any_nonpeer_available),
 historical_lmic_years=sum(historical_lmic_reporting_scope==TRUE),validation_years=sum(any_nonpeer_available&same_year_primary_validation)),by=iso3]
any_summary<-rbindlist(lapply(c("all_panel_countries","historical_LMIC_every_year"),function(sc) {
 z<-if(sc=="all_panel_countries")any_country else any_country[historical_lmic_years==7L]
 z[,.(scope=sc,countries_in_scope=.N,any_nonpeer_all_seven=sum(panel_years==7L&available_years==7L),
 primary_validation_all_seven=sum(panel_years==7L&validation_years==7L))]
}))
stable_validation<-merge(modern_loss[tier!="peer"],stable[available_all_seven==TRUE,.(iso3,tier,historical_lmic_years)],by=c("iso3","tier"))
stable_validation_summary<-stable_validation[,stat_error_metrics(.SD),by=tier]

# Reuse saved loan values and summaries; no loan calculator is called.
lv<-read_input(loan,"loan_valuations.csv")
tv<-read_input(loan,"same_loan_all_tier_valuations.csv")
loan_annual<-read_input(loan,"annual_summary.csv")
loan_periods<-read_input(loan,"period_summary.csv")
quality<-read_input(loan,"quality_sensitivity_summary.csv")
main_cohorts<-c("mpg_original_loans","aiddata_fixed_usd","add_central_all_currency")
annual_extract<-loan_annual[cohort%in%main_cohorts]
period_extract<-loan_periods[cohort%in%main_cohorts]
quality_extract<-quality[cohort%in%main_cohorts]
main<-lv[matched==TRUE&non_peer==TRUE&commitment_year>=2018L&commitment_year<=2024L&
 cohort%in%c("aiddata_fixed_usd","add_central_all_currency")&is.finite(ge_policy_pct)]
main[,delta_ge_pp:=ge_market_pct-ge_policy_pct]
design_results<-setNames(lapply(c("aiddata","add"),function(k)stat_period_design(main[dataset==k])),c("aiddata","add"))
stack<-function(k)rbindlist(lapply(names(design_results),function(ds)cbind(dataset=ds,design_results[[ds]][[k]])))
cluster<-stack("cluster");concentration<-stack("concentration");country<-stack("country");fe<-stack("fe")
old_analysis<-read_input(period,"analysis_rows.csv")
old_primary<-read_input(period,"primary_country_inference.csv")
old_shared<-read_input(period,"shared_year_inference.csv")
old_boot<-read_input(period,"country_bootstrap_draws.csv")
bootstrap_distribution<-old_boot[,.(draws=.N,minimum_pp=min(estimate_pp),q005_pp=quantile(estimate_pp,.005),
 q025_pp=quantile(estimate_pp,.025),median_pp=median(estimate_pp),q975_pp=quantile(estimate_pp,.975),
 q995_pp=quantile(estimate_pp,.995),maximum_pp=max(estimate_pp),negative_share=mean(estimate_pp<0)),by=dataset]
fixed<-merge(main[,.(loan_id,dataset,cohort,iso3,commitment_year,currency,interest_rate_pct,maturity_years,
 first_principal_payment_years,benchmark_selected_tier,selected_gap_pp=delta_ge_pp,ge_policy_pct)],
 tv[tier%in%c("ids","secondary","moodys"),.(loan_id,tier,rate_pct,tier_ge_pct,tier_minus_selected_ge_pp)],by="loan_id")
fixed[,fixed_gap_pp:=tier_ge_pct-ge_policy_pct]
fixed[,support:="method_available_on_main_nonpeer_loans"]
common_ids<-fixed[,.(method_count=uniqueN(tier)),by=.(dataset,loan_id)][method_count==3L,loan_id]
fixed_all<-rbindlist(list(fixed,copy(fixed[loan_id%in%common_ids])[,support:="all_three_methods_same_loans"]))
common_within<-fixed_all[,.(in_both=any(commitment_year<=2021L)&any(commitment_year>=2022L)),by=.(dataset,support,tier,iso3)][in_both==TRUE]
fixed_common<-merge(fixed_all,common_within[,.(dataset,support,tier,iso3)],by=c("dataset","support","tier","iso3"))
fixed_common[,support:=paste0(support,"_common_countries")]
fixed_all<-rbindlist(list(fixed_all,fixed_common),fill=TRUE)
fixed_summary<-fixed_all[,stat_period_means(.SD),by=.(dataset,cohort,support,tier)]
fixed_annual<-fixed_all[,.(records=.N,countries=uniqueN(iso3),mean_selected_gap_pp=mean(selected_gap_pp),
 mean_fixed_gap_pp=mean(fixed_gap_pp),mean_method_difference_pp=mean(fixed_gap_pp-selected_gap_pp)),
 by=.(dataset,cohort,support,tier,commitment_year)]
selected_tier_period<-main[,.(records=.N,countries=uniqueN(iso3),mean_gap_pp=mean(delta_ge_pp)),
 by=.(dataset,cohort,period,benchmark_selected_tier)]

# Existing aggregate application and creditor reversals are separate observations.
agg_annual<-read_input(pv,"summary_analysis_year.csv")
agg_period<-read_input(pv,"summary_period.csv")
agg_tier<-read_input(pv,"paired_tier_sensitivity.csv")
reversal<-read_input(pv,"matched_creditor_pairs.csv")
reversal_cases<-read_input(followup,"reversal_magnitude_cases.csv")
reversal_summary<-rbindlist(list(
 reversal[,.(grouping="all",group="all",eligible_pairs=.N,reversals=sum(interest_ranking_reversed),
 countries=uniqueN(iso3),reversal_countries=uniqueN(iso3[interest_ranking_reversed]),
 reversal_share=mean(interest_ranking_reversed))],
 reversal[,.(grouping="creditor_pair",group=pair,eligible_pairs=.N,reversals=sum(interest_ranking_reversed),
 countries=uniqueN(iso3),reversal_countries=uniqueN(iso3[interest_ranking_reversed]),reversal_share=mean(interest_ranking_reversed)),by=pair][,pair:=NULL],
 reversal[,.(grouping="selected_tier",group=selected_tier,eligible_pairs=.N,reversals=sum(interest_ranking_reversed),
 countries=uniqueN(iso3),reversal_countries=uniqueN(iso3[interest_ranking_reversed]),reversal_share=mean(interest_ranking_reversed)),by=selected_tier][,selected_tier:=NULL]))

newp<-main[,.(loan_id,delta_ge_pp)];oldp<-old_analysis[,.(loan_id,delta_ge_pp)]
setorder(newp,loan_id);setorder(oldp,loan_id)
oldm<-saved_inference[family=="modern_tier_vs_dac"&sample_view=="full_validation"]
newm<-metric[scenario=="modern2018_2024"]
check_metrics<-merge(newm,oldm,by.x="tier",by.y="focal")
primary_check<-merge(fe,old_primary,by="dataset")
checks<-data.table(check=c("unique_panel_country_year_tier","all_availability_records_cover_seven_years",
 "stable_primary_validation_is_subset_of_stable_availability","modern_metric_means_reproduce_saved",
 "loan_period_rows_and_values_reproduce_saved","country_fixed_effect_matches_independent_lm",
 "country_and_year_partial_leverage_sums_one","fixed_method_same_loan_gap_identity",
 "fixed_method_contrast_bridge_identity","all_three_support_exactly_three_methods_each_loan",
 "all_three_support_identical_record_counts_across_methods","existing_period_point_estimates_reproduce",
 "reversal_count_matches_recomputed_parent","reversal_case_ids_agree","no_new_benchmark_or_loan_valuation"),
 passed=c(!anyDuplicated(t[,.(iso3,analysis_year,tier)]),all(stable$panel_years==7L),
 all(stable[available_and_primary_validation_all_seven==TRUE,available_all_seven]),
 all(abs(check_metrics$focal_mae_pp.x-check_metrics$focal_mae_pp.y)<1e-10)&all(abs(check_metrics$improvement_pp-check_metrics$estimate_pp)<1e-10),
 isTRUE(all.equal(newp,oldp,tolerance=1e-10,check.attributes=FALSE)),
 all(abs(fe$within_country_fe_contrast_pp-fe$independent_lm_fe_contrast_pp)<1e-9),
 all(abs(cluster[,sum(partial_leverage),by=.(dataset,cluster_dimension)]$V1-1)<1e-12),
 all(abs(fixed_all$fixed_gap_pp-fixed_all$selected_gap_pp-fixed_all$tier_minus_selected_ge_pp)<1e-10),
 all(abs(fixed_summary$fixed_contrast_pp-fixed_summary$selected_contrast_pp-fixed_summary$method_component_pp)<1e-10,na.rm=TRUE),
 all(fixed_all[support=="all_three_methods_same_loans",uniqueN(tier),by=loan_id]$V1==3L),
 all(fixed_summary[support=="all_three_methods_same_loans",uniqueN(records),by=dataset]$V1==1L),
 all(abs(primary_check$record_mean_period_contrast_pp-primary_check$estimate_pp)<1e-10),
 !anyDuplicated(reversal[,.(iso3,analysis_year,pair)]) && sum(reversal$interest_ranking_reversed)==nrow(reversal_cases),
 setequal(reversal[interest_ranking_reversed==TRUE,paste(iso3,analysis_year,pair)],reversal_cases[,paste(iso3,analysis_year,pair)]),TRUE))
stopifnot(all(checks$passed))

tables<-list(benchmark_error_details=loss,benchmark_error_metrics=metric,benchmark_annual_error_metrics=metric_year,
 benchmark_year_design=benchmark_year_design,existing_benchmark_country_inference=saved_inference,
 existing_benchmark_country_year_inference=saved_two_way,existing_consecutive_details=consecutive,
 nonpeer_tier_availability_country_year=t,tier_stability_country=stable,tier_stability_summary=stable_summary,
 any_nonpeer_stability_country=any_country,any_nonpeer_stability_summary=any_summary,
 stable_tier_primary_validation_details=stable_validation,stable_tier_primary_validation_summary=stable_validation_summary,
 existing_loan_annual_summary=annual_extract,existing_loan_period_views=period_extract,
 existing_loan_quality_sensitivity=quality_extract,loan_cluster_design=cluster,loan_period_concentration=concentration,
 loan_country_period_details=country,loan_country_fe_diagnostic=fe,existing_loan_primary_inference=old_primary,
 existing_loan_shared_year_inference=old_shared,existing_loan_bootstrap_distribution=bootstrap_distribution,
 fixed_method_loan_details=fixed_all,fixed_method_period_contrasts=fixed_summary,fixed_method_annual=fixed_annual,
 selected_tier_period_summary=selected_tier_period,existing_aggregate_annual=agg_annual,
 existing_aggregate_period=agg_period,existing_aggregate_tier_pairs=agg_tier,
 creditor_reversal_summary=reversal_summary,existing_creditor_pair_details=reversal,
 existing_reversal_case_details=reversal_cases,checks=checks)
dir.create(out,recursive=TRUE)
meta<-list(build_id=basename(out),schema_id="SCHEMA-P15-STATISTICAL-ANNOTATION-REVIEW-V1",
 estimator_id="EST-P15-DESCRIPTIVE-SUPPORT-AND-INFERENCE-AUDIT-V1",admissibility_id="ADM-P15-EXISTING-ORDINARY-USE-FLAGS-V1",
 selection_id="SEL-P15-NO-CHANGE-STATISTICAL-REVIEW-V1",lifecycle_status="diagnostic",release_state="private_research")
for(nm in names(tables)) {z<-copy(tables[[nm]]);for(k in names(meta))set(z,j=k,value=rep(meta[[k]],nrow(z)));fwrite(z,file.path(out,paste0(nm,".csv")),na="")}
design<-"docs/thesis_design/feedback_2026-09-11/STATISTICAL_ANALYSIS_DESIGN.md"
sources<-"sources/literature_review/statistical_review_20260911/source_ledger.csv"
source_paths<-fread(sources)$path
inputs<-unique(c(consumed,design,"docs/thesis_design/feedback_2026-09-11/OWNER_ANNOTATIONS.json",sources,source_paths,
 "docs/thesis_design/feedback_2026-09-10/BENCHMARK_ANALYSIS_DESIGN.md",
 "docs/thesis_design/loan_period_followup_2026-09-10/INFERENCE_DESIGN.md",
 "docs/governance/P15_YOY_AND_EXTREME_CASE_USE_2026-09-08.md"))
code<-c("scripts/p15/annotation_followup_20260911/build_statistical_review.R",
 "scripts/p15/annotation_followup_20260911/statistical_helpers.R","scripts/p15/loan_extension/benchmark_matching.R",
 "scripts/p15/activate_p15_environment.R","R/research_governance.R","R/p15_dataset_assessment.R",
 "R/p15_thesis_benchmark_inference.R","scripts/p15/loan_extension/period_inference.R","renv.lock")
manifest<-function(ps)data.table(path=ps,sha256=vapply(ps,function(p)digest(file=p,algo="sha256"),character(1)),bytes=file.info(ps)$size)
fwrite(manifest(inputs),file.path(out,"input_manifest.csv"));fwrite(manifest(code),file.path(out,"code_manifest.csv"))
write_json(c(meta,list(analysis_scope="Exploratory diagnostic support audit; no new p-values or source/rule changes",
 selected_parent=base,loan_parent=loan,period_inference_parent=period)),file.path(out,"version_bundle.json"),auto_unbox=TRUE,pretty=TRUE)
writeLines(capture.output(sessionInfo()),file.path(out,"environment.txt"))
fwrite(manifest(list.files(out,full.names=TRUE)),file.path(out,"output_manifest.csv"))
cat("Completed statistical diagnostic review:",out,";",nrow(checks),"checks passed\n")
print(stable_summary);print(fe);print(fixed_summary[support=="method_available_on_main_nonpeer_loans"])
