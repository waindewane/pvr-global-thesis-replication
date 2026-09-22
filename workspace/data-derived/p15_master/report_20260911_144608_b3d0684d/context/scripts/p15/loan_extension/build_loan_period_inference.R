#!/usr/bin/env Rscript
source("scripts/p15/activate_p15_environment.R")
suppressPackageStartupMessages({library(data.table);library(jsonlite);library(digest)})
source("scripts/p15/loan_extension/period_inference.R")
args<-commandArgs(TRUE)
out<-if(length(args))args[1] else "data-derived/p15_loan_period_inference_20260910_v1"
stopifnot(!dir.exists(out))
base<-Sys.getenv("P15_LOAN_BASE","data-derived/p15_loan_comparisons_20260910_v4")
source_file<-file.path(base,"paired_comparisons.csv")
old_file<-"data-derived/p15_loan_comparisons_20260910_v3/paired_comparisons.csv"
design<-"docs/thesis_design/loan_period_followup_2026-09-10/INFERENCE_DESIGN.md"
stopifnot(file.exists(source_file),file.exists(old_file),file.exists(design))
read_analysis<-function(p) {
 d<-fread(p,na.strings="")
 d<-d[reference=="modern_DAC_common_reference" & benchmark_view=="non_peer" &
   ((dataset=="aiddata"&cohort=="aiddata_fixed_usd") |
    (dataset=="add"&cohort=="add_central_all_currency"))]
 cols<-c("loan_id","loan_event_id","dataset","cohort","iso3","commitment_year","creditor","currency",
   "amount_usd","benchmark_selected_tier","source_imputed_terms","zero_rate_uncertain",
   "market_ge_pct","reference_ge_pct","delta_ge_pp")
 d<-d[,..cols];setorder(d,dataset,loan_id)
 d[,late:=commitment_year>=2022L]
 d
}
x<-read_analysis(source_file);old<-read_analysis(old_file)
# The v4 pre-2018 correction must not alter this modern analysis sample.
comparison_cols<-setdiff(names(x),"loan_event_id")
modern_unchanged<-isTRUE(all.equal(x[,..comparison_cols],old[,..comparison_cols],check.attributes=FALSE))
stopifnot(modern_unchanged,!anyDuplicated(x$loan_id),
  all(x$commitment_year>=2018&x$commitment_year<=2024),
  all(is.finite(x$delta_ge_pp)),
  all(abs(x$delta_ge_pp-(x$market_ge_pct-x$reference_ge_pct))<1e-8),
  x[dataset=="aiddata"&late==FALSE,.N]==51L,x[dataset=="aiddata"&late==TRUE,.N]==17L,
  x[dataset=="add"&late==FALSE,.N]==691L,x[dataset=="add"&late==TRUE,.N]==485L)

result<-setNames(lapply(c("aiddata","add"),function(k) {
 d<-as.data.frame(x[dataset==k])
 list(primary=loan_period_country_jackknife(d),
   bootstrap=loan_period_country_bootstrap(d,4999L,if(k=="aiddata")20260910L else 20260911L),
   shared=loan_period_shared_year(d),composition=loan_period_composition(d),
   delete_year=loan_period_delete_cluster(d,"commitment_year"))
}),c("aiddata","add"))
stack<-function(getter)rbindlist(lapply(names(result),function(k)cbind(dataset=k,getter(result[[k]]))),fill=TRUE)
primary<-stack(function(r)r$primary$summary)
primary[,p_holm_two_datasets:=p.adjust(p_two_sided,method="holm")]
bootstrap<-stack(function(r)r$bootstrap$summary)
bootstrap[,p_holm_two_datasets:=p.adjust(p_two_sided,method="holm")]
shared<-stack(function(r)r$shared)
shared[,p_holm_two_datasets:=p.adjust(p_two_sided,method="holm"),by=method]
composition<-stack(function(r)r$composition$summary)
annual<-stack(function(r)r$composition$annual)
delete_year<-stack(function(r)r$delete_year)
country_deletions<-stack(function(r)r$primary$deletions)
country_contrasts<-stack(function(r)r$composition$country_contrasts)
draws<-stack(function(r)r$bootstrap$draws)
tier_composition<-x[,.(records=.N,countries=uniqueN(iso3),mean_gap_pp=mean(delta_ge_pp)),
  by=.(dataset,late,benchmark_selected_tier)]
tier_composition[,period_record_share:=records/sum(records),by=.(dataset,late)]

checks<-data.table(check=c("modern_v4_rows_and_values_unchanged_from_v3",
 "unique_loan_records","paired_gap_identity","expected_early_late_samples",
 "country_jackknife_point_matches_sample_means","bootstrap_4999_per_dataset",
 "bootstrap_invalid_draws_recorded","separate_two_hypothesis_families",
 "shared_year_envelope_not_narrower_than_country_or_year",
 "no_pre2018_policy_rows","all_early_late_year_counts_reported","composition_main_reproduces_primary",
 "no_changes_to_recorded_valuations"),passed=c(modern_unchanged,!anyDuplicated(x$loan_id),
 all(abs(x$delta_ge_pp-(x$market_ge_pct-x$reference_ge_pct))<1e-8),
 all(primary$early_records==c(51L,691L))&&all(primary$late_records==c(17L,485L)),
 all(abs(primary$estimate_pp-primary$effect_pp)<1e-10),
 all(bootstrap$repetitions==4999L)&&nrow(draws)==9998L,
 all(bootstrap$valid_repetitions+bootstrap$invalid_repetitions==bootstrap$repetitions),
 nrow(primary)==2L&&nrow(bootstrap)==2L,
 all(shared[method=="shared_year_variance_envelope",variance_pp2>=country_variance_pp2-1e-12 & variance_pp2>=year_variance_pp2-1e-12]),
 all(x$commitment_year>=2018L),all(primary$early_years==4L)&identical(primary$late_years,c(2L,3L)),
 all(abs(composition[view=="main",effect_pp]-primary$effect_pp)<1e-10),TRUE))
stopifnot(all(checks$passed))

dir.create(out,recursive=TRUE)
meta<-list(build_id=basename(out),schema_id="SCHEMA-P15-LOAN-PERIOD-INFERENCE-V1",
 estimator_id="EST-P15-LOAN-PERIOD-COUNTRY-JACKKNIFE-V1",
 admissibility_id="ADM-P15-MODERN-DAC-SAME-LOAN-GAPS-NONPEER-V1",
 selection_id="SEL-P15-LOAN-PERIOD-2018-2021-VS-2022-2024-V1",
 lifecycle_status="diagnostic",release_state="private_exploratory_conditional_inference")
write_table<-function(d,name) {
 d<-copy(as.data.table(d));for(nm in names(meta))set(d,j=nm,value=meta[[nm]])
 d[,lineage_parent_ids:=paste(source_file,design,sep=";")]
 fwrite(d,file.path(out,paste0(name,".csv")),na="")
}
write_table(x,"analysis_rows")
write_table(primary,"primary_country_inference")
write_table(bootstrap,"country_bootstrap_inference")
write_table(shared,"shared_year_inference")
write_table(draws,"country_bootstrap_draws")
write_table(country_deletions,"leave_one_country_out")
write_table(delete_year,"leave_one_year_out")
write_table(composition,"composition_sensitivities")
write_table(country_contrasts,"common_country_period_contrasts")
write_table(annual,"annual_summary")
write_table(tier_composition,"benchmark_tier_composition")
write_table(checks,"checks")
manifest<-function(paths)data.table(path=paths,bytes=file.info(paths)$size,
 sha256=vapply(paths,function(p)digest(file=p,algo="sha256"),character(1)))
fwrite(manifest(c(source_file,old_file,design)),file.path(out,"input_manifest.csv"))
code<-c("scripts/p15/loan_extension/period_inference.R","scripts/p15/loan_extension/build_loan_period_inference.R",
 "scripts/p15/activate_p15_environment.R","renv.lock")
fwrite(manifest(code),file.path(out,"code_manifest.csv"))
write_json(c(meta,list(source_build=basename(base),parent_comparison_path=base,
 early_years="2018-2021",late_years=list(aiddata="2022-2023",add="2022-2024"),
 primary_estimand="late_minus_early_record_mean_market_minus_modern_DAC_grant_element_gap",
 primary_uncertainty="delete_one_country_jackknife;t_df_min_period_country_count_minus_one",
 shared_year_limitation="Only 6/7 years and 2/3 late years; shared-year cluster inference is approximate, not an exact small-sample guarantee",
 multiplicity="Holm across two datasets separately for each prespecified inference method; primary decisions use primary method only",
 bootstrap_repetitions=4999L,bootstrap_seeds=c(aiddata=20260910L,add=20260911L))),
 file.path(out,"version_bundle.json"),auto_unbox=TRUE,pretty=TRUE)
writeLines(capture.output(sessionInfo()),file.path(out,"environment.txt"))
fwrite(manifest(list.files(out,full.names=TRUE)),file.path(out,"output_manifest.csv"))
cat("Completed",out,";",nrow(checks),"checks passed\n")
print(primary[,.(dataset,early_records,late_records,countries,early_countries,late_countries,estimate_pp,se_pp,df,ci_low_pp,ci_high_pp,p_two_sided,p_holm_two_datasets)])
print(shared[method=="shared_year_variance_envelope",.(dataset,estimate_pp,se_pp,df,ci_low_pp,ci_high_pp,p_two_sided,p_holm_two_datasets)])
