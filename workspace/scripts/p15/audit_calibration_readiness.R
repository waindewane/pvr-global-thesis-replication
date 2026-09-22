#!/usr/bin/env Rscript
# Fixed-rule local evidence audit; never promotes a corrected rate.
suppressPackageStartupMessages({library(data.table);library(dplyr);library(digest)})
root <- "data-derived/p15_full_replay_20260906_112727"
base <- file.path(root,"isolated_build/data-derived/p15_reviewed_evidence_20260906_v1")
old <- "data-derived/p15_local_source_rating_audit_20260906_v1"
paths <- c(panel=file.path(base,"p15_country_year_dataset.csv"),
  previews=file.path(base,"p15_reviewed_ladder_previews.csv"),
  sample=file.path(old,"p15_moodys_validation_sample.csv"),
  predictions=file.path(old,"p15_moodys_country_history_predictions.csv"),
  prior_detail="data-derived/p15_fallback_validation_2012_2024_v1/p15_rating_integrated_anchor_validation_detail_2012_2024.csv.gz",
  july_detail="data-derived/p15_rating_validation_2012_2024_v1/p15_rating_variant_anchor_gap_detail_2012_2024.csv.gz")
protocol <- "docs/governance/P15_CALIBRATION_EVIDENCE_READINESS_PROTOCOL_2026-09-06.md"
method <- "R/p15_rating_country_history_calibration.R"
sha <- function(p)digest(p,algo="sha256",file=TRUE)
stopifnot(sha(method)=="fcb7c06f390728949bfd985cd60c1d9a42ba7871964b15b56285cca42d272b3a")
source(method)
hashes <- vapply(paths,sha,character(1))
parent <- fread(file.path(root,"output_manifest.csv"))
previous <- fread(file.path(old,"p15_audit_manifest.csv"))
july_manifest <- fread("docs/governance/p15_rating_variant_validation_manifest.csv")
for(n in names(paths)) {
  f<-paths[[n]]
  found<-if(n %in% c("panel","previews"))parent[artifact_path==sub(paste0(root,"/"),"",f,fixed=TRUE),sha256] else
    if(n=="july_detail")july_manifest[artifact_path==f,sha256] else previous[artifact_path==f,sha256]
  stopifnot(length(found)==1L,found==sha(f))
}
build <- paste0("p15_calibration_readiness_",format(Sys.time(),"%Y%m%d_%H%M%S",tz="UTC"))
out<-file.path("data-derived",build);stopifnot(!dir.exists(out));dir.create(out)
schema<-"SCHEMA-P15-CALIBRATION-READINESS-V1"
bundle<-list(build_id=build,schema_id=schema,estimator_id="EST-P15-FROZEN-MOODYS-VALIDATION-V1",
  admissibility_id="ADM-DIAGNOSTIC-ONLY",selection_id="SEL-NONE",
  source_package_ids=paste(c("SRC-P15-RAW-CLOSURE-20260906","SRC-BLOOMBERG-BOY-RATINGS-20260528",
    "SRC-DAMODARAN-ARCHIVE-2000-2024","SRC-FRED-DGS7-20260512"),collapse=";"),release_id="not_applicable")
emit<-function(x,name){x<-copy(as.data.table(x));for(n in names(bundle))set(x,j=n,value=bundle[[n]]);fwrite(x,file.path(out,paste0(name,".csv")))}
p<-fread(paths[["panel"]]);v<-fread(paths[["previews"]]);train<-fread(paths[["sample"]]);saved<-fread(paths[["predictions"]])
detail<-fread(paths[["prior_detail"]]);july<-fread(paths[["july_detail"]])
key<-function(x)paste(x$analysis_year,x$iso3,sep="::")
stopifnot(nrow(p)==2743L,nrow(train)==200L,!anyDuplicated(key(p)),!anyDuplicated(key(train)))
current<-p[is.finite(primary_usd_market_rate_pct)&is.finite(rating_moodys_rate_pct),
  .(analysis_year,iso3,country,historical_lmic_reporting_scope,observed_rate_pct=primary_usd_market_rate_pct,
    rating_rate_pct=rating_moodys_rate_pct,observed_permission=observed_benchmark_selection_permitted)]
prior_primary<-detail[validation_question_id=="Q-USD-NEW-BORROWING" & is.finite(anchor_rate_pct)]
# The July surface includes multiple source objects; its role is inventory only.
current[,`:=`(in_original_moodys_sample=key(current)%in%key(train),
  in_prior_primary_validation=key(current)%in%key(prior_primary),
  in_july_comparison_any_object=key(current)%in%key(july))]
current[,prior_exact_anchor_seen:=vapply(seq_len(.N),function(i){
  a<-prior_primary[analysis_year==current$analysis_year[i]&iso3==current$iso3[i],anchor_rate_pct]
  any(abs(a-current$observed_rate_pct[i])<1e-12)
},logical(1))]
eligible_keys<-unique(key(prior_primary[accuracy_summary_eligible==TRUE]))
current[,prior_primary_accuracy_eligible:=key(current)%in%eligible_keys]
current[,local_confirmation_status:=fcase(in_original_moodys_sample,"used_in_original_calibration",
  in_prior_primary_validation,"already_in_rating_comparison_surface_not_established_untouched",
  default="requires_separate_unused_evidence_verification")]
emit(current,"current_primary_exposure_audit")
exposure<-current[,.(country_years=.N,countries=uniqueN(iso3),
  prior_exact_anchor_seen=sum(prior_exact_anchor_seen),prior_accuracy_eligible=sum(prior_primary_accuracy_eligible)),
  by=.(historical_lmic_reporting_scope,local_confirmation_status)]
emit(exposure,"exposure_summary")
emit(current[in_original_moodys_sample==FALSE],"rows_outside_original_sample")
targets<-v[preview_variant=="ids_before_secondary__ids_case_review_applied__peer_not_selected"&preview_source=="moodys",
  .(analysis_year,iso3,country,historical_lmic_reporting_scope,reference_rating_rate_pct=preview_rate_pct,
    parent_evidence_id,status_review_coverage)]
stopifnot(!anyDuplicated(key(targets)),!any(key(targets)%in%key(current)))
reviewed_train<-current[historical_lmic_reporting_scope==TRUE & !(observed_permission %in% FALSE)]
prior_count<-function(rows,year,iso)sum(rows$analysis_year<year&rows$iso3==iso)
targets[,`:=`(original_prior_rows=mapply(function(y,i)prior_count(train,y,i),analysis_year,iso3),
  reviewed_prior_rows=mapply(function(y,i)prior_count(reviewed_train,y,i),analysis_year,iso3),
  in_demonstrated_expanding_period=analysis_year>=2016L)]
targets[,history_group:=fifelse(original_prior_rows>0,"past_country_history_available","no_past_country_history")]
emit(targets,"actual_rating_target_history")
target_summary<-targets[,.(country_years=.N,countries=uniqueN(iso3),
  with_original_history=sum(original_prior_rows>0),with_reviewed_history=sum(reviewed_prior_rows>0)),
  by=.(historical_lmic_reporting_scope,in_demonstrated_expanding_period)]
emit(target_summary,"actual_rating_target_summary")

# Reuse saved errors; history membership comes from the history model for every comparator.
history_keys<-saved[model_id=="affine_year_demeaned_country_eb",
  .(validation_design,analysis_year,iso3,has_country_history=country_history_rows>0)]
stopifnot(!anyDuplicated(history_keys[,.(validation_design,analysis_year,iso3)]))
scored<-merge(saved,history_keys,by=c("validation_design","analysis_year","iso3"))
error_summary<-scored[,.(observations=.N,countries=uniqueN(iso3),
  mean_error_pp=mean(residual_pp),mae_pp=mean(absolute_error_pp),
  rmse_pp=sqrt(mean(squared_error_pp2))),by=.(validation_design,has_country_history,model_id)]
emit(error_summary,"existing_test_errors_by_history")
emit(scored[,.(validation_design,analysis_year,iso3,model_id,has_country_history,
  observed_rate_pct,predicted_rate_pct,residual_pp,absolute_error_pp)],"existing_test_subgroup_detail")

# Predictions at actual fallback rows: no observed outcomes and no scoring claim.
fits<-list();failures<-list();fit_checks<-list()
for(year in sort(unique(targets[in_demonstrated_expanding_period==TRUE,analysis_year]))) {
  t<-targets[analysis_year==year]
  tr<-as.data.frame(train[analysis_year<year])
  te<-as.data.frame(t[,.(analysis_year,iso3,country,observed_rate_pct=NA_real_,rating_rate_pct=reference_rating_rate_pct)])
  warnings<-character()
  fit<-tryCatch(withCallingHandlers(p15_fit_rating_country_history(tr,te),
    warning=function(w){warnings<<-c(warnings,conditionMessage(w));invokeRestart("muffleWarning")}),error=function(e)e)
  if(inherits(fit,"error")){failures[[as.character(year)]]<-data.table(year=year,reason=conditionMessage(fit));next}
  z<-as.data.table(fit$scored)
  z<-merge(z,t[,.(analysis_year,iso3,historical_lmic_reporting_scope,original_prior_rows)],by=c("analysis_year","iso3"))
  z[,`:=`(raw_prediction_pct=rating_rate_pct,training_rows=nrow(tr),training_last_year=max(tr$analysis_year),
    rating_outside_training_range=rating_rate_pct<min(tr$rating_rate_pct)|rating_rate_pct>max(tr$rating_rate_pct),
    fitting_warning=paste(unique(warnings),collapse=";"),prediction_role="diagnostic_no_target_outcome_no_accuracy_claim",
    affine_change_pp=affine_prediction_pct-rating_rate_pct,total_change_pp=history_prediction_pct-rating_rate_pct)]
  stopifnot(all(z$training_last_year<z$analysis_year),all(z$country_history_rows==z$original_prior_rows),
    all(z$country_history_adjustment_pp[z$country_history_rows==0]==0),
    all(z$history_prediction_pct[z$country_history_rows==0]==z$affine_prediction_pct[z$country_history_rows==0]))
  # Changing absent target outcomes must not influence a prediction.
  te$observed_rate_pct<-999
  again<-suppressWarnings(p15_fit_rating_country_history(tr,te))$scored
  expected<-as.data.table(fit$scored)
  fit_checks[[as.character(year)]]<-data.table(year=year,
    outcome_perturbation_invariant=isTRUE(all.equal(expected$history_prediction_pct,again$history_prediction_pct,tolerance=0)))
  fits[[as.character(year)]]<-z
}
pred<-rbindlist(fits,fill=TRUE)
stopifnot(nrow(pred)+sum(targets$analysis_year%in%as.integer(names(failures)))==sum(targets$in_demonstrated_expanding_period))
emit(pred,"actual_target_correction_diagnostic")
emit(if(length(failures))rbindlist(failures)else data.table(year=integer(),reason=character()),"fit_failures")
emit(pred[,.(country_years=.N,countries=uniqueN(iso3),mean_total_change_pp=mean(total_change_pp),
  median_total_change_pp=median(total_change_pp),median_absolute_change_pp=median(abs(total_change_pp)),
  max_absolute_change_pp=max(abs(total_change_pp)),outside_training_range=sum(rating_outside_training_range)),
  by=.(historical_lmic_reporting_scope,has_country_history=country_history_rows>0)],"target_correction_summary")
emit(rbindlist(fit_checks),"prediction_no_leakage_checks")
emit(data.table(evidence=c("original_calibration_and_existing_primary_comparisons","new_current_country_year_keys",
  "same_year_new_issue_rows_or_revised_values","EUR_primary","year_end_secondary","IDS_Bondholders","future_years"),
  disposition=c("previously used; not untouched confirmation",
    if(all(current$in_prior_primary_validation))"none outside prior primary comparison surface"else"unmatched keys require further provenance review before any test",
    "not automatically independent; same country-year and changed aggregation require a specified design",
    "different currency; no validated conversion for this test","different timing and target; previously compared",
    "different contractual-rate/currency universe; previously compared","no future year in the current 2012-2024 panel")),"local_evidence_inventory")
checks<-data.table(check=c("input hashes verified against prior manifests","fixed function unchanged",
  "target cohort distinct from current primary outcomes","strict past-only fitting and no-history affine identity",
  "target outcomes cannot change predictions","input products preserved"),
  passed=c(TRUE,sha(method)=="fcb7c06f390728949bfd985cd60c1d9a42ba7871964b15b56285cca42d272b3a",
    !any(key(targets)%in%key(current)),TRUE,all(rbindlist(fit_checks)$outcome_perturbation_invariant),
    identical(hashes,vapply(paths,sha,character(1)))))
stopifnot(all(checks$passed));emit(checks,"verification")
script<-"scripts/p15/audit_calibration_readiness.R"
manifest<-function(files,role)data.table(artifact_path=files,artifact_role=role,sha256=vapply(files,sha,character(1)),
  bytes=file.info(files)$size,build_id=build,schema_id=schema,estimator_id=bundle$estimator_id,
  admissibility_id=bundle$admissibility_id,selection_id=bundle$selection_id,source_package_ids=bundle$source_package_ids,
  producing_script=script,producing_script_sha256=sha(script))
fwrite(manifest(c(unname(paths),protocol,file.path(old,"p15_audit_manifest.csv"),file.path(root,"output_manifest.csv"),
  "docs/governance/p15_rating_variant_validation_manifest.csv"),"audit_input"),file.path(out,"input_manifest.csv"))
fwrite(manifest(c(script,method),"code"),file.path(out,"script_manifest.csv"))
fwrite(data.table(build_id=build,r_version=R.version.string,platform=R.version$platform,locale=Sys.getlocale(),
  nlme_version=as.character(packageVersion("nlme")),dplyr_version=as.character(packageVersion("dplyr")),
  data_table_version=as.character(packageVersion("data.table")),renv_lock_sha256=sha("renv.lock")),file.path(out,"environment_manifest.csv"))
fwrite(manifest(list.files(out,full.names=TRUE),"diagnostic_output"),file.path(out,"output_manifest.csv"))
cat("Output:",out,"\n");print(exposure);print(target_summary);print(error_summary)
print(checks)
