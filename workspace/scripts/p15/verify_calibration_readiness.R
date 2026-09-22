#!/usr/bin/env Rscript
suppressPackageStartupMessages({library(data.table);library(digest)})
args<-commandArgs(trailingOnly=TRUE);stopifnot(length(args)==2L)
sha<-function(p)digest(p,algo="sha256",file=TRUE)
files<-setdiff(list.files(args[1],pattern="\\.csv$"),
  c("input_manifest.csv","output_manifest.csv","script_manifest.csv","environment_manifest.csv"))
checks<-rbindlist(lapply(files,function(f){a<-fread(file.path(args[1],f));b<-fread(file.path(args[2],f));
  a[,build_id:=NULL];b[,build_id:=NULL];data.table(check=paste("exact repeat",f),passed=identical(a,b))}))
add<-function(n,x)checks<<-rbind(checks,data.table(check=n,passed=isTRUE(x)))
for(d in args)for(m in c("input_manifest.csv","output_manifest.csv","script_manifest.csv")){
  x<-fread(file.path(d,m));add(paste("hashes",basename(d),m),all(vapply(x$artifact_path,sha,character(1))==x$sha256))
}
read<-function(f)fread(file.path(args[1],paste0(f,".csv")))
e<-read("current_primary_exposure_audit")
add("no fresh current comparable primary rows identified",nrow(e)==308L&&all(e$in_prior_primary_validation)&&
  all(e$prior_exact_anchor_seen)&&sum(e$historical_lmic_reporting_scope)==199L&&
  all(e[historical_lmic_reporting_scope==TRUE,in_original_moodys_sample]))
t<-read("actual_rating_target_history")[historical_lmic_reporting_scope==TRUE]
add("actual fallback cohort and original/reviewed history counts",nrow(t)==454L&&sum(t$original_prior_rows>0)==38L&&
  identical(t$original_prior_rows,t$reviewed_prior_rows)&&sum(t$analysis_year>=2016)==339L&&
  sum(t$analysis_year>=2016 & t$original_prior_rows>0)==33L)
p<-read("actual_target_correction_diagnostic")[historical_lmic_reporting_scope==TRUE]
add("diagnostic predictions have no target outcome and no future training",nrow(p)==339L&&all(is.na(p$observed_rate_pct))&&
  all(p$training_last_year<p$analysis_year)&&sum(p$rating_outside_training_range)==32L)
add("no-history correction exactly equals affine",all(p[country_history_rows==0,history_prediction_pct-affine_prediction_pct]==0))
add("fit failures absent",nrow(read("fit_failures"))==0L)
add("existing two designs not pooled",uniqueN(read("existing_test_errors_by_history")$validation_design)==2L)
add("all primary audit checks passed",all(read("verification")$passed))
checks[,`:=`(first_run=args[1],repeat_run=args[2])];stopifnot(all(checks$passed))
fwrite(checks,"docs/governance/p15_calibration_readiness_verification_20260906.csv")
cat(nrow(checks),"checks passed;",length(files),"substantive tables reproduced exactly.\n")
