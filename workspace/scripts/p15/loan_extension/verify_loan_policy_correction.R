#!/usr/bin/env Rscript
source("scripts/p15/activate_p15_environment.R")
suppressPackageStartupMessages({library(data.table);library(digest);library(jsonlite)})
args<-commandArgs(TRUE)
old_dir<-if(length(args)>=1)args[1] else "data-derived/p15_loan_comparisons_20260910_v3"
new_dir<-if(length(args)>=2)args[2] else "data-derived/p15_loan_comparisons_20260910_v4"
out<-if(length(args)>=3)args[3] else "data-derived/p15_loan_policy_correction_verification_20260910_v1"
stopifnot(!dir.exists(out));dir.create(out,recursive=TRUE)
old<-fread(file.path(old_dir,"loan_valuations.csv"));new<-fread(file.path(new_dir,"loan_valuations.csv"))
setorder(old,loan_id);setorder(new,loan_id)
same<-function(a,b)identical(is.na(a),is.na(b))&&
 if(is.numeric(a)&&is.numeric(b))all(abs(a[!is.na(a)]-b[!is.na(b)])<1e-10) else identical(a,b)
unchanged<-c("loan_id","matched","non_peer","benchmark_selected_tier","benchmark_selected_rate_pct",
 "ge_fixed5_pct","ge_market_pct","ge_explicit5_pct","ge_explicit_market_pct","ge_source_replay5_pct")
modern<-new$commitment_year>=2018
oldp<-fread(file.path(old_dir,"paired_comparisons.csv"));newp<-fread(file.path(new_dir,"paired_comparisons.csv"))
keys<-c("loan_id","benchmark_view","reference");setorderv(oldp,keys);setorderv(newp,keys)
safe<-newp$reference!="historical_10pct_convention"
hist<-new[matched==TRUE&commitment_year<2018&is.finite(ge_policy_pct)]
checks<-data.table(check=c("unique_loan_ids_preserved","matching_and_nonpolicy_values_unchanged",
 "all_modern_policy_values_unchanged","all_modern_policy_differences_unchanged",
 "paired_record_keys_unchanged","all_fixed5_and_modern_paired_results_unchanged",
 "historical_matched_rates_are_10pct","historical_reference_ge_increases",
 "missing_or_ineligible_policy_stays_missing","all_v4_build_checks_pass"),
 passed=c(identical(old$loan_id,new$loan_id),all(vapply(unchanged,function(k)same(old[[k]],new[[k]]),logical(1))),
 same(old$ge_policy_pct[modern],new$ge_policy_pct[modern]),same(old$delta_policy_ge_pp[modern],new$delta_policy_ge_pp[modern]),
 identical(oldp[,..keys],newp[,..keys]),all(vapply(c("reference_ge_pct","delta_ge_pp"),function(k)same(oldp[[k]][safe],newp[[k]][safe]),logical(1))),
 all(hist$applied_policy_rate_pct==10),all(hist$ge_policy_pct>old$ge_policy_pct[match(hist$loan_id,old$loan_id)]),
 identical(is.na(old$ge_policy_pct),is.na(new$ge_policy_pct)),all(fread(file.path(new_dir,"checks.csv"))$passed)))
fwrite(checks,file.path(out,"checks.csv"));stopifnot(all(checks$passed))
paired<-merge(oldp[reference=="historical_10pct_convention",.(loan_id,benchmark_view,dataset,cohort,old_delta=delta_ge_pp)],
 newp[reference=="historical_10pct_convention",.(loan_id,benchmark_view,new_delta=delta_ge_pp)],by=c("loan_id","benchmark_view"))
fwrite(paired[,.(records=.N,previous_mislabelled_mean_pp=mean(old_delta),corrected_10pct_mean_pp=mean(new_delta),
 correction_pp=mean(new_delta-old_delta)),by=.(dataset,cohort,benchmark_view)],file.path(out,"historical_correction_summary.csv"))
fwrite(hist[,.(affected_matched_records=.N),by=.(dataset,cohort)],file.path(out,"affected_records.csv"))
inputs<-file.path(rep(c(old_dir,new_dir),each=2),rep(c("loan_valuations.csv","paired_comparisons.csv"),2))
manifest<-function(p)data.table(path=p,sha256=vapply(p,function(q)digest(file=q,algo="sha256"),character(1)))
fwrite(manifest(inputs),file.path(out,"input_manifest.csv"))
fwrite(manifest(c("scripts/p15/loan_extension/verify_loan_policy_correction.R","renv.lock")),file.path(out,"code_manifest.csv"))
write_json(list(build_id=basename(out),lifecycle_status="diagnostic",previous_reference=old_dir,corrected_reference=new_dir,
 affected_matched_records=nrow(hist)),file.path(out,"version_bundle.json"),auto_unbox=TRUE,pretty=TRUE)
writeLines(capture.output(sessionInfo()),file.path(out,"environment.txt"))
fwrite(manifest(list.files(out,full.names=TRUE)),file.path(out,"output_manifest.csv"))
cat("Policy correction verification passed:",nrow(checks),"checks;",nrow(hist),"historical matched records corrected\n")
