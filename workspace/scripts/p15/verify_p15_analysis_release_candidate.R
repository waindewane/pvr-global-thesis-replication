#!/usr/bin/env Rscript
suppressPackageStartupMessages({library(data.table);library(digest)})
source("R/p15_replay.R")
args <- commandArgs(trailingOnly=TRUE);stopifnot(length(args)%in%c(1L,2L))
run <- normalizePath(args[1]);root <- normalizePath(getwd());scratch <- file.path(run,"isolated_build")
out <- "data-derived/p15_analysis_candidate_20260907_v1"
receipt <- if(length(args)==2L)args[2] else file.path(out,"acceptance_verification.csv")
stopifnot(!file.exists(receipt))
acceptance_path <- file.path(run,"acceptance.csv")
if(!file.exists(acceptance_path)) stop("Raw build has not written its completion receipt; wait for successful completion.")
accept <- fread(file=acceptance_path)
stopifnot(nrow(accept)==6L,!anyNA(accept$passed),all(accept$passed),
  setequal(accept$check_id,c("all_stages_completed","raw_inputs_unchanged","baseline_outputs_unchanged",
    "numeric_baseline_parity","case_review_applied","no_unapproved_ladder_promotion")))
manifest <- fread(file.path(out,"output_manifest.csv"))
checks <- list();for(p in manifest$artifact_path) {
  cmp <- p15_replay_compare(file.path(root,p),file.path(scratch,p),root,scratch,0)
  checks[[p]] <- data.table(check_id=paste0("raw_replay_exact::",basename(p)),passed=cmp$semantic_match)
}
for(m in c("input_manifest.csv","script_manifest.csv","output_manifest.csv")) {
  x<-fread(file.path(out,m));actual<-vapply(x$artifact_path,digest,character(1),file=TRUE,algo="sha256")
  checks[[m]]<-data.table(check_id=paste0("current_hashes::",m),passed=all(actual==x$sha256))
}
p<-fread(file.path(out,"core_evidence.csv"));old<-fread("data-derived/p15_source_closure_20260907_v1/p15_country_year_dataset.csv.gz")
setorder(p,analysis_year,iso3);setorder(old,analysis_year,iso3)
metadata<-c("build_id","schema_id","estimator_id","admissibility_id","selection_id","source_package_ids")
cols<-setdiff(names(old),metadata)
checks$core_values_flags<-data.table(check_id="all_core_values_flags_and_case_notes_unchanged",passed=identical(p[,..cols],old[,..cols]))
old_comparisons<-fread(file=file.path(run,"output_comparison.csv"))
checks$old_comparisons<-data.table(check_id="all_historical_regression_tables_pass",passed=nrow(old_comparisons)==153L&&!anyNA(old_comparisons$semantic_match)&&all(old_comparisons$semantic_match))
checks$raw_run<-data.table(check_id="complete_raw_build_acceptance",passed=all(accept$passed))
options<-fread("data-derived/p15_reference_review_20260907_v1/p15_peer_options.csv.gz")[peer_method=="similarity_min3"]
peer<-merge(p[,.(analysis_year,iso3,peer_pool_rule,peer_country_count,global_pool_used)],
  options[,.(analysis_year,iso3,peer_pool_rule,peer_country_count,global_pool)],by=c("analysis_year","iso3"))
checks$peer_rules<-data.table(check_id="accepted_peer_pool_rules_preserved",passed=nrow(peer)==2743&&all(peer$peer_pool_rule.x==peer$peer_pool_rule.y))
checks$peer_counts<-data.table(check_id="accepted_peer_counts_preserved",passed=all(peer$peer_country_count.x==peer$peer_country_count.y))
checks$peer_global<-data.table(check_id="accepted_peer_global_labels_preserved",passed=all(peer$global_pool_used==peer$global_pool))
v<-fread(file.path(out,"selected_reference.csv"))
ids<-unique(trimws(unlist(strsplit(na.omit(v$selected_source_package_ids),";",fixed=TRUE))))
registry<-fread("docs/governance/source_package_registry.csv")
stopifnot(all(ids%in%registry$source_package_id))
for(id in ids){r<-registry[source_package_id==id];stopifnot(nrow(r)==1L)
  checks[[id]]<-data.table(check_id=paste0("registered_selected_source_hash::",id),passed=digest(r$path,file=TRUE,algo="sha256")==r$sha256)}
result<-rbindlist(checks);stopifnot(all(result$passed));result[,raw_replay:=basename(run)]
result[,verification_script_sha256:=digest("scripts/p15/verify_p15_analysis_release_candidate.R",file=TRUE,algo="sha256")]
fwrite(result,receipt);print(result)
