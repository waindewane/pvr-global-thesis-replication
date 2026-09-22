#!/usr/bin/env Rscript
# Verify the new loan stage while preserving the pre-extension calculation caches.
source("scripts/p15/activate_p15_environment.R")
source("R/p15_master.R");source("R/p15_master_findings.R")
args<-commandArgs(TRUE)
if(length(args)<2L)stop("Supply fresh verification CSV and pre-extension run.json")
if(file.exists(args[1]))stop("Verification output exists")
config<-p15_master_config()
now<-jsonlite::fromJSON(file.path(config$cache_root,"current_run.json"),simplifyVector=FALSE)
old<-jsonlite::fromJSON(args[2],simplifyVector=FALSE)
checks<-data.table::data.table(check=character(),passed=logical())
check<-function(name,value){if(!isTRUE(value))stop("Integration verification failed: ",name)
 checks<<-rbind(checks,data.table::data.table(check=name,passed=TRUE))}
check("fifteen_registered_stages",length(now$stages)==15L)
check("old_raw_cache_reused",identical(now$raw_key,old$raw_key))
check("all_fourteen_old_stage_keys_and_directories_reused",length(old$stages)==14L &&
 identical(now$stages[names(old$stages)],old$stages))
changes<-data.table::fread(file.path(now$report,"changed_tables.csv"))
check("all_286_existing_tables_unchanged",nrow(changes[analysis!="loan_comparisons"])==286L &&
 !any(changes[analysis!="loan_comparisons",changed]))
loan<-now$stages$loan_comparisons
receipt<-p15_master_verify(file.path(loan$dir,"receipt.rds"))
check("loan_stage_receipt_and_recursive_outputs_valid",identical(receipt$key,loan$key))
check("loan_stage_matches_registered_reference",!any(p15_master_table_changes("loan_comparisons",loan$dir,
 p15_master_reference(config,"loan_comparisons"))$changed))
for(child in c("mpg","aiddata","add")){
 child_dir<-file.path(loan$dir,"normalization",child)
 check(paste0(child,"_normalization_rerun_is_nested_in_new_stage"),dir.exists(child_dir))
 mpath<-file.path(child_dir,"output_manifest.csv")
 m<-data.table::fread(mpath);field<-intersect(c("artifact_path","path"),names(m))[1]
 check(paste0(child,"_normalization_output_manifest_valid"),all(vapply(m[[field]],digest::digest,character(1),file=TRUE,algo="sha256")==m$sha256))
 cpath<-file.path(child_dir,"checks.csv");z<-data.table::fread(cpath)
 check(paste0(child,"_normalization_checks_pass"),all(z$passed))
}
previous_snapshot<-data.table::fread("config/p15_analysis_source_snapshot_20260909.csv")
snapshot<-data.table::fread(config$analysis_snapshot_manifest)
at<-match(previous_snapshot$path,snapshot$path)
check("old_935_analysis_source_hashes_preserved",nrow(previous_snapshot)==935L && !anyNA(at) &&
 identical(previous_snapshot$sha256,snapshot$sha256[at]))
check("successor_snapshot_all_sources_hash_valid",all(vapply(snapshot$path,digest::digest,character(1),file=TRUE,algo="sha256")==snapshot$sha256))
checks$current_report<-now$report
checks$previous_report<-old$report
data.table::fwrite(checks,args[1]);print(checks[,.(check,passed)])
