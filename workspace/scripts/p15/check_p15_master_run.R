#!/usr/bin/env Rscript
# Read-only verification of outputs; writes only a new verification table.
source("scripts/p15/activate_p15_environment.R")
source("R/p15_master.R");source("R/p15_master_findings.R")
args<-commandArgs(TRUE)
if(!length(args))stop("Supply a fresh verification CSV path and optionally the previous report's run.json")
if(file.exists(args[1]))stop("Verification destination already exists")
config<-p15_master_config()
state<-jsonlite::fromJSON(file.path(config$cache_root,"current_run.json"),simplifyVector=FALSE)
checks<-data.table::data.table(check=character(),passed=logical())
check<-function(name,value){if(!isTRUE(value))stop("Verification failed: ",name);checks<<-data.table::rbindlist(list(checks,data.table::data.table(check=name,passed=value)))}
raw_dir<-dirname(state$candidate)
raw<-p15_master_verify(file.path(raw_dir,"receipt.rds"))
check("raw_receipt_and_rebased_provenance_valid",identical(raw$key,state$raw_key))
for(id in names(state$stages)) {
  r<-p15_master_verify(file.path(state$stages[[id]]$dir,"receipt.rds"))
  check(paste0(id,"_receipt_valid"),identical(r$key,state$stages[[id]]$key))
}
for(f in c("input_manifest.csv","code_manifest.csv","output_manifest.csv")) {
  x<-data.table::fread(file.path(state$report,f))
  check(paste0(f,"_hashes_valid"),all(vapply(x$artifact_path,digest::digest,character(1),file=TRUE,algo="sha256")==x$sha256))
}
check("current_configuration_matches",identical(state$configuration,config$version))
registered<-data.table::fread(config$analysis_registry)$id
check("all_registered_stages_completed",setequal(registered,names(state$stages)))
if(is.null(config$revised_peer_config)) {
  check("dataset_matches_authorized_candidate",!any(p15_master_table_changes("dataset",state$candidate,config$reference_candidate)$changed))
  for(id in names(state$stages))check(paste0(id,"_matches_registered_research"),
    !any(p15_master_table_changes(id,state$stages[[id]]$dir,p15_master_reference(config,id))$changed))
} else {
  # Comparison references deliberately contain the old method. Verify the
  # accepted current invariants instead of requiring unchanged historical values.
  core<-data.table::fread(file.path(state$candidate,"core_evidence.csv"))
  selected<-data.table::fread(file.path(state$candidate,"selected_reference.csv"))
  members<-data.table::fread(file.path(state$candidate,"peer_membership.csv"))
  check("current_core_unique_keys",!anyDuplicated(core[,.(iso3,analysis_year)]))
  check("accepted_five_rule_cutoff",all(core[is.finite(peer_rate_pct),peer_rule_number] %in% 1:5))
  check("no_global_fallback",!any(core$global_pool_used %in% TRUE))
  check("no_self_donors",all(members$target_iso3!=members$peer_iso3))
  check("distinct_same_year_donors",!anyDuplicated(members[,.(target_iso3,analysis_year,peer_iso3)]))
  recomputed<-members[used_for_estimate %in% TRUE,.(donors=.N,rate=median(peer_rate_pct)),by=.(iso3=target_iso3,analysis_year)]
  paired<-merge(core[is.finite(peer_rate_pct),.(iso3,analysis_year,peer_rate_pct,peer_country_count)],recomputed,by=c("iso3","analysis_year"),all=TRUE)
  check("all_peer_values_reconcile_to_members",all(is.finite(paired$rate)) &&
    all(paired$donors>=3L) && all(paired$donors==paired$peer_country_count) && all(abs(paired$rate-paired$peer_rate_pct)<1e-10))
  check("selection_separate_from_availability",nrow(core)==nrow(selected) &&
    !anyDuplicated(selected[,.(iso3,analysis_year)]))
  readiness<-data.table::fread(file.path(state$candidate,"readiness_checks.csv"))
  check("revised_core_checks_pass",all(readiness$passed))
}
notes<-data.table::fread(file.path(state$report,"notes_review.csv"))
check("current_analytical_notes_reviewed",!any(notes$review_required))
if(length(args)>1) {
  old<-jsonlite::fromJSON(args[2],simplifyVector=FALSE)
  check("unchanged_rerun_reused_raw",identical(old$raw_key,state$raw_key))
  check("unchanged_rerun_reused_all_analysis_directories",identical(old$stages,state$stages))
  check("unchanged_rerun_reports_no_table_changes",!any(data.table::fread(file.path(state$report,"changed_tables.csv"))$changed))
}
data.table::fwrite(checks,args[1]);print(checks)
