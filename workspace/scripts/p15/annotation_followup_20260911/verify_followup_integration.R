#!/usr/bin/env Rscript
# Independent integration receipts: preserve the preceding 21 stages and 387 tables.
source("scripts/p15/activate_p15_environment.R")
source("R/p15_master.R")
source("R/p15_master_findings.R")
args <- commandArgs(TRUE)
if(length(args)<2L || dir.exists(args[1]))stop("Supply a fresh directory and preceding run.json")
config <- p15_master_config()
now <- jsonlite::fromJSON(file.path(config$cache_root,"current_run.json"),simplifyVector=FALSE)
old <- jsonlite::fromJSON(args[2],simplifyVector=FALSE)
checks <- data.table::data.table(check=character(),passed=logical())
check <- function(name,value) {
  if(!isTRUE(value))stop("Follow-up integration check failed: ",name)
  checks <<- rbind(checks,data.table::data.table(check=name,passed=TRUE))
}
new_ids <- c("crs_modern_summary","gbohoui_context","panel_disbursement")
check("twenty_four_stages_retain_preceding_twenty_one_in_order",
  length(old$stages)==21L && length(now$stages)==24L &&
  identical(names(now$stages)[seq_along(old$stages)],names(old$stages)) &&
  identical(tail(names(now$stages),3L),new_ids))
check("raw_key_and_candidate_cache_reused",identical(now$raw_key,old$raw_key) &&
  identical(now$candidate,old$candidate))
check("all_twenty_one_preceding_stage_keys_and_directories_reused",
  identical(now$stages[names(old$stages)],old$stages))
preserved <- data.table::rbindlist(c(list(p15_master_table_changes("dataset",now$candidate,old$candidate)),
  lapply(names(old$stages),function(id)p15_master_table_changes(id,
    now$stages[[id]]$dir,old$stages[[id]]$dir))))
check("all_387_preceding_tables_preserve_substantive_values",nrow(preserved)==387L && !any(preserved$changed))
reference <- data.table::rbindlist(lapply(names(now$stages),function(id)
  p15_master_table_changes(id,now$stages[[id]]$dir,p15_master_reference(config,id))))
check("all_current_stage_tables_match_registered_references",!any(reference$changed))
check("thirty_eight_additive_master_indexed_tables_registered",nrow(reference[analysis %in% new_ids])==38L)
# The established headline index omits filenames containing 'checks'. These
# three panel validation tables contain useful case-level arithmetic and are
# compared explicitly here, in addition to every artifact's immutable hash.
panel_validation_names <- c("baseline_replay_checks.csv","profile_grid_checks.csv",
  "paired_grant_element_checks.csv")
panel_extra <- data.table::rbindlist(lapply(panel_validation_names,function(f) {
  a <- file.path(p15_master_reference(config,"panel_disbursement"),f)
  b <- file.path(now$stages$panel_disbursement$dir,f)
  data.table::data.table(table=f,changed=!identical(p15_master_csv_signature(a),p15_master_csv_signature(b)))
}))
check("three_case_level_panel_validation_tables_match_reference",!any(panel_extra$changed))
for(id in names(now$stages))check(paste0(id,"_immutable_receipt_valid"),identical(
  p15_master_verify(file.path(now$stages[[id]]$dir,"receipt.rds"))$key,now$stages[[id]]$key))
registry <- data.table::fread(config$analysis_registry,na.strings=NULL)
routes <- list()
for(id in new_ids) {
  deps <- strsplit(registry$dependencies[registry$id==id],";",fixed=TRUE)[[1]]
  parent_dirs <- c(dirname(now$candidate),vapply(now$stages[deps],`[[`,character(1),"dir"))
  paths <- normalizePath(p15_master_manifest_paths(file.path(now$stages[[id]]$dir,"input_manifest.csv")),mustWork=TRUE)
  fixed <- normalizePath(p15_master_external(config,id),mustWork=TRUE)
  in_parent <- Reduce(`|`,lapply(parent_dirs,function(p)startsWith(paths,paste0(p,"/"))))
  check(paste0(id,"_uses_current_declared_parents_or_registered_fixed_inputs"),all(in_parent | paths %in% fixed))
  check(paste0(id,"_does_not_pin_mutable_current_pointer"),!any(basename(paths)=="current_run.json"))
  produced <- data.table::fread(file.path(now$stages[[id]]$dir,"checks.csv"))
  check(paste0(id,"_producer_checks_pass"),all(produced$passed))
  routes[[id]] <- data.table::data.table(analysis=id,input=paths,current_parent=in_parent)
}
snapshot <- data.table::fread(config$analysis_snapshot_manifest)
previous_snapshot_path <- "config/p15_analysis_source_snapshot_20260911_annotations_v1.csv"
previous_snapshot <- data.table::fread(previous_snapshot_path)
at <- match(previous_snapshot$path,snapshot$path)
check("all_986_previous_source_hashes_preserved",nrow(previous_snapshot)==986L && !anyNA(at) &&
  identical(previous_snapshot$sha256,snapshot$sha256[at]))
check("all_registered_source_context_hashes_valid",all(vapply(snapshot$path,digest::digest,
  character(1),file=TRUE,algo="sha256")==snapshot$sha256))
parity_path <- "data-derived/p15_crs_modern_routing_parity_20260911_v1.csv"
parity <- data.table::fread(parity_path)
check("eleven_CRS_supplement_tables_preserved_by_pointer_routing",nrow(parity)==11L && !any(parity$changed))
context_checks_path <- "data-derived/p15_gbohoui_context_routing_verification_20260911_v2/checks.csv"
context_check <- data.table::fread(context_checks_path)
check("context_routing_parity_receipt_passes",all(context_check$passed))
versions <- data.table::fread(config$version_registry)
for(id in new_ids) {
  bundle <- jsonlite::fromJSON(file.path(now$stages[[id]]$dir,"version_bundle.json"))
  ids <- unlist(bundle[paste0(c("schema","estimator","admissibility","selection"),"_id")])
  check(paste0(id,"_four_version_ids_registered"),length(ids)==4L && all(ids %in% versions$version_id))
}
findings <- readLines(file.path(now$report,"CURRENT_FINDINGS.md"))
check("generated_supplements_include_currency_and_noncausal_identity_limits",
  any(grepl("verified loan denomination",findings,fixed=TRUE)) &&
  any(grepl("not confidence intervals",findings,fixed=TRUE)) &&
  any(grepl("fixed contractual final maturities",findings,fixed=TRUE)))
dir.create(args[1],recursive=TRUE)
data.table::fwrite(checks,file.path(args[1],"checks.csv"))
data.table::fwrite(preserved,file.path(args[1],"previous_387_tables.csv"))
data.table::fwrite(reference,file.path(args[1],"current_reference_parity.csv"))
data.table::fwrite(panel_extra,file.path(args[1],"panel_validation_table_parity.csv"))
data.table::fwrite(data.table::rbindlist(routes),file.path(args[1],"parent_routing.csv"))
jsonlite::write_json(list(current_report=now$report,previous_report=old$report,
  configuration=config$version,registered_sources=nrow(snapshot),registered_versions=nrow(versions),
  scope="Three additive descriptive supplements; no benchmark rule or prior output changes"),
  file.path(args[1],"verification_context.json"),auto_unbox=TRUE,pretty=TRUE)
writeLines(capture.output(sessionInfo()),file.path(args[1],"environment.txt"))
inputs <- c(args[2],file.path(now$report,"run.json"),config$config_path,config$analysis_registry,
  config$version_registry,config$analysis_snapshot_manifest,previous_snapshot_path,parity_path,
  context_checks_path,file.path(dirname(now$candidate),"receipt.rds"),
  vapply(now$stages,function(x)file.path(x$dir,"receipt.rds"),character(1)))
data.table::fwrite(p15_master_hashes(inputs),file.path(args[1],"input_manifest.csv"))
data.table::fwrite(p15_master_hashes(c("R/p15_master.R","R/p15_master_findings.R",
  "scripts/p15/annotation_followup_20260911/verify_followup_integration.R","renv.lock")),
  file.path(args[1],"code_manifest.csv"))
data.table::fwrite(p15_master_hashes(list.files(args[1],full.names=TRUE)),file.path(args[1],"output_manifest.csv"))
print(checks)
