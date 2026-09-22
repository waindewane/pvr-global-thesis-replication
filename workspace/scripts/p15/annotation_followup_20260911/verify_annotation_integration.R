#!/usr/bin/env Rscript
# Additive September 11 integration: existing numerical results must survive intact.
source("scripts/p15/activate_p15_environment.R")
source("R/p15_master.R")
source("R/p15_master_findings.R")
args <- commandArgs(TRUE)
if(length(args)<2L) stop("Supply a fresh verification directory and the preceding run.json")
if(dir.exists(args[1])) stop("Verification output already exists")
config <- p15_master_config()
now <- jsonlite::fromJSON(file.path(config$cache_root,"current_run.json"),simplifyVector=FALSE)
old <- jsonlite::fromJSON(args[2],simplifyVector=FALSE)
checks <- data.table::data.table(check=character(),passed=logical())
check <- function(name,value) {
  if(!isTRUE(value)) stop("Annotation integration verification failed: ",name)
  checks <<- rbind(checks,data.table::data.table(check=name,passed=TRUE))
}
new_ids <- c("regional_followup","statistical_review","crs_application",
  "source_overlap_currency","crs_companions")
check("twenty_one_stages_preserve_original_sixteen_in_order",
  length(now$stages)==21L && identical(names(now$stages)[seq_along(old$stages)],names(old$stages)) &&
    identical(tail(names(now$stages),5L),new_ids))
check("raw_key_and_rebased_candidate_reused",identical(now$raw_key,old$raw_key) &&
  identical(now$candidate,old$candidate))
preserved <- data.table::rbindlist(c(list(p15_master_table_changes("dataset",now$candidate,old$candidate)),
  lapply(names(old$stages),function(id)p15_master_table_changes(id,
    now$stages[[id]]$dir,old$stages[[id]]$dir))))
check("all_315_previous_tables_have_identical_substantive_values",
  nrow(preserved)==315L && !any(preserved$changed))
for(id in names(now$stages)) {
  stage <- now$stages[[id]]
  check(paste0(id,"_immutable_receipt_valid"),identical(
    p15_master_verify(file.path(stage$dir,"receipt.rds"))$key,stage$key))
  check(paste0(id,"_matches_registered_reference"),!any(
    p15_master_table_changes(id,stage$dir,p15_master_reference(config,id))$changed))
}
# Every actual non-code data input of new stages must be a registered fixed file
# or inside the raw/current declared parent directories, never a stale cache.
registry <- data.table::fread(config$analysis_registry,na.strings=NULL)
routes <- list()
for(id in new_ids) {
  deps <- strsplit(registry$dependencies[registry$id==id],";",fixed=TRUE)[[1]]
  parent_dirs <- c(dirname(now$candidate),vapply(now$stages[deps],`[[`,character(1),"dir"))
  manifest <- file.path(now$stages[[id]]$dir,"input_manifest.csv")
  paths <- normalizePath(p15_master_manifest_paths(manifest),mustWork=TRUE)
  fixed <- normalizePath(p15_master_external(config,id),mustWork=TRUE)
  in_parent <- Reduce(`|`,lapply(parent_dirs,function(p)startsWith(paths,paste0(p,"/"))))
  check(paste0(id,"_consumes_only_current_declared_parents_or_registered_sources"),
    all(in_parent | paths %in% fixed))
  routes[[id]] <- data.table::data.table(analysis=id,input=paths,current_parent=in_parent)
}
snapshot <- data.table::fread(config$analysis_snapshot_manifest)
previous_snapshot <- data.table::fread("config/p15_analysis_source_snapshot_20260910_period_v1.csv")
at <- match(previous_snapshot$path,snapshot$path)
check("all_954_preceding_source_hashes_preserved",nrow(previous_snapshot)==954L &&
  !anyNA(at) && identical(previous_snapshot$sha256,snapshot$sha256[at]))
check("986_registered_inputs_hash_valid",nrow(snapshot)==986L && all(vapply(snapshot$path,
  digest::digest,character(1),file=TRUE,algo="sha256")==snapshot$sha256))
aliases <- data.table::fread("config/p15_crs_snapshot_aliases_20260911_v1.csv")
check("fourteen_CRS_acquisition_aliases_link_exact_saved_files",nrow(aliases)==14L &&
  !anyDuplicated(aliases$producer_source_snapshot_id) && all(vapply(aliases$path,
  digest::digest,character(1),file=TRUE,algo="sha256")==aliases$sha256))
interface <- data.table::fread("data-derived/p15_annotation_interface_parity_20260911_v1.csv")
check("fifteen_interface_rebased_tables_unchanged",nrow(interface)==15L && !any(interface$changed))
findings <- readLines(file.path(now$report,"CURRENT_FINDINGS.md"))
check("generated_findings_retain_inference_qualifications_without_headline_p_columns",
  any(grepl("no p-values",findings,fixed=TRUE)) &&
  !any(grepl("p_holm|p_value|country_bootstrap_p",findings)))
dir.create(args[1],recursive=TRUE)
data.table::fwrite(checks,file.path(args[1],"checks.csv"))
data.table::fwrite(preserved,file.path(args[1],"previous_315_tables.csv"))
data.table::fwrite(data.table::rbindlist(routes),file.path(args[1],"parent_routing.csv"))
jsonlite::write_json(list(current_report=now$report,previous_report=old$report,
  current_config=config$version,scope="Additive descriptive diagnostics; previous results preserved"),
  file.path(args[1],"verification_context.json"),auto_unbox=TRUE,pretty=TRUE)
writeLines(capture.output(sessionInfo()),file.path(args[1],"environment.txt"))
inputs <- unique(c(args[2],file.path(now$report,"run.json"),config$config_path,
  config$analysis_registry,config$analysis_snapshot_manifest,
  "config/p15_analysis_source_snapshot_20260910_period_v1.csv",
  "config/p15_crs_snapshot_aliases_20260911_v1.csv",
  "data-derived/p15_annotation_interface_parity_20260911_v1.csv",
  file.path(dirname(now$candidate),"receipt.rds"),
  vapply(now$stages,function(x)file.path(x$dir,"receipt.rds"),character(1)),
  vapply(old$stages,function(x)file.path(x$dir,"receipt.rds"),character(1)),
  vapply(setdiff(names(now$stages),"pv_three_block"),function(id)
    file.path(p15_master_reference(config,id),"input_manifest.csv"),character(1))))
data.table::fwrite(p15_master_hashes(inputs),file.path(args[1],"input_manifest.csv"))
data.table::fwrite(p15_master_hashes(c("R/p15_master.R","R/p15_master_findings.R",
  "scripts/p15/annotation_followup_20260911/verify_annotation_integration.R","renv.lock")),
  file.path(args[1],"code_manifest.csv"))
data.table::fwrite(p15_master_hashes(list.files(args[1],full.names=TRUE)),
  file.path(args[1],"output_manifest.csv"))
print(checks)
