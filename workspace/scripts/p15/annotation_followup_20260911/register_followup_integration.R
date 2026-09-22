#!/usr/bin/env Rscript
# One-time additive registration; no changes to the shared stage orchestrator.
source("scripts/p15/activate_p15_environment.R")
source("R/p15_master.R")
library(data.table)
config <- p15_master_config()
stopifnot(config$version=="P15-MASTER-20260911-V1")
base <- "docs/thesis_design/feedback_2026-09-11_followup"
archive <- file.path(base,"integration_baseline")
stopifnot(!dir.exists(archive))
new <- data.table(
 id=c("crs_modern_summary","gbohoui_context","panel_disbursement"),
 script=paste0("scripts/p15/annotation_followup_20260911/",c(
   "build_crs_modern_summary.R","build_gbohoui_context.R","build_panel_disbursement_sensitivity.R")),
 dependencies=c("crs_application;loan_comparisons","regional","bullet"),
 reference=c("data-derived/p15_crs_modern_summary_20260911_v4",
   "data-derived/p15_gbohoui_context_20260911_v2","data-derived/p15_panel_disbursement_20260911_v1"),
 design=file.path(base,c("CRS_MODERN_SUMMARY_DESIGN.md","GBOHOUI_CONTEXT_DESIGN.md","PANEL_DISBURSEMENT_DESIGN.md")),
 report=file.path(base,c("CRS_MODERN_RESULTS.md","GBOHOUI_CONTEXT_FOLLOWUP.md","PANEL_DISBURSEMENT_SENSITIVITY.md")))
stopifnot(all(file.exists(c(new$script,new$design,new$report,
  file.path(new$reference,"version_bundle.json")))))
for(ref in new$reference)stopifnot(all(fread(file.path(ref,"checks.csv"))$passed))
registry <- fread(config$analysis_registry,na.strings=NULL)
stopifnot(nrow(registry)==21L,!any(new$id %in% registry$id))
old_files <- c(config$config_path,config$analysis_registry,config$version_registry,
  config$notes_registry,config$notes_review_receipt,config$analysis_snapshot_manifest,
  file.path(config$cache_root,"current_run.json"))
dir.create(archive,recursive=TRUE)
for(p in old_files) {
  dest <- file.path(archive,p)
  dir.create(dirname(dest),recursive=TRUE,showWarnings=FALSE)
  stopifnot(file.copy(p,dest))
}
fwrite(p15_master_hashes(list.files(archive,recursive=TRUE,full.names=TRUE)),
  file.path(archive,"manifest.csv"))
for(i in seq_len(nrow(new)))config$reference_stages[[new$id[i]]] <- new$reference[i]
new_registry <- rbind(registry,new[,.(id,script,dependencies,reference_group=id)])
external <- unique(unlist(lapply(new_registry$id,function(id)p15_master_external(config,id))))
conventions <- "sources/literature_review/ids_ge_conventions_20260911/source_ledger.csv"
context_sources <- unique(c(conventions,fread(conventions)$path))
old_snapshot <- fread(config$analysis_snapshot_manifest)
snapshot <- p15_master_hashes(unique(c(old_snapshot$path,external,context_sources)))
at <- match(old_snapshot$path,snapshot$path)
stopifnot(nrow(old_snapshot)==986L,!anyNA(at),
  identical(old_snapshot$sha256,snapshot$sha256[at]))
new_snapshot <- "config/p15_analysis_source_snapshot_20260911_followup_v1.csv"
stopifnot(!file.exists(new_snapshot))
fwrite(snapshot,new_snapshot)
fwrite(new_registry,config$analysis_registry)
versions <- fread(config$version_registry)
for(i in seq_len(nrow(new))) {
  bundle <- jsonlite::fromJSON(file.path(new$reference[i],"version_bundle.json"))
  for(type in c("schema","estimator","admissibility","selection")) {
    id <- bundle[[paste0(type,"_id")]]
    stopifnot(length(id)==1L,!is.na(id),nzchar(id))
    if(id %in% versions$version_id)next
    meaning <- switch(type,schema="Versioned additive descriptive output contract",
      estimator="Prespecified descriptive supplement; inherited numerical rates and repayments",
      admissibility="Inherited eligibility and explicit contextual sample restrictions",
      selection="Existing accepted benchmark selection unchanged")
    versions <- rbind(versions,data.table(version_id=id,version_type=type,
      lifecycle_status="diagnostic",scope=paste("Owner follow-up; 11 September 2026;",new$id[i]),
      parent_version_id="SPEC-P15-INTEGRATION-2012-2024-V0.1",meaning=meaning,
      compatibility_rule="Private diagnostic; no canonical promotion; preceding outputs preserved",
      decision_basis=new$design[i]))
  }
}
stopifnot(!anyDuplicated(versions$version_id))
fwrite(versions,config$version_registry)
notes <- fread(config$notes_registry)
live <- c("docs/thesis_design/PROPOSED_THESIS_DISCUSSION_2026-09-09.md",
  "docs/governance/P15_CURRENT_RESEARCH_DIRECTION.md","docs/PROJECT_STATUS.md",
  "docs/ASSUMPTIONS_AND_LIMITATIONS.md")
for(i in seq_len(nrow(new))) {
  notes <- rbind(notes,data.table(analysis=new$id[i],note=new$report[i],status="preserved_versioned"),
    data.table(analysis=new$id[i],note=live,status="live"))
}
support <- file.path(base,c("OWNER_ANNOTATIONS.md","RESEARCH_STRAND_AUDIT.md",
  "IDS_GRANT_ELEMENT_CONVENTIONS.md","TODO_STATUS_CORRECTIONS.md"))
stopifnot(all(file.exists(support)))
notes <- unique(rbind(notes,data.table(analysis="dataset",note=support,status="preserved_versioned")))
fwrite(notes,config$notes_registry)
config$version <- "P15-MASTER-20260911-V2"
config$analysis_snapshot_manifest <- new_snapshot
path <- config$config_path
config$config_path <- NULL
jsonlite::write_json(config,path,auto_unbox=TRUE,pretty=TRUE)
fwrite(new,file.path(base,"REGISTERED_FOLLOWUP_STAGES.csv"))
cat("Registered",nrow(new),"additive stages;",nrow(snapshot),"source/context inputs;",
    nrow(versions),"version IDs. Twelve new live note links await explicit review.\n")
