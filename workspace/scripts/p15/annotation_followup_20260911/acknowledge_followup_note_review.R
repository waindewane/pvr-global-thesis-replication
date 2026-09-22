#!/usr/bin/env Rscript
# Record only the parent's explicit review of the twelve new note/stage links.
source("scripts/p15/activate_p15_environment.R")
source("R/p15_master.R")
args <- commandArgs(TRUE)
if(length(args)!=1L || dir.exists(args[1]))stop("Supply a fresh review receipt directory")
config <- p15_master_config()
now <- jsonlite::fromJSON(file.path(config$cache_root,"current_run.json"),simplifyVector=FALSE)
stopifnot(config$version=="P15-MASTER-20260911-V2",length(now$stages)==24L)
review_path <- "docs/thesis_design/feedback_2026-09-11_followup/parent_live_note_review.json"
review <- data.table::as.data.table(jsonlite::fromJSON(review_path))
stopifnot(nrow(review)==4L,!anyDuplicated(review$path),all(vapply(review$path,
  digest::digest,character(1),file=TRUE,algo="sha256")==review$sha256))
alerts <- data.table::fread(file.path(now$report,"notes_review.csv"))
ids <- c("crs_modern_summary","gbohoui_context","panel_disbursement")
accepted <- alerts[analysis %in% ids & note %in% review$path & status=="live"]
stopifnot(nrow(accepted)==12L,!anyDuplicated(accepted[,.(analysis,note)]),
  all(accepted$review_required),all(!is.na(accepted$result_fingerprint)))
before <- data.table::fread(config$notes_review_receipt,colClasses="character")
stopifnot(!any(before$analysis %in% ids & before$note %in% review$path))
add <- accepted[,.(analysis,note,reviewed_fingerprint=result_fingerprint,
  reviewed_by="Parent_explicit_substantive_review_with_frozen_note_hashes_20260911_followup",
  reviewed_on="2026-09-11")]
dir.create(args[1],recursive=TRUE)
data.table::fwrite(before,file.path(args[1],"receipt_before.csv"))
data.table::fwrite(add,file.path(args[1],"twelve_reviewed_links.csv"))
for(p in review$path) {
  destination <- file.path(args[1],"reviewed_notes",p)
  dir.create(dirname(destination),recursive=TRUE,showWarnings=FALSE)
  stopifnot(file.copy(p,destination))
}
data.table::fwrite(review[,.(path,sha256)],file.path(args[1],"reviewed_note_hashes.csv"))
after <- rbind(before,add)
stopifnot(!anyDuplicated(after[,.(analysis,note)]))
data.table::fwrite(after,config$notes_review_receipt)
data.table::fwrite(after,file.path(args[1],"receipt_after.csv"))
jsonlite::write_json(list(review_basis=review_path,source_report=now$report,
  new_links=12L,previous_links_changed=0L,automatic_approval=FALSE),
  file.path(args[1],"review_context.json"),auto_unbox=TRUE,pretty=TRUE)
data.table::fwrite(p15_master_hashes(c(review_path,file.path(now$report,"run.json"),
  list.files(file.path(args[1],"reviewed_notes"),recursive=TRUE,full.names=TRUE))),
  file.path(args[1],"input_manifest.csv"))
data.table::fwrite(p15_master_hashes(c("scripts/p15/annotation_followup_20260911/acknowledge_followup_note_review.R",
  "R/p15_master.R","renv.lock")),file.path(args[1],"code_manifest.csv"))
writeLines(capture.output(sessionInfo()),file.path(args[1],"environment.txt"))
data.table::fwrite(p15_master_hashes(list.files(args[1],recursive=TRUE,full.names=TRUE)),
  file.path(args[1],"output_manifest.csv"))
cat("Acknowledged exactly twelve new links against the four parent-reviewed note hashes.\n")
