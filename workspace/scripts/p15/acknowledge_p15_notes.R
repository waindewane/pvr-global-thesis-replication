#!/usr/bin/env Rscript
# Explicit manual action only: run after reading/reconciling each named note.
source("scripts/p15/activate_p15_environment.R")
source("R/p15_master.R");source("R/p15_master_findings.R")
args<-commandArgs(TRUE)
if(length(args)<2||args[1]!="--reviewed")stop("Usage: Rscript scripts/p15/acknowledge_p15_notes.R --reviewed docs/NOTE.md [docs/OTHER.md]")
config<-p15_master_config()
state<-jsonlite::fromJSON(file.path(config$cache_root,"current_run.json"))
results<-data.table::fread(file.path(state$report,"current_outputs.csv"))
registry<-data.table::fread(config$notes_registry)
notes<-args[-1]
if(!all(notes %in% registry[status=="live"]$note))stop("Only explicitly registered live notes can be reviewed")
review<-merge(registry[status=="live" & note %in% notes],results,by="analysis")
for(i in seq_len(nrow(review))) {
  if(p15_master_result_fingerprint(review$directory[i])!=review$result_fingerprint[i])
    stop("Results changed since the report; refresh before acknowledging")
}
review<-review[,.(analysis,note,reviewed_fingerprint=result_fingerprint,
  reviewed_by=Sys.getenv("P15_NOTE_REVIEWER","unspecified_explicit_reviewer"),reviewed_on=as.character(Sys.Date()))]
old<-data.table::fread(config$notes_review_receipt,colClasses="character")
old<-old[!paste(analysis,note) %in% paste(review$analysis,review$note)]
data.table::fwrite(data.table::rbindlist(list(old,review)),config$notes_review_receipt)
cat("Recorded",nrow(review),"reviewed note/analysis links. Rerun master to refresh notices. Record the review in WORKLOG.md.\n")
