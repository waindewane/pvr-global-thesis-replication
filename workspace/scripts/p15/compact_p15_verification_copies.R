#!/usr/bin/env Rscript
# Reclaim only large identical INPUT copies made by this task's repeat tests.
# Keep all run outputs/logs/code and a complete current input copy. Original
# project sources and the targets-tracked current run are never modified.
library(data.table)
current <- "data-derived/p15_full_replay_20260906_112727"
repeats <- c("data-derived/p15_full_replay_20260906_111831","data-derived/p15_full_replay_20260906_113432")
proof <- "data-derived/p15_bucket_completion_acceptance_20260906"
cmp <- fread(file.path(proof,"independent_run_comparisons.csv"))
stopifnot(all(cmp$bytes_identical),all(repeats[-1] %in% cmp$compared_run))
hash <- function(p) digest::digest(p,algo="sha256",file=TRUE)
removed <- list()
for(r in repeats) {
  m <- fread(file.path(r,"input_manifest.csv"))
  products <- fread(file.path(r,"output_manifest.csv"))$artifact_path
  for(i in which(m$artifact_role!="code" & m$bytes>=1000000)) {
    relative <- file.path("isolated_build",m$artifact_path[i])
    old <- file.path(r,relative)
    retained <- file.path(current,relative)
    stopifnot(!relative %in% products)
    if(!file.exists(old)) next
    stopifnot(file.exists(retained),identical(hash(old),m$sha256[i]),identical(hash(retained),m$sha256[i]))
    removed[[old]] <- data.table(removed_duplicate=old,retained_identical_copy=retained,
      sha256=m$sha256[i],bytes=file.info(old)$size)
    unlink(old)
    stopifnot(!file.exists(old),file.exists(retained))
  }
}
if(length(removed)) {
  result <- rbindlist(removed)
  fwrite(result,file.path(proof,"deduplicated_verification_input_copies.csv"))
  cat("Reclaimed",round(sum(result$bytes)/1024^2,1),"MiB of duplicate task-created inputs.\n")
}
