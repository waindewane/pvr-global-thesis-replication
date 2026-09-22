#!/usr/bin/env Rscript
suppressPackageStartupMessages({library(data.table);library(digest)})
args <- commandArgs(trailingOnly=TRUE)
stopifnot(length(args)==2L,all(dir.exists(args)))
out <- "data-derived/p15_analysis_candidate_20260907_v1"
receipt <- file.path(out,"two_raw_runs_byte_verification.csv")
stopifnot(!file.exists(receipt))
for(run in args) {
  path <- file.path(run,"acceptance.csv");stopifnot(file.exists(path))
  a <- fread(file=path);stopifnot(nrow(a)==6L,!anyNA(a$passed),all(a$passed))
}
m <- fread(file=file.path(out,"output_manifest.csv"))
stopifnot(nrow(m)==12L,all(file.exists(m$artifact_path)))
paths <- lapply(args,function(run)file.path(run,"isolated_build",m$artifact_path))
stopifnot(all(file.exists(unlist(paths))))
h <- function(p)unname(vapply(p,digest,character(1),file=TRUE,algo="sha256"))
r <- data.table(artifact=basename(m$artifact_path),standalone_sha256=h(m$artifact_path),
  targets_run_sha256=h(paths[[1]]),direct_run_sha256=h(paths[[2]]))
r[,passed:=standalone_sha256==targets_run_sha256 & targets_run_sha256==direct_run_sha256]
stopifnot(all(r$passed))
r[,`:=`(targets_run=basename(args[1]),direct_run=basename(args[2]))]
fwrite(r,receipt);print(r[,.(artifact,passed)])
