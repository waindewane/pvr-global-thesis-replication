#!/usr/bin/env Rscript
# Read-only verification of run products; writes only a separate acceptance report.
args <- commandArgs(trailingOnly=TRUE)
if(length(args)<3L) stop("Supply report directory followed by at least two completed run directories")
report <- args[1]
runs <- args[-1]
dir.create(report,recursive=TRUE,showWarnings=FALSE)
library(data.table)
sha <- function(p) digest::digest(p,algo="sha256",file=TRUE)
summary <- list()
for(r in runs) {
  a <- fread(file.path(r,"acceptance.csv"))
  # The first development-success receipt used a negative assertion as a row.
  a[check_id=="new_ladder_promoted",passed:=!passed]
  stopifnot(all(a$passed))
  cmp <- fread(file.path(r,"output_comparison.csv"))
  stopifnot(all(cmp$semantic_match))
  m <- fread(file.path(r,"output_manifest.csv"))
  h <- vapply(file.path(r,m$artifact_path),sha,character(1))
  stopifnot(identical(unname(h),m$sha256))
  q <- fread(file.path(r,"isolated_build/data-derived/p15_reviewed_evidence_20260906_v1/p15_quality_checks.csv"))
  stopifnot(all(q$passed))
  summary[[r]] <- data.table(run=r,baseline_tables=nrow(cmp),baseline_semantic_pass=sum(cmp$semantic_match),
    output_hashes_verified=nrow(m),review_quality_checks=sum(q$passed))
}
base <- runs[1]
paths <- paste0("isolated_build/",fread(file.path(base,"output_comparison.csv"))$artifact_path)
review <- "isolated_build/data-derived/p15_reviewed_evidence_20260906_v1"
paths <- unique(c(paths,file.path(review,list.files(file.path(base,review),pattern="\\.csv$"))))
comparisons <- rbindlist(lapply(runs[-1],function(r) {
  data.table(reference_run=base,compared_run=r,artifact_path=paths,
    reference_sha256=vapply(file.path(base,paths),sha,character(1)),
    compared_sha256=vapply(file.path(r,paths),sha,character(1)))
}))
comparisons[,bytes_identical:=reference_sha256==compared_sha256]
fwrite(comparisons,file.path(report,"independent_run_comparisons.csv"))
fwrite(rbindlist(summary),file.path(report,"run_acceptance_summary.csv"))
if(any(!comparisons$bytes_identical)) stop("Independent product bytes differ; inspect acceptance comparisons")
cat("Verified",length(runs),"completed runs;",length(paths),"substantive tables identical per repeat.\n")
if(identical(Sys.getenv("P15_VERIFY_CACHE"),"true")) {
  before <- targets::tar_meta(names="p15_reviewed_evidence_build",fields="time")
  log <- file.path(report,"unchanged_targets_check.log")
  rc <- system2(file.path(R.home("bin"),"Rscript"),c("-e",shQuote("targets::tar_make()")),stdout=log,stderr=log)
  after <- targets::tar_meta(names="p15_reviewed_evidence_build",fields="time")
  stopifnot(rc==0L,identical(before,after))
  e <- new.env(parent=globalenv())
  source("R/p15_replay.R",local=e)
  source("R/p15_full_replay.R",local=e)
  e$p15_full_specification <- function(...) list(leaves="intentionally_missing_p15_input_for_preflight_test.csv")
  missing <- tryCatch(e$p15_run_full_replay(),error=function(err) conditionMessage(err))
  stopifnot(identical(missing,"Missing raw replay leaves: intentionally_missing_p15_input_for_preflight_test.csv"))
  fwrite(data.table(check_id=c("unchanged_build_timestamp_reused","missing_input_stops_with_exact_path"),
    passed=TRUE,detail=c("See unchanged_targets_check.log",missing)),file.path(report,"cache_and_missing_input_checks.csv"))
}
if(identical(Sys.getenv("P15_VERIFY_TESTS"),"true")) {
  result <- testthat::test_dir("tests/testthat",reporter="summary",stop_on_failure=TRUE)
  tab <- as.data.table(as.data.frame(result))
  if("result" %in% names(tab)) tab[,result:=NULL]
  fwrite(tab,file.path(report,"test_results.csv"))
}
