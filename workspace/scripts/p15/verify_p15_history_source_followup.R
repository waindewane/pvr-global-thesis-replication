#!/usr/bin/env Rscript
suppressPackageStartupMessages({library(data.table);library(digest)})
args <- commandArgs(trailingOnly=TRUE)
stopifnot(length(args)==1L,!file.exists(args[1]))
pairs <- list(peer=c("data-derived/p15_peer_history_matching_20260907_v1","data-derived/p15_peer_history_matching_20260907_repeat"),
  source=c("data-derived/p15_source_closure_20260907_v1","data-derived/p15_source_closure_20260907_repeat"))
receipts <- list()
for(kind in names(pairs)) {
  dirs <- pairs[[kind]]
  for(name in list.files(dirs[1],pattern="csv.gz$")) {
    a <- fread(file.path(dirs[1],name));b <- fread(file.path(dirs[2],name))
    if("build_id"%in%names(a)){a[,build_id:=NULL];b[,build_id:=NULL]}
    receipts[[length(receipts)+1L]] <- data.table(check=paste(kind,"repeat",name),passed=identical(a,b))
  }
  for(dir in dirs)for(m in c("input_manifest.csv","script_manifest.csv","output_manifest.csv")) {
    x <- fread(file.path(dir,m))
    actual <- vapply(x$artifact_path,digest,character(1),file=TRUE,algo="sha256")
    receipts[[length(receipts)+1L]] <- data.table(check=paste(basename(dir),m),passed=all(actual==x$sha256))
  }
}
result <- rbindlist(receipts); stopifnot(all(result$passed))
fwrite(result,args[1]);print(result)
