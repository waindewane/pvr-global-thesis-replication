#!/usr/bin/env Rscript
# Replay the v1/v2 routing-only numerical parity check into a fresh receipt.
source("scripts/p15/activate_p15_environment.R")
suppressPackageStartupMessages({library(data.table);library(digest);library(jsonlite)})
args <- commandArgs(TRUE)
out <- if(length(args))args[1] else "data-derived/p15_gbohoui_context_routing_verification_20260911_v2"
if(dir.exists(out))stop("Refusing overwrite: ",out)
a <- "data-derived/p15_gbohoui_context_20260911_v1"
b <- "data-derived/p15_gbohoui_context_20260911_v2"
files <- setdiff(list.files(a,pattern="\\.csv$"),
  c("input_manifest.csv","code_manifest.csv","output_manifest.csv"))
checks <- rbindlist(lapply(files,function(f) {
  x <- fread(file.path(a,f)); y <- fread(file.path(b,f))
  cols <- setdiff(names(x),"build_id")
  data.table(check=paste0("v2_preserves_",f),passed=identical(x[,..cols],y[,..cols]))
}))
stopifnot(all(checks$passed))
dir.create(out,recursive=TRUE)
fwrite(checks,file.path(out,"checks.csv"))
manifest <- function(paths)data.table(path=paths,
  sha256=vapply(paths,function(p)digest(file=p,algo="sha256"),character(1)),
  bytes=file.info(paths)$size)
fwrite(manifest(c(file.path(a,files),file.path(b,files))),file.path(out,"input_manifest.csv"))
fwrite(manifest("scripts/p15/annotation_followup_20260911/verify_gbohoui_routing.R"),file.path(out,"code_manifest.csv"))
writeLines(capture.output(sessionInfo()),file.path(out,"environment.txt"))
write_json(list(build_id=basename(out),baseline=a,successor=b,
  purpose="Routing-only successor preserves all fourteen data tables, excluding build identifier",
  lifecycle_status="diagnostic",release_state="private_research"),
  file.path(out,"version_bundle.json"),pretty=TRUE,auto_unbox=TRUE)
fwrite(manifest(list.files(out,full.names=TRUE)),file.path(out,"output_manifest.csv"))
cat(nrow(checks),"routing parity checks passed\n")
