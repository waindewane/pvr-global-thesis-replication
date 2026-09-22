#!/usr/bin/env Rscript
source("scripts/p15/activate_p15_environment.R")
suppressPackageStartupMessages({library(data.table); library(digest)})
out <- "data-raw/oecd_crs/2026-09-11"
dir.create(out, recursive=TRUE, showWarnings=FALSE)
meta <- "data-raw/oecd_crs/2026-05-25/crs_dataflow_references_v1_6.json"
lines <- readLines(meta, warn=FALSE)
lines <- lines[grepl('xml:lang="en">CRS 20(1[2-9]|2[0-3]) \\(dotStat', lines)]
year <- as.integer(sub('.*CRS ([0-9]{4}).*', '\\1', lines))
url <- sub('.*\\|(https://[^<]+)<.*', '\\1', lines)
stopifnot(length(year)==12L, setequal(year, 2012:2023))
plan <- data.table(year, url, path=file.path(out,paste0("crs_",year,"_dotstat_v20260408.zip")))
fwrite(plan,file.path(out,"download_plan.csv"))
fetch <- function(i) {
 p <- plan$path[i]
 if(!file.exists(p)) {
  rc <- system2("curl",c("-L","--fail","--retry","1","--max-time","240","--silent","--show-error",
   "--output",shQuote(paste0(p,".part")),shQuote(plan$url[i])),stderr=paste0(p,".download.log"))
  if(rc!=0L)return(data.table(plan[i],status=paste0("curl_",rc),bytes=NA_real_,sha256=NA_character_))
  if(system2("unzip",c("-t",shQuote(paste0(p,".part"))),stdout=FALSE,stderr=FALSE)!=0L)
   stop("Invalid archive: ",p)
  stopifnot(file.rename(paste0(p,".part"),p))
 }
 data.table(plan[i],status="validated_zip",bytes=file.info(p)$size,sha256=digest(file=p,algo="sha256"))
}
res <- rbindlist(parallel::mclapply(seq_len(nrow(plan)), fetch, mc.cores=2L))
res[,`:=`(source_snapshot_id=paste0("SRC-OECD-CRS-",year,"-20260408-RETRIEVED-20260911"),
 retrieved_date="2026-09-11", metadata_path=meta, metadata_sha256=digest(file=meta,algo="sha256"))]
fwrite(res,file.path(out,"source_manifest.csv")); print(res[,.(year,status,bytes)])
stopifnot(all(res$status=="validated_zip"))
