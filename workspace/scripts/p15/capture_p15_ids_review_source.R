#!/usr/bin/env Rscript
# Optional public-source corroboration only. Never refreshes the registered build
# snapshot; audit acquisition is separate from the offline reproduction pipeline.
library(data.table)
library(jsonlite)
library(digest)
cases <- fread("data-derived/p15_local_source_rating_audit_20260906_v1/p15_ids_source_review_cases.csv")
out <- "data-raw/p15_ids_review_public_snapshot_20260906"
dir.create(out,recursive=TRUE,showWarnings=FALSE)
countries <- paste(sort(unique(cases$iso3)),collapse=";")
years <- paste0("YR",2012:2024,collapse=";")
series <- c("DT.INR.DPPG","DT.MAT.DPPG","DT.GPA.DPPG","DT.COM.DPPG.CD")
manifest <- list()
for (s in series) {
  path <- file.path(out,paste0(gsub("\\.","_",s),"_BND.json"))
  url <- paste0("https://api.worldbank.org/v2/sources/6/country/",countries,"/series/",s,
    "/counterpart-area/BND/time/",years,"?format=json&per_page=20000")
  if(!file.exists(path)) download.file(url,path,mode="wb",quiet=TRUE,method="libcurl")
  x <- fromJSON(path)
  if(is.null(x$source$data))stop("Not a valid IDS source response: ",path)
  stopifnot(as.integer(x$pages)==1L)
  manifest[[s]] <- data.table(path=path,url=url,sha256=digest(path,algo="sha256",file=TRUE),
    source_lastupdated=as.character(x$lastupdated),role="audit_only_no_build_snapshot_replacement")
}
fwrite(rbindlist(manifest,fill=TRUE),file.path(out,"source_manifest.csv"))
