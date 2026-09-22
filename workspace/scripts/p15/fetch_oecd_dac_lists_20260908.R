#!/usr/bin/env Rscript
library(data.table)
library(digest)
out <- "sources/official_terms/oecd_dac_lists_20260908"
dir.create(out, recursive=TRUE, showWarnings=FALSE)
periods <- c("2012-13", "2014-17", "2018-19", "2020", "2021", "2022-23", "2024")
x <- data.table(file=paste0("dac_", periods, ".pdf"),
  url=paste0("https://webfs.oecd.org/oda/DAClists/DAC%20List%20of%20Aid%20Recipients%20-%20",periods,"%20flows.pdf"))
x <- rbind(x, data.table(file=paste0("dac_",c("2021","2022-23","2024"),".csv"),
  url=paste0("https://webfs.oecd.org/oda/DAClists/DAC-List-of-ODA-Recipients-for-reporting-",c("2021","2022-23","2024"),"-flows.csv")))
x <- rbind(x, data.table(file="directives_2024_40_final.pdf",url="https://one.oecd.org/document/DCD/DAC%282024%2940/FINAL/en/pdf"))
for(i in seq_len(nrow(x))) {
  p <- file.path(out,x$file[i])
  if (!file.exists(p)) {
    status <- system2("curl",c("--fail","--location","--silent","--show-error","--max-time","45",shQuote(x$url[i]),"--output",shQuote(p)))
    if(status!=0){if(x$file[i]=="directives_2024_40_final.pdf")next else stop("Download failed: ",x$file[i])}
  }
  if(grepl("pdf$",p))stopifnot(rawToChar(readBin(p,"raw",5))=="%PDF-")
  message(x$file[i],": ",file.info(p)$size," bytes")
}
x[,`:=`(artifact_path=file.path(out,file),retrieved_date="2026-09-08",source_snapshot_id="SRC-OECD-DAC-LISTS-20260908")]
x[,retrieval_state:=ifelse(file.exists(artifact_path),"downloaded","not_downloaded_http403")]
x[,sha256:=vapply(artifact_path,function(p)if(file.exists(p))digest(p,file=TRUE,algo="sha256") else NA_character_,character(1))]
fwrite(x,file.path(out,"source_manifest.csv"))
