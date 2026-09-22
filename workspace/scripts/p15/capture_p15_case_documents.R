#!/usr/bin/env Rscript
# Optional source acquisition; the actual build uses only these saved files.
library(data.table)
library(digest)
dir <- "data-raw/p15_case_review_20260906"
m <- fread(file.path(dir,"document_sources.csv"))
for(i in seq_len(nrow(m))) {
  p <- file.path(dir,m$filename[i])
  if(!file.exists(p) || file.info(p)$size<100) tryCatch(download.file(m$url[i],p,mode="wb",quiet=TRUE,method="curl",extra="--fail --location --max-time 45"),
    error=function(e) message(m$evidence_id[i],": ",conditionMessage(e)))
}
m[,artifact_path:=file.path(dir,filename)]
m[,sha256:=vapply(artifact_path,function(p) if(file.exists(p)) digest(p,algo="sha256",file=TRUE) else NA_character_,character(1))]
m[,capture_state:=ifelse(file.exists(artifact_path),"downloaded_requires_content_check","download_failed_web_index_evidence_only")]
m[,source_snapshot_id:=paste0("SRC-P15-CASE-",evidence_id,"-20260906")]
fwrite(m,file.path(dir,"source_manifest.csv"))
