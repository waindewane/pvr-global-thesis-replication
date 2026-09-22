#!/usr/bin/env Rscript
# Public documentation acquisition only; never refreshes calculation inputs.
library(data.table)
library(digest)
root <- "data-raw/p15_ids_terms_followup_20260906"
sources <- fread(file.path(root, "document_sources.csv"))
receipts <- lapply(seq_len(nrow(sources)), function(i) {
  s <- sources[i]
  path <- file.path(root, s$filename)
  status <- 0L
  if (!file.exists(path)) {
    status <- system2("curl", c("--silent", "--show-error", "--fail", "--location",
      "--max-time", "45", shQuote(s$url), "-o", shQuote(path)))
  }
  valid_file <- status == 0L && file.exists(path) && file.info(path)$size > 0
  if (valid_file && grepl("\\.pdf$",path)) {
    # HTTP 200 can be a security interstitial; incomplete PDFs are not evidence.
    valid_file <- identical(readBin(path,"raw",n=5L),charToRaw("%PDF-")) &&
      system2("pdfinfo",shQuote(path),stdout=FALSE,stderr=FALSE)==0L
  }
  data.table(evidence_id=s$evidence_id, url=s$url, scope=s$scope,
    path=path, source_snapshot_id=paste0("SRC-P15-IDS-TERMS-",s$evidence_id,"-20260906"),
    capture_date="2026-09-06", acquisition_status=if(valid_file) "downloaded_pending_content_check" else "download_failed",
    bytes=if(valid_file) file.info(path)$size else NA_real_,
    sha256=if(valid_file) digest(path, algo="sha256", file=TRUE) else NA_character_)
})
fwrite(rbindlist(receipts),file.path(root,"document_capture_manifest.csv"))
print(rbindlist(receipts)[,.(evidence_id,acquisition_status,bytes)])
