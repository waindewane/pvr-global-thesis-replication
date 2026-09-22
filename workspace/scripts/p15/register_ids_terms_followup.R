#!/usr/bin/env Rscript
# Register new audit-only public snapshots; existing IDs cannot change contents.
library(data.table)
library(digest)
root <- "data-raw/p15_ids_terms_followup_20260906"
registry_path <- "docs/governance/source_package_registry.csv"
registry <- fread(registry_path,colClasses="character")
rows <- list()
add <- function(id,path,owner,vintage,scope,route) {
  rows[[length(rows)+1L]] <<- data.table(source_package_id=id,path=path,
    sha256=digest(path,algo="sha256",file=TRUE),source_owner=owner,access_class="public",
    acquisition_or_capture_date="2026-09-06",source_vintage=vintage,scope=scope,
    extraction_route=route,immutable_state="preserved_immutable",
    approved_role="Audit corroboration only; not a replacement calculation snapshot or permission change")
}
groups <- c("a_d","e_k","n_q","r_u")
for(g in groups) add(paste0("SRC-P15-IDS-XLSX-",toupper(g),"-20260906"),
  file.path(root,paste0(g,".xlsx")),"World Bank International Debt Statistics",
  "Catalogue Dec 5 2025; downloaded Sep 6 2026; HTTP Last-Modified recorded separately",
  paste("All counterparts; debtor group",g),"Official catalogue bulk XLSX; row and cell locators retained")
add("SRC-P15-IDS-CSV-LONG-20260906",file.path(root,"ids_csv_long.zip"),"World Bank IDS",
  "CSV member Apr 17 2026; metadata members Dec 5 2025",
  "WLD data only; six-country metadata reviewed; not a BND value substitute","Official catalogue ZIP")
add("SRC-P15-IDS-PERU-ISSUE-EVIDENCE-20260906",file.path(root,"peru_2020_documented_issues.csv"),
  "Project transcription of issuer and issuer-hosted evidence","2020 issues; later source documents",
  "Six issues; amount/coupon corroboration and diagnostic dates","Source URLs and locators per row; no IDS overwrite")

docs <- fread(file.path(root,"document_sources.csv"))
receipts <- rbindlist(lapply(seq_len(nrow(docs)),function(i) {
  path <- file.path(root,docs$filename[i]); exists <- file.exists(path)
  valid <- exists && file.info(path)$size>0
  if(valid && grepl("\\.pdf$",path)) valid <-
    identical(readBin(path,"raw",n=5L),charToRaw("%PDF-")) &&
      system2("pdfinfo",shQuote(path),stdout=FALSE,stderr=FALSE)==0L
  if(valid && grepl("\\.html$",path)) valid <- file.info(path)$size>1000
  id <- paste0("SRC-P15-IDS-TERMS-",docs$evidence_id[i],"-20260906")
  if(valid) add(id,path,"Issuer/source owner identified by source URL", "Document date and content locator in research note",
    docs$scope[i],"Direct public download; valid format; relevant passages reviewed")
  data.table(evidence_id=docs$evidence_id[i],url=docs$url[i],path=path,source_snapshot_id=id,
    file_state=if(valid)"valid_preserved_document" else if(exists)"incomplete_or_interstitial_not_usable" else "download_failed",
    bytes=if(exists)file.info(path)$size else NA_real_,
    sha256=if(exists)digest(path,algo="sha256",file=TRUE) else NA_character_)
}))
fwrite(receipts,file.path(root,"document_validation_manifest.csv"))
new <- rbindlist(rows)
for(i in seq_len(nrow(new))) {
  at <- which(registry$source_package_id==new$source_package_id[i])
  if(length(at)) stopifnot(length(at)==1L,registry$sha256[at]==new$sha256[i],registry$path[at]==new$path[i])
  else registry <- rbind(registry,new[i],fill=TRUE)
}
stopifnot(!anyDuplicated(registry$source_package_id))
fwrite(registry,registry_path)
fwrite(new,file.path(root,"source_manifest.csv"))

vpath <- "docs/governance/version_registry.csv"
v <- fread(vpath,colClasses="character")
id <- "SCHEMA-P15-IDS-TERMS-FOLLOWUP-V1"
if(!id %in% v$version_id) {
  v <- rbind(v,data.table(version_id=id,version_type="schema",lifecycle_status="diagnostic",
    scope="Eight IDS term cases plus six-country cross-format audit",parent_version_id="SPEC-P15-INTEGRATION-2012-2024-V0.1",
    meaning="Source-keyed comparison and explanatory hypotheses; no rate or term permission changes",
    compatibility_rule="Diagnostic only; cannot be joined as replacement IDS observations",
    decision_basis="Owner-authorized bounded source follow-up 2026-09-06"),fill=TRUE)
  fwrite(v,vpath)
}
print(receipts[,.(evidence_id,file_state)])
cat("Audit snapshots registered:",nrow(new),"\n")

# Evidence update only. Do not equate completed checks with resolved source causes.
lpath <- "docs/governance/audit_master_status_ledger_2026-08-09.csv"
ledger <- fread(lpath,colClasses="character")
note <- "docs/governance/P15_IDS_EIGHT_CASE_SOURCE_FOLLOWUP_2026-09-06.md"
for(id in c("IDS-04","IDS-05","TERM-11")) {
  at <- which(ledger$task_id==id); stopifnot(length(at)==1L)
  if(!grepl(note,ledger$completion_evidence[at],fixed=TRUE))
    set(ledger,at,"completion_evidence",paste(ledger$completion_evidence[at],note,sep="; "))
  set(ledger,at,"last_reconciled","2026-09-06")
}
for(id in c("IDS-04","TERM-11")) {
  at <- which(ledger$task_id==id)
  addition <- " Eight-case export/source follow-up is complete; underlying terms remain unresolved. Focused World Bank query is drafted and requires owner approval to send."
  if(!grepl("Eight-case export/source follow-up",ledger$next_action[at],fixed=TRUE))
    set(ledger,at,"next_action",paste0(ledger$next_action[at],addition))
}
fwrite(ledger,lpath)
