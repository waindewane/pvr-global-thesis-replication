#!/usr/bin/env Rscript
library(data.table)
library(digest)
args <- commandArgs(trailingOnly=TRUE)
stopifnot(length(args)==2L)
a <- args[1]; b <- args[2]
files <- c("all_312_export_comparisons.csv","eight_case_comparison.csv",
  "rwanda_2021_decomposition_diagnostic.csv","peru_2020_reconstruction_diagnostic.csv",
  "matched_term_metadata.csv","verification.csv","protected_file_check.csv")
res <- rbindlist(lapply(files,function(f) {
  x <- fread(file.path(a,f)); y <- fread(file.path(b,f))
  x[,build_id:=NULL]; y[,build_id:=NULL]
  data.table(file=f,first_run=a,second_run=b,identical_substantive_table=identical(x,y))
}))
stopifnot(all(res$identical_substantive_table))
for(run in c(a,b)) {
  m <- fread(file.path(run,"output_manifest.csv"))
  stopifnot(all(vapply(m$artifact_path,function(p)digest(p,algo="sha256",file=TRUE),character(1))==m$sha256))
}
lines <- readLines("docs/governance/P15_WORLD_BANK_IDS_TERMS_QUERY_DRAFT_2026-09-06.md")
q <- lines[startsWith(lines,"| ") & grepl("[(][A-Z]{3}[)]",lines)]
stopifnot(length(q)==8L)
cases <- fread(file.path(b,"eight_case_comparison.csv"))
for(line in q) {
  cells <- trimws(strsplit(line,"|",fixed=TRUE)[[1]])
  iso <- regmatches(cells[2],regexpr("[A-Z]{3}",cells[2]))
  z <- cases[iso3==iso & analysis_year==as.integer(cells[3])]
  stopifnot(nrow(z)==1L,z$DT.MAT.DPPG==as.numeric(cells[4]),
    z$DT.GPA.DPPG==as.numeric(cells[5]),
    z$DT.COM.DPPG.CD==as.numeric(gsub(",","",cells[6],fixed=TRUE)))
}
fwrite(res,"docs/governance/p15_ids_terms_followup_repeat_check_20260906.csv")
print(res[,.(file,identical_substantive_table)])
cat("All output hashes and all eight query table rows verified.\n")
