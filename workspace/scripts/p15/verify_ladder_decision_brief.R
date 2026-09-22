#!/usr/bin/env Rscript
# Verify report inputs, exact repeatability, ledger structure and local evidence links.
suppressPackageStartupMessages({library(data.table);library(digest)})
args <- commandArgs(trailingOnly=TRUE)
stopifnot(length(args)==2L,all(dir.exists(args)))
sha <- function(p)digest(p,algo="sha256",file=TRUE)
files <- setdiff(list.files(args[1],pattern="\\.csv$"),
  c("input_manifest.csv","output_manifest.csv","script_manifest.csv","environment_manifest.csv"))
stopifnot(setequal(files,setdiff(list.files(args[2],pattern="\\.csv$"),
  c("input_manifest.csv","output_manifest.csv","script_manifest.csv","environment_manifest.csv"))))
checks <- rbindlist(lapply(files,function(f){
  a<-fread(file.path(args[1],f));b<-fread(file.path(args[2],f))
  a[,build_id:=NULL];b[,build_id:=NULL]
  data.table(check=paste("exact repeat",f),passed=identical(a,b))
}))
add <- function(name,ok)checks <<- rbind(checks,data.table(check=name,passed=isTRUE(ok)))
for(d in args)for(m in c("input_manifest.csv","output_manifest.csv","script_manifest.csv")) {
  x<-fread(file.path(d,m))
  add(paste("hashes",basename(d),m),all(file.exists(x$artifact_path))&&
    all(vapply(x$artifact_path,sha,character(1))==x$sha256))
}
read <- function(f)fread(file.path(args[1],paste0(f,".csv")))
coverage <- read("source_coverage")[scope=="historical_lmic"]
expected <- data.table(tier=c("primary_usd","primary_usd_or_eur","ids_reviewed_candidate",
  "secondary_usd","moodys","moodys_precedence","agency_median"),
  country_years=c(215L,249L,420L,251L,901L,1024L,1024L),countries=c(47L,50L,67L,39L,86L,95L,95L))
matched <- coverage[match(expected$tier,tier)]
add("report coverage and historical-LMIC denominator",all(matched$country_years==expected$country_years)&
  all(matched$countries==expected$countries)&all(coverage$denominator==1756L))
selected<-read("selection_counts")[scope=="historical_lmic"&
  preview_variant=="ids_before_secondary__ids_case_review_applied__broad_peer_included"]
want<-c(primary=215L,ids=230L,secondary=71L,moodys=454L,peer=778L,no_eligible_rate=8L)
add("reference preview counts and totals",all(selected$country_years==unname(want[selected$preview_source]))&&sum(selected$country_years)==1756L)
changes<-read("ids_secondary_change_summary")[scope=="historical_lmic"]
add("report source-order consequences",changes$country_years==30L&&changes$countries==12L&&
  round(changes$mean_absolute_gap_pp,2)==2.61&&round(changes$median_absolute_gap_pp,2)==1.64&&changes$above_1pp==19L)
peer<-read("peer_incremental_summary_lmic")
add("report incremental peer counts",peer$country_years==778L&&peer$target_moodys_missing==776L&&
  peer$rating_proximity_used==1L&&peer$global_pool_used==329L&&peer$no_status_case_record==698L)
overlap<-read("ids_primary_overlap_lmic")
add("report IDS-primary overlap",overlap$n==189L&&round(overlap$correlation,3)==0.864&&round(overlap$mean_absolute_gap_pp,3)==0.576)
ledger<-fread("docs/governance/audit_master_status_ledger_2026-08-09.csv")
registry<-fread("docs/governance/version_registry.csv")
add("ledger 292 unique IDs and registry unique IDs",nrow(ledger)==292L&&!anyDuplicated(ledger$task_id)&&!anyDuplicated(registry$version_id))
ids<-c("SEC-18","RAT-10","PEER-07","LAD-02","LAD-03","LAD-04")
add("broader review statuses preserved",identical(ledger[match(ids,task_id),current_status],
  c("decision_pending","decision_pending","evidence_complete_decision_pending",rep("decision_pending",3))))
note<-"docs/governance/P15_LADDER_REVIEW_BRIEF_2026-09-06.md"
body<-paste(readLines(note,warn=FALSE),collapse="\n")
links<-regmatches(body,gregexpr("\\]\\(([^)]+)\\)",body,perl=TRUE))[[1]]
links<-sub("^\\]\\(","",sub("\\)$","",links))
local<-links[!grepl("^https?://",links)]
add("brief local evidence links resolve",all(file.exists(file.path(dirname(note),sub("#.*$","",local)))))
checks[,`:=`(first_run=args[1],repeat_run=args[2])]
stopifnot(all(checks$passed))
receipt<-"docs/governance/p15_ladder_review_verification_20260906.csv"
fwrite(checks,receipt)
cat(nrow(checks),"checks passed; receipt:",receipt,"\n")
