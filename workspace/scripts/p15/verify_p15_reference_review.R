#!/usr/bin/env Rscript
suppressPackageStartupMessages({library(data.table);library(digest)})
args <- commandArgs(trailingOnly=TRUE)
if(!length(args))args<-c("data-derived/p15_reference_review_20260907_v1",getwd())
stopifnot(length(args)>=2)
build <- normalizePath(args[1]); project <- normalizePath(args[2])
repeat_build <- if(length(args)>=3)normalizePath(args[3]) else NULL
read <- function(name)fread(file.path(build,paste0(name,".csv.gz")))
resolve <- function(path)if(grepl("^/",path))path else file.path(project,path)
checks <- list()
check <- function(id,ok,detail="")checks[[length(checks)+1L]]<<-data.table(check_id=id,passed=isTRUE(ok),detail=detail)
for(m in c("input_manifest","script_manifest","output_manifest")) {
  manifest <- fread(file.path(build,paste0(m,".csv")))
  paths <- vapply(manifest$artifact_path,resolve,character(1))
  check(paste0(m,"_hashes"),all(file.exists(paths)) &&
    identical(unname(vapply(paths,digest,character(1),algo="sha256",file=TRUE)),manifest$sha256))
}
inputs <- fread(file.path(build,"input_manifest.csv"))
find_input <- function(pattern) {
  p<-inputs$artifact_path[grepl(pattern,inputs$artifact_path)];stopifnot(length(p)==1);resolve(p)
}
base <- fread(find_input("p15_reviewed_evidence.*p15_country_year_dataset.csv$"))
panel <- read("p15_country_year_dataset")
check("full_grid",nrow(panel)==2743 && uniqueN(panel,by=c("analysis_year","iso3"))==2743)
allowed <- c(grep("^secondary_",names(base),value=TRUE),"any_usd_observed_candidate","any_eur_observed_candidate",
  "usd_primary_secondary_gap_pp","comparison_timing_warning","fallback_audit_cohort",
  "build_id","schema_id","estimator_id","admissibility_id","selection_id","source_package_ids")
unchanged <- setdiff(names(base),allowed)
check("all_unaffected_core_columns",all(vapply(unchanged,function(n)identical(base[[n]],panel[[n]]),logical(1))),
  paste(length(unchanged),"columns including primary IDS rating peer and case restrictions"))
h <- fread(find_input("p15_secondary_history_long_2012_2024.csv.gz$"))
h[,date:=as.IDate(history_date)]
h[,offset:=as.integer(date-as.IDate(paste0(analysis_year,"-12-31")))]
h<-h[!is.na(offset)&offset>=-31&offset<=7]
h[,absolute_offset:=abs(offset)]
setorder(h,analysis_year,RIC,absolute_offset,date)
ids <- read("p15_secondary_identifier_audit")
d<-h[is.finite(yield_to_maturity)]
d<-d[, .SD[date==date[1]],by=.(analysis_year,RIC)]
d<-d[,.(expected_date=date[1],expected_yield=median(yield_to_maturity)),by=.(analysis_year,RIC)]
j<-merge(ids,d,by.x=c("analysis_year","ric"),by.y=c("analysis_year","RIC"),all.x=TRUE,sort=FALSE)
check("independent_direct_dates",identical(is.na(j$direct_quote_date),is.na(j$expected_date)) &&
  all(j$direct_quote_date==j$expected_date,na.rm=TRUE))
check("independent_direct_yields",identical(is.na(j$direct_yield_pct),is.na(j$expected_yield)) &&
  all(abs(j$direct_yield_pct-j$expected_yield)<1e-12,na.rm=TRUE))
p<-h[is.finite(mid_price)&is.finite(bid)&is.finite(ask),.(expected_date=date[1]),by=.(analysis_year,RIC)]
repairs<-read("p15_secondary_repair_identifier_audit")
j<-merge(repairs,p,by.x=c("analysis_year","ric"),by.y=c("analysis_year","RIC"),all.x=TRUE,sort=FALSE)
check("independent_price_dates",identical(is.na(j$selected_quote_date),is.na(j$expected_date)) &&
  all(j$selected_quote_date==j$expected_date,na.rm=TRUE))
peers<-read("p15_peer_options");members<-read("p15_peer_option_membership")
check("six_peer_methods",uniqueN(peers$peer_method)==6 && nrow(peers)==6*2743)
check("no_self_peers",all(members$target_iso3!=members$peer_iso3))
seeds<-read("p15_observed_anchors")[observed_market_branch=="observed_primary" & currency=="USD"]
check("membership_uses_own_year_observed_primary",all(paste(members$analysis_year,members$peer_iso3) %in% paste(seeds$analysis_year,seeds$iso3)))
medians<-members[used_for_estimate==TRUE & peer_method!="income_spread_first_min3",
  .(independent_median=median(peer_rate_pct)),by=.(analysis_year,iso3=target_iso3,peer_method)]
j<-merge(peers,medians,by=c("analysis_year","iso3","peer_method"))
check("independent_peer_medians",all(abs(j$peer_rate_pct-j$independent_median)<1e-12))
check("blocked_targets_not_used",!any(peers$last_tier_usable & peers$ordinary_fallback_selection_permitted %in% FALSE))
summary<-read("p15_peer_summary")
recount<-peers[,.(global_check=sum(last_tier_usable & global_pool),rating_check=sum(last_tier_usable & rating_proximity_used)),
  by=.(peer_method,historical_lmic_reporting_scope)]
j<-merge(summary,recount,by=c("peer_method","historical_lmic_reporting_scope"))
check("peer_summary_population_counts",all(j$last_tier_global==j$global_check & j$last_tier_rating_match==j$rating_check))
cases<-read("p15_ids_secondary_cases")
check("all_source_cases_contextualized",nrow(cases)>=30 && all(!is.na(cases$review_note)&nzchar(cases$review_note)))
if(!is.null(repeat_build)) {
  files<-list.files(build,pattern="\\.csv.gz$",full.names=FALSE)
  for(f in files) {
    a<-fread(file.path(build,f));b<-fread(file.path(repeat_build,f))
    a[,build_id:=NULL];b[,build_id:=NULL]
    check(paste0("repeat_",f),isTRUE(all.equal(a,b,tolerance=0,check.attributes=TRUE)))
  }
}
receipt<-rbindlist(checks)
fwrite(receipt,file.path(build,"independent_verification.csv"))
print(receipt)
if(!all(receipt$passed))stop("Reference review verification failed")
