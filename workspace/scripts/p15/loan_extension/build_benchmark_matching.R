#!/usr/bin/env Rscript
suppressPackageStartupMessages({library(data.table); library(jsonlite); library(digest)})
source("scripts/p15/loan_extension/benchmark_matching.R")
source("R/research_governance.R")
args <- commandArgs(TRUE)
out <- if(length(args)) args[1] else "data-derived/p15_loan_extension_feasibility_20260910_v1"
if(dir.exists(out)) stop("Fresh output directory required")
pvr_assert_write_allowed(out,"diagnostic")
started <- format(Sys.time(),"%Y-%m-%dT%H:%M:%S%z")
base <- Sys.getenv("P15_ANALYSIS_BASE")
if(!nzchar(base)) base <- fromJSON("data-derived/p15_master/current_run.json")$candidate
archive <- "sources/literature_review/loan_extension_feasibility_20260910/mpg"
csv <- file.path(archive,"extracted/Full Dataset.csv")
archive_manifest <- file.path(archive,"extracted_manifest.csv")
analysis_code <- file.path(archive,"extracted/dofiles/Chinese_World_Bank_Lending_Terms_Analysis_20200305_FINAL.do")
creation_code <- file.path(archive,"extracted/dofiles/Chinese_World_Bank_Lending_Terms_Database_Creation_20200228_FINAL.do")
country_json <- "data-raw/world_bank_countries.json"
design <- "docs/thesis_design/loan_extension_feasibility_2026-09-10/BENCHMARK_MATCHING_DESIGN.md"
use_note <- "docs/governance/P15_YOY_AND_EXTREME_CASE_USE_2026-09-08.md"
paths <- c(file.path(base,c("selected_reference.csv","tier_eligibility.csv","core_evidence.csv","output_manifest.csv")),
  csv,archive_manifest,analysis_code,creation_code,country_json,design,use_note,"renv.lock")
manifest <- fread(file.path(base,"output_manifest.csv"))
for(f in paths[dirname(paths)==base & basename(paths)!="output_manifest.csv"]){
  z<-manifest[basename(artifact_path)==basename(f)]
  stopifnot(nrow(z)==1L,digest(file=f,algo="sha256")==z$sha256)
}
archived <- fread(archive_manifest)
for(f in c(csv,analysis_code,creation_code)) {
  z<-archived[path==f]
  stopifnot(nrow(z)==1L,digest(file=f,algo="sha256")==z$sha256)
}
selected<-fread(file.path(base,"selected_reference.csv"))
tiers<-fread(file.path(base,"tier_eligibility.csv"))
core<-fread(file.path(base,"core_evidence.csv"))
reference<-p15_loan_benchmark_reference(selected,tiers,core)
ref<-reference$country_year; tier_ref<-reference$tiers
stopifnot(nrow(ref)==2743L,nrow(tier_ref)==13715L)

# Empty cells are missing; literal ISO2 NA is Namibia and must survive parsing.
mpg<-fread(csv,na.strings="",colClasses=c(id="character",countrycode="character"))
stopifnot(nrow(mpg)==7481L,!anyDuplicated(mpg$id))
wb<-fromJSON(country_json)[[2]]
country_map<-data.table(iso2=wb$iso2Code,iso3=wb$id,wb_country=wb$name,is_country=wb$region$id!="NA")
country_map<-country_map[is_country==TRUE]
stopifnot(!anyDuplicated(country_map$iso2))
index<-match(mpg$countrycode,country_map$iso2)
mpg[,`:=`(loan_id=id,iso3=country_map$iso3[index],commitment_year=year,
  source_csv_row=seq_len(.N)+1L,loan_currency=NA_character_,
  source_country_mapping_state=ifelse(is.na(index),"unmapped_ISO2_in_saved_WB_country_metadata","exact_ISO2_match"),
  in_paper_sample=country!="China",paper_filter="country != China; analysis do-file first drop command after loading Full Dataset",
  source_record_locator=paste0("Full Dataset.csv#id=",id),
  original_interest_decimal=interest,official_rate_pct=interest*100,
  source_year_definition=ifelse(source=="China","agreement_signing_year","agreement_signing_year_or_board_approval_fallback"),
  source_maturity_definition=ifelse(source=="World Bank","first_to_last_repayment_interval_weeks_divided_by_52",
    "reported_China_maturity_requires_source_definition_alignment"))]
mpg[,creditor_group:=fcase(source=="China","China",lender_d=="World Bank: IBRD","IBRD",
  lender_d=="World Bank: IDA","IDA",default="other_source_group")]
mpg[,`:=`(is_loan=type=="Loan",numeric_term_triplet_complete=is.finite(interest)&is.finite(maturity)&is.finite(grace))]
mpg[,numeric_terms_nonnegative_positive_duration:=numeric_term_triplet_complete & interest>=0 & maturity>0 & grace>=0]
mpg[,reported_grace_exceeds_reported_duration:=numeric_term_triplet_complete & grace>maturity]
mpg[,reported_grace_equals_reported_duration:=numeric_term_triplet_complete & grace==maturity]
mpg[,numbers_fit_p15_schedule_shape:=numeric_terms_nonnegative_positive_duration & grace<=maturity]
mpg[,repayment_definition_match_verified:=FALSE]
matched<-p15_match_external_loans(mpg,reference)
loans<-matched$selected; loan_tiers<-matched$tiers
stopifnot(sum(loans$in_paper_sample)==7312L,all(loans[country=="Namibia",iso3]=="NAM"))
stopifnot(!anyDuplicated(loans$loan_id),nrow(loans)==7481L)

coverage<-list(); k<-0L
for(scope in c("all_project_countries","historical_LMIC")) {
  r<-if(scope=="historical_LMIC")ref[historical_lmic_reporting_scope==TRUE] else ref
  for(group in c("period","analysis_year")) {
    z<-r[,.(country_years=.N,countries=uniqueN(iso3),selected_available=sum(benchmark_selected_available),
      selected_ordinary_usable=sum(benchmark_selected_ordinary_usable),
      observed_primary_or_secondary=sum(any_observed_dataset_eligible),
      observed_ordinary_usable=sum(any_observed_ordinary_usable),
      primary_or_ids_ordinary_usable=sum(primary_or_ids_ordinary_usable),
      status_review_absent=sum(individual_status_review_absent)),by=c(group)]
    setnames(z,group,"period_or_year");z[,`:=`(scope=scope,grouping=group,period_or_year=as.character(period_or_year))]
    k<-k+1L;coverage[[k]]<-z
  }
}
coverage<-rbindlist(coverage,fill=TRUE)
tier_coverage<-rbindlist(lapply(c("all_project_countries","historical_LMIC"),function(scope){
  z<-if(scope=="historical_LMIC")tier_ref[historical_lmic_reporting_scope==TRUE] else tier_ref
  z[,.(country_years=.N,dataset_eligible=sum(dataset_tier_eligible),ordinary_usable=sum(ordinary_cost_tier_usable),
    eligible_countries=uniqueN(iso3[dataset_tier_eligible])),by=.(period,tier)][,scope:=scope]
}))
selected_coverage<-rbindlist(lapply(c("all_project_countries","historical_LMIC"),function(scope){
  z<-if(scope=="historical_LMIC")ref[historical_lmic_reporting_scope==TRUE] else ref
  z[,.(country_years=.N,countries=uniqueN(iso3)),by=.(period,benchmark_selected_tier)][,scope:=scope]
}))
match_summary<-list(); tier_summary<-list(); j<-0L
for(sample in c("archive_all","paper_sample")) for(scope in c("all_external_countries","historical_LMIC")) {
  q<-loans[commitment_year%in%2012:2014]
  if(sample=="paper_sample")q<-q[in_paper_sample==TRUE]
  if(scope=="historical_LMIC")q<-q[historical_lmic_reporting_scope==TRUE]
  sets<-list(all_records=rep(TRUE,nrow(q)),loans=q$is_loan,
    loans_complete_numeric_terms=q$is_loan & q$numeric_term_triplet_complete,
    loans_nonnegative_terms_positive_duration=q$is_loan & q$numeric_terms_nonnegative_positive_duration,
    loans_numbers_fit_schedule_shape=q$is_loan & q$numbers_fit_p15_schedule_shape)
  for(population in names(sets)) {
    z<-q[sets[[population]]]
    s<-z[,.(records=.N,countries=uniqueN(iso3,na.rm=TRUE),
      country_years=uniqueN(paste(iso3,commitment_year)),
      selected_available=sum(benchmark_selected_available,na.rm=TRUE),
      selected_ordinary_usable=sum(benchmark_selected_ordinary_usable,na.rm=TRUE),
      observed_primary_or_secondary=sum(any_observed_dataset_eligible,na.rm=TRUE),
      observed_ordinary_usable=sum(any_observed_ordinary_usable,na.rm=TRUE),
      primary_or_ids_ordinary_usable=sum(primary_or_ids_ordinary_usable,na.rm=TRUE),
      selected_primary_or_ids=sum(benchmark_selected_tier%in%c("primary","ids")),
      selected_rating_implied=sum(benchmark_selected_tier=="moodys",na.rm=TRUE),
      selected_peer=sum(benchmark_selected_tier=="peer",na.rm=TRUE),
      reported_grace_exceeds_duration=sum(reported_grace_exceeds_reported_duration)),by=.(source,creditor_group)]
    s[,`:=`(sample=sample,scope=scope,population=population)]
    j<-j+1L;match_summary[[j]]<-s
    t<-loan_tiers[loan_id%in%z$loan_id & !is.na(tier),.(records=.N,
      eligible_matches=sum(dataset_tier_eligible),ordinary_usable_matches=sum(ordinary_cost_tier_usable),
      matched_countries=uniqueN(iso3[ordinary_cost_tier_usable])),by=.(source,creditor_group,tier)]
    t[,`:=`(sample=sample,scope=scope,population=population)];tier_summary[[j]]<-t
  }
}
checks<-data.table(check=c("candidate_country_years_and_tiers_preserved","unique_original_7481_ids_preserved",
  "paper_filter_reproduces_7312_records","Namibia_ISO2_NA_is_not_missing","all_source_rows_retained_in_selected_join",
  "all_source_rows_retained_in_all_tier_join","no_grace_duration_record_dropped",
  "no_loan_currency_inferred_from_USD_amounts"),passed=c(TRUE,TRUE,TRUE,TRUE,TRUE,
    setequal(loan_tiers$loan_id,mpg$loan_id),all(mpg$loan_id[mpg$reported_grace_exceeds_reported_duration]%in%loans$loan_id),
    all(is.na(loans$loan_currency))))
# Bounded synthetic checks exercise missing keys, calendar limits, holds and currency states.
synthetic<-data.table(loan_id=c("year_old","country_absent","bad_year","currency_USD","held_secondary"),
  iso3=c("UZB","ZZZ","UZB","UZB","LBN"),commitment_year=c(2001,2024,2013.5,2024,2022),
  loan_currency=c(NA,NA,NA,"USD","USD"))
sm<-p15_match_external_loans(synthetic,reference)$selected
stopifnot(sm$match_state[1]=="outside_benchmark_period",sm$match_state[2]=="country_not_in_project_reference",
  sm$match_state[3]=="invalid_or_missing_year",sm$loan_currency_match_state[4]=="USD_loan_and_USD_benchmark_conditional_match",
  sm$match_state[5]=="matched_selected_secondary_ordinary_cost_hold",all(checks$passed))
checks<-rbind(checks,data.table(check="unmatched_period_key_currency_and_hold_examples",passed=TRUE))
tables<-list(country_year_reference=ref,tier_reference=tier_ref,benchmark_coverage=coverage,
  benchmark_tier_coverage=tier_coverage,benchmark_selected_tier_coverage=selected_coverage,
  benchmark_status_coverage=ref[,.N,by=.(period,approved_status_rule_class,status_review_coverage,missing_result_state)],
  iso2_iso3_mapping=unique(loans[,.(country,countrycode,iso3,source_country_mapping_state)]),
  mpg_normalized_source_rows=mpg,mpg_selected_matches=loans,mpg_all_tier_matches=loan_tiers,
  mpg_2012_2014_match_summary=rbindlist(match_summary),mpg_2012_2014_tier_summary=rbindlist(tier_summary),
  mpg_match_states=loans[,.N,by=.(source,in_paper_sample,type,commitment_year,match_state)],
  mpg_paper_period_inventory=loans[in_paper_sample==TRUE,.(records=.N,loans=sum(is_loan),
    complete_numeric_loan_terms=sum(is_loan & numeric_term_triplet_complete),
    nonnegative_terms_positive_duration=sum(is_loan & numeric_terms_nonnegative_positive_duration),
    numbers_fit_schedule_shape=sum(is_loan & numbers_fit_p15_schedule_shape),
    grace_exceeds_reported_duration=sum(is_loan & reported_grace_exceeds_reported_duration)),
    by=.(source,creditor_group,period=ifelse(commitment_year<2012,"2000_2011","2012_2014"))],
  synthetic_matching_checks=sm,checks=checks)
dir.create(out,recursive=TRUE)
bundle<-list(build_id=basename(out),schema_id="SCHEMA-P15-EXTERNAL-LOAN-BENCHMARK-MATCH-V1",
  estimator_id="EST-P15-NO-VALUATION-KEYED-LOAN-MATCH-V1",admissibility_id="ADM-P15-SOURCE-CLOSURE-V1",
  selection_id="SEL-P15-REFERENCE-REGION-CORRECTED-V1",
  source_package_ids="SRC-MPG-2020-REPLICATION-ZIP-20260910;SRC-P15-RAW-CLOSURE-20260906;SRC-P15-SOURCE-CLOSURE-20260907;SRC-WB-COUNTRIES-LOCAL-PEER-20260909")
for(name in names(tables)){
  z<-copy(tables[[name]]);for(field in names(bundle))set(z,j=field,value=bundle[[field]])
  z[,release_state:="private_diagnostic_matching_only"]
  fwrite(z,file.path(out,paste0(name,".csv")),na="")
}
code<-c("scripts/p15/loan_extension/build_benchmark_matching.R","scripts/p15/loan_extension/benchmark_matching.R","R/research_governance.R")
manifest_rows<-function(ps,role)do.call(pvr_manifest_rows,c(list(paths=ps,artifact_role=role),bundle))
fwrite(manifest_rows(paths,"input"),file.path(out,"input_manifest.csv"))
fwrite(manifest_rows(code,"script"),file.path(out,"code_manifest.csv"))
write_json(c(bundle,list(run_started_at=started,run_finished_at=format(Sys.time(),"%Y-%m-%dT%H:%M:%S%z"),
  run_status="success",exception_ids="none",r_version=R.version.string,platform=R.version$platform,
  timezone=Sys.timezone(),locale=Sys.getlocale(),renv_lock_sha256=digest(file="renv.lock",algo="sha256"),
  valuation_performed=FALSE)),file.path(out,"run_environment.json"),auto_unbox=TRUE,pretty=TRUE)
capture.output(sessionInfo(),file=file.path(out,"environment.txt"))
fwrite(manifest_rows(list.files(out,full.names=TRUE),"output"),file.path(out,"output_manifest.csv"))
print(tables$mpg_2012_2014_match_summary[sample=="paper_sample" & scope=="all_external_countries" &
  population%in%c("loans","loans_complete_numeric_terms")])
cat("Saved",out,"\n")
