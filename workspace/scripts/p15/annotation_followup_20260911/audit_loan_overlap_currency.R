source("R/p15_current_inputs.R")
#!/usr/bin/env Rscript
source("scripts/p15/activate_p15_environment.R")
suppressPackageStartupMessages({library(data.table);library(jsonlite);library(digest)})
args<-commandArgs(TRUE);out<-if(length(args))args[1]else file.path("data-derived",paste0("p15_source_overlap_currency_",format(Sys.time(),"%Y%m%d_%H%M%S")))
stopifnot(!dir.exists(out));dir.create(out,recursive=TRUE)
source_path<-file.path(Sys.getenv("P15_LOAN_BASE", p15_current_input("loan_comparisons")),"loan_valuations.csv")
x<-fread(source_path,na.strings="",colClasses=c(currency="character",loan_id="character",source_loan_id="character"))
x[,currency_code:=fifelse(is.na(currency)|!nzchar(currency),"unknown",currency)]
# Local status is explicit for the only local-currency combinations in these
# matched samples. Other assigned categories below use distinct currencies and
# recipient countries; unknowns are never inferred from dollar-valued amounts.
x[,currency_category:=fcase(currency_code=="unknown","unknown",currency_code=="USD","USD",
 currency_code=="EUR","EUR",currency_code=="XDR","SDR",
 (iso3=="PAK"&currency_code=="PKR")|(iso3=="GAB"&currency_code=="XAF")|
 (iso3%in%c("BEN","BFA","CIV","GNB","MLI","NER","SEN","TGO")&currency_code=="XOF"),"local_currency",
 currency_code%in%c("JPY","GBP","CAD","CNY","KWD"),"other_foreign",default="not_classified")]
views<-rbindlist(lapply(c("all_selected","non_peer","primary_ids"),function(v){
 z<-x[matched&main_cohort&(v=="all_selected"|(v=="non_peer"&non_peer)|(v=="primary_ids"&primary_ids))];z[,benchmark_view:=v];z}))
currency_summary<-views[,.(records=.N,countries=uniqueN(iso3),amount_usd=sum(amount_usd,na.rm=TRUE)),by=.(dataset,benchmark_view,currency_category,currency_code)]
currency_by_creditor<-views[,.(records=.N),by=.(dataset,benchmark_view,creditor,currency_category,currency_code)]
source_family<-views[dataset=="add",.(records=.N,countries=uniqueN(iso3)),by=.(benchmark_view,original_source)]
local_records<-views[benchmark_view=="non_peer"&currency_category%in%c("local_currency","not_classified"),
 .(dataset,loan_id,iso3,commitment_year,creditor,currency,currency_category,currency_basis,raw_currency_field)]
mpg_years<-x[dataset=="mpg",unique(commitment_year)]
hard_overlap<-data.table(pair=c("MPG_AidData_current_application","MPG_ADD_current_application","AidData_ADD_current_application"),
 hard_identity_overlap=c(0L,0L,NA_integer_),basis=c("disjoint_commitment_years_2012_2014_vs_2015_2023",
 "disjoint_commitment_years_2012_2014_vs_2015_2024","ADD_does_not_publish_AidData_original_record_identifiers"))
a<-x[dataset=="add"&original_source=="AidData",.(add_loan_id=loan_id,iso3,commitment_year,currency,
 amount_original,interest_add=interest_rate_pct,maturity_add=maturity_years,grace_add=grace_years,
 date_add=as.character(source_issue_date),add_matched=matched,add_main=main_cohort,add_nonpeer=non_peer)]
b<-x[dataset=="aiddata",.(aiddata_loan_id=loan_id,iso3,commitment_year,currency,
 amount_original,interest_aid=interest_rate_pct,maturity_aid=maturity_years,grace_aid=grace_years,
 date_aid=as.character(as.Date(source_commitment_date,"%m/%d/%Y")),aiddata_record_id=source_record_id,
 aiddata_event_id=source_event_id,aid_matched=matched,aid_main=main_cohort,aid_nonpeer=non_peer)]
a[,amount_key:=round(amount_original)];b[,amount_key:=round(amount_original)]
candidates<-merge(a,b,by=c("iso3","commitment_year","currency","amount_key"),allow.cartesian=TRUE)
candidates<-candidates[is.finite(amount_key)&amount_key>0]
candidates[,`:=`(month_agrees=substr(date_add,1,7)==substr(date_aid,1,7),
 interest_agrees=is.finite(interest_add)&is.finite(interest_aid)&abs(interest_add-interest_aid)<1e-6,
 maturity_agrees=is.finite(maturity_add)&is.finite(maturity_aid)&abs(maturity_add-maturity_aid)<0.05,
 grace_agrees=is.finite(grace_add)&is.finite(grace_aid)&abs(grace_add-grace_aid)<0.05)]
candidates[,candidate_count_add:=uniqueN(aiddata_loan_id),by=add_loan_id]
candidates[,candidate_count_aid:=uniqueN(add_loan_id),by=aiddata_loan_id]
candidates[,match_class:=fcase(month_agrees&interest_agrees&maturity_agrees&grace_agrees&candidate_count_add==1L&candidate_count_aid==1L,
 "probable_same_loan_unique_country_year_currency_amount_month_terms",month_agrees,"possible_overlap_country_year_currency_amount_month",
 default="candidate_only_country_year_currency_amount")]
candidates[,both_main_nonpeer:=add_matched&add_main&add_nonpeer&aid_matched&aid_main&aid_nonpeer]
overlap_summary<-candidates[,.(candidate_pairs=.N,add_records=uniqueN(add_loan_id),aiddata_records=uniqueN(aiddata_loan_id),
 aiddata_events=uniqueN(aiddata_event_id)),by=.(match_class,both_main_nonpeer)]
tables<-list(currency_summary=currency_summary,currency_by_creditor=currency_by_creditor,add_source_family=source_family,
 local_currency_records=local_records,hard_identity_overlap=hard_overlap,aiddata_add_candidates=candidates,aiddata_add_overlap_summary=overlap_summary)
for(nm in names(tables))fwrite(tables[[nm]],file.path(out,paste0(nm,".csv")))
checks<-data.table(check=c("nonpeer_counts_reproduced","all_main_nonpeer_currencies_classified","MPG_AidData_years_disjoint","MPG_ADD_years_disjoint","probable_not_claimed_ID_match"),
 passed=c(identical(sort(views[benchmark_view=="non_peer",.N,by=dataset]$N),sort(c(462L,164L,1624L))),
 !any(views[benchmark_view=="non_peer",currency_category]=="not_classified"),
 length(intersect(mpg_years,x[dataset=="aiddata",unique(commitment_year)]))==0L,
 length(intersect(mpg_years,x[dataset=="add",unique(commitment_year)]))==0L,all(grepl("probable|possible|candidate",candidates$match_class))))
fwrite(checks,file.path(out,"checks.csv"));stopifnot(all(checks$passed))
inputs<-c(source_path,"sources/literature_review/loan_extension_feasibility_20260910/other_data/add_2025_paper.txt",
 "docs/thesis_design/feedback_2026-09-11/CRS_SOURCE_ANALYSIS_DESIGN.md","renv.lock")
manifest<-function(ps)data.table(path=ps,sha256=vapply(ps,function(f)digest(file=f,algo="sha256"),character(1)),bytes=file.info(ps)$size)
fwrite(manifest(inputs),file.path(out,"input_manifest.csv"))
fwrite(manifest("scripts/p15/annotation_followup_20260911/audit_loan_overlap_currency.R"),file.path(out,"code_manifest.csv"))
write_json(list(build_id=basename(out),schema_id="SCHEMA-P15-SOURCE-OVERLAP-CURRENCY-V1",
 estimator_id="EST-DESCRIPTIVE-SOURCE-AUDIT-V1",admissibility_id="ADM-EXISTING-LOAN-MAIN-VIEWS-UNCHANGED",
 selection_id="SEL-EXISTING-LOAN-VIEWS-UNCHANGED",source_package_ids="P15-LOAN-COMPARISONS-20260910-V5",
 lifecycle_status="diagnostic",release_state="private_research"),file.path(out,"version_bundle.json"),auto_unbox=TRUE,pretty=TRUE)
writeLines(capture.output(sessionInfo()),file.path(out,"environment.txt"))
fwrite(manifest(list.files(out,full.names=TRUE)),file.path(out,"output_manifest.csv"))
print(currency_summary[benchmark_view=="non_peer"]);print(overlap_summary)
