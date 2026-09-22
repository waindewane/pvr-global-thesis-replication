#!/usr/bin/env Rscript
# Source feasibility and country-year availability only. No loan revaluation.
source("scripts/p15/activate_p15_environment.R")
suppressPackageStartupMessages({library(data.table);library(readxl);library(jsonlite);library(digest)})
out <- "data-derived/p15_later_loan_sources_inspection_20260910_v1"
stopifnot(!dir.exists(out));dir.create(out,recursive=TRUE)
base <- "sources/literature_review/loan_extension_feasibility_20260910"
clg_path <- file.path(base,"aiddata/AidDatas_CLG_LMIC_Dataset_v1.0.xlsx")
add_path <- file.path(base,"other_data/add_2026may_a.xlsx")
ref_path <- "data-derived/p15_loan_extension_feasibility_20260910_v1/country_year_reference.csv"
ref <- fread(ref_path,na.strings="")
ref <- ref[,.(iso3,analysis_year,benchmark_selected_tier,benchmark_selected_rate_pct,
  benchmark_selected_ordinary_usable,historical_lmic_reporting_scope,
  any_observed_ordinary_usable,primary_or_ids_ordinary_usable)]
stopifnot(!anyDuplicated(ref[,.(iso3,analysis_year)]))
num <- function(x)suppressWarnings(as.numeric(x))
clg <- as.data.table(read_excel(clg_path,sheet="CLG-LMIC 1.0_Records",col_types="text"))
clg[,source_row:=.I]
clg[,`:=`(year=num(Commitment_Year),interest=num(Interest_at_T0),maturity=num(Maturity),grace=num(Grace_Period),amount=num(Amount_Original_Currency))]
clg[,`:=`(recommended=Recommended_for_Aggregates=="Yes" & Flow_Type=="Loan",
  central=Level_of_Public_Liability %in% c("Central government debt","Central government-guaranteed debt"),
  ppg=PPG_Debt_Status=="PPG Debt",usd=Original_Currency=="USD",fixed=Interest_Rate_Type=="Fixed Interest Rate")]
clg[,terms:=is.finite(interest)&interest>=0&is.finite(maturity)&maturity>0&is.finite(grace)&grace>=0&grace<=maturity]
clg[,complete:=terms & is.finite(amount)&amount>0 & !is.na(Original_Currency)&nzchar(Original_Currency)]
clg[,`:=`(iso3=Country_of_Activity_ISO3,analysis_year=as.integer(year))]
# Activity country is a screening key. Cross-border borrower/guarantor cases require review.
clg <- merge(clg,ref,by=c("iso3","analysis_year"),all.x=TRUE,sort=FALSE)
clg[,borrower_activity_country_agree:=!is.na(DRA_Country_of_Inc_ISO3)&DRA_Country_of_Inc_ISO3==iso3]
clg_summaries<-list()
for(bounds in list(c(2012,2014),c(2015,2023),c(2018,2023))){
 for(view in c("all_recommended","ppg","central_or_guaranteed","central_usd","central_usd_fixed")){
  z<-clg[recommended==TRUE & year>=bounds[1]&year<=bounds[2]]
  if(view=="ppg")z<-z[ppg==TRUE]
  if(view %in% c("central_or_guaranteed","central_usd","central_usd_fixed"))z<-z[central==TRUE]
  if(view %in% c("central_usd","central_usd_fixed"))z<-z[usd==TRUE]
  if(view=="central_usd_fixed")z<-z[fixed==TRUE]
  q<-z[complete==TRUE]
  clg_summaries[[length(clg_summaries)+1]]<-q[,.(period=paste(bounds,collapse="_"),view=view,all_records=nrow(z),complete_records=.N,loan_events=uniqueN(Loan_Event_ID),countries=uniqueN(iso3),country_years=uniqueN(paste(iso3,year)),selected_matches=sum(benchmark_selected_ordinary_usable,na.rm=TRUE),observed_primary_secondary_available=sum(any_observed_ordinary_usable,na.rm=TRUE),primary_ids_available=sum(primary_or_ids_ordinary_usable,na.rm=TRUE),historical_lmic_matches=sum(benchmark_selected_ordinary_usable & historical_lmic_reporting_scope,na.rm=TRUE),borrower_activity_country_agree=sum(borrower_activity_country_agree),primary=sum(benchmark_selected_tier=="primary",na.rm=TRUE),ids=sum(benchmark_selected_tier=="ids",na.rm=TRUE),secondary=sum(benchmark_selected_tier=="secondary",na.rm=TRUE),rating_implied=sum(benchmark_selected_tier=="moodys",na.rm=TRUE),peer=sum(benchmark_selected_tier=="peer",na.rm=TRUE))]
 }
}
meta<-list(build_id=basename(out),schema_id="SCHEMA-P15-LATER-LOAN-SOURCE-FEASIBILITY-V1",estimator_id="EST-P15-LOAN-SOURCE-COUNT-AND-KEY-JOIN-V1",admissibility_id="ADM-P15-FEASIBILITY-NO-ACCEPTANCE-V1",selection_id="SEL-P15-REFERENCE-REGION-CORRECTED-V1",lifecycle_status="diagnostic",release_state="private_source_feasibility_no_valuation")
write_table<-function(z,name,parents){
 z<-copy(z);for(nm in names(meta))set(z,j=nm,value=meta[[nm]])
 z[,lineage_parent_ids:=paste(parents,collapse=";")]
 fwrite(z,file.path(out,paste0(name,".csv")),na="")
}
clg_summary<-rbindlist(clg_summaries)
write_table(clg_summary,"clg_completeness_and_benchmark_matches",c(clg_path,ref_path))
write_table(clg[recommended==TRUE & year>=2012 & year<=2023,.(source_row,AidData_Record_ID,Loan_Event_ID,Loan_Event_Tranche,Parent_ID,iso3,year,DRA_Country_of_Inc_ISO3,borrower_activity_country_agree,Original_Currency,interest,maturity,grace,amount,complete,central,ppg,usd,fixed,benchmark_selected_tier,benchmark_selected_rate_pct,benchmark_selected_ordinary_usable,primary_or_ids_ordinary_usable,any_observed_ordinary_usable,historical_lmic_reporting_scope)],"clg_screened_records",c(clg_path,ref_path))
add<-as.data.table(read_excel(add_path,col_types="text"))
add[,`:=`(source_row=.I,year=num(year),interest=num(interest),maturity=num(maturity),grace=num(grace),amount_usd=num(Amount_musd))]
add[,`:=`(complete=is.finite(interest)&is.finite(maturity)&is.finite(grace),external_loan=grepl("Loan",instrument_type)&!grepl("Domestic",instrument_type))]
add[,`:=`(compatible=complete&interest>=0&maturity>0&grace>=0&grace<=maturity,currency_known=!is.na(Currency)&nzchar(Currency))]
add_summary<-add[year>=2015 & year<=2024 & external_loan==TRUE,.(records=.N,complete_terms=sum(complete),nonnegative_horizon_compatible=sum(compatible),complete_terms_currency=sum(complete&currency_known),interest_present=sum(is.finite(interest)),currency_present=sum(currency_known),grace_gt_maturity=sum(complete&grace>maturity),countries=uniqueN(ISO3)),by=.(CreditorGroup,CreditorName_short)]
write_table(add_summary,"add_later_creditor_summary",add_path)
write_table(add[year>=2015&year<=2024&external_loan==TRUE,.(records=.N,complete_terms=sum(complete),nonnegative_horizon_compatible=sum(compatible),complete_terms_currency=sum(complete&currency_known)),by=CreditorGroup],"add_later_group_summary",add_path)
write_table(add[year>=2015&year<=2024&external_loan==TRUE,.(source_row,ISO3,Country,BorrowerType,BorrowerAgency,CreditorName_short,CreditorGroup,year,Currency,instrument_type,interest,maturity,grace,amount_usd,complete,compatible,currency_known,source)],"add_later_source_records",add_path)
checks<-data.table(check=c("clg_unique_record_ids","clg_later_central_complete_630","clg_later_central_usd_fixed_215","add_rows_69849","add_later_ibrd_missing_interest_190","benchmark_reference_keys_unique","no_valuation_performed"),passed=c(!anyDuplicated(clg$AidData_Record_ID),clg_summary[period=="2015_2023"&view=="central_or_guaranteed",complete_records]==630,clg_summary[period=="2015_2023"&view=="central_usd_fixed",complete_records]==215,nrow(add)==69849,add_summary[CreditorName_short=="WB-IBRD",records]==190 && add_summary[CreditorName_short=="WB-IBRD",interest_present]==0,TRUE,TRUE))
fwrite(checks,file.path(out,"checks.csv"));stopifnot(all(checks$passed))
manifest<-function(paths)data.table(path=paths,sha256=vapply(paths,function(p)digest(file=p,algo="sha256"),character(1)),bytes=file.info(paths)$size)
fwrite(manifest(c(clg_path,add_path,ref_path)),file.path(out,"input_manifest.csv"))
fwrite(manifest(c("scripts/p15/loan_extension/inspect_later_sources.R","scripts/p15/activate_p15_environment.R","renv.lock")),file.path(out,"code_manifest.csv"))
write_json(c(meta,list(source_snapshot_ids=c("loan_extension_20260910_aiddata_clg_lmic_v1_zip","LEF-ADD-2026MAY-XLSX-20260910"),limitations="Record and event units differ; activity-country matching is preliminary; complete numbers do not verify schedules or currency conversion; no market revaluation.")),file.path(out,"version_bundle.json"),auto_unbox=TRUE,pretty=TRUE)
writeLines(capture.output(sessionInfo()),file.path(out,"environment.txt"))
fwrite(manifest(list.files(out,full.names=TRUE)),file.path(out,"output_manifest.csv"))
cat("Completed later-source inspection:",out,";",nrow(checks),"checks passed\n")
