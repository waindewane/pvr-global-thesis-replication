suppressPackageStartupMessages({library(data.table);library(jsonlite);library(digest);library(readxl)})
out <- 'docs/thesis_design/sections/official_loan_data_and_valuation/content_20260921'
pointer <- 'data-derived/p15_master/current_run.json'
j <- fromJSON(pointer);inputs <- c(pointer,file.path(out,'verify_evidence.R'))
read_input <- function(path){inputs<<-c(inputs,path);fread(path,na.strings='')}
stage <- function(k,f)read_input(file.path(j$stages[[k]]$dir,paste0(f,'.csv')))
checks <- list()
check <- function(name,value){checks[[length(checks)+1L]]<<-data.table(check=name,passed=isTRUE(value));stopifnot(isTRUE(value))}
crs <- stage('crs_application','loan_valuations')
rows <- stage('crs_application','row_eligibility')
loans <- stage('loan_comparisons','loan_valuations')
paired <- stage('loan_comparisons','paired_comparisons')
agg <- stage('pv_interpretation','analysis_rows')
crsp <- stage('crs_application','paired_comparisons')
cf <- stage('crs_application','cash_flows')
check('CRS_4795_eligible_rows_3607_financing_records',rows[exclusion_reason=='eligible_terms',.N]==4795L && nrow(crs)==3607L)
crsflow <- data.table(step=c('Eligible source rows','Consolidated financing records','Usable selected benchmark','Excluding peer-only','Modern non-peer'),
  records=c(4795,nrow(crs),crs[matched==TRUE,.N],crs[matched==TRUE&non_peer==TRUE,.N],crs[matched==TRUE&non_peer==TRUE&commitment_year>=2018,.N]))
fwrite(crsflow,file.path(out,'crs_sample_flow.csv'))
fwrite(rows[,.(records=.N),by=exclusion_reason],file.path(out,'crs_sequential_exclusions.csv'))
summ <- function(d,unit) data.table(observations=nrow(d),countries=uniqueN(d$iso3),first_year=min(d$commitment_year),last_year=max(d$commitment_year),unit=unit)
tables<-list(cbind(application='CRS',summ(crs[matched==TRUE&non_peer==TRUE],'financing record')),
  cbind(application='CRS modern',summ(crs[matched==TRUE&non_peer==TRUE&commitment_year>=2018],'financing record')))
for(k in c('mpg','aiddata','add')){
 d<-loans[dataset==k&main_cohort==TRUE&matched==TRUE&non_peer==TRUE]
 tables[[length(tables)+1L]]<-cbind(application=k,summ(d,'source loan record'))
 for(ref in c('standardized_DAC_category_rule','modern_DAC_common_reference','fixed5')){
  z<-paired[dataset==k&main_cohort==TRUE&benchmark_view=='non_peer'&reference==ref]
  if(nrow(z))tables[[length(tables)+1L]]<-cbind(application=paste(k,ref),summ(z,'source loan record'))
 }
}
fwrite(rbindlist(tables),file.path(out,'application_samples.csv'))
fwrite(agg[,.(observations=.N,countries=uniqueN(iso3)),by=.(peer_excluded=selected_tier!='peer')],file.path(out,'aggregate_modern_samples.csv'))
fwrite(crs[matched==TRUE&non_peer==TRUE,.(records=.N),by=.(borrower_scope,repayment_type,frequency)],file.path(out,'crs_scope_and_schedule.csv'))
fwrite(loans[matched==TRUE&non_peer==TRUE&main_cohort==TRUE,.(records=.N),by=.(dataset,interest_type,currency)],file.path(out,'loan_rate_and_currency.csv'))
check('MPG_reference_specific_denominators',paired[dataset=='mpg'&benchmark_view=='non_peer'&reference=='fixed5',.N]==462L && paired[dataset=='mpg'&benchmark_view=='non_peer'&reference=='standardized_DAC_category_rule',.N]==448L)
fwrite(loans[dataset=='mpg'&matched==TRUE&non_peer==TRUE&!is.finite(ge_standardized_category_pct),.(loan_id,iso3,commitment_year,creditor,dac_eligible,dac_group)],file.path(out,'mpg_category_reference_unavailable.csv'))
check('CRS_currency_unknown',all(is.na(crs$loan_currency)))
check('CRS_pairing_unique',!anyDuplicated(crsp[,.(loan_id,benchmark_view,reference)]))
f <- merge(cf,crs[,.(loan_id,benchmark_selected_rate_pct,standardized_reference_rate_pct,matched,ge_market_pct,ge_standardized_pct)],by='loan_id')
replay <- f[,.(principal=sum(principal),market=100-sum(payment/(1+benchmark_selected_rate_pct/100)^time_years),
  policy=100-sum(payment/(1+standardized_reference_rate_pct/100)^time_years)),by=loan_id]
replay<-merge(replay,crs[,.(loan_id,matched,ge_market_pct,ge_standardized_pct)],by='loan_id')
check('CRS_principal_repaid',all(abs(replay$principal-100)<1e-7))
check('CRS_PV_recomputed_from_saved_payments',replay[matched==TRUE,max(abs(market-ge_market_pct))]<1e-8 && max(abs(replay$policy-replay$ge_standardized_pct),na.rm=TRUE)<1e-8)
fwrite(replay,file.path(out,'crs_payment_replay.csv'))
inputs<-c(inputs,'scripts/p15/loan_extension/loan_valuation.R','scripts/p15/loan_extension/benchmark_matching.R',
 'scripts/p15/annotation_followup_20260911/crs_cashflows.R','scripts/p15/annotation_followup_20260911/build_crs_application.R')
source('scripts/p15/loan_extension/loan_valuation.R')
v<-loans[main_cohort==TRUE&matched==TRUE]
v[,replayed:=mapply(loan_ge,interest_rate_pct,maturity_years,first_principal_payment_years,benchmark_selected_rate_pct)]
check('All_main_loan_formula_results_reproduced',max(abs(v$replayed-v$ge_market_pct))<1e-8)
fwrite(v[,.(loan_id,dataset,replayed,ge_market_pct)],file.path(out,'loan_formula_replay.csv'))
for(z in list(c('source_overlap_currency','hard_identity_overlap'),c('source_overlap_currency','aiddata_add_overlap_summary'),
 c('crs_companions','hard_WB_overlap_summary'),c('loan_period_inference','primary_country_inference'),c('panel_disbursement','summary_overall'))){
 fwrite(stage(z[1],z[2]),file.path(out,paste0(z[2],'.csv')))
}
# Read original observations, not only normalized outputs. Bounded XLSX row reads preserve source headers.
read_excel_row <- function(path,sheet,row){
 inputs<<-c(inputs,path)
 header<-read_excel(path,sheet=sheet,n_max=0,.name_repair='minimal')
 as.data.table(read_excel(path,sheet=sheet,range=cell_rows(row),col_names=names(header),col_types='text',.name_repair='minimal'))
}
aidpath<-'sources/literature_review/loan_extension_feasibility_20260910/aiddata/AidDatas_CLG_LMIC_Dataset_v1.0.xlsx'
addpath<-'sources/literature_review/loan_extension_feasibility_20260910/other_data/add_2026may_a.xlsx'
a<-loans[dataset=='aiddata'&cohort=='aiddata_fixed_usd'&matched==TRUE&non_peer==TRUE][1]
ra<-read_excel_row(aidpath,'CLG-LMIC 1.0_Records',a$source_row)
check('AidData_original_coupon_and_total_maturity',abs(as.numeric(ra$Interest_at_T0)-a$interest_rate_pct)<1e-10 && abs(as.numeric(ra$Maturity)-a$maturity_years)<1e-10)
fwrite(ra[,.(AidData_Record_ID,Commitment_Year,Original_Currency,Interest_at_T0,Maturity,Grace_Period)],file.path(out,'raw_aiddata_example.csv'))
a<-loans[dataset=='add'&creditor=='WB-IDA'&matched==TRUE&non_peer==TRUE][1]
ra<-read_excel_row(addpath,'Sheet1',a$source_excel_row)
check('ADD_original_WB_span_plus_grace',abs(as.numeric(ra$maturity)+as.numeric(ra$grace)-a$maturity_years)<1e-8 && abs(as.numeric(ra$interest)-a$interest_rate_pct)<1e-8)
fwrite(ra[,.(ISO3,year,CreditorName_short,interest,maturity,grace,Currency)],file.path(out,'raw_add_example.csv'))
a<-loans[dataset=='mpg'&creditor=='IDA'&matched==TRUE&non_peer==TRUE][1]
raw<-read_input(a$raw_source_path);rawrow<-raw[`Credit Number`==a$source_loan_id]
date<-function(z)as.Date(z,'%m/%d/%Y')
check('MPG_original_WB_endpoints',nrow(rawrow)==1L && abs(as.numeric(date(rawrow[['Last Repayment Date']])-date(rawrow[['Effective Date (Most Recent)']]))/365.25-a$maturity_years)<1e-8)
fwrite(rawrow[,c('Credit Number','Effective Date (Most Recent)','First Repayment Date','Last Repayment Date','Service Charge Rate'),with=FALSE],file.path(out,'raw_mpg_example.csv'))
path<-'data-raw/oecd_crs/2026-05-25/crs_2024_dotstat_v20260408.zip';inputs<-c(inputs,path)
cols<-c('CrsID','DonorCode','AgencyCode','Interest1','Interest2','CommitmentDate','Repaydate1','Repaydate2','TypeRepayment','NumberRepayment','USD_Commitment','CurrencyCode')
raw<-fread(cmd=paste('unzip -p',shQuote(path)),select=cols,colClasses='character',na.strings='');raw[,source_row:=.I+1L]
a<-crs[report_year==2024&matched==TRUE&non_peer==TRUE][1];r<-raw[source_row==a$source_row]
check('CRS_original_numeric_rate_dates',nrow(r)==1L && abs(as.numeric(r$Interest1)/1000-a$interest_rate_pct)<1e-8 && r$Repaydate1==a$Repaydate1 && r$Repaydate2==a$Repaydate2)
fwrite(r,file.path(out,'raw_crs_example.csv'))
codebook<-'data-raw/oecd_crs/2026-05-25/dac_tables_crs_codebook_2025-08-19.xlsx';inputs<-c(inputs,codebook)
cb<-as.data.table(read_excel(codebook,sheet='CRS bulk data - codebook',col_names=FALSE,col_types='text',.name_repair='unique'))
fwrite(cb[apply(cb,1,function(r)any(grepl('Interest1|Interest2|TypeRepayment|NumberRepayment|CurrencyCode|Repaydate',r)))],file.path(out,'original_crs_codebook_extract.csv'))
inputs<-unique(inputs)
fwrite(rbindlist(checks),file.path(out,'verification.csv'))
fwrite(data.table(path=inputs,sha256=vapply(inputs,digest,character(1),file=TRUE,algo='sha256')),file.path(out,'input_manifest.csv'))
capture.output(sessionInfo(),file=file.path(out,'session_info.txt'))
outputs<-list.files(out,full.names=TRUE,pattern='\\.(csv|txt)$');outputs<-outputs[basename(outputs)!='output_manifest.csv']
fwrite(data.table(path=outputs,sha256=vapply(outputs,digest,character(1),file=TRUE,algo='sha256')),file.path(out,'output_manifest.csv'))
print(rbindlist(checks));print(crsflow);print(rbindlist(tables))
