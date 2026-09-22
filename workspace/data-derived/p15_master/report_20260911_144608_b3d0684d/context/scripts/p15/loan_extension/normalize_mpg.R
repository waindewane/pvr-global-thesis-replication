#!/usr/bin/env Rscript
# Normalization only; the common calculator owns borrower-rate valuation.
source("scripts/p15/activate_p15_environment.R")
suppressPackageStartupMessages({library(data.table);library(jsonlite);library(digest);library(readxl)})
source("R/research_governance.R")
args <- commandArgs(TRUE)
out <- if(length(args)) args[1] else "data-derived/p15_mpg_loan_normalization_20260910_v1"
if(dir.exists(out))stop("Fresh output directory required")
pvr_assert_write_allowed(out,"diagnostic")
base <- "sources/literature_review/loan_extension_feasibility_20260910/mpg/extracted"
csv <- file.path(base,"Full Dataset.csv")
raw_ibrd <- file.path(base,"input/IBRD_Statement_of_Loans_-_Latest_Available_Snapshot.csv")
raw_ida <- file.path(base,"input/IDA_Statement_of_Credits_and_Grants__-_Latest_Available_Snapshot.csv")
raw_china <- file.path(base,"input/GlobalChineseOfficialFinanceDataset_v1.0.xlsx")
country_json <- "data-raw/world_bank_countries.json"
design <- "docs/thesis_design/loan_valuations_2026-09-10/MPG_NORMALIZATION_DESIGN.md"
owner <- "docs/thesis_design/loan_valuations_2026-09-10/OWNER_REQUEST.md"
archive_manifest <- file.path(dirname(base),"extracted_manifest.csv")
creation <- file.path(base,"dofiles/Chinese_World_Bank_Lending_Terms_Database_Creation_20200228_FINAL.do")
analysis <- file.path(base,"dofiles/Chinese_World_Bank_Lending_Terms_Analysis_20200305_FINAL.do")
archive <- fread(archive_manifest)
for(f in c(csv,raw_ibrd,raw_ida,raw_china,creation,analysis)){
 m<-archive[path==f];stopifnot(nrow(m)==1L,digest(file=f,algo="sha256")==m$sha256)
}
all_source <- fread(csv,na.strings="",colClasses=c(id="character",countrycode="character"))
all_source[,source_row:=.I]
x <- all_source[country!="China" & type=="Loan" & year%in%2012:2014]
wb <- fromJSON(country_json)[[2]]
country_map <- data.table(iso2=wb$iso2Code,iso3=wb$id,is_country=wb$region$id!="NA")[is_country==TRUE]
stopifnot(!anyDuplicated(country_map$iso2))
x[,`:=`(dataset="mpg",loan_id=id,loan_event_id=paste0("mpg::",id),
  iso3=country_map$iso3[match(countrycode,country_map$iso2)],commitment_year=as.integer(year),
  creditor=fcase(source=="China","China",grepl("IBRD",lender_d),"IBRD",grepl("IDA",lender_d),"IDA"),
  currency=NA_character_,interest_rate_pct=100*interest,maturity_years=maturity,grace_years=grace,
  amount_usd=amount,amount_original=originalamount,amount_usd_basis="constant_2014_USD_paper_weight",
  amount_original_basis="nominal_USD_source_year",original_currency_amount=NA_real_,
  source_path=csv,source_original_maturity=maturity,source_original_grace=grace,
  source_saved_ge_pct=100*grantelement,source_saved_ge_b_pct=100*grantelement_b,
  source_row_locator=paste0("Full Dataset.csv#id=",id),
  loan_scope="paper_sample_loans_2012_2014_country_China_excluded",
  interest_type="reported_rate_type_unspecified_constant_coupon_scenario",
  schedule_basis="reported_China_total_maturity_and_grace_equal_principal_assumption",
  currency_basis="currency_unverified_common_USD_scenario",
  raw_source_path=NA_character_,raw_source_row=NA_integer_,raw_join_found=FALSE,
  source_year_uncertain=NA_character_,source_loan_type=NA_character_,
  raw_countrycode=NA_character_,raw_commitment_year=NA_integer_,
  raw_currency_field=NA_character_,raw_original_interest_rate_pct=NA_real_,
  effective_date=as.Date(NA),first_repayment_date=as.Date(NA),last_repayment_date=as.Date(NA),
  agreement_signing_date=as.Date(NA),board_approval_date=as.Date(NA),
  commitment_date=as.Date(NA),commitment_date_basis=NA_character_,
  first_principal_payment_years=grace+0.5,repayment_interval_years=maturity-grace-0.5,
  agreement_anchor_maturity_years=NA_real_,agreement_anchor_grace_years=NA_real_)]
parse_date <- function(z)as.Date(z,"%m/%d/%Y")
num <- function(z)suppressWarnings(as.numeric(gsub(",","",z,fixed=TRUE)))
for(kind in c("IBRD","IDA")){
 f<-if(kind=="IBRD")raw_ibrd else raw_ida
 raw<-fread(f,na.strings="",colClasses="character")
 id<-raw[[if(kind=="IBRD")"Loan Number" else "Credit Number"]]
 stopifnot(!anyDuplicated(id))
 rows<-which(x$creditor==kind);at<-match(x$loan_id[rows],id)
 stopifnot(!anyNA(at))
 first<-parse_date(raw[["First Repayment Date"]][at]);last<-parse_date(raw[["Last Repayment Date"]][at])
 effective<-parse_date(raw[["Effective Date (Most Recent)"]][at]);signing<-parse_date(raw[["Agreement Signing Date"]][at])
 board<-parse_date(raw[["Board Approval Date"]][at]);commitment<-signing;missing<-is.na(commitment);commitment[missing]<-board[missing]
 x[rows,`:=`(raw_join_found=TRUE,raw_source_path=f,raw_source_row=at,
   currency=raw[["Currency of Commitment"]][at],raw_currency_field=raw[["Currency of Commitment"]][at],
   currency_basis=ifelse(is.na(raw[["Currency of Commitment"]][at]),"WB_currency_missing_common_USD_scenario",
    "WB_reported_currency_of_commitment_common_USD_scenario_if_nonUSD"),
   raw_countrycode=raw[["Country Code"]][at],raw_commitment_year=as.integer(format(commitment,"%Y")),
   raw_original_interest_rate_pct=num(raw[[if(kind=="IBRD")"Interest Rate" else "Service Charge Rate"]][at]),
   effective_date=effective,first_repayment_date=first,last_repayment_date=last,
   agreement_signing_date=signing,board_approval_date=board,commitment_date=commitment,
   commitment_date_basis=ifelse(missing,"board_approval_fallback","agreement_signing"),
   maturity_years=as.numeric(last-effective)/365.25,grace_years=as.numeric(first-effective)/365.25,
   first_principal_payment_years=as.numeric(first-effective)/365.25,
   repayment_interval_years=as.numeric(last-first)/365.25,
   agreement_anchor_maturity_years=as.numeric(last-commitment)/365.25,
   agreement_anchor_grace_years=as.numeric(first-commitment)/365.25,
   schedule_basis="WB_effective_first_last_dates_daycount_365.25_equal_principal_between_endpoints_assumption",
   interest_type=if(kind=="IDA")"IDA_source_service_charge_constant_coupon_scenario" else
     "IBRD_source_rate_proxy_held_constant_no_floating_path",
   source_loan_type=if(kind=="IBRD")raw[["Loan Type"]][at] else "IDA_credit")]
}
china<-as.data.table(read_excel(raw_china,sheet="ChineseOfficialFinance1.0",col_types="text"))
stopifnot(!anyDuplicated(china$project_id))
rows<-which(x$creditor=="China");at<-match(x$loan_id[rows],china$project_id)
stopifnot(!anyNA(at))
x[rows,`:=`(raw_join_found=TRUE,raw_source_path=raw_china,raw_source_row=at,
  currency=china$currency[at],raw_currency_field=china$currency[at],
  currency_basis="AidData_currency_of_reported_amount_contract_currency_not_independently_verified",
  original_currency_amount=num(china$amount[at]),source_loan_type=china$loan_type[at],
  raw_original_interest_rate_pct=num(china$interest_rate[at]),source_year_uncertain=china$year_uncertain[at],
  raw_countrycode=china$recipient_iso2[at],raw_commitment_year=as.integer(num(china$year[at])))]
# Keep explicit bullet endpoints, including WB records with zero published span.
x[,repayment_schedule_type:=fifelse(is.finite(maturity_years)&is.finite(grace_years)&maturity_years==grace_years,
  "bullet_principal_endpoint","endpoint_matched_equal_principal_assumption")]
x[,exclusion_reason:=fcase(!raw_join_found,"source_record_not_joined",is.na(iso3),"country_code_not_mapped",
  !is.finite(interest_rate_pct),"missing_interest_rate",interest_rate_pct<0,"negative_interest_rate",
  !is.finite(maturity_years)|!is.finite(grace_years),"missing_repayment_endpoint_or_reported_term",
  maturity_years<=0,"nonpositive_total_horizon",grace_years<0,"first_repayment_before_most_recent_effective_date",
  grace_years>maturity_years,"reported_grace_exceeds_total_horizon_unresolved",
  default="none")]
x[,normalization_eligible:=exclusion_reason=="none"]
x[,agreement_anchor_alternative_eligible:=creditor%in%c("IDA","IBRD") & is.finite(interest_rate_pct)&interest_rate_pct>=0 &
  is.finite(agreement_anchor_maturity_years)&is.finite(agreement_anchor_grace_years)&
  agreement_anchor_maturity_years>0 & agreement_anchor_grace_years>=0 &
  agreement_anchor_grace_years<=agreement_anchor_maturity_years]
# Replay the archived formula without claiming that it resolves source schedules.
discount<-sqrt(1.05)-1
x[,source_formula_five_pct:=100*(1-(interest_rate_pct/100/2)/discount)*
 (1-((1/(1+discount)^(2*source_original_grace)-1/(1+discount)^(2*source_original_maturity))/
 (discount*(2*source_original_maturity-2*source_original_grace))))]
x[,source_formula_replay_error_pp:=source_formula_five_pct-source_saved_ge_pct]
x[,raw_countrycode_normalized:=fcase(creditor!="China" & raw_countrycode=="YF","RS",
  creditor!="China" & raw_countrycode=="TP","TL",default=raw_countrycode)]
x[,`:=`(source_interval_nonpositive=is.finite(source_original_maturity)&source_original_maturity<=0,
  source_grace_exceeds_interval=is.finite(source_original_maturity)&is.finite(source_original_grace)&
    source_original_grace>source_original_maturity)]
checks<-data.table(check=c("all_1044_paper_loans_retained","source_id_unique","exact_raw_source_id_joins",
 "country_codes_agree_with_raw","commitment_years_agree_with_raw","saved_5pct_formula_replays",
 "WB_date_horizon_identity","zero_WB_spans_identified_as_bullets","no_reported_values_overwritten",
 "eligible_schedules_have_nonnegative_ordered_endpoints"),passed=c(
 nrow(x)==1044L,!anyDuplicated(x$loan_id),all(x$raw_join_found),all(x$countrycode==x$raw_countrycode_normalized),
 all(x$commitment_year==x$raw_commitment_year),all(abs(x$source_formula_replay_error_pp[is.finite(x$source_formula_replay_error_pp)])<1e-4),
 all(abs(x[creditor!="China",maturity_years-grace_years-repayment_interval_years])<1e-12,na.rm=TRUE),
 nrow(x[creditor=="IBRD" & source_interval_nonpositive & repayment_schedule_type=="bullet_principal_endpoint" & normalization_eligible])==22L,
 identical(x$source_original_maturity,x$maturity)&identical(x$source_original_grace,x$grace),
 all(x[normalization_eligible==TRUE,maturity_years>0 & grace_years>=0 & grace_years<=maturity_years])))
if(!all(checks$passed)){print(checks[passed==FALSE | is.na(passed)]);stop("Normalization checks failed")}
required<-c("dataset","loan_id","loan_event_id","iso3","commitment_year","creditor","currency","interest_rate_pct",
 "maturity_years","grace_years","amount_usd","amount_original","interest_type","normalization_eligible","exclusion_reason",
 "source_row","source_path","source_original_maturity","source_original_grace","source_saved_ge_pct","schedule_basis","currency_basis","loan_scope")
setcolorder(x,c(required,setdiff(names(x),required)))
tables<-list(normalized_loans=x,
 normalization_summary=x[,.(source_loans=.N,normalization_eligible=sum(normalization_eligible),
   saved_ge_available=sum(is.finite(source_saved_ge_pct)),bullets=sum(repayment_schedule_type=="bullet_principal_endpoint"),
   reported_nonpositive_interval=sum(source_interval_nonpositive),reported_grace_exceeds_interval=sum(source_grace_exceeds_interval),
   agreement_anchor_eligible=sum(agreement_anchor_alternative_eligible)),by=.(creditor)],
 normalization_exclusions=x[normalization_eligible==FALSE,.(loan_id,iso3,commitment_year,creditor,exclusion_reason,
   interest_rate_pct,source_original_maturity,source_original_grace,maturity_years,grace_years,first_repayment_date,
   last_repayment_date,effective_date,agreement_anchor_maturity_years,agreement_anchor_grace_years,
   agreement_anchor_alternative_eligible,source_saved_ge_pct)],
 currency_summary=x[,.(records=.N,normalization_eligible=sum(normalization_eligible)),by=.(creditor,currency,currency_basis)],
 checks=checks)
dir.create(out,recursive=TRUE)
bundle<-list(build_id=basename(out),schema_id="SCHEMA-P15-MPG-LOAN-NORMALIZATION-V1",
 estimator_id="EST-P15-MPG-RAW-DATE-HORIZON-RECONSTRUCTION-V1",admissibility_id="ADM-P15-MPG-NORMALIZED-ENDPOINTS-V1",
 selection_id="SEL-P15-EXTERNAL-LOAN-INPUTS-NO-BENCHMARK-SELECTION-V1",source_package_ids="SRC-MPG-2020-REPLICATION-ZIP-20260910")
for(name in names(tables)){
 z<-copy(tables[[name]]);for(field in names(bundle))set(z,j=field,value=bundle[[field]])
 z[,`:=`(release_state="private_diagnostic_normalized_loan_inputs",lineage_parent_ids=paste(c(csv,raw_ibrd,raw_ida,raw_china),collapse=";"))]
 fwrite(z,file.path(out,paste0(name,".csv")),na="")
}
input<-c(csv,raw_ibrd,raw_ida,raw_china,country_json,archive_manifest,creation,analysis,design,owner,"renv.lock")
code<-c("scripts/p15/loan_extension/normalize_mpg.R","scripts/p15/activate_p15_environment.R","R/research_governance.R")
manifest_rows<-function(ps,role)do.call(pvr_manifest_rows,c(list(paths=ps,artifact_role=role),bundle))
fwrite(manifest_rows(input,"input"),file.path(out,"input_manifest.csv"))
fwrite(manifest_rows(code,"script"),file.path(out,"code_manifest.csv"))
write_json(c(bundle,list(run_finished_at=format(Sys.time(),"%Y-%m-%dT%H:%M:%S%z"),run_status="success",r_version=R.version.string,
 renv_lock_sha256=digest(file="renv.lock",algo="sha256"),market_valuation_performed=FALSE)),file.path(out,"run_environment.json"),auto_unbox=TRUE,pretty=TRUE)
capture.output(sessionInfo(),file=file.path(out,"environment.txt"))
fwrite(manifest_rows(list.files(out,full.names=TRUE),"output"),file.path(out,"output_manifest.csv"))
print(tables$normalization_summary);cat("Saved",out,"\n")
