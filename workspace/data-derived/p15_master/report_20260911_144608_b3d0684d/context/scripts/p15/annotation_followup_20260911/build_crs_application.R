#!/usr/bin/env Rscript
source("scripts/p15/activate_p15_environment.R")
suppressPackageStartupMessages({library(data.table);library(jsonlite);library(digest)})
source("R/research_governance.R")
source("scripts/p15/loan_extension/benchmark_matching.R")
source("scripts/p15/annotation_followup_20260911/crs_cashflows.R")
args<-commandArgs(TRUE);out<-if(length(args))args[1] else "data-derived/p15_crs_application_20260911_v1"
years<-if(length(args)>1L)as.integer(strsplit(args[2],",",fixed=TRUE)[[1]]) else 2012:2024
stopifnot(!dir.exists(out));pvr_assert_write_allowed(out,"diagnostic");dir.create(out,recursive=TRUE)
raw_paths<-ifelse(years==2024,"data-raw/oecd_crs/2026-05-25/crs_2024_dotstat_v20260408.zip",
 paste0("data-raw/oecd_crs/2026-09-11/crs_",years,"_dotstat_v20260408.zip"))
stopifnot(all(file.exists(raw_paths)))
cols<-c("Year","DonorCode","DEDonorcode","DonorName","AgencyCode","AgencyName","CrsID","ProjectNumber","InitialReport",
 "RecipientCode","DERecipientcode","RecipientName","Finance_T","FlowCode","FlowName","Bi_Multi","Category","CurrencyCode",
 "USD_Commitment","USD_Disbursement","USD_GrantEquiv","Commitment_National","GrantEquiv","TypeRepayment","NumberRepayment",
 "Interest1","Interest2","CommitmentDate","Repaydate1","Repaydate2","ChannelCode","ChannelName","ProjectTitle","ShortDescription","PSIflag")
all_debt<-list(); inventory<-list()
for(j in seq_along(years)) {
 path<-raw_paths[j];header<-fread(cmd=paste("unzip -p",shQuote(path)),nrows=0,showProgress=FALSE)
 d<-fread(cmd=paste("unzip -p",shQuote(path)),select=intersect(cols,names(header)),colClasses="character",na.strings="",showProgress=FALSE)
 for(nm in setdiff(cols,names(d)))d[,(nm):=NA_character_]
 d[,source_row:=.I+1L];d[,`:=`(report_year=years[j],source_archive=path,source_snapshot_id=paste0("SRC-OECD-CRS-",years[j],"-20260408"))]
 d[,is_debt:=grepl("^4",Finance_T)]
 inventory[[j]]<-data.table(report_year=years[j],all_reporting_rows=nrow(d),debt_rows=sum(d$is_debt),ordinary_loan_rows=sum(d$Finance_T=="421",na.rm=TRUE))
 all_debt[[j]]<-d[is_debt==TRUE];rm(d);gc(FALSE)
}
x<-rbindlist(all_debt,fill=TRUE);rm(all_debt)
x[,record_id:=paste0("crs_",report_year,"_row_",source_row)]
number<-function(v)suppressWarnings(as.numeric(v))
x[,`:=`(iso3=DERecipientcode,commitment_date=as.Date(trimws(CommitmentDate)),first_date=as.Date(trimws(Repaydate1)),
 final_date=as.Date(trimws(Repaydate2)),repayment_type=number(TypeRepayment),frequency=number(NumberRepayment),
 amount_usd=number(USD_Commitment)*1e6,reported_disbursement_usd=number(USD_Disbursement)*1e6,
 reported_grant_equivalent_usd=number(USD_GrantEquiv)*1e6,
 interest_numeric=grepl("^[0-9]+(\\.[0-9]+)?$",Interest1))]
x[,`:=`(commitment_year=as.integer(format(commitment_date,"%Y")),interest_rate_pct=fifelse(interest_numeric,number(Interest1)/1000,NA_real_),
 borrower_scope=fcase(ChannelCode=="12001","central_government_explicit",ChannelCode=="12000","recipient_government_unspecified_level",
  ChannelCode%in%c("12002","12003","12004"),"other_recipient_public_sector",default="other_or_unknown"),
 loan_currency=NA_character_,loan_currency_basis="CRS_reporting_currency_does_not_establish_loan_denomination")]
x[,exclusion_reason:=fcase(Finance_T!="421","instrument_not_standard_loan_421",
 InitialReport!="1"|is.na(InitialReport),"not_new_activity_reporting",
 is.na(commitment_date),"missing_or_invalid_commitment_date",commitment_year!=report_year,"new_activity_commitment_year_differs_from_reporting_year",
 !is.finite(amount_usd)|amount_usd<=0,"no_positive_new_commitment",
 !borrower_scope%in%c("central_government_explicit","recipient_government_unspecified_level"),"borrower_not_identified_as_recipient_government",
 is.na(iso3)|nchar(iso3)!=3L,"recipient_not_individual_country",
 !interest_numeric|is.na(interest_numeric),"missing_or_variable_interest",
 !is.finite(interest_rate_pct)|interest_rate_pct<0|interest_rate_pct>50,"interest_outside_documented_numeric_range",
 is.finite(number(Interest2))&number(Interest2)>0,"second_numeric_interest_rate_requires_interpretation",
 grepl("International Bank for Reconstruction|International Development Association",DonorName)&interest_rate_pct==0,
 "World_Bank_zero_interest_total_charge_not_verified",
 !repayment_type%in%c(1L,2L),"repayment_type_missing_or_not_EPP_or_annuity",
 !frequency%in%c(1L,2L,4L,12L),"repayment_frequency_missing_or_unsupported",
 is.na(first_date)|is.na(final_date),"missing_repayment_dates",
 first_date<=commitment_date|final_date<first_date,"invalid_repayment_date_order",
 as.numeric(final_date-commitment_date)/365.25>100,"repayment_horizon_over_100_years",
 is.na(CrsID)|!nzchar(CrsID),"missing_activity_identifier",default="eligible_terms")]
x[,activity_id:=paste(DonorCode,AgencyCode,CrsID,sep=":")]
x[,wb_loan_id:=NA_character_]
wb_rows<-which(grepl("International Bank for Reconstruction|International Development Association",x$DonorName)&
 grepl("(IBRD|IDA)[A-Z]?[0-9]+",x$ProjectNumber))
x$wb_loan_id[wb_rows]<-regmatches(x$ProjectNumber[wb_rows],regexpr("(IBRD|IDA)[A-Z]?[0-9]+",x$ProjectNumber[wb_rows]))
x[,financing_id:=fifelse(!is.na(wb_loan_id),paste(DonorCode,AgencyCode,wb_loan_id,sep=":"),activity_id)]
term_cols<-c("iso3","commitment_date","first_date","final_date","repayment_type","frequency","interest_rate_pct","borrower_scope","CurrencyCode")
x[exclusion_reason=="eligible_terms",term_signature:=do.call(paste,c(.SD,sep="|")),.SDcols=term_cols]
conflicts<-x[exclusion_reason=="eligible_terms",.(term_variants=uniqueN(term_signature)),by=.(report_year,financing_id)][term_variants>1L]
x[exclusion_reason=="eligible_terms" & paste(report_year,financing_id)%in%paste(conflicts$report_year,conflicts$financing_id),exclusion_reason:="financing_identifier_has_conflicting_country_or_terms"]
q<-x[exclusion_reason=="eligible_terms"]
u<-q[,{
 a<-as.list(.SD[1]);a$amount_usd<-sum(amount_usd);a$reported_disbursement_usd<-sum(reported_disbursement_usd,na.rm=TRUE)
 a$reported_grant_equivalent_usd<-if(all(is.na(reported_grant_equivalent_usd)))NA_real_ else sum(reported_grant_equivalent_usd,na.rm=TRUE)
 a$source_record_ids<-paste(record_id,collapse=";");a$source_record_count<-.N
 a$source_activity_ids<-paste(unique(activity_id),collapse=";");a$source_activity_count<-uniqueN(activity_id);a
},by=.(report_year,financing_id),.SDcols=setdiff(names(q),c("report_year","financing_id"))]
u[,`:=`(loan_id=paste0("crs:",report_year,":",financing_id),financing_unit_basis=fifelse(!is.na(wb_loan_id),
 "World_Bank_loan_ID_consolidating_CRS_sector_activities","CRS_provider_agency_activity_ID_no_inferred_loan_merge"))]
stopifnot(!anyDuplicated(u$loan_id))
current<-fromJSON("data-derived/p15_master/current_run.json")
base<-Sys.getenv("P15_ANALYSIS_BASE",current$candidate);dacbase<-Sys.getenv("P15_DAC_BASE",current$stages$dac$dir)
benchpaths<-file.path(base,c("selected_reference.csv","tier_eligibility.csv","core_evidence.csv"))
ref<-p15_loan_benchmark_reference(fread(benchpaths[1]),fread(benchpaths[2]),fread(benchpaths[3]))
m<-p15_match_external_loans(u[,!c("source_snapshot_id")],ref)$selected
policy_path<-file.path(dacbase,"map.csv")
p<-fread(policy_path)[,.(iso3,commitment_year=analysis_year,group_rate_pct,dac_eligible,dac_group,historical_headline_new_loan_rate_pct)]
m<-merge(m,p,by=c("iso3","commitment_year"),all.x=TRUE,sort=FALSE)
m[,`:=`(matched=benchmark_selected_ordinary_usable==TRUE,non_peer=benchmark_selected_tier!="peer",
 primary_ids=benchmark_selected_tier%in%c("primary","ids"),standardized_reference_rate_pct=fifelse(dac_eligible==TRUE,group_rate_pct,NA_real_))]
m[is.na(matched),matched:=FALSE]
for(nm in c("ge_market_pct","ge_standardized_pct","ge_fixed5_pct","ge_historical10_pct"))m[,(nm):=NA_real_]
flows<-vector("list",nrow(m));schedule_checks<-vector("list",nrow(m))
for(j in seq_len(nrow(m))) {
 a<-m[j]; f<-crs_cashflows(a$commitment_date,a$first_date,a$final_date,a$interest_rate_pct,a$frequency,a$repayment_type)
 f[,loan_id:=a$loan_id];flows[[j]]<-f
 m$ge_market_pct[j]<-crs_ge(f,if(a$matched)a$benchmark_selected_rate_pct else NA_real_)
 m$ge_standardized_pct[j]<-crs_ge(f,a$standardized_reference_rate_pct);m$ge_fixed5_pct[j]<-crs_ge(f,5)
 if(a$commitment_year<2018)m$ge_historical10_pct[j]<-crs_ge(f,10)
 schedule_checks[[j]]<-data.table(loan_id=a$loan_id,principal_sum=sum(f$principal),cashflows=nrow(f),
  end_balance=tail(f$opening_balance-f$principal,1L),finite_payments=all(is.finite(f$payment)),
  discount_monotonicity=crs_ge(f,9)>=crs_ge(f,7) && crs_ge(f,7)>=crs_ge(f,5),
  irregular_final_interval=abs(tail(f$interval_years,1L)-1/a$frequency)>7/365.25)
}
m[,`:=`(delta_standardized_ge_pp=ge_market_pct-ge_standardized_pct,delta_fixed5_ge_pp=ge_market_pct-ge_fixed5_pct,
 delta_historical10_ge_pp=ge_market_pct-ge_historical10_pct,
 period=fcase(commitment_year<2018,"2012_2017",commitment_year<=2021,"2018_2021",default="2022_2024"))]
paired<-rbindlist(lapply(c("all_selected","non_peer","primary_ids"),function(v) {
 z<-m[matched & (v=="all_selected" | (v=="non_peer" & non_peer) | (v=="primary_ids" & primary_ids))]
 rbindlist(lapply(c("standardized_DAC_category_rule","fixed5","historical_10pct_convention"),function(r) {
  a<-copy(z);a[,`:=`(benchmark_view=v,reference=r)]
  a[,reference_discount_rate_pct:=switch(r,standardized_DAC_category_rule=standardized_reference_rate_pct,fixed5=rep(5,.N),historical_10pct_convention=rep(10,.N))]
  a[,reference_ge_pct:=switch(r,standardized_DAC_category_rule=ge_standardized_pct,fixed5=ge_fixed5_pct,historical_10pct_convention=ge_historical10_pct)]
  a[,delta_ge_pp:=ge_market_pct-reference_ge_pct];a[is.finite(delta_ge_pp)]
 }))
}))
summary_fn<-function(d) d[,.(financing_records=.N,source_activities=sum(source_activity_count),source_records=sum(source_record_count),countries=uniqueN(iso3),providers=uniqueN(DonorCode),
 amount_usd=sum(amount_usd),mean_reference_ge_pct=mean(reference_ge_pct),mean_market_ge_pct=mean(ge_market_pct),
 mean_delta_ge_pp=mean(delta_ge_pp),median_delta_ge_pp=median(delta_ge_pp),amount_weighted_delta_ge_pp=weighted.mean(delta_ge_pp,amount_usd),positive_share=mean(delta_ge_pp>0))]
summarize<-function(by)paired[,summary_fn(.SD),by=by]
checks<-data.table(check=c("unique_reporting_rows","unique_activity_units","full_selected_nonpeer_subset","all_finite_cashflows","principal_conserved",
 "discount_monotonicity","same_case_delta_identity","no_reported_currency_relabelled_loan_currency","constant_annual_DAC_rule_values"),passed=c(
 !anyDuplicated(x$record_id),!anyDuplicated(m$loan_id),all(m$loan_id[m$matched&m$non_peer]%in%m$loan_id[m$matched]),
 all(rbindlist(schedule_checks)$finite_payments),all(abs(rbindlist(schedule_checks)$principal_sum-100)<1e-7),
 all(rbindlist(schedule_checks)$discount_monotonicity),all(abs(paired$delta_ge_pp-paired$ge_market_pct+paired$reference_ge_pct)<1e-8),
 all(is.na(m$loan_currency)),all(m$standardized_reference_rate_pct[is.finite(m$standardized_reference_rate_pct)]%in%c(6,7,9))))
fwrite(checks,file.path(out,"checks.csv"));stopifnot(all(checks$passed))
tables<-list(reporting_inventory=rbindlist(inventory),row_eligibility=x,
 provider_coverage=x[,.(reporting_debt_rows=.N,new_loan_rows=sum(Finance_T=="421"&InitialReport=="1",na.rm=TRUE),eligible_rows=sum(exclusion_reason=="eligible_terms")),by=.(report_year,DonorName)],
 exclusion_summary=x[,.N,by=.(report_year,exclusion_reason)],activity_conflicts=conflicts,loan_valuations=m,paired_comparisons=paired,
 cash_flows=rbindlist(flows),schedule_checks=rbindlist(schedule_checks),
 comparison_summary=summarize(c("benchmark_view","reference")),annual_summary=summarize(c("benchmark_view","reference","commitment_year")),
 period_summary=summarize(c("benchmark_view","reference","period")),provider_summary=summarize(c("benchmark_view","reference","DonorName")),
 borrower_scope_summary=summarize(c("benchmark_view","reference","borrower_scope")),
 tier_summary=summarize(c("benchmark_view","reference","benchmark_selected_tier")))
bundle<-list(build_id=basename(out),schema_id="SCHEMA-P15-CRS-APPLICATION-V2",estimator_id="EST-P15-CRS-DATED-FIXED-EPP-ANNUITY-V1",
 admissibility_id="ADM-P15-CRS-NEW-GOVERNMENT-FIXED-COMPLETE-V1",selection_id="SEL-P15-CURRENT-BENCHMARK-UNCHANGED",
 source_package_ids=paste0("SRC-OECD-CRS-",years,"-20260408",collapse=";"),lifecycle_status="diagnostic",release_state="private_research")
for(nm in names(tables))fwrite(tables[[nm]],file.path(out,paste0(nm,".csv")),na="")
write_json(bundle,file.path(out,"version_bundle.json"),pretty=TRUE,auto_unbox=TRUE)
inputs<-c(raw_paths,benchpaths,policy_path,"data-raw/oecd_crs/2026-05-25/dac_tables_crs_codebook_2025-08-19.xlsx",
 "docs/thesis_design/feedback_2026-09-11/CRS_SOURCE_ANALYSIS_DESIGN.md","renv.lock")
manifest<-function(ps)data.table(path=ps,sha256=vapply(ps,function(f)digest(file=f,algo="sha256"),character(1)),bytes=file.info(ps)$size)
fwrite(manifest(inputs),file.path(out,"input_manifest.csv"))
fwrite(manifest(c("scripts/p15/annotation_followup_20260911/build_crs_application.R","scripts/p15/annotation_followup_20260911/crs_cashflows.R",
 "scripts/p15/loan_extension/benchmark_matching.R")),file.path(out,"code_manifest.csv"))
writeLines(c(capture.output(sessionInfo()),paste0("candidate=",base),paste0("dac=",dacbase)),file.path(out,"environment.txt"))
fwrite(manifest(list.files(out,full.names=TRUE)),file.path(out,"output_manifest.csv"))
print(tables$comparison_summary);message("CRS application written: ",out)
