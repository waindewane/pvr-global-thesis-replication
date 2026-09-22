#!/usr/bin/env Rscript
# Independent base-R check against exact archived rows and calendar dates.
source("scripts/p15/activate_p15_environment.R")
args <- commandArgs(TRUE)
base <- if(length(args))args[1] else "data-derived/p15_mpg_loan_normalization_20260910_v1"
out <- if(length(args)>1)args[2] else "docs/thesis_design/loan_valuations_2026-09-10/mpg_normalization_verification.csv"
read <- function(path)read.csv(path,check.names=FALSE,stringsAsFactors=FALSE,na.strings="",colClasses="character")
normalized <- read(file.path(base,"normalized_loans.csv"))
source_base <- "sources/literature_review/loan_extension_feasibility_20260910/mpg/extracted"
source <- read(file.path(source_base,"Full Dataset.csv"))
source <- source[source$country!="China" & source$type=="Loan" & source$year%in%as.character(2012:2014),]
at <- match(source$id,normalized$loan_id)
stopifnot(!anyNA(at));n <- normalized[at,]
checks <- data.frame(check=c("all_source_loan_rows_preserved","no_duplicate_loan_ids","unchanged_paper_maturity_grace",
  "unchanged_paper_interest_scaled_to_pct","unchanged_paper_constant_USD_weights","unchanged_nominal_USD_weights",
  "unchanged_saved_grant_elements_scaled_to_pct"),passed=c(
  identical(source$id,n$loan_id)&nrow(n)==1044,!anyDuplicated(n$loan_id),
  isTRUE(all.equal(as.numeric(source$maturity),as.numeric(n$source_original_maturity))) &
    isTRUE(all.equal(as.numeric(source$grace),as.numeric(n$source_original_grace))),
  isTRUE(all.equal(as.numeric(source$interest)*100,as.numeric(n$interest_rate_pct))),
  isTRUE(all.equal(as.numeric(source$amount),as.numeric(n$amount_usd))),
  isTRUE(all.equal(as.numeric(source$originalamount),as.numeric(n$amount_original))),
  isTRUE(all.equal(as.numeric(source$grantelement)*100,as.numeric(n$source_saved_ge_pct)))))
for(kind in c("IDA","IBRD")){
 file<-list.files(file.path(source_base,"input"),pattern=paste0("^",kind,".*csv$"),full.names=TRUE)
 raw<-read(file);id<-raw[[if(kind=="IBRD")"Loan Number" else "Credit Number"]]
 z<-n[n$creditor==kind,];a<-match(z$loan_id,id);stopifnot(!anyNA(a))
 first<-as.Date(raw[["First Repayment Date"]][a],"%m/%d/%Y")
 last<-as.Date(raw[["Last Repayment Date"]][a],"%m/%d/%Y")
 effective<-as.Date(raw[["Effective Date (Most Recent)"]][a],"%m/%d/%Y")
 total<-as.numeric(last-effective)/365.25;grace<-as.numeric(first-effective)/365.25
 date_eligible<-!is.na(first)&!is.na(last)&!is.na(effective)&last>effective&first>=effective&last>=first
 checks<-rbind(checks,data.frame(check=paste0(kind,c("_total_horizon_exact_dates","_first_payment_exact_dates",
  "_eligibility_from_independent_date_order","_bullet_identification_from_equal_raw_dates","_currency_field_preserved")),
 passed=c(isTRUE(all.equal(total,as.numeric(z$maturity_years))),
  isTRUE(all.equal(grace,as.numeric(z$first_principal_payment_years))),
  identical(date_eligible,as.logical(z$normalization_eligible)),
  all((z$repayment_schedule_type=="bullet_principal_endpoint")==(!is.na(first)&!is.na(last)&first==last)),
  identical(raw[["Currency of Commitment"]][a],z$currency))))
}
hash_results<-lapply(c("input_manifest.csv","code_manifest.csv","output_manifest.csv"),function(file){
 m<-read(file.path(base,file));ok<-vapply(seq_len(nrow(m)),function(i)
   file.exists(m$artifact_path[i])&&digest::digest(file=m$artifact_path[i],algo="sha256")==m$sha256[i],logical(1))
 data.frame(check=paste0("hashes_",file),passed=all(ok))
})
checks<-rbind(checks,do.call(rbind,hash_results))
checks$normalization_build<-basename(base)
checks$normalization_output_manifest_sha256<-digest::digest(file=file.path(base,"output_manifest.csv"),algo="sha256")
write.csv(checks,out,row.names=FALSE,na="")
print(checks[,c("check","passed")]);stopifnot(all(checks$passed))
