#!/usr/bin/env Rscript
# Read-back checks independent of normalize_add() and its derived summary tables.
source("scripts/p15/activate_p15_environment.R")
suppressPackageStartupMessages({library(data.table);library(readxl);library(digest)})
args <- commandArgs(TRUE)
out <- if(length(args))args[1] else "data-derived/p15_add_loan_normalization_20260910_v1"
x <- fread(file.path(out,"normalized_loans.csv"),na.strings="")
raw <- as.data.table(read_excel(unique(x$source_path),col_types="text"))
n <- function(v)suppressWarnings(as.numeric(v))
idx <- x$source_row
stopifnot(nrow(x)==5142L,!anyDuplicated(idx),
  identical(x$iso3,raw$ISO3[idx]),identical(x$currency,raw$Currency[idx]),
  isTRUE(all.equal(x$interest_rate_pct,n(raw$interest[idx]))),
  isTRUE(all.equal(x$amount_usd,n(raw$Amount_musd[idx])*1e6)),
  isTRUE(all.equal(x$amount_original,n(raw$Amount_m[idx])*1e6)),
  identical(x$normalization_eligible,is.na(x$exclusion_reason)|!nzchar(x$exclusion_reason)))
# Direct endpoint example: the stored Benin IDA56420 credit is independently
# read from its original WB snapshot rather than the normalization audit output.
p <- "sources/literature_review/loan_extension_feasibility_20260910/mpg/extracted/input/IDA_Statement_of_Credits_and_Grants__-_Latest_Available_Snapshot.csv"
w <- fread(p,colClasses="character")
r <- w[`Credit Number`=="IDA56420"]
dt <- function(v)as.Date(v,format="%m/%d/%Y")
first <- dt(r[["First Repayment Date"]]);last<-dt(r[["Last Repayment Date"]])
effective <- dt(r[["Effective Date (Most Recent)"]])
b <- x[wb_id=="IDA56420"]
stopifnot(nrow(b)==1L,abs(b$maturity_years-as.numeric(last-effective)/365.25)<1e-10,
  abs(b$first_principal_payment_years-as.numeric(first-effective)/365.25)<1e-10,
  abs(b$source_original_maturity-as.numeric(last-first)/365.25)<1e-10,
  b$maturity_years>b$source_original_maturity)
# Numeric completeness and analytical scope are intentionally different flags.
stopifnot(all(x[normalization_eligible==TRUE,first_principal_payment_years<=maturity_years]),
  x[creditor=="WB-IBRD",sum(normalization_eligible)]==0L,
  x[creditor=="WB-IDA"&source_grace_gt_reported_maturity==TRUE,.N]==49L,
  x[creditor=="WB-IDA"&source_grace_gt_reported_maturity==TRUE,all(normalization_eligible)],
  x[normalization_eligible & zero_rate_uncertain,.N]==54L,
  x[normalization_eligible & !central_scope_eligible,.N]==29L,
  all(!x$repayment_schedule_observed),all(!x$fees_fully_observed))
for(name in c("input_manifest.csv","code_manifest.csv","output_manifest.csv")) {
  m <- fread(file.path(out,name))
  stopifnot(all(vapply(m$path,function(p)digest(file=p,algo="sha256"),character(1))==m$sha256))
}
cat("ADD independent read-back checks passed: source rows, rates, units, direct WB dates, scope, exclusions and manifests.\n")
