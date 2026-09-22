#!/usr/bin/env Rscript
source("scripts/p15/activate_p15_environment.R")
suppressPackageStartupMessages({library(data.table);library(digest);library(jsonlite)})
source("scripts/p15/annotation_followup_20260911/crs_cashflows.R")
args<-commandArgs(TRUE);base<-if(length(args))args[1]else"data-derived/p15_crs_application_20260911_v4"
out<-if(length(args)>1L)args[2]else"data-derived/p15_crs_date_grid_audit_20260911_v1"
stopifnot(!dir.exists(out));dir.create(out,recursive=TRUE)
x<-fread(file.path(base,"loan_valuations.csv"));f<-fread(file.path(base,"cash_flows.csv"))
first<-as.Date(x$first_date);last<-as.Date(x$final_date)
idx0<-as.integer(format(first,"%Y"))*12L+as.integer(format(first,"%m"))-1L
idx1<-as.integer(format(last,"%Y"))*12L+as.integer(format(last,"%m"))-1L
step<-12L/x$frequency; target<-idx0+round((idx1-idx0)/step)*step
start<-as.Date(sprintf("%04d-%02d-01",target%/%12L,target%%12L+1L))
nextstart<-as.Date(sprintf("%04d-%02d-01",(target+1L)%/%12L,(target+1L)%%12L+1L))
predicted<-start+pmin(as.integer(format(first,"%d")),as.integer(nextstart-start))-1L
x[,final_grid_deviation_days:=as.integer(last-predicted)]
x[,`:=`(zero_tolerance_changes_payment_count=final_grid_deviation_days>0&final_grid_deviation_days<=7,
 strict0_ge5=ge_fixed5_pct,strict0_ge_market=ge_market_pct,strict0_ge_standardized=ge_standardized_pct,
 first_is_month_end=as.integer(format(first+1L,"%d"))==1L)]
for(j in which(x$zero_tolerance_changes_payment_count)) {
 a<-x[j]; z<-crs_cashflows(a$commitment_date,a$first_date,a$final_date,a$interest_rate_pct,a$frequency,a$repayment_type,final_tolerance_days=0L)
 x$strict0_ge5[j]<-crs_ge(z,5);x$strict0_ge_market[j]<-crs_ge(z,if(a$matched)a$benchmark_selected_rate_pct else NA_real_)
 x$strict0_ge_standardized[j]<-crs_ge(z,a$standardized_reference_rate_pct)
}
x[,`:=`(strict0_delta_standardized=strict0_ge_market-strict0_ge_standardized,
 strict0_delta_fixed5=strict0_ge_market-strict0_ge5)]
q<-x[matched==TRUE&non_peer==TRUE]
summary<-q[,.(financing_records=.N,changed_count=sum(zero_tolerance_changes_payment_count),
 baseline_standardized_delta=mean(delta_standardized_ge_pp),strict0_standardized_delta=mean(strict0_delta_standardized),
 baseline_fixed5_delta=mean(delta_fixed5_ge_pp),strict0_fixed5_delta=mean(strict0_delta_fixed5),
 largest_absolute_individual_standardized_delta_change=max(abs(strict0_delta_standardized-delta_standardized_ge_pp))),by=period]
deviations<-x[,.(financing_records=.N,matched_nonpeer=sum(matched&non_peer)),by=.(final_grid_deviation_days,first_is_month_end)]
# Independent source-endpoint and balance identities on the written flows.
lastflows<-f[,.(first_principal_date=payment_date[which(principal>0)[1]],final_payment_date=tail(payment_date,1),principal_sum=sum(principal)),by=loan_id]
z<-merge(lastflows,x[,.(loan_id,first_date,final_date)],by="loan_id")
checks<-data.table(check=c("all_first_principal_dates_preserved","all_final_dates_preserved","all_principal_repaid","zero_tolerance_summary_finite"),
 passed=c(all(as.Date(z$first_principal_date)==z$first_date),all(as.Date(z$final_payment_date)==z$final_date),
 all(abs(z$principal_sum-100)<1e-7),all(is.finite(summary$strict0_standardized_delta))))
fwrite(checks,file.path(out,"checks.csv"));stopifnot(all(checks$passed))
fwrite(summary,file.path(out,"same_sample_zero_tolerance_summary.csv"));fwrite(deviations,file.path(out,"source_date_deviations.csv"))
fwrite(x[,.(loan_id,DonorName,commitment_year,first_date,final_date,frequency,repayment_type,first_is_month_end,
 final_grid_deviation_days,zero_tolerance_changes_payment_count,matched,non_peer,ge_fixed5_pct,strict0_ge5,
 delta_standardized_ge_pp,strict0_delta_standardized,delta_fixed5_ge_pp,strict0_delta_fixed5)],file.path(out,"record_date_grid_audit.csv"))
manifest<-function(ps)data.table(path=ps,sha256=vapply(ps,function(f)digest(file=f,algo="sha256"),character(1)),bytes=file.info(ps)$size)
fwrite(manifest(file.path(base,c("loan_valuations.csv","cash_flows.csv"))),file.path(out,"input_manifest.csv"))
fwrite(manifest(c("scripts/p15/annotation_followup_20260911/audit_crs_date_grid.R","scripts/p15/annotation_followup_20260911/crs_cashflows.R")),file.path(out,"code_manifest.csv"))
writeLines(capture.output(sessionInfo()),file.path(out,"environment.txt"))
write_json(list(build_id=basename(out),schema_id="SCHEMA-P15-CRS-DATE-GRID-AUDIT-V1",estimator_id="EST-P15-CRS-FINAL-GRID-TOLERANCE-SENSITIVITY-V1",
 admissibility_id="ADM-UNCHANGED-CRS-SAMPLE",selection_id="SEL-UNCHANGED-CRS-SAMPLE",source_package_ids=basename(base),
 lifecycle_status="diagnostic",release_state="private_research"),file.path(out,"version_bundle.json"),pretty=TRUE,auto_unbox=TRUE)
fwrite(manifest(list.files(out,full.names=TRUE)),file.path(out,"output_manifest.csv"));print(summary)
