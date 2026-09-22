#!/usr/bin/env Rscript
source("scripts/p15/activate_p15_environment.R")
suppressPackageStartupMessages({library(data.table);library(digest);library(jsonlite)})
args<-commandArgs(TRUE)
old_dir<-if(length(args)>=1)args[1] else "data-derived/p15_loan_comparisons_20260910_v4"
new_dir<-if(length(args)>=2)args[2] else "data-derived/p15_loan_comparisons_20260910_v5"
out<-if(length(args)>=3)args[3] else "data-derived/p15_standardized_loan_reference_verification_20260910_v1"
stopifnot(!dir.exists(out));dir.create(out,recursive=TRUE)
new_reference<-"standardized_DAC_category_rule"
files<-setdiff(list.files(old_dir,pattern="\\.csv$"),c("input_manifest.csv","code_manifest.csv","output_manifest.csv"))
metadata<-c("build_id","schema_id")
table_checks<-rbindlist(lapply(files,function(f) {
 a<-fread(file.path(old_dir,f),na.strings="");b<-fread(file.path(new_dir,f),na.strings="")
 if("reference"%in%names(b))b<-b[reference!=new_reference]
 if(f=="checks.csv")b<-b[check%in%a$check]
 cols<-setdiff(names(a),metadata)
 stopifnot(all(cols%in%names(b)))
 a<-a[,..cols];b<-b[,..cols]
 # Compare complete multisets, including repeated rows, independently of row order.
 if(nrow(a))setorderv(a,cols)
 if(nrow(b))setorderv(b,cols)
 data.table(table=f,previous_rows=nrow(a),preserved_rows=nrow(b),
   compared_columns=length(cols),unchanged=isTRUE(all.equal(a,b,tolerance=1e-12,check.attributes=FALSE)))
}))
fwrite(table_checks,file.path(out,"table_preservation.csv"))

x<-fread(file.path(new_dir,"loan_valuations.csv"),na.strings="")
p<-fread(file.path(new_dir,"paired_comparisons.csv"),na.strings="")
s<-p[reference==new_reference]
eligible<-x[matched==TRUE&dac_eligible==TRUE&commitment_year>=2012L&commitment_year<=2024L&is.finite(group_rate_pct)]
old_manifest<-fread(file.path(old_dir,"input_manifest.csv"))
new_manifest<-fread(file.path(new_dir,"input_manifest.csv"))
original_design<-"docs/thesis_design/loan_valuations_2026-09-10/COMPARISON_DESIGN.md"
supplement<-"docs/thesis_design/standardized_reference_choice_2026-09-10/DECISION_AND_IMPLEMENTATION.md"

# Independently discount the saved cash flows where endpoints lie on exact
# semiannual dates. Here the explicit schedule and source formula should agree.
grid<-x[is.finite(ge_standardized_category_pct)&
 abs(maturity_years*2-round(maturity_years*2))<1e-10 &
 abs(first_principal_payment_years*2-round(first_principal_payment_years*2))<1e-10,
 .(loan_id,standardized_category_rate_pct,ge_standardized_category_pct)]
flows<-fread(file.path(new_dir,"explicit_cash_flows.csv"))
independent<-merge(flows,grid,by="loan_id",allow.cartesian=TRUE)
independent<-independent[,.(independent_ge_pct=100-sum(debt_service/(1+standardized_category_rate_pct/100)^payment_time_years),
 recorded_ge_pct=first(ge_standardized_category_pct)),by=loan_id]
independent[,absolute_difference_pp:=abs(independent_ge_pct-recorded_ge_pct)]
fwrite(independent,file.path(out,"independent_grid_cashflow_check.csv"))

checks<-data.table(check=c("every_previous_data_table_preserved",
 "no_duplicate_loan_within_reference_and_view","standardized_all_selected_has_exact_eligible_loan_set",
 "standardized_rate_is_current_year_group_rate","standardized_reference_rates_are_6_7_9",
 "ineligible_or_unmapped_standardized_rates_missing","modern_reference_values_equal_standardized",
 "pre2018_historical_applied_rate_still_ten","fixed5_comparison_rate_is_five",
 "all_historical_modern_comparison_rates_match_applied_policy",
 "all_standardized_comparison_rates_match_standardized_category",
 "standardized_reference_paired_identity","independent_halfyear_cashflows_match_standardized_formula",
 "original_comparison_design_hash_unchanged","new_authorized_supplement_is_manifested",
 "all_new_producer_checks_pass","schema_v3_explicit"),
 passed=c(all(table_checks$unchanged),!anyDuplicated(p[,.(loan_id,reference,benchmark_view)]),
 setequal(s[benchmark_view=="all_selected",loan_id],eligible$loan_id),
 all(x[is.finite(standardized_category_rate_pct),abs(standardized_category_rate_pct-group_rate_pct)]<1e-10),
 all(s$reference_discount_rate_pct%in%c(6,7,9)),
 !any(x[is.na(dac_eligible)|dac_eligible==FALSE|!is.finite(group_rate_pct),is.finite(standardized_category_rate_pct)]),
 all(x[commitment_year>=2018L&is.finite(ge_policy_pct),abs(ge_policy_pct-ge_standardized_category_pct)]<1e-10),
 all(x[commitment_year<2018L&is.finite(ge_policy_pct),applied_policy_rate_pct]==10),
 all(p[reference=="fixed5",reference_discount_rate_pct]==5),
 all(p[reference%in%c("historical_10pct_convention","modern_DAC_common_reference"),reference_discount_rate_pct==applied_policy_rate_pct]),
 all(s$reference_discount_rate_pct==s$standardized_category_rate_pct),
 all(abs(s$delta_ge_pp-(s$market_ge_pct-s$ge_standardized_category_pct))<1e-10),
 nrow(independent)>0L&&all(independent$absolute_difference_pp<1e-8),
 identical(old_manifest[path==original_design,sha256],new_manifest[path==original_design,sha256]),
 supplement%in%new_manifest$path&&new_manifest[path==supplement,sha256]==digest(file=supplement,algo="sha256"),
 all(fread(file.path(new_dir,"checks.csv"))$passed),
 identical(unique(p$schema_id),"SCHEMA-P15-LOAN-COMPARISONS-V3")))
fwrite(checks,file.path(out,"checks.csv"));stopifnot(all(checks$passed))
fwrite(s[benchmark_view=="non_peer",.(records=.N,countries=uniqueN(iso3),
 mean_market_ge_pct=mean(market_ge_pct),mean_reference_ge_pct=mean(reference_ge_pct),
 mean_delta_ge_pp=mean(delta_ge_pp)),by=.(dataset,cohort,period)],file.path(out,"standardized_period_summary.csv"))
inputs<-c(file.path(old_dir,files),file.path(new_dir,files),
 file.path(old_dir,"input_manifest.csv"),file.path(new_dir,"input_manifest.csv"))
manifest<-function(ps)data.table(path=ps,sha256=vapply(ps,function(q)digest(file=q,algo="sha256"),character(1)),bytes=file.info(ps)$size)
fwrite(manifest(inputs),file.path(out,"input_manifest.csv"))
fwrite(manifest(c("scripts/p15/loan_extension/verify_standardized_loan_reference.R","renv.lock")),file.path(out,"code_manifest.csv"))
write_json(list(build_id=basename(out),lifecycle_status="diagnostic",previous_reference=old_dir,new_reference=new_dir,
 old_tables_checked=nrow(table_checks),independent_halfyear_records=nrow(independent),
 maximum_cashflow_difference_pp=max(independent$absolute_difference_pp)),file.path(out,"version_bundle.json"),auto_unbox=TRUE,pretty=TRUE)
writeLines(capture.output(sessionInfo()),file.path(out,"environment.txt"))
fwrite(manifest(list.files(out,full.names=TRUE)),file.path(out,"output_manifest.csv"))
cat("Standardized-reference verification passed:",nrow(checks),"checks;",nrow(table_checks),
 "previous tables preserved;",nrow(independent),"independent halfyear cash-flow valuations\n")
