#!/usr/bin/env Rscript
# Read-only audit of the preserved v3 loan outputs and earlier aggregate applications.
source("scripts/p15/activate_p15_environment.R")
suppressPackageStartupMessages({library(data.table); library(jsonlite); library(digest)})
source("scripts/p15/loan_extension/loan_valuation.R")
args <- commandArgs(TRUE)
out <- if(length(args)) args[1] else "data-derived/p15_policy_history_audit_20260910_v1"
stopifnot(!dir.exists(out)); dir.create(out, recursive=TRUE)
base <- "data-derived/p15_loan_comparisons_20260910_v3"
paths <- c(loans=file.path(base,"loan_valuations.csv"), paired=file.path(base,"paired_comparisons.csv"),
  periods=file.path(base,"period_summary.csv"),
  map="data-derived/p15_master/dac_e3dae7a9e5738549e6f1/map.csv",
  old_policies="data-derived/p15_master/official_policy_13987870a46c5e7f9396/paired_details.csv",
  old_pv="data-derived/p15_master/bullet_35ce98d392e57370a6a5/paired_pv_for_inclusion_check.csv",
  old_headline="data-derived/p15_master/pv_interpretation_26b5dd3c142511d5a34a/analysis_rows.csv",
  bullet_details="data-derived/p15_master/bullet_35ce98d392e57370a6a5/bullet_all_tier_valuations.csv")
stopifnot(all(file.exists(paths)))
write_table <- function(x,n) fwrite(x,file.path(out,paste0(n,".csv")),na="")
x <- fread(paths["loans"]); p <- fread(paths["paired"]); periods <- fread(paths["periods"])
m <- fread(paths["map"]); op <- fread(paths["old_policies"])
b <- fread(paths["old_pv"]); headline <- fread(paths["old_headline"]); bd <- fread(paths["bullet_details"])
x[, period_scope := fifelse(commitment_year<2018,"before_2018","2018_onward")]
write_table(x[,.(records=.N,matched_records=sum(matched),finite_policy_records=sum(is.finite(ge_policy_pct)),
  matched_finite_policy_records=sum(matched&is.finite(ge_policy_pct)),
  actual_group_rates=paste(sort(unique(group_rate_pct[is.finite(ge_policy_pct)])),collapse=";")),
  by=.(dataset,cohort,period_scope)],"raw_policy_scope")
write_table(p[,.(view_rows=.N,unique_records=uniqueN(loan_id),first_year=min(commitment_year),last_year=max(commitment_year),
  actual_group_rates=paste(sort(unique(group_rate_pct)),collapse=";")),
  by=.(dataset,cohort,benchmark_view,reference)],"reference_scope")
pre <- x[commitment_year<2018&is.finite(ge_policy_pct)]
pre[, ge_at_actual_group_rate_pct := mapply(loan_ge,interest_rate_pct,maturity_years,first_principal_payment_years,group_rate_pct)]
pre[, corrected_ge_at_10pct := mapply(loan_ge,interest_rate_pct,maturity_years,first_principal_payment_years,10)]
pre[, `:=`(stored_minus_actual_group_replay_pp=ge_policy_pct-ge_at_actual_group_rate_pct,
  increase_in_policy_ge_at_10_pp=corrected_ge_at_10pct-ge_policy_pct,
  corrected_market_minus_10_pp=ge_market_pct-corrected_ge_at_10pct)]
write_table(pre[,.(loan_id,dataset,cohort,iso3,commitment_year,matched,non_peer,group_rate_pct,
  ge_policy_pct,ge_at_actual_group_rate_pct,corrected_ge_at_10pct,stored_minus_actual_group_replay_pp,
  increase_in_policy_ge_at_10_pp,delta_policy_ge_pp,corrected_market_minus_10_pp)],"pre2018_affected_records")
write_table(pre[matched==TRUE,.(records=.N,non_peer_records=sum(non_peer),
  mean_saved_wrong_reference_gap_pp=mean(delta_policy_ge_pp),
  mean_corrected_historical10_gap_pp=mean(corrected_market_minus_10_pp),
  mean_reference_ge_increase_pp=mean(increase_in_policy_ge_at_10_pp)),by=.(dataset,cohort)],"pre2018_correction_by_cohort")
write_table(pre[matched==TRUE&non_peer==TRUE,.(records=.N,
  mean_saved_wrong_reference_gap_pp=mean(delta_policy_ge_pp),
  mean_corrected_historical10_gap_pp=mean(corrected_market_minus_10_pp),
  mean_reference_ge_increase_pp=mean(increase_in_policy_ge_at_10_pp)),by=.(dataset,cohort)],"pre2018_correction_nonpeer")
write_table(m[,.(country_years=.N,eligible_country_years=sum(dac_eligible),
  historical_headline_rates=paste(sort(unique(historical_headline_new_loan_rate_pct)),collapse=";"),
  group_rates=paste(sort(unique(group_rate_pct)),collapse=";"),
  transition_ge_rates=paste(sort(unique(grant_equivalent_new_loan_rate_pct)),collapse=";")),by=.(analysis_year,regime)],"mapping_regime_audit")
write_table(op[,.(paired_rows=.N,first_year=min(analysis_year),last_year=max(analysis_year),
  before2018_rows=sum(analysis_year<2018),before2018_policy_rates=paste(sort(unique(policy_rate[analysis_year<2018])),collapse=";")),
  by=.(policy,tier)],"earlier_benchmark_validation_scope")
write_table(b[,.(scenario_rows=.N,first_year=min(analysis_year),last_year=max(analysis_year),
  policy_rates=paste(sort(unique(dac_rate_pct)),collapse=";")),by=modern_dac_comparison],"earlier_pv_raw_scope")
write_table(headline[,.(scenario_rows=.N,first_year=min(analysis_year),last_year=max(analysis_year),
  policy_rates=paste(sort(unique(dac_rate_pct)),collapse=";")),by=creditor],"earlier_pv_headline_scope")
write_table(bd[rate_source=="DAC_common_scenario",.(records=.N,first_year=min(analysis_year),last_year=max(analysis_year),
  policy_rates=paste(sort(unique(discount_rate_pct)),collapse=";")),by=policy_period_label],"earlier_bullet_label_audit")
write_table(periods[benchmark_view=="non_peer"&reference=="modern_DAC_common_reference"&
  cohort%in%c("aiddata_fixed_usd","add_central_all_currency")],"unchanged_modern_headlines")
checks <- data.table(check=c("v3_pre2018_raw_rates_are_group_rates","v3_pre2018_labels_do_not_match_10pct_calculation",
  "modern_loan_headlines_have_no_pre2018_records","fixed5_reference_is_separate",
  "mapping_historical_rate_correctly_10_before2018","older_headline_benchmark_comparison_uses_10_before2018",
  "older_modern_benchmark_comparison_excludes_pre2018","older_pv_headline_contains_only2018_onward",
  "older_bullet_pre2018_explicitly_counterfactual"),passed=c(
  max(abs(pre$stored_minus_actual_group_replay_pp))<1e-9,
  all(abs(pre$corrected_ge_at_10pct-pre$ge_policy_pct)>1e-9),
  !any(p[reference=="modern_DAC_common_reference",commitment_year]<2018),
  all(abs(p[reference=="fixed5",reference_ge_pct-ge_fixed5_pct])<1e-9),
  all(m[analysis_year<2018&dac_eligible==TRUE,historical_headline_new_loan_rate_pct]==10),
  all(op[analysis_year<2018&policy=="dac_headline_new",policy_rate]==10),
  !any(op[policy=="dac_modern_2018",analysis_year]<2018),
  min(headline$analysis_year)>=2018,
  all(bd[analysis_year<2018,policy_period_label]=="pre_2018_grouped_counterfactual")))
write_table(checks,"checks"); stopifnot(all(checks$passed))
inputs <- data.table(role=names(paths),path=unname(paths)); inputs[,sha256:=vapply(path,function(z)digest(file=z,algo="sha256"),character(1))]
write_table(inputs,"input_manifest")
code <- c("scripts/p15/loan_extension/audit_policy_rate_history.R","scripts/p15/loan_extension/loan_valuation.R")
write_table(data.table(path=code,sha256=vapply(code,function(z)digest(file=z,algo="sha256"),character(1))),"code_manifest")
write_json(list(build_id=basename(out),audit_subject="preserved v3 loan outputs and September10 earlier aggregate builds",
  release_state="private_research_diagnostic",expected_defect="v3 pre2018 historical10 labels actually contain grouped6/7/9 valuations",
  existing_outputs_changed=FALSE),file.path(out,"version_bundle.json"),pretty=TRUE,auto_unbox=TRUE)
capture.output(sessionInfo(),file=file.path(out,"environment.txt"))
outputs <- list.files(out,full.names=TRUE)
write_table(data.table(path=outputs,sha256=vapply(outputs,function(z)digest(file=z,algo="sha256"),character(1))),"output_manifest")
print(checks); print(pre[,.(raw_records=.N,matched=sum(matched),matched_nonpeer=sum(matched&non_peer)),by=dataset])
