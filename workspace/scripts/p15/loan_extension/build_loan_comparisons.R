source("R/p15_current_inputs.R")
#!/usr/bin/env Rscript
source("scripts/p15/activate_p15_environment.R")
suppressPackageStartupMessages({library(data.table);library(jsonlite);library(digest)})
source("scripts/p15/loan_extension/loan_valuation.R")
source("scripts/p15/loan_extension/benchmark_matching.R")
args<-commandArgs(TRUE)
out<-if(length(args))args[1] else file.path("data-derived",paste0("p15_loan_comparisons_",format(Sys.time(),"%Y%m%d_%H%M%S")))
standardized_design<-"docs/thesis_design/standardized_reference_choice_2026-09-10/DECISION_AND_IMPLEMENTATION.md"
stopifnot(file.exists(standardized_design))
stopifnot(!dir.exists(out));dir.create(out,recursive=TRUE)
normalizer_scripts<-paste0("scripts/p15/loan_extension/normalize_",c("mpg","aiddata","add"),".R")
normdirs<-setNames(file.path(out,"normalization",c("mpg","aiddata","add")),c("mpg","aiddata","add"))
dir.create(file.path(out,"normalization"))
for(k in seq_along(normdirs)) {
 rc<-system2(file.path(R.home("bin"),"Rscript"),c(shQuote(normalizer_scripts[k]),shQuote(normdirs[k])),
   stdout=file.path(out,paste0(names(normdirs)[k],"_normalization.log")),stderr=file.path(out,paste0(names(normdirs)[k],"_normalization.log")))
 if(rc!=0)stop("Normalization failed: ",names(normdirs)[k])
 stopifnot(all(fread(file.path(normdirs[k],"checks.csv"))$passed))
}
raw<-rbindlist(lapply(normdirs,function(p)fread(file.path(p,"normalized_loans.csv"),na.strings="",
  colClasses=c(loan_id="character",loan_event_id="character",iso3="character",currency="character"))),fill=TRUE)
raw[,`:=`(source_loan_id=loan_id,loan_id=paste(dataset,loan_id,sep=":"),loan_event_id=ifelse(is.na(loan_event_id)|!nzchar(loan_event_id),NA_character_,paste(dataset,loan_event_id,sep=":")))]
stopifnot(!anyDuplicated(raw$loan_id))
base<-Sys.getenv("P15_ANALYSIS_BASE");dacbase<-Sys.getenv("P15_DAC_BASE")
if(!nzchar(base)||!nzchar(dacbase)) {
 current<-fromJSON("data-derived/p15_master/current_run.json")
 if(!nzchar(base))base<-current$candidate
 if(!nzchar(dacbase))dacbase<-current$stages$dac$dir
}
paths<-file.path(base,c("selected_reference.csv","tier_eligibility.csv","core_evidence.csv"))
ref<-p15_loan_benchmark_reference(fread(paths[1]),fread(paths[2]),fread(paths[3]))
reference<-ref$country_year
keep<-c("iso3","analysis_year","benchmark_selected_tier","benchmark_selected_rate_pct",
 "benchmark_selected_ordinary_usable","historical_lmic_reporting_scope","primary_or_ids_ordinary_usable",
 "any_observed_ordinary_usable","benchmark_currency_basis","benchmark_timing_basis","secondary_ordinary_use_hold")
reference<-reference[,..keep];setnames(reference,"analysis_year","commitment_year")
x<-merge(raw,reference,by=c("iso3","commitment_year"),all.x=TRUE,sort=FALSE)
policy_path<-file.path(dacbase,"map.csv")
policy<-fread(policy_path)[,.(iso3,commitment_year=analysis_year,group_rate_pct,
 historical_headline_new_loan_rate_pct,dac_eligible,dac_group,regime)]
stopifnot(!anyDuplicated(policy[,.(iso3,commitment_year)]))
policy[,applied_policy_rate_pct:=loan_policy_discount_rates(commitment_year,
 historical_headline_new_loan_rate_pct,dac_eligible)]
# The owner's standardized analytical reference applies the same 9/7/6 rule
# to each year's recipient category. It is separate from historical policy.
policy[,standardized_category_rate_pct:=fifelse(dac_eligible==TRUE &
 commitment_year>=2012L & commitment_year<=2024L & is.finite(group_rate_pct),
 group_rate_pct,NA_real_)]
stopifnot(all(policy$standardized_category_rate_pct[is.finite(policy$standardized_category_rate_pct)]%in%c(6,7,9)))
x<-merge(x,policy,by=c("iso3","commitment_year"),all.x=TRUE,sort=FALSE)
# Normalize the explicit first-payment field without changing source grace.
if(!"first_principal_payment_years"%in%names(x))x[,first_principal_payment_years:=NA_real_]
if("first_principal_years"%in%names(x))x[is.na(first_principal_payment_years),first_principal_payment_years:=first_principal_years]
x[,calculation_terms_available:=is.finite(interest_rate_pct)&interest_rate_pct>=0&is.finite(maturity_years)&maturity_years>0&
    is.finite(first_principal_payment_years)&first_principal_payment_years>=0&first_principal_payment_years<=maturity_years]
x[,`:=`(matched=normalization_eligible==TRUE & calculation_terms_available & benchmark_selected_ordinary_usable==TRUE,
    non_peer=benchmark_selected_tier!="peer",primary_ids=benchmark_selected_tier%in%c("primary","ids"),
    period=fcase(commitment_year<2018,"2012_2017",commitment_year<=2021,"2018_2021",default="2022_2024"))]
x[is.na(matched),matched:=FALSE]
for(nm in c("source_imputed_terms","zero_rate_uncertain","central_scope_eligible","original_fixed_usd_screen"))
 if(!nm%in%names(x))x[,(nm):=FALSE]
x[,cohort:=fcase(dataset=="mpg","mpg_original_loans",dataset=="aiddata"&interest_type=="fixed","aiddata_fixed_usd",
 dataset=="aiddata"&startsWith(interest_type,"variable"),"aiddata_variable_origination_usd",dataset=="aiddata","aiddata_unknown_origination_usd",
 dataset=="add"&central_scope_eligible==TRUE,"add_central_all_currency",default="add_other_borrowers")]
x[,main_cohort:=cohort%in%c("mpg_original_loans","aiddata_fixed_usd","add_central_all_currency")]
ge_cols<-c("ge_fixed5_pct","ge_policy_pct","ge_standardized_category_pct","ge_market_pct","ge_source_replay5_pct","ge_explicit5_pct","ge_explicit_market_pct")
for(nm in ge_cols)x[,(nm):=NA_real_]
flows<-list();audit<-list()
for(j in which(x$calculation_terms_available)) {
 a<-x[j];c<-a$interest_rate_pct;m<-a$maturity_years;f<-a$first_principal_payment_years
 x$ge_fixed5_pct[j]<-loan_ge(c,m,f,5)
 if(is.finite(a$applied_policy_rate_pct) && isTRUE(a$dac_eligible))x$ge_policy_pct[j]<-loan_ge(c,m,f,a$applied_policy_rate_pct)
 if(is.finite(a$standardized_category_rate_pct))x$ge_standardized_category_pct[j]<-loan_ge(c,m,f,a$standardized_category_rate_pct)
 if(is.finite(a$benchmark_selected_rate_pct))x$ge_market_pct[j]<-loan_ge(c,m,f,a$benchmark_selected_rate_pct)
 # Saved source values are distinct from reconciled schedules; do not overwrite them.
 if(a$dataset=="mpg" && is.finite(a$source_original_maturity)&&is.finite(a$source_original_grace))
   x$ge_source_replay5_pct[j]<-loan_ge(c,a$source_original_maturity,a$source_original_grace+.5,5)
 else if(a$dataset=="aiddata")x$ge_source_replay5_pct[j]<-loan_ge(c,a$source_original_maturity,a$source_original_grace,5)
 ff<-loan_explicit_schedule(c,m,f)
 x$ge_explicit5_pct[j]<-loan_explicit_ge(ff,5)
 if(is.finite(a$benchmark_selected_rate_pct))x$ge_explicit_market_pct[j]<-loan_explicit_ge(ff,a$benchmark_selected_rate_pct)
 ff$loan_id<-a$loan_id;flows[[length(flows)+1]]<-as.data.table(ff)
 audit[[length(audit)+1]]<-data.table(loan_id=a$loan_id,principal_error=abs(sum(ff$principal)-100),
  final_time_error=abs(max(ff$payment_time_years)-m),monotonic=loan_ge(c,m,f,6)>loan_ge(c,m,f,5))
}
x[,`:=`(delta_fixed5_ge_pp=ge_market_pct-ge_fixed5_pct,delta_policy_ge_pp=ge_market_pct-ge_policy_pct,
  delta_standardized_category_ge_pp=ge_market_pct-ge_standardized_category_pct,
  repayment_reconstruction_at5_pp=ge_fixed5_pct-source_saved_ge_pct,
  source_replay_error_pp=ge_source_replay5_pct-source_saved_ge_pct,
  explicit_vs_formula_at5_pp=ge_explicit5_pct-ge_fixed5_pct,
  explicit_vs_formula_delta_pp=(ge_explicit_market_pct-ge_explicit5_pct)-(ge_market_pct-ge_fixed5_pct))]
# Source IMF fields are clipped; compare like-for-like when validating their replay.
x[dataset=="aiddata",source_replay_error_pp:=pmin(100,pmax(0,ge_source_replay5_pct))-source_saved_ge_pct]
x[,source_unclipping_effect_at5_pp:=NA_real_]
x[dataset=="aiddata",`:=`(repayment_reconstruction_at5_pp=ge_fixed5_pct-ge_source_replay5_pct,
 source_unclipping_effect_at5_pp=ge_source_replay5_pct-source_saved_ge_pct)]
x[,final_exclusion_reason:=fcase(normalization_eligible!=TRUE,exclusion_reason,!calculation_terms_available,"unsupported_normalized_schedule",
 !is.finite(benchmark_selected_rate_pct),"no_country_year_benchmark",benchmark_selected_ordinary_usable!=TRUE,"benchmark_ordinary_use_hold",default="included_conditional_comparison")]

meta<-list(build_id=basename(out),schema_id="SCHEMA-P15-LOAN-COMPARISONS-V3",estimator_id="EST-P15-SEMIANNUAL-SOURCE-STYLE-GE-V1",
  admissibility_id="ADM-P15-LOAN-SOURCE-SUITABILITY-20260910-V1",selection_id="SEL-P15-LOAN-NONPEER-MAIN-FULL-SENSITIVITY-V1",
  lifecycle_status="diagnostic",release_state="private_research_loan_comparisons")
write_table<-function(z,name) {
 z<-copy(as.data.table(z));if(!nrow(z)&&!ncol(z))z<-data.table(no_rows=character())
 for(nm in names(meta))set(z,j=nm,value=rep(meta[[nm]],nrow(z)))
 fwrite(z,file.path(out,paste0(name,".csv")),na="")
}
write_table(x,"loan_valuations")
write_table(x[matched==FALSE],"excluded_records")
write_table(rbindlist(flows),"explicit_cash_flows")
schedule_audit<-rbindlist(audit);write_table(schedule_audit,"schedule_checks")
write_table(x[,.(source_records=.N,terms_available=sum(calculation_terms_available),source_eligible=sum(normalization_eligible),
 matched_records=sum(matched),nonpeer_matched=sum(matched&non_peer,na.rm=TRUE),primary_ids_matched=sum(matched&primary_ids),
 peer_only_matched=sum(matched&!non_peer,na.rm=TRUE),modern_dac_matched=sum(matched&commitment_year>=2018&is.finite(ge_policy_pct)),
 reported_usd_matched=sum(matched&currency=="USD",na.rm=TRUE)),by=.(dataset,cohort,main_cohort)],"sample_coverage")
write_table(x[,.(records=.N),by=.(dataset,cohort,final_exclusion_reason)],"exclusion_summary")

# Each row in the long comparison always contains the same loan under both rates.
keys<-c("loan_id","loan_event_id","dataset","cohort","main_cohort","iso3","commitment_year","period","creditor","currency",
 "amount_usd","matched","non_peer","primary_ids","benchmark_selected_tier","source_imputed_terms","zero_rate_uncertain",
 "historical_lmic_reporting_scope","ge_market_pct","ge_fixed5_pct","ge_policy_pct","group_rate_pct","applied_policy_rate_pct",
 "standardized_category_rate_pct","ge_standardized_category_pct")
l5<-copy(x[matched==TRUE,..keys]);l5[,`:=`(reference="fixed5",reference_ge_pct=ge_fixed5_pct,reference_discount_rate_pct=5)]
lp<-copy(x[matched==TRUE&is.finite(ge_policy_pct),..keys]);lp[,`:=`(reference=ifelse(commitment_year>=2018,"modern_DAC_common_reference","historical_10pct_convention"),reference_ge_pct=ge_policy_pct,reference_discount_rate_pct=applied_policy_rate_pct)]
ls<-copy(x[matched==TRUE&is.finite(ge_standardized_category_pct),..keys]);ls[,`:=`(reference="standardized_DAC_category_rule",reference_ge_pct=ge_standardized_category_pct,reference_discount_rate_pct=standardized_category_rate_pct)]
long<-rbindlist(list(l5,lp,ls),fill=TRUE)
setnames(long,"ge_market_pct","market_ge_pct")
long[,delta_ge_pp:=market_ge_pct-reference_ge_pct]
views<-rbindlist(list(long[,benchmark_view:="all_selected"],copy(long)[non_peer==TRUE][,benchmark_view:="non_peer"],
 copy(long)[primary_ids==TRUE][,benchmark_view:="primary_ids"]),fill=TRUE)
write_table(views,"paired_comparisons")
write_table(views[,loan_summary(.SD),by=.(dataset,cohort,main_cohort,benchmark_view,reference)],"comparison_summary")
write_table(views[,loan_summary(.SD),by=.(dataset,cohort,benchmark_view,reference,period)],"period_summary")
write_table(views[,loan_summary(.SD),by=.(dataset,cohort,benchmark_view,reference,creditor)],"creditor_summary")
write_table(views[,loan_summary(.SD),by=.(dataset,cohort,benchmark_view,reference,commitment_year)],"annual_summary")
quality<-rbindlist(list(views[currency=="USD"][,sensitivity:="reported_USD_only"],
 views[historical_lmic_reporting_scope==TRUE][,sensitivity:="historical_LMIC_only"],
 views[is.na(source_imputed_terms)|source_imputed_terms==FALSE][,sensitivity:="exclude_flagged_source_term_imputation"],
 views[is.na(zero_rate_uncertain)|zero_rate_uncertain==FALSE][,sensitivity:="exclude_uncertain_zero_rates"]),fill=TRUE)
write_table(quality[,loan_summary(.SD),by=.(dataset,cohort,benchmark_view,reference,sensitivity)],"quality_sensitivity_summary")
write_table(x[matched==TRUE,.(records=.N,mean_source_saved5_pct=mean(source_saved_ge_pct,na.rm=TRUE),
 mean_reconstructed5_pct=mean(ge_fixed5_pct),mean_reconstruction_delta_pp=mean(repayment_reconstruction_at5_pp,na.rm=TRUE),
 mean_unclipping_source5_pp=mean(source_unclipping_effect_at5_pp,na.rm=TRUE),
 mean_explicit_at5_delta_pp=mean(explicit_vs_formula_at5_pp),max_abs_explicit_at5_delta_pp=max(abs(explicit_vs_formula_at5_pp)),
 mean_explicit_market_minus5_delta_pp=mean(explicit_vs_formula_delta_pp)),by=.(dataset,cohort,creditor)],"repayment_reconciliation_summary")
write_table(x[calculation_terms_available==TRUE,.(records=.N,source_eligible=sum(normalization_eligible),matched=sum(matched),
 full_usable_mean5_pct=mean(ge_fixed5_pct[normalization_eligible]),matched_mean5_pct=mean(ge_fixed5_pct[matched]),
 nonpeer_matched_mean5_pct=mean(ge_fixed5_pct[matched&non_peer],na.rm=TRUE)),by=.(dataset,cohort)],"sample_restriction_at5")

# Same-country-year creditor means: retain separate datasets and unweighted loan means.
pair_source<-x[matched==TRUE&non_peer==TRUE&main_cohort==TRUE,
 .(records=.N,mean_market=mean(ge_market_pct),mean_fixed5=mean(ge_fixed5_pct)),by=.(dataset,iso3,commitment_year,creditor)]
pairs<-merge(pair_source,pair_source,by=c("dataset","iso3","commitment_year"),allow.cartesian=TRUE,suffixes=c("_a","_b"))
pairs<-pairs[creditor_a<creditor_b]
pairs[,`:=`(creditor_gap_market=mean_market_a-mean_market_b,creditor_gap_fixed5=mean_fixed5_a-mean_fixed5_b)]
pairs[,change_in_creditor_gap_pp:=creditor_gap_market-creditor_gap_fixed5]
write_table(pairs,"same_country_year_creditor_pairs")
write_table(pairs[,.(borrower_year_pairs=.N,countries=uniqueN(iso3),mean_creditor_gap_fixed5=mean(creditor_gap_fixed5),
 mean_creditor_gap_market=mean(creditor_gap_market),mean_gap_change_pp=mean(change_in_creditor_gap_pp)),by=.(dataset,creditor_a,creditor_b)],"creditor_pair_summary")

# All available tiers on identical loans, without substituting a new hierarchy.
tr<-copy(ref$tiers);setnames(tr,"analysis_year","commitment_year")
tr<-tr[ordinary_cost_tier_usable==TRUE]
ti<-merge(x[matched==TRUE,.(loan_id,dataset,cohort,iso3,commitment_year,interest_rate_pct,maturity_years,first_principal_payment_years,
 ge_fixed5_pct,ge_market_pct,benchmark_selected_tier)],tr[,.(iso3,commitment_year,tier,rate_pct=benchmark_tier_rate_pct)],by=c("iso3","commitment_year"),allow.cartesian=TRUE)
ti[,tier_ge_pct:=mapply(loan_ge,interest_rate_pct,maturity_years,first_principal_payment_years,rate_pct)]
ti[,`:=`(tier_minus_selected_ge_pp=tier_ge_pct-ge_market_pct,tier_minus5_ge_pp=tier_ge_pct-ge_fixed5_pct)]
write_table(ti,"same_loan_all_tier_valuations")
write_table(ti[,.(records=.N,countries=uniqueN(iso3),mean_tier_minus_selected_ge_pp=mean(tier_minus_selected_ge_pp),mean_tier_minus5_ge_pp=mean(tier_minus5_ge_pp)),by=.(dataset,cohort,tier)],"same_loan_tier_summary")

# Alternative time anchor for specifically identified MPG latest-effectiveness anomalies.
alternatives<-x[dataset=="mpg"&normalization_eligible==FALSE&agreement_anchor_alternative_eligible==TRUE]
if(nrow(alternatives)) {
 alternatives[,`:=`(alternative5=mapply(loan_ge,interest_rate_pct,agreement_anchor_maturity_years,agreement_anchor_grace_years,5),
  alternative_market=mapply(loan_ge,interest_rate_pct,agreement_anchor_maturity_years,agreement_anchor_grace_years,benchmark_selected_rate_pct))]
 alternatives[,alternative_delta_pp:=alternative_market-alternative5]
}
write_table(alternatives,"mpg_agreement_anchor_alternatives")
checks<-data.table(check=c("all_normalized_rows_preserved","unique_dataset_loan_keys","cashflow_principal_conservation",
 "cashflow_endpoints_preserved","discount_monotonicity","finite_all_matched_values","same_loan_policy_difference_identity",
 "non_peer_main_contains_no_peers","modern_DAC_never_used_before_2018","historical_reference_actually_10pct",
 "modern_reference_uses_recipient_category_rate","historical_values_reproduce_at_10pct",
 "excluded_records_retained","ADD_event_ids_not_fabricated","all_normalizer_checks_pass",
 "unique_loan_within_each_reference_and_view","standardized_category_uses_contemporaneous_eligible_map",
 "standardized_view_covers_all_matched_category_eligible_years","standardized_modern_values_equal_modern_DAC",
 "standardized_pre2018_is_distinct_from_historical_10pct","comparison_reference_rate_matches_reference"),
 passed=c(nrow(x)==nrow(raw),!anyDuplicated(x$loan_id),all(schedule_audit$principal_error<1e-8),all(schedule_audit$final_time_error<1e-8),
 all(schedule_audit$monotonic),all(is.finite(x$ge_fixed5_pct[x$matched])&is.finite(x$ge_market_pct[x$matched])),
 all(abs(views$delta_ge_pp-(views$market_ge_pct-views$reference_ge_pct))<1e-10),!any(views[benchmark_view=="non_peer",benchmark_selected_tier]=="peer"),
 !any(views[reference=="modern_DAC_common_reference",commitment_year]<2018),
 all(views[reference=="historical_10pct_convention",applied_policy_rate_pct]==10),
 all(views[reference=="modern_DAC_common_reference",abs(applied_policy_rate_pct-group_rate_pct)]<1e-10),
 all(abs(x[matched==TRUE&commitment_year<2018&is.finite(ge_policy_pct),
  ge_policy_pct-mapply(loan_ge,interest_rate_pct,maturity_years,first_principal_payment_years,10)])<1e-10),
 sum(!x$matched)==nrow(x[matched==FALSE]),all(is.na(x[dataset=="add",loan_event_id])),TRUE,
 !anyDuplicated(views[,.(loan_id,reference,benchmark_view)]),
 all(x[is.finite(standardized_category_rate_pct),dac_eligible==TRUE & commitment_year>=2012L & commitment_year<=2024L &
  abs(standardized_category_rate_pct-group_rate_pct)<1e-10]),
 nrow(ls)==x[matched==TRUE&dac_eligible==TRUE&commitment_year>=2012L&commitment_year<=2024L&is.finite(group_rate_pct),.N],
 all(x[commitment_year>=2018L&is.finite(ge_policy_pct),abs(ge_standardized_category_pct-ge_policy_pct)]<1e-10),
 all(x[commitment_year<2018L&is.finite(ge_standardized_category_pct),ge_standardized_category_pct<ge_policy_pct]),
 all(views[reference=="fixed5",reference_discount_rate_pct]==5) &&
  all(views[reference%in%c("modern_DAC_common_reference","historical_10pct_convention"),reference_discount_rate_pct==applied_policy_rate_pct]) &&
  all(views[reference=="standardized_DAC_category_rule",reference_discount_rate_pct==standardized_category_rate_pct])))
write_table(checks,"checks");stopifnot(all(checks$passed))
getpaths<-function(p){m<-fread(p);as.character(m[[intersect(c("path","artifact_path"),names(m))[1]]])}
inputs<-unique(c(paths,policy_path,"docs/thesis_design/loan_valuations_2026-09-10/COMPARISON_DESIGN.md",standardized_design,
 unlist(lapply(normdirs,function(p)getpaths(file.path(p,"input_manifest.csv"))))))
code<-unique(c("scripts/p15/loan_extension/build_loan_comparisons.R","scripts/p15/loan_extension/loan_valuation.R",
 "scripts/p15/loan_extension/benchmark_matching.R","scripts/p15/activate_p15_environment.R","renv.lock",normalizer_scripts,
 unlist(lapply(normdirs,function(p)getpaths(file.path(p,"code_manifest.csv"))))))
manifest<-function(ps)data.table(path=ps,sha256=vapply(ps,function(p)digest(file=p,algo="sha256"),character(1)),bytes=file.info(ps)$size)
fwrite(manifest(inputs),file.path(out,"input_manifest.csv"));fwrite(manifest(code),file.path(out,"code_manifest.csv"))
write_json(c(meta,list(valuation_scope="Matched constant-coupon semiannual source-style scenarios; no change to accepted aggregate baseline",
 standardized_reference="standardized_DAC_category_rule applies 9/7/6 to each year's eligible recipient category across 2012-2024; not an assertion of historical DAC practice before 2018",
 historical_policy_reference="ge_policy_pct and applied_policy_rate_pct retain the historical headline convention: 10 percent before 2018 and contemporaneous category rates thereafter",
 comparison_rate_field="reference_discount_rate_pct identifies the discount rate used in each paired comparison",
 source_packages=unique(raw$source_package_ids))),file.path(out,"version_bundle.json"),auto_unbox=TRUE,pretty=TRUE)
writeLines(capture.output(sessionInfo()),file.path(out,"environment.txt"))
fwrite(manifest(list.files(out,recursive=TRUE,full.names=TRUE)),file.path(out,"output_manifest.csv"))
cat("Completed loan comparisons:",out,"\n")
