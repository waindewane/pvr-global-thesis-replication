source("R/p15_current_inputs.R")
#!/usr/bin/env Rscript
# Describe existing peer groups; never refit or replace a benchmark.
source("scripts/p15/activate_p15_environment.R")
source("R/research_governance.R")
suppressPackageStartupMessages({library(data.table); library(jsonlite); library(digest)})
args <- commandArgs(TRUE)
out <- if(length(args)) args[1] else file.path("data-derived",paste0("p15_peer_description_",format(Sys.time(),"%Y%m%d_%H%M%S")))
if(dir.exists(out)) stop("Refusing to overwrite: ", out)
candidate <- Sys.getenv("P15_ANALYSIS_BASE",p15_current_input())
inputs <- c(file.path(candidate,c("core_evidence.csv","selected_reference.csv",
  "peer_membership.csv","peer_region_context.csv","tier_eligibility.csv","output_manifest.csv","peer_score_intervals.csv")))
stopifnot(all(file.exists(inputs)))
before <- vapply(inputs,function(p)digest(file=p,algo="sha256"),character(1))
c <- fread(inputs[1],na.strings="")
s <- fread(inputs[2],na.strings="")
m <- fread(inputs[3],na.strings="")
ctx <- fread(inputs[4],na.strings="")
stopifnot(!anyDuplicated(c[,.(analysis_year,iso3)]),!anyDuplicated(s[,.(analysis_year,iso3)]))
x <- merge(c[,.(analysis_year,iso3,country,historical_income_level,historical_lmic_reporting_scope,
  peer_rate_pct,peer_country_count,peer_pool_rule,global_pool_used,peer_rule_number,peer_target_shadow_used,
  peer_primary_donor_count,peer_ids_donor_count,peer_secondary_donor_count,peer_rating_implied_donor_count)],
  s[,.(analysis_year,iso3,selected_tier)],by=c("analysis_year","iso3"))
x <- merge(x,ctx[,.(analysis_year,iso3,rating_source_region,moodys_rating_normalized)],
  by=c("analysis_year","iso3"))
x[, prior_rating_available := !is.na(moodys_rating_normalized) & nzchar(moodys_rating_normalized)]
counted <- m[used_for_estimate %in% TRUE,
  .(member_count=.N,recomputed_median=median(peer_rate_pct),
    member_set=paste(sort(peer_iso3),collapse=";")),by=.(analysis_year,iso3=target_iso3)]
x <- merge(x,counted,by=c("analysis_year","iso3"),all.x=TRUE)
populations <- list(computed_all=x[is.finite(peer_rate_pct)],
  computed_lmic=x[historical_lmic_reporting_scope %in% TRUE & is.finite(peer_rate_pct)],
  selected_peer_lmic=x[historical_lmic_reporting_scope %in% TRUE & selected_tier=="peer"])
periods <- list(full_2012_2024=2012:2024,early_2012_2017=2012:2017,modern_2018_2024=2018:2024)
groups <- c("all","matched_on_characteristics","global")
describe <- function(d) {
  v<-d$peer_country_count
  data.table(country_years=nrow(d),target_countries=uniqueN(d$iso3),
    mean_peer_countries=if(length(v))mean(v) else NA_real_,
    sd_peer_countries=if(length(v)>1)sd(v) else NA_real_,
    median_peer_countries=if(length(v))as.numeric(median(v)) else NA_real_,
    q25_peer_countries=if(length(v))as.numeric(quantile(v,.25,type=7)) else NA_real_,
    q75_peer_countries=if(length(v))as.numeric(quantile(v,.75,type=7)) else NA_real_,
    min_peer_countries=if(length(v))min(v) else NA_real_,
    max_peer_countries=if(length(v))max(v) else NA_real_,
    distinct_year_member_sets=uniqueN(paste(d$analysis_year,d$member_set)),
    target_rows_without_moodys=sum(!d$prior_rating_available))
}
summary <- rbindlist(lapply(names(populations),function(pop)rbindlist(lapply(names(periods),function(per) {
  d<-populations[[pop]][analysis_year %in% periods[[per]]]
  rbindlist(lapply(groups,function(g) {
    z<-switch(g,all=d,matched_on_characteristics=d[global_pool_used==FALSE],global=d[global_pool_used==TRUE])
    a<-describe(z);a[,`:=`(population=pop,period=per,group_type=g)];a
  }))
}))))
selected <- populations$selected_peer_lmic
annual <- rbindlist(lapply(2012:2024,function(y)rbindlist(lapply(c(FALSE,TRUE),function(g) {
  a<-describe(selected[analysis_year==y & global_pool_used==g])
  a[,`:=`(analysis_year=y,global_pool_used=g)];a
}))))
frequency <- selected[,.(country_years=.N),by=.(analysis_year,peer_pool_rule,peer_country_count)]
rule_summary <- rbindlist(lapply(names(periods),function(per) {
  d<-selected[analysis_year %in% periods[[per]]]
  a<-d[,describe(.SD),by=peer_pool_rule];a[,period:=per];a
}))
# Preserve the previous Kosovo2024 illustration while using its actual revised group.
# It now follows Rule2 with seven mixed-source donors and a conditional target score.
# No representativeness or performance claim is made.
example <- merge(m[analysis_year==2024L & target_iso3=="XKX"],
  c[,.(analysis_year,peer_iso3=iso3,peer_country=country)],by=c("analysis_year","peer_iso3"))
example <- example[,.(analysis_year,target_iso3,peer_iso3,peer_country,peer_income,peer_region,
  peer_rating,peer_rate_pct,peer_pool_rule,peer_rule_number,peer_donor_tier,peer_rating_basis,
  peer_matching_notch_lower,peer_matching_notch_upper,peer_currency_basis,peer_timing_basis)]
setorder(example,peer_rate_pct)
example_target <- x[analysis_year==2024L & iso3=="XKX"]
score <- fread(file.path(candidate,"peer_score_intervals.csv"),na.strings="")[analysis_year==2024L&iso3=="XKX"]
example_target[, `:=`(shadow_notch_lower=score$shadow_notch_lower,shadow_notch_upper=score$shadow_notch_upper)]
example[,worst_case_rating_distance:=pmax(abs(peer_matching_notch_lower-example_target$shadow_notch_upper),
  abs(peer_matching_notch_upper-example_target$shadow_notch_lower))]
selected_keys<-selected[,.(analysis_year,target_iso3=iso3)]
selected_members<-merge(m,selected_keys,by=c("analysis_year","target_iso3"))
donor_summary<-rbindlist(list(m[,.(population="computed_all",memberships=.N,
  donor_country_years=uniqueN(paste(analysis_year,peer_iso3))),by=peer_donor_tier],
 selected_members[,.(population="selected_peer_lmic",memberships=.N,
  donor_country_years=uniqueN(paste(analysis_year,peer_iso3))),by=peer_donor_tier]))
checks <- data.table(check=c("unique_case_keys","membership_counts_equal_saved_counts",
  "membership_medians_equal_saved_rates","all_finite_peer_groups_at_least_three",
  "selected_lmic_group_counts_reconcile","example_actual_group_and_correct_median",
  "example_rule2_income_rating_bounds_and_missing_target_rating","source_inputs_unchanged"),
  passed=c(!anyDuplicated(x[,.(analysis_year,iso3)]),
    all(x[is.finite(peer_rate_pct),peer_country_count==member_count]),
    all(x[is.finite(peer_rate_pct),abs(peer_rate_pct-recomputed_median)<1e-10]),
    all(x[is.finite(peer_rate_pct),peer_country_count>=3]),
    nrow(selected)==sum(summary[population=="selected_peer_lmic" & period=="full_2012_2024" & group_type!="all",country_years]),
    nrow(example)==example_target$peer_country_count && nrow(example)>=3L && abs(median(example$peer_rate_pct)-example_target$peer_rate_pct)<1e-10,
    all(example$peer_income==example_target$historical_income_level) &&
      example_target$peer_rule_number==2L && all(example$worst_case_rating_distance<=3) &&
      !example_target$prior_rating_available && example_target$peer_target_shadow_used,
    identical(before,vapply(inputs,function(p)digest(file=p,algo="sha256"),character(1)))))
stopifnot(all(checks$passed))
ids <- list(build_id=basename(out),schema_id="SCHEMA-P15-PEER-GROUP-DESCRIPTION-V1",
  estimator_id="EST-P15-EXISTING-PEER-COUNT-DESCRIPTIVES-V1",
  admissibility_id="ADM-P15-EXISTING-CURRENT-ELIGIBILITY-V1",
  selection_id="SEL-P15-EXISTING-PEER-NO-CHANGE-V1",
  source_package_ids="SRC-P15-CURRENT-PEER-MEMBERSHIP;SRC-P15-REGION-20260909")
tables <- list(country_year_groups=x,group_size_summary=summary,annual_group_sizes=annual,
  count_frequencies=frequency,rule_summary=rule_summary,kosovo_2024_members=example,
  kosovo_2024_target=example_target,donor_source_summary=donor_summary,checks=checks)
dir.create(out,recursive=TRUE)
for(n in names(tables)) {
  d<-copy(tables[[n]])
  for(id in names(ids)) set(d,j=id,value=ids[[id]])
  d[,`:=`(lifecycle_status="diagnostic",release_state="private_research")]
  fwrite(d,file.path(out,paste0(n,".csv")),na="")
}
manifest<-function(paths,role)do.call(pvr_manifest_rows,c(list(paths=paths,artifact_role=role),ids))
fwrite(manifest(inputs,"peer_description_input"),file.path(out,"input_manifest.csv"))
code<-c("scripts/p15/summarise_peer_groups_20260912.R","scripts/p15/_targets_peer_group_description.R",
  "scripts/p15/activate_p15_environment.R","R/research_governance.R","renv.lock")
fwrite(manifest(code,"peer_description_code"),file.path(out,"code_manifest.csv"))
write_json(c(ids,list(candidate=candidate,benchmark_change=FALSE,
  lifecycle_status="diagnostic",release_state="private_research",
  weighting="One observation per target country-year; repeated peer sets remain repeated uses",
  sd="Sample standard deviation of group sizes",quantile="R type 7",
  inference="Descriptive group sizes; not independent accuracy observations",
  example="Kosovo2024 illustrates the rule; it is not claimed representative")),
  file.path(out,"version_bundle.json"),pretty=TRUE,auto_unbox=TRUE)
writeLines(capture.output(sessionInfo()),file.path(out,"environment.txt"))
fwrite(manifest(list.files(out,full.names=TRUE),"peer_description_output"),file.path(out,"output_manifest.csv"))
print(summary[population=="selected_peer_lmic",.(period,group_type,country_years,
  mean_peer_countries,sd_peer_countries,median_peer_countries,q25_peer_countries,q75_peer_countries,
  min_peer_countries,max_peer_countries)])
cat("Completed",out,"with",nrow(checks),"passing checks.\n")
