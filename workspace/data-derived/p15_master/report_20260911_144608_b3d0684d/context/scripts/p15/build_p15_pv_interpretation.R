#!/usr/bin/env Rscript
suppressPackageStartupMessages({library(dplyr);library(tidyr);library(purrr);library(data.table)})
source("R/pvr.R"); source("R/p15_pv_first_pass.R");source("R/p15_bullet_extension.R")
source("R/p15_pv_interpretation.R")
args <- commandArgs(TRUE)
out <- if(length(args))args[[1]] else "data-derived/p15_pv_interpretation_20260909_v1"
if(dir.exists(out)&&length(list.files(out)))stop("Fresh output path required")
dir.create(out,recursive=TRUE,showWarnings=FALSE)
save_csv <- function(x,n)fwrite(x,file.path(out,paste0(n,".csv")),na="")
base <- Sys.getenv("P15_BULLET_BASE","data-derived/p15_bullet_extension_20260909_v1")
paths <- c(file.path(base,c("paired_pv_for_inclusion_check.csv","inventory_with_bullet_flags.csv")),
  file.path(Sys.getenv("P15_REGIONAL_BASE","data-derived/p15_regional_assessment_20260908_v2"),"country_region_map.csv"),
  file.path(Sys.getenv("P15_ANALYSIS_BASE","data-derived/p15_analysis_candidate_20260907_v1"),"tier_eligibility.csv"))
m <- fread(file.path(base,"output_manifest.csv"))
m$path <- normalizePath(m$path,mustWork=TRUE)
for(p in paths[1:2]) {
  expected <- m$sha256[m$path==normalizePath(p,mustWork=TRUE)]
  stopifnot(length(expected)==1L,identical(digest::digest(file=p,algo="sha256"),expected))
}
x <- as_tibble(fread(paths[1])) |> filter(modern_dac_comparison)
inventory <- as_tibble(fread(paths[2]))
extra <- inventory |> select(iso3,analysis_year,creditor,country,historical_income_level,
  official_rate,official_maturity_years,official_grace_years,dac_group,selected_currency_basis,
  selected_timing_basis,selected_thin_evidence,selected_global_peer,approved_status_rule_class,
  status_case_note,source_closure_current_use,ids_review_explanation,secondary_purpose_hold,
  selected_first_rate_date,selected_last_rate_date,individual_status_review_absent)
geo <- as_tibble(fread(paths[3])) |> select(iso3,region)
stopifnot(!anyDuplicated(geo$iso3),!anyDuplicated(extra[c("iso3","analysis_year","creditor")]))
x <- x |> left_join(extra,by=c("iso3","analysis_year","creditor")) |> left_join(geo,by="iso3") |>
  mutate(period=if_else(analysis_year<=2021,"2018_2021","2022_2024"),
    maturity_group=case_when(official_maturity_years<10~"under_10",official_maturity_years<20~"10_to_under_20",TRUE~"20_plus"))
stopifnot(nrow(x)==762L,all(!is.na(x$region)),!anyDuplicated(x[c("iso3","analysis_year","creditor")]))
save_csv(x,"analysis_rows")
views <- p15_interpretation_views(x)
for(group in list(c("evidence_view"),c("evidence_view","creditor"),c("evidence_view","analysis_year"),
  c("evidence_view","creditor","analysis_year"),c("evidence_view","region"),
  c("evidence_view","creditor","region"),c("evidence_view","historical_income_level"),
  c("evidence_view","selected_tier"),c("evidence_view","maturity_group"),c("evidence_view","period"))){
  save_csv(views |> group_by(across(all_of(group))) |> p15_interpretation_summary(),
    paste0("summary_",paste(group[-1],collapse="_")))
}
# Equal country-year alternative: average represented creditors within each country-year first.
country_year <- views |> group_by(evidence_view,iso3,analysis_year,region) |>
  summarise(creditors=n(),market_ge=mean(market_grant_element_analogue_pct),
    difference=mean(policy_pv_minus_market_pv),.groups="drop")
save_csv(country_year,"country_year_equal_creditor_results")
save_csv(country_year |> group_by(evidence_view,region) |> summarise(n=n(),countries=n_distinct(iso3),
  mean_market_ge=mean(market_ge),mean_difference=mean(difference),.groups="drop"),"region_equal_country_year")
save_csv(views |> group_by(evidence_view,iso3,country,creditor) |> summarise(years=n(),
  first_year=min(analysis_year),last_year=max(analysis_year),mean_difference=mean(policy_pv_minus_market_pv),
  min_difference=min(policy_pv_minus_market_pv),max_difference=max(policy_pv_minus_market_pv),
  positive_share=mean(policy_pv_minus_market_pv>0),mean_market_ge=mean(market_grant_element_analogue_pct),
  .groups="drop") |> filter(years>=3),"persistent_country_creditor_patterns")
save_csv(x |> arrange(desc(abs(policy_pv_minus_market_pv))) |> slice_head(n=25),"largest_discrepancies")

# Recompute schedules once; used for decomposition, rate perturbations and tier alternatives.
key <- function(z)paste(z$iso3,z$analysis_year,z$creditor,sep="_")
flows <- setNames(lapply(seq_len(nrow(x)),function(i)p15_analysis_schedule(x$official_rate[i],
  x$official_maturity_years[i],x$official_grace_years[i])),key(x))
stopifnot(max(abs(vapply(seq_len(nrow(x)),function(i)p15_stream_pv(flows[[i]],x$selected_rate_pct[i]),numeric(1))-
  x$market_pv_per_100))<1e-9)
perturb <- x |> select(iso3,analysis_year,creditor,selected_tier,official_maturity_years)
perturb$ge_gain_if_benchmark_plus_one_pp <- vapply(seq_len(nrow(x)),function(i)
  x$market_pv_per_100[i]-p15_stream_pv(flows[[i]],x$selected_rate_pct[i]+1),numeric(1))
save_csv(perturb,"one_percentage_point_benchmark_sensitivity")
save_csv(perturb |> group_by(creditor) |> summarise(n=n(),median_ge_gain=median(ge_gain_if_benchmark_plus_one_pp),
  mean_ge_gain=mean(ge_gain_if_benchmark_plus_one_pp),max_ge_gain=max(ge_gain_if_benchmark_plus_one_pp),.groups="drop"),
  "benchmark_sensitivity_by_creditor")

# Exact adjacent-year overlap, holding country and creditor fixed.
prev <- x |> mutate(analysis_year=analysis_year+1)
yoy <- inner_join(x,prev,by=c("iso3","creditor","analysis_year"),suffix=c("","_previous"))
decomp <- map_dfr(seq_len(nrow(yoy)),function(i){a <- yoy[i,]
  old <- flows[[paste(a$iso3,a$analysis_year-1,a$creditor,sep="_")]]
  now <- flows[[paste(a$iso3,a$analysis_year,a$creditor,sep="_")]]
  d <- p15_pv_two_block(old,now,a$selected_rate_pct_previous,a$selected_rate_pct)
  tibble(iso3=a$iso3,creditor=a$creditor,analysis_year=a$analysis_year,region=a$region,
    selected_tier=a$selected_tier,previous_tier=a$selected_tier_previous,
    tier_changed=a$selected_tier!=a$selected_tier_previous,
    policy_category_changed=a$dac_rate_pct!=a$dac_rate_pct_previous,
    benchmark_change_pp=a$selected_rate_pct-a$selected_rate_pct_previous,
    official_rate_change_pp=a$official_rate-a$official_rate_previous,
    market_ge_change=-d[["change"]],benchmark_component=-d[["benchmark_effect"]],terms_component=-d[["terms_effect"]],
    policy_ge_change=a$policy_grant_element_analogue_pct-a$policy_grant_element_analogue_pct_previous,
    discrepancy_change=a$policy_pv_minus_market_pv-a$policy_pv_minus_market_pv_previous)
})
stopifnot(max(abs(decomp$market_ge_change-decomp$benchmark_component-decomp$terms_component))<1e-9)
save_csv(decomp,"matched_yoy_decomposition")
dviews <- bind_rows(decomp |> mutate(evidence_view="all_selected"),
  decomp |> filter(selected_tier!="peer",previous_tier!="peer") |> mutate(evidence_view="without_peers"),
  decomp |> filter(selected_tier %in% c("primary","ids"),previous_tier %in% c("primary","ids")) |>
    mutate(evidence_view="primary_or_ids"))
dsum <- function(z)z |> summarise(n=n(),countries=n_distinct(iso3),market_ge_change=mean(market_ge_change),
  benchmark_component=mean(benchmark_component),terms_component=mean(terms_component),
  discrepancy_change=mean(discrepancy_change),source_switches=sum(tier_changed),.groups="drop")
save_csv(dviews |> group_by(evidence_view,analysis_year) |> dsum(),"matched_yoy_summary")
save_csv(dviews |> group_by(evidence_view,creditor,analysis_year) |> dsum(),"matched_yoy_creditor")
save_csv(dviews |> group_by(evidence_view,region,analysis_year) |> dsum(),"matched_yoy_region")
save_csv(dviews |> filter(!tier_changed) |> group_by(evidence_view,analysis_year) |> dsum(),"matched_yoy_same_tier")
save_csv(decomp |> group_by(tier_changed) |> summarise(n=n(),mean_abs_ge_change=mean(abs(market_ge_change)),
  mean_abs_benchmark_component=mean(abs(benchmark_component)),.groups="drop"),"source_switch_diagnostic")

# Same borrower-year creditor comparisons: a common discount rate for both schedules.
pairs <- inner_join(x,x,by=c("iso3","analysis_year"),suffix=c("_a","_b"),relationship="many-to-many") |>
  filter(creditor_a<creditor_b) |>
  mutate(pair=paste(creditor_a,creditor_b,sep="__"),
    ge_difference_a_minus_b=market_grant_element_analogue_pct_a-market_grant_element_analogue_pct_b,
    interest_difference_a_minus_b=official_rate_a-official_rate_b,
    interest_ranking_reversed=interest_difference_a_minus_b*ge_difference_a_minus_b>1e-8)
stopifnot(all(pairs$selected_rate_pct_a==pairs$selected_rate_pct_b),all(pairs$selected_tier_a==pairs$selected_tier_b))
save_csv(pairs |> select(iso3,analysis_year,pair,selected_tier=selected_tier_a,ge_difference_a_minus_b,
  interest_difference_a_minus_b,interest_ranking_reversed,official_maturity_years_a,official_maturity_years_b),
  "matched_creditor_pairs")
ps <- bind_rows(pairs |> mutate(evidence_view="all_selected"),
  pairs |> filter(selected_tier_a!="peer") |> mutate(evidence_view="without_peers"),
  pairs |> filter(selected_tier_a %in% c("primary","ids")) |> mutate(evidence_view="primary_or_ids"))
save_csv(ps |> group_by(evidence_view,pair) |> summarise(n=n(),countries=n_distinct(iso3),
  mean_ge_difference=mean(ge_difference_a_minus_b),median_ge_difference=median(ge_difference_a_minus_b),
  a_more_concessional_share=mean(ge_difference_a_minus_b>0),
  rate_ranking_reversals=sum(interest_ranking_reversed),.groups="drop"),"matched_creditor_summary")

# Leave-one-country-out summaries retain all other rows and never delete source records.
loo <- map_dfr(unique(x$iso3),function(c){
  v <- p15_interpretation_views(x |> filter(iso3!=c))
  bind_rows(v |> group_by(evidence_view,creditor) |> summarise(n=n(),mean_difference=mean(policy_pv_minus_market_pv),.groups="drop"),
    v |> group_by(evidence_view) |> summarise(n=n(),mean_difference=mean(policy_pv_minus_market_pv),.groups="drop") |>
      mutate(creditor="ALL")) |> mutate(omitted_country=c)
})
save_csv(loo,"leave_one_country_out")
save_csv(loo |> group_by(evidence_view,creditor) |> summarise(min_mean=min(mean_difference),max_mean=max(mean_difference),
  country_at_min=omitted_country[which.min(mean_difference)],country_at_max=omitted_country[which.max(mean_difference)],.groups="drop"),
  "leave_one_country_out_ranges")

# Available benchmark alternatives on identical borrower-creditor terms: paired, not different samples.
tiers <- as_tibble(fread(paths[4]))
alternate <- map_dfr(seq_len(nrow(x)),function(i){a <- x[i,]
  t <- tiers |> filter(iso3==a$iso3,analysis_year==a$analysis_year,eligible,is.finite(rate_pct),rate_pct> -100,
    !(tier=="secondary" & a$secondary_purpose_hold))
  t |> transmute(iso3,analysis_year,creditor=a$creditor,tier,rate_pct,
    market_ge=100-vapply(rate_pct,function(r)p15_stream_pv(flows[[i]],r),numeric(1)))
})
save_csv(alternate,"available_tier_same_terms_valuations")
ap <- inner_join(alternate,alternate,by=c("iso3","analysis_year","creditor"),
  suffix=c("_a","_b"),relationship="many-to-many") |> filter(tier_a<tier_b)
save_csv(ap |> group_by(tier_a,tier_b) |> summarise(n=n(),country_years=n_distinct(paste(iso3,analysis_year)),
  mean_ge_a_minus_b=mean(market_ge_a-market_ge_b),mean_abs_difference=mean(abs(market_ge_a-market_ge_b)),
  median_abs_difference=median(abs(market_ge_a-market_ge_b)),.groups="drop"),"paired_tier_sensitivity")
save_csv(x |> group_by(creditor,selected_tier) |> summarise(n=n(),negative_market_ge=sum(market_grant_element_analogue_pct<0),
  .groups="drop"),"negative_market_ge_cases_by_source")
save_csv(x |> filter(market_grant_element_analogue_pct<0),"negative_market_ge_cases")
checks <- tibble(check=c("modern_rows_762","unique_case_keys","regions_complete","PV_reproduces_saved_values",
  "two_block_decomposition_exact","matched_creditor_same_benchmark"),passed=TRUE)
save_csv(checks,"checks")
manifest <- function(p)tibble(path=p,bytes=file.info(p)$size,sha256=vapply(p,function(f)digest::digest(file=f,algo="sha256"),character(1)))
save_csv(manifest(paths),"input_manifest")
save_csv(manifest(c("R/pvr.R","R/p15_pv_first_pass.R","R/p15_bullet_extension.R","R/p15_pv_interpretation.R",
  "scripts/p15/build_p15_pv_interpretation.R","scripts/p15/_targets_pv_interpretation.R",
  "tests/testthat/test-p15-pv-interpretation.R")),"code_manifest")
writeLines(capture.output(sessionInfo()),file.path(out,"environment.txt"))
writeLines(c(paste0("build_id: ",out),paste0("parent_input: ",base),"release_state: diagnostic_thesis_analysis_not_canonical",
  "schema_id: P15-PV-INTERPRETATION-1","estimator_id: inherited_schedules_and_two_block_accounting_decomposition",
  "admissibility_id: inherited_bullet_extension_762_modern_LMIC_DAC_rows",
  "selection_id: unchanged_reference_with_separate_evidence_views",
  "Equal country-year-creditor weights unless explicitly labelled equal-country-year.",
  "Regions use saved pre-2025-transfer partition; comparisons are descriptive, not causal.",
  "No loan-volume, welfare, actual ODA or full currency-matching interpretation."),file.path(out,"build_contract.txt"))
save_csv(manifest(list.files(out,full.names=TRUE)),"output_manifest")
print(views |> group_by(evidence_view,creditor) |> p15_interpretation_summary())
cat("Saved",out,"\n")
