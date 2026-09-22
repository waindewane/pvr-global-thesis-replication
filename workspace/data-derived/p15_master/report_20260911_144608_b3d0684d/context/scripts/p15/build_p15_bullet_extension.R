#!/usr/bin/env Rscript
suppressPackageStartupMessages({library(dplyr);library(tidyr);library(purrr);library(data.table)})
source("R/pvr.R"); source("R/p15_pv_first_pass.R"); source("R/p15_bullet_extension.R")
args <- commandArgs(TRUE)
out <- if(length(args))args[[1]] else "data-derived/p15_bullet_extension_20260909_v1"
if(dir.exists(out) && length(list.files(out)))stop("Choose a fresh output directory")
dir.create(out,recursive=TRUE,showWarnings=FALSE)
save_csv <- function(x,name)fwrite(x,file.path(out,paste0(name,".csv")),na="")
parent <- Sys.getenv("P15_PV_FIRST_BASE","data-derived/p15_pv_first_pass_20260909_v1")
inventory_path <- file.path(parent,"country_year_creditor_inventory.csv")
tiers_path <- file.path(Sys.getenv("P15_ANALYSIS_BASE","data-derived/p15_analysis_candidate_20260907_v1"),"tier_eligibility.csv")
audit_path <- file.path(parent,"schedule_and_date_audit.csv")
registered <- as_tibble(fread(file.path(parent,"output_manifest.csv")))
registered$path <- normalizePath(registered$path,mustWork=TRUE)
for(p in c(inventory_path,audit_path))stopifnot(identical(
  digest::digest(file=p,algo="sha256"),registered$sha256[registered$path==normalizePath(p,mustWork=TRUE)]))
x <- as_tibble(fread(inventory_path)); tiers <- as_tibble(fread(tiers_path))
stopifnot(!anyDuplicated(x[c("iso3","analysis_year","creditor")]),
  !anyDuplicated(tiers[c("iso3","analysis_year","tier")]))
x <- x |> mutate(bullet_extension_case=p15_bullet_scope(x),
  repayment_profile=case_when(bullet_extension_case~"bullet_inferred_exact_equal_average_terms",
    term_usable~"equal_principal_average_terms",TRUE~"unavailable"),
  term_usable_with_bullet=term_usable | bullet_extension_case,
  ordinary_scenario_with_bullet=term_usable_with_bullet & selected_rate_available & !selected_purpose_hold,
  modern_dac_with_bullet=ordinary_scenario_with_bullet & analysis_year>=2018 &
    coalesce(dac_eligible,FALSE) & is.finite(group_rate_pct),
  use_reason_with_bullet=if_else(bullet_extension_case & ordinary_scenario_with_bullet,
    "usable_labelled_bullet_scenario",inventory_reason))
added <- x |> filter(bullet_extension_case)
stopifnot(nrow(added)==21L,sum(added$creditor=="China")==12L,sum(added$creditor=="IBRD")==9L,
  all(added$term_state=="bullet_like_pair_not_equal_principal_baseline"),
  all(added$ordinary_scenario_with_bullet),sum(added$modern_dac_with_bullet)==12L,
  all(x$ordinary_scenario_with_bullet[!x$bullet_extension_case]==x$ordinary_scenario_available[!x$bullet_extension_case]))
save_csv(x,"inventory_with_bullet_flags"); save_csv(added,"bullet_case_inputs")
save_csv(x |> filter(historical_lmic_reporting_scope,term_state=="grace_exceeds_maturity"),"inconsistent_pairs_unchanged")
flows <- list(); detail <- list(); checks <- list()
for(i in seq_len(nrow(added))){
  a <- added[i,]; id <- paste(a$iso3,a$analysis_year,a$creditor,sep="_")
  f <- p15_bullet_equal_schedule(a$official_rate,a$official_maturity_years,a$official_grace_years)
  flows[[i]] <- f |> mutate(case_id=id)
  rates <- tiers |> filter(iso3==a$iso3,analysis_year==a$analysis_year) |>
    transmute(rate_source=tier,discount_rate_pct=rate_pct,
      usable=eligible & is.finite(rate_pct) & rate_pct> -100 & !(tier=="secondary" & a$secondary_purpose_hold),
      exclusion_reason)
  rates <- bind_rows(rates,tibble(rate_source="DAC_common_scenario",discount_rate_pct=a$group_rate_pct,
    usable=coalesce(a$dac_eligible,FALSE) & is.finite(a$group_rate_pct),exclusion_reason=""))
  rates$pv_per_100 <- vapply(seq_len(nrow(rates)),function(j)if(rates$usable[j])
    p15_stream_pv(f,rates$discount_rate_pct[j]) else NA_real_,numeric(1))
  policy <- rates$pv_per_100[rates$rate_source=="DAC_common_scenario"]
  detail[[i]] <- rates |> mutate(case_id=id,iso3=a$iso3,analysis_year=a$analysis_year,creditor=a$creditor,
    repayment_profile=a$repayment_profile,grant_element_analogue_pct=100-pv_per_100,
    policy_pv_minus_this_pv=policy-pv_per_100,selected_reference=rate_source==a$selected_tier,
    policy_period_label=if_else(analysis_year>=2018,"modern_2018_2024","pre_2018_grouped_counterfactual"))
  r <- rates |> filter(usable)
  predicted <- vapply(r$discount_rate_pct,function(d)p15_bullet_independent_pv(a$official_rate,a$official_maturity_years,d),numeric(1))
  checks[[i]] <- tibble(case_id=id,principal_error=abs(sum(f$principal)-100),
    no_early_principal=all(head(f$principal,-1)==0),
    final_time_error=abs(tail(f$payment_time_years,1)-a$official_maturity_years),
    independent_pv_error=max(abs(predicted-r$pv_per_100)),
    positive_discount_monotonic=p15_stream_pv(f,5)>p15_stream_pv(f,6))
}
checks <- bind_rows(checks)
stopifnot(all(checks$principal_error<1e-10),all(checks$no_early_principal),
  all(checks$final_time_error==0),all(checks$independent_pv_error<1e-10),all(checks$positive_discount_monotonic))
save_csv(bind_rows(flows),"bullet_cash_flows"); save_csv(bind_rows(detail),"bullet_all_tier_valuations")
save_csv(checks,"bullet_schedule_checks")

# Minimal paired calculation for the inclusion-impact check, not a new tier ranking.
sample <- x |> filter(historical_lmic_reporting_scope,ordinary_scenario_with_bullet)
paired <- map_dfr(seq_len(nrow(sample)),function(i){
  a <- sample[i,]; f <- if(a$bullet_extension_case)
    p15_bullet_equal_schedule(a$official_rate,a$official_maturity_years,a$official_grace_years) else
    p15_pv_schedule(a$official_rate,a$official_maturity_years,a$official_grace_years)
  market <- p15_stream_pv(f,a$selected_rate_pct)
  policy <- if(isTRUE(a$dac_eligible) && is.finite(a$group_rate_pct))p15_stream_pv(f,a$group_rate_pct) else NA_real_
  tibble(iso3=a$iso3,analysis_year=a$analysis_year,creditor=a$creditor,
    selected_tier=a$selected_tier,selected_rate_pct=a$selected_rate_pct,dac_rate_pct=a$group_rate_pct,
    repayment_profile=a$repayment_profile,bullet_extension_case=a$bullet_extension_case,
    modern_dac_comparison=a$modern_dac_with_bullet,
    market_pv_per_100=market,policy_pv_per_100=policy,
    policy_pv_minus_market_pv=policy-market,
    market_grant_element_analogue_pct=100-market,policy_grant_element_analogue_pct=100-policy)
})
save_csv(paired,"paired_pv_for_inclusion_check")
old <- as_tibble(fread(audit_path)) |> filter(historical_lmic_reporting_scope,ordinary_scenario_available) |>
  select(iso3,analysis_year,creditor,pv)
old_check <- paired |> filter(!bullet_extension_case) |> inner_join(old,by=c("iso3","analysis_year","creditor"))
stopifnot(nrow(old_check)==1477L,max(abs(old_check$pv-old_check$market_pv_per_100))<1e-9)
save_csv(old_check |> transmute(iso3,analysis_year,creditor,absolute_pv_difference=abs(pv-market_pv_per_100)),
  "baseline_preservation_check")
views <- bind_rows(paired |> filter(!bullet_extension_case) |> mutate(view="without_bullet_extension"),
  paired |> mutate(view="with_bullet_extension"))
coverage <- views |> group_by(view,creditor) |> summarise(country_year_creditor_rows=n(),
  modern_dac_rows=sum(modern_dac_comparison),
  primary_ids_rows=sum(selected_tier %in% c("primary","ids")),
  modern_primary_ids_rows=sum(modern_dac_comparison & selected_tier %in% c("primary","ids")),.groups="drop")
save_csv(coverage,"coverage_comparison")
modern <- views |> filter(modern_dac_comparison)
evidence_groups <- bind_rows(modern |> mutate(evidence_group="all_selected_tiers"),
  modern |> filter(selected_tier!="peer") |> mutate(evidence_group="without_peers"),
  modern |> filter(selected_tier %in% c("primary","ids")) |> mutate(evidence_group="primary_or_ids"))
summarize_impact <- function(z) z |> summarise(n=n(),
  mean_policy_pv=mean(policy_pv_per_100),mean_market_pv=mean(market_pv_per_100),
  mean_difference=mean(policy_pv_minus_market_pv),median_difference=median(policy_pv_minus_market_pv),
  positive_difference_share=mean(policy_pv_minus_market_pv>0),.groups="drop")
impact <- bind_rows(evidence_groups |> group_by(view,evidence_group,creditor) |> summarize_impact(),
  evidence_groups |> group_by(view,evidence_group) |> summarize_impact() |> mutate(creditor="ALL"))
save_csv(impact,"modern_inclusion_impact")
save_csv(modern |> count(view,creditor,selected_tier),"modern_coverage_by_tier")
save_csv(paired |> filter(bullet_extension_case),"bullet_selected_valuations")
old_sources <- fread(file.path(parent,"input_manifest.csv"))
stopifnot(all(old_sources$sha256==vapply(old_sources$path,function(p)digest::digest(file=p,algo="sha256"),character(1))))
inputs <- c(inventory_path,tiers_path,audit_path,file.path(parent,c("input_manifest.csv","output_manifest.csv")),
  "docs/governance/P15_BULLET_EQUAL_TERMS_PRECEDENT_2026-09-09.md")
manifest <- function(paths)tibble(path=paths,bytes=file.info(paths)$size,
  sha256=vapply(paths,function(p)digest::digest(file=p,algo="sha256"),character(1)))
save_csv(manifest(inputs) |> mutate(input_snapshot_id="SRC-P15-BULLET-EXTENSION-20260909-PINNED"),"input_manifest")
save_csv(manifest(c("R/pvr.R","R/p15_pv_first_pass.R","R/p15_bullet_extension.R",
  "scripts/p15/build_p15_bullet_extension.R","scripts/p15/_targets_bullet_extension.R",
  "tests/testthat/test-p15-bullet-extension.R")),"code_manifest")
writeLines(capture.output(sessionInfo()),file.path(out,"environment.txt"))
writeLines(c(paste0("build_id: ",out),paste0("parent_input: ",parent),
  "release_state: diagnostic_owner_approved_schedule_not_canonical",
  "schema_id: P15-BULLET-EXTENSION-1",
  "estimator_id: annual_interest_final_principal_actual_fractional_maturity",
  "admissibility_id: exact_equality_21_LMIC_rows_and_existing_rate_permissions",
  "selection_id: inherited_from_declared_parent_reference",
  "decision_id: OWNER-BULLET-EQUALITY-20260909",
  "All comparisons per notional 100; common DAC-rate scenario, not reported ODA.",
  "With/without summaries are equal-row composition diagnostics, not a causal repayment-shape effect."),
  file.path(out,"build_contract.txt"))
save_csv(manifest(list.files(out,full.names=TRUE)),"output_manifest")
print(coverage); print(impact |> filter(creditor=="ALL")); cat("Saved",out,"\n")
