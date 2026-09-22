#!/usr/bin/env Rscript
# Run after the raw replay; an explicit input root supports repeatable local testing.
suppressPackageStartupMessages({library(data.table);library(dplyr);library(tibble);library(tidyr);library(digest)})
args <- commandArgs(trailingOnly=TRUE)
input_root <- if(length(args)) normalizePath(args[1]) else normalizePath(getwd())
out <- if(length(args)>1) args[2] else "data-derived/p15_reference_review_20260907_v1"
if(dir.exists(out))stop("Refusing to overwrite an existing review build: ",out)
for(f in c("p15_observed_all_years","p15_observed_secondary","p15_secondary_repair_admissibility",
  "p15_status_sanity_candidate_rules","p15_status_sanity_approved_rules","p15_integrated_observed_candidate",
  "p15_local_completion","p15_bounded_fallback_comparison","p15_ladder_preview","p15_reference_review","research_governance")) source(paste0("R/",f,".R"))
used <- character(); hashes <- character()
read <- function(rel,root=input_root) {
  p <- file.path(root,rel); used <<- c(used,p); hashes <<- c(hashes,digest(p,algo="sha256",file=TRUE))
  x <- as_tibble(fread(p)) |> mutate(across(where(~inherits(.x,"Date")),~as.Date(as.character(.x))))
  if("period" %in% names(x)) x$period <- as.character(x$period)
  x
}
base <- "data-derived/p15_reviewed_evidence_20260906_v1"
original <- read(file.path(base,"p15_country_year_dataset.csv"))
old_anchors <- read(file.path(base,"p15_observed_anchors.csv"))
grid <- read("data-derived/p15_platform_2012_2024_v1/p15_country_year_grid_2012_2024.csv")
universe <- read("data-derived/p15_unified_lseg_source_2012_2024_v1/p15_instrument_year_universe_2012_2024.csv.gz")
history <- read("data-derived/p15_unified_lseg_source_2012_2024_v1/p15_secondary_history_long_2012_2024.csv.gz")
old_identifiers <- read("data-derived/p15_observed_markets_2012_2024_v1/p15_secondary_identifier_year_evidence_2012_2024.csv.gz")
old_repair <- read("data-derived/p15_secondary_repair_candidate_2012_2024_v1/p15_secondary_repair_identifier_audit_2012_2024.csv.gz")
status <- read("data-derived/p15_status_sanity_approved_2012_2024_v1/p15_approved_status_rule_contract_2012_2024.csv.gz")
recommendations <- read("docs/governance/p15_status_case_review_recommendations_2012_2024.csv.gz")
primary <- read("data-derived/p15_primary_approved_candidate_2012_2024_v1/p15_primary_approved_issue_disposition_2012_2024.csv.gz")
context <- read(p15_local_input_paths()[["rating_context"]])
treasury_paths <- file.path(input_root,"data-raw/fred_treasury_tenor_sensitivity_2026-07-21",
  paste0("fred_dgs",c(5,7,10),"_2012_2024_downloaded_2026-07-21.csv"))
used <- c(used,treasury_paths); hashes <- c(hashes,vapply(treasury_paths,digest,character(1),algo="sha256",file=TRUE))
treasury <- p15_bind_fred_treasury_curve(treasury_paths[1],treasury_paths[2],treasury_paths[3])
message("Reconstructing common closest-date direct and strict repaired evidence")
identifiers <- p15_build_secondary_identifier_evidence_all_years(universe,history,grid,"closest",31,7)
issues <- p15_build_secondary_issue_evidence_all_years(identifiers)
parameters <- p15_secondary_repair_candidate_parameters(); parameters$quote_selection_rule <- "closest"
repair <- p15_build_strict_secondary_repair_candidates(identifiers,history,recommendations,parameters)
catalogue <- p15_build_secondary_issue_disposition_catalogue(issues,repair$issue_audit)
sanity <- p15_build_approved_sanity_issue_universe(primary,issues,treasury)
sanity <- sanity |> filter(evidence_family!="observed_primary_issuance")
repair_sanity <- p15_build_repair_issue_sanity(repair$issue_audit,treasury)
sanity <- bind_rows(sanity,repair_sanity)
actions <- p15_integrate_approved_status_sanity_actions(sanity,status,p15_build_imf_inspired_context_flags(sanity))
evidence <- p15_normalize_integrated_observed_evidence(sanity,actions)

message("Reconstructing and dating preserved targeted companions")
legacy <- "experiments/full_ladder_database_all_years_2026-05-20"
source_issues <- read(paste0(legacy,"/outputs/secondary_issue_level_all_years.csv"))
wide <- read(paste0(legacy,"/data/lseg_v14/pvr-global-lseg/output/tables/lseg_secondary_market_year_end_history_oecd_base_2000-01-01_2025-12-31.csv"))
targeted_direct <- p15_review_targeted_direct(source_issues,wide)
p13 <- "experiments/full_ladder_ratings_integrated_status_rebuild_2026-06-01/outputs"
snapshots <- read(file.path(p13,"secondary_price_to_yield_identifier_price_snapshot_2024.csv"))
eligibility <- read(file.path(p13,"secondary_price_to_yield_row_classification_2024.csv"))
classes <- read(file.path(p13,"secondary_terminal_post_audit_review_classes_2024.csv"))
# A dated snapshot exactly at year-end is necessarily closest. Nonzero-offset
# targeted snapshots are not silently grandfathered into the new construction.
price_date_audit <- snapshots |> mutate(quote_date=as.Date(latest_quote_date),
  exact_year_end=quote_date==as.Date(paste0(analysis_year,"-12-31")))
allowed_ids <- price_date_audit |> group_by(p7b_target_id) |> summarise(ok=all(exact_year_end %in% TRUE),.groups="drop") |> filter(ok) |> pull(p7b_target_id)
targeted_price <- p15_build_terminal_price_to_yield_country(snapshots |> filter(p7b_target_id %in% allowed_ids),eligibility,classes)
terminal <- p15_build_terminal_direct_country(classes)
# Undated terminal-class yield strings stay in legacy evidence. Only dated raw
# standard companions and exact-date price snapshots enter this successor.
targeted <- p15_build_targeted_secondary_companion_evidence(targeted_direct$country,
  terminal[FALSE,],targeted_price,catalogue,grid,status)
price_dates <- price_date_audit |> filter(p7b_target_id %in% allowed_ids) |>
  group_by(analysis_year,iso3) |> summarise(first_rate_date=min(quote_date),last_rate_date=max(quote_date),.groups="drop") |>
  mutate(targeted_measure_class="repair")
target_dates <- bind_rows(targeted_direct$dates |> mutate(targeted_measure_class="direct",
  across(c(first_rate_date,last_rate_date),~as.Date(as.character(.x)))),price_dates)
targeted <- targeted |> left_join(target_dates,by=c("analysis_year","iso3","targeted_measure_class"))
stopifnot(all(!is.na(targeted$first_rate_date)),all(!is.na(targeted$last_rate_date)))
evidence <- evidence |> mutate(across(where(~inherits(.x,"Date")),~as.Date(as.character(.x))))
targeted <- targeted |> mutate(across(where(~inherits(.x,"Date")),~as.Date(as.character(.x))))
evidence <- bind_rows(evidence,targeted) |> mutate(
  timing_object="closest_to_year_end_minus31_plus7_earlier_tie",
  terminal_dependency_state="supported_year_end_scope_full_year_and_features_parked",
  source_evidence_row_id=paste0(source_evidence_row_id,"::CLOSEST-20260907"))
new_secondary <- p15_resolve_terminal_independent_validation_anchors(evidence)
# Preserve historical classifications and display names exactly as in the core grid.
new_secondary <- new_secondary |> select(-any_of(c("country","country_year_id","historical_income_level","historical_lmic_reporting_scope"))) |>
  left_join(original |> select(analysis_year,iso3,country,country_year_id,historical_income_level,historical_lmic_reporting_scope),by=c("analysis_year","iso3"))
anchors <- bind_rows(old_anchors |> filter(observed_market_branch=="observed_primary"),new_secondary)
panel <- p15_review_secondary_panel(original,anchors,evidence)
previews <- p15_review_previews(panel); old_previews <- p15_review_previews(original)

message("Comparing peer formulas on actual fallback and observed-primary populations")
peers <- p15_review_peer_options(panel,anchors,context)
reference <- previews |> filter(grepl("^ids_before_secondary",preview_variant),grepl("peer_not_selected$",preview_variant))
targets <- reference |> filter(preview_source=="no_eligible_rate") |> select(analysis_year,iso3)
peer_detail <- peers$detail |> left_join(targets |> mutate(last_tier_target=TRUE),by=c("analysis_year","iso3")) |>
  mutate(last_tier_target=coalesce(last_tier_target,FALSE),
    last_tier_usable=last_tier_target & peer_minimum_met & !(ordinary_fallback_selection_permitted %in% FALSE))
peer_summary <- peer_detail |> group_by(peer_method,historical_lmic_reporting_scope) |> summarise(
  last_tier_global=sum(last_tier_usable & global_pool),last_tier_rating_match=sum(last_tier_usable & rating_proximity_used),
  last_tier_targets=sum(last_tier_target),last_tier_usable=sum(last_tier_usable),
  validation_n=sum(is.finite(error_pp)),mean_error_pp=mean(error_pp,na.rm=TRUE),mae_pp=mean(abs(error_pp),na.rm=TRUE),
  rmse_pp=sqrt(mean(error_pp^2,na.rm=TRUE)),.groups="drop")
peer_reference <- peer_detail |> filter(peer_method=="similarity_min3") |> select(analysis_year,iso3,reference_peer_rate_pct=peer_rate_pct)
peer_detail <- peer_detail |> left_join(peer_reference,by=c("analysis_year","iso3")) |>
  mutate(difference_from_similarity_pp=peer_rate_pct-reference_peer_rate_pct)
peer_changes <- peer_detail |> filter(last_tier_target) |> group_by(peer_method,historical_lmic_reporting_scope) |> summarise(
  matched=sum(is.finite(difference_from_similarity_pp)),changed=sum(abs(difference_from_similarity_pp)>1e-12,na.rm=TRUE),
  mean_absolute_change_pp=mean(abs(difference_from_similarity_pp),na.rm=TRUE),
  maximum_absolute_change_pp=max(abs(difference_from_similarity_pp),na.rm=TRUE),.groups="drop")

keys <- c("analysis_year","iso3","currency")
secondary_changes <- old_anchors |> filter(observed_market_branch=="observed_secondary") |>
  select(all_of(keys),old_rate_pct=market_rate_pct,old_issues=included_issue_keys,old_tier=candidate_evidence_tier) |>
  full_join(new_secondary |> select(all_of(keys),country,historical_lmic_reporting_scope,new_rate_pct=market_rate_pct,
    new_issues=included_issue_keys,new_tier=candidate_evidence_tier,first_rate_date,last_rate_date),by=keys) |>
  mutate(difference_pp=new_rate_pct-old_rate_pct,coverage_changed=xor(is.na(new_rate_pct),is.na(old_rate_pct)),
    rate_changed=coalesce(abs(difference_pp)>1e-12,FALSE),issues_changed=coalesce(old_issues!=new_issues,FALSE))
selection_changes <- old_previews |> select(analysis_year,iso3,preview_variant,old_source=preview_source,old_rate_pct=preview_rate_pct) |>
  left_join(previews,by=c("analysis_year","iso3","preview_variant")) |>
  mutate(difference_pp=preview_rate_pct-old_rate_pct,coverage_changed=xor(is.na(old_rate_pct),is.na(preview_rate_pct)),
    source_changed=old_source!=preview_source)
identifier_changes <- old_identifiers |> select(analysis_year,ric,old_date=direct_quote_date,old_yield_pct=direct_yield_pct) |>
  left_join(identifiers |> select(analysis_year,ric,iso3,currency,new_date=direct_quote_date,new_yield_pct=direct_yield_pct,
    direct_source_record_locators),by=c("analysis_year","ric")) |>
  mutate(difference_pp=new_yield_pct-old_yield_pct)
repair_changes <- old_repair |> select(analysis_year,ric,old_date=selected_quote_date,old_yield_pct=repaired_yield_mid_pct) |>
  full_join(repair$identifier_audit |> select(analysis_year,ric,new_date=selected_quote_date,new_yield_pct=repaired_yield_mid_pct,
    identifier_technical_pass),by=c("analysis_year","ric")) |> mutate(difference_pp=new_yield_pct-old_yield_pct)
# Retain all previously order-sensitive keys, even if the updated values coincide.
case_keys <- original |> filter(historical_lmic_reporting_scope,!is.finite(primary_usd_market_rate_pct),
    ids_benchmark_proxy_candidate_permitted,is.finite(secondary_usd_market_rate_pct),
    !(ordinary_fallback_selection_permitted %in% FALSE)) |> select(analysis_year,iso3)
case_keys <- bind_rows(case_keys,panel |> filter(historical_lmic_reporting_scope,!is.finite(primary_usd_market_rate_pct),
  ids_benchmark_proxy_candidate_permitted,is.finite(secondary_usd_market_rate_pct),
  !(ordinary_fallback_selection_permitted %in% FALSE)) |> select(analysis_year,iso3)) |> distinct()
case_context <- read("data-raw/p15_reference_review_20260907/source_case_context.csv",getwd())
p15_local_assert_keys(case_context,c("analysis_year","iso3"),"curated source-case context")
cases <- panel |> semi_join(case_keys,by=c("analysis_year","iso3")) |>
  left_join(case_context,by=c("analysis_year","iso3")) |>
  mutate(ids_minus_secondary_pp=ids_rate_pct-secondary_usd_market_rate_pct,
    ids_original_minus_secondary_remaining_maturity_years=ids_maturity_years-secondary_usd_market_maturity_years,
    explanation_strength=coalesce(explanation_strength,"new_case_needs_context_review"),
    source_comparability_state="different_reporting_population_currency_and_timing_not_a_verified_security_match",
    timing_explanation_state="possible_not_established_without_within_year_history_and_commitment_dates") |>
  arrange(desc(abs(ids_minus_secondary_pp)))

test_reference <- peer_detail |> filter(peer_method=="similarity_min3") |>
  left_join(original |> select(analysis_year,iso3,old_peer_rate=peer_rate_pct),by=c("analysis_year","iso3"))
checks <- tibble(check_id=c("full_grid_retained","primary_unchanged","ids_unchanged","rating_unchanged","case_notes_unchanged",
  "direct_dates_in_window","repair_dates_in_window","targeted_dates_known","peer_self_excluded","inherited_peer_reproduced",
  "no_ladder_promotion","input_files_unchanged"),passed=c(nrow(panel)==2743&&!anyDuplicated(panel[c("analysis_year","iso3")]),
    identical(panel$primary_usd_market_rate_pct,original$primary_usd_market_rate_pct),identical(panel$ids_rate_pct,original$ids_rate_pct),
    identical(panel$rating_moodys_rate_pct,original$rating_moodys_rate_pct),identical(panel$status_case_note,original$status_case_note),
    all(identifiers$quote_offset_days[identifiers$direct_yield_available]>=-31 & identifiers$quote_offset_days[identifiers$direct_yield_available]<=7),
    all(na.omit(repair$identifier_audit$selected_quote_offset_days)>=-31 & na.omit(repair$identifier_audit$selected_quote_offset_days)<=7),
    all(!is.na(targeted$first_rate_date)),all(peers$membership$target_iso3!=peers$membership$peer_iso3),
    identical(is.finite(test_reference$peer_rate_pct),is.finite(test_reference$old_peer_rate)) &&
      all(abs(test_reference$peer_rate_pct-test_reference$old_peer_rate)<1e-12,na.rm=TRUE),
    !any(previews$approved_selection),identical(unname(hashes),unname(vapply(used,digest,character(1),algo="sha256",file=TRUE)))))
if(!all(checks$passed))stop("Review checks failed: ",paste(checks$check_id[!checks$passed],collapse=","))
dir.create(out,recursive=TRUE)
build <- basename(out); schema <- "SCHEMA-P15-REFERENCE-REVIEW-V1"
bundle <- list(build_id=build,schema_id=schema,estimator_id="EST-P15-CLOSEST-YEAR-END-V1",
  admissibility_id="ADM-P15-CASE-REVIEW-20260906-V1",selection_id="SEL-NONE-EVIDENCE-ASSEMBLY-V1",
  source_package_ids="SRC-P15-RAW-CLOSURE-20260906")
tables <- list(p15_country_year_dataset=panel,p15_observed_anchors=anchors,p15_secondary_evidence=evidence,
  p15_secondary_identifier_audit=identifiers,p15_secondary_issue_audit=issues,
  p15_secondary_repair_identifier_audit=repair$identifier_audit,p15_secondary_repair_issue_audit=repair$issue_audit,
  p15_secondary_sanity=sanity,p15_secondary_date_changes=identifier_changes,p15_repair_date_changes=repair_changes,
  p15_secondary_country_changes=secondary_changes,p15_selection_changes=selection_changes,p15_ladder_previews=previews,
  p15_targeted_direct_quote_audit=targeted_direct$quotes,p15_targeted_price_date_audit=price_date_audit,
  p15_peer_options=peer_detail,p15_peer_option_membership=peers$membership,p15_peer_summary=peer_summary,
  p15_peer_target_changes=peer_changes,p15_ids_secondary_cases=cases,p15_checks=checks)
for(n in names(tables)) {
  t <- as.data.table(tables[[n]])
  for(b in names(bundle)) {
    if(b=="source_package_ids" && b %in% names(t)) next
    t[,(b):=bundle[[b]]]
  }
  fwrite(t,file.path(out,paste0(n,".csv.gz")),na="")
}
manifest <- function(paths,role)do.call(pvr_manifest_rows,c(list(paths=paths,artifact_role=role),bundle))
fwrite(manifest(unique(used),"source_or_rebuilt_input"),file.path(out,"input_manifest.csv"))
code <- c("scripts/p15/build_p15_reference_review.R",paste0("R/",c("p15_reference_review","p15_observed_all_years",
  "p15_observed_secondary","p15_secondary_repair_admissibility","p15_status_sanity_candidate_rules",
  "p15_status_sanity_approved_rules","p15_integrated_observed_candidate","p15_local_completion",
  "p15_bounded_fallback_comparison","p15_ladder_preview","research_governance"),".R"))
fwrite(manifest(code,"code"),file.path(out,"script_manifest.csv"))
fwrite(manifest(list.files(out,pattern="csv.gz$",full.names=TRUE),"generated_output"),file.path(out,"output_manifest.csv"))
capture.output(sessionInfo(),file=file.path(out,"environment.txt"))
print(checks);print(peer_summary);print(secondary_changes |> summarise(matched=sum(is.finite(difference_pp)),changed=sum(rate_changed),coverage_changes=sum(coverage_changed),max_change=max(abs(difference_pp),na.rm=TRUE)))
cat("Completed reference review: ",out,"\n")
