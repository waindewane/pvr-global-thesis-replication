#!/usr/bin/env Rscript
suppressPackageStartupMessages({library(data.table);library(dplyr);library(tibble);library(tidyr)})
if(!file.exists(".p15_isolated_build"))stop("Run reviewed build through the isolated P15 pipeline")
source("R/p15_local_completion.R")
source("R/p15_bounded_fallback_comparison.R")
source("R/p15_case_review.R")
source("R/p15_ladder_preview.R")
base <- "data-derived/p15_local_completion_2012_2024_20260906_v1"
read <- function(p) as_tibble(fread(p))
original <- read(file.path(base,"p15_country_year_dataset.csv"))
anchors <- read(file.path(base,"p15_observed_anchors_with_historical_classification.csv"))
case_dir <- "data-raw/p15_case_review_20260906"
review <- p15_apply_ids_case_review(original,read(file.path(case_dir,"ids_case_dispositions.csv")),
  read(file.path(case_dir,"ids_decision_definitions.csv")))
observed <- p15_apply_observed_case_review(review$panel,anchors,read(file.path(case_dir,"observed_issue_dispositions.csv")))
context <- read(p15_local_input_paths()[["rating_context"]])
peer <- p15_audit_local_peer_targets(observed$panel,observed$anchors,context)
panel <- observed$panel
peer_fields <- c("peer_rate_pct","peer_minimum_met","peer_pool_rule","peer_country_count","peer_iqr_pp",
  "target_moodys_missing","rating_proximity_used","global_pool_used","peer_candidate_state")
panel <- panel |> select(-all_of(peer_fields),-peer_evidence_id) |>
  left_join(peer$detail |> select(analysis_year,iso3,all_of(peer_fields),peer_evidence_id=source_evidence_row_id),by=c("analysis_year","iso3")) |>
  mutate(build_id="BUILD-P15-REVIEWED-EVIDENCE-20260906-V1",schema_id="SCHEMA-P15-REVIEWED-EVIDENCE-V1",
    admissibility_id="ADM-P15-CASE-REVIEW-20260906-V1",selection_id="SEL-NONE-EVIDENCE-ASSEMBLY-V1")

# Independently downloaded public data corroborate; they never replace raw build values.
audit_dir <- "data-raw/p15_ids_review_public_snapshot_20260906"
audit <- bind_rows(lapply(file.path(audit_dir,c("DT_INR_DPPG_BND.json","DT_MAT_DPPG_BND.json","DT_GPA_DPPG_BND.json","DT_COM_DPPG_CD_BND.json")),p15_read_ids_audit_snapshot))
terms <- audit |> filter(indicator!="DT.COM.DPPG.CD") |> mutate(field=recode(indicator,
  "DT.INR.DPPG"="ids_rate_pct","DT.MAT.DPPG"="ids_maturity_years","DT.GPA.DPPG"="ids_grace_years")) |>
  left_join(original |> select(analysis_year,iso3,ids_rate_pct,ids_maturity_years,ids_grace_years) |>
    pivot_longer(starts_with("ids_"),names_to="field",values_to="preserved_value"),by=c("analysis_year","iso3","field")) |>
  mutate(matches_preserved=(is.na(value)&is.na(preserved_value))|(!is.na(value)&!is.na(preserved_value)&value==preserved_value))
stopifnot(all(terms$matches_preserved))
review_cases <- review$cases |> left_join(audit |> filter(indicator=="DT.COM.DPPG.CD") |>
  select(analysis_year,iso3,new_commitments_usd=value),by=c("analysis_year","iso3"))
stopifnot(all(is.finite(review_cases$new_commitments_usd)&review_cases$new_commitments_usd>0))

comparison <- original |> select(analysis_year,iso3,historical_lmic_reporting_scope,
    old_primary_rate_pct=primary_usd_market_rate_pct,old_peer_rate_pct=peer_rate_pct,old_ids_rate_pct=ids_rate_pct) |>
  left_join(panel |> select(analysis_year,iso3,reviewed_primary_rate_pct=primary_usd_market_rate_pct,
    reviewed_peer_rate_pct=peer_rate_pct,reviewed_ids_proxy_rate_pct=ids_reviewed_proxy_rate_pct),by=c("analysis_year","iso3")) |>
  mutate(primary_changed=xor(is.na(old_primary_rate_pct),is.na(reviewed_primary_rate_pct))|
      coalesce(abs(old_primary_rate_pct-reviewed_primary_rate_pct)>1e-12,FALSE),
    peer_changed=xor(is.na(old_peer_rate_pct),is.na(reviewed_peer_rate_pct))|
      coalesce(abs(old_peer_rate_pct-reviewed_peer_rate_pct)>1e-12,FALSE),
    ids_positive_held=is.finite(old_ids_rate_pct)&old_ids_rate_pct>0&is.na(reviewed_ids_proxy_rate_pct))
rating_validation <- panel |> filter(is.finite(primary_usd_market_rate_pct)) |>
  select(analysis_year,iso3,historical_lmic_reporting_scope,primary_usd_market_rate_pct,
    rating_moodys_rate_pct,rating_fitch_rate_pct,rating_precedence_rate_pct,rating_agency_median_rate_pct) |>
  pivot_longer(starts_with("rating_"),names_to="method",values_to="predicted_rate_pct") |>
  filter(is.finite(predicted_rate_pct)) |> mutate(error_pp=predicted_rate_pct-primary_usd_market_rate_pct)
rating_summary <- rating_validation |> group_by(historical_lmic_reporting_scope,method) |>
  summarise(n=n(),mean_error_pp=mean(error_pp),mae_pp=mean(abs(error_pp)),rmse_pp=sqrt(mean(error_pp^2)),.groups="drop")
previews <- bind_rows(lapply(c(TRUE,FALSE),function(ids_first) bind_rows(lapply(c(FALSE,TRUE),function(peers) {
  # The preview API is supplied reviewed eligibility; no blanket low-rate rule.
  p <- panel; p$ids_positive_rate_observed <- p$ids_benchmark_proxy_candidate_permitted
  x <- p15_local_ladder_preview(p,ids_before_secondary=ids_first,hold_low_ids_for_review=FALSE,include_broad_peer=peers)
  x$preview_variant <- sub("ids_positive_as_reported","ids_case_review_applied",x$preview_variant,fixed=TRUE)
  x
}))))
checks <- tibble(check_id=c("grid_retained","raw_ids_unchanged","all_initial_cases_disposed","audit_snapshot_corroborates",
  "held_issue_not_in_peer_seeds","sole_held_anchor_removed","no_ladder_promotion","case_notes_preserved"),
  passed=c(nrow(panel)==2743&&!anyDuplicated(panel[c("analysis_year","iso3")]),
    identical(panel$ids_rate_pct,original$ids_rate_pct),nrow(review_cases)==30&&all(!is.na(review_cases$decision_code)),
    all(terms$matches_preserved),!any(peer$membership$analysis_year==2024&peer$membership$peer_iso3=="AGO"),
    sum(comparison$primary_changed)==1&&!any(observed$anchors$analysis_year==2024&observed$anchors$iso3=="AGO"&observed$anchors$observed_market_branch=="observed_primary"&observed$anchors$currency=="USD"),
    !any(panel$selected_for_ladder|panel$canonical_benchmark),identical(panel$status_case_note,original$status_case_note)))
if(!all(checks$passed))stop("Reviewed dataset quality checks failed")
out <- "data-derived/p15_reviewed_evidence_20260906_v1"
dir.create(out,recursive=TRUE,showWarnings=FALSE)
tables <- list(p15_country_year_dataset=panel,p15_ids_case_dispositions=review_cases,
  p15_observed_anchors=observed$anchors,p15_held_observed_anchors=observed$held_anchors,
  p15_peer_membership=peer$membership,p15_peer_target_detail=peer$detail,
  p15_rating_validation_detail=rating_validation,p15_rating_validation_summary=rating_summary,
  p15_case_review_effects=comparison,p15_ids_public_snapshot_comparison=terms,
  p15_reviewed_ladder_previews=previews,p15_quality_checks=checks)
for(n in names(tables))fwrite(tables[[n]],file.path(out,paste0(n,".csv")),na="")
dictionary <- tibble(field=names(panel),storage_type=vapply(panel,function(x)paste(class(x),collapse="/"),character(1)),
  missing_values=vapply(panel,function(x)sum(is.na(x)),integer(1)),
  meaning=case_when(grepl("^ids_review",field)~"Case-reviewed permissions or source context; raw IDS columns remain unchanged",
    field=="primary_usd_pre_review_rate_pct"~"Preserved pre-review primary rate; not reviewed eligibility",
    grepl("^observed_case",field)~"Explicit source-backed observed-market case restriction",
    TRUE~"Inherited field; see P15_LOCAL_DATASET_GUIDE and prior field dictionary"))
fwrite(dictionary,file.path(out,"p15_data_dictionary.csv"))
cat("Reviewed evidence built without promoting a selected ladder.\n")
print(checks)
print(comparison |> summarise(primary_changes=sum(primary_changed),peer_changes=sum(peer_changed),positive_ids_held=sum(ids_positive_held)))
