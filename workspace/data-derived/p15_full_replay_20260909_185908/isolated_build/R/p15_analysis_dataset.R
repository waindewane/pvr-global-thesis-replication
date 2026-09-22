# Owner-directed selected views. Selection is not release promotion.
p15_analysis_views <- function(panel) {
  p15_local_assert_keys(panel,c("analysis_year","iso3"),"analysis input")
  views <- p15_review_previews(panel)
  names(views)[names(views)=="preview_rate_pct"] <- "selected_rate_pct"
  names(views)[names(views)=="preview_source"] <- "selected_tier"
  names(views)[names(views)=="preview_variant"] <- "view_id"
  names(views)[names(views)=="preview_currency_basis"] <- "selected_currency_basis"
  names(views)[names(views)=="preview_timing_basis"] <- "selected_timing_basis"
  views$view_id <- sub("__ids_positive_as_reported","",views$view_id,fixed=TRUE)
  views$view_id <- sub("broad_peer_included","with_peer",views$view_id,fixed=TRUE)
  views$view_id <- sub("peer_not_selected","without_peer",views$view_id,fixed=TRUE)
  views$preview_state <- NULL
  views$approved_selection <- NULL
  context <- c("analysis_year","iso3","approved_status_rule_class","status_case_note",
    "observed_benchmark_selection_permitted","ordinary_fallback_selection_permitted",
    "ids_benchmark_proxy_candidate_permitted","ids_reviewed_term_state",
    "ids_review_explanation","ids_review_repayment_profile_use","source_closure_decision_id",
    "source_closure_current_use","observed_case_explanation","comparison_timing_warning")
  views <- dplyr::left_join(views,panel[context],by=c("analysis_year","iso3"))
  at <- match(paste(views$analysis_year,views$iso3),paste(panel$analysis_year,panel$iso3))
  views$selected_source_package_ids <- NA_character_
  views$selected_evidence_subtype <- NA_character_
  views$selected_thin_evidence <- NA
  views$selected_global_peer <- FALSE
  views$selected_first_rate_date <- NA_character_
  views$selected_last_rate_date <- NA_character_
  for(tier in c("primary","secondary")) {
    use <- views$selected_tier==tier; prefix <- paste0(tier,"_usd_")
    views$selected_source_package_ids[use] <- panel[[paste0(prefix,"source_package_ids")]][at[use]]
    views$selected_evidence_subtype[use] <- panel[[paste0(prefix,"candidate_evidence_tier")]][at[use]]
    views$selected_thin_evidence[use] <- panel[[paste0(prefix,"thin_evidence")]][at[use]]
    views$selected_first_rate_date[use] <- as.character(panel[[paste0(prefix,"first_rate_date")]][at[use]])
    views$selected_last_rate_date[use] <- as.character(panel[[paste0(prefix,"last_rate_date")]][at[use]])
  }
  use <- views$selected_tier=="ids"
  views$selected_source_package_ids[use] <- panel$ids_source_package_id[at[use]]
  views$selected_evidence_subtype[use] <- "reported_BND_contractual_proxy_not_verified_USD_yield"
  for(tier in c("moodys","peer")) {
    use <- views$selected_tier==tier
    views$selected_source_package_ids[use] <- panel$source_package_ids[at[use]]
    views$selected_evidence_subtype[use] <- if(tier=="moodys") "original_Moodys_no_fitted_recalibration" else "similarity_min3_same_year_primary"
  }
  use <- views$selected_tier=="peer"
  views$selected_global_peer[use] <- panel$global_pool_used[at[use]] %in% TRUE
  views$selection_reason <- ifelse(views$selected_tier=="no_eligible_rate",
    "No finite eligible value in this view; see retained tier evidence and case permissions.",
    "First finite eligible tier in this view's declared order; earlier tiers absent or restricted.")
  views$individual_status_review_absent <- !panel$status_case_review_present[at] %in% TRUE
  views$missing_result_state <- ifelse(is.finite(views$selected_rate_pct),"selected_with_source_qualifications",
    ifelse(panel$observed_benchmark_selection_permitted[at] %in% FALSE &
      panel$ordinary_fallback_selection_permitted[at] %in% FALSE,"status_restricted",
      "no_eligible_evidence_for_this_view"))
  views$selected_for_reference_view <- is.finite(views$selected_rate_pct)
  views$release_state <- "candidate_for_owner_review_not_canonical"
  views
}

# Independently enumerate eligible tiers for every key and retain exclusion reasons.
p15_analysis_eligibility <- function(p) {
  rates <- list(primary=p$primary_usd_market_rate_pct,ids=p$ids_rate_pct,
    secondary=p$secondary_usd_market_rate_pct,moodys=p$rating_moodys_rate_pct,peer=p$peer_rate_pct)
  dplyr::bind_rows(lapply(names(rates),function(tier) {
    status <- if(tier %in% c("primary","secondary"))
      !(p$observed_benchmark_selection_permitted %in% FALSE) else !(p$ordinary_fallback_selection_permitted %in% FALSE)
    source <- switch(tier,ids=p$ids_benchmark_proxy_candidate_permitted %in% TRUE,
      moodys=p$rating_moodys_available %in% TRUE,peer=p$peer_minimum_met %in% TRUE,rep(TRUE,nrow(p)))
    data.frame(analysis_year=p$analysis_year,iso3=p$iso3,tier=tier,rate_pct=rates[[tier]],
      eligible=is.finite(rates[[tier]]) & status & source,
      exclusion_reason=ifelse(!status,"status_restriction",ifelse(!source,"source_or_method_restriction",
        ifelse(!is.finite(rates[[tier]]),"no_finite_source_value","eligible"))))
  }))
}
