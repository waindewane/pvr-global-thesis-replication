# Compare candidate selection consequences without approving a selection rule.
p15_local_ladder_preview <- function(panel, ids_before_secondary = TRUE,
                                     hold_low_ids_for_review = TRUE,
                                     include_broad_peer = FALSE) {
  keys <- c("analysis_year", "iso3")
  p15_local_assert_keys(panel, keys, "ladder preview panel")
  order <- if (ids_before_secondary) c("primary", "ids", "secondary", "moodys", "peer") else
    c("primary", "secondary", "ids", "moodys", "peer")
  variant <- paste(if (ids_before_secondary) "ids_before_secondary" else "secondary_before_ids",
    if (hold_low_ids_for_review) "ids_low_cases_held" else "ids_positive_as_reported",
    if (include_broad_peer) "broad_peer_included" else "peer_not_selected", sep = "__")
  observed_allowed <- !(panel$observed_benchmark_selection_permitted %in% FALSE)
  fallback_allowed <- !(panel$ordinary_fallback_selection_permitted %in% FALSE)
  rates <- list(primary = panel$primary_usd_market_rate_pct, ids = panel$ids_rate_pct,
    secondary = panel$secondary_usd_market_rate_pct, moodys = panel$rating_moodys_rate_pct,
    peer = panel$peer_rate_pct)
  eligibility <- list(
    primary = is.finite(rates$primary) & observed_allowed,
    ids = panel$ids_positive_rate_observed & fallback_allowed &
      (!hold_low_ids_for_review | !panel$ids_legacy_low_rate_rule_conflict),
    secondary = is.finite(rates$secondary) & observed_allowed,
    moodys = panel$rating_moodys_available %in% TRUE & is.finite(rates$moodys) & fallback_allowed,
    peer = include_broad_peer & panel$peer_minimum_met %in% TRUE & is.finite(rates$peer) & fallback_allowed
  )
  # Candidate-context status records are carried explicitly. The absence of an
  # individual case record is never labelled as a verified absence of stress.
  result <- panel[c(keys, "country", "historical_income_level", "historical_lmic_reporting_scope",
                    "status_review_coverage", "peer_pool_rule", "peer_country_count", "peer_iqr_pp")]
  result$preview_variant <- variant
  result$preview_source <- "no_eligible_rate"
  result$preview_rate_pct <- NA_real_
  result$parent_evidence_id <- NA_character_
  parents <- list(primary = panel$primary_usd_source_evidence_row_id,
    ids = panel$ids_evidence_id, secondary = panel$secondary_usd_source_evidence_row_id,
    moodys = paste(panel$analysis_year, panel$iso3, panel$rating_moodys_variant_id, sep = "::"),
    peer = panel$peer_evidence_id)
  for (tier in order) {
    take <- result$preview_source == "no_eligible_rate" & eligibility[[tier]] %in% TRUE
    result$preview_rate_pct[take] <- rates[[tier]][take]
    result$preview_source[take] <- tier
    result$parent_evidence_id[take] <- parents[[tier]][take]
  }
  result$preview_currency_basis <- ifelse(result$preview_source == "ids",
    "IDS_currency_composition_unknown", ifelse(is.finite(result$preview_rate_pct), "USD", NA_character_))
  result$preview_timing_basis <- dplyr::case_when(
    result$preview_source == "primary" ~ "annual_issue_flow",
    result$preview_source == "secondary" ~ "around_year_end_stock",
    result$preview_source == "ids" ~ "annual_new_commitment_terms",
    result$preview_source == "moodys" ~ "BOY_rating_and_annual_reference_inputs",
    result$preview_source == "peer" ~ "same_year_peer_primary_flow",
    TRUE ~ NA_character_
  )
  result$approved_selection <- FALSE
  result$preview_state <- "sensitivity_only_not_accepted_ladder"
  result
}
