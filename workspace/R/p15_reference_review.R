# Forward closest-date construction and bounded peer specification review.
# Call after the preserved baseline replay. Never change the legacy estimator path.

p15_review_secondary_panel <- function(panel, anchors, evidence) {
  keys <- c("analysis_year", "iso3")
  for (currency in c("USD", "EUR")) {
    prefix <- paste0("secondary_", tolower(currency), "_")
    fields <- sub(prefix, "", names(panel)[startsWith(names(panel), prefix)], fixed = TRUE)
    fields <- setdiff(fields, "availability")
    a <- anchors |> dplyr::filter(observed_market_branch == "observed_secondary", .data$currency == .env$currency)
    p15_local_assert_keys(a, keys, "updated secondary anchors")
    a <- a[c(keys, fields)]
    names(a)[-(1:2)] <- paste0(prefix, fields)
    panel <- panel |> dplyr::select(-dplyr::starts_with(prefix)) |> dplyr::left_join(a, by = keys)
    present <- evidence |> dplyr::filter(.data$currency == .env$currency) |> dplyr::distinct(analysis_year, iso3)
    panel[[paste0(prefix,"availability")]] <- ifelse(is.finite(panel[[paste0(prefix,"market_rate_pct")]]),
      "ordinary_observed_candidate_available", ifelse(paste(panel$analysis_year,panel$iso3) %in%
        paste(present$analysis_year,present$iso3), "evidence_retained_without_ordinary_anchor", "no_candidate_in_current_archive"))
  }
  panel |> dplyr::mutate(
    any_usd_observed_candidate = is.finite(primary_usd_market_rate_pct) | is.finite(secondary_usd_market_rate_pct),
    any_eur_observed_candidate = is.finite(primary_eur_market_rate_pct) | is.finite(secondary_eur_market_rate_pct),
    usd_primary_secondary_gap_pp = primary_usd_market_rate_pct-secondary_usd_market_rate_pct,
    comparison_timing_warning = dplyr::if_else(is.finite(usd_primary_secondary_gap_pp),
      "annual_issue_flow_versus_year_end_stock_not_contemporaneous", NA_character_),
    fallback_audit_cohort = dplyr::case_when(is.finite(primary_usd_market_rate_pct) ~ "usd_primary_observed",
      any_usd_observed_candidate | ids_benchmark_proxy_candidate_permitted ~ "other_observed_or_reviewed_ids",
      TRUE ~ "no_usd_observed_or_reviewed_ids"))
}

p15_review_previews <- function(panel) {
  p <- panel
  p$ids_positive_rate_observed <- p$ids_benchmark_proxy_candidate_permitted
  dplyr::bind_rows(lapply(c(TRUE,FALSE), function(ids) dplyr::bind_rows(lapply(c(FALSE,TRUE), function(peer) {
    p15_local_ladder_preview(p, ids_before_secondary=ids, hold_low_ids_for_review=FALSE, include_broad_peer=peer)
  }))))
}

p15_review_peer_options <- function(panel, anchors, context) {
  keys <- c("analysis_year","iso3")
  cols <- c(keys,"moodys_rating_normalized","rating_source_region","risk_free_7y_pct")
  seeds <- anchors |> dplyr::filter(observed_market_branch=="observed_primary",currency=="USD",
    is.finite(market_rate_pct),is.finite(sovereign_spread_pct)) |> dplyr::left_join(context[cols],by=keys)
  targets <- dplyr::left_join(panel,context[cols],by=keys)
  methods <- c("similarity_min3","income_min3","region_min3","similarity_min5",
    "similarity_min3_no_global","income_spread_first_min3")
  details <- list(); members <- list(); k <- 0L
  for(i in seq_len(nrow(targets))) {
    t <- targets[i,]; e <- seeds[seeds$analysis_year==t$analysis_year & seeds$iso3!=t$iso3,]
    similar3 <- p15_peer_similarity_pool(e,t,3L)
    similar5 <- p15_peer_similarity_pool(e,t,5L)
    inc <- e[!is.na(e$historical_income_level)&e$historical_income_level %in% t$historical_income_level,]
    reg <- e[!is.na(e$rating_source_region)&e$rating_source_region %in% t$rating_source_region,]
    income <- if(nrow(inc)>=3L) list(pool=inc,rule="same_income_min3") else list(pool=e,rule="global_min3")
    region <- if(nrow(reg)>=3L) list(pool=reg,rule="same_region_min3") else list(pool=e,rule="global_min3")
    options <- list(similar3,income,region,similar5,similar3,income)
    for(j in seq_along(methods)) {
      k <- k+1L; pick <- options[[j]]; pool <- pick$pool; minimum <- if(j==4L)5L else 3L
      permitted <- nrow(pool)>=minimum && !(j==5L && grepl("^global",pick$rule))
      values <- pool$market_rate_pct
      rate <- if(permitted) median(values) else NA_real_
      if(j==6L && permitted) rate <- median(pool$sovereign_spread_pct)+t$risk_free_7y_pct
      details[[k]] <- tibble::tibble(analysis_year=t$analysis_year,iso3=t$iso3,
        historical_lmic_reporting_scope=t$historical_lmic_reporting_scope,
        peer_method=methods[j],peer_pool_rule=pick$rule,peer_country_count=nrow(pool),
        peer_rate_pct=rate,peer_minimum_met=permitted,
        target_rating_missing=is.na(p15_rating_notch_number(t$moodys_rating_normalized)),
        global_pool=grepl("^global",pick$rule),rating_proximity_used=grepl("rating3",pick$rule),
        peer_iqr_pp=if(length(values))diff(stats::quantile(values,c(.25,.75),names=FALSE)) else NA_real_,
        largest_peer_maturity_gap_years=if(nrow(pool))max(abs(pool$market_maturity_years-7),na.rm=TRUE) else NA_real_,
        observed_primary_rate_pct=t$primary_usd_market_rate_pct,
        error_pp=rate-t$primary_usd_market_rate_pct,
        ordinary_fallback_selection_permitted=t$ordinary_fallback_selection_permitted,
        interpretation="same_year_observed_primary_peer_scenario_not_proof_of_target_market_access")
      if(nrow(pool)) members[[k]] <- tibble::tibble(analysis_year=t$analysis_year,target_iso3=t$iso3,
        peer_method=methods[j],peer_iso3=pool$iso3,peer_rate_pct=values,
        peer_spread_pct=pool$sovereign_spread_pct,peer_maturity_years=pool$market_maturity_years,
        peer_income=pool$historical_income_level,peer_region=pool$rating_source_region,
        peer_rating=pool$moodys_rating_normalized,peer_source_evidence_row_id=pool$source_evidence_row_id,
        peer_pool_rule=pick$rule,used_for_estimate=permitted)
    }
  }
  list(detail=dplyr::bind_rows(details),membership=dplyr::bind_rows(members))
}

p15_review_targeted_direct <- function(source_issues, raw_history) {
  # Original issue attributes are reconstructed upstream from the raw snapshot.
  # Re-select each identifier from its raw dated yield column; don't copy rates.
  issues <- p15_prepare_secondary_direct_issues(source_issues,2024L)
  selected <- list(); k <- 0L
  for(i in seq_len(nrow(issues))) {
    rics <- strsplit(issues$representative_rics[i],";",fixed=TRUE)[[1]]
    for(ric in rics) {
      field <- paste0(ric,"__Yield to Maturity")
      if(!field %in% names(raw_history)) next
      q <- tibble::tibble(date=as.Date(raw_history$Date),value=p15_observed_num(raw_history[[field]]),
        snapshot=as.Date("2024-12-31")) |> dplyr::filter(is.finite(value))
      q <- p15_secondary_date_subset(q,"date","snapshot","closest",31,7)
      if(!nrow(q)) next
      k<-k+1L
      selected[[k]] <- tibble::tibble(economic_issue_key=issues$economic_issue_key[i],ric=ric,
        selected_date=q$date[1],yield_pct=median(q$value),same_date_range_pp=diff(range(q$value)))
    }
  }
  quotes <- dplyr::bind_rows(selected)
  summary <- quotes |> dplyr::group_by(economic_issue_key) |> dplyr::summarise(
    new_yield=median(yield_pct),first_date=min(selected_date),last_date=max(selected_date),
    conflict=any(same_date_range_pp>.05),.groups="drop")
  issues <- issues |> dplyr::left_join(summary,by="economic_issue_key") |>
    dplyr::mutate(secondary_yield_pct=new_yield,secondary_quote_date=first_date,
      p12a_standard_usd_2_15_candidate=p12a_standard_usd_2_15_candidate &
        is.finite(new_yield) & !dplyr::coalesce(conflict,TRUE))
  country <- p15_aggregate_secondary_direct(issues,"p12a_standard_usd_2_15_candidate",
    "p12a_secondary_direct_yield_rebuild","SRC-P12A-SECONDARY-DIRECT-2024","P15_TARGETED_CLOSEST_20260907")
  dates <- issues |> dplyr::filter(p12a_standard_usd_2_15_candidate) |>
    dplyr::group_by(analysis_year,iso3) |> dplyr::summarise(first_rate_date=min(first_date),last_rate_date=max(last_date),.groups="drop")
  list(country=country,dates=dates,quotes=quotes,issues=issues)
}
