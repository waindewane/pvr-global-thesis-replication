# Fill geographic gaps only. Keep the established source-vintage partition and
# peer hierarchy; descriptive pre-2025 regions are a separate reporting choice.
p15_fill_peer_regions <- function(context, geography) {
  stopifnot(!anyDuplicated(context[c("analysis_year","iso3")]),
            !anyDuplicated(geography$iso3))
  x <- context
  original <- as.character(x$rating_source_region)
  available <- trimws(geography$wb_region[match(x$iso3,geography$iso3)])
  missing <- is.na(original) | trimws(original)==""
  fill <- missing & !is.na(available) & nzchar(available) & available!="Aggregates"
  x$rating_source_region_before <- original
  x$rating_source_region[fill] <- available[fill]
  x$peer_region_filled <- fill
  x$peer_region_source <- ifelse(fill,"SRC-WB-COUNTRIES-LOCAL-PEER-20260909","original_rating_context")
  x
}

p15_recompute_reference_peers <- function(panel, context, eligibility, seed_metadata) {
  keys <- c("analysis_year","iso3")
  stopifnot(!anyDuplicated(panel[keys]),!anyDuplicated(context[keys]))
  at <- match(paste(panel$analysis_year,panel$iso3),paste(context$analysis_year,context$iso3))
  stopifnot(!anyNA(at))
  x <- panel
  x$rating_source_region <- context$rating_source_region[at]
  x$moodys_rating_normalized <- context$moodys_rating_normalized[at]
  e <- eligibility[eligibility$tier=="primary",]
  ei <- match(paste(x$analysis_year,x$iso3),paste(e$analysis_year,e$iso3))
  stopifnot(!anyNA(ei))
  x$primary_eligible <- e$eligible[ei]
  detail <- membership <- vector("list",nrow(x))
  for(i in seq_len(nrow(x))) {
    target <- x[i,]
    seeds <- x[x$analysis_year==target$analysis_year & x$iso3!=target$iso3 & x$primary_eligible,]
    pick <- p15_peer_similarity_pool(seeds,target,3L)
    z <- pick$pool; usable <- nrow(z)>=3L
    rates <- z$primary_usd_market_rate_pct
    detail[[i]] <- data.frame(analysis_year=target$analysis_year,iso3=target$iso3,
      peer_rate_pct=if(usable)median(rates) else NA_real_,peer_minimum_met=usable,
      peer_pool_rule=pick$rule,peer_country_count=nrow(z),
      peer_iqr_pp=if(length(rates))diff(stats::quantile(rates,c(.25,.75),names=FALSE)) else NA_real_,
      global_pool_used=grepl("^global",pick$rule))
    if(nrow(z)) {
      si <- match(paste(z$analysis_year,z$iso3),paste(seed_metadata$analysis_year,seed_metadata$peer_iso3))
      stopifnot(!anyNA(si))
      membership[[i]] <- data.frame(analysis_year=target$analysis_year,target_iso3=target$iso3,
        peer_method="similarity_min3",peer_iso3=z$iso3,peer_rate_pct=rates,
        peer_spread_pct=seed_metadata$peer_spread_pct[si],
        peer_maturity_years=z$primary_usd_market_maturity_years,
        peer_income=z$historical_income_level,peer_region=z$rating_source_region,
        peer_rating=z$moodys_rating_normalized,
        peer_source_evidence_row_id=z$primary_usd_source_evidence_row_id,
        peer_pool_rule=pick$rule,used_for_estimate=usable)
    }
  }
  list(detail=dplyr::bind_rows(detail),membership=dplyr::bind_rows(membership))
}
