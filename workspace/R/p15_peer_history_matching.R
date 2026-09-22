# One fixed persistence sensitivity; never changes the accepted peer reference.
p15_persistent_match <- function(matches, observed) {
  observed <- !is.na(observed) & observed
  n <- sum(observed)
  n >= 2L && sum(matches[observed] %in% TRUE) / n >= 2/3
}

p15_peer_history_pool <- function(eligible, target, context) {
  year <- target$analysis_year[[1]]
  id <- target$iso3[[1]]
  th <- context[context$iso3 == id & context$analysis_year < year &
    context$analysis_year >= year - 3, , drop = FALSE]
  baseline <- p15_peer_similarity_pool(eligible, target, 3L)
  enough <- sum(!is.na(th$historical_income_level)) >= 2L
  audit <- lapply(seq_len(nrow(eligible)), function(j) {
    ph <- context[context$iso3 == eligible$iso3[j] &
      context$analysis_year < year & context$analysis_year >= year - 3, , drop = FALSE]
    h <- merge(th, ph, by = "analysis_year", suffixes = c("_target", "_peer"))
    inc_ok <- !is.na(h$historical_income_level_target) & !is.na(h$historical_income_level_peer)
    tn <- p15_rating_notch_number(h$moodys_rating_normalized_target)
    pn <- p15_rating_notch_number(h$moodys_rating_normalized_peer)
    rat_ok <- is.finite(tn) & is.finite(pn)
    inc_match <- h$historical_income_level_target == h$historical_income_level_peer
    rat_match <- abs(tn-pn) <= 3
    data.frame(analysis_year=year, target_iso3=id, peer_iso3=eligible$iso3[j],
      history_years=paste(h$analysis_year,collapse=";"),
      common_income_years=sum(inc_ok), matched_income_years=sum(inc_match %in% TRUE),
      common_rating_years=sum(rat_ok), matched_rating_years=sum(rat_match %in% TRUE),
      persistent_income=p15_persistent_match(inc_match,inc_ok),
      persistent_rating=p15_persistent_match(rat_match,rat_ok))
  })
  audit <- if(length(audit)) do.call(rbind,audit) else data.frame()
  if(!enough || !nrow(eligible)) return(list(pool=baseline$pool,rule=baseline$rule,
    target_history_available=enough,history_state="insufficient_target_history_reference_retained",audit=audit))
  # Apply current criteria too: old similarity does not override current differences.
  income <- !is.na(eligible$historical_income_level) &
    eligible$historical_income_level == target$historical_income_level[[1]] & audit$persistent_income
  region <- !is.na(eligible$rating_source_region) &
    eligible$rating_source_region == target$rating_source_region[[1]]
  tn <- p15_rating_notch_number(target$moodys_rating_normalized[[1]])
  pn <- p15_rating_notch_number(eligible$moodys_rating_normalized)
  rating <- is.finite(tn) & is.finite(pn) & abs(tn-pn)<=3 & audit$persistent_rating
  rules <- list(same_income_region_rating3=income & region & rating,
    same_income_rating3=income & rating,same_region_rating3=region & rating,
    rating3=rating,same_income_region=income & region,same_income=income,
    same_region=region,global=rep(TRUE,nrow(eligible)))
  for(rule in names(rules)) {
    pool <- eligible[rules[[rule]] %in% TRUE,,drop=FALSE]
    if(nrow(pool)>=3L || rule=="global") return(list(pool=pool,
      rule=paste0(rule,"_min3"),target_history_available=TRUE,
      history_state="persistent_income_rating_current_region",audit=audit))
  }
  stop("Unreachable history-pool state")
}
