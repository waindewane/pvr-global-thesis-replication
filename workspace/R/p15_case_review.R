# Case decisions are data, not undocumented exceptions embedded in an estimator.
p15_apply_ids_case_review <- function(panel, cases, definitions) {
  keys <- c("analysis_year","iso3")
  p15_local_assert_keys(cases,keys,"IDS case decisions")
  p15_local_assert_keys(definitions,"decision_code","IDS decision definitions")
  p15_local_assert_grid_members(cases,panel,"IDS case decisions")
  required_text <- c("explanation","next_evidence_needed","benchmark_proxy_use","contractual_rate_use","repayment_profile_use")
  if(any(vapply(definitions[required_text],function(x) any(is.na(x)|!nzchar(trimws(x))),logical(1)))) stop("Missing case rationale or permission")
  if(any(!cases$decision_code %in% definitions$decision_code)) stop("Unknown IDS decision code")
  if(any(!definitions$benchmark_proxy_use %in% c("hold","eligible_with_warning"))) stop("Unknown IDS benchmark permission")
  checked <- dplyr::left_join(cases,panel[c(keys,"ids_rate_pct","ids_maturity_years","ids_grace_years")],by=keys)
  for(pair in list(c("expected_rate_pct","ids_rate_pct"),c("expected_maturity_years","ids_maturity_years"),c("expected_grace_years","ids_grace_years"))) {
    if(any(!is.finite(checked[[pair[2]]]) | abs(checked[[pair[1]]]-checked[[pair[2]]])>1e-12)) stop("Case decision no longer matches source: ",pair[2])
  }
  # A fresh unreviewed anomaly is a blocker, not implicitly covered by an old case.
  needs <- panel |>
    dplyr::filter((is.finite(ids_rate_pct)&ids_rate_pct!=0&(ids_rate_pct<1|ids_rate_pct>30)) |
      (ids_positive_rate_observed & !ids_term_order_valid) |
      (ids_zero_rate_review & ids_term_order_valid))
  if(nrow(dplyr::anti_join(needs,cases,by=keys))) stop("New IDS review case lacks a disposition")
  detail <- dplyr::left_join(checked,definitions,by="decision_code")
  addition <- detail |> dplyr::select(dplyr::all_of(keys),case_id,decision_code,source_rate_interpretation,
    contractual_rate_use,benchmark_proxy_use,repayment_profile_use,explanation,next_evidence_needed,evidence_ids,
    documented_bond_coupon_pct,documented_bond_currency)
  names(addition)[-(1:2)] <- paste0("ids_review_",names(addition)[-(1:2)])
  result <- dplyr::left_join(panel,addition,by=keys) |>
    dplyr::mutate(ids_review_disposition_present=!is.na(ids_review_case_id),
      ids_benchmark_proxy_candidate_permitted=ids_positive_rate_observed &
        (is.na(ids_review_benchmark_proxy_use)|ids_review_benchmark_proxy_use=="eligible_with_warning"),
      ids_reviewed_proxy_rate_pct=dplyr::if_else(ids_benchmark_proxy_candidate_permitted,ids_rate_pct,NA_real_),
      ids_reviewed_term_state=dplyr::case_when(
        ids_review_repayment_profile_use=="blocked" ~ "blocked_inconsistent_terms",
        ids_review_repayment_profile_use=="hold" ~ "held_case_scope_or_rate_not_validated",
        ids_term_order_valid ~ "arithmetic_order_valid_not_a_validated_cashflow_schedule",
        TRUE ~ "missing_or_invalid_terms"))
  list(panel=result,cases=detail)
}

p15_apply_observed_case_review <- function(panel,anchors,cases) {
  p15_local_assert_keys(cases,c("analysis_year","iso3","observed_market_branch","currency"),"Observed case decisions")
  if(any(cases$action!="hold_anchor_and_peer_seed")) stop("Unknown observed case action")
  if(any(is.na(cases$explanation)|!nzchar(cases$explanation))) stop("Missing observed case rationale")
  panel$observed_case_id <- NA_character_
  panel$observed_case_explanation <- NA_character_
  panel$primary_usd_pre_review_rate_pct <- panel$primary_usd_market_rate_pct
  held <- rep(FALSE,nrow(anchors))
  for(i in seq_len(nrow(cases))) {
    c <- cases[i,]
    at <- which(anchors$analysis_year==c$analysis_year & anchors$iso3==c$iso3 &
      anchors$currency==c$currency & anchors$observed_market_branch==c$observed_market_branch)
    if(length(at)!=1L) stop("Observed decision does not resolve exactly one anchor")
    a <- anchors[at,]
    if(c$expected_issue_count!=1L || a$retained_quantitative_issue_count!=1L ||
       a$included_issue_keys!=c$economic_issue_key || abs(a$market_rate_pct-c$expected_country_rate_pct)>1e-12) {
      stop("Observed case changed; reaggregate and review rather than dropping a mixed country-year")
    }
    if(c$currency!="USD" || c$observed_market_branch!="observed_primary") stop("Unsupported observed case scope")
    j <- which(panel$analysis_year==c$analysis_year & panel$iso3==c$iso3)
    if(length(j)!=1L)stop("Observed case missing from panel")
    panel$primary_usd_market_rate_pct[j] <- NA_real_
    panel$primary_usd_availability[j] <- "case_review_hold_not_ordinary_bond_financing"
    panel$observed_case_id[j] <- c$case_id
    panel$observed_case_explanation[j] <- c$explanation
    held[at] <- TRUE
  }
  panel <- panel |> dplyr::mutate(
    any_usd_observed_candidate=is.finite(primary_usd_market_rate_pct)|is.finite(secondary_usd_market_rate_pct),
    usd_primary_secondary_gap_pp=primary_usd_market_rate_pct-secondary_usd_market_rate_pct,
    comparison_timing_warning=dplyr::if_else(is.finite(usd_primary_secondary_gap_pp),
      "annual_issue_flow_versus_year_end_stock_not_contemporaneous",NA_character_),
    fallback_audit_cohort=dplyr::case_when(is.finite(primary_usd_market_rate_pct)~"usd_primary_observed",
      any_usd_observed_candidate|ids_benchmark_proxy_candidate_permitted~"other_observed_or_reviewed_ids",
      TRUE~"no_usd_observed_or_reviewed_ids"))
  list(panel=panel,anchors=anchors[!held,],held_anchors=anchors[held,])
}

p15_read_ids_audit_snapshot <- function(path) {
  x <- jsonlite::fromJSON(path)
  if(as.integer(x$pages)!=1L || as.integer(x$page)!=1L || x$source$id!="6")stop("Unexpected IDS audit metadata")
  d <- x$source$data
  get <- function(v,key) v$id[match(key,v$concept)]
  result <- tibble::tibble(analysis_year=as.integer(sub("YR","",vapply(d$variable,get,character(1),key="Time"))),
    iso3=vapply(d$variable,get,character(1),key="Country"),indicator=vapply(d$variable,get,character(1),key="Series"),
    creditor=vapply(d$variable,get,character(1),key="Counterpart-Area"),value=d$value)
  if(nrow(result)!=as.integer(x$total) || any(result$creditor!="BND"))stop("IDS audit count or creditor mismatch")
  p15_local_assert_keys(result,c("analysis_year","iso3","indicator"),"IDS audit snapshot")
  result
}
