p15_analysis_schedule <- function(rate,maturity,grace) {
  if(grace==maturity)p15_bullet_equal_schedule(rate,maturity,grace) else
    p15_pv_schedule(rate,maturity,grace)
}

# Exact two-block, order-averaged decomposition. Accounting, not causation.
p15_pv_two_block <- function(old_flows,new_flows,old_discount,new_discount) {
  oo <- p15_stream_pv(old_flows,old_discount)
  on <- p15_stream_pv(old_flows,new_discount)
  no <- p15_stream_pv(new_flows,old_discount)
  nn <- p15_stream_pv(new_flows,new_discount)
  c(change=nn-oo,benchmark_effect=((on-oo)+(nn-no))/2,
    terms_effect=((no-oo)+(nn-on))/2)
}

p15_interpretation_views <- function(x) {
  dplyr::bind_rows(dplyr::mutate(x,evidence_view="all_selected"),
    dplyr::filter(x,selected_tier!="peer") |> dplyr::mutate(evidence_view="without_peers"),
    dplyr::filter(x,selected_tier %in% c("primary","ids")) |>
      dplyr::mutate(evidence_view="primary_or_ids"))
}

p15_interpretation_summary <- function(x) {
  dplyr::summarise(x,n=dplyr::n(),countries=dplyr::n_distinct(iso3),
    mean_market_ge=mean(market_grant_element_analogue_pct),
    median_market_ge=median(market_grant_element_analogue_pct),
    mean_policy_ge=mean(policy_grant_element_analogue_pct),
    mean_difference=mean(policy_pv_minus_market_pv),
    median_difference=median(policy_pv_minus_market_pv),
    p10_difference=as.numeric(stats::quantile(policy_pv_minus_market_pv,.1)),
    p90_difference=as.numeric(stats::quantile(policy_pv_minus_market_pv,.9)),
    market_ge_positive_share=mean(market_grant_element_analogue_pct>0),
    market_more_concessional_share=mean(policy_pv_minus_market_pv>0),
    mean_official_rate=mean(official_rate),mean_benchmark=mean(selected_rate_pct),
    mean_maturity=mean(official_maturity_years),mean_grace=mean(official_grace_years),
    .groups="drop")
}
