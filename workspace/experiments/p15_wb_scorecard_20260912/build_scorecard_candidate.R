#!/usr/bin/env Rscript
# Data-completeness audit and explicit scorecard-to-peer handoff.
# This build does not invent the unavailable November2018 scoring rules.
source('scripts/p15/activate_p15_environment.R')
suppressPackageStartupMessages({library(dplyr); library(tidyr); library(readr)})
root <- 'experiments/p15_wb_scorecard_20260912'
source(file.path(root,'scorecard_rules.R'))
x <- read_csv(file.path(root,'scorecard_inputs.csv'),show_col_types=FALSE)
stopifnot(nrow(x)==2743,!anyDuplicated(x[c('iso3','analysis_year')]))
policies <- tibble::tribble(
  ~input_scenario, ~interest_basis, ~gci_field,
  'strict_historical_gross_current_gci','strict','gci_same_year',
  'net_interest_current_gci','net','gci_same_year',
  'net_interest_gci_carry_max2','net','gci_carry_max2',
  'net_interest_stale_gci_hold_diagnostic','net','gci_last_available',
  'latest_gfs_gross_gci_carry_max2','gfs_latest','gci_carry_max2')
audit <- lapply(seq_len(nrow(policies)),function(i) {
  p <- policies[i,]
  ig <- switch(p$interest_basis,strict=x$gross_interest_gdp,
               net=x$implied_net_interest_gdp,gfs_latest=x$gfs_gross_interest_gdp_latest)
  ir <- switch(p$interest_basis,strict=x$gross_interest_revenue,
               net=x$implied_net_interest_revenue,gfs_latest=x$gfs_gross_interest_revenue_latest)
  input <- tibble(growth_mean=x$growth_avg7,growth_sd=x$growth_sd10,
    wef_gci=x[[p$gci_field]],gdp_usd_bn=x$nominal_gdp_usd_bn,gdppc_ppp=x$gdp_pc_ppp,
    inflation_mean=x$inflation_avg7,inflation_sd=x$inflation_sd10,
    debt_gdp=x$debt_gdp,debt_revenue=x$debt_revenue,
    interest_gdp=ig,interest_revenue=ir,debt_trend=x$debt_trend_pp,
    fc_debt_share=x$fc_share_proxy_in_range)
  ready <- scorecard_2018_readiness(input)
  scored <- scorecard_score_2018(input)
  missing <- apply(input,1,function(r) paste(names(input)[!is.finite(r)],collapse=';'))
  bind_cols(x |> select(iso3,country,analysis_year,reference_year,historical_income_level,
      historical_lmic_reporting_scope,selected_tier),
    tibble(input_scenario=p$input_scenario,interest_basis=p$interest_basis,
      gci_field=p$gci_field,gci_age_years=x$gci_age_years,
      input_metrics_complete=ready$input_metrics_complete,
      missing_numeric_inputs=missing,
      missing_rule_bundle=ready$reason,
      default_source_complete=FALSE,
      data_fidelity='External-PPG debt proxy; revised WDI/BoC history; default events not Moody series',
      scorecard_complete=is.finite(scored$rating_notch),shadow_notch=scored$rating_notch,
      scorecard_status=scored$scorecard_status,
      scorecard_missing_rules=scored$scorecard_missing_rules))
}) |> bind_rows()
write_csv(audit,file.path(root,'input_scenario_country_years.csv'))
coverage <- audit |>
  filter(historical_lmic_reporting_scope,selected_tier=='peer') |>
  group_by(input_scenario,historical_income_level) |>
  summarise(selected_peer_country_years=n(),complete_input_country_years=sum(input_metrics_complete),
    data_complete_countries=n_distinct(iso3[input_metrics_complete]),
    completed_shadow_ratings=sum(scorecard_complete & is.finite(shadow_notch)),.groups='drop')
write_csv(coverage,file.path(root,'input_scenario_coverage_income.csv'))
write_csv(audit |> filter(historical_lmic_reporting_scope,selected_tier=='peer') |>
  group_by(input_scenario,analysis_year) |>
  summarise(selected_peer_country_years=n(),complete_input_country_years=sum(input_metrics_complete),
    completed_shadow_ratings=sum(scorecard_complete & is.finite(shadow_notch)),.groups='drop'),
  file.path(root,'input_scenario_coverage_year.csv'))

# A complete absence of scorecard grades is an explicit blocked input, not a
# null-data estimate. The peer module still runs the observed-only controls.
handoff <- audit |> filter(input_scenario=='strict_historical_gross_current_gci') |>
  transmute(iso3,analysis_year,scenario='wb2018_source_reconstruction',
  shadow_notch,scorecard_complete,
  status='blocked_missing_verified_2018_indicator_bins_weights_rounding_adjustments',
  shadow_notch_lower=NA_real_,shadow_notch_upper=NA_real_)
stopifnot(!any(is.finite(handoff$shadow_notch)),!any(handoff$scorecard_complete))
write_csv(handoff,file.path(root,'scorecard_country_year.csv'))
write_csv(tibble(component=c('macro_inputs','supplemental_inputs','factor_to_range_engine',
                             'macro_to_factor_2018','new_shadow_ratings','peer_and_valuation_controls'),
  status=c('built','built_with_explicit_gaps','built_historical_matrices_six_2019_paths_verified',
           'blocked_missing_source_rules','not_produced','ready'),
  reason=c('13 dated fall WEO vintages; 2743 target rows',
    'Legacy GCI, WDI external-PPG debt, BoC default-stock history',
    'Six examples do not prove every 2018 matrix cell',
    'Exact November27,2018 Moody methodology requires access; public WB appendix omits numeric tables',
    'Input coverage is not rating coverage',
    'Observed grades plus primary/IDS/secondary donor choices, minimum three distinct donors')),
  file.path(root,'build_status.csv'))
print(coverage,width=Inf)
