#!/usr/bin/env Rscript
# Run from repository root. Reuses frozen source files; never refreshes on rebuild.
source('scripts/p15/activate_p15_environment.R')
suppressPackageStartupMessages({library(dplyr); library(tidyr); library(readr)})
root <- 'experiments/p15_wb_scorecard_20260912'
out <- file.path(root, 'supplemental')
macro <- read_csv(file.path(root, 'macro/macro_input_panel.csv'), show_col_types=FALSE)

# Only the overall WEF-owned index is extracted, under its CC BY-NC 4.0 terms.
# The newer 0–100 GCI4.0 is a different measure and is not substituted here.
wef <- readxl::read_excel(file.path(out, 'wef_gci_2007_2017.xlsx'), sheet='Data', skip=2,
                         .name_repair='unique_quiet')
gci <- wef |>
  filter(.data[['GLOBAL ID']]=='GCI', Attribute=='Value') |>
  select(Edition, matches('^[A-Z]{3}$')) |>
  pivot_longer(-Edition, names_to='iso3', values_to='wef_gci') |>
  mutate(wef_gci=suppressWarnings(as.numeric(wef_gci)),
         gci_reference_year=as.integer(substr(Edition,1,4)),
         gci_edition=Edition,
         gci_source='World Economic Forum, Global Competitiveness Index dataset 2007–2017',
         gci_license='CC BY-NC 4.0; overall index extracted; no endorsement') |>
  filter(is.finite(wef_gci)) |> select(-Edition) |> arrange(iso3,gci_reference_year)
stopifnot(!anyDuplicated(gci[c('iso3','gci_reference_year')]), all(gci$wef_gci>=1 & gci$wef_gci<=7))
write_csv(gci, file.path(out,'wef_legacy_gci_long.csv'))

# WDI is the explicit external-PPG-debt fallback named in WB Appendix D. It is
# residence-based debt, not a direct measurement of foreign-currency public debt.
debt_raw <- jsonlite::fromJSON(file.path(out,'wdi_ppg_external_debt.json'))
stopifnot(debt_raw[[1]]$pages==1, nrow(debt_raw[[2]])==debt_raw[[1]]$total)
debt <- tibble(iso3=debt_raw[[2]]$countryiso3code,
               reference_year=as.integer(debt_raw[[2]]$date),
               external_ppg_debt_usd=as.numeric(debt_raw[[2]]$value)) |>
  filter(iso3 %in% macro$iso3)
stopifnot(!anyDuplicated(debt[c('iso3','reference_year')]))
write_csv(debt,file.path(out,'wdi_external_ppg_debt_long.csv'))

source('R/p15_status_context.R')
boc_path <- 'data-raw/status_context_sources/boc_boe_sovereign_default_database_DEBT_2025_2026-06-30.json'
defaults <- p15_parse_boc_boe_default(boc_path,years=1960:2024) |>
  mutate(bond_or_bank_stock_positive = boc_fc_bonds_default_debt_usd_mn>0 |
           boc_fc_bank_loans_default_debt_usd_mn>0,
         bond_bank_or_lc_stock_positive=bond_or_bank_stock_positive |
           boc_local_currency_debt_default_usd_mn>0)
write_csv(defaults, file.path(out,'boc_default_stocks_by_year.csv'))

panel <- macro |>
  left_join(debt,by=c('iso3','reference_year')) |>
  mutate(public_debt_usd = debt_gdp/100 * nominal_gdp_usd_bn * 1e9,
         fc_share_external_ppg_proxy = if_else(public_debt_usd>0,
             100*external_ppg_debt_usd/public_debt_usd, NA_real_),
         fc_share_proxy_out_of_range = is.finite(fc_share_external_ppg_proxy) &
           (fc_share_external_ppg_proxy<0 | fc_share_external_ppg_proxy>100),
         fc_share_proxy_in_range=if_else(!fc_share_proxy_out_of_range,
           fc_share_external_ppg_proxy,NA_real_),
         fc_share_source='WDI DT.DOD.DPPG.CD, latest-revised numerator; vintage WEO general-government debt denominator',
         fc_share_wdi_last_updated=debt_raw[[1]]$lastupdated)
gci_results <- lapply(seq_len(nrow(panel)),function(i) {
  iso <- panel$iso3[i]; t <- panel$reference_year[i]
  available <- gci |> filter(iso3==iso,gci_reference_year<=t) |> arrange(desc(gci_reference_year))
  last <- available |> slice_head(n=1)
  age <- if(nrow(last)) t-last$gci_reference_year else NA_integer_
  tibble(iso3=iso,analysis_year=panel$analysis_year[i],
         gci_same_year=if(nrow(last) && age==0) last$wef_gci else NA_real_,
         gci_carry_max2=if(nrow(last) && age<=2) last$wef_gci else NA_real_,
         gci_last_available=if(nrow(last)) last$wef_gci else NA_real_,
         gci_age_years=age,
         gci_input_reference_year=if(nrow(last)) last$gci_reference_year else NA_integer_)
}) |> bind_rows()
default_results <- lapply(seq_len(nrow(panel)),function(i) {
  iso <- panel$iso3[i]; t <- panel$reference_year[i]
  h <- defaults |> filter(iso3==iso,analysis_year<=t)
  private <- h |> filter(bond_or_bank_stock_positive,analysis_year>=1983)
  broader <- h |> filter(bond_bank_or_lc_stock_positive,analysis_year>=1983)
  last <- if(nrow(private)) max(private$analysis_year) else NA_integer_
  tibble(iso3=iso,analysis_year=panel$analysis_year[i],
         boc_country_has_any_historical_record=nrow(h)>0,
         boc_bond_bank_last_positive_year=last,
         boc_bond_bank_positive_within10=any(private$analysis_year>=t-9),
         boc_bond_bank_positive_within15=any(private$analysis_year>=t-14),
         boc_bond_bank_positive_within20=any(private$analysis_year>=t-19),
         boc_bond_bank_positive_since1983=nrow(private)>0,
         boc_bond_bank_lc_positive_since1983=nrow(broader)>0,
         default_history_status='BoC–BoE2025 revised stock-history proxy, not Moody default-event series; absence means no recorded stock',
         default_history_information_basis='latest-revised historical observations; future years excluded; not a historical publication vintage')
}) |> bind_rows()
panel <- panel |> left_join(gci_results,by=c('iso3','analysis_year')) |>
  left_join(default_results,by=c('iso3','analysis_year'))
stopifnot(nrow(panel)==2743, !anyDuplicated(panel[c('iso3','analysis_year')]),
          all(is.na(panel$gci_age_years) | panel$gci_age_years>=0),
          all(is.na(panel$boc_bond_bank_last_positive_year) |
              panel$boc_bond_bank_last_positive_year<=panel$reference_year))
write_csv(panel,file.path(root,'scorecard_inputs.csv'))
coverage <- panel |>
  mutate(scope=case_when(historical_lmic_reporting_scope & selected_tier=='peer'~'selected_lmic_peer',
                        historical_lmic_reporting_scope~'other_lmic',TRUE~'other_country_year')) |>
  group_by(scope) |>
  summarise(n=n(),countries=n_distinct(iso3),
    gci_current=sum(is.finite(gci_same_year)),gci_carry_max2_n=sum(is.finite(gci_carry_max2)),
    gci_any_last=sum(is.finite(gci_last_available)),
    fc_proxy_raw=sum(is.finite(fc_share_external_ppg_proxy)),
    fc_proxy_in_range=sum(is.finite(fc_share_proxy_in_range)),
    fc_proxy_out_of_range=sum(fc_share_proxy_out_of_range),
    complete_net_gci_current_fc=sum(macro_complete_net_interest_proxy & is.finite(gci_same_year) & is.finite(fc_share_proxy_in_range)),
    complete_net_gci_max2_fc=sum(macro_complete_net_interest_proxy & is.finite(gci_carry_max2) & is.finite(fc_share_proxy_in_range)),
    complete_net_gci_any_fc=sum(macro_complete_net_interest_proxy & is.finite(gci_last_available) & is.finite(fc_share_proxy_in_range)),
    .groups='drop')
write_csv(coverage,file.path(out,'supplemental_coverage.csv'))
inputs <- c(file.path(out,'wef_gci_2007_2017.xlsx'),file.path(out,'wdi_ppg_external_debt.json'),
            file.path(out,'wdi_ppg_external_debt_meta.json'),boc_path,
            file.path(root,'macro/macro_input_panel.csv'))
manifest <- tibble(path=inputs,sha256=vapply(inputs,digest::digest,character(1),algo='sha256',file=TRUE),
                   source_snapshot_id=c('WEF-GCI-LEGACY-20260912','WDI-PPG-20260912','WDI-PPG-META-20260912',
                     'SRC-BOC-BOE-DEBT-2025-20260630','WEO-VINTAGE-PANEL-20260912'))
write_csv(manifest,file.path(out,'input_manifest.csv'))
print(coverage,width=Inf)
