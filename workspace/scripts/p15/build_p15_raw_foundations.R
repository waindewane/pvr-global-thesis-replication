#!/usr/bin/env Rscript
# Rebuild numeric IDS, annual spread inputs and historical classification from
# cached raw files; preserve historical estimator semantics for regression.
suppressPackageStartupMessages({library(data.table); library(dplyr); library(tidyr)
  library(tibble); library(stringr); library(readr); library(purrr); library(readxl)})
if (!file.exists(".p15_isolated_build")) stop("Raw foundation replay may run only in an isolated P15 build")
source("R/ids.R")
source("R/lseg_benchmarks.R")
source("R/p15_raw_foundations.R")
legacy <- "experiments/full_ladder_database_all_years_2026-05-20/scripts/run_all_years_scenario_database.R"
e <- new.env(parent = globalenv())
p15_legacy_assignments(legacy, c("clean_num", "country_join_key", "read_excel_with_first_row_headers",
  "normalize_damodaran_spread_to_pct", "parse_damodaran_archive_file", "as_logical_flag",
  "country_key_from", "first_non_missing", "collapse_values", "mean_or_na", "max_or_na"), e, TRUE)
metadata <- read_country_metadata("data-raw/world_bank_countries.json") |>
  transmute(iso3, country_metadata_name = country, country_join = e$country_join_key(country), income_level, lending_type, is_country) |>
  filter(is_country)
raw <- p15_read_ids_cached_pages("experiments/full_ladder_database_all_years_2026-05-20/data/ids_core_all_years")
terms <- raw |> filter(year %in% 2000:2025, iso3 %in% metadata$iso3) |>
  mutate(term = recode(indicator_id, "DT.INR.DPPG"="official_rate", "DT.MAT.DPPG"="official_maturity_years", "DT.GPA.DPPG"="official_grace_years")) |>
  select(iso3,country,year,creditor_id,term,value) |>
  pivot_wider(names_from=term, values_from=value) |>
  mutate(creditor="Bondholders", has_complete_terms = !is.na(official_rate) & !is.na(official_maturity_years) &
    !is.na(official_grace_years) & official_maturity_years > 0 & official_grace_years >= 0 & official_grace_years < official_maturity_years,
    ids_history_source="world_bank_ids_api_cached_all_years") |>
  arrange(country,year,creditor)
ids_out <- "experiments/full_ladder_database_all_years_2026-05-20/outputs/ids_terms_core_all_years.csv"
dir.create(dirname(ids_out), recursive=TRUE, showWarnings=FALSE)
fwrite(terms, ids_out, na="")
spread_dir <- "experiments/full_ladder_database_all_years_2026-05-20/data/damodaran_archive"
spread_paths <- file.path(spread_dir, paste0("ctryprem", sprintf("%02d",0:24), ifelse(2000:2024<=2020,".xls",".xlsx")))
stopifnot(all(file.exists(spread_paths)))
spreads <- map2_dfr(spread_paths, 2000:2024, e$parse_damodaran_archive_file) |>
  mutate(country_join=e$country_join_key(damodaran_country),
    archive_vintage_label=paste0("Damodaran ctryprem",str_sub(as.character(analysis_year),3,4)," archive")) |>
  left_join(metadata |> select(iso3,country=country_metadata_name,country_join,income_level,lending_type),by="country_join") |>
  filter(!is.na(iso3), analysis_year %in% 2000:2025)
fwrite(spreads, "experiments/full_ladder_database_all_years_2026-05-20/outputs/damodaran_archive_country_spreads_2000_2024.csv", na="")

# The old 2024 direct-yield companion is also recalculated, not seeded as an
# already aggregated issue table. These are preserved vendor-export CSV inputs.
e$country_metadata <- metadata
e$analysis_years <- 2000:2025
vendor_dir <- "experiments/full_ladder_database_all_years_2026-05-20/data/lseg_v14/pvr-global-lseg/output/tables"
e$path_secondary_snap <- file.path(vendor_dir,"lseg_secondary_outstanding_snapshots_oecd_base_2000-01-01_2025-12-31.csv")
e$path_secondary_hist <- file.path(vendor_dir,"lseg_secondary_market_year_end_history_oecd_base_2000-01-01_2025-12-31.csv")
p15_legacy_assignments(legacy,c("secondary_snap","secondary_hist","secondary_yield_cols","secondary_latest_yields","secondary_issue_level_all_years"),e)
fwrite(e$secondary_issue_level_all_years,"experiments/full_ladder_database_all_years_2026-05-20/outputs/secondary_issue_level_all_years.csv",na="")
e$path_fred_dgs7 <- "sources/literature_review/lr3_systematic_expansion_2026-05-12/official_sources/fred_dgs7.csv"
e$path_damodaran <- "sources/literature_review/lr3_systematic_expansion_2026-05-12/official_sources/damodaran_ctryprem.xlsx"
p15_legacy_assignments(legacy,c("fred_dgs7_annual","damodaran_regional"),e)
current <- crossing(e$damodaran_regional,e$fred_dgs7_annual) |>
  transmute(analysis_year,iso3,scenario_id="rating_implied_damodaran_current_vintage_dgs7",
    market_rate_pct=risk_free_7y_pct+default_spread_pct,
    yield_source=paste0("FRED DGS7 annual average; Damodaran current-vintage adjusted default spread; rating=",damodaran_moodys_rating))
fwrite(current,"experiments/full_ladder_database_all_years_2026-05-20/outputs/master_benchmark_scenarios_all_years.csv",na="")

historical <- "experiments/p14_historical_validation_and_extension_2026-06-22/scripts/build_p14_historical_quality_equalization_2012_2023.R"
h <- new.env(parent=globalenv())
p15_legacy_assignments(historical,c("read_csv","clean_text","parse_num","period_for_year","income_code_to_label","income_long_to_label","make_classification_ledger"),h,TRUE)
h$classification_years <- 2012:2024
h$wb_oghist_path <- "data-raw/world_bank_country_classifications/OGHIST_2026-06-30.xls"
h$owid_income_groups_path <- "data-raw/world_bank_country_classifications/owid_world_bank_income_groups_2026-06-30.csv"
h$owid_income_groups_metadata_path <- "data-raw/world_bank_country_classifications/owid_world_bank_income_groups_metadata_2026-06-30.json"
h$build_id <- "BUILD-P15-RAW-FOUNDATIONS-20260906"
h$generated_at <- "raw_source_replay"
p15_legacy_assignments(historical,c("wb_income_raw","wb_income_years","wb_income_cols","wb_country_rows","wb_income_history","owid_income_history","historical_income_sources"),h)
geo <- fread("data-raw/p15_curated_inputs_20260906/geography_and_static_labels.csv") |> as_tibble()
universe <- crossing(analysis_year=2012:2024,geo) |>
  mutate(period=h$period_for_year(analysis_year),income_level=static_income_level,lending_type=static_lending_type,
    included_in_lmic_reporting_scope=static_lmic_reporting_scope,
    p14_scope_class=case_when(static_lmic_reporting_scope~"core_developing_scope",static_income_level=="High income"~"comparison_high_income_scope",TRUE~"comparison_or_unclassified_scope")) |>
  select(-starts_with("static_"))
classifications <- h$make_classification_ledger(universe)
out <- "experiments/p14_historical_validation_and_extension_2026-06-22/outputs/p14_historical_quality_equalization_2012_2023/p14_historical_country_classification_ledger_2012_2024.csv"
dir.create(dirname(out),recursive=TRUE,showWarnings=FALSE)
fwrite(classifications,out,na="")

# The two legacy feature-rich country selections are regression references, not
# the forward rule. Recompute their issue medians from original vendor fields;
# only the historical inclusion decisions are curated inputs.
f <- new.env(parent=globalenv())
f$last_updated <- "2026-06-19"
feature_script <- "experiments/full_ladder_ratings_integrated_status_rebuild_2026-06-01/scripts/build_p12a_feature_rich_secondary_final_decision_2024.R"
p15_legacy_assignments(feature_script,c("has_text","collapse_unique","median_or_na","max_or_na"),f,TRUE)
permissions <- read_csv("data-raw/p15_curated_inputs_20260906/legacy_feature_row_permissions.csv",show_col_types=FALSE)
vendor <- read_csv("sources/market_rates/lseg_workspace_download_v12_partial_2026-05-11/extracted/pvr_lseg_debug/20260511_175902/lseg_pvr_benchmark_cashflow_terms_desktop_oecd_base_2000-01-01_2025-12-31.csv",show_col_types=FALSE,name_repair="unique")
v <- vendor |> transmute(terminal_identifier=Instrument,v12_pricing_mid_yield=as.numeric(`Pricing Mid Yield`),
  v12_bid_yield=as.numeric(`Bid Yield`),v12_ask_yield=as.numeric(`Ask Yield`)) |>
  filter(terminal_identifier %in% permissions$terminal_identifier)
stopifnot(!anyDuplicated(v$terminal_identifier))
f$row_decisions <- permissions |> left_join(v,by="terminal_identifier") |>
  mutate(central_rate_pct=if_else(startsWith(final_decision,"accepted"),v12_pricing_mid_yield,NA_real_),
    lower_yield_bound_pct=if_else(startsWith(final_decision,"accepted"),pmin(v12_bid_yield,v12_ask_yield,na.rm=TRUE),NA_real_),
    upper_yield_bound_pct=if_else(startsWith(final_decision,"accepted"),pmax(v12_bid_yield,v12_ask_yield,na.rm=TRUE),NA_real_))
stopifnot(all(is.finite(f$row_decisions$central_rate_pct[startsWith(f$row_decisions$final_decision,"accepted")])))
p15_legacy_assignments(feature_script,"issue_layer",f)
out <- "experiments/full_ladder_ratings_integrated_status_rebuild_2026-06-01/outputs/p12a_feature_rich_secondary_accepted_issue_layer_2024.csv"
dir.create(dirname(out),recursive=TRUE,showWarnings=FALSE)
fwrite(f$issue_layer,out,na="")
cat("Rebuilt IDS Bondholders, annual Damodaran spreads and historical classifications from cached sources.\n")
