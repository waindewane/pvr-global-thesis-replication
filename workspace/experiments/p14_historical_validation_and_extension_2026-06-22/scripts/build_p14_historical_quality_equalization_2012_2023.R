#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(countrycode)
  library(dplyr)
  library(ggplot2)
  library(jsonlite)
  library(readxl)
  library(scales)
  library(tibble)
  library(tidyr)
})

root_dir <- normalizePath(".", mustWork = TRUE)
experiment_dir <- file.path(root_dir, "experiments/p14_historical_validation_and_extension_2026-06-22")
outputs_dir <- file.path(experiment_dir, "outputs")
reports_dir <- file.path(experiment_dir, "reports")
run_outputs_dir <- file.path(outputs_dir, "p14_historical_quality_equalization_2012_2023")
fig_dir <- file.path(run_outputs_dir, "figures")
if (dir.exists(run_outputs_dir)) {
  unlink(list.files(run_outputs_dir, full.names = TRUE), recursive = TRUE, force = TRUE)
}
dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)

build_id <- "p14_historical_quality_equalization_2012_2023_2026-06-30"
generated_at <- format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")
benchmark_years <- 2012:2023
classification_years <- 2012:2024

path_out <- function(file) file.path(run_outputs_dir, file)
write_out <- function(x, file) {
  path <- path_out(file)
  write.csv(x, path, row.names = FALSE, na = "")
  invisible(path)
}
write_text <- function(lines, file) {
  path <- path_out(file)
  writeLines(lines, path, useBytes = TRUE)
  invisible(path)
}
read_csv <- function(path, nrows = -1) {
  read.csv(path, check.names = FALSE, stringsAsFactors = FALSE, comment.char = "", nrows = nrows)
}
required_file <- function(path) {
  if (!file.exists(path)) stop("Missing required file: ", path, call. = FALSE)
  path
}

clean_text <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- ""
  gsub("\\s+", " ", trimws(x))
}
parse_num <- function(x) {
  x_chr <- clean_text(x)
  x_chr[x_chr %in% c("", "NA", "NaN", "NULL", "null")] <- NA_character_
  suppressWarnings(as.numeric(gsub(",", "", gsub("%", "", x_chr, fixed = TRUE), fixed = TRUE)))
}
as_bool <- function(x) {
  if (is.logical(x)) return(replace(x, is.na(x), FALSE))
  tolower(clean_text(x)) %in% c("true", "t", "yes", "y", "1")
}
col_or <- function(df, col, default = NA) {
  if (col %in% names(df)) df[[col]] else rep(default, nrow(df))
}
first_nonempty <- function(x) {
  x <- clean_text(x)
  x <- x[x != ""]
  if (!length(x)) NA_character_ else x[[1]]
}
normalize_name_key <- function(x) {
  x <- iconv(clean_text(x), from = "", to = "ASCII//TRANSLIT")
  x <- tolower(x)
  x <- gsub("&", " and ", x)
  x <- gsub("[^a-z0-9]+", " ", x)
  gsub("\\s+", " ", trimws(x))
}
collapse_unique <- function(x, max_items = Inf) {
  x <- unique(clean_text(x))
  x <- x[x != ""]
  if (!length(x)) return(NA_character_)
  if (is.finite(max_items) && length(x) > max_items) {
    return(paste(c(x[seq_len(max_items)], paste0("...", length(x) - max_items, " more")), collapse = ";"))
  }
  paste(x, collapse = ";")
}
safe_divide <- function(num, den) ifelse(is.na(den) | den == 0, NA_real_, num / den)
rate_sanity <- function(rate_pct) {
  rate_pct <- parse_num(rate_pct)
  case_when(
    is.na(rate_pct) ~ "missing_rate",
    rate_pct <= 0 ~ "nonpositive_rate",
    rate_pct < 1 ~ "low_positive_below_1pct",
    rate_pct > 30 ~ "above_30pct_review",
    TRUE ~ "sane_1_30pct"
  )
}
weighted_mean_or_mean <- function(x, w) {
  x <- parse_num(x)
  w <- parse_num(w)
  ok <- !is.na(x)
  if (!any(ok)) return(NA_real_)
  x <- x[ok]
  w <- w[ok]
  if (!any(!is.na(w) & w > 0)) return(mean(x, na.rm = TRUE))
  w[is.na(w) | w <= 0] <- 0
  weighted.mean(x, w, na.rm = TRUE)
}
period_for_year <- function(year) {
  year <- parse_num(year)
  case_when(
    year >= 2012 & year <= 2015 ~ "2012-2015",
    year >= 2016 & year <= 2019 ~ "2016-2019",
    year >= 2020 & year <= 2023 ~ "2020-2023",
    year == 2024 ~ "2024",
    TRUE ~ NA_character_
  )
}

econ_cols <- c(
  primary = "#2F6B9A",
  secondary = "#527A61",
  ids = "#8A6F3D",
  rating = "#7E6B9E",
  peer = "#9A6A5A",
  muted = "#A7A9AC",
  dark = "#333333",
  warning = "#B45A4A",
  accent = "#D9A441"
)
theme_econ_clean <- function(base_size = 11) {
  theme_minimal(base_size = base_size) +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_line(color = "grey88", linewidth = 0.3),
      axis.title = element_text(color = "grey25"),
      axis.text = element_text(color = "grey25"),
      plot.title = element_text(face = "bold", margin = margin(b = 6)),
      plot.subtitle = element_text(color = "grey30", margin = margin(b = 8)),
      plot.caption = element_text(color = "grey45", hjust = 0, size = rel(0.8), margin = margin(t = 8)),
      legend.position = "top",
      legend.title = element_blank(),
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA),
      plot.margin = margin(8, 12, 8, 8)
    )
}
save_fig <- function(plot, file, width = 9.5, height = 5.8) {
  ggsave(file.path(fig_dir, file), plot, width = width, height = height, dpi = 240, bg = "white")
  invisible(file.path(fig_dir, file))
}

bridge_dir <- file.path(outputs_dir, "p14_provisional_bridge_2012_2024")
partial_dir <- file.path(outputs_dir, "p14_partial_resume_historical_ladder_2012_2021")
repair_dir <- file.path(outputs_dir, "p14_historical_secondary_repair_audit_2012_2023")
primary_dir <- file.path(outputs_dir, "primary_partial_trial_20260623_184707")
p13_dir <- file.path(root_dir, "experiments/full_ladder_ratings_integrated_status_rebuild_2026-06-01/outputs")
wb_oghist_path <- required_file(file.path(root_dir, "data-raw/world_bank_country_classifications/OGHIST_2026-06-30.xls"))
owid_income_groups_path <- required_file(file.path(root_dir, "data-raw/world_bank_country_classifications/owid_world_bank_income_groups_2026-06-30.csv"))
owid_income_groups_metadata_path <- required_file(file.path(root_dir, "data-raw/world_bank_country_classifications/owid_world_bank_income_groups_metadata_2026-06-30.json"))
boc_boe_default_json_path <- required_file(file.path(root_dir, "data-raw/status_context_sources/boc_boe_sovereign_default_database_DEBT_2025_2026-06-30.json"))
boc_boe_default_csv_path <- required_file(file.path(root_dir, "data-raw/status_context_sources/boc_boe_sovereign_default_database_DEBT_2025_2026-06-30.csv"))
paris_club_signed_agreements_csv_path <- required_file(file.path(root_dir, "data-raw/status_context_sources/paris_club_signed_agreements_2026-06-30/paris_club_signed_agreements_advanced_search_2026-06-30.csv"))
paris_club_signed_agreements_metadata_path <- required_file(file.path(root_dir, "data-raw/status_context_sources/paris_club_signed_agreements_2026-06-30/paris_club_signed_agreements_advanced_search_metadata_2026-06-30.json"))

bridge_long_path <- required_file(file.path(bridge_dir, "p14_provisional_bridge_rate_values_long_2012_2024.csv"))
bridge_panel_path <- required_file(file.path(bridge_dir, "p14_provisional_bridge_country_year_panel_2012_2024.csv"))
bridge_observed_path <- required_file(file.path(bridge_dir, "p14_provisional_bridge_best_available_benchmark_observed_strength_2012_2024.csv"))
bridge_ids_path <- required_file(file.path(bridge_dir, "p14_provisional_bridge_best_available_benchmark_ids_first_public_cascade_2012_2024.csv"))
secondary_triage_path <- required_file(file.path(repair_dir, "p14_secondary_repair_triage_identifier_year_2012_2023.csv"))
secondary_country_triage_path <- required_file(file.path(repair_dir, "p14_secondary_repair_triage_country_year_2012_2023.csv"))
secondary_source_scope_path <- required_file(file.path(repair_dir, "p14_secondary_repair_source_scope_by_year_2012_2023.csv"))
secondary_trial_country_path <- required_file(file.path(repair_dir, "p14_secondary_price_to_yield_trial_country_year_rates_2012_2023.csv"))
secondary_trial_issue_path <- required_file(file.path(repair_dir, "p14_secondary_price_to_yield_trial_issue_results_2012_2023.csv"))
primary_country_path <- required_file(file.path(primary_dir, "primary_partial_trial_country_year_2012_2024.csv"))
primary_issue_path <- required_file(file.path(primary_dir, "primary_partial_trial_issue_level_2012_2024.csv"))

bridge_long <- read_csv(bridge_long_path) |>
  filter(analysis_year %in% benchmark_years) |>
  mutate(
    period = period_for_year(analysis_year),
    market_rate_pct_numeric = parse_num(market_rate_pct_numeric),
    market_maturity_years_numeric = parse_num(market_maturity_years_numeric),
    bridge_in_permissible_market_rate_ladder = as_bool(bridge_in_permissible_market_rate_ladder),
    bridge_selection_candidate = as_bool(bridge_selection_candidate),
    bridge_comparison_secondary_exclusion = as_bool(bridge_comparison_secondary_exclusion),
    rate_value_present = as_bool(rate_value_present),
    source_fields_traceable = as_bool(source_fields_traceable)
  )

bridge_panel_all <- read_csv(bridge_panel_path) |>
  filter(analysis_year %in% classification_years) |>
  mutate(period = period_for_year(analysis_year))

bridge_panel <- bridge_panel_all |>
  filter(analysis_year %in% benchmark_years)

bridge_observed <- read_csv(bridge_observed_path) |>
  filter(analysis_year %in% benchmark_years) |>
  mutate(has_selected_rate = as_bool(has_selected_rate), period = period_for_year(analysis_year))

bridge_ids_first <- read_csv(bridge_ids_path) |>
  filter(analysis_year %in% benchmark_years) |>
  mutate(has_selected_rate = as_bool(has_selected_rate), period = period_for_year(analysis_year))

secondary_triage <- read_csv(secondary_triage_path) |>
  mutate(
    period = period_for_year(analysis_year),
    secondary_yield_pct = parse_num(secondary_yield_pct),
    remaining_maturity_years = parse_num(remaining_maturity_years),
    amount_weight_usd = parse_num(amount_weight_usd),
    raw_direct_ytm_present = as_bool(raw_direct_ytm_present),
    raw_price_present = as_bool(raw_price_present),
    price_without_direct_ytm = as_bool(price_without_direct_ytm),
    hard_currency_usd_eur = as_bool(hard_currency_usd_eur),
    residual_ge1 = as_bool(residual_ge1),
    residual_2_15 = as_bool(residual_2_15),
    status_ok_for_repair = as_bool(status_ok_for_repair),
    plain_vanilla_ok_for_repair = as_bool(plain_vanilla_ok_for_repair),
    static_repair_inputs_present = as_bool(static_repair_inputs_present),
    clean_price_repair_eligible_ge1 = as_bool(clean_price_repair_eligible_ge1),
    clean_price_repair_eligible_2_15 = as_bool(clean_price_repair_eligible_2_15),
    asset_status_blocks_standard = as_bool(asset_status_blocks_standard),
    standard_secondary_eligible = as_bool(standard_secondary_eligible),
    nonstandard_feature_flag = as_bool(nonstandard_feature_flag),
    broad_secondary_nonstandard_included_eligible = as_bool(broad_secondary_nonstandard_included_eligible)
  )

secondary_country_triage <- read_csv(secondary_country_triage_path) |>
  mutate(
    period = period_for_year(analysis_year),
    across(
      c(
        returned_identifier_count, direct_ytm_identifier_count,
        standard_direct_ytm_identifier_count, price_identifier_count,
        price_without_direct_ytm_identifier_count,
        price_without_ytm_quote_date_recovered_count,
        repair_candidate_ge1_identifier_count, repair_candidate_2_15_identifier_count,
        trial_repaired_country_year_rate_available, repaired_issue_count, repaired_identifier_count
      ),
      parse_num
    ),
    has_trial_repaired_country_year_rate = as_bool(has_trial_repaired_country_year_rate)
  )

secondary_source_scope <- read_csv(secondary_source_scope_path) |>
  mutate(
    period = period_for_year(analysis_year),
    across(c(analysis_year, universe_identifier_rows, universe_countries, raw_chunk_count, parsed_chunk_count), parse_num)
  )

secondary_trial_country <- read_csv(secondary_trial_country_path) |>
  mutate(
    period = period_for_year(analysis_year),
    repaired_secondary_trial_rate_pct = parse_num(repaired_secondary_trial_rate_pct),
    repaired_secondary_trial_maturity_years = parse_num(repaired_secondary_trial_maturity_years),
    repaired_issue_count = parse_num(repaired_issue_count),
    repaired_identifier_count = parse_num(repaired_identifier_count),
    total_weight_usd = parse_num(total_weight_usd),
    residual_2_15_all = as_bool(residual_2_15_all),
    max_identifier_dispersion_bps = parse_num(max_identifier_dispersion_bps),
    max_settlement_sensitivity_bps = parse_num(max_settlement_sensitivity_bps),
    max_bid_ask_yield_range_bps = parse_num(max_bid_ask_yield_range_bps),
    accepted_as_methodology = as_bool(accepted_as_methodology),
    diagnostic_only = as_bool(diagnostic_only)
  )

secondary_trial_issue <- read_csv(secondary_trial_issue_path) |>
  mutate(
    period = period_for_year(analysis_year),
    identifier_count = parse_num(identifier_count),
    identifiers_passing_trial_thresholds = parse_num(identifiers_passing_trial_thresholds),
    remaining_maturity_years = parse_num(remaining_maturity_years),
    trial_repaired_yield_mid_pct = parse_num(trial_repaired_yield_mid_pct),
    identifier_dispersion_bps = parse_num(identifier_dispersion_bps),
    max_settlement_sensitivity_bps = parse_num(max_settlement_sensitivity_bps),
    max_bid_ask_yield_range_bps = parse_num(max_bid_ask_yield_range_bps),
    residual_2_15 = as_bool(residual_2_15),
    accepted_as_methodology = as_bool(accepted_as_methodology),
    diagnostic_only = as_bool(diagnostic_only)
  )

primary_country <- read_csv(primary_country_path) |>
  filter(issue_year >= 2012, issue_year <= 2023) |>
  mutate(
    analysis_year = issue_year,
    iso3 = canonical_iso3,
    country = canonical_country,
    period = period_for_year(analysis_year),
    primary_rate_pct = parse_num(amount_weighted_original_yield_pct),
    primary_maturity_years = parse_num(amount_weighted_original_maturity_years),
    issue_rows = parse_num(issue_rows),
    distinct_isins = parse_num(distinct_isins),
    rows_with_amount_weight = parse_num(rows_with_amount_weight),
    total_weight_amount = parse_num(total_weight_amount),
    included_in_lmic_reporting_scope = as_bool(included_in_lmic_reporting_scope),
    primary_rate_sanity_state = rate_sanity(primary_rate_pct)
  )

primary_issue <- read_csv(primary_issue_path) |>
  filter(issue_year >= 2012, issue_year <= 2023) |>
  mutate(
    analysis_year = issue_year,
    iso3 = canonical_iso3,
    country = canonical_country,
    period = period_for_year(analysis_year),
    original_yield_maturity_pct = parse_num(original_yield_maturity_pct),
    original_maturity_years = parse_num(original_maturity_years),
    weight_amount = parse_num(weight_amount),
    hard_currency_usd_eur_flag = as_bool(hard_currency_usd_eur_flag),
    has_original_yield_maturity = as_bool(has_original_yield_maturity),
    has_amount_weight = as_bool(has_amount_weight),
    usable_for_primary_yield_trial = as_bool(usable_for_primary_yield_trial),
    issue_date_in_2012_2024 = as_bool(issue_date_in_2012_2024),
    primary_rate_sanity_state = rate_sanity(original_yield_maturity_pct)
  )

p14_status_existing_path <- file.path(outputs_dir, "p14_country_year_status_context_ledger_2015_2024.csv")
p14_status_existing <- if (file.exists(p14_status_existing_path)) {
  read_csv(p14_status_existing_path) |>
    filter(analysis_year >= 2015, analysis_year <= 2023) |>
    select(analysis_year, iso3, status_context_state_existing = status_context_state,
           status_context_source_state_existing = status_context_source_state,
           affects_quantitative_admissibility_existing = affects_quantitative_admissibility,
           affects_display_or_interpretation_existing = affects_display_or_interpretation)
} else {
  tibble()
}

country_universe_2012_2024 <- bridge_panel_all |>
  distinct(
    analysis_year, period, iso3, country, income_level, lending_type,
    included_in_lmic_reporting_scope, p14_scope_class, p14_core_year_flag,
    classification_basis, historical_classification_audit_state,
    bridge_year_source_layer, bridge_secondary_coverage_state
  )

country_universe <- country_universe_2012_2024 |>
  filter(analysis_year %in% benchmark_years)

income_code_to_label <- function(x) {
  case_when(
    clean_text(x) == "L" ~ "Low income",
    clean_text(x) == "LM" ~ "Lower middle income",
    clean_text(x) == "UM" ~ "Upper middle income",
    clean_text(x) == "H" ~ "High income",
    TRUE ~ NA_character_
  )
}
income_long_to_label <- function(x) {
  x_clean <- tolower(clean_text(x))
  case_when(
    grepl("lower-middle", x_clean) ~ "Lower middle income",
    grepl("upper-middle", x_clean) ~ "Upper middle income",
    grepl("low-income", x_clean) ~ "Low income",
    grepl("high-income", x_clean) ~ "High income",
    grepl("not classified|unclassified", x_clean) ~ "Not classified",
    TRUE ~ NA_character_
  )
}

wb_income_raw <- as.data.frame(
  readxl::read_excel(wb_oghist_path, sheet = "Country Analytical History", col_names = FALSE),
  stringsAsFactors = FALSE
)
wb_income_years <- suppressWarnings(as.integer(unlist(wb_income_raw[6, ], use.names = FALSE)))
wb_income_cols <- which(wb_income_years %in% classification_years)
wb_country_rows <- which(
  seq_len(nrow(wb_income_raw)) > 10 &
    clean_text(wb_income_raw[[1]]) != "" &
    clean_text(wb_income_raw[[2]]) != ""
)
wb_income_history <- bind_rows(lapply(wb_income_cols, function(col_idx) {
  tibble(
    iso3 = clean_text(wb_income_raw[[1]][wb_country_rows]),
    country_wb_income_source = clean_text(wb_income_raw[[2]][wb_country_rows]),
    analysis_year = wb_income_years[[col_idx]],
    wb_income_code = clean_text(wb_income_raw[[col_idx]][wb_country_rows]),
    wb_income_level = income_code_to_label(wb_income_code)
  )
})) |>
  filter(analysis_year %in% classification_years, clean_text(iso3) != "") |>
  mutate(
    wb_income_source_pointer = "World Bank OGHIST.xls, Country Analytical History sheet",
    wb_income_source_file = wb_oghist_path
  )

owid_income_history <- read_csv(owid_income_groups_path) |>
  transmute(
    iso3 = case_when(
      Code == "OWID_KOS" ~ "XKX",
      TRUE ~ Code
    ),
    country_owid_income_source = Entity,
    analysis_year = parse_num(Year),
    owid_income_raw = `World Bank's income classification`,
    owid_income_level = income_long_to_label(owid_income_raw),
    owid_income_source_pointer = "Our World in Data grapher world-bank-income-groups; source: World Bank Income Classifications",
    owid_income_source_file = owid_income_groups_path,
    owid_income_metadata_file = owid_income_groups_metadata_path
  ) |>
  filter(analysis_year %in% classification_years)

historical_income_sources <- full_join(
  wb_income_history,
  owid_income_history,
  by = c("iso3", "analysis_year")
) |>
  mutate(
    historical_income_level_source_backed = case_when(
      !is.na(wb_income_level) ~ wb_income_level,
      !is.na(owid_income_level) ~ owid_income_level,
      TRUE ~ NA_character_
    ),
    historical_income_source_priority = case_when(
      !is.na(wb_income_level) ~ "world_bank_oghist_primary",
      !is.na(owid_income_level) ~ "owid_structured_world_bank_fill",
      TRUE ~ "no_income_source_match"
    ),
    historical_income_source_pointer = case_when(
      !is.na(wb_income_level) ~ wb_income_source_pointer,
      !is.na(owid_income_level) ~ owid_income_source_pointer,
      TRUE ~ NA_character_
    ),
    source_income_disagreement_flag = !is.na(wb_income_level) & !is.na(owid_income_level) & wb_income_level != owid_income_level
  ) |>
  select(
    iso3, analysis_year, country_wb_income_source, country_owid_income_source,
    historical_income_level_source_backed, historical_income_source_priority,
    historical_income_source_pointer, source_income_disagreement_flag,
    wb_income_code, owid_income_raw
  )

boc_extract <- function(obs, key) {
  entry <- obs[[key]]
  if (is.null(entry)) return(NA_character_)
  value <- entry[["v"]]
  if (is.null(value)) NA_character_ else as.character(value)
}
boc_extract_num <- function(obs, key) parse_num(boc_extract(obs, key))
boc_custom_match <- c(
  "Bolivia" = "BOL",
  "Bosnia & Herzegovina" = "BIH",
  "Côte d’Ivoire" = "CIV",
  "Democratic Republic of Congo (Kinshasa)" = "COD",
  "eSwatini" = "SWZ",
  "eSwatini (Swaziland)" = "SWZ",
  "Egypt" = "EGY",
  "Gambia" = "GMB",
  "Iran" = "IRN",
  "Korea (North)" = "PRK",
  "Kyrgyz Republic" = "KGZ",
  "Kyrgyzstan" = "KGZ",
  "Laos" = "LAO",
  "Macedonia" = "MKD",
  "Moldova" = "MDA",
  "Republic of Congo (Brazzaville)" = "COG",
  "São Tomé and Príncipe" = "STP",
  "St. Kitts & Nevis" = "KNA",
  "Syria" = "SYR",
  "USSR/Russia" = "RUS",
  "Venezuela" = "VEN",
  "Vietnam" = "VNM"
)
boc_boe_default_raw <- jsonlite::fromJSON(boc_boe_default_json_path, simplifyVector = FALSE)
boc_default_country_year_raw <- bind_rows(lapply(boc_boe_default_raw$observations, function(obs) {
  tibble(
    boc_country = boc_extract(obs, "DEBT_COUNTRY"),
    boc_country_group = boc_extract(obs, "DEBT_COUNTRY_GROUP"),
    analysis_year = boc_extract_num(obs, "DEBT_YEAR"),
    boc_total_default_debt_usd_mn = boc_extract_num(obs, "DEBT_TOTAL_2025"),
    boc_private_creditors_default_debt_usd_mn = boc_extract_num(obs, "DEBT_PRIVATE_CREDITORS_2025"),
    boc_fc_bank_loans_default_debt_usd_mn = boc_extract_num(obs, "DEBT_FC_BANK_LOANS_2025"),
    boc_fc_bonds_default_debt_usd_mn = boc_extract_num(obs, "DEBT_FC_BONDS_2025"),
    boc_local_currency_debt_default_usd_mn = boc_extract_num(obs, "DEBT_LC_DEBT_2025"),
    boc_fiscal_arrears_usd_mn = boc_extract_num(obs, "DEBT_FISCAL_ARREARS_2025"),
    boc_paris_club_default_debt_usd_mn = boc_extract_num(obs, "DEBT_PARIS_CLUB_2025"),
    boc_china_default_debt_usd_mn = boc_extract_num(obs, "DEBT_CHINA_2025"),
    boc_imf_default_debt_usd_mn = boc_extract_num(obs, "DEBT_IMF_2025"),
    boc_total_defaulted_sovereign_count = boc_extract_num(obs, "DEBT_TOTAL_DEF_SOVEREIGNS_2025"),
    boc_fc_bonds_defaulted_sovereign_count = boc_extract_num(obs, "DEBT_FC_BONDS_DEF_SOVEREIGNS_2025"),
    boc_private_creditors_defaulted_sovereign_count = boc_extract_num(obs, "DEBT_PRIVATE_CREDITORS_DEF_SOVEREIGNS_2025")
  )
})) |>
  filter(analysis_year >= 2012, analysis_year <= 2023, clean_text(boc_country) != "", boc_country != "World") |>
  mutate(
    iso3 = countrycode::countrycode(boc_country, "country.name", "iso3c", custom_match = boc_custom_match, warn = FALSE)
  )

boc_default_country_year <- boc_default_country_year_raw |>
  filter(!is.na(iso3)) |>
  group_by(analysis_year, iso3) |>
  summarise(
    boc_country_names = collapse_unique(boc_country),
    boc_country_groups = collapse_unique(boc_country_group),
    across(starts_with("boc_") & where(is.numeric), ~ sum(.x, na.rm = TRUE)),
    .groups = "drop"
  ) |>
  mutate(
    boc_default_context_flag = boc_total_default_debt_usd_mn > 0,
    boc_private_or_bond_default_context_flag = boc_private_creditors_default_debt_usd_mn > 0 | boc_fc_bonds_default_debt_usd_mn > 0,
    boc_official_default_context_flag = boc_paris_club_default_debt_usd_mn > 0 |
      boc_china_default_debt_usd_mn > 0 | boc_imf_default_debt_usd_mn > 0,
    boc_default_context_strength = case_when(
      boc_private_or_bond_default_context_flag ~ "source_backed_private_or_bond_default_context",
      boc_official_default_context_flag | boc_total_default_debt_usd_mn > 0 ~ "source_backed_official_or_residual_default_context",
      TRUE ~ "no_boc_boe_default_stock_recorded"
    ),
    boc_default_source_pointer = "Bank of Canada-Bank of England Sovereign Default Database 2025, Valet group DEBT_2025",
    boc_default_source_file = boc_boe_default_csv_path
  )

write_out(
  boc_default_country_year,
  "p14_historical_default_context_source_ledger_2012_2023.csv"
)

paris_club_custom_match <- c(
  "BOLIVIA" = "BOL",
  "BOSNIA AND HERZEGOVINA" = "BIH",
  "CONGO" = "COG",
  "COTE D IVOIRE" = "CIV",
  "COTE D'IVOIRE" = "CIV",
  "DEMOCRATIC REPUBLIC OF CONGO" = "COD",
  "DRC" = "COD",
  "FIJI" = "FJI",
  "KIRGHIZIE" = "KGZ",
  "KYRGYZ REPUBLIC" = "KGZ",
  "LAOS" = "LAO",
  "MACEDONIA" = "MKD",
  "MOLDOVA" = "MDA",
  "MYANMAR" = "MMR",
  "REPUBLIC OF CONGO" = "COG",
  "SOMALIA<" = "SOM",
  "TCHAD" = "TCD",
  "TURKIYE" = "TUR",
  "TÜRKIYE" = "TUR",
  "VIETNAM" = "VNM",
  "YEMEN" = "YEM"
)
paris_club_signed_agreements_raw <- read_csv(paris_club_signed_agreements_csv_path) |>
  mutate(
    agreement_year = parse_num(agreement_year),
    agreement_date = as.Date(agreement_date),
    debtor_country_clean = clean_text(debtor_country),
    debtor_country_match_key = toupper(iconv(debtor_country_clean, from = "", to = "ASCII//TRANSLIT")),
    debtor_country_match_key = gsub("[^A-Z0-9]+", " ", debtor_country_match_key),
    debtor_country_match_key = gsub("\\s+", " ", trimws(debtor_country_match_key)),
    iso3 = countrycode::countrycode(debtor_country_clean, "country.name", "iso3c", custom_match = paris_club_custom_match, warn = FALSE),
    iso3 = ifelse(is.na(iso3), paris_club_custom_match[debtor_country_match_key], iso3),
    treatment_type_normalized = clean_text(treatment_type_normalized),
    treatment_status = clean_text(treatment_status),
    detail_url = clean_text(detail_url)
  )

paris_club_country_year <- paris_club_signed_agreements_raw |>
  filter(agreement_year >= 2012, agreement_year <= 2023, !is.na(iso3)) |>
  group_by(analysis_year = agreement_year, iso3) |>
  summarise(
    paris_club_agreement_count = n(),
    paris_club_agreement_dates = collapse_unique(as.character(agreement_date)),
    paris_club_treatment_types = collapse_unique(treatment_type_normalized),
    paris_club_treatment_statuses = collapse_unique(treatment_status),
    paris_club_debtor_country_names = collapse_unique(debtor_country_clean),
    paris_club_detail_urls = collapse_unique(detail_url, max_items = 5),
    paris_club_has_dssi = any(grepl("DSSI|ISSD", treatment_type_normalized, ignore.case = TRUE)),
    paris_club_has_common_framework = any(grepl("Common Framework", treatment_type_normalized, ignore.case = TRUE)),
    paris_club_has_non_dssi_treatment = any(!grepl("DSSI|ISSD", treatment_type_normalized, ignore.case = TRUE)),
    .groups = "drop"
  ) |>
  mutate(
    paris_club_same_year_agreement_flag = paris_club_agreement_count > 0,
    paris_club_official_treatment_context_strength = case_when(
      paris_club_has_common_framework ~ "source_backed_common_framework_official_treatment",
      paris_club_has_non_dssi_treatment ~ "source_backed_non_dssi_paris_club_official_treatment",
      paris_club_has_dssi ~ "source_backed_dssi_official_suspension_context",
      TRUE ~ "no_paris_club_agreement_recorded"
    ),
    paris_club_quantitative_review_flag = paris_club_has_non_dssi_treatment,
    paris_club_source_pointer = "Paris Club signed agreements advanced search; locally preserved public HTML extract",
    paris_club_source_file = paris_club_signed_agreements_csv_path
  )
write_out(paris_club_country_year, "p14_historical_paris_club_signed_agreements_2012_2023.csv")

legacy_bridge_secondary_scope <- bridge_long |>
  filter(
    analysis_year %in% 2022:2023,
    major_ladder_tier_id == "observed_secondary_market_evidence",
    bridge_secondary_scope_state == "existing_lmic_secondary_layer_available_2022_2024"
  ) |>
  group_by(analysis_year) |>
  summarise(
    period = first(period_for_year(analysis_year)),
    universe_countries = n_distinct(iso3),
    .groups = "drop"
  )

source_scope_register <- bind_rows(
  secondary_source_scope |>
    transmute(
      analysis_year, period,
      source_family = "observed_secondary_market_evidence",
      source_id,
      source_scope_label,
      source_scope_state = "complete_all_country_secondary_from_partial_resume_export",
      universe_identifier_rows,
      universe_countries,
      raw_chunk_count,
      parsed_chunk_count,
      source_scope_caveat = "Broader all-country partial-resume source is complete for this year."
    ),
  legacy_bridge_secondary_scope |>
    transmute(
      analysis_year, period,
      source_family = "legacy_observed_secondary_bridge_reference",
      source_id = "existing_lmic_secondary_layer_2022_2024",
      source_scope_label = "legacy_lmic_secondary_bridge_layer_2022_2024",
      source_scope_state = "legacy_bridge_retained_for_audit_and_fallback",
      universe_identifier_rows = NA_real_,
      universe_countries,
      raw_chunk_count = NA_real_,
      parsed_chunk_count = NA_real_,
      source_scope_caveat = "Legacy LMIC secondary rows remain visible and permissible. They only win selected-rate ties when the equalized all-country source has no comparable standard direct secondary row or only weaker broader evidence."
    ),
  tibble(
    analysis_year = 2012:2023,
    period = period_for_year(2012:2023),
    source_family = "observed_primary_issuance",
    source_id = "partial_primary_trial_20260623_184707",
    source_scope_label = "desktop_primary_issue_terms_partial_backup_complete_static_groups",
    source_scope_state = "primary_issue_terms_available_from_partial_backup_pending_final_export_rebuild",
    universe_identifier_rows = NA_real_,
    universe_countries = NA_real_,
    raw_chunk_count = NA_real_,
    parsed_chunk_count = NA_real_,
    source_scope_caveat = "Primary issue-term layer is usable for diagnostics and equalization, but should be rebuilt after final LSEG return."
  ),
  tibble(
    analysis_year = 2012:2023,
    period = period_for_year(2012:2023),
    source_family = "bridge_non_secondary_tiers",
    source_id = "p14_provisional_bridge_2012_2024",
    source_scope_label = "derived_bridge_from_partial_resume_and_existing_p14_layers",
    source_scope_state = "derived_bridge_input_not_final_methodology",
    universe_identifier_rows = NA_real_,
    universe_countries = 211,
    raw_chunk_count = NA_real_,
    parsed_chunk_count = NA_real_,
    source_scope_caveat = "Bridge rows are retained as starting evidence; equalization layer records unresolved method decisions but classification and default-context source fields are rebuilt below."
  ),
  tibble(
    analysis_year = 2012:2023,
    period = period_for_year(2012:2023),
    source_family = "country_income_classification",
    source_id = "world_bank_oghist_plus_owid_structured_fill",
    source_scope_label = "historical_world_bank_income_group_by_iso3_year",
    source_scope_state = "source_backed_income_classification_available",
    universe_identifier_rows = NA_real_,
    universe_countries = n_distinct(historical_income_sources$iso3[historical_income_sources$analysis_year %in% 2012:2023]),
    raw_chunk_count = NA_real_,
    parsed_chunk_count = NA_real_,
    source_scope_caveat = "World Bank OGHIST is the primary source through its available historical columns; OWID's World-Bank-sourced grapher fills later 2022-2023 income classification rows."
  ),
  tibble(
    analysis_year = 2012:2023,
    period = period_for_year(2012:2023),
    source_family = "default_context_status",
    source_id = "bank_of_canada_bank_of_england_sovereign_default_database_2025",
    source_scope_label = "country_year_default_stock_by_creditor_component",
    source_scope_state = "source_backed_default_context_available",
    universe_identifier_rows = NA_real_,
    universe_countries = n_distinct(boc_default_country_year$iso3),
    raw_chunk_count = NA_real_,
    parsed_chunk_count = NA_real_,
    source_scope_caveat = "Default-stock evidence is used as hard-context source material and warning/gating evidence, not as a complete market-access or war/sanctions ledger."
  ),
  tibble(
    analysis_year = 2012:2023,
    period = period_for_year(2012:2023),
    source_family = "official_debt_treatment_status",
    source_id = "paris_club_signed_agreements_advanced_search",
    source_scope_label = "public_paris_club_signed_agreement_by_country_year",
    source_scope_state = "source_backed_official_treatment_agreement_available",
    universe_identifier_rows = NA_real_,
    universe_countries = n_distinct(paris_club_country_year$iso3),
    raw_chunk_count = NA_real_,
    parsed_chunk_count = nrow(paris_club_country_year),
    source_scope_caveat = "Paris Club signed-agreement rows identify official treatment events, not all war, sanctions, arrears, private restructuring, or market-access contexts."
  )
) |>
  arrange(analysis_year, source_family)
write_out(source_scope_register, "p14_historical_source_scope_register_2012_2023.csv")

input_validation_summary <- tibble(
  check_name = c(
    "country_year_universe_rows_211_by_12",
    "bridge_rate_values_available",
    "secondary_repair_triage_rows_available",
    "primary_partial_issue_rows_available",
    "source_scope_years_2012_2023_present",
    "world_bank_income_classification_source_parsed",
    "owid_world_bank_income_fill_parsed",
    "boc_boe_default_context_source_parsed",
    "paris_club_signed_agreements_source_parsed"
  ),
  passed = c(
    nrow(country_universe) == 211 * 12,
    nrow(bridge_long) > 0,
    nrow(secondary_triage) > 0,
    nrow(primary_issue) > 0,
    setequal(sort(unique(source_scope_register$analysis_year)), 2012:2023),
    nrow(wb_income_history) > 0,
    nrow(owid_income_history) > 0,
    nrow(boc_default_country_year) > 0,
    nrow(paris_club_country_year) > 0
  ),
  detail = c(
    paste0("rows=", nrow(country_universe)),
    paste0("rows=", nrow(bridge_long)),
    paste0("rows=", nrow(secondary_triage)),
    paste0("rows=", nrow(primary_issue)),
    paste0("years=", paste(sort(unique(source_scope_register$analysis_year)), collapse = ",")),
    paste0("rows=", nrow(wb_income_history), "; years=", paste(range(wb_income_history$analysis_year, na.rm = TRUE), collapse = "-")),
    paste0("rows=", nrow(owid_income_history), "; years=", paste(range(owid_income_history$analysis_year, na.rm = TRUE), collapse = "-")),
    paste0("rows=", nrow(boc_default_country_year), "; countries=", n_distinct(boc_default_country_year$iso3)),
    paste0("rows=", nrow(paris_club_country_year), "; countries=", n_distinct(paris_club_country_year$iso3))
  )
)
write_out(input_validation_summary, "p14_historical_input_validation_summary_2012_2023.csv")

write_text(c(
  "# P14 Historical Source Scope Memo, 2012-2023",
  "",
  "This equalization package uses the P14 provisional bridge as the starting ladder surface and adds the new historical secondary repair-audit evidence.",
  "",
  "- 2012-2023 secondary history is taken from the broader partial-resume all-country LSEG source, using only years complete in that archive.",
  "- The preserved all-country archive also contains partial 2024 secondary-history chunks, but those are intentionally excluded here because 2024 remains the P13 anchor year.",
  "- Legacy 2022-2024 LMIC secondary bridge rows remain visible and permissible for audit continuity. In selected-rate output, the equalized all-country standard direct row is preferred where available; a legacy standard row can still win where the equalized all-country source has no standard direct country-year replacement or only broader/direct fallback evidence.",
  "- Primary issue-term evidence comes from the partial LSEG terminal backup and remains rebuildable after the final LSEG return.",
  "- Historical income classification is rebuilt by ISO3-year from World Bank OGHIST and OWID's structured World-Bank-sourced fill for the later tail years.",
  "- Historical default context is joined from the Bank of Canada-Bank of England Sovereign Default Database 2025. This source identifies default-stock context, but it is not a complete war, sanctions, arrears, or market-access ledger.",
  "- Paris Club signed agreements are parsed from the public advanced-search page and joined as official treatment context by agreement year.",
  "- This memo records source scope only. It does not approve historical repaired-secondary rates or any new paper-facing method.",
  "",
  paste0("Generated by `", build_id, "` at ", generated_at, ".")
), "p14_historical_source_scope_memo_2012_2023.md")

make_classification_ledger <- function(universe_df) {
  universe_df |>
    left_join(historical_income_sources, by = c("analysis_year", "iso3")) |>
    mutate(
      historical_income_level = coalesce(historical_income_level_source_backed, "Not classified"),
      historical_lending_type = lending_type,
      historical_lmic_reporting_scope = historical_income_level %in% c("Low income", "Lower middle income", "Upper middle income"),
      static_income_level = income_level,
      static_lending_type = lending_type,
      static_lmic_reporting_scope = included_in_lmic_reporting_scope,
      historical_p14_scope_class = case_when(
        historical_lmic_reporting_scope ~ "core_developing_scope",
        historical_income_level == "High income" ~ "comparison_high_income_scope",
        TRUE ~ "comparison_or_unclassified_scope"
      ),
      static_vs_historical_classification_conflict = case_when(
        is.na(historical_income_level_source_backed) ~ "no_world_bank_income_source_match",
        clean_text(static_income_level) != clean_text(historical_income_level) ~ "static_income_label_differs_from_historical_source",
        static_lmic_reporting_scope != historical_lmic_reporting_scope ~ "static_lmic_scope_differs_from_historical_source",
        TRUE ~ "static_and_historical_income_scope_match"
      ),
      classification_source_pointer = case_when(
        !is.na(historical_income_source_pointer) ~ historical_income_source_pointer,
        TRUE ~ "No matching ISO3-year row in World Bank OGHIST or OWID world-bank-income-groups source files"
      ),
      classification_confidence = case_when(
        historical_income_source_priority == "world_bank_oghist_primary" ~ "source_backed_world_bank_oghist",
        historical_income_source_priority == "owid_structured_world_bank_fill" ~ "source_backed_owid_world_bank_fill",
        TRUE ~ "source_missing_not_classified"
      ),
      classification_review_state = case_when(
        source_income_disagreement_flag ~ "source_disagreement_review",
        static_vs_historical_classification_conflict == "no_world_bank_income_source_match" ~ "source_missing_not_classified",
        static_vs_historical_classification_conflict == "static_income_label_differs_from_historical_source" ~ "historical_source_overrides_static_label",
        static_vs_historical_classification_conflict == "static_lmic_scope_differs_from_historical_source" ~ "historical_source_overrides_static_scope",
        TRUE ~ "source_backed_income_scope_resolved"
      ),
      lending_type_source_state = "static_project_lending_label_retained_descriptive_only_not_for_admissibility_scope_or_peer_selection",
      generated_by = build_id,
      generated_at = generated_at
    ) |>
    select(
      analysis_year, period, iso3, country,
      historical_income_level, historical_lending_type, historical_lmic_reporting_scope,
      static_income_level, static_lending_type, static_lmic_reporting_scope,
      historical_p14_scope_class, p14_scope_class, static_vs_historical_classification_conflict,
      classification_source_pointer, classification_confidence, classification_review_state,
      lending_type_source_state, source_income_disagreement_flag,
      wb_income_code, owid_income_raw,
      generated_by, generated_at
    )
}

classification_ledger <- make_classification_ledger(country_universe)
classification_ledger_2012_2024 <- make_classification_ledger(country_universe_2012_2024)
write_out(classification_ledger, "p14_historical_country_classification_ledger_2012_2023.csv")
write_out(classification_ledger_2012_2024, "p14_historical_country_classification_ledger_2012_2024.csv")

scope_classification_audit <- classification_ledger |>
  count(
    analysis_year, period, historical_p14_scope_class, historical_income_level,
    classification_confidence, classification_review_state,
    name = "country_years"
  ) |>
  arrange(analysis_year, historical_p14_scope_class, historical_income_level)
write_out(scope_classification_audit, "p14_historical_scope_classification_audit_2012_2023.csv")

scope_classification_audit_2012_2024 <- classification_ledger_2012_2024 |>
  count(
    analysis_year, period, historical_p14_scope_class, historical_income_level,
    classification_confidence, classification_review_state,
    name = "country_years"
  ) |>
  arrange(analysis_year, historical_p14_scope_class, historical_income_level)
write_out(scope_classification_audit_2012_2024, "p14_historical_scope_classification_audit_2012_2024.csv")

write_text(c(
  "# P14 Historical Classification Memo, 2012-2023",
  "",
  "The equalization package no longer treats current or static income labels as historically verified.",
  "",
  "Income group and developing-scope fields are rebuilt at the ISO3-year level. The primary source is the World Bank `OGHIST.xls` workbook, `Country Analytical History` sheet. For later years not covered by that workbook's historical columns, the ledger uses OWID's structured `world-bank-income-groups` grapher series, which cites World Bank Income Classifications and is tagged as a secondary structured fill.",
  "",
  "The ledger preserves the old static labels beside the historical source-backed labels and flags conflicts. LMIC/developing reporting scope is recomputed from the historical income group, not from the static export label.",
  "",
  "Lending-type labels are still carried from the project country table because a complete historical IDA/IBRD/blend source has not been reconstructed here. They are explicitly labelled as static project lending labels and are not used to decide the LMIC/developing reporting scope in this package.",
  "",
  paste0("Generated by `", build_id, "` at ", generated_at, ".")
), "p14_historical_classification_memo_2012_2023.md")

write_text(c(
  "# P14 Historical Classification Memo, 2012-2024",
  "",
  "This companion ledger extends the source-backed income/developing-scope classification logic through 2024 so the 2024 P13 anchor can later be reconciled into a combined historical panel.",
  "",
  "It does not turn the benchmark-rate package into a 2012-2024 historical benchmark panel. The benchmark evidence package remains 2012-2023 by design because 2024 remains the P13 anchor year and the new all-country archive contains only partial 2024 secondary-history chunks.",
  "",
  "Income group and developing-scope fields are rebuilt at the ISO3-year level from World Bank OGHIST where available and OWID's structured World-Bank-sourced income-group series where needed.",
  "",
  "Lending-type labels remain static project labels. They are retained only as descriptive/audit fields and are not allowed to decide admissibility, exclusion, reporting scope, or preferred peer-proxy selection in this package.",
  "",
  paste0("Generated by `", build_id, "` at ", generated_at, ".")
), "p14_historical_classification_memo_2012_2024.md")

secondary_country_for_status <- secondary_country_triage |>
  select(
    analysis_year, iso3,
    returned_identifier_count, direct_ytm_identifier_count,
    standard_direct_ytm_identifier_count, price_identifier_count,
    price_without_direct_ytm_identifier_count, repair_candidate_2_15_identifier_count,
    has_trial_repaired_country_year_rate
  )

bridge_availability_for_status <- bridge_panel |>
  transmute(
    analysis_year, iso3,
    observed_primary_available = parse_num(col_or(bridge_panel, "observed_primary_issuance__present_count", 0)) > 0,
    bridge_observed_secondary_present = parse_num(col_or(bridge_panel, "observed_secondary_market_evidence__present_count", 0)) > 0,
    ids_available = parse_num(col_or(bridge_panel, "ids_bondholders_public_proxy__present_count", 0)) > 0,
    rating_implied_available = parse_num(col_or(bridge_panel, "rating_implied_model_evidence__present_count", 0)) > 0,
    peer_proxy_available = parse_num(col_or(bridge_panel, "peer_proxy__present_count", 0)) > 0
  )

status_context_ledger <- country_universe |>
  left_join(secondary_country_for_status, by = c("analysis_year", "iso3")) |>
  left_join(bridge_availability_for_status, by = c("analysis_year", "iso3")) |>
  left_join(p14_status_existing, by = c("analysis_year", "iso3")) |>
  left_join(
    boc_default_country_year |>
      select(
        analysis_year, iso3, boc_country_names, boc_country_groups,
        boc_total_default_debt_usd_mn, boc_private_creditors_default_debt_usd_mn,
        boc_fc_bank_loans_default_debt_usd_mn, boc_fc_bonds_default_debt_usd_mn,
        boc_local_currency_debt_default_usd_mn, boc_fiscal_arrears_usd_mn,
        boc_paris_club_default_debt_usd_mn, boc_china_default_debt_usd_mn,
        boc_imf_default_debt_usd_mn, boc_default_context_flag,
        boc_private_or_bond_default_context_flag, boc_official_default_context_flag,
        boc_default_context_strength, boc_default_source_pointer
      ),
    by = c("analysis_year", "iso3")
  ) |>
  left_join(
    paris_club_country_year |>
      select(
        analysis_year, iso3, paris_club_agreement_count, paris_club_agreement_dates,
        paris_club_treatment_types, paris_club_treatment_statuses,
        paris_club_same_year_agreement_flag, paris_club_has_dssi,
        paris_club_has_common_framework, paris_club_has_non_dssi_treatment,
        paris_club_quantitative_review_flag,
        paris_club_official_treatment_context_strength, paris_club_source_pointer
      ),
    by = c("analysis_year", "iso3")
  ) |>
  mutate(
    across(
      c(
        returned_identifier_count, direct_ytm_identifier_count,
        standard_direct_ytm_identifier_count, price_identifier_count,
        price_without_direct_ytm_identifier_count, repair_candidate_2_15_identifier_count
      ),
      ~ replace_na(parse_num(.x), 0)
    ),
    across(
      c(
        boc_total_default_debt_usd_mn, boc_private_creditors_default_debt_usd_mn,
        boc_fc_bank_loans_default_debt_usd_mn, boc_fc_bonds_default_debt_usd_mn,
        boc_local_currency_debt_default_usd_mn, boc_fiscal_arrears_usd_mn,
        boc_paris_club_default_debt_usd_mn, boc_china_default_debt_usd_mn,
        boc_imf_default_debt_usd_mn
      ),
      ~ replace_na(parse_num(.x), 0)
    ),
    has_trial_repaired_country_year_rate = as_bool(has_trial_repaired_country_year_rate),
    observed_primary_available = as_bool(observed_primary_available),
    bridge_observed_secondary_present = as_bool(bridge_observed_secondary_present),
    ids_available = as_bool(ids_available),
    rating_implied_available = as_bool(rating_implied_available),
    peer_proxy_available = as_bool(peer_proxy_available),
    boc_default_context_flag = as_bool(boc_default_context_flag),
    boc_private_or_bond_default_context_flag = as_bool(boc_private_or_bond_default_context_flag),
    boc_official_default_context_flag = as_bool(boc_official_default_context_flag),
    paris_club_agreement_count = replace_na(parse_num(paris_club_agreement_count), 0),
    paris_club_same_year_agreement_flag = as_bool(paris_club_same_year_agreement_flag),
    paris_club_has_dssi = as_bool(paris_club_has_dssi),
    paris_club_has_common_framework = as_bool(paris_club_has_common_framework),
    paris_club_has_non_dssi_treatment = as_bool(paris_club_has_non_dssi_treatment),
    paris_club_quantitative_review_flag = as_bool(paris_club_quantitative_review_flag),
    boc_strong_quantitative_context_flag = boc_fc_bonds_default_debt_usd_mn >= 50 |
      boc_private_creditors_default_debt_usd_mn >= 100,
    observed_secondary_available = bridge_observed_secondary_present | standard_direct_ytm_identifier_count > 0,
    market_access_state = case_when(
      observed_primary_available ~ "ordinary_market_evidence_observed_primary",
      observed_secondary_available ~ "ordinary_market_evidence_observed_secondary",
      ids_available ~ "ids_public_bondholder_terms_available",
      returned_identifier_count > 0 & price_without_direct_ytm_identifier_count > 0 ~ "secondary_evidence_present_but_missing_direct_ytm",
      returned_identifier_count > 0 ~ "returned_secondary_identifier_review_needed",
      rating_implied_available | peer_proxy_available ~ "model_proxy_only_no_observed_market_evidence",
      TRUE ~ "no_known_hard_currency_market_evidence_in_project_sources"
    ),
    status_context_state = case_when(
      boc_private_or_bond_default_context_flag ~ "distress_restructuring_default_context_boc_boe_private_or_bond",
      paris_club_has_common_framework ~ "official_restructuring_context_paris_club_common_framework",
      paris_club_has_non_dssi_treatment ~ "official_restructuring_context_paris_club_non_dssi_treatment",
      paris_club_has_dssi ~ "official_debt_service_suspension_context_paris_club_dssi",
      boc_default_context_flag ~ "default_or_arrears_context_boc_boe_official_or_residual",
      !is.na(status_context_state_existing) & clean_text(status_context_state_existing) != "" &
        status_context_source_state_existing != "historical_source_backed_context_ledger_needed" ~ status_context_state_existing,
      TRUE ~ market_access_state
    ),
    status_context_source_state = case_when(
      boc_private_or_bond_default_context_flag ~ "source_backed_boc_boe_private_or_bond_default_context",
      paris_club_same_year_agreement_flag & boc_default_context_flag ~ "source_backed_paris_club_signed_agreement_and_boc_boe_default_context",
      paris_club_same_year_agreement_flag ~ "source_backed_paris_club_signed_agreement_context",
      boc_default_context_flag ~ "source_backed_boc_boe_default_context",
      grepl("ordinary_market_evidence|secondary_evidence|returned_secondary", market_access_state) ~ "source_backed_project_lseg_market_evidence_plus_boc_boe_default_check",
      market_access_state == "ids_public_bondholder_terms_available" ~ "source_backed_ids_terms_plus_boc_boe_default_check",
      rating_implied_available | peer_proxy_available ~ "model_proxy_only_plus_boc_boe_default_check",
      TRUE ~ "project_sources_no_market_evidence_plus_boc_boe_default_check"
    ),
    status_review_state = case_when(
      boc_strong_quantitative_context_flag ~ "source_backed_default_context_review_before_quantitative_use",
      paris_club_quantitative_review_flag ~ "source_backed_paris_club_treatment_review_before_quantitative_use",
      boc_default_context_flag ~ "source_backed_default_context_warning",
      paris_club_same_year_agreement_flag ~ "source_backed_paris_club_treatment_warning",
      grepl("ordinary_market_evidence", market_access_state) ~ "status_sources_resolved_for_market_evidence",
      market_access_state == "secondary_evidence_present_but_missing_direct_ytm" ~ "secondary_source_review_needed",
      market_access_state == "model_proxy_only_no_observed_market_evidence" ~ "model_proxy_only_market_access_interpretation_review",
      TRUE ~ "no_project_market_evidence_after_source_check"
    ),
    status_quantitative_review_trigger = boc_strong_quantitative_context_flag |
      paris_club_quantitative_review_flag |
      as_bool(affects_quantitative_admissibility_existing),
    status_quantitative_effect_rule = case_when(
      status_quantitative_review_trigger ~ "review_trigger_not_automatic_block_without_rate_object_problem_or_case_approval",
      boc_default_context_flag | paris_club_same_year_agreement_flag ~ "warning_context_only",
      TRUE ~ "no_status_based_quantitative_effect"
    ),
    affects_quantitative_admissibility = FALSE,
    affects_display_or_interpretation = TRUE,
    status_source_pointer = case_when(
      boc_default_context_flag ~ paste(
        "Bank of Canada-Bank of England Sovereign Default Database 2025;",
        "P14 bridge evidence counts; secondary repair triage"
      ),
      paris_club_same_year_agreement_flag ~ paste(
        "Paris Club signed agreements advanced search;",
        "P14 bridge evidence counts; secondary repair triage"
      ),
      TRUE ~ "P14 bridge evidence counts, secondary repair triage, and Bank of Canada-Bank of England no-positive-default-stock check where matched"
    )
  ) |>
  select(
    analysis_year, period, iso3, country, income_level, lending_type, included_in_lmic_reporting_scope,
    market_access_state, status_context_state, status_context_source_state, status_review_state,
    returned_identifier_count, direct_ytm_identifier_count, standard_direct_ytm_identifier_count,
    price_without_direct_ytm_identifier_count, repair_candidate_2_15_identifier_count,
    observed_primary_available, observed_secondary_available, ids_available,
    rating_implied_available, peer_proxy_available,
    boc_total_default_debt_usd_mn, boc_private_creditors_default_debt_usd_mn,
    boc_fc_bank_loans_default_debt_usd_mn, boc_fc_bonds_default_debt_usd_mn,
    boc_local_currency_debt_default_usd_mn, boc_fiscal_arrears_usd_mn,
    boc_paris_club_default_debt_usd_mn, boc_china_default_debt_usd_mn,
    boc_imf_default_debt_usd_mn, boc_default_context_strength,
    paris_club_agreement_count, paris_club_agreement_dates, paris_club_treatment_types,
    paris_club_treatment_statuses, paris_club_official_treatment_context_strength,
    status_quantitative_review_trigger, status_quantitative_effect_rule,
    affects_quantitative_admissibility, affects_display_or_interpretation, status_source_pointer,
    bridge_year_source_layer, bridge_secondary_coverage_state
  )
write_out(status_context_ledger, "p14_historical_status_context_ledger_2012_2023.csv")

market_access_register <- status_context_ledger |>
  count(analysis_year, period, market_access_state, status_review_state, name = "country_years") |>
  arrange(analysis_year, market_access_state)
write_out(market_access_register, "p14_historical_market_access_state_register_2012_2023.csv")

status_source_manifest <- tibble(
  source_name = c(
    "P14 provisional bridge country-year panel",
    "P14 historical secondary repair triage",
    "P14 2015-2024 diagnostic status ledger",
    "Bank of Canada-Bank of England Sovereign Default Database 2025",
    "Paris Club signed agreements advanced search"
  ),
  path = c(
    bridge_panel_path,
    secondary_country_triage_path,
    p14_status_existing_path,
    boc_boe_default_csv_path,
    paris_club_signed_agreements_csv_path
  ),
  role = c(
    "evidence availability and selected-rate context",
    "secondary returned-identifier and repair-opportunity context",
    "available diagnostic context for overlapping 2015-2023 years",
    "source-backed country-year default-stock context by creditor component",
    "source-backed official treatment event context by country-year from the public Paris Club signed-agreement advanced search"
  ),
  source_backed_for_final_method = c(TRUE, TRUE, FALSE, TRUE, TRUE),
  generated_by = build_id
)
write_out(status_source_manifest, "p14_historical_status_source_manifest_2012_2023.csv")

write_text(c(
  "# P14 Historical Status Context Memo, 2012-2023",
  "",
  "This package builds a source-backed market-evidence and default-context ledger. It is still not a complete war, sanctions, arrears, or all-restructuring ledger.",
  "",
  "Rows are classified using observed primary/secondary evidence, IDS availability, model/proxy-only status, secondary repair triage, the Bank of Canada-Bank of England Sovereign Default Database 2025, and the public Paris Club signed-agreement advanced-search extract. Positive default-stock observations create source-backed status context. Large private-creditor or foreign-currency bond default-stock observations create a stronger quantitative-use review flag. Non-DSSI/Common Framework Paris Club treatment events create a same-year official-treatment review flag; DSSI treatment creates a warning/context flag.",
  "",
  "The Paris Club advanced-search page is now parsed directly from the public paginated signed-agreement list and preserved under `data-raw/status_context_sources/paris_club_signed_agreements_2026-06-30/`.",
  "",
  "The current P14 equalization rule does not block quantitative admissibility from a status label alone. Strong default or Paris Club context creates a review trigger; actual exclusion still requires a rate-object problem, a rate-sanity problem, unresolved source semantics, or a later explicit case-level decision. Smaller or official/residual default-stock observations create display/context warnings rather than automatic exclusion.",
  "",
  paste0("Generated by `", build_id, "` at ", generated_at, ".")
), "p14_historical_status_context_memo_2012_2023.md")

primary_issue_review_register <- primary_issue |>
  left_join(status_context_ledger |> select(analysis_year, iso3, market_access_state, status_review_state), by = c("analysis_year", "iso3")) |>
  mutate(
    original_yield_present = has_original_yield_maturity & !is.na(original_yield_maturity_pct),
    amount_usable = has_amount_weight & !is.na(weight_amount) & weight_amount > 0,
    maturity_recorded = !is.na(original_maturity_years) & original_maturity_years > 0,
    currency_hard_usd_eur = hard_currency_usd_eur_flag,
    issue_review_state = case_when(
      !issue_date_in_2012_2024 ~ "outside_analysis_window",
      !currency_hard_usd_eur ~ "non_hard_currency_blocked",
      !original_yield_present ~ "missing_original_yield_blocked",
      primary_rate_sanity_state != "sane_1_30pct" ~ "rate_sanity_review",
      !maturity_recorded ~ "missing_maturity_review",
      !amount_usable ~ "missing_amount_weight_warning",
      status_review_state %in% c("source_backed_default_context_review_before_quantitative_use", "source_backed_default_context_warning") ~ "source_default_context_review_required",
      usable_for_primary_yield_trial ~ "primary_issue_review_ready",
      TRUE ~ "primary_issue_review_needed"
    ),
    thin_or_single_issue_flag = FALSE,
    source_trace_state = ifelse(clean_text(source_chunk_file) != "", "source_chunk_traceable", "source_chunk_missing")
  ) |>
  select(
    analysis_year, period, iso3, country, income_level, lending_type,
    ISIN, RIC, issue_date, maturity_date, currency,
    original_yield_maturity_pct, original_maturity_years, weight_amount,
    original_yield_present, amount_usable, currency_hard_usd_eur, maturity_recorded,
    primary_rate_sanity_state, issue_review_state, source_trace_state,
    market_access_state, status_review_state, context_document_title, source_chunk_file
  )
write_out(primary_issue_review_register, "p14_historical_primary_issue_review_register_2012_2023.csv")

primary_country_year_rates <- primary_country |>
  left_join(status_context_ledger |> select(analysis_year, iso3, market_access_state, status_review_state), by = c("analysis_year", "iso3")) |>
  mutate(
    primary_detailed_subtype = ifelse(issue_rows <= 1 | distinct_isins <= 1, "primary_warning_or_thin", "primary_standard"),
    primary_warning_label = case_when(
      primary_rate_sanity_state != "sane_1_30pct" ~ "primary_rate_sanity_review",
      rows_with_amount_weight < issue_rows ~ "primary_missing_amount_weight",
      issue_rows <= 1 | distinct_isins <= 1 ~ "thin_primary_issuance",
      status_review_state %in% c("source_backed_default_context_review_before_quantitative_use", "source_backed_default_context_warning") ~ "source_default_context_warning",
      TRUE ~ ""
    ),
    source_class = "lseg_primary_issue_terms_partial_backup",
    source_pointer = primary_country_path,
    admissibility_preview = ifelse(primary_rate_sanity_state == "sane_1_30pct", "potentially_permissible_after_status_review", "not_permissible_rate_sanity_or_missing")
  ) |>
  select(
    analysis_year, period, iso3, country, income_level, lending_type, included_in_lmic_reporting_scope,
    primary_rate_pct, primary_maturity_years, issue_rows, distinct_isins, currencies,
    rows_with_amount_weight, total_weight_amount,
    primary_rate_sanity_state, primary_detailed_subtype, primary_warning_label,
    market_access_state, status_review_state, source_class, source_pointer, admissibility_preview
  )
write_out(primary_country_year_rates, "p14_historical_primary_country_year_rates_2012_2023.csv")

primary_admissibility_audit <- primary_country_year_rates |>
  count(analysis_year, period, primary_detailed_subtype, primary_rate_sanity_state, admissibility_preview, name = "country_years") |>
  arrange(analysis_year, primary_detailed_subtype)
write_out(primary_admissibility_audit, "p14_historical_primary_admissibility_audit_2012_2023.csv")

secondary_identifier_review <- secondary_triage |>
  mutate(
    secondary_review_subtype = case_when(
      raw_direct_ytm_present & standard_secondary_eligible & currency == "USD" ~ "secondary_standard_usd_2_15_direct",
      raw_direct_ytm_present & hard_currency_usd_eur & residual_2_15 & !asset_status_blocks_standard ~ "secondary_broader_usd_eur_2_15_direct",
      raw_direct_ytm_present & hard_currency_usd_eur & !residual_2_15 ~ "secondary_direct_outside_2_15_sensitivity",
      clean_price_repair_eligible_2_15 ~ "secondary_price_repair_candidate_2_15",
      clean_price_repair_eligible_ge1 ~ "secondary_price_repair_candidate_ge1_long_or_short",
      price_without_direct_ytm ~ "secondary_price_without_direct_ytm_not_repair_ready",
      raw_direct_ytm_present ~ "secondary_direct_ytm_screened_out",
      TRUE ~ "secondary_no_usable_price_or_yield"
    ),
    secondary_review_state = case_when(
      secondary_review_subtype == "secondary_standard_usd_2_15_direct" ~ "direct_standard_candidate",
      secondary_review_subtype == "secondary_broader_usd_eur_2_15_direct" ~ "direct_broader_candidate",
      grepl("repair_candidate", secondary_review_subtype) ~ "repair_candidate_pending_method_decision",
      secondary_review_subtype == "secondary_direct_outside_2_15_sensitivity" ~ "maturity_sensitivity_only",
      TRUE ~ "blocked_or_context_only"
    )
  )
write_out(secondary_identifier_review, "p14_historical_secondary_identifier_review_2012_2023.csv")

secondary_aggregate <- function(df, scenario_id, detailed_ladder_tier_id, source_object, benchmark_status, benchmark_status_reason, permissibility_preview) {
  if (nrow(df) == 0) {
    return(tibble())
  }
  df |>
    group_by(analysis_year, period, iso3, country, income_level, lending_type, included_in_lmic_reporting_scope) |>
    summarise(
      market_rate_pct = weighted_mean_or_mean(secondary_yield_pct, amount_weight_usd),
      market_maturity_years = weighted_mean_or_mean(remaining_maturity_years, amount_weight_usd),
      issue_count = n_distinct(repair_issue_key[clean_text(repair_issue_key) != ""]),
      identifier_count = n(),
      total_weight_usd = sum(parse_num(amount_weight_usd), na.rm = TRUE),
      last_quote_date = collapse_unique(secondary_quote_date, max_items = 4),
      currency_basis = collapse_unique(currency, max_items = 4),
      source_ids = collapse_unique(source_id),
      source_scope_labels = collapse_unique(source_scope_label, max_items = 3),
      warning_label = collapse_unique(source_warning, max_items = 4),
      .groups = "drop"
    ) |>
    mutate(
      scenario_id = scenario_id,
      major_ladder_tier_id = "observed_secondary_market_evidence",
      major_ladder_tier_label = "Observed secondary market evidence",
      detailed_ladder_tier_id = detailed_ladder_tier_id,
      evidence_family = "observed_secondary_market_evidence",
      source_object = source_object,
      benchmark_status = benchmark_status,
      benchmark_status_reason = benchmark_status_reason,
      rate_sanity_state = rate_sanity(market_rate_pct),
      source_class = "lseg_secondary_history_direct_ytm",
      source_pointer = secondary_triage_path,
      permissibility_preview = permissibility_preview,
      diagnostic_only = permissibility_preview != "potentially_permissible"
    )
}

secondary_standard_rates <- secondary_aggregate(
  secondary_identifier_review |> filter(secondary_review_subtype == "secondary_standard_usd_2_15_direct"),
  "secondary_standard_usd_2_15_direct_historical_equalized",
  "secondary_standard_usd_2_15_direct",
  "lseg_secondary_direct_ytm_standard_usd_2_15",
  "computed_historical_equalization_direct_ytm",
  "direct YTM, USD, residual maturity 2-15, standard screen",
  "potentially_permissible"
)
secondary_broader_rates <- secondary_aggregate(
  secondary_identifier_review |> filter(secondary_review_subtype == "secondary_broader_usd_eur_2_15_direct"),
  "secondary_broader_usd_eur_2_15_direct_historical_equalized",
  "secondary_broader_usd_eur_2_15_direct",
  "lseg_secondary_direct_ytm_broader_usd_eur_2_15",
  "computed_historical_equalization_direct_ytm",
  "direct YTM, USD/EUR, residual maturity 2-15, status screen",
  "potentially_permissible"
)
secondary_outside_rates <- secondary_aggregate(
  secondary_identifier_review |> filter(secondary_review_subtype == "secondary_direct_outside_2_15_sensitivity"),
  "secondary_direct_ytm_outside_2_15_historical_sensitivity",
  "secondary_direct_ytm_outside_2_15_sensitivity",
  "lseg_secondary_direct_ytm_outside_standard_maturity_window",
  "computed_historical_maturity_sensitivity",
  "direct YTM outside the 2-15 residual maturity standard window",
  "diagnostic_only"
)

secondary_repair_rates <- secondary_trial_country |>
  transmute(
    analysis_year, period, iso3, country, income_level, lending_type,
    included_in_lmic_reporting_scope = NA,
    market_rate_pct = repaired_secondary_trial_rate_pct,
    market_maturity_years = repaired_secondary_trial_maturity_years,
    issue_count = repaired_issue_count,
    identifier_count = repaired_identifier_count,
    total_weight_usd,
    last_quote_date = quote_dates,
    currency_basis = "USD/EUR clean-price repair source basis",
    source_ids = source_id,
    source_scope_labels = source_scope_label,
    quote_date_present = clean_text(quote_dates) != "",
    dispersion_pass = is.na(max_identifier_dispersion_bps) | max_identifier_dispersion_bps <= 25,
    settlement_sensitivity_pass = is.na(max_settlement_sensitivity_bps) | max_settlement_sensitivity_bps <= 25,
    bid_ask_range_pass = is.na(max_bid_ask_yield_range_bps) | max_bid_ask_yield_range_bps <= 50,
    residual_maturity_pass = as_bool(residual_2_15_all),
    repair_2024_style_rule_pass = quote_date_present &
      dispersion_pass &
      settlement_sensitivity_pass &
      bid_ask_range_pass &
      residual_maturity_pass &
      rate_sanity(repaired_secondary_trial_rate_pct) == "sane_1_30pct",
    warning_label = ifelse(
      repair_2024_style_rule_pass,
      "repaired_secondary_2024_style_rule_candidate",
      "repaired_secondary_diagnostic_not_methodology"
    ),
    scenario_id = ifelse(
      repair_2024_style_rule_pass,
      "secondary_price_to_yield_repair_2024_style_candidate",
      "secondary_price_to_yield_repair_trial_2024_style_quarantine"
    ),
    major_ladder_tier_id = "observed_secondary_market_evidence",
    major_ladder_tier_label = "Observed secondary market evidence",
    detailed_ladder_tier_id = ifelse(
      repair_2024_style_rule_pass,
      "secondary_price_to_yield_repair_2024_style_candidate",
      "secondary_price_to_yield_repair_trial"
    ),
    evidence_family = "observed_secondary_market_evidence",
    source_object = "lseg_secondary_price_to_yield_repair_trial",
    benchmark_status,
    benchmark_status_reason,
    rate_sanity_state = rate_sanity(repaired_secondary_trial_rate_pct),
    source_class = "lseg_secondary_clean_price_repair_trial",
    source_pointer = secondary_trial_country_path,
    permissibility_preview = ifelse(
      repair_2024_style_rule_pass,
      "potentially_permissible_under_2024_style_repair_rule",
      "diagnostic_only_pending_repair_method_approval"
    ),
    diagnostic_only = !repair_2024_style_rule_pass,
    residual_2_15_all,
    max_identifier_dispersion_bps,
    max_settlement_sensitivity_bps,
    max_bid_ask_yield_range_bps
  )

secondary_country_year_rates <- bind_rows(
  secondary_standard_rates,
  secondary_broader_rates,
  secondary_outside_rates,
  secondary_repair_rates
) |>
  left_join(
    status_context_ledger |> select(analysis_year, iso3, market_access_state, status_review_state),
    by = c("analysis_year", "iso3")
  ) |>
  arrange(analysis_year, iso3, detailed_ladder_tier_id)
write_out(secondary_country_year_rates, "p14_historical_secondary_country_year_rates_2012_2023.csv")

secondary_subtype_register <- secondary_country_year_rates |>
  count(detailed_ladder_tier_id, source_object, permissibility_preview, diagnostic_only, name = "country_year_rows") |>
  arrange(diagnostic_only, detailed_ladder_tier_id)
write_out(secondary_subtype_register, "p14_historical_secondary_subtype_register_2012_2023.csv")

write_text(c(
  "# P14 Historical Secondary Method Decision Packet, 2012-2023",
  "",
  "This packet separates direct YTM evidence from repaired price-to-yield evidence.",
  "",
  "- Standard direct secondary: USD, residual maturity 2-15, standard screen.",
  "- Broader direct secondary: USD/EUR, residual maturity 2-15, status screen.",
  "- Outside-window direct secondary: retained as maturity sensitivity only.",
  "- Price-to-yield repaired secondary: split into rows that pass the 2024-style numeric/maturity repair rule and rows that remain diagnostic-only.",
  "",
  "No blanket approval of repaired secondary is made by this package. The 2024-style candidate subtype is labelled and remains subject to final historical method approval before paper-facing use.",
  "",
  paste0("Generated by `", build_id, "` at ", generated_at, ".")
), "p14_historical_secondary_method_decision_packet_2012_2023.md")

repair_validation_register <- secondary_repair_rates |>
  left_join(
    secondary_broader_rates |>
      select(analysis_year, iso3, direct_broader_rate_pct = market_rate_pct, direct_broader_maturity_years = market_maturity_years),
    by = c("analysis_year", "iso3")
  ) |>
  mutate(
    has_direct_overlap = !is.na(direct_broader_rate_pct),
    repaired_minus_direct_pp = market_rate_pct - direct_broader_rate_pct,
    abs_repaired_minus_direct_pp = abs(repaired_minus_direct_pp),
    quote_date_present = clean_text(last_quote_date) != "",
    dispersion_pass = is.na(max_identifier_dispersion_bps) | max_identifier_dispersion_bps <= 25,
    settlement_sensitivity_pass = is.na(max_settlement_sensitivity_bps) | max_settlement_sensitivity_bps <= 25,
    bid_ask_range_pass = is.na(max_bid_ask_yield_range_bps) | max_bid_ask_yield_range_bps <= 50,
    residual_maturity_pass = as_bool(residual_2_15_all),
    repair_thresholds_pass = quote_date_present & dispersion_pass & settlement_sensitivity_pass & bid_ask_range_pass,
    candidate_after_future_method_approval = repair_thresholds_pass & residual_maturity_pass & rate_sanity_state == "sane_1_30pct",
    repair_decision_state = case_when(
      repair_thresholds_pass & residual_maturity_pass & rate_sanity_state == "sane_1_30pct" ~ "candidate_permissible_under_2024_style_rule_pending_final_historical_approval",
      TRUE ~ "diagnostic_only_pending_method_approval"
    ),
    repair_decision_reason = case_when(
      !quote_date_present ~ "missing_quote_date",
      !residual_maturity_pass ~ "outside_standard_2_15_window_or_mixed_maturity",
      !dispersion_pass ~ "identifier_dispersion_threshold_fail",
      !settlement_sensitivity_pass ~ "settlement_sensitivity_threshold_fail",
      !bid_ask_range_pass ~ "bid_ask_range_threshold_fail",
      rate_sanity_state != "sane_1_30pct" ~ "rate_sanity_review",
      TRUE ~ "passes_numeric_trial_checks_but_method_not_approved"
    )
  )
write_out(repair_validation_register, "p14_historical_secondary_repair_validation_register_2012_2023.csv")

repair_sensitivity_tables <- repair_validation_register |>
  group_by(period) |>
  summarise(
    repaired_country_year_rows = n(),
    has_direct_overlap_rows = sum(has_direct_overlap, na.rm = TRUE),
    candidate_after_future_method_approval = sum(candidate_after_future_method_approval, na.rm = TRUE),
    residual_maturity_pass_rows = sum(residual_maturity_pass, na.rm = TRUE),
    median_rate_pct = median(market_rate_pct, na.rm = TRUE),
    median_maturity_years = median(market_maturity_years, na.rm = TRUE),
    median_abs_repaired_minus_direct_pp = median(abs_repaired_minus_direct_pp, na.rm = TRUE),
    p90_bid_ask_yield_range_bps = quantile(max_bid_ask_yield_range_bps, 0.90, na.rm = TRUE),
    .groups = "drop"
  )
write_out(repair_sensitivity_tables, "p14_historical_secondary_repair_sensitivity_tables_2012_2023.csv")

write_text(c(
  "# P14 Historical Secondary Repair Decision Memo, 2012-2023",
  "",
  "The repair validation register computes row-level diagnostics for the 417 country-year trial repaired rates.",
  "",
  "Decision state: repaired rows are no longer treated as one block. Rows that pass the 2024-style numeric, quote, bid/ask, dispersion, sanity, and 2-15 maturity checks enter the historical permissible ladder as a labelled candidate subtype. Rows that fail those checks remain diagnostic-only.",
  "",
  "Before the candidate subtype can become final paper-facing methodology, the project still needs explicit historical approval of field semantics, status-context treatment, source-scope implications, and whether the 2024 rule should be revised after cross-year validation.",
  "",
  paste0("Generated by `", build_id, "` at ", generated_at, ".")
), "p14_historical_secondary_repair_decision_memo_2012_2023.md")

ids_terms_existing_path <- file.path(outputs_dir, "p14_ids_bondholders_terms_panel_2015_2024.csv")
ids_terms_existing <- if (file.exists(ids_terms_existing_path)) {
  read_csv(ids_terms_existing_path) |>
    filter(year >= 2015, year <= 2023) |>
    rename(analysis_year = year)
} else tibble()

ids_bridge_rows <- bridge_long |>
  filter(major_ladder_tier_id == "ids_bondholders_public_proxy") |>
  transmute(
    analysis_year, period, iso3, country, income_level, lending_type,
    ids_bondholder_rate_pct = market_rate_pct_numeric,
    ids_bondholder_maturity_years = market_maturity_years_numeric,
    scenario_id,
    detailed_ladder_tier_id,
    source_class,
    source_url_or_path,
    source_note,
    method_warning,
    bridge_rate_value_id
  )

ids_terms_panel <- ids_bridge_rows |>
  left_join(
    ids_terms_existing |>
      select(
        analysis_year, iso3,
        official_grace_years, official_maturity_years, official_rate,
        has_complete_terms, ids_history_source, rate_class, term_shape,
        bondholder_term_validity_state
      ),
    by = c("analysis_year", "iso3")
  ) |>
  mutate(
    ids_source_object_state = "public_aggregate_bondholder_proxy_not_observed_market_pricing",
    ids_admissibility_preview = ifelse(!is.na(ids_bondholder_rate_pct), "potentially_permissible_public_proxy", "missing_rate_or_terms"),
    source_object_review_state = "source_object_review_still_required_before_final_wording"
  )
write_out(ids_terms_panel, "p14_historical_ids_bondholders_terms_panel_2012_2023.csv")

ids_admissibility_register <- ids_terms_panel |>
  transmute(
    analysis_year, period, iso3, country, income_level, lending_type,
    ids_bondholder_rate_pct, ids_bondholder_maturity_years,
    has_complete_terms = as_bool(has_complete_terms),
    ids_source_object_state,
    source_object_review_state,
    ids_admissibility_class = ifelse(!is.na(ids_bondholder_rate_pct), "permissible_market_benchmark_input", "not_permissible_market_benchmark_input"),
    ids_admissibility_reason = ifelse(!is.na(ids_bondholder_rate_pct), "permissible_labelled_public_proxy", "missing_rate_or_missing_source"),
    ids_admissibility_note = "IDS Bondholders is retained as a labelled public aggregate proxy, not observed market pricing."
  )
write_out(ids_admissibility_register, "p14_historical_ids_bondholders_admissibility_register_2012_2023.csv")

write_text(c(
  "# P14 Historical IDS Bondholders Source-Object Memo, 2012-2023",
  "",
  "IDS Bondholders remains a high-legitimacy public aggregate proxy where the row has usable rate evidence. It is not treated as observed market pricing.",
  "",
  "The equalization package keeps an IDS-first selected-rate variant for review but does not promote IDS-first as the preferred order.",
  "",
  "For 2012-2014, the bridge provides IDS proxy rate rows where available, but the richer terms panel currently starts in 2015. This is marked in the output rather than hidden.",
  "",
  paste0("Generated by `", build_id, "` at ", generated_at, ".")
), "p14_historical_ids_bondholders_source_object_memo_2012_2023.md")

rating_panel_path <- file.path(outputs_dir, "p14_rating_implied_panel_2015_2024.csv")
rating_panel <- if (file.exists(rating_panel_path)) {
  read_csv(rating_panel_path) |>
    filter(analysis_year >= 2015, analysis_year <= 2023)
} else tibble()

rating_rows <- bridge_long |>
  filter(major_ladder_tier_id == "rating_implied_model_evidence") |>
  mutate(
    source_vintage_state = case_when(
      grepl("archive", scenario_id) ~ "archive_vintage",
      grepl("current", scenario_id) ~ "current_vintage_blocked_for_historical_selection",
      TRUE ~ "unknown_vintage"
    )
  )

rating_component_audit <- rating_rows |>
  left_join(
    rating_panel |>
      select(analysis_year, iso3, selected_rating, selected_agency, selected_source_event_date, selected_source_rule, has_selected_precedence_rating),
    by = c("analysis_year", "iso3")
  ) |>
  transmute(
    analysis_year, period, iso3, country, income_level, lending_type,
    scenario_id, detailed_ladder_tier_id, market_rate_pct = market_rate_pct_numeric,
    rate_value_present = !is.na(market_rate_pct_numeric),
    selected_rating,
    selected_agency,
    selected_source_event_date,
    selected_source_rule,
    rating_input_state = case_when(
      !is.na(selected_rating) & clean_text(selected_rating) != "" ~ "rating_input_joined_from_panel",
      grepl("rating=", yield_source) ~ "rating_visible_in_yield_source_only",
      rate_value_present ~ "computed_rate_but_component_join_missing",
      TRUE ~ "missing_rating_or_uncomputed"
    ),
    damodaran_spread_state = ifelse(grepl("Damodaran|ctryprem|damodaran", paste(yield_source, source_file, source_url_or_path), ignore.case = TRUE), "damodaran_input_visible", "damodaran_input_not_visible"),
    risk_free_input_state = ifelse(grepl("DGS7|FRED", paste(yield_source, source_artifact, source_url_or_path), ignore.case = TRUE), "risk_free_input_visible", "risk_free_input_not_visible"),
    source_vintage_state,
    component_audit_state = case_when(
      source_vintage_state == "current_vintage_blocked_for_historical_selection" ~ "source_vintage_blocks_historical_use",
      rating_input_state %in% c("rating_input_joined_from_panel", "rating_visible_in_yield_source_only") &
        damodaran_spread_state == "damodaran_input_visible" &
        risk_free_input_state == "risk_free_input_visible" ~ "components_visible_or_joined",
      TRUE ~ "component_detail_incomplete"
    ),
    source_pointer = source_url_or_path
  )
write_out(rating_component_audit, "p14_historical_rating_implied_component_audit_2012_2023.csv")

rating_validation_sources <- bind_rows(
  if (file.exists(file.path(outputs_dir, "p14_rating_implied_validation_against_primary.csv"))) {
    read_csv(file.path(outputs_dir, "p14_rating_implied_validation_against_primary.csv")) |>
      filter(analysis_year >= 2015, analysis_year <= 2023) |>
      mutate(validation_reference = "observed_primary")
  } else tibble(),
  if (file.exists(file.path(outputs_dir, "p14_rating_implied_validation_against_secondary.csv"))) {
    read_csv(file.path(outputs_dir, "p14_rating_implied_validation_against_secondary.csv")) |>
      filter(analysis_year >= 2015, analysis_year <= 2023) |>
      mutate(validation_reference = "observed_secondary")
  } else tibble()
)
rating_validation_register <- rating_component_audit |>
  left_join(
    rating_validation_sources |>
      group_by(analysis_year, iso3) |>
      summarise(
        validation_overlap_count = n(),
        median_abs_validation_diff_pp = median(parse_num(abs_diff_pp), na.rm = TRUE),
        validation_references = collapse_unique(validation_reference),
        .groups = "drop"
      ),
    by = c("analysis_year", "iso3")
  ) |>
  mutate(
    validation_overlap_count = replace_na(validation_overlap_count, 0),
    rating_implied_validation_state = case_when(
      source_vintage_state == "current_vintage_blocked_for_historical_selection" ~ "blocked_current_vintage",
      validation_overlap_count > 0 & median_abs_validation_diff_pp <= 1.5 ~ "validation_overlap_moderate_or_better",
      validation_overlap_count > 0 ~ "validation_overlap_review_needed",
      TRUE ~ "no_observed_overlap_for_validation"
    )
  )
write_out(rating_validation_register, "p14_historical_rating_implied_validation_register_2012_2023.csv")

peer_partial_path <- file.path(partial_dir, "p14_partial_resume_peer_proxy_rate_panel_2012_2021.csv")
peer_2012_2021 <- if (file.exists(peer_partial_path)) read_csv(peer_partial_path) |> filter(analysis_year <= 2021) else tibble()
peer_2015_2024_path <- file.path(outputs_dir, "p14_peer_proxy_rate_panel_2015_2024.csv")
peer_2022_2023 <- if (file.exists(peer_2015_2024_path)) read_csv(peer_2015_2024_path) |> filter(analysis_year %in% 2022:2023) else tibble()
peer_pool_register <- bind_rows(peer_2012_2021, peer_2022_2023) |>
  filter(analysis_year >= 2012, analysis_year <= 2023) |>
  mutate(
    period = period_for_year(analysis_year),
    target_country_excluded = !mapply(
      function(country_iso3, pool) grepl(paste0("(^|;)", country_iso3, "($|;)"), clean_text(pool)),
      iso3,
      peer_pool_iso3
    ),
    peer_count = parse_num(peer_count),
    peer_iqr_pp = parse_num(peer_iqr_pp),
    peer_pool_acceptability_state = case_when(
      !target_country_excluded ~ "target_country_not_excluded_blocker",
      is.na(peer_count) | peer_count < 3 ~ "peer_pool_too_small",
      !is.na(peer_iqr_pp) & peer_iqr_pp > 5 ~ "peer_dispersion_high_review",
      TRUE ~ "peer_pool_mechanical_checks_pass"
    ),
    peer_proxy_role = "weakest_fallback_subject_to_validation"
  )
write_out(peer_pool_register, "p14_historical_peer_proxy_pool_register_2012_2023.csv")

peer_validation_register <- peer_pool_register |>
  left_join(
    bridge_panel |>
      select(
        analysis_year, iso3,
        observed_primary_issuance__present_count,
        observed_secondary_market_evidence__present_count,
        ids_bondholders_public_proxy__present_count
      ),
    by = c("analysis_year", "iso3")
  ) |>
  mutate(
    observed_overlap_available = parse_num(observed_primary_issuance__present_count) > 0 |
      parse_num(observed_secondary_market_evidence__present_count) > 0 |
      parse_num(ids_bondholders_public_proxy__present_count) > 0,
    peer_validation_state = case_when(
      peer_pool_acceptability_state != "peer_pool_mechanical_checks_pass" ~ peer_pool_acceptability_state,
      observed_overlap_available ~ "observed_or_ids_overlap_available_for_validation",
      TRUE ~ "no_direct_overlap_peer_only_context"
    )
  ) |>
  select(
    analysis_year, period, iso3, country, income_level, lending_type,
    peer_proxy_rate_pct, peer_proxy_maturity_years, peer_count,
    peer_pool_rule, peer_pool_iso3, peer_pool_source_mix, peer_iqr_pp,
    peer_dispersion_warning, target_country_excluded,
    peer_pool_acceptability_state, observed_overlap_available, peer_validation_state
  )
write_out(peer_validation_register, "p14_historical_peer_proxy_validation_register_2012_2023.csv")

peer_lending_type_governance <- peer_pool_register |>
  transmute(
    analysis_year, period, iso3, country, income_level, lending_type,
    peer_pool_rule,
    peer_proxy_rate_pct,
    peer_count,
    peer_iqr_pp,
    peer_lending_type_pool_unvalidated = grepl("static_lending", clean_text(peer_pool_rule), ignore.case = TRUE),
    peer_proxy_governance_state = case_when(
      peer_lending_type_pool_unvalidated ~ "static_lending_type_peer_pool_sensitivity_only_pending_historical_lending_source_or_rule_change",
      TRUE ~ "not_static_lending_type_pool"
    ),
    preferred_selection_effect = case_when(
      peer_lending_type_pool_unvalidated ~ "excluded_from_permissible_and_selected_best_rate_tables",
      TRUE ~ "no_peer_governance_exclusion"
    ),
    governance_note = case_when(
      peer_lending_type_pool_unvalidated ~ "Peer proxy uses a static lending-type pool. Static IDA/IBRD/blend labels are descriptive only in this package, so this row is retained for sensitivity but not admitted to the preferred permissible ladder.",
      TRUE ~ "Peer row does not use the static lending-type fallback rule."
    )
  )
write_out(peer_lending_type_governance, "p14_historical_peer_proxy_lending_type_governance_2012_2023.csv")

bridge_long <- bridge_long |>
  left_join(
    peer_lending_type_governance |>
      select(
        analysis_year, iso3, peer_pool_rule,
        peer_lending_type_pool_unvalidated, peer_proxy_governance_state,
        preferred_selection_effect, peer_governance_note = governance_note
      ),
    by = c("analysis_year", "iso3")
  ) |>
  mutate(
    peer_lending_type_pool_unvalidated = replace_na(peer_lending_type_pool_unvalidated, FALSE),
    peer_static_lending_pool_exclusion = major_ladder_tier_id == "peer_proxy" & peer_lending_type_pool_unvalidated,
    bridge_in_permissible_market_rate_ladder = ifelse(
      peer_static_lending_pool_exclusion,
      FALSE,
      bridge_in_permissible_market_rate_ladder
    ),
    bridge_selection_candidate = ifelse(
      peer_static_lending_pool_exclusion,
      FALSE,
      bridge_selection_candidate
    ),
    p14_admissibility_class = ifelse(
      peer_static_lending_pool_exclusion,
      "not_permissible_market_benchmark_input",
      p14_admissibility_class
    ),
    p14_admissibility_reason = ifelse(
      peer_static_lending_pool_exclusion,
      "source_semantics_not_validated",
      p14_admissibility_reason
    ),
    p14_admissibility_note = ifelse(
      peer_static_lending_pool_exclusion,
      "Peer proxy row uses static lending-type peer pool; retained as sensitivity only until historical lending status is sourced or the peer-pool rule is revised.",
      p14_admissibility_note
    ),
    method_warning = ifelse(
      peer_static_lending_pool_exclusion,
      trimws(gsub("^;+|;+$", "", paste(clean_text(method_warning), "static_lending_type_peer_pool_sensitivity_only", sep = ";"))),
      method_warning
    ),
    p14_warning_default = ifelse(
      peer_static_lending_pool_exclusion,
      trimws(gsub("^;+|;+$", "", paste(clean_text(p14_warning_default), "static_lending_type_peer_pool_sensitivity_only", sep = ";"))),
      p14_warning_default
    )
  )

make_extra_rows <- function(df, prefix) {
  if (nrow(df) == 0) return(bridge_long[0, ])
  out <- as.data.frame(matrix(NA, nrow = nrow(df), ncol = ncol(bridge_long)), stringsAsFactors = FALSE)
  names(out) <- names(bridge_long)
  historical_scope <- classification_ledger$historical_p14_scope_class[match(
    paste(df$analysis_year, df$iso3),
    paste(classification_ledger$analysis_year, classification_ledger$iso3)
  )]
  incomplete_comparison_secondary <- rep(FALSE, nrow(df))
  out$bridge_rate_value_id <- sprintf("%s-%06d", prefix, seq_len(nrow(df)))
  out$source_rate_value_id <- out$bridge_rate_value_id
  out$bridge_source_layer <- "historical_quality_equalization_secondary_subtype"
  out$bridge_source_year_rule <- "added by historical quality equalization package"
  out$bridge_build_id <- build_id
  out$bridge_scope_caveat <- df$source_scope_labels
  out$bridge_secondary_scope_state <- "complete_all_country_secondary_from_partial_resume_export"
  out$bridge_comparison_secondary_exclusion <- incomplete_comparison_secondary
  out$bridge_admissibility_adjustment <- case_when(
    df$diagnostic_only ~ "diagnostic_only_not_permissible",
    TRUE ~ "added_equalized_secondary_subtype"
  )
  out$bridge_in_permissible_market_rate_ladder <- !df$diagnostic_only &
    df$rate_sanity_state == "sane_1_30pct" &
    !incomplete_comparison_secondary
  out$bridge_selection_candidate <- out$bridge_in_permissible_market_rate_ladder
  out$market_rate_pct_numeric <- df$market_rate_pct
  out$market_maturity_years_numeric <- df$market_maturity_years
  out$p14_rate_value_id <- out$bridge_rate_value_id
  out$analysis_year <- df$analysis_year
  out$iso3 <- df$iso3
  out$country <- df$country
  out$income_level <- df$income_level
  out$lending_type <- df$lending_type
  out$p14_scope_class <- historical_scope
  out$p14_core_year_flag <- FALSE
  out$included_in_lmic_reporting_scope <- classification_ledger$historical_lmic_reporting_scope[match(paste(df$analysis_year, df$iso3), paste(classification_ledger$analysis_year, classification_ledger$iso3))]
  out$classification_basis <- "p14_static_embedded_country_universe_pending_todo_025_historical_classification_audit"
  out$historical_classification_audit_state <- "pending_todo_025"
  out$scenario_id <- df$scenario_id
  out$major_ladder_tier_id <- df$major_ladder_tier_id
  out$major_ladder_tier_label <- df$major_ladder_tier_label
  out$detailed_ladder_tier_id <- df$detailed_ladder_tier_id
  out$evidence_family <- df$evidence_family
  out$source_object <- df$source_object
  out$market_rate_pct <- df$market_rate_pct
  out$market_maturity_years <- df$market_maturity_years
  out$rate_value_present <- !is.na(df$market_rate_pct)
  out$rate_sanity_state <- df$rate_sanity_state
  out$benchmark_source_tier <- df$detailed_ladder_tier_id
  out$benchmark_quality_band <- ifelse(df$diagnostic_only, "diagnostic_or_sensitivity", "historical_equalized_secondary")
  out$benchmark_status <- df$benchmark_status
  out$benchmark_status_reason <- df$benchmark_status_reason
  out$market_rate_measure_basis <- df$source_object
  out$currency_basis <- df$currency_basis
  out$weighting_variable <- "amount_weight_usd"
  out$total_weight_usd <- df$total_weight_usd
  out$issue_count <- df$issue_count
  out$included_issue_count <- df$issue_count
  out$eligible_issue_count <- df$identifier_count
  out$yield_source <- df$source_object
  out$source_class <- df$source_class
  out$source_artifact <- df$source_pointer
  out$source_file <- basename(df$source_pointer)
  out$source_url_or_path <- df$source_pointer
  out$source_retrieval_date <- "2026-06-30"
  out$source_license_class <- "derived_project_output_from_lseg_terminal_export"
  out$method_version <- build_id
  out$source_note <- ifelse(
    df$benchmark_status_reason
  )
  out$method_warning <- ifelse(
    incomplete_comparison_secondary,
    paste(clean_text(df$warning_label), "incomplete_secondary_source_exclusion", sep = ";"),
    df$warning_label
  )
  out$decision_status <- ifelse(df$diagnostic_only, "diagnostic_pending_method_decision", "equalized_candidate_pending_final_review")
  out$last_quote_date <- df$last_quote_date
  out$reliability_label <- ifelse(df$diagnostic_only, "diagnostic secondary evidence", "observed market secondary direct YTM")
  out$p14_selection_role <- ifelse(df$diagnostic_only, "diagnostic_variant", "main_candidate")
  out$p14_observed_strength_priority <- case_when(
    df$detailed_ladder_tier_id == "secondary_broader_usd_eur_2_15_direct" ~ 30,
    df$detailed_ladder_tier_id == "secondary_direct_ytm_outside_2_15_sensitivity" ~ 90,
    df$detailed_ladder_tier_id == "secondary_price_to_yield_repair_trial" ~ 40,
    TRUE ~ 35
  )
  out$p14_ids_first_priority <- out$p14_observed_strength_priority
  out$p14_warning_default <- df$warning_label
  out$source_fields_traceable <- TRUE
  out$source_warning_state <- ifelse(clean_text(df$warning_label) == "", "no_warning", "warning_present")
  out$status_context_state <- status_context_ledger$status_context_state[match(paste(df$analysis_year, df$iso3), paste(status_context_ledger$analysis_year, status_context_ledger$iso3))]
  out$status_context_source_state <- status_context_ledger$status_context_source_state[match(paste(df$analysis_year, df$iso3), paste(status_context_ledger$analysis_year, status_context_ledger$iso3))]
  out$affects_quantitative_admissibility <- FALSE
  out$affects_display_or_interpretation <- TRUE
  out$p14_admissibility_reason <- ifelse(df$diagnostic_only, "diagnostic_or_validation_only", "permissible")
  out$p14_admissibility_class <- ifelse(out$bridge_in_permissible_market_rate_ladder, "permissible_market_benchmark_input", "not_permissible_market_benchmark_input")
  out$p14_admissibility_note <- ifelse(df$diagnostic_only, "Diagnostic or sensitivity row, not selected.", "Potentially permissible historical secondary subtype.")
  out$in_permissible_market_rate_ladder <- out$bridge_in_permissible_market_rate_ladder
  out
}

secondary_extra_full <- bind_rows(
  make_extra_rows(secondary_standard_rates, "P14HIST-SEC-STANDARD"),
  make_extra_rows(secondary_broader_rates, "P14HIST-SEC-BROAD"),
  make_extra_rows(secondary_outside_rates, "P14HIST-SEC-OUTSIDE"),
  make_extra_rows(secondary_repair_rates, "P14HIST-SEC-REPAIR")
)

full_labelled <- bind_rows(bridge_long, secondary_extra_full) |>
  mutate(
    p14_historical_rate_value_id = sprintf("P14HIST-RV-%07d", row_number()),
    period = period_for_year(analysis_year),
    historical_income_level = classification_ledger$historical_income_level[match(paste(analysis_year, iso3), paste(classification_ledger$analysis_year, classification_ledger$iso3))],
    historical_lmic_reporting_scope = classification_ledger$historical_lmic_reporting_scope[match(paste(analysis_year, iso3), paste(classification_ledger$analysis_year, classification_ledger$iso3))],
    historical_p14_scope_class = classification_ledger$historical_p14_scope_class[match(paste(analysis_year, iso3), paste(classification_ledger$analysis_year, classification_ledger$iso3))],
    historical_classification_review_state = classification_ledger$classification_review_state[match(paste(analysis_year, iso3), paste(classification_ledger$analysis_year, classification_ledger$iso3))],
    historical_status_review_state = status_context_ledger$status_review_state[match(paste(analysis_year, iso3), paste(status_context_ledger$analysis_year, status_context_ledger$iso3))],
    historical_status_quantitative_review_trigger = status_context_ledger$status_quantitative_review_trigger[match(paste(analysis_year, iso3), paste(status_context_ledger$analysis_year, status_context_ledger$iso3))],
    historical_status_quantitative_effect_rule = status_context_ledger$status_quantitative_effect_rule[match(paste(analysis_year, iso3), paste(status_context_ledger$analysis_year, status_context_ledger$iso3))],
    historical_affects_quantitative_admissibility = status_context_ledger$affects_quantitative_admissibility[match(paste(analysis_year, iso3), paste(status_context_ledger$analysis_year, status_context_ledger$iso3))],
    historical_source_scope_state = case_when(
      bridge_secondary_scope_state %in% c("complete_all_country_secondary_from_partial_resume_export", "not_secondary_evidence") ~ bridge_secondary_scope_state,
      grepl("v14", bridge_secondary_scope_state) ~ "legacy_secondary_scope_label_from_bridge_input",
      TRUE ~ bridge_secondary_scope_state
    ),
    historical_rate_value_present = !is.na(parse_num(market_rate_pct_numeric)) | !is.na(parse_num(market_rate_pct)),
    historical_rate_pct = ifelse(!is.na(parse_num(market_rate_pct_numeric)), parse_num(market_rate_pct_numeric), parse_num(market_rate_pct)),
    historical_maturity_years = ifelse(!is.na(parse_num(market_maturity_years_numeric)), parse_num(market_maturity_years_numeric), parse_num(market_maturity_years)),
    historical_rate_sanity_state = rate_sanity(historical_rate_pct),
    historical_warning_label = trimws(gsub("^;+|;+$", "", paste(clean_text(p14_warning_default), clean_text(method_warning), sep = ";"))),
    generated_by = build_id,
    generated_at = generated_at
  )

historical_rank <- function(major, detailed, warning, scenario) {
  case_when(
    major == "observed_primary_issuance" & grepl("thin|warning|context", paste(detailed, warning), ignore.case = TRUE) ~ 15,
    major == "observed_primary_issuance" ~ 10,
    detailed == "secondary_standard_usd_2_15_direct" ~ 20,
    detailed == "secondary_standard_outstanding_weighted" ~ 21,
    detailed %in% c("secondary_broader_usd_eur_2_15_direct") ~ 30,
    detailed %in% c("secondary_price_to_yield_repair_2024_style_candidate") ~ 40,
    detailed %in% c("secondary_price_to_yield_repair_trial") ~ 40,
    major == "ids_bondholders_public_proxy" ~ 50,
    detailed %in% c("rating_implied_archive_damodaran_dgs7") | scenario == "rating_implied_damodaran_archive_vintage_dgs7" ~ 60,
    major == "peer_proxy" ~ 80,
    TRUE ~ 999
  )
}

full_labelled <- full_labelled |>
  rowwise() |>
  mutate(
    p14_historical_observed_strength_rank = historical_rank(
      major_ladder_tier_id, detailed_ladder_tier_id, paste(p14_warning_default, method_warning), scenario_id
    ),
    p14_historical_ids_first_rank = ifelse(
      major_ladder_tier_id == "ids_bondholders_public_proxy",
      5,
      p14_historical_observed_strength_rank
    )
  ) |>
  ungroup()

admissibility_register <- full_labelled |>
  mutate(
    p14_historical_admissibility_reason = case_when(
      !historical_rate_value_present ~ "missing_rate_or_missing_source",
      historical_rate_sanity_state != "sane_1_30pct" ~ "rate_sanity_or_field_conflict",
      bridge_comparison_secondary_exclusion ~ "source_semantics_not_validated",
      detailed_ladder_tier_id == "secondary_price_to_yield_repair_trial" ~ "diagnostic_or_validation_only",
      detailed_ladder_tier_id == "secondary_direct_ytm_outside_2_15_sensitivity" ~ "diagnostic_or_validation_only",
      grepl("current_vintage|shadow_prs", scenario_id) ~ "source_semantics_not_validated",
      as_bool(historical_affects_quantitative_admissibility) | as_bool(affects_quantitative_admissibility) ~ "status_context_blocks_quantitative_use",
      bridge_in_permissible_market_rate_ladder ~ "permissible",
      detailed_ladder_tier_id == "secondary_broader_usd_eur_2_15_direct" ~ "permissible",
      p14_admissibility_class == "permissible_market_benchmark_input" ~ "permissible",
      TRUE ~ ifelse(clean_text(p14_admissibility_reason) == "", "source_semantics_not_validated", p14_admissibility_reason)
    ),
    p14_historical_admissibility_class = ifelse(
      p14_historical_admissibility_reason == "permissible",
      "permissible_market_benchmark_input",
      "not_permissible_market_benchmark_input"
    ),
    p14_historical_admissibility_note = case_when(
      p14_historical_admissibility_reason == "permissible" ~ "Permissible as a labelled historical market-benchmark input, pending final approval of the historical package.",
      p14_historical_admissibility_reason == "diagnostic_or_validation_only" ~ "Retained in full ladder but excluded from quantitative selection.",
      TRUE ~ paste("Not permissible:", p14_historical_admissibility_reason)
    ),
    p14_historical_selection_candidate = p14_historical_admissibility_class == "permissible_market_benchmark_input" &
      p14_historical_observed_strength_rank < 999
  )

write_out(full_labelled, "p14_historical_full_labelled_rate_value_ladder_2012_2023.csv")
write_out(admissibility_register, "p14_historical_rate_value_admissibility_register_2012_2023.csv")

nonpermissible_register <- admissibility_register |>
  filter(p14_historical_admissibility_class != "permissible_market_benchmark_input")
write_out(nonpermissible_register, "p14_historical_nonpermissible_rate_register_2012_2023.csv")

permissible_ladder <- admissibility_register |>
  filter(p14_historical_admissibility_class == "permissible_market_benchmark_input")
write_out(permissible_ladder, "p14_historical_permissible_market_rate_ladder_2012_2023.csv")

ladder_order_decision_register <- admissibility_register |>
  distinct(
    major_ladder_tier_id, major_ladder_tier_label, detailed_ladder_tier_id, scenario_id,
    evidence_family, source_object, p14_historical_observed_strength_rank, p14_historical_ids_first_rank
  ) |>
  mutate(
    p14_historical_order_status = case_when(
      p14_historical_observed_strength_rank < 999 ~ "candidate_or_diagnostic_ranked",
      TRUE ~ "not_selected_by_default_order"
    ),
    p14_historical_rank_basis = "P13-style source-object hierarchy, with source-specific equalized standard direct secondary preferred over legacy bridge standard secondary when both are available; not final methodology approval.",
    p14_historical_order_note = case_when(
      detailed_ladder_tier_id == "secondary_price_to_yield_repair_trial" ~ "Rank shown only if later approved; currently nonpermissible diagnostic.",
      detailed_ladder_tier_id == "secondary_standard_usd_2_15_direct" ~ "Equalized standard direct secondary from the historical all-country source; preferred over legacy bridge standard secondary in exact country-year ties.",
      detailed_ladder_tier_id == "secondary_standard_outstanding_weighted" ~ "Legacy bridge standard secondary; retained as permissible but ranked after equalized source-specific standard direct secondary.",
      major_ladder_tier_id == "ids_bondholders_public_proxy" ~ "IDS-first variant moves this tier ahead of observed market evidence for sensitivity.",
      major_ladder_tier_id %in% c("rating_implied_model_evidence", "peer_proxy") ~ "Model/proxy tier retained with validation warnings.",
      TRUE ~ "Observed-market or labelled source tier."
    ),
    generated_by = build_id,
    generated_at = generated_at
  ) |>
  arrange(p14_historical_observed_strength_rank, major_ladder_tier_id, detailed_ladder_tier_id)
write_out(ladder_order_decision_register, "p14_historical_ladder_order_decision_register_2012_2023.csv")

select_best <- function(variant) {
  rank_col <- if (variant == "ids_first") "p14_historical_ids_first_rank" else "p14_historical_observed_strength_rank"
  candidates <- permissible_ladder |>
    filter(p14_historical_selection_candidate) |>
    mutate(selection_rank = .data[[rank_col]]) |>
    arrange(analysis_year, iso3, selection_rank, p14_historical_rate_value_id)
  selected <- candidates |>
    group_by(analysis_year, iso3) |>
    summarise(
      selected_p14_historical_rate_value_id = first(p14_historical_rate_value_id),
      selected_bridge_rate_value_id = first(bridge_rate_value_id),
      selected_source_rate_value_id = first(source_rate_value_id),
      selected_rate_pct = first(historical_rate_pct),
      selected_maturity_years = first(historical_maturity_years),
      selected_scenario_id = first(scenario_id),
      selected_major_ladder_tier_id = first(major_ladder_tier_id),
      selected_major_ladder_tier_label = first(major_ladder_tier_label),
      selected_detailed_ladder_tier_id = first(detailed_ladder_tier_id),
      selected_evidence_family = first(evidence_family),
      selected_source_class = first(source_class),
      selected_reliability_label = first(reliability_label),
      selected_warning_label = first(paste(clean_text(p14_warning_default), clean_text(method_warning), sep = ";")),
      selected_source_pointer = first(source_url_or_path),
      selected_rank = first(selection_rank),
      selected_source_scope_state = first(historical_source_scope_state),
      selected_status_review_state = first(historical_status_review_state),
      alternative_permissible_rate_count = n() - 1,
      alternative_permissible_tiers = collapse_unique(detailed_ladder_tier_id),
      .groups = "drop"
    )
  country_universe |>
    left_join(selected, by = c("analysis_year", "iso3")) |>
    mutate(
      variant = variant,
      has_selected_rate = !is.na(selected_p14_historical_rate_value_id),
      selection_output_state = ifelse(has_selected_rate, "selected_rate", "no_permissible_rate"),
      no_permissible_rate_reason = ifelse(
        has_selected_rate,
        NA_character_,
        "no_permissible_market_benchmark_input_after_historical_equalization"
      ),
      generated_by = build_id,
      generated_at = generated_at
    )
}

best_observed_strength <- select_best("observed_strength")
best_ids_first <- select_best("ids_first")
write_out(best_observed_strength, "p14_historical_best_available_benchmark_observed_strength_2012_2023.csv")
write_out(best_ids_first, "p14_historical_best_available_benchmark_ids_first_2012_2023.csv")

selected_rate_variant_comparison <- best_observed_strength |>
  select(analysis_year, period, iso3, country, observed_selected_rate_pct = selected_rate_pct,
         observed_selected_major = selected_major_ladder_tier_id,
         observed_selected_detail = selected_detailed_ladder_tier_id,
         observed_has_selected_rate = has_selected_rate) |>
  left_join(
    best_ids_first |>
      select(analysis_year, iso3, ids_first_selected_rate_pct = selected_rate_pct,
             ids_first_selected_major = selected_major_ladder_tier_id,
             ids_first_selected_detail = selected_detailed_ladder_tier_id,
             ids_first_has_selected_rate = has_selected_rate),
    by = c("analysis_year", "iso3")
  ) |>
  mutate(
    variant_differs = observed_selected_detail != ids_first_selected_detail |
      abs(parse_num(observed_selected_rate_pct) - parse_num(ids_first_selected_rate_pct)) > 1e-9,
    rate_difference_ids_minus_observed_pp = parse_num(ids_first_selected_rate_pct) - parse_num(observed_selected_rate_pct)
  )
write_out(selected_rate_variant_comparison, "p14_historical_selected_rate_variant_comparison_2012_2023.csv")

display_decision_register <- best_observed_strength |>
  mutate(
    display_class = case_when(
      !has_selected_rate ~ "context_only",
      selected_major_ladder_tier_id %in% c("rating_implied_model_evidence", "peer_proxy", "shadow_or_fundamental_model_evidence") ~ "appendix_or_sensitivity",
      selected_major_ladder_tier_id == "ids_bondholders_public_proxy" ~ "main_table_with_warning",
      clean_text(selected_warning_label) != "" | selected_status_review_state != "source_triaged_market_evidence_present" ~ "main_table_with_warning",
      TRUE ~ "main_table_standard"
    ),
    display_decision_note = case_when(
      display_class == "appendix_or_sensitivity" ~ "Model/proxy selected rate; keep out of main observed-market table unless explicitly approved.",
      display_class == "main_table_with_warning" ~ "Selected rate can be shown only with source/context warning labels.",
      display_class == "context_only" ~ "No selected permissible rate; retain context row.",
      TRUE ~ "Standard observed-market display candidate."
    )
  ) |>
  select(
    analysis_year, period, iso3, country, included_in_lmic_reporting_scope,
    selected_rate_pct, selected_major_ladder_tier_id, selected_detailed_ladder_tier_id,
    selected_source_class, selected_warning_label, selected_status_review_state,
    display_class, display_decision_note, no_permissible_rate_reason
  )
write_out(display_decision_register, "p14_historical_display_decision_register_2012_2023.csv")

country_status_note_register <- best_observed_strength |>
  left_join(
    status_context_ledger |>
      select(analysis_year, iso3, market_access_state, status_context_state, status_context_source_state, status_review_state, status_source_pointer),
    by = c("analysis_year", "iso3")
  ) |>
  mutate(
    affects_quantitative_admissibility = !has_selected_rate,
    affects_paper_display = TRUE,
    rate_interpretation_class = case_when(
      !has_selected_rate ~ "context_only_no_permissible_rate",
      selected_major_ladder_tier_id %in% c("rating_implied_model_evidence", "peer_proxy") ~ "model_or_proxy_evidence_with_warning",
      selected_major_ladder_tier_id == "ids_bondholders_public_proxy" ~ "public_aggregate_proxy_not_observed_pricing",
      TRUE ~ "observed_market_or_source_evidence_with_context_note"
    ),
    archive_or_source_caveat = case_when(
      selected_source_scope_state == "complete_all_country_secondary_from_partial_resume_export" ~ "Secondary evidence uses the all-country partial-resume source where selected.",
      TRUE ~ ""
    )
  ) |>
  select(
    analysis_year, period, iso3, country, market_access_state, status_context_state,
    status_context_source_state, status_review_state, affects_quantitative_admissibility,
    affects_paper_display, rate_interpretation_class, selected_detailed_ladder_tier_id,
    selected_rate_pct, no_permissible_rate_reason, archive_or_source_caveat, status_source_pointer
  )
write_out(country_status_note_register, "p14_historical_country_status_note_register_2012_2023.csv")

paper_readiness_queue <- display_decision_register |>
  left_join(classification_ledger |> select(analysis_year, iso3, classification_review_state), by = c("analysis_year", "iso3")) |>
  left_join(status_context_ledger |> select(analysis_year, iso3, market_access_state, status_review_state), by = c("analysis_year", "iso3")) |>
  mutate(
    readiness_issue = case_when(
      classification_review_state %in% c("source_missing_not_classified", "source_disagreement_review") ~ "classification_source_review",
      status_review_state %in% c("source_backed_default_context_review_before_quantitative_use", "source_backed_default_context_warning") ~ "source_default_context_review",
      status_review_state %in% c("secondary_source_review_needed", "model_proxy_only_market_access_interpretation_review") ~ "market_evidence_context_review",
      display_class == "appendix_or_sensitivity" ~ "model_proxy_selected",
      display_class == "main_table_with_warning" ~ "warning_label_or_context_needed",
      display_class == "context_only" ~ "no_permissible_rate_context_only",
      TRUE ~ "paper_ready_standard_candidate"
    ),
    paper_readiness_state = ifelse(readiness_issue == "paper_ready_standard_candidate", "ready_for_method_review", "not_paper_ready_without_followup")
  ) |>
  select(
    analysis_year, period, iso3, country, selected_rate_pct, selected_major_ladder_tier_id,
    selected_detailed_ladder_tier_id, display_class, readiness_issue, paper_readiness_state,
    classification_review_state, market_access_state, status_review_state
  )
write_out(paper_readiness_queue, "p14_historical_paper_readiness_queue_2012_2023.csv")

selected_tier_patterns <- bind_rows(
  best_observed_strength |> mutate(variant_label = "observed_strength"),
  best_ids_first |> mutate(variant_label = "ids_first")
) |>
  count(variant_label, analysis_year, period, selected_major_ladder_tier_id, selected_detailed_ladder_tier_id, name = "country_years") |>
  group_by(variant_label, analysis_year) |>
  mutate(share = country_years / sum(country_years)) |>
  ungroup()
write_out(selected_tier_patterns, "p14_historical_selected_tier_patterns_by_period_2012_2023.csv")

primary_candidates <- admissibility_register |>
  filter(
    p14_historical_admissibility_class == "permissible_market_benchmark_input",
    major_ladder_tier_id == "observed_primary_issuance"
  ) |>
  group_by(analysis_year, iso3) |>
  arrange(p14_historical_observed_strength_rank) |>
  summarise(
    primary_rate_pct = first(historical_rate_pct),
    primary_maturity_years = first(historical_maturity_years),
    primary_detail = first(detailed_ladder_tier_id),
    .groups = "drop"
  )

validation_candidates <- admissibility_register |>
  filter(
    p14_historical_admissibility_class == "permissible_market_benchmark_input",
    major_ladder_tier_id != "observed_primary_issuance"
  ) |>
  select(analysis_year, iso3, country, income_level, lending_type, major_ladder_tier_id, detailed_ladder_tier_id, comparator_rate_pct = historical_rate_pct, comparator_maturity_years = historical_maturity_years)

observed_primary_validation <- primary_candidates |>
  inner_join(validation_candidates, by = c("analysis_year", "iso3")) |>
  mutate(
    signed_diff_pp = comparator_rate_pct - primary_rate_pct,
    abs_diff_pp = abs(signed_diff_pp),
    period = period_for_year(analysis_year)
  ) |>
  arrange(analysis_year, iso3, abs_diff_pp)
write_out(observed_primary_validation, "p14_historical_observed_primary_validation_2012_2023.csv")

primary_secondary_overlap <- observed_primary_validation |>
  filter(major_ladder_tier_id == "observed_secondary_market_evidence") |>
  group_by(analysis_year, period, detailed_ladder_tier_id) |>
  summarise(
    overlap_country_years = n(),
    median_signed_gap_pp = median(signed_diff_pp, na.rm = TRUE),
    mean_signed_gap_pp = mean(signed_diff_pp, na.rm = TRUE),
    median_abs_gap_pp = median(abs_diff_pp, na.rm = TRUE),
    mean_abs_gap_pp = mean(abs_diff_pp, na.rm = TRUE),
    .groups = "drop"
  )
write_out(primary_secondary_overlap, "p14_historical_primary_secondary_overlap_diagnostics_2012_2023.csv")

rate_sanity_status_review_queue <- admissibility_register |>
  filter(
    historical_rate_value_present,
    historical_rate_sanity_state %in% c("nonpositive_rate", "low_positive_below_1pct", "above_30pct_review")
  ) |>
  left_join(
    status_context_ledger |>
      select(
        analysis_year, iso3, market_access_state,
        boc_total_default_debt_usd_mn, boc_private_creditors_default_debt_usd_mn,
        boc_fc_bonds_default_debt_usd_mn, paris_club_agreement_count,
        paris_club_treatment_types, status_source_pointer
      ),
    by = c("analysis_year", "iso3")
  ) |>
  transmute(
    analysis_year, period, iso3, country, income_level, lending_type,
    major_ladder_tier_id, detailed_ladder_tier_id, scenario_id,
    historical_rate_pct, historical_maturity_years, historical_rate_sanity_state,
    p14_historical_admissibility_reason, market_access_state,
    status_context_state, status_context_source_state,
    status_review_state = historical_status_review_state,
    boc_total_default_debt_usd_mn, boc_private_creditors_default_debt_usd_mn,
    boc_fc_bonds_default_debt_usd_mn, paris_club_agreement_count,
    paris_club_treatment_types, source_url_or_path, status_source_pointer,
    review_priority = case_when(
      historical_rate_sanity_state == "above_30pct_review" ~ "high",
      historical_rate_sanity_state == "nonpositive_rate" ~ "high",
      TRUE ~ "medium"
    ),
    review_note = "Rate outside the current sanity bounds; inspect source, status context, and whether the rate is diagnostic/model/proxy before any paper use."
  ) |>
  arrange(factor(review_priority, levels = c("high", "medium", "low")), analysis_year, iso3, major_ladder_tier_id)
write_out(rate_sanity_status_review_queue, "p14_historical_rate_sanity_status_review_queue_2012_2023.csv")

status_context_admissibility_sensitivity <- admissibility_register |>
  filter(as_bool(historical_status_quantitative_review_trigger) | historical_status_review_state != "status_sources_resolved_for_market_evidence") |>
  mutate(
    status_review_category = case_when(
      as_bool(historical_status_quantitative_review_trigger) ~ "source_backed_status_review_trigger",
      historical_status_review_state %in% c("source_backed_default_context_warning", "source_backed_paris_club_treatment_warning") ~ "source_backed_status_warning",
      historical_status_review_state %in% c("secondary_source_review_needed", "model_proxy_only_market_access_interpretation_review") ~ "market_access_or_model_proxy_review",
      TRUE ~ "other_status_context"
    ),
    status_blocks_current_admissibility = as_bool(historical_affects_quantitative_admissibility),
    otherwise_sane_rate = historical_rate_sanity_state == "sane_1_30pct",
    currently_permissible = p14_historical_admissibility_class == "permissible_market_benchmark_input"
  ) |>
  group_by(
    status_review_category, historical_status_review_state,
    historical_status_quantitative_effect_rule, status_blocks_current_admissibility,
    otherwise_sane_rate, currently_permissible,
    major_ladder_tier_id, p14_historical_admissibility_reason
  ) |>
  summarise(rate_value_rows = n(), country_years = n_distinct(paste(analysis_year, iso3)), .groups = "drop") |>
  arrange(desc(rate_value_rows), status_review_category, major_ladder_tier_id)
write_out(status_context_admissibility_sensitivity, "p14_historical_status_context_admissibility_sensitivity_2012_2023.csv")

historical_review_queue <- bind_rows(
  paper_readiness_queue |>
    filter(readiness_issue != "paper_ready_standard_candidate") |>
    transmute(
      analysis_year, period, iso3, country,
      review_queue = readiness_issue,
      review_priority = case_when(
        readiness_issue == "source_default_context_review" ~ "high",
        readiness_issue == "no_permissible_rate_context_only" ~ "high",
        readiness_issue == "classification_source_review" ~ "high",
        readiness_issue == "model_proxy_selected" ~ "medium",
        TRUE ~ "medium"
      ),
      selected_major_ladder_tier_id,
      selected_detailed_ladder_tier_id,
      review_note = "Paper-readiness issue from display/status/classification register."
    ),
  repair_validation_register |>
    filter(candidate_after_future_method_approval | has_direct_overlap) |>
    transmute(
      analysis_year, period, iso3, country,
      review_queue = "repaired_secondary_method_decision",
      review_priority = ifelse(candidate_after_future_method_approval, "high", "medium"),
      selected_major_ladder_tier_id = "observed_secondary_market_evidence",
      selected_detailed_ladder_tier_id = "secondary_price_to_yield_repair_trial",
      review_note = repair_decision_reason
    ),
  peer_lending_type_governance |>
    filter(peer_lending_type_pool_unvalidated) |>
    transmute(
      analysis_year, period, iso3, country,
      review_queue = "peer_proxy_static_lending_type_pool_sensitivity_only",
      review_priority = "medium",
      selected_major_ladder_tier_id = "peer_proxy",
      selected_detailed_ladder_tier_id = "p14_peer_target_excluding_observed_ids_median",
      review_note = governance_note
    ),
  rate_sanity_status_review_queue |>
    transmute(
      analysis_year, period, iso3, country,
      review_queue = paste0("rate_sanity_", historical_rate_sanity_state),
      review_priority,
      selected_major_ladder_tier_id = major_ladder_tier_id,
      selected_detailed_ladder_tier_id = detailed_ladder_tier_id,
      review_note
    )
) |>
  arrange(factor(review_priority, levels = c("high", "medium", "low")), analysis_year, iso3)
write_out(historical_review_queue, "p14_historical_review_queue_2012_2023.csv")

admissibility_summary <- admissibility_register |>
  count(major_ladder_tier_id, p14_historical_admissibility_class, p14_historical_admissibility_reason, name = "rate_value_rows") |>
  arrange(major_ladder_tier_id, desc(rate_value_rows))
write_out(admissibility_summary, "p14_historical_admissibility_summary_2012_2023.csv")

quality_check_summary <- tibble(
  check_name = c(
    "country_year_universe_complete",
    "rate_value_ids_unique",
    "permissible_rows_subset_of_admissibility",
    "observed_strength_selected_ids_trace_to_permissible",
    "ids_first_selected_ids_trace_to_permissible",
    "one_observed_strength_row_per_country_year",
    "one_ids_first_row_per_country_year",
    "diagnostic_repair_rows_not_permissible",
    "no_source_excluded_secondary_selected",
    "no_static_lending_type_peer_proxy_selected"
  ),
  passed = c(
    nrow(country_universe) == 211 * 12,
    n_distinct(admissibility_register$p14_historical_rate_value_id) == nrow(admissibility_register),
    all(permissible_ladder$p14_historical_rate_value_id %in% admissibility_register$p14_historical_rate_value_id),
    all(best_observed_strength$selected_p14_historical_rate_value_id[best_observed_strength$has_selected_rate] %in% permissible_ladder$p14_historical_rate_value_id),
    all(best_ids_first$selected_p14_historical_rate_value_id[best_ids_first$has_selected_rate] %in% permissible_ladder$p14_historical_rate_value_id),
    nrow(best_observed_strength) == nrow(country_universe) && n_distinct(paste(best_observed_strength$analysis_year, best_observed_strength$iso3)) == nrow(country_universe),
    nrow(best_ids_first) == nrow(country_universe) && n_distinct(paste(best_ids_first$analysis_year, best_ids_first$iso3)) == nrow(country_universe),
      !any(permissible_ladder$detailed_ladder_tier_id == "secondary_price_to_yield_repair_trial"),
    !any(c(
      best_observed_strength$selected_p14_historical_rate_value_id,
      best_ids_first$selected_p14_historical_rate_value_id
    ) %in% admissibility_register$p14_historical_rate_value_id[as_bool(admissibility_register$bridge_comparison_secondary_exclusion)]),
    !any(c(
      best_observed_strength$selected_p14_historical_rate_value_id,
      best_ids_first$selected_p14_historical_rate_value_id
    ) %in% admissibility_register$p14_historical_rate_value_id[as_bool(admissibility_register$peer_static_lending_pool_exclusion)])
  ),
  detail = c(
    paste0("rows=", nrow(country_universe)),
    paste0("rows=", nrow(admissibility_register)),
    paste0("permissible_rows=", nrow(permissible_ladder)),
    paste0("selected_rows=", sum(best_observed_strength$has_selected_rate)),
    paste0("selected_rows=", sum(best_ids_first$has_selected_rate)),
    paste0("rows=", nrow(best_observed_strength)),
    paste0("rows=", nrow(best_ids_first)),
    paste0("repair_candidate_permissible_rows=", sum(permissible_ladder$detailed_ladder_tier_id == "secondary_price_to_yield_repair_2024_style_candidate")),
    paste0("source_excluded_selected_rows=", sum(c(
      best_observed_strength$selected_p14_historical_rate_value_id,
      best_ids_first$selected_p14_historical_rate_value_id
    ) %in% admissibility_register$p14_historical_rate_value_id[as_bool(admissibility_register$bridge_comparison_secondary_exclusion)], na.rm = TRUE)),
    paste0("static_lending_peer_rows_sensitivity_only=", sum(as_bool(admissibility_register$peer_static_lending_pool_exclusion), na.rm = TRUE))
  )
)
write_out(quality_check_summary, "p14_historical_quality_check_summary_2012_2023.csv")

selected_plot <- selected_tier_patterns |>
  filter(variant_label == "observed_strength") |>
  mutate(selected_major_ladder_tier_id = ifelse(is.na(selected_major_ladder_tier_id), "no_selected_rate", selected_major_ladder_tier_id)) |>
  ggplot(aes(analysis_year, country_years, fill = selected_major_ladder_tier_id)) +
  geom_col(width = 0.75) +
  scale_x_continuous(breaks = 2012:2023) +
  scale_y_continuous(labels = comma) +
  scale_fill_manual(values = c(
    observed_primary_issuance = econ_cols[["primary"]],
    observed_secondary_market_evidence = econ_cols[["secondary"]],
    ids_bondholders_public_proxy = econ_cols[["ids"]],
    rating_implied_model_evidence = econ_cols[["rating"]],
    peer_proxy = econ_cols[["peer"]],
    no_selected_rate = econ_cols[["muted"]]
  ), drop = FALSE) +
  labs(
    title = "Selected-rate composition remains heavily model/proxy-dependent",
    subtitle = "Observed-strength variant after historical equalization structure",
    x = NULL,
    y = "Country-years",
    caption = "Source: P14 historical quality-equalization package. This is still not accepted paper-facing historical methodology."
  ) +
  theme_econ_clean()
save_fig(selected_plot, "p14_historical_selected_tier_by_year_2012_2023.png", width = 10, height = 5.8)

admiss_plot <- admissibility_summary |>
  group_by(major_ladder_tier_id, p14_historical_admissibility_class) |>
  summarise(rate_value_rows = sum(rate_value_rows), .groups = "drop") |>
  ggplot(aes(rate_value_rows, major_ladder_tier_id, fill = p14_historical_admissibility_class)) +
  geom_col(width = 0.68) +
  scale_x_continuous(labels = comma) +
  scale_fill_manual(values = c(
    permissible_market_benchmark_input = econ_cols[["primary"]],
    not_permissible_market_benchmark_input = econ_cols[["muted"]]
  )) +
  labs(
    title = "The full ladder is broader than the permissible ladder",
    subtitle = "Rate-value rows by family and admissibility class",
    x = "Rate-value rows",
    y = NULL,
    caption = "Source: P14 historical quality-equalization admissibility register."
  ) +
  theme_econ_clean()
save_fig(admiss_plot, "p14_historical_admissibility_by_family_2012_2023.png", width = 9.5, height = 5.6)

repair_plot <- repair_validation_register |>
  count(period, repair_decision_reason, name = "country_years") |>
  ggplot(aes(country_years, repair_decision_reason, fill = period)) +
  geom_col(position = position_dodge2(width = 0.75, preserve = "single"), width = 0.65) +
  scale_x_continuous(labels = comma) +
  scale_fill_manual(values = c("2012-2015" = econ_cols[["muted"]], "2016-2019" = econ_cols[["secondary"]], "2020-2023" = econ_cols[["primary"]])) +
  labs(
    title = "Repaired secondary remains a decision queue",
    subtitle = "Diagnostic trial rows by blocking or review reason",
    x = "Country-years",
    y = NULL,
    caption = "Source: P14 historical secondary repair validation register."
  ) +
  theme_econ_clean()
save_fig(repair_plot, "p14_historical_repair_decision_reasons_2012_2023.png", width = 10, height = 6.2)

source_plot <- source_scope_register |>
  filter(source_family == "observed_secondary_market_evidence") |>
  ggplot(aes(analysis_year, universe_identifier_rows, color = source_id)) +
  geom_line(linewidth = 0.85) +
  geom_point(size = 1.8) +
  scale_x_continuous(breaks = 2012:2023) +
  scale_y_continuous(labels = comma) +
  scale_color_manual(values = c(partial_resume_20260630 = econ_cols[["primary"]])) +
  labs(
    title = "Secondary source scope is uniform through 2023",
    subtitle = "Returned identifier rows from the all-country partial-resume source",
    x = NULL,
    y = "Identifier-year rows",
    caption = "Source: P14 historical source-scope register."
  ) +
  theme_econ_clean()
save_fig(source_plot, "p14_historical_source_scope_by_year_2012_2023.png", width = 9.5, height = 5.4)

if (nrow(primary_secondary_overlap) > 0) {
  gap_plot <- primary_secondary_overlap |>
    ggplot(aes(analysis_year, median_abs_gap_pp, color = detailed_ladder_tier_id)) +
    geom_line(linewidth = 0.85) +
    geom_point(size = 1.8) +
    scale_x_continuous(breaks = 2012:2023) +
    scale_y_continuous(labels = label_number(suffix = " pp")) +
    labs(
      title = "Primary-secondary overlap is the strongest validation surface",
      subtitle = "Median absolute gap where observed primary and secondary evidence both exist",
      x = NULL,
      y = "Median absolute gap",
      caption = "Source: P14 historical observed-primary validation table."
    ) +
    theme_econ_clean()
  save_fig(gap_plot, "p14_historical_primary_secondary_gap_2012_2023.png", width = 9.5, height = 5.4)
}

readiness_plot <- paper_readiness_queue |>
  count(readiness_issue, name = "country_years") |>
  arrange(country_years) |>
  ggplot(aes(country_years, reorder(readiness_issue, country_years))) +
  geom_col(fill = econ_cols[["warning"]], width = 0.65) +
  scale_x_continuous(labels = comma) +
  labs(
    title = "Most historical rows still need non-rate method work before paper use",
    subtitle = "Paper-readiness issue counts under observed-strength selected-rate variant",
    x = "Country-years",
    y = NULL,
    caption = "Source: P14 historical paper-readiness queue."
  ) +
  theme_econ_clean()
save_fig(readiness_plot, "p14_historical_paper_readiness_issues_2012_2023.png", width = 9.5, height = 5.2)

analysis_overview <- tibble(
  metric = c(
    "country_year_rows",
    "full_labelled_rate_value_rows",
    "permissible_market_rate_rows",
    "nonpermissible_rate_rows",
    "observed_strength_selected_rows",
    "ids_first_selected_rows",
    "repaired_trial_country_year_rows",
    "repaired_2024_style_candidate_rows",
    "selected_repaired_2024_style_rows",
    "source_backed_income_scope_rows",
    "static_historical_income_conflict_rows",
    "boc_boe_default_context_rows",
    "paris_club_signed_agreement_context_rows",
    "source_status_context_review_rows"
  ),
  value = c(
    nrow(country_universe),
    nrow(full_labelled),
    nrow(permissible_ladder),
    nrow(nonpermissible_register),
    sum(best_observed_strength$has_selected_rate),
    sum(best_ids_first$has_selected_rate),
    nrow(secondary_trial_country),
    sum(repair_validation_register$repair_decision_state == "candidate_permissible_under_2024_style_rule_pending_final_historical_approval", na.rm = TRUE),
    sum(best_observed_strength$selected_detailed_ladder_tier_id == "secondary_price_to_yield_repair_2024_style_candidate", na.rm = TRUE),
    sum(classification_ledger$classification_confidence %in% c("source_backed_world_bank_oghist", "source_backed_owid_world_bank_fill"), na.rm = TRUE),
    sum(classification_ledger$static_vs_historical_classification_conflict %in% c("static_income_label_differs_from_historical_source", "static_lmic_scope_differs_from_historical_source"), na.rm = TRUE),
    sum(status_context_ledger$boc_total_default_debt_usd_mn > 0, na.rm = TRUE),
    sum(status_context_ledger$paris_club_agreement_count > 0, na.rm = TRUE),
    sum(status_context_ledger$status_review_state %in% c(
      "source_backed_default_context_review_before_quantitative_use",
      "source_backed_default_context_warning",
      "source_backed_paris_club_treatment_review_before_quantitative_use",
      "source_backed_paris_club_treatment_warning"
    ), na.rm = TRUE)
  ),
  plain_language = c(
    "One row per country-year in the 211-country by 12-year equalization universe.",
    "All retained computed, diagnostic, missing, and blocked rate-value evidence rows.",
    "Rows currently allowed as quantitative benchmark inputs in this equalization package.",
    "Rows retained in the full ladder but blocked from quantitative selection.",
    "Country-years with a selected observed-strength best rate.",
    "Country-years with a selected IDS-first best rate.",
    "All repaired secondary country-year trial rows retained for audit.",
    "Repaired secondary trial rows that pass the 2024-style quote, spread, dispersion, sanity, and 2-15 maturity checks before status/source-scope gates.",
    "Observed-strength selected country-years where the labelled 2024-style repaired-secondary candidate subtype wins.",
    "Country-years with source-backed historical income/developing-scope classification from World Bank OGHIST or OWID's World-Bank-sourced fill.",
    "Country-years where the old static label differs from the historical source-backed income/scope label.",
    "Country-years with positive Bank of Canada-Bank of England default-stock context.",
    "Country-years with a same-year Paris Club signed agreement in the parsed public advanced-search source.",
    "Country-years where Bank of Canada or Paris Club source-backed context creates a display or quantitative-use review flag."
  )
)
write_out(analysis_overview, "p14_historical_results_analysis_overview_2012_2023.csv")

manifest_paths <- c(
  list.files(run_outputs_dir, pattern = "^p14_historical_.*\\.(csv|md)$", full.names = TRUE),
  list.files(fig_dir, pattern = "^p14_historical_.*\\.png$", full.names = TRUE)
)
artifact_manifest <- tibble(
  file = basename(manifest_paths),
  path = manifest_paths,
  artifact_role = case_when(
    grepl("\\.png$", manifest_paths) ~ "figure",
    grepl("\\.md$", manifest_paths) ~ "memo",
    TRUE ~ "csv"
  ),
  generated_by = build_id,
  generated_at = generated_at
)
write_out(artifact_manifest, "p14_historical_quality_equalization_artifact_manifest_2012_2023.csv")

parse_paths <- list.files(run_outputs_dir, pattern = "^p14_historical_.*\\.csv$", full.names = TRUE)
parse_check <- bind_rows(lapply(parse_paths, function(path) {
  tryCatch({
    x <- read.csv(path, check.names = FALSE, stringsAsFactors = FALSE, comment.char = "")
    tibble(file = basename(path), parse_status = "PASS", rows = nrow(x), error = NA_character_)
  }, error = function(e) {
    tibble(file = basename(path), parse_status = "FAIL", rows = NA_integer_, error = e$message)
  })
}))
write_out(parse_check, "p14_historical_quality_equalization_parse_check_2012_2023.csv")

report_source <- file.path(reports_dir, "p14_historical_quality_equalization_report_2012_2023.Rmd")
if (!file.exists(report_source)) {
  stop("Missing report source: ", report_source, call. = FALSE)
}

rendered_report <- rmarkdown::render(
  input = report_source,
  output_file = path_out("p14_historical_results_analysis_report_2012_2023.html"),
  quiet = TRUE,
  envir = new.env(parent = globalenv())
)

final_manifest_paths <- c(
  list.files(run_outputs_dir, pattern = "^p14_historical_.*\\.(csv|md|html)$", full.names = TRUE),
  list.files(fig_dir, pattern = "^p14_historical_.*\\.png$", full.names = TRUE),
  file.path(experiment_dir, "scripts/build_p14_historical_quality_equalization_2012_2023.R"),
  report_source,
  wb_oghist_path,
  owid_income_groups_path,
  owid_income_groups_metadata_path,
  boc_boe_default_csv_path,
  boc_boe_default_json_path
)
artifact_manifest <- tibble(
  file = basename(final_manifest_paths),
  path = final_manifest_paths,
  artifact_role = case_when(
    grepl("\\.png$", final_manifest_paths) ~ "figure",
    grepl("\\.html$", final_manifest_paths) ~ "rendered_report",
    grepl("\\.Rmd$", final_manifest_paths) ~ "report_source",
    grepl("\\.R$", final_manifest_paths) ~ "builder_script",
    grepl("\\.md$", final_manifest_paths) ~ "memo",
    grepl("data-raw", final_manifest_paths) ~ "raw_source",
    TRUE ~ "csv"
  ),
  generated_by = build_id,
  generated_at = generated_at
)
write_out(artifact_manifest, "p14_historical_quality_equalization_artifact_manifest_2012_2023.csv")

message("Wrote P14 historical quality-equalization package: ", run_outputs_dir)
message("Rendered report: ", rendered_report)
