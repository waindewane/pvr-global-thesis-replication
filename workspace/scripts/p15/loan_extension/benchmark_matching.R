# Join-only helpers. No valuation, currency conversion or benchmark selection.
p15_loan_benchmark_reference <- function(selected, tiers, core) {
  selected <- data.table::as.data.table(data.table::copy(selected))
  tiers <- data.table::as.data.table(data.table::copy(tiers))
  core <- data.table::as.data.table(data.table::copy(core))
  stopifnot(!anyDuplicated(selected[, .(iso3, analysis_year)]),
    !anyDuplicated(tiers[, .(iso3, analysis_year, tier)]),
    !anyDuplicated(core[, .(iso3, analysis_year)]))
  ref <- selected[, .(iso3, analysis_year, benchmark_country = country,
    benchmark_country_year_present = TRUE, historical_income_level,
    historical_lmic_reporting_scope, benchmark_selected_tier = selected_tier,
    benchmark_selected_rate_pct = selected_rate_pct,
    benchmark_currency_basis = selected_currency_basis,
    benchmark_timing_basis = selected_timing_basis,
    benchmark_first_rate_date = selected_first_rate_date,
    benchmark_last_rate_date = selected_last_rate_date,
    benchmark_source_package_ids = selected_source_package_ids,
    benchmark_parent_evidence_id = parent_evidence_id,
    benchmark_thin_evidence = selected_thin_evidence,
    benchmark_global_peer = selected_global_peer,
    peer_pool_rule, peer_country_count, peer_iqr_pp,
    approved_status_rule_class, status_case_note, status_review_coverage,
    individual_status_review_absent, source_closure_current_use,
    comparison_timing_warning, missing_result_state)]
  ref[, secondary_ordinary_use_hold := (iso3 == "LBN" & analysis_year %in% 2020:2023) |
    (iso3 == "BLR" & analysis_year %in% 2022:2024) | (iso3 == "RUS" & analysis_year == 2022)]
  ref[, benchmark_selected_available := is.finite(benchmark_selected_rate_pct)]
  ref[, benchmark_selected_ordinary_usable := benchmark_selected_available &
    !(benchmark_selected_tier == "secondary" & secondary_ordinary_use_hold)]
  long <- tiers[, .(iso3, analysis_year, tier, benchmark_tier_rate_pct = rate_pct,
    dataset_tier_eligible = eligible, dataset_exclusion_reason = exclusion_reason)]
  at <- match(paste(long$iso3, long$analysis_year), paste(ref$iso3, ref$analysis_year))
  stopifnot(!anyNA(at))
  long[, `:=`(historical_lmic_reporting_scope = ref$historical_lmic_reporting_scope[at],
    secondary_ordinary_use_hold = ref$secondary_ordinary_use_hold[at],
    ordinary_cost_tier_usable = dataset_tier_eligible &
      !(tier == "secondary" & ref$secondary_ordinary_use_hold[at]))]
  long[, benchmark_tier_currency_basis := data.table::fifelse(tier == "ids",
    "aggregate_currency_composition_not_verified", "USD")]
  long[, benchmark_tier_timing_basis := data.table::fcase(
    tier == "primary", "annual_issue_flow", tier == "ids", "annual_commitment_terms",
    tier == "secondary", "year_end_secondary_window", tier == "moodys",
    "beginning_of_year_rating_plus_annual_USD_reference", tier == "peer", "same_year_peer_primary_flow")]
  long[, benchmark_tier_evidence_family := data.table::fcase(
    tier == "primary", "observed_primary", tier == "secondary", "observed_secondary",
    tier == "ids", "reported_contractual_proxy", tier == "moodys", "rating_implied_estimate",
    tier == "peer", "peer_estimate")]
  core_at <- match(paste(long$iso3, long$analysis_year), paste(core$iso3, core$analysis_year))
  long[, `:=`(benchmark_tier_source_package_ids = NA_character_, benchmark_tier_evidence_id = NA_character_,
    benchmark_tier_first_rate_date = NA_character_, benchmark_tier_last_rate_date = NA_character_,
    benchmark_tier_maturity_years = NA_real_)]
  for (tier_name in c("primary", "secondary")) {
    rows <- which(long$tier == tier_name); prefix <- paste0(tier_name, "_usd_")
    long[rows, `:=`(benchmark_tier_source_package_ids = core[[paste0(prefix, "source_package_ids")]][core_at[rows]],
      benchmark_tier_evidence_id = core[[paste0(prefix, "source_evidence_row_id")]][core_at[rows]],
      benchmark_tier_first_rate_date = as.character(core[[paste0(prefix, "first_rate_date")]][core_at[rows]]),
      benchmark_tier_last_rate_date = as.character(core[[paste0(prefix, "last_rate_date")]][core_at[rows]]),
      benchmark_tier_maturity_years = core[[paste0(prefix, "market_maturity_years")]][core_at[rows]])]
  }
  rows <- which(long$tier == "ids")
  long[rows, `:=`(benchmark_tier_source_package_ids = core$ids_source_package_id[core_at[rows]],
    benchmark_tier_evidence_id = core$ids_evidence_id[core_at[rows]],
    benchmark_tier_maturity_years = core$ids_maturity_years[core_at[rows]])]
  rows <- which(long$tier %in% c("moodys", "peer"))
  long[rows, benchmark_tier_source_package_ids := core$source_package_ids[core_at[rows]]]
  rows <- which(long$tier == "moodys")
  long[rows, benchmark_tier_evidence_id := NA_character_]
  long[, benchmark_core_row_locator := paste0("core_evidence.csv#iso3=", iso3, ";analysis_year=", analysis_year)]
  long[, benchmark_rating_variant_id := NA_character_]
  long[rows, benchmark_rating_variant_id := core$rating_moodys_variant_id[core_at[rows]]]
  rows <- which(long$tier == "peer")
  long[rows, benchmark_tier_evidence_id := core$peer_evidence_id[core_at[rows]]]
  if("peer_currency_basis" %in% names(core)) {
    long[rows, benchmark_tier_currency_basis := core$peer_currency_basis[core_at[rows]]]
    long[rows, benchmark_tier_timing_basis := core$peer_timing_basis[core_at[rows]]]
    long[rows, benchmark_tier_evidence_family := "model_assisted_mixed_source_peer"]
    if("peer_source_package_ids" %in% names(core))
      long[rows, benchmark_tier_source_package_ids := core$peer_source_package_ids[core_at[rows]]]
  }
  for (tier_name in c("primary", "ids", "secondary", "moodys", "peer")) {
    z <- long[tier == tier_name]
    index <- match(paste(ref$iso3, ref$analysis_year), paste(z$iso3, z$analysis_year))
    data.table::set(ref, j = paste0(tier_name, "_dataset_eligible"), value = z$dataset_tier_eligible[index])
    data.table::set(ref, j = paste0(tier_name, "_ordinary_usable"), value = z$ordinary_cost_tier_usable[index])
  }
  ref[, `:=`(any_observed_dataset_eligible = primary_dataset_eligible | secondary_dataset_eligible,
    any_observed_ordinary_usable = primary_ordinary_usable | secondary_ordinary_usable,
    primary_or_ids_ordinary_usable = primary_ordinary_usable | ids_ordinary_usable,
    period = data.table::fifelse(analysis_year <= 2014, "2012_2014", "2015_2024"))]
  long[, period := data.table::fifelse(analysis_year <= 2014, "2012_2014", "2015_2024")]
  list(country_year = ref, tiers = long)
}

p15_match_external_loans <- function(loans, reference) {
  loans <- data.table::as.data.table(data.table::copy(loans))
  stopifnot(all(c("loan_id", "iso3", "commitment_year") %in% names(loans)),
    !anyNA(loans$loan_id), all(nzchar(as.character(loans$loan_id))), !anyDuplicated(loans$loan_id))
  if (any(startsWith(names(loans), "benchmark_"))) stop("Input benchmark_* columns would collide with match fields")
  loans[, loan_id := as.character(loan_id)]
  loans[, input_row_order := seq_len(.N)]
  loans[, supplied_commitment_year := as.character(commitment_year)]
  parsed_year <- suppressWarnings(as.numeric(loans$commitment_year))
  data.table::set(loans, j = "commitment_year", value = data.table::fifelse(
    is.finite(parsed_year) & parsed_year == floor(parsed_year), as.integer(parsed_year), NA_integer_))
  if (!"loan_currency" %in% names(loans)) loans[, loan_currency := NA_character_]
  joined <- merge(loans, reference$country_year, by.x = c("iso3", "commitment_year"),
    by.y = c("iso3", "analysis_year"), all.x = TRUE, sort = FALSE)
  data.table::setorder(joined, input_row_order)
  stopifnot(nrow(joined) == nrow(loans), identical(joined$loan_id, loans$loan_id))
  joined[, match_state := data.table::fcase(
    is.na(commitment_year), "invalid_or_missing_year", is.na(iso3) | !nzchar(iso3), "missing_country_code",
    commitment_year < 2012 | commitment_year > 2024, "outside_benchmark_period",
    is.na(benchmark_country_year_present), "country_not_in_project_reference",
    !benchmark_selected_available, "matched_country_year_no_eligible_selected_rate",
    !benchmark_selected_ordinary_usable, "matched_selected_secondary_ordinary_cost_hold",
    default = "matched_conditional_selected_benchmark")]
  joined[, loan_currency_match_state := data.table::fcase(
    !benchmark_selected_available | is.na(benchmark_selected_available), "no_selected_benchmark",
    is.na(loan_currency) | !nzchar(loan_currency), "loan_currency_unknown_common_USD_scenario_requires_assumption",
    loan_currency != "USD", "non_USD_loan_requires_currency_design",
    benchmark_selected_tier == "ids", "USD_loan_but_IDS_aggregate_currency_not_verified",
    benchmark_selected_tier == "peer" & grepl("unverified|unknown|mixed|not_verified", benchmark_currency_basis),
      "USD_loan_but_peer_donor_currency_composition_not_verified",
    default = "USD_loan_and_USD_benchmark_conditional_match")]
  joined[, date_match_state := "calendar_year_match_only_not_exact_commitment_date"]
  joined[is.na(benchmark_country_year_present), date_match_state := "no_country_year_match"]
  joined[, rate_match_is_observed := benchmark_selected_tier %in% c("primary", "secondary")]
  joined[, rate_match_is_modelled := benchmark_selected_tier %in% c("moodys", "peer")]
  all_tiers <- merge(loans, reference$tiers, by.x = c("iso3", "commitment_year"),
    by.y = c("iso3", "analysis_year"), all.x = TRUE, sort = FALSE, allow.cartesian = TRUE)
  data.table::setorder(all_tiers, input_row_order, tier)
  stopifnot(setequal(all_tiers$loan_id, loans$loan_id))
  list(selected = joined, tiers = all_tiers)
}
