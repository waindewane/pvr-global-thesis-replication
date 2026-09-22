read_market_benchmarks <- function(path) {
  readr::read_csv(path, show_col_types = FALSE) |>
    dplyr::mutate(
      analysis_year = as.integer(.data$analysis_year),
      market_rate = as.numeric(.data$market_rate),
      market_maturity_years = as.numeric(.data$market_maturity_years),
      principal_amount_usd_millions = as.numeric(.data$principal_amount_usd_millions)
    )
}

build_market_benchmarks_from_ids <- function(ids_terms) {
  ids_terms |>
    dplyr::filter(.data$creditor == "Bondholders", .data$has_complete_terms) |>
    dplyr::transmute(
      iso3 = .data$iso3,
      country = .data$country,
      analysis_year = .data$year,
      market_issue_name = "IDS Bondholders new external debt commitments",
      issue_date = NA_character_,
      currency = "mixed",
      principal_amount_usd_millions = NA_real_,
      market_rate = .data$official_rate,
      market_maturity_years = .data$official_maturity_years,
      market_repayment_type = "bullet_assumption",
      discount_rate_concept = "country_year_market_equivalent_borrowing_rate",
      market_rate_measure_basis = "commitment_weighted_contract_rate",
      market_rate_selection_role = "fallback_public_proxy",
      benchmark_cashflow_representation = "synthetic_bullet_market_reference",
      benchmark_cashflow_note = paste0(
        "The Bonds row is a synthetic market-equivalent reference row used for PVR discounting. ",
        "It is not a literal replication of a specific bond cash-flow schedule. ",
        "IDS provides commitment-weighted contractual annual terms, not issue-level market pricing."
      ),
      market_rate_source_class = "world_bank_ids",
      market_rate_source_url = "https://api.worldbank.org/v2/sources/6",
      market_rate_source_note = paste0(
        "Market benchmark generated from World Bank IDS source 6, counterpart-area BND (Bondholders), ",
        "using 2024 average interest and maturity on new external debt commitments. ",
        "IDS bondholder grace is retained in ids_terms_2024.csv but not used for the Bonds row; ",
        "the market instrument is modeled as bullet debt."
      )
    ) |>
    dplyr::arrange(.data$country)
}

read_add_market_benchmarks <- function(path, analysis_year = 2024) {
  if (!file.exists(path)) {
    return(tibble::tibble(
      iso3 = character(),
      country = character(),
      analysis_year = integer(),
      market_issue_name = character(),
      issue_date = character(),
      currency = character(),
      principal_amount_usd_millions = numeric(),
      market_rate = numeric(),
      market_maturity_years = numeric(),
      market_repayment_type = character(),
      market_rate_source_class = character(),
      market_rate_source_url = character(),
      market_rate_source_note = character()
    ))
  }

  readxl::read_xlsx(path) |>
    dplyr::filter(
      .data$year == analysis_year,
      .data$instrument_type == "International Bond",
      .data$Currency %in% c("USD", "EUR"),
      !is.na(.data$interest),
      !is.na(.data$maturity),
      !is.na(.data$Amount_musd)
    ) |>
    dplyr::arrange(.data$ISO3, dplyr::desc(.data$Amount_musd), dplyr::desc(.data$maturity)) |>
    dplyr::group_by(.data$ISO3) |>
    dplyr::slice(1) |>
    dplyr::ungroup() |>
    dplyr::transmute(
      iso3 = .data$ISO3,
      country = .data$Country,
      analysis_year = .data$year,
      market_issue_name = paste0(
        "African Debt Database 2024 international bond",
        dplyr::if_else(is.na(.data$source), "", paste0(" (", .data$source, ")"))
      ),
      issue_date = as.character(as.Date(.data$issue_date)),
      currency = .data$Currency,
      principal_amount_usd_millions = as.numeric(.data$Amount_musd),
      market_rate = as.numeric(.data$interest),
      market_maturity_years = as.numeric(.data$maturity),
      market_repayment_type = "bullet_assumption",
      discount_rate_concept = "country_year_market_equivalent_borrowing_rate",
      market_rate_measure_basis = "single_security_interest_field_proxy",
      market_rate_selection_role = "preferred_observed_benchmark",
      benchmark_cashflow_representation = "synthetic_bullet_market_reference",
      benchmark_cashflow_note = paste0(
        "The Bonds row is a synthetic market-equivalent reference row used for PVR discounting. ",
        "It is not a literal replication of a specific bond cash-flow schedule."
      ),
      market_rate_source_class = "african_debt_database_public",
      market_rate_source_url = "https://africandebtdatabase.com/downloads/ADD_v2026Mar_b.xlsx",
      market_rate_source_note = paste0(
        "Selected largest 2024 USD/EUR central-government International Bond in the African Debt Database ",
        "with non-missing interest and maturity. The ADD interest field is used as the market-equivalent ",
        "issue yield/rate; rows without a usable interest value are excluded from computed PVR benchmarks."
      )
    ) |>
    dplyr::arrange(.data$country)
}

build_market_benchmarks <- function(ids_terms, add_path = "data-raw/market_rates/ADD_v2026Mar_b.xlsx") {
  add_benchmarks <- read_add_market_benchmarks(add_path)
  ids_benchmarks <- build_market_benchmarks_from_ids(ids_terms)

  dplyr::bind_rows(
    add_benchmarks,
    ids_benchmarks |> dplyr::anti_join(add_benchmarks |> dplyr::select("iso3"), by = "iso3")
  ) |>
    dplyr::arrange(.data$country)
}

build_market_benchmarks_from_lseg_tier2 <- function(lseg_country_rates) {
  lseg_country_rates |>
    dplyr::transmute(
      iso3 = .data$iso3,
      country = .data$country,
      analysis_year = .data$analysis_year,
      market_issue_name = paste0(
        "LSEG Tier 2 observed benchmark (",
        .data$observed_rate_strength,
        ", ",
        .data$included_issue_count,
        " issue",
        dplyr::if_else(.data$included_issue_count == 1, "", "s"),
        ")"
      ),
      issue_date = as.character(.data$first_issue_date),
      currency = "mixed_hard_currency",
      principal_amount_usd_millions = .data$total_weight_usd / 1e6,
      market_rate = .data$market_rate_pct,
      market_maturity_years = .data$weighted_average_maturity_years,
      market_repayment_type = "bullet_assumption",
      discount_rate_concept = "country_year_market_equivalent_borrowing_rate",
      market_rate_measure_basis = "issue_level_primary_market_yield_weighted",
      market_rate_selection_role = "preferred_observed_benchmark",
      benchmark_cashflow_representation = "synthetic_bullet_market_reference",
      benchmark_cashflow_note = paste0(
        "The Bonds row is a synthetic market-equivalent reference row used for PVR discounting. ",
        "It does not literally replicate the exact cash-flow structure of every underlying bond. ",
        "Observed issue-level yields determine the discount-rate input, while the market row remains a synthetic benchmark reference."
      ),
      market_rate_source_class = "lseg_workspace_tier2_observed",
      market_rate_source_url = "sources/market_rates/lseg_workspace_download_v6_2026-05-07/",
      market_rate_source_note = paste0(
        "Observed 2024 sovereign market-equivalent rate aggregated from the preserved LSEG Tier 2 issue set. ",
        "Observed-rate strength: ", .data$observed_rate_strength, ". ",
        "Included issue count: ", .data$included_issue_count, ". ",
        "Weighting variable: ", .data$weighting_variable, ". ",
        "See output/tables/lseg_core_benchmark_rates_2024.csv and related Tier 2 audit tables."
      )
    ) |>
    dplyr::arrange(.data$country)
}

build_market_benchmarks_2024 <- function(ids_terms, lseg_outputs) {
  ids_benchmarks <- build_market_benchmarks_from_ids(ids_terms)
  lseg_benchmarks <- build_market_benchmarks_from_lseg_tier2(lseg_outputs$country_rates)

  dplyr::bind_rows(
    lseg_benchmarks,
    ids_benchmarks |>
      dplyr::anti_join(lseg_benchmarks |> dplyr::select("iso3"), by = "iso3")
  ) |>
    dplyr::arrange(.data$country)
}

build_market_rows <- function(market_benchmarks) {
  market_benchmarks |>
    dplyr::transmute(
      iso3,
      country,
      analysis_year,
      creditor = "Bonds",
      market_issue_name,
      market_rate,
      market_maturity_years,
      market_repayment_type,
      discount_rate_concept,
      market_rate_measure_basis,
      market_rate_selection_role,
      benchmark_cashflow_representation,
      benchmark_cashflow_note,
      official_rate = market_rate,
      official_maturity_years = market_maturity_years,
      official_grace_years = NA_real_,
      discount_rate = market_rate,
      pvr = 100,
      implicit_saving_per_100 = 0,
      coverage_flag = "included",
      official_terms_source = "World Bank IDS source 6, counterpart-area BND",
      creditor_id = "BND",
      creditor_name = "Bondholders",
      creditor_scope = "market_bondholders",
      discount_rate_concept,
      market_rate_measure_basis,
      market_rate_selection_role,
      benchmark_cashflow_representation,
      benchmark_cashflow_note,
      market_rate_source_class,
      market_rate_source_url,
      market_rate_source_note
    )
}

build_official_rows <- function(ids_terms, market_benchmarks) {
  ids_terms |>
    dplyr::filter(.data$has_complete_terms, .data$creditor != "Bondholders") |>
    dplyr::inner_join(
      market_benchmarks |>
        dplyr::select(
          "iso3",
          "analysis_year",
          "market_issue_name",
          "market_rate",
          "market_maturity_years",
          "market_repayment_type",
          "discount_rate_concept",
          "market_rate_measure_basis",
          "market_rate_selection_role",
          "benchmark_cashflow_representation",
          "benchmark_cashflow_note",
          "market_rate_source_class",
          "market_rate_source_url",
          "market_rate_source_note"
        ),
      by = "iso3"
    ) |>
    dplyr::rowwise() |>
    dplyr::mutate(
      discount_rate = .data$market_rate,
      pvr = calculate_official_pvr(
        annual_rate_percent = .data$official_rate,
        maturity_years = .data$official_maturity_years,
        grace_years = .data$official_grace_years,
        discount_rate_percent = .data$discount_rate
      ),
      implicit_saving_per_100 = 100 - .data$pvr,
      coverage_flag = "included"
    ) |>
    dplyr::ungroup() |>
    dplyr::transmute(
      .data$iso3,
      .data$country,
      .data$analysis_year,
      .data$creditor,
      .data$market_issue_name,
      .data$market_rate,
      .data$market_maturity_years,
      .data$market_repayment_type,
      .data$official_rate,
      .data$official_maturity_years,
      .data$official_grace_years,
      .data$discount_rate,
      pvr = round(.data$pvr, 2),
      implicit_saving_per_100 = round(.data$implicit_saving_per_100, 2),
      .data$coverage_flag,
      official_terms_source = paste0("World Bank IDS source 6, counterpart-area ", .data$creditor_id),
      creditor_id = .data$creditor_id,
      creditor_name = dplyr::coalesce(.data$creditor_name, .data$creditor),
      creditor_scope = dplyr::coalesce(.data$creditor_scope, "unknown"),
      .data$discount_rate_concept,
      .data$market_rate_measure_basis,
      .data$market_rate_selection_role,
      .data$benchmark_cashflow_representation,
      .data$benchmark_cashflow_note,
      .data$market_rate_source_class,
      .data$market_rate_source_url,
      .data$market_rate_source_note
    )
}

build_pvr_results <- function(ids_terms, market_benchmarks) {
  computed <- dplyr::bind_rows(
    build_market_rows(market_benchmarks),
    build_official_rows(ids_terms, market_benchmarks)
  )

  unavailable <- ids_terms |>
    dplyr::filter(.data$creditor != "Bondholders") |>
    dplyr::anti_join(
      computed |> dplyr::select("iso3", "creditor"),
      by = c("iso3", "creditor")
    ) |>
    dplyr::transmute(
      iso3 = .data$iso3,
      country = .data$country,
      analysis_year = .data$year,
      creditor = .data$creditor,
      market_issue_name = NA_character_,
      market_rate = NA_real_,
      market_maturity_years = NA_real_,
      market_repayment_type = NA_character_,
      official_rate = .data$official_rate,
      official_maturity_years = .data$official_maturity_years,
      official_grace_years = .data$official_grace_years,
      discount_rate = NA_real_,
      pvr = NA_real_,
      implicit_saving_per_100 = NA_real_,
      coverage_flag = dplyr::case_when(
        !.data$has_complete_terms ~ "missing_lender_terms",
        TRUE ~ "missing_market_benchmark"
      ),
      official_terms_source = paste0("World Bank IDS source 6, counterpart-area ", .data$creditor_id),
      creditor_id = .data$creditor_id,
      creditor_name = dplyr::coalesce(.data$creditor_name, .data$creditor),
      creditor_scope = dplyr::coalesce(.data$creditor_scope, "unknown"),
      discount_rate_concept = NA_character_,
      market_rate_measure_basis = NA_character_,
      market_rate_selection_role = NA_character_,
      benchmark_cashflow_representation = NA_character_,
      benchmark_cashflow_note = NA_character_,
      market_rate_source_class = NA_character_,
      market_rate_source_url = NA_character_,
      market_rate_source_note = NA_character_
    )

  dplyr::bind_rows(computed, unavailable) |>
    dplyr::arrange(.data$country, factor(.data$creditor, levels = c("Bonds", "IBRD", "IDA", "China")))
}

build_pvr_matrix <- function(pvr_results, country_coverage) {
  matrix <- pvr_results |>
    dplyr::mutate(
      creditor_key = make.names(.data$creditor),
      pvr = as.numeric(.data$pvr),
      official_rate = as.numeric(.data$official_rate),
      official_maturity_years = as.numeric(.data$official_maturity_years),
      official_grace_years = as.numeric(.data$official_grace_years)
    ) |>
    dplyr::select(
      "iso3", "country", "analysis_year", "creditor_key",
      "pvr", "official_rate", "official_maturity_years", "official_grace_years", "coverage_flag"
    ) |>
    tidyr::pivot_wider(
      names_from = "creditor_key",
      values_from = c("pvr", "official_rate", "official_maturity_years", "official_grace_years", "coverage_flag"),
      names_glue = "{.value}__{creditor_key}"
    )

  country_coverage |>
    dplyr::select("iso3", "country", "income_level", "lending_type") |>
    dplyr::left_join(matrix, by = c("iso3", "country")) |>
    dplyr::arrange(.data$country)
}

build_country_coverage <- function(country_metadata, ids_terms, market_benchmarks, pvr_results) {
  candidate_countries <- country_metadata |>
    dplyr::filter(
      .data$is_country,
      .data$income_level %in% c("Low income", "Lower middle income", "Upper middle income")
    ) |>
    dplyr::select("iso3", "country", "income_level", "lending_type")

  has_terms <- ids_terms |>
    dplyr::filter(.data$has_complete_terms) |>
    dplyr::distinct(.data$iso3) |>
    dplyr::mutate(has_complete_ids_terms = TRUE)

  has_market <- market_benchmarks |>
    dplyr::distinct(.data$iso3) |>
    dplyr::mutate(has_public_market_benchmark = TRUE)

  included <- pvr_results |>
    dplyr::filter(.data$coverage_flag == "included") |>
    dplyr::distinct(.data$iso3) |>
    dplyr::mutate(has_pvr_result = TRUE)

  candidate_countries |>
    dplyr::left_join(has_terms, by = "iso3") |>
    dplyr::left_join(has_market, by = "iso3") |>
    dplyr::left_join(included, by = "iso3") |>
    dplyr::mutate(
      has_complete_ids_terms = dplyr::coalesce(.data$has_complete_ids_terms, FALSE),
      has_public_market_benchmark = dplyr::coalesce(.data$has_public_market_benchmark, FALSE),
      has_pvr_result = dplyr::coalesce(.data$has_pvr_result, FALSE),
      coverage_flag = dplyr::case_when(
        .data$has_pvr_result ~ "included",
        .data$has_complete_ids_terms & !.data$has_public_market_benchmark ~ "excluded_no_public_market_benchmark",
        !.data$has_complete_ids_terms ~ "excluded_missing_ids_terms",
        TRUE ~ "deferred_priced_out_phase2"
      )
    ) |>
    dplyr::arrange(.data$coverage_flag, .data$country)
}

write_output_csv <- function(data, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  readr::write_csv(data, path, na = "")
  path
}
