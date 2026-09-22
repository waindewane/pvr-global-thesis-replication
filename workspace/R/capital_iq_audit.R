read_capital_iq_export <- function(path) {
  data <- readxl::read_xlsx(path, sheet = "Sheet1", skip = 4) |>
    tibble::as_tibble()

  data <- data[rowSums(!is.na(data)) > 0, ]

  data |>
    dplyr::rename_with(~ make.unique(.x, sep = "_")) |>
    dplyr::mutate(
      dplyr::across(
        where(is.character),
        ~ dplyr::na_if(dplyr::na_if(trimws(.x), "NA"), "")
      ),
      issue_year = as.integer(format(.data$SPT_ISSUE_DATE, "%Y")),
      market_rate_observed = dplyr::coalesce(
        as.numeric(.data$SPT_OFFERING_YIELD),
        as.numeric(.data$SPT_YIELD_TO_MATURITY_AT_ISSUANCE)
      ),
      has_observed_yield = !is.na(.data$market_rate_observed),
      has_auction_amount = !is.na(.data$SPT_AUCTION_TOTAL_AMOUNT_ISSUED),
      has_deal_amount = !is.na(.data$SPT_DEAL_TOTAL_OFFERING_AMOUNT),
      issue_currency_group = dplyr::case_when(
        .data$SPT_ISSUE_CURRENCY %in% c("USD", "EUR") ~ "hard_currency_usd_eur",
        is.na(.data$SPT_ISSUE_CURRENCY) ~ "missing_currency",
        TRUE ~ "other_currency"
      ),
      candidate_external_benchmark = .data$issue_currency_group == "hard_currency_usd_eur" &
        !is.na(.data$SPT_MAT_DATE) &
        .data$has_observed_yield
    )
}

build_capital_iq_amount_diagnostic <- function(capital_iq_export) {
  capital_iq_export |>
    dplyr::mutate(
      amount_population_case = dplyr::case_when(
        .data$has_auction_amount & .data$has_deal_amount ~ "both_non_missing",
        .data$has_auction_amount & !.data$has_deal_amount ~ "auction_only",
        !.data$has_auction_amount & .data$has_deal_amount ~ "deal_only",
        TRUE ~ "both_missing"
      )
    ) |>
    dplyr::count(.data$amount_population_case, name = "rows")
}

build_capital_iq_currency_summary <- function(capital_iq_export) {
  capital_iq_export |>
    dplyr::count(
      .data$issue_year,
      .data$SPT_ISSUE_CURRENCY,
      .data$SPT_FIXED_INCOME,
      name = "rows",
      sort = TRUE
    )
}

build_capital_iq_issuer_summary_2024 <- function(capital_iq_export) {
  capital_iq_export |>
    dplyr::filter(.data$issue_year == 2024) |>
    dplyr::group_by(.data$SPT_ISSUER_NAME) |>
    dplyr::summarise(
      rows = dplyr::n(),
      usable_yield_rows = sum(.data$has_observed_yield, na.rm = TRUE),
      hard_currency_rows = sum(.data$issue_currency_group == "hard_currency_usd_eur", na.rm = TRUE),
      currencies = paste(sort(unique(stats::na.omit(.data$SPT_ISSUE_CURRENCY))), collapse = ", "),
      fixed_income_types = paste(sort(unique(stats::na.omit(.data$SPT_FIXED_INCOME))), collapse = ", "),
      .groups = "drop"
    ) |>
    dplyr::arrange(dplyr::desc(.data$usable_yield_rows), dplyr::desc(.data$rows), .data$SPT_ISSUER_NAME)
}

build_capital_iq_candidate_benchmarks <- function(capital_iq_export, hard_currency_only = FALSE) {
  candidates <- capital_iq_export |>
    dplyr::filter(.data$issue_year == 2024, .data$has_observed_yield)

  if (hard_currency_only) {
    candidates <- candidates |>
      dplyr::filter(.data$issue_currency_group == "hard_currency_usd_eur")
  }

  candidates |>
    dplyr::arrange(
      .data$SPT_ISSUER_NAME,
      dplyr::desc(.data$has_deal_amount),
      dplyr::desc(as.numeric(.data$SPT_DEAL_TOTAL_OFFERING_AMOUNT)),
      dplyr::desc(as.numeric(.data$SPT_AUCTION_TOTAL_AMOUNT_ISSUED)),
      dplyr::desc(as.numeric(.data$SPT_TENOR))
    ) |>
    dplyr::group_by(.data$SPT_ISSUER_NAME) |>
    dplyr::slice(1) |>
    dplyr::ungroup() |>
    dplyr::transmute(
      issuer = .data$SPT_ISSUER_NAME,
      description = .data$SPT_DESCRIPTION,
      issue_date = as.Date(.data$SPT_ISSUE_DATE),
      fixed_income_type = .data$SPT_FIXED_INCOME,
      issue_currency = .data$SPT_ISSUE_CURRENCY,
      market_rate_observed = .data$market_rate_observed,
      tenor_years = as.numeric(.data$SPT_TENOR),
      maturity_date = as.Date(.data$SPT_MAT_DATE),
      issue_geography = .data$SPT_ISSUE_GEOGRAPHY,
      country_of_issuance = .data$SPT_COUNTRYOFISSUANCE,
      auction_total_amount_issued = as.numeric(.data$SPT_AUCTION_TOTAL_AMOUNT_ISSUED),
      deal_total_offering_amount = as.numeric(.data$SPT_DEAL_TOTAL_OFFERING_AMOUNT),
      isin = .data$SPT_ISIN,
      has_auction_amount = .data$has_auction_amount,
      has_deal_amount = .data$has_deal_amount,
      issue_currency_group = .data$issue_currency_group
    ) |>
    dplyr::arrange(.data$issuer)
}

write_capital_iq_audit_outputs <- function(
  export_path = "data-raw/market_rates/SPGlobal_Export_4-25-2026_668976f7-4a3c-4e50-b46b-ec90bd160b97.xlsx",
  output_dir = "output/tables"
) {
  export <- read_capital_iq_export(export_path)

  outputs <- list(
    capital_iq_amount_field_diagnostic = build_capital_iq_amount_diagnostic(export),
    capital_iq_currency_summary = build_capital_iq_currency_summary(export),
    capital_iq_issuer_summary_2024 = build_capital_iq_issuer_summary_2024(export),
    capital_iq_candidate_benchmarks_2024_any_currency = build_capital_iq_candidate_benchmarks(export, hard_currency_only = FALSE),
    capital_iq_candidate_benchmarks_2024_hard_currency = build_capital_iq_candidate_benchmarks(export, hard_currency_only = TRUE)
  )

  purrr::iwalk(outputs, ~ write_output_csv(.x, file.path(output_dir, paste0(.y, ".csv"))))

  invisible(outputs)
}
