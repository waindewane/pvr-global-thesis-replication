read_capital_iq_field_list <- function(path, workbook_name = NULL) {
  if (is.null(workbook_name)) {
    workbook_name <- tools::file_path_sans_ext(basename(path))
  }
  sheet <- readxl::excel_sheets(path)[1]

  readxl::read_xlsx(path, sheet = sheet, skip = 4) |>
    rlang::set_names(c(
      "category",
      "subcategory_1",
      "subcategory_2",
      "subcategory_3",
      "subcategory_4",
      "subcategory_5",
      "subcategory_6",
      "keyfield",
      "field_alias",
      "field_name",
      "definition"
    )) |>
    tibble::as_tibble() |>
    dplyr::mutate(
      workbook = workbook_name,
      source_file = basename(path),
      search_text = stringr::str_to_lower(
        stringr::str_squish(
          paste(
            .data$category,
            .data$subcategory_1,
            .data$subcategory_2,
            .data$subcategory_3,
            .data$subcategory_4,
            .data$subcategory_5,
            .data$subcategory_6,
            .data$field_alias,
            .data$field_name,
            .data$definition
          )
        )
      )
    )
}

build_capital_iq_field_index <- function(
  raw_dir = "sources/market_rates/capital_iq_research_2026-04-25/raw",
  output_dir = "data-derived/capital_iq_field_index"
) {
  files <- c(
    companies = file.path(raw_dir, "MI Office Field List Companies.xlsx"),
    securities = file.path(raw_dir, "MI Office Field List Securities.xlsx"),
    transactions = file.path(raw_dir, "MI Office Field List Transactions.xlsx"),
    geographic_intelligence = file.path(raw_dir, "MI_Office_Field_List_Geographic Intelligence.xlsx"),
    key_developments = file.path(raw_dir, "MI_Office_Field_List_Key Developments.xlsx")
  )

  index <- purrr::imap_dfr(files, read_capital_iq_field_list)

  keyword_patterns <- c(
    "sovereign",
    "government institution",
    "issue amount",
    "deal total offering",
    "offering yield",
    "yield to maturity at issuance",
    "yield to maturity",
    "issue currency",
    "maturity",
    "coupon",
    "reg s",
    "144a",
    "global security",
    "issue geography",
    "country of issuance",
    "offering price",
    "settlement date",
    "offering announcement date",
    "private placement",
    "gross amount offered"
  )

  keyword_hits <- purrr::map_dfr(keyword_patterns, function(pattern) {
    index |>
      dplyr::filter(stringr::str_detect(.data$search_text, stringr::fixed(pattern))) |>
      dplyr::mutate(search_pattern = pattern)
  }) |>
    dplyr::distinct(.data$search_pattern, .data$workbook, .data$field_alias, .keep_all = TRUE)

  recommended_fields <- tibble::tribble(
    ~workflow_layer, ~workbook, ~field_alias, ~field_name, ~why_relevant,
    "transactions_primary", "transactions", "SPTR_OFFERING_ANNOUNCEMENT_DATE", "Offering Announcement Date", "Best transaction-date candidate for identifying the 2024 new issue event.",
    "transactions_primary", "transactions", "SPTR_TRADE_DATE", "Trade Date", "Alternative transaction date if announcement date is too broad.",
    "transactions_primary", "transactions", "SPTR_SETTLEMENT_DATE", "Settlement Date", "Useful for final pricing/closing timing.",
    "transactions_primary", "transactions", "SPTR_YTM", "Yield to Maturity", "Deal-level yield-at-issuance concept aligned with OECD-style primary market benchmarking.",
    "transactions_primary", "transactions", "SPTR_OFFERING_PRICE", "Offering Price", "Can support yield reconstruction when yield is missing.",
    "transactions_primary", "transactions", "SPTR_DIV_RATE", "Coupon", "Fallback nominal rate and bond term metadata.",
    "transactions_primary", "transactions", "SPTR_MATURITY", "Maturity", "Required for maturity date and term logic.",
    "transactions_primary", "transactions", "SPTR_GROSS_AMT", "Gross Amount Offered, Including Overallotment", "Closest observed deal-level amount field in the Offerings domain.",
    "transactions_primary", "transactions", "SPTR_PAR_VALUE", "Par Value", "Useful for understanding debt pricing conventions and face value.",
    "transactions_primary", "transactions", "SPTR_ISIN", "ISIN", "Needed for bond-level identification and deduplication.",
    "transactions_primary", "transactions", "SPTR_144A", "Restricted 144a?", "Helps identify US tranche structure and deal duplication issues.",
    "transactions_primary", "transactions", "SPTR_PRIVATE_PLACEMENT", "Private Placement?", "Can exclude non-public placements if needed.",
    "transactions_primary", "transactions", "SPTR_OFFERING_DESC", "Offering Description", "May help isolate debt public offerings from other capital raises.",
    "securities_support", "securities", "SPT_ISSUE_CURRENCY", "Issue Currency", "Cleanest confirmed currency field for the security itself.",
    "securities_support", "securities", "SPT_FIXED_INCOME", "Fixed Income Type", "Needed to check bond/note/sukuk/program classification.",
    "securities_support", "securities", "SPT_ISSUE_AMOUNT", "Issue Amount", "Confirmed populated security-level amount field.",
    "securities_support", "securities", "SPT_COUNTRYOFISSUANCE", "Country of Issuance", "Supports issuer/country mapping.",
    "securities_support", "securities", "SPT_ISSUE_GEOGRAPHY", "Country/Region Name", "Useful extra issuer geography label.",
    "securities_support", "securities", "SPT_OFFERING_YIELD", "Offering Yield", "Security-level observed yield at issuance.",
    "securities_support", "securities", "SPT_YIELD_TO_MATURITY_AT_ISSUANCE", "Yield to Maturity at Issuance", "Security-level observed yield at issuance.",
    "securities_support", "securities", "SPT_REGS", "RegS", "Supports Reg S / 144A deduplication checks.",
    "securities_support", "securities", "SPT_144A", "144A", "Supports Reg S / 144A deduplication checks.",
    "securities_support", "securities", "SPT_GLOBAL_SECURITY", "Global Security?", "May help identify exchanged/global securities after 144A/Reg S processes.",
    "geographic_fallback", "geographic_intelligence", "CURRENT_YLD", "Current Yield", "Potential fallback sovereign yield series when issue-level data are absent.",
    "geographic_fallback", "geographic_intelligence", "DATE_OF_CURRENT_YLD", "Date of Current Yield", "Observation date for current yield.",
    "geographic_fallback", "geographic_intelligence", "OBSERVED_IMPLIED_INDICATOR", "Observed/ Implied Indicator", "Clarifies whether the sovereign yield is observed or carried forward/implied.",
    "geographic_fallback", "geographic_intelligence", "SOVEREIGN_YLD_NAME", "Sovereign Yield Name", "Naming for sovereign yield curve/instrument series.",
    "geographic_fallback", "geographic_intelligence", "SOVEREIGN_YLD_TICKER", "Sovereign Yield Ticker", "Ticker or code for sovereign yield series."
  ) |>
    dplyr::left_join(
      index |>
        dplyr::select("workbook", "field_alias", "definition") |>
        dplyr::distinct(),
      by = c("workbook", "field_alias")
    )

  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  readr::write_csv(index, file.path(output_dir, "capital_iq_field_index.csv"), na = "")
  readr::write_csv(keyword_hits, file.path(output_dir, "capital_iq_priority_field_hits.csv"), na = "")
  readr::write_csv(recommended_fields, file.path(output_dir, "capital_iq_recommended_fields.csv"), na = "")

  invisible(
    list(
      index = index,
      keyword_hits = keyword_hits,
      recommended_fields = recommended_fields
    )
  )
}
