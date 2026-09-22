normalize_country_name_for_join <- function(country) {
  dplyr::case_when(
    country == "Cote Ivoire" ~ "Cote d'Ivoire",
    country == "Egypt" ~ "Egypt, Arab Rep.",
    country == "Congo" ~ "Congo, Rep.",
    country == "Swaziland" ~ "Eswatini",
    country == "Türkiye" ~ "Turkiye",
    TRUE ~ country
  )
}

classify_lseg_nonstandard_issue <- function(document_title, debt_type, instrument_type, coupon_type) {
  text <- paste(document_title, debt_type, instrument_type, coupon_type, sep = " | ")
  grepl(
    "Step Up|Zero Coupon|Zero then|Multiple Payment|Index Linked|Pay at Maturity|Discount Bond",
    text,
    ignore.case = TRUE
  )
}

build_lseg_economic_issue_key <- function(country, issue_date, maturity_date, currency, coupon_rate, face_issued_usd) {
  paste(
    country,
    issue_date,
    maturity_date,
    currency,
    format(round(as.numeric(coupon_rate), 6), trim = TRUE, scientific = FALSE),
    format(round(as.numeric(face_issued_usd), 0), trim = TRUE, scientific = FALSE),
    sep = " | "
  )
}

read_lseg_benchmark_candidates <- function(path) {
  readr::read_csv(path, show_col_types = FALSE) |>
    dplyr::mutate(
      IssueDate = as.Date(.data$IssueDate),
      MaturityDate = as.Date(.data$MaturityDate),
      issue_year = as.integer(format(.data$IssueDate, "%Y")),
      CouponRate = as.numeric(.data$CouponRate),
      FaceIssuedUSD = as.numeric(.data$FaceIssuedUSD),
      FaceOutstandingUSD = as.numeric(.data$FaceOutstandingUSD),
      original_maturity_days = as.numeric(.data$original_maturity_days),
      IssuePrice = as.numeric(.data$IssuePrice),
      yield_final_preference = as.numeric(.data$yield_final_preference)
    )
}

build_lseg_core_benchmark_outputs_2024 <- function(
  candidate_path,
  base_path = "sources/market_rates/lseg_workspace_download_v6_2026-05-07/output/tables/lseg_oecd_base_with_yield_fallback_oecd_base_2000-01-01_2025-12-31.csv",
  ids_terms_path,
  country_metadata_path = "data-raw/world_bank_countries.json"
) {
  candidates <- read_lseg_benchmark_candidates(candidate_path) |>
    dplyr::filter(.data$issue_year == 2024)

  broader_base <- read_lseg_benchmark_candidates(base_path) |>
    dplyr::filter(.data$issue_year == 2024) |>
    dplyr::mutate(
      country = .data$oecd_country_request,
      economic_issue_key = build_lseg_economic_issue_key(
        .data$oecd_country_request,
        .data$IssueDate,
        .data$MaturityDate,
        .data$Currency,
        .data$CouponRate,
        .data$FaceIssuedUSD
      )
    )

  ids_terms <- readr::read_csv(ids_terms_path, show_col_types = FALSE) |>
    dplyr::filter(.data$creditor == "Bondholders", .data$has_complete_terms) |>
    dplyr::transmute(
      ids_country = .data$country,
      has_ids_bondholders = TRUE,
      ids_bondholders_rate = as.numeric(.data$official_rate),
      ids_bondholders_maturity_years = as.numeric(.data$official_maturity_years)
    )

  country_metadata <- read_country_metadata(country_metadata_path) |>
    dplyr::transmute(
      iso3,
      country_join = normalize_country_name_for_join(.data$country)
    )

  screened_rows <- candidates |>
    dplyr::mutate(
      country = .data$oecd_country_request,
      country_join = normalize_country_name_for_join(.data$country),
      has_final_yield = !is.na(.data$yield_final_preference),
      nonstandard_issue = classify_lseg_nonstandard_issue(
        .data$DocumentTitle,
        .data$DebtTypeDescription,
        .data$InstrumentTypeDescription,
        .data$RCSCouponTypeLeaf
      ),
      economic_issue_key = build_lseg_economic_issue_key(
        .data$country,
        .data$IssueDate,
        .data$MaturityDate,
        .data$Currency,
        .data$CouponRate,
        .data$FaceIssuedUSD
      ),
      row_screen_status = dplyr::case_when(
        !.data$has_final_yield ~ "excluded_missing_yield",
        .data$nonstandard_issue ~ "excluded_nonstandard_issue",
        TRUE ~ "included_issue_candidate"
      )
    ) |>
    dplyr::left_join(ids_terms, by = c("country" = "ids_country")) |>
    dplyr::mutate(
      has_ids_bondholders = dplyr::coalesce(.data$has_ids_bondholders, FALSE),
      evidence_tier_preference = dplyr::case_when(
        .data$has_ids_bondholders ~ "tier1_ids_direct",
        TRUE ~ "tier2_lseg_core_candidate"
      )
    ) |>
    dplyr::left_join(country_metadata, by = "country_join") |>
    dplyr::select(
      "iso3",
      "country",
      "IssueDate",
      "MaturityDate",
      "Currency",
      "ISIN",
      "RIC",
      "CouponRate",
      "IssuePrice",
      "FaceIssuedUSD",
      "FaceOutstandingUSD",
      "yield_final_preference",
      "yield_source_final",
      "DebtTypeDescription",
      "InstrumentTypeDescription",
      "RCSCouponTypeLeaf",
      "DocumentTitle",
      "economic_issue_key",
      "has_ids_bondholders",
      "ids_bondholders_rate",
      "ids_bondholders_maturity_years",
      "evidence_tier_preference",
      "has_final_yield",
      "nonstandard_issue",
      "row_screen_status"
    ) |>
    dplyr::arrange(.data$country, .data$IssueDate, .data$MaturityDate, .data$ISIN)

  broader_base_audit <- broader_base |>
    dplyr::group_by(.data$country) |>
    dplyr::summarise(
      broader_base_rows_2024 = dplyr::n(),
      broader_base_usdeur_rows_2024 = sum(.data$Currency %in% c("USD", "EUR"), na.rm = TRUE),
      broader_base_usdeur_issue_count_2024 = dplyr::n_distinct(.data$economic_issue_key[.data$Currency %in% c("USD", "EUR")]),
      broader_base_candidate_rows_2024 = sum(.data$benchmark_candidate_flag %in% TRUE, na.rm = TRUE),
      broader_base_candidate_issue_count_2024 = dplyr::n_distinct(.data$economic_issue_key[.data$benchmark_candidate_flag %in% TRUE]),
      .groups = "drop"
    )

  issue_level <- screened_rows |>
    dplyr::group_by(.data$country, .data$economic_issue_key) |>
    dplyr::summarise(
      iso3 = dplyr::first(.data$iso3),
      issue_date = dplyr::first(.data$IssueDate),
      maturity_date = dplyr::first(.data$MaturityDate),
      currency = dplyr::first(.data$Currency),
      coupon_rate = dplyr::first(.data$CouponRate),
      issue_price = dplyr::first(.data$IssuePrice),
      face_issued_usd = dplyr::first(.data$FaceIssuedUSD),
      face_outstanding_usd = dplyr::first(.data$FaceOutstandingUSD),
      maturity_years = as.numeric(difftime(dplyr::first(.data$MaturityDate), dplyr::first(.data$IssueDate), units = "days")) / 365.25,
      bond_yield_pct = mean(.data$yield_final_preference, na.rm = TRUE),
      yield_source_final = paste(sort(unique(stats::na.omit(.data$yield_source_final))), collapse = ";"),
      duplicate_row_count = dplyr::n(),
      representative_isins = paste(sort(unique(stats::na.omit(.data$ISIN))), collapse = ";"),
      representative_rics = paste(sort(unique(stats::na.omit(.data$RIC))), collapse = ";"),
      representative_titles = paste(sort(unique(stats::na.omit(.data$DocumentTitle))), collapse = " || "),
      has_ids_bondholders = dplyr::first(.data$has_ids_bondholders),
      ids_bondholders_rate = dplyr::first(.data$ids_bondholders_rate),
      evidence_tier_preference = dplyr::first(.data$evidence_tier_preference),
      issue_screen_status = dplyr::case_when(
        any(.data$row_screen_status == "included_issue_candidate") ~ "included_issue",
        any(.data$row_screen_status == "excluded_nonstandard_issue") ~ "excluded_nonstandard_issue",
        TRUE ~ "excluded_missing_yield"
      ),
      .groups = "drop"
    ) |>
    dplyr::arrange(.data$country, .data$issue_date, .data$maturity_date, .data$representative_isins)

  issue_level_included <- issue_level |>
    dplyr::filter(.data$issue_screen_status == "included_issue")

  issue_level_country_flags <- issue_level_included |>
    dplyr::group_by(.data$country) |>
    dplyr::summarise(
      min_included_yield_pct = min(.data$bond_yield_pct, na.rm = TRUE),
      max_included_maturity_years = max(.data$maturity_years, na.rm = TRUE),
      suspected_restructuring_signal = any(.data$bond_yield_pct < 2 & .data$maturity_years > 10, na.rm = TRUE),
      .groups = "drop"
    )

  country_issue_summary <- issue_level |>
    dplyr::group_by(.data$country) |>
    dplyr::summarise(
      iso3 = dplyr::first(.data$iso3),
      has_ids_bondholders = dplyr::first(.data$has_ids_bondholders),
      ids_bondholders_rate = dplyr::first(.data$ids_bondholders_rate),
      issue_count_total = dplyr::n(),
      issue_count_included = sum(.data$issue_screen_status == "included_issue"),
      issue_count_excluded_nonstandard = sum(.data$issue_screen_status == "excluded_nonstandard_issue"),
      issue_count_excluded_missing_yield = sum(.data$issue_screen_status == "excluded_missing_yield"),
      duplicate_rows_collapsed = sum(.data$duplicate_row_count - 1),
      included_currency_count = dplyr::n_distinct(.data$currency[.data$issue_screen_status == "included_issue"]),
      included_weight_total_usd = sum(.data$face_issued_usd[.data$issue_screen_status == "included_issue"], na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::left_join(broader_base_audit, by = "country") |>
    dplyr::left_join(issue_level_country_flags, by = "country") |>
    dplyr::mutate(
      suspected_restructuring_signal = dplyr::coalesce(.data$suspected_restructuring_signal, FALSE),
      country_computation_status = dplyr::case_when(
        .data$issue_count_included == 0 & .data$issue_count_excluded_nonstandard > 0 ~ "deferred_nonstandard_only",
        .data$issue_count_included == 0 ~ "deferred_no_usable_issue",
        .data$suspected_restructuring_signal & .data$issue_count_excluded_nonstandard > 0 ~ "deferred_suspected_restructuring",
        .data$issue_count_excluded_nonstandard > 0 | .data$issue_count_excluded_missing_yield > 0 ~ "computed_with_review",
        TRUE ~ "computed_clean"
      ),
      country_status_reason = dplyr::case_when(
        .data$country == "Jordan" ~ "One benchmark-like issue is usable, but another 2024 issue is missing yield information, so coverage is incomplete within the country-year.",
        .data$country == "Romania" ~ "Rate is computed from many usable benchmark-like issues, but the country-year also contains short pay-at-maturity fixed bonds that were excluded as nonstandard for the PVR benchmark.",
        .data$country == "Argentina" ~ "2024 candidate rows are discount or zero-coupon style instruments rather than clean benchmark-like external market borrowing observations.",
        .data$country == "Ghana" ~ "The country-year contains restructuring-style exchange instruments; the one remaining standard-looking included issue implies an implausibly low borrowing cost for a distressed market context.",
        .data$country == "Sri Lanka" ~ "2024 candidate rows are dominated by restructuring-style step-up, index-linked, or otherwise nonstandard instruments rather than clean market benchmark issues.",
        .data$country == "Ukraine" ~ "2024 candidate rows are dominated by wartime or restructuring-style instruments, with substantial missing-yield coverage and no clean market benchmark issue set.",
        .data$country == "Zambia" ~ "The country-year contains restructuring-style instruments; the one remaining standard-looking included issue implies an implausibly low borrowing cost for a distressed market context.",
        .data$country_computation_status == "computed_clean" ~ "Included issue set looks internally consistent under the current Tier 2 screening rules.",
        .data$country_computation_status == "computed_with_review" ~ "Rate is usable for the current Tier 2 build but the country-year contains excluded or missing-yield rows that should be reviewed before paper-stage use.",
        .data$country_computation_status == "deferred_nonstandard_only" ~ "No clean benchmark-like issue remains after removing nonstandard instruments.",
        .data$country_computation_status == "deferred_no_usable_issue" ~ "No usable yield-bearing issue remains after screening.",
        .data$country_computation_status == "deferred_suspected_restructuring" ~ "Remaining included issue set appears inconsistent with ordinary market borrowing conditions.",
        TRUE ~ "Needs further review."
      ),
      recommended_next_step = dplyr::case_when(
        .data$country == "Jordan" ~ "Keep in Tier 2 for now, but inspect the missing-yield issue before promoting to final paper tables.",
        .data$country == "Romania" ~ "Keep in Tier 2 for now, but confirm that the excluded pay-at-maturity paper should stay outside the benchmark definition.",
        .data$country == "Argentina" ~ "Defer from Tier 2 and revisit later under a restructuring or approximation framework rather than a standard observed-market benchmark rule.",
        .data$country == "Ghana" ~ "Defer from Tier 2 and handle later as a restructuring-sensitive case with a bespoke rule or approximation layer.",
        .data$country == "Sri Lanka" ~ "Defer from Tier 2 and handle later as a restructuring-sensitive case with a bespoke rule or approximation layer.",
        .data$country == "Ukraine" ~ "Defer from Tier 2 and handle later as a wartime / restructuring-sensitive case with a bespoke rule or approximation layer.",
        .data$country == "Zambia" ~ "Defer from Tier 2 and handle later as a restructuring-sensitive case with a bespoke rule or approximation layer.",
        .data$country_computation_status == "computed_clean" ~ "Retain in the Tier 2 core benchmark set.",
        .data$country_computation_status == "computed_with_review" ~ "Retain provisionally, but require a short manual note before final paper use.",
        TRUE ~ "Do not use in Tier 2 headline results yet."
      ),
      observed_rate_strength = dplyr::case_when(
        .data$country_computation_status == "computed_with_review" ~ "review_needed",
        .data$country_computation_status != "computed_clean" ~ "deferred",
        .data$issue_count_included >= 3 ~ "robust_multi_issue",
        .data$issue_count_included == 2 ~ "supported_two_issue",
        .data$issue_count_included == 1 ~ "thin_single_issue",
        TRUE ~ "unknown"
      ),
      thin_country_audit_result = dplyr::case_when(
        .data$observed_rate_strength != "thin_single_issue" ~ NA_character_,
        .data$broader_base_candidate_issue_count_2024 == 1 ~ "confirmed_single_candidate_issue_in_broader_base",
        .data$broader_base_candidate_issue_count_2024 > 1 ~ "candidate_screen_may_have_missed_or_excluded_extra_issue",
        TRUE ~ "needs_manual_base_audit"
      )
    )

  country_rates <- issue_level_included |>
    dplyr::group_by(.data$country) |>
    dplyr::summarise(
      iso3 = dplyr::first(.data$iso3),
      analysis_year = 2024L,
      evidence_tier = "tier2_lseg_core_candidate",
      has_ids_bondholders = dplyr::first(.data$has_ids_bondholders),
      ids_bondholders_rate = dplyr::first(.data$ids_bondholders_rate),
      first_issue_date = min(.data$issue_date, na.rm = TRUE),
      last_issue_date = max(.data$issue_date, na.rm = TRUE),
      included_issue_count = dplyr::n(),
      included_currency_count = dplyr::n_distinct(.data$currency),
      weighting_variable = "FaceIssuedUSD",
      total_weight_usd = sum(.data$face_issued_usd, na.rm = TRUE),
      market_rate_pct = stats::weighted.mean(.data$bond_yield_pct, .data$face_issued_usd, na.rm = TRUE),
      weighted_average_maturity_years = stats::weighted.mean(.data$maturity_years, .data$face_issued_usd, na.rm = TRUE),
      included_issue_keys = paste(.data$economic_issue_key, collapse = " || "),
      included_isins = paste(.data$representative_isins, collapse = " || "),
      included_titles = paste(.data$representative_titles, collapse = " || "),
      .groups = "drop"
    ) |>
    dplyr::left_join(
      country_issue_summary |>
        dplyr::select(
          "country",
          "country_computation_status",
          "country_status_reason",
          "recommended_next_step",
          "observed_rate_strength",
          "thin_country_audit_result",
          "broader_base_candidate_issue_count_2024",
          "issue_count_total",
          "issue_count_excluded_nonstandard",
          "issue_count_excluded_missing_yield",
          "duplicate_rows_collapsed",
          "suspected_restructuring_signal"
        ),
      by = "country"
    ) |>
    dplyr::mutate(
      rate_legitimacy_band = dplyr::case_when(
        .data$observed_rate_strength == "robust_multi_issue" ~ "tier2_core_robust",
        .data$observed_rate_strength == "supported_two_issue" ~ "tier2_core_supported",
        .data$observed_rate_strength == "thin_single_issue" ~ "tier2_core_thin",
        .data$country_computation_status == "computed_with_review" ~ "tier2_core_review",
        TRUE ~ "tier2_core_deferred"
      )
    ) |>
    dplyr::filter(!grepl("^deferred_", .data$country_computation_status)) |>
    dplyr::arrange(.data$country)

  excluded_country_rates <- country_issue_summary |>
    dplyr::filter(grepl("^deferred_", .data$country_computation_status)) |>
    dplyr::select(
      "iso3",
      "country",
      "country_computation_status",
      "issue_count_total",
      "issue_count_included",
      "issue_count_excluded_nonstandard",
      "issue_count_excluded_missing_yield",
      "suspected_restructuring_signal",
      "country_status_reason",
      "recommended_next_step",
      "observed_rate_strength",
      "thin_country_audit_result",
      "broader_base_candidate_issue_count_2024"
    ) |>
    dplyr::arrange(.data$country)

  questionable_cases <- country_issue_summary |>
    dplyr::filter(.data$country_computation_status != "computed_clean") |>
    dplyr::select(
      "iso3",
      "country",
      "country_computation_status",
      "issue_count_total",
      "issue_count_included",
      "issue_count_excluded_nonstandard",
      "issue_count_excluded_missing_yield",
      "duplicate_rows_collapsed",
      "suspected_restructuring_signal",
      "country_status_reason",
      "recommended_next_step",
      "observed_rate_strength",
      "thin_country_audit_result",
      "broader_base_candidate_issue_count_2024"
    ) |>
    dplyr::arrange(.data$country)

  overlap_comparison <- country_rates |>
    dplyr::filter(.data$has_ids_bondholders) |>
    dplyr::transmute(
      iso3,
      country,
      analysis_year,
      ids_bondholders_rate,
      lseg_market_rate_pct = .data$market_rate_pct,
      rate_difference_pct_points = .data$lseg_market_rate_pct - .data$ids_bondholders_rate,
      absolute_difference_pct_points = abs(.data$lseg_market_rate_pct - .data$ids_bondholders_rate),
      included_issue_count,
      observed_rate_strength,
      country_computation_status,
      rate_legitimacy_band
    ) |>
    dplyr::arrange(.data$country)

  overlap_audit <- overlap_comparison |>
    dplyr::mutate(
      difference_magnitude_band = dplyr::case_when(
        .data$absolute_difference_pct_points < 0.25 ~ "minor_difference",
        .data$absolute_difference_pct_points < 1 ~ "moderate_difference",
        TRUE ~ "large_difference"
      ),
      likely_methodological_reason = dplyr::case_when(
        .data$country == "Kenya" ~ "LSEG uses issue-price-implied yield from a below-par 2024 bond, while IDS Bondholders is an annual country-level average commitment-rate concept that aligns more closely with coupon-level terms here.",
        .data$country == "Dominican Republic" ~ "The LSEG result reflects one observed 2024 bond issued near par, while IDS Bondholders is a country-level annual average over bondholder commitments and may capture a broader or differently weighted concept.",
        .data$country == "Paraguay" ~ "The LSEG result reflects one observed 2024 bond issued at par, while IDS Bondholders is a country-level annual average over bondholder commitments and may capture a broader or differently weighted concept.",
        .data$country == "South Africa" ~ "The LSEG result reflects two long-dated 2024 external market issues at par, while IDS Bondholders is a country-level annual average commitment-rate concept that may mix a broader set of bondholder commitments.",
        .data$country %in% c("Brazil", "Mexico") ~ "The two measures are close; the remaining gap is likely ordinary concept and weighting differences between issue-level observed yields and IDS annual average bondholder terms.",
        TRUE ~ "Likely concept and weighting differences between issue-level observed yields and IDS annual average bondholder terms."
      ),
      discrepancy_priority = dplyr::case_when(
        .data$difference_magnitude_band == "large_difference" ~ "high_priority_review",
        .data$difference_magnitude_band == "moderate_difference" ~ "medium_priority_review",
        TRUE ~ "low_priority_review"
      ),
      audit_next_step = dplyr::case_when(
        .data$discrepancy_priority == "high_priority_review" ~ "Check whether IDS is averaging across a broader 2024 bondholder commitment set than the narrow observed issue set used in Tier 2, and document the concept mismatch explicitly.",
        .data$discrepancy_priority == "medium_priority_review" ~ "Keep both rates visible and document the likely concept mismatch before paper-stage use.",
        TRUE ~ "No immediate action needed beyond preserving the comparison table."
      )
    ) |>
    dplyr::arrange(dplyr::desc(.data$absolute_difference_pct_points), .data$country)

  list(
    screened_rows = screened_rows,
    broader_base_audit = broader_base_audit,
    issue_level = issue_level,
    country_summary = country_issue_summary,
    country_rates = country_rates,
    overlap_comparison = overlap_comparison,
    overlap_audit = overlap_audit,
    excluded_country_rates = excluded_country_rates,
    questionable_cases = questionable_cases
  )
}
