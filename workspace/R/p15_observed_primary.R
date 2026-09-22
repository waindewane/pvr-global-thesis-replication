# Common P15 observed-primary processor.
#
# The processor has no authority to change the P13 baseline. In parity mode it
# reconstructs the paper-candidate/P13 issue sanitation and country aggregation from
# preserved source rows. Later P15 source improvements must call the same functions
# under a separate version and compare against the preserved parity result.

p15_primary_clean_num <- function(x) {
  suppressWarnings(as.numeric(as.character(x)))
}

p15_primary_first_non_missing <- function(x) {
  x <- x[!is.na(x) & as.character(x) != ""]
  if (!length(x)) NA else x[[1]]
}

p15_primary_collapse_values <- function(x, sep = "; ", n = Inf) {
  x <- sort(unique(stats::na.omit(as.character(x))))
  x <- x[x != ""]
  if (!length(x)) return(NA_character_)
  shown <- utils::head(x, n)
  suffix <- if (is.finite(n) && length(x) > n) {
    paste0("; ... +", length(x) - n, " more")
  } else {
    ""
  }
  paste0(paste(shown, collapse = sep), suffix)
}

p15_primary_weighted_mean_or_na <- function(x, w) {
  x <- p15_primary_clean_num(x)
  w <- p15_primary_clean_num(w)
  ok <- !is.na(x) & !is.na(w) & w > 0
  if (!any(ok)) return(NA_real_)
  stats::weighted.mean(x[ok], w[ok])
}

p15_primary_mean_or_na <- function(x) {
  x <- p15_primary_clean_num(x)
  x <- x[!is.na(x)]
  if (!length(x)) NA_real_ else mean(x)
}

p15_primary_sum_or_na <- function(x) {
  x <- p15_primary_clean_num(x)
  if (all(is.na(x))) NA_real_ else sum(x, na.rm = TRUE)
}

p15_primary_max_or_na <- function(x) {
  x <- p15_primary_clean_num(x)
  if (all(is.na(x))) NA_real_ else max(x, na.rm = TRUE)
}

p15_primary_country_join_key <- function(country) {
  if (!exists("normalize_country_name_for_join", mode = "function")) {
    stop("Source R/lseg_benchmarks.R before using the P15 primary processor.", call. = FALSE)
  }
  out <- normalize_country_name_for_join(country)
  dplyr::recode(
    out,
    "Bahamas" = "Bahamas, The",
    "Bosnia Herzegovina" = "Bosnia and Herzegovina",
    "Bosnia and Herzegovina, Fed Rep" = "Bosnia and Herzegovina",
    "Czechia" = "Czech Republic",
    "Hong Kong" = "Hong Kong SAR, China",
    "Korea South" = "Korea, Rep.",
    "Macao" = "Macao SAR, China",
    "Russia" = "Russian Federation",
    "Slovakia" = "Slovak Republic",
    "Syria" = "Syrian Arab Republic",
    "Venezuela" = "Venezuela, RB",
    .default = out
  )
}

p15_primary_country_key_from <- function(iso3, country) {
  iso3 <- as.character(iso3)
  country <- as.character(country)
  n <- max(length(iso3), length(country))
  iso3 <- rep_len(iso3, n)
  country <- rep_len(country, n)
  out <- p15_primary_country_join_key(country)
  has_iso3 <- !is.na(iso3) & iso3 != ""
  out[has_iso3] <- iso3[has_iso3]
  out
}

p15_primary_infer_coupon_frequency <- function(text) {
  text <- stringr::str_to_lower(dplyr::coalesce(as.character(text), ""))
  dplyr::case_when(
    stringr::str_detect(text, "semi|2") ~ 2L,
    stringr::str_detect(text, "quarter|4") ~ 4L,
    stringr::str_detect(text, "month|12") ~ 12L,
    TRUE ~ 1L
  )
}

p15_primary_approx_ytm_from_price <- function(
    price,
    coupon_rate_pct,
    years_to_maturity,
    freq = 1,
    face = 100) {
  if (any(is.na(c(price, coupon_rate_pct, years_to_maturity, freq)))) return(NA_real_)
  if (years_to_maturity <= 0 || price <= 0 || coupon_rate_pct <= 0 || freq <= 0) {
    return(NA_real_)
  }
  periods <- max(1L, as.integer(round(years_to_maturity * freq)))
  coupon <- face * (coupon_rate_pct / 100) / freq
  pv <- function(y) {
    r <- y / freq
    sum(coupon / ((1 + r) ^ seq_len(periods))) + face / ((1 + r) ^ periods)
  }
  low <- -0.049
  high <- 2
  mid <- NA_real_
  for (i in seq_len(200)) {
    mid <- (low + high) / 2
    pv_mid <- pv(mid)
    if (!is.finite(pv_mid)) return(NA_real_)
    if (abs(pv_mid - price) < 1e-8) return(mid * 100)
    if (pv_mid > price) low <- mid else high <- mid
  }
  mid * 100
}

p15_primary_issue_quality_from_count <- function(count, prefix) {
  dplyr::case_when(
    is.na(count) | count <= 0 ~ paste0(prefix, "_not_available"),
    count == 1 ~ paste0(prefix, "_single_issue"),
    count == 2 ~ paste0(prefix, "_two_issue"),
    count >= 3 ~ paste0(prefix, "_multi_issue"),
    TRUE ~ paste0(prefix, "_not_available")
  )
}

p15_primary_economic_issue_key <- function(
    country,
    issue_date,
    maturity_date,
    currency,
    coupon_rate,
    face_issued_usd) {
  paste(
    country,
    issue_date,
    maturity_date,
    currency,
    format(
      round(p15_primary_clean_num(coupon_rate), 6),
      trim = TRUE,
      scientific = FALSE
    ),
    format(
      round(p15_primary_clean_num(face_issued_usd), 0),
      trim = TRUE,
      scientific = FALSE
    ),
    sep = " | "
  )
}

p15_prepare_primary_country_metadata <- function(country_metadata) {
  country_metadata |>
    dplyr::filter(.data$is_country) |>
    dplyr::mutate(
      country_join = p15_primary_country_join_key(.data$country),
      country_key = p15_primary_country_key_from(.data$iso3, .data$country),
      included_in_lmic_reporting_scope = .data$income_level %in% c(
        "Low income", "Lower middle income", "Upper middle income"
      )
    )
}

p15_build_primary_issue_audit <- function(
    source_rows,
    country_metadata,
    target_year = 2024L,
    materiality_usd = 50000000,
    source_artifact,
    source_file,
    source_package_id,
    source_role = "p13_legacy_parity_input") {
  if (!exists("classify_lseg_nonstandard_issue", mode = "function")) {
    stop("Source R/lseg_benchmarks.R before using the P15 primary processor.", call. = FALSE)
  }
  required <- c(
    "IssueDate", "MaturityDate", "CouponRate", "IssuePrice",
    "MaturityStandardYield", "OriginalYieldMaturity", "FaceIssuedUSD",
    "FaceOutstandingUSD", "original_maturity_days", "canonical_country",
    "requested_country", "CouponFrequencyDescription", "DocumentTitle",
    "DebtTypeDescription", "InstrumentTypeDescription", "RCSCouponTypeLeaf",
    "CouponTypeDescription", "IssuerName", "IssuerCommonName", "Currency",
    "flag_short_bill_cp_like", "flag_central_bank_like", "is_usd_eur_scope",
    "ISIN", "RIC"
  )
  missing <- setdiff(required, names(source_rows))
  if (length(missing)) {
    stop("Primary source rows are missing: ", paste(missing, collapse = ", "), call. = FALSE)
  }

  primary_rows <- source_rows |>
    dplyr::mutate(
      IssueDate = as.Date(.data$IssueDate),
      MaturityDate = as.Date(.data$MaturityDate),
      analysis_year = as.integer(format(.data$IssueDate, "%Y")),
      CouponRate = p15_primary_clean_num(.data$CouponRate),
      IssuePrice = p15_primary_clean_num(.data$IssuePrice),
      MaturityStandardYield = p15_primary_clean_num(.data$MaturityStandardYield),
      OriginalYieldMaturity = p15_primary_clean_num(.data$OriginalYieldMaturity),
      FaceIssuedUSD = p15_primary_clean_num(.data$FaceIssuedUSD),
      FaceOutstandingUSD = p15_primary_clean_num(.data$FaceOutstandingUSD),
      original_maturity_days = p15_primary_clean_num(.data$original_maturity_days),
      country = dplyr::coalesce(.data$canonical_country, .data$requested_country),
      country_join = p15_primary_country_join_key(.data$country),
      coupon_frequency_inferred = p15_primary_infer_coupon_frequency(
        .data$CouponFrequencyDescription
      ),
      title_text = paste(
        dplyr::coalesce(.data$DocumentTitle, ""),
        dplyr::coalesce(.data$DebtTypeDescription, ""),
        dplyr::coalesce(.data$InstrumentTypeDescription, ""),
        dplyr::coalesce(.data$RCSCouponTypeLeaf, ""),
        dplyr::coalesce(.data$CouponTypeDescription, ""),
        sep = " | "
      ),
      title_lower = stringr::str_to_lower(.data$title_text),
      issuer_lower = stringr::str_to_lower(paste(
        dplyr::coalesce(.data$IssuerName, ""),
        dplyr::coalesce(.data$IssuerCommonName, ""),
        sep = " | "
      )),
      plain_vanilla_fixed_coupon = stringr::str_detect(
        dplyr::coalesce(.data$RCSCouponTypeLeaf, ""),
        stringr::regex("Plain Vanilla Fixed Coupon", ignore_case = TRUE)
      ),
      flag_hard_currency = .data$Currency %in% c("USD", "EUR"),
      flag_original_maturity_ok = !is.na(.data$original_maturity_days) &
        .data$original_maturity_days >= 365,
      flag_bill_cp_like = dplyr::coalesce(as.logical(.data$flag_short_bill_cp_like), FALSE) |
        stringr::str_detect(
          .data$title_lower,
          "bill|treasury bill|commercial paper|letra|strip|depositary note"
        ),
      flag_central_bank_like = dplyr::coalesce(
        as.logical(.data$flag_central_bank_like), FALSE
      ) | stringr::str_detect(.data$issuer_lower, "central bank|monetary authority"),
      flag_restructuring_like = stringr::str_detect(
        .data$title_lower,
        "exchange offer|debt exchange|restructur|swap|settlement|sanction|consent|haircut|reprofil|defaulted|distressed"
      ),
      flag_special_structure = classify_lseg_nonstandard_issue(
        .data$DocumentTitle,
        .data$DebtTypeDescription,
        .data$InstrumentTypeDescription,
        .data$RCSCouponTypeLeaf
      ) | stringr::str_detect(
        .data$title_lower,
        "zero|discount|step up|step down|pay at maturity|variable|floating|index|multiple payment|fixed margin|structured|sukuk|linked"
      ),
      nonstandard_issue = .data$flag_restructuring_like | .data$flag_special_structure,
      price_implied_allowed = .data$plain_vanilla_fixed_coupon &
        !.data$nonstandard_issue & !.data$flag_bill_cp_like &
        !.data$flag_central_bank_like & !is.na(.data$IssuePrice) &
        .data$IssuePrice > 0 & .data$IssuePrice < 200 &
        !is.na(.data$CouponRate) & .data$CouponRate > 0,
      yield_price_implied = dplyr::if_else(
        .data$price_implied_allowed,
        purrr::pmap_dbl(
          list(
            .data$IssuePrice,
            .data$CouponRate,
            .data$original_maturity_days / 365.25,
            .data$coupon_frequency_inferred
          ),
          p15_primary_approx_ytm_from_price
        ),
        NA_real_
      ),
      yield_final_preference = dplyr::case_when(
        !is.na(.data$OriginalYieldMaturity) ~ .data$OriginalYieldMaturity,
        !is.na(.data$yield_price_implied) ~ .data$yield_price_implied,
        TRUE ~ NA_real_
      ),
      yield_source_final = dplyr::case_when(
        !is.na(.data$OriginalYieldMaturity) ~ "OriginalYieldMaturity",
        !is.na(.data$yield_price_implied) ~ "price_implied_plain_vanilla_fixed_coupon",
        TRUE ~ NA_character_
      ),
      economic_issue_key = p15_primary_economic_issue_key(
        .data$country,
        .data$IssueDate,
        .data$MaturityDate,
        .data$Currency,
        .data$CouponRate,
        .data$FaceIssuedUSD
      ),
      lseg_candidate_status = dplyr::case_when(
        dplyr::coalesce(as.logical(.data$is_usd_eur_scope), FALSE) &
          !.data$flag_bill_cp_like & !.data$flag_central_bank_like ~
          "lseg_context_candidate",
        TRUE ~ "lseg_context_not_candidate"
      ),
      row_exclusion_reason = dplyr::case_when(
        !.data$flag_hard_currency ~ "excluded_not_usd_eur",
        !.data$flag_original_maturity_ok ~ "excluded_short_original_maturity",
        .data$flag_bill_cp_like ~ "excluded_bill_or_commercial_paper_like",
        .data$flag_central_bank_like ~ "excluded_central_bank_or_non_budget_like",
        .data$flag_restructuring_like ~
          "excluded_restructuring_exchange_swap_settlement_like",
        .data$flag_special_structure ~ "excluded_special_structure",
        is.na(.data$yield_final_preference) ~ "excluded_missing_selected_yield",
        .data$yield_final_preference <= 0 ~ "excluded_nonpositive_yield",
        .data$yield_final_preference < 1 ~ "excluded_below_one_percent_yield",
        .data$yield_final_preference > 30 ~ "excluded_above_thirty_percent_yield",
        is.na(.data$FaceIssuedUSD) | .data$FaceIssuedUSD <= 0 ~
          "excluded_missing_or_nonpositive_face_issued_usd",
        TRUE ~ "passes_hard_screens"
      ),
      row_hard_exclusion = .data$row_exclusion_reason != "passes_hard_screens",
      row_materiality_pass = !is.na(.data$FaceIssuedUSD) &
        .data$FaceIssuedUSD >= materiality_usd
    ) |>
    dplyr::filter(.data$analysis_year == target_year) |>
    dplyr::left_join(
      country_metadata |>
        dplyr::select(
          "iso3", "country_join", "income_level", "lending_type",
          "included_in_lmic_reporting_scope"
        ),
      by = "country_join"
    ) |>
    dplyr::mutate(country_key = p15_primary_country_key_from(.data$iso3, .data$country))

  primary_rows |>
    dplyr::group_by(
      .data$analysis_year, .data$country, .data$country_key, .data$economic_issue_key
    ) |>
    dplyr::summarise(
      iso3 = p15_primary_first_non_missing(.data$iso3),
      income_level = p15_primary_first_non_missing(.data$income_level),
      lending_type = p15_primary_first_non_missing(.data$lending_type),
      included_in_lmic_reporting_scope = p15_primary_first_non_missing(
        .data$included_in_lmic_reporting_scope
      ),
      issue_date = p15_primary_first_non_missing(.data$IssueDate),
      maturity_date = p15_primary_first_non_missing(.data$MaturityDate),
      currency = p15_primary_first_non_missing(.data$Currency),
      coupon_rate = p15_primary_mean_or_na(.data$CouponRate),
      issue_price = p15_primary_mean_or_na(.data$IssuePrice),
      face_issued_usd = p15_primary_max_or_na(.data$FaceIssuedUSD),
      face_outstanding_usd = p15_primary_max_or_na(.data$FaceOutstandingUSD),
      original_maturity_days = p15_primary_mean_or_na(.data$original_maturity_days),
      market_maturity_years = p15_primary_mean_or_na(
        as.numeric(difftime(.data$MaturityDate, .data$IssueDate, units = "days")) / 365.25
      ),
      market_rate_pct = p15_primary_mean_or_na(.data$yield_final_preference),
      original_yield_maturity = p15_primary_mean_or_na(.data$OriginalYieldMaturity),
      price_implied_yield = p15_primary_mean_or_na(.data$yield_price_implied),
      maturity_standard_yield_diagnostic = p15_primary_mean_or_na(
        .data$MaturityStandardYield
      ),
      yield_source = p15_primary_collapse_values(.data$yield_source_final),
      duplicate_row_count = dplyr::n(),
      raw_status_conflict = dplyr::n_distinct(.data$lseg_candidate_status) > 1,
      raw_status_values = p15_primary_collapse_values(.data$lseg_candidate_status),
      any_hard_exclusion = any(.data$row_hard_exclusion, na.rm = TRUE),
      hard_exclusion_reasons = p15_primary_collapse_values(
        .data$row_exclusion_reason[.data$row_exclusion_reason != "passes_hard_screens"]
      ),
      materiality_pass = any(.data$row_materiality_pass, na.rm = TRUE),
      selected_primary_issue = !any(.data$row_hard_exclusion, na.rm = TRUE) &
        any(.data$row_materiality_pass, na.rm = TRUE),
      below_materiality_only = !any(.data$row_hard_exclusion, na.rm = TRUE) &
        !any(.data$row_materiality_pass, na.rm = TRUE),
      has_restructuring_like_signal = any(.data$flag_restructuring_like, na.rm = TRUE),
      has_special_structure_signal = any(.data$flag_special_structure, na.rm = TRUE),
      has_below_one_yield_signal = any(
        !is.na(.data$yield_final_preference) & .data$yield_final_preference > 0 &
          .data$yield_final_preference < 1,
        na.rm = TRUE
      ),
      has_nonpositive_yield_signal = any(
        !is.na(.data$yield_final_preference) & .data$yield_final_preference <= 0,
        na.rm = TRUE
      ),
      has_above_thirty_yield_signal = any(
        !is.na(.data$yield_final_preference) & .data$yield_final_preference > 30,
        na.rm = TRUE
      ),
      has_missing_selected_yield_signal = any(
        is.na(.data$yield_final_preference), na.rm = TRUE
      ),
      representative_isins = p15_primary_collapse_values(.data$ISIN),
      representative_rics = p15_primary_collapse_values(.data$RIC),
      representative_titles = p15_primary_collapse_values(
        .data$DocumentTitle, sep = " || ", n = 5
      ),
      source_artifact = source_artifact,
      source_file = source_file,
      source_package_id = source_package_id,
      source_role = source_role,
      .groups = "drop"
    ) |>
    dplyr::mutate(
      issue_audit_status = dplyr::case_when(
        .data$selected_primary_issue & .data$raw_status_conflict ~
          "included_material_issue_with_raw_status_conflict_flag",
        .data$selected_primary_issue ~ "included_material_issue",
        .data$below_materiality_only ~ "excluded_below_usd_50m_materiality",
        .data$any_hard_exclusion ~ "excluded_hard_screen",
        TRUE ~ "excluded_unclassified"
      ),
      issue_audit_reason = dplyr::case_when(
        .data$selected_primary_issue & .data$raw_status_conflict ~
          paste(
            "Issue passes independent hard screens and USD 50m materiality,",
            "but duplicate raw rows disagree on LSEG context candidate status."
          ),
        .data$selected_primary_issue ~
          "Issue passes independent hard screens and USD 50m materiality.",
        .data$below_materiality_only ~
          "Issue passes hard screens but is below the USD 50m paper-candidate materiality screen.",
        TRUE ~ dplyr::coalesce(
          .data$hard_exclusion_reasons,
          "Issue did not pass the paper-candidate primary screen."
        )
      )
    ) |>
    dplyr::arrange(
      .data$country, .data$issue_date, .data$maturity_date, .data$economic_issue_key
    )
}

p15_build_primary_country_evidence <- function(
    primary_issue_audit,
    target_year = 2024L,
    known_distress_context = c(
      "Argentina", "Bolivia", "Ecuador", "Ethiopia", "Ghana", "Lebanon",
      "Sri Lanka", "Ukraine", "Venezuela", "Zambia"
    ),
    ladder_variant = "p15_p13_legacy_parity_2024",
    source_extraction_run_id,
    source_license_class = "LSEG_workspace_export_restricted") {
  flags <- primary_issue_audit |>
    dplyr::group_by(.data$analysis_year, .data$country, .data$country_key) |>
    dplyr::summarise(
      iso3 = p15_primary_first_non_missing(.data$iso3),
      primary_raw_issue_count = dplyr::n(),
      primary_material_selected_issue_count = sum(.data$selected_primary_issue, na.rm = TRUE),
      primary_total_eligible_issue_count = sum(!.data$any_hard_exclusion, na.rm = TRUE),
      primary_below_materiality_issue_count = sum(.data$below_materiality_only, na.rm = TRUE),
      primary_hard_excluded_issue_count = sum(.data$any_hard_exclusion, na.rm = TRUE),
      primary_raw_status_conflict_issue_count = sum(.data$raw_status_conflict, na.rm = TRUE),
      primary_restructuring_like_issue_count = sum(
        .data$has_restructuring_like_signal, na.rm = TRUE
      ),
      primary_special_structure_issue_count = sum(
        .data$has_special_structure_signal, na.rm = TRUE
      ),
      primary_below_one_issue_count = sum(.data$has_below_one_yield_signal, na.rm = TRUE),
      primary_nonpositive_issue_count = sum(.data$has_nonpositive_yield_signal, na.rm = TRUE),
      primary_above_thirty_issue_count = sum(
        .data$has_above_thirty_yield_signal, na.rm = TRUE
      ),
      primary_missing_yield_issue_count = sum(
        .data$has_missing_selected_yield_signal, na.rm = TRUE
      ),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      primary_distress_or_restructuring_signal =
        .data$primary_restructuring_like_issue_count > 0 |
        .data$primary_above_thirty_issue_count > 0 |
        (
          .data$country %in% known_distress_context &
            (
              .data$primary_special_structure_issue_count > 0 |
                .data$primary_below_one_issue_count > 0 |
                .data$primary_nonpositive_issue_count > 0 |
                .data$primary_missing_yield_issue_count > 0
            )
        ),
      primary_data_quality_signal = .data$primary_below_one_issue_count > 0 |
        .data$primary_nonpositive_issue_count > 0 |
        .data$primary_missing_yield_issue_count > 0 |
        .data$primary_raw_status_conflict_issue_count > 0
    )

  primary_issue_audit |>
    dplyr::filter(.data$selected_primary_issue) |>
    dplyr::group_by(.data$analysis_year, .data$country, .data$country_key) |>
    dplyr::summarise(
      iso3 = p15_primary_first_non_missing(.data$iso3),
      income_level = p15_primary_first_non_missing(.data$income_level),
      lending_type = p15_primary_first_non_missing(.data$lending_type),
      included_in_lmic_reporting_scope = p15_primary_first_non_missing(
        .data$included_in_lmic_reporting_scope
      ),
      ladder_variant = ladder_variant,
      ladder_priority = 10L,
      scenario_id = "primary_usd_eur_material_issue_clean_v1",
      ladder_component = "observed_primary_issuance",
      benchmark_source_tier = "observed_primary_issuance",
      method_family = "primary_market",
      market_rate_pct = p15_primary_weighted_mean_or_na(
        .data$market_rate_pct, .data$face_issued_usd
      ),
      market_maturity_years = p15_primary_weighted_mean_or_na(
        .data$market_maturity_years, .data$face_issued_usd
      ),
      benchmark_quality_band = p15_primary_issue_quality_from_count(
        dplyr::n(), "primary_material"
      ),
      benchmark_status = dplyr::case_when(
        dplyr::n() == 1 ~ "diagnostic_thin_single_issue",
        dplyr::n() == 2 ~ "computed_two_material_issue",
        dplyr::n() >= 3 ~ "computed_multi_material_issue",
        TRUE ~ "not_computed"
      ),
      review_category = dplyr::if_else(
        dplyr::n() == 1, "diagnostic_thin_single_issue", "selected_clean"
      ),
      market_rate_measure_basis = paste(
        "issue-amount-weighted primary issue yield; OriginalYieldMaturity first;",
        "constrained plain-vanilla issue-price-implied fallback;",
        "MaturityStandardYield diagnostic only"
      ),
      currency_basis = dplyr::if_else(
        dplyr::n_distinct(.data$currency) > 1,
        "mixed_usd_eur",
        p15_primary_first_non_missing(.data$currency)
      ),
      weighting_variable = "FaceIssuedUSD",
      total_weight_usd = p15_primary_sum_or_na(.data$face_issued_usd),
      issue_count = as.integer(dplyr::n()),
      total_eligible_issue_count = as.integer(dplyr::n()),
      material_eligible_issue_count = as.integer(dplyr::n()),
      yield_source = p15_primary_collapse_values(.data$yield_source),
      included_issue_keys = p15_primary_collapse_values(
        .data$economic_issue_key, sep = " || "
      ),
      included_isins = p15_primary_collapse_values(
        .data$representative_isins, sep = " || "
      ),
      first_issue_date = min(.data$issue_date, na.rm = TRUE),
      last_issue_date = max(.data$issue_date, na.rm = TRUE),
      source_artifact = p15_primary_first_non_missing(.data$source_artifact),
      source_file = p15_primary_first_non_missing(.data$source_file),
      source_package_id = p15_primary_first_non_missing(.data$source_package_id),
      source_role = p15_primary_first_non_missing(.data$source_role),
      source_url_or_path = p15_primary_first_non_missing(.data$source_artifact),
      source_extraction_run_id = source_extraction_run_id,
      source_license_class = source_license_class,
      source_note = paste(
        "Paper-candidate 2024 primary aggregation after issue-level hard screens,",
        "duplicate collapse, and USD 50m materiality."
      ),
      selected_pvr_admissible = dplyr::n() >= 2,
      manual_review_required = dplyr::n() == 1,
      distress_or_market_access_signal = FALSE,
      .groups = "drop"
    ) |>
    dplyr::left_join(
      flags,
      by = c("analysis_year", "country", "country_key", "iso3")
    ) |>
    dplyr::mutate(
      total_eligible_issue_count = .data$primary_total_eligible_issue_count,
      material_eligible_issue_count = .data$issue_count,
      raw_status_conflict = .data$primary_raw_status_conflict_issue_count > 0,
      selected_pvr_admissible = .data$selected_pvr_admissible &
        !.data$primary_distress_or_restructuring_signal,
      manual_review_required = .data$manual_review_required |
        .data$primary_distress_or_restructuring_signal |
        .data$primary_data_quality_signal,
      distress_or_market_access_signal = .data$primary_distress_or_restructuring_signal,
      review_category = dplyr::case_when(
        .data$primary_distress_or_restructuring_signal ~
          "review_pending_distress_or_restructuring",
        .data$issue_count == 1 ~ "diagnostic_thin_single_issue",
        TRUE ~ "selected_clean"
      ),
      sanitation_class = dplyr::case_when(
        .data$primary_distress_or_restructuring_signal ~
          "review_pending_distress_or_restructuring",
        .data$issue_count == 1 ~ "diagnostic_thin_single_issue",
        TRUE ~ "pvr_admissible_candidate"
      ),
      sanitation_action = dplyr::case_when(
        .data$primary_distress_or_restructuring_signal ~
          "block_from_selected_pvr_pending_distress_review",
        .data$issue_count == 1 ~ "retain_as_diagnostic_not_pvr_admissible",
        TRUE ~ "retain_for_selection"
      ),
      sanitation_reason = dplyr::case_when(
        .data$primary_distress_or_restructuring_signal ~ paste(
          "Country-year contains restructuring-like or above-30 percent raw primary",
          "signals; do not let a cleaner-looking subset drive PVR without review."
        ),
        .data$issue_count == 1 ~ paste(
          "Only one USD 50m material clean primary issue remains; retained as",
          "diagnostic thin evidence by default."
        ),
        TRUE ~ paste(
          "Primary evidence passed paper-candidate issue-level sanitation and",
          "materiality screens."
        )
      ),
      parity_specification_id = "SPEC-P15-P13-LEGACY-PARITY-V1"
    ) |>
    dplyr::filter(.data$analysis_year == target_year)
}
