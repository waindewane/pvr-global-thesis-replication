# Common all-years observed-market evidence for P15.
#
# These functions normalize primary and secondary evidence and create controlled
# candidate variants. They deliberately stop before final admissibility or selection:
# status, sanity, repair, and ladder-order decisions remain separate gates.

p15_observed_all_years_schema_version <- function() {
  "SCHEMA-P15-OBSERVED-MARKET-EVIDENCE-V1"
}

p15_observed_assert_columns <- function(data, required, object_name) {
  missing <- setdiff(required, names(data))
  if (length(missing)) {
    stop(object_name, " is missing: ", paste(missing, collapse = ", "),
         call. = FALSE)
  }
  invisible(TRUE)
}

p15_observed_num <- function(x) suppressWarnings(as.numeric(as.character(x)))

p15_observed_date <- function(x) suppressWarnings(as.Date(x))

p15_observed_truthy <- function(x) {
  if (is.logical(x)) return(!is.na(x) & x)
  tolower(trimws(as.character(x))) %in% c("true", "t", "1", "yes")
}

p15_observed_collapse <- function(x, sep = ";") {
  values <- sort(unique(as.character(x[!is.na(x) & nzchar(as.character(x))])))
  if (!length(values)) return(NA_character_)
  paste(values, collapse = sep)
}

p15_observed_first <- function(x) {
  keep <- !is.na(x) & nzchar(as.character(x))
  if (!any(keep)) return(x[NA_integer_][1])
  x[which(keep)[[1]]]
}

p15_observed_weighted_mean <- function(x, w) {
  x <- p15_observed_num(x)
  w <- p15_observed_num(w)
  keep <- is.finite(x) & is.finite(w) & w > 0
  if (!any(keep)) return(NA_real_)
  stats::weighted.mean(x[keep], w[keep])
}

p15_observed_max_or_na <- function(x) {
  x <- p15_observed_num(x)
  if (!any(is.finite(x))) return(NA_real_)
  max(x, na.rm = TRUE)
}

p15_observed_min_or_na <- function(x) {
  x <- p15_observed_num(x)
  if (!any(is.finite(x))) return(NA_real_)
  min(x, na.rm = TRUE)
}

p15_observed_median_or_na <- function(x) {
  x <- p15_observed_num(x)
  if (!any(is.finite(x))) return(NA_real_)
  stats::median(x, na.rm = TRUE)
}

p15_observed_largest_weight_share <- function(w) {
  w <- p15_observed_num(w)
  w <- w[is.finite(w) & w > 0]
  if (!length(w)) return(NA_real_)
  max(w) / sum(w)
}

p15_observed_last_date <- function(x) {
  x <- p15_observed_date(x)
  x <- x[!is.na(x)]
  if (!length(x)) return(as.Date(NA))
  max(x)
}

p15_observed_issue_key <- function(
    iso3, issue_date, maturity_date, currency, coupon_rate, face_issued_usd) {
  paste(
    iso3,
    as.character(issue_date),
    as.character(maturity_date),
    currency,
    format(round(p15_observed_num(coupon_rate), 6), trim = TRUE,
           scientific = FALSE),
    format(round(p15_observed_num(face_issued_usd), 0), trim = TRUE,
           scientific = FALSE),
    sep = "|"
  )
}

p15_observed_period <- function(year) {
  dplyr::case_when(
    year <= 2015L ~ "2012-2015",
    year <= 2019L ~ "2016-2019",
    year <= 2023L ~ "2020-2023",
    TRUE ~ "2024"
  )
}

p15_primary_static_by_isin <- function(instrument_universe) {
  required <- c(
    "snapshot_year", "issue_year", "canonical_iso3", "ISIN", "RIC",
    "IssueDate", "MaturityDate", "Currency", "CouponRate", "FaceIssuedUSD",
    "FaceOutstandingUSD", "AssetStatus", "IssuerName", "DocumentTitle",
    "CouponFrequencyDescription", "RCSCouponTypeLeaf",
    "CouponTypeDescription", "DebtTypeDescription",
    "InstrumentTypeDescription", "IsCallable", "IsPutable", "IsSinkable",
    "IsConvertible", "flag_short_bill_cp_like", "flag_central_bank_like",
    "p15_history_source_package_id", "p15_source_record_locator"
  )
  p15_observed_assert_columns(
    instrument_universe, required, "P15 instrument-year universe"
  )

  instrument_universe |>
    dplyr::filter(
      .data$snapshot_year == .data$issue_year,
      !is.na(.data$ISIN), nzchar(.data$ISIN)
    ) |>
    dplyr::mutate(
      analysis_year = as.integer(.data$snapshot_year),
      issue_date_static = p15_observed_date(.data$IssueDate),
      maturity_date_static = p15_observed_date(.data$MaturityDate),
      static_nonstandard_flag =
        p15_observed_truthy(.data$IsCallable) |
        p15_observed_truthy(.data$IsPutable) |
        p15_observed_truthy(.data$IsSinkable) |
        p15_observed_truthy(.data$IsConvertible) |
        stringr::str_detect(
          tolower(paste(
            dplyr::coalesce(as.character(.data$RCSCouponTypeLeaf), ""),
            dplyr::coalesce(as.character(.data$CouponTypeDescription), ""),
            dplyr::coalesce(as.character(.data$DebtTypeDescription), ""),
            dplyr::coalesce(as.character(.data$InstrumentTypeDescription), ""),
            dplyr::coalesce(as.character(.data$DocumentTitle), "")
          )),
          "call|put|sink|convert|floating|variable|zero|strip|step|index|sukuk|linked"
        )
    ) |>
    dplyr::group_by(.data$analysis_year, .data$canonical_iso3, .data$ISIN) |>
    dplyr::summarise(
      static_ric = p15_observed_collapse(.data$RIC),
      issue_date_static = p15_observed_first(.data$issue_date_static),
      maturity_date_static = p15_observed_first(.data$maturity_date_static),
      currency_static = p15_observed_first(.data$Currency),
      coupon_rate_static = p15_observed_median_or_na(.data$CouponRate),
      face_issued_usd_static = p15_observed_max_or_na(.data$FaceIssuedUSD),
      face_outstanding_usd_static = p15_observed_max_or_na(
        .data$FaceOutstandingUSD
      ),
      asset_status_static = p15_observed_collapse(.data$AssetStatus),
      issuer_name_static = p15_observed_first(.data$IssuerName),
      document_title_static = p15_observed_first(.data$DocumentTitle),
      coupon_frequency_description_static = p15_observed_first(
        .data$CouponFrequencyDescription
      ),
      coupon_type_leaf_static = p15_observed_first(.data$RCSCouponTypeLeaf),
      coupon_type_description_static = p15_observed_first(
        .data$CouponTypeDescription
      ),
      flag_short_bill_cp_like_static = any(
        p15_observed_truthy(.data$flag_short_bill_cp_like)
      ),
      flag_central_bank_like_static = any(
        p15_observed_truthy(.data$flag_central_bank_like)
      ),
      static_nonstandard_flag = any(.data$static_nonstandard_flag),
      static_source_package_ids = p15_observed_collapse(
        .data$p15_history_source_package_id
      ),
      static_source_record_locators = p15_observed_collapse(
        .data$p15_source_record_locator
      ),
      static_match_row_count = dplyr::n(),
      .groups = "drop"
    )
}

p15_build_primary_issue_evidence_all_years <- function(
    primary_terms, identifier_context, instrument_universe, country_grid) {
  p15_observed_assert_columns(
    primary_terms,
    c(
      "request_identifier", "Instrument", "Issue Date",
      "First Announcement Date", "Original Yield Maturity", "Issue Price",
      "Original Amount Issued", "Face Issued Total",
      "Original Issue Currency", "Currency", "Maturity Date",
      "source_archive_member", "source_member_row"
    ),
    "LSEG primary-issue terms"
  )
  p15_observed_assert_columns(
    identifier_context,
    c(
      "desktop_identifier", "desktop_identifier_type", "ISIN", "RIC",
      "canonical_iso3", "canonical_country", "requested_country",
      "IssueDate", "MaturityDate", "DocumentTitle"
    ),
    "LSEG desktop identifier context"
  )
  p15_observed_assert_columns(
    country_grid,
    c(
      "analysis_year", "iso3", "country", "historical_income_level",
      "historical_lmic_reporting_scope", "country_year_id", "period"
    ),
    "P15 country-year grid"
  )

  static <- p15_primary_static_by_isin(instrument_universe)
  context <- identifier_context |>
    dplyr::transmute(
      request_identifier = as.character(.data$desktop_identifier),
      identifier_type = as.character(.data$desktop_identifier_type),
      context_isin = as.character(.data$ISIN),
      context_ric = as.character(.data$RIC),
      iso3 = as.character(.data$canonical_iso3),
      context_country = dplyr::coalesce(
        as.character(.data$canonical_country),
        as.character(.data$requested_country)
      ),
      context_issue_date = p15_observed_date(.data$IssueDate),
      context_maturity_date = p15_observed_date(.data$MaturityDate),
      context_document_title = as.character(.data$DocumentTitle)
    ) |>
    dplyr::distinct(.data$request_identifier, .keep_all = TRUE)

  normalized <- primary_terms |>
    dplyr::mutate(
      request_identifier = as.character(.data$request_identifier),
      instrument = as.character(.data$Instrument),
      issue_date = p15_observed_date(.data$`Issue Date`),
      first_announcement_date = p15_observed_date(
        .data$`First Announcement Date`
      ),
      maturity_date = p15_observed_date(.data$`Maturity Date`),
      original_yield_maturity_pct = p15_observed_num(
        .data$`Original Yield Maturity`
      ),
      issue_price = p15_observed_num(.data$`Issue Price`),
      original_amount_issued = p15_observed_num(
        .data$`Original Amount Issued`
      ),
      face_issued_total = p15_observed_num(.data$`Face Issued Total`),
      original_issue_currency = as.character(.data$`Original Issue Currency`),
      currency = dplyr::coalesce(
        as.character(.data$Currency), .data$original_issue_currency
      ),
      analysis_year = as.integer(format(.data$issue_date, "%Y"))
    ) |>
    dplyr::filter(.data$analysis_year %in% 2012:2024) |>
    dplyr::left_join(context, by = "request_identifier") |>
    dplyr::left_join(
      static,
      by = c(
        "analysis_year", "iso3" = "canonical_iso3",
        "context_isin" = "ISIN"
      )
    ) |>
    dplyr::left_join(
      country_grid |>
        dplyr::select(
          "analysis_year", "iso3", grid_country = "country",
          "historical_income_level", "historical_lmic_reporting_scope",
          "country_year_id", "period"
        ),
      by = c("analysis_year", "iso3")
    ) |>
    dplyr::mutate(
      country = dplyr::coalesce(.data$grid_country, .data$context_country),
      isin = dplyr::coalesce(.data$context_isin, dplyr::if_else(
        .data$identifier_type == "ISIN", .data$request_identifier, NA_character_
      )),
      ric = dplyr::coalesce(.data$context_ric, .data$static_ric),
      issue_date = dplyr::coalesce(.data$issue_date, .data$context_issue_date,
                                   .data$issue_date_static),
      maturity_date = dplyr::coalesce(
        .data$maturity_date, .data$context_maturity_date,
        .data$maturity_date_static
      ),
      currency = dplyr::coalesce(.data$currency, .data$currency_static),
      original_maturity_years = as.numeric(
        .data$maturity_date - .data$issue_date
      ) / 365.25,
      direct_original_yield_available = is.finite(
        .data$original_yield_maturity_pct
      ),
      hard_currency_usd_eur = .data$currency %in% c("USD", "EUR"),
      source_currency_weight_value = dplyr::coalesce(
        .data$original_amount_issued, .data$face_issued_total
      ),
      face_issued_usd = .data$face_issued_usd_static,
      aggregation_weight_value = dplyr::coalesce(
        .data$face_issued_usd, .data$source_currency_weight_value
      ),
      aggregation_weight_basis = dplyr::case_when(
        is.finite(.data$face_issued_usd) & .data$face_issued_usd > 0 ~
          "face_issued_usd",
        is.finite(.data$source_currency_weight_value) &
          .data$source_currency_weight_value > 0 ~
          "source_currency_amount_fallback_not_cross_currency_comparable",
        TRUE ~ "missing_positive_weight"
      ),
      title_text = tolower(paste(
        dplyr::coalesce(.data$context_document_title, ""),
        dplyr::coalesce(.data$document_title_static, "")
      )),
      bill_or_strip_flag = dplyr::coalesce(
        .data$flag_short_bill_cp_like_static, FALSE
      ) | stringr::str_detect(
        .data$title_text,
        "treasury bill|commercial paper|strip|interest only|depositary note"
      ),
      central_bank_flag = dplyr::coalesce(
        .data$flag_central_bank_like_static, FALSE
      ),
      restructuring_flag = stringr::str_detect(
        .data$title_text,
        "exchange offer|debt exchange|restructur|swap|haircut|reprofil|defaulted"
      ),
      nonstandard_feature_flag = dplyr::coalesce(
        .data$static_nonstandard_flag, FALSE
      ) | .data$bill_or_strip_flag | .data$restructuring_flag,
      source_object_standard = .data$direct_original_yield_available &
        .data$hard_currency_usd_eur &
        is.finite(.data$original_maturity_years) &
        .data$original_maturity_years >= 1 &
        !.data$bill_or_strip_flag & !.data$central_bank_flag &
        !.data$nonstandard_feature_flag,
      usd_weight_available = is.finite(.data$face_issued_usd) &
        .data$face_issued_usd > 0,
      positive_any_weight_available = is.finite(.data$aggregation_weight_value) &
        .data$aggregation_weight_value > 0,
      legacy_1_30_screen_pass = .data$direct_original_yield_available &
        .data$original_yield_maturity_pct >= 1 &
        .data$original_yield_maturity_pct <= 30,
      source_package_id = "SRC-LSEG-EXT-20260623-R4",
      source_record_locator = paste0(
        .data$source_archive_member, "::", .data$source_member_row
      ),
      source_object = "lseg_original_issue_yield",
      schema_version = p15_observed_all_years_schema_version()
    )

  normalized |>
    dplyr::filter(!is.na(.data$iso3), !is.na(.data$issue_date)) |>
    dplyr::mutate(
      economic_issue_key = p15_observed_issue_key(
        .data$iso3, .data$issue_date, .data$maturity_date, .data$currency,
        .data$coupon_rate_static, .data$face_issued_usd
      )
    ) |>
    dplyr::group_by(
      .data$analysis_year, .data$iso3, .data$country, .data$country_year_id,
      .data$period, .data$historical_income_level,
      .data$historical_lmic_reporting_scope, .data$economic_issue_key
    ) |>
    dplyr::summarise(
      issue_date = p15_observed_first(.data$issue_date),
      first_announcement_date = p15_observed_first(
        .data$first_announcement_date
      ),
      maturity_date = p15_observed_first(.data$maturity_date),
      currency = p15_observed_first(.data$currency),
      coupon_rate_pct = p15_observed_median_or_na(.data$coupon_rate_static),
      original_maturity_years = p15_observed_median_or_na(
        .data$original_maturity_years
      ),
      original_issue_yield_pct = p15_observed_median_or_na(
        .data$original_yield_maturity_pct
      ),
      issue_price = p15_observed_median_or_na(.data$issue_price),
      face_issued_usd = p15_observed_max_or_na(.data$face_issued_usd),
      aggregation_weight_value = p15_observed_max_or_na(
        .data$aggregation_weight_value
      ),
      aggregation_weight_basis = p15_observed_collapse(
        .data$aggregation_weight_basis
      ),
      request_identifier_count = dplyr::n_distinct(.data$request_identifier),
      request_identifiers = p15_observed_collapse(.data$request_identifier),
      representative_isins = p15_observed_collapse(.data$isin),
      representative_rics = p15_observed_collapse(.data$ric),
      direct_yield_value_count = sum(
        is.finite(.data$original_yield_maturity_pct)
      ),
      direct_yield_range_bps = (
        p15_observed_max_or_na(.data$original_yield_maturity_pct) -
          p15_observed_min_or_na(.data$original_yield_maturity_pct)
      ) * 100,
      source_object_standard = any(.data$source_object_standard),
      usd_weight_available = any(.data$usd_weight_available),
      positive_any_weight_available = any(
        .data$positive_any_weight_available
      ),
      nonstandard_feature_flag = any(.data$nonstandard_feature_flag),
      bill_or_strip_flag = any(.data$bill_or_strip_flag),
      central_bank_flag = any(.data$central_bank_flag),
      restructuring_flag = any(.data$restructuring_flag),
      legacy_1_30_screen_pass = any(.data$legacy_1_30_screen_pass),
      source_package_ids = p15_observed_collapse(.data$source_package_id),
      source_record_locators = p15_observed_collapse(
        .data$source_record_locator
      ),
      source_object = "lseg_original_issue_yield",
      schema_version = p15_observed_all_years_schema_version(),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      materiality_50m_usd_pass = .data$usd_weight_available &
        .data$face_issued_usd >= 50000000,
      identifier_yield_conflict = is.finite(.data$direct_yield_range_bps) &
        .data$direct_yield_range_bps > 25,
      primary_evidence_state = dplyr::case_when(
        is.na(.data$original_issue_yield_pct) ~
          "source_issue_missing_original_yield",
        !.data$source_object_standard ~
          "direct_original_yield_nonstandard_or_out_of_scope",
        !.data$usd_weight_available ~
          "standard_direct_yield_missing_usd_weight",
        .data$identifier_yield_conflict ~
          "standard_direct_yield_identifier_conflict",
        TRUE ~ "standard_direct_original_yield_candidate"
      ),
      candidate_primary_predecessor_trial =
        !is.na(.data$original_issue_yield_pct) &
        .data$currency %in% c("USD", "EUR") &
        .data$original_maturity_years >= 1 &
        .data$positive_any_weight_available,
      candidate_primary_standard = .data$source_object_standard &
        .data$usd_weight_available & !.data$identifier_yield_conflict,
      candidate_primary_standard_50m = .data$candidate_primary_standard &
        .data$materiality_50m_usd_pass,
      candidate_primary_predecessor_1_30_50m =
        .data$candidate_primary_standard_50m &
        .data$legacy_1_30_screen_pass
    ) |>
    dplyr::arrange(.data$analysis_year, .data$iso3, .data$issue_date,
                   .data$economic_issue_key)
}

p15_primary_variant_register <- function() {
  tibble::tribble(
    ~candidate_variant_id, ~candidate_field, ~currency_rule, ~plain_language_role,
    "primary_predecessor_partial_trial_all_direct",
    "candidate_primary_predecessor_trial", "USD_or_EUR",
    "Replication candidate matching the old partial-trial aggregation surface.",
    "primary_standard_usd_eur_all_issue_counts",
    "candidate_primary_standard", "USD_or_EUR",
    "Direct original-yield evidence with standard source-object and USD-weight checks.",
    "primary_standard_usd_eur_50m",
    "candidate_primary_standard_50m", "USD_or_EUR",
    "Standard direct original yields with the inherited USD 50m materiality screen.",
    "primary_standard_usd_only_50m",
    "candidate_primary_standard_50m", "USD_only",
    "USD-only material primary candidate for currency sensitivity.",
    "primary_predecessor_1_30_usd_eur_50m",
    "candidate_primary_predecessor_1_30_50m", "USD_or_EUR",
    "Inherited 1-30 percent candidate retained only as a labelled comparison."
  ) |>
    dplyr::mutate(
      candidate_decision_state = "not_selected_decision_evidence",
      schema_version = p15_observed_all_years_schema_version()
    )
}

p15_aggregate_primary_candidate_variants <- function(issue_evidence) {
  register <- p15_primary_variant_register()
  dplyr::bind_rows(lapply(seq_len(nrow(register)), function(i) {
    rule <- register[i, ]
    issue_evidence |>
      dplyr::filter(
        .data[[rule$candidate_field]],
        if (rule$currency_rule == "USD_only") .data$currency == "USD" else TRUE
      ) |>
      dplyr::group_by(
        .data$analysis_year, .data$iso3, .data$country,
        .data$country_year_id, .data$period,
        .data$historical_income_level,
        .data$historical_lmic_reporting_scope
      ) |>
      dplyr::summarise(
        market_rate_pct = p15_observed_weighted_mean(
          .data$original_issue_yield_pct, .data$aggregation_weight_value
        ),
        market_maturity_years = p15_observed_weighted_mean(
          .data$original_maturity_years, .data$aggregation_weight_value
        ),
        issue_count = dplyr::n(),
        total_weight_value = sum(.data$aggregation_weight_value, na.rm = TRUE),
        weight_basis = p15_observed_collapse(.data$aggregation_weight_basis),
        largest_issue_weight_share = p15_observed_largest_weight_share(
          .data$aggregation_weight_value
        ),
        min_issue_rate_pct = p15_observed_min_or_na(
          .data$original_issue_yield_pct
        ),
        max_issue_rate_pct = p15_observed_max_or_na(
          .data$original_issue_yield_pct
        ),
        currency_basis = p15_observed_collapse(.data$currency),
        included_issue_keys = p15_observed_collapse(
          .data$economic_issue_key
        ),
        included_isins = p15_observed_collapse(.data$representative_isins),
        source_package_ids = p15_observed_collapse(.data$source_package_ids),
        .groups = "drop"
      ) |>
      dplyr::mutate(
        candidate_variant_id = rule$candidate_variant_id,
        candidate_decision_state = "not_selected_decision_evidence",
        evidence_family = "observed_primary_issuance",
        source_object = "lseg_original_issue_yield",
        schema_version = p15_observed_all_years_schema_version()
      )
  })) |>
    dplyr::arrange(.data$analysis_year, .data$iso3,
                   .data$candidate_variant_id)
}

p15_secondary_date_subset <- function(rows, date_column, snapshot_column,
                                      selection_rule = c("latest", "closest"),
                                      pre_days = Inf, post_days = Inf) {
  selection_rule <- match.arg(selection_rule)
  # Called within identifier-year groups. Keep all same-date source observations
  # for the existing duplicate/conflict checks, not a convenient single record.
  dates <- as.Date(rows[[date_column]])
  snapshots <- as.Date(rows[[snapshot_column]])
  offset <- as.numeric(dates - snapshots)
  at <- which(!is.na(offset) & offset >= -pre_days & offset <= post_days)
  if (!length(at)) return(rows[FALSE, , drop = FALSE])
  if (selection_rule == "closest") {
    at <- at[abs(offset[at]) == min(abs(offset[at]))]
    at <- at[offset[at] == min(offset[at])]
  } else at <- at[dates[at] == max(dates[at])]
  rows[at, , drop = FALSE]
}

p15_latest_secondary_direct_quotes <- function(history, selection_rule = c("latest", "closest"),
                                                pre_days = Inf, post_days = Inf) {
  selection_rule <- match.arg(selection_rule)
  p15_observed_assert_columns(
    history,
    c(
      "analysis_year", "history_date", "source_subrow", "RIC",
      "mid_price", "bid", "ask", "yield_to_maturity",
      "any_observed_measure", "source_package_id", "source_role",
      "source_record_locator"
    ),
    "P15 secondary history"
  )

  coverage <- history |>
    dplyr::mutate(
      analysis_year = as.integer(.data$analysis_year),
      history_date = p15_observed_date(.data$history_date),
      any_price = is.finite(p15_observed_num(.data$mid_price)) |
        is.finite(p15_observed_num(.data$bid)) |
        is.finite(p15_observed_num(.data$ask)),
      direct_yield_available = is.finite(
        p15_observed_num(.data$yield_to_maturity)
      )
    ) |>
    dplyr::group_by(.data$analysis_year, .data$RIC) |>
    dplyr::summarise(
      history_row_count = dplyr::n(),
      observed_measure_row_count = sum(.data$any_observed_measure, na.rm = TRUE),
      price_row_count = sum(.data$any_price, na.rm = TRUE),
      direct_yield_row_count = sum(.data$direct_yield_available, na.rm = TRUE),
      latest_any_measure_date = p15_observed_max_or_na(
        as.numeric(.data$history_date[.data$any_observed_measure])
      ),
      latest_price_date = p15_observed_max_or_na(
        as.numeric(.data$history_date[.data$any_price])
      ),
      source_package_ids_all = p15_observed_collapse(.data$source_package_id),
      source_roles_all = p15_observed_collapse(.data$source_role),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      latest_any_measure_date = as.Date(
        .data$latest_any_measure_date, origin = "1970-01-01"
      ),
      latest_price_date = as.Date(
        .data$latest_price_date, origin = "1970-01-01"
      )
    )

  direct <- history |>
    dplyr::mutate(
      analysis_year = as.integer(.data$analysis_year),
      history_date = p15_observed_date(.data$history_date),
      yield_to_maturity = p15_observed_num(.data$yield_to_maturity)
    ) |>
    dplyr::filter(is.finite(.data$yield_to_maturity)) |>
    dplyr::mutate(quote_snapshot_date = as.Date(paste0(.data$analysis_year, "-12-31"))) |>
    dplyr::group_by(.data$analysis_year, .data$RIC) |>
    dplyr::group_modify(~ p15_secondary_date_subset(.x, "history_date", "quote_snapshot_date",
      selection_rule, pre_days, post_days)) |>
    dplyr::summarise(
      direct_quote_date = p15_observed_first(.data$history_date),
      direct_yield_pct = stats::median(.data$yield_to_maturity, na.rm = TRUE),
      latest_date_direct_value_count = dplyr::n(),
      latest_date_direct_min_pct = min(.data$yield_to_maturity, na.rm = TRUE),
      latest_date_direct_max_pct = max(.data$yield_to_maturity, na.rm = TRUE),
      latest_date_direct_range_bps = (
        .data$latest_date_direct_max_pct - .data$latest_date_direct_min_pct
      ) * 100,
      direct_source_package_ids = p15_observed_collapse(
        .data$source_package_id
      ),
      direct_source_roles = p15_observed_collapse(.data$source_role),
      direct_source_record_locators = p15_observed_collapse(
        .data$source_record_locator
      ),
      .groups = "drop"
    )

  dplyr::left_join(coverage, direct, by = c("analysis_year", "RIC"))
}

p15_build_secondary_identifier_evidence_all_years <- function(
    instrument_universe, history, country_grid, selection_rule = "latest",
    pre_days = Inf, post_days = Inf) {
  p15_observed_assert_columns(
    instrument_universe,
    c(
      "snapshot_year", "snapshot_date", "RIC", "ISIN", "canonical_iso3",
      "canonical_country", "IssueDate", "MaturityDate", "Currency",
      "CouponRate", "CouponFrequencyDescription", "RCSCouponTypeLeaf",
      "CouponTypeDescription", "FaceIssuedUSD", "FaceOutstandingUSD",
      "AssetStatus", "IsCallable", "IsPutable", "IsSinkable",
      "IsConvertible", "flag_short_bill_cp_like", "flag_central_bank_like",
      "p15_history_coverage_state", "p15_history_source_package_id",
      "p15_history_source_role", "p15_available_measure_contract",
      "p15_source_record_locator"
    ),
    "P15 instrument-year universe"
  )

  latest <- p15_latest_secondary_direct_quotes(history, selection_rule, pre_days, post_days)
  instrument_universe |>
    dplyr::transmute(
      analysis_year = as.integer(.data$snapshot_year),
      snapshot_date = p15_observed_date(.data$snapshot_date),
      ric = as.character(.data$RIC),
      isin = as.character(.data$ISIN),
      iso3 = as.character(.data$canonical_iso3),
      source_country = as.character(.data$canonical_country),
      issue_date = p15_observed_date(.data$IssueDate),
      maturity_date = p15_observed_date(.data$MaturityDate),
      currency = as.character(.data$Currency),
      coupon_rate_pct = p15_observed_num(.data$CouponRate),
      coupon_frequency_description = as.character(
        .data$CouponFrequencyDescription
      ),
      coupon_type_leaf = as.character(.data$RCSCouponTypeLeaf),
      coupon_type_description = as.character(.data$CouponTypeDescription),
      face_issued_usd = p15_observed_num(.data$FaceIssuedUSD),
      face_outstanding_usd = p15_observed_num(.data$FaceOutstandingUSD),
      asset_status = as.character(.data$AssetStatus),
      is_callable = p15_observed_truthy(.data$IsCallable),
      is_putable = p15_observed_truthy(.data$IsPutable),
      is_sinkable = p15_observed_truthy(.data$IsSinkable),
      is_convertible = p15_observed_truthy(.data$IsConvertible),
      flag_short_bill_cp_like = p15_observed_truthy(
        .data$flag_short_bill_cp_like
      ),
      flag_central_bank_like = p15_observed_truthy(
        .data$flag_central_bank_like
      ),
      history_coverage_state = as.character(
        .data$p15_history_coverage_state
      ),
      universe_source_package_id = as.character(
        .data$p15_history_source_package_id
      ),
      universe_source_role = as.character(.data$p15_history_source_role),
      available_measure_contract = as.character(
        .data$p15_available_measure_contract
      ),
      universe_source_record_locator = as.character(
        .data$p15_source_record_locator
      )
    ) |>
    dplyr::left_join(
      latest,
      by = c("analysis_year", "ric" = "RIC")
    ) |>
    dplyr::left_join(
      country_grid |>
        dplyr::select(
          "analysis_year", "iso3", country = "country",
          "historical_income_level", "historical_lmic_reporting_scope",
          "country_year_id", "period"
        ),
      by = c("analysis_year", "iso3")
    ) |>
    dplyr::mutate(
      country = dplyr::coalesce(.data$country, .data$source_country),
      remaining_maturity_years = as.numeric(
        .data$maturity_date - .data$snapshot_date
      ) / 365.25,
      direct_yield_available = is.finite(.data$direct_yield_pct),
      any_price_available = .data$price_row_count > 0,
      any_observed_measure_available = .data$observed_measure_row_count > 0,
      quote_offset_days = as.numeric(
        .data$direct_quote_date - .data$snapshot_date
      ),
      quote_recency_abs_days = abs(.data$quote_offset_days),
      quote_timing_state = dplyr::case_when(
        !.data$direct_yield_available ~ "no_direct_yield_quote",
        .data$quote_offset_days == 0 ~ "year_end_quote",
        .data$quote_offset_days < 0 & .data$quote_recency_abs_days <= 31 ~
          "pre_year_end_within_31_days",
        .data$quote_offset_days > 0 & .data$quote_recency_abs_days <= 7 ~
          "post_year_end_within_7_days",
        TRUE ~ "outside_preferred_year_end_window"
      ),
      nonstandard_feature_flag = .data$is_callable | .data$is_putable |
        .data$is_sinkable | .data$is_convertible |
        .data$flag_short_bill_cp_like |
        stringr::str_detect(
          tolower(paste(
            dplyr::coalesce(.data$coupon_type_leaf, ""),
            dplyr::coalesce(.data$coupon_type_description, "")
          )),
          "floating|variable|zero|strip|step|index|sukuk|linked"
        ),
      positive_outstanding = is.finite(.data$face_outstanding_usd) &
        .data$face_outstanding_usd > 0,
      hard_currency_usd_eur = .data$currency %in% c("USD", "EUR"),
      residual_ge1 = is.finite(.data$remaining_maturity_years) &
        .data$remaining_maturity_years >= 1,
      residual_2_15 = is.finite(.data$remaining_maturity_years) &
        .data$remaining_maturity_years >= 2 &
        .data$remaining_maturity_years <= 15,
      latest_date_direct_conflict = is.finite(
        .data$latest_date_direct_range_bps
      ) & .data$latest_date_direct_range_bps > 5,
      source_object_standard = .data$direct_yield_available &
        .data$hard_currency_usd_eur & .data$positive_outstanding &
        !.data$nonstandard_feature_flag &
        !.data$flag_central_bank_like &
        !.data$latest_date_direct_conflict,
      legacy_1_30_screen_pass = .data$direct_yield_available &
        .data$direct_yield_pct >= 1 & .data$direct_yield_pct <= 30,
      economic_issue_key = p15_observed_issue_key(
        .data$iso3, .data$issue_date, .data$maturity_date, .data$currency,
        .data$coupon_rate_pct, .data$face_issued_usd
      ),
      source_object = "lseg_secondary_direct_yield_to_maturity",
      schema_version = p15_observed_all_years_schema_version()
    ) |>
    dplyr::mutate(
      secondary_identifier_evidence_state = dplyr::case_when(
        .data$direct_yield_available & .data$source_object_standard &
          .data$residual_2_15 ~ "standard_direct_ytm_candidate_2_15",
        .data$direct_yield_available & .data$source_object_standard ~
          "standard_direct_ytm_outside_2_15",
        .data$direct_yield_available ~
          "direct_ytm_nonstandard_or_weight_conflict",
        .data$any_price_available ~ "price_available_without_direct_ytm",
        .data$any_observed_measure_available ~
          "other_measure_available_without_direct_ytm",
        TRUE ~ "no_observed_history_measure"
      )
    ) |>
    dplyr::arrange(.data$analysis_year, .data$iso3, .data$economic_issue_key,
                   .data$ric)
}

p15_build_secondary_issue_evidence_all_years <- function(identifier_evidence) {
  identifier_evidence |>
    dplyr::group_by(
      .data$analysis_year, .data$iso3, .data$country,
      .data$country_year_id, .data$period,
      .data$historical_income_level,
      .data$historical_lmic_reporting_scope, .data$economic_issue_key
    ) |>
    dplyr::summarise(
      issue_date = p15_observed_first(.data$issue_date),
      maturity_date = p15_observed_first(.data$maturity_date),
      currency = p15_observed_first(.data$currency),
      coupon_rate_pct = p15_observed_median_or_na(.data$coupon_rate_pct),
      remaining_maturity_years = p15_observed_median_or_na(
        .data$remaining_maturity_years
      ),
      face_outstanding_usd = p15_observed_max_or_na(
        .data$face_outstanding_usd
      ),
      direct_yield_pct = p15_observed_median_or_na(.data$direct_yield_pct),
      direct_quote_date = p15_observed_first(.data$direct_quote_date),
      quote_recency_abs_days = p15_observed_min_or_na(
        .data$quote_recency_abs_days
      ),
      quote_timing_states = p15_observed_collapse(.data$quote_timing_state),
      identifier_count = dplyr::n(),
      identifiers_with_direct_yield = sum(.data$direct_yield_available),
      identifiers_with_price = sum(.data$any_price_available),
      identifier_direct_yield_dispersion_bps = (
        p15_observed_max_or_na(.data$direct_yield_pct) -
          p15_observed_min_or_na(.data$direct_yield_pct)
      ) * 100,
      representative_rics = p15_observed_collapse(.data$ric),
      representative_isins = p15_observed_collapse(.data$isin),
      asset_statuses = p15_observed_collapse(.data$asset_status),
      nonstandard_feature_flag = any(
        .data$nonstandard_feature_flag, na.rm = TRUE
      ),
      positive_outstanding = any(.data$positive_outstanding, na.rm = TRUE),
      source_object_standard = any(
        .data$source_object_standard, na.rm = TRUE
      ),
      residual_ge1 = any(.data$residual_ge1, na.rm = TRUE),
      residual_2_15 = any(.data$residual_2_15, na.rm = TRUE),
      legacy_1_30_screen_pass = any(
        .data$legacy_1_30_screen_pass, na.rm = TRUE
      ),
      direct_source_package_ids = p15_observed_collapse(
        .data$direct_source_package_ids
      ),
      direct_source_roles = p15_observed_collapse(.data$direct_source_roles),
      direct_source_record_locators = p15_observed_collapse(
        .data$direct_source_record_locators
      ),
      schema_version = p15_observed_all_years_schema_version(),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      identifier_rate_conflict = is.finite(
        .data$identifier_direct_yield_dispersion_bps
      ) & .data$identifier_direct_yield_dispersion_bps > 25,
      candidate_secondary_standard = .data$source_object_standard &
        !.data$identifier_rate_conflict,
      candidate_secondary_usd_2_15 = .data$candidate_secondary_standard &
        .data$currency == "USD" & .data$residual_2_15,
      candidate_secondary_usd_eur_2_15 =
        .data$candidate_secondary_standard & .data$residual_2_15,
      candidate_secondary_usd_ge1 = .data$candidate_secondary_standard &
        .data$currency == "USD" & .data$residual_ge1,
      candidate_secondary_predecessor_1_30_usd_2_15 =
        .data$candidate_secondary_usd_2_15 &
        .data$legacy_1_30_screen_pass,
      secondary_issue_evidence_state = dplyr::case_when(
        is.na(.data$direct_yield_pct) & .data$identifiers_with_price > 0 ~
          "economic_issue_price_without_direct_ytm",
        is.na(.data$direct_yield_pct) ~
          "economic_issue_no_observed_direct_ytm",
        .data$identifier_rate_conflict ~
          "economic_issue_unresolved_identifier_rate_conflict",
        .data$candidate_secondary_usd_2_15 ~
          "economic_issue_standard_usd_2_15_direct_candidate",
        .data$candidate_secondary_standard ~
          "economic_issue_standard_direct_outside_usd_2_15",
        TRUE ~ "economic_issue_direct_nonstandard_or_weight_problem"
      )
    ) |>
    dplyr::arrange(.data$analysis_year, .data$iso3,
                   .data$economic_issue_key)
}

p15_secondary_variant_register <- function() {
  tibble::tribble(
    ~candidate_variant_id, ~candidate_field, ~plain_language_role,
    "secondary_standard_usd_2_15_direct",
    "candidate_secondary_usd_2_15",
    "Standard USD direct YTM with 2-15 years remaining maturity.",
    "secondary_broader_usd_eur_2_15_direct",
    "candidate_secondary_usd_eur_2_15",
    "Broader USD/EUR direct YTM with 2-15 years remaining maturity.",
    "secondary_usd_ge1_direct_sensitivity",
    "candidate_secondary_usd_ge1",
    "USD direct-YTM maturity-window sensitivity with at least one year remaining.",
    "secondary_predecessor_1_30_usd_2_15_direct",
    "candidate_secondary_predecessor_1_30_usd_2_15",
    "Inherited 1-30 percent secondary candidate retained only as a comparison."
  ) |>
    dplyr::mutate(
      candidate_decision_state = "not_selected_decision_evidence",
      schema_version = p15_observed_all_years_schema_version()
    )
}

p15_aggregate_secondary_candidate_variants <- function(issue_evidence) {
  register <- p15_secondary_variant_register()
  dplyr::bind_rows(lapply(seq_len(nrow(register)), function(i) {
    rule <- register[i, ]
    issue_evidence |>
      dplyr::filter(.data[[rule$candidate_field]]) |>
      dplyr::group_by(
        .data$analysis_year, .data$iso3, .data$country,
        .data$country_year_id, .data$period,
        .data$historical_income_level,
        .data$historical_lmic_reporting_scope
      ) |>
      dplyr::summarise(
        market_rate_pct = p15_observed_weighted_mean(
          .data$direct_yield_pct, .data$face_outstanding_usd
        ),
        market_maturity_years = p15_observed_weighted_mean(
          .data$remaining_maturity_years, .data$face_outstanding_usd
        ),
        issue_count = dplyr::n(),
        identifier_count = sum(.data$identifier_count),
        total_weight_usd = sum(.data$face_outstanding_usd, na.rm = TRUE),
        largest_issue_weight_share = p15_observed_largest_weight_share(
          .data$face_outstanding_usd
        ),
        min_issue_rate_pct = p15_observed_min_or_na(.data$direct_yield_pct),
        max_issue_rate_pct = p15_observed_max_or_na(.data$direct_yield_pct),
        currency_basis = p15_observed_collapse(.data$currency),
        last_quote_date = p15_observed_last_date(.data$direct_quote_date),
        max_quote_recency_abs_days = p15_observed_max_or_na(
          .data$quote_recency_abs_days
        ),
        included_issue_keys = p15_observed_collapse(
          .data$economic_issue_key
        ),
        included_rics = p15_observed_collapse(.data$representative_rics),
        included_isins = p15_observed_collapse(.data$representative_isins),
        source_package_ids = p15_observed_collapse(
          .data$direct_source_package_ids
        ),
        source_roles = p15_observed_collapse(.data$direct_source_roles),
        .groups = "drop"
      ) |>
      dplyr::mutate(
        candidate_variant_id = rule$candidate_variant_id,
        candidate_decision_state = "not_selected_decision_evidence",
        evidence_family = "observed_secondary_market_evidence",
        source_object = "lseg_secondary_direct_yield_to_maturity",
        schema_version = p15_observed_all_years_schema_version()
      )
  })) |>
    dplyr::arrange(.data$analysis_year, .data$iso3,
                   .data$candidate_variant_id)
}

p15_build_observed_availability_ledgers <- function(
    country_grid, primary_issues, secondary_identifiers, secondary_issues) {
  primary_counts <- primary_issues |>
    dplyr::group_by(.data$analysis_year, .data$iso3) |>
    dplyr::summarise(
      primary_source_issue_count = dplyr::n(),
      primary_direct_yield_issue_count = sum(
        !is.na(.data$original_issue_yield_pct)
      ),
      primary_standard_issue_count = sum(.data$candidate_primary_standard),
      primary_standard_50m_issue_count = sum(
        .data$candidate_primary_standard_50m
      ),
      primary_missing_usd_weight_issue_count = sum(
        .data$source_object_standard & !.data$usd_weight_available
      ),
      .groups = "drop"
    )
  primary <- country_grid |>
    dplyr::left_join(primary_counts, by = c("analysis_year", "iso3")) |>
    dplyr::mutate(
      dplyr::across(
        dplyr::ends_with("_count"), ~ dplyr::coalesce(as.integer(.x), 0L)
      ),
      primary_source_availability_state = dplyr::case_when(
        .data$primary_source_issue_count == 0 ~
          "no_primary_issue_in_project_source",
        .data$primary_direct_yield_issue_count == 0 ~
          "primary_issues_present_original_yield_missing",
        .data$primary_standard_issue_count == 0 &
          .data$primary_missing_usd_weight_issue_count > 0 ~
          "direct_primary_present_usd_weight_missing",
        .data$primary_standard_issue_count == 0 ~
          "direct_primary_present_only_nonstandard_or_out_of_scope",
        .data$primary_standard_50m_issue_count == 0 ~
          "standard_direct_primary_present_below_materiality",
        TRUE ~ "standard_direct_primary_candidate_available"
      ),
      automatic_consequence = "none",
      schema_version = p15_observed_all_years_schema_version()
    )

  secondary_identifier_counts <- secondary_identifiers |>
    dplyr::group_by(.data$analysis_year, .data$iso3) |>
    dplyr::summarise(
      secondary_instrument_count = dplyr::n(),
      secondary_instruments_with_measure_count = sum(
        .data$any_observed_measure_available
      ),
      secondary_instruments_with_price_count = sum(.data$any_price_available),
      secondary_instruments_with_direct_ytm_count = sum(
        .data$direct_yield_available
      ),
      .groups = "drop"
    )
  secondary_issue_counts <- secondary_issues |>
    dplyr::group_by(.data$analysis_year, .data$iso3) |>
    dplyr::summarise(
      secondary_standard_direct_issue_count = sum(
        .data$candidate_secondary_standard
      ),
      secondary_standard_usd_2_15_issue_count = sum(
        .data$candidate_secondary_usd_2_15
      ),
      secondary_identifier_conflict_issue_count = sum(
        .data$identifier_rate_conflict
      ),
      .groups = "drop"
    )
  secondary <- country_grid |>
    dplyr::left_join(
      secondary_identifier_counts, by = c("analysis_year", "iso3")
    ) |>
    dplyr::left_join(
      secondary_issue_counts, by = c("analysis_year", "iso3")
    ) |>
    dplyr::mutate(
      dplyr::across(
        dplyr::ends_with("_count"), ~ dplyr::coalesce(as.integer(.x), 0L)
      ),
      secondary_source_availability_state = dplyr::case_when(
        .data$secondary_instrument_count == 0 ~
          "no_secondary_instrument_in_project_source_universe",
        .data$secondary_instruments_with_measure_count == 0 ~
          "secondary_stock_present_no_observed_history_measure",
        .data$secondary_instruments_with_direct_ytm_count == 0 &
          .data$secondary_instruments_with_price_count > 0 ~
          "secondary_price_present_without_direct_ytm",
        .data$secondary_instruments_with_direct_ytm_count == 0 ~
          "secondary_measure_present_direct_ytm_missing",
        .data$secondary_standard_direct_issue_count == 0 ~
          "direct_ytm_present_only_nonstandard_weight_or_conflict",
        .data$secondary_standard_usd_2_15_issue_count == 0 ~
          "standard_direct_ytm_present_outside_usd_2_15",
        TRUE ~ "standard_direct_secondary_usd_2_15_candidate_available"
      ),
      automatic_consequence = "none",
      schema_version = p15_observed_all_years_schema_version()
    )
  list(primary = primary, secondary = secondary)
}

p15_build_primary_aggregation_diagnostics <- function(
    primary_issue_evidence, primary_candidate_variants) {
  p15_observed_assert_columns(
    primary_issue_evidence,
    c(
      "analysis_year", "iso3", "country", "issue_date", "maturity_date",
      "currency", "coupon_rate_pct", "economic_issue_key",
      "original_issue_yield_pct", "original_maturity_years",
      "face_issued_usd", "aggregation_weight_basis",
      "request_identifier_count", "request_identifiers",
      "representative_isins", "direct_yield_range_bps",
      "source_object_standard", "usd_weight_available",
      "identifier_yield_conflict", "source_record_locators"
    ),
    "P15 primary issue evidence"
  )
  p15_observed_assert_columns(
    primary_candidate_variants,
    c(
      "analysis_year", "iso3", "country", "candidate_variant_id",
      "market_rate_pct", "market_maturity_years", "issue_count",
      "largest_issue_weight_share", "min_issue_rate_pct",
      "max_issue_rate_pct", "currency_basis", "included_issue_keys",
      "included_isins", "source_package_ids"
    ),
    "P15 primary candidate variants"
  )

  reopening_families <- primary_issue_evidence |>
    dplyr::mutate(
      reopening_family_key = paste(
        .data$iso3, .data$issue_date, .data$maturity_date, .data$currency,
        format(
          round(.data$coupon_rate_pct, 6), trim = TRUE, scientific = FALSE
        ),
        sep = "|"
      )
    ) |>
    dplyr::group_by(
      .data$analysis_year, .data$iso3, .data$country,
      .data$reopening_family_key
    ) |>
    dplyr::summarise(
      economic_issue_count = dplyr::n(),
      request_identifier_count = sum(.data$request_identifier_count),
      economic_issue_keys = p15_observed_collapse(.data$economic_issue_key),
      request_identifiers = p15_observed_collapse(.data$request_identifiers),
      representative_isins = p15_observed_collapse(.data$representative_isins),
      distinct_face_issued_usd = dplyr::n_distinct(
        .data$face_issued_usd[is.finite(.data$face_issued_usd)]
      ),
      total_face_issued_usd = sum(.data$face_issued_usd, na.rm = TRUE),
      minimum_issue_yield_pct = p15_observed_min_or_na(
        .data$original_issue_yield_pct
      ),
      maximum_issue_yield_pct = p15_observed_max_or_na(
        .data$original_issue_yield_pct
      ),
      family_yield_range_bps = (
        .data$maximum_issue_yield_pct - .data$minimum_issue_yield_pct
      ) * 100,
      source_record_locators = p15_observed_collapse(
        .data$source_record_locators
      ),
      .groups = "drop"
    ) |>
    dplyr::filter(
      .data$economic_issue_count > 1L | .data$request_identifier_count > 1L
    ) |>
    dplyr::mutate(
      reopening_diagnostic_class = dplyr::case_when(
        .data$economic_issue_count == 1L &
          .data$request_identifier_count > 1L ~
          "same_economic_issue_multiple_identifiers_already_collapsed",
        .data$family_yield_range_bps <= 5 ~
          "potential_reopening_or_tranche_similar_yield",
        .data$family_yield_range_bps > 25 ~
          "potential_distinct_tranche_or_source_conflict_review",
        TRUE ~ "potential_reopening_or_tap_requires_review"
      ),
      recommended_treatment = dplyr::case_when(
        .data$reopening_diagnostic_class ==
          "same_economic_issue_multiple_identifiers_already_collapsed" ~
          "Retain one economic issue with all identifier lineage.",
        .data$reopening_diagnostic_class ==
          "potential_reopening_or_tranche_similar_yield" ~
          "Review whether amounts are tranches or reopening increments before deciding whether to consolidate weights.",
        TRUE ~
          "Keep the economic issues separate until tranche or reopening status is resolved."
      ),
      decision_state = "diagnostic_not_applied_pending_PRI_05"
    ) |>
    dplyr::arrange(.data$analysis_year, .data$iso3,
                   .data$reopening_family_key)

  issue_casebook <- primary_issue_evidence |>
    dplyr::mutate(
      negative_yield_flag = is.finite(.data$original_issue_yield_pct) &
        .data$original_issue_yield_pct < 0,
      low_positive_yield_flag = is.finite(.data$original_issue_yield_pct) &
        .data$original_issue_yield_pct >= 0 &
        .data$original_issue_yield_pct < 1,
      high_yield_flag = is.finite(.data$original_issue_yield_pct) &
        .data$original_issue_yield_pct > 15,
      long_maturity_flag = is.finite(.data$original_maturity_years) &
        .data$original_maturity_years > 30,
      missing_usd_weight_flag = .data$source_object_standard &
        !.data$usd_weight_available,
      review_reason_codes = paste0(
        ifelse(.data$negative_yield_flag, "negative_yield;", ""),
        ifelse(.data$low_positive_yield_flag, "low_positive_yield;", ""),
        ifelse(.data$high_yield_flag, "high_yield_above_15;", ""),
        ifelse(.data$long_maturity_flag, "maturity_above_30_years;", ""),
        ifelse(.data$identifier_yield_conflict, "identifier_yield_conflict;", ""),
        ifelse(.data$missing_usd_weight_flag, "missing_usd_weight;", "")
      ),
      review_reason_codes = sub(";$", "", .data$review_reason_codes)
    ) |>
    dplyr::filter(nzchar(.data$review_reason_codes)) |>
    dplyr::mutate(
      recommended_treatment = dplyr::case_when(
        .data$identifier_yield_conflict ~
          "Resolve the identifier-level yield conflict before aggregation.",
        .data$missing_usd_weight_flag ~
          "Retain as evidence but exclude from USD-weighted candidates until a USD amount is sourced.",
        .data$high_yield_flag ~
          "Retain as a crisis or source-review observation; do not delete solely through a numeric threshold.",
        TRUE ~
          "Retain as observed evidence with a low-yield or maturity context flag."
      ),
      decision_state = "diagnostic_not_applied_pending_PRI_05"
    ) |>
    dplyr::arrange(.data$analysis_year, .data$iso3,
                   .data$original_issue_yield_pct)

  country_casebook <- primary_candidate_variants |>
    dplyr::filter(
      .data$candidate_variant_id == "primary_standard_usd_eur_50m"
    ) |>
    dplyr::mutate(
      single_issue_flag = .data$issue_count == 1L,
      dominant_issue_flag = .data$largest_issue_weight_share >= 0.8,
      wide_issue_rate_range_flag =
        .data$max_issue_rate_pct - .data$min_issue_rate_pct >= 3,
      mixed_currency_flag = stringr::str_detect(
        dplyr::coalesce(.data$currency_basis, ""), ";"
      ),
      review_reason_codes = paste0(
        ifelse(.data$single_issue_flag, "single_issue;", ""),
        ifelse(.data$dominant_issue_flag, "largest_issue_share_ge_80pct;", ""),
        ifelse(.data$wide_issue_rate_range_flag, "issue_rate_range_ge_3pp;", ""),
        ifelse(.data$mixed_currency_flag, "mixed_usd_eur_issue_set;", "")
      ),
      review_reason_codes = sub(";$", "", .data$review_reason_codes)
    ) |>
    dplyr::filter(nzchar(.data$review_reason_codes)) |>
    dplyr::mutate(
      recommended_treatment = dplyr::case_when(
        .data$wide_issue_rate_range_flag ~
          "Retain the weighted rate but require an issue-level range and composition warning.",
        .data$single_issue_flag | .data$dominant_issue_flag ~
          "Retain as observed but label the country-year as thin or concentrated primary evidence.",
        .data$mixed_currency_flag ~
          "Retain only under an approved USD/EUR primary rule and report currency composition.",
        TRUE ~ "Retain for aggregation review."
      ),
      decision_state = "diagnostic_not_applied_pending_PRI_05"
    ) |>
    dplyr::arrange(.data$analysis_year, .data$iso3)

  list(
    reopening_families = reopening_families,
    issue_casebook = issue_casebook,
    country_casebook = country_casebook
  )
}
