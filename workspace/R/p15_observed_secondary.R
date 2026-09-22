# Common P15 observed-secondary processors.
#
# Exact P13 parity uses the preserved 2024 issue and targeted-terminal evidence.
# The broader P15 history source is assessed separately after the legacy result is
# reproduced, so source-vintage changes cannot silently alter the P13 baseline.

p15_secondary_num <- function(x) suppressWarnings(as.numeric(as.character(x)))

p15_secondary_truthy <- function(x) {
  if (is.logical(x)) return(!is.na(x) & x)
  tolower(as.character(x)) %in% c("true", "t", "1", "yes")
}

p15_secondary_collapse <- function(x, sep = ";") {
  x <- sort(unique(as.character(x[!is.na(x) & as.character(x) != ""])))
  if (!length(x)) return(NA_character_)
  paste(x, collapse = sep)
}

p15_secondary_weighted_mean <- function(x, w) {
  x <- p15_secondary_num(x)
  w <- p15_secondary_num(w)
  ok <- !is.na(x) & !is.na(w) & w > 0
  if (!any(ok)) return(NA_real_)
  stats::weighted.mean(x[ok], w[ok])
}

p15_prepare_secondary_direct_issues <- function(source_rows, target_year = 2024L) {
  required <- c(
    "analysis_year", "iso3", "country", "economic_issue_key", "issue_date",
    "maturity_date", "currency", "face_outstanding_usd",
    "remaining_maturity_years", "secondary_yield_pct", "secondary_quote_date",
    "baseline_status", "nonstandard_issue", "representative_isins",
    "representative_rics"
  )
  missing <- setdiff(required, names(source_rows))
  if (length(missing)) {
    stop("Secondary source rows are missing: ", paste(missing, collapse = ", "),
         call. = FALSE)
  }

  source_rows |>
    dplyr::mutate(
      analysis_year = as.integer(.data$analysis_year),
      issue_date = as.Date(.data$issue_date),
      maturity_date = as.Date(.data$maturity_date),
      secondary_quote_date = as.Date(.data$secondary_quote_date),
      quote_year = as.integer(format(.data$secondary_quote_date, "%Y")),
      face_outstanding_usd = p15_secondary_num(.data$face_outstanding_usd),
      remaining_maturity_years = p15_secondary_num(.data$remaining_maturity_years),
      secondary_yield_pct = p15_secondary_num(.data$secondary_yield_pct),
      direct_ytm_available = !is.na(.data$secondary_yield_pct),
      nonstandard_issue = p15_secondary_truthy(.data$nonstandard_issue) |
        stringr::str_detect(
          dplyr::coalesce(as.character(.data$baseline_status), ""),
          "excluded_nonstandard"
        ),
      usd_only = .data$currency == "USD",
      hard_currency_usd_eur = .data$currency %in% c("USD", "EUR"),
      residual_ge1 = !is.na(.data$remaining_maturity_years) &
        .data$remaining_maturity_years >= 1,
      residual_2_15 = !is.na(.data$remaining_maturity_years) &
        .data$remaining_maturity_years >= 2 &
        .data$remaining_maturity_years <= 15,
      positive_outstanding = !is.na(.data$face_outstanding_usd) &
        .data$face_outstanding_usd > 0,
      p12a_standard_usd_2_15_candidate = .data$direct_ytm_available &
        .data$usd_only & .data$residual_2_15 & .data$positive_outstanding &
        !.data$nonstandard_issue,
      p13_legacy_direct_candidate = .data$quote_year == target_year &
        stringr::str_detect(
          dplyr::coalesce(as.character(.data$baseline_status), ""),
          "included_standard_secondary"
        ) &
        !stringr::str_detect(
          dplyr::coalesce(as.character(.data$baseline_status), ""),
          paste(
            "excluded_missing_direct_ytm|excluded_nonstandard|",
            "excluded_short_residual_maturity|",
            "excluded_missing_or_zero_outstanding",
            sep = ""
          )
        ) &
        !.data$nonstandard_issue &
        !is.na(.data$secondary_yield_pct) &
        .data$secondary_yield_pct >= 1 & .data$secondary_yield_pct <= 30 &
        !is.na(.data$remaining_maturity_years) &
        .data$remaining_maturity_years > 0 &
        .data$positive_outstanding
    ) |>
    dplyr::filter(.data$analysis_year == target_year)
}

p15_aggregate_secondary_direct <- function(
    issue_rows,
    candidate_field,
    source_class,
    source_package_id,
    method_id,
    issue_key_separator = ";") {
  if (!candidate_field %in% names(issue_rows)) {
    stop("Unknown secondary candidate field: ", candidate_field, call. = FALSE)
  }

  issue_rows |>
    dplyr::filter(.data[[candidate_field]]) |>
    dplyr::group_by(.data$analysis_year, .data$iso3, .data$country) |>
    dplyr::summarise(
      market_rate_pct = p15_secondary_weighted_mean(
        .data$secondary_yield_pct, .data$face_outstanding_usd
      ),
      market_maturity_years = p15_secondary_weighted_mean(
        .data$remaining_maturity_years, .data$face_outstanding_usd
      ),
      issue_count = as.integer(dplyr::n()),
      total_weight_usd = sum(.data$face_outstanding_usd, na.rm = TRUE),
      included_issue_keys = p15_secondary_collapse(
        .data$economic_issue_key, sep = issue_key_separator
      ),
      included_isins = p15_secondary_collapse(.data$representative_isins),
      included_rics = p15_secondary_collapse(.data$representative_rics),
      currency_basis = p15_secondary_collapse(.data$currency),
      first_issue_date = min(.data$issue_date, na.rm = TRUE),
      last_issue_date = max(.data$issue_date, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      selected_source_class = source_class,
      source_package_id = source_package_id,
      method_id = method_id,
      parity_specification_id = "SPEC-P15-P13-LEGACY-PARITY-V1"
    )
}

p15_secondary_parse_value_string <- function(x) {
  parts <- unlist(strsplit(as.character(x), ";", fixed = TRUE))
  values <- suppressWarnings(as.numeric(sub("^.*=", "", parts)))
  values[!is.na(values)]
}

p15_build_terminal_direct_country <- function(
    terminal_classes,
    accepted_target_ids = c("P7B-SEC-001", "P7B-SEC-002")) {
  terminal_classes |>
    dplyr::filter(
      .data$p7b_target_id %in% accepted_target_ids,
      p15_secondary_truthy(.data$has_numeric_latest_direct_yield),
      !p15_secondary_truthy(.data$p2_status_gate_overlap)
    ) |>
    dplyr::rowwise() |>
    dplyr::mutate(
      issue_yield_pct = stats::median(
        p15_secondary_parse_value_string(.data$latest_direct_yield_values_pct)
      )
    ) |>
    dplyr::ungroup() |>
    dplyr::group_by(.data$analysis_year, .data$iso3, .data$country) |>
    dplyr::summarise(
      market_rate_pct = p15_secondary_weighted_mean(
        .data$issue_yield_pct, .data$face_outstanding_usd
      ),
      market_maturity_years = p15_secondary_weighted_mean(
        .data$residual_maturity_years, .data$face_outstanding_usd
      ),
      issue_count = as.integer(dplyr::n()),
      total_weight_usd = sum(.data$face_outstanding_usd, na.rm = TRUE),
      included_issue_keys = p15_secondary_collapse(.data$candidate_issue_key),
      included_target_ids = p15_secondary_collapse(.data$p7b_target_id),
      currency_basis = p15_secondary_collapse(.data$issue_currency),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      selected_source_class = "lseg_terminal_secondary_direct_ytm",
      source_package_id = "SRC-P7B-LSEG-TERMINAL-20260605",
      method_id = "P7N_P8_terminal_direct_ytm_v1",
      parity_specification_id = "SPEC-P15-P13-LEGACY-PARITY-V1"
    )
}

p15_secondary_last_day_of_month <- function(year, month) {
  first_next <- if (month == 12) {
    as.Date(sprintf("%04d-01-01", year + 1))
  } else {
    as.Date(sprintf("%04d-%02d-01", year, month + 1))
  }
  as.integer(format(first_next - 1, "%d"))
}

p15_secondary_add_months <- function(d, months) {
  lt <- as.POSIXlt(d)
  year <- lt$year + 1900
  month <- lt$mon + 1
  day <- lt$mday
  target_index <- (year * 12 + (month - 1)) + months
  target_year <- target_index %/% 12
  target_month <- target_index %% 12 + 1
  target_day <- min(
    day, p15_secondary_last_day_of_month(target_year, target_month)
  )
  as.Date(sprintf("%04d-%02d-%02d", target_year, target_month, target_day))
}

p15_secondary_coupon_schedule <- function(settle_date, maturity_date, frequency) {
  frequency <- suppressWarnings(as.integer(round(frequency)))
  if (is.na(frequency) || frequency <= 0) frequency <- 1L
  months <- as.integer(round(12 / frequency))
  pay_dates <- as.Date(character())
  d <- maturity_date
  guard <- 0L
  while (!is.na(d) && d > settle_date && guard < 400L) {
    pay_dates <- c(pay_dates, d)
    d <- p15_secondary_add_months(d, -months)
    guard <- guard + 1L
  }
  list(
    previous_coupon = d,
    next_coupon = if (length(pay_dates)) sort(unique(pay_dates))[[1]] else maturity_date,
    pay_dates = sort(unique(pay_dates))
  )
}

p15_secondary_accrued_interest <- function(
    settle_date, maturity_date, coupon_rate_pct, frequency) {
  schedule <- p15_secondary_coupon_schedule(settle_date, maturity_date, frequency)
  denominator <- as.numeric(schedule$next_coupon - schedule$previous_coupon)
  if (is.na(denominator) || denominator <= 0) return(0)
  elapsed <- max(0, as.numeric(settle_date - schedule$previous_coupon))
  coupon_rate_pct / frequency * elapsed / denominator
}

p15_secondary_solve_ytm <- function(
    dirty_price, settle_date, maturity_date, coupon_rate_pct, frequency) {
  if (is.na(dirty_price) || dirty_price <= 0 || is.na(settle_date) ||
      is.na(maturity_date) || maturity_date <= settle_date) return(NA_real_)
  frequency <- suppressWarnings(as.integer(round(frequency)))
  if (is.na(frequency) || frequency <= 0) frequency <- 1L
  schedule <- p15_secondary_coupon_schedule(settle_date, maturity_date, frequency)
  if (!length(schedule$pay_dates)) return(NA_real_)
  times <- as.numeric(schedule$pay_dates - settle_date) / 365.25
  cashflows <- rep(coupon_rate_pct / frequency, length(schedule$pay_dates))
  cashflows[schedule$pay_dates == maturity_date] <-
    cashflows[schedule$pay_dates == maturity_date] + 100
  keep <- times > 0
  times <- times[keep]
  cashflows <- cashflows[keep]
  if (!length(times)) return(NA_real_)
  present_value <- function(y_decimal) {
    if (1 + y_decimal / frequency <= 0) return(Inf)
    sum(cashflows / ((1 + y_decimal / frequency) ^ (frequency * times)))
  }
  low <- -0.95 * frequency
  high <- 10
  for (i in seq_len(12)) {
    if (!is.na(present_value(high)) && present_value(high) < dirty_price) break
    high <- high * 2
  }
  for (i in seq_len(80)) {
    mid <- (low + high) / 2
    value <- present_value(mid)
    if (is.na(value)) return(NA_real_)
    if (value > dirty_price) low <- mid else high <- mid
  }
  (low + high) / 2 * 100
}

p15_secondary_ytm_from_clean_price <- function(
    clean_price, settle_date, maturity_date, coupon_rate_pct, frequency) {
  frequency <- suppressWarnings(as.integer(round(frequency)))
  if (is.na(frequency) || frequency <= 0) frequency <- 1L
  dirty_price <- clean_price + p15_secondary_accrued_interest(
    settle_date, maturity_date, coupon_rate_pct, frequency
  )
  p15_secondary_solve_ytm(
    dirty_price, settle_date, maturity_date, coupon_rate_pct, frequency
  )
}

p15_build_terminal_price_to_yield_country <- function(
    identifier_snapshots,
    eligibility_rows,
    terminal_classes) {
  eligible_ids <- eligibility_rows |>
    dplyr::filter(.data$price_to_yield_class == "eligible_for_price_to_yield_review") |>
    dplyr::pull(.data$p7b_target_id)

  trials <- identifier_snapshots |>
    dplyr::filter(.data$p7b_target_id %in% eligible_ids) |>
    dplyr::mutate(
      quote_date = as.Date(.data$latest_quote_date),
      maturity_date = as.Date(.data$static_maturity_date),
      coupon_rate_pct = p15_secondary_num(.data$static_coupon_rate),
      coupon_frequency = p15_secondary_num(.data$static_coupon_frequency),
      mid_yield_quote_date_pct = mapply(
        p15_secondary_ytm_from_clean_price,
        .data$latest_mid_price, .data$quote_date, .data$maturity_date,
        .data$coupon_rate_pct, .data$coupon_frequency
      ),
      bid_yield_quote_date_pct = mapply(
        p15_secondary_ytm_from_clean_price,
        .data$latest_bid_price, .data$quote_date, .data$maturity_date,
        .data$coupon_rate_pct, .data$coupon_frequency
      ),
      ask_yield_quote_date_pct = mapply(
        p15_secondary_ytm_from_clean_price,
        .data$latest_ask_price, .data$quote_date, .data$maturity_date,
        .data$coupon_rate_pct, .data$coupon_frequency
      ),
      mid_yield_t2_pct = mapply(
        p15_secondary_ytm_from_clean_price,
        .data$latest_mid_price, .data$quote_date + 2, .data$maturity_date,
        .data$coupon_rate_pct, .data$coupon_frequency
      ),
      settlement_sensitivity_bps = abs(
        .data$mid_yield_t2_pct - .data$mid_yield_quote_date_pct
      ) * 100,
      bid_ask_yield_range_bps = abs(
        .data$bid_yield_quote_date_pct - .data$ask_yield_quote_date_pct
      ) * 100
    )

  country_trials <- trials |>
    dplyr::group_by(
      .data$p7b_target_id, .data$request_id, .data$analysis_year,
      .data$iso3, .data$country
    ) |>
    dplyr::summarise(
      identifier_count = dplyr::n(),
      identifiers_with_computed_trial_yield = sum(
        !is.na(.data$mid_yield_quote_date_pct)
      ),
      market_rate_pct = stats::median(
        .data$mid_yield_quote_date_pct, na.rm = TRUE
      ),
      identifier_dispersion_bps = (
        max(.data$mid_yield_quote_date_pct, na.rm = TRUE) -
          min(.data$mid_yield_quote_date_pct, na.rm = TRUE)
      ) * 100,
      max_settlement_sensitivity_bps = max(
        .data$settlement_sensitivity_bps, na.rm = TRUE
      ),
      max_bid_ask_yield_range_bps = max(
        .data$bid_ask_yield_range_bps, na.rm = TRUE
      ),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      parity_gate_pass =
        .data$identifiers_with_computed_trial_yield == .data$identifier_count &
        .data$market_rate_pct > 1 & .data$market_rate_pct <= 30 &
        .data$identifier_dispersion_bps <= 25 &
        .data$max_settlement_sensitivity_bps <= 25 &
        .data$max_bid_ask_yield_range_bps <= 50
    )

  country_trials |>
    dplyr::filter(.data$parity_gate_pass) |>
    dplyr::left_join(
      terminal_classes |>
        dplyr::select(
          "p7b_target_id", "candidate_issue_key", "issue_currency",
          "residual_maturity_years", "face_outstanding_usd"
        ),
      by = "p7b_target_id"
    ) |>
    dplyr::transmute(
      analysis_year = as.integer(.data$analysis_year),
      iso3 = .data$iso3,
      country = .data$country,
      market_rate_pct = .data$market_rate_pct,
      market_maturity_years = p15_secondary_num(.data$residual_maturity_years),
      issue_count = 1L,
      identifier_count = as.integer(.data$identifier_count),
      total_weight_usd = p15_secondary_num(.data$face_outstanding_usd),
      included_issue_keys = .data$candidate_issue_key,
      included_target_ids = .data$p7b_target_id,
      currency_basis = .data$issue_currency,
      identifier_dispersion_bps = .data$identifier_dispersion_bps,
      max_settlement_sensitivity_bps = .data$max_settlement_sensitivity_bps,
      max_bid_ask_yield_range_bps = .data$max_bid_ask_yield_range_bps,
      selected_source_class = "lseg_terminal_secondary_price_to_yield",
      source_package_id = "SRC-P7B-LSEG-TERMINAL-20260605",
      method_id = "P7N_P8_clean_mid_price_to_yield_v1",
      parity_specification_id = "SPEC-P15-P13-LEGACY-PARITY-V1"
    )
}

p15_build_feature_rich_secondary_country <- function(accepted_issue_layer) {
  accepted_issue_layer |>
    dplyr::filter(
      .data$accepted_scope == paste0(
        "full_ladder_labelled_secondary_feature_rich_vendor_yield_",
        "usd_2_15_standard"
      )
    ) |>
    dplyr::group_by(
      .data$analysis_year, .data$iso3,
      country = .data$issuer_country_name
    ) |>
    dplyr::summarise(
      market_rate_pct = p15_secondary_weighted_mean(
        .data$central_rate_pct, .data$face_outstanding_usd
      ),
      market_maturity_years = p15_secondary_weighted_mean(
        .data$residual_maturity_years_at_quote, .data$face_outstanding_usd
      ),
      issue_count = as.integer(dplyr::n()),
      identifier_count = as.integer(sum(.data$identifier_count, na.rm = TRUE)),
      total_weight_usd = sum(.data$face_outstanding_usd, na.rm = TRUE),
      included_issue_keys = p15_secondary_collapse(.data$issue_layer_id),
      currency_basis = p15_secondary_collapse(.data$currency),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      selected_source_class = "p12a_feature_rich_vendor_yield_review",
      source_package_id = "SRC-P12A-FEATURE-RICH-2024",
      method_id = "P12A_FR2_feature_rich_vendor_yield_v1",
      parity_specification_id = "SPEC-P15-P13-LEGACY-PARITY-V1"
    )
}
