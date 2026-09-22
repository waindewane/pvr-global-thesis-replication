# P15 rating-implied candidate and validation helpers.
#
# These functions prepare labelled variants and descriptive validation evidence.
# They do not choose a final estimator, calibrate it, or approve headline use.

p15_rating_variant_schema_version <- function() {
  "SCHEMA-P15-RATING-VARIANT-VALIDATION-V1"
}

p15_validation_mean_or_na <- function(x) {
  x <- x[!is.na(x)]
  if (!length(x)) return(NA_real_)
  mean(x)
}

p15_weighted_mean_or_na <- function(x, w) {
  keep <- !is.na(x) & !is.na(w) & w > 0
  if (!any(keep)) return(NA_real_)
  stats::weighted.mean(x[keep], w[keep])
}

p15_rating_validation_period <- function(year) {
  dplyr::case_when(
    year >= 2012L & year <= 2015L ~ "2012-2015",
    year >= 2016L & year <= 2019L ~ "2016-2019",
    year >= 2020L & year <= 2023L ~ "2020-2023",
    year == 2024L ~ "2024_anchor",
    TRUE ~ "outside_scope"
  )
}

p15_build_lseg_risk_free_annual <- function(risk_free_history) {
  required <- c(
    "Date", "MID_PRICE", "curve", "tenor_years", "ric", "history_field"
  )
  missing <- setdiff(required, names(risk_free_history))
  if (length(missing)) {
    stop("LSEG risk-free history is missing: ", paste(missing, collapse = ", "),
         call. = FALSE)
  }

  risk_free_history |>
    dplyr::mutate(
      observation_date = as.Date(.data$Date),
      analysis_year = as.integer(format(.data$observation_date, "%Y")),
      risk_free_rate_pct = as.numeric(.data$MID_PRICE),
      tenor_years = as.numeric(.data$tenor_years)
    ) |>
    dplyr::filter(
      .data$history_field == "MID_PRICE",
      !is.na(.data$risk_free_rate_pct)
    ) |>
    dplyr::group_by(
      .data$analysis_year, .data$curve, .data$tenor_years, .data$ric
    ) |>
    dplyr::summarise(
      source_value_annual_mean = mean(.data$risk_free_rate_pct),
      source_value_annual_median = stats::median(.data$risk_free_rate_pct),
      source_value_min = min(.data$risk_free_rate_pct),
      source_value_max = max(.data$risk_free_rate_pct),
      observation_count = dplyr::n(),
      first_observation_date = min(.data$observation_date),
      last_observation_date = max(.data$observation_date),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      source_object_state = dplyr::case_when(
        abs(.data$source_value_annual_median) <= 25 ~
          "numeric_scale_rate_like_but_mid_price_semantics_unvalidated",
        TRUE ~ "price_or_index_scale_not_usable_as_rate"
      ),
      risk_free_admissibility_state =
        "quarantined_pending_field_definition_and_rate_semantics"
    ) |>
    dplyr::arrange(.data$analysis_year, .data$curve, .data$tenor_years)
}

p15_build_fred_tenor_annual <- function(fred_capture, series_id, tenor_years) {
  required <- c("date", "value")
  missing <- setdiff(required, names(fred_capture))
  if (length(missing)) {
    stop("FRED tenor capture is missing: ", paste(missing, collapse = ", "),
         call. = FALSE)
  }

  fred_capture |>
    dplyr::mutate(
      observation_date = as.Date(.data$date),
      analysis_year = as.integer(format(.data$observation_date, "%Y")),
      risk_free_rate_pct = as.numeric(.data$value)
    ) |>
    dplyr::group_by(.data$analysis_year) |>
    dplyr::summarise(
      series_id = series_id,
      tenor_years = as.numeric(tenor_years),
      risk_free_annual_mean_pct = p15_validation_mean_or_na(
        .data$risk_free_rate_pct
      ),
      observation_count = sum(!is.na(.data$risk_free_rate_pct)),
      first_observation_date = min(
        .data$observation_date[!is.na(.data$risk_free_rate_pct)], na.rm = TRUE
      ),
      last_observation_date = max(
        .data$observation_date[!is.na(.data$risk_free_rate_pct)], na.rm = TRUE
      ),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      source_object_state =
        "validated_us_treasury_constant_maturity_market_yield_percent",
      risk_free_admissibility_state = "available_for_tenor_sensitivity"
    )
}

p15_summarise_rating_gaps <- function(data, group_cols, weight_col = NULL) {
  required <- c(group_cols, "signed_gap_pp", "iso3", "analysis_year")
  missing <- setdiff(required, names(data))
  if (length(missing)) {
    stop("Rating validation data are missing: ", paste(missing, collapse = ", "),
         call. = FALSE)
  }
  if (!is.null(weight_col) && !weight_col %in% names(data)) {
    stop("Rating validation weight column is missing: ", weight_col,
         call. = FALSE)
  }

  data |>
    dplyr::filter(!is.na(.data$signed_gap_pp)) |>
    dplyr::group_by(dplyr::across(dplyr::all_of(group_cols))) |>
    dplyr::summarise(
      rows = dplyr::n(),
      countries = dplyr::n_distinct(.data$iso3),
      years = dplyr::n_distinct(.data$analysis_year),
      mean_signed_gap_pp = mean(.data$signed_gap_pp),
      median_signed_gap_pp = stats::median(.data$signed_gap_pp),
      mean_abs_gap_pp = mean(abs(.data$signed_gap_pp)),
      median_abs_gap_pp = stats::median(abs(.data$signed_gap_pp)),
      rmse_gap_pp = sqrt(mean(.data$signed_gap_pp^2)),
      weighted_mean_signed_gap_pp = if (is.null(weight_col)) {
        NA_real_
      } else {
        p15_weighted_mean_or_na(
          .data$signed_gap_pp, .data[[weight_col]]
        )
      },
      weighted_mean_abs_gap_pp = if (is.null(weight_col)) {
        NA_real_
      } else {
        p15_weighted_mean_or_na(
          abs(.data$signed_gap_pp), .data[[weight_col]]
        )
      },
      within_0_5pp_share = mean(abs(.data$signed_gap_pp) <= 0.5),
      within_1pp_share = mean(abs(.data$signed_gap_pp) <= 1),
      within_2pp_share = mean(abs(.data$signed_gap_pp) <= 2),
      .groups = "drop"
    )
}

p15_add_rating_validation_sample_cuts <- function(data) {
  required <- c(
    "historical_lmic_reporting_scope", "historical_income_level"
  )
  missing <- setdiff(required, names(data))
  if (length(missing)) {
    stop("Sample-cut input is missing: ", paste(missing, collapse = ", "),
         call. = FALSE)
  }

  dplyr::bind_rows(
    data |> dplyr::mutate(sample_cut = "all_country_years"),
    data |>
      dplyr::filter(.data$historical_lmic_reporting_scope %in% TRUE) |>
      dplyr::mutate(sample_cut = "historical_lmic_scope"),
    data |>
      dplyr::filter(.data$historical_income_level == "High income") |>
      dplyr::mutate(sample_cut = "historical_high_income_comparison")
  )
}
