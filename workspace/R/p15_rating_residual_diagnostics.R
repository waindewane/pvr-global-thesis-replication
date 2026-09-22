# Residual and country-persistence diagnostics for a matched rating candidate.

p15_rating_residual_schema_version <- function() {
  "SCHEMA-P15-RATING-RESIDUAL-DIAGNOSTICS-V1"
}

p15_prepare_rating_residual_sample <- function(
    detail,
    variant_id = "boy_available_agencies_median_mapped_spread_dgs7") {
  required <- c(
    "validation_question_id", "variant_id",
    "historical_lmic_reporting_scope", "accuracy_summary_eligible",
    "analysis_year", "iso3", "country", "anchor_rate_pct",
    "variant_rate_pct", "signed_gap_pp", "anchor_thin_evidence",
    "historical_income_level", "anchor_status_rule_class"
  )
  missing <- setdiff(required, names(detail))
  if (length(missing)) {
    stop("Residual detail is missing: ", paste(missing, collapse = ", "),
         call. = FALSE)
  }
  out <- detail |>
    dplyr::filter(
      .data$validation_question_id == "Q-USD-NEW-BORROWING",
      .data$variant_id == .env$variant_id,
      .data$historical_lmic_reporting_scope %in% TRUE,
      .data$accuracy_summary_eligible %in% TRUE
    ) |>
    dplyr::transmute(
      analysis_year = as.integer(.data$analysis_year),
      iso3 = as.character(.data$iso3),
      country = as.character(.data$country),
      observed_primary_rate_pct = as.numeric(.data$anchor_rate_pct),
      rating_implied_rate_pct = as.numeric(.data$variant_rate_pct),
      residual_pp = as.numeric(.data$signed_gap_pp),
      residual_direction = dplyr::case_when(
        .data$signed_gap_pp > 0 ~ "Rating-implied rate higher",
        .data$signed_gap_pp < 0 ~ "Rating-implied rate lower",
        TRUE ~ "Equal at stored precision"
      ),
      anchor_thin_evidence = as.logical(.data$anchor_thin_evidence),
      historical_income_level = as.character(.data$historical_income_level),
      anchor_status_rule_class = as.character(.data$anchor_status_rule_class),
      variant_id = .env$variant_id,
      residual_definition =
        "rating_implied_rate_pct_minus_observed_primary_rate_pct",
      schema_version = p15_rating_residual_schema_version()
    ) |>
    dplyr::arrange(.data$iso3, .data$analysis_year)
  country_year_key <- base::paste(
    out$analysis_year, out$iso3, sep = "::"
  )
  if (base::anyDuplicated(country_year_key)) {
    duplicate_key <- country_year_key[
      duplicated(country_year_key) | duplicated(country_year_key, fromLast = TRUE)
    ][[1]]
    stop(
      "Residual sample is not unique by country-year; first duplicate: ",
      duplicate_key, ".",
      call. = FALSE
    )
  }
  out
}

p15_summarise_country_residuals <- function(sample, minimum_years = 3L) {
  required <- c("analysis_year", "iso3", "country", "residual_pp")
  missing <- setdiff(required, names(sample))
  if (length(missing)) {
    stop("Residual sample is missing: ", paste(missing, collapse = ", "),
         call. = FALSE)
  }
  out <- sample |>
    dplyr::group_by(.data$iso3, .data$country) |>
    dplyr::summarise(
      observed_years = dplyr::n(),
      first_year = min(.data$analysis_year),
      last_year = max(.data$analysis_year),
      mean_residual_pp = mean(.data$residual_pp),
      median_residual_pp = stats::median(.data$residual_pp),
      sd_residual_pp = stats::sd(.data$residual_pp),
      min_residual_pp = min(.data$residual_pp),
      max_residual_pp = max(.data$residual_pp),
      positive_years = sum(.data$residual_pp > 0),
      negative_years = sum(.data$residual_pp < 0),
      zero_years = sum(.data$residual_pp == 0),
      share_positive = mean(.data$residual_pp > 0),
      all_positive = all(.data$residual_pp > 0),
      all_negative = all(.data$residual_pp < 0),
      .groups = "drop"
    ) |>
    dplyr::rowwise() |>
    dplyr::mutate(
      standard_error_pp = .data$sd_residual_pp / sqrt(.data$observed_years),
      confidence_interval_95_lower_pp = dplyr::if_else(
        .data$observed_years >= 2L,
        .data$mean_residual_pp -
          stats::qt(0.975, pmax(.data$observed_years - 1L, 1L)) *
            .data$standard_error_pp,
        NA_real_
      ),
      confidence_interval_95_upper_pp = dplyr::if_else(
        .data$observed_years >= 2L,
        .data$mean_residual_pp +
          stats::qt(0.975, pmax(.data$observed_years - 1L, 1L)) *
            .data$standard_error_pp,
        NA_real_
      ),
      exact_sign_test_p_value = if (.data$observed_years >= minimum_years) {
        stats::binom.test(
          .data$positive_years, .data$observed_years, p = 0.5
        )$p.value
      } else {
        NA_real_
      }
    ) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      exact_sign_test_bh_adjusted_p = stats::p.adjust(
        .data$exact_sign_test_p_value, method = "BH"
      ),
      dominant_sign_share = pmax(
        .data$share_positive, 1 - .data$share_positive
      ),
      directional_pattern = dplyr::case_when(
        .data$observed_years < minimum_years ~
          "insufficient_repeated_years",
        .data$share_positive >= 0.75 ~
          "predominantly_rating_implied_higher",
        .data$share_positive <= 0.25 ~
          "predominantly_rating_implied_lower",
        TRUE ~ "mixed_direction"
      ),
      individual_country_inference_state = dplyr::case_when(
        .data$observed_years < minimum_years ~ "not_tested_fewer_than_3_years",
        .data$exact_sign_test_bh_adjusted_p < 0.05 ~
          "sign_consistency_after_multiple_testing",
        TRUE ~ "no_sign_consistency_after_multiple_testing"
      ),
      schema_version = p15_rating_residual_schema_version()
    ) |>
    dplyr::arrange(dplyr::desc(abs(.data$mean_residual_pp)))
  out
}

p15_country_effect_permutation_test <- function(sample, simulations = 9999L,
                                                seed = 20260817L) {
  x <- sample |>
    dplyr::transmute(
      residual = as.numeric(.data$residual_pp),
      country = factor(.data$iso3),
      year = factor(.data$analysis_year)
    )
  x0 <- stats::model.matrix(~ year, data = x)
  x1 <- stats::model.matrix(~ year + country, data = x)
  qr0 <- qr(x0)
  qr1 <- qr(x1)
  residual_df <- nrow(x) - qr1$rank
  restriction_df <- qr1$rank - qr0$rank
  f_statistic <- function(y) {
    rss0 <- sum(qr.resid(qr0, y)^2)
    rss1 <- sum(qr.resid(qr1, y)^2)
    ((rss0 - rss1) / restriction_df) / (rss1 / residual_df)
  }
  observed <- f_statistic(x$residual)
  year_rows <- split(seq_len(nrow(x)), x$year)
  set.seed(seed)
  permuted <- numeric(simulations)
  for (i in seq_len(simulations)) {
    y <- x$residual
    for (rows in year_rows) {
      y[rows] <- sample(y[rows], length(rows), replace = FALSE)
    }
    permuted[[i]] <- f_statistic(y)
  }
  tibble::tibble(
    test_id = "country_effects_conditional_on_year_permutation",
    statistic = observed,
    numerator_df = restriction_df,
    denominator_df = residual_df,
    simulations = simulations,
    p_value = (1 + sum(permuted >= observed)) / (simulations + 1),
    null_hypothesis =
      "no_country_specific_residual_pattern_after_common_year_effects",
    permutation_rule =
      "residuals_permuted_across_country_slots_within_each_year",
    schema_version = p15_rating_residual_schema_version()
  )
}
