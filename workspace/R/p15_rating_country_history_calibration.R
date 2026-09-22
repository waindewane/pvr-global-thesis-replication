# Strictly past-only country-history calibration for the leading rating candidate.
#
# This helper preserves a diagnostic candidate. It does not promote the method or
# alter any benchmark-selection rule.

p15_rating_country_history_schema_version <- function() {
  "SCHEMA-P15-RATING-COUNTRY-HISTORY-CALIBRATION-V1"
}

p15_prepare_rating_country_history_sample <- function(
    detail,
    variant_id = "boy_available_agencies_median_mapped_spread_dgs7") {
  required <- c(
    "validation_question_id", "variant_id",
    "historical_lmic_reporting_scope", "accuracy_summary_eligible",
    "analysis_year", "iso3", "country", "anchor_rate_pct",
    "variant_rate_pct"
  )
  missing <- setdiff(required, names(detail))
  if (length(missing)) {
    stop(
      "Country-history detail is missing: ", paste(missing, collapse = ", "),
      call. = FALSE
    )
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
      observed_rate_pct = as.numeric(.data$anchor_rate_pct),
      rating_rate_pct = as.numeric(.data$variant_rate_pct)
    ) |>
    dplyr::arrange(.data$iso3, .data$analysis_year)
  key <- paste(out$analysis_year, out$iso3, sep = "::")
  if (anyDuplicated(key)) {
    stop("Country-history sample is not unique by country-year.", call. = FALSE)
  }
  out
}

p15_fit_rating_country_history <- function(train, test) {
  required <- c(
    "analysis_year", "iso3", "country", "observed_rate_pct",
    "rating_rate_pct"
  )
  missing_train <- setdiff(required, names(train))
  missing_test <- setdiff(required, names(test))
  if (length(missing_train) || length(missing_test)) {
    stop(
      "Country-history fit inputs are missing required columns.",
      call. = FALSE
    )
  }
  if (!nrow(train) || !nrow(test)) {
    stop("Country-history fit requires non-empty train and test data.", call. = FALSE)
  }

  affine_fit <- stats::lm(
    observed_rate_pct ~ rating_rate_pct,
    data = train
  )
  affine_train <- as.numeric(stats::predict(affine_fit, newdata = train))
  affine_test <- as.numeric(stats::predict(affine_fit, newdata = test))

  residual_training <- train |>
    dplyr::mutate(
      affine_residual_pp = .data$observed_rate_pct - affine_train
    ) |>
    dplyr::group_by(.data$analysis_year) |>
    dplyr::mutate(
      pooled_residual_pp =
        .data$affine_residual_pp - mean(.data$affine_residual_pp)
    ) |>
    dplyr::ungroup()

  variance_fit <- nlme::lme(
    pooled_residual_pp ~ 1,
    random = ~1 | iso3,
    data = residual_training,
    method = "REML",
    control = nlme::lmeControl(opt = "optim")
  )
  variance_components <- nlme::VarCorr(variance_fit)
  country_sd <- as.numeric(variance_components[1, "StdDev"])
  residual_sd <- as.numeric(variance_components[2, "StdDev"])
  shrinkage_k <- if (is.finite(country_sd) && country_sd > 0) {
    (residual_sd / country_sd)^2
  } else {
    Inf
  }

  country_adjustments <- residual_training |>
    dplyr::group_by(.data$iso3) |>
    dplyr::summarise(
      country_history_rows = dplyr::n(),
      country_history_first_year = min(.data$analysis_year),
      country_history_last_year = max(.data$analysis_year),
      country_mean_year_demeaned_residual_pp = mean(.data$pooled_residual_pp),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      country_shrinkage_weight = if (is.finite(shrinkage_k)) {
        .data$country_history_rows /
          (.data$country_history_rows + .env$shrinkage_k)
      } else {
        rep(0, dplyr::n())
      },
      country_history_adjustment_pp =
        .data$country_shrinkage_weight *
          .data$country_mean_year_demeaned_residual_pp
    )

  scored <- test |>
    dplyr::left_join(country_adjustments, by = "iso3") |>
    dplyr::mutate(
      country_history_rows = dplyr::coalesce(.data$country_history_rows, 0L),
      country_shrinkage_weight = dplyr::coalesce(
        .data$country_shrinkage_weight, 0
      ),
      country_history_adjustment_pp = dplyr::coalesce(
        .data$country_history_adjustment_pp, 0
      ),
      affine_prediction_pct = affine_test,
      history_prediction_pct =
        .data$affine_prediction_pct + .data$country_history_adjustment_pp
    )

  list(
    scored = scored,
    country_adjustments = country_adjustments,
    affine_intercept = unname(stats::coef(affine_fit)[[1]]),
    affine_slope = unname(stats::coef(affine_fit)[[2]]),
    country_sd = country_sd,
    residual_sd = residual_sd,
    shrinkage_k = shrinkage_k,
    training_rows = nrow(train),
    training_countries = dplyr::n_distinct(train$iso3),
    training_first_year = min(train$analysis_year),
    training_last_year = max(train$analysis_year)
  )
}

p15_build_rating_country_history_predictions <- function(
    sample, rolling_start_year = 2016L, fixed_test_start = 2020L) {
  add_fold <- function(train, test, validation_design, fold_id) {
    fit <- p15_fit_rating_country_history(train, test)
    model_predictions <- list(
      raw = fit$scored$rating_rate_pct,
      affine_rate = fit$scored$affine_prediction_pct,
      affine_year_demeaned_country_eb = fit$scored$history_prediction_pct
    )
    dplyr::bind_rows(lapply(names(model_predictions), function(model_id) {
      predicted <- model_predictions[[model_id]]
      history_model <- model_id == "affine_year_demeaned_country_eb"
      fit$scored |>
        dplyr::transmute(
          validation_design = .env$validation_design,
          fold_id = as.character(.env$fold_id),
          .data$analysis_year, .data$iso3, .data$country,
          .data$observed_rate_pct, .data$rating_rate_pct,
          model_id = .env$model_id,
          predicted_rate_pct = .env$predicted,
          residual_pp = .data$predicted_rate_pct - .data$observed_rate_pct,
          absolute_error_pp = abs(.data$residual_pp),
          squared_error_pp2 = .data$residual_pp^2,
          country_history_rows = if (.env$history_model) {
            .data$country_history_rows
          } else 0L,
          country_history_first_year = if (.env$history_model) {
            as.integer(.data$country_history_first_year)
          } else NA_integer_,
          country_history_last_year = if (.env$history_model) {
            as.integer(.data$country_history_last_year)
          } else NA_integer_,
          country_shrinkage_weight = if (.env$history_model) {
            .data$country_shrinkage_weight
          } else 0,
          country_history_adjustment_pp = if (.env$history_model) {
            .data$country_history_adjustment_pp
          } else 0,
          training_rows = fit$training_rows,
          training_countries = fit$training_countries,
          training_first_year = fit$training_first_year,
          training_last_year = fit$training_last_year,
          affine_intercept = fit$affine_intercept,
          affine_slope = fit$affine_slope,
          country_sd = fit$country_sd,
          residual_sd = fit$residual_sd,
          shrinkage_k = fit$shrinkage_k,
          schema_version = p15_rating_country_history_schema_version()
        )
    }))
  }

  fixed_train <- dplyr::filter(sample, .data$analysis_year < fixed_test_start)
  fixed_test <- dplyr::filter(sample, .data$analysis_year >= fixed_test_start)
  fixed <- add_fold(
    fixed_train, fixed_test, "fixed_future_2020_2024", "2020-2024"
  )
  expanding <- dplyr::bind_rows(lapply(
    seq.int(rolling_start_year, max(sample$analysis_year)),
    function(year) {
      add_fold(
        dplyr::filter(sample, .data$analysis_year < year),
        dplyr::filter(sample, .data$analysis_year == year),
        "expanding_past_only", year
      )
    }
  ))
  dplyr::bind_rows(fixed, expanding) |>
    dplyr::arrange(
      .data$validation_design, .data$fold_id, .data$model_id,
      .data$iso3, .data$analysis_year
    )
}

p15_summarise_rating_country_history <- function(predictions) {
  predictions |>
    dplyr::group_by(.data$validation_design, .data$model_id) |>
    dplyr::summarise(
      test_rows = dplyr::n(),
      test_countries = dplyr::n_distinct(.data$iso3),
      test_years = dplyr::n_distinct(.data$analysis_year),
      prior_country_history_share = if (dplyr::first(.data$model_id) ==
                                          "affine_year_demeaned_country_eb") {
        mean(.data$country_history_rows > 0)
      } else {
        NA_real_
      },
      mean_error_pp = mean(.data$residual_pp),
      mean_absolute_error_pp = mean(.data$absolute_error_pp),
      median_absolute_error_pp = stats::median(.data$absolute_error_pp),
      rmse_pp = sqrt(mean(.data$squared_error_pp2)),
      within_1pp_share = mean(.data$absolute_error_pp <= 1),
      within_2pp_share = mean(.data$absolute_error_pp <= 2),
      maximum_absolute_error_pp = max(.data$absolute_error_pp),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      schema_version = p15_rating_country_history_schema_version()
    )
}
