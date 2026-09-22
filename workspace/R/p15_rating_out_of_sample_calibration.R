# Out-of-sample calibration helpers for the leading rating candidate.

p15_rating_oos_schema_version <- function() {
  "SCHEMA-P15-RATING-OOS-CALIBRATION-V1"
}

p15_fit_rating_calibration <- function(train, test,
                                       method = c("affine_rate", "spread_component")) {
  method <- match.arg(method)
  if (method == "affine_rate") {
    fit <- stats::lm(observed_rate_pct ~ rating_rate_pct, data = train)
    prediction <- as.numeric(stats::predict(fit, newdata = test))
  } else {
    fit <- stats::lm(
      I(observed_rate_pct - risk_free_rate_pct) ~ rating_spread_pct,
      data = train
    )
    prediction <- test$risk_free_rate_pct +
      as.numeric(stats::predict(fit, newdata = test))
  }
  list(
    prediction = prediction,
    intercept = unname(stats::coef(fit)[[1]]),
    slope = unname(stats::coef(fit)[[2]])
  )
}

p15_build_rating_oos_predictions <- function(sample,
                                              rolling_start_year = 2016L,
                                              fixed_test_start = 2020L) {
  required <- c(
    "analysis_year", "iso3", "country", "observed_rate_pct",
    "rating_rate_pct", "risk_free_rate_pct", "rating_spread_pct"
  )
  missing <- setdiff(required, names(sample))
  if (length(missing)) stop("OOS sample is missing: ", paste(missing, collapse = ", "))

  add_models <- function(train, test, design_id, fold_id) {
    models <- c("raw", "affine_rate", "spread_component")
    rows <- lapply(models, function(model_id) {
      if (model_id == "raw") {
        pred <- test$rating_rate_pct
        intercept <- 0
        slope <- 1
      } else {
        fit <- p15_fit_rating_calibration(train, test, model_id)
        pred <- fit$prediction
        intercept <- fit$intercept
        slope <- fit$slope
      }
      test |>
        dplyr::transmute(
          analysis_year, iso3, country, observed_rate_pct, rating_rate_pct,
          risk_free_rate_pct, rating_spread_pct,
          validation_design = design_id,
          fold_id = as.character(fold_id),
          model_id = model_id,
          predicted_rate_pct = pred,
          residual_pp = pred - observed_rate_pct,
          absolute_error_pp = abs(.data$residual_pp),
          squared_error_pp2 = .data$residual_pp^2,
          training_rows = nrow(train),
          training_countries = dplyr::n_distinct(train$iso3),
          training_first_year = min(train$analysis_year),
          training_last_year = max(train$analysis_year),
          calibration_intercept = intercept,
          calibration_slope = slope,
          schema_version = p15_rating_oos_schema_version()
        )
    })
    dplyr::bind_rows(rows)
  }

  fixed_train <- dplyr::filter(sample, .data$analysis_year < fixed_test_start)
  fixed_test <- dplyr::filter(sample, .data$analysis_year >= fixed_test_start)
  fixed <- add_models(
    fixed_train, fixed_test, "fixed_future_2020_2024", "2020-2024"
  )

  rolling <- dplyr::bind_rows(lapply(
    seq.int(rolling_start_year, max(sample$analysis_year)),
    function(year) {
      train <- dplyr::filter(sample, .data$analysis_year < year)
      test <- dplyr::filter(sample, .data$analysis_year == year)
      add_models(train, test, "expanding_past_only", as.character(year))
    }
  ))

  country <- dplyr::bind_rows(lapply(sort(unique(sample$iso3)), function(code) {
    train <- dplyr::filter(sample, .data$iso3 != code)
    test <- dplyr::filter(sample, .data$iso3 == code)
    add_models(train, test, "leave_one_country_out", code)
  }))

  dplyr::bind_rows(fixed, rolling, country) |>
    dplyr::arrange(.data$validation_design, .data$fold_id, .data$model_id,
                   .data$iso3, .data$analysis_year)
}

p15_summarise_rating_oos <- function(predictions) {
  predictions |>
    dplyr::group_by(.data$validation_design, .data$model_id) |>
    dplyr::summarise(
      test_rows = dplyr::n(),
      test_countries = dplyr::n_distinct(.data$iso3),
      test_years = dplyr::n_distinct(.data$analysis_year),
      mean_error_pp = mean(.data$residual_pp),
      mean_absolute_error_pp = mean(.data$absolute_error_pp),
      median_absolute_error_pp = stats::median(.data$absolute_error_pp),
      rmse_pp = sqrt(mean(.data$squared_error_pp2)),
      within_1pp_share = mean(.data$absolute_error_pp <= 1),
      within_2pp_share = mean(.data$absolute_error_pp <= 2),
      maximum_absolute_error_pp = max(.data$absolute_error_pp),
      .groups = "drop"
    ) |>
    dplyr::mutate(schema_version = p15_rating_oos_schema_version())
}

p15_rating_oos_paired_inference <- function(predictions) {
  raw <- predictions |>
    dplyr::filter(.data$model_id == "raw") |>
    dplyr::select(
      "validation_design", "analysis_year", "iso3",
      raw_absolute_error_pp = "absolute_error_pp"
    )
  compared <- predictions |>
    dplyr::filter(.data$model_id != "raw") |>
    dplyr::left_join(raw, by = c("validation_design", "analysis_year", "iso3")) |>
    dplyr::mutate(
      absolute_error_change_pp =
        .data$absolute_error_pp - .data$raw_absolute_error_pp
    )
  keys <- unique(compared[c("validation_design", "model_id")])
  dplyr::bind_rows(lapply(seq_len(nrow(keys)), function(i) {
    x <- dplyr::semi_join(compared, keys[i, ], by = c("validation_design", "model_id"))
    fit <- stats::lm(absolute_error_change_pp ~ 1, data = x)
    vc <- sandwich::vcovCL(
      fit, cluster = list(x$iso3, x$analysis_year), type = "HC1", multi0 = TRUE
    )
    estimate <- unname(stats::coef(fit)[[1]])
    se <- sqrt(vc[[1]])
    df <- min(dplyr::n_distinct(x$iso3), dplyr::n_distinct(x$analysis_year)) - 1L
    tibble::tibble(
      validation_design = keys$validation_design[[i]],
      model_id = keys$model_id[[i]],
      mean_absolute_error_change_pp = estimate,
      two_way_clustered_standard_error = se,
      statistic = estimate / se,
      degrees_of_freedom = df,
      p_value = 2 * stats::pt(abs(estimate / se), df = df, lower.tail = FALSE),
      improved_observation_share = mean(x$absolute_error_change_pp < 0),
      interpretation = "negative_change_favors_calibration",
      schema_version = p15_rating_oos_schema_version()
    )
  }))
}

