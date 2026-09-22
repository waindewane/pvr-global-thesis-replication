# Inference for the mean signed error of a matched rating-implied candidate.
# The main panel result uses two-way country/year clustered standard errors.

p15_rating_mean_error_inference_schema <- function() {
  "SCHEMA-P15-RATING-MEAN-ERROR-INFERENCE-V1"
}

p15_build_rating_mean_error_inference <- function(
    data, error_col = "signed_gap_pp", country_col = "iso3",
    year_col = "analysis_year") {
  required <- c(error_col, country_col, year_col)
  missing <- setdiff(required, names(data))
  if (length(missing)) {
    stop("Mean-error input is missing: ", paste(missing, collapse = ", "),
         call. = FALSE)
  }
  x <- data |>
    dplyr::transmute(
      error = as.numeric(.data[[error_col]]),
      country = as.character(.data[[country_col]]),
      year = as.integer(.data[[year_col]])
    ) |>
    dplyr::filter(is.finite(.data$error), !is.na(.data$country),
                  !is.na(.data$year))
  if (nrow(x) < 2L || dplyr::n_distinct(x$country) < 2L ||
      dplyr::n_distinct(x$year) < 2L) {
    stop("Mean-error inference requires repeated countries and years.",
         call. = FALSE)
  }

  fit <- stats::lm(error ~ 1, data = x)
  estimate <- unname(stats::coef(fit)[[1]])
  countries <- dplyr::n_distinct(x$country)
  years <- dplyr::n_distinct(x$year)

  make_row <- function(method, variance, df, estimand, preferred = FALSE) {
    standard_error <- sqrt(as.numeric(variance[[1, 1]]))
    statistic <- estimate / standard_error
    critical <- stats::qt(0.975, df = df)
    tibble::tibble(
      inference_method = method,
      estimand = estimand,
      matched_country_years = nrow(x),
      country_clusters = countries,
      year_clusters = years,
      mean_signed_error_pp = estimate,
      standard_error_pp = standard_error,
      t_statistic = statistic,
      degrees_of_freedom = df,
      p_value_two_sided = 2 * stats::pt(
        abs(statistic), df = df, lower.tail = FALSE
      ),
      confidence_interval_95_lower_pp = estimate - critical * standard_error,
      confidence_interval_95_upper_pp = estimate + critical * standard_error,
      preferred_panel_inference = preferred,
      null_hypothesis = "mean_signed_error_equals_zero",
      interpretation_scope =
        "tests average directional bias; does not test individual prediction equality",
      schema_version = p15_rating_mean_error_inference_schema()
    )
  }

  ordinary <- make_row(
    "ordinary_paired_t",
    stats::vcov(fit), stats::df.residual(fit),
    "country_year_weighted_mean_signed_error"
  )
  country_clustered <- make_row(
    "country_clustered",
    sandwich::vcovCL(fit, cluster = x$country, type = "HC1"),
    countries - 1L,
    "country_year_weighted_mean_signed_error"
  )
  year_clustered <- make_row(
    "year_clustered",
    sandwich::vcovCL(fit, cluster = x$year, type = "HC1"),
    years - 1L,
    "country_year_weighted_mean_signed_error"
  )
  two_way <- make_row(
    "two_way_country_year_clustered",
    sandwich::vcovCL(
      fit, cluster = list(x$country, x$year), type = "HC1", multi0 = TRUE
    ),
    min(countries, years) - 1L,
    "country_year_weighted_mean_signed_error",
    preferred = TRUE
  )

  country_means <- x |>
    dplyr::group_by(.data$country) |>
    dplyr::summarise(error = mean(.data$error), .groups = "drop")
  country_test <- stats::t.test(country_means$error, mu = 0)
  equal_country <- tibble::tibble(
    inference_method = "equal_country_weight_sensitivity",
    estimand = "equal_country_weight_mean_of_country_mean_errors",
    matched_country_years = nrow(x),
    country_clusters = countries,
    year_clusters = years,
    mean_signed_error_pp = unname(country_test$estimate),
    standard_error_pp = unname(country_test$stderr),
    t_statistic = unname(country_test$statistic),
    degrees_of_freedom = unname(country_test$parameter),
    p_value_two_sided = country_test$p.value,
    confidence_interval_95_lower_pp = country_test$conf.int[[1]],
    confidence_interval_95_upper_pp = country_test$conf.int[[2]],
    preferred_panel_inference = FALSE,
    null_hypothesis = "mean_signed_error_equals_zero",
    interpretation_scope =
      "country-weighted sensitivity; not the reported country-year mean",
    schema_version = p15_rating_mean_error_inference_schema()
  )

  dplyr::bind_rows(
    ordinary, country_clustered, year_clustered, two_way, equal_country
  )
}
