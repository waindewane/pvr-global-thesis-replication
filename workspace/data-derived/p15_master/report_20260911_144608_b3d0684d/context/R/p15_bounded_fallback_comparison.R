# Bounded P15 rating, peer, and IDS comparison helpers.
#
# These functions prepare decision evidence. They do not promote a rating method,
# peer formula, ladder order, or canonical benchmark.

p15_bounded_fallback_schema_version <- function() {
  "SCHEMA-P15-BOUNDED-FALLBACK-COMPARISON-V1"
}

p15_rating_notch_number <- function(rating) {
  scale <- c(
    "Aaa", "Aa1", "Aa2", "Aa3", "A1", "A2", "A3", "Baa1", "Baa2",
    "Baa3", "Ba1", "Ba2", "Ba3", "B1", "B2", "B3", "Caa1", "Caa2",
    "Caa3", "Ca", "C"
  )
  match(as.character(rating), scale)
}

p15_clustered_mean_difference <- function(
    data, difference_col, country_col = "iso3", year_col = "analysis_year") {
  required <- c(difference_col, country_col, year_col)
  missing <- setdiff(required, names(data))
  if (length(missing)) {
    stop("Difference input is missing: ", paste(missing, collapse = ", "),
         call. = FALSE)
  }
  x <- data |>
    dplyr::transmute(
      difference = as.numeric(.data[[difference_col]]),
      country = as.character(.data[[country_col]]),
      year = as.integer(.data[[year_col]])
    ) |>
    dplyr::filter(
      is.finite(.data$difference), !is.na(.data$country), !is.na(.data$year)
    )
  if (nrow(x) < 2L || dplyr::n_distinct(x$country) < 2L ||
      dplyr::n_distinct(x$year) < 2L) {
    return(tibble::tibble(
      matched_rows = nrow(x), countries = dplyr::n_distinct(x$country),
      years = dplyr::n_distinct(x$year), mean_difference_pp = NA_real_,
      standard_error_pp = NA_real_, t_statistic = NA_real_,
      degrees_of_freedom = NA_real_, p_value_two_sided = NA_real_,
      confidence_interval_95_lower_pp = NA_real_,
      confidence_interval_95_upper_pp = NA_real_
    ))
  }
  fit <- stats::lm(difference ~ 1, data = x)
  variance <- sandwich::vcovCL(
    fit, cluster = list(x$country, x$year), type = "HC1", multi0 = TRUE
  )
  estimate <- unname(stats::coef(fit)[[1]])
  standard_error <- sqrt(as.numeric(variance[[1, 1]]))
  degrees_of_freedom <- min(
    dplyr::n_distinct(x$country), dplyr::n_distinct(x$year)
  ) - 1L
  statistic <- estimate / standard_error
  critical <- stats::qt(0.975, degrees_of_freedom)
  tibble::tibble(
    matched_rows = nrow(x),
    countries = dplyr::n_distinct(x$country),
    years = dplyr::n_distinct(x$year),
    mean_difference_pp = estimate,
    standard_error_pp = standard_error,
    t_statistic = statistic,
    degrees_of_freedom = degrees_of_freedom,
    p_value_two_sided = 2 * stats::pt(
      abs(statistic), degrees_of_freedom, lower.tail = FALSE
    ),
    confidence_interval_95_lower_pp = estimate - critical * standard_error,
    confidence_interval_95_upper_pp = estimate + critical * standard_error
  )
}

p15_peer_similarity_pool <- function(eligible, target, minimum_count = 3L) {
  if (!nrow(eligible)) {
    return(list(pool = eligible, rule = "no_eligible_peers"))
  }
  target_notch <- p15_rating_notch_number(target$moodys_rating_normalized[[1]])
  peer_notch <- p15_rating_notch_number(eligible$moodys_rating_normalized)
  same_income <- !is.na(eligible$historical_income_level) &
    eligible$historical_income_level == target$historical_income_level[[1]]
  same_region <- !is.na(eligible$rating_source_region) &
    eligible$rating_source_region == target$rating_source_region[[1]]
  rating_close <- is.finite(peer_notch) & is.finite(target_notch) &
    abs(peer_notch - target_notch) <= 3L

  rules <- list(
    same_income_region_rating3 = same_income & same_region & rating_close,
    same_income_rating3 = same_income & rating_close,
    same_region_rating3 = same_region & rating_close,
    rating3 = rating_close,
    same_income_region = same_income & same_region,
    same_income = same_income,
    same_region = same_region,
    global = rep(TRUE, nrow(eligible))
  )
  for (rule in names(rules)) {
    pool <- eligible[rules[[rule]] %in% TRUE, , drop = FALSE]
    if (nrow(pool) >= minimum_count || rule == "global") {
      return(list(pool = pool, rule = paste0(rule, "_min", minimum_count)))
    }
  }
  stop("Unreachable peer-pool state.", call. = FALSE)
}

p15_build_bounded_peer_variants <- function(
    primary_usd_anchors, rating_context, minimum_count = 3L) {
  required_anchor <- c(
    "analysis_year", "iso3", "country", "historical_income_level",
    "historical_lmic_reporting_scope", "market_rate_pct",
    "market_maturity_years", "sovereign_spread_pct"
  )
  missing_anchor <- setdiff(required_anchor, names(primary_usd_anchors))
  if (length(missing_anchor)) {
    stop("Primary anchors are missing: ", paste(missing_anchor, collapse = ", "),
         call. = FALSE)
  }
  required_rating <- c(
    "analysis_year", "iso3", "rating_source_region",
    "moodys_rating_normalized", "risk_free_7y_pct"
  )
  missing_rating <- setdiff(required_rating, names(rating_context))
  if (length(missing_rating)) {
    stop("Rating context is missing: ", paste(missing_rating, collapse = ", "),
         call. = FALSE)
  }
  anchors <- primary_usd_anchors |>
    dplyr::left_join(
      rating_context |>
        dplyr::select(dplyr::all_of(required_rating)),
      by = c("analysis_year", "iso3")
    )
  if (anyDuplicated(anchors[c("analysis_year", "iso3")])) {
    stop("Primary anchors must be unique by country-year.", call. = FALSE)
  }

  detail_parts <- list()
  member_parts <- list()
  detail_index <- 0L
  member_index <- 0L
  for (target in split(anchors, seq_len(nrow(anchors)))) {
    eligible <- anchors |>
      dplyr::filter(
        .data$analysis_year == target$analysis_year[[1]],
        .data$iso3 != target$iso3[[1]],
        is.finite(.data$market_rate_pct),
        is.finite(.data$sovereign_spread_pct)
      )
    same_income <- eligible |>
      dplyr::filter(
        !is.na(.data$historical_income_level),
        .data$historical_income_level == target$historical_income_level[[1]]
      )
    raw_pool <- if (nrow(same_income) >= minimum_count) same_income else eligible
    raw_rule <- if (nrow(same_income) >= minimum_count) {
      paste0("same_historical_income_min", minimum_count)
    } else {
      paste0("global_fallback_after_income_min", minimum_count)
    }
    similarity <- p15_peer_similarity_pool(eligible, target, minimum_count)

    variants <- list(
      current_same_income_raw_yield = list(
        pool = raw_pool, rule = raw_rule,
        value = stats::median(raw_pool$market_rate_pct, na.rm = TRUE),
        value_component = "peer_raw_yield_pct"
      ),
      same_income_spread_first_7y = list(
        pool = raw_pool, rule = raw_rule,
        value = target$risk_free_7y_pct[[1]] +
          stats::median(raw_pool$sovereign_spread_pct, na.rm = TRUE),
        value_component = "peer_maturity_matched_spread_plus_target_year_dgs7"
      ),
      observable_similarity_raw_yield = list(
        pool = similarity$pool, rule = similarity$rule,
        value = stats::median(similarity$pool$market_rate_pct, na.rm = TRUE),
        value_component = "similarity_pool_raw_yield_pct"
      )
    )
    for (variant_id in names(variants)) {
      item <- variants[[variant_id]]
      pool <- item$pool
      detail_index <- detail_index + 1L
      detail_parts[[detail_index]] <- tibble::tibble(
        analysis_year = as.integer(target$analysis_year[[1]]),
        iso3 = as.character(target$iso3[[1]]),
        country = as.character(target$country[[1]]),
        historical_income_level = as.character(
          target$historical_income_level[[1]]
        ),
        historical_lmic_reporting_scope = as.logical(
          target$historical_lmic_reporting_scope[[1]]
        ),
        target_moodys_rating_normalized = as.character(
          target$moodys_rating_normalized[[1]]
        ),
        target_rating_source_region = as.character(
          target$rating_source_region[[1]]
        ),
        target_risk_free_7y_pct = as.numeric(target$risk_free_7y_pct[[1]]),
        anchor_rate_pct = as.numeric(target$market_rate_pct[[1]]),
        anchor_maturity_years = as.numeric(target$market_maturity_years[[1]]),
        peer_method_id = variant_id,
        peer_pool_rule = item$rule,
        peer_country_count = nrow(pool),
        peer_country_list = paste(sort(pool$iso3), collapse = ";"),
        peer_rate_pct = as.numeric(item$value),
        peer_rate_component = item$value_component,
        peer_raw_rate_median_pct = stats::median(
          pool$market_rate_pct, na.rm = TRUE
        ),
        peer_sovereign_spread_median_pct = stats::median(
          pool$sovereign_spread_pct, na.rm = TRUE
        ),
        peer_rate_iqr_width_pct = diff(stats::quantile(
          pool$market_rate_pct, c(0.25, 0.75), names = FALSE, na.rm = TRUE
        )),
        signed_gap_pp = as.numeric(item$value) - target$market_rate_pct[[1]],
        abs_gap_pp = abs(as.numeric(item$value) - target$market_rate_pct[[1]]),
        target_excluded = !target$iso3[[1]] %in% pool$iso3,
        nonrecursive_observed_primary_pool = TRUE,
        comparison_state = "bounded_diagnostic_not_approved_for_selection",
        schema_version = p15_bounded_fallback_schema_version()
      )
      if (nrow(pool)) {
        member_index <- member_index + 1L
        member_parts[[member_index]] <- pool |>
          dplyr::transmute(
            analysis_year = as.integer(target$analysis_year[[1]]),
            target_iso3 = as.character(target$iso3[[1]]),
            peer_method_id = variant_id,
            peer_pool_rule = item$rule,
            peer_iso3 = .data$iso3,
            peer_country = .data$country,
            peer_income_level = .data$historical_income_level,
            peer_region = .data$rating_source_region,
            peer_moodys_rating_normalized = .data$moodys_rating_normalized,
            peer_observed_rate_pct = .data$market_rate_pct,
            peer_sovereign_spread_pct = .data$sovereign_spread_pct,
            target_excluded = .data$iso3 != target$iso3[[1]],
            nonrecursive_observed_primary_seed = TRUE,
            schema_version = p15_bounded_fallback_schema_version()
          )
      }
    }
  }
  detail <- dplyr::bind_rows(detail_parts) |>
    dplyr::arrange(.data$peer_method_id, .data$analysis_year, .data$iso3)
  members <- dplyr::bind_rows(member_parts) |>
    dplyr::arrange(
      .data$peer_method_id, .data$analysis_year,
      .data$target_iso3, .data$peer_iso3
    )
  stopifnot(
    nrow(detail) == 3L * nrow(anchors),
    !anyDuplicated(detail[c("peer_method_id", "analysis_year", "iso3")]),
    all(detail$target_excluded),
    all(members$target_excluded)
  )
  list(detail = detail, members = members)
}

p15_summarise_bounded_method <- function(data, method_col) {
  data |>
    dplyr::filter(is.finite(.data$peer_rate_pct), is.finite(.data$anchor_rate_pct)) |>
    dplyr::group_by(.data[[method_col]]) |>
    dplyr::summarise(
      matched_rows = dplyr::n(),
      countries = dplyr::n_distinct(.data$iso3),
      years = dplyr::n_distinct(.data$analysis_year),
      mean_signed_gap_pp = mean(.data$signed_gap_pp),
      median_signed_gap_pp = stats::median(.data$signed_gap_pp),
      mean_abs_gap_pp = mean(.data$abs_gap_pp),
      median_abs_gap_pp = stats::median(.data$abs_gap_pp),
      rmse_gap_pp = sqrt(mean(.data$signed_gap_pp^2)),
      within_1pp_share = mean(.data$abs_gap_pp <= 1),
      within_2pp_share = mean(.data$abs_gap_pp <= 2),
      median_peer_count = stats::median(.data$peer_country_count),
      mean_peer_iqr_width_pp = mean(.data$peer_rate_iqr_width_pct),
      .groups = "drop"
    )
}
