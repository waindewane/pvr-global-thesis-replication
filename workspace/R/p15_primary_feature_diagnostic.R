# Non-selecting diagnostic for feature-rich primary-market issues.

p15_primary_feature_diagnostic_schema <- function() {
  "SCHEMA-P15-PRIMARY-FEATURE-DIAGNOSTIC-V1"
}

p15_primary_feature_diagnostic_build <- function() {
  "BUILD-P15-PRIMARY-FEATURE-DIAGNOSTIC-20260814-V1"
}

p15_primary_feature_text <- function(instrument_universe) {
  tolower(paste(
    dplyr::coalesce(as.character(instrument_universe$RCSCouponTypeLeaf), ""),
    dplyr::coalesce(as.character(instrument_universe$CouponTypeDescription), ""),
    dplyr::coalesce(as.character(instrument_universe$DebtTypeDescription), ""),
    dplyr::coalesce(
      as.character(instrument_universe$InstrumentTypeDescription), ""
    ),
    dplyr::coalesce(as.character(instrument_universe$DocumentTitle), "")
  ))
}

p15_classify_primary_instrument_features <- function(instrument_universe) {
  required <- c(
    "snapshot_year", "issue_year", "canonical_iso3", "ISIN",
    "IsCallable", "IsPutable", "IsSinkable", "IsConvertible",
    "RCSCouponTypeLeaf", "CouponTypeDescription", "DebtTypeDescription",
    "InstrumentTypeDescription", "DocumentTitle"
  )
  missing <- setdiff(required, names(instrument_universe))
  if (length(missing)) {
    stop("Instrument universe missing feature columns: ",
         paste(missing, collapse = ", "), call. = FALSE)
  }
  text <- p15_primary_feature_text(instrument_universe)
  instrument_universe |>
    dplyr::mutate(
      analysis_year = as.integer(.data$snapshot_year),
      feature_callable = p15_observed_truthy(.data$IsCallable) |
        stringr::str_detect(text, "call"),
      feature_putable = p15_observed_truthy(.data$IsPutable) |
        stringr::str_detect(text, "put"),
      feature_sinkable = p15_observed_truthy(.data$IsSinkable) |
        stringr::str_detect(text, "sink"),
      feature_convertible = p15_observed_truthy(.data$IsConvertible) |
        stringr::str_detect(text, "convert"),
      feature_floating_or_variable = stringr::str_detect(
        text, "floating|variable|float rate|fixed margin"
      ),
      feature_zero_discount_or_strip = stringr::str_detect(
        text, "zero|discount|strip|interest only"
      ),
      feature_step_or_index_linked = stringr::str_detect(
        text, "step|index|inflation.linked|linked note"
      ),
      feature_sukuk = stringr::str_detect(text, "sukuk|islamic")
    ) |>
    dplyr::filter(
      .data$snapshot_year == .data$issue_year,
      !is.na(.data$canonical_iso3),
      !is.na(.data$ISIN), nzchar(.data$ISIN)
    ) |>
    dplyr::group_by(.data$analysis_year, .data$canonical_iso3, .data$ISIN) |>
    dplyr::summarise(
      dplyr::across(
        dplyr::starts_with("feature_"), ~ any(.x, na.rm = TRUE)
      ),
      .groups = "drop"
    )
}

p15_primary_issue_feature_diagnostic <- function(
    issue_evidence, instrument_universe) {
  required <- c(
    "analysis_year", "iso3", "country", "country_year_id",
    "historical_lmic_reporting_scope", "economic_issue_key",
    "representative_isins", "currency", "original_maturity_years",
    "original_issue_yield_pct", "face_issued_usd", "usd_weight_available",
    "identifier_yield_conflict", "bill_or_strip_flag", "central_bank_flag",
    "restructuring_flag", "nonstandard_feature_flag",
    "candidate_primary_standard", "candidate_primary_standard_50m"
  )
  missing <- setdiff(required, names(issue_evidence))
  if (length(missing)) {
    stop("Primary issue evidence missing columns: ",
         paste(missing, collapse = ", "), call. = FALSE)
  }
  feature_map <- p15_classify_primary_instrument_features(instrument_universe)
  issue_features <- issue_evidence |>
    dplyr::select(
      "analysis_year", "iso3", "economic_issue_key", "representative_isins"
    ) |>
    tidyr::separate_rows("representative_isins", sep = ";") |>
    dplyr::mutate(representative_isins = trimws(.data$representative_isins)) |>
    dplyr::filter(
      !is.na(.data$representative_isins), nzchar(.data$representative_isins)
    ) |>
    dplyr::left_join(
      feature_map,
      by = c(
        "analysis_year", "iso3" = "canonical_iso3",
        "representative_isins" = "ISIN"
      )
    ) |>
    dplyr::group_by(.data$analysis_year, .data$iso3, .data$economic_issue_key) |>
    dplyr::summarise(
      dplyr::across(
        dplyr::starts_with("feature_"), ~ any(.x, na.rm = TRUE)
      ),
      matched_feature_isins = sum(!is.na(.data$feature_callable)),
      .groups = "drop"
    )

  feature_columns <- grep("^feature_", names(feature_map), value = TRUE)
  joined <- issue_evidence |>
    dplyr::left_join(
      issue_features,
      by = c("analysis_year", "iso3", "economic_issue_key")
    ) |>
    dplyr::mutate(
      dplyr::across(
        dplyr::starts_with("feature_"),
        ~ dplyr::coalesce(.x, FALSE)
      ),
      matched_feature_isins = dplyr::coalesce(.data$matched_feature_isins, 0L),
      candidate_structural_prerequisites =
        is.finite(.data$original_issue_yield_pct) &
        .data$currency %in% c("USD", "EUR") &
        is.finite(.data$original_maturity_years) &
        .data$original_maturity_years >= 1 &
        .data$usd_weight_available &
        !.data$identifier_yield_conflict &
        !.data$bill_or_strip_flag & !.data$central_bank_flag,
      feature_rich_nonrestructuring_review =
        .data$candidate_structural_prerequisites &
        .data$nonstandard_feature_flag & !.data$restructuring_flag,
      candidate_primary_fixed_call_sink_review =
        .data$feature_rich_nonrestructuring_review &
        (.data$feature_callable | .data$feature_sinkable) &
        !.data$feature_putable & !.data$feature_convertible &
        !.data$feature_floating_or_variable &
        !.data$feature_zero_discount_or_strip &
        !.data$feature_step_or_index_linked,
      candidate_primary_fixed_call_sink_50m_review =
        .data$candidate_primary_fixed_call_sink_review &
        is.finite(.data$face_issued_usd) & .data$face_issued_usd >= 50000000
    )
  profiles <- apply(
    as.data.frame(joined[feature_columns]), 1,
    function(row) {
      labels <- sub("^feature_", "", names(row)[as.logical(row)])
      if (length(labels)) paste(labels, collapse = ";") else "unresolved_feature"
    }
  )
  joined |>
    dplyr::mutate(
      feature_profile = profiles,
      selected_for_ladder = FALSE,
      automatic_consequence = "none",
      decision_state = "diagnostic_pending_PRI_05",
      schema_version = p15_primary_feature_diagnostic_schema(),
      build_id = p15_primary_feature_diagnostic_build()
    ) |>
    dplyr::filter(
      .data$candidate_primary_standard |
        .data$feature_rich_nonrestructuring_review
    )
}

p15_compare_primary_fixed_call_sink_candidate <- function(issue_audit) {
  aggregate <- function(data, rate_name, issue_name, weight_name) {
    data |>
      dplyr::group_by(
        .data$analysis_year, .data$iso3, .data$country,
        .data$country_year_id, .data$historical_lmic_reporting_scope
      ) |>
      dplyr::summarise(
        !!rate_name := stats::weighted.mean(
          .data$original_issue_yield_pct, .data$face_issued_usd
        ),
        !!issue_name := dplyr::n(),
        !!weight_name := sum(.data$face_issued_usd),
        .groups = "drop"
      )
  }
  baseline <- issue_audit |>
    dplyr::filter(.data$candidate_primary_standard_50m) |>
    aggregate("baseline_rate_pct", "baseline_issue_count", "baseline_weight_usd")
  expanded <- issue_audit |>
    dplyr::filter(
      .data$candidate_primary_standard_50m |
        .data$candidate_primary_fixed_call_sink_50m_review
    ) |>
    aggregate("expanded_rate_pct", "expanded_issue_count", "expanded_weight_usd")
  dplyr::full_join(
    baseline, expanded,
    by = c(
      "analysis_year", "iso3", "country", "country_year_id",
      "historical_lmic_reporting_scope"
    )
  ) |>
    dplyr::mutate(
      coverage_state = dplyr::case_when(
        is.finite(.data$baseline_rate_pct) & is.finite(.data$expanded_rate_pct) ~
          "present_in_both",
        is.finite(.data$expanded_rate_pct) ~ "fixed_call_sink_only",
        is.finite(.data$baseline_rate_pct) ~ "baseline_only",
        TRUE ~ "absent_in_both"
      ),
      rate_difference_pp = .data$expanded_rate_pct - .data$baseline_rate_pct,
      absolute_rate_difference_pp = abs(.data$rate_difference_pp),
      selected_for_ladder = FALSE,
      automatic_consequence = "none",
      decision_state = "diagnostic_pending_PRI_05",
      schema_version = p15_primary_feature_diagnostic_schema(),
      build_id = p15_primary_feature_diagnostic_build()
    )
}

p15_summarise_primary_feature_diagnostic <- function(issue_audit) {
  feature_rows <- issue_audit |>
    dplyr::filter(.data$feature_rich_nonrestructuring_review)
  standard_country_years <- issue_audit |>
    dplyr::filter(.data$candidate_primary_standard) |>
    dplyr::distinct(.data$historical_lmic_reporting_scope, .data$country_year_id)
  feature_country_years <- feature_rows |>
    dplyr::distinct(
      .data$analysis_year, .data$iso3, .data$country, .data$country_year_id,
      .data$historical_lmic_reporting_scope
    ) |>
    dplyr::left_join(
      standard_country_years |>
        dplyr::mutate(current_standard_present = TRUE),
      by = c("historical_lmic_reporting_scope", "country_year_id")
    ) |>
    dplyr::mutate(
      current_standard_present = dplyr::coalesce(
        .data$current_standard_present, FALSE
      ),
      coverage_effect = dplyr::if_else(
        .data$current_standard_present,
        "supplements_existing_country_year",
        "feature_evidence_only_country_year"
      )
    )

  feature_columns <- setdiff(
    grep("^feature_", names(feature_rows), value = TRUE),
    c("feature_profile", "feature_rich_nonrestructuring_review")
  )
  by_feature <- dplyr::bind_rows(lapply(feature_columns, function(column) {
    feature_rows |>
      dplyr::filter(.data[[column]]) |>
      dplyr::summarise(
        feature_category = sub("^feature_", "", column),
        issues = dplyr::n(),
        country_years = dplyr::n_distinct(.data$country_year_id),
        lmic_issues = sum(.data$historical_lmic_reporting_scope),
        lmic_country_years = dplyr::n_distinct(
          .data$country_year_id[.data$historical_lmic_reporting_scope]
        )
      )
  }))

  fixed_call_sink_comparison <-
    p15_compare_primary_fixed_call_sink_candidate(issue_audit)
  fixed_call_sink_lmic <- fixed_call_sink_comparison |>
    dplyr::filter(.data$historical_lmic_reporting_scope)

  overall <- tibble::tibble(
    standard_lmic_issues = sum(
      issue_audit$candidate_primary_standard &
        issue_audit$historical_lmic_reporting_scope
    ),
    standard_lmic_country_years = dplyr::n_distinct(
      issue_audit$country_year_id[
        issue_audit$candidate_primary_standard &
          issue_audit$historical_lmic_reporting_scope
      ]
    ),
    feature_review_lmic_issues = sum(
      issue_audit$feature_rich_nonrestructuring_review &
        issue_audit$historical_lmic_reporting_scope
    ),
    feature_review_lmic_country_years = dplyr::n_distinct(
      issue_audit$country_year_id[
        issue_audit$feature_rich_nonrestructuring_review &
          issue_audit$historical_lmic_reporting_scope
      ]
    ),
    feature_only_lmic_country_years = sum(
      feature_country_years$historical_lmic_reporting_scope &
        !feature_country_years$current_standard_present
    ),
    fixed_call_sink_lmic_issues = sum(
      issue_audit$candidate_primary_fixed_call_sink_review &
        issue_audit$historical_lmic_reporting_scope
    ),
    fixed_call_sink_lmic_country_years = dplyr::n_distinct(
      issue_audit$country_year_id[
        issue_audit$candidate_primary_fixed_call_sink_review &
          issue_audit$historical_lmic_reporting_scope
      ]
    ),
    fixed_call_sink_50m_expanded_lmic_country_years = sum(
      is.finite(fixed_call_sink_lmic$expanded_rate_pct)
    ),
    fixed_call_sink_50m_added_lmic_country_years = sum(
      fixed_call_sink_lmic$coverage_state == "fixed_call_sink_only"
    ),
    fixed_call_sink_50m_changes_over_0_25pp = sum(
      fixed_call_sink_lmic$absolute_rate_difference_pp > 0.25,
      na.rm = TRUE
    ),
    fixed_call_sink_50m_maximum_change_pp = max(
      fixed_call_sink_lmic$absolute_rate_difference_pp, na.rm = TRUE
    ),
    selected_for_ladder = FALSE,
    decision_state = "diagnostic_pending_PRI_05",
    schema_version = p15_primary_feature_diagnostic_schema(),
    build_id = p15_primary_feature_diagnostic_build()
  )
  list(
    overall = overall,
    by_feature = by_feature,
    country_year_coverage = feature_country_years,
    fixed_call_sink_comparison = fixed_call_sink_comparison
  )
}
