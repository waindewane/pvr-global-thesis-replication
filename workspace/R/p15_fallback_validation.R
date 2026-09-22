# P15 question-specific fallback validation helpers.
#
# These functions classify the authority of integrated observed anchors and
# summarize matched fallback comparisons. They prepare a decision surface; they
# do not approve a fallback estimator, source rank, selected ladder, or headline
# use.

p15_fallback_validation_schema_version <- function() {
  "SCHEMA-P15-FALLBACK-VALIDATION-V1"
}

p15_fallback_safe_quantile <- function(x, probability) {
  x <- as.numeric(x)
  x <- x[is.finite(x)]
  if (!length(x)) return(NA_real_)
  as.numeric(stats::quantile(
    x, probability, names = FALSE, na.rm = TRUE, type = 7
  ))
}

p15_expand_validation_anchor_authority <- function(anchors) {
  required <- c(
    "analysis_year", "iso3", "country", "country_year_id", "currency",
    "observed_market_branch", "candidate_evidence_tier", "market_rate_pct",
    "market_maturity_years", "thin_evidence", "evidence_strength_label",
    "source_evidence_row_id", "historical_lmic_reporting_scope",
    "approved_status_rule_class", "timing_object"
  )
  missing <- setdiff(required, names(anchors))
  if (length(missing)) {
    stop(
      "Integrated validation anchors are missing: ",
      paste(missing, collapse = ", "), call. = FALSE
    )
  }
  if (anyDuplicated(anchors[c(
    "analysis_year", "iso3", "observed_market_branch", "currency"
  )])) {
    stop("Integrated anchors are not unique by branch and currency.",
         call. = FALSE)
  }

  base <- anchors |>
    dplyr::transmute(
      analysis_year = as.integer(.data$analysis_year),
      iso3 = as.character(.data$iso3),
      country = as.character(.data$country),
      country_year_id = as.character(.data$country_year_id),
      historical_lmic_reporting_scope = as.logical(
        .data$historical_lmic_reporting_scope
      ),
      anchor_currency = as.character(.data$currency),
      anchor_branch = as.character(.data$observed_market_branch),
      anchor_evidence_tier = as.character(.data$candidate_evidence_tier),
      anchor_rate_pct = as.numeric(.data$market_rate_pct),
      anchor_maturity_years = as.numeric(.data$market_maturity_years),
      anchor_thin_evidence = as.logical(.data$thin_evidence),
      anchor_evidence_strength = as.character(.data$evidence_strength_label),
      anchor_status_rule_class = as.character(.data$approved_status_rule_class),
      anchor_timing_object = as.character(.data$timing_object),
      anchor_source_evidence_row_id = as.character(
        .data$source_evidence_row_id
      )
    )

  make_question <- function(
      data, question_id, question_label, target_object,
      authority_class, authority_reason, level_comparability,
      timing_comparability, use_in_provisional_summary) {
    data |>
      dplyr::mutate(
        validation_question_id = question_id,
        validation_question = question_label,
        validation_target_object = target_object,
        authority_class = authority_class,
        authority_reason = authority_reason,
        level_comparability = level_comparability,
        timing_comparability = timing_comparability,
        use_in_provisional_summary = use_in_provisional_summary,
        authority_decision_state =
          "provisional_proposal_pending_owner_review",
        fallback_validation_schema_version =
          p15_fallback_validation_schema_version()
      )
  }

  usd_new_borrowing <- make_question(
    base,
    "Q-USD-NEW-BORROWING",
    "How closely does a fallback approximate observed USD sovereign borrowing costs in the issuance year?",
    "annual observed USD primary issuance cost",
    dplyr::case_when(
      base$anchor_branch == "observed_primary" &
        base$anchor_currency == "USD" ~
        "preferred_provisional_accuracy_anchor",
      base$anchor_branch == "observed_secondary" &
        base$anchor_currency == "USD" ~
        "different_market_timing_object_support_only",
      base$anchor_currency == "EUR" ~
        "currency_specific_evidence_not_usd_level_comparable",
      TRUE ~ "not_authorized_for_this_question"
    ),
    dplyr::case_when(
      base$anchor_branch == "observed_primary" &
        base$anchor_currency == "USD" ~
        "Directly observed USD issuance yields are the closest available country-year evidence for new sovereign borrowing costs.",
      base$anchor_branch == "observed_secondary" &
        base$anchor_currency == "USD" ~
        "USD secondary yields measure year-end pricing of outstanding debt, not the issuance-year borrowing cost.",
      base$anchor_currency == "EUR" ~
        "Nominal EUR yields include the EUR rate environment and cannot validate a USD level estimate without normalization.",
      TRUE ~ "The observed object does not match the validation target."
    ),
    dplyr::if_else(
      base$anchor_currency == "USD", "same_currency_usd",
      "currency_mismatch_for_usd_level"
    ),
    dplyr::case_when(
      base$anchor_branch == "observed_primary" ~
        "annual_primary_aggregate_best_available_not_exact_date_match",
      TRUE ~ "year_end_secondary_not_primary_timing"
    ),
    base$anchor_branch == "observed_primary" & base$anchor_currency == "USD"
  )

  secondary_is_direct <- !grepl(
    "price_derived", base$anchor_evidence_tier, ignore.case = TRUE
  )
  usd_year_end <- make_question(
    base,
    "Q-USD-YEAR-END-MARKET",
    "How closely does a fallback approximate the USD sovereign market rate at year-end?",
    "year-end observed USD secondary-market yield",
    dplyr::case_when(
      base$anchor_branch == "observed_secondary" &
        base$anchor_currency == "USD" & secondary_is_direct ~
        "preferred_provisional_accuracy_anchor",
      base$anchor_branch == "observed_secondary" &
        base$anchor_currency == "USD" & !secondary_is_direct ~
        "supporting_reconstructed_secondary_anchor",
      base$anchor_branch == "observed_primary" &
        base$anchor_currency == "USD" ~
        "different_market_timing_object_support_only",
      base$anchor_currency == "EUR" ~
        "currency_specific_evidence_not_usd_level_comparable",
      TRUE ~ "not_authorized_for_this_question"
    ),
    dplyr::case_when(
      base$anchor_branch == "observed_secondary" &
        base$anchor_currency == "USD" & secondary_is_direct ~
        "Directly reported USD secondary YTM is the closest available year-end market-price anchor.",
      base$anchor_branch == "observed_secondary" &
        base$anchor_currency == "USD" & !secondary_is_direct ~
        "Strict price-derived USD YTM is usable as lower-ranked supporting evidence, but it is not equivalent to a directly reported YTM.",
      base$anchor_branch == "observed_primary" &
        base$anchor_currency == "USD" ~
        "Primary issuance aggregates measure borrowing during the year, not the year-end market state.",
      base$anchor_currency == "EUR" ~
        "Nominal EUR yields cannot validate a USD level estimate without normalization.",
      TRUE ~ "The observed object does not match the validation target."
    ),
    dplyr::if_else(
      base$anchor_currency == "USD", "same_currency_usd",
      "currency_mismatch_for_usd_level"
    ),
    dplyr::case_when(
      base$anchor_branch == "observed_secondary" ~
        "year_end_window_candidate_pending_final_SEC_18",
      TRUE ~ "primary_issuance_timing_not_year_end_market_state"
    ),
    base$anchor_branch == "observed_secondary" &
      base$anchor_currency == "USD"
  )

  ids_consistency <- make_question(
    base,
    "Q-IDS-SOURCE-CONSISTENCY",
    "How does the IDS Bondholders contractual average differ from observed market evidence?",
    "source-object consistency rather than estimator accuracy",
    "descriptive_source_object_overlap_only",
    paste0(
      "IDS and observed primary are both amount-weighted country-year borrowing-",
      "cost aggregates, but contractual interest and security-level issue yield, ",
      "currency observability, and timing remain distinct; the observed anchor is ",
      "used for comparability analysis and is not treated as ground truth."
    ),
    "currency_and_instrument_mix_not_fully_aligned",
    "annual_contractual_average_vs_observed_market_object",
    TRUE
  )

  coverage <- make_question(
    base,
    "Q-COVERAGE-OVERLAP",
    "Where do fallback and observed evidence overlap?",
    "coverage and missingness only",
    "authoritative_for_observed_coverage_only",
    "Every retained integrated anchor is valid evidence that an observed branch/currency rate exists for coverage accounting.",
    "not_applicable_to_coverage",
    "not_applicable_to_coverage",
    TRUE
  )

  out <- dplyr::bind_rows(
    usd_new_borrowing, usd_year_end, ids_consistency, coverage
  ) |>
    dplyr::arrange(
      .data$validation_question_id, .data$analysis_year, .data$iso3,
      .data$anchor_branch, .data$anchor_currency
    )
  stopifnot(nrow(out) == 4L * nrow(base))
  out
}

p15_summarise_fallback_gaps <- function(data, group_cols) {
  required <- c(
    group_cols, "signed_gap_pp", "analysis_year", "iso3",
    "historical_lmic_reporting_scope"
  )
  missing <- setdiff(required, names(data))
  if (length(missing)) {
    stop("Fallback validation data are missing: ",
         paste(missing, collapse = ", "), call. = FALSE)
  }
  data |>
    dplyr::filter(is.finite(.data$signed_gap_pp)) |>
    dplyr::group_by(dplyr::across(dplyr::all_of(group_cols))) |>
    dplyr::summarise(
      matched_rows = dplyr::n(),
      countries = dplyr::n_distinct(.data$iso3),
      years = dplyr::n_distinct(.data$analysis_year),
      lmic_rows = sum(.data$historical_lmic_reporting_scope %in% TRUE),
      mean_signed_gap_pp = mean(.data$signed_gap_pp),
      median_signed_gap_pp = stats::median(.data$signed_gap_pp),
      mean_abs_gap_pp = mean(abs(.data$signed_gap_pp)),
      median_abs_gap_pp = stats::median(abs(.data$signed_gap_pp)),
      rmse_gap_pp = sqrt(mean(.data$signed_gap_pp^2)),
      p90_abs_gap_pp = p15_fallback_safe_quantile(
        abs(.data$signed_gap_pp), 0.90
      ),
      max_abs_gap_pp = max(abs(.data$signed_gap_pp)),
      within_0_5pp_share = mean(abs(.data$signed_gap_pp) <= 0.5),
      within_1pp_share = mean(abs(.data$signed_gap_pp) <= 1),
      within_2pp_share = mean(abs(.data$signed_gap_pp) <= 2),
      .groups = "drop"
    )
}
