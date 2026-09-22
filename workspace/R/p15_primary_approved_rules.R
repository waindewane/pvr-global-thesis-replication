# Approved forward P15 observed-primary construction.
#
# This layer applies the owner's 2026-08-15 PRI-05 decision to the neutral
# all-years issue evidence. It is intentionally separate from the immutable P13
# parity branch and does not select a ladder rate.

p15_primary_approved_schema_version <- function() {
  "SCHEMA-P15-PRIMARY-APPROVED-CANDIDATE-V1"
}

p15_primary_approved_build_id <- function() {
  "BUILD-P15-PRIMARY-APPROVED-CANDIDATE-20260815-V1"
}

p15_primary_approved_method_id <- function() {
  "ADM-P15-PRIMARY-FORWARD-V1"
}

p15_apply_approved_primary_rules <- function(issue_evidence, instrument_universe) {
  required <- c(
    "analysis_year", "iso3", "country", "country_year_id", "period",
    "historical_income_level", "historical_lmic_reporting_scope",
    "economic_issue_key", "issue_date", "maturity_date", "currency",
    "original_maturity_years", "original_issue_yield_pct", "face_issued_usd",
    "usd_weight_available", "identifier_yield_conflict", "bill_or_strip_flag",
    "central_bank_flag", "restructuring_flag", "nonstandard_feature_flag",
    "candidate_primary_standard", "candidate_primary_standard_50m",
    "representative_isins", "representative_rics", "source_package_ids",
    "source_record_locators", "source_object"
  )
  missing <- setdiff(required, names(issue_evidence))
  if (length(missing)) {
    stop(
      "Primary issue evidence missing approved-rule columns: ",
      paste(missing, collapse = ", "), call. = FALSE
    )
  }

  feature_review <- p15_primary_issue_feature_diagnostic(
    issue_evidence, instrument_universe
  ) |>
    dplyr::select(
      "analysis_year", "iso3", "economic_issue_key",
      dplyr::starts_with("feature_"), "matched_feature_isins",
      "candidate_primary_fixed_call_sink_review",
      "candidate_primary_fixed_call_sink_50m_review"
    )

  feature_columns <- grep(
    "^feature_", names(feature_review), value = TRUE
  )

  out <- issue_evidence |>
    dplyr::left_join(
      feature_review,
      by = c("analysis_year", "iso3", "economic_issue_key")
    ) |>
    dplyr::mutate(
      dplyr::across(
        dplyr::all_of(feature_columns),
        ~ dplyr::coalesce(as.logical(.x), FALSE)
      ),
      matched_feature_isins = dplyr::coalesce(
        as.integer(.data$matched_feature_isins), 0L
      ),
      candidate_primary_fixed_call_sink_review = dplyr::coalesce(
        .data$candidate_primary_fixed_call_sink_review, FALSE
      ),
      candidate_primary_fixed_call_sink_50m_review = dplyr::coalesce(
        .data$candidate_primary_fixed_call_sink_50m_review, FALSE
      ),
      approved_plain_vanilla_issue = .data$candidate_primary_standard,
      approved_fixed_call_sink_subtype =
        .data$candidate_primary_fixed_call_sink_review,
      approved_primary_structural_issue =
        .data$approved_plain_vanilla_issue |
        .data$approved_fixed_call_sink_subtype,
      approved_materiality_50m_pass = is.finite(.data$face_issued_usd) &
        .data$face_issued_usd >= 50000000,
      approved_primary_preferred_issue_before_sanity =
        .data$approved_primary_structural_issue &
        .data$approved_materiality_50m_pass,
      approved_primary_all_issue_sensitivity_before_sanity =
        .data$approved_primary_structural_issue,
      approved_primary_subtype = dplyr::case_when(
        .data$approved_fixed_call_sink_subtype ~
          "observed_primary_fixed_callable_or_sinkable",
        .data$approved_plain_vanilla_issue ~ "observed_primary_plain_vanilla",
        TRUE ~ "not_admitted_under_approved_primary_rule"
      ),
      approved_weight_usd = dplyr::if_else(
        .data$approved_primary_structural_issue,
        as.numeric(.data$face_issued_usd), NA_real_
      ),
      primary_rule_disposition = dplyr::case_when(
        !is.finite(.data$original_issue_yield_pct) ~
          "retained_missing_direct_original_yield",
        !.data$currency %in% c("USD", "EUR") ~
          "retained_outside_approved_currency_scope",
        !is.finite(.data$original_maturity_years) ~
          "retained_missing_original_maturity",
        .data$original_maturity_years < 1 ~
          "retained_short_term_under_one_year",
        !.data$usd_weight_available ~
          "retained_missing_positive_true_usd_issue_weight",
        .data$identifier_yield_conflict ~
          "retained_unresolved_identifier_yield_conflict",
        .data$bill_or_strip_flag ~ "retained_bill_strip_or_cash_management",
        .data$central_bank_flag ~ "retained_central_bank_or_nonbudget_issuer",
        .data$restructuring_flag ~ "retained_restructuring_or_exchange_issue",
        .data$approved_primary_preferred_issue_before_sanity ~
          "admitted_preferred_primary_before_sanity",
        .data$approved_primary_all_issue_sensitivity_before_sanity ~
          "admitted_all_issue_sensitivity_below_50m",
        .data$nonstandard_feature_flag ~
          "retained_nonapproved_feature_structure",
        TRUE ~ "retained_other_nonadmitted_primary_evidence"
      ),
      raw_evidence_retained = TRUE,
      selected_for_ladder = FALSE,
      decision_state =
        "approved_primary_method_candidate_pending_sanity_and_ladder",
      method_id = p15_primary_approved_method_id(),
      schema_version = p15_primary_approved_schema_version(),
      build_id = p15_primary_approved_build_id()
    )

  if (any(
    out$approved_primary_structural_issue &
      (!is.finite(out$approved_weight_usd) | out$approved_weight_usd <= 0)
  )) {
    stop("Approved primary issues must have a positive true USD weight.",
         call. = FALSE)
  }
  if (any(
    out$approved_fixed_call_sink_subtype &
      (out$feature_putable | out$feature_convertible |
         out$feature_floating_or_variable |
         out$feature_zero_discount_or_strip |
         out$feature_step_or_index_linked)
  )) {
    stop("Disallowed feature structure entered fixed call/sink subtype.",
         call. = FALSE)
  }
  out |>
    dplyr::arrange(
      .data$analysis_year, .data$iso3, .data$issue_date,
      .data$economic_issue_key
    )
}

p15_primary_approved_variant_register <- function() {
  tibble::tribble(
    ~candidate_variant_id, ~currency_rule, ~materiality_rule,
    ~plain_language_role, ~method_preferred_candidate,
    "primary_forward_usd_50m_preferred", "USD", "at_least_usd_50m",
    paste(
      "Preferred currency-comparable primary candidate using direct original",
      "issue yields, true USD weights, and the approved plain-vanilla or fixed",
      "callable/sinkable scope."
    ), TRUE,
    "primary_forward_eur_50m_evidence", "EUR", "at_least_usd_50m",
    paste(
      "Separate EUR primary evidence using the approved issue rule; retained",
      "without pooling its nominal yields with USD."
    ), FALSE,
    "primary_forward_usd_all_issue_sensitivity", "USD", "all_approved_issues",
    "USD sensitivity retaining approved issues below the USD 50 million floor.",
    FALSE,
    "primary_forward_eur_all_issue_sensitivity", "EUR", "all_approved_issues",
    "EUR sensitivity retaining approved issues below the USD 50 million floor.",
    FALSE
  ) |>
    dplyr::mutate(
      candidate_decision_state =
        "approved_primary_method_candidate_not_ladder_selected",
      method_id = p15_primary_approved_method_id(),
      schema_version = p15_primary_approved_schema_version()
    )
}

p15_aggregate_approved_primary_candidates <- function(issue_disposition) {
  register <- p15_primary_approved_variant_register()
  dplyr::bind_rows(lapply(seq_len(nrow(register)), function(i) {
    rule <- register[i, ]
    issues <- issue_disposition |>
      dplyr::filter(
        .data$approved_primary_structural_issue,
        .data$currency == rule$currency_rule
      )
    if (rule$materiality_rule == "at_least_usd_50m") {
      issues <- issues |>
        dplyr::filter(.data$approved_materiality_50m_pass)
    }
    issues |>
      dplyr::group_by(
        .data$analysis_year, .data$iso3, .data$country,
        .data$country_year_id, .data$period,
        .data$historical_income_level,
        .data$historical_lmic_reporting_scope
      ) |>
      dplyr::summarise(
        market_rate_pct_before_sanity = p15_observed_weighted_mean(
          .data$original_issue_yield_pct, .data$approved_weight_usd
        ),
        market_maturity_years = p15_observed_weighted_mean(
          .data$original_maturity_years, .data$approved_weight_usd
        ),
        issue_count = dplyr::n(),
        plain_vanilla_issue_count = sum(.data$approved_plain_vanilla_issue),
        fixed_call_sink_issue_count = sum(
          .data$approved_fixed_call_sink_subtype
        ),
        total_weight_usd = sum(.data$approved_weight_usd),
        largest_issue_weight_share = p15_observed_largest_weight_share(
          .data$approved_weight_usd
        ),
        min_issue_rate_pct = p15_observed_min_or_na(
          .data$original_issue_yield_pct
        ),
        max_issue_rate_pct = p15_observed_max_or_na(
          .data$original_issue_yield_pct
        ),
        included_issue_keys = p15_observed_collapse(
          .data$economic_issue_key
        ),
        included_isins = p15_observed_collapse(.data$representative_isins),
        source_package_ids = p15_observed_collapse(.data$source_package_ids),
        .groups = "drop"
      ) |>
      dplyr::mutate(
        thin_evidence = .data$issue_count == 1L,
        high_concentration = .data$largest_issue_weight_share >= 0.8,
        evidence_strength_label = dplyr::case_when(
          .data$thin_evidence ~ "thin_single_issue_observed_evidence",
          .data$high_concentration ~ "multiple_issue_high_concentration",
          TRUE ~ "multiple_issue_diversified_evidence"
        ),
        candidate_variant_id = rule$candidate_variant_id,
        currency_basis = rule$currency_rule,
        materiality_rule = rule$materiality_rule,
        method_preferred_candidate = rule$method_preferred_candidate,
        sanity_application_state = "pending_approved_sanity_overlay",
        selected_for_ladder = FALSE,
        candidate_decision_state =
          "approved_primary_method_candidate_not_ladder_selected",
        evidence_family = "observed_primary_issuance",
        source_object = "lseg_original_issue_yield",
        method_id = p15_primary_approved_method_id(),
        schema_version = p15_primary_approved_schema_version(),
        build_id = p15_primary_approved_build_id()
      )
  })) |>
    dplyr::arrange(
      .data$analysis_year, .data$iso3, .data$candidate_variant_id
    )
}

p15_summarise_approved_primary_rules <- function(
    issue_disposition, country_candidates) {
  issue_summary <- issue_disposition |>
    dplyr::group_by(
      .data$historical_lmic_reporting_scope,
      .data$primary_rule_disposition
    ) |>
    dplyr::summarise(
      issues = dplyr::n(),
      country_years = dplyr::n_distinct(.data$country_year_id),
      .groups = "drop"
    )
  candidate_summary <- country_candidates |>
    dplyr::group_by(
      .data$candidate_variant_id,
      .data$historical_lmic_reporting_scope,
      .data$evidence_strength_label
    ) |>
    dplyr::summarise(
      country_years = dplyr::n(),
      issues = sum(.data$issue_count),
      fixed_call_sink_issues = sum(.data$fixed_call_sink_issue_count),
      .groups = "drop"
    )
  list(issue_summary = issue_summary, candidate_summary = candidate_summary)
}
