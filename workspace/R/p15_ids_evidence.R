# P15 World Bank IDS creditor-terms evidence helpers.
#
# This layer records the public creditor-country-year source object and its
# missingness. It does not decide the IDS ladder rank or headline role.

p15_ids_evidence_schema_version <- function() {
  "SCHEMA-P15-IDS-BONDHOLDER-EVIDENCE-V1"
}

p15_build_ids_bondholder_evidence <- function(grid, ids_terms) {
  required_grid <- c(
    "analysis_year", "iso3", "country", "country_year_id",
    "historical_income_level", "historical_lmic_reporting_scope"
  )
  missing_grid <- setdiff(required_grid, names(grid))
  if (length(missing_grid)) {
    stop("IDS grid is missing: ", paste(missing_grid, collapse = ", "),
         call. = FALSE)
  }
  required_ids <- c(
    "iso3", "year", "creditor", "creditor_id", "official_grace_years",
    "official_maturity_years", "official_rate", "has_complete_terms",
    "ids_history_source"
  )
  missing_ids <- setdiff(required_ids, names(ids_terms))
  if (length(missing_ids)) {
    stop("IDS terms are missing: ", paste(missing_ids, collapse = ", "),
         call. = FALSE)
  }

  bondholders <- ids_terms |>
    dplyr::filter(
      .data$year %in% unique(grid$analysis_year),
      .data$creditor_id == "BND" | .data$creditor == "Bondholders"
    ) |>
    dplyr::transmute(
      analysis_year = as.integer(.data$year),
      iso3 = as.character(.data$iso3),
      ids_source_country = as.character(.data$country),
      ids_creditor = as.character(.data$creditor),
      ids_creditor_id = as.character(.data$creditor_id),
      official_grace_years = as.numeric(.data$official_grace_years),
      official_maturity_years = as.numeric(.data$official_maturity_years),
      official_rate_pct = as.numeric(.data$official_rate),
      source_has_complete_terms_flag = as.logical(.data$has_complete_terms),
      ids_history_source = as.character(.data$ids_history_source)
    )
  if (anyDuplicated(bondholders[c("analysis_year", "iso3")])) {
    stop("IDS Bondholders input has duplicate country-year rows.", call. = FALSE)
  }

  out <- grid |>
    dplyr::left_join(bondholders, by = c("analysis_year", "iso3")) |>
    dplyr::mutate(
      ids_source_country_year_row_present = !is.na(.data$ids_creditor_id),
      ids_rate_class = dplyr::case_when(
        !.data$ids_source_country_year_row_present ~
          "country_not_in_ids_bondholder_source_universe",
        is.na(.data$official_rate_pct) ~ "missing_rate",
        .data$official_rate_pct <= 0 ~ "nonpositive_rate",
        .data$official_rate_pct < 1 ~ "low_positive_rate",
        .data$official_rate_pct > 30 ~ "above_30pct_review",
        TRUE ~ "positive_1_30"
      ),
      ids_term_shape = dplyr::case_when(
        !.data$ids_source_country_year_row_present ~ "source_row_absent",
        is.na(.data$official_maturity_years) |
          is.na(.data$official_grace_years) ~ "missing_terms",
        .data$official_maturity_years == 0 &
          .data$official_grace_years == 0 ~ "zero_zero_terms",
        .data$official_grace_years < 0 |
          .data$official_maturity_years < 0 ~ "negative_terms",
        .data$official_grace_years > .data$official_maturity_years ~
          "grace_exceeds_maturity",
        abs(.data$official_grace_years - .data$official_maturity_years) < 1e-8 &
          .data$official_maturity_years > 0 ~
          "bullet_like_grace_equals_maturity",
        .data$official_grace_years < .data$official_maturity_years ~
          "amortizing_like_grace_less_maturity",
        TRUE ~ "other"
      ),
      bondholder_term_validity_state = dplyr::case_when(
        .data$ids_rate_class != "positive_1_30" ~
          paste0("rate_not_sane_", .data$ids_rate_class),
        .data$ids_term_shape %in% c(
          "amortizing_like_grace_less_maturity",
          "bullet_like_grace_equals_maturity"
        ) ~ "bondholder_terms_usable_subject_to_source_checks",
        TRUE ~ paste0("terms_not_usable_", .data$ids_term_shape)
      ),
      ids_missingness_state = dplyr::case_when(
        !.data$ids_source_country_year_row_present ~
          "country_not_in_ids_bondholder_source_universe",
        is.na(.data$official_rate_pct) &
          is.na(.data$official_maturity_years) &
          is.na(.data$official_grace_years) ~ "all_three_terms_missing",
        is.na(.data$official_rate_pct) ~ "rate_missing",
        is.na(.data$official_maturity_years) ~ "maturity_missing",
        is.na(.data$official_grace_years) ~ "grace_missing",
        TRUE ~ "source_values_present"
      ),
      ids_source_object_type =
        "public_aggregate_new_commitment_terms_by_country_year_creditor_category",
      ids_rate_indicator_id = "DT.INR.DPPG",
      ids_rate_indicator_meaning =
        "Average interest on new external debt commitments (%)",
      ids_maturity_indicator_id = "DT.MAT.DPPG",
      ids_maturity_indicator_meaning =
        "Average maturity on new external debt commitments (years)",
      ids_grace_indicator_id = "DT.GPA.DPPG",
      ids_grace_indicator_meaning =
        "Average grace period on new external debt commitments (years)",
      ids_source_package_id = "SRC-WB-IDS-CORE-ALL-YEARS-20260520",
      ids_source_vintage = "cached_2026-05-20; API lastupdated 2025-12-03",
      ids_market_rate_object_state =
        "aggregate_primary_bond_commitment_terms_proxy_not_security_level_issue_yield",
      ids_repayment_profile_object_state =
        "aggregate_average_terms_not_instrument_cash_flows",
      ids_admissibility_state = "not_evaluated",
      ids_selection_state = "not_evaluated",
      ids_evidence_id = paste("IDS-BND", .data$analysis_year, .data$iso3, sep = "::"),
      ids_evidence_schema_version = p15_ids_evidence_schema_version()
    ) |>
    dplyr::arrange(.data$analysis_year, .data$iso3)

  stopifnot(
    nrow(out) == nrow(grid),
    !anyDuplicated(out[c("analysis_year", "iso3")]),
    all(out$ids_admissibility_state == "not_evaluated"),
    all(out$ids_selection_state == "not_evaluated"),
    all(out$ids_evidence_schema_version == p15_ids_evidence_schema_version())
  )
  out
}
