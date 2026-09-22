# P15 rating-implied benchmark component helpers.
#
# This layer makes rating, spread, risk-free, vintage, fallback, conflict, and
# missingness components explicit. It computes reproducibility candidates but
# does not decide rating-implied admissibility, selection, or headline use.

p15_rating_component_schema_version <- function() {
  "SCHEMA-P15-RATING-COMPONENT-LEDGER-V1"
}

p15_rating_notches <- function() {
  c(
    "Aaa", "Aa1", "Aa2", "Aa3", "A1", "A2", "A3",
    "Baa1", "Baa2", "Baa3", "Ba1", "Ba2", "Ba3",
    "B1", "B2", "B3", "Caa1", "Caa2", "Caa3", "Ca", "C"
  )
}

p15_normalize_rating_to_moodys <- function(rating) {
  rating <- stringr::str_trim(as.character(rating))
  rating[rating %in% c(
    "", "NA", "N/A", "N.R.", "NR", "WR", "WD", "SD", "RD", "D", "--"
  )] <- NA_character_
  rating <- stringr::str_remove(rating, "[*]$")
  rating <- stringr::str_remove(rating, "u$")
  rating <- stringr::str_remove(rating, "U$")
  rating[rating == "AA2"] <- "Aa2"

  sp_fitch_to_moodys <- c(
    "AAA" = "Aaa", "AA+" = "Aa1", "AA" = "Aa2", "AA-" = "Aa3",
    "A+" = "A1", "A" = "A2", "A-" = "A3",
    "BBB+" = "Baa1", "BBB" = "Baa2", "BBB-" = "Baa3",
    "BB+" = "Ba1", "BB" = "Ba2", "BB-" = "Ba3",
    "B+" = "B1", "B" = "B2", "B-" = "B3",
    "CCC+" = "Caa1", "CCC" = "Caa2", "CCC-" = "Caa3",
    "CC" = "Ca", "C" = "C"
  )
  hit <- !is.na(rating) & rating %in% names(sp_fitch_to_moodys)
  rating[hit] <- unname(sp_fitch_to_moodys[rating[hit]])
  rating[!is.na(rating) & !rating %in% p15_rating_notches()] <- NA_character_
  rating
}

p15_mean_or_na <- function(x) {
  x <- x[!is.na(x)]
  if (!length(x)) return(NA_real_)
  mean(x)
}

p15_build_damodaran_rating_spread_lookup <- function(damodaran_archive) {
  required <- c(
    "analysis_year", "damodaran_rating", "default_spread_pct",
    "archive_parser", "archive_vintage_label"
  )
  missing <- setdiff(required, names(damodaran_archive))
  if (length(missing)) {
    stop("Damodaran archive is missing: ", paste(missing, collapse = ", "),
         call. = FALSE)
  }

  damodaran_archive |>
    dplyr::mutate(
      analysis_year = as.integer(.data$analysis_year),
      normalized_rating = p15_normalize_rating_to_moodys(
        .data$damodaran_rating
      ),
      default_spread_pct = as.numeric(.data$default_spread_pct)
    ) |>
    dplyr::filter(
      !is.na(.data$normalized_rating), !is.na(.data$default_spread_pct)
    ) |>
    dplyr::group_by(.data$analysis_year, .data$normalized_rating) |>
    dplyr::summarise(
      damodaran_rating_mapped_spread_pct = stats::median(
        .data$default_spread_pct, na.rm = TRUE
      ),
      damodaran_rating_mapped_spread_min_pct = min(
        .data$default_spread_pct, na.rm = TRUE
      ),
      damodaran_rating_mapped_spread_max_pct = max(
        .data$default_spread_pct, na.rm = TRUE
      ),
      spread_observation_count = dplyr::n(),
      distinct_spread_count = dplyr::n_distinct(.data$default_spread_pct),
      spread_values_observed = paste(
        sort(unique(.data$default_spread_pct)), collapse = ";"
      ),
      damodaran_rating_values_observed = paste(
        sort(unique(.data$damodaran_rating)), collapse = ";"
      ),
      damodaran_archive_vintage_labels = paste(
        sort(unique(.data$archive_vintage_label)), collapse = ";"
      ),
      damodaran_archive_parsers = paste(
        sort(unique(.data$archive_parser)), collapse = ";"
      ),
      .groups = "drop"
    )
}

p15_build_fred_dgs7_annual <- function(fred_dgs7, analysis_years) {
  required <- c("observation_date", "DGS7")
  missing <- setdiff(required, names(fred_dgs7))
  if (length(missing)) {
    stop("FRED DGS7 input is missing: ", paste(missing, collapse = ", "),
         call. = FALSE)
  }

  fred_dgs7 |>
    dplyr::mutate(
      observation_date = as.Date(.data$observation_date),
      analysis_year = as.integer(format(.data$observation_date, "%Y")),
      risk_free_7y_pct = readr::parse_number(as.character(.data$DGS7))
    ) |>
    dplyr::filter(.data$analysis_year %in% analysis_years) |>
    dplyr::group_by(.data$analysis_year) |>
    dplyr::summarise(
      risk_free_7y_pct = p15_mean_or_na(.data$risk_free_7y_pct),
      risk_free_observation_count = sum(!is.na(.data$risk_free_7y_pct)),
      risk_free_first_observation_date = min(
        .data$observation_date[!is.na(.data$risk_free_7y_pct)], na.rm = TRUE
      ),
      risk_free_last_observation_date = max(
        .data$observation_date[!is.na(.data$risk_free_7y_pct)], na.rm = TRUE
      ),
      .groups = "drop"
    )
}

p15_build_rating_component_ledger <- function(
    grid, ratings, missing_ratings, damodaran_archive, fred_dgs7,
    rating_event_conflicts) {
  required_grid <- c(
    "analysis_year", "iso3", "country", "country_year_id",
    "historical_income_level", "historical_lmic_reporting_scope"
  )
  missing_grid <- setdiff(required_grid, names(grid))
  if (length(missing_grid)) {
    stop("Rating grid is missing: ", paste(missing_grid, collapse = ", "),
         call. = FALSE)
  }
  required_ratings <- c(
    "iso3", "region", "year", "moodys_rating", "fitch_rating", "sp_rating",
    "preferred_agency_count", "preferred_agencies_available",
    "selected_rating", "selected_agency", "selected_source_event_date",
    "selected_source_rule", "has_selected_precedence_rating",
    "agency_precedence_rule"
  )
  missing_rating_cols <- setdiff(required_ratings, names(ratings))
  if (length(missing_rating_cols)) {
    stop("Bloomberg rating panel is missing: ",
         paste(missing_rating_cols, collapse = ", "), call. = FALSE)
  }

  analysis_years <- sort(unique(as.integer(grid$analysis_year)))
  rating_rows <- ratings |>
    dplyr::filter(
      .data$year %in% analysis_years, !is.na(.data$iso3), .data$iso3 != ""
    ) |>
    dplyr::transmute(
      analysis_year = as.integer(.data$year),
      iso3 = as.character(.data$iso3),
      bloomberg_panel_row_present = TRUE,
      bloomberg_country_code = as.character(.data$country_code_bloomberg),
      rating_source_region = as.character(.data$region),
      moodys_rating = as.character(.data$moodys_rating),
      moodys_rating_normalized = p15_normalize_rating_to_moodys(
        .data$moodys_rating
      ),
      moodys_source_event_date = as.Date(.data$moodys_source_event_date),
      fitch_rating = as.character(.data$fitch_rating),
      fitch_rating_normalized = p15_normalize_rating_to_moodys(
        .data$fitch_rating
      ),
      fitch_source_event_date = as.Date(.data$fitch_source_event_date),
      sp_rating = as.character(.data$sp_rating),
      sp_rating_normalized = p15_normalize_rating_to_moodys(.data$sp_rating),
      sp_source_event_date = as.Date(.data$sp_source_event_date),
      preferred_agency_count = as.integer(.data$preferred_agency_count),
      preferred_agencies_available = as.character(
        .data$preferred_agencies_available
      ),
      selected_rating = as.character(.data$selected_rating),
      selected_rating_normalized = p15_normalize_rating_to_moodys(
        .data$selected_rating
      ),
      selected_agency = as.character(.data$selected_agency),
      selected_source_event_date = as.Date(.data$selected_source_event_date),
      selected_source_rule = as.character(.data$selected_source_rule),
      has_selected_precedence_rating = as.logical(
        .data$has_selected_precedence_rating
      ),
      agency_precedence_rule = as.character(.data$agency_precedence_rule)
    )
  if (anyDuplicated(rating_rows[c("analysis_year", "iso3")])) {
    stop("Bloomberg rating panel has duplicate country-year rows.",
         call. = FALSE)
  }

  missing_rows <- missing_ratings |>
    dplyr::filter(
      .data$year %in% analysis_years, !is.na(.data$iso3), .data$iso3 != ""
    ) |>
    dplyr::transmute(
      analysis_year = as.integer(.data$year),
      iso3 = as.character(.data$iso3),
      bloomberg_missing_reason = as.character(.data$missing_reason),
      has_any_active_rating_type = as.logical(.data$has_any_rating_type),
      active_nonpreferred_agencies = as.character(
        .data$active_nonpreferred_agencies
      ),
      active_nonpreferred_rating_types = as.character(
        .data$active_nonpreferred_rating_types
      )
    )
  if (anyDuplicated(missing_rows[c("analysis_year", "iso3")])) {
    stop("Bloomberg missing-rating panel has duplicate country-year rows.",
         call. = FALSE)
  }

  spread_lookup <- p15_build_damodaran_rating_spread_lookup(
    damodaran_archive
  ) |>
    dplyr::filter(.data$analysis_year %in% analysis_years)
  risk_free <- p15_build_fred_dgs7_annual(fred_dgs7, analysis_years)

  conflict_summary <- rating_event_conflicts |>
    dplyr::filter(
      .data$event_year %in% analysis_years,
      !is.na(.data$iso3), .data$iso3 != ""
    ) |>
    dplyr::group_by(
      analysis_year = as.integer(.data$event_year), .data$iso3
    ) |>
    dplyr::summarise(
      same_day_conflict_event_rows = dplyr::n(),
      same_day_conflict_event_dates = paste(
        sort(unique(as.character(.data$event_date))), collapse = ";"
      ),
      same_day_conflict_agencies = paste(
        sort(unique(.data$agency)), collapse = ";"
      ),
      same_day_preferred_rating_conflict_rows = sum(
        .data$preferred_external_rating_type %in% TRUE
      ),
      .groups = "drop"
    )

  out <- grid |>
    dplyr::left_join(rating_rows, by = c("analysis_year", "iso3")) |>
    dplyr::left_join(missing_rows, by = c("analysis_year", "iso3")) |>
    dplyr::left_join(
      spread_lookup,
      by = c("analysis_year", "selected_rating_normalized" = "normalized_rating")
    ) |>
    dplyr::left_join(risk_free, by = "analysis_year") |>
    dplyr::left_join(conflict_summary, by = c("analysis_year", "iso3")) |>
    dplyr::mutate(
      bloomberg_country_year_row_present = dplyr::coalesce(
        .data$bloomberg_panel_row_present, FALSE
      ),
      preferred_agency_count = dplyr::coalesce(
        .data$preferred_agency_count, 0L
      ),
      same_day_conflict_event_rows = dplyr::coalesce(
        .data$same_day_conflict_event_rows, 0L
      ),
      same_day_preferred_rating_conflict_rows = dplyr::coalesce(
        .data$same_day_preferred_rating_conflict_rows, 0L
      ),
      rating_availability_state = dplyr::case_when(
        !.data$bloomberg_country_year_row_present ~
          "country_not_in_bloomberg_rating_panel_universe",
        .data$has_selected_precedence_rating %in% TRUE &
          !is.na(.data$selected_rating_normalized) ~
          "selected_precedence_rating_available",
        .data$has_selected_precedence_rating %in% TRUE ~
          "selected_rating_inactive_or_unmapped",
        .data$has_any_active_rating_type %in% TRUE ~
          "only_nonpreferred_active_rating_available",
        TRUE ~ "no_active_preferred_rating"
      ),
      agency_fallback_state = dplyr::case_when(
        .data$selected_agency == "Moodys" ~
          "moodys_first_precedence_selected",
        .data$selected_agency %in% c("Fitch", "SP") ~
          paste0(
            tolower(.data$selected_agency),
            "_selected_by_documented_agency_fallback"
          ),
        .data$rating_availability_state ==
          "only_nonpreferred_active_rating_available" ~
          "preferred_agency_fallback_exhausted_nonpreferred_only",
        TRUE ~ "no_selected_precedence_rating"
      ),
      agency_disagreement_state = NA_character_,
      analysis_date = as.Date(paste0(.data$analysis_year, "-01-01")),
      selected_rating_age_days = as.integer(
        .data$analysis_date - .data$selected_source_event_date
      ),
      selected_rating_age_band = dplyr::case_when(
        is.na(.data$selected_source_event_date) ~ "no_selected_event_date",
        .data$selected_rating_age_days < 0L ~ "event_after_analysis_date_review",
        .data$selected_rating_age_days <= 365L ~ "zero_to_one_year_old",
        .data$selected_rating_age_days <= 730L ~ "one_to_two_years_old",
        .data$selected_rating_age_days <= 1826L ~ "two_to_five_years_old",
        TRUE ~ "more_than_five_years_old"
      ),
      damodaran_archive_year_available = .data$analysis_year %in%
        unique(as.integer(damodaran_archive$analysis_year)),
      rating_implied_candidate_computed =
        .data$rating_availability_state ==
          "selected_precedence_rating_available" &
        !is.na(.data$damodaran_rating_mapped_spread_pct) &
        !is.na(.data$risk_free_7y_pct),
      rating_implied_candidate_rate_pct = dplyr::if_else(
        .data$rating_implied_candidate_computed,
        .data$risk_free_7y_pct +
          .data$damodaran_rating_mapped_spread_pct,
        NA_real_
      ),
      rating_implied_uncomputed_reason = dplyr::case_when(
        .data$rating_implied_candidate_computed ~ NA_character_,
        .data$rating_availability_state ==
          "country_not_in_bloomberg_rating_panel_universe" ~
          "country_not_in_bloomberg_rating_panel_universe",
        .data$rating_availability_state ==
          "selected_rating_inactive_or_unmapped" ~
          "selected_rating_inactive_or_unmapped",
        .data$rating_availability_state ==
          "only_nonpreferred_active_rating_available" ~
          "only_nonpreferred_rating_types_available",
        .data$rating_availability_state == "no_active_preferred_rating" ~
          "no_active_preferred_rating",
        !.data$damodaran_archive_year_available ~
          "damodaran_archive_year_missing",
        is.na(.data$damodaran_rating_mapped_spread_pct) ~
          "damodaran_rating_year_spread_missing",
        is.na(.data$risk_free_7y_pct) ~ "risk_free_rate_missing",
        TRUE ~ "unclassified_component_gap"
      ),
      spread_lookup_state = dplyr::case_when(
        is.na(.data$damodaran_rating_mapped_spread_pct) ~
          "rating_year_spread_not_available",
        .data$distinct_spread_count > 1L ~
          "multiple_source_spreads_median_used_for_reproduction",
        TRUE ~ "single_distinct_rating_year_spread"
      ),
      rating_timing_convention = "beginning_of_year_january_1_in_force",
      rating_source_vintage = "Bloomberg event panel captured 2026-05-28",
      spread_source_vintage = paste0(
        "Damodaran annual archive for analysis year ", .data$analysis_year
      ),
      risk_free_source_vintage =
        "FRED DGS7 history captured 2026-05-12; annual arithmetic mean",
      estimator_id =
        "RATING-BLOOMBERG-BOY-DAMODARAN-RATING-YEAR-MEDIAN-DGS7-V1",
      mapping_rule =
        "selected Bloomberg rating -> Moody-equivalent notch -> same-year Damodaran archive rating-group median spread",
      formula_rule =
        "annual mean FRED DGS7 + mapped Damodaran default spread",
      current_vintage_substitution_state =
        "not_used_historical_beginning_of_year_and_archive_year_inputs_only",
      rating_admissibility_state = "not_evaluated",
      rating_selection_state = "not_evaluated",
      rating_headline_role_state = "not_evaluated",
      rating_source_package_id = "SRC-BLOOMBERG-BOY-RATINGS-20260528",
      rating_missingness_source_package_id =
        "SRC-BLOOMBERG-MISSING-RATINGS-20260528",
      rating_conflict_source_package_id =
        "SRC-BLOOMBERG-RATING-CONFLICTS-20260528",
      spread_source_package_id = "SRC-DAMODARAN-ARCHIVE-2000-2024",
      risk_free_source_package_id = "SRC-FRED-DGS7-20260512",
      rating_component_id = paste(
        "RATING", .data$analysis_year, .data$iso3, sep = "::"
      ),
      rating_component_schema_version =
        p15_rating_component_schema_version()
    ) |>
    dplyr::select(-"analysis_date") |>
    dplyr::arrange(.data$analysis_year, .data$iso3)

  # The row-wise disagreement classification above cannot use n_distinct over
  # three columns directly inside mutate. Resolve it deterministically here.
  normalized_agencies <- cbind(
    out$moodys_rating_normalized,
    out$fitch_rating_normalized,
    out$sp_rating_normalized
  )
  out$agency_disagreement_state <- vapply(
    seq_len(nrow(out)),
    function(i) {
      vals <- unique(normalized_agencies[i, !is.na(normalized_agencies[i, ])])
      if (!length(vals)) return("no_preferred_agency_rating")
      if (sum(!is.na(normalized_agencies[i, ])) == 1L) {
        return("single_preferred_agency_rating")
      }
      if (length(vals) == 1L) {
        return("multiple_agencies_same_equivalent_notch")
      }
      "multiple_agencies_different_equivalent_notches"
    },
    character(1)
  )

  stopifnot(
    nrow(out) == nrow(grid),
    !anyDuplicated(out[c("analysis_year", "iso3")]),
    all(out$rating_admissibility_state == "not_evaluated"),
    all(out$rating_selection_state == "not_evaluated"),
    all(out$rating_headline_role_state == "not_evaluated"),
    all(
      out$rating_component_schema_version ==
        p15_rating_component_schema_version()
    )
  )
  out
}
