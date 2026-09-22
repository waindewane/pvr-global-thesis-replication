#!/usr/bin/env Rscript

# Build the neutral P15 rating-component ledger, diagnose rating gaps and
# conflicts, reproduce P13's 2024 rating-implied rows, and make the distinct
# P14 historical estimator explicit. No admissibility or headline decision is
# made here.

suppressPackageStartupMessages({
  library(data.table)
  library(digest)
  library(dplyr)
  library(readr)
  library(tibble)
})

source("R/p15_rating_components.R")

root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
derived_dir <- file.path(root, "data-derived", "p15_rating_components_2012_2024_v1")
governance_dir <- file.path(root, "docs", "governance")
dir.create(derived_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(governance_dir, recursive = TRUE, showWarnings = FALSE)

input_rel <- c(
  grid = "data-derived/p15_platform_2012_2024_v1/p15_country_year_grid_2012_2024.csv",
  ratings = "data-raw/ratings/bloomberg_sovereign_rating_panel_2026-05-28/bloomberg_boy_ratings_single_precedence_country_year_2000_2025.csv",
  missing = "data-raw/ratings/bloomberg_sovereign_rating_panel_2026-05-28/bloomberg_missing_preferred_country_years_2000_2025.csv",
  conflicts = "data-raw/ratings/bloomberg_sovereign_rating_panel_2026-05-28/bloomberg_rating_event_conflicts_2000_2025.csv",
  damodaran = "experiments/full_ladder_database_all_years_2026-05-20/outputs/damodaran_archive_country_spreads_2000_2024.csv",
  fred = "sources/literature_review/lr3_systematic_expansion_2026-05-12/official_sources/fred_dgs7.csv",
  p14 = "experiments/p14_historical_validation_and_extension_2026-06-22/outputs/p14_rating_implied_panel_2015_2024.csv",
  p13 = "experiments/full_ladder_ratings_integrated_status_rebuild_2026-06-01/outputs/p13_full_labelled_rate_value_ladder_2024.csv"
)
input_paths <- stats::setNames(file.path(root, unname(input_rel)), names(input_rel))
stopifnot(all(file.exists(input_paths)))

grid <- readr::read_csv(input_paths[["grid"]], show_col_types = FALSE)
ratings <- readr::read_csv(
  input_paths[["ratings"]], show_col_types = FALSE, guess_max = 100000
)
missing_ratings <- readr::read_csv(
  input_paths[["missing"]], show_col_types = FALSE, guess_max = 100000
)
rating_event_conflicts <- readr::read_csv(
  input_paths[["conflicts"]], show_col_types = FALSE, guess_max = 100000
)
damodaran <- readr::read_csv(
  input_paths[["damodaran"]], show_col_types = FALSE, guess_max = 100000
)
fred <- readr::read_csv(
  input_paths[["fred"]], show_col_types = FALSE, guess_max = 100000
)
p14 <- readr::read_csv(input_paths[["p14"]], show_col_types = FALSE)
p13 <- readr::read_csv(input_paths[["p13"]], show_col_types = FALSE)

ledger <- p15_build_rating_component_ledger(
  grid = grid,
  ratings = ratings,
  missing_ratings = missing_ratings,
  damodaran_archive = damodaran,
  fred_dgs7 = fred,
  rating_event_conflicts = rating_event_conflicts
)

ledger_path <- file.path(
  derived_dir, "p15_rating_component_ledger_2012_2024.csv.gz"
)
data.table::fwrite(data.table::as.data.table(ledger), ledger_path, na = "")

# P13 uses the Bloomberg-selected-rating -> Damodaran rating-year lookup. This
# comparison is the required exact 2024 legacy gate for the component.
p13_rating <- p13 |>
  dplyr::filter(.data$tier_id == "rating_implied_model") |>
  dplyr::select(
    analysis_year, iso3, p13_rate_value_id,
    p13_rate_present = rate_value_present,
    p13_rate_pct = rate_pct,
    p13_maturity_years = maturity_years,
    p13_source_pointer = source_pointer,
    p13_detailed_ladder_tier_id = detailed_ladder_tier_id
  )
p13_crosswalk <- ledger |>
  dplyr::filter(.data$analysis_year == 2024L) |>
  dplyr::left_join(p13_rating, by = c("analysis_year", "iso3")) |>
  dplyr::mutate(
    p13_rate_reconstructed =
      !.data$p13_rate_present |
      (!is.na(.data$rating_implied_candidate_rate_pct) &
         abs(
           .data$rating_implied_candidate_rate_pct - .data$p13_rate_pct
         ) <= 1e-12),
    p13_maturity_reconstructed =
      !.data$p13_rate_present |
      (!is.na(.data$p13_maturity_years) &
         abs(.data$p13_maturity_years - 7) <= 1e-12),
    p13_component_state = dplyr::case_when(
      .data$p13_rate_present ~ "p13_present_rate_reconstructed",
      .data$rating_implied_candidate_computed ~
        "component_candidate_available_but_p13_rate_absent",
      TRUE ~ "component_candidate_uncomputed_and_p13_rate_absent"
    )
  )
stopifnot(
  nrow(p13_crosswalk) == 211L,
  sum(p13_crosswalk$p13_rate_present) == 141L,
  all(p13_crosswalk$p13_rate_reconstructed),
  all(p13_crosswalk$p13_maturity_reconstructed)
)

p13_crosswalk_path <- file.path(
  governance_dir, "p15_rating_p13_anchor_crosswalk_2024.csv"
)
data.table::fwrite(
  data.table::as.data.table(p13_crosswalk), p13_crosswalk_path, na = ""
)

# P14 used a different formula: the annual risk-free component plus the
# Damodaran country-year spread for that country. Bloomberg fields were joined
# as metadata but did not determine the P14 spread. Reconstruct that estimator
# and decompose the difference from the P13-style estimator.
damodaran_country <- damodaran |>
  dplyr::filter(.data$analysis_year >= 2015L, .data$analysis_year <= 2024L) |>
  dplyr::transmute(
    analysis_year = as.integer(.data$analysis_year),
    iso3 = as.character(.data$iso3),
    p14_formula_damodaran_country = as.character(.data$damodaran_country),
    p14_formula_damodaran_rating = as.character(.data$damodaran_rating),
    p14_formula_damodaran_rating_normalized =
      p15_normalize_rating_to_moodys(.data$damodaran_rating),
    p14_formula_country_spread_pct = as.numeric(.data$default_spread_pct),
    p14_formula_archive_parser = as.character(.data$archive_parser),
    p14_formula_archive_vintage = as.character(.data$archive_vintage_label)
  )
stopifnot(!anyDuplicated(damodaran_country[c("analysis_year", "iso3")]))

p14_comparison <- p14 |>
  dplyr::left_join(
    ledger |>
      dplyr::select(
        analysis_year, iso3,
        p15_bloomberg_selected_rating = selected_rating,
        p15_bloomberg_selected_rating_normalized = selected_rating_normalized,
        p15_bloomberg_selected_agency = selected_agency,
        p15_bloomberg_selected_source_event_date = selected_source_event_date,
        p15_bloomberg_rating_age_days = selected_rating_age_days,
        p15_rating_availability_state = rating_availability_state,
        p15_rating_mapped_spread_pct = damodaran_rating_mapped_spread_pct,
        p15_risk_free_7y_pct = risk_free_7y_pct,
        p15_bloomberg_rating_candidate_rate_pct =
          rating_implied_candidate_rate_pct,
        p15_rating_candidate_computed = rating_implied_candidate_computed
      ),
    by = c("analysis_year", "iso3")
  ) |>
  dplyr::left_join(damodaran_country, by = c("analysis_year", "iso3")) |>
  dplyr::mutate(
    p14_rate_reconstructed_from_components =
      .data$p15_risk_free_7y_pct + .data$p14_formula_country_spread_pct,
    p14_rate_exact_reproduction =
      !is.na(.data$p14_rate_reconstructed_from_components) &
      abs(
        .data$rating_implied_rate_pct -
          .data$p14_rate_reconstructed_from_components
      ) <= 1e-12,
    estimators_both_computed =
      !is.na(.data$p15_bloomberg_rating_candidate_rate_pct),
    p15_minus_p14_rate_pp =
      .data$p15_bloomberg_rating_candidate_rate_pct -
      .data$rating_implied_rate_pct,
    risk_free_component_difference_pp = 0,
    spread_component_difference_pp =
      .data$p15_rating_mapped_spread_pct -
      .data$p14_formula_country_spread_pct,
    formula_difference_reconciles =
      !.data$estimators_both_computed |
      abs(
        .data$p15_minus_p14_rate_pp -
          .data$spread_component_difference_pp
      ) <= 1e-12,
    formula_rating_object_same_equivalent_notch =
      !is.na(.data$p15_bloomberg_selected_rating_normalized) &
      !is.na(.data$p14_formula_damodaran_rating_normalized) &
      .data$p15_bloomberg_selected_rating_normalized ==
        .data$p14_formula_damodaran_rating_normalized,
    estimator_difference_class = dplyr::case_when(
      !.data$estimators_both_computed ~
        "p14_country_spread_available_p15_bloomberg_formula_uncomputed",
      abs(.data$p15_minus_p14_rate_pp) <= 1e-12 ~
        "same_numeric_rate",
      .data$formula_rating_object_same_equivalent_notch ~
        "same_equivalent_rating_but_country_and_group_spread_objects_differ",
      TRUE ~ "different_rating_and_spread_objects"
    ),
    p14_estimator_id =
      "RATING-DAMODARAN-COUNTRY-YEAR-SPREAD-DGS7-P14-LEGACY",
    p15_estimator_id =
      "RATING-BLOOMBERG-BOY-DAMODARAN-RATING-YEAR-MEDIAN-DGS7-V1"
  )
stopifnot(
  nrow(p14_comparison) == nrow(p14),
  all(p14_comparison$p14_rate_exact_reproduction),
  all(p14_comparison$formula_difference_reconciles)
)

p14_comparison_path <- file.path(
  governance_dir, "p15_rating_estimator_comparison_2015_2024.csv"
)
data.table::fwrite(
  data.table::as.data.table(p14_comparison), p14_comparison_path, na = ""
)

conflict_gap_detail <- ledger |>
  dplyr::transmute(
    analysis_year, iso3, country, historical_income_level,
    historical_lmic_reporting_scope,
    rating_availability_state,
    bloomberg_missing_reason,
    agency_fallback_state,
    agency_disagreement_state,
    selected_agency, selected_rating, selected_rating_normalized,
    selected_source_event_date, selected_rating_age_days,
    selected_rating_age_band,
    same_day_conflict_event_rows,
    same_day_conflict_event_dates,
    same_day_conflict_agencies,
    same_day_preferred_rating_conflict_rows,
    spread_lookup_state,
    rating_implied_uncomputed_reason,
    current_vintage_substitution_state,
    consequence_state = "evidence_only_no_admissibility_or_selection_consequence"
  )
conflict_gap_detail_path <- file.path(
  governance_dir, "p15_rating_conflict_gap_detail_2012_2024.csv.gz"
)
data.table::fwrite(
  data.table::as.data.table(conflict_gap_detail),
  conflict_gap_detail_path,
  na = ""
)

make_metric <- function(metric, rows, note) {
  tibble::tibble(metric = metric, country_year_rows = rows, interpretation = note)
}
conflict_gap_summary <- dplyr::bind_rows(
  make_metric(
    "country_year_grid_rows", nrow(ledger),
    "Complete neutral P15 country-year grid."
  ),
  make_metric(
    "selected_precedence_rating_available",
    sum(
      ledger$rating_availability_state ==
        "selected_precedence_rating_available"
    ),
    "A Bloomberg preferred-agency rating is active under documented precedence."
  ),
  make_metric(
    "rating_candidate_computed",
    sum(ledger$rating_implied_candidate_computed),
    "All three formula components are present; this is not an admissibility decision."
  ),
  make_metric(
    "country_not_in_bloomberg_rating_panel_universe",
    sum(
      ledger$rating_availability_state ==
        "country_not_in_bloomberg_rating_panel_universe"
    ),
    "No country-year row exists in the captured Bloomberg panel universe."
  ),
  make_metric(
    "no_active_preferred_rating",
    sum(ledger$rating_availability_state == "no_active_preferred_rating"),
    "The country is in the panel but has no active preferred rating."
  ),
  make_metric(
    "only_nonpreferred_active_rating_available",
    sum(
      ledger$rating_availability_state ==
        "only_nonpreferred_active_rating_available"
    ),
    "Bloomberg contains another active rating type but not an accepted preferred type."
  ),
  make_metric(
    "selected_rating_inactive_or_unmapped",
    sum(
      ledger$rating_availability_state ==
        "selected_rating_inactive_or_unmapped"
    ),
    "A selected raw label exists but is inactive/default-style or outside the mapping."
  ),
  make_metric(
    "damodaran_rating_year_spread_missing",
    sum(
      ledger$rating_implied_uncomputed_reason ==
        "damodaran_rating_year_spread_missing",
      na.rm = TRUE
    ),
    "A selected normalized rating exists but the same-year archive has no matching rating-group spread."
  ),
  make_metric(
    "agency_fallback_used",
    sum(grepl("selected_by_documented_agency_fallback", ledger$agency_fallback_state)),
    "Fitch or S&P is used only after the documented Moody's-first precedence."
  ),
  make_metric(
    "multiple_agencies_different_equivalent_notches",
    sum(
      ledger$agency_disagreement_state ==
        "multiple_agencies_different_equivalent_notches"
    ),
    "Two or more preferred agencies are active and disagree after notch normalization."
  ),
  make_metric(
    "same_day_conflict_event_rows",
    sum(ledger$same_day_conflict_event_rows),
    "Source event rows contain conflicting values on the same agency/date."
  ),
  make_metric(
    "country_years_with_same_day_conflict_events",
    sum(ledger$same_day_conflict_event_rows > 0L),
    "Country-years touched by at least one captured same-day event conflict."
  ),
  make_metric(
    "selected_rating_more_than_five_years_old",
    sum(ledger$selected_rating_age_band == "more_than_five_years_old"),
    "Descriptive age band only; no staleness exclusion threshold has been approved."
  ),
  make_metric(
    "rating_year_spread_lookup_collisions",
    sum(ledger$spread_lookup_state ==
          "multiple_source_spreads_median_used_for_reproduction"),
    "Country-year rows affected by one of the four rating-year source collisions; median reproduces P13."
  ),
  make_metric(
    "current_vintage_substitutions_used", 0L,
    "The main ledger uses historical beginning-of-year ratings and same-year archive spreads only."
  ),
  make_metric(
    "admissibility_or_selection_decisions_made", 0L,
    "This branch records evidence and reproduces formulas; RAT-07 remains a later decision gate."
  )
)
conflict_gap_summary_path <- file.path(
  governance_dir, "p15_rating_conflict_gap_summary.csv"
)
data.table::fwrite(
  data.table::as.data.table(conflict_gap_summary),
  conflict_gap_summary_path,
  na = ""
)

year_summary <- ledger |>
  dplyr::count(
    .data$analysis_year, .data$rating_availability_state,
    .data$rating_implied_candidate_computed,
    name = "country_year_rows"
  ) |>
  dplyr::arrange(
    .data$analysis_year, .data$rating_availability_state,
    dplyr::desc(.data$rating_implied_candidate_computed)
  )
year_summary_path <- file.path(
  governance_dir, "p15_rating_component_availability_by_year.csv"
)
data.table::fwrite(
  data.table::as.data.table(year_summary), year_summary_path, na = ""
)

estimator_register <- tibble::tribble(
  ~estimator_id, ~period_used_by_predecessor, ~rating_or_spread_object, ~risk_free_object, ~timing_rule, ~p15_state,
  "RATING-BLOOMBERG-BOY-DAMODARAN-RATING-YEAR-MEDIAN-DGS7-V1", "P13 2024", "Bloomberg selected preferred-agency rating mapped to Moody-equivalent notch and same-year Damodaran rating-group median spread", "annual arithmetic mean FRED DGS7", "rating active at January 1; same analysis-year spread and risk-free inputs", "exactly_reproduced_candidate_not_yet_approved_for_headline_use",
  "RATING-DAMODARAN-COUNTRY-YEAR-SPREAD-DGS7-P14-LEGACY", "P14 2015-2024", "Damodaran's country-year rating and country-specific adjusted default spread; Bloomberg metadata did not choose the spread", "annual arithmetic mean FRED DGS7", "same analysis-year Damodaran archive and risk-free inputs", "legacy_comparator_exactly_reproduced_not_silently_pooled_with_p13_formula",
  "RATING-CURRENT-VINTAGE-VARIANTS", "diagnostic experiments only", "current-vintage rating or spread inputs applied outside their historical year", "variant-specific", "not historical-vintage consistent", "excluded_from_main_component_ledger"
)
estimator_register_path <- file.path(
  governance_dir, "p15_rating_estimator_register.csv"
)
data.table::fwrite(
  data.table::as.data.table(estimator_register), estimator_register_path,
  na = ""
)

source_dictionary <- tibble::tribble(
  ~component, ~source_package_id, ~source_object, ~p15_use, ~not_equivalent_to,
  "sovereign_rating", "SRC-BLOOMBERG-BOY-RATINGS-20260528", "preferred external sovereign rating active at January 1 under Moody's-Fitch-S&P precedence", "rating label and event-vintage component", "observed bond yield or market-access status",
  "rating_missingness", "SRC-BLOOMBERG-MISSING-RATINGS-20260528", "country-year rows without a selected preferred rating and any active nonpreferred types", "explicit availability and fallback-exhaustion evidence", "proof that the sovereign had no rating anywhere",
  "rating_event_conflicts", "SRC-BLOOMBERG-RATING-CONFLICTS-20260528", "same-agency same-date source rows with conflicting current or previous rating values", "source-conflict evidence", "all cross-agency rating disagreement",
  "default_spread", "SRC-DAMODARAN-ARCHIVE-2000-2024", "annual archive country default-spread table grouped by normalized rating for P13 reproduction", "model spread component with collision diagnostics", "country-specific market yield",
  "risk_free_rate", "SRC-FRED-DGS7-20260512", "daily seven-year US Treasury constant-maturity rate summarized as an annual arithmetic mean", "USD seven-year risk-free component", "issue-date curve or EUR risk-free curve"
) |>
  dplyr::mutate(
    schema_version = p15_rating_component_schema_version()
  )
source_dictionary_path <- file.path(
  governance_dir, "p15_rating_component_source_dictionary.csv"
)
data.table::fwrite(
  data.table::as.data.table(source_dictionary), source_dictionary_path,
  na = ""
)

summary <- tibble::tibble(
  metric = c(
    "rating_component_country_year_rows",
    "selected_precedence_rating_rows",
    "computed_p13_style_candidate_rows",
    "uncomputed_candidate_rows",
    "p13_2024_present_rating_rows",
    "p13_2024_present_rate_mismatches",
    "p13_2024_present_maturity_mismatches",
    "p14_legacy_rows",
    "p14_legacy_formula_reproduction_mismatches",
    "p14_and_p15_formula_both_computed_rows",
    "p14_and_p15_formula_numeric_differences",
    "p14_rows_without_p15_bloomberg_formula_candidate",
    "rating_admissibility_decisions_made",
    "rating_selection_decisions_made",
    "rating_component_schema_version"
  ),
  value = c(
    as.character(nrow(ledger)),
    as.character(sum(
      ledger$rating_availability_state ==
        "selected_precedence_rating_available"
    )),
    as.character(sum(ledger$rating_implied_candidate_computed)),
    as.character(sum(!ledger$rating_implied_candidate_computed)),
    as.character(sum(p13_crosswalk$p13_rate_present)),
    as.character(sum(!p13_crosswalk$p13_rate_reconstructed)),
    as.character(sum(!p13_crosswalk$p13_maturity_reconstructed)),
    as.character(nrow(p14_comparison)),
    as.character(sum(!p14_comparison$p14_rate_exact_reproduction)),
    as.character(sum(p14_comparison$estimators_both_computed)),
    as.character(sum(
      p14_comparison$estimators_both_computed &
        abs(p14_comparison$p15_minus_p14_rate_pp) > 1e-12
    )),
    as.character(sum(!p14_comparison$estimators_both_computed)),
    "0", "0", p15_rating_component_schema_version()
  )
)
summary_path <- file.path(
  governance_dir, "p15_rating_component_ledger_summary.csv"
)
data.table::fwrite(data.table::as.data.table(summary), summary_path, na = "")

script_path <- file.path(root, "scripts/p15/build_p15_rating_component_ledger.R")
helper_path <- file.path(root, "R", "p15_rating_components.R")
manifest_source_inputs <- unname(input_paths)
manifest_inputs <- c(manifest_source_inputs, helper_path)
manifest_outputs <- c(
  ledger_path, p13_crosswalk_path, p14_comparison_path,
  conflict_gap_detail_path, conflict_gap_summary_path, year_summary_path,
  estimator_register_path, source_dictionary_path, summary_path
)
manifest <- data.table::rbindlist(lapply(
  c(manifest_inputs, manifest_outputs),
  function(path) {
    data.table::data.table(
      artifact_path = sub(paste0("^", root, "/"), "", path),
      artifact_role = if (path == helper_path) {
        "code_dependency"
      } else if (path %in% manifest_source_inputs) {
        "source_or_anchor_input"
      } else {
        "generated_output"
      },
      bytes = file.info(path)$size,
      sha256 = digest::digest(file = path, algo = "sha256"),
      producing_script = sub(paste0("^", root, "/"), "", script_path),
      producing_script_sha256 = digest::digest(
        file = script_path, algo = "sha256"
      ),
      build_id = "BUILD-P15-RATING-COMPONENT-LEDGER-20260721-V1",
      schema_version = p15_rating_component_schema_version(),
      build_date = "2026-07-21"
    )
  }
), use.names = TRUE, fill = TRUE)
manifest_path <- file.path(
  governance_dir, "p15_rating_component_ledger_manifest.csv"
)
data.table::fwrite(manifest, manifest_path, na = "")

cat("P15 rating-component ledger: PASS\n")
cat("Country-year rows:", nrow(ledger), "\n")
cat("P13-style candidates computed:",
    sum(ledger$rating_implied_candidate_computed), "\n")
cat("P13 2024 present-rate mismatches: 0\n")
cat("P14 legacy formula reproduction mismatches: 0\n")
cat("Admissibility or selection decisions made: 0\n")
