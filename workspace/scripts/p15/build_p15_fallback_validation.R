#!/usr/bin/env Rscript

# Build question-specific validation evidence for IDS Bondholders,
# rating-implied candidates, and peer-proxy candidates against the integrated
# P15 observed anchors. This is a provisional decision package only: it does
# not promote a fallback source or alter the selected ladder.

suppressPackageStartupMessages({
  library(data.table)
  library(digest)
  library(dplyr)
  library(readr)
  library(tibble)
})

source("R/p15_fallback_validation.R")
source("R/p15_peer_proxy.R")

root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
derived_dir <- file.path(
  root, "data-derived", "p15_fallback_validation_2012_2024_v1"
)
governance_dir <- file.path(root, "docs", "governance")
dir.create(derived_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(governance_dir, recursive = TRUE, showWarnings = FALSE)

input_rel <- c(
  anchors = paste0(
    "data-derived/p15_integrated_observed_candidate_2012_2024_v1/",
    "p15_integrated_observed_validation_anchors_2012_2024.csv.gz"
  ),
  grid = paste0(
    "data-derived/p15_platform_2012_2024_v1/",
    "p15_country_year_grid_2012_2024.csv"
  ),
  status = paste0(
    "data-derived/p15_status_context_2012_2024_v1/",
    "p15_status_context_evidence_2012_2024.csv.gz"
  ),
  ids = paste0(
    "data-derived/p15_ids_evidence_2012_2024_v1/",
    "p15_ids_bondholders_country_year_evidence_2012_2024.csv.gz"
  ),
  rating = paste0(
    "data-derived/p15_rating_validation_2012_2024_v1/",
    "p15_rating_candidate_variants_2012_2024.csv.gz"
  )
)
input_paths <- stats::setNames(file.path(root, unname(input_rel)), names(input_rel))
stopifnot(all(file.exists(input_paths)))

read_input <- function(name) {
  readr::read_csv(
    input_paths[[name]], show_col_types = FALSE, guess_max = 100000,
    name_repair = "unique"
  )
}

anchors <- read_input("anchors")
grid <- read_input("grid")
status <- read_input("status")
ids <- read_input("ids")
rating <- read_input("rating")

build_id <- "BUILD-P15-FALLBACK-VALIDATION-20260817-V1"
method_id <- "VAL-P15-QUESTION-SPECIFIC-PROVISIONAL-20260817-V1"

# ---------------------------------------------------------------------------
# 1. Provisional question-specific anchor authority.
# ---------------------------------------------------------------------------

authority <- p15_expand_validation_anchor_authority(anchors)
authority_path <- file.path(
  derived_dir, "p15_validation_anchor_authority_2012_2024.csv.gz"
)
data.table::fwrite(data.table::as.data.table(authority), authority_path, na = "")

question_register <- tibble::tribble(
  ~validation_question_id, ~manager_meaning, ~preferred_observed_object, ~important_limit, ~decision_state,
  "Q-USD-NEW-BORROWING", "Can the fallback approximate what a sovereign paid when it borrowed in USD during the year?", "Observed USD primary issuance", "The country-year primary rate aggregates issues across the year; it is not an exact issue-date match.", "provisional_pending_owner_review",
  "Q-USD-YEAR-END-MARKET", "Can the fallback approximate the USD market price of sovereign risk at year-end?", "Directly reported USD secondary YTM, with strict price-derived YTM as supporting evidence", "The current year-end window remains a candidate until SEC-18 and does not measure the full-year borrowing cost.", "provisional_pending_owner_review",
  "Q-IDS-SOURCE-CONSISTENCY", "How closely does the IDS Bondholders country-year contractual average align with observed market-rate aggregates?", "Compare alternative country-year measurements of the broad borrowing-cost concept", "IDS and P15 primary are both amount-weighted annual aggregates, but IDS does not expose security-level issue yield, exact currency composition, or issue timing; preserve those distinctions in one common platform.", "direction_approved_common_platform_distinctions_retained",
  "Q-COVERAGE-OVERLAP", "Where do fallback and observed evidence coexist?", "Every retained integrated observed anchor", "Coverage overlap says nothing by itself about rate accuracy.", "provisional_pending_owner_review"
)
question_register_path <- file.path(
  governance_dir, "p15_fallback_validation_question_register.csv"
)
data.table::fwrite(
  data.table::as.data.table(question_register), question_register_path, na = ""
)

authority_summary <- authority |>
  dplyr::count(
    .data$validation_question_id, .data$authority_class,
    .data$anchor_branch, .data$anchor_currency,
    .data$use_in_provisional_summary,
    name = "anchor_rows"
  ) |>
  dplyr::arrange(
    .data$validation_question_id, dplyr::desc(.data$anchor_rows),
    .data$authority_class
  )
authority_summary_path <- file.path(
  governance_dir, "p15_fallback_validation_authority_summary.csv"
)
data.table::fwrite(
  data.table::as.data.table(authority_summary), authority_summary_path, na = ""
)

# ---------------------------------------------------------------------------
# 2. IDS source-object consistency. These are discrepancies, not accuracy
#    errors, because IDS and market observations are different empirical objects.
# ---------------------------------------------------------------------------

ids_usable <- ids |>
  dplyr::filter(
    .data$bondholder_term_validity_state ==
      "bondholder_terms_usable_subject_to_source_checks",
    is.finite(.data$official_rate_pct)
  ) |>
  dplyr::transmute(
    analysis_year = as.integer(.data$analysis_year),
    iso3 = as.character(.data$iso3),
    ids_rate_pct = as.numeric(.data$official_rate_pct),
    ids_maturity_years = as.numeric(.data$official_maturity_years),
    ids_grace_years = as.numeric(.data$official_grace_years),
    ids_rate_class = as.character(.data$ids_rate_class),
    ids_term_shape = as.character(.data$ids_term_shape),
    ids_source_package_id = as.character(.data$ids_source_package_id),
    ids_evidence_id = as.character(.data$ids_evidence_id)
  )
stopifnot(!anyDuplicated(ids_usable[c("analysis_year", "iso3")]))

ids_detail <- authority |>
  dplyr::filter(
    .data$validation_question_id == "Q-IDS-SOURCE-CONSISTENCY"
  ) |>
  dplyr::inner_join(ids_usable, by = c("analysis_year", "iso3")) |>
  dplyr::mutate(
    fallback_family = "ids_bondholders_contractual_average",
    fallback_rate_pct = .data$ids_rate_pct,
    signed_gap_pp = .data$fallback_rate_pct - .data$anchor_rate_pct,
    abs_gap_pp = abs(.data$signed_gap_pp),
    comparison_interpretation =
      "descriptive_source_object_difference_not_accuracy_error",
    selection_or_promotion_state = "not_evaluated"
  )
ids_detail_path <- file.path(
  derived_dir, "p15_ids_integrated_anchor_validation_detail_2012_2024.csv.gz"
)
data.table::fwrite(data.table::as.data.table(ids_detail), ids_detail_path, na = "")

ids_summary <- p15_summarise_fallback_gaps(
  ids_detail,
  c("validation_question_id", "anchor_branch", "anchor_currency")
) |>
  dplyr::mutate(
    metric_interpretation =
      "descriptive discrepancy; do not call mean absolute gap an accuracy score"
  )
ids_summary_path <- file.path(
  governance_dir, "p15_ids_integrated_anchor_validation_summary.csv"
)
data.table::fwrite(data.table::as.data.table(ids_summary), ids_summary_path, na = "")

# ---------------------------------------------------------------------------
# 3. Rating-implied validation. Preserve all comparisons, but calculate the
#    provisional accuracy summaries only against same-currency target objects.
# ---------------------------------------------------------------------------

rating_available <- rating |>
  dplyr::filter(.data$variant_available, is.finite(.data$variant_rate_pct)) |>
  dplyr::select(
    "analysis_year", "iso3", "variant_id", "variant_label",
    "variant_family", "variant_role", "variant_rating",
    "variant_rating_normalized", "variant_agency", "agency_rule",
    "timing_rule", "spread_rule", "variant_spread_pct",
    "risk_free_7y_pct", "variant_rate_pct", "selected_rating_age_band",
    "agency_disagreement_state", "rating_source_region",
    "historical_income_level"
  )

rating_detail <- authority |>
  dplyr::filter(.data$validation_question_id %in% c(
    "Q-USD-NEW-BORROWING", "Q-USD-YEAR-END-MARKET"
  )) |>
  dplyr::inner_join(
    rating_available, by = c("analysis_year", "iso3"),
    relationship = "many-to-many"
  ) |>
  dplyr::mutate(
    fallback_family = "rating_implied_usd_model",
    fallback_rate_pct = .data$variant_rate_pct,
    signed_gap_pp = .data$fallback_rate_pct - .data$anchor_rate_pct,
    abs_gap_pp = abs(.data$signed_gap_pp),
    model_timing_fit = dplyr::case_when(
      .data$validation_question_id == "Q-USD-NEW-BORROWING" &
        grepl("January 1", .data$timing_rule, ignore.case = TRUE) ~
        "beginning_of_year_rating_with_annual_risk_free_best_available_not_issue_date_exact",
      .data$validation_question_id == "Q-USD-YEAR-END-MARKET" &
        grepl("next January 1", .data$timing_rule, ignore.case = TRUE) ~
        "end_year_rating_proxy_but_annual_risk_free_not_exact_year_end",
      TRUE ~ "timing_not_aligned_to_validation_target"
    ),
    accuracy_summary_eligible = .data$use_in_provisional_summary,
    selection_or_promotion_state = "not_evaluated"
  )
rating_detail_path <- file.path(
  derived_dir,
  "p15_rating_integrated_anchor_validation_detail_2012_2024.csv.gz"
)
data.table::fwrite(
  data.table::as.data.table(rating_detail), rating_detail_path, na = ""
)

add_sample_cuts <- function(data) {
  dplyr::bind_rows(
    data |>
      dplyr::mutate(validation_sample = "all_scope"),
    data |>
      dplyr::filter(.data$historical_lmic_reporting_scope %in% TRUE) |>
      dplyr::mutate(validation_sample = "historical_lmic_scope"),
    data |>
      dplyr::filter(!.data$anchor_thin_evidence) |>
      dplyr::mutate(validation_sample = "nonthin_anchor_only"),
    data |>
      dplyr::filter(
        .data$historical_lmic_reporting_scope %in% TRUE,
        !.data$anchor_thin_evidence
      ) |>
      dplyr::mutate(validation_sample = "historical_lmic_nonthin_anchor")
  )
}

rating_summary <- rating_detail |>
  dplyr::filter(.data$accuracy_summary_eligible) |>
  add_sample_cuts() |>
  p15_summarise_fallback_gaps(c(
    "validation_sample", "validation_question_id", "authority_class",
    "variant_id", "variant_label", "model_timing_fit"
  )) |>
  dplyr::arrange(
    .data$validation_sample, .data$validation_question_id,
    .data$mean_abs_gap_pp, .data$variant_id
  )
rating_summary_path <- file.path(
  governance_dir, "p15_rating_integrated_anchor_validation_summary.csv"
)
data.table::fwrite(
  data.table::as.data.table(rating_summary), rating_summary_path, na = ""
)

validation_period <- function(year) {
  dplyr::case_when(
    year <= 2015L ~ "2012-2015",
    year <= 2019L ~ "2016-2019",
    year <= 2023L ~ "2020-2023",
    TRUE ~ "2024_anchor"
  )
}
summarise_rating_breakdown <- function(data, dimension, field) {
  data |>
    dplyr::mutate(
      breakdown_dimension = dimension,
      breakdown_value = as.character(.data[[field]])
    ) |>
    p15_summarise_fallback_gaps(c(
      "validation_question_id", "variant_id", "breakdown_dimension",
      "breakdown_value"
    ))
}
rating_breakdown_base <- rating_detail |>
  dplyr::filter(
    .data$accuracy_summary_eligible,
    .data$historical_lmic_reporting_scope %in% TRUE
  ) |>
  dplyr::mutate(
    validation_period = validation_period(.data$analysis_year),
    thin_evidence_class = dplyr::if_else(
      .data$anchor_thin_evidence, "thin", "nonthin"
    )
  )
rating_breakdown_summary <- dplyr::bind_rows(
  summarise_rating_breakdown(
    rating_breakdown_base, "period", "validation_period"
  ),
  summarise_rating_breakdown(
    rating_breakdown_base, "income", "historical_income_level"
  ),
  summarise_rating_breakdown(
    rating_breakdown_base, "thin_evidence", "thin_evidence_class"
  ),
  summarise_rating_breakdown(
    rating_breakdown_base, "status_context", "anchor_status_rule_class"
  ),
  summarise_rating_breakdown(
    rating_breakdown_base, "rating_age", "selected_rating_age_band"
  ),
  summarise_rating_breakdown(
    rating_breakdown_base, "agency_disagreement",
    "agency_disagreement_state"
  ),
  summarise_rating_breakdown(
    rating_breakdown_base, "anchor_evidence_tier", "anchor_evidence_tier"
  )
) |>
  dplyr::arrange(
    .data$validation_question_id, .data$variant_id,
    .data$breakdown_dimension, .data$breakdown_value
  )
rating_breakdown_path <- file.path(
  governance_dir, "p15_rating_integrated_validation_breakdown_summary.csv"
)
data.table::fwrite(
  data.table::as.data.table(rating_breakdown_summary),
  rating_breakdown_path, na = ""
)

# ---------------------------------------------------------------------------
# 4. Rebuild peer candidates from the integrated P15 observed USD anchors.
#    Primary-only and primary-first-with-secondary variants remain separate.
# ---------------------------------------------------------------------------

status_grid <- grid |>
  dplyr::left_join(
    status |>
      dplyr::transmute(
        analysis_year = as.integer(.data$analysis_year),
        iso3 = as.character(.data$iso3),
        positive_status_context = dplyr::coalesce(
          as.logical(.data$positive_status_evidence_present), FALSE
        )
      ),
    by = c("analysis_year", "iso3")
  ) |>
  dplyr::mutate(
    positive_status_context = dplyr::coalesce(
      .data$positive_status_context, FALSE
    )
  )
stopifnot(!anyDuplicated(status_grid[c("analysis_year", "iso3")]))

peer_seeds <- anchors |>
  dplyr::filter(.data$currency == "USD") |>
  dplyr::select(-dplyr::any_of("historical_income_level")) |>
  dplyr::left_join(
    status_grid |>
      dplyr::select(
        "analysis_year", "iso3", "historical_income_level",
        "positive_status_context"
      ),
    by = c("analysis_year", "iso3")
  ) |>
  dplyr::transmute(
    analysis_year = as.integer(.data$analysis_year),
    iso3 = as.character(.data$iso3),
    country = as.character(.data$country),
    historical_income_level = as.character(.data$historical_income_level),
    positive_status_context = .data$positive_status_context,
    seed_pool_class = dplyr::if_else(
      .data$observed_market_branch == "observed_primary",
      "strict_observed", "broad_observed"
    ),
    seed_priority = dplyr::case_when(
      .data$observed_market_branch == "observed_primary" ~ 10L,
      !grepl("price_derived", .data$candidate_evidence_tier,
             ignore.case = TRUE) ~ 20L,
      TRUE ~ 30L
    ),
    seed_rate_pct = as.numeric(.data$market_rate_pct),
    seed_maturity_years = as.numeric(.data$market_maturity_years),
    seed_source_family = dplyr::if_else(
      .data$observed_market_branch == "observed_primary",
      "integrated_observed_primary_usd",
      "integrated_observed_secondary_usd"
    ),
    seed_source_tier = as.character(.data$candidate_evidence_tier),
    seed_source_package_id = as.character(.data$included_source_package_ids),
    seed_record_locator = as.character(.data$source_evidence_row_id),
    seed_is_model_or_proxy = FALSE,
    seed_dependency_state = "current_integrated_p15_observed_anchor"
  )
p15_peer_validate_seed_ledger(peer_seeds)
stopifnot(all(!is.na(peer_seeds$historical_income_level)))

peer_variant_register <- tibble::tribble(
  ~peer_variant_id, ~seed_rule, ~minimum_same_income_count, ~exclude_positive_status_context, ~calculation_state, ~manager_meaning,
  "PEER-P15-USD-PRIMARY-INC5-ALLSTATUS-V1", "strict_observed", 5L, FALSE, "computed_candidate", "USD primary seeds only; same-income pool if at least five peers; status context retained.",
  "PEER-P15-USD-PRIMARY-INC5-STATUSEXCL-V1", "strict_observed", 5L, TRUE, "computed_candidate", "USD primary seeds only; same-income pool if at least five peers; status-context peers excluded as a sensitivity.",
  "PEER-P15-USD-PRIMARY-INC3-ALLSTATUS-V1", "strict_observed", 3L, FALSE, "computed_candidate", "USD primary seeds only; lower three-peer same-income threshold; status context retained.",
  "PEER-P15-USD-PRIMARY-INC3-STATUSEXCL-V1", "strict_observed", 3L, TRUE, "computed_candidate", "USD primary seeds only; lower three-peer same-income threshold; status-context peers excluded.",
  "PEER-P15-USD-PRIMARY-FIRST-SECONDARY-INC5-ALLSTATUS-V1", "broad_observed", 5L, FALSE, "computed_candidate", "Use USD primary where available, otherwise direct then strict repaired secondary; status context retained.",
  "PEER-P15-USD-PRIMARY-FIRST-SECONDARY-INC5-STATUSEXCL-V1", "broad_observed", 5L, TRUE, "computed_candidate", "Use USD primary where available, otherwise direct then strict repaired secondary; status-context peers excluded as a sensitivity.",
  "PEER-P15-USD-PRIMARY-FIRST-SECONDARY-INC3-ALLSTATUS-V1", "broad_observed", 3L, FALSE, "computed_candidate", "Use USD primary where available, otherwise direct then strict repaired secondary; lower three-peer same-income threshold; status context retained."
) |>
  dplyr::mutate(
    peer_decision_state = "provisional_candidate_not_approved",
    source_currency = "USD",
    peer_evidence_schema_version = p15_peer_proxy_schema_version()
  )
peer_variant_register_path <- file.path(
  governance_dir, "p15_integrated_peer_method_register.csv"
)
data.table::fwrite(
  data.table::as.data.table(peer_variant_register),
  peer_variant_register_path, na = ""
)

peer_build <- p15_build_peer_candidates(
  status_grid, peer_seeds, peer_variant_register
)
peer_candidate_path <- file.path(
  derived_dir, "p15_peer_integrated_candidate_variants_2012_2024.csv.gz"
)
peer_member_path <- file.path(
  derived_dir, "p15_peer_integrated_membership_ledger_2012_2024.csv.gz"
)
data.table::fwrite(
  data.table::as.data.table(peer_build$candidates), peer_candidate_path, na = ""
)
data.table::fwrite(
  data.table::as.data.table(peer_build$members), peer_member_path, na = ""
)

peer_detail <- authority |>
  dplyr::filter(.data$validation_question_id %in% c(
    "Q-USD-NEW-BORROWING", "Q-USD-YEAR-END-MARKET"
  )) |>
  dplyr::inner_join(
    peer_build$candidates |>
      dplyr::filter(.data$candidate_computed),
    by = c("analysis_year", "iso3"), relationship = "many-to-many",
    suffix = c("_anchor", "_peer")
  ) |>
  dplyr::mutate(
    country = .data$country_anchor,
    historical_lmic_reporting_scope =
      .data$historical_lmic_reporting_scope_anchor,
    historical_income_level = .data$historical_income_level,
    fallback_family = "target_excluding_same_year_peer_proxy_usd",
    fallback_rate_pct = .data$peer_rate_median_pct,
    signed_gap_pp = .data$fallback_rate_pct - .data$anchor_rate_pct,
    abs_gap_pp = abs(.data$signed_gap_pp),
    accuracy_summary_eligible = .data$use_in_provisional_summary,
    peer_source_object_fit = dplyr::case_when(
      .data$validation_question_id == "Q-USD-NEW-BORROWING" &
        .data$seed_rule == "strict_observed" ~
        "same_currency_same_primary_object_peer_pool",
      .data$validation_question_id == "Q-USD-NEW-BORROWING" ~
        "same_currency_primary_first_mixed_observed_object_pool",
      .data$validation_question_id == "Q-USD-YEAR-END-MARKET" &
        .data$seed_rule == "broad_observed" ~
        "same_currency_but_primary_first_mixed_observed_object_pool",
      TRUE ~ "same_currency_primary_peer_pool_not_year_end_object"
    ),
    selection_or_promotion_state = "not_evaluated"
  )
peer_detail_path <- file.path(
  derived_dir, "p15_peer_integrated_anchor_validation_detail_2012_2024.csv.gz"
)
data.table::fwrite(
  data.table::as.data.table(peer_detail), peer_detail_path, na = ""
)

peer_summary <- peer_detail |>
  dplyr::filter(.data$accuracy_summary_eligible) |>
  add_sample_cuts() |>
  p15_summarise_fallback_gaps(c(
    "validation_sample", "validation_question_id", "authority_class",
    "peer_variant_id", "seed_rule", "peer_source_object_fit"
  )) |>
  dplyr::left_join(
    peer_build$candidates |>
      dplyr::group_by(.data$peer_variant_id) |>
      dplyr::summarise(
        candidate_country_years = sum(.data$candidate_computed),
        median_peer_count = stats::median(.data$peer_country_count),
        global_fallback_share = mean(grepl(
          "global_same_year_fallback", .data$peer_pool_rule
        )),
        mean_peer_iqr_width_pp = mean(
          .data$peer_rate_iqr_width_pct, na.rm = TRUE
        ),
        .groups = "drop"
      ),
    by = "peer_variant_id"
  ) |>
  dplyr::arrange(
    .data$validation_sample, .data$validation_question_id,
    .data$mean_abs_gap_pp, .data$peer_variant_id
  )
peer_summary_path <- file.path(
  governance_dir, "p15_peer_integrated_anchor_validation_summary.csv"
)
data.table::fwrite(
  data.table::as.data.table(peer_summary), peer_summary_path, na = ""
)

summarise_peer_breakdown <- function(data, dimension, field) {
  data |>
    dplyr::mutate(
      breakdown_dimension = dimension,
      breakdown_value = as.character(.data[[field]])
    ) |>
    p15_summarise_fallback_gaps(c(
      "validation_question_id", "peer_variant_id", "breakdown_dimension",
      "breakdown_value"
    ))
}
peer_breakdown_base <- peer_detail |>
  dplyr::filter(
    .data$accuracy_summary_eligible,
    .data$historical_lmic_reporting_scope %in% TRUE
  ) |>
  dplyr::mutate(
    validation_period = validation_period(.data$analysis_year),
    thin_evidence_class = dplyr::if_else(
      .data$anchor_thin_evidence, "thin", "nonthin"
    ),
    pool_scope = dplyr::if_else(
      grepl("global_same_year_fallback", .data$peer_pool_rule),
      "global_fallback", "same_income_pool"
    ),
    peer_count_band = dplyr::case_when(
      .data$peer_country_count < 5 ~ "fewer_than_5",
      .data$peer_country_count < 10 ~ "5_to_9",
      TRUE ~ "10_or_more"
    )
  )
peer_breakdown_summary <- dplyr::bind_rows(
  summarise_peer_breakdown(
    peer_breakdown_base, "period", "validation_period"
  ),
  summarise_peer_breakdown(
    peer_breakdown_base, "income", "historical_income_level"
  ),
  summarise_peer_breakdown(
    peer_breakdown_base, "thin_evidence", "thin_evidence_class"
  ),
  summarise_peer_breakdown(
    peer_breakdown_base, "status_context", "anchor_status_rule_class"
  ),
  summarise_peer_breakdown(
    peer_breakdown_base, "pool_scope", "pool_scope"
  ),
  summarise_peer_breakdown(
    peer_breakdown_base, "peer_count", "peer_count_band"
  ),
  summarise_peer_breakdown(
    peer_breakdown_base, "anchor_evidence_tier", "anchor_evidence_tier"
  )
) |>
  dplyr::arrange(
    .data$validation_question_id, .data$peer_variant_id,
    .data$breakdown_dimension, .data$breakdown_value
  )
peer_breakdown_path <- file.path(
  governance_dir, "p15_peer_integrated_validation_breakdown_summary.csv"
)
data.table::fwrite(
  data.table::as.data.table(peer_breakdown_summary),
  peer_breakdown_path, na = ""
)

# ---------------------------------------------------------------------------
# 5. Coverage and worst-case review surfaces.
# ---------------------------------------------------------------------------

observed_country_years <- anchors |>
  dplyr::distinct(.data$analysis_year, .data$iso3)
coverage_summary <- dplyr::bind_rows(
  ids |>
    dplyr::transmute(
      analysis_year, iso3,
      fallback_family = "ids_bondholders_contractual_average",
      candidate_available =
        .data$bondholder_term_validity_state ==
          "bondholder_terms_usable_subject_to_source_checks" &
          is.finite(.data$official_rate_pct),
      historical_lmic_reporting_scope
    ),
  rating |>
    dplyr::transmute(
      analysis_year, iso3,
      fallback_family = paste0("rating::", .data$variant_id),
      candidate_available = .data$variant_available,
      historical_lmic_reporting_scope
    ),
  peer_build$candidates |>
    dplyr::transmute(
      analysis_year, iso3,
      fallback_family = paste0("peer::", .data$peer_variant_id),
      candidate_available = .data$candidate_computed,
      historical_lmic_reporting_scope
    )
) |>
  dplyr::left_join(
    observed_country_years |>
      dplyr::mutate(observed_evidence_available = TRUE),
    by = c("analysis_year", "iso3")
  ) |>
  dplyr::mutate(
    observed_evidence_available = dplyr::coalesce(
      .data$observed_evidence_available, FALSE
    )
  ) |>
  dplyr::group_by(.data$fallback_family) |>
  dplyr::summarise(
    grid_rows = dplyr::n(),
    available_rows = sum(.data$candidate_available),
    available_lmic_rows = sum(
      .data$candidate_available &
        .data$historical_lmic_reporting_scope %in% TRUE
    ),
    observed_overlap_rows = sum(
      .data$candidate_available & .data$observed_evidence_available
    ),
    observed_overlap_lmic_rows = sum(
      .data$candidate_available & .data$observed_evidence_available &
        .data$historical_lmic_reporting_scope %in% TRUE
    ),
    .groups = "drop"
  ) |>
  dplyr::arrange(.data$fallback_family)
coverage_summary_path <- file.path(
  governance_dir, "p15_fallback_integrated_coverage_summary.csv"
)
data.table::fwrite(
  data.table::as.data.table(coverage_summary), coverage_summary_path, na = ""
)

worst_cases <- dplyr::bind_rows(
  ids_detail |>
    dplyr::transmute(
      fallback_family, fallback_variant = "IDS-BONDHOLDERS",
      validation_question_id, analysis_year, iso3, country,
      anchor_branch, anchor_currency, authority_class,
      anchor_rate_pct, fallback_rate_pct, signed_gap_pp, abs_gap_pp,
      interpretation = comparison_interpretation
    ),
  rating_detail |>
    dplyr::filter(.data$accuracy_summary_eligible) |>
    dplyr::transmute(
      fallback_family, fallback_variant = .data$variant_id,
      validation_question_id, analysis_year, iso3, country,
      anchor_branch, anchor_currency, authority_class,
      anchor_rate_pct, fallback_rate_pct, signed_gap_pp, abs_gap_pp,
      interpretation = .data$model_timing_fit
    ),
  peer_detail |>
    dplyr::filter(.data$accuracy_summary_eligible) |>
    dplyr::transmute(
      fallback_family, fallback_variant = .data$peer_variant_id,
      validation_question_id, analysis_year, iso3, country,
      anchor_branch, anchor_currency, authority_class,
      anchor_rate_pct, fallback_rate_pct, signed_gap_pp, abs_gap_pp,
      interpretation = .data$peer_source_object_fit
    )
) |>
  dplyr::group_by(
    .data$fallback_family, .data$fallback_variant,
    .data$validation_question_id
  ) |>
  dplyr::slice_max(.data$abs_gap_pp, n = 20L, with_ties = FALSE) |>
  dplyr::ungroup() |>
  dplyr::arrange(
    .data$fallback_family, .data$fallback_variant,
    .data$validation_question_id, dplyr::desc(.data$abs_gap_pp)
  )
worst_cases_path <- file.path(
  governance_dir, "p15_fallback_integrated_worst_case_review.csv.gz"
)
data.table::fwrite(data.table::as.data.table(worst_cases), worst_cases_path, na = "")

# ---------------------------------------------------------------------------
# 6. Build record and manifest.
# ---------------------------------------------------------------------------

build_summary <- tibble::tibble(
  metric = c(
    "integrated_anchor_rows", "authority_rows", "ids_usable_rows",
    "ids_matched_rows", "rating_available_variant_rows",
    "rating_matched_rows", "peer_seed_rows", "peer_candidate_rows",
    "peer_membership_rows", "peer_matched_rows"
  ),
  value = c(
    nrow(anchors), nrow(authority), nrow(ids_usable), nrow(ids_detail),
    nrow(rating_available), nrow(rating_detail), nrow(peer_seeds),
    nrow(peer_build$candidates), nrow(peer_build$members), nrow(peer_detail)
  ),
  build_id = build_id,
  method_id = method_id,
  decision_state = "provisional_evidence_no_promotion"
)
build_summary_path <- file.path(
  governance_dir, "p15_fallback_validation_build_summary.csv"
)
data.table::fwrite(
  data.table::as.data.table(build_summary), build_summary_path, na = ""
)

output_paths <- c(
  authority_path, question_register_path, authority_summary_path,
  ids_detail_path, ids_summary_path, rating_detail_path, rating_summary_path,
  rating_breakdown_path,
  peer_variant_register_path, peer_candidate_path, peer_member_path,
  peer_detail_path, peer_summary_path, peer_breakdown_path, coverage_summary_path,
  worst_cases_path, build_summary_path
)
code_rel <- c(
  "R/p15_fallback_validation.R", "R/p15_peer_proxy.R",
  "scripts/p15/build_p15_fallback_validation.R",
  "tests/testthat/test-p15-fallback-validation.R"
)
stopifnot(all(file.exists(file.path(root, code_rel))))
manifest <- dplyr::bind_rows(
  tibble::tibble(
    artifact_role = "input", path = unname(input_rel),
    sha256 = vapply(
      input_paths, digest::digest, character(1), algo = "sha256", file = TRUE
    )
  ),
  tibble::tibble(
    artifact_role = "output",
    path = sub(paste0("^", root, "/"), "", output_paths),
    sha256 = vapply(
      output_paths, digest::digest, character(1), algo = "sha256", file = TRUE
    )
  ),
  tibble::tibble(
    artifact_role = "code", path = code_rel,
    sha256 = vapply(
      file.path(root, code_rel), digest::digest, character(1),
      algo = "sha256", file = TRUE
    )
  )
) |>
  dplyr::mutate(
    build_id = build_id,
    method_id = method_id,
    schema_version = p15_fallback_validation_schema_version()
  )
manifest_path <- file.path(
  governance_dir, "p15_fallback_validation_manifest.csv"
)
data.table::fwrite(data.table::as.data.table(manifest), manifest_path, na = "")

stopifnot(
  all(authority$authority_decision_state ==
        "provisional_proposal_pending_owner_review"),
  all(rating_detail$selection_or_promotion_state == "not_evaluated"),
  all(peer_detail$selection_or_promotion_state == "not_evaluated"),
  all(peer_build$candidates$target_excluded),
  all(peer_build$candidates$nonrecursive_seed_pool),
  all(peer_build$members$target_excluded),
  !any(peer_build$members$recursive_seed_flag),
  any(!grepl(
    "global_same_year_fallback", peer_build$candidates$peer_pool_rule
  )),
  !anyDuplicated(peer_build$candidates[c(
    "peer_variant_id", "analysis_year", "iso3"
  )])
)

message(
  "Built provisional P15 fallback validation: ", nrow(ids_detail),
  " IDS overlaps; ", nrow(rating_detail), " rating comparisons; ",
  nrow(peer_detail), " peer comparisons. No fallback promoted."
)
