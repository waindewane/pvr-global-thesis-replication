#!/usr/bin/env Rscript

# Run the bounded owner-requested comparison of rating-agency constructions,
# peer-proxy constructions, and IDS-versus-primary country-year comparability.
# This build creates decision evidence only and changes no selected ladder.

suppressPackageStartupMessages({
  library(data.table)
  library(digest)
  library(dplyr)
  library(readr)
  library(tidyr)
  library(tibble)
})

source("R/p15_bounded_fallback_comparison.R")

root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
derived_dir <- file.path(
  root, "data-derived", "p15_bounded_fallback_comparison_2012_2024_v1"
)
governance_dir <- file.path(root, "docs", "governance")
dir.create(derived_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(governance_dir, recursive = TRUE, showWarnings = FALSE)

input_rel <- c(
  rating_detail = paste0(
    "data-derived/p15_fallback_validation_2012_2024_v1/",
    "p15_rating_integrated_anchor_validation_detail_2012_2024.csv.gz"
  ),
  rating_variants = paste0(
    "data-derived/p15_rating_validation_2012_2024_v1/",
    "p15_rating_candidate_variants_2012_2024.csv.gz"
  ),
  rating_ledger = paste0(
    "data-derived/p15_rating_components_2012_2024_v1/",
    "p15_rating_component_ledger_2012_2024.csv.gz"
  ),
  observed_anchors = paste0(
    "data-derived/p15_integrated_observed_candidate_2012_2024_v1/",
    "p15_integrated_observed_validation_anchors_2012_2024.csv.gz"
  ),
  ids_detail = paste0(
    "data-derived/p15_fallback_validation_2012_2024_v1/",
    "p15_ids_integrated_anchor_validation_detail_2012_2024.csv.gz"
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

rating_detail <- read_input("rating_detail")
rating_variants <- read_input("rating_variants")
rating_ledger <- read_input("rating_ledger")
observed_anchors <- read_input("observed_anchors")
ids_detail <- read_input("ids_detail")

build_id <- "BUILD-P15-BOUNDED-FALLBACK-COMPARISON-20260820-V1"
schema_version <- p15_bounded_fallback_schema_version()

write_csv <- function(data, filename, compressed = FALSE) {
  path <- file.path(if (compressed) derived_dir else governance_dir, filename)
  data.table::fwrite(data.table::as.data.table(data), path, na = "")
  path
}

# ---------------------------------------------------------------------------
# 1. Rating-agency comparison.
# ---------------------------------------------------------------------------

rating_ids <- c(
  moodys = "boy_moodys_only_rating_group_median_dgs7",
  fitch = "boy_fitch_only_rating_group_median_dgs7",
  precedence = "boy_selected_precedence_rating_group_median_dgs7",
  available_median = "boy_available_agencies_median_mapped_spread_dgs7"
)

rating_sample <- rating_detail |>
  dplyr::filter(
    .data$validation_question_id == "Q-USD-NEW-BORROWING",
    .data$historical_lmic_reporting_scope %in% TRUE,
    .data$accuracy_summary_eligible %in% TRUE,
    .data$variant_id %in% unname(rating_ids)
  ) |>
  dplyr::mutate(
    rating_method = names(rating_ids)[match(.data$variant_id, rating_ids)]
  ) |>
  dplyr::select(
    analysis_year, iso3, country, rating_method, variant_id,
    variant_rate_pct, anchor_rate_pct, signed_gap_pp, abs_gap_pp,
    variant_agency, agency_disagreement_state, selected_rating_age_band
  )
stopifnot(!anyDuplicated(rating_sample[c(
  "analysis_year", "iso3", "rating_method"
)]))

rating_coverage <- rating_variants |>
  dplyr::filter(.data$variant_id %in% unname(rating_ids)) |>
  dplyr::mutate(
    rating_method = names(rating_ids)[match(.data$variant_id, rating_ids)]
  ) |>
  dplyr::group_by(.data$rating_method, .data$variant_id) |>
  dplyr::summarise(
    all_country_years_available = sum(.data$variant_available %in% TRUE),
    all_countries_available = dplyr::n_distinct(
      .data$iso3[.data$variant_available %in% TRUE]
    ),
    lmic_country_years_available = sum(
      .data$variant_available %in% TRUE &
        .data$historical_lmic_reporting_scope %in% TRUE
    ),
    lmic_countries_available = dplyr::n_distinct(
      .data$iso3[
        .data$variant_available %in% TRUE &
          .data$historical_lmic_reporting_scope %in% TRUE
      ]
    ),
    .groups = "drop"
  ) |>
  dplyr::mutate(build_id = build_id, schema_version = schema_version)

rating_all_available <- rating_sample |>
  dplyr::group_by(.data$rating_method, .data$variant_id) |>
  dplyr::summarise(
    sample_basis = "each_method_all_available_primary_overlaps",
    matched_rows = dplyr::n(),
    countries = dplyr::n_distinct(.data$iso3),
    years = dplyr::n_distinct(.data$analysis_year),
    mean_signed_gap_pp = mean(.data$signed_gap_pp),
    mean_abs_gap_pp = mean(.data$abs_gap_pp),
    median_abs_gap_pp = stats::median(.data$abs_gap_pp),
    rmse_gap_pp = sqrt(mean(.data$signed_gap_pp^2)),
    within_1pp_share = mean(.data$abs_gap_pp <= 1),
    within_2pp_share = mean(.data$abs_gap_pp <= 2),
    .groups = "drop"
  )

rating_wide <- rating_sample |>
  dplyr::select(
    analysis_year, iso3, country, rating_method, variant_rate_pct,
    anchor_rate_pct
  ) |>
  tidyr::pivot_wider(names_from = "rating_method", values_from = "variant_rate_pct")

summarise_rating_common <- function(data, methods, label) {
  common <- data |>
    dplyr::filter(dplyr::if_all(dplyr::all_of(methods), is.finite))
  dplyr::bind_rows(lapply(methods, function(method) {
    gap <- common[[method]] - common$anchor_rate_pct
    tibble::tibble(
      rating_method = method,
      variant_id = unname(rating_ids[[method]]),
      sample_basis = label,
      matched_rows = nrow(common),
      countries = dplyr::n_distinct(common$iso3),
      years = dplyr::n_distinct(common$analysis_year),
      mean_signed_gap_pp = mean(gap),
      mean_abs_gap_pp = mean(abs(gap)),
      median_abs_gap_pp = stats::median(abs(gap)),
      rmse_gap_pp = sqrt(mean(gap^2)),
      within_1pp_share = mean(abs(gap) <= 1),
      within_2pp_share = mean(abs(gap) <= 2)
    )
  }))
}

rating_summary <- dplyr::bind_rows(
  rating_all_available,
  summarise_rating_common(
    rating_wide, c("moodys", "precedence", "available_median"),
    "common_moodys_precedence_available_median"
  ),
  summarise_rating_common(
    rating_wide, c("moodys", "fitch"), "common_moodys_fitch"
  )
) |>
  dplyr::mutate(
    decision_state = "bounded_comparison_no_automatic_promotion",
    build_id = build_id, schema_version = schema_version
  ) |>
  dplyr::arrange(.data$sample_basis, .data$mean_abs_gap_pp)

rating_pairwise <- dplyr::bind_rows(lapply(
  c("fitch", "precedence", "available_median"),
  function(comparator) {
    pair <- rating_wide |>
      dplyr::filter(
        is.finite(.data$moodys), is.finite(.data[[comparator]])
      ) |>
      dplyr::mutate(
        absolute_error_difference_pp =
          abs(.data[[comparator]] - .data$anchor_rate_pct) -
          abs(.data$moodys - .data$anchor_rate_pct)
      )
    p15_clustered_mean_difference(
      pair, "absolute_error_difference_pp"
    ) |>
      dplyr::mutate(
        baseline_method = "moodys",
        comparator_method = comparator,
        estimand = paste0(
          "mean_absolute_error_", comparator,
          "_minus_mean_absolute_error_moodys"
        ),
        negative_favors_comparator = TRUE,
        .before = 1L
      )
  }
)) |>
  dplyr::mutate(
    inference_method = "two_way_country_year_clustered",
    decision_state = "descriptive_and_inferential_evidence_not_promotion",
    build_id = build_id, schema_version = schema_version
  )

rating_yearly <- rating_sample |>
  dplyr::group_by(.data$rating_method, .data$analysis_year) |>
  dplyr::summarise(
    matched_rows = dplyr::n(),
    mean_signed_gap_pp = mean(.data$signed_gap_pp),
    mean_abs_gap_pp = mean(.data$abs_gap_pp),
    median_abs_gap_pp = stats::median(.data$abs_gap_pp),
    .groups = "drop"
  ) |>
  dplyr::mutate(build_id = build_id, schema_version = schema_version)

rating_country <- rating_sample |>
  dplyr::group_by(.data$rating_method, .data$iso3, .data$country) |>
  dplyr::summarise(
    matched_years = dplyr::n(),
    mean_signed_gap_pp = mean(.data$signed_gap_pp),
    mean_abs_gap_pp = mean(.data$abs_gap_pp),
    same_sign_share = max(
      mean(.data$signed_gap_pp >= 0), mean(.data$signed_gap_pp <= 0)
    ),
    .groups = "drop"
  ) |>
  dplyr::mutate(build_id = build_id, schema_version = schema_version)

rating_agency_composition <- rating_ledger |>
  dplyr::filter(.data$historical_lmic_reporting_scope %in% TRUE) |>
  dplyr::count(
    .data$preferred_agency_count, .data$preferred_agencies_available,
    name = "lmic_country_years"
  ) |>
  dplyr::mutate(
    share_of_lmic_grid = .data$lmic_country_years / sum(.data$lmic_country_years),
    build_id = build_id, schema_version = schema_version
  ) |>
  dplyr::arrange(.data$preferred_agency_count, .data$preferred_agencies_available)

# ---------------------------------------------------------------------------
# 2. Three-method peer comparison.
# ---------------------------------------------------------------------------

primary_usd <- observed_anchors |>
  dplyr::filter(
    .data$observed_market_branch == "observed_primary",
    .data$currency == "USD", is.finite(.data$market_rate_pct),
    is.finite(.data$sovereign_spread_pct)
  ) |>
  dplyr::select(
    analysis_year, iso3, country,
    historical_lmic_reporting_scope, market_rate_pct,
    market_maturity_years, sovereign_spread_pct
  ) |>
  dplyr::left_join(
    rating_ledger |>
      dplyr::select(analysis_year, iso3, historical_income_level),
    by = c("analysis_year", "iso3")
  )

rating_context <- rating_ledger |>
  dplyr::select(
    analysis_year, iso3, rating_source_region, moodys_rating_normalized,
    risk_free_7y_pct
  )

peer_build <- p15_build_bounded_peer_variants(
  primary_usd, rating_context, minimum_count = 3L
)
peer_detail <- peer_build$detail |>
  dplyr::mutate(build_id = build_id)
peer_members <- peer_build$members |>
  dplyr::mutate(build_id = build_id)

peer_summary <- dplyr::bind_rows(
  p15_summarise_bounded_method(peer_detail, "peer_method_id") |>
    dplyr::mutate(sample_basis = "all_primary_usd_validation_anchors"),
  p15_summarise_bounded_method(
    peer_detail |>
      dplyr::filter(.data$historical_lmic_reporting_scope %in% TRUE),
    "peer_method_id"
  ) |>
    dplyr::mutate(sample_basis = "historical_lmic_primary_usd_anchors")
) |>
  dplyr::mutate(
    decision_state = "bounded_comparison_no_automatic_promotion",
    build_id = build_id, schema_version = schema_version
  ) |>
  dplyr::arrange(.data$sample_basis, .data$mean_abs_gap_pp)

peer_wide <- peer_detail |>
  dplyr::filter(.data$historical_lmic_reporting_scope %in% TRUE) |>
  dplyr::select(
    analysis_year, iso3, anchor_rate_pct, peer_method_id, peer_rate_pct
  ) |>
  tidyr::pivot_wider(names_from = "peer_method_id", values_from = "peer_rate_pct")

peer_pairwise <- dplyr::bind_rows(lapply(
  c("same_income_spread_first_7y", "observable_similarity_raw_yield"),
  function(comparator) {
    pair <- peer_wide |>
      dplyr::filter(
        is.finite(.data$current_same_income_raw_yield),
        is.finite(.data[[comparator]])
      ) |>
      dplyr::mutate(
        absolute_error_difference_pp =
          abs(.data[[comparator]] - .data$anchor_rate_pct) -
          abs(.data$current_same_income_raw_yield - .data$anchor_rate_pct)
      )
    p15_clustered_mean_difference(
      pair, "absolute_error_difference_pp"
    ) |>
      dplyr::mutate(
        baseline_method = "current_same_income_raw_yield",
        comparator_method = comparator,
        estimand = paste0(
          "mean_absolute_error_", comparator,
          "_minus_current_same_income_raw_yield"
        ),
        negative_favors_comparator = TRUE,
        .before = 1L
      )
  }
)) |>
  dplyr::mutate(
    inference_method = "two_way_country_year_clustered",
    decision_state = "descriptive_and_inferential_evidence_not_promotion",
    build_id = build_id, schema_version = schema_version
  )

peer_yearly <- peer_detail |>
  dplyr::filter(.data$historical_lmic_reporting_scope %in% TRUE) |>
  dplyr::group_by(.data$peer_method_id, .data$analysis_year) |>
  dplyr::summarise(
    matched_rows = dplyr::n(),
    mean_signed_gap_pp = mean(.data$signed_gap_pp),
    mean_abs_gap_pp = mean(.data$abs_gap_pp),
    median_abs_gap_pp = stats::median(.data$abs_gap_pp),
    median_peer_count = stats::median(.data$peer_country_count),
    .groups = "drop"
  ) |>
  dplyr::mutate(build_id = build_id, schema_version = schema_version)

peer_pool_rules <- peer_detail |>
  dplyr::filter(.data$historical_lmic_reporting_scope %in% TRUE) |>
  dplyr::count(
    .data$peer_method_id, .data$peer_pool_rule, name = "country_years"
  ) |>
  dplyr::group_by(.data$peer_method_id) |>
  dplyr::mutate(share = .data$country_years / sum(.data$country_years)) |>
  dplyr::ungroup() |>
  dplyr::mutate(build_id = build_id, schema_version = schema_version)

# ---------------------------------------------------------------------------
# 3. IDS versus the project's own country-year primary aggregate.
# ---------------------------------------------------------------------------

ids_primary_usd <- ids_detail |>
  dplyr::filter(
    .data$anchor_branch == "observed_primary",
    .data$anchor_currency == "USD",
    .data$historical_lmic_reporting_scope %in% TRUE,
    is.finite(.data$anchor_rate_pct), is.finite(.data$fallback_rate_pct)
  )

ids_primary_summary <- dplyr::bind_rows(
  ids_primary_usd |>
    dplyr::mutate(sample_basis = "all_lmic_usd_primary_overlap"),
  ids_primary_usd |>
    dplyr::filter(!.data$anchor_thin_evidence) |>
    dplyr::mutate(sample_basis = "nonthin_lmic_usd_primary_overlap")
) |>
  dplyr::group_by(.data$sample_basis) |>
  dplyr::summarise(
    matched_country_years = dplyr::n(),
    countries = dplyr::n_distinct(.data$iso3),
    years = dplyr::n_distinct(.data$analysis_year),
    pearson_correlation = stats::cor(
      .data$fallback_rate_pct, .data$anchor_rate_pct
    ),
    spearman_correlation = stats::cor(
      .data$fallback_rate_pct, .data$anchor_rate_pct, method = "spearman"
    ),
    mean_ids_minus_primary_pp = mean(.data$signed_gap_pp),
    median_ids_minus_primary_pp = stats::median(.data$signed_gap_pp),
    mean_abs_gap_pp = mean(.data$abs_gap_pp),
    median_abs_gap_pp = stats::median(.data$abs_gap_pp),
    rmse_gap_pp = sqrt(mean(.data$signed_gap_pp^2)),
    within_0_5pp_share = mean(.data$abs_gap_pp <= 0.5),
    within_1pp_share = mean(.data$abs_gap_pp <= 1),
    within_2pp_share = mean(.data$abs_gap_pp <= 2),
    .groups = "drop"
  ) |>
  dplyr::mutate(
    interpretation = paste(
      "Both are amount-weighted country-year aggregates, but IDS is a public",
      "all-external-currency contractual interest-rate aggregate and P15 is a",
      "security-level USD issue-yield aggregate."
    ),
    decision_state = "supports_labelled_ladder_comparability_not_identity",
    build_id = build_id, schema_version = schema_version
  )

ids_primary_yearly <- ids_primary_usd |>
  dplyr::group_by(.data$analysis_year) |>
  dplyr::summarise(
    matched_country_years = dplyr::n(),
    mean_ids_minus_primary_pp = mean(.data$signed_gap_pp),
    mean_abs_gap_pp = mean(.data$abs_gap_pp),
    median_abs_gap_pp = stats::median(.data$abs_gap_pp),
    within_1pp_share = mean(.data$abs_gap_pp <= 1),
    .groups = "drop"
  ) |>
  dplyr::mutate(build_id = build_id, schema_version = schema_version)

# ---------------------------------------------------------------------------
# 4. Persist outputs and manifest.
# ---------------------------------------------------------------------------

output_paths <- c(
  write_csv(rating_coverage, "p15_rating_agency_bounded_coverage.csv"),
  write_csv(rating_summary, "p15_rating_agency_bounded_comparison_summary.csv"),
  write_csv(rating_pairwise, "p15_rating_agency_bounded_pairwise_inference.csv"),
  write_csv(rating_yearly, "p15_rating_agency_bounded_yearly.csv"),
  write_csv(rating_country, "p15_rating_agency_bounded_country_stability.csv"),
  write_csv(rating_agency_composition, "p15_rating_agency_composition.csv"),
  write_csv(peer_detail, "p15_peer_bounded_comparison_detail.csv.gz", TRUE),
  write_csv(peer_members, "p15_peer_bounded_membership.csv.gz", TRUE),
  write_csv(peer_summary, "p15_peer_bounded_comparison_summary.csv"),
  write_csv(peer_pairwise, "p15_peer_bounded_pairwise_inference.csv"),
  write_csv(peer_yearly, "p15_peer_bounded_yearly.csv"),
  write_csv(peer_pool_rules, "p15_peer_bounded_pool_rules.csv"),
  write_csv(ids_primary_summary, "p15_ids_primary_comparability_summary.csv"),
  write_csv(ids_primary_yearly, "p15_ids_primary_comparability_yearly.csv")
)

code_rel <- c(
  "R/p15_bounded_fallback_comparison.R",
  "scripts/p15/build_p15_bounded_fallback_comparison.R",
  "tests/testthat/test-p15-bounded-fallback-comparison.R"
)
manifest_paths <- c(unname(input_rel), substring(output_paths, nchar(root) + 2L), code_rel)
manifest <- tibble::tibble(
  artifact_role = c(
    rep("input", length(input_rel)), rep("output", length(output_paths)),
    rep("code", length(code_rel))
  ),
  path = manifest_paths,
  sha256 = vapply(
    file.path(root, manifest_paths), digest::digest, character(1),
    algo = "sha256", file = TRUE
  ),
  build_id = build_id,
  schema_version = schema_version
)
manifest_path <- file.path(
  governance_dir, "p15_bounded_fallback_comparison_manifest.csv"
)
data.table::fwrite(data.table::as.data.table(manifest), manifest_path, na = "")

stopifnot(
  nrow(rating_pairwise) == 3L,
  nrow(peer_summary) == 6L,
  nrow(peer_pairwise) == 2L,
  nrow(ids_primary_summary) == 2L,
  all(peer_detail$target_excluded),
  all(peer_members$target_excluded),
  all(rating_summary$decision_state ==
        "bounded_comparison_no_automatic_promotion"),
  all(peer_summary$decision_state ==
        "bounded_comparison_no_automatic_promotion")
)

message(
  "P15 bounded fallback comparison: PASS (",
  nrow(rating_sample), " rating comparisons; ",
  nrow(peer_detail), " peer comparisons; ",
  nrow(ids_primary_usd), " IDS-primary overlaps)."
)
