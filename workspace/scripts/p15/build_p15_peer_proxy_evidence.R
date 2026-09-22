#!/usr/bin/env Rscript

# Build the neutral P15 peer-proxy evidence package. The package reproduces the
# P13 2024 peer tier, audits predecessor pools, and calculates controlled P15
# candidates with relational membership provenance. It does not approve a peer
# rule or give peer estimates a selected/headline role.

suppressPackageStartupMessages({
  library(data.table)
  library(digest)
  library(dplyr)
  library(readr)
  library(tibble)
  library(tidyr)
})

source("R/p15_peer_proxy.R")

root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
derived_dir <- file.path(root, "data-derived", "p15_peer_proxy_2012_2024_v1")
governance_dir <- file.path(root, "docs", "governance")
dir.create(derived_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(governance_dir, recursive = TRUE, showWarnings = FALSE)

input_rel <- c(
  grid = "data-derived/p15_platform_2012_2024_v1/p15_country_year_grid_2012_2024.csv",
  status = "data-derived/p15_status_context_2012_2024_v1/p15_status_context_evidence_2012_2024.csv.gz",
  ids = "data-derived/p15_ids_evidence_2012_2024_v1/p15_ids_bondholders_country_year_evidence_2012_2024.csv.gz",
  historical_primary = "experiments/p14_historical_validation_and_extension_2026-06-22/outputs/p14_historical_quality_equalization_2012_2023/p14_historical_primary_country_year_rates_2012_2023.csv",
  historical_secondary = "experiments/p14_historical_validation_and_extension_2026-06-22/outputs/p14_historical_quality_equalization_2012_2023/p14_historical_secondary_country_year_rates_2012_2023.csv",
  historical_peer = "experiments/p14_historical_validation_and_extension_2026-06-22/outputs/p14_historical_quality_equalization_2012_2023/p14_historical_peer_proxy_pool_register_2012_2023.csv",
  primary_2024 = "data-derived/p15_observed_market_2024_v1/p15_p13_primary_country_evidence_2024.csv.gz",
  secondary_2024 = "data-derived/p15_observed_market_2024_v1/p15_p13_secondary_selected_parity_2024.csv",
  p13_status = "experiments/full_ladder_ratings_integrated_status_rebuild_2026-06-01/outputs/diagnostic_country_year_status_all_years.csv",
  p13_strict = "output/tables/market_benchmarks_2024.csv",
  p13_peer = "experiments/full_ladder_ratings_integrated_status_rebuild_2026-06-01/outputs/p11b_peer_proxy_revised_2024.csv"
)
input_paths <- stats::setNames(file.path(root, unname(input_rel)), names(input_rel))
stopifnot(all(file.exists(input_paths)))

read_input <- function(name) {
  readr::read_csv(
    input_paths[[name]], show_col_types = FALSE, guess_max = 100000
  )
}

grid <- read_input("grid")
status <- read_input("status")
ids <- read_input("ids")
historical_primary <- read_input("historical_primary")
historical_secondary <- read_input("historical_secondary")
historical_peer <- read_input("historical_peer")
primary_2024 <- read_input("primary_2024")
secondary_2024 <- read_input("secondary_2024")
p13_status <- read_input("p13_status")
p13_strict <- read_input("p13_strict")
p13_peer <- read_input("p13_peer")

status_flags <- status |>
  dplyr::transmute(
    analysis_year = as.integer(.data$analysis_year),
    iso3 = as.character(.data$iso3),
    positive_status_context = dplyr::coalesce(
      as.logical(.data$positive_status_evidence_present), FALSE
    ),
    status_consequence_state = as.character(.data$status_consequence_state),
    status_evidence_ids = as.character(.data$status_evidence_ids)
  )
stopifnot(!anyDuplicated(status_flags[c("analysis_year", "iso3")]))

peer_grid <- grid |>
  dplyr::left_join(status_flags, by = c("analysis_year", "iso3")) |>
  dplyr::mutate(
    positive_status_context = dplyr::coalesce(
      .data$positive_status_context, FALSE
    )
  )
stopifnot(nrow(peer_grid) == 2743L)

# Seed ledger. Historical observed rows remain labelled as predecessor-derived
# until the all-years P15 observed processors replace them. No rating, peer, or
# other modelled output is admitted as a seed.
primary_historical_seeds <- historical_primary |>
  dplyr::filter(
    .data$analysis_year >= 2012L, .data$analysis_year <= 2023L,
    .data$primary_rate_sanity_state == "sane_1_30pct",
    .data$admissibility_preview ==
      "potentially_permissible_after_status_review",
    is.finite(.data$primary_rate_pct)
  ) |>
  dplyr::transmute(
    analysis_year = as.integer(.data$analysis_year),
    iso3 = as.character(.data$iso3),
    country = as.character(.data$country),
    historical_income_level = as.character(.data$income_level),
    seed_pool_class = dplyr::if_else(
      .data$primary_detailed_subtype == "primary_standard",
      "strict_observed", "broad_observed"
    ),
    seed_priority = dplyr::if_else(
      .data$primary_detailed_subtype == "primary_standard", 10L, 15L
    ),
    seed_rate_pct = as.numeric(.data$primary_rate_pct),
    seed_maturity_years = as.numeric(.data$primary_maturity_years),
    seed_source_family = "observed_primary",
    seed_source_tier = paste0(
      "p14_predecessor_", .data$primary_detailed_subtype
    ),
    seed_source_package_id = "SRC-P14-HQE-DERIVED-20260630",
    seed_record_locator = paste(
      "p14_historical_primary_country_year_rates_2012_2023.csv",
      .data$analysis_year, .data$iso3, sep = "::"
    ),
    seed_is_model_or_proxy = FALSE,
    seed_dependency_state =
      "predecessor_country_year_aggregate_pending_common_p15_processor"
  )

secondary_historical_seeds <- historical_secondary |>
  dplyr::filter(
    .data$analysis_year >= 2012L, .data$analysis_year <= 2023L,
    !.data$diagnostic_only,
    .data$rate_sanity_state == "sane_1_30pct",
    .data$detailed_ladder_tier_id %in% c(
      "secondary_standard_usd_2_15_direct",
      "secondary_broader_usd_eur_2_15_direct"
    ),
    is.finite(.data$market_rate_pct)
  ) |>
  dplyr::transmute(
    analysis_year = as.integer(.data$analysis_year),
    iso3 = as.character(.data$iso3),
    country = as.character(.data$country),
    historical_income_level = as.character(.data$income_level),
    seed_pool_class = dplyr::if_else(
      .data$detailed_ladder_tier_id ==
        "secondary_standard_usd_2_15_direct",
      "strict_observed", "broad_observed"
    ),
    seed_priority = dplyr::if_else(
      .data$detailed_ladder_tier_id ==
        "secondary_standard_usd_2_15_direct", 20L, 30L
    ),
    seed_rate_pct = as.numeric(.data$market_rate_pct),
    seed_maturity_years = as.numeric(.data$market_maturity_years),
    seed_source_family = "observed_secondary",
    seed_source_tier = paste0(
      "p14_predecessor_", .data$detailed_ladder_tier_id
    ),
    seed_source_package_id = "SRC-P14-HQE-DERIVED-20260630",
    seed_record_locator = paste(
      "p14_historical_secondary_country_year_rates_2012_2023.csv",
      .data$analysis_year, .data$iso3, .data$detailed_ladder_tier_id,
      sep = "::"
    ),
    seed_is_model_or_proxy = FALSE,
    seed_dependency_state =
      "predecessor_country_year_aggregate_pending_common_p15_processor"
  )

primary_2024_seeds <- primary_2024 |>
  dplyr::filter(
    .data$selected_pvr_admissible %in% TRUE,
    is.finite(.data$market_rate_pct)
  ) |>
  dplyr::transmute(
    analysis_year = 2024L,
    iso3 = as.character(.data$iso3),
    country = as.character(.data$country),
    historical_income_level = as.character(.data$income_level),
    seed_pool_class = "strict_observed",
    seed_priority = 10L,
    seed_rate_pct = as.numeric(.data$market_rate_pct),
    seed_maturity_years = as.numeric(.data$market_maturity_years),
    seed_source_family = "observed_primary",
    seed_source_tier = "p15_p13_parity_observed_primary_selected",
    seed_source_package_id = as.character(.data$source_package_id),
    seed_record_locator = paste(
      "p15_p13_primary_country_evidence_2024.csv.gz", .data$iso3,
      sep = "::"
    ),
    seed_is_model_or_proxy = FALSE,
    seed_dependency_state = "current_p15_issue_traceable_processor"
  )

secondary_2024_seeds <- secondary_2024 |>
  dplyr::filter(is.finite(.data$market_rate_pct)) |>
  dplyr::transmute(
    analysis_year = 2024L,
    iso3 = as.character(.data$iso3),
    country = as.character(.data$country_p15),
    historical_income_level = NA_character_,
    seed_pool_class = dplyr::if_else(
      grepl("direct_yield|direct_ytm", .data$selected_source_class),
      "strict_observed", "broad_observed"
    ),
    seed_priority = dplyr::if_else(
      grepl("direct_yield|direct_ytm", .data$selected_source_class), 20L, 30L
    ),
    seed_rate_pct = as.numeric(.data$market_rate_pct),
    seed_maturity_years = as.numeric(.data$market_maturity_years),
    seed_source_family = "observed_secondary",
    seed_source_tier = paste0(
      "p15_p13_parity_", .data$selected_source_class
    ),
    seed_source_package_id = as.character(.data$source_package_id),
    seed_record_locator = paste(
      "p15_p13_secondary_selected_parity_2024.csv", .data$iso3,
      sep = "::"
    ),
    seed_is_model_or_proxy = FALSE,
    seed_dependency_state = "current_p15_issue_traceable_processor"
  )

ids_seeds <- ids |>
  dplyr::filter(
    .data$bondholder_term_validity_state ==
      "bondholder_terms_usable_subject_to_source_checks",
    is.finite(.data$official_rate_pct)
  ) |>
  dplyr::transmute(
    analysis_year = as.integer(.data$analysis_year),
    iso3 = as.character(.data$iso3),
    country = as.character(.data$country),
    historical_income_level = as.character(.data$historical_income_level),
    seed_pool_class = "ids_contractual_proxy",
    seed_priority = 50L,
    seed_rate_pct = as.numeric(.data$official_rate_pct),
    seed_maturity_years = as.numeric(.data$official_maturity_years),
    seed_source_family = "ids_bondholder_contractual_proxy",
    seed_source_tier = "p15_ids_bondholder_contractual_proxy",
    seed_source_package_id = as.character(.data$ids_source_package_id),
    seed_record_locator = as.character(.data$ids_evidence_id),
    seed_is_model_or_proxy = FALSE,
    seed_dependency_state = "current_p15_public_contractual_proxy_component"
  )

seed_ledger <- dplyr::bind_rows(
  primary_historical_seeds,
  secondary_historical_seeds,
  primary_2024_seeds,
  secondary_2024_seeds,
  ids_seeds
) |>
  dplyr::select(-dplyr::any_of("positive_status_context")) |>
  dplyr::left_join(
    peer_grid |>
      dplyr::select(
        "analysis_year", "iso3",
        grid_country = "country",
        grid_income = "historical_income_level",
        "positive_status_context"
      ),
    by = c("analysis_year", "iso3")
  ) |>
  dplyr::mutate(
    country = dplyr::coalesce(.data$grid_country, .data$country),
    historical_income_level = dplyr::coalesce(
      .data$grid_income, .data$historical_income_level
    ),
    positive_status_context = dplyr::coalesce(
      .data$positive_status_context, FALSE
    )
  ) |>
  dplyr::select(-"grid_country", -"grid_income") |>
  dplyr::arrange(
    .data$analysis_year, .data$iso3, .data$seed_priority,
    .data$seed_source_family, .data$seed_source_tier
  )
p15_peer_validate_seed_ledger(seed_ledger)

seed_path <- file.path(
  derived_dir, "p15_peer_seed_ledger_2012_2024.csv.gz"
)
data.table::fwrite(data.table::as.data.table(seed_ledger), seed_path, na = "")

variant_register <- tibble::tribble(
  ~peer_variant_id, ~seed_rule, ~minimum_same_income_count, ~exclude_positive_status_context, ~calculation_state, ~research_role, ~dependency_note,
  "PEER-OBS-STRICT-INC5-ALLSTATUS-V1", "strict_observed", 5L, FALSE, "computed_candidate", "controlled_diagnostic", "Uses strict observed seeds; status evidence is retained but does not remove peers.",
  "PEER-OBS-STRICT-INC5-STATUSEXCL-V1", "strict_observed", 5L, TRUE, "computed_candidate", "controlled_diagnostic", "Sensitivity that excludes peers with positive status-context evidence; no status consequence is approved.",
  "PEER-OBS-BROAD-INC5-ALLSTATUS-V1", "broad_observed", 5L, FALSE, "computed_candidate", "controlled_diagnostic", "Adds warning/thin primary and broader direct-secondary seeds.",
  "PEER-OBS-BROAD-INC5-STATUSEXCL-V1", "broad_observed", 5L, TRUE, "computed_candidate", "controlled_diagnostic", "Broad observed-seed sensitivity with status-context peer exclusion.",
  "PEER-OBS-STRICT-IDS-INC5-STATUSEXCL-V1", "strict_observed_plus_ids", 5L, TRUE, "computed_candidate", "controlled_diagnostic", "Adds IDS Bondholders contractual proxies only where stronger strict observed seeds are absent.",
  "PEER-OBS-STRICT-INC3-STATUSEXCL-V1", "strict_observed", 3L, TRUE, "computed_candidate", "controlled_diagnostic", "Tests the coverage/dispersion effect of a three-peer same-income threshold.",
  "PEER-SAME-REGION", "not_applicable", NA_integer_, NA, "not_evaluated", "future_candidate", "Requires an authoritative historical region classification and a bounded hierarchy.",
  "PEER-SAME-RATING", "not_applicable", NA_integer_, NA, "not_evaluated", "future_candidate", "Depends on the final rating-object decision and must remain nonrecursive.",
  "PEER-SAME-MARKET-ACCESS", "not_applicable", NA_integer_, NA, "not_evaluated", "future_candidate", "Depends on approved market-access/status consequences.",
  "PEER-NEARBY-YEAR", "not_applicable", NA_integer_, NA, "deferred_extension", "future_spatiotemporal_experiment", "Deferred under EXT-04 until same-year peer governance is settled.",
  "PEER-STATIC-LENDING", "not_applicable", NA_integer_, NA, "prohibited_current_input", "audit_only", "Static 2024 lending labels cannot determine historical peer membership."
) |>
  dplyr::mutate(
    peer_admissibility_state = "not_evaluated",
    peer_selection_state = "not_evaluated",
    peer_headline_state = "not_evaluated",
    peer_evidence_schema_version = p15_peer_proxy_schema_version()
  )
variant_register_path <- file.path(
  governance_dir, "p15_peer_method_register.csv"
)
data.table::fwrite(
  data.table::as.data.table(variant_register), variant_register_path, na = ""
)

peer_build <- p15_build_peer_candidates(
  peer_grid, seed_ledger, variant_register
)
candidate_path <- file.path(
  derived_dir, "p15_peer_candidate_variants_2012_2024.csv.gz"
)
member_path <- file.path(
  derived_dir, "p15_peer_membership_ledger_2012_2024.csv.gz"
)
data.table::fwrite(
  data.table::as.data.table(peer_build$candidates), candidate_path, na = ""
)
data.table::fwrite(
  data.table::as.data.table(peer_build$members), member_path, na = ""
)

# Exact P13 legacy gate. This is a reference reconstruction, not the P15 rule.
p13_targets_2024 <- p13_status |>
  dplyr::filter(.data$analysis_year == 2024L) |>
  dplyr::mutate(
    included_in_lmic_reporting_scope = as.logical(
      .data$included_in_lmic_reporting_scope
    ),
    has_status_gate = as.logical(.data$has_status_gate)
  ) |>
  dplyr::select(
    "analysis_year", "iso3", "country", "income_level", "lending_type",
    "included_in_lmic_reporting_scope", "has_status_gate",
    "status_or_evidence_state"
  )
p13_legacy <- p15_build_p13_legacy_peer(p13_targets_2024, p13_strict)
p13_expected <- p13_peer |>
  dplyr::select(
    "iso3",
    expected_reliability_label = "reliability_label",
    expected_peer_pool_rule = "peer_pool_rule",
    expected_peer_country_count = "peer_country_count",
    expected_peer_country_list = "peer_country_list",
    expected_peer_rate_median_pct = "peer_rate_median_pct",
    expected_peer_rate_mean_pct = "peer_rate_mean_pct",
    expected_peer_rate_iqr_low_pct = "peer_rate_iqr_low_pct",
    expected_peer_rate_iqr_high_pct = "peer_rate_iqr_high_pct",
    expected_peer_rate_min_pct = "peer_rate_min_pct",
    expected_peer_rate_max_pct = "peer_rate_max_pct",
    expected_peer_rate_iqr_width_pct = "peer_rate_iqr_width_pct",
    expected_peer_maturity_median_years = "peer_maturity_median_years"
  )
p13_crosswalk <- p13_legacy$candidates |>
  dplyr::left_join(p13_expected, by = "iso3") |>
  dplyr::mutate(
    rate_median_abs_diff_pp = abs(
      .data$peer_rate_median_pct - .data$expected_peer_rate_median_pct
    ),
    rate_mean_abs_diff_pp = abs(
      .data$peer_rate_mean_pct - .data$expected_peer_rate_mean_pct
    ),
    maturity_abs_diff_years = abs(
      .data$peer_maturity_median_years -
        .data$expected_peer_maturity_median_years
    ),
    numeric_parity_pass = dplyr::if_all(
      dplyr::all_of(c(
        "rate_median_abs_diff_pp", "rate_mean_abs_diff_pp",
        "maturity_abs_diff_years"
      )),
      ~ .x <= 1e-12
    ) &
      abs(.data$peer_rate_iqr_low_pct -
            .data$expected_peer_rate_iqr_low_pct) <= 1e-12 &
      abs(.data$peer_rate_iqr_high_pct -
            .data$expected_peer_rate_iqr_high_pct) <= 1e-12 &
      abs(.data$peer_rate_min_pct -
            .data$expected_peer_rate_min_pct) <= 1e-12 &
      abs(.data$peer_rate_max_pct -
            .data$expected_peer_rate_max_pct) <= 1e-12 &
      abs(.data$peer_rate_iqr_width_pct -
            .data$expected_peer_rate_iqr_width_pct) <= 1e-12,
    structural_parity_pass =
      .data$reliability_label == .data$expected_reliability_label &
      .data$peer_pool_rule == .data$expected_peer_pool_rule &
      .data$peer_country_count == .data$expected_peer_country_count &
      .data$peer_country_list == .data$expected_peer_country_list,
    p13_peer_exact_parity_pass =
      .data$numeric_parity_pass & .data$structural_parity_pass
  )
stopifnot(
  nrow(p13_crosswalk) == 211L,
  all(p13_crosswalk$p13_peer_exact_parity_pass),
  nrow(p13_legacy$members) == sum(p13_crosswalk$peer_country_count)
)
p13_crosswalk_path <- file.path(
  governance_dir, "p15_peer_p13_exact_parity_2024.csv"
)
p13_member_path <- file.path(
  governance_dir, "p15_peer_p13_membership_reconstruction_2024.csv.gz"
)
data.table::fwrite(
  data.table::as.data.table(p13_crosswalk), p13_crosswalk_path, na = ""
)
data.table::fwrite(
  data.table::as.data.table(p13_legacy$members), p13_member_path, na = ""
)

# Audit P14 predecessor pools at the relational member level.
predecessor_members <- historical_peer |>
  dplyr::select(
    target_analysis_year = "analysis_year",
    target_iso3 = "iso3",
    target_country = "country",
    "peer_pool_rule", "peer_pool_iso3", "peer_pool_source_mix"
  ) |>
  tidyr::separate_rows("peer_pool_iso3", sep = ";") |>
  dplyr::rename(peer_iso3 = "peer_pool_iso3") |>
  dplyr::filter(!is.na(.data$peer_iso3), .data$peer_iso3 != "") |>
  dplyr::left_join(
    status_flags |>
      dplyr::select(
        target_analysis_year = "analysis_year",
        peer_iso3 = "iso3",
        peer_positive_status_context = "positive_status_context",
        peer_status_evidence_ids = "status_evidence_ids"
      ),
    by = c("target_analysis_year", "peer_iso3")
  ) |>
  dplyr::mutate(
    target_excluded = .data$target_iso3 != .data$peer_iso3,
    peer_positive_status_context = dplyr::coalesce(
      .data$peer_positive_status_context, FALSE
    ),
    static_lending_type_rule = grepl(
      "static_lending", .data$peer_pool_rule, ignore.case = TRUE
    ),
    ids_proxy_in_source_mix = grepl(
      "ids_bondholder", .data$peer_pool_source_mix, ignore.case = TRUE
    ),
    member_rate_and_source_relationally_preserved = FALSE,
    predecessor_member_audit_state =
      "member_identity_preserved_rate_source_mapping_not_preserved"
  )

predecessor_audit <- historical_peer |>
  dplyr::select(
    "analysis_year", "period", "iso3", "country", "income_level",
    "lending_type", "peer_proxy_rate_pct", "peer_count",
    "peer_pool_rule", "peer_pool_iso3", "peer_pool_source_mix",
    "peer_iqr_pp", "target_country_excluded",
    "peer_pool_acceptability_state"
  ) |>
  dplyr::left_join(
    predecessor_members |>
      dplyr::group_by(
        analysis_year = .data$target_analysis_year,
        iso3 = .data$target_iso3
      ) |>
      dplyr::summarise(
        reconstructed_member_count = dplyr::n(),
        status_context_peer_count = sum(
          .data$peer_positive_status_context, na.rm = TRUE
        ),
        all_members_target_excluding = all(.data$target_excluded),
        static_lending_type_rule = any(.data$static_lending_type_rule),
        ids_proxy_in_source_mix = any(.data$ids_proxy_in_source_mix),
        .groups = "drop"
      ),
    by = c("analysis_year", "iso3")
  ) |>
  dplyr::mutate(
    member_count_reconciles = .data$peer_count == .data$reconstructed_member_count,
    predecessor_method_version_state = dplyr::if_else(
      .data$analysis_year <= 2021L,
      "partial_resume_predecessor_2012_2021",
      "p14_main_predecessor_2022_2023"
    ),
    contamination_flags = purrr::pmap_chr(
      list(
        .data$static_lending_type_rule,
        .data$status_context_peer_count > 0,
        .data$ids_proxy_in_source_mix,
        !.data$member_count_reconciles,
        !.data$all_members_target_excluding
      ),
      function(static_lending, status_peer, ids_mix, count_gap, target_included) {
        flags <- c(
          if (isTRUE(static_lending)) "static_lending_type_pool" else NULL,
          if (isTRUE(status_peer)) "positive_status_context_peer" else NULL,
          if (isTRUE(ids_mix)) "ids_contractual_proxy_seed" else NULL,
          if (isTRUE(count_gap)) "member_count_nonreconciliation" else NULL,
          if (isTRUE(target_included)) "target_country_included" else NULL,
          "member_rate_source_mapping_not_relationally_preserved"
        )
        paste(flags, collapse = ";")
      }
    ),
    p15_predecessor_governance_state =
      "historical_reference_not_directly_promotable_to_p15_candidate"
  )

predecessor_audit_path <- file.path(
  governance_dir, "p15_peer_predecessor_pool_audit_2012_2023.csv.gz"
)
predecessor_member_path <- file.path(
  governance_dir, "p15_peer_predecessor_member_audit_2012_2023.csv.gz"
)
data.table::fwrite(
  data.table::as.data.table(predecessor_audit),
  predecessor_audit_path, na = ""
)
data.table::fwrite(
  data.table::as.data.table(predecessor_members),
  predecessor_member_path, na = ""
)

# Descriptive validation only. Authority remains pending VAL-02.
anchor_ledger <- seed_ledger |>
  dplyr::filter(
    .data$seed_pool_class == "strict_observed",
    .data$seed_source_family %in% c(
      "observed_primary", "observed_secondary"
    )
  ) |>
  dplyr::transmute(
    .data$analysis_year, .data$iso3,
    anchor_family = .data$seed_source_family,
    anchor_rate_pct = .data$seed_rate_pct,
    anchor_maturity_years = .data$seed_maturity_years,
    anchor_quality_state = dplyr::if_else(
      grepl("predecessor", .data$seed_dependency_state),
      "strict_predecessor_observed_anchor_pending_common_p15_rebuild",
      "current_p15_issue_traceable_observed_anchor"
    ),
    anchor_source_package_id = .data$seed_source_package_id,
    anchor_record_locator = .data$seed_record_locator
  )
stopifnot(!anyDuplicated(anchor_ledger[c(
  "analysis_year", "iso3", "anchor_family"
)]))
validation <- p15_build_peer_validation(
  peer_build$candidates, anchor_ledger
)
validation_path <- file.path(
  derived_dir, "p15_peer_anchor_validation_detail_2012_2024.csv.gz"
)
data.table::fwrite(
  data.table::as.data.table(validation), validation_path, na = ""
)

validation_summary <- validation |>
  dplyr::group_by(.data$peer_variant_id, .data$anchor_family) |>
  dplyr::summarise(
    validation_rows = dplyr::n(),
    countries = dplyr::n_distinct(.data$iso3),
    years = dplyr::n_distinct(.data$analysis_year),
    mean_signed_gap_pp = mean(.data$signed_gap_pp, na.rm = TRUE),
    median_signed_gap_pp = stats::median(.data$signed_gap_pp, na.rm = TRUE),
    mean_abs_gap_pp = mean(.data$abs_gap_pp, na.rm = TRUE),
    median_abs_gap_pp = stats::median(.data$abs_gap_pp, na.rm = TRUE),
    p90_abs_gap_pp = p15_peer_safe_quantile(.data$abs_gap_pp, 0.90),
    max_abs_gap_pp = max(.data$abs_gap_pp, na.rm = TRUE),
    all_validation_integrity_pass = all(.data$validation_integrity_pass),
    validation_authority_state = dplyr::first(
      .data$validation_authority_state
    ),
    .groups = "drop"
  )
validation_summary_path <- file.path(
  governance_dir, "p15_peer_anchor_validation_summary.csv"
)
data.table::fwrite(
  data.table::as.data.table(validation_summary),
  validation_summary_path, na = ""
)

candidate_summary <- peer_build$candidates |>
  dplyr::group_by(.data$peer_variant_id) |>
  dplyr::summarise(
    country_year_rows = dplyr::n(),
    computed_rows = sum(.data$candidate_computed),
    lmic_rows = sum(.data$historical_lmic_reporting_scope, na.rm = TRUE),
    median_peer_count = stats::median(.data$peer_country_count),
    minimum_peer_count = min(.data$peer_country_count),
    maximum_peer_count = max(.data$peer_country_count),
    mean_peer_iqr_width_pp = mean(
      .data$peer_rate_iqr_width_pct, na.rm = TRUE
    ),
    global_fallback_rows = sum(grepl(
      "global_same_year_fallback", .data$peer_pool_rule
    )),
    rows_with_positive_status_context_peer = sum(
      .data$peer_positive_status_context_count > 0
    ),
    rows_with_predecessor_seed_dependency = sum(
      .data$peer_predecessor_dependency_count > 0
    ),
    all_target_excluded = all(.data$target_excluded),
    all_nonrecursive = all(.data$nonrecursive_seed_pool),
    all_relational_provenance_complete = all(
      .data$relational_provenance_complete
    ),
    candidate_decision_state = "not_evaluated",
    .groups = "drop"
  )
candidate_summary_path <- file.path(
  governance_dir, "p15_peer_candidate_summary.csv"
)
data.table::fwrite(
  data.table::as.data.table(candidate_summary), candidate_summary_path, na = ""
)

predecessor_summary <- predecessor_audit |>
  dplyr::summarise(
    predecessor_country_year_rows = dplyr::n(),
    static_lending_type_pool_rows = sum(.data$static_lending_type_rule),
    rows_with_positive_status_context_peer = sum(
      .data$status_context_peer_count > 0
    ),
    rows_with_ids_proxy_source_mix = sum(.data$ids_proxy_in_source_mix),
    member_count_nonreconciliation_rows = sum(!.data$member_count_reconciles),
    target_inclusion_rows = sum(!.data$all_members_target_excluding),
    rows_without_relational_member_rate_source_mapping = dplyr::n(),
    direct_p15_promotion_state =
      "blocked_use_rebuilt_relational_candidates_instead"
  )
predecessor_summary_path <- file.path(
  governance_dir, "p15_peer_predecessor_audit_summary.csv"
)
data.table::fwrite(
  data.table::as.data.table(predecessor_summary),
  predecessor_summary_path, na = ""
)

build_summary <- tibble::tibble(
  metric = c(
    "seed_ledger_rows", "computed_variant_count", "candidate_rows",
    "membership_rows", "p13_exact_parity_rows",
    "p13_exact_parity_failures", "predecessor_pool_rows",
    "predecessor_member_rows", "validation_detail_rows"
  ),
  value = c(
    nrow(seed_ledger),
    sum(variant_register$calculation_state == "computed_candidate"),
    nrow(peer_build$candidates), nrow(peer_build$members),
    nrow(p13_crosswalk), sum(!p13_crosswalk$p13_peer_exact_parity_pass),
    nrow(predecessor_audit), nrow(predecessor_members), nrow(validation)
  )
)
build_summary_path <- file.path(
  governance_dir, "p15_peer_build_summary.csv"
)
data.table::fwrite(
  data.table::as.data.table(build_summary), build_summary_path, na = ""
)

output_paths <- c(
  seed_path, candidate_path, member_path, validation_path,
  variant_register_path, p13_crosswalk_path, p13_member_path,
  predecessor_audit_path, predecessor_member_path,
  validation_summary_path, candidate_summary_path,
  predecessor_summary_path, build_summary_path
)
manifest <- dplyr::bind_rows(
  tibble::tibble(
    artifact_role = "input",
    path = unname(input_rel),
    sha256 = vapply(input_paths, digest::digest, character(1),
                    algo = "sha256", file = TRUE)
  ),
  tibble::tibble(
    artifact_role = "output",
    path = sub(paste0("^", root, "/"), "", output_paths),
    sha256 = vapply(output_paths, digest::digest, character(1),
                    algo = "sha256", file = TRUE)
  ),
  tibble::tibble(
    artifact_role = "code",
    path = c(
      "R/p15_peer_proxy.R",
      "scripts/p15/build_p15_peer_proxy_evidence.R",
      "tests/testthat/test-p15-peer-proxy.R"
    ),
    sha256 = vapply(
      file.path(root, c(
        "R/p15_peer_proxy.R",
        "scripts/p15/build_p15_peer_proxy_evidence.R",
        "tests/testthat/test-p15-peer-proxy.R"
      )),
      digest::digest, character(1), algo = "sha256", file = TRUE
    )
  )
) |>
  dplyr::mutate(
    build_id = "BUILD-P15-PEER-PROXY-20260721-V1",
    schema_version = p15_peer_proxy_schema_version()
  )
manifest_path <- file.path(
  governance_dir, "p15_peer_evidence_manifest.csv"
)
data.table::fwrite(
  data.table::as.data.table(manifest), manifest_path, na = ""
)

stopifnot(
  all(peer_build$candidates$target_excluded),
  all(peer_build$candidates$nonrecursive_seed_pool),
  all(peer_build$members$target_excluded),
  !any(peer_build$members$recursive_seed_flag),
  all(validation$validation_integrity_pass),
  all(variant_register$peer_admissibility_state == "not_evaluated"),
  all(variant_register$peer_selection_state == "not_evaluated"),
  all(variant_register$peer_headline_state == "not_evaluated")
)

message(
  "Built P15 peer evidence: ", nrow(seed_ledger), " seeds; ",
  nrow(peer_build$candidates), " candidate rows; ",
  nrow(peer_build$members), " relational memberships; 211/211 P13 parity."
)
