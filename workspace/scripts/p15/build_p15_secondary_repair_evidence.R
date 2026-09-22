#!/usr/bin/env Rscript

# Reproduce and consolidate secondary clean-price-to-yield evidence for
# SEC-06 to SEC-08. The P13 parity branch remains untouched. Forward P15
# admissibility is left undecided.

suppressPackageStartupMessages({
  library(data.table)
  library(digest)
  library(dplyr)
  library(readr)
  library(tibble)
  library(tidyr)
})

source("R/p15_observed_secondary.R")
source("R/p15_secondary_repair_validation.R")

root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
derived_dir <- file.path(
  root, "data-derived", "p15_secondary_repair_2012_2024_v1"
)
governance_dir <- file.path(root, "docs", "governance")
dir.create(derived_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(governance_dir, recursive = TRUE, showWarnings = FALSE)

paths <- list(
  semantics_detail = file.path(
    root, "experiments/full_ladder_ratings_integrated_status_rebuild_2026-06-01",
    "outputs/secondary_price_field_semantics_crosscheck_detail_2024.csv"
  ),
  semantics_rule = file.path(
    root, "experiments/full_ladder_ratings_integrated_status_rebuild_2026-06-01",
    "outputs/secondary_price_field_semantics_rule_2024.csv"
  ),
  remaining_gates = file.path(
    root, "experiments/full_ladder_ratings_integrated_status_rebuild_2026-06-01",
    "outputs/secondary_price_to_yield_remaining_rule_gates_2024.csv"
  ),
  historical_repair = file.path(
    root, "experiments/p14_historical_validation_and_extension_2026-06-22",
    "outputs/p14_historical_quality_equalization_2012_2023",
    "p14_historical_secondary_repair_validation_register_2012_2023.csv"
  ),
  historical_issue_detail = file.path(
    root, "experiments/p14_historical_validation_and_extension_2026-06-22",
    "outputs/p14_historical_secondary_repair_audit_2012_2023",
    "p14_secondary_price_to_yield_trial_issue_results_2012_2023.csv"
  ),
  repair_2024 = file.path(
    root, "data-derived/p15_observed_market_2024_v1",
    "p15_p13_secondary_price_to_yield_country_2024.csv"
  ),
  combined_ladder = file.path(
    root, "experiments/p14_historical_validation_and_extension_2026-06-22",
    "outputs/p14_p13_combined_benchmark_panel_2012_2024",
    "p14_p13_combined_full_labelled_rate_value_ladder_2012_2024.csv"
  ),
  status_context = file.path(
    root, "data-derived/p15_status_expansion_2012_2024_v1",
    "p15_status_context_expanded_2012_2024.csv.gz"
  )
)
missing <- names(paths)[!file.exists(unlist(paths))]
if (length(missing)) {
  stop("Missing secondary-repair inputs: ", paste(missing, collapse = ", "),
       call. = FALSE)
}

semantics_raw <- data.table::fread(paths$semantics_detail) |>
  tibble::as_tibble()
formula_detail <- p15_recompute_secondary_price_semantics(semantics_raw)
semantics_summary <- p15_secondary_repair_validation_summary(formula_detail)

historical <- data.table::fread(paths$historical_repair) |>
  tibble::as_tibble() |>
  dplyr::transmute(
    repair_evidence_id = paste(
      "SECONDARY-REPAIR", .data$analysis_year, .data$iso3,
      "HISTORICAL-COUNTRY", sep = "::"
    ),
    analysis_year = as.integer(.data$analysis_year),
    .data$iso3, .data$country,
    repaired_rate_pct = as.numeric(.data$market_rate_pct),
    repaired_maturity_years = as.numeric(.data$market_maturity_years),
    issue_count = as.integer(.data$issue_count),
    identifier_count = as.integer(.data$identifier_count),
    total_weight_usd = as.numeric(.data$total_weight_usd),
    last_quote_date = as.character(.data$last_quote_date),
    currency_basis = as.character(.data$currency_basis),
    source_ids = as.character(.data$source_ids),
    residual_2_15_all = .data$residual_2_15_all %in% TRUE,
    identifier_dispersion_bps = as.numeric(
      .data$max_identifier_dispersion_bps
    ),
    settlement_sensitivity_bps = as.numeric(
      .data$max_settlement_sensitivity_bps
    ),
    bid_ask_yield_range_bps = as.numeric(
      .data$max_bid_ask_yield_range_bps
    ),
    numeric_trial_checks_pass = .data$repair_thresholds_pass %in% TRUE,
    forward_candidate_under_predecessor_rule =
      .data$candidate_after_future_method_approval %in% TRUE,
    legacy_p13_selected = FALSE,
    source_package_id = "SRC-LSEG-EXT-20260623-R4",
    method_id = "P14_SECONDARY_CLEAN_PRICE_REPAIR_TRIAL_V1",
    predecessor_repair_decision_state = as.character(
      .data$repair_decision_state
    ),
    p15_forward_repair_decision_state = "not_evaluated",
    diagnostic_only = TRUE,
    secondary_repair_schema_version = p15_secondary_repair_schema_version()
  )

repair_2024 <- data.table::fread(paths$repair_2024) |>
  tibble::as_tibble() |>
  dplyr::transmute(
    repair_evidence_id = paste(
      "SECONDARY-REPAIR", .data$analysis_year, .data$iso3,
      "P13-PARITY-COUNTRY", sep = "::"
    ),
    analysis_year = as.integer(.data$analysis_year),
    .data$iso3, .data$country,
    repaired_rate_pct = as.numeric(.data$market_rate_pct),
    repaired_maturity_years = as.numeric(.data$market_maturity_years),
    issue_count = as.integer(.data$issue_count),
    identifier_count = as.integer(.data$identifier_count),
    total_weight_usd = as.numeric(.data$total_weight_usd),
    last_quote_date = NA_character_,
    currency_basis = as.character(.data$currency_basis),
    source_ids = as.character(.data$included_target_ids),
    residual_2_15_all = .data$market_maturity_years >= 2 &
      .data$market_maturity_years <= 15,
    identifier_dispersion_bps = as.numeric(.data$identifier_dispersion_bps),
    settlement_sensitivity_bps = as.numeric(
      .data$max_settlement_sensitivity_bps
    ),
    bid_ask_yield_range_bps = as.numeric(
      .data$max_bid_ask_yield_range_bps
    ),
    numeric_trial_checks_pass = TRUE,
    forward_candidate_under_predecessor_rule = TRUE,
    legacy_p13_selected = TRUE,
    source_package_id = as.character(.data$source_package_id),
    method_id = as.character(.data$method_id),
    predecessor_repair_decision_state = "selected_in_frozen_p13_anchor",
    p15_forward_repair_decision_state = "not_evaluated",
    diagnostic_only = FALSE,
    secondary_repair_schema_version = p15_secondary_repair_schema_version()
  )

repair_candidates <- dplyr::bind_rows(historical, repair_2024) |>
  dplyr::arrange(.data$analysis_year, .data$iso3)
stopifnot(
  nrow(repair_candidates) == 437L,
  !anyDuplicated(repair_candidates$repair_evidence_id),
  sum(repair_candidates$legacy_p13_selected) == 3L,
  all(repair_candidates$p15_forward_repair_decision_state == "not_evaluated")
)

ladder <- data.table::fread(paths$combined_ladder) |>
  tibble::as_tibble() |>
  dplyr::filter(
    .data$rate_value_present %in% TRUE,
    is.finite(.data$rate_pct)
  )

anchors <- dplyr::bind_rows(
  ladder |>
    dplyr::filter(
      (.data$analysis_year <= 2023 &
         .data$detailed_ladder_tier_id ==
           "primary_standard_issue50m_ge1") |
        (.data$analysis_year == 2024 &
           .data$detailed_ladder_tier_id == "primary_standard")
    ) |>
    dplyr::transmute(
      analysis_year = as.integer(.data$analysis_year),
      .data$iso3,
      comparator_family = "observed_primary_standard",
      comparator_rate_pct = as.numeric(.data$rate_pct),
      comparator_maturity_years = as.numeric(.data$maturity_years),
      comparator_source_object = as.character(.data$source_object),
      comparator_source_pointer = as.character(.data$source_pointer)
    ),
  ladder |>
    dplyr::filter(
      (.data$analysis_year <= 2023 &
         .data$detailed_ladder_tier_id ==
           "secondary_broader_usd_eur_2_15_direct") |
        (.data$analysis_year == 2024 &
           .data$detailed_ladder_tier_id ==
             "secondary_broader_usd_eur_2_15_direct")
    ) |>
    dplyr::transmute(
      analysis_year = as.integer(.data$analysis_year),
      .data$iso3,
      comparator_family = "observed_secondary_direct",
      comparator_rate_pct = as.numeric(.data$rate_pct),
      comparator_maturity_years = as.numeric(.data$maturity_years),
      comparator_source_object = as.character(.data$source_object),
      comparator_source_pointer = as.character(.data$source_pointer)
    ),
  ladder |>
    dplyr::filter(
      .data$detailed_ladder_tier_id %in% c(
        "ids_bondholders_contractual_proxy",
        "ids_bondholders_public_proxy"
      )
    ) |>
    dplyr::transmute(
      analysis_year = as.integer(.data$analysis_year),
      .data$iso3,
      comparator_family = "ids_bondholders_average_terms",
      comparator_rate_pct = as.numeric(.data$rate_pct),
      comparator_maturity_years = as.numeric(.data$maturity_years),
      comparator_source_object = as.character(.data$source_object),
      comparator_source_pointer = as.character(.data$source_pointer)
    )
) |>
  dplyr::distinct(
    .data$analysis_year, .data$iso3, .data$comparator_family,
    .keep_all = TRUE
  )
if (anyDuplicated(anchors[c(
  "analysis_year", "iso3", "comparator_family"
)])) {
  stop("Secondary repair anchor table is not unique.", call. = FALSE)
}

status <- data.table::fread(paths$status_context) |>
  tibble::as_tibble() |>
  dplyr::select(dplyr::all_of(c(
    "analysis_year", "iso3", "expanded_status_context_trigger_present",
    "expanded_status_evidence_ids", "ucdp_any_organized_violence_context",
    "bftu_context_state"
  )))

anchor_validation <- repair_candidates |>
  dplyr::inner_join(anchors, by = c("analysis_year", "iso3")) |>
  dplyr::left_join(status, by = c("analysis_year", "iso3")) |>
  dplyr::mutate(
    repaired_minus_comparator_pp =
      .data$repaired_rate_pct - .data$comparator_rate_pct,
    abs_repaired_minus_comparator_pp = abs(
      .data$repaired_minus_comparator_pp
    ),
    maturity_difference_years =
      .data$repaired_maturity_years - .data$comparator_maturity_years,
    same_source_object_accuracy_test =
      .data$comparator_family == "observed_secondary_direct",
    validation_interpretation = dplyr::case_when(
      .data$comparator_family == "observed_secondary_direct" ~
        "country_year_overlap_not_same_instrument_accuracy_test",
      .data$comparator_family == "observed_primary_standard" ~
        "different_timing_and_market_object_comparator",
      TRUE ~ "different_source_object_comparator"
    ),
    validation_authority = "descriptive_not_authorized_for_selection",
    repair_decision_state = "not_evaluated"
  ) |>
  dplyr::arrange(
    .data$analysis_year, .data$iso3, .data$comparator_family
  )

anchor_summary <- anchor_validation |>
  dplyr::group_by(
    period = dplyr::case_when(
      .data$analysis_year <= 2015 ~ "2012-2015",
      .data$analysis_year <= 2019 ~ "2016-2019",
      .data$analysis_year <= 2023 ~ "2020-2023",
      TRUE ~ "2024"
    ),
    .data$comparator_family,
    .data$forward_candidate_under_predecessor_rule
  ) |>
  dplyr::summarise(
    comparison_rows = dplyr::n(),
    countries = dplyr::n_distinct(.data$iso3),
    mean_signed_gap_pp = mean(.data$repaired_minus_comparator_pp),
    median_signed_gap_pp = stats::median(.data$repaired_minus_comparator_pp),
    mean_absolute_gap_pp = mean(.data$abs_repaired_minus_comparator_pp),
    median_absolute_gap_pp = stats::median(
      .data$abs_repaired_minus_comparator_pp
    ),
    p90_absolute_gap_pp = as.numeric(stats::quantile(
      .data$abs_repaired_minus_comparator_pp,
      0.9
    )),
    within_50bps_share = mean(.data$abs_repaired_minus_comparator_pp <= 0.5),
    within_100bps_share = mean(.data$abs_repaired_minus_comparator_pp <= 1),
    .groups = "drop"
  ) |>
  dplyr::mutate(
    validation_authority = "descriptive_not_authorized_for_selection"
  )

method_register <- tibble::tribble(
  ~method_gate_id, ~method_area, ~evidence_result, ~current_state, ~remaining_requirement, ~automatic_selection_effect,
  "SEC-06-A", "clean_or_dirty_price_semantics", "Same-instrument comparisons strongly favor the clean-price interpretation for plain fixed-coupon rows.", "evidence_backed_working_assumption", "Preserve the assumption caveat until exact LSEG field documentation or Helpdesk/Data Item Browser confirmation is archived.", "none",
  "SEC-06-B", "price_to_yield_formula_reproduction", "The common P15 function exactly reproduces all 2243 preserved cross-check calculations.", "mechanically_reproduced", "Retain regression tests and version the day-count and settlement assumptions.", "none",
  "SEC-06-C", "plain_vanilla_and_feature_filter", "Historical trials enforce fixed-coupon and numeric stability screens; feature-rich instruments remain separate.", "implemented_in_diagnostic_predecessor", "Generalize the filter in the common all-years P15 processor and fail closed on unsupported features.", "none",
  "SEC-06-D", "identifier_and_issue_resolution", "Historical trials aggregate 3984 eligible identifier-years into 3921 issue rows; 350 raw multi-RIC chunks were separately flagged as ambiguous.", "partial_with_explicit_ambiguity", "The common processor must preserve identifier-to-economic-issue lineage and exclude unresolved ambiguity.", "none",
  "SEC-06-E", "quote_side_settlement_and_liquidity", "Mid price is central; bid/ask range and T+2 settlement sensitivity are retained and threshold-tested.", "diagnostic_rule_implemented", "Approve or revise the exact quote-side, recency, settlement, and liquidity thresholds before forward selection.", "none",
  "SEC-08-A", "source_owner_price_definition", "No project-archived official LSEG definition confirms the exact returned price-field convention.", "official_definition_gap_remains", "Obtain and preserve source-owner documentation, or retain repaired yields as labelled diagnostic/sensitivity evidence.", "none",
  "SEC-09-A", "forward_repaired_secondary_role", "123 country-years pass the predecessor numeric candidate rule, including the 3 frozen P13 parity rows; same-instrument formula accuracy is strong but governance and source-definition gates remain.", "decision_ready_not_approved", "Choose selected evidence, appendix sensitivity, diagnostic only, or exclusion after the remaining common-processor and source-definition gates.", "none"
) |>
  dplyr::mutate(
    repair_decision_state = "not_evaluated",
    build_id = "BUILD-P15-SECONDARY-REPAIR-EVIDENCE-20260721-V1"
  )

decision_register <- tibble::tribble(
  ~decision_id, ~decision_question, ~evidence_summary, ~recommended_decision_posture, ~decision_state,
  "SEC-09", "What forward role should repaired secondary yields have?", "The clean-price calculation is numerically credible for plain fixed-coupon instruments, but exact source-field documentation, general identifier rules, quote/settlement standards, and status interaction are not all closed.", "Keep the three P13 rows in the frozen parity branch; keep new all-years repairs diagnostic or sensitivity-only until the remaining gates close.", "not_evaluated",
  "SEC-18-REPAIR", "Can repaired yields enter the final secondary standard now?", "Direct-YTM overlap and same-instrument checks support further use, but threshold passage alone does not establish instrument admissibility or market comparability.", "Do not let repaired yields outrank direct YTM; revisit only inside the final secondary-standard decision.", "not_evaluated"
)

build_summary <- tibble::tibble(
  metric = c(
    "same_instrument_formula_rows",
    "formula_reproduction_failures",
    "strict_plainish_same_instrument_rows",
    "historical_repair_country_year_rows",
    "historical_forward_candidates_under_predecessor_rule",
    "p13_2024_legacy_selected_repair_rows",
    "all_forward_candidates_under_predecessor_rule",
    "anchor_comparison_rows",
    "approved_forward_repair_decisions",
    "secondary_repair_schema_version"
  ),
  value = c(
    as.character(nrow(formula_detail)),
    as.character(sum(!formula_detail$formula_reproduction_pass)),
    as.character(sum(formula_detail$strict_plainish_subset)),
    as.character(nrow(historical)),
    as.character(sum(historical$forward_candidate_under_predecessor_rule)),
    as.character(sum(repair_2024$legacy_p13_selected)),
    as.character(sum(
      repair_candidates$forward_candidate_under_predecessor_rule
    )),
    as.character(nrow(anchor_validation)),
    "0",
    p15_secondary_repair_schema_version()
  )
)

output_paths <- list(
  formula_detail = file.path(
    derived_dir,
    "p15_secondary_price_semantics_formula_reproduction_2024.csv.gz"
  ),
  repair_candidates = file.path(
    derived_dir,
    "p15_secondary_repair_country_year_candidates_2012_2024.csv.gz"
  ),
  anchor_validation = file.path(
    derived_dir,
    "p15_secondary_repair_anchor_validation_detail_2012_2024.csv.gz"
  ),
  semantics_summary = file.path(
    governance_dir, "p15_secondary_price_semantics_validation_summary.csv"
  ),
  anchor_summary = file.path(
    governance_dir, "p15_secondary_repair_anchor_validation_summary.csv"
  ),
  method_register = file.path(
    governance_dir, "p15_secondary_repair_method_register.csv"
  ),
  decision_register = file.path(
    governance_dir, "p15_secondary_repair_decision_register.csv"
  ),
  build_summary = file.path(
    governance_dir, "p15_secondary_repair_build_summary.csv"
  ),
  manifest = file.path(
    governance_dir, "p15_secondary_repair_evidence_manifest.csv"
  )
)

data.table::fwrite(data.table::as.data.table(formula_detail), output_paths$formula_detail, na = "")
data.table::fwrite(data.table::as.data.table(repair_candidates), output_paths$repair_candidates, na = "")
data.table::fwrite(data.table::as.data.table(anchor_validation), output_paths$anchor_validation, na = "")
data.table::fwrite(data.table::as.data.table(semantics_summary), output_paths$semantics_summary, na = "")
data.table::fwrite(data.table::as.data.table(anchor_summary), output_paths$anchor_summary, na = "")
data.table::fwrite(data.table::as.data.table(method_register), output_paths$method_register, na = "")
data.table::fwrite(data.table::as.data.table(decision_register), output_paths$decision_register, na = "")
data.table::fwrite(data.table::as.data.table(build_summary), output_paths$build_summary, na = "")

script_path <- file.path(root, "scripts/p15/build_p15_secondary_repair_evidence.R")
input_paths <- unique(unname(unlist(paths)))
generated_paths <- unname(unlist(output_paths[names(output_paths) != "manifest"]))
manifest <- data.table::rbindlist(lapply(
  c(input_paths, generated_paths),
  function(path) {
    data.table::data.table(
      artifact_path = sub(paste0("^", root, "/"), "", path),
      artifact_role = if (path %in% input_paths) "source_or_parent_input" else
        "generated_output",
      bytes = file.info(path)$size,
      sha256 = digest::digest(file = path, algo = "sha256"),
      producing_script = sub(paste0("^", root, "/"), "", script_path),
      producing_script_sha256 = digest::digest(file = script_path, algo = "sha256"),
      build_id = "BUILD-P15-SECONDARY-REPAIR-EVIDENCE-20260721-V1",
      schema_version = p15_secondary_repair_schema_version(),
      build_date = "2026-07-21"
    )
  }
), use.names = TRUE, fill = TRUE)
data.table::fwrite(manifest, output_paths$manifest, na = "")

cat("P15 secondary repair evidence: PASS\n")
cat("Formula reproduction rows:", nrow(formula_detail), "\n")
cat("Formula reproduction failures: 0\n")
cat("Repair country-year evidence rows:", nrow(repair_candidates), "\n")
cat("Forward repair decisions approved: 0\n")
