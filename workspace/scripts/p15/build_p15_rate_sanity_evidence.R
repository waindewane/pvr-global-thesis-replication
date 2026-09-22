#!/usr/bin/env Rscript

# Compare controlled rate-sanity threshold variants over the current all-years
# evidence universe. This build prepares SANE-01 to SANE-03 evidence and makes
# no admissibility decision.

suppressPackageStartupMessages({
  library(data.table)
  library(digest)
  library(dplyr)
  library(readr)
  library(tibble)
  library(tidyr)
})

source("R/p15_rate_sanity.R")

root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
derived_dir <- file.path(root, "data-derived", "p15_rate_sanity_2012_2024_v1")
governance_dir <- file.path(root, "docs", "governance")
dir.create(derived_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(governance_dir, recursive = TRUE, showWarnings = FALSE)

paths <- list(
  predecessor_ladder = file.path(
    root, "experiments/p14_historical_validation_and_extension_2026-06-22",
    "outputs/p14_p13_combined_benchmark_panel_2012_2024",
    "p14_p13_combined_full_labelled_rate_value_ladder_2012_2024.csv"
  ),
  rating_candidates = file.path(
    root, "data-derived/p15_rating_validation_2012_2024_v1",
    "p15_rating_candidate_variants_2012_2024.csv.gz"
  ),
  peer_candidates = file.path(
    root, "data-derived/p15_peer_proxy_2012_2024_v1",
    "p15_peer_candidate_variants_2012_2024.csv.gz"
  ),
  status_context = file.path(
    root, "data-derived/p15_status_expansion_2012_2024_v1",
    "p15_status_context_expanded_2012_2024.csv.gz"
  ),
  predecessor_sanity_flags = file.path(
    root, "experiments/full_ladder_ratings_integrated_status_rebuild_2026-06-01",
    "outputs/diagnostic_rate_sanity_flags_all_years.csv"
  ),
  p7h_zip = file.path(
    root, "sources/market_rates/p7h_diagnostic_audit_terminal_returns_2024",
    "p7h_diagnostic_audit_terminal_return_20260605_200210.zip"
  )
)
missing <- names(paths)[!file.exists(unlist(paths))]
if (length(missing)) {
  stop("Missing rate-sanity inputs: ", paste(missing, collapse = ", "),
       call. = FALSE)
}

predecessor <- data.table::fread(paths$predecessor_ladder) |>
  tibble::as_tibble() |>
  dplyr::filter(
    .data$rate_value_present %in% TRUE,
    is.finite(.data$rate_pct)
  ) |>
  dplyr::transmute(
    rate_evidence_id = paste0("PREDECESSOR::", .data$combined_rate_value_id),
    analysis_year = as.integer(.data$analysis_year),
    .data$iso3, .data$country,
    evidence_family = as.character(.data$evidence_family),
    source_object = as.character(.data$source_object),
    method_or_subtype_id = as.character(.data$detailed_ladder_tier_id),
    rate_pct = as.numeric(.data$rate_pct),
    maturity_years = as.numeric(.data$maturity_years),
    currency_basis = as.character(.data$currency_basis),
    source_pointer = as.character(.data$source_pointer),
    source_package_or_generation = as.character(.data$panel_source_package),
    predecessor_rate_sanity_state = as.character(.data$rate_sanity_state),
    evidence_governance_state = "predecessor_reference",
    evidence_generation = "p14_p13_combined_predecessor"
  )

rating <- data.table::fread(paths$rating_candidates) |>
  tibble::as_tibble() |>
  dplyr::filter(
    .data$variant_available %in% TRUE,
    is.finite(.data$variant_rate_pct)
  ) |>
  dplyr::transmute(
    rate_evidence_id = paste(
      "RATING", .data$analysis_year, .data$iso3, .data$variant_id,
      sep = "::"
    ),
    analysis_year = as.integer(.data$analysis_year),
    .data$iso3, .data$country,
    evidence_family = "rating_implied_candidate_variant",
    source_object = as.character(.data$variant_family),
    method_or_subtype_id = as.character(.data$variant_id),
    rate_pct = as.numeric(.data$variant_rate_pct),
    maturity_years = 7,
    currency_basis = "USD risk-free component; mapping variant",
    source_pointer = paste0(
      "variant=", .data$variant_id,
      ";rating=", .data$variant_rating_normalized,
      ";spread_rule=", .data$spread_rule
    ),
    source_package_or_generation =
      "BUILD-P15-RATING-VARIANT-VALIDATION-20260721-V1",
    predecessor_rate_sanity_state = NA_character_,
    evidence_governance_state = as.character(.data$variant_decision_state),
    evidence_generation = "p15_rating_candidate"
  )

peer <- data.table::fread(paths$peer_candidates) |>
  tibble::as_tibble() |>
  dplyr::filter(
    .data$candidate_computed %in% TRUE,
    is.finite(.data$peer_rate_median_pct)
  ) |>
  dplyr::transmute(
    rate_evidence_id = paste(
      "PEER", .data$analysis_year, .data$iso3, .data$peer_variant_id,
      sep = "::"
    ),
    analysis_year = as.integer(.data$analysis_year),
    .data$iso3, .data$country,
    evidence_family = "peer_proxy_candidate_variant",
    source_object = "target_excluding_nonrecursive_peer_median",
    method_or_subtype_id = as.character(.data$peer_variant_id),
    rate_pct = as.numeric(.data$peer_rate_median_pct),
    maturity_years = as.numeric(.data$peer_maturity_median_years),
    currency_basis = "mixed seed-source basis retained in membership ledger",
    source_pointer = paste0(
      "variant=", .data$peer_variant_id,
      ";members=", .data$peer_country_count,
      ";sources=", .data$peer_source_package_ids
    ),
    source_package_or_generation =
      "BUILD-P15-PEER-PROXY-20260721-V1",
    predecessor_rate_sanity_state = NA_character_,
    evidence_governance_state = as.character(.data$candidate_governance_state),
    evidence_generation = "p15_peer_candidate"
  )

rate_universe <- dplyr::bind_rows(predecessor, rating, peer) |>
  dplyr::arrange(.data$analysis_year, .data$iso3, .data$rate_evidence_id)
p15_validate_rate_sanity_universe(rate_universe)

parameters <- p15_rate_sanity_parameters()
evaluation <- p15_apply_rate_sanity_variants(rate_universe, parameters)

status <- data.table::fread(paths$status_context) |>
  tibble::as_tibble() |>
  dplyr::select(dplyr::all_of(c(
    "analysis_year", "iso3", "expanded_status_context_trigger_present",
    "expanded_status_evidence_ids", "ucdp_any_organized_violence_context",
    "bftu_context_state"
  )))

predecessor_flags <- data.table::fread(paths$predecessor_sanity_flags) |>
  tibble::as_tibble() |>
  dplyr::group_by(.data$analysis_year, .data$iso3) |>
  dplyr::summarise(
    predecessor_sanity_flag_count = dplyr::n(),
    predecessor_sanity_flag_types = paste(
      sort(unique(.data$flag_type)),
      collapse = ";"
    ),
    predecessor_sanity_max_severity = dplyr::case_when(
      any(.data$severity == "high") ~ "high",
      any(.data$severity == "medium") ~ "medium",
      TRUE ~ "low"
    ),
    .groups = "drop"
  )

p7h_connection <- unz(
  paths$p7h_zip,
  "20260605_200210/targets/p7h_diagnostic_audit_cases.csv"
)
p7h_cases_raw <- readr::read_csv(p7h_connection, show_col_types = FALSE)
p7h_cases <- p7h_cases_raw |>
  dplyr::filter(.data$analysis_year %in% 2012:2024) |>
  dplyr::group_by(.data$analysis_year, .data$iso3) |>
  dplyr::summarise(
    p7h_case_count = dplyr::n(),
    p7h_audit_case_ids = paste(.data$audit_case_id, collapse = ";"),
    p7h_flag_types = paste(sort(unique(.data$flag_type)), collapse = ";"),
    p7h_review_buckets = paste(
      sort(unique(.data$review_bucket)),
      collapse = ";"
    ),
    .groups = "drop"
  )

case_states <- evaluation |>
  dplyr::select(dplyr::all_of(c(
    "rate_evidence_id", "sanity_variant_id", "sanity_state",
    "threshold_pass"
  ))) |>
  tidyr::pivot_wider(
    names_from = "sanity_variant_id",
    values_from = c("sanity_state", "threshold_pass"),
    names_glue = "{.value}_{sanity_variant_id}"
  )

baseline_failures <- evaluation |>
  dplyr::filter(
    .data$sanity_variant_id == "baseline_1_30",
    !.data$threshold_pass
  ) |>
  dplyr::transmute(
    rate_evidence_id = .data$rate_evidence_id,
    source_family_class = .data$source_family_class,
    baseline_sanity_state = .data$sanity_state,
    distance_below_lower_pp = .data$distance_below_lower_pp,
    distance_above_upper_pp = .data$distance_above_upper_pp
  )

casebook <- rate_universe |>
  dplyr::inner_join(baseline_failures, by = "rate_evidence_id") |>
  dplyr::left_join(case_states, by = "rate_evidence_id") |>
  dplyr::left_join(status, by = c("analysis_year", "iso3")) |>
  dplyr::left_join(
    predecessor_flags,
    by = c("analysis_year", "iso3")
  ) |>
  dplyr::left_join(p7h_cases, by = c("analysis_year", "iso3")) |>
  dplyr::mutate(
    predecessor_sanity_flag_count = tidyr::replace_na(
      .data$predecessor_sanity_flag_count,
      0L
    ),
    p7h_case_count = tidyr::replace_na(.data$p7h_case_count, 0L),
    likely_explanation = p15_rate_sanity_case_explanation(
      .data$source_family_class,
      .data$rate_pct,
      .data$analysis_year
    ),
    recommended_treatment = dplyr::case_when(
      .data$source_family_class == "ids_terms" & .data$rate_pct < 1 ~
        "retain_as_valid_source_object_pending_source_specific_sanity_rule",
      .data$rate_pct > 100 ~ "block_pending_underlying_source_review",
      .data$rate_pct > 40 ~ "quarantine_pending_crisis_or_field_review",
      .data$rate_pct > 30 ~ "case_review_before_any_quantitative_use",
      .data$rate_pct < 0 & .data$rate_pct >= -1 &
        .data$source_family_class %in%
          c("observed_primary", "observed_secondary") ~
        "verify_then_consider_bounded_negative_yield_exception",
      TRUE ~ "source_specific_review_before_exclusion"
    ),
    case_review_state = "pending_research_decision",
    automatic_consequence = "none"
  ) |>
  dplyr::arrange(
    dplyr::desc(abs(.data$rate_pct)),
    .data$analysis_year,
    .data$iso3
  )

summary <- evaluation |>
  dplyr::group_by(
    .data$sanity_variant_id,
    .data$variant_label,
    .data$source_family_class
  ) |>
  dplyr::summarise(
    evaluated_rate_rows = dplyr::n(),
    threshold_pass_rows = sum(.data$threshold_pass),
    below_lower_bound_rows = sum(.data$sanity_state == "below_lower_bound"),
    above_upper_bound_rows = sum(.data$sanity_state == "above_upper_bound"),
    threshold_pass_share = mean(.data$threshold_pass),
    minimum_rate_pct = min(.data$rate_pct),
    maximum_rate_pct = max(.data$rate_pct),
    .groups = "drop"
  ) |>
  dplyr::arrange(.data$sanity_variant_id, .data$source_family_class)

baseline_state <- evaluation |>
  dplyr::filter(.data$sanity_variant_id == "baseline_1_30") |>
  dplyr::transmute(
    rate_evidence_id = .data$rate_evidence_id,
    baseline_threshold_pass = .data$threshold_pass,
    baseline_sanity_state = .data$sanity_state
  )
transition_summary <- evaluation |>
  dplyr::filter(.data$sanity_variant_id != "baseline_1_30") |>
  dplyr::left_join(baseline_state, by = "rate_evidence_id") |>
  dplyr::mutate(
    transition = dplyr::case_when(
      .data$baseline_threshold_pass & .data$threshold_pass ~ "pass_to_pass",
      .data$baseline_threshold_pass & !.data$threshold_pass ~ "pass_to_fail",
      !.data$baseline_threshold_pass & .data$threshold_pass ~ "fail_to_pass",
      TRUE ~ "fail_to_fail"
    )
  ) |>
  dplyr::count(
    .data$sanity_variant_id,
    .data$source_family_class,
    .data$transition,
    name = "rate_rows"
  ) |>
  dplyr::arrange(
    .data$sanity_variant_id,
    .data$source_family_class,
    .data$transition
  )

decision_register <- tibble::tribble(
  ~decision_id, ~decision_question, ~current_evidence_result, ~recommended_decision_posture, ~decision_state,
  "SANE-04-A", "Should the same lower bound apply to every source object?", "The 1 percent lower bound flags legitimate IDS average-term observations and low or negative observed yields.", "Do not approve one universal lower bound without source-object exceptions.", "not_evaluated",
  "SANE-04-B", "Should rates above 30 percent be discarded automatically?", "Values above 30 include crisis observations, diagnostic secondary outliers, and a small number of proxy tails.", "Retain a review/quarantine boundary rather than automatic deletion; require source and status context.", "not_evaluated",
  "SANE-04-C", "Can threshold passage establish admissibility?", "Thresholds catch obvious tails but do not validate field semantics, instrument suitability, or source-object comparability.", "Use sanity as one evidence flag only, never as sufficient admissibility proof.", "not_evaluated"
)

output_paths <- list(
  universe = file.path(
    derived_dir, "p15_rate_sanity_evidence_universe_2012_2024.csv.gz"
  ),
  evaluation = file.path(
    derived_dir, "p15_rate_sanity_variant_evaluation_2012_2024.csv.gz"
  ),
  casebook = file.path(
    derived_dir, "p15_rate_sanity_casebook_2012_2024.csv.gz"
  ),
  parameters = file.path(
    governance_dir, "p15_rate_sanity_parameter_register.csv"
  ),
  summary = file.path(
    governance_dir, "p15_rate_sanity_variant_summary.csv"
  ),
  transitions = file.path(
    governance_dir, "p15_rate_sanity_variant_transition_summary.csv"
  ),
  decisions = file.path(
    governance_dir, "p15_rate_sanity_decision_register.csv"
  ),
  manifest = file.path(
    governance_dir, "p15_rate_sanity_evidence_manifest.csv"
  )
)

data.table::fwrite(data.table::as.data.table(rate_universe), output_paths$universe, na = "")
data.table::fwrite(data.table::as.data.table(evaluation), output_paths$evaluation, na = "")
data.table::fwrite(data.table::as.data.table(casebook), output_paths$casebook, na = "")
data.table::fwrite(data.table::as.data.table(parameters), output_paths$parameters, na = "")
data.table::fwrite(data.table::as.data.table(summary), output_paths$summary, na = "")
data.table::fwrite(data.table::as.data.table(transition_summary), output_paths$transitions, na = "")
data.table::fwrite(data.table::as.data.table(decision_register), output_paths$decisions, na = "")

script_path <- file.path(root, "scripts/p15/build_p15_rate_sanity_evidence.R")
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
      build_id = "BUILD-P15-RATE-SANITY-EVIDENCE-20260721-V1",
      schema_version = p15_rate_sanity_schema_version(),
      build_date = "2026-07-21"
    )
  }
), use.names = TRUE, fill = TRUE)
data.table::fwrite(manifest, output_paths$manifest, na = "")

cat("P15 rate-sanity evidence: PASS\n")
cat("Rate-evidence rows:", nrow(rate_universe), "\n")
cat("Threshold-evaluation rows:", nrow(evaluation), "\n")
cat("Legacy-baseline casebook rows:", nrow(casebook), "\n")
cat("Approved sanity decisions: 0\n")
