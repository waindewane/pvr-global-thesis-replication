#!/usr/bin/env Rscript

# Build neutral common 2012-2024 observed-primary and observed-secondary
# evidence. Candidate variants are decision surfaces, not selected benchmarks.

suppressPackageStartupMessages({
  library(data.table)
  library(digest)
  library(dplyr)
  library(tibble)
})

source("R/p15_observed_all_years.R")

root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
derived_dir <- file.path(
  root, "data-derived", "p15_observed_markets_2012_2024_v1"
)
governance_dir <- file.path(root, "docs", "governance")
dir.create(derived_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(governance_dir, recursive = TRUE, showWarnings = FALSE)

paths <- list(
  base_zip = file.path(
    root, "sources", "market_rates",
    "lseg_extension_2012_2024_partial_resume_20260721",
    paste0(
      "pvr-global-lseg-extension-2012-2024-usd-eur_",
      "20260623_184707_partial_resume_20260721.zip"
    )
  ),
  instrument_universe = file.path(
    root, "data-derived", "p15_unified_lseg_source_2012_2024_v1",
    "p15_instrument_year_universe_2012_2024.csv.gz"
  ),
  secondary_history = file.path(
    root, "data-derived", "p15_unified_lseg_source_2012_2024_v1",
    "p15_secondary_history_long_2012_2024.csv.gz"
  ),
  country_grid = file.path(
    root, "data-derived", "p15_platform_2012_2024_v1",
    "p15_country_year_grid_2012_2024.csv"
  ),
  predecessor_primary = file.path(
    root, "experiments", "p14_historical_validation_and_extension_2026-06-22",
    "outputs", "primary_partial_trial_20260623_184707",
    "primary_partial_trial_country_year_2012_2024.csv"
  ),
  predecessor_secondary = file.path(
    root, "experiments", "p14_historical_validation_and_extension_2026-06-22",
    "outputs", "p14_historical_quality_equalization_2012_2023",
    "p14_historical_secondary_country_year_rates_2012_2023.csv"
  ),
  p13_primary = file.path(
    root, "data-derived", "p15_observed_market_2024_v1",
    "p15_p13_primary_selected_parity_2024.csv"
  ),
  p13_secondary = file.path(
    root, "data-derived", "p15_observed_market_2024_v1",
    "p15_p13_secondary_selected_parity_2024.csv"
  )
)
missing <- names(paths)[!file.exists(unlist(paths))]
if (length(missing)) {
  stop("Missing observed-market inputs: ", paste(missing, collapse = ", "),
       call. = FALSE)
}

zip_info <- utils::unzip(paths$base_zip, list = TRUE)
primary_members <- zip_info$Name[grepl(
  paste0(
    "raw/desktop_enrichment_chunks/",
    "lseg_desktop_primary_issue_terms_chunk_[0-9]{5}_.*[.]csv$"
  ),
  zip_info$Name
)]
context_members <- zip_info$Name[grepl(
  "derived/lseg_all_desktop_identifier_context_.*[.]csv$",
  zip_info$Name
)]
if (length(primary_members) != 502L || length(context_members) != 1L) {
  stop(
    "Unexpected primary source member inventory: ", length(primary_members),
    " primary chunks and ", length(context_members), " context files.",
    call. = FALSE
  )
}

extract_dir <- tempfile("p15_observed_primary_source_")
dir.create(extract_dir, recursive = TRUE)
on.exit(unlink(extract_dir, recursive = TRUE, force = TRUE), add = TRUE)
utils::unzip(
  paths$base_zip,
  files = c(primary_members, context_members),
  exdir = extract_dir
)
primary_terms <- data.table::rbindlist(lapply(primary_members, function(member) {
  x <- data.table::fread(
    file.path(extract_dir, member),
    data.table = FALSE,
    check.names = FALSE,
    integer64 = "double",
    showProgress = FALSE
  )
  x$source_archive_member <- member
  x$source_member_row <- seq_len(nrow(x))
  x
}), use.names = TRUE, fill = TRUE) |>
  tibble::as_tibble()
identifier_context <- data.table::fread(
  file.path(extract_dir, context_members[[1]]),
  data.table = FALSE,
  check.names = FALSE,
  showProgress = FALSE
) |>
  tibble::as_tibble()

instrument_universe <- data.table::fread(paths$instrument_universe) |>
  tibble::as_tibble()
secondary_history <- data.table::fread(paths$secondary_history) |>
  tibble::as_tibble()
country_grid <- data.table::fread(paths$country_grid) |>
  tibble::as_tibble()

primary_issues <- p15_build_primary_issue_evidence_all_years(
  primary_terms, identifier_context, instrument_universe, country_grid
)
primary_candidates <- p15_aggregate_primary_candidate_variants(primary_issues)
secondary_identifiers <- p15_build_secondary_identifier_evidence_all_years(
  instrument_universe, secondary_history, country_grid
)
secondary_issues <- p15_build_secondary_issue_evidence_all_years(
  secondary_identifiers
)
secondary_candidates <- p15_aggregate_secondary_candidate_variants(
  secondary_issues
)
availability <- p15_build_observed_availability_ledgers(
  country_grid, primary_issues, secondary_identifiers, secondary_issues
)

predecessor_primary <- data.table::fread(paths$predecessor_primary) |>
  tibble::as_tibble() |>
  dplyr::transmute(
    analysis_year = as.integer(.data$issue_year),
    iso3 = as.character(.data$canonical_iso3),
    predecessor_rate_pct = as.numeric(
      .data$amount_weighted_original_yield_pct
    ),
    predecessor_maturity_years = as.numeric(
      .data$amount_weighted_original_maturity_years
    ),
    predecessor_issue_count = as.integer(.data$issue_rows)
  )
primary_predecessor_comparison <- primary_candidates |>
  dplyr::filter(
    .data$candidate_variant_id ==
      "primary_predecessor_partial_trial_all_direct"
  ) |>
  dplyr::select(
    "analysis_year", "iso3", "country", "market_rate_pct",
    "market_maturity_years", "issue_count", "weight_basis"
  ) |>
  dplyr::full_join(
    predecessor_primary,
    by = c("analysis_year", "iso3")
  ) |>
  dplyr::mutate(
    rate_abs_diff_pp = abs(.data$market_rate_pct - .data$predecessor_rate_pct),
    maturity_abs_diff_years = abs(
      .data$market_maturity_years - .data$predecessor_maturity_years
    ),
    issue_count_delta = .data$issue_count - .data$predecessor_issue_count,
    weight_unit_normalization_present = stringr::str_detect(
      dplyr::coalesce(.data$weight_basis, ""), "face_issued_usd"
    ),
    comparison_state = dplyr::case_when(
      is.na(.data$predecessor_rate_pct) ~ "new_p15_country_year",
      is.na(.data$market_rate_pct) ~ "predecessor_only_country_year",
      .data$rate_abs_diff_pp <= 1e-10 &
        .data$maturity_abs_diff_years <= 1e-10 &
        .data$issue_count == .data$predecessor_issue_count ~
          "exact_predecessor_replication",
      TRUE ~ "source_or_processor_difference_requires_classification"
    ),
    difference_reason_code = dplyr::case_when(
      .data$comparison_state == "exact_predecessor_replication" ~
        "none_exact_replication",
      .data$comparison_state == "new_p15_country_year" ~
        "new_country_mapping_or_positive_weight_recovery",
      .data$comparison_state == "predecessor_only_country_year" ~
        "predecessor_source_row_not_in_common_resolved_issue_surface",
      .data$issue_count_delta != 0 &
        .data$weight_unit_normalization_present ~
        "economic_issue_deduplication_plus_usd_weight_normalization",
      .data$issue_count_delta != 0 ~
        "economic_issue_deduplication_or_identifier_scope_normalization",
      .data$weight_unit_normalization_present ~
        "usd_weight_normalization_replaces_source_currency_amount",
      .data$rate_abs_diff_pp > 1e-10 ~
        "original_yield_source_value_or_weight_normalization",
      TRUE ~ "maturity_or_static_source_normalization"
    ),
    comparison_role = "regression_diagnostic_not_authority"
  ) |>
  dplyr::arrange(.data$analysis_year, .data$iso3)

predecessor_secondary <- data.table::fread(paths$predecessor_secondary) |>
  tibble::as_tibble() |>
  dplyr::filter(
    .data$scenario_id ==
      "secondary_standard_usd_2_15_direct_historical_equalized"
  ) |>
  dplyr::transmute(
    analysis_year = as.integer(.data$analysis_year),
    iso3 = as.character(.data$iso3),
    predecessor_rate_pct = as.numeric(.data$market_rate_pct),
    predecessor_maturity_years = as.numeric(.data$market_maturity_years),
    predecessor_issue_count = as.integer(.data$issue_count)
  )
secondary_predecessor_comparison <- secondary_candidates |>
  dplyr::filter(
    .data$candidate_variant_id == "secondary_standard_usd_2_15_direct",
    .data$analysis_year <= 2023L
  ) |>
  dplyr::select(
    "analysis_year", "iso3", "country", "market_rate_pct",
    "market_maturity_years", "issue_count", "last_quote_date",
    "max_quote_recency_abs_days"
  ) |>
  dplyr::full_join(
    predecessor_secondary,
    by = c("analysis_year", "iso3")
  ) |>
  dplyr::mutate(
    rate_abs_diff_pp = abs(.data$market_rate_pct - .data$predecessor_rate_pct),
    maturity_abs_diff_years = abs(
      .data$market_maturity_years - .data$predecessor_maturity_years
    ),
    issue_count_delta = .data$issue_count - .data$predecessor_issue_count,
    comparison_state = dplyr::case_when(
      is.na(.data$predecessor_rate_pct) ~ "new_p15_country_year",
      is.na(.data$market_rate_pct) ~ "predecessor_only_country_year",
      .data$rate_abs_diff_pp <= 1e-10 &
        .data$maturity_abs_diff_years <= 1e-10 &
        .data$issue_count == .data$predecessor_issue_count ~
          "exact_predecessor_replication",
      TRUE ~ "source_or_processor_difference_requires_classification"
    ),
    difference_reason_code = dplyr::case_when(
      .data$comparison_state == "exact_predecessor_replication" ~
        "none_exact_replication",
      .data$comparison_state == "new_p15_country_year" ~
        "new_direct_quote_or_standard_issue_recovery",
      .data$comparison_state == "predecessor_only_country_year" ~
        "predecessor_candidate_not_in_common_standard_surface",
      .data$issue_count_delta != 0 ~
        "economic_issue_or_identifier_resolution_change",
      .data$rate_abs_diff_pp > 1e-10 &
        .data$max_quote_recency_abs_days > 0 ~
        "latest_quote_date_or_same_date_subrow_normalization",
      .data$rate_abs_diff_pp > 1e-10 ~
        "direct_yield_source_value_or_identifier_resolution",
      TRUE ~ "snapshot_date_maturity_normalization"
    ),
    comparison_role = "regression_diagnostic_not_authority"
  ) |>
  dplyr::arrange(.data$analysis_year, .data$iso3)

p13_primary <- data.table::fread(paths$p13_primary) |>
  tibble::as_tibble() |>
  dplyr::select(
    "analysis_year", "iso3", p13_country = "country",
    p13_rate_pct = "p13_rate_pct",
    p13_maturity_years = "p13_maturity_years"
  )
primary_anchor_comparison <- p13_primary |>
  dplyr::left_join(
    primary_candidates |>
      dplyr::filter(
        .data$candidate_variant_id == "primary_standard_usd_eur_50m",
        .data$analysis_year == 2024L
      ) |>
      dplyr::select(
        "analysis_year", "iso3", common_country = "country",
        common_rate_pct = "market_rate_pct",
        common_maturity_years = "market_maturity_years",
        common_issue_count = "issue_count", "source_package_ids"
      ),
    by = c("analysis_year", "iso3")
  ) |>
  dplyr::mutate(
    rate_abs_diff_pp = abs(.data$common_rate_pct - .data$p13_rate_pct),
    anchor_comparison_state = dplyr::case_when(
      is.na(.data$common_rate_pct) ~
        "common_broad_candidate_missing_companion_parity_branch_required",
      .data$rate_abs_diff_pp <= 1e-10 ~ "common_candidate_matches_p13_anchor",
      TRUE ~ "common_candidate_differs_p13_parity_branch_remains_authoritative"
    ),
    difference_reason_code = dplyr::case_when(
      is.na(.data$common_rate_pct) ~
        "broad_source_or_common_standard_candidate_missing_use_companion_parity",
      .data$rate_abs_diff_pp <= 1e-10 ~ "none_exact_rate_match",
      TRUE ~
        "broad_source_scope_issue_resolution_or_usd_weight_normalization"
    ),
    exact_parity_contract_state =
      "already_passed_in_SPEC-P15-P13-LEGACY-PARITY-V1"
  )

p13_secondary <- data.table::fread(paths$p13_secondary) |>
  tibble::as_tibble() |>
  dplyr::select(
    "analysis_year", "iso3", p13_country = "country_p13",
    p13_rate_pct = "p13_rate_pct",
    p13_maturity_years = "p13_maturity_years",
    "selected_source_class", "selected_tier_id"
  )
secondary_anchor_comparison <- p13_secondary |>
  dplyr::left_join(
    secondary_candidates |>
      dplyr::filter(
        .data$candidate_variant_id == "secondary_standard_usd_2_15_direct",
        .data$analysis_year == 2024L
      ) |>
      dplyr::select(
        "analysis_year", "iso3", common_country = "country",
        common_rate_pct = "market_rate_pct",
        common_maturity_years = "market_maturity_years",
        common_issue_count = "issue_count", "source_package_ids"
      ),
    by = c("analysis_year", "iso3")
  ) |>
  dplyr::mutate(
    rate_abs_diff_pp = abs(.data$common_rate_pct - .data$p13_rate_pct),
    anchor_comparison_state = dplyr::case_when(
      is.na(.data$common_rate_pct) ~
        "common_broad_candidate_missing_companion_parity_branch_required",
      .data$rate_abs_diff_pp <= 1e-10 ~ "common_candidate_matches_p13_anchor",
      TRUE ~ "common_candidate_differs_p13_parity_branch_remains_authoritative"
    ),
    difference_reason_code = dplyr::case_when(
      is.na(.data$common_rate_pct) ~
        "broad_direct_standard_candidate_missing_use_targeted_companion_parity",
      .data$rate_abs_diff_pp <= 1e-10 ~ "none_exact_rate_match",
      TRUE ~
        "broad_quote_vintage_or_latest_quote_identifier_resolution_difference"
    ),
    exact_parity_contract_state =
      "already_passed_in_SPEC-P15-P13-LEGACY-PARITY-V1"
  )

method_register <- dplyr::bind_rows(
  p15_primary_variant_register() |>
    dplyr::mutate(evidence_family = "observed_primary_issuance"),
  p15_secondary_variant_register() |>
    dplyr::mutate(evidence_family = "observed_secondary_market_evidence")
) |>
  dplyr::mutate(
    final_method_decision = "not_evaluated",
    automatic_consequence = "none"
  )

comparison_summary <- dplyr::bind_rows(
  primary_predecessor_comparison |>
    dplyr::count(
      .data$comparison_state, .data$difference_reason_code,
      name = "country_years"
    ) |>
    dplyr::mutate(comparison_family = "primary_predecessor"),
  secondary_predecessor_comparison |>
    dplyr::count(
      .data$comparison_state, .data$difference_reason_code,
      name = "country_years"
    ) |>
    dplyr::mutate(comparison_family = "secondary_predecessor"),
  primary_anchor_comparison |>
    dplyr::count(
      comparison_state = .data$anchor_comparison_state,
      .data$difference_reason_code,
      name = "country_years"
    ) |>
    dplyr::mutate(comparison_family = "primary_p13_anchor"),
  secondary_anchor_comparison |>
    dplyr::count(
      comparison_state = .data$anchor_comparison_state,
      .data$difference_reason_code,
      name = "country_years"
    ) |>
    dplyr::mutate(comparison_family = "secondary_p13_anchor")
) |>
  dplyr::select(
    "comparison_family", "comparison_state", "difference_reason_code",
    "country_years"
  )

build_summary <- tibble::tibble(
  metric = c(
    "primary_raw_source_rows",
    "primary_economic_issue_rows",
    "primary_candidate_country_year_variant_rows",
    "primary_standard_50m_country_years",
    "secondary_history_rows",
    "secondary_identifier_year_rows",
    "secondary_economic_issue_year_rows",
    "secondary_candidate_country_year_variant_rows",
    "secondary_standard_usd_2_15_country_years",
    "primary_availability_rows",
    "secondary_availability_rows",
    "automatic_consequences_set",
    "final_observed_method_decisions",
    "schema_version"
  ),
  value = c(
    nrow(primary_terms),
    nrow(primary_issues),
    nrow(primary_candidates),
    sum(
      primary_candidates$candidate_variant_id ==
        "primary_standard_usd_eur_50m"
    ),
    nrow(secondary_history),
    nrow(secondary_identifiers),
    nrow(secondary_issues),
    nrow(secondary_candidates),
    sum(
      secondary_candidates$candidate_variant_id ==
        "secondary_standard_usd_2_15_direct"
    ),
    nrow(availability$primary),
    nrow(availability$secondary),
    0L,
    0L,
    p15_observed_all_years_schema_version()
  )
)

output_paths <- list(
  primary_issues = file.path(
    derived_dir, "p15_primary_issue_evidence_2012_2024.csv.gz"
  ),
  primary_candidates = file.path(
    derived_dir, "p15_primary_country_year_candidate_variants_2012_2024.csv.gz"
  ),
  primary_availability = file.path(
    derived_dir, "p15_primary_source_availability_2012_2024.csv.gz"
  ),
  secondary_identifiers = file.path(
    derived_dir, "p15_secondary_identifier_year_evidence_2012_2024.csv.gz"
  ),
  secondary_issues = file.path(
    derived_dir, "p15_secondary_issue_year_evidence_2012_2024.csv.gz"
  ),
  secondary_candidates = file.path(
    derived_dir, "p15_secondary_country_year_candidate_variants_2012_2024.csv.gz"
  ),
  secondary_availability = file.path(
    derived_dir, "p15_secondary_source_availability_2012_2024.csv.gz"
  ),
  primary_predecessor = file.path(
    governance_dir, "p15_primary_predecessor_comparison_2012_2024.csv.gz"
  ),
  secondary_predecessor = file.path(
    governance_dir, "p15_secondary_predecessor_comparison_2012_2023.csv.gz"
  ),
  primary_anchor = file.path(
    governance_dir, "p15_primary_common_candidate_p13_anchor_comparison_2024.csv"
  ),
  secondary_anchor = file.path(
    governance_dir, "p15_secondary_common_candidate_p13_anchor_comparison_2024.csv"
  ),
  method_register = file.path(
    governance_dir, "p15_observed_market_candidate_method_register.csv"
  ),
  comparison_summary = file.path(
    governance_dir, "p15_observed_market_comparison_summary.csv"
  ),
  build_summary = file.path(
    governance_dir, "p15_observed_market_build_summary.csv"
  ),
  manifest = file.path(
    governance_dir, "p15_observed_market_evidence_manifest.csv"
  )
)

objects <- list(
  primary_issues, primary_candidates, availability$primary,
  secondary_identifiers, secondary_issues, secondary_candidates,
  availability$secondary, primary_predecessor_comparison,
  secondary_predecessor_comparison, primary_anchor_comparison,
  secondary_anchor_comparison, method_register, comparison_summary,
  build_summary
)
for (i in seq_along(objects)) {
  data.table::fwrite(
    data.table::as.data.table(objects[[i]]),
    output_paths[[i]],
    na = ""
  )
}

input_paths <- unname(unlist(paths))
generated_paths <- unname(unlist(
  output_paths[names(output_paths) != "manifest"]
))
script_path <- file.path(
  root, "scripts", "p15", "build_p15_observed_all_years_evidence.R"
)
manifest <- data.table::rbindlist(lapply(
  c(input_paths, generated_paths, file.path(root, "R", "p15_observed_all_years.R"),
    script_path),
  function(path) {
    data.table::data.table(
      artifact_path = sub(paste0("^", root, "/"), "", path),
      artifact_role = if (path %in% input_paths) {
        "source_or_parent_input"
      } else {
        "generated_output_or_code"
      },
      bytes = file.info(path)$size,
      sha256 = digest::digest(file = path, algo = "sha256"),
      producing_script = sub(paste0("^", root, "/"), "", script_path),
      producing_script_sha256 = digest::digest(
        file = script_path, algo = "sha256"
      ),
      build_id = "BUILD-P15-OBSERVED-MARKETS-20260721-V1",
      schema_version = p15_observed_all_years_schema_version(),
      build_date = "2026-07-21"
    )
  }
), use.names = TRUE, fill = TRUE)
data.table::fwrite(manifest, output_paths$manifest, na = "")

cat("P15 common observed-market evidence: PASS\n")
cat("Primary issue evidence rows:", nrow(primary_issues), "\n")
cat("Primary candidate variant rows:", nrow(primary_candidates), "\n")
cat("Secondary identifier-year rows:", nrow(secondary_identifiers), "\n")
cat("Secondary issue-year rows:", nrow(secondary_issues), "\n")
cat("Secondary candidate variant rows:", nrow(secondary_candidates), "\n")
cat("Automatic consequences set: 0\n")
cat("Final observed-method decisions: 0\n")
