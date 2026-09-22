# Local assembly and acceptance evidence. Upstream estimators are unchanged.

p15_local_input_paths <- function() {
  c(
    grid = "data-derived/p15_platform_2012_2024_v1/p15_country_year_grid_2012_2024.csv",
    anchors = "data-derived/p15_integrated_observed_candidate_2012_2024_v1/p15_integrated_observed_validation_anchors_2012_2024.csv.gz",
    observed = "data-derived/p15_integrated_observed_candidate_2012_2024_v1/p15_integrated_observed_evidence_catalogue_2012_2024.csv.gz",
    ids = "data-derived/p15_ids_evidence_2012_2024_v1/p15_ids_bondholders_country_year_evidence_2012_2024.csv.gz",
    ratings = "data-derived/p15_rating_validation_2012_2024_v1/p15_rating_candidate_variants_2012_2024.csv.gz",
    rating_context = "data-derived/p15_rating_components_2012_2024_v1/p15_rating_component_ledger_2012_2024.csv.gz",
    status = "data-derived/p15_status_sanity_approved_2012_2024_v1/p15_approved_status_rule_contract_2012_2024.csv.gz",
    peer_comparison = "data-derived/p15_bounded_fallback_comparison_2012_2024_v1/p15_peer_bounded_comparison_detail.csv.gz",
    regression = "data-derived/p15_integrated_observed_candidate_2012_2024_v1/p15_integrated_observed_p13_regression_2024.csv",
    upstream_manifest = "docs/governance/p15_integrated_observed_manifest.csv"
  )
}

p15_local_assert_keys <- function(x, keys, label) {
  if (!all(keys %in% names(x))) stop(label, ": missing key columns", call. = FALSE)
  bad <- vapply(x[keys], function(v) any(is.na(v) | trimws(as.character(v)) == ""), logical(1))
  if (any(bad) || anyDuplicated(x[keys])) {
    stop(label, ": missing or duplicate keys", call. = FALSE)
  }
  invisible(TRUE)
}

p15_local_assert_grid_members <- function(x, grid, label) {
  unknown <- dplyr::anti_join(x, grid, by = c("analysis_year", "iso3"))
  if (nrow(unknown)) stop(label, ": country-year outside master grid", call. = FALSE)
}

# Grid classifications override incomplete inherited export columns, but a
# populated disagreement must be investigated rather than silently overwritten.
p15_local_classify <- function(x, grid) {
  keys <- c("analysis_year", "iso3")
  metadata <- c("country", "country_year_id", "historical_income_level",
                "historical_lmic_reporting_scope")
  p15_local_assert_keys(grid, keys, "grid")
  p15_local_assert_grid_members(x, grid, "evidence")
  present <- intersect(metadata, names(x))
  checked <- dplyr::left_join(x, grid[c(keys, metadata)], by = keys,
                              suffix = c("", "_grid"))
  for (nm in intersect(present, c("historical_income_level", "historical_lmic_reporting_scope"))) {
    old <- as.character(checked[[nm]])
    new <- as.character(checked[[paste0(nm, "_grid")]])
    if (any(!is.na(old) & nzchar(old) & !is.na(new) & old != new)) {
      stop("Historical classification conflict: ", nm, call. = FALSE)
    }
  }
  dplyr::left_join(dplyr::select(x, -dplyr::any_of(metadata)),
                   grid[c(keys, metadata)], by = keys)
}

p15_local_ids_fields <- function(ids) {
  ids |>
    dplyr::transmute(
      analysis_year, iso3,
      ids_rate_pct = official_rate_pct,
      ids_maturity_years = official_maturity_years,
      ids_grace_years = official_grace_years,
      ids_rate_observed = is.finite(official_rate_pct),
      ids_positive_rate_observed = is.finite(official_rate_pct) & official_rate_pct > 0,
      ids_below_1_review = is.finite(official_rate_pct) & official_rate_pct < 1,
      ids_above_30_review = is.finite(official_rate_pct) & official_rate_pct > 30,
      ids_zero_rate_review = is.finite(official_rate_pct) & official_rate_pct == 0,
      ids_term_order_valid = is.finite(official_maturity_years) &
        is.finite(official_grace_years) & official_maturity_years > 0 &
        official_grace_years >= 0 & official_grace_years <= official_maturity_years,
      ids_legacy_term_validity_state = bondholder_term_validity_state,
      ids_legacy_low_rate_rule_conflict = is.finite(official_rate_pct) &
        official_rate_pct > 0 & official_rate_pct < 1,
      ids_missingness_state,
      ids_currency_basis = "aggregate_currency_composition_unknown",
      ids_evidence_id, ids_source_package_id,
      ids_term_validation_state = "separate_term_validation_required_before_PVR"
    )
}

p15_build_local_panel <- function(inputs) {
  grid <- inputs$grid
  keys <- c("analysis_year", "iso3")
  p15_local_assert_keys(grid, keys, "grid")
  for (nm in c("ids", "rating_context", "status")) {
    p15_local_assert_keys(inputs[[nm]], keys, nm)
    p15_local_assert_grid_members(inputs[[nm]], grid, nm)
  }
  for (nm in c("ids", "rating_context")) {
    if (nrow(inputs[[nm]]) != nrow(grid)) stop(nm, ": incomplete grid", call. = FALSE)
  }
  anchors <- p15_local_classify(inputs$anchors, grid)
  p15_local_assert_keys(anchors, c(keys, "observed_market_branch", "currency"), "anchors")
  p15_local_assert_keys(inputs$observed, "source_evidence_row_id", "observed evidence")
  p15_local_assert_grid_members(inputs$observed, grid, "observed evidence")
  if (any(!is.finite(anchors$market_rate_pct)) ||
      any(!anchors$candidate_for_validation_anchor) ||
      any(is.na(anchors$source_package_ids) | anchors$source_package_ids == "")) {
    stop("Anchor rate, eligibility, or lineage failure", call. = FALSE)
  }
  panel <- grid
  for (branch in c("primary", "secondary")) {
    for (currency in c("USD", "EUR")) {
      prefix <- paste(branch, tolower(currency), sep = "_")
      a <- anchors[anchors$observed_market_branch == paste0("observed_", branch) &
                     anchors$currency == currency, ]
      fields <- c("market_rate_pct", "market_maturity_years", "retained_quantitative_issue_count",
                  "total_weight_usd", "thin_evidence", "largest_issue_weight_share",
                  "first_rate_date", "last_rate_date", "candidate_evidence_tier",
                  "timing_object", "source_evidence_row_id", "source_package_ids",
                  "included_issue_keys", "low_rate_context_present", "high_rate_context_present",
                  "imf_inspired_500bps_plus_doubling_context_flag")
      a <- a[c(keys, fields)]
      names(a)[match(fields, names(a))] <- paste0(prefix, "_", fields)
      panel <- dplyr::left_join(panel, a, by = keys)
      catalogue <- inputs$observed |>
        dplyr::filter(observed_market_branch == paste0("observed_", branch),
                      .data$currency == .env$currency) |>
        dplyr::distinct(analysis_year, iso3)
      candidate_present <- is.finite(panel[[paste0(prefix, "_market_rate_pct")]])
      evidence_present <- paste(panel$analysis_year, panel$iso3) %in%
        paste(catalogue$analysis_year, catalogue$iso3)
      panel[[paste0(prefix, "_availability")]] <- ifelse(candidate_present,
        "ordinary_observed_candidate_available", ifelse(evidence_present,
        "evidence_retained_without_ordinary_anchor", "no_candidate_in_current_archive"))
    }
  }
  panel <- dplyr::left_join(panel, p15_local_ids_fields(inputs$ids), by = keys)
  rating_ids <- c(
    moodys = "boy_moodys_only_rating_group_median_dgs7",
    fitch = "boy_fitch_only_rating_group_median_dgs7",
    precedence = "boy_selected_precedence_rating_group_median_dgs7",
    agency_median = "boy_available_agencies_median_mapped_spread_dgs7"
  )
  for (method in names(rating_ids)) {
    r <- inputs$ratings[inputs$ratings$variant_id == rating_ids[[method]], ]
    p15_local_assert_keys(r, keys, paste("rating", method))
    p15_local_assert_grid_members(r, grid, method)
    if (nrow(r) != nrow(grid)) stop("Incomplete rating variant: ", method, call. = FALSE)
    r <- r[c(keys, "variant_rate_pct", "variant_available", "variant_uncomputed_reason",
               "variant_rating_normalized", "variant_id")]
    names(r)[-(1:2)] <- paste0("rating_", method, "_",
                              c("rate_pct", "available", "missing_reason", "rating", "variant_id"))
    panel <- dplyr::left_join(panel, r, by = keys)
  }
  status <- inputs$status[c(keys, "approved_status_rule_class",
    "observed_benchmark_selection_permitted", "ordinary_fallback_selection_permitted",
    "case_level_note", "expanded_positive_evidence_source_package_ids")]
  names(status)[names(status) == "case_level_note"] <- "status_case_note"
  panel <- dplyr::left_join(panel, status, by = keys) |>
    dplyr::mutate(
      status_case_review_present = !is.na(approved_status_rule_class),
      status_review_coverage = dplyr::if_else(status_case_review_present,
        "case_review_record_available", "no_case_review_record_not_proof_of_no_stress"),
      any_usd_observed_candidate = is.finite(primary_usd_market_rate_pct) |
        is.finite(secondary_usd_market_rate_pct),
      any_eur_observed_candidate = is.finite(primary_eur_market_rate_pct) |
        is.finite(secondary_eur_market_rate_pct),
      usd_primary_secondary_gap_pp = primary_usd_market_rate_pct - secondary_usd_market_rate_pct,
      comparison_timing_warning = dplyr::if_else(is.finite(usd_primary_secondary_gap_pp),
        "annual_issue_flow_versus_year_end_stock_not_contemporaneous", NA_character_),
      fallback_audit_cohort = dplyr::case_when(
        is.finite(primary_usd_market_rate_pct) ~ "usd_primary_observed",
        any_usd_observed_candidate | ids_positive_rate_observed ~ "other_observed_or_positive_ids",
        TRUE ~ "no_usd_observed_or_positive_ids"
      ),
      selected_for_ladder = FALSE, canonical_benchmark = FALSE,
      estimator_id = "EST-P15-LOCAL-ASSEMBLY-V1",
      admissibility_id = "ADM-P15-INHERITED-CANDIDATE-STATES-V1",
      selection_id = "SEL-NONE-EVIDENCE-ASSEMBLY-V1",
      schema_id = "SCHEMA-P15-LOCAL-DATASET-V1",
      build_id = "BUILD-P15-LOCAL-COMPLETION-20260906-V1"
    ) |>
    dplyr::arrange(analysis_year, iso3)
  list(panel = panel, anchors = anchors)
}

# Apply the already compared similarity specification to the whole target grid.
# This is an applicability audit, not evidence of accuracy on unobserved targets.
p15_audit_local_peer_targets <- function(panel, anchors, context, minimum_count = 3L) {
  keys <- c("analysis_year", "iso3")
  rating_cols <- c(keys, "moodys_rating_normalized", "rating_source_region")
  p15_local_assert_keys(context, keys, "peer context")
  seeds <- anchors |>
    dplyr::filter(observed_market_branch == "observed_primary", currency == "USD",
                  is.finite(market_rate_pct), is.finite(sovereign_spread_pct)) |>
    dplyr::left_join(context[rating_cols], by = keys)
  targets <- dplyr::left_join(panel, context[rating_cols], by = keys)
  details <- vector("list", nrow(targets))
  members <- vector("list", nrow(targets))
  for (i in seq_len(nrow(targets))) {
    target <- targets[i, ]
    eligible <- seeds[seeds$analysis_year == target$analysis_year & seeds$iso3 != target$iso3, ]
    pick <- p15_peer_similarity_pool(eligible, target, minimum_count)
    pool <- pick$pool
    values <- pool$market_rate_pct
    notch_gap <- abs(p15_rating_notch_number(pool$moodys_rating_normalized) -
                       p15_rating_notch_number(target$moodys_rating_normalized))
    enough <- nrow(pool) >= minimum_count
    details[[i]] <- tibble::tibble(
      analysis_year = target$analysis_year, iso3 = target$iso3,
      historical_lmic_reporting_scope = target$historical_lmic_reporting_scope,
      fallback_audit_cohort = target$fallback_audit_cohort,
      target_moodys_missing = is.na(p15_rating_notch_number(target$moodys_rating_normalized)),
      peer_pool_rule = pick$rule, peer_country_count = nrow(pool),
      peer_rate_pct = if (length(values)) stats::median(values) else NA_real_,
      peer_minimum_met = enough,
      peer_iqr_pp = if (length(values)) diff(stats::quantile(values, c(.25, .75), names = FALSE)) else NA_real_,
      peer_min_rate_pct = if (length(values)) min(values) else NA_real_,
      peer_max_rate_pct = if (length(values)) max(values) else NA_real_,
      rating_proximity_used = grepl("rating3", pick$rule),
      global_pool_used = grepl("^global", pick$rule),
      thin_primary_peer_share = if (length(values)) mean(pool$thin_evidence) else NA_real_,
      largest_rating_gap_notches = if (any(is.finite(notch_gap))) max(notch_gap, na.rm = TRUE) else NA_real_,
      target_fallback_permission_record = target$ordinary_fallback_selection_permitted,
      peer_candidate_state = if (enough) "diagnostic_formula_pending_acceptance" else "insufficient_peers",
      source_evidence_row_id = paste("P15-PEER-TARGET", target$analysis_year, target$iso3, sep = "::"),
      source_package_ids = paste(sort(unique(unlist(strsplit(pool$source_package_ids, ";", fixed = TRUE)))), collapse = ";"),
      selected_for_ladder = FALSE
    )
    if (nrow(pool)) members[[i]] <- tibble::tibble(
      analysis_year = target$analysis_year, target_iso3 = target$iso3,
      peer_iso3 = pool$iso3, peer_rate_pct = values,
      peer_source_evidence_row_id = pool$source_evidence_row_id,
      peer_source_package_ids = pool$source_package_ids,
      peer_included_issue_keys = pool$included_issue_keys,
      peer_pool_rule = pick$rule
    )
  }
  detail <- dplyr::bind_rows(details)
  membership <- dplyr::bind_rows(members)
  if (nrow(membership) && any(membership$target_iso3 == membership$peer_iso3)) {
    stop("Peer self-inclusion", call. = FALSE)
  }
  p15_local_assert_keys(detail, keys, "peer targets")
  list(detail = detail, membership = membership)
}

p15_local_peer_summary <- function(detail) {
  detail |>
    dplyr::group_by(historical_lmic_reporting_scope, fallback_audit_cohort) |>
    dplyr::summarise(
      country_years = dplyr::n(),
      countries = dplyr::n_distinct(iso3),
      moodys_missing = sum(target_moodys_missing),
      rating_proximity_used = sum(rating_proximity_used),
      global_pool_used = sum(global_pool_used),
      fewer_than_three_peers = sum(!peer_minimum_met),
      median_peer_count = stats::median(peer_country_count),
      median_peer_iqr_pp = stats::median(peer_iqr_pp, na.rm = TRUE),
      mean_thin_peer_share = mean(thin_primary_peer_share, na.rm = TRUE),
      explicit_status_blocks = sum(target_fallback_permission_record %in% FALSE),
      .groups = "drop"
    )
}

p15_local_coverage <- function(panel) {
  panel |>
    dplyr::group_by(analysis_year, historical_income_level, historical_lmic_reporting_scope) |>
    dplyr::summarise(
      country_years = dplyr::n(),
      primary_usd = sum(is.finite(primary_usd_market_rate_pct)),
      secondary_usd = sum(is.finite(secondary_usd_market_rate_pct)),
      primary_eur = sum(is.finite(primary_eur_market_rate_pct)),
      secondary_eur = sum(is.finite(secondary_eur_market_rate_pct)),
      either_usd_observed = sum(any_usd_observed_candidate),
      eur_additional = sum(!any_usd_observed_candidate & any_eur_observed_candidate),
      ids_positive = sum(ids_positive_rate_observed),
      ids_additional_without_usd_observed = sum(ids_positive_rate_observed & !any_usd_observed_candidate),
      moodys_available = sum(rating_moodys_available %in% TRUE),
      neither_usd_observed_nor_positive_ids = sum(fallback_audit_cohort == "no_usd_observed_or_positive_ids"),
      .groups = "drop"
    )
}
