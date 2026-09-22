# P15 rate-sanity evidence helpers.
#
# Thresholds in this file are controlled comparison variants. None is an
# approved admissibility rule until SANE-04 records the research decision.

p15_rate_sanity_schema_version <- function() {
  "SCHEMA-P15-RATE-SANITY-EVIDENCE-V1"
}

p15_rate_sanity_parameters <- function() {
  universal <- tibble::tribble(
    ~sanity_variant_id, ~variant_label, ~source_family_class, ~lower_bound_pct, ~upper_bound_pct, ~variant_role,
    "baseline_1_30", "Current legacy screen: 1 to 30 percent", "all", 1, 30, "legacy_baseline",
    "zero_30", "Allow zero through 30 percent", "all", 0, 30, "controlled_comparison",
    "zero_40", "Allow zero through 40 percent", "all", 0, 40, "controlled_comparison",
    "negative_1_to_30", "Allow rates down to minus 1 percent", "all", -1, 30, "controlled_comparison",
    "hyperinflation_1_100", "Allow crisis rates through 100 percent", "all", 1, 100, "controlled_comparison"
  )
  source_specific <- tibble::tribble(
    ~sanity_variant_id, ~variant_label, ~source_family_class, ~lower_bound_pct, ~upper_bound_pct, ~variant_role,
    "source_specific", "Source-object-specific comparison", "observed_primary", -1, 40, "controlled_comparison",
    "source_specific", "Source-object-specific comparison", "observed_secondary", -1, 40, "controlled_comparison",
    "source_specific", "Source-object-specific comparison", "ids_terms", 0, 30, "controlled_comparison",
    "source_specific", "Source-object-specific comparison", "rating_model", 0, 40, "controlled_comparison",
    "source_specific", "Source-object-specific comparison", "peer_proxy", 0, 40, "controlled_comparison",
    "source_specific", "Source-object-specific comparison", "policy_comparator", 0, 100, "controlled_comparison",
    "source_specific", "Source-object-specific comparison", "other_model_or_diagnostic", 0, 40, "controlled_comparison"
  )
  dplyr::bind_rows(universal, source_specific) |>
    dplyr::mutate(
      lower_bound_inclusive = TRUE,
      upper_bound_inclusive = TRUE,
      consequence_decision_state = "not_evaluated",
      parameter_schema_version = p15_rate_sanity_schema_version()
    )
}

p15_classify_rate_source_family <- function(evidence_family, source_object = "") {
  evidence_family <- tolower(dplyr::coalesce(as.character(evidence_family), ""))
  source_object <- tolower(dplyr::coalesce(as.character(source_object), ""))
  dplyr::case_when(
    grepl("primary", evidence_family) | grepl("primary", source_object) ~
      "observed_primary",
    grepl("secondary", evidence_family) | grepl("secondary", source_object) ~
      "observed_secondary",
    grepl("ids", evidence_family) | grepl("bondholder", source_object) ~
      "ids_terms",
    grepl("rating", evidence_family) | grepl("rating", source_object) ~
      "rating_model",
    grepl("peer", evidence_family) | grepl("peer", source_object) ~
      "peer_proxy",
    grepl("policy", evidence_family) | grepl("policy", source_object) ~
      "policy_comparator",
    TRUE ~ "other_model_or_diagnostic"
  )
}

p15_validate_rate_sanity_universe <- function(rate_universe) {
  required <- c(
    "rate_evidence_id", "analysis_year", "iso3", "country",
    "evidence_family", "source_object", "rate_pct"
  )
  missing <- setdiff(required, names(rate_universe))
  if (length(missing)) {
    stop(
      "Rate-sanity universe is missing: ", paste(missing, collapse = ", "),
      call. = FALSE
    )
  }
  if (anyDuplicated(rate_universe$rate_evidence_id)) {
    stop("Rate-sanity universe contains duplicate evidence IDs.", call. = FALSE)
  }
  if (any(!is.finite(rate_universe$rate_pct))) {
    stop("Rate-sanity universe contains missing or non-finite rates.", call. = FALSE)
  }
  invisible(TRUE)
}

p15_apply_rate_sanity_variants <- function(
    rate_universe, parameters = p15_rate_sanity_parameters()) {
  p15_validate_rate_sanity_universe(rate_universe)
  universe <- rate_universe |>
    dplyr::mutate(
      source_family_class = p15_classify_rate_source_family(
        .data$evidence_family,
        .data$source_object
      )
    )
  universal <- parameters |>
    dplyr::filter(.data$source_family_class == "all")
  source_specific <- parameters |>
    dplyr::filter(.data$source_family_class != "all")

  universal_results <- tidyr::crossing(
    universe,
    universal |>
      dplyr::select(-"source_family_class")
  )
  source_specific_results <- universe |>
    dplyr::inner_join(source_specific, by = "source_family_class")

  out <- dplyr::bind_rows(universal_results, source_specific_results) |>
    dplyr::mutate(
      sanity_state = dplyr::case_when(
        .data$rate_pct < .data$lower_bound_pct ~ "below_lower_bound",
        .data$rate_pct > .data$upper_bound_pct ~ "above_upper_bound",
        TRUE ~ "within_threshold"
      ),
      threshold_pass = .data$sanity_state == "within_threshold",
      distance_below_lower_pp = pmax(
        .data$lower_bound_pct - .data$rate_pct,
        0
      ),
      distance_above_upper_pp = pmax(
        .data$rate_pct - .data$upper_bound_pct,
        0
      ),
      sanity_decision_state = "not_evaluated",
      rate_sanity_schema_version = p15_rate_sanity_schema_version()
    ) |>
    dplyr::arrange(
      .data$rate_evidence_id,
      .data$sanity_variant_id
    )

  expected <- nrow(universe) *
    (dplyr::n_distinct(universal$sanity_variant_id) + 1L)
  stopifnot(
    nrow(out) == expected,
    !anyDuplicated(out[c("rate_evidence_id", "sanity_variant_id")]),
    all(out$sanity_decision_state == "not_evaluated")
  )
  out
}

p15_rate_sanity_case_explanation <- function(
    source_family_class, rate_pct, analysis_year) {
  dplyr::case_when(
    source_family_class == "ids_terms" & rate_pct < 1 ~
      "IDS average commitment terms can legitimately be below 1 percent; this is a source-object distinction, not automatically a data error.",
    source_family_class %in% c("observed_primary", "observed_secondary") &
      rate_pct < 0 & rate_pct >= -1 & analysis_year <= 2021 ~
      "A slightly negative observed yield can be economically possible in the low-rate period, but the field, currency, and instrument still require confirmation.",
    source_family_class %in% c("observed_primary", "observed_secondary") &
      rate_pct >= 0 & rate_pct < 1 ~
      "A very low observed yield may be genuine; review currency, timing, maturity, and source-field semantics before excluding it.",
    source_family_class == "observed_secondary" & rate_pct > 100 ~
      "The extreme secondary value is more consistent with a field, maturity, price, or distressed-instrument problem than with ordinary benchmark evidence.",
    source_family_class %in% c("observed_primary", "observed_secondary") &
      rate_pct > 30 ~
      "This may be genuine crisis pricing or a source/aggregation problem; inspect the underlying instruments and status context.",
    source_family_class %in% c("rating_model", "peer_proxy") & rate_pct > 30 ~
      "The model or proxy tail is extreme and should be traced to its spread, pool, or source inputs rather than treated as observed pricing.",
    source_family_class %in% c("rating_model", "peer_proxy") & rate_pct < 1 ~
      "The model or proxy produces a very low positive rate; inspect the risk-free component, spread mapping, or peer pool.",
    rate_pct < 1 ~ "The rate is below the legacy lower bound and requires source-specific interpretation.",
    rate_pct > 30 ~ "The rate is above the legacy upper bound and requires source-specific interpretation.",
    TRUE ~ "No legacy threshold failure."
  )
}
