# Detailed, non-selecting maturity-window diagnostics for P15 direct secondary YTM.

p15_secondary_maturity_diagnostic_schema <- function() {
  "SCHEMA-P15-SECONDARY-MATURITY-DIAGNOSTIC-V1"
}

p15_secondary_maturity_diagnostic_build <- function() {
  "BUILD-P15-SECONDARY-MATURITY-DIAGNOSTIC-20260814-V1"
}

p15_secondary_maturity_scenarios_detailed <- function() {
  tibble::tribble(
    ~scenario_id, ~min_years, ~max_years, ~scenario_role,
    "usd_2_15_baseline_candidate", 2, 15, "current_candidate_baseline",
    "usd_2_16_near_cutoff", 2, 16, "upper_bound_near_cutoff",
    "usd_2_17_near_cutoff", 2, 17, "upper_bound_near_cutoff",
    "usd_2_20_medium_extension", 2, 20, "upper_bound_extension",
    "usd_2_30_long_extension", 2, 30, "upper_bound_extension",
    "usd_ge2_no_upper_bound", 2, Inf, "upper_bound_removed",
    "usd_1_15_lower_extension", 1, 15, "lower_bound_extension",
    "usd_ge1_inherited_sensitivity", 1, Inf, "both_bounds_relaxed"
  )
}

p15_secondary_maturity_bin <- function(x) {
  cut(
    x,
    breaks = c(-Inf, 1, 2, 15, 16, 17, 20, 30, Inf),
    labels = c(
      "at_or_below_1", "over_1_to_2", "over_2_to_15", "over_15_to_16",
      "over_16_to_17", "over_17_to_20", "over_20_to_30", "over_30"
    ),
    right = TRUE
  ) |>
    as.character()
}

p15_secondary_maturity_eligible_issues <- function(issue_evidence) {
  required <- c(
    "analysis_year", "iso3", "country", "country_year_id", "period",
    "historical_income_level", "historical_lmic_reporting_scope",
    "economic_issue_key", "currency", "remaining_maturity_years",
    "face_outstanding_usd", "direct_yield_pct", "candidate_secondary_standard",
    "identifier_count", "representative_rics", "representative_isins",
    "asset_statuses", "direct_quote_date"
  )
  missing <- setdiff(required, names(issue_evidence))
  if (length(missing)) {
    stop("Secondary issue evidence missing columns: ",
         paste(missing, collapse = ", "), call. = FALSE)
  }
  issue_evidence |>
    dplyr::filter(
      .data$candidate_secondary_standard,
      .data$currency == "USD",
      is.finite(.data$remaining_maturity_years),
      is.finite(.data$direct_yield_pct),
      is.finite(.data$face_outstanding_usd),
      .data$face_outstanding_usd > 0
    ) |>
    dplyr::mutate(
      maturity_bin = p15_secondary_maturity_bin(.data$remaining_maturity_years)
    )
}

p15_build_secondary_maturity_scenario_rates_detailed <- function(issue_evidence) {
  issues <- p15_secondary_maturity_eligible_issues(issue_evidence)
  register <- p15_secondary_maturity_scenarios_detailed()
  dplyr::bind_rows(lapply(seq_len(nrow(register)), function(i) {
    rule <- register[i, ]
    issues |>
      dplyr::filter(
        .data$remaining_maturity_years >= rule$min_years,
        .data$remaining_maturity_years <= rule$max_years
      ) |>
      dplyr::group_by(
        .data$analysis_year, .data$iso3, .data$country,
        .data$country_year_id, .data$period,
        .data$historical_income_level,
        .data$historical_lmic_reporting_scope
      ) |>
      dplyr::summarise(
        market_rate_pct = stats::weighted.mean(
          .data$direct_yield_pct, .data$face_outstanding_usd
        ),
        market_maturity_years = stats::weighted.mean(
          .data$remaining_maturity_years, .data$face_outstanding_usd
        ),
        issue_count = dplyr::n(),
        identifier_count = sum(.data$identifier_count),
        total_weight_usd = sum(.data$face_outstanding_usd),
        minimum_issue_maturity_years = min(.data$remaining_maturity_years),
        maximum_issue_maturity_years = max(.data$remaining_maturity_years),
        minimum_issue_yield_pct = min(.data$direct_yield_pct),
        maximum_issue_yield_pct = max(.data$direct_yield_pct),
        included_issue_keys = paste(
          sort(unique(.data$economic_issue_key)), collapse = ";"
        ),
        .groups = "drop"
      ) |>
      dplyr::mutate(
        scenario_id = rule$scenario_id,
        scenario_role = rule$scenario_role,
        min_years = rule$min_years,
        max_years = rule$max_years,
        selected_for_ladder = FALSE,
        schema_version = p15_secondary_maturity_diagnostic_schema(),
        build_id = p15_secondary_maturity_diagnostic_build()
      )
  }))
}

p15_compare_secondary_maturity_scenarios_detailed <- function(
    scenario_rates,
    baseline_id = "usd_2_15_baseline_candidate") {
  baseline <- scenario_rates |>
    dplyr::filter(.data$scenario_id == baseline_id) |>
    dplyr::select(
      "analysis_year", "iso3",
      baseline_rate_pct = "market_rate_pct",
      baseline_maturity_years = "market_maturity_years",
      baseline_issue_count = "issue_count",
      baseline_total_weight_usd = "total_weight_usd"
    )
  alternative_ids <- setdiff(unique(scenario_rates$scenario_id), baseline_id)
  dplyr::bind_rows(lapply(alternative_ids, function(id) {
    scenario_rates |>
      dplyr::filter(.data$scenario_id == id) |>
      dplyr::full_join(baseline, by = c("analysis_year", "iso3")) |>
      dplyr::mutate(scenario_id = dplyr::coalesce(.data$scenario_id, id))
  })) |>
    dplyr::mutate(
      baseline_scenario_id = baseline_id,
      coverage_state = dplyr::case_when(
        is.finite(.data$market_rate_pct) & is.finite(.data$baseline_rate_pct) ~
          "present_in_both",
        is.finite(.data$market_rate_pct) ~ "scenario_only",
        is.finite(.data$baseline_rate_pct) ~ "baseline_only",
        TRUE ~ "absent_in_both"
      ),
      rate_difference_pp = .data$market_rate_pct - .data$baseline_rate_pct,
      absolute_rate_difference_pp = abs(.data$rate_difference_pp),
      maturity_difference_years =
        .data$market_maturity_years - .data$baseline_maturity_years,
      issue_count_difference = .data$issue_count - .data$baseline_issue_count,
      weight_difference_usd =
        .data$total_weight_usd - .data$baseline_total_weight_usd,
      selected_for_ladder = FALSE,
      schema_version = p15_secondary_maturity_diagnostic_schema(),
      build_id = p15_secondary_maturity_diagnostic_build()
    )
}

p15_build_secondary_maturity_membership_changes <- function(
    issue_evidence, scenario_rates) {
  issues <- p15_secondary_maturity_eligible_issues(issue_evidence)
  register <- p15_secondary_maturity_scenarios_detailed()
  membership <- tidyr::crossing(
    issues,
    register
  ) |>
    dplyr::mutate(
      included_in_scenario =
        .data$remaining_maturity_years >= .data$min_years &
        .data$remaining_maturity_years <= .data$max_years,
      included_in_baseline =
        .data$remaining_maturity_years >= 2 &
        .data$remaining_maturity_years <= 15,
      membership_change = dplyr::case_when(
        .data$included_in_scenario & !.data$included_in_baseline ~ "entered",
        !.data$included_in_scenario & .data$included_in_baseline ~ "left",
        TRUE ~ "unchanged"
      )
    ) |>
    dplyr::filter(
      .data$scenario_id != "usd_2_15_baseline_candidate",
      .data$membership_change != "unchanged"
    )

  weights <- scenario_rates |>
    dplyr::select(
      "analysis_year", "iso3", "scenario_id", "market_rate_pct",
      "total_weight_usd", "issue_count"
    )
  membership |>
    dplyr::left_join(weights, by = c("analysis_year", "iso3", "scenario_id")) |>
    dplyr::mutate(
      issue_weight_share_in_scenario = dplyr::if_else(
        .data$included_in_scenario & .data$total_weight_usd > 0,
        .data$face_outstanding_usd / .data$total_weight_usd,
        NA_real_
      ),
      issue_weighted_yield_contribution_pp = dplyr::if_else(
        is.finite(.data$issue_weight_share_in_scenario),
        .data$issue_weight_share_in_scenario * .data$direct_yield_pct,
        NA_real_
      ),
      selected_for_ladder = FALSE,
      schema_version = p15_secondary_maturity_diagnostic_schema(),
      build_id = p15_secondary_maturity_diagnostic_build()
    )
}

p15_summarise_secondary_maturity_diagnostic <- function(
    scenario_rates, comparisons, membership_changes, status_context = NULL) {
  register <- p15_secondary_maturity_scenarios_detailed()
  summarise_scope <- function(scope_id) {
    rates_scope <- if (scope_id == "historical_lmic_scope") {
      scenario_rates |>
        dplyr::filter(.data$historical_lmic_reporting_scope)
    } else scenario_rates
    comparisons_scope <- if (scope_id == "historical_lmic_scope") {
      comparisons |>
        dplyr::filter(.data$historical_lmic_reporting_scope %in% TRUE)
    } else comparisons
    membership_scope <- if (scope_id == "historical_lmic_scope") {
      membership_changes |>
        dplyr::filter(.data$historical_lmic_reporting_scope)
    } else membership_changes

    coverage <- rates_scope |>
      dplyr::group_by(.data$scenario_id, .data$scenario_role) |>
      dplyr::summarise(
        country_years = dplyr::n(),
        countries = dplyr::n_distinct(.data$iso3),
        .groups = "drop"
      )
    comparison_summary <- comparisons_scope |>
      dplyr::group_by(.data$scenario_id) |>
      dplyr::summarise(
        present_in_both = sum(.data$coverage_state == "present_in_both"),
        scenario_only = sum(.data$coverage_state == "scenario_only"),
        baseline_only = sum(.data$coverage_state == "baseline_only"),
        changed_over_0_25pp = sum(
          .data$absolute_rate_difference_pp > 0.25, na.rm = TRUE
        ),
        changed_over_0_5pp = sum(
          .data$absolute_rate_difference_pp > 0.5, na.rm = TRUE
        ),
        changed_over_1pp = sum(
          .data$absolute_rate_difference_pp > 1, na.rm = TRUE
        ),
        median_absolute_change_pp = stats::median(
          .data$absolute_rate_difference_pp, na.rm = TRUE
        ),
        p95_absolute_change_pp = as.numeric(stats::quantile(
          .data$absolute_rate_difference_pp, 0.95, na.rm = TRUE
        )),
        maximum_absolute_change_pp = max(
          .data$absolute_rate_difference_pp, na.rm = TRUE
        ),
        .groups = "drop"
      )
    membership_summary <- membership_scope |>
      dplyr::group_by(.data$scenario_id) |>
      dplyr::summarise(
        entering_issues = sum(.data$membership_change == "entered"),
        leaving_issues = sum(.data$membership_change == "left"),
        entering_country_years = dplyr::n_distinct(
          paste(.data$analysis_year[.data$membership_change == "entered"],
                .data$iso3[.data$membership_change == "entered"])
        ),
        .groups = "drop"
      )
    register |>
      dplyr::left_join(coverage, by = c("scenario_id", "scenario_role")) |>
      dplyr::left_join(comparison_summary, by = "scenario_id") |>
      dplyr::left_join(membership_summary, by = "scenario_id") |>
      dplyr::mutate(analysis_scope = scope_id, .before = 1)
  }
  overall <- dplyr::bind_rows(
    summarise_scope("all_country_years"),
    summarise_scope("historical_lmic_scope")
  ) |>
    dplyr::mutate(
      diagnostic_state = "evidence_complete_decision_not_made",
      downstream_ladder_pvr_state =
        "pending_final_ladder_and_pvr_pipeline",
      schema_version = p15_secondary_maturity_diagnostic_schema(),
      build_id = p15_secondary_maturity_diagnostic_build()
    )

  by_bin <- membership_changes |>
    dplyr::count(
      .data$scenario_id, .data$membership_change, .data$maturity_bin,
      wt = .data$face_outstanding_usd,
      name = "entering_or_leaving_weight_usd"
    ) |>
    dplyr::left_join(
      membership_changes |>
        dplyr::count(
          .data$scenario_id, .data$membership_change, .data$maturity_bin,
          name = "issue_rows"
        ),
      by = c("scenario_id", "membership_change", "maturity_bin")
    )

  changed_cases <- comparisons |>
    dplyr::filter(
      .data$coverage_state != "present_in_both" |
        .data$absolute_rate_difference_pp > 0
    )
  if (!is.null(status_context)) {
    status_keep <- status_context |>
      dplyr::select(
        "analysis_year", "iso3", "expanded_status_context_trigger_present",
        "expanded_status_evidence_ids",
        "ucdp_any_organized_violence_context", "bftu_context_state"
      ) |>
      dplyr::distinct(.data$analysis_year, .data$iso3, .keep_all = TRUE)
    changed_cases <- changed_cases |>
      dplyr::left_join(status_keep, by = c("analysis_year", "iso3"))
  }
  list(overall = overall, by_bin = by_bin, changed_cases = changed_cases)
}
