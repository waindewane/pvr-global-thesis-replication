# Decision evidence for the P15 primary-market materiality rule.

p15_primary_closure_schema <- function() {
  "SCHEMA-P15-PRIMARY-CLOSURE-EVIDENCE-V1"
}

p15_primary_closure_build <- function() {
  "BUILD-P15-PRIMARY-CLOSURE-EVIDENCE-20260814-V1"
}

p15_compare_primary_materiality_variants <- function(
    candidate_variants,
    main_id = "primary_standard_usd_eur_50m",
    sensitivity_id = "primary_standard_usd_eur_all_issue_counts",
    lmic_only = TRUE) {
  required <- c(
    "analysis_year", "iso3", "country", "country_year_id",
    "historical_lmic_reporting_scope", "market_rate_pct", "issue_count",
    "total_weight_value", "candidate_variant_id"
  )
  missing <- setdiff(required, names(candidate_variants))
  if (length(missing)) {
    stop("Primary candidate variants missing columns: ",
         paste(missing, collapse = ", "), call. = FALSE)
  }

  data <- candidate_variants
  if (lmic_only) {
    data <- dplyr::filter(data, .data$historical_lmic_reporting_scope)
  }

  select_variant <- function(id, prefix) {
    data |>
      dplyr::filter(.data$candidate_variant_id == id) |>
      dplyr::select(
        "analysis_year", "iso3", "country", "country_year_id",
        "historical_lmic_reporting_scope",
        !!paste0(prefix, "_rate_pct") := "market_rate_pct",
        !!paste0(prefix, "_issue_count") := "issue_count",
        !!paste0(prefix, "_total_weight_usd") := "total_weight_value"
      )
  }

  main <- select_variant(main_id, "main")
  sensitivity <- select_variant(sensitivity_id, "all_issue")
  detail <- dplyr::full_join(
    main, sensitivity,
    by = c(
      "analysis_year", "iso3", "country", "country_year_id",
      "historical_lmic_reporting_scope"
    )
  ) |>
    dplyr::mutate(
      coverage_state = dplyr::case_when(
        is.finite(.data$main_rate_pct) & is.finite(.data$all_issue_rate_pct) ~
          "present_in_both",
        is.finite(.data$main_rate_pct) ~ "main_only",
        is.finite(.data$all_issue_rate_pct) ~ "all_issue_only",
        TRUE ~ "absent_in_both"
      ),
      rate_difference_pp = .data$all_issue_rate_pct - .data$main_rate_pct,
      absolute_rate_difference_pp = abs(.data$rate_difference_pp),
      main_variant_id = main_id,
      sensitivity_variant_id = sensitivity_id,
      selected_for_ladder = FALSE,
      schema_version = p15_primary_closure_schema(),
      build_id = p15_primary_closure_build()
    )

  overlap <- dplyr::filter(detail, .data$coverage_state == "present_in_both")
  summary <- tibble::tibble(
    reporting_scope = if (lmic_only) "historical_lmic_scope" else "all_countries",
    main_variant_id = main_id,
    sensitivity_variant_id = sensitivity_id,
    main_country_years = nrow(main),
    all_issue_country_years = nrow(sensitivity),
    present_in_both = nrow(overlap),
    main_only = sum(detail$coverage_state == "main_only"),
    all_issue_only = sum(detail$coverage_state == "all_issue_only"),
    changed_over_0_01pp = sum(
      overlap$absolute_rate_difference_pp > 0.01, na.rm = TRUE
    ),
    changed_over_0_05pp = sum(
      overlap$absolute_rate_difference_pp > 0.05, na.rm = TRUE
    ),
    changed_over_0_25pp = sum(
      overlap$absolute_rate_difference_pp > 0.25, na.rm = TRUE
    ),
    maximum_absolute_change_pp = max(
      overlap$absolute_rate_difference_pp, na.rm = TRUE
    ),
    selected_for_ladder = FALSE,
    schema_version = p15_primary_closure_schema(),
    build_id = p15_primary_closure_build()
  )

  list(
    summary = summary,
    coverage_differences = dplyr::filter(
      detail, .data$coverage_state != "present_in_both"
    ),
    rate_differences = dplyr::filter(
      detail,
      .data$coverage_state == "present_in_both",
      .data$absolute_rate_difference_pp > 0
    ) |>
      dplyr::arrange(dplyr::desc(.data$absolute_rate_difference_pp))
  )
}
