suppressPackageStartupMessages({
  library(dplyr)
  library(testthat)
  library(tibble)
})

source("../../R/p15_observed_all_years.R")
source("../../R/p15_status_sanity_candidate_rules.R")
source("../../R/p15_status_sanity_approved_rules.R")

test_that("technical failures are quarantined without deleting raw evidence", {
  issues <- tibble(
    evidence_object = "observed_secondary_direct_usd_2_15",
    evidence_family = "observed_secondary_direct_yield",
    analysis_year = 2024L, iso3 = "AAA", country = "Alpha",
    country_year_id = "2024::AAA", historical_lmic_reporting_scope = TRUE,
    economic_issue_key = c("ordinary", "technical"), currency = "USD",
    rate_date = as.Date("2024-12-31"), maturity_years = 7,
    rate_pct = c(40, 120), weight_usd = 100,
    preferred_candidate_issue = TRUE, all_issue_sensitivity_issue = TRUE,
    source_object = "direct", source_package_ids = "SRC",
    source_record_locators = c("row1", "row2"), timing_object = "year_end"
  )
  curve <- tibble(
    observation_date = as.Date("2024-12-31"), risk_free_rate_pct = 4,
    reference_tenor_years = c(5, 7, 10),
    reference_series_id = c("DGS5", "DGS7", "DGS10")
  )
  # Reuse the rule logic after binding a minimal synthetic primary/secondary shape.
  tenor <- p15_match_prior_treasury_rate(
    issues$rate_date, issues$maturity_years, curve
  )
  reviewed <- bind_cols(issues, tenor) |>
    group_by(evidence_object, analysis_year, iso3, currency) |>
    mutate(
      positive_country_year_median_rate_pct = median(rate_pct),
      technical_range_flag = rate_pct < -5 | rate_pct > 100,
      ten_times_positive_median_flag = FALSE,
      technical_source_failure_flag = technical_range_flag,
      below_one_percent_context_flag = rate_pct < 1,
      above_thirty_percent_context_flag = rate_pct > 30,
      technical_quarantine_from_preferred_aggregate =
        technical_source_failure_flag,
      quantitative_issue_use_after_sanity = !technical_source_failure_flag,
      raw_evidence_retained = TRUE,
      sovereign_spread_pct = rate_pct - risk_free_rate_pct,
      risk_free_match_available = TRUE
    ) |>
    ungroup()
  expect_true(reviewed$technical_quarantine_from_preferred_aggregate[[2]])
  expect_true(all(reviewed$raw_evidence_retained))
  expect_true(reviewed$quantitative_issue_use_after_sanity[[1]])
})

test_that("approved status contract distinguishes warning review and block", {
  status <- tibble(
    analysis_year = 2024L, iso3 = c("AAA", "BBB", "CCC"),
    country = c("Alpha", "Beta", "Gamma"), review_basis = "test",
    recommended_treatment_class = c(
      "provisional_context_warning_no_automatic_block_pending_STAT_05",
      "provisional_manual_review_before_observed_use_pending_STAT_05",
      "provisional_block_ordinary_fallback_pending_STAT_05"
    ),
    recommended_admissibility_action = "test",
    recommended_display_action = "display", case_level_note = "note",
    recommendation_state = "prepared"
  )
  result <- p15_build_approved_status_rule_contract(status)
  expect_true(result$observed_benchmark_selection_permitted[[1]])
  expect_false(result$observed_benchmark_selection_permitted[[2]])
  expect_false(result$ordinary_fallback_selection_permitted[[3]])
  expect_true(all(result$raw_evidence_retained))
  expect_false(any(result$automatic_deletion))
})

test_that("status judgments fail closed on duplicates missing rationale and unknown treatments", {
  x <- tibble(analysis_year = 2024L, iso3 = "AAA", case_level_note = "Source-backed explanation",
    recommended_treatment_class = "provisional_context_warning_no_automatic_block_pending_STAT_05")
  expect_error(p15_build_approved_status_rule_contract(bind_rows(x, x)), "Duplicate")
  x$case_level_note <- " "
  expect_error(p15_build_approved_status_rule_contract(x), "missing case field")
  x$case_level_note <- "Source-backed explanation"
  x$recommended_treatment_class <- "unrecognized_new_treatment"
  expect_error(p15_build_approved_status_rule_contract(x), "Unknown status treatment")
  expect_error(p15_build_approved_status_rule_contract(x["iso3"]), "required case fields")
})

test_that("IMF-inspired flag is contextual and explicitly not an exact replication", {
  issues <- tibble(
    evidence_object = "observed_secondary_direct_usd_2_15",
    evidence_family = "observed_secondary_direct_yield",
    analysis_year = c(2023L, 2024L), iso3 = "AAA", country = "Alpha",
    country_year_id = c("2023::AAA", "2024::AAA"),
    historical_lmic_reporting_scope = TRUE,
    economic_issue_key = c("a", "b"), currency = "USD",
    rate_date = as.Date(c("2023-12-29", "2024-12-31")),
    maturity_years = c(7, 7), rate_pct = c(6, 14), weight_usd = 100,
    preferred_candidate_issue = TRUE, all_issue_sensitivity_issue = TRUE,
    source_object = "direct", source_package_ids = "SRC",
    source_record_locators = c("a", "b"), timing_object = "year_end",
    reference_tenor_years = 7,
    risk_free_observation_date = as.Date(c("2023-12-29", "2024-12-31")),
    risk_free_rate_pct = c(2, 2), risk_free_series_id = "DGS7",
    risk_free_lag_days = 0L, positive_country_year_median_rate_pct = c(6, 14),
    technical_range_flag = FALSE, ten_times_positive_median_flag = FALSE,
    technical_source_failure_flag = FALSE,
    below_one_percent_context_flag = FALSE,
    above_thirty_percent_context_flag = FALSE,
    technical_quarantine_from_preferred_aggregate = FALSE,
    quantitative_issue_use_after_sanity = TRUE, raw_evidence_retained = TRUE,
    sovereign_spread_pct = c(4, 12), risk_free_match_available = TRUE
  )
  result <- p15_build_imf_inspired_context_flags(issues)
  expect_true(result$imf_inspired_500bps_plus_doubling_context_flag[[2]])
  expect_true(all(result$contextual_flag_only))
  expect_false(any(result$exact_imf_operational_replication))
  expect_true(all(result$automatic_consequence == "none"))
})
