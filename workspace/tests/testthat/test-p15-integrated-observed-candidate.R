suppressPackageStartupMessages({
  library(dplyr)
  library(testthat)
  library(tibble)
})

source("../../R/p15_observed_all_years.R")
source("../../R/p15_status_sanity_candidate_rules.R")
source("../../R/p15_status_sanity_approved_rules.R")
source("../../R/p15_integrated_observed_candidate.R")

test_that("integrated register never creates a mixed USD/EUR nominal rate", {
  register <- p15_integrated_evidence_register()
  expect_setequal(
    register$evidence_object,
    c(
      "observed_primary_forward_usd", "observed_primary_forward_eur",
      "observed_secondary_direct_usd_2_15",
      "observed_secondary_direct_eur_2_15",
      "observed_secondary_repair_usd_2_15",
      "observed_secondary_repair_eur_2_15"
    )
  )
  expect_false(any(grepl("usd_eur", register$evidence_object)))
})

test_that("secondary core uses direct evidence before strict repair", {
  evidence <- tibble(
    analysis_year = c(2024L, 2024L, 2024L),
    iso3 = c("AAA", "AAA", "BBB"),
    currency = "USD",
    observed_market_branch = "observed_secondary",
    candidate_for_validation_anchor = TRUE,
    evidence_priority_within_branch_currency = c(2L, 1L, 2L),
    candidate_variant_id = c("repair", "direct", "repair"),
    candidate_evidence_tier = c(
      "price_derived_secondary", "direct_yield_to_maturity",
      "price_derived_secondary"
    ),
    selected_for_ladder = FALSE,
    canonical_benchmark = FALSE
  )
  anchors <- p15_resolve_terminal_independent_validation_anchors(evidence)
  expect_equal(anchors$candidate_variant_id[anchors$iso3 == "AAA"], "direct")
  expect_equal(anchors$candidate_variant_id[anchors$iso3 == "BBB"], "repair")
  expect_false(any(anchors$selected_for_ladder))
  expect_false(any(anchors$canonical_benchmark))
})

test_that("repair sanity repeats approved raw-retention and context rules", {
  repair <- tibble(
    analysis_year = c(2023L, 2023L),
    iso3 = c("AAA", "AAA"), country = c("Alpha", "Alpha"),
    country_year_id = c("2023::AAA", "2023::AAA"), period = "2023",
    historical_lmic_reporting_scope = TRUE,
    repair_issue_key = c("r1", "r2"), currency = "USD",
    selected_quote_date = as.Date(c("2023-12-29", "2023-12-29")),
    remaining_maturity_years = c(7, 7),
    repaired_yield_pct = c(0.5, 35),
    face_outstanding_usd = c(100, 100),
    strict_candidate_admitted = TRUE,
    source_package_ids = "SRC",
    source_record_locators = c("row:1", "row:2")
  )
  treasury <- tibble(
    observation_date = as.Date(rep("2023-12-29", 3)),
    risk_free_rate_pct = c(4, 4.1, 4.2),
    reference_tenor_years = c(5, 7, 10),
    reference_series_id = c("DGS5", "DGS7", "DGS10")
  )
  result <- p15_build_repair_issue_sanity(repair, treasury)
  expect_true(all(result$raw_evidence_retained))
  expect_true(result$below_one_percent_context_flag[result$economic_issue_key == "r1"])
  expect_true(result$above_thirty_percent_context_flag[result$economic_issue_key == "r2"])
  expect_false(any(result$technical_source_failure_flag))
  expect_false(any(result$automatic_pvr_consequence))
})

test_that("2024 regression preserves P13 parity and reports forward differences", {
  anchors <- tibble(
    analysis_year = c(2024L, 2024L), iso3 = c("AAA", "BBB"),
    observed_market_branch = c("observed_primary", "observed_secondary"),
    currency = "USD", market_rate_pct = c(5, 8),
    market_maturity_years = c(10, 7),
    candidate_evidence_tier = c(
      "direct_original_issue_yield", "direct_yield_to_maturity"
    ),
    candidate_variant_id = c("primary", "secondary"),
    within_branch_resolution = c("primary_direct", "secondary_direct"),
    approved_status_rule_class = "no_registered_status_case",
    candidate_use_state = "ordinary_validation_anchor_candidate",
    source_evidence_row_id = c("row-a", "row-b")
  )
  parity <- tibble(
    analysis_year = c(2024L, 2024L, 2024L),
    iso3 = c("AAA", "BBB", "CCC"),
    country = c("Alpha", "Beta", "Gamma"),
    observed_market_branch = c(
      "observed_primary", "observed_secondary", "observed_secondary"
    ),
    p13_rate_pct = c(5, 7.5, 9),
    p13_maturity_years = c(10, 7, 6),
    p13_source_class = c(
      "lseg_primary", "lseg_secondary", "p12a_feature_rich_vendor_yield_review"
    ),
    p13_tier_id = c("primary", "secondary", "secondary_feature"),
    p13_currency_basis = c("USD", "USD", "USD"),
    p13_mixed_currency_basis = FALSE,
    common_feature_evidence_present = c(FALSE, FALSE, TRUE),
    observed_market_parity_gate_pass = TRUE
  )
  result <- p15_build_integrated_2024_regression(anchors, parity)
  expect_true(all(result$legacy_p13_parity_still_passes))
  expect_equal(
    result$forward_regression_class[result$iso3 == "AAA"],
    "exact_forward_rate_and_maturity_match"
  )
  expect_equal(
    result$forward_regression_class[result$iso3 == "BBB"],
    "forward_secondary_quote_vintage_or_issue_scope_difference"
  )
  expect_equal(
    result$forward_regression_class[result$iso3 == "CCC"],
    "forward_feature_rich_secondary_pending_TODO_036"
  )
})

test_that("targeted companion fills source gaps but not feature-rich blocks", {
  standard <- tibble(
    analysis_year = c(2024L, 2024L), iso3 = c("AAA", "BBB"),
    country = c("Alpha", "Beta"), market_rate_pct = c(6, 7),
    market_maturity_years = c(5, 6), issue_count = 1L,
    total_weight_usd = 100, included_issue_keys = c("a", "b"),
    currency_basis = "USD", selected_source_class = "targeted_direct",
    source_package_id = "SRC"
  )
  common <- tibble(
    analysis_year = 2024L, iso3 = "BBB", currency = "USD",
    nonstandard_feature_flag = TRUE,
    candidate_secondary_standard = FALSE,
    strict_candidate_admitted = FALSE,
    direct_yield_present = TRUE, identifiers_with_price = 1L
  )
  grid <- tibble(
    analysis_year = c(2024L, 2024L), iso3 = c("AAA", "BBB"),
    historical_lmic_reporting_scope = TRUE,
    historical_income_level = "Lower middle income"
  )
  status <- tibble(
    analysis_year = integer(), iso3 = character(),
    approved_status_rule_class = character(),
    observed_rate_use_state = character(),
    observed_benchmark_selection_permitted = logical(),
    raw_evidence_retained = logical()
  )
  result <- p15_build_targeted_secondary_companion_evidence(
    standard, standard[0, ], standard[0, ], common, grid, status
  )
  expect_equal(result$iso3, "AAA")
  expect_equal(result$candidate_evidence_tier,
               "targeted_direct_yield_to_maturity")
  expect_true(result$candidate_for_validation_anchor)
  expect_false(result$selected_for_ladder)
})

test_that("integration identifiers keep candidate authority explicit", {
  expect_match(p15_integrated_observed_schema_version(), "CANDIDATE")
  expect_match(p15_integrated_observed_build_id(), "20260817")
  expect_match(p15_integrated_observed_method_id(), "TERMINAL-INDEPENDENT")
  expect_match(
    p15_integrated_observed_parity_specification_id(), "REGRESSION"
  )
})
