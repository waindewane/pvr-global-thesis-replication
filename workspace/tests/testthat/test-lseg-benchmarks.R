source("../../R/ids.R")
source("../../R/build_outputs.R")
source("../../R/lseg_benchmarks.R")

build_lseg_test_fixture <- function() {
  countries <- read_country_metadata("../../data-raw/world_bank_countries.json")
  counterpart_areas <- read_counterpart_areas("../../data-raw/ids_counterpart_area.json")
  ids_terms <- build_ids_terms("../../data-raw/ids_terms_all_lenders", countries, counterpart_areas)
  ids_path <- tempfile(fileext = ".csv")
  write_output_csv(ids_terms, ids_path)

  lseg_outputs <- build_lseg_core_benchmark_outputs_2024(
    candidate_path = "../../sources/market_rates/lseg_workspace_download_v6_2026-05-07/output/tables/lseg_pvr_benchmark_candidates_with_yield_oecd_base_2000-01-01_2025-12-31.csv",
    base_path = "../../sources/market_rates/lseg_workspace_download_v6_2026-05-07/output/tables/lseg_oecd_base_with_yield_fallback_oecd_base_2000-01-01_2025-12-31.csv",
    ids_terms_path = ids_path,
    country_metadata_path = "../../data-raw/world_bank_countries.json"
  )

  list(ids_terms = ids_terms, lseg_outputs = lseg_outputs)
}

lseg_test_fixture <- build_lseg_test_fixture()

testthat::test_that("hardened 2024 LSEG build is reproducible from repo-stored files", {
  fixture <- lseg_test_fixture
  outputs <- fixture$lseg_outputs

  testthat::expect_equal(nrow(outputs$screened_rows), 201)
  testthat::expect_equal(nrow(outputs$country_summary), 41)
  testthat::expect_equal(nrow(outputs$country_rates), 36)
  testthat::expect_equal(nrow(outputs$excluded_country_rates), 5)
  testthat::expect_equal(nrow(outputs$overlap_comparison), 6)
  testthat::expect_equal(nrow(outputs$broader_base_audit), 72)
})

testthat::test_that("hardened 2024 LSEG build has expected status and strength counts", {
  fixture <- lseg_test_fixture
  summary <- fixture$lseg_outputs$country_summary
  rates <- fixture$lseg_outputs$country_rates

  testthat::expect_equal(
    as.integer(table(summary$country_computation_status)[c(
      "computed_clean",
      "computed_with_review",
      "deferred_nonstandard_only",
      "deferred_suspected_restructuring"
    )]),
    c(34, 2, 3, 2)
  )

  testthat::expect_equal(
    as.integer(table(rates$observed_rate_strength)[c(
      "review_needed",
      "robust_multi_issue",
      "supported_two_issue",
      "thin_single_issue"
    )]),
    c(2, 10, 11, 13)
  )
})

testthat::test_that("non-clean country summaries always carry reason and next-step fields", {
  fixture <- lseg_test_fixture
  summary <- fixture$lseg_outputs$country_summary
  non_clean <- subset(summary, country_computation_status != "computed_clean")

  testthat::expect_true(all(nzchar(non_clean$country_status_reason)))
  testthat::expect_true(all(nzchar(non_clean$recommended_next_step)))
})

testthat::test_that("thin single-issue countries are confirmed against the broader local base", {
  fixture <- lseg_test_fixture
  rates <- fixture$lseg_outputs$country_rates
  thin <- subset(rates, observed_rate_strength == "thin_single_issue")

  testthat::expect_equal(nrow(thin), 13)
  testthat::expect_true(all(thin$thin_country_audit_result == "confirmed_single_candidate_issue_in_broader_base"))
})

testthat::test_that("canonical 2024 benchmark build prefers Tier 2 LSEG observed rates and retains IDS as fallback only", {
  fixture <- lseg_test_fixture
  combined <- build_market_benchmarks_2024(fixture$ids_terms, fixture$lseg_outputs)

  testthat::expect_equal(nrow(combined), 42)

  kenya <- combined[combined$iso3 == "KEN", ]
  brazil <- combined[combined$iso3 == "BRA", ]
  benin <- combined[combined$country == "Benin", ]
  ghana <- combined[combined$country == "Ghana", ]
  dominica <- combined[combined$country == "Dominica", ]

  testthat::expect_equal(kenya$market_rate_source_class, "lseg_workspace_tier2_observed")
  testthat::expect_equal(round(kenya$market_rate, 3), 10.307)
  testthat::expect_equal(kenya$market_rate_measure_basis, "issue_level_primary_market_yield_weighted")
  testthat::expect_equal(kenya$market_rate_selection_role, "preferred_observed_benchmark")

  testthat::expect_equal(brazil$market_rate_source_class, "lseg_workspace_tier2_observed")
  testthat::expect_equal(round(brazil$market_rate, 3), 6.637)

  testthat::expect_equal(benin$market_rate_source_class, "lseg_workspace_tier2_observed")
  testthat::expect_equal(round(benin$market_rate, 3), 8.358)

  testthat::expect_equal(nrow(ghana), 0)
  testthat::expect_equal(dominica$market_rate_source_class, "world_bank_ids")
  testthat::expect_equal(dominica$market_rate_selection_role, "fallback_public_proxy")
  testthat::expect_true(all(combined$benchmark_cashflow_representation == "synthetic_bullet_market_reference"))
})

testthat::test_that("IDS-LSEG discrepancy audit is present for all overlap countries with both rates", {
  fixture <- lseg_test_fixture
  audit <- fixture$lseg_outputs$overlap_audit

  testthat::expect_equal(nrow(audit), 6)
  testthat::expect_true(all(nzchar(audit$likely_methodological_reason)))
  testthat::expect_true(all(nzchar(audit$audit_next_step)))
  testthat::expect_true(all(audit$difference_magnitude_band %in% c("minor_difference", "moderate_difference", "large_difference")))
})
