suppressPackageStartupMessages({
  library(dplyr)
  library(testthat)
  library(tibble)
  library(tidyr)
})

source("../../R/p15_status_context.R")

test_that("status dimension contract covers every required audit category", {
  dimensions <- p15_required_status_dimensions()
  expect_equal(nrow(dimensions), 13L)
  expect_true(all(c(
    "sovereign_default", "fiscal_arrears", "official_restructuring",
    "private_or_bond_restructuring", "sanctions", "war_or_conflict",
    "capital_controls", "no_qualifying_issuance", "general_market_access"
  ) %in% dimensions$status_dimension))
  expect_false(anyDuplicated(dimensions$status_dimension) > 0L)
})

test_that("gap builder keeps missing dimensions explicit", {
  path <- tempfile()
  writeLines("source", path)
  inventory <- tibble::tibble(
    source_component_id = "TEST-S01",
    source_package_id = "TEST-SOURCE",
    status_dimension = "sovereign_default",
    source_name = "Test source",
    path = path,
    coverage_class = "full_p15_country_year_panel",
    year_scope = "2012-2024",
    evidence_role = "official_default_stock_context",
    final_method_eligible = TRUE,
    limitation = "Test limitation",
    file_exists = TRUE,
    sha256 = digest::digest(file = path, algo = "sha256"),
    schema_version = p15_status_gap_schema_version()
  )
  gap <- p15_build_status_source_gap_matrix(inventory)
  expect_equal(nrow(gap), 13L)
  expect_equal(
    gap$source_readiness_state[gap$status_dimension == "sovereign_default"],
    "source_backed_panel_or_event_input_available"
  )
  expect_equal(
    gap$source_readiness_state[gap$status_dimension == "capital_controls"],
    "no_source_identified"
  )
  expect_true(all(
    gap$decision_consequence ==
      "evidence_inventory_only_no_warning_or_block_rule_set"
  ))
})

test_that("registered status sources parse into unique country-year evidence", {
  root <- normalizePath(file.path("..", ".."), winslash = "/", mustWork = TRUE)
  boc <- p15_parse_boc_boe_default(
    file.path(
      root,
      "data-raw/status_context_sources/boc_boe_sovereign_default_database_DEBT_2025_2026-06-30.json"
    ),
    years = 2012:2024
  )
  paris <- p15_parse_paris_club_agreements(
    file.path(
      root,
      "data-raw/status_context_sources/paris_club_signed_agreements_2026-06-30/paris_club_signed_agreements_advanced_search_2026-06-30.csv"
    ),
    years = 2012:2024
  )
  expect_false(anyDuplicated(boc[c("analysis_year", "iso3")]) > 0L)
  expect_false(anyDuplicated(paris[c("analysis_year", "iso3")]) > 0L)
  expect_equal(range(boc$analysis_year), c(2012, 2024))
  expect_true(all(paris$analysis_year %in% 2012:2024))
  expect_true(any(boc$boc_default_context_flag))
  expect_true(any(paris$paris_club_same_year_agreement_flag))
})
