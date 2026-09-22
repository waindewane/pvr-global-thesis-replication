suppressPackageStartupMessages({
  library(dplyr)
  library(testthat)
  library(tibble)
  library(tidyr)
})

source("../../R/p15_status_context.R")
source("../../R/p15_status_expansion.R")

test_that("systematic status sources parse to unique bounded country-years", {
  root <- normalizePath(file.path("..", ".."), winslash = "/", mustWork = TRUE)
  ucdp <- p15_parse_ucdp_organized_violence(file.path(
    root, "data-raw/status_context_sources",
    "ucdp_organized_violence_country_year_v26_1_2026-07-21",
    "OrganizedViolenceCYDataSet26_1.csv"
  ))
  bftu <- p15_parse_bftu_capital_restrictions(
    file.path(
      root, "data-raw/status_context_sources",
      "bftu_cross_border_restrictions_dfbcbfb_2026-07-21",
      "BFTU_iBoPS.xlsx"
    ),
    file.path(
      root, "data-raw/status_context_sources",
      "bftu_cross_border_restrictions_dfbcbfb_2026-07-21",
      "FKRSU_LLM.xlsx"
    )
  )

  expect_equal(nrow(ucdp), 2548L)
  expect_equal(range(ucdp$analysis_year), c(2012L, 2024L))
  expect_false(anyDuplicated(ucdp[c("analysis_year", "iso3")]) > 0L)
  expect_true(any(ucdp$ucdp_any_organized_violence_context))
  expect_true(all(ucdp$ucdp_version == "26.1"))

  expect_equal(nrow(bftu), 2340L)
  expect_equal(range(bftu$analysis_year), c(2012L, 2023L))
  expect_false(anyDuplicated(bftu[c("analysis_year", "iso3")]) > 0L)
  expect_true(any(bftu$bftu_any_capital_restriction_indicator_available))
  expect_false(any(bftu$analysis_year == 2024L))
})

test_that("expanded status evidence never invents 2024 BFTU values or consequences", {
  status <- tibble::tibble(
    analysis_year = c(2023L, 2024L),
    iso3 = c("AAA", "AAA"),
    country = c("Alpha", "Alpha"),
    country_year_id = c("2023::AAA", "2024::AAA"),
    positive_status_evidence_present = c(FALSE, TRUE),
    status_evidence_ids = c("", "STATUS-EVIDENCE-DEFAULT"),
    positive_evidence_source_package_ids = c("", "TEST-DEFAULT"),
    queried_source_package_ids = c("TEST", "TEST"),
    status_consequence_state = c("not_evaluated", "not_evaluated"),
    status_consequence_rule_id = c(NA_character_, NA_character_)
  )
  ucdp <- tibble::tibble(
    analysis_year = c(2023L, 2024L),
    iso3 = c("AAA", "AAA"),
    ucdp_country_id = c(1L, 1L),
    ucdp_any_organized_violence_context = c(TRUE, FALSE)
  )
  bftu <- tibble::tibble(
    analysis_year = 2023L,
    iso3 = "AAA",
    bftu_country_name = "Alpha",
    bftu_any_capital_restriction_indicator_available = TRUE,
    bftu_ibops_capital_account_restriction_stance = 0.5,
    bftu_fkrsu_capital_restriction_stance = 0.6
  )

  out <- p15_build_expanded_status_context(status, ucdp, bftu)
  row_2024 <- out[out$analysis_year == 2024L, ]
  expect_equal(row_2024$bftu_context_state, "source_year_unavailable_2024")
  expect_true(is.na(row_2024$bftu_ibops_capital_account_restriction_stance))
  expect_true(all(out$expanded_status_consequence_state == "not_evaluated"))
  expect_true(all(is.na(out$expanded_status_consequence_rule_id)))
  expect_match(
    out$expanded_status_evidence_ids[out$analysis_year == 2023L],
    "ORGANIZED-VIOLENCE"
  )
})

test_that("status taxonomy defines evidence states without automatic rules", {
  taxonomy <- p15_status_evidence_taxonomy()
  expect_equal(nrow(taxonomy), 13L)
  expect_false(anyDuplicated(taxonomy$taxonomy_id) > 0L)
  expect_true(all(taxonomy$automatic_consequence == "none"))
  expect_true(all(taxonomy$decision_state == "not_evaluated"))
  expect_true(all(c(
    "war_or_conflict", "capital_controls", "sanctions", "debt_distress",
    "priced_out_or_no_access", "general_market_access"
  ) %in% taxonomy$status_dimension))
})

test_that("organized-violence context alone does not enlarge the quantitative queue", {
  expanded <- tibble::tibble(
    analysis_year = c(2023L, 2023L, 2024L),
    iso3 = c("AAA", "BBB", "CCC"),
    country = c("Alpha", "Beta", "Gamma"),
    expanded_status_context_trigger_present = TRUE,
    positive_status_evidence_present = c(TRUE, FALSE, TRUE),
    ucdp_any_organized_violence_context = c(FALSE, TRUE, TRUE),
    p13_case_evidence_present = c(FALSE, FALSE, TRUE),
    ucdp_state_based_violence_flag = c(FALSE, TRUE, TRUE),
    ucdp_non_state_violence_flag = FALSE,
    ucdp_one_sided_violence_flag = FALSE,
    ucdp_total_organized_violence_deaths_best = c(0, 10, 20),
    bftu_ibops_capital_account_restriction_stance = NA_real_,
    bftu_fkrsu_capital_restriction_stance = NA_real_,
    expanded_status_evidence_ids = c("DEFAULT", "VIOLENCE", "CASE;VIOLENCE"),
    expanded_positive_evidence_source_package_ids = c("A", "B", "C"),
    p13_case_status_categories = c(NA, NA, "market_access"),
    p13_case_status_values = c(NA, NA, "restricted"),
    p13_case_market_access_implications = c(NA, NA, "review")
  )
  historical <- tibble::tibble(
    analysis_year = c(2023L, 2023L),
    iso3 = c("AAA", "BBB"),
    status_quantitative_review_trigger = c(TRUE, FALSE),
    status_quantitative_effect_rule = c("review", NA),
    affects_quantitative_admissibility = c(FALSE, FALSE),
    affects_display_or_interpretation = c(TRUE, FALSE),
    status_source_pointer = c("source-a", "source-b")
  )

  queues <- p15_build_status_review_queues(expanded, historical)
  expect_equal(nrow(queues$context_screening_queue), 3L)
  expect_equal(nrow(queues$quantitative_case_review_queue), 2L)
  expect_setequal(
    queues$quantitative_case_review_queue$iso3,
    c("AAA", "CCC")
  )
  expect_false("BBB" %in% queues$quantitative_case_review_queue$iso3)
  expect_true(all(
    queues$quantitative_case_review_queue$automatic_consequence == "none"
  ))
})

test_that("status case recommendations remain provisional and consequence-free", {
  queue <- tibble::tibble(
    analysis_year = c(2023L, 2024L),
    iso3 = c("AAA", "BBB"),
    country = c("Alpha", "Beta"),
    review_basis = c(
      "predecessor_quantitative_review_trigger", "p13_2024_case_evidence"
    ),
    predecessor_effect_rule = c("review_not_block", NA),
    predecessor_affects_quantitative_admissibility = c(FALSE, NA),
    predecessor_affects_display_or_interpretation = c(TRUE, NA),
    predecessor_status_source_pointer = c("default source", NA),
    p13_case_status_categories = c(NA, "active_default_or_restructuring"),
    p13_case_status_values = c(NA, "ongoing restructuring"),
    p13_case_market_access_implications = c(
      NA, "blocks ordinary fallback interpretation"
    ),
    ucdp_any_organized_violence_context = c(FALSE, TRUE),
    bftu_ibops_capital_account_restriction_stance = c(0.5, NA),
    bftu_fkrsu_capital_restriction_stance = c(0.4, NA),
    expanded_status_evidence_ids = c("DEFAULT", "CASE;VIOLENCE"),
    expanded_positive_evidence_source_package_ids = c("SRC-A", "SRC-B")
  )
  out <- p15_prepare_status_case_recommendations(queue)
  expect_equal(nrow(out), 2L)
  expect_equal(
    out$recommended_treatment_class[out$iso3 == "AAA"],
    "provisional_context_warning_no_automatic_block_pending_STAT_05"
  )
  expect_equal(
    out$recommended_treatment_class[out$iso3 == "BBB"],
    "provisional_block_ordinary_fallback_pending_STAT_05"
  )
  expect_true(all(out$automatic_consequence == "none"))
  expect_true(all(grepl("not_applied", out$recommendation_state)))
})
