suppressPackageStartupMessages({
  library(dplyr)
  library(testthat)
})

source("../../R/p15_ids_evidence.R")

test_that("IDS evidence keeps source availability separate from admissibility", {
  grid <- tibble::tibble(
    analysis_year = c(2024L, 2024L),
    iso3 = c("AAA", "BBB"),
    country = c("A", "B"),
    country_year_id = c("2024::AAA", "2024::BBB"),
    historical_income_level = c("Low income", "High income"),
    historical_lmic_reporting_scope = c(TRUE, FALSE)
  )
  ids <- tibble::tibble(
    iso3 = "AAA", country = "A", year = 2024L,
    creditor = "Bondholders", creditor_id = "BND",
    official_grace_years = 5, official_maturity_years = 5,
    official_rate = 7, has_complete_terms = TRUE,
    ids_history_source = "test"
  )
  out <- p15_build_ids_bondholder_evidence(grid, ids)
  expect_equal(nrow(out), 2L)
  expect_equal(out$ids_rate_class, c(
    "positive_1_30", "country_not_in_ids_bondholder_source_universe"
  ))
  expect_equal(
    out$bondholder_term_validity_state[[1]],
    "bondholder_terms_usable_subject_to_source_checks"
  )
  expect_true(all(out$ids_admissibility_state == "not_evaluated"))
  expect_true(all(out$ids_selection_state == "not_evaluated"))
})
