suppressPackageStartupMessages({
  library(dplyr)
  library(testthat)
})

source("../../R/p15_rating_components.R")

test_that("rating labels normalize to the P13 Moody-equivalent notch map", {
  raw <- c("AAA", "AA-", "BBB+", "BB", "B-", "CCC", "Aa2", "B1", "D", "NR")
  expected <- c("Aaa", "Aa3", "Baa1", "Ba2", "B3", "Caa2", "Aa2", "B1", NA, NA)
  expect_equal(p15_normalize_rating_to_moodys(raw), expected)
})

test_that("rating-year spread collisions use the documented P13 median rule", {
  damodaran <- tibble::tibble(
    analysis_year = c(2024L, 2024L, 2024L),
    damodaran_rating = c("B1", "B1", "B2"),
    default_spread_pct = c(4, 6, 7),
    archive_parser = "test",
    archive_vintage_label = "test archive"
  )
  out <- p15_build_damodaran_rating_spread_lookup(damodaran)
  b1 <- dplyr::filter(out, .data$normalized_rating == "B1")
  expect_equal(b1$damodaran_rating_mapped_spread_pct, 5)
  expect_equal(b1$spread_observation_count, 2L)
  expect_equal(b1$distinct_spread_count, 2L)
})

test_that("component ledger separates availability from later decisions", {
  grid <- tibble::tibble(
    analysis_year = c(2024L, 2024L, 2024L),
    iso3 = c("AAA", "BBB", "CCC"),
    country = c("A", "B", "C"),
    country_year_id = c("2024::AAA", "2024::BBB", "2024::CCC"),
    historical_income_level = c("Low income", "High income", "High income"),
    historical_lmic_reporting_scope = c(TRUE, FALSE, FALSE)
  )
  ratings <- tibble::tibble(
    country_code_bloomberg = c("AA", "BB"),
    iso3 = c("AAA", "BBB"),
    region = c("Region 1", "Region 2"),
    year = 2024L,
    moodys_rating = c("B1", NA),
    moodys_source_event_date = as.Date(c("2023-01-01", NA)),
    fitch_rating = c("B+", "BB"),
    fitch_source_event_date = as.Date(c("2023-02-01", "2020-01-01")),
    sp_rating = c(NA, NA),
    sp_source_event_date = as.Date(c(NA, NA)),
    preferred_agency_count = c(2L, 1L),
    preferred_agencies_available = c("Moodys;Fitch", "Fitch"),
    selected_rating = c("B1", "BB"),
    selected_agency = c("Moodys", "Fitch"),
    selected_source_event_date = as.Date(c("2023-01-01", "2020-01-01")),
    selected_source_rule = "latest_event_on_or_before_jan1_current_rating",
    has_selected_precedence_rating = TRUE,
    agency_precedence_rule = "Moodys then Fitch then SP"
  )
  missing_ratings <- tibble::tibble(
    iso3 = character(), year = integer(), missing_reason = character(),
    has_any_rating_type = logical(),
    active_nonpreferred_agencies = character(),
    active_nonpreferred_rating_types = character()
  )
  damodaran <- tibble::tibble(
    analysis_year = c(2024L, 2024L),
    damodaran_rating = c("B1", "Ba2"),
    default_spread_pct = c(4, 3),
    archive_parser = "test",
    archive_vintage_label = "test archive"
  )
  fred <- tibble::tibble(
    observation_date = as.Date(c("2024-01-02", "2024-01-03")),
    DGS7 = c(4, 6)
  )
  conflicts <- tibble::tibble(
    event_year = 2024L, iso3 = "BBB", event_date = as.Date("2024-02-01"),
    agency = "Fitch", preferred_external_rating_type = TRUE
  )

  out <- p15_build_rating_component_ledger(
    grid, ratings, missing_ratings, damodaran, fred, conflicts
  )
  expect_equal(nrow(out), 3L)
  expect_equal(out$rating_implied_candidate_rate_pct[1:2], c(9, 8))
  expect_equal(
    out$agency_fallback_state[[2]],
    "fitch_selected_by_documented_agency_fallback"
  )
  expect_equal(
    out$rating_availability_state[[3]],
    "country_not_in_bloomberg_rating_panel_universe"
  )
  expect_equal(out$same_day_conflict_event_rows[[2]], 1L)
  expect_true(all(out$rating_admissibility_state == "not_evaluated"))
  expect_true(all(out$rating_selection_state == "not_evaluated"))
})
