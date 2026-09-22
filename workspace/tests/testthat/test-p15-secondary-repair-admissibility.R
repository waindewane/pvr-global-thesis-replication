suppressPackageStartupMessages({
  library(dplyr)
  library(testthat)
  library(tibble)
})

source("../../R/p15_observed_all_years.R")
source("../../R/p15_observed_secondary.R")
source("../../R/p15_secondary_repair_admissibility.R")

p15_test_repair_identifier <- function(
    iso3, ric, currency = "USD", direct = FALSE, callable = FALSE) {
  tibble(
    analysis_year = 2024L,
    snapshot_date = as.Date("2024-12-31"),
    ric = ric,
    isin = paste0("ISIN", ric),
    economic_issue_key = paste0("ECONOMIC-ISSUE-", ric),
    iso3 = iso3,
    country = paste("Country", iso3),
    country_year_id = paste0(iso3, "_2024"),
    period = "2024",
    historical_income_level = "Lower middle income",
    historical_lmic_reporting_scope = TRUE,
    issue_date = as.Date("2020-01-01"),
    maturity_date = as.Date("2030-12-31"),
    currency = currency,
    coupon_rate_pct = 5,
    coupon_frequency_description = "Semiannually",
    coupon_type_leaf = "Plain Vanilla Fixed Coupon",
    coupon_type_description = "Fixed:Plain Vanilla Fixed Coupon",
    face_outstanding_usd = 1e8,
    is_callable = callable,
    is_putable = FALSE,
    is_sinkable = FALSE,
    is_convertible = FALSE,
    flag_short_bill_cp_like = FALSE,
    flag_central_bank_like = FALSE,
    nonstandard_feature_flag = callable,
    any_price_available = TRUE,
    direct_yield_available = direct,
    direct_quote_date = if (direct) as.Date("2024-12-31") else as.Date(NA),
    direct_yield_pct = if (direct) 5 else NA_real_,
    universe_source_package_id = "PKG-TEST",
    universe_source_record_locator = paste0("universe:", ric)
  )
}

p15_test_repair_history <- function(identifier_evidence) {
  identifier_evidence |>
    transmute(
      analysis_year,
      history_date = as.Date("2024-12-31"),
      RIC = ric,
      mid_price = 100,
      bid = 99.9,
      ask = 100.1,
      yield_to_maturity = if_else(direct_yield_available, 5, NA_real_),
      source_package_id = "PKG-TEST",
      source_role = "test_quote",
      source_record_locator = paste0("history:", ric)
    )
}

p15_test_status_recommendations <- function() {
  tibble(
    analysis_year = c(2024L, 2024L),
    iso3 = c("DDD", "EEE"),
    recommended_treatment_class = c(
      "provisional_context_warning_no_automatic_block_pending_STAT_05",
      "provisional_manual_review_before_observed_use_pending_STAT_05"
    ),
    recommended_admissibility_action = c("retain_with_warning", "hold"),
    recommended_display_action = c("warning", "review"),
    expanded_status_evidence_ids = c("STATUS-1", "STATUS-2"),
    recommendation_state = c("provisional", "provisional")
  )
}

test_that("closest repair date does not skip a complete but invalid price", {
  ids <- p15_test_repair_identifier("AAA","AAA1")
  h <- bind_rows(p15_test_repair_history(ids),p15_test_repair_history(ids))
  h$history_date <- as.Date(c("2025-01-02","2025-01-07"))
  h$mid_price <- c(-1,100)
  pars <- p15_secondary_repair_candidate_parameters()
  pars$quote_selection_rule <- "closest"
  result <- p15_build_strict_secondary_repair_candidates(ids,h,p15_test_status_recommendations(),pars)
  expect_equal(result$identifier_audit$selected_quote_date,as.Date("2025-01-02"))
  expect_false(result$identifier_audit$price_order_gate)
  expect_false(result$issue_audit$strict_candidate_admitted)
})

test_that("coupon-frequency parser fails closed on unsupported conventions", {
  expect_equal(
    p15_secondary_repair_frequency(
      c("Annually", "Semiannually", "Quarterly", "Monthly")
    ),
    c(1, 2, 4, 12)
  )
  expect_true(is.na(p15_secondary_repair_frequency("At maturity")))
  expect_true(is.na(p15_secondary_repair_frequency(NA_character_)))
})

test_that("strict repair admits only rows passing every gate", {
  identifiers <- bind_rows(
    p15_test_repair_identifier("AAA", "AAA1"),
    p15_test_repair_identifier("BBB", "BBB1", direct = TRUE),
    p15_test_repair_identifier("CCC", "CCC1", callable = TRUE),
    p15_test_repair_identifier("DDD", "DDD1"),
    p15_test_repair_identifier("EEE", "EEE1"),
    p15_test_repair_identifier("FFF", "FFF1", currency = "EUR")
  )

  result <- p15_build_strict_secondary_repair_candidates(
    identifiers,
    p15_test_repair_history(identifiers),
    p15_test_status_recommendations()
  )

  issue <- result$issue_audit
  expect_true(issue$strict_candidate_admitted[issue$iso3 == "AAA"])
  expect_false(issue$direct_ytm_absent_gate[issue$iso3 == "BBB"])
  expect_false(issue$strict_candidate_admitted[issue$iso3 == "BBB"])
  expect_equal(
    issue$direct_repair_comparison_identifier_count[issue$iso3 == "BBB"],
    1L
  )
  expect_lt(
    issue$repaired_minus_direct_abs_bps[issue$iso3 == "BBB"],
    2
  )
  expect_false(
    issue$all_identifiers_technical_pass[issue$iso3 == "CCC"]
  )
  expect_equal(
    issue$admission_state[issue$iso3 == "DDD"],
    "admitted_candidate_with_status_warning"
  )
  expect_equal(
    issue$admission_state[issue$iso3 == "EEE"],
    "held_pending_STAT_05_case_decision"
  )
  expect_false(issue$strict_candidate_admitted[issue$iso3 == "EEE"])
  expect_true(all(!issue$selected_for_ladder))
})

test_that("country candidates preserve currencies and remain unselected", {
  identifiers <- bind_rows(
    p15_test_repair_identifier("AAA", "AAA1", currency = "USD"),
    p15_test_repair_identifier("FFF", "FFF1", currency = "EUR")
  )
  empty_status <- p15_test_status_recommendations()[0, ]

  candidates <- p15_build_strict_secondary_repair_candidates(
    identifiers,
    p15_test_repair_history(identifiers),
    empty_status
  )$country_candidates

  expect_setequal(candidates$currency, c("USD", "EUR"))
  expect_setequal(
    candidates$candidate_variant_id,
    c(
      "price_derived_secondary_strict_usd_2_15",
      "price_derived_secondary_strict_eur_2_15"
    )
  )
  expect_true(all(!candidates$selected_for_ladder))
  expect_true(all(candidates$selection_state == "not_selected_pending_SEC_18"))
})

test_that("confirmed paired identifiers collapse to one economic issue", {
  first <- p15_test_repair_identifier("AAA", "AAA1")
  second <- p15_test_repair_identifier("AAA", "AAA2") |>
    mutate(economic_issue_key = first$economic_issue_key)
  identifiers <- bind_rows(first, second)

  result <- p15_build_strict_secondary_repair_candidates(
    identifiers,
    p15_test_repair_history(identifiers),
    p15_test_status_recommendations()[0, ]
  )

  expect_equal(nrow(result$issue_audit), 1L)
  expect_equal(result$issue_audit$issue_price_identifier_count, 2L)
  expect_equal(nrow(result$country_candidates), 1L)
  expect_equal(result$country_candidates$issue_count, 1L)
  expect_equal(result$country_candidates$identifier_count, 2L)
  expect_equal(result$country_candidates$total_weight_usd, 1e8)
})

test_that("integrated disposition keeps direct repaired and blocked rows distinct", {
  identifiers <- bind_rows(
    p15_test_repair_identifier("AAA", "AAA1", direct = TRUE),
    p15_test_repair_identifier("BBB", "BBB1"),
    p15_test_repair_identifier("CCC", "CCC1", callable = TRUE)
  )
  repair <- p15_build_strict_secondary_repair_candidates(
    identifiers,
    p15_test_repair_history(identifiers),
    p15_test_status_recommendations()[0, ]
  )$issue_audit
  direct <- identifiers |>
    transmute(
      analysis_year, iso3, economic_issue_key,
      direct_yield_pct = if_else(direct_yield_available, 5, NA_real_),
      identifiers_with_price = 1L,
      candidate_secondary_standard = direct_yield_available,
      candidate_secondary_usd_eur_2_15 = direct_yield_available,
      nonstandard_feature_flag
    )

  catalogue <- p15_build_secondary_issue_disposition_catalogue(
    direct, repair
  )

  expect_equal(
    catalogue$secondary_evidence_disposition[catalogue$iso3 == "AAA"],
    "direct_secondary_standard_candidate"
  )
  expect_equal(
    catalogue$secondary_evidence_disposition[catalogue$iso3 == "BBB"],
    "price_derived_secondary_strict_candidate"
  )
  expect_equal(
    catalogue$secondary_evidence_disposition[catalogue$iso3 == "CCC"],
    "feature_rich_price_evidence_blocked"
  )
  expect_true(all(!catalogue$selected_for_ladder))
})
