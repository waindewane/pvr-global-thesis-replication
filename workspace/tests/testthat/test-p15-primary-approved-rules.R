suppressPackageStartupMessages({
  library(dplyr)
  library(testthat)
  library(tibble)
  library(tidyr)
})

source("../../R/p15_observed_all_years.R")
source("../../R/p15_primary_feature_diagnostic.R")
source("../../R/p15_primary_approved_rules.R")

p15_approved_test_issue <- function(key, isin, currency = "USD", rate = 5,
                                    amount = 100e6, standard = TRUE,
                                    nonstandard = FALSE) {
  tibble(
    analysis_year = 2024L, iso3 = "AAA", country = "Alpha",
    country_year_id = "2024::AAA", period = "2024",
    historical_income_level = "Lower middle income",
    historical_lmic_reporting_scope = TRUE, economic_issue_key = key,
    issue_date = as.Date("2024-01-01"),
    maturity_date = as.Date("2030-01-01"), currency = currency,
    original_maturity_years = 6, original_issue_yield_pct = rate,
    face_issued_usd = amount, aggregation_weight_value = amount,
    usd_weight_available = TRUE, identifier_yield_conflict = FALSE,
    bill_or_strip_flag = FALSE, central_bank_flag = FALSE,
    restructuring_flag = FALSE, nonstandard_feature_flag = nonstandard,
    candidate_primary_standard = standard,
    candidate_primary_standard_50m = standard & amount >= 50e6,
    representative_isins = isin, representative_rics = paste0(isin, "="),
    source_package_ids = "SRC", source_record_locators = paste0("row:", key),
    source_object = "lseg_original_issue_yield"
  )
}

p15_approved_test_instruments <- function() {
  tibble(
    snapshot_year = rep(2024L, 3), issue_year = rep(2024L, 3),
    canonical_iso3 = "AAA", ISIN = c("PLAIN", "CALL", "FLOAT"),
    IsCallable = c(FALSE, TRUE, TRUE), IsPutable = FALSE,
    IsSinkable = FALSE, IsConvertible = FALSE,
    RCSCouponTypeLeaf = c("Fixed", "Fixed", "Floating Rate"),
    CouponTypeDescription = c("Fixed", "Fixed", "Floating"),
    DebtTypeDescription = "Bond", InstrumentTypeDescription = "Bond",
    DocumentTitle = c("Plain bond", "Callable bond", "Callable floating bond")
  )
}

test_that("approved primary rule admits only the narrow fixed call-sink subtype", {
  issues <- bind_rows(
    p15_approved_test_issue("plain", "PLAIN"),
    p15_approved_test_issue(
      "call", "CALL", rate = 7, standard = FALSE, nonstandard = TRUE
    ),
    p15_approved_test_issue(
      "float", "FLOAT", rate = 8, standard = FALSE, nonstandard = TRUE
    )
  )
  result <- p15_apply_approved_primary_rules(
    issues, p15_approved_test_instruments()
  )
  expect_true(result$approved_plain_vanilla_issue[result$economic_issue_key == "plain"])
  expect_true(result$approved_fixed_call_sink_subtype[result$economic_issue_key == "call"])
  expect_false(result$approved_primary_structural_issue[result$economic_issue_key == "float"])
  expect_true(all(result$raw_evidence_retained))
  expect_false(any(result$selected_for_ladder))
})

test_that("approved primary variants keep USD and EUR separate and label thinness", {
  issues <- bind_rows(
    p15_approved_test_issue("usd", "PLAIN", currency = "USD", rate = 5),
    p15_approved_test_issue("eur", "EURO", currency = "EUR", rate = 2)
  )
  instruments <- bind_rows(
    p15_approved_test_instruments() |> filter(ISIN == "PLAIN"),
    p15_approved_test_instruments() |> filter(ISIN == "PLAIN") |>
      mutate(ISIN = "EURO")
  )
  disposition <- p15_apply_approved_primary_rules(issues, instruments)
  candidates <- p15_aggregate_approved_primary_candidates(disposition)
  preferred <- candidates |>
    filter(candidate_variant_id == "primary_forward_usd_50m_preferred")
  euro <- candidates |>
    filter(candidate_variant_id == "primary_forward_eur_50m_evidence")
  expect_equal(preferred$market_rate_pct_before_sanity, 5)
  expect_equal(euro$market_rate_pct_before_sanity, 2)
  expect_equal(preferred$currency_basis, "USD")
  expect_equal(euro$currency_basis, "EUR")
  expect_true(preferred$thin_evidence)
  expect_false(any(candidates$selected_for_ladder))
})

test_that("approved primary method retains low and high rates without deletion", {
  issues <- bind_rows(
    p15_approved_test_issue("low", "PLAIN", rate = 0.5),
    p15_approved_test_issue("high", "HIGH", rate = 35)
  )
  instruments <- bind_rows(
    p15_approved_test_instruments() |> filter(ISIN == "PLAIN"),
    p15_approved_test_instruments() |> filter(ISIN == "PLAIN") |>
      mutate(ISIN = "HIGH")
  )
  result <- p15_apply_approved_primary_rules(issues, instruments)
  expect_true(all(result$approved_primary_structural_issue))
  expect_true(all(result$raw_evidence_retained))
})
