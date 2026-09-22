suppressPackageStartupMessages({
  library(dplyr)
  library(testthat)
  library(tibble)
})

source("../../R/p15_observed_all_years.R")

test_grid <- function() {
  tibble::tibble(
    analysis_year = 2024L,
    iso3 = "AAA",
    country = "Alpha",
    historical_income_level = "Lower middle income",
    historical_lmic_reporting_scope = TRUE,
    country_year_id = "2024::AAA",
    period = "2024"
  )
}

test_instrument_universe <- function() {
  tibble::tibble(
    snapshot_year = rep(2024L, 3),
    issue_year = rep(2024L, 3),
    snapshot_date = as.Date(rep("2024-12-31", 3)),
    canonical_iso3 = rep("AAA", 3),
    canonical_country = rep("Alpha", 3),
    ISIN = c("ISIN1", "ISIN2", "ISIN3"),
    RIC = c("RIC1", "RIC2", "RIC3"),
    IssueDate = as.Date(c("2024-01-01", "2024-02-01", "2024-03-01")),
    MaturityDate = as.Date(c("2030-01-01", "2031-02-01", "2032-03-01")),
    Currency = c("USD", "USD", "USD"),
    CouponRate = c(5, 6, 7),
    FaceIssuedUSD = c(100e6, 120e6, NA_real_),
    FaceOutstandingUSD = c(90e6, 110e6, 80e6),
    AssetStatus = rep("ISS", 3),
    IssuerName = rep("Alpha", 3),
    DocumentTitle = c("Plain bond one", "Plain bond two", "Plain bond three"),
    CouponFrequencyDescription = rep("Semiannually", 3),
    RCSCouponTypeLeaf = rep("Plain Vanilla Fixed Coupon", 3),
    CouponTypeDescription = rep("Fixed:Plain Vanilla Fixed Coupon", 3),
    DebtTypeDescription = rep("Senior Note", 3),
    InstrumentTypeDescription = rep("Note", 3),
    IsCallable = FALSE,
    IsPutable = FALSE,
    IsSinkable = FALSE,
    IsConvertible = FALSE,
    flag_short_bill_cp_like = FALSE,
    flag_central_bank_like = FALSE,
    p15_history_coverage_state = rep("broad_archive_written", 3),
    p15_history_source_package_id = rep("SRC-BASE", 3),
    p15_history_source_role = rep("broad_archive_base", 3),
    p15_available_measure_contract = rep(
      "mid_price_bid_ask_and_direct_yield_requested", 3
    ),
    p15_source_record_locator = paste0("LOC", 1:3)
  )
}

test_that("primary common evidence preserves original yields outside 1-30", {
  primary_terms <- tibble::tibble(
    request_identifier = c("ISIN1", "ISIN2", "ISIN3"),
    Instrument = c("ISIN1", "ISIN2", "ISIN3"),
    `Issue Date` = as.Date(c("2024-01-01", "2024-02-01", "2024-03-01")),
    `First Announcement Date` = as.Date(c(
      "2023-12-20", "2024-01-20", "2024-02-20"
    )),
    `Original Yield Maturity` = c(0.5, 35, 5),
    `Issue Price` = c(100, 100, 100),
    `Original Amount Issued` = c(100e6, 120e6, 80e6),
    `Face Issued Total` = c(100e6, 120e6, 80e6),
    `Original Issue Currency` = c("USD", "USD", "USD"),
    Currency = c("USD", "USD", "USD"),
    `Maturity Date` = as.Date(c("2030-01-01", "2031-02-01", "2032-03-01")),
    source_archive_member = rep("primary.csv", 3),
    source_member_row = 1:3
  )
  context <- tibble::tibble(
    desktop_identifier = c("ISIN1", "ISIN2", "ISIN3"),
    desktop_identifier_type = rep("ISIN", 3),
    ISIN = c("ISIN1", "ISIN2", "ISIN3"),
    RIC = c("RIC1", "RIC2", "RIC3"),
    canonical_iso3 = rep("AAA", 3),
    canonical_country = rep("Alpha", 3),
    requested_country = rep("Alpha", 3),
    IssueDate = primary_terms$`Issue Date`,
    MaturityDate = primary_terms$`Maturity Date`,
    DocumentTitle = rep("Plain bond", 3)
  )

  issues <- p15_build_primary_issue_evidence_all_years(
    primary_terms, context, test_instrument_universe(), test_grid()
  )
  expect_equal(nrow(issues), 3L)
  expect_true(all(c(0.5, 35) %in% issues$original_issue_yield_pct))
  expect_true(all(
    issues$candidate_primary_standard[
      issues$original_issue_yield_pct %in% c(0.5, 35)
    ]
  ))
  expect_false(any(
    issues$candidate_primary_predecessor_1_30_50m[
      issues$original_issue_yield_pct %in% c(0.5, 35)
    ]
  ))
  expect_equal(
    issues$aggregation_weight_basis[issues$original_issue_yield_pct == 5],
    "source_currency_amount_fallback_not_cross_currency_comparable"
  )
  expect_false(
    issues$candidate_primary_standard[issues$original_issue_yield_pct == 5]
  )

  candidates <- p15_aggregate_primary_candidate_variants(issues)
  expect_true(
    "primary_standard_usd_eur_50m" %in% candidates$candidate_variant_id
  )
  expect_false(
    "primary_predecessor_1_30_usd_eur_50m" %in%
      candidates$candidate_variant_id
  )
})

test_that("secondary common evidence uses latest direct quote without deleting tails", {
  history <- tibble::tibble(
    analysis_year = c(2024L, 2024L, 2024L, 2024L),
    history_date = as.Date(c(
      "2024-12-20", "2024-12-31", "2024-12-31", "2024-12-31"
    )),
    source_subrow = 1L,
    RIC = c("RIC1", "RIC1", "RIC2", "RIC3"),
    mid_price = c(99, 98, 70, 101),
    bid = c(98, 97, 69, 100),
    ask = c(100, 99, 71, 102),
    yield_to_maturity = c(4, 35, 0.5, NA_real_),
    any_observed_measure = TRUE,
    source_package_id = "SRC-BASE",
    source_role = "broad_archive_base",
    source_record_locator = paste0("H", 1:4)
  )

  identifiers <- p15_build_secondary_identifier_evidence_all_years(
    test_instrument_universe(), history, test_grid()
  )
  expect_equal(
    identifiers$direct_yield_pct[identifiers$ric == "RIC1"], 35
  )
  expect_equal(
    identifiers$secondary_identifier_evidence_state[
      identifiers$ric == "RIC3"
    ],
    "price_available_without_direct_ytm"
  )

  issues <- p15_build_secondary_issue_evidence_all_years(identifiers)
  expect_true(
    issues$candidate_secondary_usd_2_15[
      which(issues$direct_yield_pct == 35)
    ]
  )
  expect_false(
    issues$candidate_secondary_predecessor_1_30_usd_2_15[
      which(issues$direct_yield_pct == 35)
    ]
  )
  expect_true(
    issues$candidate_secondary_usd_2_15[
      which(issues$direct_yield_pct == 0.5)
    ]
  )

  candidates <- p15_aggregate_secondary_candidate_variants(issues)
  standard <- candidates |>
    dplyr::filter(
      .data$candidate_variant_id == "secondary_standard_usd_2_15_direct"
    )
  expect_equal(nrow(standard), 1L)
  expect_equal(standard$issue_count, 2L)

  availability <- p15_build_observed_availability_ledgers(
    test_grid(),
    p15_build_primary_issue_evidence_all_years(
      tibble::tibble(
        request_identifier = "ISIN1", Instrument = "ISIN1",
        `Issue Date` = as.Date("2024-01-01"),
        `First Announcement Date` = as.Date("2023-12-20"),
        `Original Yield Maturity` = 5, `Issue Price` = 100,
        `Original Amount Issued` = 100e6, `Face Issued Total` = 100e6,
        `Original Issue Currency` = "USD", Currency = "USD",
        `Maturity Date` = as.Date("2030-01-01"),
        source_archive_member = "primary.csv", source_member_row = 1L
      ),
      tibble::tibble(
        desktop_identifier = "ISIN1", desktop_identifier_type = "ISIN",
        ISIN = "ISIN1", RIC = "RIC1", canonical_iso3 = "AAA",
        canonical_country = "Alpha", requested_country = "Alpha",
        IssueDate = as.Date("2024-01-01"),
        MaturityDate = as.Date("2030-01-01"), DocumentTitle = "Plain bond"
      ),
      test_instrument_universe(), test_grid()
    ),
    identifiers,
    issues
  )
  expect_true(all(availability$primary$automatic_consequence == "none"))
  expect_true(all(availability$secondary$automatic_consequence == "none"))
})

test_that("primary diagnostics distinguish identifier collapse from country concentration", {
  issues <- tibble::tibble(
    analysis_year = c(2024L, 2024L), iso3 = "AAA", country = "Alpha",
    issue_date = as.Date(c("2024-01-01", "2024-01-01")),
    maturity_date = as.Date(c("2030-01-01", "2030-01-01")),
    currency = "USD", coupon_rate_pct = 5,
    economic_issue_key = c("ISSUE-A", "ISSUE-B"),
    original_issue_yield_pct = c(5, 5.02),
    original_maturity_years = 6, face_issued_usd = c(100e6, 20e6),
    aggregation_weight_basis = "face_issued_usd",
    request_identifier_count = c(2L, 1L),
    request_identifiers = c("A1;A2", "B1"),
    representative_isins = c("A1;A2", "B1"),
    direct_yield_range_bps = c(0, 0), source_object_standard = TRUE,
    usd_weight_available = TRUE, identifier_yield_conflict = FALSE,
    source_record_locators = c("L1;L2", "L3")
  )
  candidates <- tibble::tibble(
    analysis_year = 2024L, iso3 = "AAA", country = "Alpha",
    candidate_variant_id = "primary_standard_usd_eur_50m",
    market_rate_pct = 5.003, market_maturity_years = 6,
    issue_count = 2L, largest_issue_weight_share = 100 / 120,
    min_issue_rate_pct = 5, max_issue_rate_pct = 5.02,
    currency_basis = "USD", included_issue_keys = "ISSUE-A;ISSUE-B",
    included_isins = "A1;A2;B1", source_package_ids = "SRC"
  )
  out <- p15_build_primary_aggregation_diagnostics(issues, candidates)
  expect_equal(nrow(out$reopening_families), 1L)
  expect_equal(
    out$reopening_families$reopening_diagnostic_class,
    "potential_reopening_or_tranche_similar_yield"
  )
  expect_equal(nrow(out$country_casebook), 1L)
  expect_true(out$country_casebook$dominant_issue_flag)
  expect_match(out$country_casebook$review_reason_codes, "ge_80pct")
})
