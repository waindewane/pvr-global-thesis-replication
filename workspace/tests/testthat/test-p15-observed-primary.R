source("../../R/lseg_benchmarks.R")
source("../../R/p15_observed_primary.R")

p15_primary_test_metadata <- function() {
  p15_prepare_primary_country_metadata(data.frame(
    iso3 = "TST",
    country = "Testland",
    is_country = TRUE,
    income_level = "Lower middle income",
    lending_type = "IDA",
    stringsAsFactors = FALSE
  ))
}

p15_primary_test_row <- function(
    isin,
    issue_date,
    maturity_date,
    original_yield,
    face_issued,
    issue_price = 100,
    coupon_rate = 5) {
  data.frame(
    IssueDate = issue_date,
    MaturityDate = maturity_date,
    CouponRate = coupon_rate,
    IssuePrice = issue_price,
    MaturityStandardYield = 99,
    OriginalYieldMaturity = original_yield,
    FaceIssuedUSD = face_issued,
    FaceOutstandingUSD = NA_real_,
    original_maturity_days = as.numeric(
      as.Date(maturity_date) - as.Date(issue_date)
    ),
    canonical_country = "Testland",
    requested_country = "Testland",
    CouponFrequencyDescription = "Semi Annual",
    DocumentTitle = "Testland fixed coupon bond",
    DebtTypeDescription = "Bond",
    InstrumentTypeDescription = "Bond",
    RCSCouponTypeLeaf = "Plain Vanilla Fixed Coupon",
    CouponTypeDescription = "Fixed",
    IssuerName = "Republic of Testland",
    IssuerCommonName = "Testland",
    Currency = "USD",
    flag_short_bill_cp_like = FALSE,
    flag_central_bank_like = FALSE,
    is_usd_eur_scope = TRUE,
    ISIN = isin,
    RIC = paste0(isin, "="),
    stringsAsFactors = FALSE
  )
}

testthat::test_that("P15 primary parity mode preserves original-yield precedence", {
  rows <- p15_primary_test_row(
    "TST001", "2024-01-01", "2034-01-01", 6, 100000000,
    issue_price = 80, coupon_rate = 5
  )
  audit <- p15_build_primary_issue_audit(
    rows,
    p15_primary_test_metadata(),
    source_artifact = "synthetic.csv",
    source_file = "synthetic.csv",
    source_package_id = "TEST"
  )

  testthat::expect_equal(nrow(audit), 1)
  testthat::expect_equal(audit$market_rate_pct, 6)
  testthat::expect_equal(audit$yield_source, "OriginalYieldMaturity")
  testthat::expect_true(audit$selected_primary_issue)
  testthat::expect_true(is.na(audit$face_outstanding_usd))
})

testthat::test_that("P15 primary parity mode collapses duplicates before weighting", {
  row <- p15_primary_test_row(
    "TST001", "2024-01-01", "2034-01-01", 6, 100000000
  )
  rows <- rbind(row, transform(row, RIC = "TST001_ALT="))
  audit <- p15_build_primary_issue_audit(
    rows,
    p15_primary_test_metadata(),
    source_artifact = "synthetic.csv",
    source_file = "synthetic.csv",
    source_package_id = "TEST"
  )

  testthat::expect_equal(nrow(audit), 1)
  testthat::expect_equal(audit$duplicate_row_count, 2)
  testthat::expect_equal(audit$face_issued_usd, 100000000)
})

testthat::test_that("P15 primary aggregation enforces materiality and amount weights", {
  rows <- rbind(
    p15_primary_test_row(
      "TST001", "2024-01-01", "2034-01-01", 5, 100000000
    ),
    p15_primary_test_row(
      "TST002", "2024-02-01", "2034-02-01", 10, 200000000
    ),
    p15_primary_test_row(
      "TST003", "2024-03-01", "2034-03-01", 20, 40000000
    )
  )
  audit <- p15_build_primary_issue_audit(
    rows,
    p15_primary_test_metadata(),
    source_artifact = "synthetic.csv",
    source_file = "synthetic.csv",
    source_package_id = "TEST"
  )
  evidence <- p15_build_primary_country_evidence(
    audit,
    source_extraction_run_id = "TEST-RUN"
  )

  testthat::expect_equal(sum(audit$selected_primary_issue), 2)
  testthat::expect_equal(sum(audit$below_materiality_only), 1)
  testthat::expect_equal(evidence$issue_count, 2)
  testthat::expect_equal(evidence$total_eligible_issue_count, 3)
  testthat::expect_equal(evidence$market_rate_pct, (5 * 1 + 10 * 2) / 3)
  testthat::expect_true(evidence$selected_pvr_admissible)
})
