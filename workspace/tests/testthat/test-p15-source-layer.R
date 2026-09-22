source("../../R/p15_source_layer.R")

testthat::test_that("P15 normalizes price and direct-yield chunks without changing meaning", {
  x <- data.frame(
    Date = c("2024-12-30", "2024-12-31"),
    `RIC1=__MID_PRICE` = c(99, 100),
    `RIC1=__BID` = c(98.5, 99.5),
    `RIC1=__ASK` = c(99.5, 100.5),
    `RIC1=__Yield to Maturity` = c(5.1, 5.0),
    snapshot_year = 2024,
    history_window_start = "2024-11-16",
    history_window_end = "2025-01-07",
    chunk_no = 1,
    run_id = "run-one",
    history_fields_requested = "MID_PRICE | BID | ASK | TR.YieldToMaturity",
    check.names = FALSE
  )
  out <- p15_normalize_secondary_chunk(x, "SRC-BASE", "base", "member.csv")
  testthat::expect_equal(nrow(out), 2)
  testthat::expect_equal(out$yield_to_maturity, c(5.1, 5.0))
  testthat::expect_equal(out$RIC, rep("RIC1=", 2))
  testthat::expect_true(all(out$any_observed_measure))
  testthat::expect_true(p15_validate_history_layer(out))
})

testthat::test_that("P15 keeps price-only supplement rows explicitly price-only", {
  x <- data.frame(
    Date = "2024-12-31",
    `RIC2=__MID_PRICE` = 95,
    `RIC2=__BID` = 94.5,
    `RIC2=__ASK` = 95.5,
    snapshot_year = 2024,
    chunk_no = 3,
    run_id = "reference-run",
    history_fields_requested = "MID_PRICE | BID | ASK",
    check.names = FALSE
  )
  out <- p15_normalize_secondary_chunk(
    x,
    "SRC-REFERENCE",
    "2024_tail_fill",
    "reference-member.csv"
  )
  testthat::expect_true(is.na(out$yield_to_maturity))
  testthat::expect_equal(out$available_measure_set, "mid_price;bid;ask")
  testthat::expect_equal(out$source_role, "2024_tail_fill")
})

testthat::test_that("P15 filters supplement chunks to the requested RICs", {
  x <- data.frame(
    Date = "2024-12-31",
    `KEEP=__MID_PRICE` = 99,
    `DROP=__MID_PRICE` = 101,
    snapshot_year = 2024,
    check.names = FALSE
  )
  out <- p15_normalize_secondary_chunk(
    x,
    "SRC-REFERENCE",
    "2024_tail_fill",
    "reference-member.csv",
    keep_rics = "KEEP="
  )
  testthat::expect_equal(unique(out$RIC), "KEEP=")
})

testthat::test_that("P15 validation rejects duplicate observation keys", {
  x <- p15_empty_history()
  x[1:2, ] <- NA
  x$analysis_year <- 2024
  x$history_date <- "2024-12-31"
  x$RIC <- "RIC="
  x$source_package_id <- "SRC"
  x$source_record_locator <- c("one", "two")
  testthat::expect_error(p15_validate_history_layer(x), "duplicate")
})
