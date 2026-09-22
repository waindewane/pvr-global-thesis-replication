source("../../R/ids.R")
source("../../R/pvr.R")
source("../../R/build_outputs.R")

testthat::test_that("IDS-derived market benchmark has required fields", {
  countries <- read_country_metadata("../../data-raw/world_bank_countries.json")
  terms <- build_ids_terms("../../data-raw/ids_terms", countries)
  benchmarks <- build_market_benchmarks(terms, add_path = tempfile(fileext = ".xlsx"))
  required <- c(
    "iso3", "country", "analysis_year", "market_issue_name", "market_rate",
    "market_maturity_years", "market_repayment_type", "market_rate_source_class",
    "market_rate_source_url", "market_rate_source_note"
  )
  testthat::expect_true(all(required %in% names(benchmarks)))
  testthat::expect_true(all(benchmarks$market_rate_source_class == "world_bank_ids"))
  testthat::expect_true(nrow(benchmarks) >= 1)
})

testthat::test_that("African Debt Database market benchmark parser selects usable 2024 international bonds", {
  add_path <- "../../data-raw/market_rates/ADD_v2026Mar_b.xlsx"
  testthat::skip_if_not(file.exists(add_path))

  benchmarks <- read_add_market_benchmarks(add_path)
  kenya <- benchmarks[benchmarks$iso3 == "KEN", ]

  testthat::expect_true(nrow(benchmarks) >= 8)
  testthat::expect_equal(nrow(kenya), 1)
  testthat::expect_equal(round(kenya$market_rate, 2), 10.30)
  testthat::expect_equal(kenya$currency, "USD")
  testthat::expect_true(all(benchmarks$market_rate_source_class == "african_debt_database_public"))
})

testthat::test_that("Kenya IDS files parse with expected 2024 IBRD values", {
  countries <- read_country_metadata("../../data-raw/world_bank_countries.json")
  terms <- build_ids_terms("../../data-raw/ids_terms", countries)
  kenya_ibrd <- terms[terms$iso3 == "KEN" & terms$creditor == "IBRD", ]
  testthat::expect_equal(round(kenya_ibrd$official_rate, 2), 6.57)
  testthat::expect_equal(round(kenya_ibrd$official_maturity_years), 21)
  testthat::expect_equal(round(kenya_ibrd$official_grace_years), 8)
})
