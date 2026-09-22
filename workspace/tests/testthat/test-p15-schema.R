source("../../R/p15_schema.R")

p15_schema_test_classification <- function() {
  data.frame(
    analysis_year = rep(2012:2013, each = 2),
    period = "test",
    iso3 = rep(c("AAA", "XKX"), 2),
    country = rep(c("Source A", "Kosovo"), 2),
    historical_income_level = c(
      "Low income", "Lower middle income",
      "Lower middle income", "Upper middle income"
    ),
    historical_lending_type = c("IDA", "IDA", "IBRD", "IDA"),
    historical_lmic_reporting_scope = TRUE,
    static_income_level = "Upper middle income",
    static_lending_type = c("IBRD", "IDA", "IBRD", "IDA"),
    static_lmic_reporting_scope = TRUE,
    static_vs_historical_classification_conflict =
      "static_income_label_differs_from_historical_source",
    classification_source_pointer = "synthetic",
    classification_confidence = "source_backed_world_bank_oghist",
    classification_review_state = "source_backed_income_scope_resolved",
    lending_type_source_state = "static_descriptive_only",
    source_income_disagreement_flag = FALSE,
    wb_income_code = c("L", "LM", "LM", "UM"),
    owid_income_raw = NA_character_,
    stringsAsFactors = FALSE
  )
}

testthat::test_that("P15 grid preserves historical labels and stable canonical names", {
  lookup <- data.frame(
    iso3 = c("AAA", "XKX"),
    country = c("Canonical A", "Kosovo")
  )
  grid <- p15_build_country_year_grid(
    p15_schema_test_classification(),
    years = 2012:2013,
    canonical_country_lookup = lookup,
    expected_countries_per_year = 2L
  )

  testthat::expect_equal(nrow(grid), 4)
  testthat::expect_equal(unique(grid$country[grid$iso3 == "AAA"]), "Canonical A")
  testthat::expect_equal(
    grid$historical_income_level[grid$iso3 == "AAA"],
    c("Low income", "Lower middle income")
  )
  testthat::expect_true("project_lending_type_static" %in% names(grid))
  testthat::expect_false("historical_lending_type" %in% names(grid))
  testthat::expect_true(all(grid$country_year_id == paste(
    grid$analysis_year, grid$iso3, sep = "::"
  )))
})

testthat::test_that("P15 neutral ladder schema separates evidence from selection", {
  prototype <- p15_empty_neutral_ladder()
  dictionary <- p15_neutral_schema_dictionary()

  testthat::expect_silent(p15_validate_neutral_ladder(prototype))
  testthat::expect_true(all(c(
    "source_object_id", "evidence_family", "rate_availability_state",
    "admissibility_state", "selection_variant_id", "selection_state"
  ) %in% names(prototype)))
  testthat::expect_equal(
    dictionary$layer[dictionary$field == "rate_pct"], "evidence"
  )
  testthat::expect_equal(
    dictionary$layer[dictionary$field == "selection_state"], "selection"
  )
})

testthat::test_that("P15 grid validation rejects duplicate country-year keys", {
  lookup <- data.frame(
    iso3 = c("AAA", "XKX"),
    country = c("Canonical A", "Kosovo")
  )
  grid <- p15_build_country_year_grid(
    p15_schema_test_classification(),
    years = 2012:2013,
    canonical_country_lookup = lookup,
    expected_countries_per_year = 2L
  )
  duplicate <- rbind(grid, grid[1, ])
  testthat::expect_error(
    p15_validate_country_year_grid(
      duplicate,
      years = 2012:2013,
      expected_countries_per_year = 2L
    ),
    "duplicate"
  )
})
