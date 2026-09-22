# Neutral P15 panel and ladder schemas.
#
# These functions define keys and evidence containers. They do not rank evidence,
# decide admissibility, select a benchmark, or calculate PVR.

p15_country_year_grid_schema_version <- function() {
  "SCHEMA-P15-COUNTRY-YEAR-GRID-V1"
}

p15_ladder_schema_version <- function() {
  "SCHEMA-P15-NEUTRAL-LADDER-V1"
}

p15_build_country_year_grid <- function(
    classification_ledger,
    years = 2012:2024,
    geography_universe_id = "P13-211-COUNTRY-UNIVERSE-V1",
    canonical_country_lookup = NULL,
    expected_countries_per_year = 211L) {
  required <- c(
    "analysis_year", "period", "iso3", "country",
    "historical_income_level", "historical_lending_type",
    "historical_lmic_reporting_scope", "static_income_level",
    "static_lending_type", "static_lmic_reporting_scope",
    "static_vs_historical_classification_conflict",
    "classification_source_pointer", "classification_confidence",
    "classification_review_state", "lending_type_source_state",
    "source_income_disagreement_flag", "wb_income_code", "owid_income_raw"
  )
  missing <- setdiff(required, names(classification_ledger))
  if (length(missing)) {
    stop(
      "Classification ledger is missing: ", paste(missing, collapse = ", "),
      call. = FALSE
    )
  }

  input <- classification_ledger
  if (is.null(canonical_country_lookup)) {
    input$canonical_country <- as.character(input$country)
  } else {
    lookup_missing <- setdiff(c("iso3", "country"), names(canonical_country_lookup))
    if (length(lookup_missing)) {
      stop(
        "Canonical country lookup is missing: ",
        paste(lookup_missing, collapse = ", "),
        call. = FALSE
      )
    }
    lookup <- canonical_country_lookup |>
      dplyr::transmute(
        iso3 = as.character(.data$iso3),
        canonical_country = as.character(.data$country)
      ) |>
      dplyr::distinct(.data$iso3, .keep_all = TRUE)
    input <- input |>
      dplyr::left_join(lookup, by = "iso3")
    if (any(is.na(input$canonical_country))) {
      stop("Canonical country lookup does not cover every grid ISO3.", call. = FALSE)
    }
  }

  out <- input |>
    dplyr::filter(.data$analysis_year %in% years) |>
    dplyr::transmute(
      analysis_year = as.integer(.data$analysis_year),
      iso3 = as.character(.data$iso3),
      country = as.character(.data$canonical_country),
      classification_source_country = as.character(.data$country),
      country_year_id = paste(.data$analysis_year, .data$iso3, sep = "::"),
      period = as.character(.data$period),
      historical_income_level = as.character(.data$historical_income_level),
      historical_lmic_reporting_scope = as.logical(
        .data$historical_lmic_reporting_scope
      ),
      project_lending_type_static = as.character(.data$static_lending_type),
      lending_type_time_basis = as.character(.data$lending_type_source_state),
      static_2024_income_level_audit_only = as.character(.data$static_income_level),
      static_2024_lmic_scope_audit_only = as.logical(
        .data$static_lmic_reporting_scope
      ),
      static_vs_historical_classification_conflict = as.character(
        .data$static_vs_historical_classification_conflict
      ),
      classification_source_pointer = as.character(
        .data$classification_source_pointer
      ),
      classification_confidence = as.character(.data$classification_confidence),
      classification_review_state = as.character(
        .data$classification_review_state
      ),
      source_income_disagreement_flag = as.logical(
        .data$source_income_disagreement_flag
      ),
      wb_income_code = as.character(.data$wb_income_code),
      owid_income_raw = as.character(.data$owid_income_raw),
      geography_universe_id = geography_universe_id,
      income_classification_source_package_ids = dplyr::case_when(
        .data$classification_confidence == "source_backed_world_bank_oghist" ~
          "SRC-WB-OGHIST-20260630",
        .data$classification_confidence ==
          "source_backed_owid_world_bank_fill" ~
          "SRC-OWID-WB-INCOME-20260630",
        TRUE ~ NA_character_
      ),
      classification_ledger_package_id =
        "SRC-P14-CLASSIFICATION-LEDGER-20260630",
      schema_version = p15_country_year_grid_schema_version()
    ) |>
    dplyr::arrange(.data$analysis_year, .data$iso3)

  p15_validate_country_year_grid(
    out,
    years = years,
    expected_countries_per_year = expected_countries_per_year
  )
  out
}

p15_validate_country_year_grid <- function(
    grid,
    years = 2012:2024,
    expected_countries_per_year = 211L) {
  required <- c(
    "analysis_year", "iso3", "country", "country_year_id",
    "historical_income_level", "historical_lmic_reporting_scope",
    "classification_review_state", "geography_universe_id", "schema_version"
  )
  missing <- setdiff(required, names(grid))
  if (length(missing)) {
    stop("Country-year grid is missing: ", paste(missing, collapse = ", "),
         call. = FALSE)
  }
  if (anyDuplicated(grid[c("analysis_year", "iso3")])) {
    stop("Country-year grid has duplicate year-ISO3 keys.", call. = FALSE)
  }
  if (any(is.na(grid$iso3) | nchar(grid$iso3) != 3L)) {
    stop("Country-year grid contains an invalid ISO3 key.", call. = FALSE)
  }
  if (!identical(sort(unique(grid$analysis_year)), as.integer(sort(years)))) {
    stop("Country-year grid does not contain the required years.", call. = FALSE)
  }
  counts <- table(grid$analysis_year)
  if (any(as.integer(counts) != expected_countries_per_year)) {
    stop("Country-year grid does not contain 211 countries in every year.",
         call. = FALSE)
  }
  expected_ids <- paste(grid$analysis_year, grid$iso3, sep = "::")
  if (!identical(as.character(grid$country_year_id), expected_ids)) {
    stop("Country-year IDs do not match year and ISO3 keys.", call. = FALSE)
  }
  if (!all(grid$schema_version == p15_country_year_grid_schema_version())) {
    stop("Country-year grid schema version drifted.", call. = FALSE)
  }
  if (!any(grid$iso3 == "XKX")) {
    stop("Country-year grid is missing the approved Kosovo/XKX mapping.",
         call. = FALSE)
  }
  invisible(TRUE)
}

p15_empty_neutral_ladder <- function() {
  tibble::tibble(
    analysis_year = integer(),
    iso3 = character(),
    country_year_id = character(),
    rate_value_id = character(),
    source_object_id = character(),
    source_object_type = character(),
    source_package_id = character(),
    source_record_locator = character(),
    evidence_family = character(),
    major_tier_id = character(),
    detailed_tier_id = character(),
    method_id = character(),
    rate_pct = numeric(),
    maturity_years = numeric(),
    rate_availability_state = character(),
    issue_count = integer(),
    total_weight_usd = numeric(),
    currency_basis = character(),
    observation_start = as.Date(character()),
    observation_end = as.Date(character()),
    status_state = character(),
    admissibility_state = character(),
    admissibility_reason = character(),
    selection_variant_id = character(),
    selection_state = character(),
    warning_ids = character(),
    method_version = character(),
    schema_version = character()
  )
}

p15_validate_neutral_ladder <- function(ladder) {
  required <- names(p15_empty_neutral_ladder())
  missing <- setdiff(required, names(ladder))
  if (length(missing)) {
    stop("Neutral ladder is missing: ", paste(missing, collapse = ", "),
         call. = FALSE)
  }
  if (nrow(ladder)) {
    if (anyDuplicated(ladder[c("analysis_year", "iso3", "rate_value_id")])) {
      stop("Neutral ladder has duplicate country-year-rate keys.", call. = FALSE)
    }
    expected_country_year_id <- paste(
      ladder$analysis_year, ladder$iso3, sep = "::"
    )
    if (!identical(as.character(ladder$country_year_id), expected_country_year_id)) {
      stop("Neutral ladder country-year IDs are inconsistent.", call. = FALSE)
    }
    if (!all(ladder$schema_version == p15_ladder_schema_version())) {
      stop("Neutral ladder schema version drifted.", call. = FALSE)
    }
  }
  invisible(TRUE)
}

p15_neutral_schema_dictionary <- function() {
  prototype <- p15_empty_neutral_ladder()
  types <- vapply(prototype, function(x) class(x)[[1]], character(1))
  descriptions <- c(
    analysis_year = "Calendar year of the country-year evidence row.",
    iso3 = "Stable project ISO3 geography key, including XKX for Kosovo.",
    country_year_id = "Deterministic analysis_year::iso3 key.",
    rate_value_id = "Stable identifier for one labelled rate value.",
    source_object_id = "Stable identifier for the underlying evidence object.",
    source_object_type = "Typed source object; never inferred from ladder rank.",
    source_package_id = "Registered immutable input package identifier.",
    source_record_locator = "Row, issue, identifier, or file locator within source.",
    evidence_family = "Neutral empirical family; not a selection rank.",
    major_tier_id = "Versioned broad ladder category.",
    detailed_tier_id = "Versioned detailed method/source category.",
    method_id = "Versioned computation method identifier.",
    rate_pct = "Rate in percentage points; decimals are not stored here.",
    maturity_years = "Associated maturity or tenor in years.",
    rate_availability_state = "Reason a numeric rate is present or unavailable.",
    issue_count = "Number of economic issues represented, where applicable.",
    total_weight_usd = "Aggregation weight total in US dollars, where applicable.",
    currency_basis = "Currency set or conversion basis used by the method.",
    observation_start = "Start of the evidence observation window.",
    observation_end = "End or as-of date of the evidence observation window.",
    status_state = "Separate country-year market-access/status evidence state.",
    admissibility_state = "Outcome under a named admissibility specification.",
    admissibility_reason = "Machine-readable reason for admissibility outcome.",
    selection_variant_id = "Named benchmark-selection variant, if evaluated.",
    selection_state = "Selected, alternative, blocked, or not evaluated.",
    warning_ids = "Semicolon-separated stable warning identifiers.",
    method_version = "Complete estimation/admissibility/selection version bundle.",
    schema_version = "Neutral ladder schema version."
  )
  tibble::tibble(
    field = names(prototype),
    storage_type = unname(types),
    required_column = TRUE,
    layer = dplyr::case_when(
      field %in% c("analysis_year", "iso3", "country_year_id") ~ "key",
      field %in% c(
        "source_object_id", "source_object_type", "source_package_id",
        "source_record_locator"
      ) ~ "provenance",
      field %in% c(
        "evidence_family", "major_tier_id", "detailed_tier_id", "method_id",
        "rate_pct", "maturity_years", "rate_availability_state", "issue_count",
        "total_weight_usd", "currency_basis", "observation_start",
        "observation_end"
      ) ~ "evidence",
      field %in% c("status_state") ~ "status",
      field %in% c("admissibility_state", "admissibility_reason") ~
        "admissibility",
      field %in% c("selection_variant_id", "selection_state") ~ "selection",
      field %in% c("warning_ids") ~ "warning",
      TRUE ~ "version"
    ),
    description = unname(descriptions[field]),
    schema_version = p15_ladder_schema_version()
  )
}

p15_neutral_taxonomy <- function() {
  tibble::tribble(
    ~field, ~value, ~plain_language_meaning,
    "source_object_type", "primary_issue", "A bond observed at original issuance.",
    "source_object_type", "secondary_issue_quote", "A bond price or yield observed after issuance.",
    "source_object_type", "creditor_country_year_terms", "A creditor-country-year contractual terms object.",
    "source_object_type", "rating_component", "A rating, spread, or risk-free component used in a model.",
    "source_object_type", "peer_pool", "A target-excluded comparison-country pool.",
    "source_object_type", "policy_reference", "A policy or grant-element reference outside market evidence.",
    "evidence_family", "observed_primary_market", "Observed original-issuance borrowing cost.",
    "evidence_family", "observed_secondary_direct", "Observed secondary-market yield supplied directly by the source.",
    "evidence_family", "observed_secondary_derived", "Secondary-market yield derived from a price under a named method.",
    "evidence_family", "ids_contractual_proxy", "World Bank IDS contractual bondholder terms used as a proxy.",
    "evidence_family", "rating_implied_model", "Model-implied rate using rating and spread components.",
    "evidence_family", "peer_proxy_model", "Rate inferred from a target-excluded peer pool.",
    "evidence_family", "policy_reference", "Non-market policy reference retained outside ordinary benchmark selection.",
    "rate_availability_state", "numeric_observed", "A numeric observed rate is present.",
    "rate_availability_state", "numeric_derived", "A numeric rate was computed from other observed inputs.",
    "rate_availability_state", "no_instrument_stock", "No qualifying instrument stock was identified.",
    "rate_availability_state", "no_quote", "Qualifying instrument exists but no in-window quote is present.",
    "rate_availability_state", "missing_required_field", "Evidence exists but a required field is missing.",
    "rate_availability_state", "outside_window", "Evidence exists only outside the method's time or maturity window.",
    "rate_availability_state", "excluded_by_rule", "A numeric input exists but fails a named evidence rule.",
    "rate_availability_state", "source_scope_limit", "The source package did not request or cover the required field.",
    "status_state", "no_status_gate", "No source-backed status gate applies.",
    "status_state", "status_review_pending", "Status evidence requires review before selection.",
    "status_state", "status_blocked", "A source-backed status rule blocks ordinary selection.",
    "status_state", "status_not_evaluated", "Status consequences have not yet been evaluated.",
    "admissibility_state", "permissible", "Passes the named admissibility specification.",
    "admissibility_state", "diagnostic_only", "Retained for analysis but not eligible for selection.",
    "admissibility_state", "blocked", "Fails a named admissibility or status rule.",
    "admissibility_state", "not_evaluated", "No admissibility specification has yet been applied.",
    "selection_state", "selected", "Chosen under the named selection variant.",
    "selection_state", "permissible_alternative", "Permissible but not selected under the variant.",
    "selection_state", "blocked_alternative", "Considered but blocked under the variant.",
    "selection_state", "not_evaluated", "Selection has not yet been evaluated."
  ) |>
    dplyr::mutate(schema_version = p15_ladder_schema_version())
}
