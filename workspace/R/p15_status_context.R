# P15 status and market-access source-gap helpers.
#
# These functions inventory evidence availability. They do not decide whether a
# status should warn, block, or change benchmark selection.

p15_status_gap_schema_version <- function() {
  "SCHEMA-P15-STATUS-SOURCE-GAP-V1"
}

p15_required_status_dimensions <- function() {
  tibble::tribble(
    ~status_dimension, ~plain_language_question,
    "sovereign_default", "Was sovereign debt recorded in default in the country-year?",
    "fiscal_arrears", "Were fiscal arrears recorded in the country-year?",
    "official_restructuring", "Was an official bilateral debt treatment or restructuring recorded?",
    "private_or_bond_restructuring", "Was private-creditor or bond debt in default or restructuring?",
    "sanctions", "Did sanctions materially affect ordinary sovereign market access?",
    "war_or_conflict", "Did war or conflict materially affect ordinary market interpretation?",
    "capital_controls", "Did capital controls materially constrain sovereign market access or pricing?",
    "no_qualifying_issuance", "Was no qualifying primary issuance observed in the project source?",
    "no_qualifying_traded_stock", "Was no qualifying hard-currency bond stock identified in the project source?",
    "quote_missing", "Did a qualifying instrument exist without a usable in-window quote?",
    "priced_out_or_no_access", "Is there affirmative evidence that the sovereign was priced out or lacked access?",
    "general_market_access", "What is the country-year's source-backed market-access state?",
    "debt_distress", "Was the country in a source-backed debt-distress state?"
  ) |>
    dplyr::mutate(schema_version = p15_status_gap_schema_version())
}

p15_status_evidence_schema_version <- function() {
  "SCHEMA-P15-STATUS-EVIDENCE-V1"
}

p15_status_clean_text <- function(x) {
  out <- trimws(as.character(x))
  out[out == ""] <- NA_character_
  out
}

p15_status_parse_num <- function(x) {
  suppressWarnings(as.numeric(gsub(",", "", as.character(x), fixed = TRUE)))
}

p15_status_collapse_unique <- function(x, max_items = Inf) {
  values <- unique(p15_status_clean_text(x))
  values <- values[!is.na(values)]
  if (!length(values)) return(NA_character_)
  paste(utils::head(values, max_items), collapse = ";")
}

p15_parse_boc_boe_default <- function(json_path, years = 2012:2024) {
  stopifnot(file.exists(json_path))
  extract_value <- function(observation, key) {
    entry <- observation[[key]]
    if (is.null(entry) || is.null(entry[["v"]])) return(NA_character_)
    as.character(entry[["v"]])
  }
  extract_number <- function(observation, key) {
    p15_status_parse_num(extract_value(observation, key))
  }
  custom_match <- c(
    "Bolivia" = "BOL", "Bosnia & Herzegovina" = "BIH",
    "Côte d’Ivoire" = "CIV",
    "Democratic Republic of Congo (Kinshasa)" = "COD",
    "eSwatini" = "SWZ", "eSwatini (Swaziland)" = "SWZ",
    "Egypt" = "EGY", "Gambia" = "GMB", "Iran" = "IRN",
    "Korea (North)" = "PRK", "Kyrgyz Republic" = "KGZ",
    "Kyrgyzstan" = "KGZ", "Laos" = "LAO", "Macedonia" = "MKD",
    "Moldova" = "MDA", "Republic of Congo (Brazzaville)" = "COG",
    "São Tomé and Príncipe" = "STP", "St. Kitts & Nevis" = "KNA",
    "Syria" = "SYR", "USSR/Russia" = "RUS", "Venezuela" = "VEN",
    "Vietnam" = "VNM"
  )
  raw <- jsonlite::fromJSON(json_path, simplifyVector = FALSE)
  observations <- dplyr::bind_rows(lapply(raw$observations, function(obs) {
    tibble::tibble(
      boc_country = extract_value(obs, "DEBT_COUNTRY"),
      boc_country_group = extract_value(obs, "DEBT_COUNTRY_GROUP"),
      analysis_year = extract_number(obs, "DEBT_YEAR"),
      boc_total_default_debt_usd_mn = extract_number(obs, "DEBT_TOTAL_2025"),
      boc_private_creditors_default_debt_usd_mn = extract_number(obs, "DEBT_PRIVATE_CREDITORS_2025"),
      boc_fc_bank_loans_default_debt_usd_mn = extract_number(obs, "DEBT_FC_BANK_LOANS_2025"),
      boc_fc_bonds_default_debt_usd_mn = extract_number(obs, "DEBT_FC_BONDS_2025"),
      boc_local_currency_debt_default_usd_mn = extract_number(obs, "DEBT_LC_DEBT_2025"),
      boc_fiscal_arrears_usd_mn = extract_number(obs, "DEBT_FISCAL_ARREARS_2025"),
      boc_paris_club_default_debt_usd_mn = extract_number(obs, "DEBT_PARIS_CLUB_2025"),
      boc_china_default_debt_usd_mn = extract_number(obs, "DEBT_CHINA_2025"),
      boc_imf_default_debt_usd_mn = extract_number(obs, "DEBT_IMF_2025"),
      boc_total_defaulted_sovereign_count = extract_number(obs, "DEBT_TOTAL_DEF_SOVEREIGNS_2025"),
      boc_fc_bonds_defaulted_sovereign_count = extract_number(obs, "DEBT_FC_BONDS_DEF_SOVEREIGNS_2025"),
      boc_private_creditors_defaulted_sovereign_count = extract_number(obs, "DEBT_PRIVATE_CREDITORS_DEF_SOVEREIGNS_2025")
    )
  })) |>
    dplyr::filter(
      .data$analysis_year %in% years,
      !is.na(.data$boc_country),
      .data$boc_country != "World"
    ) |>
    dplyr::mutate(
      iso3 = countrycode::countrycode(
        .data$boc_country, "country.name", "iso3c",
        custom_match = custom_match, warn = FALSE
      )
    )

  observations |>
    dplyr::filter(!is.na(.data$iso3)) |>
    dplyr::group_by(.data$analysis_year, .data$iso3) |>
    dplyr::summarise(
      boc_country_names = p15_status_collapse_unique(.data$boc_country),
      boc_country_groups = p15_status_collapse_unique(.data$boc_country_group),
      dplyr::across(
        dplyr::starts_with("boc_") & where(is.numeric),
        ~ sum(.x, na.rm = TRUE)
      ),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      boc_default_context_flag = .data$boc_total_default_debt_usd_mn > 0,
      boc_private_or_bond_default_context_flag =
        .data$boc_private_creditors_default_debt_usd_mn > 0 |
        .data$boc_fc_bonds_default_debt_usd_mn > 0,
      boc_official_default_context_flag =
        .data$boc_paris_club_default_debt_usd_mn > 0 |
        .data$boc_china_default_debt_usd_mn > 0 |
        .data$boc_imf_default_debt_usd_mn > 0,
      boc_fiscal_arrears_context_flag = .data$boc_fiscal_arrears_usd_mn > 0,
      boc_default_context_strength = dplyr::case_when(
        .data$boc_private_or_bond_default_context_flag ~
          "source_backed_private_or_bond_default_context",
        .data$boc_official_default_context_flag |
          .data$boc_total_default_debt_usd_mn > 0 ~
          "source_backed_official_or_residual_default_context",
        TRUE ~ "no_boc_boe_default_stock_recorded"
      ),
      boc_default_source_package_id = "SRC-BOC-BOE-DEBT-2025-20260630",
      boc_default_source_pointer =
        "Bank of Canada-Bank of England Sovereign Default Database 2025, Valet group DEBT_2025"
    ) |>
    dplyr::arrange(.data$analysis_year, .data$iso3)
}

p15_parse_paris_club_agreements <- function(csv_path, years = 2012:2024) {
  stopifnot(file.exists(csv_path))
  custom_match <- c(
    "BOLIVIA" = "BOL", "BOSNIA AND HERZEGOVINA" = "BIH",
    "CONGO" = "COG", "COTE D IVOIRE" = "CIV", "COTE D'IVOIRE" = "CIV",
    "DEMOCRATIC REPUBLIC OF CONGO" = "COD", "DRC" = "COD", "FIJI" = "FJI",
    "KIRGHIZIE" = "KGZ", "KYRGYZ REPUBLIC" = "KGZ", "LAOS" = "LAO",
    "MACEDONIA" = "MKD", "MOLDOVA" = "MDA", "MYANMAR" = "MMR",
    "REPUBLIC OF CONGO" = "COG", "SOMALIA<" = "SOM", "TCHAD" = "TCD",
    "TURKIYE" = "TUR", "TÜRKIYE" = "TUR", "VIETNAM" = "VNM",
    "YEMEN" = "YEM"
  )
  readr::read_csv(csv_path, show_col_types = FALSE) |>
    dplyr::mutate(
      agreement_year = p15_status_parse_num(.data$agreement_year),
      agreement_date = as.Date(.data$agreement_date),
      debtor_country_clean = p15_status_clean_text(.data$debtor_country),
      debtor_country_match_key = toupper(iconv(
        .data$debtor_country_clean, from = "", to = "ASCII//TRANSLIT"
      )),
      debtor_country_match_key = gsub(
        "[^A-Z0-9]+", " ", .data$debtor_country_match_key
      ),
      debtor_country_match_key = gsub(
        "\\s+", " ", trimws(.data$debtor_country_match_key)
      ),
      iso3 = countrycode::countrycode(
        .data$debtor_country_clean, "country.name", "iso3c",
        custom_match = custom_match, warn = FALSE
      ),
      iso3 = ifelse(
        is.na(.data$iso3),
        unname(custom_match[.data$debtor_country_match_key]),
        .data$iso3
      ),
      treatment_type_normalized = p15_status_clean_text(
        .data$treatment_type_normalized
      ),
      treatment_status = p15_status_clean_text(.data$treatment_status),
      detail_url = p15_status_clean_text(.data$detail_url)
    ) |>
    dplyr::filter(.data$agreement_year %in% years, !is.na(.data$iso3)) |>
    dplyr::group_by(analysis_year = .data$agreement_year, .data$iso3) |>
    dplyr::summarise(
      paris_club_agreement_count = dplyr::n(),
      paris_club_agreement_dates = p15_status_collapse_unique(
        as.character(.data$agreement_date)
      ),
      paris_club_treatment_types = p15_status_collapse_unique(
        .data$treatment_type_normalized
      ),
      paris_club_treatment_statuses = p15_status_collapse_unique(
        .data$treatment_status
      ),
      paris_club_debtor_country_names = p15_status_collapse_unique(
        .data$debtor_country_clean
      ),
      paris_club_detail_urls = p15_status_collapse_unique(
        .data$detail_url, max_items = 5
      ),
      paris_club_has_dssi = any(grepl(
        "DSSI|ISSD", .data$treatment_type_normalized, ignore.case = TRUE
      )),
      paris_club_has_common_framework = any(grepl(
        "Common Framework", .data$treatment_type_normalized, ignore.case = TRUE
      )),
      paris_club_has_non_dssi_treatment = any(!grepl(
        "DSSI|ISSD", .data$treatment_type_normalized, ignore.case = TRUE
      )),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      paris_club_same_year_agreement_flag =
        .data$paris_club_agreement_count > 0,
      paris_club_official_treatment_context_strength = dplyr::case_when(
        .data$paris_club_has_common_framework ~
          "source_backed_common_framework_official_treatment",
        .data$paris_club_has_non_dssi_treatment ~
          "source_backed_non_dssi_paris_club_official_treatment",
        .data$paris_club_has_dssi ~
          "source_backed_dssi_official_suspension_context",
        TRUE ~ "no_paris_club_agreement_recorded"
      ),
      paris_club_source_package_id = "SRC-PARIS-CLUB-AGREEMENTS-20260630",
      paris_club_source_pointer =
        "Paris Club signed agreements advanced search; locally preserved public extract"
    ) |>
    dplyr::arrange(.data$analysis_year, .data$iso3)
}

p15_build_status_context_evidence <- function(
    grid, boc_default, paris_club, p13_case_evidence) {
  required_grid <- c(
    "analysis_year", "iso3", "country", "country_year_id",
    "historical_income_level", "historical_lmic_reporting_scope"
  )
  missing <- setdiff(required_grid, names(grid))
  if (length(missing)) {
    stop("Status evidence grid is missing: ", paste(missing, collapse = ", "),
         call. = FALSE)
  }
  case_by_country <- p13_case_evidence |>
    dplyr::group_by(.data$analysis_year, .data$iso3) |>
    dplyr::summarise(
      p13_case_evidence_row_count = dplyr::n(),
      p13_case_status_categories = p15_status_collapse_unique(
        .data$status_category
      ),
      p13_case_status_values = p15_status_collapse_unique(.data$status_value),
      p13_case_market_access_implications = p15_status_collapse_unique(
        .data$market_access_implication
      ),
      p13_case_source_names = p15_status_collapse_unique(.data$source_name),
      p13_case_source_locators = p15_status_collapse_unique(
        .data$source_locator, max_items = 10
      ),
      .groups = "drop"
    )

  out <- grid |>
    dplyr::left_join(boc_default, by = c("analysis_year", "iso3")) |>
    dplyr::left_join(paris_club, by = c("analysis_year", "iso3")) |>
    dplyr::left_join(case_by_country, by = c("analysis_year", "iso3")) |>
    dplyr::mutate(
      boc_source_country_year_covered = !is.na(
        .data$boc_default_context_strength
      ),
      paris_club_source_checked = TRUE,
      paris_club_agreement_count = tidyr::replace_na(
        .data$paris_club_agreement_count, 0L
      ),
      paris_club_same_year_agreement_flag = tidyr::replace_na(
        .data$paris_club_same_year_agreement_flag, FALSE
      ),
      paris_club_has_dssi = tidyr::replace_na(
        .data$paris_club_has_dssi, FALSE
      ),
      paris_club_has_common_framework = tidyr::replace_na(
        .data$paris_club_has_common_framework, FALSE
      ),
      paris_club_has_non_dssi_treatment = tidyr::replace_na(
        .data$paris_club_has_non_dssi_treatment, FALSE
      ),
      p13_case_evidence_row_count = tidyr::replace_na(
        .data$p13_case_evidence_row_count, 0L
      ),
      p13_case_evidence_present = .data$p13_case_evidence_row_count > 0,
      positive_status_evidence_present = dplyr::coalesce(
        .data$boc_default_context_flag, FALSE
      ) | dplyr::coalesce(.data$boc_fiscal_arrears_context_flag, FALSE) |
        .data$paris_club_same_year_agreement_flag |
        .data$p13_case_evidence_present,
      status_evidence_ids = vapply(
        seq_len(dplyr::n()),
        function(i) {
          ids <- c(
            if (isTRUE(boc_default_context_flag[[i]])) "STATUS-EVIDENCE-DEFAULT" else NULL,
            if (isTRUE(boc_fiscal_arrears_context_flag[[i]])) "STATUS-EVIDENCE-ARREARS" else NULL,
            if (isTRUE(paris_club_same_year_agreement_flag[[i]])) "STATUS-EVIDENCE-OFFICIAL-TREATMENT" else NULL,
            if (isTRUE(p13_case_evidence_present[[i]])) "STATUS-EVIDENCE-P13-CASE" else NULL
          )
          if (length(ids)) paste(ids, collapse = ";") else ""
        },
        character(1)
      ),
      positive_evidence_source_package_ids = vapply(
        seq_len(dplyr::n()),
        function(i) {
          ids <- c(
            if (isTRUE(boc_default_context_flag[[i]]) ||
                isTRUE(boc_fiscal_arrears_context_flag[[i]]))
              "SRC-BOC-BOE-DEBT-2025-20260630" else NULL,
            if (isTRUE(paris_club_same_year_agreement_flag[[i]]))
              "SRC-PARIS-CLUB-AGREEMENTS-20260630" else NULL,
            if (isTRUE(p13_case_evidence_present[[i]]))
              "SRC-P13-DISTRESS-STATUS-20240601" else NULL
          )
          if (length(ids)) paste(ids, collapse = ";") else ""
        },
        character(1)
      ),
      queried_source_package_ids = paste(
        "SRC-BOC-BOE-DEBT-2025-20260630",
        "SRC-PARIS-CLUB-AGREEMENTS-20260630",
        "SRC-P13-DISTRESS-STATUS-20240601",
        sep = ";"
      ),
      status_consequence_state = "not_evaluated",
      status_consequence_rule_id = NA_character_,
      systematic_sanctions_panel_state = "material_source_gap",
      systematic_war_conflict_panel_state = "material_source_gap",
      systematic_capital_controls_panel_state = "material_source_gap",
      systematic_market_access_panel_state = "material_source_gap",
      systematic_debt_distress_panel_state = "material_source_gap",
      status_evidence_id = paste("STATUS", .data$analysis_year, .data$iso3, sep = "::"),
      status_evidence_schema_version = p15_status_evidence_schema_version()
    ) |>
    dplyr::arrange(.data$analysis_year, .data$iso3)

  stopifnot(
    nrow(out) == nrow(grid),
    !anyDuplicated(out[c("analysis_year", "iso3")]),
    all(out$status_consequence_state == "not_evaluated"),
    all(out$status_evidence_schema_version == p15_status_evidence_schema_version())
  )
  out
}

p15_validate_status_source_inventory <- function(inventory) {
  required <- c(
    "source_component_id", "source_package_id", "status_dimension",
    "source_name", "path", "coverage_class", "year_scope",
    "evidence_role", "final_method_eligible", "limitation",
    "file_exists", "sha256", "schema_version"
  )
  missing <- setdiff(required, names(inventory))
  if (length(missing)) {
    stop("Status source inventory is missing: ", paste(missing, collapse = ", "),
         call. = FALSE)
  }
  allowed_dimensions <- p15_required_status_dimensions()$status_dimension
  unknown <- setdiff(unique(inventory$status_dimension), allowed_dimensions)
  if (length(unknown)) {
    stop("Unknown status dimensions: ", paste(unknown, collapse = ", "),
         call. = FALSE)
  }
  if (anyDuplicated(inventory[c("source_component_id", "status_dimension")])) {
    stop("Status source inventory has duplicate component-dimension rows.",
         call. = FALSE)
  }
  if (any(!inventory$file_exists)) {
    stop("A registered status source path is missing.", call. = FALSE)
  }
  if (any(is.na(inventory$sha256) | nchar(inventory$sha256) != 64L)) {
    stop("A registered status source lacks a valid SHA-256 hash.", call. = FALSE)
  }
  if (!all(inventory$schema_version == p15_status_gap_schema_version())) {
    stop("Status source inventory schema version drifted.", call. = FALSE)
  }
  invisible(TRUE)
}

p15_build_status_source_gap_matrix <- function(inventory) {
  p15_validate_status_source_inventory(inventory)
  dimensions <- p15_required_status_dimensions()

  out <- dimensions |>
    dplyr::left_join(
      inventory |>
        dplyr::group_by(.data$status_dimension) |>
        dplyr::summarise(
          registered_source_components = dplyr::n(),
          source_package_ids = paste(sort(unique(.data$source_package_id)), collapse = ";"),
          coverage_classes = paste(sort(unique(.data$coverage_class)), collapse = ";"),
          has_full_or_bounded_panel_source = any(
            .data$coverage_class %in% c(
              "full_p15_country_year_panel",
              "bounded_official_event_panel",
              "complete_project_source_scope_panel"
            )
          ),
          has_case_specific_source = any(
            .data$coverage_class %in% c(
              "case_specific_2024",
              "two_year_snapshot_screen",
              "current_snapshot_only"
            )
          ),
          has_project_derived_state_only = all(
            .data$evidence_role %in% c(
              "project_source_availability_state",
              "diagnostic_derived_market_access_state"
            )
          ),
          any_final_method_eligible = any(.data$final_method_eligible),
          .groups = "drop"
        ),
      by = "status_dimension"
    ) |>
    dplyr::mutate(
      registered_source_components = tidyr::replace_na(
        .data$registered_source_components, 0L
      ),
      source_package_ids = tidyr::replace_na(.data$source_package_ids, ""),
      coverage_classes = tidyr::replace_na(.data$coverage_classes, ""),
      has_full_or_bounded_panel_source = tidyr::replace_na(
        .data$has_full_or_bounded_panel_source, FALSE
      ),
      has_case_specific_source = tidyr::replace_na(
        .data$has_case_specific_source, FALSE
      ),
      has_project_derived_state_only = tidyr::replace_na(
        .data$has_project_derived_state_only, FALSE
      ),
      any_final_method_eligible = tidyr::replace_na(
        .data$any_final_method_eligible, FALSE
      ),
      source_readiness_state = dplyr::case_when(
        .data$registered_source_components == 0L ~ "no_source_identified",
        .data$has_full_or_bounded_panel_source & .data$any_final_method_eligible ~
          "source_backed_panel_or_event_input_available",
        .data$has_full_or_bounded_panel_source ~
          "bounded_or_project_scope_input_available",
        .data$has_case_specific_source ~ "case_specific_or_snapshot_only",
        .data$has_project_derived_state_only ~
          "project_derived_state_not_economic_status_proof",
        TRUE ~ "partial_source_input_available"
      ),
      research_gap_state = dplyr::case_when(
        .data$status_dimension %in% c(
          "sanctions", "war_or_conflict", "capital_controls",
          "priced_out_or_no_access", "general_market_access", "debt_distress"
        ) ~ "material_source_gap_remains",
        .data$status_dimension %in% c(
          "no_qualifying_issuance", "no_qualifying_traded_stock"
        ) ~ "project_source_absence_must_not_be_overinterpreted",
        .data$status_dimension %in% c(
          "official_restructuring", "private_or_bond_restructuring"
        ) ~ "available_source_is_not_a_complete_global_restructuring_panel",
        TRUE ~ "bounded_source_ready_for_ingestion"
      ),
      decision_consequence = "evidence_inventory_only_no_warning_or_block_rule_set",
      schema_version = p15_status_gap_schema_version()
    ) |>
    dplyr::arrange(.data$status_dimension)

  stopifnot(
    nrow(out) == nrow(dimensions),
    setequal(out$status_dimension, dimensions$status_dimension)
  )
  out
}
