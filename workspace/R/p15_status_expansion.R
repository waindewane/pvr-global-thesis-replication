# P15 systematic status-context source expansion.
#
# This layer adds structured conflict and capital-restriction evidence to the
# existing neutral status ledger. It deliberately does not turn those source
# observations into exclusion, warning, or benchmark-selection rules.

p15_status_expansion_schema_version <- function() {
  "SCHEMA-P15-STATUS-EXPANSION-EVIDENCE-V1"
}

p15_status_taxonomy_schema_version <- function() {
  "SCHEMA-P15-STATUS-EVIDENCE-TAXONOMY-V1"
}

p15_assert_required_columns <- function(data, required, object_name) {
  missing <- setdiff(required, names(data))
  if (length(missing)) {
    stop(
      object_name, " is missing: ", paste(missing, collapse = ", "),
      call. = FALSE
    )
  }
  invisible(TRUE)
}

p15_parse_ucdp_organized_violence <- function(csv_path, years = 2012:2024) {
  stopifnot(file.exists(csv_path))
  raw <- readr::read_csv(csv_path, show_col_types = FALSE)
  required <- c(
    "country", "country_id", "year", "region", "sb_exist",
    "sb_dyad_count", "sb_dyad_names", "sb_total_deaths_best",
    "ns_exist", "ns_dyad_count", "ns_dyad_names",
    "ns_total_deaths_best", "os_exist", "os_dyad_count",
    "os_dyad_names", "os_total_deaths_best", "Version"
  )
  p15_assert_required_columns(raw, required, "UCDP organized-violence source")

  custom_match <- c(
    "Kosovo" = "XKX",
    "Yemen (North Yemen)" = "YEM"
  )
  out <- raw |>
    dplyr::filter(.data$year %in% years) |>
    dplyr::mutate(
      iso3 = countrycode::countrycode(
        .data$country,
        "country.name",
        "iso3c",
        custom_match = custom_match,
        warn = FALSE
      )
    ) |>
    dplyr::transmute(
      analysis_year = as.integer(.data$year),
      .data$iso3,
      ucdp_country_name = .data$country,
      ucdp_country_id = as.integer(.data$country_id),
      ucdp_region = .data$region,
      ucdp_state_based_violence_flag = .data$sb_exist %in% 1,
      ucdp_state_based_dyad_count = as.integer(.data$sb_dyad_count),
      ucdp_state_based_dyad_names = as.character(.data$sb_dyad_names),
      ucdp_state_based_deaths_best = as.numeric(.data$sb_total_deaths_best),
      ucdp_non_state_violence_flag = .data$ns_exist %in% 1,
      ucdp_non_state_dyad_count = as.integer(.data$ns_dyad_count),
      ucdp_non_state_dyad_names = as.character(.data$ns_dyad_names),
      ucdp_non_state_deaths_best = as.numeric(.data$ns_total_deaths_best),
      ucdp_one_sided_violence_flag = .data$os_exist %in% 1,
      ucdp_one_sided_dyad_count = as.integer(.data$os_dyad_count),
      ucdp_one_sided_dyad_names = as.character(.data$os_dyad_names),
      ucdp_one_sided_deaths_best = as.numeric(.data$os_total_deaths_best),
      ucdp_version = as.character(.data$Version),
      ucdp_any_organized_violence_context =
        .data$sb_exist %in% 1 | .data$ns_exist %in% 1 | .data$os_exist %in% 1,
      ucdp_total_organized_violence_deaths_best =
        dplyr::coalesce(as.numeric(.data$sb_total_deaths_best), 0) +
        dplyr::coalesce(as.numeric(.data$ns_total_deaths_best), 0) +
        dplyr::coalesce(as.numeric(.data$os_total_deaths_best), 0),
      ucdp_source_package_id =
        "SRC-UCDP-ORGANIZED-VIOLENCE-CY-26.1-20260721",
      ucdp_source_pointer = paste0(
        "UCDP OrganizedViolenceCY 26.1 country_id=", .data$country_id,
        ";year=", .data$year
      )
    ) |>
    dplyr::arrange(.data$analysis_year, .data$iso3)

  if (any(is.na(out$iso3))) {
    stop("UCDP source contains unmapped country-years.", call. = FALSE)
  }
  if (anyDuplicated(out[c("analysis_year", "iso3")])) {
    stop("UCDP source contains duplicate P15 country-years.", call. = FALSE)
  }
  out
}

p15_parse_bftu_capital_restrictions <- function(
    ibops_path, fkrsu_path, years = 2012:2024) {
  stopifnot(file.exists(ibops_path), file.exists(fkrsu_path))
  # The source workbooks use explicit textual non-observation codes. Treat
  # them as missing rather than allowing spreadsheet type inference to emit
  # hundreds of coercion warnings or, worse, turn them into zeros.
  na_values <- c("", "n.a", "n.a.", "NA", "N/A", "d.n.e", "n.r")
  ibops_raw <- readxl::read_excel(
    ibops_path,
    na = na_values,
    .name_repair = "unique"
  )
  fkrsu_raw <- readxl::read_excel(
    fkrsu_path,
    na = na_values,
    .name_repair = "unique"
  )
  p15_assert_required_columns(
    ibops_raw,
    c("country", "year", "code_wdi", "total", "c7.capital_account"),
    "BFTU iBoP-S source"
  )
  p15_assert_required_columns(
    fkrsu_raw,
    c("country", "year", "code_wdi", "ka", "kai", "kao"),
    "BFTU FKRSU source"
  )

  ibops <- ibops_raw |>
    dplyr::filter(.data$year %in% years) |>
    dplyr::transmute(
      analysis_year = as.integer(.data$year),
      iso3 = as.character(.data$code_wdi),
      bftu_country_name_ibops = as.character(.data$country),
      bftu_ibops_total_restriction_stance = as.numeric(.data$total),
      bftu_ibops_capital_account_restriction_stance =
        as.numeric(.data$c7.capital_account)
    )
  fkrsu <- fkrsu_raw |>
    dplyr::filter(.data$year %in% years) |>
    dplyr::transmute(
      analysis_year = as.integer(.data$year),
      iso3 = as.character(.data$code_wdi),
      bftu_country_name_fkrsu = as.character(.data$country),
      bftu_fkrsu_capital_restriction_stance = as.numeric(.data$ka),
      bftu_fkrsu_inflow_restriction_stance = as.numeric(.data$kai),
      bftu_fkrsu_outflow_restriction_stance = as.numeric(.data$kao)
    )

  if (anyDuplicated(ibops[c("analysis_year", "iso3")]) ||
      anyDuplicated(fkrsu[c("analysis_year", "iso3")])) {
    stop("BFTU source contains duplicate P15 country-years.", call. = FALSE)
  }

  out <- dplyr::full_join(
    ibops,
    fkrsu,
    by = c("analysis_year", "iso3")
  ) |>
    dplyr::mutate(
      bftu_country_name = dplyr::coalesce(
        .data$bftu_country_name_ibops,
        .data$bftu_country_name_fkrsu
      ),
      bftu_ibops_indicator_available = !is.na(
        .data$bftu_ibops_capital_account_restriction_stance
      ),
      bftu_fkrsu_indicator_available = !is.na(
        .data$bftu_fkrsu_capital_restriction_stance
      ),
      bftu_any_capital_restriction_indicator_available =
        .data$bftu_ibops_indicator_available |
        .data$bftu_fkrsu_indicator_available,
      bftu_source_package_id = "SRC-BFTU-CROSS-BORDER-DFBCBFB-20260721",
      bftu_source_pointer = paste0(
        "BFTU commit dfbcbfb;code_wdi=", .data$iso3,
        ";year=", .data$analysis_year
      )
    ) |>
    dplyr::select(-dplyr::ends_with("_ibops"), -dplyr::ends_with("_fkrsu")) |>
    dplyr::arrange(.data$analysis_year, .data$iso3)

  if (any(is.na(out$iso3) | nchar(out$iso3) != 3L)) {
    stop("BFTU source contains missing or malformed ISO3 codes.", call. = FALSE)
  }
  if (anyDuplicated(out[c("analysis_year", "iso3")])) {
    stop("Merged BFTU source contains duplicate P15 country-years.", call. = FALSE)
  }
  out
}

p15_append_semicolon_id <- function(base, add, include) {
  base <- ifelse(is.na(base), "", as.character(base))
  add <- ifelse(include, add, "")
  out <- ifelse(
    nzchar(base) & nzchar(add),
    paste(base, add, sep = ";"),
    paste0(base, add)
  )
  out
}

p15_build_expanded_status_context <- function(
    status_context, ucdp_context, bftu_context) {
  required <- c(
    "analysis_year", "iso3", "country", "country_year_id",
    "positive_status_evidence_present", "status_evidence_ids",
    "positive_evidence_source_package_ids", "queried_source_package_ids",
    "status_consequence_state", "status_consequence_rule_id"
  )
  p15_assert_required_columns(status_context, required, "P15 status context")

  out <- status_context |>
    dplyr::left_join(ucdp_context, by = c("analysis_year", "iso3")) |>
    dplyr::left_join(bftu_context, by = c("analysis_year", "iso3")) |>
    dplyr::mutate(
      ucdp_source_year_available = .data$analysis_year %in% 2012:2024,
      ucdp_source_country_year_covered = !is.na(.data$ucdp_country_id),
      ucdp_any_organized_violence_context = dplyr::coalesce(
        .data$ucdp_any_organized_violence_context,
        FALSE
      ),
      ucdp_context_state = dplyr::case_when(
        !.data$ucdp_source_country_year_covered ~
          "outside_ucdp_state_country_universe",
        .data$ucdp_any_organized_violence_context ~
          "source_backed_organized_violence_context",
        TRUE ~ "source_covered_no_organized_violence_record"
      ),
      bftu_source_year_available = .data$analysis_year %in% 2012:2023,
      bftu_source_country_year_covered = !is.na(.data$bftu_country_name),
      bftu_any_capital_restriction_indicator_available = dplyr::coalesce(
        .data$bftu_any_capital_restriction_indicator_available,
        FALSE
      ),
      bftu_context_state = dplyr::case_when(
        !.data$bftu_source_year_available ~ "source_year_unavailable_2024",
        !.data$bftu_source_country_year_covered ~
          "outside_bftu_country_universe",
        .data$bftu_any_capital_restriction_indicator_available ~
          "source_backed_restriction_indicator_available",
        TRUE ~ "source_country_year_row_with_indicator_missing"
      ),
      expanded_status_context_trigger_present =
        .data$positive_status_evidence_present |
        .data$ucdp_any_organized_violence_context,
      expanded_status_evidence_ids = p15_append_semicolon_id(
        .data$status_evidence_ids,
        "STATUS-EVIDENCE-ORGANIZED-VIOLENCE-CONTEXT",
        .data$ucdp_any_organized_violence_context
      ),
      expanded_positive_evidence_source_package_ids =
        p15_append_semicolon_id(
          .data$positive_evidence_source_package_ids,
          "SRC-UCDP-ORGANIZED-VIOLENCE-CY-26.1-20260721",
          .data$ucdp_any_organized_violence_context
        ),
      expanded_queried_source_package_ids = paste(
        .data$queried_source_package_ids,
        "SRC-UCDP-ORGANIZED-VIOLENCE-CY-26.1-20260721",
        "SRC-BFTU-CROSS-BORDER-DFBCBFB-20260721",
        sep = ";"
      ),
      systematic_war_conflict_panel_state =
        "organized_violence_context_panel_available_not_market_access_measure",
      systematic_capital_controls_panel_state =
        "research_stance_panel_available_2012_2023_2024_unavailable",
      expanded_status_consequence_state = "not_evaluated",
      expanded_status_consequence_rule_id = NA_character_,
      status_expansion_evidence_id = paste(
        "STATUS-EXPANSION", .data$analysis_year, .data$iso3, sep = "::"
      ),
      status_expansion_schema_version = p15_status_expansion_schema_version()
    ) |>
    dplyr::arrange(.data$analysis_year, .data$iso3)

  stopifnot(
    nrow(out) == nrow(status_context),
    !anyDuplicated(out[c("analysis_year", "iso3")]),
    all(out$status_consequence_state == "not_evaluated"),
    all(out$expanded_status_consequence_state == "not_evaluated"),
    all(out$status_expansion_schema_version ==
          p15_status_expansion_schema_version())
  )
  out
}

p15_build_status_review_queues <- function(expanded_status, historical_status) {
  p15_assert_required_columns(
    expanded_status,
    c(
      "analysis_year", "iso3", "country",
      "expanded_status_context_trigger_present",
      "positive_status_evidence_present",
      "ucdp_any_organized_violence_context",
      "p13_case_evidence_present"
    ),
    "Expanded P15 status context"
  )
  p15_assert_required_columns(
    historical_status,
    c(
      "analysis_year", "iso3", "status_quantitative_review_trigger",
      "status_quantitative_effect_rule", "affects_quantitative_admissibility",
      "affects_display_or_interpretation", "status_source_pointer"
    ),
    "P14 historical status ledger"
  )

  context_screening_queue <- expanded_status |>
    dplyr::filter(.data$expanded_status_context_trigger_present) |>
    dplyr::mutate(
      status_trigger_count =
        as.integer(.data$positive_status_evidence_present) +
        as.integer(.data$ucdp_any_organized_violence_context),
      screening_priority = dplyr::case_when(
        .data$status_trigger_count > 1L ~ "multiple_source_families",
        .data$positive_status_evidence_present ~
          "default_treatment_or_case_context",
        TRUE ~ "organized_violence_context_only"
      ),
      screening_state = "context_only_not_quantitative_review_trigger",
      automatic_consequence = "none"
    ) |>
    dplyr::select(dplyr::all_of(c(
      "analysis_year", "iso3", "country", "screening_priority",
      "status_trigger_count", "positive_status_evidence_present",
      "ucdp_any_organized_violence_context",
      "ucdp_state_based_violence_flag", "ucdp_non_state_violence_flag",
      "ucdp_one_sided_violence_flag",
      "ucdp_total_organized_violence_deaths_best",
      "bftu_ibops_capital_account_restriction_stance",
      "bftu_fkrsu_capital_restriction_stance",
      "expanded_status_evidence_ids",
      "expanded_positive_evidence_source_package_ids",
      "screening_state", "automatic_consequence"
    ))) |>
    dplyr::arrange(
      dplyr::desc(.data$status_trigger_count),
      .data$analysis_year,
      .data$iso3
    )

  historical_review_keys <- historical_status |>
    dplyr::filter(.data$status_quantitative_review_trigger %in% TRUE) |>
    dplyr::transmute(
      analysis_year = as.integer(.data$analysis_year),
      iso3 = as.character(.data$iso3),
      review_basis = "predecessor_quantitative_review_trigger",
      predecessor_effect_rule = as.character(
        .data$status_quantitative_effect_rule
      ),
      predecessor_affects_quantitative_admissibility = as.logical(
        .data$affects_quantitative_admissibility
      ),
      predecessor_affects_display_or_interpretation = as.logical(
        .data$affects_display_or_interpretation
      ),
      predecessor_status_source_pointer = as.character(
        .data$status_source_pointer
      )
    )
  p13_review_keys <- expanded_status |>
    dplyr::filter(
      .data$analysis_year == 2024L,
      .data$p13_case_evidence_present %in% TRUE
    ) |>
    dplyr::transmute(
      analysis_year = as.integer(.data$analysis_year),
      iso3 = as.character(.data$iso3),
      review_basis = "p13_2024_case_evidence",
      predecessor_effect_rule = NA_character_,
      predecessor_affects_quantitative_admissibility = NA,
      predecessor_affects_display_or_interpretation = NA,
      predecessor_status_source_pointer = NA_character_
    )

  review_keys <- dplyr::bind_rows(historical_review_keys, p13_review_keys) |>
    dplyr::distinct(.data$analysis_year, .data$iso3, .keep_all = TRUE)

  quantitative_case_review_queue <- expanded_status |>
    dplyr::inner_join(review_keys, by = c("analysis_year", "iso3")) |>
    dplyr::mutate(
      review_state = "pending_case_review",
      automatic_consequence = "none"
    ) |>
    dplyr::select(dplyr::all_of(c(
      "analysis_year", "iso3", "country", "review_basis",
      "predecessor_effect_rule",
      "predecessor_affects_quantitative_admissibility",
      "predecessor_affects_display_or_interpretation",
      "predecessor_status_source_pointer",
      "positive_status_evidence_present",
      "p13_case_evidence_present", "p13_case_status_categories",
      "p13_case_status_values", "p13_case_market_access_implications",
      "ucdp_any_organized_violence_context",
      "bftu_ibops_capital_account_restriction_stance",
      "bftu_fkrsu_capital_restriction_stance",
      "expanded_status_evidence_ids",
      "expanded_positive_evidence_source_package_ids",
      "review_state", "automatic_consequence"
    ))) |>
    dplyr::arrange(.data$analysis_year, .data$iso3)

  stopifnot(
    !anyDuplicated(context_screening_queue[c("analysis_year", "iso3")]),
    !anyDuplicated(quantitative_case_review_queue[c("analysis_year", "iso3")]),
    all(context_screening_queue$automatic_consequence == "none"),
    all(quantitative_case_review_queue$automatic_consequence == "none")
  )
  list(
    context_screening_queue = context_screening_queue,
    quantitative_case_review_queue = quantitative_case_review_queue
  )
}

p15_prepare_status_case_recommendations <- function(case_review_queue) {
  p15_assert_required_columns(
    case_review_queue,
    c(
      "analysis_year", "iso3", "country", "review_basis",
      "predecessor_effect_rule",
      "predecessor_affects_quantitative_admissibility",
      "predecessor_affects_display_or_interpretation",
      "predecessor_status_source_pointer", "p13_case_status_categories",
      "p13_case_status_values", "p13_case_market_access_implications",
      "ucdp_any_organized_violence_context",
      "bftu_ibops_capital_account_restriction_stance",
      "bftu_fkrsu_capital_restriction_stance",
      "expanded_status_evidence_ids",
      "expanded_positive_evidence_source_package_ids"
    ),
    "P15 status case-review queue"
  )
  text_or_none <- function(x) {
    x <- trimws(as.character(x))
    ifelse(is.na(x) | !nzchar(x), "none recorded", x)
  }
  number_or_missing <- function(x) {
    ifelse(
      is.na(x), "not available",
      format(round(as.numeric(x), 3), trim = TRUE, scientific = FALSE)
    )
  }

  case_review_queue |>
    dplyr::mutate(
      p13_severe_status_signal = .data$review_basis ==
        "p13_2024_case_evidence" & stringr::str_detect(
          dplyr::coalesce(.data$p13_case_status_categories, ""),
          paste(
            "active_default_or_restructuring|recent_or_ongoing_debt_exchange|",
            "war_sanctions_or_arrears_impairment",
            sep = ""
          )
        ),
      recommended_treatment_class = dplyr::case_when(
        .data$p13_severe_status_signal ~
          "provisional_block_ordinary_fallback_pending_STAT_05",
        .data$review_basis == "p13_2024_case_evidence" ~
          "provisional_manual_review_before_observed_use_pending_STAT_05",
        TRUE ~
          "provisional_context_warning_no_automatic_block_pending_STAT_05"
      ),
      recommended_admissibility_action = dplyr::case_when(
        .data$p13_severe_status_signal ~
          "Do not treat an ordinary fallback as usable until the case-specific status rule is approved.",
        .data$review_basis == "p13_2024_case_evidence" ~
          "Require manual review of the observed rate object before ordinary benchmark use.",
        TRUE ~
          "Retain the rate evidence and status warning; do not block it solely because the context trigger is present."
      ),
      recommended_display_action = dplyr::case_when(
        .data$review_basis == "p13_2024_case_evidence" ~
          "Display the case-specific distress, restructuring, sanctions, conflict, or market-access note with any rate evidence.",
        TRUE ~
          "Display a source-backed default or treatment context warning when the country-year is used."
      ),
      organized_violence_context_note = dplyr::if_else(
        .data$ucdp_any_organized_violence_context,
        "UCDP records organized-violence context in this country-year; this is context rather than proof of lost market access.",
        "UCDP does not record organized-violence context in this country-year or the economy is outside its state universe."
      ),
      capital_restriction_context_note = paste0(
        "BFTU restriction indicators (iBoP-S / FKRSU): ",
        number_or_missing(.data$bftu_ibops_capital_account_restriction_stance),
        " / ",
        number_or_missing(.data$bftu_fkrsu_capital_restriction_stance),
        ". These indicators are context only."
      ),
      case_level_note = dplyr::case_when(
        .data$review_basis == "p13_2024_case_evidence" ~ paste0(
          .data$country, " (", .data$analysis_year,
          ") has preserved case-specific status evidence: ",
          text_or_none(.data$p13_case_status_categories), ". Recorded values: ",
          text_or_none(.data$p13_case_status_values),
          ". The preserved market-access implication is: ",
          text_or_none(.data$p13_case_market_access_implications), "."
        ),
        TRUE ~ paste0(
          .data$country, " (", .data$analysis_year,
          ") entered the historical quantitative-review queue through the ",
          "source-backed default/treatment context ledger. The predecessor rule ",
          "treated this as a display or interpretation warning rather than an ",
          "automatic admissibility block. Source pointer: ",
          text_or_none(.data$predecessor_status_source_pointer), "."
        )
      ),
      recommendation_basis = paste0(
        .data$case_level_note, " ", .data$organized_violence_context_note,
        " ", .data$capital_restriction_context_note
      ),
      recommendation_state =
        "case_level_recommendation_prepared_not_applied_pending_STAT_05",
      automatic_consequence = "none",
      recommendation_build_id = "BUILD-P15-STATUS-EXPANSION-20260721-V1"
    ) |>
    dplyr::select(
      "analysis_year", "iso3", "country", "review_basis",
      "recommended_treatment_class", "recommended_admissibility_action",
      "recommended_display_action", "case_level_note",
      "organized_violence_context_note", "capital_restriction_context_note",
      "recommendation_basis", "expanded_status_evidence_ids",
      "expanded_positive_evidence_source_package_ids",
      "recommendation_state", "automatic_consequence",
      "recommendation_build_id"
    ) |>
    dplyr::arrange(.data$analysis_year, .data$iso3)
}

p15_status_evidence_taxonomy <- function() {
  tibble::tribble(
    ~taxonomy_id, ~status_dimension, ~evidence_state, ~plain_language_meaning, ~evidence_strength, ~automatic_consequence, ~decision_state, ~source_package_ids, ~known_limitation,
    "STATUS-TAX-01", "sovereign_default", "source_backed_default_stock_context", "The official database records sovereign debt in default in this country-year.", "systematic_country_year_context", "none", "not_evaluated", "SRC-BOC-BOE-DEBT-2025-20260630", "Default stock does not itself prove that market access was absent or that an observed rate is unusable.",
    "STATUS-TAX-02", "fiscal_arrears", "source_backed_fiscal_arrears_context", "The official database records fiscal arrears in this country-year.", "systematic_country_year_context", "none", "not_evaluated", "SRC-BOC-BOE-DEBT-2025-20260630", "Arrears are context and require a separate consequence rule.",
    "STATUS-TAX-03", "private_or_bond_restructuring", "source_backed_private_or_bond_default_context", "The official database records private-creditor or bond debt in default.", "systematic_country_year_context", "none", "not_evaluated", "SRC-BOC-BOE-DEBT-2025-20260630", "This is a stock measure rather than a complete event history.",
    "STATUS-TAX-04", "official_restructuring", "source_backed_official_treatment_event", "A Paris Club treatment was signed in this country-year.", "bounded_official_event_evidence", "none", "not_evaluated", "SRC-PARIS-CLUB-AGREEMENTS-20260630", "Paris Club events are not a complete global official-restructuring panel.",
    "STATUS-TAX-05", "war_or_conflict", "source_backed_organized_violence_context", "UCDP records organized violence inside the country in this year.", "systematic_country_year_context", "none", "not_evaluated", "SRC-UCDP-ORGANIZED-VIOLENCE-CY-26.1-20260721", "Organized violence is neither identical to war nor a measure of sovereign market access.",
    "STATUS-TAX-06", "capital_controls", "source_backed_restriction_indicator_available", "A research index derived from IMF AREAER information measures the stance of cross-border restrictions.", "systematic_research_country_year_context", "none", "not_evaluated", "SRC-BFTU-CROSS-BORDER-DFBCBFB-20260721", "The source ends in 2023 and the index is not an official IMF market-access classification.",
    "STATUS-TAX-07", "sanctions", "case_specific_or_external_panel_not_locally_acquired", "Sanctions evidence is available only for selected project cases; a broader research panel requires separate access.", "source_boundary_documented", "none", "not_evaluated", "SRC-P13-DISTRESS-STATUS-20240601", "No complete locally preserved 2012-2024 panel is available.",
    "STATUS-TAX-08", "debt_distress", "current_official_snapshot_only", "An official current LIC-DSF classification is available, but it cannot be applied retrospectively.", "current_snapshot_context", "none", "not_evaluated", "SRC-IMF-LIC-DSA-20260331", "Country-year historical microdata have not been acquired.",
    "STATUS-TAX-09", "priced_out_or_no_access", "affirmative_case_evidence_required", "A country can be labelled priced out or without access only when affirmative case evidence supports it.", "case_specific_evidence_only", "none", "not_evaluated", "SRC-ONE-PRICED-OUT-METHOD-20260507;SRC-P13-DISTRESS-STATUS-20240601", "No systematic global country-year panel was identified.",
    "STATUS-TAX-10", "general_market_access", "no_systematic_public_panel_identified", "The project currently has no external global country-year classification of sovereign market access.", "source_boundary_documented", "none", "not_evaluated", "SRC-P14-HIST-STATUS-20260630", "Project data availability must not be treated as economic proof of access or no access.",
    "STATUS-TAX-11", "no_qualifying_issuance", "project_source_no_qualifying_issuance", "No qualifying primary issue is observed in the project source for this country-year.", "complete_project_source_scope_state", "none", "not_evaluated", "SRC-LSEG-EXT-20260623-R4", "Absence in the project source is not proof that no issuance occurred anywhere.",
    "STATUS-TAX-12", "no_qualifying_traded_stock", "project_source_no_qualifying_stock", "No qualifying hard-currency bond stock is identified in the project source.", "complete_project_source_scope_state", "none", "not_evaluated", "SRC-P15-LSEG-UNIFIED-20260721-V1", "Absence in the project source is not proof that no traded stock existed.",
    "STATUS-TAX-13", "quote_missing", "project_source_qualifying_instrument_quote_missing", "A qualifying instrument exists but the project source has no usable in-window quote.", "complete_project_source_scope_state", "none", "not_evaluated", "SRC-P15-LSEG-UNIFIED-20260721-V1", "Quote missingness is source- and window-specific."
  ) |>
    dplyr::mutate(schema_version = p15_status_taxonomy_schema_version())
}
