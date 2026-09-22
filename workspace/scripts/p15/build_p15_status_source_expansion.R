#!/usr/bin/env Rscript

# Add systematic organized-violence and capital-restriction context to the P15
# status evidence layer. This build defines evidence states and source-search
# boundaries only; it makes no warning, exclusion, or benchmark-selection rule.

suppressPackageStartupMessages({
  library(countrycode)
  library(data.table)
  library(digest)
  library(dplyr)
  library(readr)
  library(readxl)
  library(tibble)
  library(tidyr)
})

source("R/p15_status_context.R")
source("R/p15_status_expansion.R")

root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
derived_dir <- file.path(
  root, "data-derived", "p15_status_expansion_2012_2024_v1"
)
governance_dir <- file.path(root, "docs", "governance")
dir.create(derived_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(governance_dir, recursive = TRUE, showWarnings = FALSE)

paths <- list(
  status_context = file.path(
    root, "data-derived", "p15_status_context_2012_2024_v1",
    "p15_status_context_evidence_2012_2024.csv.gz"
  ),
  historical_status = file.path(
    root, "experiments", "p14_historical_validation_and_extension_2026-06-22",
    "outputs", "p14_historical_quality_equalization_2012_2023",
    "p14_historical_status_context_ledger_2012_2023.csv"
  ),
  ucdp_csv = file.path(
    root, "data-raw", "status_context_sources",
    "ucdp_organized_violence_country_year_v26_1_2026-07-21",
    "OrganizedViolenceCYDataSet26_1.csv"
  ),
  ucdp_zip = file.path(
    root, "data-raw", "status_context_sources",
    "ucdp_organized_violence_country_year_v26_1_2026-07-21",
    "organizedviolencecy-261-csv.zip"
  ),
  ucdp_codebook = file.path(
    root, "data-raw", "status_context_sources",
    "ucdp_organized_violence_country_year_v26_1_2026-07-21",
    "UCDP_OrganizedViolenceCY_Codebook_261.pdf"
  ),
  ucdp_metadata = file.path(
    root, "data-raw", "status_context_sources",
    "ucdp_organized_violence_country_year_v26_1_2026-07-21",
    "SOURCE_METADATA.md"
  ),
  bftu_ibops = file.path(
    root, "data-raw", "status_context_sources",
    "bftu_cross_border_restrictions_dfbcbfb_2026-07-21",
    "BFTU_iBoPS.xlsx"
  ),
  bftu_fkrsu = file.path(
    root, "data-raw", "status_context_sources",
    "bftu_cross_border_restrictions_dfbcbfb_2026-07-21",
    "FKRSU_LLM.xlsx"
  ),
  bftu_readme = file.path(
    root, "data-raw", "status_context_sources",
    "bftu_cross_border_restrictions_dfbcbfb_2026-07-21",
    "README.md"
  ),
  bftu_metadata = file.path(
    root, "data-raw", "status_context_sources",
    "bftu_cross_border_restrictions_dfbcbfb_2026-07-21",
    "SOURCE_METADATA.md"
  )
)
missing <- names(paths)[!file.exists(unlist(paths))]
if (length(missing)) {
  stop("Missing status expansion inputs: ", paste(missing, collapse = ", "),
       call. = FALSE)
}

status_context <- data.table::fread(paths$status_context) |>
  tibble::as_tibble()
ucdp <- p15_parse_ucdp_organized_violence(paths$ucdp_csv, years = 2012:2024)
bftu <- p15_parse_bftu_capital_restrictions(
  paths$bftu_ibops,
  paths$bftu_fkrsu,
  years = 2012:2024
)
expanded <- p15_build_expanded_status_context(status_context, ucdp, bftu)
taxonomy <- p15_status_evidence_taxonomy()
historical_status <- data.table::fread(paths$historical_status) |>
  tibble::as_tibble()
review_queues <- p15_build_status_review_queues(expanded, historical_status)
context_screening_queue <- review_queues$context_screening_queue
case_review_queue <- review_queues$quantitative_case_review_queue
case_recommendations <- p15_prepare_status_case_recommendations(
  case_review_queue
)

ucdp_grid_covered <- sum(expanded$ucdp_source_country_year_covered)
bftu_grid_covered <- sum(expanded$bftu_source_country_year_covered)
gap_resolution <- tibble::tribble(
  ~status_dimension, ~source_search_result, ~systematic_source_or_boundary, ~usable_p15_period, ~p15_grid_country_years_covered, ~source_role, ~remaining_limitation, ~source_url, ~automatic_consequence,
  "war_or_conflict", "systematic_context_panel_acquired", "UCDP OrganizedViolenceCY 26.1", "2012-2024", ucdp_grid_covered, "organized_violence_context_only", "Organized violence is not identical to war and is not a measure of sovereign market access; 18 P15 territorial economies are outside the UCDP state universe.", "https://ucdp.uu.se/downloads/", "none",
  "capital_controls", "systematic_research_panel_acquired_through_2023", "BFTU iBoP-S and FKRSU LLM", "2012-2023", bftu_grid_covered, "cross_border_restriction_stance_context_only", "The author-coded AREAER-derived panel ends in 2023, excludes 19 P15 economies, and is not an official IMF market-access classification.", "https://github.com/kteoh37/bftu_cross_border", "none",
  "sanctions", "systematic_research_panel_identified_but_not_locally_acquired", "Global Sanctions Data Base R3/R4", "R3 through 2022; R4 through 2023", 0L, "source_search_boundary", "Public information indicates request-based data access and no complete 2024 observation; sanctions would still require a separate material-market-access interpretation.", "https://www.globalsanctionsdatabase.com/publications/", "none",
  "debt_distress", "official_current_snapshot_and_aggregate_history_only", "IMF current LIC DSA list and published aggregate historical summaries", "current snapshot only", 0L, "source_search_boundary", "A public country-year historical LIC-DSF microdata panel was not acquired; the current snapshot cannot be back-cast.", "https://www.imf.org/en/publications/policy-papers/issues/2026/04/08/macroeconomic-developments-and-prospects-in-low-income-countries-2026-565934", "none",
  "priced_out_or_no_access", "no_systematic_global_country_year_panel_identified", "ONE methodology plus P13 selected case evidence", "selected cases only", 0L, "affirmative_case_evidence_boundary", "No issuance or no quote in the project source is not affirmative proof that a sovereign was priced out or lacked access.", "https://data.one.org/topics/sovereign-debt/priced-out/", "none",
  "general_market_access", "no_systematic_public_global_country_year_panel_identified", "project availability states retained as diagnostics", "2012-2024 project-source scope", 0L, "source_search_boundary", "Project data availability cannot be promoted into an economic access classification without affirmative external evidence.", "", "none"
) |>
  dplyr::mutate(
    source_search_completion_state = "search_boundary_documented",
    consequence_decision_state = "not_evaluated",
    build_id = "BUILD-P15-STATUS-EXPANSION-20260721-V1"
  )

summary <- tibble::tibble(
  metric = c(
    "status_country_year_rows",
    "ucdp_source_rows_2012_2024",
    "ucdp_p15_grid_country_years_covered",
    "ucdp_p15_grid_countries_covered",
    "organized_violence_context_country_years",
    "bftu_source_rows_2012_2023",
    "bftu_p15_grid_country_years_covered",
    "bftu_p15_grid_countries_covered",
    "bftu_indicator_available_country_years",
    "bftu_2024_source_rows",
    "expanded_context_screening_rows",
    "quantitative_case_review_rows",
    "case_level_recommendations_prepared",
    "status_taxonomy_rows",
    "status_gap_search_boundaries_documented",
    "automatic_consequences_set",
    "status_expansion_schema_version"
  ),
  value = c(
    as.character(nrow(expanded)),
    as.character(nrow(ucdp)),
    as.character(ucdp_grid_covered),
    as.character(dplyr::n_distinct(
      expanded$iso3[expanded$ucdp_source_country_year_covered]
    )),
    as.character(sum(expanded$ucdp_any_organized_violence_context)),
    as.character(nrow(bftu)),
    as.character(bftu_grid_covered),
    as.character(dplyr::n_distinct(
      expanded$iso3[expanded$bftu_source_country_year_covered]
    )),
    as.character(sum(
      expanded$bftu_any_capital_restriction_indicator_available
    )),
    as.character(sum(bftu$analysis_year == 2024L)),
    as.character(nrow(context_screening_queue)),
    as.character(nrow(case_review_queue)),
    as.character(nrow(case_recommendations)),
    as.character(nrow(taxonomy)),
    as.character(nrow(gap_resolution)),
    "0",
    p15_status_expansion_schema_version()
  )
)

output_paths <- list(
  ucdp = file.path(
    derived_dir, "p15_ucdp_organized_violence_2012_2024.csv.gz"
  ),
  bftu = file.path(
    derived_dir, "p15_bftu_capital_restrictions_2012_2023.csv.gz"
  ),
  expanded = file.path(
    derived_dir, "p15_status_context_expanded_2012_2024.csv.gz"
  ),
  taxonomy = file.path(governance_dir, "p15_status_evidence_taxonomy.csv"),
  gap_resolution = file.path(
    governance_dir, "p15_status_source_gap_resolution_2026-07-21.csv"
  ),
  case_review_queue = file.path(
    governance_dir, "p15_status_triggered_case_review_queue_2012_2024.csv.gz"
  ),
  case_recommendations = file.path(
    governance_dir,
    "p15_status_case_review_recommendations_2012_2024.csv.gz"
  ),
  context_screening_queue = file.path(
    governance_dir, "p15_status_context_screening_queue_2012_2024.csv.gz"
  ),
  summary = file.path(
    governance_dir, "p15_status_source_expansion_summary.csv"
  ),
  manifest = file.path(
    governance_dir, "p15_status_source_expansion_manifest.csv"
  )
)

data.table::fwrite(data.table::as.data.table(ucdp), output_paths$ucdp, na = "")
data.table::fwrite(data.table::as.data.table(bftu), output_paths$bftu, na = "")
data.table::fwrite(
  data.table::as.data.table(expanded), output_paths$expanded, na = ""
)
data.table::fwrite(
  data.table::as.data.table(taxonomy), output_paths$taxonomy, na = ""
)
data.table::fwrite(
  data.table::as.data.table(gap_resolution),
  output_paths$gap_resolution,
  na = ""
)
data.table::fwrite(
  data.table::as.data.table(case_review_queue),
  output_paths$case_review_queue,
  na = ""
)
data.table::fwrite(
  data.table::as.data.table(context_screening_queue),
  output_paths$context_screening_queue,
  na = ""
)
data.table::fwrite(
  data.table::as.data.table(case_recommendations),
  output_paths$case_recommendations,
  na = ""
)
data.table::fwrite(
  data.table::as.data.table(summary), output_paths$summary, na = ""
)

script_path <- file.path(root, "scripts", "p15", basename(commandArgs()[1]))
if (!file.exists(script_path)) {
  script_path <- file.path(
    root, "scripts", "p15", "build_p15_status_source_expansion.R"
  )
}
input_paths <- unique(unname(unlist(paths)))
generated_paths <- unname(unlist(output_paths[names(output_paths) != "manifest"]))
manifest <- data.table::rbindlist(lapply(
  c(input_paths, generated_paths),
  function(path) {
    data.table::data.table(
      artifact_path = sub(paste0("^", root, "/"), "", path),
      artifact_role = if (path %in% input_paths) "source_or_parent_input" else
        "generated_output",
      bytes = file.info(path)$size,
      sha256 = digest::digest(file = path, algo = "sha256"),
      producing_script = sub(paste0("^", root, "/"), "", script_path),
      producing_script_sha256 = digest::digest(
        file = script_path,
        algo = "sha256"
      ),
      build_id = "BUILD-P15-STATUS-EXPANSION-20260721-V1",
      schema_version = p15_status_expansion_schema_version(),
      build_date = "2026-07-21"
    )
  }
), use.names = TRUE, fill = TRUE)
data.table::fwrite(manifest, output_paths$manifest, na = "")

cat("P15 status source expansion: PASS\n")
cat("Expanded country-year rows:", nrow(expanded), "\n")
cat("UCDP P15 coverage:", ucdp_grid_covered, "country-years\n")
cat("BFTU P15 coverage:", bftu_grid_covered, "country-years\n")
cat("Context screening queue:", nrow(context_screening_queue), "country-years\n")
cat("Quantitative case-review queue:", nrow(case_review_queue), "country-years\n")
cat("Case-level recommendations:", nrow(case_recommendations), "country-years\n")
cat("Automatic consequences set: 0\n")
