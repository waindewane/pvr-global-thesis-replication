#!/usr/bin/env Rscript

# Inventory source support for P15 status and market-access dimensions. This build
# completes the gap-matrix step only; it does not set warning or blocking rules.

suppressPackageStartupMessages({
  library(data.table)
  library(digest)
  library(dplyr)
  library(tibble)
  library(tidyr)
})

source("R/p15_status_context.R")

root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
governance_dir <- file.path(root, "docs", "governance")
dir.create(governance_dir, recursive = TRUE, showWarnings = FALSE)

paths <- list(
  boc_boe = "data-raw/status_context_sources/boc_boe_sovereign_default_database_DEBT_2025_2026-06-30.json",
  paris_club = "data-raw/status_context_sources/paris_club_signed_agreements_2026-06-30/paris_club_signed_agreements_advanced_search_2026-06-30.csv",
  p13_distress = "experiments/full_ladder_ratings_integrated_status_rebuild_2026-06-01/outputs/distress_restructuring_status_ledger_2024.csv",
  p14_status = "experiments/p14_historical_validation_and_extension_2026-06-22/outputs/p14_historical_quality_equalization_2012_2023/p14_historical_status_context_ledger_2012_2023.csv",
  fcs_fy24 = "sources/methodology_decisions/p11c_country_context_review_2024_2026-06-17/world_bank_fcs_fy24.txt",
  fcs_fy25 = "sources/methodology_decisions/p11c_country_context_review_2024_2026-06-17/world_bank_fcs_fy25.txt",
  imf_dsa = "sources/methodology_decisions/p11c_country_context_review_2024_2026-06-17/imf_lic_dsa_list_2026-03-31.txt",
  p15_primary = "sources/market_rates/lseg_extension_2012_2024_partial_resume_20260721/pvr-global-lseg-extension-2012-2024-usd-eur_20260623_184707_partial_resume_20260721.zip",
  p15_secondary = "data-derived/p15_unified_lseg_source_2012_2024_v1/p15_instrument_year_universe_2012_2024.csv.gz",
  one_method = "sources/one_priced_out_methodology_2026-05-07.html"
)
missing_paths <- names(paths)[!file.exists(file.path(root, unlist(paths)))]
if (length(missing_paths)) {
  stop("Missing status inventory paths: ", paste(missing_paths, collapse = ", "),
       call. = FALSE)
}

source_rows <- tibble::tribble(
  ~source_component_id, ~source_package_id, ~status_dimension, ~source_name, ~path_key, ~coverage_class, ~year_scope, ~evidence_role, ~final_method_eligible, ~limitation,
  "STATUS-S01", "SRC-BOC-BOE-DEBT-2025-20260630", "sovereign_default", "Bank of Canada-Bank of England Sovereign Default Database 2025", "boc_boe", "full_p15_country_year_panel", "2012-2024", "official_default_stock_context", TRUE, "Default-stock data do not by themselves prove loss of market access or define a selection block.",
  "STATUS-S02", "SRC-BOC-BOE-DEBT-2025-20260630", "fiscal_arrears", "Bank of Canada-Bank of England Sovereign Default Database 2025", "boc_boe", "full_p15_country_year_panel", "2012-2024", "official_fiscal_arrears_context", TRUE, "Fiscal-arrears amounts are status context and require a separate consequence rule.",
  "STATUS-S03", "SRC-BOC-BOE-DEBT-2025-20260630", "private_or_bond_restructuring", "Bank of Canada-Bank of England Sovereign Default Database 2025", "boc_boe", "full_p15_country_year_panel", "2012-2024", "private_creditor_and_bond_default_stock_context", TRUE, "Default stocks are not a complete event-level private restructuring history.",
  "STATUS-S04", "SRC-PARIS-CLUB-AGREEMENTS-20260630", "official_restructuring", "Paris Club signed agreements", "paris_club", "bounded_official_event_panel", "2012-2024", "official_bilateral_treatment_event", TRUE, "Covers Paris Club signed treatments only; it is not a complete global official restructuring panel.",
  "STATUS-S05", "SRC-P13-DISTRESS-STATUS-20240601", "private_or_bond_restructuring", "P13 source-backed distress and restructuring ledger", "p13_distress", "case_specific_2024", "2024 selected cases", "case_specific_status_evidence", FALSE, "Curated 2024 cases cannot be generalized into a complete panel.",
  "STATUS-S06", "SRC-P13-DISTRESS-STATUS-20240601", "sanctions", "P13 source-backed distress and restructuring ledger", "p13_distress", "case_specific_2024", "2024 selected cases", "case_specific_status_evidence", FALSE, "Case coverage is selective rather than systematic.",
  "STATUS-S07", "SRC-P13-DISTRESS-STATUS-20240601", "capital_controls", "P13 source-backed distress and restructuring ledger", "p13_distress", "case_specific_2024", "2024 selected cases", "case_specific_status_evidence", FALSE, "Stress and foreign-exchange context is not a systematic capital-controls panel.",
  "STATUS-S08", "SRC-P13-DISTRESS-STATUS-20240601", "priced_out_or_no_access", "P13 source-backed distress and restructuring ledger", "p13_distress", "case_specific_2024", "2024 selected cases", "case_specific_status_evidence", FALSE, "Case-specific stress evidence does not establish a complete priced-out universe.",
  "STATUS-S09", "SRC-WB-FCS-FY24-20260617", "war_or_conflict", "World Bank FY24 fragile and conflict-affected situations list", "fcs_fy24", "two_year_snapshot_screen", "FY2024", "official_fragility_conflict_screen", FALSE, "FCS classification is a screen and is not identical to a war or market-access state.",
  "STATUS-S10", "SRC-WB-FCS-FY25-20260617", "war_or_conflict", "World Bank FY25 fragile and conflict-affected situations list", "fcs_fy25", "two_year_snapshot_screen", "FY2025", "official_fragility_conflict_screen", FALSE, "FCS classification is a screen and does not provide a 2012-2024 war panel.",
  "STATUS-S11", "SRC-IMF-LIC-DSA-20260331", "debt_distress", "IMF current LIC DSA list", "imf_dsa", "current_snapshot_only", "current snapshot captured 2026-03-31", "official_current_debt_distress_screen", FALSE, "Current status cannot be back-cast to historical country-years.",
  "STATUS-S12", "SRC-LSEG-EXT-20260623-R4", "no_qualifying_issuance", "P15 broad primary source", "p15_primary", "complete_project_source_scope_panel", "2012-2024", "project_source_availability_state", TRUE, "No observed qualifying issue in this source is not proof that no issuance occurred anywhere.",
  "STATUS-S13", "SRC-P15-LSEG-UNIFIED-20260721-V1", "no_qualifying_traded_stock", "P15 normalized secondary instrument-year universe", "p15_secondary", "complete_project_source_scope_panel", "2012-2024", "project_source_availability_state", TRUE, "No identified stock in the project source is not proof that no traded stock existed.",
  "STATUS-S14", "SRC-P15-LSEG-UNIFIED-20260721-V1", "quote_missing", "P15 normalized secondary instrument-year universe", "p15_secondary", "complete_project_source_scope_panel", "2012-2024", "project_source_availability_state", TRUE, "Quote missingness is defined only within the captured LSEG fields and windows.",
  "STATUS-S15", "SRC-P14-HIST-STATUS-20260630", "general_market_access", "P14 historical status context ledger", "p14_status", "derived_full_p15_panel_except_2024", "2012-2023", "diagnostic_derived_market_access_state", FALSE, "Availability-derived states are not affirmative economic proof of access or no access.",
  "STATUS-S16", "SRC-ONE-PRICED-OUT-METHOD-20260507", "priced_out_or_no_access", "ONE Priced Out methodology", "one_method", "case_specific_external_method", "selected published cases", "external_priced_out_method_reference", FALSE, "Provides a precedent and selected classifications rather than a reusable 2012-2024 global status panel."
)

inventory <- source_rows |>
  dplyr::mutate(
    path = unname(unlist(paths[.data$path_key])),
    absolute_path = file.path(root, .data$path),
    file_exists = file.exists(.data$absolute_path),
    sha256 = vapply(
      .data$absolute_path,
      digest::digest,
      character(1),
      file = TRUE,
      algo = "sha256"
    ),
    schema_version = p15_status_gap_schema_version()
  ) |>
  dplyr::select(-path_key, -absolute_path)

p15_validate_status_source_inventory(inventory)
gap_matrix <- p15_build_status_source_gap_matrix(inventory)

summary <- tibble::tibble(
  metric = c(
    "required_status_dimensions",
    "registered_source_component_dimension_rows",
    "dimensions_with_panel_or_bounded_event_source",
    "dimensions_with_material_source_gap",
    "dimensions_with_no_source_identified",
    "decision_rules_set_by_this_build",
    "status_gap_schema_version"
  ),
  value = c(
    as.character(nrow(gap_matrix)),
    as.character(nrow(inventory)),
    as.character(sum(gap_matrix$has_full_or_bounded_panel_source)),
    as.character(sum(gap_matrix$research_gap_state == "material_source_gap_remains")),
    as.character(sum(gap_matrix$source_readiness_state == "no_source_identified")),
    "0",
    p15_status_gap_schema_version()
  )
)

inventory_path <- file.path(governance_dir, "p15_status_source_inventory.csv")
gap_path <- file.path(governance_dir, "p15_status_source_gap_matrix.csv")
summary_path <- file.path(governance_dir, "p15_status_source_gap_summary.csv")
manifest_path <- file.path(governance_dir, "p15_status_source_gap_manifest.csv")

data.table::fwrite(data.table::as.data.table(inventory), inventory_path, na = "")
data.table::fwrite(data.table::as.data.table(gap_matrix), gap_path, na = "")
data.table::fwrite(data.table::as.data.table(summary), summary_path, na = "")

script_path <- file.path(root, "scripts/p15/build_p15_status_source_gap_matrix.R")
manifest_inputs <- unique(file.path(root, inventory$path))
manifest <- data.table::rbindlist(lapply(
  c(manifest_inputs, inventory_path, gap_path, summary_path),
  function(path) {
    data.table::data.table(
      artifact_path = sub(paste0("^", root, "/"), "", path),
      artifact_role = if (path %in% manifest_inputs) "source_input" else "generated_output",
      bytes = file.info(path)$size,
      sha256 = digest::digest(file = path, algo = "sha256"),
      producing_script = sub(paste0("^", root, "/"), "", script_path),
      producing_script_sha256 = digest::digest(file = script_path, algo = "sha256"),
      build_id = "BUILD-P15-STATUS-SOURCE-GAP-20260721-V1",
      schema_version = p15_status_gap_schema_version(),
      build_date = "2026-07-21"
    )
  }
), use.names = TRUE, fill = TRUE)
data.table::fwrite(manifest, manifest_path, na = "")

cat("P15 status source gap matrix: PASS\n")
cat("Status dimensions:", nrow(gap_matrix), "\n")
cat("Source component-dimension rows:", nrow(inventory), "\n")
cat("Dimensions with a panel or bounded event source:",
    sum(gap_matrix$has_full_or_bounded_panel_source), "\n")
cat("Dimensions with a material source gap:",
    sum(gap_matrix$research_gap_state == "material_source_gap_remains"), "\n")
cat("Warning/block decisions made: 0\n")
