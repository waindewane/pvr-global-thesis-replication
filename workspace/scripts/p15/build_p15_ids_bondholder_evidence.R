#!/usr/bin/env Rscript

# Build the cached 2012-2024 IDS Bondholders evidence and missingness panel. This
# stage identifies the source object and validates P13 overlaps without choosing
# its final ladder rank.

suppressPackageStartupMessages({
  library(data.table)
  library(digest)
  library(dplyr)
  library(readr)
  library(tibble)
})

source("R/p15_ids_evidence.R")

root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
derived_dir <- file.path(root, "data-derived", "p15_ids_evidence_2012_2024_v1")
governance_dir <- file.path(root, "docs", "governance")
dir.create(derived_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(governance_dir, recursive = TRUE, showWarnings = FALSE)

grid_rel <- "data-derived/p15_platform_2012_2024_v1/p15_country_year_grid_2012_2024.csv"
ids_rel <- "experiments/full_ladder_database_all_years_2026-05-20/outputs/ids_terms_core_all_years.csv"
p13_rel <- "experiments/full_ladder_ratings_integrated_status_rebuild_2026-06-01/outputs/p13_full_labelled_rate_value_ladder_2024.csv"
grid_path <- file.path(root, grid_rel)
ids_path <- file.path(root, ids_rel)
p13_path <- file.path(root, p13_rel)
stopifnot(file.exists(grid_path), file.exists(ids_path), file.exists(p13_path))

grid <- readr::read_csv(grid_path, show_col_types = FALSE, guess_max = 100000)
ids_terms <- readr::read_csv(ids_path, show_col_types = FALSE, guess_max = 100000)
p13 <- readr::read_csv(p13_path, show_col_types = FALSE, guess_max = 100000)

evidence <- p15_build_ids_bondholder_evidence(grid, ids_terms)

source_object_dictionary <- tibble::tribble(
  ~indicator_id, ~source_owner_label, ~stored_field, ~measurement_basis, ~p15_source_object_interpretation, ~not_equivalent_to,
  "DT.INR.DPPG", "Average interest on new external debt commitments (%)", "official_rate_pct", "new_external_debt_commitments_country_year_creditor_category", "aggregate_contractual_commitment_rate_proxy", "security_level_issue_yield_or_secondary_market_yield",
  "DT.MAT.DPPG", "Average maturity on new external debt commitments (years)", "official_maturity_years", "new_external_debt_commitments_country_year_creditor_category", "aggregate_commitment_weighted_maturity_proxy", "instrument_level_contractual_cash_flow_schedule",
  "DT.GPA.DPPG", "Average grace period on new external debt commitments (years)", "official_grace_years", "new_external_debt_commitments_country_year_creditor_category", "aggregate_commitment_weighted_grace_proxy", "instrument_level_first_payment_date_or_amortization_schedule"
) |>
  dplyr::mutate(
    creditor_counterpart_id = "BND",
    creditor_counterpart_label = "Bondholders",
    source_package_id = "SRC-WB-IDS-CORE-ALL-YEARS-20260520",
    schema_version = p15_ids_evidence_schema_version()
  )

p13_ids <- p13 |>
  dplyr::filter(.data$tier_id == "ids_bondholders_public_proxy") |>
  dplyr::select(
    analysis_year, iso3,
    p13_rate_value_id,
    p13_rate_present = rate_value_present,
    p13_rate_pct = rate_pct,
    p13_maturity_years = maturity_years,
    p13_detailed_tier_id = detailed_ladder_tier_id
  )
p13_crosswalk <- evidence |>
  dplyr::filter(.data$analysis_year == 2024L) |>
  dplyr::left_join(p13_ids, by = c("analysis_year", "iso3")) |>
  dplyr::mutate(
    p13_present_rate_reconstructed =
      !.data$p13_rate_present |
      (!is.na(.data$official_rate_pct) &
         abs(.data$official_rate_pct - .data$p13_rate_pct) <= 1e-12),
    p13_present_maturity_reconstructed =
      !.data$p13_rate_present |
      (!is.na(.data$official_maturity_years) &
         abs(.data$official_maturity_years - .data$p13_maturity_years) <= 1e-12),
    p13_legacy_presence_state = dplyr::case_when(
      .data$p13_rate_present ~ "present_in_p13_legacy_ladder",
      .data$ids_rate_class == "positive_1_30" ~
        "source_rate_exists_but_not_present_in_p13_legacy_ladder",
      TRUE ~ "no_p13_legacy_rate"
    )
  )

stopifnot(
  nrow(evidence) == 2743L,
  nrow(p13_crosswalk) == 211L,
  sum(p13_crosswalk$p13_rate_present) == 4L,
  all(p13_crosswalk$p13_present_rate_reconstructed),
  all(p13_crosswalk$p13_present_maturity_reconstructed)
)

evidence_path <- file.path(
  derived_dir, "p15_ids_bondholders_country_year_evidence_2012_2024.csv.gz"
)
crosswalk_path <- file.path(
  governance_dir, "p15_ids_bondholders_p13_anchor_crosswalk_2024.csv"
)
summary_path <- file.path(
  governance_dir, "p15_ids_bondholders_evidence_summary.csv"
)
missingness_path <- file.path(
  governance_dir, "p15_ids_bondholders_missingness_summary_2012_2024.csv"
)
dictionary_path <- file.path(
  governance_dir, "p15_ids_source_object_dictionary.csv"
)
manifest_path <- file.path(
  governance_dir, "p15_ids_bondholders_evidence_manifest.csv"
)

data.table::fwrite(data.table::as.data.table(evidence), evidence_path, na = "")
data.table::fwrite(
  data.table::as.data.table(p13_crosswalk), crosswalk_path, na = ""
)

missingness <- evidence |>
  dplyr::count(
    .data$analysis_year, .data$ids_missingness_state,
    .data$ids_rate_class, .data$bondholder_term_validity_state,
    name = "country_year_rows"
  ) |>
  dplyr::arrange(
    .data$analysis_year, .data$ids_missingness_state, .data$ids_rate_class
  )
data.table::fwrite(
  data.table::as.data.table(missingness), missingness_path, na = ""
)
data.table::fwrite(
  data.table::as.data.table(source_object_dictionary), dictionary_path, na = ""
)

summary <- tibble::tibble(
  metric = c(
    "ids_bondholder_country_year_rows",
    "ids_source_object_dictionary_rows",
    "ids_source_country_year_rows_present",
    "ids_source_country_year_rows_absent",
    "ids_positive_1_30_rate_rows",
    "ids_usable_term_candidate_rows",
    "ids_complete_source_terms_flag_rows",
    "p13_2024_ids_present_rows",
    "p13_2024_rate_mismatches",
    "p13_2024_maturity_mismatches",
    "p13_2024_source_positive_rates_not_in_legacy_ladder",
    "ids_admissibility_decisions_made",
    "ids_selection_decisions_made",
    "ids_evidence_schema_version"
  ),
  value = c(
    as.character(nrow(evidence)),
    as.character(nrow(source_object_dictionary)),
    as.character(sum(evidence$ids_source_country_year_row_present)),
    as.character(sum(!evidence$ids_source_country_year_row_present)),
    as.character(sum(evidence$ids_rate_class == "positive_1_30")),
    as.character(sum(
      evidence$bondholder_term_validity_state ==
        "bondholder_terms_usable_subject_to_source_checks"
    )),
    as.character(sum(evidence$source_has_complete_terms_flag %in% TRUE)),
    as.character(sum(p13_crosswalk$p13_rate_present)),
    as.character(sum(!p13_crosswalk$p13_present_rate_reconstructed)),
    as.character(sum(!p13_crosswalk$p13_present_maturity_reconstructed)),
    as.character(sum(
      p13_crosswalk$p13_legacy_presence_state ==
        "source_rate_exists_but_not_present_in_p13_legacy_ladder"
    )),
    "0",
    "0",
    p15_ids_evidence_schema_version()
  )
)
data.table::fwrite(data.table::as.data.table(summary), summary_path, na = "")

script_path <- file.path(root, "scripts/p15/build_p15_ids_bondholder_evidence.R")
manifest <- data.table::rbindlist(lapply(
  c(grid_path, ids_path, p13_path, evidence_path, crosswalk_path,
    missingness_path, dictionary_path, summary_path),
  function(path) {
    data.table::data.table(
      artifact_path = sub(paste0("^", root, "/"), "", path),
      artifact_role = if (path %in% c(grid_path, ids_path, p13_path)) {
        "source_or_anchor_input"
      } else {
        "generated_output"
      },
      bytes = file.info(path)$size,
      sha256 = digest::digest(file = path, algo = "sha256"),
      producing_script = sub(paste0("^", root, "/"), "", script_path),
      producing_script_sha256 = digest::digest(file = script_path, algo = "sha256"),
      build_id = "BUILD-P15-IDS-BONDHOLDER-EVIDENCE-20260721-V1",
      schema_version = p15_ids_evidence_schema_version(),
      build_date = "2026-07-21"
    )
  }
), use.names = TRUE, fill = TRUE)
data.table::fwrite(manifest, manifest_path, na = "")

cat("P15 IDS Bondholders evidence layer: PASS\n")
cat("Country-year rows:", nrow(evidence), "\n")
cat("Source rows present:", sum(evidence$ids_source_country_year_row_present), "\n")
cat("Positive 1-30 percent rates:",
    sum(evidence$ids_rate_class == "positive_1_30"), "\n")
cat("P13 present-rate mismatches: 0\n")
cat("Admissibility or selection decisions made: 0\n")
