#!/usr/bin/env Rscript

# Build a neutral 2012-2024 status evidence layer from registered sources. This
# script records evidence and source gaps; it deliberately does not set warning or
# blocking consequences.

suppressPackageStartupMessages({
  library(countrycode)
  library(data.table)
  library(digest)
  library(dplyr)
  library(jsonlite)
  library(readr)
  library(tibble)
  library(tidyr)
})

source("R/p15_status_context.R")

root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
derived_dir <- file.path(root, "data-derived", "p15_status_context_2012_2024_v1")
governance_dir <- file.path(root, "docs", "governance")
dir.create(derived_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(governance_dir, recursive = TRUE, showWarnings = FALSE)

rel <- list(
  grid = "data-derived/p15_platform_2012_2024_v1/p15_country_year_grid_2012_2024.csv",
  boc = "data-raw/status_context_sources/boc_boe_sovereign_default_database_DEBT_2025_2026-06-30.json",
  paris = "data-raw/status_context_sources/paris_club_signed_agreements_2026-06-30/paris_club_signed_agreements_advanced_search_2026-06-30.csv",
  p13_case = "experiments/full_ladder_ratings_integrated_status_rebuild_2026-06-01/outputs/distress_restructuring_status_ledger_2024.csv",
  p14_boc = "experiments/p14_historical_validation_and_extension_2026-06-22/outputs/p14_historical_quality_equalization_2012_2023/p14_historical_default_context_source_ledger_2012_2023.csv",
  p14_paris = "experiments/p14_historical_validation_and_extension_2026-06-22/outputs/p14_historical_quality_equalization_2012_2023/p14_historical_paris_club_signed_agreements_2012_2023.csv"
)
paths <- lapply(rel, function(path) file.path(root, path))
missing <- names(paths)[!vapply(paths, file.exists, logical(1))]
if (length(missing)) {
  stop("Missing P15 status evidence inputs: ", paste(missing, collapse = ", "),
       call. = FALSE)
}

grid <- readr::read_csv(paths$grid, show_col_types = FALSE, guess_max = 100000)
p13_case <- readr::read_csv(
  paths$p13_case, show_col_types = FALSE, guess_max = 100000
)
boc <- p15_parse_boc_boe_default(paths$boc, years = 2012:2024)
paris <- p15_parse_paris_club_agreements(paths$paris, years = 2012:2024)
status_evidence <- p15_build_status_context_evidence(grid, boc, paris, p13_case)

count_semantic_mismatches <- function(rebuilt, fixture, keys, exclude = character()) {
  columns <- setdiff(intersect(names(rebuilt), names(fixture)), c(keys, exclude))
  joined <- rebuilt |>
    dplyr::inner_join(fixture, by = keys, suffix = c("__rebuilt", "__fixture"))
  key_mismatch_rows <- nrow(dplyr::anti_join(rebuilt, fixture, by = keys)) +
    nrow(dplyr::anti_join(fixture, rebuilt, by = keys))
  cell_mismatches <- 0L
  for (column in columns) {
    lhs <- joined[[paste0(column, "__rebuilt")]]
    rhs <- joined[[paste0(column, "__fixture")]]
    if (is.numeric(lhs) || is.numeric(rhs)) {
      lhs <- suppressWarnings(as.numeric(lhs))
      rhs <- suppressWarnings(as.numeric(rhs))
      mismatch <- xor(is.na(lhs), is.na(rhs)) |
        (!is.na(lhs) & !is.na(rhs) & abs(lhs - rhs) > 1e-10)
    } else {
      lhs <- as.character(lhs)
      rhs <- as.character(rhs)
      mismatch <- xor(is.na(lhs), is.na(rhs)) |
        (!is.na(lhs) & !is.na(rhs) & lhs != rhs)
    }
    cell_mismatches <- cell_mismatches + sum(mismatch)
  }
  list(
    key_mismatch_rows = key_mismatch_rows,
    cell_mismatches = cell_mismatches,
    compared_rows = nrow(joined),
    compared_columns = length(columns)
  )
}

p14_boc <- readr::read_csv(
  paths$p14_boc, show_col_types = FALSE, guess_max = 100000
)
p14_paris <- readr::read_csv(
  paths$p14_paris, show_col_types = FALSE, guess_max = 100000
)
boc_parity <- count_semantic_mismatches(
  boc |> dplyr::filter(.data$analysis_year <= 2023L),
  p14_boc,
  keys = c("analysis_year", "iso3"),
  exclude = c(
    "boc_default_source_pointer", "boc_default_source_file",
    "boc_default_source_package_id", "boc_fiscal_arrears_context_flag"
  )
)
paris_parity <- count_semantic_mismatches(
  paris |> dplyr::filter(.data$analysis_year <= 2023L),
  p14_paris,
  keys = c("analysis_year", "iso3"),
  exclude = c(
    "paris_club_source_pointer", "paris_club_source_file",
    "paris_club_source_package_id"
  )
)

stopifnot(
  nrow(status_evidence) == 2743L,
  !anyDuplicated(status_evidence[c("analysis_year", "iso3")]),
  sum(status_evidence$p13_case_evidence_row_count) == nrow(p13_case),
  boc_parity$key_mismatch_rows == 0L,
  boc_parity$cell_mismatches == 0L,
  paris_parity$key_mismatch_rows == 0L,
  paris_parity$cell_mismatches == 0L
)

boc_path <- file.path(derived_dir, "p15_boc_boe_default_context_2012_2024.csv.gz")
paris_path <- file.path(derived_dir, "p15_paris_club_treatment_context_2012_2024.csv.gz")
evidence_path <- file.path(derived_dir, "p15_status_context_evidence_2012_2024.csv.gz")
parity_path <- file.path(governance_dir, "p15_status_source_rebuild_parity_summary.csv")
summary_path <- file.path(governance_dir, "p15_status_context_evidence_summary.csv")
manifest_path <- file.path(governance_dir, "p15_status_context_evidence_manifest.csv")

data.table::fwrite(data.table::as.data.table(boc), boc_path, na = "")
data.table::fwrite(data.table::as.data.table(paris), paris_path, na = "")
data.table::fwrite(
  data.table::as.data.table(status_evidence), evidence_path, na = ""
)

parity_summary <- tibble::tibble(
  source_branch = c("boc_boe_default_and_arrears", "paris_club_treatments"),
  compared_rows = c(boc_parity$compared_rows, paris_parity$compared_rows),
  compared_columns = c(
    boc_parity$compared_columns, paris_parity$compared_columns
  ),
  key_mismatch_rows = c(
    boc_parity$key_mismatch_rows, paris_parity$key_mismatch_rows
  ),
  semantic_cell_mismatches = c(
    boc_parity$cell_mismatches, paris_parity$cell_mismatches
  ),
  parity_status = "pass"
)
data.table::fwrite(data.table::as.data.table(parity_summary), parity_path, na = "")

summary <- tibble::tibble(
  metric = c(
    "status_country_year_rows",
    "boc_boe_source_covered_country_years",
    "positive_default_stock_country_years",
    "positive_fiscal_arrears_country_years",
    "private_or_bond_default_country_years",
    "paris_club_treatment_country_years",
    "p13_case_evidence_rows",
    "p13_case_evidence_country_years",
    "positive_status_evidence_country_years",
    "status_consequences_decided",
    "systematic_status_dimensions_with_material_gap",
    "status_evidence_schema_version"
  ),
  value = c(
    as.character(nrow(status_evidence)),
    as.character(sum(status_evidence$boc_source_country_year_covered)),
    as.character(sum(
      status_evidence$boc_default_context_flag %in% TRUE, na.rm = TRUE
    )),
    as.character(sum(
      status_evidence$boc_fiscal_arrears_context_flag %in% TRUE, na.rm = TRUE
    )),
    as.character(sum(
      status_evidence$boc_private_or_bond_default_context_flag %in% TRUE,
      na.rm = TRUE
    )),
    as.character(sum(status_evidence$paris_club_same_year_agreement_flag)),
    as.character(nrow(p13_case)),
    as.character(sum(status_evidence$p13_case_evidence_present)),
    as.character(sum(status_evidence$positive_status_evidence_present)),
    "0",
    "6",
    p15_status_evidence_schema_version()
  )
)
data.table::fwrite(data.table::as.data.table(summary), summary_path, na = "")

script_path <- file.path(root, "scripts/p15/build_p15_status_context_evidence.R")
input_paths <- unique(unlist(paths))
manifest <- data.table::rbindlist(lapply(
  c(input_paths, boc_path, paris_path, evidence_path, parity_path, summary_path),
  function(path) {
    data.table::data.table(
      artifact_path = sub(paste0("^", root, "/"), "", path),
      artifact_role = if (path %in% input_paths) "source_or_parity_input" else "generated_output",
      bytes = file.info(path)$size,
      sha256 = digest::digest(file = path, algo = "sha256"),
      producing_script = sub(paste0("^", root, "/"), "", script_path),
      producing_script_sha256 = digest::digest(file = script_path, algo = "sha256"),
      build_id = "BUILD-P15-STATUS-EVIDENCE-20260721-V1",
      schema_version = p15_status_evidence_schema_version(),
      build_date = "2026-07-21"
    )
  }
), use.names = TRUE, fill = TRUE)
data.table::fwrite(manifest, manifest_path, na = "")

cat("P15 status context evidence layer: PASS\n")
cat("Country-year rows:", nrow(status_evidence), "\n")
cat("BoC/BoE predecessor mismatches: 0\n")
cat("Paris Club predecessor mismatches: 0\n")
cat("Positive status-evidence country-years:",
    sum(status_evidence$positive_status_evidence_present), "\n")
cat("Warning/block decisions made: 0\n")
