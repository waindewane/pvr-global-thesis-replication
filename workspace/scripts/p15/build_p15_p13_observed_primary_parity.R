#!/usr/bin/env Rscript

# Reconstruct the complete P13 observed-primary processor baseline through common
# P15 functions. The exact P13 source snapshot is the mandatory parity input. The
# newer broad P15 source is processed separately as a candidate source comparison.

suppressPackageStartupMessages({
  library(data.table)
  library(dplyr)
  library(readr)
  library(stringr)
  library(purrr)
})

source("R/ids.R")
source("R/lseg_benchmarks.R")
source("R/p15_observed_primary.R")

root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
derived_dir <- file.path(root, "data-derived", "p15_observed_market_2024_v1")
governance_dir <- file.path(root, "docs", "governance")
dir.create(derived_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(governance_dir, recursive = TRUE, showWarnings = FALSE)

legacy_zip_rel <- paste0(
  "sources/market_rates/lseg_research_grade_usd_eur_2026-05-22/",
  "pvr-global-lseg-research-grade-usd-eur_20260522_143419.zip"
)
legacy_member <- paste0(
  "pvr-global-lseg-research-grade-usd-eur_20260522_143419/derived/",
  "lseg_usd_eur_bond_context_20260522_143419.csv"
)
p15_universe_rel <- paste0(
  "data-derived/p15_unified_lseg_source_2012_2024_v1/",
  "p15_instrument_year_universe_2012_2024.csv.gz"
)
reference_issue_rel <- paste0(
  "experiments/paper_candidate_2024_clean_v1_2026-05-27/outputs/",
  "paper_candidate_issue_audit_2024.csv"
)
reference_evidence_rel <- paste0(
  "experiments/paper_candidate_2024_clean_v1_2026-05-27/outputs/",
  "paper_candidate_benchmark_evidence_2024.csv"
)
p13_best_rel <- paste0(
  "experiments/full_ladder_ratings_integrated_status_rebuild_2026-06-01/outputs/",
  "p13_best_available_benchmark_rate_2024.csv"
)

required_paths <- file.path(root, c(
  legacy_zip_rel,
  p15_universe_rel,
  reference_issue_rel,
  reference_evidence_rel,
  p13_best_rel,
  "data-raw/world_bank_countries.json"
))
stopifnot(all(file.exists(required_paths)))

read_zip_csv <- function(zip_path, member) {
  con <- unz(zip_path, member, open = "rb")
  on.exit(close(con), add = TRUE)
  readr::read_csv(con, show_col_types = FALSE, guess_max = 100000)
}

normalize_for_compare <- function(x) {
  if (inherits(x, "Date") || inherits(x, "IDate")) return(as.character(x))
  if (inherits(x, "integer64")) return(as.numeric(x))
  x
}

values_equal <- function(actual, reference, tolerance = 1e-12) {
  actual <- normalize_for_compare(actual)
  reference <- normalize_for_compare(reference)
  if (is.numeric(actual) || is.integer(actual) ||
      is.numeric(reference) || is.integer(reference)) {
    actual <- suppressWarnings(as.numeric(actual))
    reference <- suppressWarnings(as.numeric(reference))
    return(
      (is.na(actual) & is.na(reference)) |
        (!is.na(actual) & !is.na(reference) & abs(actual - reference) <= tolerance)
    )
  }
  actual <- as.character(actual)
  reference <- as.character(reference)
  (is.na(actual) & is.na(reference)) |
    (!is.na(actual) & !is.na(reference) & actual == reference)
}

compare_semantic_tables <- function(actual, reference, keys, fields, tolerance = 1e-12) {
  actual <- data.table::as.data.table(actual)
  reference <- data.table::as.data.table(reference)
  stopifnot(!anyDuplicated(actual[, ..keys]), !anyDuplicated(reference[, ..keys]))
  merged <- merge(
    actual[, c(keys, fields), with = FALSE],
    reference[, c(keys, fields), with = FALSE],
    by = keys,
    all = TRUE,
    suffixes = c("__p15", "__p13"),
    sort = TRUE
  )
  merged[, key_presence_state := data.table::fcase(
    Reduce(`&`, lapply(paste0(fields, "__p15"), function(nm) is.na(get(nm)))) &
      !Reduce(`&`, lapply(paste0(fields, "__p13"), function(nm) is.na(get(nm)))),
    "reference_only",
    !Reduce(`&`, lapply(paste0(fields, "__p15"), function(nm) is.na(get(nm)))) &
      Reduce(`&`, lapply(paste0(fields, "__p13"), function(nm) is.na(get(nm)))),
    "p15_only",
    default = "both"
  )]
  mismatch_matrix <- vapply(fields, function(field) {
    !values_equal(
      merged[[paste0(field, "__p15")]],
      merged[[paste0(field, "__p13")]],
      tolerance = tolerance
    )
  }, logical(nrow(merged)))
  if (!is.matrix(mismatch_matrix)) {
    mismatch_matrix <- matrix(mismatch_matrix, ncol = length(fields))
  }
  colnames(mismatch_matrix) <- fields
  merged[, mismatch_field_count := rowSums(mismatch_matrix)]
  merged[, mismatch_fields := apply(mismatch_matrix, 1, function(row) {
    paste(fields[row], collapse = ";")
  })]
  merged[, parity_pass := key_presence_state == "both" & mismatch_field_count == 0L]
  merged
}

country_metadata <- p15_prepare_primary_country_metadata(
  read_country_metadata(file.path(root, "data-raw/world_bank_countries.json"))
)

legacy_rows <- read_zip_csv(file.path(root, legacy_zip_rel), legacy_member)
legacy_issue <- p15_build_primary_issue_audit(
  legacy_rows,
  country_metadata,
  target_year = 2024L,
  source_artifact = legacy_zip_rel,
  source_file = legacy_member,
  source_package_id = "SRC-LSEG-RG-20260522",
  source_role = "p13_legacy_parity_input"
)
legacy_evidence <- p15_build_primary_country_evidence(
  legacy_issue,
  target_year = 2024L,
  ladder_variant = "p15_p13_legacy_parity_2024",
  source_extraction_run_id = "20260522_143419"
)

reference_issue <- readr::read_csv(
  file.path(root, reference_issue_rel),
  show_col_types = FALSE,
  guess_max = 100000
)
reference_evidence <- readr::read_csv(
  file.path(root, reference_evidence_rel),
  show_col_types = FALSE,
  guess_max = 100000
) |>
  dplyr::filter(.data$benchmark_source_tier == "observed_primary_issuance")
p13_best <- readr::read_csv(
  file.path(root, p13_best_rel),
  show_col_types = FALSE,
  guess_max = 100000
) |>
  dplyr::filter(.data$selected_source_class == "lseg_research_grade_observed_primary")

issue_keys <- c("analysis_year", "country_key", "economic_issue_key")
issue_compare_fields <- setdiff(
  intersect(names(legacy_issue), names(reference_issue)),
  c("source_package_id", "source_role")
)
issue_compare_fields <- setdiff(issue_compare_fields, issue_keys)
issue_parity <- compare_semantic_tables(
  legacy_issue,
  reference_issue,
  keys = issue_keys,
  fields = issue_compare_fields
)

country_keys <- c("analysis_year", "iso3")
country_compare_fields <- setdiff(
  intersect(names(legacy_evidence), names(reference_evidence)),
  c(
    country_keys,
    "ladder_variant",
    "source_package_id",
    "source_role",
    "parity_specification_id",
    # These two fields are overwritten later by the combined primary-secondary
    # country signal gate. They belong to the full-ladder parity gate, not the
    # observed-primary processor parity gate.
    "manual_review_required",
    "sanitation_reason"
  )
)
country_parity <- compare_semantic_tables(
  legacy_evidence,
  reference_evidence,
  keys = country_keys,
  fields = country_compare_fields
)

selected <- legacy_evidence |>
  dplyr::filter(.data$selected_pvr_admissible) |>
  dplyr::select(
    analysis_year,
    iso3,
    country,
    p15_rate_pct = market_rate_pct,
    p15_maturity_years = market_maturity_years,
    p15_issue_count = issue_count,
    p15_total_weight_usd = total_weight_usd,
    p15_quality_band = benchmark_quality_band,
    p15_included_issue_keys = included_issue_keys,
    p15_included_isins = included_isins,
    p15_source_package_id = source_package_id,
    parity_specification_id
  ) |>
  dplyr::left_join(
    p13_best |>
      dplyr::select(
        analysis_year,
        iso3,
        p13_rate_pct = selected_rate_pct,
        p13_maturity_years = selected_maturity_years,
        p13_issue_count = selected_issue_count,
        p13_source_class = selected_source_class,
        p13_tier_id = selected_tier_id,
        p13_warning_label = selected_warning_label,
        p13_rate_value_id = selected_p13_rate_value_id
      ),
    by = c("analysis_year", "iso3")
  ) |>
  dplyr::mutate(
    rate_abs_diff_pp = abs(.data$p15_rate_pct - .data$p13_rate_pct),
    maturity_abs_diff_years = abs(
      .data$p15_maturity_years - .data$p13_maturity_years
    ),
    rate_parity_pass = .data$rate_abs_diff_pp <= 5e-12,
    maturity_parity_pass = .data$maturity_abs_diff_years <= 5e-12,
    issue_count_reference_available = !is.na(.data$p13_issue_count),
    issue_count_parity_pass = is.na(.data$p13_issue_count) |
      .data$p15_issue_count == .data$p13_issue_count,
    source_class_parity_pass =
      .data$p13_source_class == "lseg_research_grade_observed_primary",
    observed_primary_processor_parity_pass = .data$rate_parity_pass &
      .data$maturity_parity_pass & .data$issue_count_parity_pass &
      .data$source_class_parity_pass
  ) |>
  dplyr::arrange(.data$iso3)

if (any(!issue_parity$parity_pass)) {
  message("Observed-primary issue parity mismatches:")
  print(issue_parity[parity_pass == FALSE, c(issue_keys, "key_presence_state", "mismatch_fields"), with = FALSE])
}
if (any(!country_parity$parity_pass)) {
  message("Observed-primary country parity mismatches:")
  print(country_parity[parity_pass == FALSE, c(country_keys, "key_presence_state", "mismatch_fields"), with = FALSE])
}
if (any(!selected$observed_primary_processor_parity_pass, na.rm = TRUE)) {
  message("P13-selected observed-primary parity mismatches:")
  print(selected |> dplyr::filter(!.data$observed_primary_processor_parity_pass))
}

stopifnot(
  nrow(legacy_issue) == 186L,
  sum(legacy_issue$selected_primary_issue) == 114L,
  nrow(legacy_evidence) == 40L,
  sum(legacy_evidence$selected_pvr_admissible) == 25L,
  nrow(issue_parity) == 186L,
  all(issue_parity$parity_pass),
  nrow(country_parity) == 40L,
  all(country_parity$parity_pass),
  nrow(selected) == 25L,
  all(selected$observed_primary_processor_parity_pass)
)

# The newer broad source is deliberately a separate candidate input. Processing it
# here measures source-vintage/scope changes without changing the passing P13 mode.
p15_universe <- data.table::fread(
  file.path(root, p15_universe_rel),
  showProgress = FALSE
) |>
  tibble::as_tibble()
new_issue <- p15_build_primary_issue_audit(
  p15_universe,
  country_metadata,
  target_year = 2024L,
  source_artifact = p15_universe_rel,
  source_file = basename(p15_universe_rel),
  source_package_id = "SRC-P15-LSEG-UNIFIED-20260721-V1",
  source_role = "p15_new_source_candidate"
)
new_evidence <- p15_build_primary_country_evidence(
  new_issue,
  target_year = 2024L,
  ladder_variant = "p15_new_source_candidate_2024",
  source_extraction_run_id = "BUILD-P15-LSEG-SOURCE-20260721"
)

source_comparison <- merge(
  data.table::as.data.table(legacy_evidence)[, .(
    country_key,
    iso3_legacy = iso3,
    country_legacy = country,
    legacy_rate_pct = market_rate_pct,
    legacy_maturity_years = market_maturity_years,
    legacy_issue_count = issue_count,
    legacy_selected_pvr_admissible = selected_pvr_admissible
  )],
  data.table::as.data.table(new_evidence)[, .(
    country_key,
    iso3_new = iso3,
    country_new = country,
    new_source_rate_pct = market_rate_pct,
    new_source_maturity_years = market_maturity_years,
    new_source_issue_count = issue_count,
    new_source_selected_pvr_admissible = selected_pvr_admissible
  )],
  by = "country_key",
  all = TRUE,
  sort = TRUE
)
source_comparison[, `:=`(
  iso3 = data.table::fcoalesce(iso3_legacy, iso3_new),
  country = data.table::fcoalesce(country_legacy, country_new),
  rate_abs_diff_pp = abs(legacy_rate_pct - new_source_rate_pct),
  maturity_abs_diff_years = abs(legacy_maturity_years - new_source_maturity_years)
)]
source_comparison[, source_comparison_state := data.table::fcase(
  is.na(legacy_rate_pct), "new_source_only",
  is.na(new_source_rate_pct), "legacy_parity_source_only",
  abs(legacy_rate_pct - new_source_rate_pct) <= 1e-12 &
    legacy_issue_count == new_source_issue_count,
  "exact_same_rate_and_issue_count",
  default = "shared_country_changed_source_result"
)]
data.table::setcolorder(source_comparison, c(
  "country_key", "iso3", "country", "source_comparison_state",
  setdiff(names(source_comparison), c(
    "country_key", "iso3", "country", "source_comparison_state"
  ))
))

paths <- c(
  primary_issue = file.path(derived_dir, "p15_p13_primary_issue_audit_2024.csv.gz"),
  primary_country = file.path(derived_dir, "p15_p13_primary_country_evidence_2024.csv.gz"),
  primary_selected = file.path(derived_dir, "p15_p13_primary_selected_parity_2024.csv"),
  new_source_issue = file.path(derived_dir, "p15_new_source_primary_issue_audit_2024.csv.gz"),
  new_source_country = file.path(derived_dir, "p15_new_source_primary_country_evidence_2024.csv.gz"),
  source_comparison = file.path(derived_dir, "p15_primary_source_change_comparison_2024.csv")
)
data.table::fwrite(data.table::as.data.table(legacy_issue), paths[["primary_issue"]], na = "")
data.table::fwrite(data.table::as.data.table(legacy_evidence), paths[["primary_country"]], na = "")
data.table::fwrite(data.table::as.data.table(selected), paths[["primary_selected"]], na = "")
data.table::fwrite(data.table::as.data.table(new_issue), paths[["new_source_issue"]], na = "")
data.table::fwrite(data.table::as.data.table(new_evidence), paths[["new_source_country"]], na = "")
data.table::fwrite(source_comparison, paths[["source_comparison"]], na = "")

source_counts <- source_comparison[, .N, by = source_comparison_state]
summary <- data.table::rbindlist(list(
  data.table::data.table(metric = c(
    "p13_primary_issue_audit_rows",
    "p13_primary_selected_issue_rows",
    "p13_primary_issue_fields_compared",
    "p13_primary_issue_mismatch_rows",
    "p13_primary_country_evidence_rows",
    "p13_primary_country_fields_compared",
    "p13_primary_country_mismatch_rows",
    "p13_selected_primary_rows",
    "p13_selected_primary_mismatch_rows",
    "p13_selected_primary_max_rate_abs_diff_pp",
    "p13_selected_primary_max_maturity_abs_diff_years",
    "p15_new_source_primary_issue_rows",
    "p15_new_source_primary_country_rows",
    "p15_new_source_primary_admissible_rows"
  ), value = as.character(c(
    nrow(legacy_issue),
    sum(legacy_issue$selected_primary_issue),
    length(issue_compare_fields),
    sum(!issue_parity$parity_pass),
    nrow(legacy_evidence),
    length(country_compare_fields),
    sum(!country_parity$parity_pass),
    nrow(selected),
    sum(!selected$observed_primary_processor_parity_pass),
    max(selected$rate_abs_diff_pp),
    max(selected$maturity_abs_diff_years),
    nrow(new_issue),
    nrow(new_evidence),
    sum(new_evidence$selected_pvr_admissible)
  ))),
  source_counts[, .(
    metric = paste0("new_source_comparison_", source_comparison_state),
    value = as.character(N)
  )]
), use.names = TRUE, fill = TRUE)
summary_path <- file.path(governance_dir, "p15_p13_observed_primary_parity_summary.csv")
data.table::fwrite(summary, summary_path, na = "")

script_path <- file.path(root, "scripts/p15/build_p15_p13_observed_primary_parity.R")
manifest_paths <- c(unname(paths), summary_path)
manifest <- data.table::rbindlist(lapply(manifest_paths, function(path) {
  contents <- data.table::fread(path, showProgress = FALSE)
  data.table::data.table(
    artifact_path = sub(paste0("^", root, "/"), "", path),
    rows = nrow(contents),
    columns = ncol(contents),
    bytes = file.info(path)$size,
    sha256 = digest::digest(file = path, algo = "sha256"),
    producing_script = sub(paste0("^", root, "/"), "", script_path),
    producing_script_sha256 = digest::digest(file = script_path, algo = "sha256"),
    build_id = "BUILD-P15-P13-PRIMARY-PARITY-20260721-V1",
    parity_specification_id = "SPEC-P15-P13-LEGACY-PARITY-V1",
    build_date = "2026-07-21"
  )
}), use.names = TRUE, fill = TRUE)
data.table::fwrite(
  manifest,
  file.path(governance_dir, "p15_p13_observed_primary_parity_manifest.csv"),
  na = ""
)

cat("P15/P13 observed-primary parity: PASS\n")
cat("Issue rows reconstructed:", nrow(legacy_issue), "\n")
cat("Primary evidence rows reconstructed:", nrow(legacy_evidence), "\n")
cat("P13-selected primary rows reconstructed:", nrow(selected), "\n")
cat("Maximum selected-rate difference:", max(selected$rate_abs_diff_pp), "pp\n")
