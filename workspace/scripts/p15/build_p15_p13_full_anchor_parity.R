#!/usr/bin/env Rscript

# Execute the frozen P13 architecture in an isolated P15 staging directory and
# compare every generated P13 CSV semantically against the preserved P13 fixtures.
# Frozen P13 outputs and output/tables are never written by this driver.

suppressPackageStartupMessages({
  library(data.table)
  library(readr)
  library(dplyr)
  library(stringr)
})

root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
legacy_experiment_rel <-
  "experiments/full_ladder_ratings_integrated_status_rebuild_2026-06-01"
legacy_out <- file.path(root, legacy_experiment_rel, "outputs")
legacy_script <- file.path(
  root, legacy_experiment_rel, "scripts/build_p13_paper_facing_freeze_2024.R"
)
staging_experiment_rel <-
  "data-derived/p15_p13_full_anchor_parity_2024_v1/isolated_p13_rebuild"
staging_out <- file.path(root, staging_experiment_rel, "outputs")
governance_dir <- file.path(root, "docs", "governance")
dir.create(staging_out, recursive = TRUE, showWarnings = FALSE)
dir.create(governance_dir, recursive = TRUE, showWarnings = FALSE)

input_files <- c(
  "p12_full_ladder_country_year_2024.csv",
  "p12a_secondary_direct_issue_universe_2024.csv",
  "p12a_feature_rich_secondary_accepted_issue_layer_2024.csv",
  "method_warnings.csv",
  "missing_data_and_blockers.csv",
  "task_register.csv",
  "computational_decision_log.csv",
  "reproducibility_manifest.csv"
)
input_source_paths <- file.path(legacy_out, input_files)
input_staging_paths <- file.path(staging_out, input_files)
stopifnot(file.exists(legacy_script), all(file.exists(input_source_paths)))
copied <- file.copy(
  input_source_paths,
  input_staging_paths,
  overwrite = TRUE,
  copy.date = TRUE
)
stopifnot(all(copied))

generated_csvs <- c(
  "p13_input_validation_summary_2024.csv",
  "p13_full_labelled_rate_value_ladder_2024.csv",
  "p13_rate_value_admissibility_register_2024.csv",
  "p13_legacy_p8_audit_crosswalk_2024.csv",
  "p13_nonpermissible_rate_and_exclusion_register_2024.csv",
  "p13_permissible_market_rate_ladder_2024.csv",
  "p13_ladder_order_decision_register_2024.csv",
  "p13_best_available_benchmark_rate_2024.csv",
  "p13_country_status_note_register_2024.csv",
  "p13_country_context_source_pointer_2024.csv",
  "p13_display_decision_register_2024.csv",
  "p13_main_best_available_benchmark_table_2024.csv",
  "p13_appendix_full_labelled_ladder_table_2024.csv",
  "p13_appendix_permissible_market_rate_ladder_2024.csv",
  "p13_sensitivity_and_diagnostic_ladder_table_2024.csv",
  "p13_warning_label_dictionary_2024.csv",
  "p13_observed_primary_diagnostic_table_2024.csv",
  "p13_ladder_tier_observed_primary_validation_2024.csv",
  "p13_ladder_tier_observed_primary_validation_summary_2024.csv",
  "p13_quality_check_summary_2024.csv"
)
stopifnot(all(file.exists(file.path(legacy_out, generated_csvs))))

# The legacy builder hard-codes its experiment directory. Change that one line in
# memory so the exact code executes against staging copies and writes only to the
# isolated P15 directory.
legacy_lines <- readLines(legacy_script, warn = FALSE)
experiment_line <- grep(
  '^experiment_dir <- "experiments/full_ladder_ratings_integrated_status_rebuild_2026-06-01"$',
  legacy_lines
)
stopifnot(length(experiment_line) == 1L)
legacy_lines[[experiment_line]] <- paste0(
  "experiment_dir <- \"", staging_experiment_rel, "\""
)
legacy_environment <- new.env(parent = globalenv())
legacy_warning_messages <- character()
withCallingHandlers(
  eval(parse(text = legacy_lines, keep.source = TRUE), envir = legacy_environment),
  warning = function(warning) {
    legacy_warning_messages <<- c(
      legacy_warning_messages,
      conditionMessage(warning)
    )
    invokeRestart("muffleWarning")
  }
)

staging_generated <- file.path(staging_out, generated_csvs)
stopifnot(all(file.exists(staging_generated)))

volatile_fields <- c(
  "generated_at", "created_at", "last_updated", "retrieval_or_generation_date"
)

read_semantic_csv <- function(path) {
  x <- readr::read_csv(
    path,
    show_col_types = FALSE,
    col_types = readr::cols(.default = readr::col_character()),
    na = character()
  )
  keep <- setdiff(names(x), volatile_fields)
  x <- x[, sort(keep), drop = FALSE]
  for (name in names(x)) {
    value <- as.character(x[[name]])
    value[is.na(value)] <- "<NA>"
    value <- gsub(
      staging_experiment_rel,
      legacy_experiment_rel,
      value,
      fixed = TRUE
    )
    x[[name]] <- value
  }
  x
}

row_fingerprints <- function(x) {
  if (!nrow(x)) return(character())
  values <- lapply(x, function(value) {
    gsub("\\u001f", "<UNIT_SEPARATOR>", value, fixed = TRUE)
  })
  sort(do.call(paste, c(values, sep = "\u001f")))
}

compare_one <- function(file_name) {
  rebuilt <- read_semantic_csv(file.path(staging_out, file_name))
  reference <- read_semantic_csv(file.path(legacy_out, file_name))
  same_columns <- identical(names(rebuilt), names(reference))
  rebuilt_rows <- row_fingerprints(rebuilt)
  reference_rows <- row_fingerprints(reference)
  rebuilt_only <- length(setdiff(rebuilt_rows, reference_rows))
  reference_only <- length(setdiff(reference_rows, rebuilt_rows))
  exact_semantic_match <- same_columns &&
    identical(rebuilt_rows, reference_rows)
  data.table::data.table(
    artifact = file_name,
    rebuilt_rows = nrow(rebuilt),
    reference_rows = nrow(reference),
    rebuilt_columns = ncol(rebuilt),
    reference_columns = ncol(reference),
    same_semantic_columns = same_columns,
    rebuilt_only_row_fingerprints = rebuilt_only,
    reference_only_row_fingerprints = reference_only,
    exact_semantic_match = exact_semantic_match,
    rebuilt_sha256 = digest::digest(
      file = file.path(staging_out, file_name), algo = "sha256"
    ),
    reference_sha256 = digest::digest(
      file = file.path(legacy_out, file_name), algo = "sha256"
    ),
    raw_hash_match_expected = FALSE,
    comparison_note = paste(
      "Raw hashes differ when generated_at differs; semantic comparison excludes",
      "volatile timestamps and normalizes the isolated staging path."
    )
  )
}

comparison <- data.table::rbindlist(
  lapply(generated_csvs, compare_one),
  use.names = TRUE,
  fill = TRUE
)

mandatory_counts <- c(
  p13_full_labelled_rate_value_ladder_2024.csv = 5064L,
  p13_rate_value_admissibility_register_2024.csv = 5064L,
  p13_nonpermissible_rate_and_exclusion_register_2024.csv = 4608L,
  p13_permissible_market_rate_ladder_2024.csv = 456L,
  p13_best_available_benchmark_rate_2024.csv = 211L
)
for (file_name in names(mandatory_counts)) {
  found <- comparison[artifact == file_name, rebuilt_rows]
  stopifnot(identical(as.integer(found), as.integer(mandatory_counts[[file_name]])))
}
stopifnot(nrow(comparison) == length(generated_csvs))

comparison_path <- file.path(
  governance_dir, "p15_p13_full_anchor_parity_artifact_comparison.csv"
)
summary_path <- file.path(
  governance_dir, "p15_p13_full_anchor_parity_summary.csv"
)
warning_path <- file.path(
  governance_dir, "p15_p13_full_anchor_rebuild_warning_summary.csv"
)
manifest_path <- file.path(
  governance_dir, "p15_p13_full_anchor_parity_manifest.csv"
)
data.table::fwrite(comparison, comparison_path, na = "")

warning_summary <- data.table::data.table(
  warning_message = legacy_warning_messages
)[, .N, by = warning_message][order(-N, warning_message)]
data.table::setnames(warning_summary, "N", "occurrences")
data.table::fwrite(warning_summary, warning_path, na = "")

summary <- data.table::data.table(
  metric = c(
    "p13_generated_csv_artifacts_compared",
    "p13_generated_csv_artifact_mismatches",
    "p13_full_ladder_rows",
    "p13_admissibility_rows",
    "p13_nonpermissible_rows",
    "p13_permissible_rows",
    "p13_selected_country_rows",
    "legacy_builder_warning_occurrences",
    "legacy_builder_unique_warning_messages",
    "p13_full_anchor_parity_status"
  ),
  value = c(
    as.character(nrow(comparison)),
    as.character(sum(!comparison$exact_semantic_match)),
    "5064",
    "5064",
    "4608",
    "456",
    "211",
    as.character(length(legacy_warning_messages)),
    as.character(nrow(warning_summary)),
    if (all(comparison$exact_semantic_match)) "pass" else "fail"
  )
)
data.table::fwrite(summary, summary_path, na = "")

script_path <- file.path(root, "scripts/p15/build_p15_p13_full_anchor_parity.R")
manifest_files <- c(
  legacy_script,
  input_source_paths,
  staging_generated,
  comparison_path,
  summary_path,
  warning_path
)
manifest <- data.table::rbindlist(lapply(manifest_files, function(path) {
  data.table::data.table(
    artifact_path = sub(paste0("^", root, "/"), "", path),
    artifact_role = dplyr::case_when(
      path == legacy_script ~ "frozen_parity_builder",
      path %in% input_source_paths ~ "parity_input",
      path %in% staging_generated ~ "isolated_rebuild_output",
      TRUE ~ "parity_governance_output"
    ),
    bytes = file.info(path)$size,
    sha256 = digest::digest(file = path, algo = "sha256"),
    producing_script = sub(paste0("^", root, "/"), "", script_path),
    producing_script_sha256 = digest::digest(
      file = script_path, algo = "sha256"
    ),
    build_id = "BUILD-P15-P13-FULL-ANCHOR-PARITY-20260721-V1",
    parity_specification_id = "SPEC-P15-P13-LEGACY-PARITY-V1",
    build_date = "2026-07-21"
  )
}), use.names = TRUE, fill = TRUE)
data.table::fwrite(manifest, manifest_path, na = "")

if (!all(comparison$exact_semantic_match)) {
  failed <- comparison[exact_semantic_match == FALSE, artifact]
  stop(
    "P15/P13 full-anchor parity failed for: ", paste(failed, collapse = ", "),
    call. = FALSE
  )
}

cat("P15/P13 full 2024 anchor parity: PASS\n")
cat("Generated CSV artifacts compared:", nrow(comparison), "\n")
cat("Full labelled rows: 5064\n")
cat("Permissible rows: 456\n")
cat("Selected country rows: 211\n")
