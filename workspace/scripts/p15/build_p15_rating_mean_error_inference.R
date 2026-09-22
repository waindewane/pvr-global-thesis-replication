#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(data.table)
  library(digest)
  library(dplyr)
  library(readr)
})

source("R/p15_rating_mean_error_inference.R")

root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
input_rel <- paste0(
  "data-derived/p15_fallback_validation_2012_2024_v1/",
  "p15_rating_integrated_anchor_validation_detail_2012_2024.csv.gz"
)
input_path <- file.path(root, input_rel)
output_rel <- paste0(
  "docs/governance/",
  "p15_rating_leading_candidate_mean_error_inference.csv"
)
output_path <- file.path(root, output_rel)
manifest_rel <- "docs/governance/p15_rating_mean_error_inference_manifest.csv"
manifest_path <- file.path(root, manifest_rel)

detail <- readr::read_csv(
  input_path, show_col_types = FALSE, guess_max = 100000
)
sample <- detail |>
  dplyr::filter(
    .data$validation_question_id == "Q-USD-NEW-BORROWING",
    .data$variant_id ==
      "boy_available_agencies_median_mapped_spread_dgs7",
    .data$historical_lmic_reporting_scope %in% TRUE,
    .data$accuracy_summary_eligible %in% TRUE
  )
inference <- p15_build_rating_mean_error_inference(sample) |>
  dplyr::mutate(
    rating_variant_id =
      "boy_available_agencies_median_mapped_spread_dgs7",
    validation_question_id = "Q-USD-NEW-BORROWING",
    validation_sample = "historical_lmic_scope",
    build_id = "BUILD-P15-RATING-MEAN-ERROR-INFERENCE-20260817-V1",
    decision_state = "diagnostic_inference_no_promotion",
    .before = 1L
  )
data.table::fwrite(data.table::as.data.table(inference), output_path, na = "")

code_rel <- c(
  "R/p15_rating_mean_error_inference.R",
  "scripts/p15/build_p15_rating_mean_error_inference.R",
  "tests/testthat/test-p15-rating-mean-error-inference.R"
)
paths <- c(input_rel, output_rel, code_rel)
manifest <- data.frame(
  artifact_role = c("input", "output", rep("code", length(code_rel))),
  path = paths,
  sha256 = vapply(
    file.path(root, paths), digest::digest, character(1),
    algo = "sha256", file = TRUE
  ),
  build_id = "BUILD-P15-RATING-MEAN-ERROR-INFERENCE-20260817-V1",
  schema_version = p15_rating_mean_error_inference_schema(),
  stringsAsFactors = FALSE
)
data.table::fwrite(data.table::as.data.table(manifest), manifest_path, na = "")

preferred <- inference |>
  dplyr::filter(.data$preferred_panel_inference)
stopifnot(
  nrow(sample) == 210L,
  nrow(preferred) == 1L,
  abs(preferred$mean_signed_error_pp + 0.04308542) < 1e-7,
  preferred$p_value_two_sided > 0.8,
  all(inference$decision_state == "diagnostic_inference_no_promotion")
)
message(
  "Saved rating mean-error inference: two-way clustered p = ",
  round(preferred$p_value_two_sided, 4), "."
)
