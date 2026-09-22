#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(data.table); library(digest); library(dplyr); library(readr)
  library(sandwich); library(tibble); library(tidyr)
})
source("R/p15_rating_country_history_calibration.R")

root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
input_rel <- paste0(
  "data-derived/p15_fallback_validation_2012_2024_v1/",
  "p15_rating_integrated_anchor_validation_detail_2012_2024.csv.gz"
)
input_path <- file.path(root, input_rel)
derived_rel <- "data-derived/p15_rating_country_history_calibration_2012_2024_v1"
derived_dir <- file.path(root, derived_rel)
governance_dir <- file.path(root, "docs", "governance")
dir.create(derived_dir, recursive = TRUE, showWarnings = FALSE)

detail <- readr::read_csv(input_path, show_col_types = FALSE, guess_max = 100000)
sample <- p15_prepare_rating_country_history_sample(detail)
stopifnot(nrow(sample) == 210L)

predictions <- p15_build_rating_country_history_predictions(sample)
metrics <- p15_summarise_rating_country_history(predictions)
annual <- predictions |>
  dplyr::filter(.data$validation_design == "expanding_past_only") |>
  dplyr::group_by(.data$analysis_year, .data$model_id) |>
  dplyr::summarise(
    test_rows = dplyr::n(),
    mean_error_pp = mean(.data$residual_pp),
    mean_absolute_error_pp = mean(.data$absolute_error_pp),
    rmse_pp = sqrt(mean(.data$squared_error_pp2)),
    .groups = "drop"
  ) |>
  dplyr::mutate(schema_version = p15_rating_country_history_schema_version())
coefficients <- predictions |>
  dplyr::distinct(
    .data$validation_design, .data$fold_id,
    .data$training_rows, .data$training_countries,
    .data$training_first_year, .data$training_last_year,
    .data$affine_intercept, .data$affine_slope,
    .data$country_sd, .data$residual_sd, .data$shrinkage_k,
    .data$schema_version
  )
worst_cases <- predictions |>
  dplyr::filter(.data$model_id == "affine_year_demeaned_country_eb") |>
  dplyr::group_by(.data$validation_design) |>
  dplyr::slice_max(.data$absolute_error_pp, n = 15L, with_ties = FALSE) |>
  dplyr::ungroup() |>
  dplyr::arrange(.data$validation_design, dplyr::desc(.data$absolute_error_pp))

paired_inference <- function(predictions, comparator) {
  wide <- predictions |>
    dplyr::filter(.data$model_id %in% c(
      comparator, "affine_year_demeaned_country_eb"
    )) |>
    dplyr::select(
      "validation_design", "analysis_year", "iso3",
      "model_id", "absolute_error_pp"
    ) |>
    tidyr::pivot_wider(
      names_from = "model_id", values_from = "absolute_error_pp"
    ) |>
    dplyr::mutate(
      absolute_error_change_pp =
        .data$affine_year_demeaned_country_eb - .data[[comparator]]
    )
  dplyr::bind_rows(lapply(unique(wide$validation_design), function(design) {
    x <- dplyr::filter(wide, .data$validation_design == design)
    fit <- stats::lm(absolute_error_change_pp ~ 1, data = x)
    vc <- sandwich::vcovCL(
      fit, cluster = list(x$iso3, x$analysis_year),
      type = "HC1", multi0 = TRUE
    )
    estimate <- mean(x$absolute_error_change_pp)
    standard_error <- sqrt(vc[[1]])
    degrees_of_freedom <- min(
      dplyr::n_distinct(x$iso3), dplyr::n_distinct(x$analysis_year)
    ) - 1L
    annual_blocks <- x |>
      dplyr::group_by(.data$analysis_year) |>
      dplyr::summarise(
        block_sum = sum(.data$absolute_error_change_pp),
        block_mean = mean(.data$absolute_error_change_pp),
        .groups = "drop"
      )
    signs <- as.matrix(expand.grid(rep(
      list(c(-1, 1)), nrow(annual_blocks)
    )))
    observed_block_statistic <- abs(sum(annual_blocks$block_sum))
    permuted_block_statistics <- abs(as.numeric(
      signs %*% annual_blocks$block_sum
    ))
    tibble::tibble(
      validation_design = design,
      comparator_model_id = comparator,
      candidate_model_id = "affine_year_demeaned_country_eb",
      test_rows = nrow(x),
      mean_absolute_error_change_pp = estimate,
      two_way_clustered_standard_error = standard_error,
      degrees_of_freedom = degrees_of_freedom,
      two_way_clustered_p_value = 2 * stats::pt(
        abs(estimate / standard_error),
        df = degrees_of_freedom, lower.tail = FALSE
      ),
      improved_observation_share = mean(x$absolute_error_change_pp < 0),
      improved_years = sum(annual_blocks$block_mean < 0),
      tested_years = nrow(annual_blocks),
      exact_year_block_signflip_p_value = mean(
        permuted_block_statistics >= observed_block_statistic - 1e-12
      ),
      inference_state = "exploratory_not_confirmatory",
      schema_version = p15_rating_country_history_schema_version()
    )
  }))
}
inference <- dplyr::bind_rows(
  paired_inference(predictions, "raw"),
  paired_inference(predictions, "affine_rate")
)

paths <- c(
  predictions = file.path(
    derived_dir, "p15_rating_country_history_predictions.csv.gz"
  ),
  metrics = file.path(
    derived_dir, "p15_rating_country_history_metrics.csv"
  ),
  annual = file.path(
    derived_dir, "p15_rating_country_history_annual_summary.csv"
  ),
  coefficients = file.path(
    derived_dir, "p15_rating_country_history_fold_parameters.csv"
  ),
  inference = file.path(
    derived_dir, "p15_rating_country_history_paired_inference.csv"
  ),
  worst_cases = file.path(
    derived_dir, "p15_rating_country_history_worst_cases.csv"
  )
)
data.table::fwrite(predictions, paths[["predictions"]], na = "")
data.table::fwrite(metrics, paths[["metrics"]], na = "")
data.table::fwrite(annual, paths[["annual"]], na = "")
data.table::fwrite(coefficients, paths[["coefficients"]], na = "")
data.table::fwrite(inference, paths[["inference"]], na = "")
data.table::fwrite(worst_cases, paths[["worst_cases"]], na = "")

candidate_summary <- metrics |>
  dplyr::left_join(
    inference,
    by = c(
      "validation_design",
      "model_id" = "candidate_model_id",
      "schema_version"
    )
  ) |>
  dplyr::mutate(
    build_id = "BUILD-P15-RATING-COUNTRY-HISTORY-20260818-V1",
    candidate_state = dplyr::if_else(
      .data$model_id == "affine_year_demeaned_country_eb",
      "promising_exploratory_candidate_requires_confirmatory_gate",
      "comparison_reference"
    ),
    promotion_state = "diagnostic_non_promoted",
    canonical_output_changed = FALSE
  )
summary_path <- file.path(
  governance_dir, "p15_rating_country_history_candidate_summary.csv"
)
data.table::fwrite(candidate_summary, summary_path, na = "")

output_paths <- c(unname(paths), summary_path)
code_rel <- c(
  "R/p15_rating_country_history_calibration.R",
  "scripts/p15/build_p15_rating_country_history_calibration.R",
  "tests/testthat/test-p15-rating-country-history-calibration.R"
)
manifest <- dplyr::bind_rows(
  tibble::tibble(
    artifact_role = "input", path = input_rel,
    sha256 = digest::digest(input_path, "sha256", file = TRUE)
  ),
  tibble::tibble(
    artifact_role = "output",
    path = sub(paste0("^", root, "/"), "", output_paths),
    sha256 = vapply(
      output_paths, digest::digest, character(1), "sha256", file = TRUE
    )
  ),
  tibble::tibble(
    artifact_role = "code", path = code_rel,
    sha256 = vapply(
      file.path(root, code_rel), digest::digest, character(1),
      "sha256", file = TRUE
    )
  )
) |>
  dplyr::mutate(
    build_id = "BUILD-P15-RATING-COUNTRY-HISTORY-20260818-V1",
    promotion_state = "diagnostic_non_promoted",
    schema_version = p15_rating_country_history_schema_version()
  )
manifest_path <- file.path(
  governance_dir, "p15_rating_country_history_calibration_manifest.csv"
)
data.table::fwrite(manifest, manifest_path, na = "")

stopifnot(
  nrow(predictions) == 828L,
  nrow(dplyr::filter(
    predictions,
    .data$validation_design == "expanding_past_only",
    .data$model_id == "affine_year_demeaned_country_eb"
  )) == 176L,
  all(dplyr::filter(
    predictions,
    .data$validation_design == "expanding_past_only"
  )$training_last_year < dplyr::filter(
    predictions,
    .data$validation_design == "expanding_past_only"
  )$analysis_year)
)
message("Built non-promoted P15 rating country-history candidate evidence.")
