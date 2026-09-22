#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(data.table); library(digest); library(dplyr); library(ggplot2)
  library(patchwork); library(readr); library(scales); library(tibble); library(tidyr)
})
source("R/p15_rating_out_of_sample_calibration.R")
source("R/p15_rating_residual_diagnostics.R")

root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
input_rel <- paste0(
  "data-derived/p15_fallback_validation_2012_2024_v1/",
  "p15_rating_integrated_anchor_validation_detail_2012_2024.csv.gz"
)
input_path <- file.path(root, input_rel)
derived_rel <- "data-derived/p15_rating_oos_calibration_2012_2024_v1"
derived_dir <- file.path(root, derived_rel)
figure_dir <- file.path(derived_dir, "figures")
governance_dir <- file.path(root, "docs", "governance")
dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)

detail <- readr::read_csv(input_path, show_col_types = FALSE, guess_max = 100000)
sample <- detail |>
  dplyr::filter(
    .data$validation_question_id == "Q-USD-NEW-BORROWING",
    .data$variant_id == "boy_available_agencies_median_mapped_spread_dgs7",
    .data$historical_lmic_reporting_scope %in% TRUE,
    .data$accuracy_summary_eligible %in% TRUE
  ) |>
  dplyr::transmute(
    analysis_year = as.integer(.data$analysis_year),
    iso3 = as.character(.data$iso3), country = as.character(.data$country),
    observed_rate_pct = as.numeric(.data$anchor_rate_pct),
    rating_rate_pct = as.numeric(.data$variant_rate_pct),
    risk_free_rate_pct = as.numeric(.data$risk_free_7y_pct),
    rating_spread_pct = as.numeric(.data$variant_spread_pct)
  ) |>
  dplyr::arrange(.data$iso3, .data$analysis_year)
stopifnot(nrow(sample) == 210L, !anyDuplicated(sample[c("analysis_year", "iso3")]))

predictions <- p15_build_rating_oos_predictions(sample)
metrics <- p15_summarise_rating_oos(predictions)
inference <- p15_rating_oos_paired_inference(predictions)
coefficients <- predictions |>
  dplyr::filter(.data$model_id != "raw") |>
  dplyr::distinct(
    .data$validation_design, .data$fold_id, .data$model_id,
    .data$training_rows, .data$training_countries,
    .data$training_first_year, .data$training_last_year,
    .data$calibration_intercept, .data$calibration_slope,
    .data$schema_version
  )
rolling_year <- predictions |>
  dplyr::filter(.data$validation_design == "expanding_past_only") |>
  dplyr::group_by(.data$analysis_year, .data$model_id) |>
  dplyr::summarise(
    test_rows = dplyr::n(),
    mean_absolute_error_pp = mean(.data$absolute_error_pp),
    mean_error_pp = mean(.data$residual_pp),
    .groups = "drop"
  ) |>
  dplyr::left_join(
    coefficients |>
      dplyr::filter(.data$validation_design == "expanding_past_only") |>
      dplyr::transmute(
        analysis_year = as.integer(.data$fold_id), .data$model_id,
        .data$calibration_intercept, .data$calibration_slope
      ),
    by = c("analysis_year", "model_id")
  ) |>
  dplyr::mutate(schema_version = p15_rating_oos_schema_version())

# Check whether cross-country calibration removes the previously diagnosed country
# structure. This is descriptive and does not create country adjustments.
loco_structure <- dplyr::bind_rows(lapply(c("raw", "affine_rate"), function(model) {
  x <- predictions |>
    dplyr::filter(
      .data$validation_design == "leave_one_country_out",
      .data$model_id == model
    ) |>
    dplyr::transmute(
      analysis_year, iso3, country, residual_pp
    )
  perm <- p15_country_effect_permutation_test(x, simulations = 9999L, seed = 20260817L)
  mixed <- nlme::lme(
    residual_pp ~ factor(analysis_year), random = ~1 | iso3,
    data = x, method = "ML", control = nlme::lmeControl(opt = "optim")
  )
  vc <- nlme::VarCorr(mixed)
  country_var <- as.numeric(vc[1, "Variance"])
  residual_var <- as.numeric(vc[2, "Variance"])
  tibble::tibble(
    model_id = model,
    country_permutation_p_value = perm$p_value,
    country_variance = country_var,
    residual_variance = residual_var,
    country_intraclass_correlation = country_var / (country_var + residual_var),
    schema_version = p15_rating_oos_schema_version()
  )
}))

paths <- c(
  predictions = file.path(derived_dir, "p15_rating_oos_predictions.csv.gz"),
  metrics = file.path(derived_dir, "p15_rating_oos_metrics.csv"),
  inference = file.path(derived_dir, "p15_rating_oos_paired_inference.csv"),
  coefficients = file.path(derived_dir, "p15_rating_oos_coefficients.csv"),
  rolling_year = file.path(derived_dir, "p15_rating_oos_rolling_year_summary.csv"),
  country_structure = file.path(derived_dir, "p15_rating_oos_country_structure.csv"),
  governance = file.path(governance_dir, "p15_rating_oos_calibration_decision_summary.csv")
)
data.table::fwrite(predictions, paths[["predictions"]], na = "")
data.table::fwrite(metrics, paths[["metrics"]], na = "")
data.table::fwrite(inference, paths[["inference"]], na = "")
data.table::fwrite(coefficients, paths[["coefficients"]], na = "")
data.table::fwrite(rolling_year, paths[["rolling_year"]], na = "")
data.table::fwrite(loco_structure, paths[["country_structure"]], na = "")

decision_summary <- metrics |>
  dplyr::left_join(
    inference,
    by = c("validation_design", "model_id", "schema_version")
  ) |>
  dplyr::mutate(
    build_id = "BUILD-P15-RATING-OOS-CALIBRATION-20260817-V1",
    decision_state = dplyr::case_when(
      .data$model_id == "raw" ~ "reference_candidate_unchanged",
      .data$model_id == "affine_rate" ~
        "modest_inconsistent_time_gain_not_approved",
      TRUE ~ "economically_structured_sensitivity_does_not_improve_mae"
    ),
    promotion_state = "diagnostic_no_calibration_promotion"
  )
data.table::fwrite(decision_summary, paths[["governance"]], na = "")

model_labels <- c(
  raw = "Original rating method",
  affine_rate = "Affine rate recalibration",
  spread_component = "Spread-only recalibration"
)
design_labels <- c(
  fixed_future_2020_2024 = "Train 2012-2019; test 2020-2024",
  expanding_past_only = "Expanding past-only tests, 2016-2024",
  leave_one_country_out = "Leave one country out"
)
cols <- c(raw = "#777777", affine_rate = "#2F6B9A", spread_component = "#B45A4A")
theme_clean <- function() ggplot2::theme_minimal(base_size = 11) +
  ggplot2::theme(
    panel.grid.minor = ggplot2::element_blank(),
    panel.grid.major.y = ggplot2::element_blank(),
    plot.title = ggplot2::element_text(face = "bold"),
    plot.subtitle = ggplot2::element_text(color = "grey30"),
    plot.caption = ggplot2::element_text(color = "grey45", hjust = 0, size = 8),
    legend.position = "top", legend.title = ggplot2::element_blank()
  )

p1 <- metrics |>
  dplyr::mutate(
    design = factor(design_labels[.data$validation_design], levels = rev(design_labels)),
    model = factor(model_labels[.data$model_id], levels = model_labels)
  ) |>
  ggplot2::ggplot(ggplot2::aes(.data$mean_absolute_error_pp, .data$design,
                               color = .data$model)) +
  ggplot2::geom_point(size = 3, position = ggplot2::position_dodge(width = 0.5)) +
  ggplot2::scale_color_manual(values = unname(cols)) +
  ggplot2::scale_x_continuous(labels = scales::label_number(suffix = " pp")) +
  ggplot2::labs(
    title = "Recalibration improves cross-country fit more than future-year fit",
    subtitle = "Mean absolute error; lower is better",
    x = "Mean absolute error", y = NULL
  ) + theme_clean()

rolling_plot <- rolling_year |>
  dplyr::filter(.data$model_id %in% c("raw", "affine_rate")) |>
  dplyr::select("analysis_year", "model_id", "mean_absolute_error_pp") |>
  tidyr::pivot_wider(names_from = "model_id", values_from = "mean_absolute_error_pp") |>
  dplyr::mutate(
    mae_change_pp = .data$affine_rate - .data$raw,
    result = dplyr::if_else(.data$mae_change_pp < 0, "Recalibration better", "Original better")
  )
p2 <- ggplot2::ggplot(
  rolling_plot,
  ggplot2::aes(factor(.data$analysis_year), .data$mae_change_pp, fill = .data$result)
) +
  ggplot2::geom_hline(yintercept = 0, color = "grey45", linewidth = 0.45) +
  ggplot2::geom_col(width = 0.7) +
  ggplot2::scale_fill_manual(values = c(
    "Recalibration better" = cols[["affine_rate"]],
    "Original better" = cols[["spread_component"]]
  )) +
  ggplot2::scale_y_continuous(labels = scales::label_number(suffix = " pp")) +
  ggplot2::labs(
    title = "The future-year gain is inconsistent",
    subtitle = "Change in mean absolute error from training only on earlier years",
    x = "Test year", y = "Recalibrated minus original MAE"
  ) + theme_clean()

figure <- p1 / p2 + patchwork::plot_annotation(
  title = "Out-of-sample rating recalibration",
  subtitle = "LMIC USD-primary validation; 2012-2024 evidence",
  caption = paste0(
    "Note: Negative changes favor recalibration. The leave-country-out design tests ",
    "geographic generalization but is not a future-time test.\n",
    "The fixed and expanding designs never train on the test year. No calibration ",
    "rule is promoted by this diagnostic."
  ),
  theme = ggplot2::theme(
    plot.title = ggplot2::element_text(face = "bold", size = 15),
    plot.subtitle = ggplot2::element_text(color = "grey30"),
    plot.caption = ggplot2::element_text(color = "grey45", hjust = 0, size = 8)
  )
)
figure_png <- file.path(figure_dir, "p15_rating_oos_calibration_comparison.png")
figure_pdf <- file.path(figure_dir, "p15_rating_oos_calibration_comparison.pdf")
ggplot2::ggsave(figure_png, figure, width = 10.5, height = 8, dpi = 320, bg = "white")
ggplot2::ggsave(figure_pdf, figure, width = 10.5, height = 8,
                device = grDevices::pdf, bg = "white")

output_paths <- c(unname(paths), figure_png, figure_pdf)
code_rel <- c(
  "R/p15_rating_out_of_sample_calibration.R",
  "scripts/p15/build_p15_rating_out_of_sample_calibration.R",
  "tests/testthat/test-p15-rating-out-of-sample-calibration.R"
)
manifest <- dplyr::bind_rows(
  tibble::tibble(artifact_role = "input", path = input_rel,
                 sha256 = digest::digest(input_path, "sha256", file = TRUE)),
  tibble::tibble(
    artifact_role = "output", path = sub(paste0("^", root, "/"), "", output_paths),
    sha256 = vapply(output_paths, digest::digest, character(1), "sha256", file = TRUE)
  ),
  tibble::tibble(
    artifact_role = "code", path = code_rel,
    sha256 = vapply(file.path(root, code_rel), digest::digest, character(1),
                    "sha256", file = TRUE)
  )
) |>
  dplyr::mutate(
    build_id = "BUILD-P15-RATING-OOS-CALIBRATION-20260817-V1",
    schema_version = p15_rating_oos_schema_version()
  )
manifest_path <- file.path(governance_dir, "p15_rating_oos_calibration_manifest.csv")
data.table::fwrite(manifest, manifest_path, na = "")

stopifnot(
  nrow(predictions) == 1458L,
  all(c("fixed_future_2020_2024", "expanding_past_only", "leave_one_country_out") %in%
        metrics$validation_design),
  all(file.exists(output_paths))
)
message("Built out-of-sample rating calibration evidence.")

