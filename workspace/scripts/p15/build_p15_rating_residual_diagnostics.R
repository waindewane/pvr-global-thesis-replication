#!/usr/bin/env Rscript

# Build exploratory residual and country-persistence diagnostics for the leading
# rating-implied candidate. These outputs diagnose calibration and heterogeneity;
# they do not approve the candidate or change the ladder.

suppressPackageStartupMessages({
  library(data.table)
  library(digest)
  library(dplyr)
  library(ggplot2)
  library(patchwork)
  library(readr)
  library(scales)
  library(tibble)
  library(tidyr)
})

source("R/p15_rating_residual_diagnostics.R")

root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
input_rel <- paste0(
  "data-derived/p15_fallback_validation_2012_2024_v1/",
  "p15_rating_integrated_anchor_validation_detail_2012_2024.csv.gz"
)
input_path <- file.path(root, input_rel)
derived_rel <- "data-derived/p15_rating_residual_diagnostics_2012_2024_v1"
derived_dir <- file.path(root, derived_rel)
figure_dir <- file.path(derived_dir, "figures")
governance_dir <- file.path(root, "docs", "governance")
dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)

detail <- readr::read_csv(
  input_path, show_col_types = FALSE, guess_max = 100000
)
sample <- p15_prepare_rating_residual_sample(detail)
country_summary <- p15_summarise_country_residuals(sample, minimum_years = 3L)
permutation_test <- p15_country_effect_permutation_test(
  sample, simulations = 9999L, seed = 20260817L
)

# Classical fixed-effect comparison, used together with the within-year
# permutation test rather than as the sole evidence.
year_only <- stats::lm(
  residual_pp ~ factor(analysis_year), data = sample
)
year_country <- stats::lm(
  residual_pp ~ factor(analysis_year) + factor(iso3), data = sample
)
country_fe_test <- stats::anova(year_only, year_country)

# Random-intercept variance decomposition conditional on common year effects.
gls_year <- nlme::gls(
  residual_pp ~ factor(analysis_year), data = sample, method = "ML"
)
lme_country <- nlme::lme(
  residual_pp ~ factor(analysis_year), random = ~1 | iso3,
  data = sample, method = "ML",
  control = nlme::lmeControl(opt = "optim")
)
lme_comparison <- stats::anova(gls_year, lme_country)
variance_components <- nlme::VarCorr(lme_country)
country_variance <- as.numeric(variance_components[1, "Variance"])
residual_variance <- as.numeric(variance_components[2, "Variance"])
intraclass_correlation <- country_variance /
  (country_variance + residual_variance)

# A calibration diagnostic: does signed error change with the estimated rate?
residual_trend <- stats::lm(
  residual_pp ~ rating_implied_rate_pct, data = sample
)
trend_vcov <- sandwich::vcovCL(
  residual_trend,
  cluster = list(sample$iso3, sample$analysis_year),
  type = "HC1", multi0 = TRUE
)
trend_estimate <- stats::coef(residual_trend)[["rating_implied_rate_pct"]]
trend_se <- sqrt(trend_vcov[
  "rating_implied_rate_pct", "rating_implied_rate_pct"
])
trend_df <- min(
  dplyr::n_distinct(sample$iso3),
  dplyr::n_distinct(sample$analysis_year)
) - 1L
trend_t <- trend_estimate / trend_se
trend_p <- 2 * stats::pt(abs(trend_t), df = trend_df, lower.tail = FALSE)

diagnostic_summary <- tibble::tribble(
  ~metric_id, ~estimate, ~standard_error, ~statistic, ~degrees_of_freedom, ~p_value, ~interpretation,
  "sample_country_years", nrow(sample), NA_real_, NA_real_, NA_real_, NA_real_, "Matched LMIC USD-primary country-years.",
  "sample_countries", dplyr::n_distinct(sample$iso3), NA_real_, NA_real_, NA_real_, NA_real_, "Countries represented in the matched sample.",
  "countries_with_3plus_years", sum(country_summary$observed_years >= 3L), NA_real_, NA_real_, NA_real_, NA_real_, "Countries shown in the persistence figure.",
  "predominantly_one_direction_3plus_years", sum(country_summary$observed_years >= 3L & country_summary$directional_pattern != "mixed_direction"), NA_real_, NA_real_, NA_real_, NA_real_, "At least 75 percent of residuals have the same sign; descriptive threshold.",
  "always_same_sign_3plus_years", sum(country_summary$observed_years >= 3L & (country_summary$all_positive | country_summary$all_negative)), NA_real_, NA_real_, NA_real_, NA_real_, "Every observed residual has the same sign; small samples remain possible.",
  "country_fixed_effect_joint_F", country_fe_test$F[[2]], NA_real_, country_fe_test$F[[2]], country_fe_test$Df[[2]], country_fe_test$`Pr(>F)`[[2]], "Classical joint test of country effects conditional on year effects.",
  "country_effect_within_year_permutation_F", permutation_test$statistic, NA_real_, permutation_test$statistic, permutation_test$numerator_df, permutation_test$p_value, "Preferred distribution-light test; residuals permuted within year.",
  "country_random_intercept_variance", country_variance, NA_real_, NA_real_, NA_real_, lme_comparison$`p-value`[[2]], "Country-level residual variance conditional on common year effects; likelihood-ratio p-value shown.",
  "residual_variance_after_country_year_structure", residual_variance, NA_real_, NA_real_, NA_real_, NA_real_, "Within-country residual variance from the random-intercept model.",
  "country_intraclass_correlation", intraclass_correlation, NA_real_, NA_real_, NA_real_, lme_comparison$`p-value`[[2]], "Share of conditional residual variance attributable to persistent country differences.",
  "residual_on_rating_rate_slope", trend_estimate, trend_se, trend_t, trend_df, trend_p, "Change in signed error per percentage-point increase in the rating-implied rate; two-way clustered inference.",
  "residual_rating_rate_spearman", stats::cor(sample$residual_pp, sample$rating_implied_rate_pct, method = "spearman"), NA_real_, NA_real_, NA_real_, NA_real_, "Rank association between signed error and rating-implied rate.",
  "absolute_residual_rating_rate_spearman", stats::cor(abs(sample$residual_pp), sample$rating_implied_rate_pct, method = "spearman"), NA_real_, NA_real_, NA_real_, NA_real_, "Rank association between error magnitude and rating-implied rate."
) |>
  dplyr::mutate(
    build_id = "BUILD-P15-RATING-RESIDUAL-DIAGNOSTICS-20260817-V1",
    decision_state = "diagnostic_no_candidate_promotion",
    schema_version = p15_rating_residual_schema_version()
  )

year_summary <- sample |>
  dplyr::group_by(.data$analysis_year) |>
  dplyr::summarise(
    matched_country_years = dplyr::n(),
    mean_residual_pp = mean(.data$residual_pp),
    median_residual_pp = stats::median(.data$residual_pp),
    mean_absolute_residual_pp = mean(abs(.data$residual_pp)),
    share_rating_implied_higher = mean(.data$residual_pp > 0),
    .groups = "drop"
  ) |>
  dplyr::mutate(schema_version = p15_rating_residual_schema_version())

worst_cases <- sample |>
  dplyr::mutate(abs_residual_pp = abs(.data$residual_pp)) |>
  dplyr::slice_max(.data$abs_residual_pp, n = 25L, with_ties = FALSE) |>
  dplyr::arrange(dplyr::desc(.data$abs_residual_pp))

sample_path <- file.path(derived_dir, "p15_rating_residual_sample.csv.gz")
country_path <- file.path(
  derived_dir, "p15_rating_residual_country_summary.csv"
)
year_path <- file.path(derived_dir, "p15_rating_residual_year_summary.csv")
worst_path <- file.path(derived_dir, "p15_rating_residual_worst_cases.csv")
summary_path <- file.path(
  governance_dir, "p15_rating_residual_diagnostic_summary.csv"
)
data.table::fwrite(data.table::as.data.table(sample), sample_path, na = "")
data.table::fwrite(
  data.table::as.data.table(country_summary), country_path, na = ""
)
data.table::fwrite(data.table::as.data.table(year_summary), year_path, na = "")
data.table::fwrite(data.table::as.data.table(worst_cases), worst_path, na = "")
data.table::fwrite(
  data.table::as.data.table(diagnostic_summary), summary_path, na = ""
)

econ_cols <- c(
  focus = "#2F6B9A", lower = "#B45A4A", neutral = "#7A7A7A",
  pale = "#E8E8E8", dark = "#303030"
)
theme_econ_clean <- function(base_size = 11) {
  ggplot2::theme_minimal(base_size = base_size, base_family = "sans") +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_line(
        color = "grey88", linewidth = 0.3
      ),
      axis.title = ggplot2::element_text(color = "grey25"),
      axis.text = ggplot2::element_text(color = "grey25"),
      plot.title = ggplot2::element_text(face = "bold", margin = ggplot2::margin(b = 6)),
      plot.subtitle = ggplot2::element_text(color = "grey30", margin = ggplot2::margin(b = 8)),
      plot.caption = ggplot2::element_text(
        color = "grey45", hjust = 0, size = ggplot2::rel(0.78),
        margin = ggplot2::margin(t = 8)
      ),
      legend.position = "top",
      legend.title = ggplot2::element_blank(),
      plot.background = ggplot2::element_rect(fill = "white", color = NA),
      panel.background = ggplot2::element_rect(fill = "white", color = NA)
    )
}

residual_scatter <- ggplot2::ggplot(
  sample,
  ggplot2::aes(
    x = .data$rating_implied_rate_pct, y = .data$residual_pp,
    color = .data$residual_direction
  )
) +
  ggplot2::geom_hline(yintercept = 0, color = "grey45", linewidth = 0.45) +
  ggplot2::geom_point(alpha = 0.68, size = 2) +
  ggplot2::geom_smooth(
    method = "lm", formula = y ~ x, se = TRUE,
    color = econ_cols[["dark"]], fill = "grey78", linewidth = 0.7,
    show.legend = FALSE
  ) +
  ggplot2::scale_color_manual(values = c(
    "Rating-implied rate higher" = econ_cols[["focus"]],
    "Rating-implied rate lower" = econ_cols[["lower"]],
    "Equal at stored precision" = econ_cols[["neutral"]]
  )) +
  ggplot2::scale_x_continuous(labels = scales::label_number(suffix = "%")) +
  ggplot2::scale_y_continuous(labels = scales::label_number(suffix = " pp")) +
  ggplot2::labs(
    title = "Residuals rise with the rating-implied rate",
    subtitle = paste0(
      "Residual = rating-implied minus observed USD primary rate\n",
      "Two-way clustered slope = ", round(trend_estimate, 2),
      " (p < 0.001)"
    ),
    x = "Rating-implied rate",
    y = "Residual (percentage points)"
  ) +
  theme_econ_clean() +
  ggplot2::theme(panel.grid.major.x = ggplot2::element_blank())

residual_histogram <- ggplot2::ggplot(
  sample, ggplot2::aes(x = .data$residual_pp)
) +
  ggplot2::geom_histogram(
    binwidth = 0.5, boundary = 0,
    fill = econ_cols[["focus"]], color = "white", linewidth = 0.35
  ) +
  ggplot2::geom_vline(xintercept = 0, color = "grey45", linewidth = 0.45) +
  ggplot2::geom_vline(
    xintercept = mean(sample$residual_pp),
    color = econ_cols[["lower"]], linetype = "dashed", linewidth = 0.7
  ) +
  ggplot2::annotate(
    "text", x = mean(sample$residual_pp), y = Inf,
    label = "Mean = -0.04 pp", vjust = 1.5, hjust = 1.05,
    color = econ_cols[["lower"]], size = 3.1
  ) +
  ggplot2::scale_x_continuous(labels = scales::label_number(suffix = " pp")) +
  ggplot2::labs(
    title = "The distribution is centered near zero",
    subtitle = "Individual errors remain substantial",
    x = "Residual",
    y = "Country-years"
  ) +
  theme_econ_clean() +
  ggplot2::theme(legend.position = "none")

residual_figure <- residual_scatter + residual_histogram +
  patchwork::plot_layout(widths = c(1.65, 1)) +
  patchwork::plot_annotation(
    title = "Rating-implied residual diagnostics",
    subtitle = "210 matched LMIC USD-primary country-years, 2012-2024",
    caption = paste0(
      "Note: Positive residuals mean the rating-implied rate exceeds the observed ",
      "issuance rate.\nThe fitted line is a calibration diagnostic, not a causal ",
      "relationship. Source: P15 integrated observed and rating candidate layers."
    ),
    theme = ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", size = 15),
      plot.subtitle = ggplot2::element_text(color = "grey30", size = 11),
      plot.caption = ggplot2::element_text(
        color = "grey45", hjust = 0, size = 8.5
      )
    )
  )

repeated <- country_summary |>
  dplyr::filter(.data$observed_years >= 3L) |>
  dplyr::mutate(country_label = paste0(.data$country, " (", .data$iso3, ")")) |>
  dplyr::arrange(.data$mean_residual_pp)
country_levels <- repeated$country_label
heatmap_grid <- tidyr::expand_grid(
  analysis_year = 2012:2024,
  iso3 = repeated$iso3
) |>
  dplyr::left_join(
    repeated |>
      dplyr::select("iso3", "country_label"),
    by = "iso3"
  ) |>
  dplyr::left_join(
    sample |>
      dplyr::select("analysis_year", "iso3", "residual_pp"),
    by = c("analysis_year", "iso3")
  ) |>
  dplyr::mutate(
    country_label = factor(.data$country_label, levels = country_levels)
  )
repeated <- repeated |>
  dplyr::mutate(
    country_label = factor(.data$country_label, levels = country_levels)
  )
residual_limit <- max(abs(sample$residual_pp))

country_heatmap <- ggplot2::ggplot(
  heatmap_grid,
  ggplot2::aes(
    x = .data$analysis_year, y = .data$country_label,
    fill = .data$residual_pp
  )
) +
  ggplot2::geom_tile(color = "white", linewidth = 0.25) +
  ggplot2::scale_fill_gradient2(
    low = econ_cols[["lower"]], mid = "white", high = econ_cols[["focus"]],
    midpoint = 0, limits = c(-residual_limit, residual_limit),
    na.value = "grey94",
    labels = scales::label_number(suffix = " pp")
  ) +
  ggplot2::scale_x_continuous(
    breaks = 2012:2024, expand = ggplot2::expansion(add = 0)
  ) +
  ggplot2::labs(
    title = "Country-year residuals",
    x = NULL, y = NULL,
    fill = "Residual"
  ) +
  theme_econ_clean(base_size = 9.5) +
  ggplot2::theme(
    panel.grid = ggplot2::element_blank(),
    axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
    legend.position = "top"
  )

country_means_plot <- ggplot2::ggplot(
  repeated,
  ggplot2::aes(x = .data$mean_residual_pp, y = .data$country_label)
) +
  ggplot2::geom_vline(xintercept = 0, color = "grey50", linewidth = 0.45) +
  ggplot2::geom_segment(
    ggplot2::aes(
      x = .data$confidence_interval_95_lower_pp,
      xend = .data$confidence_interval_95_upper_pp,
      yend = .data$country_label
    ),
    color = "grey65", linewidth = 0.5
  ) +
  ggplot2::geom_point(
    ggplot2::aes(
      size = .data$observed_years,
      color = .data$directional_pattern
    ),
    alpha = 0.9
  ) +
  ggplot2::scale_color_manual(values = c(
    "predominantly_rating_implied_higher" = econ_cols[["focus"]],
    "predominantly_rating_implied_lower" = econ_cols[["lower"]],
    "mixed_direction" = econ_cols[["neutral"]]
  ), labels = c(
    "predominantly_rating_implied_higher" = "Higher in at least 75% of years",
    "predominantly_rating_implied_lower" = "Lower in at least 75% of years",
    "mixed_direction" = "Mixed direction"
  )) +
  ggplot2::scale_size_continuous(range = c(1.8, 4.2), breaks = c(3, 6, 9, 12)) +
  ggplot2::scale_x_continuous(labels = scales::label_number(suffix = " pp")) +
  ggplot2::labs(
    title = "Country means",
    x = "Mean residual",
    y = NULL,
    size = "Observed years",
    color = "Direction"
  ) +
  theme_econ_clean(base_size = 9.5) +
  ggplot2::guides(color = "none", size = "none") +
  ggplot2::theme(
    axis.text.y = ggplot2::element_blank(),
    axis.ticks.y = ggplot2::element_blank(),
    panel.grid.major.y = ggplot2::element_blank(),
    legend.position = "none"
  )

country_figure <- country_heatmap + country_means_plot +
  patchwork::plot_layout(widths = c(2.4, 1.6)) +
  patchwork::plot_annotation(
    title = "Residuals contain persistent country-level structure",
    subtitle = paste0(
      "Countries with at least three matched years; within-year permutation ",
      "p < 0.001; country ICC = ",
      scales::percent(intraclass_correlation, accuracy = 1)
    ),
    caption = paste0(
      "Note: Residual = rating-implied minus observed USD primary rate. Grey ",
      "cells are years without a matched observation.\nBlue/red points mean the ",
      "rating-implied rate was higher/lower in at least 75% of observed years; grey ",
      "means mixed direction. Lines are descriptive 95% intervals.\n",
      "No individual-country sign test survives false-discovery-rate correction."
    ),
    theme = ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", size = 15),
      plot.subtitle = ggplot2::element_text(color = "grey30", size = 11),
      plot.caption = ggplot2::element_text(
        color = "grey45", hjust = 0, size = 8.5
      )
    )
  )

residual_png <- file.path(figure_dir, "p15_rating_residual_diagnostics.png")
residual_pdf <- file.path(figure_dir, "p15_rating_residual_diagnostics.pdf")
country_png <- file.path(
  figure_dir, "p15_rating_country_residual_persistence.png"
)
country_pdf <- file.path(
  figure_dir, "p15_rating_country_residual_persistence.pdf"
)
ggplot2::ggsave(
  residual_png, residual_figure, width = 11, height = 5.4,
  dpi = 320, bg = "white"
)
ggplot2::ggsave(
  residual_pdf, residual_figure, width = 11, height = 5.4,
  device = grDevices::pdf, bg = "white"
)
ggplot2::ggsave(
  country_png, country_figure, width = 12, height = 11.5,
  dpi = 320, bg = "white"
)
ggplot2::ggsave(
  country_pdf, country_figure, width = 12, height = 11.5,
  device = grDevices::pdf, bg = "white"
)

output_paths <- c(
  sample_path, country_path, year_path, worst_path, summary_path,
  residual_png, residual_pdf, country_png, country_pdf
)
code_rel <- c(
  "R/p15_rating_residual_diagnostics.R",
  "scripts/p15/build_p15_rating_residual_diagnostics.R",
  "tests/testthat/test-p15-rating-residual-diagnostics.R"
)
manifest <- dplyr::bind_rows(
  tibble::tibble(
    artifact_role = "input", path = input_rel,
    sha256 = digest::digest(input_path, algo = "sha256", file = TRUE)
  ),
  tibble::tibble(
    artifact_role = "output",
    path = sub(paste0("^", root, "/"), "", output_paths),
    sha256 = vapply(
      output_paths, digest::digest, character(1), algo = "sha256", file = TRUE
    )
  ),
  tibble::tibble(
    artifact_role = "code", path = code_rel,
    sha256 = vapply(
      file.path(root, code_rel), digest::digest, character(1),
      algo = "sha256", file = TRUE
    )
  )
) |>
  dplyr::mutate(
    build_id = "BUILD-P15-RATING-RESIDUAL-DIAGNOSTICS-20260817-V1",
    schema_version = p15_rating_residual_schema_version()
  )
manifest_path <- file.path(
  governance_dir, "p15_rating_residual_diagnostics_manifest.csv"
)
data.table::fwrite(data.table::as.data.table(manifest), manifest_path, na = "")

stopifnot(
  nrow(sample) == 210L,
  dplyr::n_distinct(sample$iso3) == 43L,
  sum(country_summary$observed_years >= 3L) == 30L,
  permutation_test$p_value <= 0.001,
  intraclass_correlation > 0,
  all(country_summary$individual_country_inference_state !=
        "sign_consistency_after_multiple_testing"),
  all(file.exists(output_paths))
)
message(
  "Built rating residual diagnostics: 210 country-years; country permutation p = ",
  permutation_test$p_value, "; ICC = ",
  round(intraclass_correlation, 3), "."
)
