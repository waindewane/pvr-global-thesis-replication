#!/usr/bin/env Rscript
# Standalone scientific figure of reported means; no uncertainty intervals inferred.
source("scripts/p15/activate_p15_environment.R")
suppressPackageStartupMessages({library(data.table); library(jsonlite); library(digest)})
args <- commandArgs(trailingOnly = TRUE)
input <- if (length(args) >= 1) args[[1]] else "data-derived/p15_loan_comparisons_20260910_v3/period_summary.csv"
out <- if (length(args) >= 2) args[[2]] else "docs/thesis_design/loan_valuations_2026-09-10/figures"
dir.create(out, recursive = TRUE, showWarnings = FALSE)
all <- fread(input)
z <- all[benchmark_view == "non_peer" & reference == "modern_DAC_common_reference" &
  cohort %in% c("aiddata_fixed_usd", "add_central_all_currency")]
stopifnot(nrow(z) == 4L, !anyDuplicated(z[, .(dataset, period)]), all(is.finite(z$mean_delta_ge_pp)))
z[, dataset_order := match(dataset, c("aiddata", "add"))]
z[, period_order := match(period, c("2018_2021", "2022_2024"))]
setorder(z, dataset_order, period_order)
z[, actual_period_label := fifelse(period == "2018_2021", "2018–2021",
  fifelse(dataset == "aiddata", "2022–2023", "2022–2024"))]
z[, plot_y := c(4.25, 3.40, 1.65, 0.80)]
z[, value_label := sprintf("%+.2f", mean_delta_ge_pp)]
z[, sample_label := paste0("n = ", format(records, big.mark = ",", trim = TRUE), " records")]
z[, bar_color := fifelse(period == "2018_2021", "#527387", "#23475E")]
make_plot <- function() {
  par(mar = c(3.0, 7.9, 3.8, 1.3), oma = c(3.0, 0.4, 0, 0.1),
      family = "sans", mgp = c(1.7, 0.5, 0), tcl = -0.2, las = 1)
  plot(NA_real_, NA_real_, xlim = c(-10, 10), ylim = c(0.35, 5.10),
    xaxs = "i", yaxs = "i", axes = FALSE, xlab = "", ylab = "")
  abline(v = seq(-10, 10, 5), col = "#E5E8EA", lwd = 0.7)
  abline(v = 0, col = "#75818A", lwd = 1)
  for (j in seq_len(nrow(z))) {
    value <- z$mean_delta_ge_pp[j]; y <- z$plot_y[j]
    rect(min(0, value), y - 0.14, max(0, value), y + 0.14,
      col = z$bar_color[j], border = NA)
    text(value + ifelse(value < 0, -0.23, 0.23), y, z$value_label[j],
      adj = ifelse(value < 0, 1, 0), cex = 0.98, col = "#162A37", font = 2)
    text(-10.65, y + 0.075, z$actual_period_label[j], adj = 1,
      cex = 0.99, xpd = NA, col = "#253541")
    text(-10.65, y - 0.17, z$sample_label[j], adj = 1,
      cex = 0.81, xpd = NA, col = "#56636B")
  }
  text(-10, 4.91, "AidData: fixed-rate USD loans", adj = 0, font = 2, cex = 1.02)
  text(-10, 4.64, "China; central governments or sovereign guarantees", adj = 0,
    cex = 0.83, col = "#56636B")
  text(-10, 2.31, "African Debt Database: central-government loans", adj = 0,
    font = 2, cex = 1.02)
  text(-10, 2.04, "Multiple official creditors; includes other/unknown currencies", adj = 0,
    cex = 0.83, col = "#56636B")
  axis(1, at = seq(-10, 10, 5), col = "#75818A", col.axis = "#253541", cex.axis = 0.9)
  mtext("Mean market minus DAC grant element (percentage points)", side = 1, line = 1.8, cex = 0.92)
  mtext("Market–DAC grant-element differences", side = 3, line = 2.4,
    adj = 0, font = 2, cex = 1.25)
  mtext("Non-peer benchmarks · modern DAC reference · equal weight per record", side = 3,
    line = 1.1, adj = 0, cex = 0.85, col = "#56636B")
  mtext("Each record compares the same assumed repayments under two discount rates.", side = 1,
    outer = TRUE, line = 0.55, adj = 0.025, cex = 0.78, col = "#56636B")
  mtext("Different records enter the two periods; these bars do not show a same-loan change or confidence intervals.", side = 1,
    outer = TRUE, line = 1.55, adj = 0.025, cex = 0.78, col = "#56636B")
}
png_path <- file.path(out, "period_comparison.png")
pdf_path <- file.path(out, "period_comparison.pdf")
ragg::agg_png(png_path, width = 2400, height = 1650, res = 260)
make_plot(); dev.off()
pdf(pdf_path, width = 2400/260, height = 1650/260, family = "Helvetica", encoding = "WinAnsi.enc")
make_plot(); dev.off()
plot_data <- z[, .(dataset, cohort, benchmark_view, reference, period, actual_period_label,
  records, events, countries, mean_delta_ge_pp)]
fwrite(plot_data, file.path(out, "period_comparison_data.csv"))
sidecar <- list(source_path = input, source_sha256 = digest(file = input, algo = "sha256"),
  source_build_id = unique(z$build_id), script_path = "scripts/p15/loan_extension/plot_loan_findings.R",
  script_sha256 = digest(file = "scripts/p15/loan_extension/plot_loan_findings.R", algo = "sha256"),
  selection = "non_peer; modern_DAC_common_reference; AidData fixed USD and ADD central all currency",
  measure = "Arithmetic mean of paired market-minus-DAC grant-element differences; equal weight per source record",
  units = "grant-element percentage points", uncertainty = "No confidence interval is estimated or depicted",
  period_composition = "Different loan records enter the two periods; AidData ends 2023, ADD ends 2024",
  currency = "AidData USD. ADD includes non-USD and missing-currency records under the common conditional benchmark scenario",
  artifacts = lapply(c(png_path, pdf_path, file.path(out, "period_comparison_data.csv")), function(path)
    list(path = path, sha256 = digest(file = path, algo = "sha256"), bytes = file.info(path)$size)))
write_json(sidecar, file.path(out, "period_comparison_provenance.json"), pretty = TRUE, auto_unbox = TRUE, na = "null")
cat("Created", png_path, "and", pdf_path, "\n")
