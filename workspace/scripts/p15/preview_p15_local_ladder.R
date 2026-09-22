#!/usr/bin/env Rscript
suppressPackageStartupMessages({library(data.table); library(dplyr); library(tibble); library(digest)})
source("R/p15_local_completion.R")
source("R/p15_ladder_preview.R")
source("R/research_governance.R")
input <- "data-derived/p15_local_completion_2012_2024_20260906_v1/p15_country_year_dataset.csv"
panel <- as_tibble(fread(input))
variants <- expand.grid(ids_first = c(TRUE, FALSE), hold_ids = c(TRUE, FALSE), peers = c(FALSE, TRUE))
detail <- bind_rows(lapply(seq_len(nrow(variants)), function(i) {
  v <- variants[i, ]
  p15_local_ladder_preview(panel, v$ids_first, v$hold_ids, v$peers)
}))
stopifnot(nrow(detail) == 8L * nrow(panel),
          !anyDuplicated(detail[c("analysis_year", "iso3", "preview_variant")]),
          !any(detail$approved_selection),
          all(!is.na(detail$parent_evidence_id[is.finite(detail$preview_rate_pct)])))
summary <- detail |> group_by(preview_variant, historical_lmic_reporting_scope, preview_source) |>
  summarise(country_years = n(), countries = n_distinct(iso3), .groups = "drop")
base <- detail |> filter(preview_variant == "ids_before_secondary__ids_low_cases_held__peer_not_selected") |>
  select(analysis_year, iso3, reference_source = preview_source, reference_rate_pct = preview_rate_pct)
changes <- detail |> left_join(base, by = c("analysis_year", "iso3")) |>
  mutate(source_changed = preview_source != reference_source,
         additional_rate = is.finite(preview_rate_pct) & !is.finite(reference_rate_pct),
         rate_change_pp = preview_rate_pct - reference_rate_pct) |>
  filter(source_changed | additional_rate | abs(rate_change_pp) > 1e-12)
out <- "data-derived/p15_local_ladder_previews_20260906_v1"
dir.create(out, recursive = TRUE, showWarnings = FALSE)
data <- list(p15_ladder_preview_detail = detail, p15_ladder_preview_coverage = summary,
             p15_ladder_preview_changes = changes)
outputs <- file.path(out, paste0(names(data), ".csv"))
for (i in seq_along(data)) {
  tmp <- tempfile(fileext = ".csv")
  fwrite(as.data.table(data[[i]]), tmp, na = "")
  if (file.exists(outputs[[i]])) stopifnot(identical(digest(tmp, algo = "sha256", file = TRUE),
    digest(outputs[[i]], algo = "sha256", file = TRUE))) else stopifnot(file.copy(tmp, outputs[[i]]))
  unlink(tmp)
}
code <- c("R/p15_ladder_preview.R", "R/p15_local_completion.R", "scripts/p15/preview_p15_local_ladder.R",
          "tests/testthat/test-p15-ladder-preview.R", "renv.lock")
manifest <- pvr_manifest_rows(c(input, code, outputs),
  c("candidate_input", rep("code_environment", length(code)), rep("preview_output", length(outputs))),
  build_id = "BUILD-P15-LADDER-PREVIEW-20260906-V1", schema_id = "SCHEMA-P15-LADDER-PREVIEW-V1",
  estimator_id = "EST-P15-INHERITED-RATES-NO-REESTIMATION",
  admissibility_id = "ADM-P15-EXPLICIT-PREVIEW-SENSITIVITIES-V1",
  selection_id = "SEL-P15-EIGHT-UNAPPROVED-PREVIEWS-V1",
  source_package_ids = "BUILD-P15-LOCAL-COMPLETION-20260906-V1")
fwrite(manifest, file.path(out, "p15_preview_manifest.csv"), na = "")
print(summary |> filter(historical_lmic_reporting_scope), n = Inf)
