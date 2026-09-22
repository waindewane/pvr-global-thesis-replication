#!/usr/bin/env Rscript
# One-time extraction of the established geography/static labels; no historical
# income value, observed rate or model estimate is admitted to this input.
library(data.table)
source("R/research_governance.R")
parent <- "experiments/p14_historical_validation_and_extension_2026-06-22/outputs/p14_historical_quality_equalization_2012_2023/p14_historical_country_classification_ledger_2012_2024.csv"
out <- "data-raw/p15_curated_inputs_20260906"
dir.create(out, recursive = TRUE, showWarnings = FALSE)
x <- fread(parent)
x <- unique(x[, .(iso3, country, static_income_level, static_lending_type, static_lmic_reporting_scope)])
stopifnot(nrow(x) == 211L, !anyDuplicated(x$iso3))
setorder(x, iso3)
path <- file.path(out, "geography_and_static_labels.csv")
if (file.exists(path)) stop("Curated snapshot already exists; do not replace in place")
fwrite(x, path, na = "")
fwrite(pvr_manifest_rows(c(parent, path, "scripts/p15/prepare_p15_curated_geography.R"),
  c("preserved_scope_parent", "curated_scope_input", "extractor_code"),
  build_id = "BUILD-P15-CURATED-GEOGRAPHY-20260906", schema_id = "SCHEMA-P15-CURATED-GEOGRAPHY-V1",
  estimator_id = "EST-NONE-STATIC-SCOPE", admissibility_id = "ADM-NONE",
  selection_id = "SEL-NONE", source_package_ids = "SRC-P14-CLASSIFICATION-LEDGER-20260630"),
  file.path(out, "source_manifest.csv"))
