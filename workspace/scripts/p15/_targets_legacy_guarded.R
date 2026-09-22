# Preserved legacy route. The default entrypoint now builds isolated P15 evidence.
legacy_override <- Sys.getenv("PVR_ALLOW_LEGACY_TOP_LEVEL_WRITE", unset = "")
if (!identical(legacy_override, "YES-I-UNDERSTAND")) {
  stop(
    paste(
      "_targets.R is temporarily disabled by PIPE-01.",
      "It builds the legacy ADD/IDS 2024 route and can overwrite shared",
      "output/tables files with results that differ from R/run_pipeline.R, P13, and P14.",
      "No official build product has been selected yet (PIPE-03).",
      "See docs/AUDIT_ACTION_REGISTER_2026-07-17.md and docs/PROJECT_STATUS.md.",
      "For an explicitly authorized legacy reproduction only, set",
      "PVR_ALLOW_LEGACY_TOP_LEVEL_WRITE=YES-I-UNDERSTAND in that isolated run."
    ),
    call. = FALSE
  )
}
warning("PIPE-01 override active: running the legacy ADD/IDS 2024 targets route.", call. = FALSE)

library(targets)

source("R/ids.R")
source("R/pvr.R")
source("R/build_outputs.R")

tar_option_set(packages = c("dplyr", "jsonlite", "purrr", "readr", "readxl", "stringr", "tibble", "tidyr"))

list(
  tar_target(country_metadata, read_country_metadata("data-raw/world_bank_countries.json")),
  tar_target(counterpart_areas, read_counterpart_areas("data-raw/ids_counterpart_area.json")),
  tar_target(ids_terms_dir, if (dir.exists("data-raw/ids_terms_all_lenders")) "data-raw/ids_terms_all_lenders" else "data-raw/ids_terms"),
  tar_target(ids_terms, build_ids_terms(ids_terms_dir, country_metadata, counterpart_areas)),
  tar_target(market_benchmarks, build_market_benchmarks(ids_terms)),
  tar_target(pvr_results, build_pvr_results(ids_terms, market_benchmarks)),
  tar_target(country_coverage, build_country_coverage(country_metadata, ids_terms, market_benchmarks, pvr_results)),
  tar_target(pvr_matrix, build_pvr_matrix(pvr_results, country_coverage)),
  tar_target(write_ids_terms, write_output_csv(ids_terms, "output/tables/ids_terms_2024.csv"), format = "file"),
  tar_target(write_market_benchmarks, write_output_csv(market_benchmarks, "output/tables/market_benchmarks_2024.csv"), format = "file"),
  tar_target(write_pvr_results, write_output_csv(pvr_results, "output/tables/pvr_results_2024.csv"), format = "file"),
  tar_target(write_pvr_matrix, write_output_csv(pvr_matrix, "output/tables/pvr_matrix_2024.csv"), format = "file"),
  tar_target(write_country_coverage, write_output_csv(country_coverage, "output/tables/country_coverage_2024.csv"), format = "file"),
  tar_target(write_counterpart_areas, write_output_csv(counterpart_areas, "output/tables/ids_counterpart_areas.csv"), format = "file")
)
