source("R/ids.R")
source("R/build_outputs.R")
source("R/lseg_benchmarks.R")

candidate_path <- "sources/market_rates/lseg_workspace_download_v6_2026-05-07/output/tables/lseg_pvr_benchmark_candidates_with_yield_oecd_base_2000-01-01_2025-12-31.csv"
ids_terms_path <- "output/tables/ids_terms_2024.csv"

if (!file.exists(ids_terms_path)) {
  source("R/run_pipeline.R")
}

outputs <- build_lseg_core_benchmark_outputs_2024(
  candidate_path = candidate_path,
  base_path = "sources/market_rates/lseg_workspace_download_v6_2026-05-07/output/tables/lseg_oecd_base_with_yield_fallback_oecd_base_2000-01-01_2025-12-31.csv",
  ids_terms_path = ids_terms_path,
  country_metadata_path = "data-raw/world_bank_countries.json"
)

write_output_csv(outputs$screened_rows, "output/tables/lseg_core_benchmark_row_screen_2024.csv")
write_output_csv(outputs$broader_base_audit, "output/tables/lseg_core_benchmark_broader_base_audit_2024.csv")
write_output_csv(outputs$issue_level, "output/tables/lseg_core_benchmark_issue_level_2024.csv")
write_output_csv(outputs$country_summary, "output/tables/lseg_core_benchmark_country_summary_2024.csv")
write_output_csv(outputs$country_rates, "output/tables/lseg_core_benchmark_rates_2024.csv")
write_output_csv(outputs$overlap_comparison, "output/tables/lseg_core_benchmark_ids_overlap_2024.csv")
write_output_csv(outputs$overlap_audit, "output/tables/lseg_core_benchmark_ids_discrepancy_audit_2024.csv")
write_output_csv(outputs$excluded_country_rates, "output/tables/lseg_core_benchmark_excluded_countries_2024.csv")
write_output_csv(outputs$questionable_cases, "output/tables/lseg_core_benchmark_questionable_cases_2024.csv")

message("Wrote output/tables/lseg_core_benchmark_*.csv")
