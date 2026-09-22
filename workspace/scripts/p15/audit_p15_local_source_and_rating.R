#!/usr/bin/env Rscript
# Supplement the local dataset with source-level IDS checks and a frozen Moody's
# validation rerun. No download, rate correction, or method promotion is performed.
suppressPackageStartupMessages({
  library(data.table); library(dplyr); library(tibble); library(tidyr)
  library(jsonlite); library(digest)
})
source("R/p15_bounded_fallback_comparison.R")
source("R/p15_rating_out_of_sample_calibration.R")
source("R/p15_rating_country_history_calibration.R")
source("R/research_governance.R")

output_dir <- "data-derived/p15_local_source_rating_audit_20260906_v1"
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
panel_path <- "data-derived/p15_local_completion_2012_2024_20260906_v1/p15_country_year_dataset.csv"
panel <- as_tibble(fread(panel_path))
cache_dir <- "experiments/full_ladder_database_all_years_2026-05-20/data/ids_core_all_years"
source_paths <- character()
source_rows <- list()
pagination <- list()
for (series in c("DT_INR_DPPG", "DT_MAT_DPPG", "DT_GPA_DPPG")) {
  first <- file.path(cache_dir, paste0("ids_BND_", series, "_page_1.json"))
  meta <- fromJSON(first)
  pages <- as.integer(meta$pages)
  if (length(pages) != 1L || is.na(pages) || pages < 1L) stop("Missing top-level cached page count")
  pagination[[series]] <- tibble(series = series, declared_pages = pages,
    legacy_nested_source_pages_present = !is.null(meta$source$pages),
    legacy_seq_2_to_1_bug_applicable = pages == 1L,
    cached_page2_exists = file.exists(file.path(cache_dir, paste0("ids_BND_", series, "_page_2.json"))))
  for (page in seq_len(pages)) {
    path <- file.path(cache_dir, paste0("ids_BND_", series, "_page_", page, ".json"))
    raw <- fromJSON(path)$source$data
    source_paths <- c(source_paths, path)
    get_id <- function(v, concept) v$id[match(concept, v$concept)]
    source_rows[[paste(series, page)]] <- tibble(
      iso3 = vapply(raw$variable, get_id, character(1), concept = "Country"),
      analysis_year = as.integer(sub("YR", "", vapply(raw$variable, get_id, character(1), concept = "Time"))),
      indicator_id = vapply(raw$variable, get_id, character(1), concept = "Series"),
      creditor_id = vapply(raw$variable, get_id, character(1), concept = "Counterpart-Area"),
      source_value = raw$value, source_file = path,
      source_record_locator = paste0("source.data[", seq_len(nrow(raw)), "]")
    ) |> semi_join(panel, by = c("analysis_year", "iso3"))
  }
}
raw_detail <- bind_rows(source_rows)
stopifnot(!anyDuplicated(raw_detail[c("analysis_year", "iso3", "indicator_id")]),
          all(raw_detail$creditor_id == "BND"))
source_comparison <- panel |>
  select(analysis_year, iso3, ids_rate_pct, ids_maturity_years, ids_grace_years) |>
  pivot_longer(starts_with("ids_"), names_to = "field", values_to = "p15_value") |>
  mutate(indicator_id = recode(field, ids_rate_pct = "DT.INR.DPPG",
    ids_maturity_years = "DT.MAT.DPPG", ids_grace_years = "DT.GPA.DPPG")) |>
  left_join(raw_detail, by = c("analysis_year", "iso3", "indicator_id")) |>
  mutate(raw_to_p15_match = (is.na(p15_value) & is.na(source_value)) |
    (!is.na(p15_value) & !is.na(source_value) & abs(p15_value - source_value) <= 1e-10))
stopifnot(all(source_comparison$raw_to_p15_match))
ids_review <- panel |>
  filter(ids_legacy_low_rate_rule_conflict | (ids_positive_rate_observed & !ids_term_order_valid) |
         (ids_zero_rate_review & ids_term_order_valid)) |>
  select(analysis_year, iso3, country, historical_lmic_reporting_scope,
         starts_with("ids_"), primary_usd_market_rate_pct, secondary_usd_market_rate_pct) |>
  mutate(
    source_issue_review_reason = case_when(
      ids_zero_rate_review ~ "zero_rate_with_positive_maturity_check_source_semantics",
      ids_legacy_low_rate_rule_conflict ~ "low_positive_rate_inspect_source_units_and_instrument_scope",
      TRUE ~ "aggregate_grace_exceeds_maturity_inspect_denominators_and_precision"
    ),
    primary_to_ids_ratio = if_else(ids_positive_rate_observed,
      primary_usd_market_rate_pct / ids_rate_pct, NA_real_),
    hypothetical_100x_ids_gap_to_primary_pp = if_else(ids_legacy_low_rate_rule_conflict,
      100 * ids_rate_pct - primary_usd_market_rate_pct, NA_real_),
    maturity_grace_gap_days_approximate = 365.25 * (ids_grace_years - ids_maturity_years),
    rates_corrected = FALSE,
    interpretation = "source_level_review_case_not_authorization_to_rescale_or_drop"
  ) |>
  left_join(source_comparison |> filter(field == "ids_rate_pct") |>
    select(analysis_year, iso3, source_file, source_record_locator, raw_to_p15_match),
    by = c("analysis_year", "iso3"))

# Reuse the exact earlier designs, changing only the rating variant to Moody's.
detail_path <- "data-derived/p15_fallback_validation_2012_2024_v1/p15_rating_integrated_anchor_validation_detail_2012_2024.csv.gz"
detail <- as_tibble(fread(detail_path))
moodys_variant <- "boy_moodys_only_rating_group_median_dgs7"
sample <- detail |>
  filter(validation_question_id == "Q-USD-NEW-BORROWING", variant_id == moodys_variant,
    historical_lmic_reporting_scope %in% TRUE, accuracy_summary_eligible %in% TRUE) |>
  transmute(analysis_year, iso3, country, observed_rate_pct = anchor_rate_pct,
    rating_rate_pct = variant_rate_pct, risk_free_rate_pct = risk_free_7y_pct,
    rating_spread_pct = variant_spread_pct) |>
  arrange(iso3, analysis_year)
stopifnot(nrow(sample) == 200L, !anyDuplicated(sample[c("analysis_year", "iso3")]))
oos <- p15_build_rating_oos_predictions(sample)
history <- p15_build_rating_country_history_predictions(sample)
stopifnot(all(history$training_last_year < history$analysis_year),
  all(history$country_history_last_year[history$country_history_rows > 0] <
        history$analysis_year[history$country_history_rows > 0]))
oos_summary <- p15_summarise_rating_oos(oos)
history_summary <- p15_summarise_rating_country_history(history)
history_inference <- bind_rows(lapply(unique(history$validation_design), function(design) {
  x <- history |> filter(validation_design == design) |>
    select(analysis_year, iso3, model_id, absolute_error_pp) |>
    pivot_wider(names_from = model_id, values_from = absolute_error_pp) |>
    mutate(delta = affine_year_demeaned_country_eb - raw)
  p15_clustered_mean_difference(x, "delta") |> mutate(validation_design = design,
    comparison = "history_adjusted_minus_raw", inference_state = "exploratory_not_untouched_confirmation")
}))
# Is the required past observed outcome actually available at deployment targets?
history_availability <- panel |>
  select(analysis_year, iso3, historical_lmic_reporting_scope, fallback_audit_cohort,
         rating_moodys_available)
history_availability$prior_validated_primary_rows <- vapply(seq_len(nrow(panel)), function(i) {
  sum(sample$iso3 == panel$iso3[[i]] & sample$analysis_year < panel$analysis_year[[i]])
}, integer(1))
history_coverage <- history_availability |>
  group_by(historical_lmic_reporting_scope, fallback_audit_cohort) |>
  summarise(rows = n(), moodys_available = sum(rating_moodys_available),
    moodys_and_prior_observed_history = sum(rating_moodys_available & prior_validated_primary_rows > 0),
    .groups = "drop")

tables <- list(p15_ids_raw_source_reconstruction = source_comparison,
  p15_ids_source_review_cases = ids_review, p15_ids_cached_pagination_audit = bind_rows(pagination),
  p15_moodys_validation_sample = sample, p15_moodys_oos_predictions = oos,
  p15_moodys_oos_summary = oos_summary, p15_moodys_oos_inference = p15_rating_oos_paired_inference(oos),
  p15_moodys_country_history_predictions = history,
  p15_moodys_country_history_summary = history_summary,
  p15_moodys_country_history_inference = history_inference,
  p15_moodys_history_target_availability = history_availability,
  p15_moodys_history_target_summary = history_coverage)
outputs <- file.path(output_dir, paste0(names(tables), ".csv"))
for (i in seq_along(tables)) {
  tmp <- tempfile(fileext = ".csv")
  fwrite(as.data.table(tables[[i]]), tmp, na = "")
  if (file.exists(outputs[[i]])) {
    stopifnot(identical(digest(tmp, algo = "sha256", file = TRUE),
                       digest(outputs[[i]], algo = "sha256", file = TRUE)))
  } else stopifnot(file.copy(tmp, outputs[[i]]))
  unlink(tmp)
}
code <- c("scripts/p15/audit_p15_local_source_and_rating.R", "R/p15_bounded_fallback_comparison.R",
  "R/p15_rating_out_of_sample_calibration.R", "R/p15_rating_country_history_calibration.R", "renv.lock")
manifest <- pvr_manifest_rows(c(source_paths, panel_path, detail_path, code, outputs),
  c(rep("cached_source_or_candidate", length(source_paths) + 2L), rep("code_environment", length(code)),
    rep("diagnostic_output", length(outputs))), build_id = "BUILD-P15-LOCAL-SOURCE-RATING-20260906-V1",
  schema_id = "SCHEMA-P15-LOCAL-SOURCE-RATING-V1", estimator_id = "EST-P15-FROZEN-MOODYS-VALIDATION-V1",
  admissibility_id = "ADM-DIAGNOSTIC-ONLY", selection_id = "SEL-NONE",
  source_package_ids = paste(c("SRC-WB-IDS-CORE-ALL-YEARS-20260520",
    "SRC-BLOOMBERG-BOY-RATINGS-20260528", "SRC-DAMODARAN-ARCHIVE-2000-2024",
    "SRC-FRED-DGS7-20260512", "SRC-WB-OGHIST-20260630"), collapse = ";"))
fwrite(manifest, file.path(output_dir, "p15_audit_manifest.csv"), na = "")
print(oos_summary, width = Inf)
print(history_summary, width = Inf)
print(history_inference, width = Inf)
print(history_coverage, width = Inf)
cat("IDS review cases:", nrow(ids_review), "; raw-to-P15 comparisons passing:", nrow(source_comparison), "\n")
