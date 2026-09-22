#!/usr/bin/env Rscript
source("scripts/p15/activate_p15_environment.R")
suppressPackageStartupMessages({library(data.table); library(jsonlite); library(digest); library(countrycode)})

args <- commandArgs(TRUE)
out <- if (length(args)) args[1] else "data-derived/p15_crs_modern_summary_20260911_v4"
crs_base <- Sys.getenv("P15_CRS_BASE", "data-derived/p15_crs_application_20260911_v4")
loan_base <- Sys.getenv("P15_LOAN_BASE", "data-derived/p15_loan_comparisons_20260910_v5")
design <- "docs/thesis_design/feedback_2026-09-11_followup/CRS_MODERN_SUMMARY_DESIGN.md"
owner <- "docs/thesis_design/feedback_2026-09-11_followup/OWNER_ANNOTATIONS.json"
# A report pointer is only a standalone preservation check, never a numerical input.
# Master builds use the explicit current parents and must not depend on the prior report.
master_active <- identical(Sys.getenv("P15_MASTER_ACTIVE"), "true")
pointer <- if (master_active) character() else "data-derived/p15_master/current_run.json"
script <- "scripts/p15/annotation_followup_20260911/build_crs_modern_summary.R"
manifest <- function(paths) data.table(path = paths,
  sha256 = vapply(paths, function(p) digest(file = p, algo = "sha256"), character(1)),
  bytes = file.info(paths)$size)
input_paths <- c(file.path(crs_base, c("loan_valuations.csv", "annual_summary.csv", "period_summary.csv",
  "input_manifest.csv", "code_manifest.csv", "version_bundle.json")),
  file.path(loan_base, c("loan_valuations.csv", "input_manifest.csv", "code_manifest.csv", "version_bundle.json")),
  design, owner, pointer, "renv.lock")
stopifnot(!dir.exists(out), all(file.exists(input_paths)))
input_before <- manifest(input_paths)
dir.create(out, recursive = TRUE)

x <- fread(file.path(crs_base, "loan_valuations.csv"), na.strings = "")
a <- fread(file.path(loan_base, "loan_valuations.csv"), na.strings = "")
x <- x[matched == TRUE & commitment_year %between% c(2018L, 2024L)]
a <- a[dataset == "add" & main_cohort == TRUE & matched == TRUE & commitment_year %between% c(2018L, 2024L)]
x[, continent := countrycode(iso3, "iso3c", "continent", warn = FALSE)]
x[iso3 == "XKX", continent := "Europe"]
x[, `:=`(pvr_market_per100 = 100 - ge_market_pct,
  pvr_standardized_per100 = 100 - ge_standardized_pct,
  pvr_fixed5_per100 = 100 - ge_fixed5_pct,
  delta_standardized_pvr_per100 = -delta_standardized_ge_pp,
  delta_fixed5_pvr_per100 = -delta_fixed5_ge_pp)]

views <- c("all_selected", "non_peer", "primary_ids")
view_rows <- function(z, view) {
  if (view == "non_peer") return(z[non_peer == TRUE])
  if (view == "primary_ids") return(z[primary_ids == TRUE])
  z
}
statistics <- function(z) {
  stopifnot(nrow(z) > 0, all(is.finite(z$amount_usd) & z$amount_usd > 0))
  rbindlist(lapply(c("financing_record_equal", "nominal_USD_commitment"), function(weighting) {
    w <- if (weighting == "financing_record_equal") rep(1, nrow(z)) else z$amount_usd
    avg <- function(v) weighted.mean(v, w)
    data.table(weighting = weighting, financing_records = nrow(z),
      source_activities = sum(z$source_activity_count), source_rows = sum(z$source_record_count),
      countries = uniqueN(z$iso3), providers = uniqueN(z$DonorCode),
      commitment_years = uniqueN(z$commitment_year), amount_usd = sum(z$amount_usd),
      mean_market_ge_pct = avg(z$ge_market_pct),
      mean_standardized_ge_pct = avg(z$ge_standardized_pct),
      mean_fixed5_ge_pct = avg(z$ge_fixed5_pct),
      mean_market_minus_standardized_ge_pp = avg(z$delta_standardized_ge_pp),
      mean_market_minus_fixed5_ge_pp = avg(z$delta_fixed5_ge_pp),
      mean_market_pvr_per100 = avg(z$pvr_market_per100),
      mean_standardized_pvr_per100 = avg(z$pvr_standardized_per100),
      mean_fixed5_pvr_per100 = avg(z$pvr_fixed5_per100),
      mean_market_minus_standardized_pvr_per100 = avg(z$delta_standardized_pvr_per100),
      mean_market_minus_fixed5_pvr_per100 = avg(z$delta_fixed5_pvr_per100))
  }))
}
summarize_views <- function(group = NULL) rbindlist(lapply(views, function(view) {
  z <- view_rows(x, view)
  ans <- if (is.null(group)) statistics(z) else z[, statistics(.SD), by = group, .SDcols = names(z)]
  ans[, benchmark_view := view]
  setcolorder(ans, c("benchmark_view", group, "weighting")); ans
}))
modern <- summarize_views()
annual <- summarize_views("commitment_year")
period <- summarize_views("period")

count_views <- function(z, dataset_name) rbindlist(lapply(views, function(view) {
  q <- view_rows(z, view)
  is_crs <- dataset_name == "CRS"
  data.table(dataset = dataset_name, benchmark_view = view, years = "2018-2024",
    financing_records = nrow(q), countries = uniqueN(q$iso3),
    provider_labels = if (is_crs) uniqueN(q$DonorCode) else uniqueN(q$creditor),
    provider_count_basis = if (is_crs) "CRS_DonorCode" else "ADD_creditor_label",
    record_count_basis = if (is_crs) "WB_loan_ID_where_available_else_CRS_activity" else "existing_ADD_loan_record",
    amount_usd = sum(q$amount_usd),
    standardized_reference_records = sum(is.finite(if (is_crs) q$ge_standardized_pct else q$ge_standardized_category_pct)),
    standardized_reference_unavailable_records = sum(!is.finite(if (is_crs) q$ge_standardized_pct else q$ge_standardized_category_pct)))
}))
comparison <- rbindlist(list(count_views(x, "CRS"), count_views(a, "ADD")))
coverage <- rbindlist(lapply(views, function(view) {
  q <- view_rows(x, view); b <- view_rows(a, view)
  q[, present_in_ADD_same_view := iso3 %in% b$iso3]
  ans <- q[, .(financing_records = .N, countries = uniqueN(iso3), providers = uniqueN(DonorCode)),
    by = .(continent, present_in_ADD_same_view)]
  ans[, benchmark_view := view]; ans
}))
providers <- rbindlist(lapply(views, function(view) {
  ans <- view_rows(x, view)[, .(financing_records = .N, countries = uniqueN(iso3),
    years = paste(sort(unique(commitment_year)), collapse = ";"), amount_usd = sum(amount_usd),
    mean_market_ge_pct = mean(ge_market_pct), mean_standardized_ge_pct = mean(ge_standardized_pct),
    mean_standardized_delta_ge_pp = mean(delta_standardized_ge_pp)), by = .(DonorCode, DonorName)]
  ans[, benchmark_view := view]; ans
}))
types <- rbindlist(lapply(views, function(view) {
  ans <- view_rows(x, view)[, .(financing_records = .N, countries = uniqueN(iso3),
    mean_market_ge_pct = mean(ge_market_pct), mean_standardized_ge_pct = mean(ge_standardized_pct),
    mean_standardized_delta_ge_pp = mean(delta_standardized_ge_pp)), by = benchmark_selected_tier]
  ans[, benchmark_view := view]; ans
}))
currency <- rbindlist(lapply(views, function(view) {
  q <- view_rows(x, view); b <- view_rows(a, view)
  cq <- q[, .(financing_records = .N, countries = uniqueN(iso3)), by = .(currency_basis = loan_currency_basis)]
  cq[, `:=`(dataset = "CRS", benchmark_view = view, recorded_currency = "unknown_loan_denomination")]
  cb <- b[, .(financing_records = .N, countries = uniqueN(iso3)), by = .(recorded_currency = currency, currency_basis)]
  cb[, `:=`(dataset = "ADD", benchmark_view = view)]
  rbindlist(list(cq, cb), use.names = TRUE)
}))
scope <- rbindlist(lapply(views, function(view) {
  ans <- view_rows(x, view)[, .(financing_records = .N, countries = uniqueN(iso3), providers = uniqueN(DonorCode)),
    by = borrower_scope]
  ans[, benchmark_view := view]; ans
}))

# Reconcile existing reference summaries without recalculating any cash flows.
replay <- rbindlist(lapply(c("annual", "period"), function(granularity) {
  keys <- if (granularity == "annual") c("benchmark_view", "commitment_year") else c("benchmark_view", "period")
  old <- fread(file.path(crs_base, paste0(granularity, "_summary.csv")))
  old <- old[reference %in% c("standardized_DAC_category_rule", "fixed5")]
  if (granularity == "annual") old <- old[commitment_year >= 2018] else old <- old[period != "2012_2017"]
  new <- if (granularity == "annual") annual else period
  rbindlist(lapply(c("standardized_DAC_category_rule", "fixed5"), function(ref) {
    ref_suffix <- if (ref == "standardized_DAC_category_rule") "standardized" else "fixed5"
    eq <- new[weighting == "financing_record_equal"]
    am <- new[weighting == "nominal_USD_commitment"]
    eq <- eq[, c(keys, "financing_records", "source_activities", "source_rows", "countries", "providers",
      "amount_usd", "mean_market_ge_pct", paste0("mean_", ref_suffix, "_ge_pct"),
      paste0("mean_market_minus_", ref_suffix, "_ge_pp")), with = FALSE]
    setnames(eq, c("source_rows", paste0("mean_", ref_suffix, "_ge_pct"),
      paste0("mean_market_minus_", ref_suffix, "_ge_pp")), c("source_records", "mean_reference_ge_pct", "mean_delta_ge_pp"))
    am <- am[, c(keys, paste0("mean_market_minus_", ref_suffix, "_ge_pp")), with = FALSE]
    setnames(am, paste0("mean_market_minus_", ref_suffix, "_ge_pp"), "amount_weighted_delta_ge_pp")
    eq <- merge(eq, am, by = keys)
    both <- merge(old[reference == ref], eq, by = keys, suffixes = c("_old", "_new"))
    metrics <- c("financing_records", "source_activities", "source_records", "countries", "providers",
      "amount_usd", "mean_market_ge_pct", "mean_reference_ge_pct", "mean_delta_ge_pp", "amount_weighted_delta_ge_pp")
    rbindlist(lapply(metrics, function(metric) data.table(granularity, reference = ref, metric,
      compared_groups = nrow(both), max_absolute_difference = max(abs(as.numeric(both[[paste0(metric, "_old")]]) - as.numeric(both[[paste0(metric, "_new")]]))))))
  }))
}))
check_rows <- list()
check <- function(name, passed) check_rows[[length(check_rows) + 1L]] <<- data.table(check = name, passed = isTRUE(passed))
check("CRS_modern_unique_financing_ID", !anyDuplicated(x$loan_id))
check("ADD_modern_unique_loan_ID", !anyDuplicated(a$loan_id))
check("all_modern_CRS_rates_and_valuations_finite", all(is.finite(as.matrix(x[, .(ge_market_pct, ge_standardized_pct, ge_fixed5_pct)]))))
check("all_modern_records_DAC_eligible", all(x$dac_eligible & x$standardized_reference_rate_pct %in% c(9, 7, 6)))
check("primary_IDS_is_subset_of_nonpeer", all(x[primary_ids == TRUE, non_peer]))
check("nonpeer_contains_no_selected_peers", !any(x[non_peer == TRUE, benchmark_selected_tier] == "peer"))
check("primary_IDS_selected_source_filter", all(x[primary_ids == TRUE, benchmark_selected_tier] %in% c("primary", "ids")))
check("modern_counts_match_existing_periods", all(modern[weighting == "financing_record_equal", financing_records] == c(1531L, 1274L, 798L)))
check("existing_annual_and_period_summary_reproduced", nrow(replay) == 40L && all(replay$max_absolute_difference < 1e-8))
check("all_expected_reference_groups_compared", all(replay[granularity == "annual", compared_groups] == 21L) && all(replay[granularity == "period", compared_groups] == 6L))
for (view in views) {
  pooled <- modern[benchmark_view == view & weighting == "financing_record_equal"]
  yy <- annual[benchmark_view == view & weighting == "financing_record_equal"]
  check(paste0(view, "_annual_counts_sum_to_pooled"), sum(yy$financing_records) == pooled$financing_records)
  check(paste0(view, "_annual_mean_recombines_to_pooled"), abs(weighted.mean(yy$mean_market_ge_pct, yy$financing_records) - pooled$mean_market_ge_pct) < 1e-10)
  ww <- annual[benchmark_view == view & weighting == "nominal_USD_commitment"]
  check(paste0(view, "_annual_commitment_mean_recombines_to_pooled"), abs(weighted.mean(ww$mean_market_ge_pct, ww$amount_usd) - modern[benchmark_view == view & weighting == "nominal_USD_commitment", mean_market_ge_pct]) < 1e-10)
}
for (nm in c("market", "standardized", "fixed5")) {
  check(paste0(nm, "_PVR_plus_GE_equals100_all_summaries"), all(abs(rbindlist(list(modern, annual, period), fill = TRUE)[[paste0("mean_", nm, "_ge_pct")]] +
    rbindlist(list(modern, annual, period), fill = TRUE)[[paste0("mean_", nm, "_pvr_per100")]] - 100) < 1e-10))
}
check("CRS_loan_currency_not_inferred_from_reporting_currency", all(is.na(x$loan_currency) | x$loan_currency == ""))
check("GE_differences_equal_negative_PVR_differences", all(abs(modern$mean_market_minus_standardized_ge_pp + modern$mean_market_minus_standardized_pvr_per100) < 1e-10))
check("CRS_source_and_code_manifest_unchanged", identical(input_before$sha256, manifest(input_paths)$sha256))
checks <- rbindlist(check_rows)

tables <- list(modern_summary = modern, annual_summary = annual, period_summary = period,
  modern_matched_financing_records = x, modern_crs_add_comparison = comparison,
  modern_geographic_coverage = coverage, modern_provider_summary = providers,
  modern_benchmark_type_summary = types, modern_currency_interpretation = currency,
  modern_borrower_scope = scope, reference_summary_replay = replay, checks = checks)
for (nm in names(tables)) fwrite(tables[[nm]], file.path(out, paste0(nm, ".csv")))
fwrite(input_before, file.path(out, "input_manifest.csv"))
fwrite(manifest(c(script, "scripts/p15/activate_p15_environment.R")), file.path(out, "code_manifest.csv"))
lineage <- rbindlist(lapply(c(crs_base, loan_base), function(parent) {
  z <- fread(file.path(parent, "input_manifest.csv")); z[, `:=`(parent_package = basename(parent),
    verification_scope = "inherited_from_preserved_parent_input_manifest")]; z
}), fill = TRUE)
fwrite(lineage, file.path(out, "source_lineage_manifest.csv"))
writeLines(capture.output(sessionInfo()), file.path(out, "environment.txt"))
write_json(list(build_id = basename(out), schema_id = "SCHEMA-P15-CRS-MODERN-SUMMARY-V1",
  estimator_id = "EST-EXISTING-GE-PVR-MATCHED-DESCRIPTIVE-MEANS-V1",
  admissibility_id = "ADM-EXISTING-CRS-V4-AND-ADD-MAIN-MODERN-V1",
  selection_id = "SEL-EXISTING-SELECTED-NONPEER-PRIMARYIDS-UNCHANGED",
  source_package_ids = c(basename(crs_base), basename(loan_base)),
  lifecycle_status = "diagnostic", release_state = "private_research", years = c(2018L, 2024L),
  baseline_master_pointer_sha256 = if (length(pointer)) input_before[path == pointer, sha256] else NA_character_,
  changes_existing_values = FALSE, registered_master_stage = master_active),
  file.path(out, "version_bundle.json"), pretty = TRUE, auto_unbox = TRUE)
fwrite(manifest(list.files(out, full.names = TRUE)), file.path(out, "output_manifest.csv"))
stopifnot(all(checks$passed))
print(modern); print(comparison); print(checks)
