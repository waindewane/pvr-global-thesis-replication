source("R/p15_current_inputs.R")
#!/usr/bin/env Rscript
source("scripts/p15/activate_p15_environment.R")
source("R/research_governance.R")
source("R/p15_rating_country_history_calibration.R")
source("R/p15_rating_nested_check.R")
suppressPackageStartupMessages({library(data.table); library(jsonlite); library(digest)})
args <- commandArgs(TRUE)
out <- if (length(args)) args[1] else file.path("data-derived",paste0("p15_rating_nested_",format(Sys.time(),"%Y%m%d_%H%M%S")))
if (dir.exists(out)) stop("Refusing to overwrite: ", out)
protocol <- "docs/governance/P15_RATING_NESTED_CHECK_PROTOCOL_2026-09-12.md"
stopifnot(digest(file = protocol, algo = "sha256") ==
  "427abbb0e47b4dfe343be3cb8235d316c43a29be9dd528458210fd04c04314ee")
candidate <- Sys.getenv("P15_ANALYSIS_BASE",p15_current_input())
core_path <- file.path(candidate, "core_evidence.csv")
selected_path <- file.path(candidate, "selected_reference.csv")
inputs <- c(core_path, selected_path, protocol,
  file.path(candidate, "input_manifest.csv"), file.path(candidate, "output_manifest.csv"))
stopifnot(all(file.exists(inputs)))
protected <- c(inputs, "R/p15_rating_country_history_calibration.R")
before <- vapply(protected, function(p) digest(file = p, algo = "sha256"), character(1))
core <- fread(core_path, na.strings = "")
sample <- core[historical_lmic_reporting_scope %in% TRUE &
  is.finite(primary_usd_market_rate_pct) & primary_usd_market_rate_pct > 0 &
  is.finite(rating_moodys_rate_pct) &
  !(observed_benchmark_selection_permitted %in% FALSE),
  .(analysis_year, iso3, country, observed_rate_pct = primary_usd_market_rate_pct,
    rating_rate_pct = rating_moodys_rate_pct, variant_id = rating_moodys_variant_id)]
stopifnot(nrow(sample) == 199L, uniqueN(sample$iso3) == 41L,
  identical(unique(sample$variant_id), "boy_moodys_only_rating_group_median_dgs7"))
setorder(sample, analysis_year, iso3)
fit_sample <- sample[, .(analysis_year, iso3, country, observed_rate_pct, rating_rate_pct)]
result <- p15_run_rating_nested_check(fit_sample)
p <- result$outer
summary <- p[, p15_nested_summary(.SD), by = branch]
# Keep the grouping year in the summary's .SD.
annual <- rbindlist(lapply(unique(p$branch), function(b) rbindlist(lapply(2018:2024, function(y) {
  d <- p15_nested_summary(p[branch == b & analysis_year == y])
  d[, `:=`(branch = b, analysis_year = y)]
  d
}))))
history_summary <- rbindlist(lapply(c(FALSE, TRUE), function(has_history) {
  d <- p15_nested_summary(p[branch == "temporal" & (natural_history_rows > 0) == has_history])
  d[, has_prior_country_history := has_history]
  d
}))
influence <- rbindlist(lapply(unique(p$branch), function(b) {
  d <- p[branch == b]
  rbindlist(lapply(c("country", "year"), function(kind) {
    values <- if (kind == "country") sort(unique(d$iso3)) else as.character(2018:2024)
    rbindlist(lapply(values, function(v) {
      q <- if (kind == "country") d[iso3 != v] else d[analysis_year != as.integer(v)]
      s <- p15_nested_summary(q)
      s[, `:=`(branch = b, omitted_type = kind, omitted_value = v)]
      s
    }))
  }))
}))
target <- fread(selected_path, na.strings = "")[
  historical_lmic_reporting_scope %in% TRUE & selected_tier == "moodys",
  .(analysis_year, iso3, country, selected_rate_pct, view_id)]
target[, prior_eligible_primary_rows := vapply(seq_len(.N), function(i)
  nrow(fit_sample[iso3 == target$iso3[i] & analysis_year < target$analysis_year[i]]), integer(1))]
target_summary <- rbindlist(lapply(c("2012_2024", "2018_2024"), function(period) {
  d <- if (period == "2012_2024") target else target[analysis_year >= 2018L]
  d[, .(period = period, rows = .N, countries = uniqueN(iso3),
    rows_with_prior_history = sum(prior_eligible_primary_rows > 0),
    rows_without_prior_history = sum(prior_eligible_primary_rows == 0))]
}))

# Recompute one complete outer selection with its observed outcome altered.
probe_country <- sort(fit_sample[analysis_year == 2024L, iso3])[1]
perturbed <- copy(fit_sample)
perturbed[analysis_year == 2024L & iso3 == probe_country, observed_rate_pct := observed_rate_pct + 100]
probe <- p15_run_rating_nested_check(perturbed, 2024L, probe_country, quiet = TRUE)
keys <- c("branch", "analysis_year", "iso3", "model")
base_probe <- p[analysis_year == 2024L & iso3 == probe_country,
  c(keys, "prediction_pct", "selected_candidate"), with = FALSE]
new_probe <- probe$outer[, c(keys, "prediction_pct", "selected_candidate"), with = FALSE]
setorderv(base_probe, keys); setorderv(new_probe, keys)
a <- result$audit
contains_country <- function(ids, country) nzchar(country) && country %in% strsplit(ids, ";", fixed = TRUE)[[1]]
held_absent <- !mapply(contains_country, a$training_country_ids, a$held_outer_country)
inner_held_absent <- !mapply(contains_country,
  a[branch == "country_history_withheld" & phase == "inner", training_country_ids],
  a[branch == "country_history_withheld" & phase == "inner", test_country_ids])
same_support <- p[, .(support = paste(sort(paste(iso3, analysis_year)), collapse = ";")),
  by = .(branch, model)][, .(supports = uniqueN(support)), by = branch]
history_no_prior <- dcast(p[branch == "temporal" & natural_history_rows == 0 &
  model %in% c("affine", "history")], iso3 + analysis_year ~ model, value.var = "prediction_pct")
independent_mae <- aggregate(abs(p$prediction_pct - p$observed_rate_pct),
  list(branch = p$branch, model = p$model), mean)
independent_check <- merge(summary, independent_mae, by = c("branch", "model"))
checks <- data.table(check = c("sample_unique_199_rows_41_countries",
  "only_current_moodys_variant", "outer_years_2018_2024_and_130_rows",
  "training_strictly_before_test_year", "inner_test_strictly_before_outer_year",
  "held_outer_country_absent_from_all_training",
  "held_outer_country_absent_from_inner_outcomes",
  "inner_validation_country_absent_from_inner_training",
  "same_outer_support_for_all_candidates", "all_predictions_finite",
  "history_equals_affine_without_prior_observation",
  "own_withheld_outcome_cannot_change_prediction_or_choice",
  "mae_independently_reconstructed", "source_inputs_and_existing_method_unchanged"),
  passed = c(!anyDuplicated(sample[, .(iso3, analysis_year)]) && nrow(sample) == 199L && uniqueN(sample$iso3) == 41L,
    uniqueN(sample$variant_id) == 1L,
    all(p[model == "raw", .N, by = branch]$N == 130L) && setequal(p$analysis_year, 2018:2024),
    all(a$training_last_year < a$test_year),
    all(a[phase == "inner", test_year < outer_year]),
    all(held_absent),
    all(result$inner[branch == "country_history_withheld", iso3 != held_outer_country]),
    all(inner_held_absent), all(same_support$supports == 1L), all(is.finite(p$prediction_pct)),
    all(abs(history_no_prior$affine - history_no_prior$history) < 1e-10),
    isTRUE(all.equal(as.data.frame(base_probe), as.data.frame(new_probe), tolerance = 1e-10)),
    all(abs(independent_check$mae_pp - independent_check$x) < 1e-10),
    identical(before, vapply(protected, function(q) digest(file = q, algo = "sha256"), character(1)))))
stopifnot(all(checks$passed))
tables <- list(sample = sample, outer_predictions = p, inner_predictions = result$inner,
  selections = result$choices, fit_audit = a, summary = summary, annual = annual,
  natural_history_summary = history_summary, deletion_influence = influence,
  actual_rating_target_history = target, actual_target_summary = target_summary,
  checks = checks)
ids <- list(build_id = basename(out), schema_id = "SCHEMA-P15-RATING-NESTED-CHECK-20260912-V1",
  estimator_id = "EST-P15-FROZEN-AFFINE-HISTORY-NESTED-PAST-ONLY-V1",
  admissibility_id = "ADM-P15-CURRENT-LMIC-ELIGIBLE-USD-PRIMARY-MOODYS-V1",
  selection_id = "SEL-P15-INNER-MAE-DIAGNOSTIC-NO-PROMOTION-V1",
  source_package_ids = "SRC-P15-CURRENT-MASTER;SRC-P15-FROZEN-COUNTRY-HISTORY-CALIBRATION")
dir.create(out, recursive = TRUE)
for (nm in names(tables)) {
  tables[[nm]][, `:=`(build_id = ids$build_id, schema_id = ids$schema_id,
    estimator_id = ids$estimator_id, admissibility_id = ids$admissibility_id,
    selection_id = ids$selection_id, lifecycle_status = "diagnostic",
    release_state = "private_research")]
  fwrite(tables[[nm]], file.path(out, paste0(nm, ".csv")), na = "")
}
manifest <- function(paths, role) do.call(pvr_manifest_rows,
  c(list(paths = paths, artifact_role = role), ids))
fwrite(manifest(inputs, "nested_check_input"), file.path(out, "input_manifest.csv"))
code <- c("R/p15_rating_nested_check.R", "R/p15_rating_country_history_calibration.R",
  "scripts/p15/build_rating_nested_check.R", "scripts/p15/_targets_rating_nested_check.R",
  "tests/testthat/test-p15-rating-nested-check.R",
  "scripts/p15/activate_p15_environment.R", "R/research_governance.R", "renv.lock")
fwrite(manifest(code, "nested_check_code"), file.path(out, "code_manifest.csv"))
write_json(c(ids, list(candidate = candidate, protocol = protocol,
  lifecycle_status = "diagnostic", release_state = "private_research",
  outer_years = 2018:2024, inner_start_year = 2015,
  benchmark_change = FALSE, untouched_confirmation = FALSE,
  inference = "Descriptive retrospective comparison; deletion ranges are not confidence intervals",
  source_snapshot_id = paste0("SRC-P15-CORE-", substr(before[[core_path]], 1, 16)),
  base_variant = unique(sample$variant_id))), file.path(out, "version_bundle.json"),
  pretty = TRUE, auto_unbox = TRUE)
writeLines(capture.output(sessionInfo()), file.path(out, "environment.txt"))
fwrite(manifest(list.files(out, full.names = TRUE), "nested_check_output"),
  file.path(out, "output_manifest.csv"))
print(summary[, .(branch, model, rows, countries, mae_pp, rmse_pp, bias_pp, mae_gain_vs_raw_pp)])
cat("Completed bounded check with", nrow(checks), "passing checks:", out, "\n")
