#!/usr/bin/env Rscript
# Bounded influence audit of saved creditor comparisons; no valuation/selection refit.
suppressPackageStartupMessages({library(data.table); library(jsonlite); library(digest)})
started <- format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")
args <- commandArgs(TRUE)
out <- if (length(args)) args[[1]] else "data-derived/p15_thesis_repayment_influence_20260910_v1"
if (dir.exists(out) && length(list.files(out))) stop("Fresh output directory required")
current <- NULL
resolve_base <- function(variable, stage = NULL) {
  configured <- Sys.getenv(variable)
  if (nzchar(configured)) return(configured)
  if (is.null(current)) current <<- read_json("data-derived/p15_master/current_run.json", simplifyVector = TRUE)
  if (is.null(stage)) current$candidate else current$stages[[stage]]$dir
}
pv_base <- resolve_base("P15_PV_INTERPRETATION_BASE", "pv_interpretation")
follow_base <- resolve_base("P15_PV_FOLLOWUP_BASE", "pv_followup")
bullet_base <- resolve_base("P15_BULLET_BASE", "bullet")
analysis_base <- resolve_base("P15_ANALYSIS_BASE")
script <- "scripts/p15/build_p15_thesis_repayment_influence.R"
design <- "docs/thesis_design/feedback_2026-09-10/REPAYMENT_OUTLIER_DESIGN.md"
use_note <- "docs/governance/P15_YOY_AND_EXTREME_CASE_USE_2026-09-08.md"
follow_note <- "docs/governance/P15_PV_RATINGS_IBRD_AND_REVERSALS_2026-09-09.md"
input_paths <- character()
input_verification <- list()
read_checked <- function(base, name) {
  path <- file.path(base, paste0(name, ".csv"))
  manifest_path <- file.path(base, "output_manifest.csv")
  manifest <- fread(manifest_path)
  path_column <- if ("artifact_path" %in% names(manifest)) "artifact_path" else "path"
  index <- which(basename(manifest[[path_column]]) == basename(path))
  stopifnot(length(index) == 1L)
  actual <- digest(file = path, algo = "sha256")
  stopifnot(identical(actual, manifest$sha256[index]))
  input_paths <<- unique(c(input_paths, path, manifest_path))
  input_verification[[length(input_verification) + 1L]] <<- data.table(
    artifact_path = path, expected_sha256 = manifest$sha256[index], actual_sha256 = actual,
    hash_matches = TRUE)
  fread(path)
}
x <- read_checked(pv_base, "analysis_rows")
pairs <- read_checked(pv_base, "matched_creditor_pairs")
saved_reversals <- read_checked(follow_base, "reversal_magnitude_cases")
saved_summary <- read_checked(follow_base, "reversal_magnitude_summary")
alternatives <- read_checked(follow_base, "reversal_alternative_benchmarks")
period_context <- read_checked(pv_base, "summary_period")
rating_context <- read_checked(follow_base, "rating_same_case_difference")
inventory <- read_checked(bullet_base, "inventory_with_bullet_flags")
selected <- read_checked(analysis_base, "selected_reference")
tiers <- read_checked(analysis_base, "tier_eligibility")
closure <- read_checked(analysis_base, "source_case_dispositions")
input_paths <- unique(c(input_paths, design, use_note, follow_note))
keys <- c("iso3", "analysis_year", "creditor")
stopifnot(nrow(x) == 762L, nrow(pairs) == 209L, !anyDuplicated(x[, ..keys]))
stopifnot(!anyDuplicated(pairs[, .(iso3, analysis_year, pair)]))

dir.create(out, recursive = TRUE, showWarnings = FALSE)
build_id <- basename(out)
schema_id <- "SCHEMA-P15-THESIS-REPAYMENT-INFLUENCE-V1"
estimator_id <- "EST-P15-SAVED-PV-PAIR-INFLUENCE-V1"
admissibility_id <- "ADM-P15-INHERITED-PV-PURPOSE-HOLDS-V1"
selection_id <- "SEL-P15-UNCHANGED-REFERENCE-DIAGNOSTIC-SUBSETS-V1"
source_ids <- paste(sort(unique(c(strsplit(paste(selected$source_package_ids, collapse = ";"), ";")[[1]],
  "SRC-WB-IDS-CORE-ALL-YEARS-20260520", "SRC-OECD-DAC-LISTS-20260908"))), collapse = ";")
lineage <- paste(c(file.path(pv_base, "analysis_rows.csv"),
  file.path(pv_base, "matched_creditor_pairs.csv"), file.path(analysis_base, "source_case_dispositions.csv")), collapse = ";")
save_table <- function(z, name) {
  z <- copy(as.data.table(z))
  metadata <- list(build_id = build_id, schema_id = schema_id, estimator_id = estimator_id,
    admissibility_id = admissibility_id, selection_id = selection_id,
    source_package_ids = source_ids, release_state = "diagnostic_private_not_canonical")
  for (field in names(metadata)) set(z, j = field, value = metadata[[field]])
  if (!"lineage_parent_ids" %in% names(z)) z[, lineage_parent_ids := lineage]
  fwrite(z, file.path(out, paste0(name, ".csv")), na = "")
}
pair_parts <- tstrsplit(pairs$pair, "__", fixed = TRUE)
pairs[, `:=`(creditor_a = pair_parts[[1]], creditor_b = pair_parts[[2]],
  case_id = paste(iso3, analysis_year, pair, sep = "::"),
  country_year_id = paste(iso3, analysis_year, sep = "::"))]
for (side in c("a", "b")) {
  index <- match(paste(pairs$iso3, pairs$analysis_year, pairs[[paste0("creditor_", side)]]),
    paste(x$iso3, x$analysis_year, x$creditor))
  stopifnot(!anyNA(index))
  for (field in c("official_rate", "official_grace_years", "market_grant_element_analogue_pct",
    "selected_rate_pct", "secondary_purpose_hold", "repayment_profile")) {
    pairs[, (paste0(field, "_", side)) := x[[field]][index]]
  }
}
index <- match(pairs$country_year_id, paste(selected$iso3, selected$analysis_year, sep = "::"))
stopifnot(!anyNA(index))
for (field in c("country", "selected_currency_basis", "selected_timing_basis",
  "selected_first_rate_date", "selected_last_rate_date", "selected_source_package_ids",
  "parent_evidence_id", "approved_status_rule_class", "status_case_note",
  "comparison_timing_warning", "individual_status_review_absent")) {
  pairs[, (field) := selected[[field]][index]]
}
closure_index <- match(pairs$country_year_id, paste(closure$iso3, closure$analysis_year, sep = "::"))
pairs[, `:=`(existing_source_review_case = !is.na(closure_index),
  existing_source_review_disposition = closure$current_use[closure_index],
  source_review_evidence = closure$interpretation[closure_index],
  source_review_remaining_evidence = closure$remaining_evidence[closure_index])]
pairs[, selected_reviewed_ids_proxy := existing_source_review_case & selected_tier == "ids"]
pairs[, brazil_zero_interest_context := iso3 == "BRA" & analysis_year == 2020 &
  (creditor_a == "China" | creditor_b == "China")]
pairs[, source_use_hold := selected_tier == "secondary" &
  (secondary_purpose_hold_a | secondary_purpose_hold_b)]
pairs[, invalid_repayment_dates := official_grace_years_a > official_maturity_years_a |
  official_grace_years_b > official_maturity_years_b]
tier_index <- match(paste(pairs$iso3, pairs$analysis_year, pairs$selected_tier),
  paste(tiers$iso3, tiers$analysis_year, tiers$tier))
stopifnot(!anyNA(tier_index))
pairs[, existing_benchmark_ineligible := !tiers$eligible[tier_index]]
pairs[, existing_formal_restriction := source_use_hold | invalid_repayment_dates | existing_benchmark_ineligible]
pairs[, `:=`(abs_ge_gap = abs(ge_difference_a_minus_b),
  parent_pair_locator = paste0(file.path(pv_base, "matched_creditor_pairs.csv"), "#", case_id),
  official_term_source_package_id = "SRC-WB-IDS-CORE-ALL-YEARS-20260520")]
pairs[, lineage_parent_ids := paste(parent_pair_locator,
  paste0(file.path(pv_base, "analysis_rows.csv"), "#", iso3, "::", analysis_year, "::", creditor_a),
  paste0(file.path(pv_base, "analysis_rows.csv"), "#", iso3, "::", analysis_year, "::", creditor_b),
  parent_evidence_id, sep = ";")]
stopifnot(max(abs(pairs$market_grant_element_analogue_pct_a - pairs$market_grant_element_analogue_pct_b -
  pairs$ge_difference_a_minus_b)) < 1e-8)
stopifnot(all(pairs$selected_rate_pct_a == pairs$selected_rate_pct_b),
  all(pairs$interest_ranking_reversed ==
    (pairs$interest_difference_a_minus_b * pairs$ge_difference_a_minus_b > 1e-8)))

summarise_pairs <- function(z) {
  reversal_gaps <- z[interest_ranking_reversed == TRUE, abs_ge_gap]
  data.table(n_pairs = nrow(z), n_countries = uniqueN(z$iso3),
    n_country_years = uniqueN(z$country_year_id), n_reversals = length(reversal_gaps),
    reversal_share_pct = 100 * length(reversal_gaps) / nrow(z),
    median_reversal_abs_ge_gap = if (length(reversal_gaps)) median(reversal_gaps) else NA_real_,
    mean_reversal_abs_ge_gap = if (length(reversal_gaps)) mean(reversal_gaps) else NA_real_,
    max_reversal_abs_ge_gap = if (length(reversal_gaps)) max(reversal_gaps) else NA_real_,
    mean_all_pairs_abs_ge_gap = mean(z$abs_ge_gap),
    reversals_above_5 = sum(reversal_gaps > 5), reversals_above_10 = sum(reversal_gaps > 10))
}
# Base-R, independently expressed order-statistic/arithmetic check for every summary.
check_summary <- function(z, result) {
  f <- as.data.frame(z)
  v <- sort(abs(f$ge_difference_a_minus_b[f$interest_ranking_reversed]))
  n <- length(v)
  med <- if (!n) NA_real_ else if (n %% 2L) v[(n + 1L) / 2L] else sum(v[c(n / 2L, n / 2L + 1L)]) / 2
  expected <- c(nrow(f), length(unique(f$iso3)), length(unique(f$country_year_id)), n,
    n / nrow(f) * 100, med, if(n) sum(v) / n else NA_real_, if(n) v[n] else NA_real_,
    sum(abs(f$ge_difference_a_minus_b)) / nrow(f), sum(v > 5), sum(v > 10))
  stopifnot(isTRUE(all.equal(unname(unlist(result)), expected, tolerance = 1e-12, check.attributes = FALSE)))
}
baseline <- summarise_pairs(pairs)
check_summary(pairs, baseline)
stopifnot(baseline$n_reversals == 19L,
  abs(baseline$median_reversal_abs_ge_gap - saved_summary$median_abs_ge_gap) < 1e-10,
  abs(baseline$mean_reversal_abs_ge_gap - saved_summary$mean_abs_ge_gap) < 1e-10,
  setequal(pairs[interest_ranking_reversed == TRUE, case_id],
    paste(saved_reversals$iso3, saved_reversals$analysis_year, saved_reversals$pair, sep = "::")))
save_table(pairs, "pair_case_audit")
save_table(pairs[interest_ranking_reversed == TRUE][order(-abs_ge_gap)], "reversal_cases_with_flags")

subsets <- list(
  unchanged_all_pairs = rep(TRUE, nrow(pairs)),
  existing_formal_restrictions_only = !pairs$existing_formal_restriction,
  without_reviewed_selected_ids_proxies = !pairs$selected_reviewed_ids_proxy,
  without_brazil_2020_zero_interest_context = !pairs$brazil_zero_interest_context,
  without_reviewed_ids_or_brazil_context = !pairs$selected_reviewed_ids_proxy & !pairs$brazil_zero_interest_context,
  primary_or_ids_only = pairs$selected_tier %in% c("primary", "ids"),
  without_pakistan_influence_only = pairs$iso3 != "PAK")
roles <- c("unchanged_result", "existing_source_use_rule_already_applied",
  "unresolved_source_context_sensitivity_not_accepted_exclusion",
  "unresolved_source_context_sensitivity_not_accepted_exclusion",
  "unresolved_source_context_sensitivity_not_accepted_exclusion",
  "existing_evidence_view_different_cases", "country_influence_not_accepted_exclusion")
sensitivities <- rbindlist(lapply(seq_along(subsets), function(j) {
  z <- pairs[subsets[[j]]]
  result <- summarise_pairs(z)
  check_summary(z, result)
  result[, `:=`(view = names(subsets)[j], interpretation = roles[j], omitted_pairs = nrow(pairs) - nrow(z))]
  result
}))
save_table(sensitivities, "sensitivity_summary")
membership <- rbindlist(lapply(seq_along(subsets), function(j) data.table(
  view = names(subsets)[j], case_id = pairs$case_id, included = subsets[[j]], interpretation = roles[j])))
save_table(membership, "sensitivity_membership")
for (level in c("iso3", "country_year_id", "case_id")) {
  loo <- rbindlist(lapply(sort(unique(pairs[[level]])), function(id) {
    z <- pairs[pairs[[level]] != id]
    result <- summarise_pairs(z)
    check_summary(z, result)
    result[, `:=`(omission_unit = level, omitted_id = id, omitted_pairs = nrow(pairs) - nrow(z),
      median_reversal_change = median_reversal_abs_ge_gap - baseline$median_reversal_abs_ge_gap,
      mean_reversal_change = mean_reversal_abs_ge_gap - baseline$mean_reversal_abs_ge_gap)]
    result
  }))
  save_table(loo, paste0("leave_one_", level, "_out"))
}
pair_type_loo <- rbindlist(lapply(sort(unique(pairs$iso3)), function(id) {
  pairs[iso3 != id, .(n_pairs = .N, n_countries = uniqueN(iso3),
    mean_ge_a_minus_b = mean(ge_difference_a_minus_b),
    median_ge_a_minus_b = median(ge_difference_a_minus_b),
    n_reversals = sum(interest_ranking_reversed)), by = pair][, omitted_country := id]
}))
save_table(pair_type_loo, "creditor_pair_leave_one_country_out")
save_table(pairs[, .(n_pairs = .N, n_countries = uniqueN(iso3),
  mean_ge_a_minus_b = mean(ge_difference_a_minus_b),
  median_ge_a_minus_b = median(ge_difference_a_minus_b),
  n_reversals = sum(interest_ranking_reversed)), by = pair], "creditor_pair_baseline")

# Explicitly audit already held observations in the full inventory, including absent scenarios.
inventory[, scenario_key := paste(iso3, analysis_year, creditor, sep = "::")]
inventory[, enters_modern_scenarios := scenario_key %in% paste(x$iso3, x$analysis_year, x$creditor, sep = "::")]
inventory_audit <- inventory[(secondary_purpose_hold | term_state == "grace_exceeds_maturity" |
  (iso3 == "PAK" & analysis_year >= 2018)) & historical_lmic_reporting_scope == TRUE,
  .(iso3, country, analysis_year, creditor, selected_tier, selected_rate_pct,
    official_rate, official_maturity_years, official_grace_years, term_state,
    secondary_purpose_hold, selected_purpose_hold, use_reason_with_bullet,
    enters_modern_scenarios, scenario_key, selected_source_package_ids)]
save_table(inventory_audit, "previous_restriction_and_pakistan_inventory")
save_table(x[iso3 == "PAK"], "pakistan_modern_scenarios")
save_table(pairs[iso3 == "PAK"], "pakistan_creditor_pairs")
save_table(closure[iso3 == "PAK"], "pakistan_existing_source_evidence")
save_table(alternatives, "existing_reversal_alternative_benchmarks")
save_table(period_context, "existing_period_valuation_context")
save_table(rating_context, "existing_same_case_rating_context")
checks <- data.table(check = c("all_consumed_table_hashes_match", "saved_762_scenarios_209_pairs_19_reversals",
  "saved_reversal_median_and_mean_reproduced", "each_pair_has_same_discount_rate",
  "pair_gaps_match_scenario_values", "all_summaries_pass_independent_base_R_arithmetic",
  "existing_formal_restrictions_absent_from_pairs", "no_grace_exceeds_maturity_in_modern_scenarios",
  "no_purpose_held_secondary_in_modern_scenarios", "pakistan_not_in_19_reversals"),
  passed = c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE,
    !any(pairs$existing_formal_restriction),
    !any(x$official_grace_years > x$official_maturity_years),
    !any(x$secondary_purpose_hold & x$selected_tier == "secondary"),
    !any(pairs$iso3 == "PAK" & pairs$interest_ranking_reversed)))
stopifnot(all(checks$passed))
save_table(checks, "checks")
save_table(rbindlist(input_verification), "input_hash_verification")
script_sha <- digest(file = script, algo = "sha256")
file_manifest <- function(paths, role) {
  rbindlist(lapply(paths, function(path) {
    dimensions <- if (grepl("[.]csv$", path)) dim(fread(path)) else c(NA_integer_, NA_integer_)
    data.table(artifact_path = path, artifact_role = role, bytes = file.info(path)$size,
      rows = dimensions[1], columns = dimensions[2], sha256 = digest(file = path, algo = "sha256"),
      source_package_ids = source_ids, estimator_id = estimator_id, admissibility_id = admissibility_id,
      selection_id = selection_id, schema_id = schema_id, build_id = build_id,
      producing_script = script, producing_script_sha256 = script_sha)
  }))
}
fwrite(file_manifest(input_paths, "intermediate_input"), file.path(out, "input_manifest.csv"), na = "")
fwrite(file_manifest(script, "script"), file.path(out, "code_manifest.csv"), na = "")
writeLines(capture.output(sessionInfo()), file.path(out, "environment.txt"))
write_json(list(renv_lock_sha256 = digest(file = "renv.lock", algo = "sha256"),
  r_version = R.version.string, platform = R.version$platform, locale = Sys.getlocale(),
  timezone = Sys.timezone(), run_started_at = started,
  run_finished_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"), run_status = "success",
  exception_ids = "none"), file.path(out, "environment_manifest.json"), pretty = TRUE, auto_unbox = TRUE)
writeLines(c(paste0("build_id: ", build_id), paste0("schema_id: ", schema_id),
  paste0("estimator_id: ", estimator_id), paste0("admissibility_id: ", admissibility_id),
  paste0("selection_id: ", selection_id), paste0("source_package_ids: ", source_ids),
  "release_state: diagnostic_private_not_canonical", "selection_and_repayment_rules: unchanged",
  "unit: matched_country_year_creditor_pair_equal_weight",
  "influence_ranges_are_not_confidence_intervals",
  "unresolved_context_sensitivities_are_not_accepted_exclusions"), file.path(out, "build_contract.txt"))
fwrite(file_manifest(list.files(out, full.names = TRUE), "generated_output"),
  file.path(out, "output_manifest.csv"), na = "")
print(sensitivities[, .(view, n_pairs, n_reversals, median_reversal_abs_ge_gap, mean_reversal_abs_ge_gap)])
cat("Saved", out, "\n")
