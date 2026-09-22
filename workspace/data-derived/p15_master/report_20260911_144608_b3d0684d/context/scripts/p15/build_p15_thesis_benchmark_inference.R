#!/usr/bin/env Rscript
suppressPackageStartupMessages({library(data.table); library(jsonlite); library(digest)})
source("R/p15_thesis_benchmark_inference.R")
source("R/p15_dataset_assessment.R")
source("R/research_governance.R")
args <- commandArgs(trailingOnly = TRUE)
out <- if (length(args)) args[1] else "data-derived/p15_thesis_benchmark_inference_20260910_v2"
if (dir.exists(out)) stop("Refusing overwrite: ", out)
pvr_assert_write_allowed(out, "diagnostic")
started <- format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")
pointer <- function(stage, env) {
  configured <- Sys.getenv(env)
  if (nzchar(configured)) return(configured)
  current <- fromJSON("data-derived/p15_master/current_run.json")
  if (stage == "candidate") current$candidate else current$stages[[stage]]$dir
}
official <- pointer("official_policy", "P15_OFFICIAL_POLICY_BASE")
assessment <- pointer("assessment", "P15_ASSESSMENT_BASE")
base <- pointer("candidate", "P15_ANALYSIS_BASE")
design <- "docs/thesis_design/feedback_2026-09-10/BENCHMARK_ANALYSIS_DESIGN.md"
restriction <- "docs/governance/P15_YOY_AND_EXTREME_CASE_USE_2026-09-08.md"
inputs <- c(file.path(official, c("comparison_inputs.csv", "paired_summary.csv",
             "paired_inference.csv", "change_details.csv", "output_manifest.csv")),
            file.path(assessment, c("pair_summary.csv", "pair_details.csv", "output_manifest.csv")),
            file.path(base, c("core_evidence.csv", "tier_eligibility.csv", "selected_reference.csv", "output_manifest.csv")),
            design, restriction,
            "docs/governance/P15_OFFICIAL_DAC_MAPPING_AND_COMPARISON_PROTOCOL_2026-09-08.md",
            "renv.lock")
# Check consumed source files against their already recorded source manifests.
for (folder in c(official, assessment, base)) {
  manifest <- fread(file.path(folder, "output_manifest.csv"))
  for (f in inputs[dirname(inputs) == folder & basename(inputs) != "output_manifest.csv"]) {
    recorded <- manifest[basename(artifact_path) == basename(f)]
    stopifnot(nrow(recorded) == 1L,
              digest(f, file = TRUE, algo = "sha256") == recorded$sha256)
  }
}
d <- fread(file.path(official, "comparison_inputs.csv"))
core <- fread(file.path(base, "core_evidence.csv"))
eligibility <- fread(file.path(base, "tier_eligibility.csv"))
selected_reference <- fread(file.path(base, "selected_reference.csv"))
stopifnot(all(selected_reference$view_id == "ids_before_secondary__with_peer"))
stopifnot(!anyDuplicated(d[, .(iso3, analysis_year)]), nrow(d) == 215L)
for (tier_name in c("primary", "ids", "secondary", "moodys", "peer")) {
  z <- eligibility[tier == tier_name]
  at <- match(paste(d$iso3, d$analysis_year), paste(z$iso3, z$analysis_year))
  expected <- ifelse(z$eligible[at], z$rate_pct[at], NA_real_)
  stopifnot(isTRUE(all.equal(d[[tier_name]], expected, tolerance = 1e-11)))
}
restrictions <- data.table(iso3 = c(rep("LBN", 4), rep("BLR", 3), "RUS"),
                           analysis_year = c(2020:2023, 2022:2024, 2022))
restrictions[, reason := "Previously recorded secondary restriction for ordinary borrowing-cost use"]
restrictions[, present_in_primary_validation := paste(iso3, analysis_year) %in%
               paste(d$iso3, d$analysis_year)]
d[, secondary_use_restricted := paste(iso3, analysis_year) %in%
    paste(restrictions$iso3, restrictions$analysis_year)]
at <- match(paste(d$iso3, d$analysis_year), paste(core$iso3, core$analysis_year))
d[, `:=`(primary_thin = core$primary_usd_thin_evidence[at],
          secondary_thin = core$secondary_usd_thin_evidence[at],
          timing_warning = core$comparison_timing_warning[at],
          status_review_coverage = core$status_review_coverage[at])]
tiers <- c("ids", "secondary", "moodys", "peer")
all_rows <- list()
for (view in c("full_validation", "recorded_secondary_use_sensitivity")) {
  v <- copy(d)
  if (view != "full_validation") v[secondary_use_restricted == TRUE, secondary := NA_real_]
  modern <- v[analysis_year >= 2018 & is.finite(dac_modern_2018)]
  for (tier in tiers) all_rows[[length(all_rows) + 1L]] <-
    p15_thesis_loss_rows(modern, tier, "dac_modern_2018", "modern_tier_vs_dac", view)
  pairings <- combn(tiers, 2, simplify = FALSE)
  for (pair in pairings) all_rows[[length(all_rows) + 1L]] <-
    p15_thesis_loss_rows(modern, pair[1], pair[2], "modern_pairwise_tiers", view)
  historical <- list(
    pre2018_headline = list(rows = v[analysis_year <= 2017], rate = "dac_headline_new"),
    transition2015_2017 = list(rows = v[analysis_year %in% 2015:2017], rate = "dac_ge_from2015"),
    full2012_2024_headline = list(rows = v, rate = "dac_headline_new"))
  for (period in names(historical)) for (tier in tiers)
    all_rows[[length(all_rows) + 1L]] <- p15_thesis_loss_rows(
      historical[[period]]$rows, tier, historical[[period]]$rate,
      paste0("historical_", period), view)
}
loss <- rbindlist(all_rows)
summary_loss <- function(z, seed) {
  data.table(n = nrow(z), countries = uniqueN(z$iso3), years = uniqueN(z$analysis_year),
    first_year = min(z$analysis_year), last_year = max(z$analysis_year),
    focal_mae_pp = mean(abs(z$focal_error)), comparator_mae_pp = mean(abs(z$comparator_error)),
    relative_mae_reduction = 1 - mean(abs(z$focal_error)) / mean(abs(z$comparator_error)),
    focal_minus_primary_mean_pp = mean(z$focal_error),
    comparator_minus_primary_mean_pp = mean(z$comparator_error),
    equal_country_improvement_pp = mean(tapply(z$improvement_pp, z$iso3, mean)),
    closer_share = mean(z$improvement_pp > 1e-10),
    p15_thesis_cluster_boot(z$improvement_pp, z$iso3, seed = seed))
}
setorder(loss, family, sample_view, focal, comparator, iso3, analysis_year)
loss[, bootstrap_seed := 20260910L + match(paste(family, focal, comparator),
                                          unique(paste(family, focal, comparator)))]
summary <- loss[, summary_loss(.SD, first(bootstrap_seed)),
                by = .(family, sample_view, focal, comparator)]
summary[, p_holm_within_family := p.adjust(p_boot_two_sided, "holm"),
        by = .(family, sample_view)]
summary[, inference_family_size := .N, by = .(family, sample_view)]
joint_inference <- loss[, p15_assessment_inference(data.frame(
    iso3 = iso3, analysis_year = analysis_year, gap_pp = improvement_pp)),
    by = .(family, sample_view, focal, comparator)]
joint_inference[, p_holm_within_family := p.adjust(p_value, "holm"),
                by = .(family, sample_view, inference)]

# The existing country/year/two-way inference is complementary, not replaced.
existing_inference <- fread(file.path(official, "paired_inference.csv"))
existing_inference <- existing_inference[policy == "dac_modern_2018"]
existing_inference[, p_holm_modern_four := p.adjust(p_value, "holm"), by = inference]

# Retain all 189 primary-IDS comparisons; signed direction is explicitly reversed.
ids <- fread(file.path(assessment, "pair_details.csv"))[pair == "primary__ids"]
ids_summary <- data.table(n = nrow(ids), countries = uniqueN(ids$iso3),
                         primary_minus_ids_mean_pp = mean(ids$anchor_rate - ids$comparison_rate),
                         ids_minus_primary_mean_pp = mean(ids$comparison_rate - ids$anchor_rate),
                         mae_pp = mean(abs(ids$anchor_rate - ids$comparison_rate)))

# Lower tiers and primary-withheld fallback choices, on common consecutive years.
fallback <- p15_thesis_add_fallback(d, "fallback_with_ids", tiers)
fallback <- p15_thesis_add_fallback(fallback, "fallback_without_ids", tiers[-1])
level_rows <- list()
for (view in c("full_validation", "recorded_secondary_use_sensitivity")) {
  modern <- fallback[analysis_year >= 2018 & is.finite(dac_modern_2018)]
  for (focal in c(tiers, "fallback_with_ids", "fallback_without_ids")) {
    z <- p15_thesis_loss_rows(modern, focal, "dac_modern_2018", "modern_levels_and_changes", view)
    at <- match(paste(z$iso3, z$analysis_year), paste(modern$iso3, modern$analysis_year))
    if (startsWith(focal, "fallback")) set(z, j = "focal_source", value = modern[[paste0(focal, "_source")]][at])
    else z[, focal_source := focal]
    z[, recorded_secondary_use_hold := focal_source == "secondary" &
        modern$secondary_use_restricted[at]]
    # Drop the restricted selected evidence; never manufacture a replacement rate.
    if (view != "full_validation") z <- z[recorded_secondary_use_hold == FALSE]
    level_rows[[length(level_rows) + 1L]] <- z
  }
}
levels <- rbindlist(level_rows)
changes <- p15_thesis_consecutive(levels)
change_summary <- changes[, {
  z <- .SD
  data.table(n_transitions = nrow(z), countries = uniqueN(z$iso3),
    transition_years = uniqueN(z$analysis_year),
    focal_endpoint_mae_pp = mean(c(abs(z$focal_rate-z$primary), abs(z$focal_prior-z$primary_prior))),
    dac_endpoint_mae_pp = mean(c(abs(z$comparator_rate-z$primary), abs(z$comparator_prior-z$primary_prior))),
    focal_change_mae_pp = mean(abs(z$focal_change-z$primary_change)),
    dac_change_mae_pp = mean(abs(z$comparator_change-z$primary_change)),
    both_endpoint_levels_closer_n = sum(z$both_endpoint_levels_closer),
    both_endpoint_levels_closer_share = mean(z$both_endpoint_levels_closer),
    both_levels_closer_but_change_worse_n = sum(z$both_endpoint_levels_closer & z$change_worse),
    change_closer_n = sum(z$change_closer), change_worse_n = sum(z$change_worse),
    source_switch_n = sum(z$source_switch),
    p15_thesis_cluster_boot(z$change_improvement_pp, z$iso3,
      seed = 20261910L + match(.BY$focal, c(tiers, "fallback_with_ids", "fallback_without_ids"))))
}, by = .(sample_view, focal)]
change_summary[, p_holm_change_six := p.adjust(p_boot_two_sided, "holm"), by = sample_view]
change_joint <- changes[, p15_assessment_inference(data.frame(
  iso3 = iso3, analysis_year = analysis_year, gap_pp = change_improvement_pp)),
  by = .(sample_view, focal)]
change_joint[, p_holm_change_six := p.adjust(p_value, "holm"), by = .(sample_view, inference)]
switch_summary <- changes[grepl("^fallback", focal), .(
    n_transitions = .N, countries = uniqueN(iso3),
    focal_endpoint_mae_pp = mean(c(abs(focal_rate-primary), abs(focal_prior-primary_prior))),
    dac_endpoint_mae_pp = mean(c(abs(comparator_rate-primary), abs(comparator_prior-primary_prior))),
    focal_change_mae_pp = mean(abs(focal_change-primary_change)),
    dac_change_mae_pp = mean(abs(comparator_change-primary_change)),
    both_endpoint_levels_closer_n = sum(both_endpoint_levels_closer),
    both_levels_closer_but_change_worse_n = sum(both_endpoint_levels_closer & change_worse)),
    by = .(sample_view, focal, source_switch)]
switch_transitions <- changes[grepl("^fallback", focal), .(n_transitions = .N),
      by = .(sample_view, focal, focal_source_prior, focal_source)]

# Exact comparison to pre-existing modern means and consecutive change rows.
old_summary <- fread(file.path(official, "paired_summary.csv"))[policy == "dac_modern_2018"]
replay <- merge(summary[family == "modern_tier_vs_dac" & sample_view == "full_validation"],
                old_summary, by.x = "focal", by.y = "tier")
old_changes <- fread(file.path(official, "change_details.csv"))[policy == "dac_modern_2018"]
new_changes <- changes[sample_view == "full_validation" & focal %in% tiers]
joined_changes <- merge(new_changes, old_changes,
    by.x = c("iso3", "analysis_year", "focal"), by.y = c("iso3", "analysis_year", "tier"),
    suffixes = c("_new", "_old"))
checks <- data.table(check = c("existing_modern_levels_reproduced", "existing_change_rows_reproduced",
    "all_189_ids_pairs_reproduced", "improvement_sign_and_identity", "no_duplicate_loss_keys",
    "primary_withheld_from_fallback", "recorded_restrictions_not_replaced", "six_pairwise_modern_tests"),
    passed = c(nrow(replay)==4L && all(abs(replay$estimate_pp-replay$mean_improvement)<1e-11) &&
                 all(replay$n.x==replay$n.y),
      nrow(joined_changes)==nrow(old_changes) && nrow(new_changes)==nrow(old_changes) &&
        all(abs(joined_changes$primary_change_new-joined_changes$primary_change_old)<1e-11) &&
        all(abs(joined_changes$focal_change-joined_changes$tier_change)<1e-11),
      nrow(ids)==189L && abs(ids_summary$mae_pp-0.57602747271681)<1e-12,
      all(abs(loss$improvement_pp-(abs(loss$comparator_error)-abs(loss$focal_error)))<1e-12),
      !anyDuplicated(loss[, .(iso3, analysis_year, family, sample_view, focal, comparator)]),
      !any(levels$focal_source == "primary"),
      !any(levels[sample_view != "full_validation"]$recorded_secondary_use_hold),
      summary[family=="modern_pairwise_tiers" & sample_view=="full_validation", .N] == 6L))
stopifnot(all(checks$passed))
tables <- list(comparison_inputs = d, paired_loss_details = loss, paired_loss_inference = summary,
  paired_loss_country_year_sensitivity = joint_inference,
  existing_modern_cluster_sensitivity = existing_inference, primary_ids_signed_summary = ids_summary,
  level_details = levels, consecutive_details = changes, consecutive_summary = change_summary,
  consecutive_country_year_sensitivity = change_joint,
  source_switch_summary = switch_summary, source_transition_counts = switch_transitions,
  recorded_secondary_use_restrictions = restrictions, checks = checks)
dir.create(out, recursive = TRUE)
for (name in names(tables)) fwrite(tables[[name]], file.path(out, paste0(name, ".csv")))
code <- c("scripts/p15/build_p15_thesis_benchmark_inference.R", "R/p15_thesis_benchmark_inference.R",
          "R/p15_dataset_assessment.R",
          "R/research_governance.R", "tests/testthat/test-p15-thesis-benchmark-inference.R")
bundle <- list(build_id = basename(out), lifecycle_status = "diagnostic",
  estimator_id = "EST-P15-THESIS-PAIRED-CLUSTER-LOSS-20260910-V1",
  admissibility_id = "ADM-P15-SOURCE-CLOSURE-V1", selection_id = "SEL-P15-NO-PROMOTION-V1",
  schema_id = "SCHEMA-P15-THESIS-BENCHMARK-INFERENCE-V1",
  source_package_ids = "SRC-OECD-DAC-LISTS-20260908;SRC-P15-SOURCE-CLOSURE-20260907",
  repetitions = 4999L, main_seed = 20260910L, code_version = "2026-09-10-v2")
write_json(bundle, file.path(out, "version_bundle.json"), auto_unbox = TRUE, pretty = TRUE)
run <- list(run_started_at = started, run_finished_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
            run_status = "success", exception_ids = "none", r_version = R.version.string,
            platform = R.version$platform, locale = Sys.getlocale(), timezone = Sys.timezone(),
            renv_lock_sha256 = digest("renv.lock", file = TRUE, algo = "sha256"),
            method_note = "Exploratory conditional country-cluster bootstrap; no ladder promotion")
write_json(run, file.path(out, "run_environment.json"), auto_unbox = TRUE, pretty = TRUE)
capture.output(sessionInfo(), file = file.path(out, "environment.txt"))
manifest <- function(paths, role) {
  z <- pvr_manifest_rows(paths, role, bundle$build_id, bundle$schema_id, bundle$estimator_id,
                        bundle$admissibility_id, bundle$selection_id, bundle$source_package_ids)
  z$producing_script <- code[1]
  z$producing_script_sha256 <- digest(code[1], file = TRUE, algo = "sha256")
  z
}
fwrite(manifest(inputs, "input"), file.path(out, "input_manifest.csv"))
fwrite(manifest(code, "script"), file.path(out, "code_manifest.csv"))
fwrite(manifest(list.files(out, full.names = TRUE), "output"), file.path(out, "output_manifest.csv"))
print(summary[family %in% c("modern_tier_vs_dac", "modern_pairwise_tiers") &
  sample_view=="full_validation", .(family, focal, comparator, n, countries, focal_mae_pp,
    comparator_mae_pp, estimate_pp, ci_low_pp, ci_high_pp, p_holm_within_family)])
print(change_summary[sample_view=="full_validation"])
print(checks)
