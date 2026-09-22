#!/usr/bin/env Rscript
# Run from the repository root. Reads cached inputs only; no network requests.
suppressPackageStartupMessages({
  library(data.table)
  library(dplyr)
  library(tibble)
  library(digest)
})
source("R/p15_bounded_fallback_comparison.R")
source("R/p15_local_completion.R")
source("R/research_governance.R")

paths <- p15_local_input_paths()
if (any(!file.exists(paths))) stop("Missing local inputs: ", paste(paths[!file.exists(paths)], collapse = "; "))
input_hashes <- vapply(paths, digest, character(1), algo = "sha256", file = TRUE)
inputs <- lapply(paths, function(p) as_tibble(fread(p)))
assembly <- p15_build_local_panel(inputs)
peer <- p15_audit_local_peer_targets(assembly$panel, assembly$anchors, inputs$rating_context)
panel <- assembly$panel |>
  left_join(peer$detail |>
    select(analysis_year, iso3, peer_rate_pct, peer_minimum_met,
           peer_pool_rule, peer_country_count, peer_iqr_pp, target_moodys_missing,
           rating_proximity_used, global_pool_used, peer_candidate_state,
           peer_evidence_id = source_evidence_row_id), by = c("analysis_year", "iso3"))

# Compare the full-target implementation to the previous held-out anchor surface.
previous_peer <- inputs$peer_comparison |>
  filter(peer_method_id == "observable_similarity_raw_yield") |>
  select(analysis_year, iso3, old_peer_rate_pct = peer_rate_pct,
         old_peer_pool_rule = peer_pool_rule, old_peer_count = peer_country_count)
peer_regression <- previous_peer |>
  left_join(peer$detail, by = c("analysis_year", "iso3")) |>
  mutate(rate_difference_pp = peer_rate_pct - old_peer_rate_pct,
         exact_pool_match = peer_pool_rule == old_peer_pool_rule & peer_country_count == old_peer_count,
         regression_pass = abs(rate_difference_pp) <= 1e-12 & exact_pool_match)

# Verify the existing input/output manifest, including all observed build parents.
upstream <- inputs$upstream_manifest
upstream$present <- file.exists(upstream$artifact_path)
upstream$actual_sha256 <- vapply(upstream$artifact_path, function(p) {
  if (file.exists(p)) digest(p, algo = "sha256", file = TRUE) else NA_character_
}, character(1))
upstream$hash_matches <- upstream$present & upstream$sha256 == upstream$actual_sha256

check <- function(id, pass, evidence) tibble(check_id = id, passed = isTRUE(pass), evidence = evidence)
quality <- bind_rows(
  check("grid_complete_211_by_13", nrow(panel) == 2743L &&
          identical(sort(unique(panel$analysis_year)), 2012:2024) &&
          all(table(panel$analysis_year) == 211L), "211 countries in every year, 2012-2024"),
  check("grid_keys_unique", !anyDuplicated(panel[c("analysis_year", "iso3")]), "No multiplied or dropped country-years"),
  check("historical_income_join_complete", all(!is.na(assembly$anchors$historical_income_level) &
          nzchar(assembly$anchors$historical_income_level)), "Mandatory historical master-grid classification"),
  check("upstream_observed_hashes_match", all(upstream$hash_matches), paste(nrow(upstream), "recorded parent/output hashes")),
  check("p13_observed_parity_preserved", nrow(inputs$regression) == 50L &&
          all(inputs$regression$legacy_p13_parity_still_passes), "50 previously reconstructed P13 observed cases still pass saved parity"),
  check("peer_validation_surface_reproduced", nrow(peer_regression) == nrow(previous_peer) &&
          all(peer_regression$regression_pass), paste(nrow(peer_regression), "earlier similarity validation rows")),
  check("peer_target_excluded", all(peer$membership$target_iso3 != peer$membership$peer_iso3), "Every peer membership excludes its target"),
  check("peer_seed_lineage_resolves", all(peer$membership$peer_source_evidence_row_id %in%
          assembly$anchors$source_evidence_row_id), "Every peer resolves to an observed anchor"),
  check("no_unapproved_selection", !any(panel$selected_for_ladder) && !any(panel$canonical_benchmark), "Assembly is a candidate, without selected-rate promotion"),
  check("upstream_inputs_unchanged", identical(input_hashes,
          vapply(paths, digest, character(1), algo = "sha256", file = TRUE)), "Input hashes unchanged after computation")
)
print(quality)
if (any(!quality$passed)) stop("Local completion quality gate failed before writing outputs")

out <- "data-derived/p15_local_completion_2012_2024_20260906_v1"
pvr_assert_write_allowed(out, "candidate", root_dir = getwd())
dir.create(out, recursive = TRUE, showWarnings = FALSE)
tables <- list(
  p15_country_year_dataset = panel,
  p15_observed_anchors_with_historical_classification = assembly$anchors,
  p15_observed_evidence_preserved = p15_local_classify(inputs$observed, inputs$grid),
  p15_rating_alternatives_preserved = inputs$ratings,
  p15_peer_target_audit = peer$detail,
  p15_peer_target_membership = peer$membership,
  p15_peer_target_summary = p15_local_peer_summary(peer$detail),
  p15_peer_validation_regression = peer_regression,
  p15_coverage_by_year_income = p15_local_coverage(panel),
  p15_ids_review_cases = panel |>
    filter(ids_legacy_low_rate_rule_conflict | (ids_positive_rate_observed & !ids_term_order_valid)) |>
    select(analysis_year, iso3, country, historical_lmic_reporting_scope,
           starts_with("ids_")),
  p15_quality_checks = quality,
  p15_upstream_manifest_verification = upstream
)
output_paths <- file.path(out, paste0(names(tables), ".csv"))
# An existing build is immutable: identical content is a no-op; changed content
# requires a newly versioned output directory.
write_immutable <- function(data, path) {
  temp <- tempfile(fileext = ".csv")
  on.exit(unlink(temp))
  fwrite(as.data.table(data), temp, na = "")
  if (file.exists(path)) {
    if (!identical(digest(path, algo = "sha256", file = TRUE),
                   digest(temp, algo = "sha256", file = TRUE))) {
      stop("Existing build differs; create a new version: ", path)
    }
  } else if (!file.copy(temp, path)) stop("Could not save ", path)
}
for (j in seq_along(tables)) write_immutable(tables[[j]], output_paths[[j]])

dictionary <- tibble(
  field = names(panel),
  storage_type = vapply(panel, function(x) paste(class(x), collapse = "/"), character(1)),
  missing_values = vapply(panel, function(x) sum(is.na(x)), integer(1)),
  source_family = case_when(
    grepl("^(primary|secondary)_", names(panel)) ~ "integrated_observed_anchor; branch/currency-specific",
    grepl("^ids_", names(panel)) ~ "IDS Bondholders aggregate; currency composition unknown",
    grepl("^rating_", names(panel)) ~ "retained rating variant; uncalibrated; not approved for selection",
    grepl("^peer_|^target_moodys|^rating_proximity|^global_pool", names(panel)) ~ "observable-similarity peer applicability diagnostic",
    TRUE ~ "master grid, status contract, or explicitly derived assembly field"
  ),
  units = if_else(grepl("_pct$|_pp$", names(panel)), "percentage points (5 means 5 percent)",
                  if_else(grepl("_years$", names(panel)), "years", "see dataset README"))
)
write_immutable(dictionary, file.path(out, "p15_data_dictionary.csv"))
output_paths <- c(output_paths, file.path(out, "p15_data_dictionary.csv"))

code_paths <- c("R/p15_local_completion.R", "R/p15_bounded_fallback_comparison.R",
  "R/research_governance.R", "scripts/p15/build_p15_local_completion.R",
  "tests/testthat/test-p15-local-completion.R", "renv.lock")
all_paths <- c(unname(paths), code_paths, output_paths)
manifest <- pvr_manifest_rows(
  all_paths, c(rep("cached_input", length(paths)), rep("code_or_environment_lock", length(code_paths)),
               rep("generated_candidate_output", length(output_paths))),
  build_id = unique(panel$build_id), schema_id = unique(panel$schema_id),
  estimator_id = unique(panel$estimator_id), admissibility_id = unique(panel$admissibility_id),
  selection_id = unique(panel$selection_id),
  source_package_ids = paste(sort(unique(c(inputs$ids$ids_source_package_id,
    unlist(strsplit(assembly$anchors$source_package_ids, ";", fixed = TRUE))))), collapse = ";")
)
write_immutable(manifest, file.path(out, "p15_build_manifest.csv"))
environment <- tibble(package = c("R", "data.table", "dplyr", "tibble", "digest", "testthat"),
  version = c(as.character(getRversion()), vapply(c("data.table", "dplyr", "tibble", "digest", "testthat"),
    function(x) as.character(packageVersion(x)), character(1))))
write_immutable(environment, file.path(out, "p15_environment.csv"))
cat("\nLocal dataset created:", out, "\n")
print(p15_local_peer_summary(peer$detail), width = Inf)
print(panel |> filter(historical_lmic_reporting_scope) |> summarise(
  rows = n(), primary_usd = sum(is.finite(primary_usd_market_rate_pct)),
  secondary_usd = sum(is.finite(secondary_usd_market_rate_pct)),
  either_usd = sum(any_usd_observed_candidate), ids_positive = sum(ids_positive_rate_observed),
  ids_additional = sum(ids_positive_rate_observed & !any_usd_observed_candidate),
  no_usd_or_ids = sum(fallback_audit_cohort == "no_usd_observed_or_positive_ids")))
