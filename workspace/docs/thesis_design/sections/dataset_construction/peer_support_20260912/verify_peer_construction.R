# Read-only verification of the construction described in the peer subsection.
# Run from project root; no benchmark or analysis output is written.
source("scripts/p15/activate_p15_environment.R")
source("R/p15_bounded_fallback_comparison.R")
suppressPackageStartupMessages({library(data.table); library(jsonlite); library(digest)})
candidate <- fromJSON("data-derived/p15_master/current_run.json")$candidate
files <- file.path(candidate, c("core_evidence.csv", "peer_membership.csv",
  "peer_region_context.csv", "tier_eligibility.csv"))
before <- vapply(files, function(p) digest(file = p, algo = "sha256"), character(1))
core <- fread(files[1], na.strings = "")
members <- fread(files[2], na.strings = "")
context <- fread(files[3], na.strings = "")
eligibility <- fread(files[4], na.strings = "")[tier == "primary"]
eligible <- merge(core, eligibility[, .(analysis_year, iso3, eligible)],
  by = c("analysis_year", "iso3"))
eligible <- merge(eligible, context[, .(analysis_year, iso3,
  rating_source_region, moodys_rating_normalized)], by = c("analysis_year", "iso3"))
selected <- members[used_for_estimate %in% TRUE]
aggregates <- selected[, .(rate = median(peer_rate_pct), count = .N),
  by = .(analysis_year, iso3 = target_iso3)]
joined <- merge(core, aggregates, by = c("analysis_year", "iso3"), all.x = TRUE)
member_source <- merge(members, eligibility[, .(analysis_year, peer_iso3 = iso3, eligible)],
  by = c("analysis_year", "peer_iso3"), all.x = TRUE)
member_source <- merge(member_source, core[, .(analysis_year, peer_iso3 = iso3,
  observed_rate = primary_usd_market_rate_pct)], by = c("analysis_year", "peer_iso3"), all.x = TRUE)
same_values <- function(a, b) identical(is.na(a), is.na(b)) && all(abs(a - b) < 1e-10, na.rm = TRUE)
pool_matches <- vapply(seq_len(nrow(eligible)), function(i) {
  target <- eligible[i]
  seeds <- eligible[analysis_year == target$analysis_year & iso3 != target$iso3 & eligible %in% TRUE]
  pick <- p15_peer_similarity_pool(as.data.frame(seeds), as.data.frame(target), 3L)
  actual <- members[analysis_year == target$analysis_year & target_iso3 == target$iso3]
  identical(as.character(target$peer_pool_rule), as.character(pick$rule)) &&
    setequal(actual$peer_iso3, pick$pool$iso3)
}, logical(1))
checks <- list(
  unique_country_year_peer_members = !anyDuplicated(members[, .(analysis_year, target_iso3, peer_iso3)]),
  target_excluded = all(members$target_iso3 != members$peer_iso3),
  members_are_eligible_primary_issuers = all(member_source$eligible %in% TRUE),
  member_rates_equal_same_year_primary_rates = same_values(member_source$peer_rate_pct, member_source$observed_rate),
  saved_medians_reproduced = same_values(joined$peer_rate_pct, joined$rate),
  minimum_three_for_finite_estimates = all(joined[is.finite(peer_rate_pct), count >= 3L]),
  counts_match_saved_members = all(joined[is.finite(peer_rate_pct), count == peer_country_count]),
  first_qualifying_group_and_membership_reproduced = all(pool_matches),
  source_inputs_unchanged = identical(before, vapply(files, function(p) digest(file = p, algo = "sha256"), character(1)))
)
stopifnot(all(unlist(checks)))
receipt <- list(date = "2026-09-12", purpose = "Existing method checked for subsection drafting",
  checks = checks, benchmark_change = FALSE, candidate = candidate,
  sha256 = as.list(before))
write_json(receipt,
  "docs/thesis_design/sections/dataset_construction/peer_support_20260912/method_verification.json",
  pretty = TRUE, auto_unbox = TRUE)
cat("Passed", length(checks), "read-only method checks. No benchmark values changed.\n")
