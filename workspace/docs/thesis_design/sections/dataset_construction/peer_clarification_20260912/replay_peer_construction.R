# Run from the project root. Read-only replay of the production peer calculation.
# Output is a verification receipt; this does not rebuild or change the platform.
suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(jsonlite)
  library(digest)
})
source("R/p15_bounded_fallback_comparison.R")
source("R/p15_peer_region_correction.R")

receipt_dir <- "docs/thesis_design/sections/dataset_construction/peer_clarification_20260912"
pointer <- jsonlite::read_json("data-derived/p15_master/current_run.json", simplifyVector = TRUE)
candidate <- pointer$candidate
input_paths <- file.path(candidate, c("core_evidence.csv", "peer_membership.csv",
                                    "peer_region_context.csv", "tier_eligibility.csv"))
code_paths <- c("R/p15_bounded_fallback_comparison.R", "R/p15_peer_region_correction.R")
hash_files <- function(paths) setNames(vapply(paths, digest::digest, character(1),
                                            algo = "sha256", file = TRUE), paths)
input_hash_before <- hash_files(input_paths)
panel <- read_csv(input_paths[1], show_col_types = FALSE, guess_max = Inf)
saved_membership <- read_csv(input_paths[2], show_col_types = FALSE, guess_max = Inf)
context <- read_csv(input_paths[3], show_col_types = FALSE, guess_max = Inf)
eligibility <- read_csv(input_paths[4], show_col_types = FALSE, guess_max = Inf)
stopifnot(all(vapply(list(panel, saved_membership, context, eligibility),
                    function(x) nrow(readr::problems(x)) == 0L, logical(1))))
seed_metadata <- saved_membership |>
  distinct(analysis_year, peer_iso3, peer_spread_pct)
stopifnot(!anyDuplicated(seed_metadata[c("analysis_year", "peer_iso3")]))

replayed <- p15_recompute_reference_peers(
  as.data.frame(panel), as.data.frame(context), as.data.frame(eligibility),
  as.data.frame(seed_metadata)
)

compare_column <- function(a, b, tolerance = 1e-12) {
  both_na <- is.na(a) & is.na(b)
  same <- both_na
  observed <- !is.na(a) & !is.na(b)
  if (is.numeric(a) && is.numeric(b)) {
    same[observed] <- abs(a[observed] - b[observed]) <= tolerance
  } else {
    same[observed] <- a[observed] == b[observed]
  }
  sum(!same)
}
detail <- replayed$detail |> arrange(analysis_year, iso3)
saved_detail <- panel |> arrange(analysis_year, iso3)
stopifnot(identical(detail$analysis_year, saved_detail$analysis_year),
          identical(detail$iso3, saved_detail$iso3))
detail_columns <- setdiff(names(detail), c("analysis_year", "iso3"))
detail_mismatches <- setNames(vapply(detail_columns, function(nm)
  compare_column(detail[[nm]], saved_detail[[nm]]), integer(1)), detail_columns)

members <- replayed$membership |> arrange(analysis_year, target_iso3, peer_iso3)
saved_members <- saved_membership |> arrange(analysis_year, target_iso3, peer_iso3)
stopifnot(nrow(members) == nrow(saved_members))
member_columns <- names(members)
membership_mismatches <- setNames(vapply(member_columns, function(nm)
  compare_column(members[[nm]], saved_members[[nm]]), integer(1)), member_columns)

# Independently check the selected sets and simple-median arithmetic.
# These checks use categorical equality, an explicit Moody's ordinal scale,
# and the documented ordered rules, not the production pool helper.
scale <- c("Aaa", "Aa1", "Aa2", "Aa3", "A1", "A2", "A3", "Baa1", "Baa2",
           "Baa3", "Ba1", "Ba2", "Ba3", "B1", "B2", "B3", "Caa1", "Caa2",
           "Caa3", "Ca", "C")
joined <- panel |>
  select(analysis_year, iso3, historical_income_level, primary_usd_market_rate_pct) |>
  left_join(context |> select(analysis_year, iso3, rating_source_region,
                             moodys_rating_normalized), by = c("analysis_year", "iso3")) |>
  left_join(eligibility |> filter(tier == "primary") |>
              select(analysis_year, iso3, eligible), by = c("analysis_year", "iso3"))
independent_set_mismatches <- 0L
independent_rule_mismatches <- 0L
independent_median_mismatches <- 0L
for (i in seq_len(nrow(joined))) {
  target <- joined[i, ]
  available <- joined[joined$analysis_year == target$analysis_year &
                        joined$iso3 != target$iso3 & joined$eligible, ]
  income <- !is.na(available$historical_income_level) &
    available$historical_income_level == target$historical_income_level
  region <- !is.na(available$rating_source_region) &
    available$rating_source_region == target$rating_source_region
  proximity <- abs(match(available$moodys_rating_normalized, scale) -
                     match(target$moodys_rating_normalized, scale)) <= 3
  predicates <- list(income & region & proximity, income & proximity,
                     region & proximity, proximity, income & region, income,
                     region, rep(TRUE, nrow(available)))
  labels <- c("same_income_region_rating3_min3", "same_income_rating3_min3",
              "same_region_rating3_min3", "rating3_min3", "same_income_region_min3",
              "same_income_min3", "same_region_min3", "global_min3")
  if (nrow(available)) {
    counts <- vapply(predicates, function(p) sum(p %in% TRUE), integer(1))
    group <- if (any(counts >= 3)) which(counts >= 3)[1] else 8L
    chosen <- available[predicates[[group]] %in% TRUE, ]
    rule <- labels[group]
  } else {
    chosen <- available
    rule <- "no_eligible_peers"
  }
  saved <- saved_membership[saved_membership$analysis_year == target$analysis_year &
                              saved_membership$target_iso3 == target$iso3, ]
  independent_set_mismatches <- independent_set_mismatches +
    as.integer(!identical(sort(chosen$iso3), sort(saved$peer_iso3)))
  independent_rule_mismatches <- independent_rule_mismatches +
    as.integer(panel$peer_pool_rule[i] != rule)
  expected_rate <- if (nrow(chosen) >= 3) median(chosen$primary_usd_market_rate_pct) else NA_real_
  independent_median_mismatches <- independent_median_mismatches +
    compare_column(expected_rate, panel$peer_rate_pct[i])
}

input_hash_after <- hash_files(input_paths)
checks <- c(
  detail_matches_saved = all(detail_mismatches == 0),
  membership_matches_saved = all(membership_mismatches == 0),
  independent_sets_match = independent_set_mismatches == 0,
  independent_ordered_rules_match = independent_rule_mismatches == 0,
  independent_unweighted_medians_match = independent_median_mismatches == 0,
  no_self_peers = all(saved_membership$target_iso3 != saved_membership$peer_iso3),
  no_duplicate_peer_per_target_year = !anyDuplicated(saved_membership[c("analysis_year", "target_iso3", "peer_iso3")]),
  all_usable_groups_meet_minimum = all(panel$peer_country_count[panel$peer_minimum_met] >= 3),
  all_eligible_primary_rates_finite = all(is.finite(joined$primary_usd_market_rate_pct[joined$eligible])),
  raw_candidate_inputs_unchanged = identical(input_hash_before, input_hash_after)
)
receipt <- list(
  verification_date = "2026-09-12",
  candidate_path = candidate,
  calculation = "Executed p15_recompute_reference_peers, then independent set/rule/median reconstruction.",
  scope = "Read-only current candidate replay, all country-years including outside the historical LMIC reporting scope.",
  country_years = nrow(panel),
  years = range(panel$analysis_year),
  peer_membership_records = nrow(saved_membership),
  distinct_peer_seed_country_years = nrow(seed_metadata),
  usable_groups = sum(panel$peer_minimum_met),
  unavailable_groups = sum(!panel$peer_minimum_met),
  detail_mismatches = as.list(detail_mismatches),
  membership_mismatches = as.list(membership_mismatches),
  independent_set_mismatches = independent_set_mismatches,
  independent_rule_mismatches = independent_rule_mismatches,
  independent_median_mismatches = independent_median_mismatches,
  checks = as.list(checks),
  input_sha256 = as.list(input_hash_before),
  production_code_sha256 = as.list(hash_files(code_paths)),
  replay_script_sha256 = digest::digest(file.path(receipt_dir, "replay_peer_construction.R"),
                                       algo = "sha256", file = TRUE),
  semantics = c(
    "Same income means exact category, not closest income or a numeric income distance.",
    "Same region means exact recorded region, not geographic distance.",
    "Rating proximity means at most three ordinal steps on Moody's scale.",
    "The first ordered group containing at least three countries is chosen in full.",
    "Three is a minimum, not a cap; there is no nearest-three ranking.",
    "The aggregation is a simple median of eligible annual USD primary country rates.",
    "No similarity score or similarity weighting is calculated.",
    "Within-country primary rates already reflect issuance-amount weighting.",
    "Seed spread metadata is carried into the replay membership output only; it does not determine peer selection or the raw-rate median."
  )
)
dir.create(receipt_dir, recursive = TRUE, showWarnings = FALSE)
jsonlite::write_json(receipt, file.path(receipt_dir, "peer_execution_verification.json"),
                     pretty = TRUE, auto_unbox = TRUE, na = "null")
writeLines(capture.output(sessionInfo()), file.path(receipt_dir, "replay_session_info.txt"))
print(checks)
cat("Replayed", nrow(panel), "country-years and", nrow(saved_membership),
    "peer membership records.\n")
stopifnot(all(checks))
