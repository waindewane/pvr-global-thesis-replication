#!/usr/bin/env Rscript

# First P15/P13 parity gate: determine whether the instruments underlying every
# P13-selected observed-market row can be found in the P15 unified source layer.
# This is a source-readiness audit, not a recomputation of rates or admissibility.

options(stringsAsFactors = FALSE)

`%||%` <- function(x, y) if (is.null(x) || !length(x) || all(is.na(x))) y else x

root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
output_dir <- file.path(root, "docs", "governance")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

p15_universe_path <- file.path(
  root,
  "data-derived/p15_unified_lseg_source_2012_2024_v1/p15_instrument_year_universe_2012_2024.csv.gz"
)
p15_history_path <- file.path(
  root,
  "data-derived/p15_unified_lseg_source_2012_2024_v1/p15_secondary_history_long_2012_2024.csv.gz"
)
p13_path <- file.path(
  root,
  "experiments/full_ladder_ratings_integrated_status_rebuild_2026-06-01/outputs/",
  "p13_best_available_benchmark_rate_2024.csv"
)
p13_full_path <- file.path(
  root,
  "experiments/full_ladder_ratings_integrated_status_rebuild_2026-06-01/outputs/",
  "p13_full_labelled_rate_value_ladder_2024.csv"
)
p12_direct_path <- file.path(
  root,
  "experiments/full_ladder_ratings_integrated_status_rebuild_2026-06-01/outputs/",
  "p12a_secondary_direct_country_scenarios_2024.csv"
)
p12_feature_path <- file.path(
  root,
  "experiments/full_ladder_ratings_integrated_status_rebuild_2026-06-01/outputs/",
  "p12a_feature_rich_secondary_accepted_issue_layer_2024.csv"
)
terminal_review_path <- file.path(
  root,
  "experiments/full_ladder_ratings_integrated_status_rebuild_2026-06-01/outputs/",
  "secondary_terminal_post_audit_review_classes_2024.csv"
)
price_trial_path <- file.path(
  root,
  "experiments/full_ladder_ratings_integrated_status_rebuild_2026-06-01/outputs/",
  "secondary_price_to_yield_trial_results_2024.csv"
)
historical_secondary_path <- file.path(
  root,
  "experiments/full_ladder_database_all_years_2026-05-20/outputs/",
  "secondary_issue_level_all_years.csv"
)
stopifnot(vapply(
  c(
    p15_universe_path, p15_history_path, p13_path, p13_full_path, p12_direct_path,
    p12_feature_path, terminal_review_path, price_trial_path,
    historical_secondary_path
  ),
  file.exists,
  logical(1)
))

u <- data.table::fread(p15_universe_path, showProgress = FALSE)
u <- u[snapshot_year == 2024]
h <- data.table::fread(p15_history_path, showProgress = FALSE)
h <- h[analysis_year == 2024]
p13 <- data.table::fread(p13_path, showProgress = FALSE)
p13_full <- data.table::fread(p13_full_path, showProgress = FALSE)
p12_direct <- data.table::fread(p12_direct_path, showProgress = FALSE)
p12_feature <- data.table::fread(p12_feature_path, showProgress = FALSE)
terminal_review <- data.table::fread(terminal_review_path, showProgress = FALSE)
price_trial <- data.table::fread(price_trial_path, showProgress = FALSE)
historical_secondary <- data.table::fread(historical_secondary_path, showProgress = FALSE)

lseg_selected <- p13[
  grepl("lseg|p12a", selected_source_class, ignore.case = TRUE)
]
stopifnot(nrow(lseg_selected) == 50L)

history_stats <- h[, .(
  p15_history_rows = .N,
  p15_rows_with_any_measure = sum(any_observed_measure),
  p15_rows_with_price = sum(!is.na(mid_price) | !is.na(bid) | !is.na(ask)),
  p15_rows_with_direct_yield = sum(!is.na(yield_to_maturity)),
  p15_history_source_packages = paste(sort(unique(source_package_id)), collapse = ";")
), by = RIC]

split_values <- function(x) {
  x <- x[!is.na(x) & nzchar(x)]
  if (!length(x)) return(character())
  trimws(unlist(strsplit(x, ";", fixed = TRUE), use.names = FALSE))
}

parse_issue_keys <- function(pointer) {
  keys <- split_values(pointer)
  rows <- lapply(keys, function(key) {
    parts <- trimws(strsplit(key, "|", fixed = TRUE)[[1]])
    if (length(parts) < 5L) return(NULL)
    data.frame(
      issue_key = key,
      issue_date = parts[[2]],
      maturity_date = parts[[3]],
      currency = parts[[4]],
      coupon_rate = suppressWarnings(as.numeric(parts[[5]])),
      stringsAsFactors = FALSE
    )
  })
  rows <- rows[!vapply(rows, is.null, logical(1))]
  if (!length(rows)) {
    return(data.frame(
      issue_key = character(), issue_date = character(), maturity_date = character(),
      currency = character(), coupon_rate = numeric(), stringsAsFactors = FALSE
    ))
  }
  do.call(rbind, rows)
}

match_issue_keys_to_p15 <- function(iso3, pointer) {
  iso3_value <- iso3
  parsed <- parse_issue_keys(pointer)
  if (!nrow(parsed)) return(data.table::data.table())
  matched <- lapply(seq_len(nrow(parsed)), function(i) {
    row <- parsed[i, ]

    # P12A was constructed from the historical issue layer. Its representative
    # identifiers are therefore the authoritative identifier set for parity.
    # Static-term matching against the broader P15 universe is only a fallback.
    historical_match <- historical_secondary[
      analysis_year == 2024 & iso3 == iso3_value & economic_issue_key == row$issue_key
    ]
    historical_rics <- split_values(historical_match$representative_rics)
    if (length(historical_rics)) {
      return(data.table::data.table(
        issue_key = row$issue_key,
        RIC = historical_rics,
        ISIN = NA_character_,
        match_route = "p12a_issue_key_to_historical_representative_ric"
      ))
    }

    candidates <- u[
      canonical_iso3 == iso3_value &
        as.character(IssueDate) == row$issue_date &
        as.character(MaturityDate) == row$maturity_date &
        Currency == row$currency &
        abs(CouponRate - row$coupon_rate) < 1e-7
    ]
    if (!nrow(candidates)) {
      return(data.table::data.table(
        issue_key = row$issue_key,
        RIC = NA_character_,
        ISIN = NA_character_,
        match_route = "issue_key_no_p15_match"
      ))
    }
    candidates[, .(
      issue_key = row$issue_key,
      RIC,
      ISIN,
      match_route = "issue_key_exact_terms"
    )]
  })
  data.table::rbindlist(matched, use.names = TRUE, fill = TRUE)
}

expand_identifier_rows <- function(source_rows, key_col, ric_col, match_route) {
  if (!nrow(source_rows)) return(data.table::data.table())
  expanded <- lapply(seq_len(nrow(source_rows)), function(i) {
    issue_key <- as.character(source_rows[[key_col]][[i]])
    rics <- split_values(source_rows[[ric_col]][[i]])
    if (!length(rics)) rics <- NA_character_
    data.table::data.table(
      issue_key = issue_key,
      RIC = rics,
      ISIN = NA_character_,
      match_route = match_route
    )
  })
  data.table::rbindlist(expanded, use.names = TRUE, fill = TRUE)
}

secondary_classes <- c(
  "p12a_secondary_direct_yield_rebuild",
  "p12a_feature_rich_vendor_yield_review",
  "lseg_terminal_secondary_direct_ytm",
  "lseg_terminal_secondary_price_to_yield",
  "lseg_research_grade_secondary_direct_ytm"
)
secondary_selected <- lseg_selected[selected_source_class %in% secondary_classes]
stopifnot(nrow(secondary_selected) == 25L)

crosswalk_rows <- lapply(seq_len(nrow(secondary_selected)), function(i) {
  row <- secondary_selected[i]
  class <- row$selected_source_class
  matches <- data.table::data.table()

  if (class == "p12a_secondary_direct_yield_rebuild") {
    matches <- match_issue_keys_to_p15(row$iso3, row$selected_source_pointer)
  } else if (class == "p12a_feature_rich_vendor_yield_review") {
    ids <- split_values(row$selected_source_pointer)
    source_rows <- p12_feature[issue_layer_id %in% ids]
    matches <- expand_identifier_rows(
      source_rows,
      key_col = "issue_layer_id",
      ric_col = "terminal_identifiers",
      match_route = "p12a_feature_issue_id_to_terminal_ric"
    )
  } else if (class == "lseg_terminal_secondary_direct_ytm") {
    # The target IDs are in P13's source note rather than the pointer path.
    notes <- p13_full[
      iso3 == row$iso3 & source_class == class & !is.na(rate_pct),
      source_note
    ]
    note <- paste(notes[!is.na(notes)], collapse = ";")
    target_ids <- unique(unlist(regmatches(
      note,
      gregexpr("P7B-SEC-[0-9]{3}", note)
    )))
    source_rows <- terminal_review[p7b_target_id %in% target_ids]
    matches <- expand_identifier_rows(
      source_rows,
      key_col = "p7b_target_id",
      ric_col = "ric",
      match_route = "p7b_target_to_terminal_ric"
    )
  } else if (class == "lseg_terminal_secondary_price_to_yield") {
    targets <- unique(price_trial[iso3 == row$iso3, p7b_target_id])
    source_rows <- terminal_review[p7b_target_id %in% targets]
    matches <- expand_identifier_rows(
      source_rows,
      key_col = "p7b_target_id",
      ric_col = "ric",
      match_route = "price_trial_target_to_terminal_ric"
    )
  } else if (class == "lseg_research_grade_secondary_direct_ytm") {
    source_rows <- historical_secondary[
      analysis_year == 2024 & iso3 == row$iso3 &
        !is.na(secondary_yield_pct) & grepl("included_standard_secondary", baseline_status)
    ]
    matches <- expand_identifier_rows(
      source_rows,
      key_col = "economic_issue_key",
      ric_col = "representative_rics",
      match_route = "historical_secondary_representative_ric"
    )
  }

  if (!nrow(matches)) {
    matches <- data.table::data.table(
      issue_key = NA_character_, RIC = NA_character_, ISIN = NA_character_,
      match_route = "no_crosswalk_generated"
    )
  }
  matches <- unique(matches)
  matches[, `:=`(
    analysis_year = 2024L,
    iso3 = row$iso3,
    country = row$country,
    p13_selected_rate_pct = row$selected_rate_pct,
    p13_selected_tier_id = row$selected_tier_id,
    p13_selected_source_class = class
  )]
  data.table::setcolorder(matches, c(
    "analysis_year", "iso3", "country", "p13_selected_rate_pct",
    "p13_selected_tier_id", "p13_selected_source_class", "issue_key",
    "RIC", "ISIN", "match_route"
  ))
  matches
})
secondary_crosswalk <- data.table::rbindlist(crosswalk_rows, use.names = TRUE, fill = TRUE)

universe_match <- u[, .(
  RIC,
  p15_universe_match = TRUE,
  p15_universe_source_state = p15_history_coverage_state,
  p15_universe_measure_contract = p15_available_measure_contract,
  p15_universe_source_package = p15_history_source_package_id
)]
secondary_crosswalk <- merge(
  secondary_crosswalk,
  universe_match,
  by = "RIC",
  all.x = TRUE,
  sort = FALSE
)
secondary_crosswalk <- merge(
  secondary_crosswalk,
  history_stats,
  by = "RIC",
  all.x = TRUE,
  sort = FALSE
)
secondary_crosswalk[, p15_universe_match := !is.na(p15_universe_match) & p15_universe_match]
for (nm in c(
  "p15_history_rows", "p15_rows_with_any_measure", "p15_rows_with_price",
  "p15_rows_with_direct_yield"
)) {
  data.table::set(secondary_crosswalk, which(is.na(secondary_crosswalk[[nm]])), nm, 0)
}

secondary_crosswalk[, required_measure := data.table::fcase(
  p13_selected_source_class == "lseg_terminal_secondary_price_to_yield", "price",
  default = "direct_yield"
)]
secondary_crosswalk[, required_measure_present := data.table::fcase(
  required_measure == "price", p15_rows_with_price > 0,
  required_measure == "direct_yield", p15_rows_with_direct_yield > 0,
  default = FALSE
)]

issue_support <- secondary_crosswalk[, .(
  expected_identifier_rows = .N,
  expected_unique_rics = data.table::uniqueN(RIC[!is.na(RIC)]),
  p15_universe_matched_rics = data.table::uniqueN(RIC[!is.na(RIC) & p15_universe_match]),
  p15_required_measure_rics = data.table::uniqueN(RIC[!is.na(RIC) & required_measure_present]),
  has_unresolved_identifier = any(is.na(RIC)),
  required_measure = unique(required_measure)[[1]]
), by = .(
  analysis_year, iso3, country, p13_selected_rate_pct,
  p13_selected_tier_id, p13_selected_source_class, issue_key
)]
issue_support[, issue_support_state := data.table::fcase(
  expected_unique_rics == 0L, "unresolved_issue_identifier",
  p15_required_measure_rics == expected_unique_rics & !has_unresolved_identifier,
  "complete_required_measure_support",
  p15_required_measure_rics > 0L, "partial_required_measure_support",
  p15_universe_matched_rics > 0L, "instrument_present_required_measure_absent",
  default = "instrument_absent_from_p15"
)]

secondary_audit <- secondary_crosswalk[, .(
  p13_selected_rate_pct = unique(p13_selected_rate_pct)[[1]],
  p13_selected_tier_id = unique(p13_selected_tier_id)[[1]],
  p13_selected_source_class = unique(p13_selected_source_class)[[1]],
  expected_identifier_rows = .N,
  expected_unique_rics = data.table::uniqueN(RIC[!is.na(RIC)]),
  p15_universe_matched_rics = data.table::uniqueN(RIC[!is.na(RIC) & p15_universe_match]),
  p15_history_matched_rics = data.table::uniqueN(RIC[!is.na(RIC) & p15_history_rows > 0]),
  p15_rics_with_price = data.table::uniqueN(RIC[!is.na(RIC) & p15_rows_with_price > 0]),
  p15_rics_with_direct_yield = data.table::uniqueN(RIC[!is.na(RIC) & p15_rows_with_direct_yield > 0]),
  p15_tail_fill_rics = data.table::uniqueN(RIC[
    !is.na(RIC) & !is.na(p15_universe_source_package) &
      p15_universe_source_package == "SRC-LSEG-RG-20260522"
  ]),
  exact_identifier_crosswalk_state = if (any(is.na(RIC))) "incomplete" else "complete"
), by = .(analysis_year, iso3, country)]

issue_rollup <- issue_support[, .(
  expected_issue_groups = .N,
  complete_issue_groups = sum(issue_support_state == "complete_required_measure_support"),
  partial_issue_groups = sum(issue_support_state == "partial_required_measure_support"),
  unsupported_issue_groups = sum(!issue_support_state %in% c(
    "complete_required_measure_support", "partial_required_measure_support"
  )),
  issue_groups_with_any_required_measure = sum(p15_required_measure_rics > 0L),
  required_measure = unique(required_measure)[[1]]
), by = .(analysis_year, iso3, country)]
secondary_audit <- merge(
  secondary_audit,
  issue_rollup,
  by = c("analysis_year", "iso3", "country"),
  all.x = TRUE,
  sort = FALSE
)

secondary_audit[, p15_source_readiness_state := data.table::fcase(
  complete_issue_groups == expected_issue_groups & exact_identifier_crosswalk_state == "complete",
  "source_ready_for_exact_method_replay",
  issue_groups_with_any_required_measure == expected_issue_groups & expected_issue_groups > 0L,
  "all_issues_structurally_covered_but_companion_needed_for_exact_replay",
  issue_groups_with_any_required_measure > 0L,
  "partial_p15_support_requires_p13_companion",
  default = "p15_source_gap_requires_p13_companion"
)]

primary_selected <- lseg_selected[selected_source_class == "lseg_research_grade_observed_primary"]
stopifnot(nrow(primary_selected) == 25L)
primary_audit <- primary_selected[, {
  instruments <- u[
    canonical_iso3 == .BY$iso3 & issue_year == 2024 & Currency %in% c("USD", "EUR")
  ]
  yield_present <- !is.na(instruments$OriginalYieldMaturity) |
    !is.na(instruments$MaturityStandardYield)
  .(
    p13_selected_rate_pct = selected_rate_pct[[1]],
    p13_selected_tier_id = selected_tier_id[[1]],
    p13_selected_source_class = selected_source_class[[1]],
    p15_2024_issued_instruments = nrow(instruments),
    p15_2024_issued_instruments_with_primary_yield_field = sum(yield_present),
    p15_source_readiness_state = if (any(yield_present)) {
      "source_ready_for_primary_method_replay"
    } else {
      "primary_source_gap_requires_review"
    }
  )
}, by = .(analysis_year, iso3, country)]

observed_audit <- data.table::rbindlist(list(
  primary_audit,
  secondary_audit
), use.names = TRUE, fill = TRUE)
data.table::setorder(observed_audit, iso3)
data.table::setorder(secondary_crosswalk, iso3, RIC)

summary <- observed_audit[, .N, by = .(
  p13_selected_source_class,
  p15_source_readiness_state
)][order(p13_selected_source_class, p15_source_readiness_state)]
summary <- data.table::rbindlist(list(
  summary,
  data.table::data.table(
    p13_selected_source_class = "ALL_OBSERVED_LSEG_SELECTED_ROWS",
    p15_source_readiness_state = "total",
    N = nrow(observed_audit)
  )
), use.names = TRUE)

stopifnot(
  nrow(primary_audit) == 25L,
  nrow(secondary_audit) == 25L,
  nrow(observed_audit) == 50L,
  all(secondary_audit$expected_issue_groups > 0L),
  !any(
    secondary_audit$p15_source_readiness_state == "source_ready_for_exact_method_replay" &
      secondary_audit$complete_issue_groups != secondary_audit$expected_issue_groups
  )
)

observed_path <- file.path(output_dir, "p15_p13_2024_observed_source_readiness.csv")
crosswalk_path <- file.path(output_dir, "p15_p13_2024_secondary_instrument_crosswalk.csv")
issue_support_path <- file.path(output_dir, "p15_p13_2024_secondary_issue_support.csv")
summary_path <- file.path(output_dir, "p15_p13_2024_source_readiness_summary.csv")

data.table::fwrite(
  observed_audit,
  observed_path,
  na = ""
)
data.table::fwrite(
  secondary_crosswalk,
  crosswalk_path,
  na = ""
)
data.table::fwrite(
  issue_support,
  issue_support_path,
  na = ""
)
data.table::fwrite(
  summary,
  summary_path,
  na = ""
)

manifest_paths <- c(observed_path, crosswalk_path, issue_support_path, summary_path)
script_path <- file.path(root, "scripts/p15/audit_p15_p13_2024_source_readiness.R")
manifest <- data.table::rbindlist(lapply(manifest_paths, function(path) {
  contents <- data.table::fread(path, showProgress = FALSE)
  data.table::data.table(
    artifact_path = sub(paste0("^", root, "/"), "", path),
    artifact_role = gsub("^p15_p13_2024_|\\.csv$", "", basename(path)),
    rows = nrow(contents),
    columns = ncol(contents),
    bytes = file.info(path)$size,
    sha256 = digest::digest(file = path, algo = "sha256"),
    producing_script = sub(paste0("^", root, "/"), "", script_path),
    producing_script_sha256 = digest::digest(file = script_path, algo = "sha256"),
    build_id = "BUILD-P15-P13-SOURCE-READINESS-20260721-V1",
    build_date = "2026-07-21"
  )
}), use.names = TRUE, fill = TRUE)
data.table::fwrite(
  manifest,
  file.path(output_dir, "p15_p13_2024_source_readiness_manifest.csv"),
  na = ""
)

cat("P15/P13 source-readiness audit written.\n")
cat("P13 observed LSEG selected rows:", nrow(observed_audit), "\n")
print(summary)
