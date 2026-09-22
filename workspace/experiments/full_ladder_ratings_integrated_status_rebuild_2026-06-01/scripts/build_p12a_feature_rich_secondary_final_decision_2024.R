suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(tidyr)
  library(tibble)
  library(stringr)
})

experiment_dir <- "experiments/full_ladder_ratings_integrated_status_rebuild_2026-06-01"
out_dir <- file.path(experiment_dir, "outputs")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

generated_by <- file.path(
  experiment_dir,
  "scripts",
  "build_p12a_feature_rich_secondary_final_decision_2024.R"
)
generated_at <- format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")
last_updated <- as.character(Sys.Date())

row_review_path <- file.path(out_dir, "p12a_feature_rich_v12_matched_yield_review_2024.csv")
issue_review_path <- file.path(out_dir, "p12a_feature_rich_v12_matched_issue_summary_2024.csv")
raw_opportunity_path <- file.path(out_dir, "p12a_secondary_raw_price_identifier_opportunity_2024.csv")
p12_path <- file.path(out_dir, "p12_full_ladder_country_year_2024.csv")

output_files <- c(
  row_decisions = "p12a_feature_rich_secondary_final_decision_2024.csv",
  issue_layer = "p12a_feature_rich_secondary_accepted_issue_layer_2024.csv",
  country_layer = "p12a_feature_rich_secondary_country_layer_2024.csv",
  source_basis = "p12a_feature_rich_secondary_source_basis_2024.csv",
  decision_memo = "p12a_feature_rich_secondary_final_decision_memo_2024.md"
)

path_out <- function(file_name) file.path(out_dir, file_name)

read_csv_required <- function(path) {
  if (!file.exists(path)) stop("Required file missing: ", path, call. = FALSE)
  readr::read_csv(path, show_col_types = FALSE, guess_max = 100000)
}

write_csv_na <- function(x, path) {
  readr::write_csv(x, path, na = "")
  invisible(path)
}

has_text <- function(x) !is.na(x) & nzchar(as.character(x))

collapse_unique <- function(x, n = Inf) {
  x <- sort(unique(as.character(x[has_text(x)])))
  if (!length(x)) return("")
  if (is.finite(n)) x <- head(x, n)
  paste(x, collapse = ";")
}

median_or_na <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  x <- x[!is.na(x)]
  if (!length(x)) return(NA_real_)
  median(x)
}

max_or_na <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  x <- x[!is.na(x)]
  if (!length(x)) return(NA_real_)
  max(x)
}

weighted_mean_or_na <- function(x, w) {
  x <- suppressWarnings(as.numeric(x))
  w <- suppressWarnings(as.numeric(w))
  ok <- !is.na(x) & !is.na(w) & w > 0
  if (!any(ok)) return(NA_real_)
  sum(x[ok] * w[ok]) / sum(w[ok])
}

sha256_file <- function(path) {
  if (!file.exists(path)) return(NA_character_)
  out <- tryCatch(
    system2("shasum", c("-a", "256", path), stdout = TRUE, stderr = FALSE),
    error = function(e) character()
  )
  if (!length(out) || !nzchar(out[[1]])) return(NA_character_)
  strsplit(out[[1]], "[[:space:]]+")[[1]][[1]]
}

file_shape <- function(path) {
  if (!file.exists(path) || !str_detect(path, "[.]csv$")) {
    return(tibble(rows = NA_integer_, columns = NA_integer_))
  }
  x <- readr::read_csv(path, show_col_types = FALSE, guess_max = 100000)
  tibble(rows = nrow(x), columns = ncol(x))
}

upsert_by_key <- function(existing, new_rows, key) {
  if (is.null(existing) || nrow(existing) == 0) return(new_rows)
  missing_existing <- setdiff(names(new_rows), names(existing))
  for (nm in missing_existing) existing[[nm]] <- NA
  missing_new <- setdiff(names(existing), names(new_rows))
  for (nm in missing_new) new_rows[[nm]] <- NA
  for (nm in intersect(names(existing), names(new_rows))) {
    if (!identical(class(existing[[nm]]), class(new_rows[[nm]]))) {
      existing[[nm]] <- as.character(existing[[nm]])
      new_rows[[nm]] <- as.character(new_rows[[nm]])
    }
  }
  new_rows <- new_rows[, names(existing)]
  bind_rows(
    existing |> filter(!.data[[key]] %in% new_rows[[key]]),
    new_rows
  )
}

upsert_manifest <- function(existing, new_rows) {
  if (is.null(existing) || nrow(existing) == 0) return(new_rows)
  missing_existing <- setdiff(names(new_rows), names(existing))
  for (nm in missing_existing) existing[[nm]] <- NA
  missing_new <- setdiff(names(existing), names(new_rows))
  for (nm in missing_new) new_rows[[nm]] <- NA
  for (nm in intersect(names(existing), names(new_rows))) {
    if (!identical(class(existing[[nm]]), class(new_rows[[nm]]))) {
      existing[[nm]] <- as.character(existing[[nm]])
      new_rows[[nm]] <- as.character(new_rows[[nm]])
    }
  }
  new_rows <- new_rows[, names(existing)]
  bind_rows(
    existing |> filter(!.data$artifact_path %in% new_rows$artifact_path),
    new_rows
  )
}

row_review <- read_csv_required(row_review_path)
issue_review <- read_csv_required(issue_review_path)
raw_opportunity <- read_csv_required(raw_opportunity_path)
p12 <- read_csv_required(p12_path)

if (nrow(row_review) != 22L) stop("Expected 22 P12A-FR row-review rows.", call. = FALSE)
if (nrow(issue_review) != 19L) stop("Expected 19 P12A-FR issue groups.", call. = FALSE)
if (nrow(raw_opportunity) != 310L) stop("Expected 310 P12A raw price/no-YTM rows.", call. = FALSE)

class_counts <- table(row_review$review_class)
expected_counts <- c(
  standard_window_v12_static_yield_review_candidate = 8L,
  call_event_needs_yield_to_worst_rule = 4L,
  broader_hard_currency_feature_review = 1L,
  sensitivity_only_outside_2_15 = 8L,
  quarantine_wide_bid_ask_or_field_conflict = 1L
)
for (nm in names(expected_counts)) {
  if (!identical(as.integer(class_counts[[nm]]), expected_counts[[nm]])) {
    stop("P12A-FR review class drift: ", nm, call. = FALSE)
  }
}

issue_flags <- issue_review |>
  select(
    issue_review_key,
    issue_review_class,
    any_identifier_worst_event_conflict,
    identifier_count
  )

country_lookup <- p12 |>
  select(any_of(c("iso3", "country"))) |>
  distinct() |>
  rename(issuer_country_name = country)

raw_fields <- raw_opportunity |>
  select(
    terminal_identifier,
    face_outstanding_usd,
    latest_price_date,
    latest_mid_price,
    latest_bid_price,
    latest_ask_price,
    positive_outstanding,
    p2_status_gate_overlap,
    plain_vanilla_fixed_coupon,
    complex_instrument_flag
  )

row_decisions <- row_review |>
  left_join(issue_flags, by = "issue_review_key") |>
  left_join(raw_fields, by = "terminal_identifier") |>
  left_join(country_lookup, by = "issuer_country_name") |>
  mutate(
    within_rate_sanity_band = !is.na(.data$v12_pricing_mid_yield) &
      .data$v12_pricing_mid_yield >= 1 &
      .data$v12_pricing_mid_yield <= 30,
    standard_feature_rich_gate_pass = .data$review_class == "standard_window_v12_static_yield_review_candidate" &
      !.data$any_identifier_worst_event_conflict &
      .data$v12_worst_redem_event == "MAT" &
      !.data$p2_status_gate_overlap &
      .data$positive_outstanding &
      !is.na(.data$face_outstanding_usd) &
      .data$face_outstanding_usd > 0 &
      .data$within_rate_sanity_band,
    broader_hard_currency_gate_pass = .data$review_class == "broader_hard_currency_feature_review" &
      .data$v12_worst_redem_event == "MAT" &
      !.data$p2_status_gate_overlap &
      .data$positive_outstanding &
      !is.na(.data$face_outstanding_usd) &
      .data$face_outstanding_usd > 0 &
      .data$within_rate_sanity_band,
    sensitivity_feature_rich_gate_pass = .data$review_class == "sensitivity_only_outside_2_15" &
      .data$v12_worst_redem_event == "MAT" &
      !.data$p2_status_gate_overlap &
      .data$positive_outstanding &
      !is.na(.data$face_outstanding_usd) &
      .data$face_outstanding_usd > 0 &
      .data$within_rate_sanity_band &
      .data$v12_yield_fields_broadly_consistent,
    final_decision = case_when(
      .data$standard_feature_rich_gate_pass ~
        "accepted_limited_feature_rich_vendor_ytm_usd_standard",
      .data$broader_hard_currency_gate_pass ~
        "accepted_limited_feature_rich_vendor_ytm_broader_hard_currency",
      .data$sensitivity_feature_rich_gate_pass ~
        "accepted_sensitivity_feature_rich_vendor_ytm_outside_standard_window",
      .data$review_class == "call_event_needs_yield_to_worst_rule" ~
        "blocked_call_event_requires_yield_to_worst_rule",
      .data$any_identifier_worst_event_conflict ~
        "blocked_identifier_pair_worst_event_conflict",
      .data$review_class == "quarantine_wide_bid_ask_or_field_conflict" ~
        "blocked_wide_bid_ask_or_field_conflict",
      TRUE ~ "blocked_or_review_only_not_standard"
    ),
    accepted_scope = case_when(
      .data$final_decision == "accepted_limited_feature_rich_vendor_ytm_usd_standard" ~
        "full_ladder_labelled_secondary_feature_rich_vendor_yield_usd_2_15_standard",
      .data$final_decision == "accepted_limited_feature_rich_vendor_ytm_broader_hard_currency" ~
        "full_ladder_labelled_secondary_feature_rich_vendor_yield_usd_eur_2_15_broader_only",
      .data$final_decision == "accepted_sensitivity_feature_rich_vendor_ytm_outside_standard_window" ~
        "full_ladder_sensitivity_only_outside_2_15",
      TRUE ~ "not_accepted"
    ),
    central_rate_pct = case_when(
      str_starts(.data$final_decision, "accepted") ~ .data$v12_pricing_mid_yield,
      TRUE ~ NA_real_
    ),
    lower_yield_bound_pct = case_when(
      str_starts(.data$final_decision, "accepted") ~ pmin(.data$v12_bid_yield, .data$v12_ask_yield, na.rm = TRUE),
      TRUE ~ NA_real_
    ),
    upper_yield_bound_pct = case_when(
      str_starts(.data$final_decision, "accepted") ~ pmax(.data$v12_bid_yield, .data$v12_ask_yield, na.rm = TRUE),
      TRUE ~ NA_real_
    ),
    reliability_label = case_when(
      .data$final_decision == "accepted_limited_feature_rich_vendor_ytm_usd_standard" ~
        "observed_secondary_feature_rich_vendor_yield_limited",
      .data$final_decision == "accepted_limited_feature_rich_vendor_ytm_broader_hard_currency" ~
        "observed_secondary_feature_rich_vendor_yield_broader_hard_currency",
      .data$final_decision == "accepted_sensitivity_feature_rich_vendor_ytm_outside_standard_window" ~
        "observed_secondary_feature_rich_vendor_yield_sensitivity_only",
      TRUE ~ "not_usable_without_additional_rule_or_review"
    ),
    canonical_implication = case_when(
      .data$final_decision == "accepted_limited_feature_rich_vendor_ytm_usd_standard" ~
        "may_enter_P12B_full_ladder_as_labelled_supplemental_feature_rich_secondary_tier_not_strict_P8",
      .data$final_decision == "accepted_limited_feature_rich_vendor_ytm_broader_hard_currency" ~
        "may_enter_P12B_full_ladder_as_broader_hard_currency_labelled_supplemental_feature_rich_tier_not_usd_standard",
      .data$final_decision == "accepted_sensitivity_feature_rich_vendor_ytm_outside_standard_window" ~
        "may_enter_P12B_full_ladder_as_sensitivity_only_not_standard_comparator",
      TRUE ~ "do_not_enter_standard_or_supplemental_feature_rich_tier_without_further_rule_or_review"
    ),
    final_decision_note = case_when(
      .data$final_decision == "accepted_limited_feature_rich_vendor_ytm_usd_standard" ~
        "Accepted only as a labelled feature-rich vendor-yield tier: USD, 2-15 years, positive outstanding, non-status, worst-redemption event MAT, close v12/v14 fields, and no paired-identifier conflict.",
      .data$final_decision == "accepted_limited_feature_rich_vendor_ytm_broader_hard_currency" ~
        "Accepted only as broader hard-currency feature-rich evidence because the row is EUR, not USD.",
      .data$final_decision == "accepted_sensitivity_feature_rich_vendor_ytm_outside_standard_window" ~
        "Accepted only as outside-window sensitivity evidence; it must not be read as the standard medium-term comparator.",
      .data$final_decision == "blocked_call_event_requires_yield_to_worst_rule" ~
        "Blocked because the vendor worst-redemption event is CALL and no accepted yield-to-worst/yield-to-call field rule exists for this feature-rich pass.",
      .data$final_decision == "blocked_identifier_pair_worst_event_conflict" ~
        "Blocked because paired identifiers for the issue disagree on the vendor worst-redemption event.",
      .data$final_decision == "blocked_wide_bid_ask_or_field_conflict" ~
        "Blocked because bid/ask or field disagreement is too wide for automatic use.",
      TRUE ~ "Not accepted under the final feature-rich rule."
    ),
    source_basis = paste(
      "local P12A-FR row review",
      "raw v14 price/outstanding opportunity surface",
      "older v12 cash-flow enrichment",
      "published bond-yield convention sources for callable/YTC/YTW treatment",
      sep = "; "
    ),
    noncanonical_flag = TRUE,
    diagnostic_only = .data$accepted_scope != "full_ladder_labelled_secondary_feature_rich_vendor_yield_usd_2_15_standard",
    last_updated = last_updated
  ) |>
  select(
    analysis_year,
    iso3,
    issuer_country_name,
    terminal_identifier,
    isin,
    currency,
    issue_review_key,
    document_title,
    issue_date,
    maturity_date,
    coupon_rate,
    coupon_frequency,
    latest_price_date,
    latest_mid_price,
    latest_bid_price,
    latest_ask_price,
    face_outstanding_usd,
    residual_maturity_years_at_quote,
    feature_structure_summary,
    v12_worst_redem_event,
    v12_worst_redem_date,
    v12_worst_redem_price,
    v12_worst_average_life,
    v12_bid_yield,
    v12_ask_yield,
    v12_pricing_mid_yield,
    v12_mean_yield,
    v12_yield_to_maturity_1,
    v12_yield_to_maturity_2,
    v14_yield_search_direct,
    v14_yield_price_implied,
    v12_bid_ask_yield_range_bps,
    v12_ytm_vs_v14_yield_search_abs_diff_pct,
    v12_pricing_mid_vs_v14_yield_search_abs_diff_pct,
    review_class,
    issue_review_class,
    any_identifier_worst_event_conflict,
    positive_outstanding,
    p2_status_gate_overlap,
    plain_vanilla_fixed_coupon,
    complex_instrument_flag,
    within_rate_sanity_band,
    final_decision,
    accepted_scope,
    central_rate_pct,
    lower_yield_bound_pct,
    upper_yield_bound_pct,
    reliability_label,
    canonical_implication,
    final_decision_note,
    source_basis,
    diagnostic_only,
    noncanonical_flag,
    last_updated
  ) |>
  arrange(.data$issuer_country_name, .data$maturity_date, .data$terminal_identifier)

if (sum(row_decisions$final_decision == "accepted_limited_feature_rich_vendor_ytm_usd_standard") != 7L) {
  stop("Expected 7 accepted USD-standard feature-rich identifier rows.", call. = FALSE)
}
if (sum(row_decisions$final_decision == "accepted_limited_feature_rich_vendor_ytm_broader_hard_currency") != 1L) {
  stop("Expected 1 accepted broader hard-currency feature-rich identifier row.", call. = FALSE)
}
if (sum(row_decisions$final_decision == "accepted_sensitivity_feature_rich_vendor_ytm_outside_standard_window") != 8L) {
  stop("Expected 8 accepted sensitivity-only feature-rich identifier rows.", call. = FALSE)
}

issue_layer <- row_decisions |>
  filter(.data$final_decision %in% c(
    "accepted_limited_feature_rich_vendor_ytm_usd_standard",
    "accepted_limited_feature_rich_vendor_ytm_broader_hard_currency",
    "accepted_sensitivity_feature_rich_vendor_ytm_outside_standard_window"
  )) |>
  group_by(.data$analysis_year, .data$iso3, .data$issuer_country_name, .data$issue_review_key) |>
  summarise(
    terminal_identifiers = collapse_unique(.data$terminal_identifier),
    isins = collapse_unique(.data$isin),
    currency = collapse_unique(.data$currency),
    maturity_date = min(.data$maturity_date, na.rm = TRUE),
    coupon_rate = median_or_na(.data$coupon_rate),
    residual_maturity_years_at_quote = median_or_na(.data$residual_maturity_years_at_quote),
    feature_structure_summary = collapse_unique(.data$feature_structure_summary),
    v12_worst_redem_events = collapse_unique(.data$v12_worst_redem_event),
    final_decision = collapse_unique(.data$final_decision),
    accepted_scope = collapse_unique(.data$accepted_scope),
    central_rate_pct = median_or_na(.data$central_rate_pct),
    lower_yield_bound_pct = median_or_na(.data$lower_yield_bound_pct),
    upper_yield_bound_pct = median_or_na(.data$upper_yield_bound_pct),
    face_outstanding_usd = max_or_na(.data$face_outstanding_usd),
    identifier_count = n(),
    reliability_label = collapse_unique(.data$reliability_label),
    source_basis = collapse_unique(.data$source_basis),
    .groups = "drop"
  ) |>
  mutate(
    issue_layer_id = paste0("P12A-FR-ISSUE-", sprintf("%03d", row_number())),
    noncanonical_flag = TRUE,
    diagnostic_only = .data$accepted_scope != "full_ladder_labelled_secondary_feature_rich_vendor_yield_usd_2_15_standard",
    last_updated = last_updated
  ) |>
  select(
    issue_layer_id,
    everything()
  ) |>
  arrange(.data$issuer_country_name, .data$maturity_date)

if (sum(issue_layer$accepted_scope == "full_ladder_labelled_secondary_feature_rich_vendor_yield_usd_2_15_standard") != 6L) {
  stop("Expected 6 accepted USD-standard feature-rich issue groups.", call. = FALSE)
}

country_layer <- issue_layer |>
  group_by(.data$analysis_year, .data$iso3, .data$issuer_country_name, .data$accepted_scope) |>
  summarise(
    feature_rich_country_rate_pct = weighted_mean_or_na(.data$central_rate_pct, .data$face_outstanding_usd),
    feature_rich_issue_count = n(),
    feature_rich_identifier_count = sum(.data$identifier_count, na.rm = TRUE),
    feature_rich_total_weight_usd = sum(.data$face_outstanding_usd, na.rm = TRUE),
    feature_rich_min_issue_rate_pct = min(.data$central_rate_pct, na.rm = TRUE),
    feature_rich_max_issue_rate_pct = max(.data$central_rate_pct, na.rm = TRUE),
    feature_rich_currency_basis = collapse_unique(.data$currency),
    feature_rich_issue_layer_ids = collapse_unique(.data$issue_layer_id),
    reliability_label = collapse_unique(.data$reliability_label),
    .groups = "drop"
  ) |>
  mutate(
    country_layer_id = paste0("P12A-FR-COUNTRY-", sprintf("%03d", row_number())),
    paper_facing_treatment = case_when(
      .data$accepted_scope == "full_ladder_labelled_secondary_feature_rich_vendor_yield_usd_2_15_standard" ~
        "labelled_standard_window_feature_rich_vendor_yield_tier",
      .data$accepted_scope == "full_ladder_labelled_secondary_feature_rich_vendor_yield_usd_eur_2_15_broader_only" ~
        "broader_hard_currency_feature_rich_vendor_yield_only",
      TRUE ~ "sensitivity_only_outside_standard_window"
    ),
    noncanonical_flag = TRUE,
    diagnostic_only = .data$paper_facing_treatment != "labelled_standard_window_feature_rich_vendor_yield_tier",
    last_updated = last_updated
  ) |>
  select(country_layer_id, everything()) |>
  arrange(.data$issuer_country_name, .data$accepted_scope)

source_basis <- tribble(
  ~source_id, ~source_name, ~source_type, ~source_url_or_path, ~source_locator, ~role_in_decision, ~source_limit,
  "P12A-FR2-SRC-001", "P12A-FR row review", "local_output", row_review_path, "22 row-level v12-matched feature-rich review rows", "Local row classification authority for the feature-rich subset.", "Generated review output, not an external convention source.",
  "P12A-FR2-SRC-002", "P12A raw price/no-YTM opportunity surface", "local_output", raw_opportunity_path, "310 raw v14 latest-price/no-direct-YTM identifier rows", "Provides prices, quote dates, outstanding amounts, status overlap, and complex-instrument flags.", "Does not by itself validate feature-rich yield semantics.",
  "P12A-FR2-SRC-003", "Investopedia: Yield to Maturity vs. Yield to Call", "external_reference", "https://www.investopedia.com/ask/answers/012615/what-difference-between-yield-maturity-and-yield-call.asp", "Yield-to-call and callable-bond overview", "Supports the distinction between maturity yield and call yield for callable bonds.", "Secondary explanatory source; not a terminal field definition.",
  "P12A-FR2-SRC-004", "Investopedia: Understanding Yield to Call", "external_reference", "https://www.investopedia.com/terms/y/yieldtocall.asp", "Yield-to-call formula and early-redemption discussion", "Supports blocking CALL worst-redemption rows until a yield-to-call/yield-to-worst rule is accepted.", "Secondary explanatory source; not sovereign-specific.",
  "P12A-FR2-SRC-005", "Investopedia: Callable Bonds", "external_reference", "https://www.investopedia.com/articles/bonds/07/callable_bonds.asp", "Callable bonds have two potential life spans", "Supports the point that callable bonds introduce cash-flow uncertainty relative to standard bonds.", "Secondary explanatory source; not an LSEG field manual.",
  "P12A-FR2-SRC-006", "Yield to maturity reference", "external_reference", "https://en.wikipedia.org/wiki/Yield_to_maturity", "Variants of yield to maturity including yield to call and yield to worst", "Cross-checks that yield-to-worst is the lower of possible maturity/call/put-style yields for feature-rich bonds.", "Tertiary reference; used only as a convention cross-check."
) |>
  mutate(
    package_id = "P12A-FR2",
    noncanonical_flag = TRUE,
    generated_by = generated_by,
    generated_at = generated_at,
    last_updated = last_updated
  )

memo <- c(
  "# P12A-FR2 Feature-Rich Secondary Final Decision",
  "",
  paste0("Generated: `", generated_at, "`"),
  "",
  "This package resolves the inclusion rule for the `22` feature-rich secondary rows matched to older v12 cash-flow enrichment.",
  "",
  "## Final Decision",
  "",
  "Previous project audits and rules are treated as context, not as binding authority. The decision here rests on the current row evidence and the cash-flow logic of callable, sinkable, and otherwise feature-rich bonds.",
  "",
  "Missing historical direct YTM is not by itself an exclusion. But for callable, sinkable, or otherwise feature-rich bonds, the project should not compute a simple plain-bond YTM from price, coupon, frequency, and maturity alone. That shortcut ignores the feature schedule and can use the wrong cash-flow assumption.",
  "",
  "The accepted rule is therefore a limited vendor-analytics rule:",
  "",
  "- Use a feature-rich vendor-yield row only when it is non-status-gated, has positive outstanding amount, is within the relevant maturity/currency window, has a v12 pricing mid-yield, has a vendor worst-redemption event of `MAT`, has internally close v12/v14 yield fields, has acceptable bid/ask yield range, and has no paired-identifier worst-event conflict.",
  "- The central rate is `v12_pricing_mid_yield`; bid/ask yields and the v14 yield-search field remain diagnostics.",
  "- Duplicate or paired identifiers are collapsed to the issue level before country aggregation. The issue weight is the maximum observed `face_outstanding_usd`, so paired 144A/Reg S identifiers are not double-counted.",
  "- CALL worst-redemption rows stay blocked until a yield-to-worst or yield-to-call field rule is explicitly validated.",
  "- EUR rows can be used only as broader hard-currency evidence with a visible currency-basis label.",
  "- Outside-window rows are sensitivity-only.",
  "",
  "## Counts",
  "",
  "- `7` identifier rows, collapsing to `6` issue groups across `3` countries, pass the USD `2-15` standard-window feature-rich vendor-yield rule.",
  "- `1` EUR row passes only the broader hard-currency feature-rich rule.",
  "- `8` rows pass only as outside-window sensitivity evidence.",
  "- `4` CALL-event rows remain blocked pending a yield-to-worst/yield-to-call rule.",
  "- `1` wide/conflict row remains blocked.",
  "- `1` otherwise clean Bahamas identifier is blocked because its paired identifier has a conflicting worst-redemption event.",
  "",
  "## Boundary",
  "",
  "This decision authorizes P12B to carry these rows as labelled full-ladder feature-rich secondary evidence. It does not add them to the strict P8 canonical PVR output, does not make them ordinary direct-YTM evidence, and does not mutate `output/tables/*`."
)

write_csv_na(row_decisions, path_out(output_files[["row_decisions"]]))
write_csv_na(issue_layer, path_out(output_files[["issue_layer"]]))
write_csv_na(country_layer, path_out(output_files[["country_layer"]]))
write_csv_na(source_basis, path_out(output_files[["source_basis"]]))
writeLines(memo, path_out(output_files[["decision_memo"]]))

for (file_name in output_files) {
  if (!file.exists(path_out(file_name))) stop("Output not written: ", file_name, call. = FALSE)
}
for (file_name in output_files[str_detect(output_files, "[.]csv$")]) {
  invisible(readr::read_csv(path_out(file_name), show_col_types = FALSE, guess_max = 100000))
}

existing_warnings <- read_csv_required(path_out("method_warnings.csv"))
new_warnings <- tribble(
  ~warning_id, ~analysis_year, ~iso3, ~country, ~source_table, ~source_row_id, ~warning_type, ~severity, ~warning_message, ~selection_implication, ~review_status, ~last_updated, ~package_id, ~warning_scope, ~affected_rows, ~source_file, ~resolution_status, ~accepted_as_methodology, ~diagnostic_only, ~noncanonical_flag,
  "P12A-FR2-WARN-01", 2024L, NA_character_, NA_character_, output_files[["row_decisions"]], NA_character_, "feature_rich_vendor_yield_not_direct_ytm", "medium", "Accepted feature-rich rows use a vendor pricing mid-yield from v12 enrichment, not a non-missing historical direct-YTM time-series field.", "Carry as a labelled feature-rich vendor-yield tier, not ordinary direct-YTM evidence.", "accepted_limited_with_label", last_updated, "P12A-FR2", "secondary_feature_rich_final_decision", 16L, row_review_path, "limited_rule_accepted", TRUE, TRUE, TRUE,
  "P12A-FR2-WARN-02", 2024L, NA_character_, NA_character_, output_files[["row_decisions"]], NA_character_, "call_event_rows_blocked_without_ytw_rule", "medium", "CALL worst-redemption rows remain blocked because yield-to-call/yield-to-worst field semantics are not validated for this pass.", "Do not include CALL rows in the accepted feature-rich tier.", "blocked", last_updated, "P12A-FR2", "secondary_feature_rich_final_decision", 4L, row_review_path, "blocked_pending_ytw_rule", TRUE, TRUE, TRUE
)
warnings_updated <- upsert_by_key(existing_warnings, new_warnings, "warning_id")
write_csv_na(warnings_updated, path_out("method_warnings.csv"))

existing_blockers <- read_csv_required(path_out("missing_data_and_blockers.csv"))
new_blocker <- tribble(
  ~blocker_id, ~analysis_year, ~iso3, ~country, ~component, ~blocker_type, ~blocker_description, ~needed_input, ~possible_source, ~decision_required, ~blocks_selection, ~blocks_diagnostic_computation, ~review_status, ~last_updated, ~package_id, ~blocker_scope, ~affected_rows, ~blocker_detail, ~required_resolution, ~accepted_as_methodology, ~diagnostic_only, ~noncanonical_flag,
  "P12A-FR2-BLOCK-01", 2024L, NA_character_, NA_character_, "secondary_feature_rich_yield", "yield_to_worst_rule_missing", "Four feature-rich rows have CALL as the vendor worst-redemption event. They are not accepted because the project has not validated a yield-to-worst or yield-to-call field rule for this subset.", "Validated yield-to-worst/yield-to-call field semantics and call schedule treatment.", "LSEG/Bloomberg field documentation, Data Item Browser/Formula Builder notes, and instrument call schedules.", TRUE, TRUE, FALSE, "blocked_pending_ytw_rule", last_updated, "P12A-FR2", "secondary_feature_rich_final_decision", 4L, "CALL rows are not data-absent, but the accepted rule is not available yet.", "Validate a YTW/YTC source field or keep blocked.", TRUE, TRUE, TRUE
)
blockers_updated <- upsert_by_key(existing_blockers, new_blocker, "blocker_id")
write_csv_na(blockers_updated, path_out("missing_data_and_blockers.csv"))

existing_tasks <- read_csv_required(path_out("task_register.csv"))
new_task <- tibble(
  work_package_id = "P12A-FR2",
  work_package = "Feature-rich secondary final inclusion decision",
  status = "done",
  priority = "high",
  rule_status = "accepted_limited_full_ladder_rule",
  source_inputs = paste(c(row_review_path, issue_review_path, raw_opportunity_path, p12_path), collapse = ";"),
  expected_outputs = paste(output_files, collapse = ";"),
  next_action = "Use accepted feature-rich country/issue layer as a labelled P12B full-ladder tier; keep CALL rows blocked.",
  owner_note = "Final decision accepts 7 USD standard-window identifiers as limited vendor-yield evidence and blocks CALL/conflict rows from standard use.",
  last_updated = last_updated,
  task_id = NA_character_,
  task_name = NA_character_,
  task_group = NA_character_,
  blocked_by = "P12A-FR2-BLOCK-01 for CALL rows only",
  output_files = paste(output_files, collapse = ";"),
  notes = "P12A-FR2 does not mutate output/tables/*.",
  noncanonical_flag = TRUE
)
tasks_updated <- upsert_by_key(existing_tasks, new_task, "work_package_id")
write_csv_na(tasks_updated, path_out("task_register.csv"))

existing_decisions <- read_csv_required(path_out("computational_decision_log.csv"))
new_decisions <- tibble(
  decision_id = c("P12A-FR2-COMP-01", "P12A-FR2-COMP-02", "P12A-FR2-COMP-03"),
  package_id = "P12A-FR2",
  decision_date = last_updated,
  decision_type = c("feature_rich_vendor_yield_rule", "call_event_blocker_rule", "duplicate_identifier_weighting_rule"),
  decision_summary = c(
    "Accepted a limited feature-rich vendor-yield rule for non-status USD 2-15 rows with MAT worst-redemption event, close v12/v14 fields, positive outstanding, rate sanity, and no paired-identifier conflict.",
    "Blocked CALL worst-redemption rows until a yield-to-worst or yield-to-call field rule is validated.",
    "Collapsed paired identifiers to issue level and used the maximum face-outstanding value as the issue weight to avoid double counting."
  ),
  affected_outputs = paste(output_files, collapse = ";"),
  source_inputs = paste(c(row_review_path, issue_review_path, raw_opportunity_path), collapse = ";"),
  rationale = c(
    "Missing direct YTM is not automatically disqualifying, but feature-rich bonds need a field-aware rule; vendor pricing mid-yield is acceptable only under strict gates and explicit labels.",
    "Callable bonds can have materially different yield-to-call/yield-to-worst behavior, so CALL rows should not use ordinary YTM without a validated convention.",
    "Many sovereign external issues have paired 144A/Reg S identifiers; issue-level weighting should not double-count identical economic exposure."
  ),
  noncanonical_flag = TRUE,
  accepted_as_methodology = TRUE,
  diagnostic_only = c(FALSE, TRUE, FALSE),
  generated_by = generated_by,
  generated_at = generated_at
)
decisions_updated <- upsert_by_key(existing_decisions, new_decisions, "decision_id")
write_csv_na(decisions_updated, path_out("computational_decision_log.csv"))

manifest_path <- path_out("reproducibility_manifest.csv")
existing_manifest <- read_csv_required(manifest_path)

manifest_files <- c(
  row_review_path,
  issue_review_path,
  raw_opportunity_path,
  p12_path,
  generated_by,
  path_out(output_files)
)

new_manifest <- tibble(
  artifact_path = manifest_files,
  artifact_type = case_when(
    artifact_path == generated_by ~ "script",
    str_detect(artifact_path, "[.]csv$") & str_detect(artifact_path, "/outputs/") ~ "experiment_output_csv",
    str_detect(artifact_path, "[.]md$") ~ "experiment_output_markdown",
    TRUE ~ "input_file"
  ),
  package_id = "P12A-FR2",
  description = case_when(
    artifact_path == row_review_path ~ "P12A-FR row-review input for feature-rich final decision.",
    artifact_path == issue_review_path ~ "P12A-FR issue-review input for feature-rich final decision.",
    artifact_path == raw_opportunity_path ~ "Raw v14 latest-price/no-direct-YTM opportunity surface used for outstanding, price, and status fields.",
    artifact_path == p12_path ~ "P12 wide table used for country ISO lookup.",
    artifact_path == generated_by ~ "P12A-FR2 final feature-rich secondary decision script.",
    TRUE ~ "P12A-FR2 generated output."
  ),
  source_path = artifact_path,
  source_name = basename(artifact_path),
  source_type = artifact_type,
  source_role = if_else(str_detect(artifact_path, "/outputs/p12a_feature_rich_secondary_final|/outputs/p12a_feature_rich_secondary_accepted|/outputs/p12a_feature_rich_secondary_country|/outputs/p12a_feature_rich_secondary_source|/outputs/p12a_feature_rich_secondary_final_decision_memo"), "generated_output", "input_or_script"),
  source_url = NA_character_,
  retrieval_or_generation_date = last_updated,
  row_count = purrr::map_int(artifact_path, ~ file_shape(.x)$rows[[1]]),
  column_count = purrr::map_int(artifact_path, ~ file_shape(.x)$columns[[1]]),
  sha256 = purrr::map_chr(artifact_path, sha256_file),
  noncanonical_flag = TRUE,
  generated_by = generated_by,
  generated_at = generated_at,
  notes = "P12A-FR2 final feature-rich secondary decision package.",
  last_updated = last_updated,
  schema_version = NA_character_
)
manifest_updated <- upsert_manifest(existing_manifest, new_manifest)
write_csv_na(manifest_updated, manifest_path)

cat("P12A-FR2 row decisions:", nrow(row_decisions), "\n")
cat("Accepted USD-standard identifiers:", sum(row_decisions$final_decision == "accepted_limited_feature_rich_vendor_ytm_usd_standard"), "\n")
cat("Accepted USD-standard issue groups:", sum(issue_layer$accepted_scope == "full_ladder_labelled_secondary_feature_rich_vendor_yield_usd_2_15_standard"), "\n")
cat("Country layer rows:", nrow(country_layer), "\n")
