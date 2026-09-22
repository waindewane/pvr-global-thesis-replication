#!/usr/bin/env Rscript

# Build the non-selecting evidence package for SEC-12 through SEC-18.

suppressPackageStartupMessages({
  library(data.table)
  library(digest)
  library(dplyr)
  library(stringr)
  library(tibble)
  library(tidyr)
})

source("R/p15_observed_all_years.R")
source("R/p15_secondary_decision_evidence.R")

root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
derived_dir <- file.path(
  root, "data-derived", "p15_secondary_decision_evidence_2012_2024_v1"
)
governance_dir <- file.path(root, "docs", "governance")
dir.create(derived_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(governance_dir, recursive = TRUE, showWarnings = FALSE)

p13_dir <- file.path(
  root,
  "experiments/full_ladder_ratings_integrated_status_rebuild_2026-06-01",
  "outputs"
)
paths <- list(
  identifier_evidence = file.path(
    root, "data-derived/p15_observed_markets_2012_2024_v1",
    "p15_secondary_identifier_year_evidence_2012_2024.csv.gz"
  ),
  secondary_issues = file.path(
    root, "data-derived/p15_observed_markets_2012_2024_v1",
    "p15_secondary_issue_year_evidence_2012_2024.csv.gz"
  ),
  primary_issues = file.path(
    root, "data-derived/p15_observed_markets_2012_2024_v1",
    "p15_primary_issue_evidence_2012_2024.csv.gz"
  ),
  secondary_history = file.path(
    root, "data-derived/p15_unified_lseg_source_2012_2024_v1",
    "p15_secondary_history_long_2012_2024.csv.gz"
  ),
  targeted_feature_validation = file.path(
    p13_dir, "p12a_feature_rich_secondary_field_validation_2024.csv"
  ),
  targeted_feature_review = file.path(
    p13_dir, "p12a_feature_rich_v12_matched_yield_review_2024.csv"
  )
)
missing <- names(paths)[!file.exists(unlist(paths))]
if (length(missing)) {
  stop("Missing secondary-decision inputs: ", paste(missing, collapse = ", "),
       call. = FALSE)
}

identifier_evidence <- fread(paths$identifier_evidence) |> as_tibble()
secondary_issues <- fread(paths$secondary_issues) |> as_tibble()
primary_issues <- fread(paths$primary_issues) |> as_tibble()
secondary_history <- fread(paths$secondary_history) |> as_tibble()
targeted_validation <- fread(paths$targeted_feature_validation) |> as_tibble()
targeted_review <- fread(paths$targeted_feature_review) |> as_tibble()

feature <- p15_build_secondary_feature_evidence(
  identifier_evidence, targeted_validation, targeted_review
)
feature_identifier_audit <- feature$identifier_audit
feature_issue_audit <- feature$issue_audit
callable_audit <- p15_build_secondary_callable_audit(feature_identifier_audit)

maturity_scenarios <- p15_build_secondary_maturity_sensitivity(
  secondary_issues
)
maturity_differences <- p15_compare_secondary_scenarios(
  maturity_scenarios, "usd_2_15_current_candidate", "maturity_window"
)

quote_scenarios <- p15_build_secondary_quote_sensitivity(
  secondary_history, identifier_evidence
)
quote_differences <- p15_compare_secondary_scenarios(
  quote_scenarios, "latest_available_current_common", "quote_window"
)

currency_overlap <- p15_build_currency_overlap_feasibility(
  primary_issues, secondary_issues
)

feature_summary <- feature_identifier_audit |>
  count(
    feature_types, feature_evidence_state,
    historical_lmic_reporting_scope, name = "identifier_rows"
  ) |>
  arrange(desc(identifier_rows), feature_types)
callable_summary <- callable_audit |>
  count(
    callable_yield_validation_state, historical_lmic_reporting_scope,
    name = "identifier_rows"
  ) |>
  arrange(desc(identifier_rows), callable_yield_validation_state)

scenario_summary <- function(scenarios, differences, dimension) {
  coverage <- scenarios |>
    group_by(scenario_id, scenario_role) |>
    summarise(
      country_years = n(),
      lmic_country_years = sum(historical_lmic_reporting_scope),
      median_rate_pct = median(market_rate_pct, na.rm = TRUE),
      median_maturity_years = median(market_maturity_years, na.rm = TRUE),
      .groups = "drop"
    )
  difference <- differences |>
    group_by(scenario_id) |>
    summarise(
      present_in_both = sum(coverage_state == "present_in_both"),
      scenario_only = sum(coverage_state == "scenario_only"),
      baseline_only = sum(coverage_state == "baseline_only"),
      median_absolute_rate_difference_pp = median(
        absolute_rate_difference_pp, na.rm = TRUE
      ),
      p95_absolute_rate_difference_pp = quantile(
        absolute_rate_difference_pp, 0.95, na.rm = TRUE, names = FALSE
      ),
      maximum_absolute_rate_difference_pp = max(
        absolute_rate_difference_pp, na.rm = TRUE
      ),
      .groups = "drop"
    )
  left_join(coverage, difference, by = "scenario_id") |>
    mutate(scenario_dimension = dimension, .before = 1)
}

maturity_summary <- scenario_summary(
  maturity_scenarios, maturity_differences, "maturity_window"
)
quote_summary <- scenario_summary(
  quote_scenarios, quote_differences, "quote_window"
)
currency_summary <- currency_overlap |>
  group_by(market) |>
  summarise(
    mixed_currency_country_years = n(),
    overlap_90d_3y_country_years = sum(pair_90d_3y),
    overlap_365d_5y_country_years = sum(pair_365d_5y),
    .groups = "drop"
  )

worst_case <- function(differences, n = 25L) {
  differences |>
    filter(coverage_state == "present_in_both") |>
    arrange(desc(absolute_rate_difference_pp)) |>
    slice_head(n = n)
}
maturity_worst_cases <- worst_case(maturity_differences)
quote_worst_cases <- worst_case(quote_differences)

swap_named_data_files <- list.files(
  root, recursive = TRUE, full.names = TRUE,
  pattern = "swap|cross.?currency|eur.?usd", ignore.case = TRUE
) |>
  normalizePath(winslash = "/", mustWork = FALSE)
swap_named_data_files <- swap_named_data_files[
  str_detect(swap_named_data_files, "/(data-raw|data-derived|sources)/") &
    str_detect(swap_named_data_files, "[.](csv|csv[.]gz|rds|parquet|xlsx)$")
]
currency_probe <- tibble(
  probe = c(
    "normalized_history_contains_swap_curve_fields",
    "swap_named_project_data_artifacts",
    "bounded_local_feasibility_result",
    "core_pipeline_dependency"
  ),
  value = c(
    as.character(any(str_detect(
      names(secondary_history), regex("swap|basis|zero.?curve", TRUE)
    ))),
    as.character(length(swap_named_data_files)),
    if (length(swap_named_data_files)) {
      "candidate_local_artifact_requires_semantic_review"
    } else {
      "stop_condition_reached_no_validated_local_swap_curve_input"
    },
    "no"
  ),
  interpretation = c(
    "The common LSEG history schema contains prices and direct YTM, not a cross-currency swap curve.",
    "Only actual data artifacts under data-raw, data-derived, or sources count; documentation mentions do not.",
    "Do not fabricate a normalized series. Preserve USD and EUR separately and keep the optional entitlement probe in TODO-005.",
    "The absent optional input does not block the USD-only or separate-EUR secondary standard."
  )
)

gate_register <- tibble::tribble(
  ~audit_id, ~implementation_state, ~decision_state, ~manager_consequence,
  "SEC-12", "evidence_complete", "awaits_SEC_14",
  "All feature-rich identifiers and issues now have an evidence state and block reason.",
  "SEC-13", "evidence_complete_blocked_where_unvalidated", "awaits_SEC_14",
  "No consistently defined explicit YTW/YTC field exists in the common export; CALL cases remain blocked.",
  "SEC-15", "evidence_complete", "awaits_SEC_18",
  "Four bounded maturity constructions are compared without choosing one.",
  "SEC-16", "bounded_local_probe_complete_stop_condition_reached", "deferred_optional",
  "No validated local swap-curve input exists; currency-specific results remain usable and the optional entitlement probe is preserved.",
  "SEC-17", "evidence_complete", "awaits_SEC_18",
  "Four quote timing constructions are compared and every candidate records recency.",
  "SEC-14", "not_implemented_research_decision", "owner_approval_required",
  "Choose which feature-rich subtypes are admissible, sensitivity-only, or blocked.",
  "SEC-18", "not_implemented_final_gate", "owner_approval_required",
  "Approve the full secondary standard after reviewing this package."
) |>
  mutate(
    selected_ladder_rows = 0L,
    build_id = p15_secondary_decision_build_id(),
    schema_version = p15_secondary_decision_schema_version()
  )

output_paths <- list(
  feature_identifier_audit = file.path(
    derived_dir, "p15_secondary_feature_identifier_audit_2012_2024.csv.gz"
  ),
  feature_issue_audit = file.path(
    derived_dir, "p15_secondary_feature_issue_audit_2012_2024.csv.gz"
  ),
  callable_audit = file.path(
    derived_dir, "p15_secondary_callable_yield_audit_2012_2024.csv.gz"
  ),
  maturity_scenarios = file.path(
    derived_dir, "p15_secondary_maturity_scenarios_2012_2024.csv.gz"
  ),
  maturity_differences = file.path(
    derived_dir, "p15_secondary_maturity_differences_2012_2024.csv.gz"
  ),
  quote_scenarios = file.path(
    derived_dir, "p15_secondary_quote_scenarios_2012_2024.csv.gz"
  ),
  quote_differences = file.path(
    derived_dir, "p15_secondary_quote_differences_2012_2024.csv.gz"
  ),
  currency_overlap = file.path(
    derived_dir, "p15_secondary_currency_overlap_feasibility_2012_2024.csv.gz"
  ),
  feature_summary = file.path(
    governance_dir, "p15_secondary_feature_evidence_summary.csv"
  ),
  callable_summary = file.path(
    governance_dir, "p15_secondary_callable_yield_summary.csv"
  ),
  maturity_summary = file.path(
    governance_dir, "p15_secondary_maturity_sensitivity_summary.csv"
  ),
  quote_summary = file.path(
    governance_dir, "p15_secondary_quote_sensitivity_summary.csv"
  ),
  currency_summary = file.path(
    governance_dir, "p15_secondary_currency_overlap_summary.csv"
  ),
  currency_probe = file.path(
    governance_dir, "p15_secondary_currency_normalization_local_probe.csv"
  ),
  maturity_worst_cases = file.path(
    governance_dir, "p15_secondary_maturity_sensitivity_worst_cases.csv"
  ),
  quote_worst_cases = file.path(
    governance_dir, "p15_secondary_quote_sensitivity_worst_cases.csv"
  ),
  gate_register = file.path(
    governance_dir, "p15_secondary_decision_gate_register.csv"
  ),
  manager_report = file.path(
    governance_dir, "P15_SECONDARY_DECISION_REVIEW_PACKET_2026-08-09.md"
  ),
  manifest = file.path(
    governance_dir, "p15_secondary_decision_evidence_manifest.csv"
  )
)

objects <- list(
  feature_identifier_audit = feature_identifier_audit,
  feature_issue_audit = feature_issue_audit,
  callable_audit = callable_audit,
  maturity_scenarios = maturity_scenarios,
  maturity_differences = maturity_differences,
  quote_scenarios = quote_scenarios,
  quote_differences = quote_differences,
  currency_overlap = currency_overlap,
  feature_summary = feature_summary,
  callable_summary = callable_summary,
  maturity_summary = maturity_summary,
  quote_summary = quote_summary,
  currency_summary = currency_summary,
  currency_probe = currency_probe,
  maturity_worst_cases = maturity_worst_cases,
  quote_worst_cases = quote_worst_cases,
  gate_register = gate_register
)
for (nm in names(objects)) {
  fwrite(as.data.table(objects[[nm]]), output_paths[[nm]], na = "")
}

value_for <- function(data, scenario, field) {
  value <- data[data$scenario_id == scenario, field, drop = TRUE]
  if (!length(value)) NA else value[[1]]
}
fmt <- function(x, digits = 2) {
  ifelse(is.finite(x), format(round(x, digits), trim = TRUE), "not available")
}

report <- c(
  "# P15 Secondary-Market Decision Review Packet",
  "",
  "Date: 2026-08-09",
  "",
  "Status: evidence complete for SEC-12, SEC-13, SEC-15, and SEC-17; no final secondary rule has been selected.",
  "",
  "## Manager conclusion",
  "",
  paste0(
    "The evidence batch audited ", format(nrow(feature_identifier_audit), big.mark = ","),
    " feature-rich identifier-years representing ",
    format(nrow(feature_issue_audit), big.mark = ","),
    " issue-years. Every row now has an evidence state or an explicit block reason."
  ),
  paste0(
    "The callable audit covers ", format(nrow(callable_audit), big.mark = ","),
    " identifier-years. The common export contains ordinary YTM but no consistently defined explicit YTW/YTC field or complete call schedules. The targeted 2024 companion helps classify a small subset, but does not remove the all-years limitation."
  ),
  "",
  "Nothing in this package changes selected rates. SEC-14 and SEC-18 remain owner decision gates.",
  "",
  "## Maturity-window evidence",
  "",
  paste0(
    "The current USD 2-15 candidate covers ",
    value_for(maturity_summary, "usd_2_15_current_candidate", "country_years"),
    " country-years (",
    value_for(maturity_summary, "usd_2_15_current_candidate", "lmic_country_years"),
    " LMIC). The inherited >=1-year sensitivity covers ",
    value_for(maturity_summary, "usd_ge1_inherited_sensitivity", "country_years"),
    " country-years. Among overlaps, its median absolute change is ",
    fmt(value_for(maturity_summary, "usd_ge1_inherited_sensitivity", "median_absolute_rate_difference_pp")),
    " percentage points and the 95th percentile is ",
    fmt(value_for(maturity_summary, "usd_ge1_inherited_sensitivity", "p95_absolute_rate_difference_pp")),
    "."
  ),
  "",
  "The 1-15 and >=2 diagnostics separately expose the effect of relaxing the lower and upper maturity boundaries; they are diagnostics, not proposed preferred rules.",
  "",
  "## Quote-window evidence",
  "",
  paste0(
  "The current latest-available construction covers ",
    value_for(quote_summary, "latest_available_current_common", "country_years"),
    " country-years. Requiring the -31/+7 window covers ",
    value_for(quote_summary, "preferred_31_pre_7_post", "country_years"),
    "; the pre-year-end-only and genuinely closest-to-31-December variants separate the no-lookahead question from calendar-date proximity. Rate differences and the 25 largest changes are saved separately."
  ),
  "",
  "## Currency feasibility",
  "",
  paste0(
    "The rebuilt overlap check finds ",
    currency_summary$overlap_90d_3y_country_years[currency_summary$market == "primary"],
    " primary and ",
    currency_summary$overlap_90d_3y_country_years[currency_summary$market == "secondary"],
    " secondary mixed-currency country-years with the documented close-pair tolerance."
  ),
  "",
  "The local source probe found no validated 2012-2024 EUR/USD cross-currency swap curve in the project inputs. Under the agreed stop condition, no approximate normalized series was fabricated. USD and EUR remain preserved separately; the optional LSEG entitlement probe remains in TODO-005 and is not a core dependency.",
  "",
  "## Decisions required next",
  "",
  "1. SEC-14: approve which feature-rich subtypes, if any, may be admitted, retained as sensitivity-only, or blocked.",
  "2. SEC-18: approve the complete secondary standard, including maturity, quote timing, repair rank, currency presentation, and display rules.",
  "",
  "Before those decisions, review the gate register, scenario summaries, and worst-case tables. The implementation deliberately offers no automatic promotion path."
)
writeLines(report, output_paths$manager_report, useBytes = TRUE)

stopifnot(
  nrow(feature_identifier_audit) > 0,
  nrow(feature_issue_audit) > 0,
  nrow(callable_audit) > 0,
  nrow(maturity_scenarios) > 0,
  nrow(quote_scenarios) > 0,
  nrow(currency_overlap) > 0,
  !any(feature_identifier_audit$selected_for_ladder),
  !any(callable_audit$selected_for_ladder),
  !any(maturity_scenarios$selected_for_ladder),
  !any(quote_scenarios$selected_for_ladder),
  !any(currency_overlap$selected_for_ladder),
  all(gate_register$selected_ladder_rows == 0L)
)

script_path <- file.path(
  root, "scripts/p15/build_p15_secondary_decision_evidence.R"
)
function_paths <- c(
  file.path(root, "R/p15_observed_all_years.R"),
  file.path(root, "R/p15_secondary_decision_evidence.R")
)
input_paths <- c(unname(unlist(paths)), function_paths)
generated_paths <- unname(unlist(output_paths[names(output_paths) != "manifest"]))
manifest <- rbindlist(lapply(c(input_paths, generated_paths), function(path) {
  data.table(
    artifact_path = sub(paste0("^", root, "/"), "", path),
    artifact_role = if (path %in% input_paths) {
      if (path %in% function_paths) "processor_code" else "parent_input"
    } else {
      "generated_output"
    },
    bytes = file.info(path)$size,
    sha256 = digest(file = path, algo = "sha256"),
    producing_script = sub(paste0("^", root, "/"), "", script_path),
    producing_script_sha256 = digest(file = script_path, algo = "sha256"),
    build_id = p15_secondary_decision_build_id(),
    schema_version = p15_secondary_decision_schema_version(),
    build_date = "2026-08-09"
  )
}), use.names = TRUE, fill = TRUE)
fwrite(manifest, output_paths$manifest, na = "")

cat("P15 secondary decision evidence: PASS\n")
cat("Feature-rich identifier-years:", nrow(feature_identifier_audit), "\n")
cat("Feature-rich issue-years:", nrow(feature_issue_audit), "\n")
cat("Callable identifier-years:", nrow(callable_audit), "\n")
cat("Maturity scenario rows:", nrow(maturity_scenarios), "\n")
cat("Quote scenario rows:", nrow(quote_scenarios), "\n")
cat("Selected ladder rows: 0\n")
