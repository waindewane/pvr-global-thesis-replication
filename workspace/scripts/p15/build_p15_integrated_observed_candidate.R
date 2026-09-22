#!/usr/bin/env Rscript

# Build the terminal-independent integrated observed-market candidate.
# The output is suitable for common-anchor validation but remains unselected,
# noncanonical, and explicitly prior to final SEC-14/SEC-18 closure.

suppressPackageStartupMessages({
  library(data.table)
  library(digest)
  library(dplyr)
  library(tibble)
})

source("R/p15_observed_all_years.R")
source("R/p15_status_sanity_candidate_rules.R")
source("R/p15_status_sanity_approved_rules.R")
source("R/p15_integrated_observed_candidate.R")

root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
output_dir <- file.path(
  root, "data-derived", "p15_integrated_observed_candidate_2012_2024_v1"
)
governance_dir <- file.path(root, "docs", "governance")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(governance_dir, recursive = TRUE, showWarnings = FALSE)

paths <- list(
  approved_issue_sanity = file.path(
    root, "data-derived/p15_status_sanity_approved_2012_2024_v1",
    "p15_approved_issue_rate_sanity_2012_2024.csv.gz"
  ),
  approved_country_actions = file.path(
    root, "data-derived/p15_status_sanity_approved_2012_2024_v1",
    "p15_approved_status_sanity_country_actions_2012_2024.csv.gz"
  ),
  approved_status_contract = file.path(
    root, "data-derived/p15_status_sanity_approved_2012_2024_v1",
    "p15_approved_status_rule_contract_2012_2024.csv.gz"
  ),
  repair_issue_audit = file.path(
    root, "data-derived/p15_secondary_repair_candidate_2012_2024_v1",
    "p15_secondary_repair_issue_audit_2012_2024.csv.gz"
  ),
  repair_country_candidates = file.path(
    root, "data-derived/p15_secondary_repair_candidate_2012_2024_v1",
    "p15_secondary_repair_country_candidates_2012_2024.csv.gz"
  ),
  common_secondary_issue_catalogue = file.path(
    root, "data-derived/p15_secondary_repair_candidate_2012_2024_v1",
    "p15_secondary_issue_disposition_catalogue_2012_2024.csv.gz"
  ),
  country_year_grid = file.path(
    root, "data-derived/p15_platform_2012_2024_v1",
    "p15_country_year_grid_2012_2024.csv"
  ),
  targeted_standard_direct = file.path(
    root, "data-derived/p15_observed_market_2024_v1",
    "p15_p13_secondary_standard_direct_country_2024.csv"
  ),
  targeted_terminal_direct = file.path(
    root, "data-derived/p15_observed_market_2024_v1",
    "p15_p13_secondary_terminal_direct_country_2024.csv"
  ),
  targeted_price_derived = file.path(
    root, "data-derived/p15_observed_market_2024_v1",
    "p15_p13_secondary_price_to_yield_country_2024.csv"
  ),
  p13_secondary_selected = file.path(
    root, "data-derived/p15_observed_market_2024_v1",
    "p15_p13_secondary_selected_parity_2024.csv"
  ),
  p13_primary_issue_audit = file.path(
    root, "data-derived/p15_observed_market_2024_v1",
    "p15_p13_primary_issue_audit_2024.csv.gz"
  ),
  p13_observed_parity = file.path(
    root, "data-derived/p15_observed_market_2024_v1",
    "p15_p13_observed_market_selected_parity_2024.csv"
  ),
  dgs5 = file.path(
    root, "data-raw/fred_treasury_tenor_sensitivity_2026-07-21",
    "fred_dgs5_2012_2024_downloaded_2026-07-21.csv"
  ),
  dgs7 = file.path(
    root, "data-raw/fred_treasury_tenor_sensitivity_2026-07-21",
    "fred_dgs7_2012_2024_downloaded_2026-07-21.csv"
  ),
  dgs10 = file.path(
    root, "data-raw/fred_treasury_tenor_sensitivity_2026-07-21",
    "fred_dgs10_2012_2024_downloaded_2026-07-21.csv"
  )
)
missing <- names(paths)[!file.exists(unlist(paths))]
if (length(missing)) {
  stop(
    "Missing integrated observed-candidate inputs: ",
    paste(missing, collapse = ", "), call. = FALSE
  )
}

approved_issue_sanity <- fread(paths$approved_issue_sanity) |>
  as_tibble() |>
  mutate(
    rate_date = as.Date(.data$rate_date),
    risk_free_observation_date = as.Date(.data$risk_free_observation_date)
  )
approved_actions <- fread(paths$approved_country_actions) |> as_tibble()
status_contract <- fread(paths$approved_status_contract) |> as_tibble()
repair_issue_audit <- fread(paths$repair_issue_audit) |> as_tibble()
repair_country_candidates <- fread(paths$repair_country_candidates) |>
  as_tibble()
common_secondary_issue_catalogue <- fread(
  paths$common_secondary_issue_catalogue
) |> as_tibble()
country_year_grid <- fread(paths$country_year_grid) |> as_tibble()
targeted_standard_direct <- fread(paths$targeted_standard_direct) |> as_tibble()
targeted_terminal_direct <- fread(paths$targeted_terminal_direct) |> as_tibble()
targeted_price_derived <- fread(paths$targeted_price_derived) |> as_tibble()
p13_secondary_selected <- fread(paths$p13_secondary_selected) |> as_tibble()
p13_primary_issue_audit <- fread(paths$p13_primary_issue_audit) |> as_tibble()
primary_currency_scope <- p13_primary_issue_audit |>
  filter(.data$selected_primary_issue) |>
  group_by(.data$analysis_year, .data$iso3) |>
  summarise(
    p13_primary_currency_basis = paste(
      sort(unique(.data$currency)), collapse = ";"
    ),
    p13_primary_mixed_currency_basis = dplyr::n_distinct(.data$currency) > 1L,
    .groups = "drop"
  )
feature_country_diagnostic <- common_secondary_issue_catalogue |>
  filter(.data$analysis_year == 2024L) |>
  group_by(.data$analysis_year, .data$iso3) |>
  summarise(
    common_feature_evidence_present = any(
      .data$nonstandard_feature_flag &
        (.data$direct_yield_present | .data$identifiers_with_price > 0)
    ),
    .groups = "drop"
  )
p13_parity <- fread(paths$p13_observed_parity) |>
  as_tibble() |>
  left_join(
    p13_secondary_selected |>
      select(
        analysis_year, iso3,
        p13_currency_basis = currency_basis
      ),
    by = c("analysis_year", "iso3")
  ) |>
  left_join(
    primary_currency_scope,
    by = c("analysis_year", "iso3")
  ) |>
  left_join(
    feature_country_diagnostic,
    by = c("analysis_year", "iso3")
  ) |>
  mutate(
    p13_currency_basis = if_else(
      .data$observed_market_branch == "observed_primary",
      .data$p13_primary_currency_basis,
      .data$p13_currency_basis
    ),
    p13_mixed_currency_basis = if_else(
      .data$observed_market_branch == "observed_primary",
      dplyr::coalesce(.data$p13_primary_mixed_currency_basis, FALSE),
      grepl(";", .data$p13_currency_basis)
    ),
    common_feature_evidence_present = dplyr::coalesce(
      .data$common_feature_evidence_present, FALSE
    )
  )
treasury <- p15_bind_fred_treasury_curve(
  paths$dgs5, paths$dgs7, paths$dgs10
)

repair_issue_sanity <- p15_build_repair_issue_sanity(
  repair_issue_audit, treasury
)
repair_imf_context <- p15_build_imf_inspired_context_flags(
  repair_issue_sanity
)
repair_actions <- p15_integrate_approved_status_sanity_actions(
  repair_issue_sanity, status_contract, repair_imf_context
)

all_issue_sanity <- bind_rows(approved_issue_sanity, repair_issue_sanity)
all_actions <- bind_rows(approved_actions, repair_actions)
evidence <- p15_normalize_integrated_observed_evidence(
  all_issue_sanity, all_actions
)
targeted_companion <- p15_build_targeted_secondary_companion_evidence(
  targeted_standard_direct, targeted_terminal_direct,
  targeted_price_derived, common_secondary_issue_catalogue,
  country_year_grid, status_contract
)
evidence <- bind_rows(evidence, targeted_companion) |>
  arrange(
    .data$analysis_year, .data$iso3, .data$observed_market_branch,
    .data$currency, .data$evidence_priority_within_branch_currency
  )
anchors <- p15_resolve_terminal_independent_validation_anchors(evidence)
regression_2024 <- p15_build_integrated_2024_regression(
  anchors, p13_parity
)
summary <- p15_summarise_integrated_observed(
  evidence, anchors, regression_2024
)

repair_source_regression <- evidence |>
  filter(.data$candidate_evidence_tier == "price_derived_secondary") |>
  transmute(
    analysis_year, iso3, currency,
    integrated_rate_pct = market_rate_pct,
    integrated_maturity_years = market_maturity_years,
    integrated_issue_count = retained_quantitative_issue_count,
    integrated_total_weight_usd = total_weight_usd
  ) |>
  full_join(
    repair_country_candidates |>
      transmute(
        analysis_year, iso3, currency,
        source_rate_pct = market_rate_pct,
        source_maturity_years = market_maturity_years,
        source_issue_count = issue_count,
        source_total_weight_usd = total_weight_usd
      ),
    by = c("analysis_year", "iso3", "currency")
  ) |>
  mutate(
    integrated_candidate_present = is.finite(.data$integrated_rate_pct),
    source_candidate_present = is.finite(.data$source_rate_pct),
    rate_abs_diff_pp = abs(
      .data$integrated_rate_pct - .data$source_rate_pct
    ),
    maturity_abs_diff_years = abs(
      .data$integrated_maturity_years - .data$source_maturity_years
    ),
    exact_source_regression_pass =
      .data$integrated_candidate_present & .data$source_candidate_present &
      .data$rate_abs_diff_pp <= 1e-10 &
      .data$maturity_abs_diff_years <= 1e-10 &
      .data$integrated_issue_count == .data$source_issue_count &
      abs(.data$integrated_total_weight_usd -
            .data$source_total_weight_usd) <= 1e-4,
    schema_version = p15_integrated_observed_schema_version(),
    build_id = p15_integrated_observed_build_id()
  ) |>
  arrange(.data$analysis_year, .data$iso3, .data$currency)

coverage_summary <- evidence |>
  count(
    .data$observed_market_branch, .data$currency,
    .data$candidate_evidence_tier,
    .data$historical_lmic_reporting_scope,
    .data$candidate_use_state,
    name = "country_year_rows"
  ) |>
  mutate(
    schema_version = p15_integrated_observed_schema_version(),
    build_id = p15_integrated_observed_build_id()
  ) |>
  arrange(
    .data$observed_market_branch, .data$currency,
    .data$candidate_evidence_tier,
    desc(.data$historical_lmic_reporting_scope), .data$candidate_use_state
  )

method_register <- tribble(
  ~method_component, ~approved_or_supported_rule, ~implementation_state,
  ~selection_effect, ~remaining_boundary,
  "primary_core",
  "Direct original issuance yield; at least one-year maturity; USD 50m preferred materiality; separate USD/EUR; labelled fixed callable/sinkable subtype.",
  "approved_integrated", "none",
  "No primary-versus-secondary ladder hierarchy is selected.",
  "secondary_direct_core",
  "Direct YTM; 2-15 years residual maturity; separate USD/EUR; year-end object.",
  "supported_terminal_independent_candidate", "none",
  "Full-year timing remains TODO-038; final secondary package remains SEC-18.",
  "secondary_repair_fallback",
  "Strict price-derived secondary evidence ranks below direct YTM and is used only where no direct branch/currency candidate exists.",
  "approved_lower_rank_integrated_candidate", "none",
  "Final secondary package remains SEC-18.",
  "status_and_rate_sanity",
  "Raw evidence retained; technical quarantine only in derived aggregates; low/high/IMF-inspired fields contextual; approved status contract applied.",
  "approved_integrated", "none",
  "Downstream ladder and PVR consequences remain unimplemented.",
  "p13_2024_regression",
  "Immutable P13 parity must continue to pass; forward-method differences are reported rather than forced to match.",
  "implemented", "none",
  "Forward differences require classification before any promotion."
) |>
  mutate(
    method_id = p15_integrated_observed_method_id(),
    schema_version = p15_integrated_observed_schema_version(),
    build_id = p15_integrated_observed_build_id()
  )

stopifnot(
  nrow(evidence) > 0L,
  nrow(anchors) > 0L,
  nrow(regression_2024) == 50L,
  all(regression_2024$legacy_p13_parity_still_passes),
  all(repair_source_regression$exact_source_regression_pass),
  all(evidence$raw_evidence_retained),
  all(!evidence$selected_for_ladder),
  all(!anchors$selected_for_ladder),
  all(!evidence$canonical_benchmark),
  all(!anchors$canonical_benchmark),
  all(evidence$currency %in% c("USD", "EUR")),
  !anyDuplicated(anchors[c(
    "analysis_year", "iso3", "observed_market_branch", "currency"
  )])
)

output_paths <- list(
  evidence = file.path(
    output_dir,
    "p15_integrated_observed_evidence_catalogue_2012_2024.csv.gz"
  ),
  anchors = file.path(
    output_dir,
    "p15_integrated_observed_validation_anchors_2012_2024.csv.gz"
  ),
  repair_regression = file.path(
    output_dir,
    "p15_integrated_repair_source_regression_2012_2024.csv.gz"
  ),
  regression_2024 = file.path(
    output_dir, "p15_integrated_observed_p13_regression_2024.csv"
  ),
  coverage_summary = file.path(
    governance_dir, "p15_integrated_observed_coverage_summary.csv"
  ),
  build_summary = file.path(
    governance_dir, "p15_integrated_observed_build_summary.csv"
  ),
  method_register = file.path(
    governance_dir, "p15_integrated_observed_method_register.csv"
  )
)
fwrite(as.data.table(evidence), output_paths$evidence, na = "")
fwrite(as.data.table(anchors), output_paths$anchors, na = "")
fwrite(
  as.data.table(repair_source_regression),
  output_paths$repair_regression, na = ""
)
fwrite(as.data.table(regression_2024), output_paths$regression_2024, na = "")
fwrite(as.data.table(coverage_summary), output_paths$coverage_summary, na = "")
fwrite(as.data.table(summary), output_paths$build_summary, na = "")
fwrite(as.data.table(method_register), output_paths$method_register, na = "")

manifest <- data.table(
  artifact_path = sub(
    paste0("^", root, "/"), "", c(unlist(paths), unlist(output_paths))
  ),
  artifact_role = c(
    rep("source_input", length(paths)),
    rep("generated_output", length(output_paths))
  )
)
manifest[, `:=`(
  bytes = file.info(file.path(root, artifact_path))$size,
  sha256 = vapply(
    file.path(root, artifact_path), digest, character(1),
    algo = "sha256", file = TRUE
  ),
  producing_script =
    "scripts/p15/build_p15_integrated_observed_candidate.R",
  build_id = p15_integrated_observed_build_id(),
  schema_version = p15_integrated_observed_schema_version(),
  build_date = "2026-08-17"
)]
fwrite(
  manifest,
  file.path(governance_dir, "p15_integrated_observed_manifest.csv"),
  na = ""
)

cat("Built integrated terminal-independent P15 observed candidate.\n")
print(as.data.table(summary))
cat("\n2024 forward regression classes:\n")
print(as.data.table(regression_2024)[, .N, by = forward_regression_class])
