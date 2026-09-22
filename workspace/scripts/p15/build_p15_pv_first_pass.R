source("R/p15_current_inputs.R")
#!/usr/bin/env Rscript
# Offline, bounded diagnostic. No candidate benchmark or legacy output is changed.
suppressPackageStartupMessages({library(dplyr); library(tidyr); library(purrr); library(data.table)})
source("R/ids.R"); source("R/pvr.R"); source("R/p15_raw_foundations.R"); source("R/p15_pv_first_pass.R")
args <- commandArgs(TRUE)
out <- if (length(args)) args[[1]] else file.path("data-derived",paste0("p15_pv_first_",format(Sys.time(),"%Y%m%d_%H%M%S")))
if (dir.exists(out) && length(list.files(out))) stop("Use a fresh output directory")
dir.create(out, recursive = TRUE, showWarnings = FALSE)
write_csv <- function(x, name) fwrite(x, file.path(out, paste0(name, ".csv")), na = "")
base <- Sys.getenv("P15_ANALYSIS_BASE", p15_current_input("dataset"))
history <- "experiments/full_ladder_database_all_years_2026-05-20/data/ids_core_all_years"
mapping_path <- file.path(Sys.getenv("P15_DAC_BASE", p15_current_input("dac")),"map.csv")
selected <- as_tibble(fread(file.path(base, "selected_reference.csv")))
tiers <- as_tibble(fread(file.path(base, "tier_eligibility.csv")))
mapping <- as_tibble(fread(mapping_path)) |>
  select(iso3, analysis_year, group_rate_pct, dac_eligible, dac_group, regime, source_snapshot_id)
stopifnot(!anyDuplicated(selected[c("iso3","analysis_year")]),
  !anyDuplicated(mapping[c("iso3","analysis_year")]),
  !anyDuplicated(tiers[c("iso3","analysis_year","tier")]))
meta <- read_counterpart_areas("data-raw/ids_counterpart_area.json")
creditors <- tibble(creditor_id=c("730","901","905"), creditor=c("China","IBRD","IDA"))
raw <- map_dfr(creditors$creditor_id, ~p15_read_ids_cached_pages(history, .x))
write_csv(raw |> filter(year %in% 2012:2024), "historical_raw_terms_long")
terms <- raw |> filter(year %in% 2012:2024) |>
  mutate(term=term_label(indicator_id)) |>
  select(iso3, analysis_year=year, creditor_id, term, value) |>
  pivot_wider(names_from=term,values_from=value) |> mutate(source_row_present=TRUE)
inventory <- crossing(selected, creditors) |>
  left_join(terms, by=c("iso3","analysis_year","creditor_id")) |>
  left_join(mapping, by=c("iso3","analysis_year")) |>
  mutate(term_state=p15_term_reason(official_rate,official_maturity_years,official_grace_years),
    source_row_present=coalesce(source_row_present,FALSE),
    term_state=if_else(!source_row_present,"no_country_record_in_term_archive",term_state),
    term_usable=term_state=="usable_stylized_terms",
    secondary_purpose_hold=(iso3=="LBN" & analysis_year %in% 2020:2023) |
      (iso3=="BLR" & analysis_year %in% 2022:2024) | (iso3=="RUS" & analysis_year==2022),
    selected_purpose_hold=secondary_purpose_hold & selected_tier=="secondary",
    selected_rate_available=is.finite(selected_rate_pct) & selected_rate_pct > -100,
    computationally_available=term_usable & selected_rate_available,
    ordinary_scenario_available=computationally_available & !selected_purpose_hold,
    modern_dac_comparison=ordinary_scenario_available & analysis_year>=2018 &
      coalesce(dac_eligible,FALSE) & is.finite(group_rate_pct),
    primary_or_ids=ordinary_scenario_available & selected_tier %in% c("primary","ids"),
    non_peer=ordinary_scenario_available & selected_tier!="peer",
    inventory_reason=case_when(!term_usable~term_state,
      !selected_rate_available~"no_eligible_selected_benchmark",
      selected_purpose_hold~"selected_secondary_restricted_for_ordinary_cost_use",
      TRUE~"usable_conditional_scenario"))
write_csv(inventory, "country_year_creditor_inventory")
summary_fun <- function(x) x |> summarise(grid_rows=n(),terms_usable=sum(term_usable),
  selected_rate_and_terms=sum(computationally_available), ordinary_scenario=sum(ordinary_scenario_available),
  primary_or_ids=sum(primary_or_ids), non_peer=sum(non_peer),
  modern_dac_comparison=sum(modern_dac_comparison),.groups="drop")
scope_rows <- bind_rows(inventory |> mutate(scope="all_project_countries"),
  inventory |> filter(historical_lmic_reporting_scope) |> mutate(scope="historical_WB_LMIC"))
write_csv(scope_rows |> group_by(scope,creditor) |> summary_fun(), "coverage_by_creditor")
write_csv(scope_rows |> group_by(scope,analysis_year,creditor) |> summary_fun(), "coverage_by_year_creditor")
write_csv(scope_rows |> count(scope,creditor,inventory_reason), "unavailability_reasons")
write_csv(scope_rows |> filter(ordinary_scenario_available) |>
  count(scope,creditor,selected_tier), "coverage_by_selected_tier")
write_csv(scope_rows |> filter(modern_dac_comparison) |>
  count(scope,creditor,selected_tier), "modern_dac_coverage_by_selected_tier")

# A distinct inventory for every available tier; overlap across tiers is intentional.
cross <- inventory |> select(iso3,analysis_year,creditor,historical_lmic_reporting_scope,
  term_usable,secondary_purpose_hold) |>
  left_join(tiers |> select(iso3,analysis_year,tier,rate_pct,eligible,exclusion_reason),
    by=c("iso3","analysis_year"),relationship="many-to-many") |>
  mutate(usable=term_usable & eligible & is.finite(rate_pct) & rate_pct > -100 &
    !(tier=="secondary" & secondary_purpose_hold))
write_csv(cross, "all_tier_overlap")
write_csv(cross |> filter(historical_lmic_reporting_scope) |> group_by(creditor,tier) |>
  summarise(usable=sum(usable),.groups="drop"), "lmic_all_tier_coverage")

# Wide 2024 archive: enumerate categories, not an assertion they are all official,
# independent creditors. The known core is never overwritten by this snapshot.
wide_paths <- list.files("data-raw/ids_terms_all_lenders",pattern="\\.json$",full.names=TRUE)
wide_raw <- map_dfr(wide_paths,p15_parse_term_snapshot)
stopifnot(all(wide_raw$year==2024),!anyDuplicated(wide_raw[c("iso3","year","creditor_id","indicator_id")]))
wide <- wide_raw |> mutate(term=term_label(indicator_id)) |>
  select(iso3,analysis_year=year,creditor_id,term,value) |>
  pivot_wider(names_from=term,values_from=value) |>
  inner_join(selected |> filter(analysis_year==2024) |>
    select(iso3,analysis_year,historical_lmic_reporting_scope,selected_rate_pct,selected_tier),
    by=c("iso3","analysis_year")) |>
  mutate(term_state=p15_term_reason(official_rate,official_maturity_years,official_grace_years),
    term_usable=term_state=="usable_stylized_terms")
write_csv(wide,"wider_2024_term_inventory")
write_csv(wide |> filter(historical_lmic_reporting_scope) |> group_by(creditor_id) |>
  summarise(rows=n(),usable_terms=sum(term_usable),
    usable_with_selected_rate=sum(term_usable & is.finite(selected_rate_pct)),.groups="drop") |>
  left_join(meta |> select(creditor_id,creditor_name),by="creditor_id") |>
  arrange(desc(usable_terms),creditor_id), "wider_2024_creditor_coverage")
snapshot_comparison <- inventory |> filter(analysis_year==2024,source_row_present) |>
  select(iso3,analysis_year,creditor_id,official_rate,official_maturity_years,official_grace_years) |>
  inner_join(wide |> select(iso3,analysis_year,creditor_id,official_rate,official_maturity_years,official_grace_years),
    by=c("iso3","analysis_year","creditor_id"),suffix=c("_history","_2024"))
write_csv(snapshot_comparison,"core_snapshot_comparison")

# Deterministic illustrative selection before PV computation: for each creditor,
# pick the lower and upper median maturity of the 2024 primary/IDS-supported pool.
example_pool <- inventory |> filter(historical_lmic_reporting_scope, modern_dac_comparison,
  analysis_year==2024,selected_tier %in% c("primary","ids")) |>
  arrange(creditor,official_maturity_years,iso3) |> group_by(creditor) |>
  mutate(position=row_number(),pool_n=n()) |>
  filter(position %in% unique(pmax(1,round(c(.25,.75)*n())))) |> ungroup() |>
  mutate(example_id=paste(iso3,analysis_year,creditor,sep="_"))
stopifnot(nrow(example_pool)==6L)
write_csv(example_pool,"example_inputs")
flow_rows <- list(); valuation_rows <- list(); test_rows <- list()
for (i in seq_len(nrow(example_pool))) {
  a <- example_pool[i,]
  f <- p15_pv_schedule(a$official_rate,a$official_maturity_years,a$official_grace_years)
  f$example_id <- a$example_id
  flow_rows[[i]] <- f
  rates <- tiers |> filter(iso3==a$iso3,analysis_year==a$analysis_year,eligible,is.finite(rate_pct)) |>
    transmute(rate_source=tier,discount_rate_pct=rate_pct)
  rates <- bind_rows(tibble(rate_source="DAC_common_scenario",discount_rate_pct=a$group_rate_pct),rates)
  rates$pv_per_100 <- vapply(rates$discount_rate_pct,function(r)p15_stream_pv(f,r),numeric(1))
  rates$legacy_ceiling_date_pv <- vapply(rates$discount_rate_pct,function(r)present_value(f,r),numeric(1))
  rates <- rates |> mutate(example_id=a$example_id,
    grant_equivalent_analogue_per_100=100-pv_per_100,
    policy_pv_minus_this_pv=pv_per_100[rate_source=="DAC_common_scenario"]-pv_per_100,
    fractional_date_effect=pv_per_100-legacy_ceiling_date_pv,
    selected_reference=rate_source==a$selected_tier)
  valuation_rows[[i]] <- rates
}
write_csv(bind_rows(flow_rows),"example_cash_flows")
write_csv(bind_rows(valuation_rows),"example_valuations")

# Audit the schedule and date convention on ALL usable core terms, not just examples.
usable <- inventory |> filter(term_usable)
audit <- map_dfr(seq_len(nrow(usable)),function(i){
  a <- usable[i,]; f <- p15_pv_schedule(a$official_rate,a$official_maturity_years,a$official_grace_years)
  independent <- p15_reference_stream(a$official_rate,a$official_maturity_years,a$official_grace_years)
  dr <- if (is.finite(a$selected_rate_pct)) a$selected_rate_pct else 5
  tibble(iso3=a$iso3,analysis_year=a$analysis_year,creditor=a$creditor,
    historical_lmic_reporting_scope=a$historical_lmic_reporting_scope,
    ordinary_scenario_available=a$ordinary_scenario_available,
    discount_rate_pct=dr,rate_is_test_only=!is.finite(a$selected_rate_pct),
    principal_error=abs(sum(f$principal)-100),
    independent_flow_error=max(abs(f$debt_service-independent$debt_service)),
    pv=p15_stream_pv(f,dr),legacy_pv=present_value(f,dr),
    date_effect=p15_stream_pv(f,dr)-present_value(f,dr),
    monotonic=p15_stream_pv(f,dr)>p15_stream_pv(f,dr+1))
})
stopifnot(all(audit$principal_error<1e-8),all(audit$independent_flow_error<1e-8),all(audit$monotonic))
write_csv(audit,"schedule_and_date_audit")
checks <- tibble(check=c("unique_inventory_key","full_project_grid","six_examples",
  "principal_conservation_all_usable","independent_flow_match_all_usable","discount_monotonicity_all_usable"),
  passed=c(!anyDuplicated(inventory[c("iso3","analysis_year","creditor")]),
    nrow(inventory)==nrow(selected)*3,nrow(example_pool)==6,
    all(audit$principal_error<1e-8),all(audit$independent_flow_error<1e-8),all(audit$monotonic)))
stopifnot(all(checks$passed)); write_csv(checks,"checks")
inputs <- unique(c(raw$source_file,wide_paths,file.path(base,c("selected_reference.csv","tier_eligibility.csv")),
  mapping_path,"data-raw/ids_counterpart_area.json",
  "docs/governance/P15_YOY_AND_EXTREME_CASE_USE_2026-09-08.md"))
manifest <- function(paths) tibble(path=paths,bytes=file.info(paths)$size,
  sha256=vapply(paths,function(p)digest::digest(file=p,algo="sha256"),character(1)))
write_csv(manifest(inputs) |> mutate(source_snapshot_id=case_when(
  grepl("ids_core_all_years",path)~"SRC-IDS-CORE-CACHED-20260520-PV-AUDIT-20260909",
  grepl("ids_terms_all_lenders",path)~"SRC-IDS-ALL-LENDERS-2024-PV-AUDIT-20260909",
  TRUE~"P15-PV-FIRST-PASS-20260909-INPUT")),"input_manifest")
code <- c("scripts/p15/build_p15_pv_first_pass.R","R/p15_pv_first_pass.R","R/ids.R","R/pvr.R","R/p15_raw_foundations.R")
write_csv(manifest(code),"code_manifest")
writeLines(capture.output(sessionInfo()),file.path(out,"environment.txt"))
writeLines(c(paste0("build_id: ",out),paste0("benchmark_input: ",base),
  "release_state: diagnostic_not_canonical", "schema_id: P15-PV-FIRST-PASS-1",
  "estimator_id: normalized_100_equal_principal_actual_fractional_end",
  "admissibility_id: existing_tier_permissions_plus_20260908_secondary_purpose_holds",
  "selection_id: existing_reference_unchanged_no_automatic_substitution",
  "Currency: hypothetical common USD notional; IDS creditor averages not verified USD contracts",
  "Policy comparison: common DAC-rate scenario, NOT actual ODA reporting"),file.path(out,"build_contract.txt"))
write_csv(manifest(list.files(out,full.names=TRUE)),"output_manifest")
print(read.csv(file.path(out,"coverage_by_creditor.csv")))
cat("Completed:",out,"\n")
