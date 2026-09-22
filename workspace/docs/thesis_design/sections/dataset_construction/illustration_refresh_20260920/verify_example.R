# Run from the project root. Verify the illustration against the current authority.
current <- jsonlite::fromJSON('data-derived/p15_master/current_run.json')
out <- 'docs/thesis_design/sections/dataset_construction/illustration_refresh_20260920'
issue_path <- 'data-derived/p15_full_replay_20260909_185908/isolated_build/data-derived/p15_primary_approved_candidate_2012_2024_v1/p15_primary_approved_issue_disposition_2012_2024.csv.gz'
issues <- read.csv(gzfile(issue_path), stringsAsFactors = FALSE)
issues <- subset(issues, iso3 == 'PER' & analysis_year == 2024 & approved_primary_preferred_issue_before_sanity)
issues <- issues[order(issues$maturity_date), ]
core <- read.csv(file.path(current$candidate, 'core_evidence.csv'), stringsAsFactors = FALSE)
core <- subset(core, iso3 == 'PER' & analysis_year == 2024)
selected <- read.csv(file.path(current$candidate, 'selected_reference.csv'), stringsAsFactors = FALSE)
selected <- subset(selected, iso3 == 'PER' & analysis_year == 2024)
eligibility <- read.csv(file.path(current$candidate, 'tier_eligibility.csv'), stringsAsFactors = FALSE)
eligibility <- subset(eligibility, iso3 == 'PER' & analysis_year == 2024)
rate <- weighted.mean(issues$original_issue_yield_pct, issues$approved_weight_usd)
stopifnot(nrow(issues) == 2L, nrow(core) == 1L, nrow(selected) == 1L,
 all(issues$currency == 'USD'), all(issues$issue_date == '2024-08-08'),
 all(issues$approved_fixed_call_sink_subtype), !any(issues$identifier_yield_conflict),
 abs(rate - 5.70775) < 1e-10,
 abs(rate - core$primary_usd_market_rate_pct) < 1e-10,
 abs(rate - selected$selected_rate_pct) < 1e-10,
 selected$selected_tier == 'primary', nrow(eligibility) == 5L, all(eligibility$eligible))
display <- data.frame(isin = issues$representative_isins, issue_date = issues$issue_date,
 maturity_date = issues$maturity_date, issued_usd_billion = issues$approved_weight_usd / 1e9,
 issue_yield_pct = issues$original_issue_yield_pct,
 weight_pct = 100 * issues$approved_weight_usd / sum(issues$approved_weight_usd))
write.csv(display, file.path(out, 'peru_issue_table.csv'), row.names = FALSE)
write.csv(eligibility, file.path(out, 'peru_available_references.csv'), row.names = FALSE)
jsonlite::write_json(list(configuration = current$configuration, run_key = current$run_key,
 candidate = current$candidate, issue_source = issue_path, weighted_rate_pct = rate,
 selected_rate_pct = selected$selected_rate_pct, current_primary_selection_reproduced = TRUE,
 all_five_references_eligible = TRUE, method_changed = FALSE),
 file.path(out, 'verification.json'), pretty = TRUE, auto_unbox = TRUE, digits = 15)
writeLines(capture.output(sessionInfo()), file.path(out, 'session_info.txt'))
cat('Peru construction verified:', format(rate, digits=8), 'percent; primary selected; all five alternatives eligible.\n')
