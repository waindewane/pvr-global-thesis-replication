# Reproduce manuscript-planning tables from the adopted, already-computed run.
# Run from the project root with Rscript --vanilla <this file>.
root <- normalizePath('.', mustWork = TRUE)
out <- file.path(root, 'docs/thesis_design/sections/dataset_construction/peer_plan_refresh_20260920/display_tables_v1')
dir.create(out, recursive = TRUE, showWarnings = FALSE)
run_path <- file.path(root, 'data-derived/p15_master/current_run.json')
run <- jsonlite::fromJSON(run_path)
paths <- c(run_path, file.path(run$candidate, c('core_evidence.csv', 'peer_score_intervals.csv', 'peer_changes.csv')),
           file.path(run$stages$peer_description$dir, 'country_year_groups.csv'))
read <- function(path) read.csv(path, stringsAsFactors = FALSE, na.strings = c('', 'NA'))
core <- read(paths[2]); scores <- read(paths[3]); changes <- read(paths[4]); groups <- read(paths[5])
key <- function(x) paste(x$iso3, x$analysis_year)
stopifnot(!anyDuplicated(key(core)), !anyDuplicated(key(scores)), !anyDuplicated(key(groups)))
x <- core[core$historical_lmic_reporting_scope %in% TRUE, ]
g <- groups[match(key(x), key(groups)), ]; s <- scores[match(key(x), key(scores)), ]
x$actual_rating <- !is.na(x$rating_moodys_rating)
x$bounded_score_without_rating <- !x$actual_rating & is.finite(s$shadow_notch_lower) & is.finite(s$shadow_notch_upper)
x$peer_computed <- is.finite(g$peer_rate_pct)
x$peer_selected <- g$selected_tier %in% 'peer'
x$rule <- g$peer_rule_number
x$uses_target_estimate <- x$peer_selected & x$rule %in% 1:4 & g$peer_target_shadow_used %in% TRUE
x$uses_actual_target_rating <- x$peer_selected & x$rule %in% 1:4 & x$actual_rating
x$no_target_rating_condition <- x$peer_selected & x$rule %in% 5
aggregate_coverage <- function(z, label) data.frame(period = label, country_years = nrow(z),
  countries = length(unique(z$iso3)), actual_rating_recorded = sum(z$actual_rating),
  bounded_score_without_recorded_rating = sum(z$bounded_score_without_rating),
  peer_computable = sum(z$peer_computed), peer_selected = sum(z$peer_selected),
  selected_actual_target_rating = sum(z$uses_actual_target_rating),
  selected_estimated_target_rating = sum(z$uses_target_estimate),
  selected_income_region_only = sum(z$no_target_rating_condition))
annual <- do.call(rbind, lapply(split(x, x$analysis_year), function(z) aggregate_coverage(z, as.character(z$analysis_year[1]))))
annual <- rbind(annual, aggregate_coverage(x, '2012–2024'))
sel <- groups[groups$historical_lmic_reporting_scope %in% TRUE & groups$selected_tier %in% 'peer', ]
rule_counts <- function(z, label) {
  counts <- tabulate(z$peer_rule_number, nbins = 5)
  data.frame(period = label, income_region_rating = counts[1], income_rating = counts[2],
    region_rating = counts[3], rating_only = counts[4], income_region_only = counts[5], total = nrow(z))
}
rules <- do.call(rbind, lapply(split(sel, sel$analysis_year), function(z) rule_counts(z, as.character(z$analysis_year[1]))))
rules <- rbind(rules, rule_counts(sel, '2012–2024'))
source_counts <- function(z, label) data.frame(period = label, selected_borrower_years = nrow(z),
  primary_memberships = sum(z$peer_primary_donor_count), ids_memberships = sum(z$peer_ids_donor_count),
  secondary_memberships = sum(z$peer_secondary_donor_count), rating_implied_memberships = sum(z$peer_rating_implied_donor_count),
  total_memberships = sum(z$member_count), groups_with_rating_implied = sum(z$peer_rating_implied_donor_count > 0),
  groups_only_rating_implied = sum(z$peer_rating_implied_donor_count == z$member_count))
sources <- do.call(rbind, lapply(split(sel, sel$analysis_year), function(z) source_counts(z, as.character(z$analysis_year[1]))))
sources <- rbind(sources, source_counts(sel, '2012–2024'))
legacy <- changes[changes$historical_lmic_reporting_scope %in% TRUE & changes$selected_tier %in% 'peer', ]
labels <- c('same_income_region_rating3_min3', 'same_income_rating3_min3', 'same_region_rating3_min3', 'rating3_min3', 'same_income_region_min3', 'same_income_min3', 'same_region_min3', 'global_min3')
comparison <- data.frame(rule = 1:8, old_selected = as.integer(table(factor(legacy$old_peer_rule, levels = labels))),
  revised_selected = c(tabulate(sel$peer_rule_number, nbins = 5), 0L, 0L, 0L))
stopifnot(nrow(sel) == 731, nrow(legacy) == 778, sum(comparison$old_selected) == 778,
          identical(as.integer(comparison$revised_selected), c(207L,235L,51L,91L,147L,0L,0L,0L)),
          all(rowSums(sources[,3:6]) == sources$total_memberships),
          all(annual$peer_selected == annual$selected_actual_target_rating + annual$selected_estimated_target_rating + annual$selected_income_region_only))
tables <- list(annual_coverage = annual, annual_matching_rules = rules,
               annual_peer_rate_sources = sources, previous_and_adopted_rule_counts = comparison)
for (name in names(tables)) write.csv(tables[[name]], file.path(out, paste0(name,'.csv')), row.names = FALSE, na = '')
md_table <- function(z, labels) {
  stopifnot(length(labels)==ncol(z))
  c(paste0('| ',paste(labels,collapse=' | '),' |'),
    paste0('| ',paste(rep('---',ncol(z)),collapse=' | '),' |'),
    apply(z,1,function(row)paste0('| ',paste(row,collapse=' | '),' |')))
}
readable <- c('# Peer construction: numerical display candidates', '',
  '20 September 2026. Reproducible descriptive summaries of the adopted run. Counts refer to low- and middle-income country-years unless stated otherwise. Repeated observations of a country across years are counted separately.', '',
  '## Annual matching rules', '',
  md_table(rules,c('Year','Income + region + grade','Income + grade','Region + grade','Grade only','Income + region','Total selected')), '',
  'Grade matching uses available actual ratings or the adopted estimated ranges. All groups require at least three countries. The total is the number of borrower-years using a peer rate as their selected reference.', '',
  '## Annual use of actual and estimated target ratings', '',
  md_table(annual[,c('period','country_years','actual_rating_recorded','bounded_score_without_recorded_rating','selected_actual_target_rating','selected_estimated_target_rating','selected_income_region_only')],
    c('Year','All borrower-years','Recorded target rating','Estimated range with no recorded rating','Selected: actual grade','Selected: estimated grade','Selected: income + region')), '',
  'The first coverage columns describe the whole sample. The last three columns partition selected peer references. An estimated range does not guarantee a qualifying peer group. No recorded rating means missing from this dataset, not proof that no agency rated the country.', '',
  '## Rate sources entering selected peer groups', '',
  md_table(sources[,1:7],c('Year','Selected borrower-years','Primary','IDS','Secondary','Rating-implied','Total country memberships')), '',
  'Each source count records a country contributing to a target peer group. The same country-year counts again when used by another target. Source counts sum to memberships, not to the number of borrower-years or to an additive decomposition of the median. Across the full period, 697 selected groups contain rating-implied rates and 71 contain only rating-implied rates.', '',
  '## Previous and adopted matching rules — supporting development evidence', '',
  md_table(comparison,c('Search rule','Previously selected','Currently selected')), '',
  'Rules 6, 7 and 8 were income-only, region-only and unrestricted global groups. Their removal accompanies other changes, so this table does not isolate the effect of the scorecard or the expanded rate-source pool. The previous selected sample has 778 borrower-years; the adopted method retains 731. These historical development counts are supporting rationale, not parallel current methods.', '',
  '## Placement', '',
  'The annual matching-rule table is a main-text candidate. The fuller rating-coverage and source-membership tables fit supporting material, with their substantive totals discussed in the main text. Landscape orientation is available if needed at typesetting; legibility takes priority over squeezing columns. The old/new table can remain supporting research evidence unless the methodological-development narrative benefits from it.')
writeLines(readable,file.path(out,'NUMERICAL_TABLES.md'))
hash <- function(path) digest::digest(file = path, algo = 'sha256', serialize = FALSE)
manifest <- data.frame(path = paths, sha256 = vapply(paths, hash, character(1)))
write.csv(manifest, file.path(out, 'input_manifest.csv'), row.names = FALSE)
capture.output(sessionInfo(), file = file.path(out, 'environment.txt'))
receipt <- list(date='2026-09-20', scope='manuscript_display_preparation', current_run_key=run$run_key,
  population='historical low/middle income country-years, 2012–2024', lifecycle_status='diagnostic',
  release_state='private_research', numerical_method_changed=FALSE,
  selected_countries=length(unique(sel$iso3)), selected_without_recorded_rating=sum(!sel$prior_rating_available),
  check_rule_totals=TRUE, check_source_totals=TRUE, check_rating_basis_partition=TRUE,
  note='Membership counts repeat a peer country whenever it contributes to another target. They are not additive source effects on the median. Missing recorded ratings do not establish absence of any agency rating.')
jsonlite::write_json(receipt, file.path(out,'verification.json'), pretty=TRUE, auto_unbox=TRUE)
files <- list.files(out, full.names=TRUE); files <- files[basename(files) != 'output_manifest.csv']
write.csv(data.frame(path=files,sha256=vapply(files,hash,character(1))),file.path(out,'output_manifest.csv'),row.names=FALSE)
print(annual[nrow(annual),]);print(sources[nrow(sources),]);print(comparison)
