# Independent reconciliation from selected references and source-level core.
# Run from project root using Rscript --vanilla <this file>.
run <- jsonlite::fromJSON('data-derived/p15_master/current_run.json')
paths <- file.path(run$candidate, c('selected_reference.csv', 'core_evidence.csv'))
read <- function(p) read.csv(p, stringsAsFactors=FALSE, na.strings=c('', 'NA'))
selected <- read(paths[1]); core <- read(paths[2])
key <- function(z) paste(z$iso3,z$analysis_year)
stopifnot(!anyDuplicated(key(selected)),!anyDuplicated(key(core)))
selected <- selected[selected$historical_lmic_reporting_scope %in% TRUE,]
c <- core[match(key(selected),key(core)),]
stopifnot(identical(key(selected),key(c)))
peer <- selected$selected_tier %in% 'peer'
p <- c[peer,]
basis <- ifelse(p$peer_rule_number==5,'income_region',
  ifelse(!is.na(p$rating_moodys_rating),'actual_grade','estimated_grade'))
stopifnot(sum(peer)==731, sum(basis=='actual_grade')==2,
  sum(basis=='estimated_grade')==582, sum(basis=='income_region')==147,
  all(p$peer_target_shadow_used[basis=='estimated_grade'] %in% TRUE))
out <- 'docs/thesis_design/sections/dataset_construction/peer_plan_refresh_20260920'
write.csv(p[basis=='actual_grade',c('iso3','analysis_year','rating_moodys_rating',
  'rating_moodys_missing_reason','peer_rule_number')],file.path(out,'two_recorded_rating_cases.csv'),row.names=FALSE)
receipt <- list(date='2026-09-20',method='independent selected_reference to core_evidence join',
  whole_lmic_country_years=nrow(c), whole_lmic_recorded_ratings=sum(!is.na(c$rating_moodys_rating)),
  selected_peer_country_years=sum(peer), selected_rating_basis=as.list(table(basis)),
  source_hashes=as.list(setNames(vapply(paths,function(p)digest::digest(file=p,algo='sha256',serialize=FALSE),character(1)),paths)),
  matches_display_tables=TRUE)
jsonlite::write_json(receipt,file.path(out,'rating_basis_recheck.json'),pretty=TRUE,auto_unbox=TRUE)
print(receipt[c('whole_lmic_country_years','whole_lmic_recorded_ratings','selected_peer_country_years','selected_rating_basis')])
