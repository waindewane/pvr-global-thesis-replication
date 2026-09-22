library(data.table)
library(jsonlite)
library(digest)

out <- 'docs/thesis_design/sections/benchmark_evaluation/temporal_use_20260921'
j <- fromJSON('data-derived/p15_master/current_run.json')
inputs <- 'data-derived/p15_master/current_run.json'
read_stage <- function(stage, file) {
  path <- file.path(j$stages[[stage]]$dir, file)
  inputs <<- c(inputs, path)
  fread(path)
}
d <- read_stage('benchmark_inference', 'consecutive_details.csv')[sample_view == 'full_validation']
stopifnot(all(d$analysis_year %in% 2019:2024),
          !anyDuplicated(d[, .(iso3, analysis_year, focal)]),
          all(abs(d$primary_change - (d$primary - d$primary_prior)) < 1e-10),
          all(abs(d$focal_change - (d$focal_rate - d$focal_prior)) < 1e-10))
summary <- d[, .(n_transitions = .N, countries = uniqueN(iso3),
  focal_endpoint_mae_pp = mean(c(abs(focal_rate-primary), abs(focal_prior-primary_prior))),
  dac_endpoint_mae_pp = mean(c(abs(comparator_rate-primary), abs(comparator_prior-primary_prior))),
  focal_change_mae_pp = mean(abs(focal_change-primary_change)),
  dac_change_mae_pp = mean(abs(comparator_change-primary_change)),
  source_switch_n = sum(focal_source != focal_source_prior),
  both_closer_change_worse = sum(abs(focal_rate-primary) < abs(comparator_rate-primary) &
    abs(focal_prior-primary_prior) < abs(comparator_prior-primary_prior) &
    abs(focal_change-primary_change) > abs(comparator_change-primary_change))), by=focal]
saved <- read_stage('benchmark_inference', 'consecutive_summary.csv')[sample_view == 'full_validation']
joined <- merge(summary, saved, by='focal', suffixes=c('_checked','_saved'))
for (column in c('n_transitions','countries','focal_endpoint_mae_pp','dac_endpoint_mae_pp',
                 'focal_change_mae_pp','dac_change_mae_pp','source_switch_n')) {
  stopifnot(all(abs(joined[[paste0(column,'_checked')]]-joined[[paste0(column,'_saved')]]) < 1e-10))
}
stopifnot(all(joined$both_closer_change_worse == joined$both_levels_closer_but_change_worse_n))
fwrite(summary, file.path(out,'levels_and_changes.csv'))

t <- read_stage('statistical_review','nonpeer_tier_availability_country_year.csv')
country <- t[, .(years=uniqueN(analysis_year), lmic_years=sum(historical_lmic_reporting_scope),
  available_years=sum(usable), selected_years=sum(selected_usable)), by=.(iso3,tier)]
stable <- country[lmic_years==7, .(countries_in_scope=.N,
  available_all_seven=sum(years==7 & available_years==7),
  selected_all_seven=sum(years==7 & selected_years==7)), by=tier]
saved_stable <- read_stage('statistical_review','tier_stability_summary.csv')[scope=='historical_LMIC_every_year']
m <- merge(stable,saved_stable,by='tier',suffixes=c('_checked','_saved'))
for (column in c('countries_in_scope','available_all_seven','selected_all_seven')) {
 stopifnot(all(m[[paste0(column,'_checked')]]==m[[paste0(column,'_saved')]]))
}
fwrite(stable,file.path(out,'complete_nonpeer_histories.csv'))
inputs <- c(inputs,'R/p15_thesis_benchmark_inference.R',
 'scripts/p15/build_p15_thesis_benchmark_inference.R',
 'scripts/p15/annotation_followup_20260911/build_statistical_review.R',
 file.path(out,'prepare_evidence.R'))
fwrite(data.table(path=inputs,sha256=vapply(inputs,digest,character(1),file=TRUE,algo='sha256')),
 file.path(out,'input_manifest.csv'))
fwrite(data.table(check=c('consecutive_change_arithmetic','published_temporal_summary',
 'endpoint_counterexamples','complete_nonpeer_availability'),passed=TRUE),file.path(out,'verification.csv'))
print(summary)
print(stable)
