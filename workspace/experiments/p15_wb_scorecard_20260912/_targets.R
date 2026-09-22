source('scripts/p15/activate_p15_environment.R')
library(targets)
experiment_dir <- 'experiments/p15_wb_scorecard_20260912'
run_frozen_candidate <- function(scripts, snapshot_manifest) {
  stopifnot(all(file.exists(scripts)),file.exists(snapshot_manifest))
  for (script in scripts) {
    status <- system2(file.path(R.home('bin'),'Rscript'),shQuote(script))
    if(status!=0L) stop('Candidate stage failed: ',script)
  }
  file.path(experiment_dir,c('scorecard_country_year.csv','build_status.csv',
    'peer/coverage_summary.csv','peer/rule_counts.csv','peer/crs_valuation_summary.csv'))
}
list(
 tar_target(scorecard_scripts,file.path(experiment_dir,c('build_macro_inputs.R',
   'build_supplemental_inputs.R','build_scorecard_candidate.R','build_peer_candidate.R')),format='file'),
 tar_target(scorecard_snapshot,file.path(experiment_dir,'production_input_manifest.csv'),format='file'),
 tar_target(scorecard_tables,run_frozen_candidate(scorecard_scripts,scorecard_snapshot),format='file')
)
