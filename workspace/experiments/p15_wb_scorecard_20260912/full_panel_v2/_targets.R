source('scripts/p15/activate_p15_environment.R')
library(targets)
v2 <- 'experiments/p15_wb_scorecard_20260912/full_panel_v2'
list(
  tar_target(full_panel_code,c(list.files(v2,full.names=TRUE,pattern='\\.(R|py)$'),
    list.files(file.path(v2,'snapshot'),full.names=TRUE,pattern='\\.R$')),format='file'),
  tar_target(full_panel_sources,{
    z <- data.table::fread(file.path(v2,'snapshot/preserved_baseline_manifest.csv'))
    c(z$path,file.path(v2,'snapshot/current_run.json'),
      file.path(v2,'snapshot/gross_interest_uncertainty_check.csv'),
      'data-derived/p15_peer_options_20260912_v2/seed_country_years.csv')
  },format='file'),
  tar_target(full_panel_results,{
    stopifnot(all(file.exists(full_panel_code)),all(file.exists(full_panel_sources)))
    status <- system2(file.path(R.home('bin'),'Rscript'),shQuote(file.path(v2,'run.R')))
    if(status!=0L)stop('Full-panel peer candidate failed')
    c(file.path(v2,'REPORT.md'),list.files(file.path(v2,'results'),full.names=TRUE,pattern='\\.(csv|json|png|txt)$'))
  },format='file')
)
