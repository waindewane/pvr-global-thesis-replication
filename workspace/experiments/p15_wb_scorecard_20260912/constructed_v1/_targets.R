source('scripts/p15/activate_p15_environment.R');library(targets)
b<-'experiments/p15_wb_scorecard_20260912/constructed_v1'
list(tar_target(constructed_code,list.files(b,pattern='\\.R$',full.names=TRUE),format='file'),
 tar_target(constructed_inputs,c(file.path(dirname(b),'scorecard_inputs.csv'),
 file.path(b,c('historical_eligibility.csv','qpsd_fc_share.csv','gci4_2017_2019.csv','default_events.csv',
 'economic_resiliency.csv','government_financial_strength.csv','rating_midpoint.csv')),
 'experiments/p15_peer_rating_expansion_20260912/model_feature_panel.csv',
 file.path(dirname(b),'supplemental/wef_legacy_gci_long.csv')),format='file'),
 tar_target(constructed_outputs,{
 stopifnot(all(file.exists(constructed_code)),all(file.exists(constructed_inputs)))
 status<-system2(file.path(R.home('bin'),'Rscript'),shQuote(file.path(b,'run.R')))
 if(status!=0L)stop('Constructed experiment failed')
 file.path(b,c('scorecard_country_year.csv','report_coverage.csv','report_rules.csv',
 'missing_inputs_summary.csv','peer_final/coverage_summary.csv','peer_final/checks.csv'))
 },format='file'))
