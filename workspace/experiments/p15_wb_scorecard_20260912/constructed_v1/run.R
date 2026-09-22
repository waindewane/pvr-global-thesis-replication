# Offline rebuild using captured sources. Run from repository root.
source('scripts/p15/activate_p15_environment.R')
b<-'experiments/p15_wb_scorecard_20260912/constructed_v1'
run<-function(script,args=character()){
 status<-system2(file.path(R.home('bin'),'Rscript'),c(shQuote(file.path(b,script)),shQuote(args)))
 if(status!=0L)stop('Stage failed: ',script)
}
run('build.R');run('test_score_model.R');run('test_matching.R')
run('build_peer_candidate.R',c(file.path(b,'scorecard_country_year.csv'),file.path(b,'peer_final')))
run('summarize.R')
