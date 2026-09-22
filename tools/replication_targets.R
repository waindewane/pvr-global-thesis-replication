library(targets)
source('scripts/p15/activate_p15_environment.R')
tar_option_set(packages=c('data.table','digest','jsonlite'))
list(tar_target(replication_complete,{
 source('../tools/run_all.R')
 'replication-results/current_run.json'
},format='file',cue=tar_cue(mode='always')))
