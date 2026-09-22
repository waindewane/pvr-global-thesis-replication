#!/usr/bin/env Rscript
# Executed from the staged workspace. All scientific stage code is unchanged.
source('scripts/p15/activate_p15_environment.R')
source('R/p15_master.R')
source('R/p15_environment.R')
p15_environment_audit()
config<-p15_master_config()
# Fresh outputs; original snapshots are reference fixtures, not reused stage receipts.
config$cache_root<-'replication-results'
dir.create(config$cache_root,showWarnings=FALSE)
raw<-p15_master_raw(config)
registry<-data.table::fread(config$analysis_registry,na.strings=NULL)
done<-list()
for(i in seq_len(nrow(registry))) {
 done[[registry$id[i]]]<-p15_master_stage(config,registry[i],raw,done)
 jsonlite::write_json(list(configuration=config$version,candidate=file.path(raw$dir,'candidate'),
  stages=lapply(done,function(x)list(key=x$key,dir=x$dir))),
  'replication-results/progress.json',pretty=TRUE,auto_unbox=TRUE)
}
state<-list(configuration=config$version,candidate=file.path(raw$dir,'candidate'),
 stages=lapply(done,function(x)list(key=x$key,dir=x$dir)))
jsonlite::write_json(state,'data-derived/p15_master/current_run.json',pretty=TRUE,auto_unbox=TRUE)
jsonlite::write_json(state,'replication-results/current_run.json',pretty=TRUE,auto_unbox=TRUE)
writeLines(capture.output(sessionInfo()),'replication-results/session_info.txt')
cat('ALL 27 ANALYSIS STAGES COMPLETED FROM REBUILT BENCHMARKS\n')
