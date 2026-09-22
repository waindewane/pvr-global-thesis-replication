source('scripts/p15/activate_p15_environment.R')
source('R/p15_master.R');source('R/p15_replay.R');source('R/p15_full_replay.R')
cfg<-p15_master_config();s<-p15_full_specification()
paths<-unique(c(s$leaves,s$outputs))
r<-data.table::fread(cfg$analysis_registry)
for(i in seq_len(nrow(r)))paths<-unique(c(paths,p15_master_external(cfg,r$id[i]),p15_master_code(cfg,r$id[i],r$script[i])))
missing<-paths[!file.exists(paths)]
if(length(missing)){print(missing);stop('Missing registered dependencies')}
lock<-data.table::fread(cfg$raw_snapshot_manifest)
lock<-lock[artifact_role!='code']
stopifnot(all(vapply(lock$artifact_path,digest::digest,character(1),file=TRUE,algo='sha256')==lock$sha256))
cat(length(paths),'registered raw/stage/code dependencies exist; raw snapshot hashes verified.\n')
