source('scripts/p15/activate_p15_environment.R')
library(data.table);library(jsonlite)
ref<-fromJSON('../tools/reference_run.json');new<-fromJSON('replication-results/current_run.json')
checks<-list()
compare<-function(label,old,now){
 files<-list.files(old,pattern='[.]csv([.]gz)?$',recursive=TRUE)
 files<-files[!grepl('manifest|receipt|snapshot|source_register|provenance|environment|hash|session|replay|artifact|inventory',files,ignore.case=TRUE)]
 for(f in files){
  if(!file.exists(file.path(now,f))){checks[[length(checks)+1L]]<<-data.frame(stage=label,file=f,passed=FALSE,detail='Missing rebuilt output');next}
  a<-fread(file.path(old,f));b<-fread(file.path(now,f))
  cols<-names(a)[vapply(a,function(x)is.numeric(x)||is.logical(x),logical(1))]
  ok<-identical(names(a),names(b))&&nrow(a)==nrow(b)
  msg<-if(ok)'numeric/logical values agree to tolerance 1e-10' else 'schema or row count differs'
  if(ok&&length(cols)){
   z<-all.equal(as.data.frame(a[,..cols]),as.data.frame(b[,..cols]),tolerance=1e-10,check.attributes=FALSE)
   ok<-isTRUE(z);if(!ok)msg<-paste(z,collapse='; ')
  }
  if(!length(cols)&&ok)msg<-'schema and row count agree; no numeric/logical columns'
  checks[[length(checks)+1L]]<<-data.frame(stage=label,file=f,passed=ok,detail=msg)
 }
}
compare('benchmarks',ref$candidate,new$candidate)
for(id in names(ref$stages))compare(id,ref$stages[[id]],new$stages[[id]]$dir)
x<-rbindlist(checks);fwrite(x,'replication-results/numerical_comparison.csv')
print(x[,.(tables=.N,passed=sum(passed)),by=stage])
if(any(!x$passed)){print(x[passed==FALSE]);stop('Numerical comparison requires review.')}
cat('All',nrow(x),'table comparisons passed.\n')
