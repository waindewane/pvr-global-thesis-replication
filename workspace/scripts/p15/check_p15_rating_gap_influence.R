source("R/p15_current_inputs.R")
source("scripts/p15/activate_p15_environment.R")
library(data.table)
p<-Sys.getenv("P15_PV_FOLLOWUP_BASE",p15_current_input("pv_followup"))
args<-commandArgs(TRUE)
out<-if(length(args))args[1]else file.path("data-derived",paste0("p15_rating_gap_influence_",format(Sys.time(),"%Y%m%d_%H%M%S")))
if(dir.exists(out)&&length(list.files(out)))stop("Fresh directory required")
dir.create(out,recursive=TRUE)
source_path<-file.path(p,"rating_comparison_cases.csv")
x<-fread(source_path)
a<-x[view=="paired_moodys",.(iso3,analysis_year,creditor,region,period,moodys=gap)]
b<-x[view=="paired_primary_ids",.(iso3,analysis_year,creditor,observed=gap)]
m<-merge(a,b,by=c("iso3","analysis_year","creditor"))
m[,extra_ge:=moodys-observed]
loo<-rbindlist(lapply(unique(m$period),function(pr){z<-m[period==pr]
  rbindlist(lapply(unique(z$iso3),function(c)z[iso3!=c,.(period=pr,omitted_country=c,n=.N,mean_extra_ge=mean(extra_ge))]))}))
fwrite(m,file.path(out,"paired_case_differences.csv"))
fwrite(loo,file.path(out,"leave_one_country_out.csv"))
fwrite(loo[,.(min_mean=min(mean_extra_ge),max_mean=max(mean_extra_ge),
  omitted_at_min=omitted_country[which.min(mean_extra_ge)],omitted_at_max=omitted_country[which.max(mean_extra_ge)]),by=period],
  file.path(out,"influence_ranges.csv"))
fwrite(m[,.(n=.N,countries=uniqueN(iso3),mean_extra=mean(extra_ge),median_extra=median(extra_ge)),by=.(region,period)],
  file.path(out,"regional_paired_differences.csv"))
paths<-c(source_path,"scripts/p15/check_p15_rating_gap_influence.R")
fwrite(data.table(path=paths,sha256=vapply(paths,function(f)digest::digest(file=f,algo="sha256"),character(1))),
  file.path(out,"input_manifest.csv"))
writeLines(c("build_id: BUILD-P15-PV-ANNOTATION-INFLUENCE-20260909-V1",
  "release_state: diagnostic_not_canonical","schema_id: P15-RATING-GE-INFLUENCE-1",
  "estimator_id: equal_case_leave_one_country_out","admissibility_id: inherited_paired_primary_IDS_Moodys",
  "selection_id: unchanged","No case deletion or recalibration. Influence ranges are not confidence intervals."),
  file.path(out,"build_contract.txt"))
writeLines(capture.output(sessionInfo()),file.path(out,"environment.txt"))
files<-list.files(out,full.names=TRUE)
fwrite(data.table(path=files,sha256=vapply(files,function(f)digest::digest(file=f,algo="sha256"),character(1))),
  file.path(out,"output_manifest.csv"))
print(loo[,.(min_mean=min(mean_extra_ge),max_mean=max(mean_extra_ge)),by=period])
