#!/usr/bin/env Rscript
# Independent base-R checks against the immutable delivered tables.
args<-commandArgs(trailingOnly=TRUE)
out<-if(length(args))args[1] else "data-derived/p15_dataset_assessment_20260908_v4"
base<-"data-derived/p15_analysis_candidate_20260907_v1"
read<-function(n)read.csv(file.path(base,paste0(n,".csv")),check.names=FALSE)
p<-read("core_evidence");e<-read("tier_eligibility");s<-read("selected_reference")
key<-function(d)paste(d$iso3,d$analysis_year)
e<-e[e$eligible,]
l<-p[p$historical_lmic_reporting_scope,]
observed<-read.csv(file.path(out,"pair_summary.csv"))
checks<-list()
for(i in seq_len(nrow(observed))){
  r<-observed[i,]
  a<-e[e$tier==r$anchor & key(e)%in%key(l),c("iso3","analysis_year","rate_pct")]
  b<-e[e$tier==r$comparison & key(e)%in%key(l),c("iso3","analysis_year","rate_pct")]
  d<-merge(a,b,by=c("iso3","analysis_year"));z<-d$rate_pct.y-d$rate_pct.x
  checks[[length(checks)+1L]]<-data.frame(check=r$pair,
    passed=nrow(d)==r$n && abs(mean(z)-r$mean_gap_pp)<1e-12 && abs(mean(abs(z))-r$mae_pp)<1e-12)
}
m<-read.csv(file.path(out,"output_manifest.csv"))
checks[[length(checks)+1L]]<-data.frame(check="output_hashes",
  passed=all(vapply(m$artifact_path,digest::digest,character(1),file=TRUE,algo="sha256")==m$sha256))
m<-read("output_manifest")
checks[[length(checks)+1L]]<-data.frame(check="unchanged_delivered_candidate",
  passed=all(vapply(m$artifact_path,digest::digest,character(1),file=TRUE,algo="sha256")==m$sha256))
cov<-read.csv(file.path(out,"coverage_overall.csv"))
checks[[length(checks)+1L]]<-data.frame(check="independent_coverage",
  passed=nrow(l)==1756 && cov$any_selected[cov$scope=="historical_LMIC"]==sum(is.finite(s$selected_rate_pct[s$historical_lmic_reporting_scope])))
bridge<-read.csv(file.path(out,"source_switch_details.csv"))
sa<-s[s$historical_lmic_reporting_scope & is.finite(s$selected_rate_pct),]
sb<-sa;sb$analysis_year<-sb$analysis_year+1L
tr<-merge(sa,sb,by=c("iso3","analysis_year"))
checks[[length(checks)+1L]]<-data.frame(check="independent_source_switches",
  passed=sum(tr$selected_tier.x!=tr$selected_tier.y)==334 && nrow(bridge)==334)
checks[[length(checks)+1L]]<-data.frame(check="all_six_pdf_and_png_figures_exist",
  passed=length(list.files(out,pattern="^figure_.*[.]pdf$"))==6 &&
    length(list.files(out,pattern="^figure_.*[.]png$"))==6 &&
    all(file.info(list.files(out,pattern="^figure_",full.names=TRUE))$size>1000))
receipt<-do.call(rbind,checks)
print(receipt)
if(!all(receipt$passed))stop("Independent verification failed")
write.csv(receipt,file.path(out,"independent_verification.csv"),row.names=FALSE)
