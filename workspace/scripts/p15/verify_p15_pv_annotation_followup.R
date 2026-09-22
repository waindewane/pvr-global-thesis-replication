#!/usr/bin/env Rscript
library(data.table)
p<-"data-derived/p15_pv_annotation_followup_20260909_v1"
r<-"/tmp/p15_pv_annotation_replay_20260909_v1"
out<-"data-derived/p15_pv_annotation_verification_20260909_v1"
if(dir.exists(out)&&length(list.files(out)))stop("Fresh verification directory required")
dir.create(out,recursive=TRUE)
checks<-data.table(check=character(),passed=logical())
ck<-function(n,v){stopifnot(isTRUE(v));checks<<-rbind(checks,data.table(check=n,passed=v))}
for(f in setdiff(list.files(p,pattern="\\.csv$"),"output_manifest.csv"))
  ck(paste0("replay_",f),identical(readLines(file.path(p,f)),readLines(file.path(r,f))))
for(m in c("input_manifest.csv","code_manifest.csv","output_manifest.csv")){
  z<-fread(file.path(p,m));for(i in seq_len(nrow(z)))ck(paste0("hash_",z$path[i]),
    digest::digest(file=z$path[i],algo="sha256")==z$sha256[i])}
x<-fread("data-derived/p15_pv_interpretation_20260909_v1/analysis_rows.csv")
rc<-fread(file.path(p,"rating_comparison_cases.csv"))
rs<-fread(file.path(p,"rating_comparison_period.csv"))
for(i in seq_len(nrow(rs))){a<-rs[i];z<-rc[view==a$view&period==a$period]
  ck(paste("mean_count",a$view,a$period),nrow(z)==a$n&&abs(mean(z$gap)-a$mean_gap)<1e-9)}
rev<-fread(file.path(p,"reversal_magnitude_cases.csv"))
ck("19_reversals_and_5_over_10",nrow(rev)==19&&sum(abs(rev$ge_a-rev$ge_b)>10)==5)
ck("rank_reversal_identity",all((rev$rate_a-rev$rate_b)*(rev$ge_a-rev$ge_b)>1e-8))
steps<-fread(file.path(p,"ibrd_interest_step_cases.csv"))
ib<-x[creditor=="IBRD"]
# Independent discounted difference in coupon payments after year one.
for(i in seq_len(nrow(steps))){a<-steps[i];b<-ib[iso3==a$iso3&analysis_year==a$analysis_year]
  M<-b$official_maturity_years;G<-b$official_grace_years
  ends<-pmin(seq_len(ceiling(M)),M);starts<-c(0,head(ends,-1))
  principal<-if(G==M)ifelse(ends==M,100,0) else 100*(pmax(ends-G,0)-pmax(starts-G,0))/(M-G)
  balance<-100-c(0,head(cumsum(principal),-1))
  dc<-balance*(max(0,b$official_rate+a$change_after_year_one_pp)-b$official_rate)/100*
    pmax(0,ends-pmax(starts,1))
  dm<- -sum(dc/(1+b$selected_rate_pct/100)^ends)
  dp<- -sum(dc/(1+b$dac_rate_pct/100)^ends)
  stopifnot(abs(a$market_ge_change-dm)<1e-8,abs(a$gap_change-(dm-dp))<1e-8)
}
ck("786_independent_interest_step_values",nrow(steps)==786)
# For positive repayments and flat discount rates, gap direction is algebraically fixed.
joined<-merge(steps,ib[,.(iso3,analysis_year,selected_rate_pct,dac_rate_pct)],by=c("iso3","analysis_year"))
ck("discount_order_preserves_gap_sign",all(joined$gap*(joined$selected_rate_pct-joined$dac_rate_pct)>= -1e-8))
fwrite(checks,file.path(out,"checks.csv"))
paths<-c("scripts/p15/verify_p15_pv_annotation_followup.R","scripts/p15/_targets_pv_annotation_followup.R",
  "sources/official_terms/ibrd_rate_interpretation_20260909/source_reading_note.md")
fwrite(data.table(path=paths,sha256=vapply(paths,function(f)digest::digest(file=f,algo="sha256"),character(1))),
  file.path(out,"support_manifest.csv"))
cat(nrow(checks),"checks passed, including independent verification of 786 interest-step scenarios.\n")
