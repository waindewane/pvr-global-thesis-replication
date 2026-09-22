#!/usr/bin/env Rscript
args<-commandArgs(trailingOnly=TRUE);o<-if(length(args))args[1] else "data-derived/p15_official_policy_comparison_20260908_v1"
b<-"data-derived/p15_analysis_candidate_20260907_v1";mp<-"data-derived/p15_official_dac_mapping_20260908_v1/map.csv"
p<-read.csv(file.path(b,"core_evidence.csv"));e<-read.csv(file.path(b,"tier_eligibility.csv"));m<-read.csv(mp)
key<-function(x)paste(x$iso3,x$analysis_year)
for(t in c("primary","ids","secondary","moodys","peer")){q<-e[e$tier==t,];at<-match(key(p),key(q));p[[t]]<-ifelse(q$eligible[at],q$rate_pct[at],NA_real_)}
p<-p[p$historical_lmic_reporting_scope&is.finite(p$primary),]
p$fixed5<-5;p$fixed10<-10;p$group976<-ifelse(p$historical_income_level=="Upper middle income",6,ifelse(p$historical_income_level=="Lower middle income",7,9));p$group976[p$iso3%in%c("AGO","BEN","ETH","GIN","RWA","SEN")]<-9
at<-match(key(p),key(m));dg<-m$dac_group[at]
p$dac_group976<-unname(c(LDCs=9,"Other LICs"=9,LMICs=7,UMICs=6)[dg])
p$dac_headline_new<-ifelse(is.finite(p$dac_group976),ifelse(p$analysis_year<2018,10,p$dac_group976),NA_real_)
p$dac_ge_from2015<-ifelse(p$analysis_year>=2015,p$dac_group976,NA_real_)
p$dac_modern_2018<-ifelse(p$analysis_year>=2018,p$dac_group976,NA_real_)
checks<-list();add<-function(n,v)checks[[length(checks)+1L]]<<-data.frame(check=n,passed=isTRUE(v))
z<-read.csv(file.path(o,"paired_summary.csv"))
for(i in seq_len(nrow(z))){t<-z$tier[i];q<-z$policy[i];d<-p[is.finite(p[[t]])&is.finite(p[[q]]),];a<-abs(d[[t]]-d$primary);c<-abs(d[[q]]-d$primary)
 add(paste(t,q,"paired_losses",sep="_"),nrow(d)==z$n[i]&&abs(mean(a)-z$tier_mae[i])<1e-12&&abs(mean(c)-z$policy_mae[i])<1e-12&&abs(mean(c-a)-z$mean_improvement[i])<1e-12)
}
cs<-read.csv(file.path(o,"common_all_tiers_summary.csv"))
for(i in seq_len(nrow(cs))){q<-cs$policy[i];method<-cs$method[i];d<-p[is.finite(p$ids)&is.finite(p$secondary)&is.finite(p$moodys)&is.finite(p$peer)&is.finite(p[[q]]),];add(paste("common",q,method,sep="_"),nrow(d)==cs$n[i]&&abs(mean(abs(d[[method]]-d$primary))-cs$mae_pp[i])<1e-12)}
for(f in c("change_summary.csv","change_excluding_2018_boundary.csv")){
 y<-read.csv(file.path(o,f))
 for(i in seq_len(nrow(y))){t<-y$tier[i];q<-y$policy[i];d<-p[is.finite(p[[t]])&is.finite(p[[q]]),c("iso3","analysis_year","primary",t,q)];a<-d;a$analysis_year<-a$analysis_year+1;v<-merge(a,d,by=c("iso3","analysis_year"));if(grepl("excluding",f))v<-v[v$analysis_year!=2018,];target<-v$primary.y-v$primary.x;dt<-v[[paste0(t,".y")]]-v[[paste0(t,".x")]];dq<-v[[paste0(q,".y")]]-v[[paste0(q,".x")]]
 add(paste(f,t,q,sep="_"),nrow(v)==y$n[i]&&abs(mean(abs(dt-target))-y$tier_change_mae[i])<1e-12&&abs(mean(abs(dq-target))-y$policy_change_mae[i])<1e-12)
 }
}
for(f in c("input_manifest.csv","script_manifest.csv","output_manifest.csv")){a<-read.csv(file.path(o,f));add(f,all(vapply(a$artifact_path,digest::digest,character(1),file=TRUE,algo="sha256")==a$sha256))}
a<-read.csv(file.path(b,"output_manifest.csv"));add("candidate_unchanged",all(vapply(a$artifact_path,digest::digest,character(1),file=TRUE,algo="sha256")==a$sha256))
r<-do.call(rbind,checks);stopifnot(all(r$passed));write.csv(r,file.path(o,"independent_verification.csv"),row.names=FALSE);print(data.frame(checks=nrow(r),all_passed=all(r$passed)))
