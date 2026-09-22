#!/usr/bin/env Rscript
args<-commandArgs(trailingOnly=TRUE);o<-if(length(args))args[1] else "data-derived/p15_policy_comparison_20260908_v2"
b<-"data-derived/p15_analysis_candidate_20260907_v1"
p<-read.csv(file.path(b,"core_evidence.csv"));e<-read.csv(file.path(b,"tier_eligibility.csv"))
key<-function(x)paste(x$iso3,x$analysis_year)
for(t in c("primary","ids","secondary","moodys","peer")){q<-e[e$tier==t,];a<-match(key(p),key(q));p[[t]]<-ifelse(q$eligible[a],q$rate_pct[a],NA_real_)}
p<-p[p$historical_lmic_reporting_scope&is.finite(p$primary),]
p$group976<-ifelse(p$historical_income_level=="Upper middle income",6,ifelse(p$historical_income_level=="Lower middle income",7,9))
p$group976[p$iso3%in%c("AGO","BEN","ETH","GIN","RWA","SEN")]<-9
p$fixed5<-5;p$fixed10<-10
z<-read.csv(file.path(o,"paired_summary.csv"));checks<-list();add<-function(n,x)checks[[length(checks)+1L]]<<-data.frame(check=n,passed=isTRUE(x))
for(i in seq_len(nrow(z))){t<-z$tier[i];q<-z$policy[i];d<-p[is.finite(p[[t]]),];err<-abs(d[[t]]-d$primary);pol<-abs(d[[q]]-d$primary)
 add(paste0(t,"_",q,"_matched_loss"),nrow(d)==z$n[i]&&abs(mean(err)-z$tier_mae[i])<1e-12&&abs(mean(pol)-z$policy_mae[i])<1e-12&&abs(mean(pol-err)-z$mean_improvement[i])<1e-12)
}
mapping<-read.csv(file.path(o,"policy_mapping.csv"));at<-match(key(p),key(mapping));add("all_215_policy_rules",!anyNA(at)&&all(p$group976==mapping$group976[at]))
cs<-read.csv(file.path(o,"common_all_tiers_summary.csv"));common<-p[is.finite(p$ids)&is.finite(p$secondary)&is.finite(p$moodys)&is.finite(p$peer),]
add("common_124_sample",nrow(common)==124&&all(cs$n==124))
for(m in cs$method)add(paste0("common_mae_",m),abs(mean(abs(common[[m]]-common$primary))-cs$mae_pp[cs$method==m])<1e-12)
y<-read.csv(file.path(o,"change_summary.csv"))
for(i in seq_len(nrow(y))){t<-y$tier[i];q<-y$policy[i];d<-p[is.finite(p[[t]]),c("iso3","analysis_year","primary",t,q)];a<-d;a$analysis_year<-a$analysis_year+1;m<-merge(a,d,by=c("iso3","analysis_year"));target<-m$primary.y-m$primary.x;dt<-m[[paste0(t,".y")]]-m[[paste0(t,".x")]];dp<-m[[paste0(q,".y")]]-m[[paste0(q,".x")]]
 add(paste0("annual_change_",t,"_",q),nrow(m)==y$n[i]&&abs(mean(abs(dt-target))-y$tier_change_mae[i])<1e-12&&abs(mean(abs(dp-target))-y$policy_change_mae[i])<1e-12)
}
regions<-read.csv("data-derived/p15_regional_assessment_20260908_v2/country_region_map.csv")
p$region<-regions$region[match(p$iso3,regions$iso3)]
rr<-read.csv(file.path(o,"leave_one_region.csv"))
for(i in seq_len(nrow(rr))){t<-rr$tier[i];q<-rr$policy[i];d<-p[is.finite(p[[t]])&p$region!=rr$region_removed[i],];gain<-mean(abs(d[[q]]-d$primary)-abs(d[[t]]-d$primary));add(paste("region_removal",t,q,rr$region_removed[i],sep="_"),nrow(d)==rr$n[i]&&abs(gain-rr$mean_improvement[i])<1e-12)}
for(f in c("input_manifest.csv","script_manifest.csv","output_manifest.csv")){m<-read.csv(file.path(o,f));add(f,all(vapply(m$artifact_path,digest::digest,character(1),file=TRUE,algo="sha256")==m$sha256))}
m<-read.csv(file.path(b,"output_manifest.csv"));add("candidate_unchanged",all(vapply(m$artifact_path,digest::digest,character(1),file=TRUE,algo="sha256")==m$sha256))
r<-do.call(rbind,checks);stopifnot(all(r$passed));write.csv(r,file.path(o,"independent_verification.csv"),row.names=FALSE);print(r)
