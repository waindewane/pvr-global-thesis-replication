#!/usr/bin/env Rscript
args<-commandArgs(trailingOnly=TRUE)
out<-if(length(args))args[1] else "data-derived/p15_regional_assessment_20260908_v2"
base<-"data-derived/p15_analysis_candidate_20260907_v1"
p<-read.csv(file.path(base,"core_evidence.csv"));e<-read.csv(file.path(base,"tier_eligibility.csv"));s<-read.csv(file.path(base,"selected_reference.csv"))
g<-read.csv(file.path(out,"country_region_map.csv"))
k<-function(d)paste(d$iso3,d$analysis_year)
p$region<-g$region[match(p$iso3,g$iso3)]
p$selected<-s$selected_rate_pct[match(k(p),k(s))]
for(t in c("primary","secondary","moodys")){a<-e[e$tier==t,];at<-match(k(p),k(a));p[[t]]<-ifelse(a$eligible[at],a$rate_pct[at],NA_real_)}
p$observed<-ifelse(is.finite(p$primary),p$primary,p$secondary)
checks<-list();add<-function(n,b)checks[[length(checks)+1L]]<<-data.frame(check=n,passed=isTRUE(b))
eq<-function(a,b)isTRUE(all.equal(a,b,tolerance=1e-10,check.attributes=FALSE))
a<-read.csv(file.path(out,"regional_levels.csv"));y<-read.csv(file.path(out,"regional_yoy.csv"))
for(v in c("selected","observed","moodys")){
 z<-a[a$scope=="historical_LMIC"&a$view==v,];res<-vapply(seq_len(nrow(z)),function(i){x<-p[p$historical_lmic_reporting_scope&p$region==z$region[i]&p$analysis_year==z$analysis_year[i],v];x<-x[is.finite(x)];length(x)==z$n[i]&&eq(if(length(x))mean(x) else NA_real_,z$mean_rate[i])},logical(1));add(paste0(v,"_all_regional_year_levels"),all(res))
 z<-y[y$scope=="historical_LMIC"&y$view==v,];res<-vapply(seq_len(nrow(z)),function(i){b<-p[p$historical_lmic_reporting_scope&p$region==z$region[i]&p$analysis_year==z$analysis_year[i],c("iso3",v)];a<-p[p$historical_lmic_reporting_scope&p$region==z$region[i]&p$analysis_year==z$analysis_year[i]-1,c("iso3",v)];m<-merge(a,b,by="iso3");x<-m[[paste0(v,".y")]]-m[[paste0(v,".x")]];x<-x[is.finite(x)];length(x)==z$n[i]&&eq(mean(x),z$mean_change[i])},logical(1));add(paste0(v,"_all_regional_common_country_changes"),all(res))
}
add("AFG_PAK_historical_South_Asia",all(g$region[g$iso3%in%c("AFG","PAK")]=="South Asia"))
add("LMIC_142_economies_1756_rows",length(unique(p$iso3[p$historical_lmic_reporting_scope]))==142&&sum(p$historical_lmic_reporting_scope)==1756)
add("South_Asia_observed_one_2022_2024",all(a$n[a$scope=="historical_LMIC"&a$view=="observed"&a$region=="South Asia"&a$analysis_year%in%2022:2024]==1))
for(f in c("input_manifest.csv","script_manifest.csv","output_manifest.csv")){m<-read.csv(file.path(out,f));add(f,all(vapply(m$artifact_path,digest::digest,character(1),file=TRUE,algo="sha256")==m$sha256))}
m<-read.csv(file.path(base,"output_manifest.csv"));add("candidate_unchanged",all(vapply(m$artifact_path,digest::digest,character(1),file=TRUE,algo="sha256")==m$sha256))
r<-do.call(rbind,checks);print(r);stopifnot(all(r$passed));write.csv(r,file.path(out,"independent_verification.csv"),row.names=FALSE)
