source("R/p15_current_inputs.R")
#!/usr/bin/env Rscript
suppressPackageStartupMessages({library(data.table);library(digest)})
source("R/p15_dataset_assessment.R");source("R/p15_deeper_assessment.R");source("R/research_governance.R")
args<-commandArgs(trailingOnly=TRUE);out<-if(length(args))args[1] else file.path("data-derived",paste0("p15_policy_",format(Sys.time(),"%Y%m%d_%H%M%S")))
if(dir.exists(out))stop("Refusing overwrite")
base<-Sys.getenv("P15_ANALYSIS_BASE", p15_current_input("dataset"))
inputs<-c(file.path(base,c("core_evidence.csv","tier_eligibility.csv","peer_membership.csv","output_manifest.csv")),
 file.path(Sys.getenv("P15_REGIONAL_BASE", p15_current_input("regional")),"country_region_map.csv"),
 "docs/governance/P15_POLICY_COMPARISON_PROTOCOL_2026-09-08.md")
p<-fread(inputs[1]);e<-fread(inputs[2]);mem<-fread(inputs[3]);frozen<-fread(inputs[4]);g<-fread(inputs[5])
stopifnot(all(vapply(frozen$artifact_path,digest,character(1),file=TRUE,algo="sha256")==frozen$sha256))
for(t in c("primary","ids","secondary","moodys","peer")){a<-e[tier==t];at<-match(paste(p$iso3,p$analysis_year),paste(a$iso3,a$analysis_year));stopifnot(!anyNA(at));set(p,j=t,value=ifelse(a$eligible[at],a$rate_pct[at],NA_real_))}
d<-p[historical_lmic_reporting_scope==TRUE&is.finite(primary)]
universe<-strsplit("AGO ARM BEN BLR BOL BRA CHN CIV CMR COL CRI DOM ECU EGY ETH GAB GHA GIN GTM HND IDN JAM JOR KAZ KEN LBN LKA MAR MEX MNE MNG NGA PAK PAN PER PHL PRY ROU RUS RWA SEN SRB SLV TJK TUR UZB ZAF"," ")[[1]]
stopifnot(setequal(unique(d$iso3),universe),nrow(d)==215)
d[,`:=`(ldc=iso3%in%c("AGO","BEN","ETH","GIN","RWA","SEN"),region=g$region[match(iso3,g$iso3)],era=fcase(analysis_year<=2016,"2012-2016",analysis_year<=2020,"2017-2020",default="2021-2024"))]
d[,`:=`(fixed5=5,fixed10=10,group976=fcase(ldc|historical_income_level=="Low income",9,historical_income_level=="Lower middle income",7,historical_income_level=="Upper middle income",6,default=NA_real_))]
stopifnot(!anyNA(d$group976),!anyNA(d$region),!any(mem$target_iso3==mem$peer_iso3))
tiers<-c("ids","secondary","moodys","peer");policies<-c("fixed5","fixed10","group976")
methods<-c(tiers,policies)
metric<-function(z,m){x<-data.frame(iso3=z$iso3,analysis_year=z$analysis_year,anchor_rate=z$primary,comparison_rate=z[[m]],gap_pp=z[[m]]-z$primary);p15_assessment_metrics(x)}
tab<-list(policy_mapping=d[,.(iso3,country,analysis_year,historical_income_level,ldc,group976,fixed5,fixed10)],
 comparison_inputs=d[,c("iso3","country","analysis_year","region","era","historical_income_level","primary",methods),with=FALSE])
tab$native_summary<-rbindlist(lapply(methods,function(m)cbind(method=m,metric(d[is.finite(get(m))],m))))
common<-d[is.finite(ids)&is.finite(secondary)&is.finite(moodys)&is.finite(peer)]
tab$common_all_tiers_summary<-rbindlist(lapply(methods,function(m)cbind(method=m,metric(common,m))))
paired<-rbindlist(lapply(tiers,function(t)rbindlist(lapply(policies,function(q){z<-copy(d[is.finite(get(t))]);z[,.(iso3,country,analysis_year,region,era,historical_income_level,tier=t,policy=q,primary,
 tier_rate=get(t),policy_rate=get(q),tier_error=get(t)-primary,policy_error=get(q)-primary,
 improvement=abs(get(q)-primary)-abs(get(t)-primary))]}))))
tab$paired_details<-paired
summ<-function(z)data.table(n=nrow(z),countries=uniqueN(z$iso3),tier_mae=mean(abs(z$tier_error)),policy_mae=mean(abs(z$policy_error)),
 mean_improvement=mean(z$improvement),relative_mae_reduction=1-mean(abs(z$tier_error))/mean(abs(z$policy_error)),
 tier_bias=mean(z$tier_error),policy_bias=mean(z$policy_error),win_share=mean(z$improvement>1e-10),tie_share=mean(abs(z$improvement)<=1e-10),
 equal_country_improvement=mean(tapply(z$improvement,z$iso3,mean)))
tab$paired_summary<-paired[,summ(.SD),by=.(tier,policy)]
tab$bootstrap_improvement<-paired[,p15_deep_boot_mean(improvement,iso3),by=.(tier,policy)]
tab$paired_inference<-paired[,p15_assessment_inference(data.frame(iso3=iso3,analysis_year=analysis_year,gap_pp=improvement)),by=.(tier,policy)]
tab$paired_inference[,p_bh:=p.adjust(p_value,method="BH"),by=inference]
for(group in c("analysis_year","era","region","historical_income_level","iso3")){
 bycols<-c("tier","policy",group)
 tab[[paste0("by_",group)]]<-paired[, {z<-copy(.SD);if(group=="iso3")z[,iso3:=.BY$iso3];summ(z)},by=bycols]
}
tab$recent_scope_sensitivity<-paired[analysis_year>=2018&!iso3%in%c("RUS","ROU"),summ(.SD),by=.(tier,policy)]
tab$leave_one_country<-paired[,{
 total<-sum(improvement);nn<-.N;ct<-data.table(iso3=iso3,improvement=improvement)[,.(n=.N,loss_sum=sum(improvement)),by=iso3]
 ct[,`:=`(full_mean=total/nn,without_country=(total-loss_sum)/(nn-n))];ct
},by=.(tier,policy)]
tab$influence_summary<-tab$leave_one_country[,.(full_mean=first(full_mean),minimum_without=min(without_country),maximum_without=max(without_country),
  sign_reversals=sum(sign(without_country)!=sign(full_mean))),by=.(tier,policy)]
tab$leave_one_region<-rbindlist(lapply(unique(paired$region),function(r){z<-paired[region!=r];z[,summ(.SD),by=.(tier,policy)][,region_removed:=r]}))
# Changes: a constant rate predicts zero; compare absolute change errors, not undefined sign correlations.
prev<-copy(paired);prev[,analysis_year:=analysis_year+1L]
chg<-merge(paired[,.(iso3,tier,policy,analysis_year,primary,tier_rate,policy_rate)],prev[,.(iso3,tier,policy,analysis_year,primary_prior=primary,tier_prior=tier_rate,policy_prior=policy_rate)],by=c("iso3","tier","policy","analysis_year"))
chg[,`:=`(primary_change=primary-primary_prior,tier_change=tier_rate-tier_prior,policy_change=policy_rate-policy_prior)]
tab$change_details<-chg
tab$change_summary<-chg[,.(n=.N,countries=uniqueN(iso3),tier_change_mae=mean(abs(tier_change-primary_change)),policy_change_mae=mean(abs(policy_change-primary_change)),
 improvement=mean(abs(policy_change-primary_change)-abs(tier_change-primary_change))),by=.(tier,policy)]
tab$checks<-data.table(check=c("215_primary_47_economies","complete_policy_assignment","no_target_peer_members","common_sample_nonempty","12_paired_comparisons","paired_loss_identity","retained_inputs_unchanged"),
 passed=c(nrow(d)==215&&uniqueN(d$iso3)==47,!anyNA(d$group976),!any(mem$target_iso3==mem$peer_iso3),nrow(common)>0,nrow(tab$paired_summary)==12,
 all(abs(paired$improvement-(abs(paired$policy_error)-abs(paired$tier_error)))<1e-12),all(vapply(frozen$artifact_path,digest,character(1),file=TRUE,algo="sha256")==frozen$sha256)))
stopifnot(all(tab$checks$passed));dir.create(out,recursive=TRUE)
for(n in names(tab))fwrite(tab[[n]],file.path(out,paste0(n,".csv")))
manifest<-function(paths,role)pvr_manifest_rows(paths,role,basename(out),"SCHEMA-P15-POLICY-COMPARISON-V1","EST-P15-PAIRED-PRIMARY-LOSSES-V1","ADM-P15-SOURCE-CLOSURE-V1","SEL-P15-NO-PROMOTION-V1","SRC-P15-RAW-CLOSURE-20260906;SRC-P15-SOURCE-CLOSURE-20260907;SRC-UN-LDC-REVIEW-20260908")
fwrite(manifest(inputs,"policy_comparison_input"),file.path(out,"input_manifest.csv"))
fwrite(manifest(c("scripts/p15/build_p15_policy_comparison.R","R/p15_dataset_assessment.R","R/p15_deeper_assessment.R","R/research_governance.R","R/p15_rating_components.R","R/p15_peer_proxy.R"),"policy_comparison_code"),file.path(out,"script_manifest.csv"))
fwrite(manifest(list.files(out,full.names=TRUE),"policy_comparison_output"),file.path(out,"output_manifest.csv"))
capture.output(sessionInfo(),file=file.path(out,"environment.txt"))
print(tab$paired_summary);print(tab$common_all_tiers_summary[,.(method,n,mae_pp,mean_gap_pp)]);print(tab$checks)
