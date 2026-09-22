source("R/p15_current_inputs.R")
#!/usr/bin/env Rscript
suppressPackageStartupMessages({library(data.table);library(digest)})
source("R/p15_dataset_assessment.R");source("R/p15_deeper_assessment.R");source("R/research_governance.R")
args<-commandArgs(trailingOnly=TRUE);out<-if(length(args))args[1] else file.path("data-derived",paste0("p15_official_policy_",format(Sys.time(),"%Y%m%d_%H%M%S")))
mapping<-if(length(args)>1)args[2] else Sys.getenv("P15_DAC_BASE", p15_current_input("dac"))
if(dir.exists(out))stop("Refusing overwrite")
old<-Sys.getenv("P15_POLICY_BASE", p15_current_input("policy"));base<-Sys.getenv("P15_ANALYSIS_BASE", p15_current_input("dataset"))
inputs<-c(file.path(old,c("comparison_inputs.csv","paired_summary.csv","output_manifest.csv")),file.path(mapping,c("map.csv","output_manifest.csv")),file.path(base,"output_manifest.csv"),"docs/governance/P15_OFFICIAL_DAC_MAPPING_AND_COMPARISON_PROTOCOL_2026-09-08.md")
for(f in inputs[grepl("manifest",inputs)]){a<-fread(f);stopifnot(all(vapply(a$artifact_path,digest,character(1),file=TRUE,algo="sha256")==a$sha256))}
d<-fread(inputs[1]);m<-fread(file.path(mapping,"map.csv"));at<-match(paste(d$iso3,d$analysis_year),paste(m$iso3,m$analysis_year));stopifnot(!anyNA(at))
d[,`:=`(dac_eligible=m$dac_eligible[at],dac_group=m$dac_group[at],dac_group976=m$group_rate_pct[at],dac_headline_new=m$historical_headline_new_loan_rate_pct[at],dac_ge_from2015=m$grant_equivalent_new_loan_rate_pct[at])]
d[,dac_modern_2018:=ifelse(analysis_year>=2018,dac_group976,NA_real_)]
tiers<-c("ids","secondary","moodys","peer");policies<-c("fixed5","fixed10","group976","dac_group976","dac_headline_new","dac_ge_from2015","dac_modern_2018")
metric<-function(z,method)p15_assessment_metrics(data.frame(iso3=z$iso3,analysis_year=z$analysis_year,anchor_rate=z$primary,comparison_rate=z[[method]],gap_pp=z[[method]]-z$primary))
tab<-list(comparison_inputs=d)
tab$native_summary<-rbindlist(lapply(c(tiers,policies),function(a)cbind(method=a,metric(d[is.finite(get(a))],a))))
tab$common_all_tiers_summary<-rbindlist(lapply(policies,function(q){z<-d[is.finite(ids)&is.finite(secondary)&is.finite(moodys)&is.finite(peer)&is.finite(get(q))];rbindlist(lapply(c(tiers,q),function(a)cbind(policy=q,method=a,metric(z,a))))}))
paired<-rbindlist(lapply(tiers,function(t)rbindlist(lapply(policies,function(q){z<-d[is.finite(get(t))&is.finite(get(q))];z[,.(iso3,country,analysis_year,region,era,historical_income_level,dac_group,tier=t,policy=q,primary,tier_rate=get(t),policy_rate=get(q),old_group_rate=group976,tier_error=get(t)-primary,policy_error=get(q)-primary,improvement=abs(get(q)-primary)-abs(get(t)-primary))]}))))
tab$paired_details<-paired
summ<-function(z)data.table(n=nrow(z),countries=uniqueN(z$iso3),tier_mae=mean(abs(z$tier_error)),policy_mae=mean(abs(z$policy_error)),mean_improvement=mean(z$improvement),relative_mae_reduction=1-mean(abs(z$tier_error))/mean(abs(z$policy_error)),tier_bias=mean(z$tier_error),policy_bias=mean(z$policy_error),win_share=mean(z$improvement>1e-10),tie_share=mean(abs(z$improvement)<=1e-10),equal_country_improvement=mean(tapply(z$improvement,z$iso3,mean)))
tab$paired_summary<-paired[,summ(.SD),by=.(tier,policy)]
tab$bootstrap_improvement<-paired[,p15_deep_boot_mean(improvement,iso3),by=.(tier,policy)]
tab$paired_inference<-paired[,p15_assessment_inference(data.frame(iso3=iso3,analysis_year=analysis_year,gap_pp=improvement)),by=.(tier,policy)]
tab$paired_inference[,p_bh_28:=p.adjust(p_value,method="BH"),by=inference]
for(group in c("analysis_year","era","region","historical_income_level","iso3")){
 tab[[paste0("by_",group)]]<-paired[,{z<-copy(.SD);if(group=="iso3")z[,iso3:=.BY$iso3];summ(z)},by=c("tier","policy",group)]
}
tab$matched_assignment_effect<-paired[policy%in%c("dac_group976","dac_modern_2018"),.(n=.N,old_policy_mae=mean(abs(old_group_rate-primary)),official_policy_mae=mean(abs(policy_error)),tier_mae=mean(abs(tier_error)),old_improvement=mean(abs(old_group_rate-primary)-abs(tier_error)),official_improvement=mean(improvement),category_loss_change=mean(abs(policy_error)-abs(old_group_rate-primary))),by=.(tier,policy)]
# Recalculate country removals explicitly, without evaluating any changed model.
tab$leave_one_country<-paired[,{total<-sum(improvement);nn<-.N;ct<-data.table(iso3,improvement)[,.(n=.N,loss_sum=sum(improvement)),by=iso3];ct[,`:=`(full_mean=total/nn,without_country=(total-loss_sum)/(nn-n))];ct},by=.(tier,policy)]
tab$leave_one_region<-rbindlist(lapply(unique(paired$region),function(r)paired[region!=r,summ(.SD),by=.(tier,policy)][,region_removed:=r]))
prev<-copy(paired);prev[,analysis_year:=analysis_year+1L]
chg<-merge(paired[,.(iso3,tier,policy,analysis_year,primary,tier_rate,policy_rate)],prev[,.(iso3,tier,policy,analysis_year,primary_prior=primary,tier_prior=tier_rate,policy_prior=policy_rate)],by=c("iso3","tier","policy","analysis_year"))
chg[,`:=`(primary_change=primary-primary_prior,tier_change=tier_rate-tier_prior,policy_change=policy_rate-policy_prior,regime_boundary=analysis_year==2018)]
tab$change_details<-chg
cs<-function(z)data.table(n=nrow(z),countries=uniqueN(z$iso3),tier_change_mae=mean(abs(z$tier_change-z$primary_change)),policy_change_mae=mean(abs(z$policy_change-z$primary_change)),no_change_mae=mean(abs(z$primary_change)))
tab$change_summary<-chg[,cs(.SD),by=.(tier,policy)]
tab$change_excluding_2018_boundary<-chg[regime_boundary==FALSE,cs(.SD),by=.(tier,policy)]
prior<-fread(file.path(old,"paired_summary.csv"));cmp<-merge(prior,tab$paired_summary,by=c("tier","policy"),suffixes=c("_old","_new"))
checks<-data.table(check=c("215_primary_rows","209_dac_eligible","28_paired_tests","unchanged_previous_12_losses","no_ineligible_official_pairs","all28_finite_loss_summaries","original_candidate_unchanged"),passed=c(nrow(d)==215,sum(d$dac_eligible)==209,nrow(tab$paired_summary)==28,nrow(cmp)==12&&all(abs(cmp$mean_improvement_old-cmp$mean_improvement_new)<1e-12)&&all(cmp$n_old==cmp$n_new),all(paired[grepl("^dac_",policy),dac_group]!="not_on_dac_list"),all(is.finite(tab$paired_summary$mean_improvement)),{a<-fread(file.path(base,"output_manifest.csv"));all(vapply(a$artifact_path,digest,character(1),file=TRUE,algo="sha256")==a$sha256)}))
tab$checks<-checks;stopifnot(all(checks$passed));dir.create(out,recursive=TRUE)
for(n in names(tab))fwrite(tab[[n]],file.path(out,paste0(n,".csv")))
mm<-function(paths,role)pvr_manifest_rows(paths,role,basename(out),"SCHEMA-P15-OFFICIAL-POLICY-COMPARISON-V1","EST-P15-PAIRED-PRIMARY-LOSSES-V1","ADM-P15-DAC-ELIGIBILITY-V1","SEL-P15-NO-PROMOTION-V1","SRC-OECD-DAC-LISTS-20260908;SRC-P15-SOURCE-CLOSURE-20260907")
fwrite(mm(inputs,"input"),file.path(out,"input_manifest.csv"))
fwrite(mm(c("scripts/p15/build_p15_official_policy_comparison.R","R/p15_dataset_assessment.R","R/p15_deeper_assessment.R","R/research_governance.R"),"code"),file.path(out,"script_manifest.csv"))
fwrite(mm(list.files(out,full.names=TRUE),"output"),file.path(out,"output_manifest.csv"))
capture.output(sessionInfo(),file=file.path(out,"environment.txt"))
print(tab$paired_summary[grepl("dac_",policy),.(tier,policy,n,tier_mae,policy_mae,relative_mae_reduction)]);print(checks)
