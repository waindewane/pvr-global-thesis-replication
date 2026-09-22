#!/usr/bin/env Rscript
suppressPackageStartupMessages({library(data.table);library(ggplot2);library(digest)})
source("R/p15_dataset_assessment.R")
source("R/p15_deeper_assessment.R")
source("R/research_governance.R")
args<-commandArgs(trailingOnly=TRUE)
out<-if(length(args))args[1] else "data-derived/p15_deeper_assessment_20260908_v4"
if(dir.exists(out))stop("Use a new diagnostic directory")
base<-Sys.getenv("P15_ANALYSIS_BASE","data-derived/p15_analysis_candidate_20260907_v1")
prior<-Sys.getenv("P15_ASSESSMENT_BASE","data-derived/p15_dataset_assessment_20260908_v4")
inputs<-character()
read<-function(path){inputs<<-unique(c(inputs,path));fread(path)}
p<-read(file.path(base,"core_evidence.csv"));e<-read(file.path(base,"tier_eligibility.csv"))
s<-read(file.path(base,"selected_reference.csv"));members<-read(file.path(base,"peer_membership.csv"))
pair<-read(file.path(prior,"pair_details.csv"));bridge<-read(file.path(prior,"source_switch_details.csv"))
manifest<-read(file.path(base,"output_manifest.csv"))
stopifnot(all(vapply(manifest$artifact_path,digest,character(1),file=TRUE,algo="sha256")==manifest$sha256))
tiers<-c("primary","ids","secondary","moodys","peer")
for(t in tiers){z<-e[tier==t];at<-match(paste(p$iso3,p$analysis_year),paste(z$iso3,z$analysis_year));
  stopifnot(!anyNA(at));set(p,j=t,value=ifelse(z$eligible[at],z$rate_pct[at],NA_real_))}
p<-merge(p,s[,.(iso3,analysis_year,selected_tier,selected_rate_pct)],by=c("iso3","analysis_year"))
p[,era:=fcase(analysis_year<=2016,"2012-2016",analysis_year<=2020,"2017-2020",default="2021-2024")]
setorder(p,iso3,analysis_year)
tab<-list()
# 1. Source switches: both arithmetic bridges, not an invented causal allocation.
bridge[,old_opposite:=p15_deep_sign_disagreement(total_change_pp,old_source_within_change_pp)]
bridge[,new_opposite:=p15_deep_sign_disagreement(total_change_pp,new_source_within_change_pp)]
bridge[,any_opposite:=(old_opposite%in%TRUE)|(new_opposite%in%TRUE)]
bridge[,both_opposite:=old_opposite%in%TRUE&new_opposite%in%TRUE]
bridge[,both_available:=old_source_bridge_available&new_source_bridge_available]
bridge[,`:=`(old_source_available_now=is.finite(old_source_within_change_pp),
  new_source_available_previous=is.finite(new_source_within_change_pp))]
tab$switch_details<-bridge
tab$switch_direction_summary<-bridge[,.(n=.N,old_bridge=sum(old_source_bridge_available),new_bridge=sum(new_source_bridge_available),
  either_bridge=sum(old_source_bridge_available|new_source_bridge_available),both_bridge=sum(both_available),
  old_opposite=sum(old_opposite%in%TRUE),new_opposite=sum(new_opposite%in%TRUE),
  any_opposite=sum(any_opposite),both_opposite=sum(both_opposite),
  old_gap_exceeds_within=sum(abs(source_difference_current_year_pp)>abs(old_source_within_change_pp),na.rm=TRUE),
  new_gap_exceeds_within=sum(abs(source_difference_previous_year_pp)>abs(new_source_within_change_pp),na.rm=TRUE))]
tab$switch_by_transition<-bridge[,.(n=.N,mean_abs_selected=mean(abs(total_change_pp)),
  any_opposite=sum(any_opposite),old_available=sum(old_source_available_now),new_available_before=sum(new_source_available_previous)),
  by=.(previous_tier,selected_tier)][order(-n)]
tab$largest_direction_reversals<-bridge[any_opposite==TRUE][order(-abs(total_change_pp))]
tr<-copy(p)
for(v in c(tiers,"analysis_year","selected_rate_pct","selected_tier","historical_lmic_reporting_scope"))
  set(tr,j=paste0(v,"_lag"),value=tr[,shift(get(v)),by=iso3]$V1)
tr<-tr[historical_lmic_reporting_scope==TRUE&historical_lmic_reporting_scope_lag==TRUE&
  analysis_year-analysis_year_lag==1&is.finite(selected_rate_pct)&is.finite(selected_rate_pct_lag)]
tr[,`:=`(switch=selected_tier!=selected_tier_lag,selected_change=selected_rate_pct-selected_rate_pct_lag)]
bothcountry<-tr[,.(has_both=uniqueN(switch)==2),by=iso3][has_both==TRUE,iso3]
cw<-dcast(tr[iso3%in%bothcountry,.(mean_abs=mean(abs(selected_change))),by=.(iso3,switch)],iso3~switch,value.var="mean_abs")
setnames(cw,c("FALSE","TRUE"),c("no_switch_mean_abs","switch_mean_abs"))
cw[,difference:=switch_mean_abs-no_switch_mean_abs]
tab$switch_within_country<-cw
tab$switch_within_country_summary<-p15_deep_boot_mean(cw$difference,cw$iso3)
tab$switch_regression<-rbindlist(lapply(c("abs(selected_change) ~ switch","abs(selected_change) ~ switch + factor(iso3) + factor(analysis_year)"),function(f){
  fit<-lm(as.formula(f),data=tr);v<-sandwich::vcovCL(fit,cluster=list(tr$iso3,tr$analysis_year),type="HC1",multi0=TRUE)
  nm<-"switchTRUE";vv<-v[nm,nm];se<-if(vv>0)sqrt(vv) else NA_real_;est<-coef(fit)[nm]
  data.table(formula=f,n=nrow(tr),estimate=est,se=se,low=est-qt(.975,11)*se,high=est+qt(.975,11)*se,
    interpretation="descriptive conditional association, not causal")
}))
tab$within_tier_changes<-rbindlist(lapply(tiers,function(t){
  d<-tr[is.finite(get(t))&is.finite(get(paste0(t,"_lag")))]
  delta<-d[[t]]-d[[paste0(t,"_lag")]]
  data.table(tier=t,n=nrow(d),mean_abs=mean(abs(delta)),median_abs=median(abs(delta)),
    corr_with_selected=cor(delta,d$selected_change),opposite=sum(p15_deep_sign_disagreement(d$selected_change,delta)%in%TRUE))
}))
# Changes in two source tiers on the exact same consecutive observations.
tab$paired_changes<-rbindlist(lapply(combn(tiers,2,simplify=FALSE),function(ts){
  keep<-Reduce(`&`,lapply(c(ts,paste0(ts,"_lag")),function(v)is.finite(tr[[v]])))
  d<-tr[keep];a<-d[[ts[1]]]-d[[paste0(ts[1],"_lag")]];b<-d[[ts[2]]]-d[[paste0(ts[2],"_lag")]]
  data.table(pair=paste(ts,collapse="__"),n=nrow(d),countries=uniqueN(d$iso3),
    correlation=cor(a,b),mean_difference=mean(b-a),mae_difference=mean(abs(b-a)),
    opposite=sum(p15_deep_sign_disagreement(a,b)%in%TRUE))
}))

# 2. Country and year effects are descriptive fits to gaps, not new predictions.
pr<-copy(pair[anchor=="primary"])
tab$gap_variation_fits<-rbindlist(lapply(unique(pr$pair),function(k){
  d<-pr[pair==k];fits<-list(intercept=lm(gap_pp~1,d),country=lm(gap_pp~factor(iso3),d),
    year=lm(gap_pp~factor(analysis_year),d),country_year=lm(gap_pp~factor(iso3)+factor(analysis_year),d))
  rbindlist(lapply(names(fits),function(n){fit<-fits[[n]];z<-summary(fit)
    data.table(pair=k,fit=n,n=nrow(d),parameters=fit$rank,r_squared=z$r.squared,adjusted_r_squared=z$adj.r.squared,
      residual_rmse=sqrt(mean(residuals(fit)^2))) }))
}))
pr[,year_adjusted_gap:=gap_pp-mean(gap_pp),by=.(pair,analysis_year)]
per<-pr[,.(n=.N,mean_gap=mean(gap_pp),year_adjusted_mean=mean(year_adjusted_gap)),by=.(pair,iso3,era)]
tab$country_period_gaps<-per
tab$common_country_era_change<-rbindlist(lapply(unique(pr$pair),function(k){
  a<-per[pair==k&era=="2012-2016"];b<-per[pair==k&era=="2021-2024"]
  z<-merge(a,b,by=c("pair","iso3"));z[,gap_change:=mean_gap.y-mean_gap.x]
  z
}))
tab$common_country_era_summary<-rbindlist(lapply(unique(pr$pair),function(k){
  z<-tab$common_country_era_change[pair==k]
  rbindlist(lapply(c(1L,2L),function(minyears){d<-z[n.x>=minyears&n.y>=minyears]
    cbind(pair=k,minimum_years_each_period=minyears,p15_deep_boot_mean(d$gap_change,d$iso3))}))
}))
tab$country_pattern_repeat<-tab$common_country_era_change[n.x>=2&n.y>=2,
  .(n=.N,raw_correlation=cor(mean_gap.x,mean_gap.y),year_adjusted_correlation=cor(year_adjusted_mean.x,year_adjusted_mean.y),
    raw_same_sign=mean(sign(mean_gap.x)==sign(mean_gap.y)),
    adjusted_same_sign=mean(sign(year_adjusted_mean.x)==sign(year_adjusted_mean.y))),by=pair]

# 3. Matched model tests against a deliberately simple target-excluded comparator.
model<-copy(p[historical_lmic_reporting_scope==TRUE&is.finite(primary)&is.finite(moodys)&is.finite(peer)])
model[,global_median:=vapply(seq_len(.N),function(i){yr<-analysis_year[i];id<-iso3[i];median(p[analysis_year==yr&iso3!=id,primary],na.rm=TRUE)},numeric(1))]
tab$model_common_details<-model[,.(iso3,analysis_year,primary,moodys,peer,global_median)]
tab$model_common_summary<-rbindlist(lapply(c("moodys","peer","global_median"),function(m){
  z<-data.frame(iso3=model$iso3,analysis_year=model$analysis_year,anchor_rate=model$primary,
    comparison_rate=model[[m]],gap_pp=model[[m]]-model$primary)
  cbind(method=m,p15_assessment_metrics(z))
}))
tab$paired_model_losses<-rbindlist(lapply(combn(c("moodys","peer","global_median"),2,simplify=FALSE),function(ts){
  rbindlist(lapply(c("absolute","squared"),function(loss){
    power<-if(loss=="absolute")1 else 2
    delta<-abs(model[[ts[2]]]-model$primary)^power-abs(model[[ts[1]]]-model$primary)^power
    cbind(comparison=paste(ts,collapse="__"),loss=loss,p15_deep_boot_mean(delta,model$iso3))
  }))
}))
tab$paired_model_losses_two_way<-rbindlist(lapply(combn(c("moodys","peer","global_median"),2,simplify=FALSE),function(ts){
  d<-data.frame(iso3=model$iso3,analysis_year=model$analysis_year,
    gap_pp=abs(model[[ts[2]]]-model$primary)-abs(model[[ts[1]]]-model$primary))
  cbind(comparison=paste(ts,collapse="__"),p15_assessment_inference(d))
}))
model[,yield_band:=cut(primary,breaks=c(-Inf,4,6,8,Inf),labels=c("Up to 4%","4-6%","6-8%","Above 8%"))]
tab$paired_model_losses_two_way[,p_bh:=p.adjust(p_value,method="BH"),by=inference]
tab$model_by_yield_band<-rbindlist(lapply(c("moodys","peer","global_median"),function(m){
  model[,.(method=m,n=.N,mean_primary=mean(primary),mean_prediction=mean(get(m)),
    bias=mean(get(m)-primary),mae=mean(abs(get(m)-primary))),by=yield_band]
}))
ranked<-rbindlist(lapply(c("moodys","peer"),function(m){
  model[,{
    observed_top<-primary>=quantile(primary,.75)
    predicted_top<-get(m)>=quantile(get(m),.75)
    .(method=m,n=.N,observed_top=sum(observed_top),predicted_top=sum(predicted_top),
      overlap=sum(observed_top&predicted_top),spearman=if(sd(get(m))>0)cor(primary,get(m),method="spearman") else NA_real_)
  },by=analysis_year]
}))
tab$within_year_rank<-ranked
tab$within_year_rank_summary<-ranked[n>=8,.(years=.N,n=sum(n),top_recall=sum(overlap)/sum(observed_top),
  top_precision=sum(overlap)/sum(predicted_top),mean_spearman=mean(spearman,na.rm=TRUE)),by=method]
rank_compare<-dcast(ranked[n>=8],analysis_year~method,value.var="spearman")
tab$within_year_rank_difference<-rank_compare
tab$within_year_rank_difference_summary<-data.table(years=nrow(rank_compare),
  moodys_higher=sum(rank_compare$moodys>rank_compare$peer),mean_difference=mean(rank_compare$moodys-rank_compare$peer))

# 4. Tail influence and shared-object consistency, retaining the full inputs.
tab$tail_influence<-rbindlist(lapply(unique(pair$pair),function(k){d<-copy(pair[pair==k]);setorder(d,-gap_pp)
  d<-d[order(-abs(gap_pp))];n<-nrow(d)
  rbindlist(lapply(c(0,.01,.05,.1),function(fr){dropn<-ceiling(fr*n);keep<-seq_len(n)>dropn
    data.table(pair=k,removed_fraction=fr,removed_n=dropn,n_retained=sum(keep),mean_gap=mean(d$gap_pp[keep]),
      mae=mean(abs(d$gap_pp[keep])),rmse=sqrt(mean(d$gap_pp[keep]^2)),
      removed_abs_share=sum(abs(d$gap_pp[!keep]))/sum(abs(d$gap_pp)),
      removed_squared_share=sum(d$gap_pp[!keep]^2)/sum(d$gap_pp^2))}))
}))
tab$country_loss_contribution<-pair[,.(n=.N,absolute_loss=sum(abs(gap_pp)),squared_loss=sum(gap_pp^2)),by=.(pair,iso3,country)]
tab$country_loss_contribution[,`:=`(absolute_share=absolute_loss/sum(absolute_loss),squared_share=squared_loss/sum(squared_loss)),by=pair]
ip<-p[historical_lmic_reporting_scope==TRUE&is.finite(primary)&is.finite(ids)]
ip[,`:=`(rate_gap=ids-primary,maturity_gap=ids_maturity_years-primary_usd_market_maturity_years)]
tab$ids_primary_consistency<-ip[,.(n=.N,within_001=sum(abs(rate_gap)<=.01),within_005=sum(abs(rate_gap)<=.05),
  within_025=sum(abs(rate_gap)<=.25),both_rate005_maturity1=sum(abs(rate_gap)<=.05&abs(maturity_gap)<=1,na.rm=TRUE),
  maturity_available=sum(is.finite(maturity_gap)),maturity_within1=sum(abs(maturity_gap)<=1,na.rm=TRUE),
  rate_exact_4decimals=sum(round(primary,4)==round(ids,4)))]
tab$ids_primary_details<-ip[,.(iso3,analysis_year,primary,ids,rate_gap,ids_maturity_years,primary_usd_market_maturity_years,maturity_gap)]

# 5. Descriptive relation of timing and maturity mismatch to observed-source gaps.
sp<-copy(pair[pair=="primary__secondary"])
sp[,`:=`(date_separation=as.numeric(secondary_usd_first_rate_date-primary_usd_last_rate_date)/365.25,
  maturity_difference=secondary_usd_market_maturity_years-primary_usd_market_maturity_years)]
sp[,`:=`(absolute_gap=abs(gap_pp),absolute_maturity_difference=abs(maturity_difference))]
sp[,close_dates:=date_separation<=90/365.25&date_separation>=0]
sp[,close_maturity:=absolute_maturity_difference<=3]
tab$timing_maturity_sensitivity<-rbindlist(lapply(c("all","without_Lebanon"),function(sample){
  d<-if(sample=="all")sp else sp[iso3!="LBN"]
  d[,.(sample=sample,n=.N,bias=mean(gap_pp),mae=mean(absolute_gap)),by=.(close_dates,close_maturity)]
}))
tab$timing_maturity_regressions<-rbindlist(lapply(c("all","without_Lebanon"),function(sample){
  d<-if(sample=="all")sp else sp[iso3!="LBN"]
  rbindlist(lapply(c("absolute_gap ~ date_separation + absolute_maturity_difference",
    "absolute_gap ~ date_separation + absolute_maturity_difference + factor(iso3) + factor(analysis_year)"),function(f){
    fit<-lm(as.formula(f),d);v<-sandwich::vcovCL(fit,cluster=list(d$iso3,d$analysis_year),type="HC1",multi0=TRUE)
    nm<-c("date_separation","absolute_maturity_difference");vv<-diag(v)[nm];se<-ifelse(vv>0,sqrt(pmax(vv,0)),NA_real_)
    data.table(sample=sample,formula=f,term=nm,n=nrow(d),estimate=coef(fit)[nm],se=se,
      low=coef(fit)[nm]-qt(.975,12)*se,high=coef(fit)[nm]+qt(.975,12)*se,r_squared=summary(fit)$r.squared)
  }))
}))

# 6. Shared pools and sensitivity to one observed seed in fixed memberships.
selpeer<-s[historical_lmic_reporting_scope==TRUE&selected_tier=="peer",.(analysis_year,target_iso3=iso3)]
mm<-merge(members,selpeer,by=c("analysis_year","target_iso3"))
pool<-mm[,.(signature=paste(sort(peer_iso3),collapse=";"),n_peers=.N,
  hic_share=mean(peer_income=="High income"),minimum_peer_rate=min(peer_rate_pct),maximum_peer_rate=max(peer_rate_pct)),by=.(analysis_year,target_iso3)]
tab$actual_peer_pool_details<-pool
groups<-pool[,.N,by=.(analysis_year,signature)]
tab$shared_pool_summary<-pool[,.(targets=.N,unique_pools=uniqueN(signature),
  mean_hic_share=mean(hic_share),targets_with_hic=sum(hic_share>0),median_seed_count=as.numeric(median(n_peers))),by=analysis_year]
tab$shared_pool_groups<-groups[order(-N)]
rem<-mm[,{
  q<-p15_deep_pool_removal(peer_rate_pct)
  cbind(peer_iso3=peer_iso3,q)
},by=.(analysis_year,target_iso3)]
tab$peer_delete_one_details<-rem
tab$peer_delete_one_target<-rem[,.(minimum_still_met=all(minimum_still_met),
  max_abs_change=max(abs(change_pp)),spread_of_medians=max(arithmetic_median_after_removal)-min(arithmetic_median_after_removal)),by=.(analysis_year,target_iso3)]
tab$peer_delete_one_summary<-tab$peer_delete_one_target[,.(n=.N,median_max_change=median(max_abs_change),
  over05=sum(max_abs_change>.5),over1=sum(max_abs_change>1),maximum=max(max_abs_change)),by=minimum_still_met]
tab$seed_usage<-mm[,.(target_year_links=.N,distinct_targets=uniqueN(target_iso3),years=uniqueN(analysis_year)),by=peer_iso3][order(-target_year_links)]
tab$seed_deletion_influence<-rem[minimum_still_met==TRUE,.(affected=sum(abs(change_pp)>1e-10),
  mean_abs_change=mean(abs(change_pp)),maximum=max(abs(change_pp))),by=peer_iso3][order(-affected)]
pt<-tr[selected_tier=="peer"&selected_tier_lag=="peer"]
tab$peer_membership_turnover<-rbindlist(lapply(seq_len(nrow(pt)),function(i){
  id<-pt$iso3[i];yr<-pt$analysis_year[i]
  a<-mm[target_iso3==id&analysis_year==yr-1L];b<-mm[target_iso3==id&analysis_year==yr]
  common<-intersect(a$peer_iso3,b$peer_iso3)
  av<-median(a$peer_rate_pct);bv<-median(b$peer_rate_pct)
  ac<-if(length(common))median(a[peer_iso3%in%common,peer_rate_pct]) else NA_real_
  bc<-if(length(common))median(b[peer_iso3%in%common,peer_rate_pct]) else NA_real_
  data.table(iso3=id,analysis_year=yr,previous_peers=nrow(a),current_peers=nrow(b),common_peers=length(common),
    jaccard=length(common)/length(union(a$peer_iso3,b$peer_iso3)),
    previous_rate=av,current_rate=bv,total_change=bv-av,common_peer_change=bc-ac,
    current_composition=bv-bc,previous_composition=ac-av,
    opposite=p15_deep_sign_disagreement(bv-av,bc-ac))
}))
tab$peer_turnover_summary<-tab$peer_membership_turnover[,.(n=.N,identical_membership=sum(jaccard==1),
  median_jaccard=median(jaccard),mean_abs_change=mean(abs(total_change)),
  opposite=sum(opposite%in%TRUE),median_abs_composition=median(abs(current_composition+previous_composition),na.rm=TRUE)),
  by=.(at_least_three_common=common_peers>=3)]
tab$paired_changes_common_models<-rbindlist(lapply(c("moodys","peer"),function(t){
  d<-tr[is.finite(primary)&is.finite(primary_lag)&is.finite(moodys)&is.finite(moodys_lag)&is.finite(peer)&is.finite(peer_lag)]
  a<-d$primary-d$primary_lag;b<-d[[t]]-d[[paste0(t,"_lag")]]
  data.table(method=t,n=nrow(d),correlation=cor(a,b),mae=mean(abs(b-a)),opposite=sum(p15_deep_sign_disagreement(a,b)%in%TRUE))
}))

# 7. Exact composition accounting of annual selected means.
tab$annual_composition<-rbindlist(lapply(2013:2024,function(yr){
  a<-s[analysis_year==yr-1&historical_lmic_reporting_scope==TRUE&is.finite(selected_rate_pct)]
  b<-s[analysis_year==yr&historical_lmic_reporting_scope==TRUE&is.finite(selected_rate_pct)]
  cbind(analysis_year=yr,p15_deep_composition(a$selected_rate_pct,b$selected_rate_pct,a$iso3,b$iso3))
}))
tab$annual_exits<-rbindlist(lapply(2013:2024,function(yr){
  a<-s[analysis_year==yr-1&historical_lmic_reporting_scope==TRUE&is.finite(selected_rate_pct)]
  b<-s[analysis_year==yr&historical_lmic_reporting_scope==TRUE&is.finite(selected_rate_pct)]
  common_ids<-intersect(a$iso3,b$iso3)
  exited<-a[!iso3%in%common_ids]
  future<-p[analysis_year==yr,.(iso3,next_income=historical_income_level,next_lmic=historical_lmic_reporting_scope,
    next_selected_tier=selected_tier,next_selected_rate=selected_rate_pct,
    next_status_class=approved_status_rule_class,next_classification_state=classification_review_state)]
  z<-merge(exited[,.(iso3,country,previous_rate=selected_rate_pct)],future,by="iso3")
  if(!nrow(z))return(NULL)
  z[,`:=`(analysis_year=yr,composition_contribution=(mean(a[iso3%in%common_ids,selected_rate_pct])-previous_rate)/nrow(a))]
  z
}))
tab$annual_fixed_source_changes<-rbindlist(lapply(tiers,function(t){
  tr[is.finite(get(t))&is.finite(get(paste0(t,"_lag"))),
    .(tier=t,n=.N,mean_change=mean(get(t)-get(paste0(t,"_lag")))),by=analysis_year]
}))
tab$income_scope_sensitivity<-rbindlist(lapply(c("historical_lmic_reporting_scope","static_2024_lmic_scope_audit_only"),function(scope){
  p[get(scope)==TRUE,.(scope=scope,grid=.N,selected=sum(is.finite(selected_rate_pct)),
    mean=mean(selected_rate_pct,na.rm=TRUE),peer_selected=sum(selected_tier=="peer")),by=analysis_year]
}))

checks<-data.table(check=c("source_switch_334","opposite_is_subset","common_model_199","pool_targets_778",
  "composition_identity","no_self_peer","all_ten_tail_pairs"),passed=c(nrow(bridge)==334,
  sum(bridge$any_opposite)<=sum(bridge$old_source_bridge_available|bridge$new_source_bridge_available),
  nrow(model)==199,nrow(pool)==778,
  all(abs(with(tab$annual_composition,total_mean_change-common_country_change-current_composition-prior_composition))<1e-10),
  all(mm$target_iso3!=mm$peer_iso3),uniqueN(tab$tail_influence$pair)==10))
checks<-rbind(checks,data.table(check=c("peer_transition_667","common_peer_composition_identity"),
  passed=c(nrow(tab$peer_membership_turnover)==667,
    all(abs(with(tab$peer_membership_turnover,total_change-common_peer_change-current_composition-previous_composition))<1e-10,na.rm=TRUE))))
tab$checks<-checks
stopifnot(all(checks$passed))
dir.create(out,recursive=TRUE)
for(n in names(tab))fwrite(tab[[n]],file.path(out,paste0(n,".csv")),na="")

theme_set(theme_minimal(base_size=11)+theme(panel.grid.minor=element_blank(),plot.caption=element_text(hjust=0,size=8),legend.position="bottom"))
fig<-function(name,g,w=9,h=5){ggsave(file.path(out,paste0(name,".png")),g,width=w,height=h,dpi=160,bg="white")
  ggsave(file.path(out,paste0(name,".pdf")),g,width=w,height=h,device="pdf",useDingbats=FALSE)}
example<-data.table(year=c(2022,2023,2022,2023),series=rep(c("Selected rate","Secondary market"),each=2),
  rate=c(p[iso3=="PAK"&analysis_year==2022,selected_rate_pct],p[iso3=="PAK"&analysis_year==2023,selected_rate_pct],
    p[iso3=="PAK"&analysis_year==2022,secondary],p[iso3=="PAK"&analysis_year==2023,secondary]))
fig("figure_1_pakistan_source_switch",ggplot(example,aes(year,rate,color=series,group=series))+geom_line(linewidth=1)+geom_point(size=3)+
  geom_text(aes(label=sprintf("%.2f%%",rate)),nudge_x=-.045,hjust=1,show.legend=FALSE)+
  scale_x_continuous(breaks=c(2022,2023),limits=c(2021.75,2023.1))+scale_color_manual(values=c("#2F6B9A","#B45A4A"))+
  labs(title="Pakistan: the selected rate rises while the secondary yield falls",x=NULL,y="Rate (%)",color=NULL,
    caption="2022 selected: IDS Bondholders. 2023 selected: secondary. Both series end at 18.99%."))
fg<-melt(tab$model_by_yield_band,id.vars=c("method","yield_band","n"),measure.vars="bias")
method_labels<-c(moodys="Moody's estimate",peer="Peer estimate",global_median="Global median baseline")
fg[,method:=method_labels[method]]
fig("figure_2_model_yield_range",ggplot(fg,aes(yield_band,value,color=method,group=method))+geom_hline(yintercept=0,color="grey60")+
  geom_point(position=position_dodge(.3),size=2)+labs(title="Where estimates are higher or lower than actual issuance yields",
    x="Observed primary yield band",y="Mean estimate minus primary (percentage points)",color=NULL,
    caption="Same 199 country-years. Outcome-defined bins are descriptive, not an independent calibration test."))
fd<-tab$common_country_era_summary[minimum_years_each_period==2&is.finite(mean)]
fd[,pair:=c(primary__ids="IDS minus primary",primary__moodys="Moody's minus primary",primary__peer="Peer minus primary")[pair]]
fig("figure_3_same_country_period_change",ggplot(fd,aes(mean,pair))+geom_vline(xintercept=0,color="grey60")+
  geom_segment(aes(x=low,xend=high,yend=pair))+geom_point(color="#2F6B9A",size=2)+
  labs(title="Do period differences remain when comparing the same countries?",x="Change in comparator-primary gap: late minus early (percentage points)",y=NULL,
    caption="At least 2 observations in each period; equal country weights; country-bootstrap intervals.\nSecondary: no countries meet this repeated-period requirement; not estimable."))
pc<-tab$country_loss_contribution[pair=="secondary__peer"][order(-squared_share)][1:8]
fig("figure_4_secondary_peer_tail",ggplot(pc,aes(squared_share,reorder(country,squared_share)))+geom_point(size=2,color="#2F6B9A")+
  scale_x_continuous(labels=scales::label_percent())+labs(title="Which countries dominate secondary-versus-peer discrepancies?",x="Share of total squared discrepancy",y=NULL,
    caption="All eligible 2012-2024 LMIC pairs. Largest eight country contributions; no observations removed."))
dep<-tab$shared_pool_summary
fdep<-melt(dep,id.vars="analysis_year",measure.vars=c("targets","unique_pools"))
fdep[,variable:=c(targets="Country-years receiving peer estimates",unique_pools="Distinct peer membership sets")[as.character(variable)]]
fig("figure_5_peer_shared_information",ggplot(fdep,aes(analysis_year,value,color=variable))+geom_line()+geom_point()+
  scale_x_continuous(breaks=c(2012,2016,2020,2024))+labs(title="Many peer estimates reuse the same group of observed borrowers",x=NULL,y="Number",color=NULL,
    caption="Actual selected LMIC peer targets versus distinct membership sets. Distinct sets can still share seeds."))
fig("figure_6_annual_composition",ggplot(tab$annual_composition,aes(analysis_year))+geom_hline(yintercept=0,color="grey60")+
  geom_line(aes(y=total_mean_change,color="Change in annual average"))+
  geom_line(aes(y=common_country_change,color="Change among continuing countries"))+geom_point(aes(y=total_mean_change),size=1)+
  scale_x_continuous(breaks=c(2013,2016,2020,2024))+labs(title="How much do changing country samples alter the aggregate trend?",x=NULL,y="Annual change (percentage points)",color=NULL,
    caption="Selected reference rates; historical LMIC scope. Continuing-country changes still include tier switches."))
inputs<-c(inputs,"docs/governance/P15_DEEPER_ASSESSMENT_PROTOCOL_2026-09-08.md","renv.lock")
bundle<-list(build_id=basename(out),schema_id="SCHEMA-P15-DEEPER-ASSESSMENT-V1",estimator_id="EST-P15-DESCRIPTIVE-DEEPER-V1",
  admissibility_id="ADM-P15-SOURCE-CLOSURE-V1",selection_id="SEL-P15-NO-PROMOTION-V1",
  source_package_ids=paste(unique(s$source_package_ids),collapse=";"))
make_manifest<-function(paths,role)do.call(pvr_manifest_rows,c(list(paths=paths,artifact_role=role),bundle))
fwrite(make_manifest(inputs,"assessment_input"),file.path(out,"input_manifest.csv"))
fwrite(make_manifest(c("R/p15_deeper_assessment.R","R/p15_dataset_assessment.R","R/research_governance.R","scripts/p15/build_p15_deeper_assessment.R"),"assessment_code"),file.path(out,"script_manifest.csv"))
fwrite(make_manifest(list.files(out,pattern="[.](csv|png|pdf)$",full.names=TRUE),"diagnostic_output"),file.path(out,"output_manifest.csv"))
capture.output(sessionInfo(),file=file.path(out,"environment.txt"))
stopifnot(all(vapply(manifest$artifact_path,digest,character(1),file=TRUE,algo="sha256")==manifest$sha256))
print(checks);print(tab$switch_direction_summary);print(tab$model_common_summary)
