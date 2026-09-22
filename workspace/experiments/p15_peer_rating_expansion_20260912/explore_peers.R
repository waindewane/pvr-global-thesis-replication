source('scripts/p15/activate_p15_environment.R')
suppressPackageStartupMessages(library(data.table))
out<-'experiments/p15_peer_rating_expansion_20260912'
ptr<-jsonlite::fromJSON('data-derived/p15_master/current_run.json')
c<-fread(file.path(ptr$candidate,'core_evidence.csv'));s<-fread(file.path(ptr$candidate,'selected_reference.csv'));ctx<-fread(file.path(ptr$candidate,'peer_region_context.csv'))
x<-merge(c[,.(iso3,analysis_year,country,historical_income_level,historical_lmic_reporting_scope,ordinary_fallback_selection_permitted)],s[,.(iso3,analysis_year,selected_tier,selected_rate_pct,peer_pool_rule)],by=c('iso3','analysis_year'))
x<-merge(x,ctx[,.(iso3,analysis_year,rating_source_region,moodys_rating_normalized)],by=c('iso3','analysis_year'))
scale<-c('Aaa','Aa1','Aa2','Aa3','A1','A2','A3','Baa1','Baa2','Baa3','Ba1','Ba2','Ba3','B1','B2','B3','Caa1','Caa2','Caa3','Ca','C')
x[,notch:=match(moodys_rating_normalized,scale)]
seeds<-fread('data-derived/p15_peer_options_20260912_v2/seed_country_years.csv')
sh<-fread(file.path(out,'shadow_predictions.csv'))
for(m in unique(sh$model))x[,(m):=sh[model==m,predicted_notch][match(paste(iso3,analysis_year),sh[model==m,paste(iso3,analysis_year)])]]
bi<-fread(file.path(out,'biennial_predictions.csv'))[model=='fiscal_ols']
x[,biennial:=bi$biennial_prediction[match(paste(iso3,analysis_year),paste(bi$iso3,bi$analysis_year))]]
pub<-fread(file.path(out,'public_rating_reconciliation.csv'))
# The published supplementary dataset contains conflicts and a plainly implausible
# pre-2013 Mozambique value; fill-only, exclude Mozambique, and label as candidates.
x[,public_moody:=pub$Moody_notch[match(paste(iso3,analysis_year),paste(pub$iso3,pub$analysis_year))]]
x[iso3=='MOZ',public_moody:=NA_real_]
primary<-seeds[sources=='P',.(iso3,analysis_year,actual=seed_rate)]
x<-merge(x,primary,by=c('iso3','analysis_year'),all.x=TRUE)
designs<-data.table(method=c('current','drop8','P_stop5','PIS_stop5'),sources=c('P','P','P','PIS'),rating='none',cutoff=c(8,7,5,5),width=3,skip4=FALSE,model_donors=FALSE)
for(r in c('core_ols','fiscal_ols','fiscal_ridge','fiscal_tree','external_ridge','biennial','public_moody'))for(src in c('P','PIS'))designs<-rbind(designs,data.table(method=paste(src,r,'stop5',sep='_'),sources=src,rating=r,cutoff=5,width=3,skip4=FALSE,model_donors=FALSE))
for(src in c('P','PIS')){
 designs<-rbind(designs,data.table(method=paste(src,'fiscal_no_rule4',sep='_'),sources=src,rating='fiscal_ols',cutoff=5,width=3,skip4=TRUE,model_donors=FALSE))
 designs<-rbind(designs,data.table(method=paste(src,'fiscal_width1',sep='_'),sources=src,rating='fiscal_ols',cutoff=5,width=1,skip4=FALSE,model_donors=FALSE))
 designs<-rbind(designs,data.table(method=paste(src,'fiscal_model_both',sep='_'),sources=src,rating='fiscal_ols',cutoff=5,width=3,skip4=FALSE,model_donors=TRUE))
}
fwrite(designs,file.path(out,'peer_designs.csv'))
one<-function(t,p,de,hidden){
 p<-p[p$iso3!=t$iso3,];tn<-if(hidden)NA_real_ else t$notch
 pn<-p$notch;ts<-FALSE;ps<-rep(FALSE,nrow(p))
 if(de$rating!='none'){
  if(!is.finite(tn)){tn<-t[[de$rating]];ts<-is.finite(tn)}
  if(de$model_donors){pn<-p[[de$rating]];ps<-is.finite(pn)}else{
   ii<-!is.finite(pn);pn[ii]<-p[[de$rating]][ii];ps[ii]<-is.finite(pn[ii])
  }
 }
 inc<-p$historical_income_level==t$historical_income_level;reg<-p$rating_source_region==t$rating_source_region
 inc[is.na(inc)]<-FALSE;reg[is.na(reg)]<-FALSE
 rat<-is.finite(pn)&is.finite(tn)&abs(pn-tn)<=de$width
 masks<-list(inc&reg&rat,inc&rat,reg&rat,rat,inc&reg,inc,reg,rep(TRUE,nrow(p)))
 counts<-vapply(masks,sum,integer(1));search<-seq_len(de$cutoff);if(de$skip4)search<-setdiff(search,4L)
 eligible<-search[counts[search]>=3];rule<-if(length(eligible))eligible[1] else NA_integer_
 use<-if(is.finite(rule))masks[[rule]] else rep(FALSE,nrow(p))
 data.table(iso3=t$iso3,analysis_year=t$analysis_year,method=de$method,mode=if(hidden)'target_rating_hidden' else 'deployment',rule=rule,
 estimate=if(sum(use)>=3)median(p$seed_rate[use]) else NA_real_,n_peers=sum(use),target_notch=tn,target_supplement_used=ts,
 primary_members=sum(p$seed_source[use]=='primary'),ids_members=sum(p$seed_source[use]=='ids'),secondary_members=sum(p$seed_source[use]=='secondary'),
 model_donor_members=sum(ps[use]),member_ids=paste(sort(p$iso3[use]),collapse=';'),member_sources=paste(paste(p$iso3[use],p$seed_source[use],sep=':'),collapse=';'))
}
# Freeze inputs before reusing expensive cached matches. A changed upstream
# snapshot or matching implementation must use a new versioned experiment.
peer_inputs<-c('data-derived/p15_master/current_run.json',
 file.path(ptr$candidate,c('core_evidence.csv','selected_reference.csv','peer_region_context.csv')),
 'data-derived/p15_peer_options_20260912_v2/seed_country_years.csv',
 file.path(out,c('shadow_predictions.csv','biennial_predictions.csv','public_rating_reconciliation.csv','explore_peers.R')))
peer_input_manifest<-data.table(path=peer_inputs,sha256=vapply(peer_inputs,digest::digest,character(1),file=TRUE,algo='sha256'))
if(file.exists(file.path(out,'peer_prediction_input_manifest.csv'))){
 stopifnot(identical(peer_input_manifest,fread(file.path(out,'peer_prediction_input_manifest.csv'))))
}else fwrite(peer_input_manifest,file.path(out,'peer_prediction_input_manifest.csv'))
if(!file.exists(file.path(out,'peer_country_year_predictions.csv'))){
pp<-list();k<-0L
for(j in seq_len(nrow(designs))){
 de<-designs[j];cat('Matching',de$method,'\n');flush.console()
 for(y in 2012:2024){
  pool<-merge(seeds[sources==de$sources & analysis_year==y],x[,c('iso3','analysis_year',unique(sh$model),'biennial','public_moody'),with=FALSE],by=c('iso3','analysis_year'))
  targets<-x[analysis_year==y & historical_lmic_reporting_scope & (selected_tier%in%c('peer','no_eligible_rate')|is.finite(actual))]
  for(i in seq_len(nrow(targets))){t<-targets[i]
   if(t$selected_tier%in%c('peer','no_eligible_rate')){k<-k+1L;pp[[k]]<-one(t,pool,de,FALSE)}
   if(is.finite(t$actual)){k<-k+1L;pp[[k]]<-one(t,pool,de,TRUE)}
  }
 }
}
p<-rbindlist(pp);p<-merge(p,x[,.(iso3,analysis_year,country,historical_income_level,selected_tier,selected_rate_pct,ordinary_fallback_selection_permitted,actual)],by=c('iso3','analysis_year'))
p[,selected_peer_new:=mode=='deployment' & is.finite(estimate) & !(ordinary_fallback_selection_permitted%in%FALSE)]
fwrite(p,file.path(out,'peer_country_year_predictions.csv'))
}else p<-fread(file.path(out,'peer_country_year_predictions.csv'))
base<-p[method=='current' & mode=='deployment' & selected_tier=='peer']
stopifnot(nrow(base)==778L,all(abs(base$estimate-base$selected_rate_pct)<1e-10),all(p$n_peers[is.finite(p$estimate)]>=3))
for(i in seq_len(nrow(p)))stopifnot(!p$iso3[i]%in%strsplit(p$member_ids[i],';',fixed=TRUE)[[1]])
dep<-p[mode=='deployment']
su<-dep[,.(retained_current_peers=sum(selected_tier=='peer'&selected_peer_new),lost_current_peers=sum(selected_tier=='peer'&!selected_peer_new),gained=sum(selected_tier!='peer'&selected_peer_new),countries=uniqueN(iso3[selected_peer_new]),changed_retained=sum(selected_tier=='peer'&selected_peer_new&abs(estimate-selected_rate_pct)>1e-10),mean_abs_rate_change=mean(abs(estimate-selected_rate_pct)[selected_tier=='peer'&selected_peer_new]),median_group_size=as.numeric(median(n_peers[selected_peer_new]))),by=method]
fwrite(su,file.path(out,'peer_coverage_summary.csv'));print(su)
fwrite(dep[selected_peer_new==TRUE,.(country_years=.N,countries=uniqueN(iso3)),by=.(method,rule)],file.path(out,'peer_rule_counts.csv'))
fwrite(dep[,.(country_years=.N,retained=sum(selected_peer_new)),by=.(method,analysis_year)],file.path(out,'peer_coverage_year.csv'))
fwrite(dep[selected_tier=='peer' & !selected_peer_new,.(country_years=.N,years=paste(sort(analysis_year),collapse=';')),by=.(method,iso3,country)],file.path(out,'lost_peer_country_years.csv'))
val<-p[mode=='target_rating_hidden' & is.finite(estimate)]
b<-val[method=='current',.(iso3,analysis_year,baseline=estimate)]
val<-merge(val,b,by=c('iso3','analysis_year'))
val[,benefit:=abs(baseline-actual)-abs(estimate-actual)]
vs<-val[,.(n=.N,countries=uniqueN(iso3),baseline_mae=mean(abs(baseline-actual)),variant_mae=mean(abs(estimate-actual)),benefit=mean(benefit),rmse=sqrt(mean((estimate-actual)^2)),bias=mean(estimate-actual)),by=method]
fwrite(vs,file.path(out,'peer_rate_validation.csv'));print(vs)
# Country-resampling intervals preserve each country's repeated errors. Donor
# reuse and the exploratory comparisons mean these are descriptive intervals.
set.seed(12092026);cis<-val[,{
 a<-split(benefit,iso3);bs<-replicate(1000,mean(unlist(a[sample(seq_along(a),length(a),replace=TRUE)])))
 list(country_bootstrap_low=as.numeric(quantile(bs,.025)),country_bootstrap_high=as.numeric(quantile(bs,.975)))
},by=method];fwrite(cis,file.path(out,'peer_rate_validation_intervals.csv'))
fwrite(val,file.path(out,'peer_rate_validation_cases.csv'))
fwrite(data.table(check=c('baseline_778_reproduced','minimum_three_countries','target_excluded'),passed=TRUE),file.path(out,'peer_checks.csv'))
