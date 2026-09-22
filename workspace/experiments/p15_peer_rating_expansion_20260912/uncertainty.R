source('scripts/p15/activate_p15_environment.R');library(data.table)
out<-'experiments/p15_peer_rating_expansion_20260912';ptr<-jsonlite::fromJSON('data-derived/p15_master/current_run.json')
base<-fread('data-derived/p15_peer_options_20260912_v2/seed_country_years.csv')[sources=='PIS']
ctx<-fread(file.path(ptr$candidate,'peer_region_context.csv'))
c<-fread(file.path(ptr$candidate,'core_evidence.csv'));s<-fread(file.path(ptr$candidate,'selected_reference.csv'))
sh<-fread(file.path(out,'shadow_predictions.csv'))[model=='fiscal_ols']
scale<-c('Aaa','Aa1','Aa2','Aa3','A1','A2','A3','Baa1','Baa2','Baa3','Ba1','Ba2','Ba3','B1','B2','B3','Caa1','Caa2','Caa3','Ca','C')
x<-merge(ctx[,.(iso3,analysis_year,rating_source_region,notch=match(moodys_rating_normalized,scale))],c[,.(iso3,analysis_year,historical_income_level,historical_lmic_reporting_scope)],by=c('iso3','analysis_year'))
x<-merge(x,s[,.(iso3,analysis_year,selected_tier)],by=c('iso3','analysis_year'));x<-merge(x,sh[,.(iso3,analysis_year,predicted_notch)],by=c('iso3','analysis_year'))
pool<-merge(base[,.(iso3,analysis_year,seed_rate)],x,by=c('iso3','analysis_year'))
targets<-x[historical_lmic_reporting_scope & selected_tier=='peer']
err<-sh[is.finite(predicted_notch)&is.finite(rating_notch),.(iso3,error=predicted_notch-rating_notch)]
eg<-split(err$error,err$iso3);set.seed(12092026);countries<-unique(x$iso3)
draws<-matrix(NA_real_,nrow(targets),100);rules<-matrix(NA_integer_,nrow(targets),100)
for(b in 1:100){
 # One randomly selected empirical forecast error per country, shared across
 # years, perturbs shadow components only. This is sensitivity, not a posterior.
 shifts<-setNames(vapply(countries,function(a){g<-eg[[sample.int(length(eg),1)]];g[sample.int(length(g),1)]},numeric(1)),countries)
 for(y in 2012:2024){
  pp<-pool[analysis_year==y];tt<-which(targets$analysis_year==y)
  pn<-ifelse(is.finite(pp$notch),pp$notch,pmin(21,pmax(1,pp$predicted_notch-shifts[pp$iso3])))
  for(i in tt){
   t<-targets[i];tn<-if(is.finite(t$notch))t$notch else pmin(21,pmax(1,t$predicted_notch-shifts[t$iso3]))
   inc<-pp$historical_income_level==t$historical_income_level;reg<-pp$rating_source_region==t$rating_source_region
   rat<-is.finite(pn)&is.finite(tn)&abs(pn-tn)<=3
   masks<-list(inc&reg&rat,inc&rat,reg&rat,rat,inc&reg)
   for(r in 1:5){use<-masks[[r]] & pp$iso3!=t$iso3;use[is.na(use)]<-FALSE
    if(sum(use)>=3){draws[i,b]<-median(pp$seed_rate[use]);rules[i,b]<-r;break}
   }
  }
 }
 if(b%%20==0){cat('Sensitivity draw',b,'\n');flush.console()}
}
point<-fread(file.path(out,'peer_country_year_predictions.csv'))[method=='PIS_fiscal_ols_stop5' & mode=='deployment' & selected_tier=='peer']
targets[,point_rate:=point$estimate[match(paste(iso3,analysis_year),paste(point$iso3,point$analysis_year))]]
targets[,`:=`(finite_draw_share=rowMeans(is.finite(draws)),q10_rate=apply(draws,1,function(v)if(any(is.finite(v)))quantile(v,.1,na.rm=TRUE) else NA_real_),q90_rate=apply(draws,1,function(v)if(any(is.finite(v)))quantile(v,.9,na.rm=TRUE) else NA_real_))]
targets[,q90_q10_width:=q90_rate-q10_rate]
fwrite(targets,file.path(out,'shadow_uncertainty_country_year.csv'))
saveRDS(list(rate_draws=draws,rule_draws=rules,target_keys=targets[,.(iso3,analysis_year)]),file.path(out,'shadow_uncertainty_draws.rds'))
su<-data.table(draws=100L,point_retained=sum(is.finite(targets$point_rate)),median_retained=median(colSums(is.finite(draws))),q10_retained=quantile(colSums(is.finite(draws)),.1),q90_retained=quantile(colSums(is.finite(draws)),.9),point_retained_with_90pct_stability=sum(is.finite(targets$point_rate)&targets$finite_draw_share>=.9),median_rate_band_width=median(targets$q90_q10_width[is.finite(targets$point_rate)],na.rm=TRUE))
fwrite(su,file.path(out,'shadow_uncertainty_summary.csv'));print(su)
