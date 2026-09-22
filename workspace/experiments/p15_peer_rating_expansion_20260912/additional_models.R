source('scripts/p15/activate_p15_environment.R');library(data.table)
out<-'experiments/p15_peer_rating_expansion_20260912'
z<-merge(fread(file.path(out,'model_feature_panel.csv')),fread(file.path(out,'country_folds.csv')),by='iso3')
v<-c('log_gdp_pc','log_gdp','growth3','growth_vol5','asinh_inflation','rule_law','general_debt','fiscal_balance')
lag2<-copy(z);setorder(lag2,iso3,analysis_year)
lag2[,(v):=shift(.SD),.SDcols=v,by=iso3]
res<-list();k<-0
for(m in c('fiscal_lag2','fiscal_recent_events'))for(y in 2012:2024)for(f in 1:10){
 zz<-if(m=='fiscal_lag2')lag2 else z
 tr<-zz[analysis_year<y & fold!=f];if(m=='fiscal_recent_events')tr<-tr[event_age_years<=5]
 tr<-tr[complete.cases(tr[,c('rating_notch',v),with=FALSE])]
 te<-zz[analysis_year==y & fold==f];ok<-complete.cases(te[,v,with=FALSE]);p<-rep(NA_real_,nrow(te))
 if(nrow(tr)>=80 && any(ok))p[ok]<-pmin(21,pmax(1,predict(lm(reformulate(v,'rating_notch'),data=tr),newdata=te[ok])))
 k<-k+1;res[[k]]<-data.table(iso3=te$iso3,analysis_year=te$analysis_year,model=m,predicted_notch=p,rating_notch=te$rating_notch,historical_lmic_reporting_scope=te$historical_lmic_reporting_scope)
}
r<-rbindlist(res);fwrite(r,file.path(out,'shadow_additional_predictions.csv'))
fwrite(r[is.finite(predicted_notch)&is.finite(rating_notch),.(n=.N,mae=mean(abs(predicted_notch-rating_notch)),within3=mean(abs(predicted_notch-rating_notch)<=3)),by=.(model,historical_lmic_reporting_scope)],file.path(out,'shadow_additional_validation.csv'))
