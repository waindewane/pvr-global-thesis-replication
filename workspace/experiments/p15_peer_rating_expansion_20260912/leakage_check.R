# Refit every donor model after removing the validation borrower's full history.
source('experiments/p15_peer_rating_expansion_20260912/explore_peers.R')
z<-merge(fread(file.path(out,'model_feature_panel.csv')),fread(file.path(out,'country_folds.csv')),by='iso3')
corevars<-c('log_gdp_pc','log_gdp','growth3','growth_vol5','asinh_inflation','rule_law')
fiscalvars<-c(corevars,'general_debt','fiscal_balance')
mv<-list(core_ols=corevars,fiscal_ols=fiscalvars,fiscal_ridge=fiscalvars,fiscal_tree=fiscalvars,external_ridge=c(fiscalvars,'current_account','reserves'),fiscal_lag2=fiscalvars,fiscal_recent_events=fiscalvars)
lag2<-copy(z);setorder(lag2,iso3,analysis_year);allvars<-unique(unlist(mv));lag2[,(allvars):=shift(.SD),.SDcols=allvars,by=iso3]
ex<-fread(file.path(out,'shadow_additional_predictions.csv'))
for(m in unique(ex$model))x[,(m):=ex[model==m,predicted_notch][match(paste(iso3,analysis_year),ex[model==m,paste(iso3,analysis_year)])]]
x[,fiscal_core_cascade:=fcoalesce(fiscal_ols,core_ols)]
des<-rbind(designs,fread(file.path(out,'peer_additional_designs.csv')),fill=TRUE)
des<-des[!is.na(rating) & !rating%in%c('public_moody','web_moody','web_then_fiscal','wb_snapshots')]
des<-des[!is.na(model_donors) & nzchar(rating)]
targets<-x[historical_lmic_reporting_scope & is.finite(actual)]
res<-list();k<-0L
for(i in seq_len(nrow(targets))){
 t<-targets[i];y<-t$analysis_year;cc<-t$iso3
 needed<-unique(seeds[analysis_year==y & sources=='PIS',iso3])
 banks<-list()
 for(m in names(mv)){
  zz<-if(m=='fiscal_lag2')lag2 else z;v<-mv[[m]]
  te<-zz[analysis_year==y & iso3%in%needed];te[,pp:=NA_real_]
  for(f in unique(te$fold)){
   tr<-zz[analysis_year<y & fold!=f & iso3!=cc]
   if(m=='fiscal_recent_events')tr<-tr[event_age_years<=5]
   tr<-tr[complete.cases(tr[,c('rating_notch',v),with=FALSE])]
   jj<-which(te$fold==f & complete.cases(te[,v,with=FALSE]))
   if(nrow(tr)<80 || !length(jj))next
   if(grepl('ridge',m)){
    fit<-glmnet::glmnet(as.matrix(tr[,v,with=FALSE]),tr$rating_notch,alpha=0,lambda=.2,standardize=TRUE)
    pp<-as.numeric(predict(fit,as.matrix(te[jj,v,with=FALSE]),s=.2))
   }else if(m=='fiscal_tree'){
    fit<-rpart::rpart(reformulate(v,'rating_notch'),data=tr,control=rpart::rpart.control(cp=.01,minbucket=30,maxdepth=4,xval=0));pp<-predict(fit,newdata=te[jj])
   }else pp<-predict(lm(reformulate(v,'rating_notch'),data=tr),newdata=te[jj])
   te[jj,pp:=pmin(21,pmax(1,pp))]
  }
  banks[[m]]<-te[,.(iso3,pp)];setnames(banks[[m]],'pp',m)
 }
 # Biennial donors need the preceding even-year fit, also excluding borrower cc.
 ay<-y-y%%2;te<-z[analysis_year==ay & iso3%in%needed];te[,biennial:=NA_real_]
 for(f in unique(te$fold)){
  tr<-z[analysis_year<ay & fold!=f & iso3!=cc];tr<-tr[complete.cases(tr[,c('rating_notch',fiscalvars),with=FALSE])]
  jj<-which(te$fold==f & complete.cases(te[,fiscalvars,with=FALSE]))
  if(nrow(tr)>=80 && length(jj))te[jj,biennial:=pmin(21,pmax(1,predict(lm(reformulate(fiscalvars,'rating_notch'),data=tr),newdata=te[jj])))]
 }
 bank<-Reduce(function(a,b)merge(a,b,by='iso3',all=TRUE),c(banks,list(te[,.(iso3,biennial)])))
 bank[,fiscal_core_cascade:=fcoalesce(fiscal_ols,core_ols)]
 for(j in seq_len(nrow(des))){de<-des[j];pool<-merge(seeds[analysis_year==y & sources==de$sources],bank,by='iso3',all.x=TRUE);k<-k+1L;res[[k]]<-one(t,pool,de,TRUE)}
 if(i%%40==0){cat('Strict validation',i,'of',nrow(targets),'\n');flush.console()}
}
r<-rbindlist(res);r<-merge(r,x[,.(iso3,analysis_year,country,actual,historical_income_level)],by=c('iso3','analysis_year'))
r<-r[is.finite(estimate)];b<-r[method=='current',.(iso3,analysis_year,baseline=estimate)]
r<-merge(r,b,by=c('iso3','analysis_year'))
r[,`:=`(benefit=abs(baseline-actual)-abs(estimate-actual),estimate_a=estimate,actual_a=actual,estimate_b=baseline,actual_b=actual)]
source('R/p15_peer_geography_validation.R')
fwrite(r,file.path(out,'strict_peer_validation_cases.csv'))
fwrite(r[,as.data.table(p15_geography_inference(as.data.frame(.SD))),by=method],file.path(out,'strict_peer_validation.csv'))
fwrite(r[,.(n=.N,baseline_mae=mean(abs(baseline-actual)),variant_mae=mean(abs(estimate-actual))),by=.(method,historical_income_level)],file.path(out,'strict_peer_validation_income.csv'))
fwrite(r[,.(n=.N,baseline_mae=mean(abs(baseline-actual)),variant_mae=mean(abs(estimate-actual))),by=.(method,period=ifelse(analysis_year>=2018,'2018-2024','2012-2017'))],file.path(out,'strict_peer_validation_period.csv'))
