source('scripts/p15/activate_p15_environment.R')
suppressPackageStartupMessages({library(data.table);library(glmnet);library(rpart)})
out<-'experiments/p15_peer_rating_expansion_20260912'
z<-fread(file.path(out,'model_feature_panel.csv'))
set.seed(12092026)
countries<-sort(unique(z$iso3));folds<-data.table(iso3=countries,fold=sample(rep(1:10,length.out=length(countries))))
z<-merge(z,folds,by='iso3');fwrite(folds,file.path(out,'country_folds.csv'))
corevars<-c('log_gdp_pc','log_gdp','growth3','growth_vol5','asinh_inflation','rule_law')
fiscalvars<-c(corevars,'general_debt','fiscal_balance')
vars<-list(core_ols=corevars,fiscal_ols=fiscalvars,fiscal_ridge=fiscalvars,
 fiscal_tree=fiscalvars,external_ridge=c(fiscalvars,'current_account','reserves'))
fitpredict<-function(tr,te,v,method){
 result<-rep(NA_real_,nrow(te));goodtr<-complete.cases(tr[,c('rating_notch',v),with=FALSE]);goodte<-complete.cases(te[,v,with=FALSE])
 tr<-tr[goodtr];tt<-te[goodte]
 if(nrow(tr)<80 || !nrow(tt))return(result)
 # Full-country holdouts, complete cases, and prespecified transformations.
 # No tuning against coverage or held-out rate errors.
 if(grepl('ridge',method)){
  fit<-glmnet(as.matrix(tr[,v,with=FALSE]),tr$rating_notch,alpha=0,lambda=0.2,standardize=TRUE)
  pp<-as.numeric(predict(fit,as.matrix(tt[,v,with=FALSE]),s=0.2))
 }else if(grepl('tree',method)){
  fit<-rpart(reformulate(v,'rating_notch'),data=tr,control=rpart.control(cp=.01,minbucket=30,maxdepth=4,xval=0))
  pp<-as.numeric(predict(fit,newdata=tt))
 }else{
  fit<-lm(reformulate(v,'rating_notch'),data=tr)
  pp<-as.numeric(predict(fit,newdata=tt))
 }
 result[goodte]<-pmin(21,pmax(1,pp));result
}
res<-list();k<-0L
# Main experiment: each target's entire country is excluded from the rating
# training sample, and training rating years precede the target year.
for(method in names(vars)){
 cat('Fitting',method,'\n');flush.console()
 for(y in 2012:2024)for(f in 1:10){
  te<-z[analysis_year==y & fold==f];tr<-z[analysis_year<y & fold!=f]
  p<-fitpredict(tr,te,vars[[method]],method)
  k<-k+1L;res[[k]]<-cbind(te[,.(iso3,analysis_year,country,rating_notch,historical_lmic_reporting_scope,historical_income_level,selected_tier)],data.table(model=method,predicted_notch=p,training_last_year=y-1L,validation='country_and_time_holdout'))
 }
}
pred<-rbindlist(res);fwrite(pred,file.path(out,'shadow_predictions.csv'))
metrics<-function(d){e<-d$predicted_notch-d$rating_notch;data.table(n=length(e),countries=uniqueN(d$iso3),mae=mean(abs(e)),rmse=sqrt(mean(e^2)),bias=mean(e),rounded_exact=mean(round(d$predicted_notch)==d$rating_notch),within1=mean(abs(e)<=1),within2=mean(abs(e)<=2),within3=mean(abs(e)<=3),q90_absolute_error=as.numeric(quantile(abs(e),.9)))}
valid<-pred[is.finite(predicted_notch)&is.finite(rating_notch)]
met<-rbind(valid[,metrics(.SD),by=.(model)],valid[historical_lmic_reporting_scope==TRUE,metrics(.SD),by=.(model)][,model:=paste0(model,'_LMIC')])
fwrite(met,file.path(out,'shadow_validation_metrics.csv'));print(met)
fwrite(valid[,metrics(.SD),by=.(model,historical_income_level)],file.path(out,'shadow_validation_income.csv'))
fwrite(valid[,metrics(.SD),by=.(model,analysis_year)],file.path(out,'shadow_validation_year.csv'))
fwrite(pred[historical_lmic_reporting_scope==TRUE,.(total=.N,predictions=sum(is.finite(predicted_notch)),countries=uniqueN(iso3[is.finite(predicted_notch)])),by=.(model,selected_tier)],file.path(out,'shadow_coverage.csv'))
# One intervening year is filled from the preceding even-year snapshot. This
# tests a two-year update cycle, not indefinite carry-forward.
bi<-pred[,.(iso3,analysis_year,model,annual_prediction=predicted_notch,rating_notch,historical_lmic_reporting_scope,selected_tier)]
anchor<-copy(pred)[,anchor_year:=analysis_year]
bi[,anchor_year:=analysis_year-analysis_year%%2]
bi<-merge(bi,anchor[,.(iso3,anchor_year,model,biennial_prediction=predicted_notch)],by=c('iso3','anchor_year','model'),all.x=TRUE)
fwrite(bi,file.path(out,'biennial_predictions.csv'))
bm<-bi[analysis_year%%2==1 & is.finite(rating_notch) & is.finite(annual_prediction) & is.finite(biennial_prediction),.(n=.N,annual_mae=mean(abs(annual_prediction-rating_notch)),biennial_mae=mean(abs(biennial_prediction-rating_notch)),mean_abs_rating_shift=mean(abs(annual_prediction-biennial_prediction))),by=model]
fwrite(bm,file.path(out,'biennial_validation.csv'));print(bm)
# Actual-rating persistence benchmark for the same two-year update cycle.
aa<-z[analysis_year%in%2012:2024,.(iso3,analysis_year,rating_notch,historical_lmic_reporting_scope)]
aa[,anchor_year:=analysis_year-analysis_year%%2]
aa<-merge(aa,z[,.(iso3,anchor_year=analysis_year,anchor_rating=rating_notch)],by=c('iso3','anchor_year'),all.x=TRUE)
aa<-aa[analysis_year%%2==1 & is.finite(anchor_rating)&is.finite(rating_notch)]
fwrite(aa,file.path(out,'actual_biennial_comparison.csv'))
print(aa[,.(n=.N,unchanged=mean(rating_notch==anchor_rating),within1=mean(abs(rating_notch-anchor_rating)<=1),mae=mean(abs(rating_notch-anchor_rating)))])
capture.output(sessionInfo(),file=file.path(out,'model_environment.txt'))
