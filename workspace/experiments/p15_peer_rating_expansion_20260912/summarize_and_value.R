source('scripts/p15/activate_p15_environment.R');library(data.table)
source('R/p15_peer_geography_validation.R')
out<-'experiments/p15_peer_rating_expansion_20260912';ptr<-jsonlite::fromJSON('data-derived/p15_master/current_run.json')
p<-rbind(fread(file.path(out,'peer_country_year_predictions.csv')),fread(file.path(out,'peer_additional_predictions.csv')),fill=TRUE)
stopifnot(!anyDuplicated(p[,.(iso3,analysis_year,method,mode)]))
dep<-p[mode=='deployment'];val<-p[mode=='target_rating_hidden' & is.finite(estimate)]
su<-dep[,.(retained_current_peers=sum(selected_tier=='peer'&selected_peer_new),lost_current_peers=sum(selected_tier=='peer'&!selected_peer_new),gained=sum(selected_tier!='peer'&selected_peer_new),countries=uniqueN(iso3[selected_peer_new]),changed_retained=sum(selected_tier=='peer'&selected_peer_new&abs(estimate-selected_rate_pct)>1e-10),mean_abs_rate_change=mean(abs(estimate-selected_rate_pct)[selected_tier=='peer'&selected_peer_new]),median_group_size=as.numeric(median(n_peers[selected_peer_new]))),by=method]
fwrite(su,file.path(out,'all_peer_coverage.csv'))
fwrite(dep[selected_peer_new==TRUE,.(country_years=.N,countries=uniqueN(iso3)),by=.(method,rule)],file.path(out,'all_peer_rule_counts.csv'))
fwrite(dep[,.(country_years=.N,retained=sum(selected_peer_new)),by=.(method,analysis_year)],file.path(out,'all_peer_coverage_year.csv'))
fwrite(dep[,.(country_years=.N,retained=sum(selected_peer_new)),by=.(method,historical_income_level)],file.path(out,'all_peer_coverage_income.csv'))
fwrite(dep[selected_tier=='peer' & !selected_peer_new,.(country_years=.N,years=paste(sort(analysis_year),collapse=';')),by=.(method,iso3,country)],file.path(out,'all_lost_peers_by_country.csv'))
b<-val[method=='current',.(iso3,analysis_year,baseline=estimate)]
val<-merge(val,b,by=c('iso3','analysis_year'))
val[,`:=`(benefit=abs(baseline-actual)-abs(estimate-actual),estimate_a=estimate,actual_a=actual,estimate_b=baseline,actual_b=actual)]
vs<-val[,as.data.table(p15_geography_inference(as.data.frame(.SD))),by=method]
fwrite(vs,file.path(out,'all_peer_validation.csv'))
val[,period:=ifelse(analysis_year>=2018,'2018-2024','2012-2017')]
fwrite(val[,.(n=.N,countries=uniqueN(iso3),baseline_mae=mean(abs(baseline-actual)),variant_mae=mean(abs(estimate-actual)),benefit=mean(benefit)),by=.(method,period)],file.path(out,'all_peer_validation_period.csv'))
fwrite(val[,.(n=.N,countries=uniqueN(iso3),baseline_mae=mean(abs(baseline-actual)),variant_mae=mean(abs(estimate-actual)),benefit=mean(benefit)),by=.(method,historical_income_level)],file.path(out,'all_peer_validation_income.csv'))
# Fixed common validation support across the main sensible candidates.
methods<-c('current','P_fiscal_ols_stop5','PIS_fiscal_ols_stop5','PIS_fiscal_no_rule4','P_biennial_stop5','PIS_biennial_stop5')
kk<-val[method%in%methods,.(n=uniqueN(method)),by=.(iso3,analysis_year)][n==length(methods)]
common<-merge(val[method%in%methods],kk[,.(iso3,analysis_year)],by=c('iso3','analysis_year'))
fwrite(common[,.(n=.N,mae=mean(abs(estimate-actual))),by=method],file.path(out,'same_support_validation.csv'))
fwrite(dep,file.path(out,'all_peer_deployment_cases.csv'))
crs<-fread(file.path(ptr$stages$crs_application$dir,'loan_valuations.csv'))
flows<-fread(file.path(ptr$stages$crs_application$dir,'cash_flows.csv'))
cp<-crs[benchmark_selected_tier=='peer' & is.finite(ge_market_pct)]
fs<-split(flows,flows$loan_id)
value<-function(id,rate){if(!is.finite(rate))return(NA_real_);f<-fs[[id]];stopifnot(nrow(f)>0);100-sum(f$payment/(1+rate/100)^f$time_years)}
stopifnot(all(abs(mapply(value,cp$loan_id,cp$benchmark_selected_rate_pct)-cp$ge_market_pct)<1e-9))
v<-merge(dep[selected_tier=='peer',.(method,iso3,commitment_year=analysis_year,estimate,rule)],cp[,.(iso3,commitment_year,loan_id,ge_market_pct,ge_standardized_pct,amount_usd)],by=c('iso3','commitment_year'),allow.cartesian=TRUE)
v[,new_ge:=mapply(value,loan_id,estimate)];v[,`:=`(ge_change=new_ge-ge_market_pct,period=ifelse(commitment_year>=2018,'2018-2024','2012-2017'))]
fwrite(v,file.path(out,'crs_diagnostic_valuation_cases.csv'))
fwrite(v[,.(current_records=.N,retained=sum(is.finite(new_ge)),lost=sum(!is.finite(new_ge)),mean_ge_change_retained=mean(ge_change,na.rm=TRUE),mean_abs_ge_change_retained=mean(abs(ge_change),na.rm=TRUE),weighted_ge_change_retained=weighted.mean(ge_change,amount_usd,na.rm=TRUE)),by=.(method,period)],file.path(out,'crs_diagnostic_valuation_summary.csv'))
cat('All designs:',uniqueN(p$method),'CRS existing finite peer records:',nrow(cp),'\n')
print(su[method%in%c('PIS_fiscal_ols_stop5','PIS_fiscal_no_rule4','PIS_fiscal_lag2_stop5','PIS_fiscal_recent_events_stop5','PIS_fiscal_core_cascade_stop5','PIS_macro_nearest3','P_macro_nearest3','PIS_wb_snapshots_stop5','PIS_web_moody_stop5','PIS_web_then_fiscal_stop5')])
