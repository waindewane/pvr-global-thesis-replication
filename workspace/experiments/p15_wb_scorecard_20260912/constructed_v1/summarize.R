source('scripts/p15/activate_p15_environment.R');library(data.table)
b<-'experiments/p15_wb_scorecard_20260912/constructed_v1';p<-fread(file.path(b,'peer_final/all_peer_predictions.csv'))
x<-fread(file.path(b,'inputs.csv'));s<-fread(file.path(b,'scorecard_country_year.csv'))
d<-p[mode=='deployment'&historical_lmic_reporting_scope==TRUE&selected_tier=='peer']
labels<-data.table(method=c('current','P_stop5','PIS_stop5','PISR_stop5',
 'PIS__wb_max2__stop5__point','PIS__wb_legacy_hold__stop5__point',
 'PIS__wgi_legacy_hold__strict__point','PIS__wgi_legacy_hold__stop5__point',
 'PIS__wgi_gci_bridge_max2__stop5__point',
 'PIS__wgi_bounded__strict__guaranteed_interval','PIS__wgi_bounded__stop5__guaranteed_interval',
 'PISR__wgi_bounded__strict__guaranteed_interval','PISR__wgi_bounded__stop5__guaranteed_interval',
 'PIS__wgi_legacy_hold__strict__caliper5','PIS_stop7'),label=c('Original eight-rule method','Primary only; stop5',
 'Observed donors; stop5','Add existing rating-implied donors; no shadows; stop5',
 'WB inflation-only; GCI max2; stop5','WB inflation-only; legacy GCI hold; stop5',
 'Governance scorecard; legacy GCI hold; strict','Governance scorecard; legacy GCI hold; stop5',
 'Governance scorecard; bridged GCI max2; stop5',
 'Governance score intervals; strict','Governance score intervals; stop5',
 'Governance score intervals + rating-implied donors; strict','Governance score intervals + rating-implied donors; stop5',
 'Governance point score; strict;5-notch radius','Observed donors; stop7'))
fwrite(labels,file.path(b,'report_labels.csv'))
a<-d[,.(covered=sum(selected_peer_new),lost=sum(!selected_peer_new),countries=uniqueN(iso3[selected_peer_new]),
 rating_matched=sum(selected_peer_new & rule<=4),income_region=sum(selected_peer_new & rule==5),
 rating_only=sum(selected_peer_new & rule==4),groups_using_rating_implied=sum(selected_peer_new & rating_implied_members>0),
 rating_implied_majority=sum(selected_peer_new & rating_implied_members>n_peers/2),
 low_income=sum(selected_peer_new & historical_income_level=='Low income'),
 lower_middle=sum(selected_peer_new & historical_income_level=='Lower middle income'),
 upper_middle=sum(selected_peer_new & historical_income_level=='Upper middle income')),by=method]
a<-merge(labels,a,by='method',sort=FALSE);fwrite(a,file.path(b,'report_coverage.csv'))
r<-d[selected_peer_new==TRUE,.N,by=.(method,rule)];r<-dcast(merge(labels,r,by='method'),method+label~rule,value.var='N',fill=0)
fwrite(r,file.path(b,'report_rules.csv'))
v<-p[mode=='target_rating_hidden'&historical_lmic_reporting_scope==TRUE & is.finite(estimate)]
fwrite(v[,.(n=.N,mae=mean(abs(estimate-actual)),bias=mean(estimate-actual),rmse=sqrt(mean((estimate-actual)^2))),by=.(method,historical_income_level)],file.path(b,'validation_income.csv'))
comps<-list(
 c('PIS_stop5','PISR_stop5'),
 c('PIS__wgi_legacy_hold__stop5__point','PIS__wb_legacy_hold__stop5__point'),
 c('PIS__wgi_legacy_hold__stop5__point','PISR__wgi_legacy_hold__stop5__point'),
 c('PIS__wgi_bounded__strict__guaranteed_interval','PIS__wgi_bounded__stop5__guaranteed_interval'),
 c('PIS__wgi_bounded__stop5__guaranteed_interval','PISR__wgi_bounded__stop5__guaranteed_interval'),
 c('PIS__wgi_legacy_hold__strict__point','PIS__wgi_legacy_hold__strict__caliper5'))
paired<-rbindlist(lapply(comps,function(c){z<-merge(v[method==c[1],.(iso3,analysis_year,actual,a=estimate)],v[method==c[2],.(iso3,analysis_year,b=estimate)],by=c('iso3','analysis_year'))
 data.table(a=c[1],b=c[2],n=nrow(z),countries=uniqueN(z$iso3),mae_a=mean(abs(z$a-z$actual)),mae_b=mean(abs(z$b-z$actual)),bias_a=mean(z$a-z$actual),bias_b=mean(z$b-z$actual))}))
fwrite(paired,file.path(b,'paired_validation.csv'))
fwrite(paired,file.path(b,'paired_rate_validation.csv'))
rv<-merge(s,x[,.(iso3,analysis_year,historical_lmic_reporting_scope,historical_income_level,rating_moodys_rating)],by=c('iso3','analysis_year'))
source(file.path(b,'score_model.R'));rv[,actual:=match(rating_moodys_rating,rating_labels)]
rv<-rv[historical_lmic_reporting_scope==TRUE & is.finite(actual)&is.finite(shadow_notch)]
w<-dcast(rv,iso3+analysis_year+actual~scenario,value.var='shadow_notch')
w<-w[is.finite(wb_legacy_hold)&is.finite(wgi_legacy_hold)]
fwrite(data.table(n=nrow(w),wb_mae=mean(abs(w$wb_legacy_hold-w$actual)),wgi_mae=mean(abs(w$wgi_legacy_hold-w$actual)),
 wb_bias=mean(w$wb_legacy_hold-w$actual),wgi_bias=mean(w$wgi_legacy_hold-w$actual)),file.path(b,'paired_rating_validation.csv'))
fwrite(rv[,.(n=.N,mae=mean(abs(shadow_notch-actual)),bias=mean(shadow_notch-actual),within2=mean(abs(shadow_notch-actual)<=2),
 interval_contains_actual=mean(actual>=shadow_notch_lower&actual<=shadow_notch_upper)),by=.(scenario,historical_income_level)],file.path(b,'rating_validation_income.csv'))
# Missingness is overlapping. Count only required numeric measures, not absent
# zero-weight interest or an unobserved analyst decision made invisible by a proxy.
t<-x[historical_lmic_reporting_scope==TRUE & selected_tier=='peer']
fields<-list('Growth average'=is.finite(t$growth_avg7),'Growth volatility'=is.finite(t$growth_sd10),
 'GDP size'=is.finite(t$nominal_gdp_usd_bn),'PPP GDP per capita'=is.finite(t$gdp_pc_ppp),
 'Inflation average'=is.finite(t$inflation_avg7),'Inflation volatility'=is.finite(t$inflation_sd10),
 'General government debt/GDP'=is.finite(t$debt_gdp),'General government debt/revenue'=is.finite(t$debt_revenue),
 'Debt trend'=is.finite(t$debt_trend_pp),'All three governance indicators'=is.finite(t$government_effectiveness)&is.finite(t$rule_law)&is.finite(t$corruption),
 'Legacy competitiveness carried at most2 years'=is.finite(t$gci_carry_max2),
 'Legacy competitiveness with unlimited hold'=is.finite(t$gci_last_available),
 'Competitiveness crosswalk carried at most2 years'=is.finite(t$gci_bridge_max2),
 'Direct general-government foreign-currency share'=is.finite(t$qpsd_fc_share),
 'Currency share including external-PPG approximation'=is.finite(t$fc_share),
 'Gross interest or zero-weight IDA/HIPC exemption'=t$concessional_weight_exception|t$interest_source=='GFS_general_government_revised_gross',
 'Interest including net-interest proxy or exemption'=t$concessional_weight_exception|(is.finite(t$interest_gdp)&is.finite(t$interest_revenue)))
missing<-rbindlist(lapply(names(fields),function(k)data.table(variable=k,available=sum(fields[[k]]),missing=sum(!fields[[k]]),denominator=nrow(t))))
fwrite(missing,file.path(b,'missing_inputs_summary.csv'))
m<-rbindlist(lapply(names(fields),function(k)t[!fields[[k]],.(iso3,analysis_year,country,missing_variable=k)]));fwrite(m,file.path(b,'missing_input_country_years.csv'))
# Uniform input screening discarded zero-weight interest: quantify correction.
fwrite(t[,.(targets=.N,interest_zero_weight=sum(concessional_weight_exception),
 zero_weight_with_missing_gross=sum(concessional_weight_exception&interest_source!='GFS_general_government_revised_gross'),
 newly_direct_fc=sum(is.finite(qpsd_fc_share)&!is.finite(fc_share_proxy_in_range)),
 direct_fc_replaces_proxy=sum(is.finite(qpsd_fc_share)&is.finite(fc_share_proxy_in_range)))],file.path(b,'input_improvements.csv'))
years<-merge(labels,d[,.(covered=sum(selected_peer_new)),by=.(method,analysis_year)],by='method');fwrite(years,file.path(b,'report_coverage_year.csv'))
print(a[,.(label,covered,low_income,lower_middle,upper_middle)]);print(paired);print(missing)
