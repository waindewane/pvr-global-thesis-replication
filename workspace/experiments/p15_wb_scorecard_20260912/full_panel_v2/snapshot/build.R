source('scripts/p15/activate_p15_environment.R')
suppressPackageStartupMessages({library(data.table);library(jsonlite);library(digest)})
b<-'experiments/p15_wb_scorecard_20260912/constructed_v1';source(file.path(b,'score_model.R'))
x<-fread(dirname(b)|>file.path('scorecard_inputs.csv'))
x<-merge(x,fread(file.path(b,'historical_eligibility.csv'))[,.(iso3,analysis_year,ida,hipc,concessional_weight_exception)],by=c('iso3','analysis_year'))
wgi<-fread('experiments/p15_peer_rating_expansion_20260912/model_feature_panel.csv')[analysis_year%in%2012:2024,
 .(iso3,analysis_year,government_effectiveness,rule_law,corruption)]
x<-merge(x,wgi,by=c('iso3','analysis_year'),all.x=TRUE)
x<-merge(x,fread(file.path(b,'qpsd_fc_share.csv')),by=c('iso3','reference_year'),all.x=TRUE)
x[qpsd_fc_share<0|qpsd_fc_share>100,qpsd_fc_share:=NA_real_]
# QPSD is actual general-government currency-denomination data, both numerator
# and denominator from the same Q4 release. Retrospectively revised, not real-time.
x[,fc_share:=fcoalesce(qpsd_fc_share,fc_share_proxy_in_range)]
x[,fc_source:=fifelse(is.finite(qpsd_fc_share),'QPSD_general_government_Q4',fifelse(is.finite(fc_share_proxy_in_range),'WDI_external_PPG_proxy','missing'))]
has_gross<-is.finite(x$gfs_gross_interest_gdp_latest)&is.finite(x$gfs_gross_interest_revenue_latest)
x[,interest_gdp:=ifelse(has_gross,gfs_gross_interest_gdp_latest,implied_net_interest_gdp)]
x[,interest_revenue:=ifelse(has_gross,gfs_gross_interest_revenue_latest,implied_net_interest_revenue)]
x[,interest_source:=ifelse(has_gross,'GFS_general_government_revised_gross','WEO_vintage_net_interest_proxy')]
x[interest_gdp<0|interest_revenue<0,`:=`(interest_gdp=NA_real_,interest_revenue=NA_real_)]
# Intermediate aggregation uses the published UNEVEN numerical midpoints.
n<-nrow(x);base_econ<-data.frame(growth=indicator(x$growth_avg7,'growth'),growth_sd=indicator(x$growth_sd10,'growth_sd'),
  gci=indicator(x$gci_carry_max2,'gci'),gdp=indicator(x$nominal_gdp_usd_bn,'gdp'),gdppc=indicator(x$gdp_pc_ppp,'gdppc'))
ew<-matrix(rep(c(.25,.125,.125,.25,.25),each=n),ncol=5)
fi<-data.frame(debt_gdp=indicator(x$debt_gdp,'debt_gdp'),debt_revenue=indicator(x$debt_revenue,'debt_revenue'),
 interest_gdp=indicator(x$interest_gdp,'interest_gdp'),interest_revenue=indicator(x$interest_revenue,'interest_revenue'))
fw<-matrix(.25,n,4);fw[x$concessional_weight_exception,]<-matrix(rep(c(.5,.5,0,0),each=sum(x$concessional_weight_exception)),ncol=4)
reserve<-x$iso3%in%c('JPN','CHE','GBR','USA','DEU','FRA');fw[reserve,]<-matrix(rep(c(.05,.05,.45,.45),each=sum(reserve)),ncol=4)
x[,fiscal_initial:=weighted_factor(fi,fw)]
x[,debt_trend_penalty:=low_good(debt_trend_pp,c(10,20,30))-1L]
x[,fc_penalty:=low_good(fc_share,c(20,25,30,40,50,60))-1L]
# Published low-debt exception, applied mechanically as a labelled adaptation.
x[debt_gdp<25,fc_penalty:=pmin(fc_penalty,3L)]
# Source does not specify all hedging/peg/adopted-currency cases. No blanket
# dollarization imputation from geography; all FC penalties bracketed below.
x[,f3:=pmin(15L,fiscal_initial+pmin(6L,debt_trend_penalty+fc_penalty))]
infl<-data.frame(level=inflation_index(x$inflation_avg7),vol=indicator(x$inflation_sd10,'inflation_sd'))
iw<-matrix(.5,n,2);wb_f2<-weighted_factor(infl,iw)
fullinst<-data.frame(ge=indicator(x$government_effectiveness,'ge'),rl=indicator(x$rule_law,'rl'),cc=indicator(x$corruption,'cc'),infl)
fullw<-matrix(rep(c(.375,.1875,.1875,.125,.125),each=n),ncol=5);wgi_f2<-weighted_factor(fullinst,fullw)
# Source event dates have been transcribed from the user-accessible Moody's
# default study Exhibit2 and visually verified. The2020 Argentina episode uses
# its August2019 start from the footnote. No future episode or recovery is used.
events<-fread(file.path(b,'default_events.csv'));events[,event_year:=as.integer(substr(event_month,1,4))]
x[,moodys_prior20_events:=vapply(seq_len(.N),function(i)events[iso3==x$iso3[i]&event_year<=x$reference_year[i]&event_year>x$reference_year[i]-20,.N],integer(1))]
# Explicit central scenario: two factor scores for a single event (loss unknown),
# three for two or more source episodes. BoC private stock history supplements
# the rated-only source. Zero recorded events is not certified absence of default.
x[,default_penalty:=fifelse(moodys_prior20_events>=2,3L,
 fifelse(moodys_prior20_events==1|boc_bond_bank_positive_within20%in%TRUE,2L,0L))]
x[,default_penalty_basis:='conditional:3 for >=2 Moody episodes;2 for1 episode or recentBoC private default stock;else0; uncertainty0..3']
mat<-read_exact_matrices(b)
# Certify monotonicity before using factor endpoints for exact interval extrema.
for(nm in names(mat)){
 m<-mat[[nm]][if(nm=='rating_midpoint')rev(strength_labels) else strength_labels,,drop=FALSE]
 vals<-matrix(match(m,if(nm=='rating_midpoint')rating_labels else strength_labels),15)
 stopifnot(all(apply(vals,1,diff)>=0),all(apply(vals,2,diff)>=0))
}
# Paired-2017 empirical interpolation bridges the two GCI definitions only as a
# separate adaptation. All bridge information first available October2018.
gci<-fread(file.path(b,'gci4_2017_2019.csv'));old<-fread(dirname(b)|>file.path('supplemental/wef_legacy_gci_long.csv'))
pairs<-merge(gci[year==2017,.(iso3,gci4)],old[gci_reference_year==2017,.(iso3,wef_gci)],by='iso3')
# Isotonic nonparametric calibration of the index, NOT a fitted sovereign-rating model.
setorder(pairs,gci4);pairs[,calibrated_legacy:=isoreg(gci4,wef_gci)$yf]
bridge<-function(v,drop=NULL){p<-pairs[!iso3%in%drop];fit<-isoreg(p$gci4,p$wef_gci);approx(p$gci4,fit$yf,xout=v,rule=2,ties=mean)$y}
pairs[,loo_prediction:=vapply(seq_len(.N),function(i)bridge(gci4[i],iso3[i]),numeric(1))]
pairs[,`:=`(absolute_error=abs(loo_prediction-wef_gci),score_notch_error=abs(indicator(loo_prediction,'gci')-indicator(wef_gci,'gci')))]
fwrite(pairs,file.path(b,'gci_bridge_validation.csv'))
x[,gci_bridge_max2:=gci_carry_max2]
for(i in seq_len(nrow(x))){
 if(x$reference_year[i]<2018)next
 available<-gci[iso3==x$iso3[i]&year<=x$reference_year[i]&first_publication_year<=x$reference_year[i]&x$reference_year[i]-year<=2]
 if(nrow(available)){setorder(available,-year);x$gci_bridge_max2[i]<-bridge(available$gci4[1],x$iso3[i])}
}
scenarios<-data.table(scenario=c('wb_max2','wb_legacy_hold','wgi_legacy_hold','wgi_gci_bridge_max2','wgi_bounded'),
 institution=c('wb','wb','wgi','wgi','wgi'),gci=c('max2','hold','hold','bridge','bounds'))
out<-list();details<-list()
for(j in seq_len(nrow(scenarios))){
 sn<-scenarios[j];ei<-base_econ;ei$gci<-indicator(switch(sn$gci,max2=x$gci_carry_max2,hold=x$gci_last_available,bridge=x$gci_bridge_max2,bounds=x$gci_carry_max2),'gci')
 f1<-weighted_factor(ei,ew);f2<-pmin(15L,if(sn$institution=='wb')wb_f2 else wgi_f2 + 0L)
 f2<-pmin(15L,f2+x$default_penalty);f3<-x$f3
 lower<-upper<-aggregate_factors(f1,f2,f3,mat)
 if(sn$gci=='bounds'){
  elo<-ehi<-ei;elo$gci[is.na(elo$gci)]<-1L;ehi$gci[is.na(ehi$gci)]<-15L
  f1lo<-weighted_factor(elo,ew);f1hi<-weighted_factor(ehi,ew)
  # Missing gross-interest inputs get their entire allowable indicator range;
  # zero-weight affordability components have no influence for IDA/HIPC.
  flo<-fhi<-fi
  for(k in c('interest_gdp','interest_revenue')){flo[[k]][is.na(flo[[k]])]<-1L;fhi[[k]][is.na(fhi[[k]])]<-15L}
  fisc_lo<-weighted_factor(flo,fw);fisc_hi<-weighted_factor(fhi,fw)
  # Bracket currency exposure/hedging/adopted-currency adjustment for ALL rows,
  # including the external-PPG proxy, instead of treating proxy as exact FC share.
  f3lo<-pmin(15L,fisc_lo+x$debt_trend_penalty)
  f3hi<-pmin(15L,fisc_hi+pmin(6L,x$debt_trend_penalty+ifelse(x$debt_gdp<25,3L,6L)))
  f2lo<-wgi_f2;f2hi<-pmin(15L,wgi_f2+3L)
  lower<-aggregate_factors(f1lo,f2lo,f3lo,mat);upper<-aggregate_factors(f1hi,f2hi,f3hi,mat)
  # Display midpoint is just the centre of an interval, not a complete rating.
  f1<-factor_index((midpoints[f1lo]+midpoints[f1hi])/2)
  score<-(lower+upper)/2
 }else score<-lower
 out[[j]]<-data.table(iso3=x$iso3,analysis_year=x$analysis_year,scenario=sn$scenario,
  shadow_notch=score,shadow_notch_lower=lower,shadow_notch_upper=upper,scorecard_complete=FALSE,
  status=ifelse(is.finite(score),if(sn$gci=='bounds')'conditional_interval_not_certified_rating' else 'conditional_quantitative_adaptation','missing_required_inputs'))
 details[[j]]<-data.table(iso3=x$iso3,analysis_year=x$analysis_year,scenario=sn$scenario,
  f1=f1,f2=f2,f3=f3,gci_indicator=ei$gci,shadow_notch=score,
  note=if(sn$gci=='bounds')'f1 display centre; f2/f3 point diagnostics may be missing; interval endpoints define output' else 'point factor diagnostic')
}
y<-rbindlist(out);stopifnot(nrow(y)==n*5,!anyDuplicated(y[,.(iso3,analysis_year,scenario)]))
fwrite(y,file.path(b,'scorecard_country_year.csv'));fwrite(rbindlist(details),file.path(b,'factor_diagnostics.csv'));fwrite(x,file.path(b,'inputs.csv'))
fwrite(scenarios,file.path(b,'scenarios.csv'))
cov<-merge(y,x[,.(iso3,analysis_year,selected_tier,historical_lmic_reporting_scope,historical_income_level)],by=c('iso3','analysis_year'))
fwrite(cov[,.(country_years=.N,finite_scores=sum(is.finite(shadow_notch)),countries=uniqueN(iso3[is.finite(shadow_notch)]),
  mean_width=mean(shadow_notch_upper-shadow_notch_lower,na.rm=TRUE)),by=.(scenario,selected_tier,historical_lmic_reporting_scope,historical_income_level)],file.path(b,'score_coverage.csv'))
print(cov[historical_lmic_reporting_scope==TRUE & selected_tier=='peer',.(finite=sum(is.finite(shadow_notch)),mean_width=mean(shadow_notch_upper-shadow_notch_lower,na.rm=TRUE)),by=scenario])
writeLines(capture.output(sessionInfo()),file.path(b,'r_session_info.txt'))
