source('scripts/p15/activate_p15_environment.R')
suppressPackageStartupMessages(library(data.table))
out<-'experiments/p15_peer_rating_expansion_20260912'
ptr<-jsonlite::fromJSON('data-derived/p15_master/current_run.json')
core<-fread(file.path(ptr$candidate,'core_evidence.csv'));sel<-fread(file.path(ptr$candidate,'selected_reference.csv'))
inputs<-c('data-derived/p15_master/current_run.json',file.path(ptr$candidate,c('core_evidence.csv','selected_reference.csv','tier_eligibility.csv','peer_region_context.csv','peer_membership.csv')),file.path(ptr$stages$crs_application$dir,c('loan_valuations.csv','cash_flows.csv')),file.path(ptr$stages$loan_comparisons$dir,'loan_valuations.csv'))
manifest<-data.table(path=inputs,sha256=vapply(inputs,digest::digest,character(1),file=TRUE,algo='sha256'))
if(file.exists(file.path(out,'production_input_manifest.csv')))stopifnot(identical(manifest,fread(file.path(out,'production_input_manifest.csv')))) else fwrite(manifest,file.path(out,'production_input_manifest.csv'))
raw<-rbindlist(lapply(file.path(out,c('macro_raw_long.csv','macro_fallback_long.csv','weo_macro_long.csv')),fread))
# World Bank governance JSON leaves ISO3 blank for some real economies. Resolve
# exact names through countrycode and preserve the mapping diagnostics.
bad<-unique(raw[is.na(iso3)|iso3=='',.(country)])
bad[,mapped_iso3:=countrycode::countrycode(country,'country.name','iso3c',custom_match=c('Kosovo'='XKX','West Bank and Gaza'='PSE'))]
fwrite(bad,file.path(out,'blank_iso3_mapping.csv'))
raw[is.na(iso3)|iso3=='',iso3:=bad$mapped_iso3[match(country,bad$country)]]
raw<-raw[iso3%in%core$iso3];stopifnot(!anyDuplicated(raw[,.(indicator,iso3,year)]))
w<-dcast(raw,iso3+year~indicator,value.var='value')
grid<-CJ(iso3=unique(core$iso3),year=2000:2024);w<-merge(grid,w,by=c('iso3','year'),all.x=TRUE)
dict<-c(gdp_pc='NY.GDP.PCAP.CD',gdp='NY.GDP.MKTP.CD',gni_pc='NY.GNP.PCAP.CD',growth='NY.GDP.MKTP.KD.ZG',inflation='FP.CPI.TOTL.ZG',current_account='BN.CAB.XOKA.GD.ZS',reserves='FI.RES.TOTL.MO',external_debt='DT.DOD.DECT.GN.ZS',central_debt='GC.DOD.TOTL.GD.ZS',rule_law='GOV_WGI_RL.EST',government_effectiveness='GOV_WGI_GE.EST',corruption='GOV_WGI_CC.EST',political_stability='GOV_WGI_PV.EST',interest_revenue='GC.XPN.INTP.RV.ZS',general_debt='GGXWDG_NGDP',fiscal_balance='GGXCNL_NGDP')
for(n in names(dict))w[,(n):=get(dict[[n]])]
# WEO substitutes only for the same macro concept. GDP is in USD billions there.
for(p in list(c('gdp_pc','NGDPDPC'),c('growth','NGDP_RPCH'),c('inflation','PCPIPCH')))w[!is.finite(get(p[1])),(p[1]):=get(p[2])]
w[!is.finite(gdp),gdp:=NGDPD*1e9]
w[,growth3:=frollmean(growth,3,align='right'),by=iso3]
w[,growth_vol5:=frollapply(growth,5,sd,align='right'),by=iso3]
w[,`:=`(log_gdp_pc=log(gdp_pc),log_gdp=log(gdp),asinh_inflation=asinh(inflation/5))]
w[,`:=`(macro_year=year,analysis_year=year+1L)]
# Candidate uses Jan1 ratings. All economic variables end in t-1; macro data are
# latest/revised snapshots, so this is retrospective, not a real-time backtest.
r<-fread('data-raw/ratings/bloomberg_sovereign_rating_panel_2026-05-28/bloomberg_boy_ratings_preferred_country_year_2000_2025.csv')
scale<-c('Aaa','Aa1','Aa2','Aa3','A1','A2','A3','Baa1','Baa2','Baa3','Ba1','Ba2','Ba3','B1','B2','B3','Caa1','Caa2','Caa3','Ca','C')
r[,rating_notch:=match(moodys_rating,scale)];setnames(r,'year','analysis_year')
z<-merge(w,r[,.(iso3,analysis_year,rating_notch,moodys_rating,moodys_source_event_date,moodys_source_rule)],by=c('iso3','analysis_year'),all.x=TRUE)
z<-merge(z,core[,.(iso3,analysis_year,country,historical_income_level,historical_lmic_reporting_scope,rating_moodys_rating)],by=c('iso3','analysis_year'),all.x=TRUE)
z<-merge(z,sel[,.(iso3,analysis_year,selected_tier)],by=c('iso3','analysis_year'),all.x=TRUE)
z[,country:=fcoalesce(country,core$country[match(iso3,core$iso3)])]
z[,event_age_years:=analysis_year-as.integer(substr(moodys_source_event_date,1,4))]
z<-z[analysis_year%in%2005:2024]
features<-c('log_gdp_pc','log_gdp','growth3','growth_vol5','asinh_inflation','rule_law','general_debt','fiscal_balance','current_account','reserves','external_debt','central_debt','interest_revenue')
for(n in features)z[!is.finite(get(n)),(n):=NA_real_]
fwrite(z,file.path(out,'model_feature_panel.csv'))
cov<-rbindlist(lapply(features,function(v)z[analysis_year%in%2012:2024,.(indicator=v,total=.N,available=sum(is.finite(get(v)))),by=.(historical_lmic_reporting_scope,selected_tier)]))
fwrite(cov,file.path(out,'macro_coverage_by_tier.csv'))
print(cov[historical_lmic_reporting_scope==TRUE & selected_tier=='peer'])
cat('Training labels by year\n');print(z[,.(ratings=sum(is.finite(rating_notch))),by=analysis_year])
