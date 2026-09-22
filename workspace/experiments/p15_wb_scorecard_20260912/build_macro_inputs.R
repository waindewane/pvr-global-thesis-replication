# Offline reconstruction of WB2021 Appendix D macro windows from frozen WEO vintages.
source('scripts/p15/activate_p15_environment.R')
suppressPackageStartupMessages(library(data.table))
out <- 'experiments/p15_wb_scorecard_20260912/macro'
subjects <- c('NGDP_RPCH','PCPIPCH','NGDPD','PPPPC','GGXWDG_NGDP','GGXCNL_NGDP','GGXONLB_NGDP','GGR_NGDP')
ptr <- jsonlite::fromJSON('data-derived/p15_master/current_run.json')
core <- fread(file.path(ptr$candidate,'core_evidence.csv'))
sel <- fread(file.path(ptr$candidate,'selected_reference.csv'))
stopifnot(uniqueN(core$iso3)==211L, nrow(core)==2743L)
manifest <- as.data.table(jsonlite::fromJSON(file.path(out,'weo_download_manifest.json')))
if(!'error'%in%names(manifest))manifest[,error:=NA_character_]
valid <- manifest[is.na(error) | error=='']
for(i in seq_len(nrow(valid))) stopifnot(digest::digest(file=file.path(out,valid$file[i]),algo='sha256')==valid$sha256[i])
num <- function(x) suppressWarnings(as.numeric(gsub(',','',trimws(as.character(x)),fixed=TRUE)))
allraw <- list(); metarows <- list(); checks <- list()
for(vy in 2011:2023) {
 rec <- valid[vintage_year==vy]; direct <- rec[format=='IMF_TSV']; mirrors <- rec[format=='DBnomics_JSON']
 if(nrow(direct)) {
  f <- file.path(out,direct$file[1]); z <- fread(f,sep='\t',quote='',fill=TRUE,check.names=FALSE,colClasses='character',na.strings=c('','n/a','--','NA'))
  years <- names(z)[grepl('^[0-9]{4}$',names(z))]
  keep <- z[['WEO Subject Code']] %in% subjects
  z <- z[keep]; stopifnot(!anyDuplicated(z[,.(ISO,`WEO Subject Code`)]))
  meta <- z[,.(iso3=ISO,indicator=`WEO Subject Code`,country=Country,subject_descriptor=`Subject Descriptor`,subject_notes=`Subject Notes`,units=Units,scale=Scale,country_series_notes=`Country/Series-specific Notes`,estimates_start_after=num(`Estimates Start After`))]
  z <- melt(z,id.vars=c('ISO','WEO Subject Code'),measure.vars=years,variable.name='year',value.name='value')
  setnames(z,c('ISO','WEO Subject Code'),c('iso3','indicator'));z[,`:=`(year=as.integer(as.character(year)),value=num(value))]
  z <- merge(z,meta[,.(iso3,indicator,estimates_start_after)],by=c('iso3','indicator'),all.x=TRUE)
  z[,`:=`(vintage_year=vy,vintage_month=direct$vintage_month[1],source_route='IMF_direct_TSV',source_file=direct$file[1])]
  checks[[length(checks)+1L]] <- data.table(vintage_year=vy,source_route='IMF_direct_TSV',series_expected=nrow(meta),series_retrieved=uniqueN(z[,.(iso3,indicator)]),pages=1L,all_subjects_present=all(subjects%in%meta$indicator),countries=uniqueN(meta$iso3))
 } else if(nrow(mirrors)) {
  docs<-list();lastn<-NA_integer_
  for(f in mirrors$file) {d<-jsonlite::fromJSON(file.path(out,f),simplifyVector=FALSE);lastn<-d$series$num_found;docs<-c(docs,d$series$docs)}
  stopifnot(length(docs)==lastn,!anyDuplicated(vapply(docs,`[[`,character(1),'series_code')))
  z<-rbindlist(lapply(docs,function(s)data.table(iso3=s$dimensions[['weo-country']],indicator=s$dimensions[['weo-subject']],year=as.integer(unlist(s$period)),value=num(unlist(s$value)),estimates_start_after=NA_real_)))
  meta<-rbindlist(lapply(docs,function(s)data.table(iso3=s$dimensions[['weo-country']],indicator=s$dimensions[['weo-subject']],country=NA_character_,subject_descriptor=s$series_name,subject_notes=NA_character_,units=s$dimensions$unit,scale=NA_character_,country_series_notes=NA_character_,estimates_start_after=NA_real_)))
  z[,`:=`(vintage_year=vy,vintage_month=10L,source_route='IMF_DBnomics_mirror',source_file=paste(mirrors$file,collapse=';'))]
  checks[[length(checks)+1L]]<-data.table(vintage_year=vy,source_route='IMF_DBnomics_mirror',series_expected=lastn,series_retrieved=length(docs),pages=nrow(mirrors),all_subjects_present=all(subjects%in%meta$indicator),countries=uniqueN(meta$iso3))
 } else stop(paste('No complete WEO source for',vy))
 meta[,`:=`(vintage_year=vy,source_country_code=iso3)];meta[iso3=='UVK',iso3:='XKX'];meta[iso3=='WBG',iso3:='PSE']
 z[,source_country_code:=iso3];z[iso3=='UVK',iso3:='XKX'];z[iso3=='WBG',iso3:='PSE']
 metarows[[length(metarows)+1L]]<-meta
 z[,`:=`(reference_year=vy,analysis_year=vy+1L,vintage=paste0(vy,'-',sprintf('%02d',vintage_month)),is_after_vintage_year=year>vy)]
 z[,is_imf_estimate:=fifelse(is.finite(estimates_start_after),year>estimates_start_after,NA)]
 allraw[[length(allraw)+1L]]<-z[iso3%in%core$iso3 & year>=vy-9L & year<=vy+2L]
}
raw<-rbindlist(allraw);stopifnot(!anyDuplicated(raw[,.(iso3,indicator,vintage,year)]))
fwrite(raw,file.path(out,'weo_required_observations_long.csv'))
metadata<-rbindlist(metarows,fill=TRUE);fwrite(metadata,file.path(out,'weo_series_metadata.csv'));fwrite(unique(metadata[,.(source_country_code,iso3,in_current_universe=iso3%in%core$iso3)]),file.path(out,'weo_country_code_mapping.csv'))
coverage<-rbindlist(checks);stopifnot(all(coverage$series_expected==coverage$series_retrieved),all(coverage$all_subjects_present));fwrite(coverage,file.path(out,'weo_vintage_completeness.csv'))
w<-dcast(raw,iso3+analysis_year+reference_year+vintage+vintage_month+year+source_route~indicator,value.var='value')
for(v in subjects)if(!v%in%names(w))w[,(v):=NA_real_]
calc <- function(z,t) {
 stopifnot(length(t)==1L)
 getat<-function(v,yr) {r<-z[year==yr,get(v)];if(length(r))r else NA_real_}
 win<-function(v,start,end) {x<-z[year>=start & year<=end,get(v)];n<-sum(is.finite(x));expected<-end-start+1L;list(mean=if(n==expected)mean(x) else NA_real_,sd=if(n==expected)sd(x) else NA_real_,n=n,partial_mean=if(n>0)mean(x,na.rm=TRUE) else NA_real_)}
 g7<-win('NGDP_RPCH',t-4L,t+2L);g10<-win('NGDP_RPCH',t-9L,t);p7<-win('PCPIPCH',t-4L,t+2L);p10<-win('PCPIPCH',t-9L,t)
 rev<-getat('GGR_NGDP',t);debt<-getat('GGXWDG_NGDP',t);pb<-getat('GGXONLB_NGDP',t);ob<-getat('GGXCNL_NGDP',t)
 ni<-pb-ob; d4<-getat('GGXWDG_NGDP',t-4L);d1<-getat('GGXWDG_NGDP',t+1L)
 data.table(growth_avg7=g7$mean,growth_sd10=g10$sd,inflation_avg7=p7$mean,inflation_sd10=p10$sd,
 growth_avg7_n=g7$n,growth_sd10_n=g10$n,inflation_avg7_n=p7$n,inflation_sd10_n=p10$n,
 growth_avg7_partial=g7$partial_mean,inflation_avg7_partial=p7$partial_mean,
 nominal_gdp_usd_bn=getat('NGDPD',t),gdp_pc_ppp=getat('PPPPC',t),debt_gdp=debt,revenue_gdp=rev,
 debt_revenue=if(is.finite(rev)&&rev>0)100*debt/rev else NA_real_,primary_balance_gdp=pb,overall_balance_gdp=ob,
 implied_net_interest_gdp=ni,implied_net_interest_revenue=if(is.finite(rev)&&rev>0)100*ni/rev else NA_real_,
 net_interest_negative=is.finite(ni)&&ni<0,debt_gdp_t_minus4=d4,debt_gdp_t_plus1=d1,debt_trend_pp=d1-d4,
 debt_trend_relative_pct=if(is.finite(d4)&&d4>0)100*(d1-d4)/d4 else NA_real_)
}
panel<-w[,calc(.SD,.BY$reference_year),by=.(iso3,analysis_year,reference_year,vintage,vintage_month,source_route)]
grid<-merge(core[,.(iso3,analysis_year,country,historical_income_level,historical_lmic_reporting_scope,rating_moodys_rating)],sel[,.(iso3,analysis_year,selected_tier)],by=c('iso3','analysis_year'))
panel<-merge(grid,panel,by=c('iso3','analysis_year'),all.x=TRUE)
panel[is.na(reference_year),reference_year:=analysis_year-1L]
panel[is.na(vintage),vintage:=paste0(reference_year,'-',ifelse(reference_year==2011,'09','10'))]
panel[,`:=`(interest_measure='primary_minus_overall_balance_equals_NET_interest;not_gross_expense',gross_interest_gdp=NA_real_,gross_interest_revenue=NA_real_,fallback_used=FALSE,macro_information_date=paste0(vintage,'-31'),rating_application_date=paste0(analysis_year,'-01-01'))]
panel[reference_year==2011,macro_information_date:='2011-09-30']
vars<-c('growth_avg7','growth_sd10','inflation_avg7','inflation_sd10','nominal_gdp_usd_bn','gdp_pc_ppp','debt_gdp','debt_revenue','implied_net_interest_gdp','implied_net_interest_revenue','debt_trend_pp')
panel[,macro_complete_net_interest_proxy:=rowSums(is.na(.SD))==0L,.SDcols=vars]
panel[,missing_macro_fields:=apply(.SD,1,function(x)paste(vars[is.na(x)],collapse=';')),.SDcols=vars]
stopifnot(nrow(panel)==2743L,!anyDuplicated(panel[,.(iso3,analysis_year)]),all(panel$reference_year==panel$analysis_year-1L))
# General-government gross interest expense exists in GFS, but this is a later
# revised snapshot, without vintage-specific forecasts. It is a separate sensitivity.
gfs_manifest <- as.data.table(jsonlite::fromJSON(file.path(out,'gfs_interest_manifest.json')))
gfs_docs<-list()
for(i in seq_len(nrow(gfs_manifest))) {
 f<-file.path(out,gfs_manifest$file[i]);stopifnot(digest::digest(file=f,algo='sha256')==gfs_manifest$sha256[i])
 d<-jsonlite::fromJSON(f,simplifyVector=FALSE);gfs_docs<-c(gfs_docs,d$series$docs);gfs_n<-d$series$num_found;gfs_updated<-d$dataset$updated_at
}
stopifnot(length(gfs_docs)==gfs_n,!anyDuplicated(vapply(gfs_docs,`[[`,character(1),'series_code')))
gfs<-rbindlist(lapply(gfs_docs,function(s) {
 stopifnot(s$dimensions$REF_SECTOR=='S13',s$dimensions$UNIT_MEASURE=='XDC_R_B1GQ',s$dimensions$FREQ=='A')
 data.table(iso2=s$dimensions$REF_AREA,indicator=s$dimensions$CLASSIFICATION,reference_year=as.integer(unlist(s$period)),value=num(unlist(s$value)),sector=s$dimensions$REF_SECTOR,units=s$dimensions$UNIT_MEASURE,source_series=s$series_code,source_indexed_at=s$indexed_at)
}))
gfs[,iso3:=countrycode::countrycode(iso2,'iso2c','iso3c',custom_match=c('XK'='XKX'))]
fwrite(unique(gfs[is.na(iso3),.(iso2,source_series)]),file.path(out,'gfs_unmapped_series.csv'))
gfs<-gfs[iso3%in%core$iso3 & reference_year%in%2011:2023]
fwrite(gfs,file.path(out,'gfs_general_government_interest_long.csv'))
gfswide<-dcast(gfs,iso3+reference_year~indicator,value.var='value')
setnames(gfswide,c('G24__Z','G1__Z'),c('gfs_gross_interest_gdp_latest','gfs_revenue_gdp_latest'))
gfswide[,gfs_gross_interest_revenue_latest:=fifelse(gfs_revenue_gdp_latest>0,100*gfs_gross_interest_gdp_latest/gfs_revenue_gdp_latest,NA_real_)]
panel<-merge(panel,gfswide,by=c('iso3','reference_year'),all.x=TRUE)
panel[,`:=`(gfs_snapshot_updated=gfs_updated,gfs_information_basis='latest_revised_GFS_not_historical_vintage;observed_t_without_forecasts')]
fwrite(panel,file.path(out,'macro_input_panel.csv'))
fwrite(rbindlist(lapply(vars,function(v)panel[,.(variable=v,total=.N,available=sum(is.finite(get(v)))),by=.(historical_lmic_reporting_scope,selected_tier)])),file.path(out,'macro_coverage_by_tier.csv'))
fwrite(panel[,.(country_years=.N,complete_net_interest_proxy=sum(macro_complete_net_interest_proxy),countries=uniqueN(iso3[macro_complete_net_interest_proxy])),by=.(analysis_year,historical_lmic_reporting_scope,selected_tier)],file.path(out,'macro_coverage_by_year.csv'))
fwrite(panel[macro_complete_net_interest_proxy==FALSE,.(iso3,country,analysis_year,selected_tier,historical_income_level,missing_macro_fields)],file.path(out,'macro_missing_country_years.csv'))
fwrite(panel[net_interest_negative==TRUE],file.path(out,'negative_net_interest_diagnostics.csv'))
peer_scope<-panel[historical_lmic_reporting_scope==TRUE & selected_tier=='peer']
coverage_vars<-c(vars,'gfs_gross_interest_gdp_latest','gfs_gross_interest_revenue_latest')
fwrite(rbindlist(lapply(coverage_vars,function(v)data.table(variable=v,available=sum(is.finite(peer_scope[[v]])),total=nrow(peer_scope)))),file.path(out,'selected_peer_macro_coverage.csv'))
fwrite(peer_scope[,.(n=.N,complete_macro_net_interest_proxy=sum(macro_complete_net_interest_proxy),gross_interest=sum(is.finite(gfs_gross_interest_revenue_latest))),by=historical_income_level],file.path(out,'selected_peer_macro_coverage_income.csv'))
inputs<-c('data-derived/p15_master/current_run.json',file.path(ptr$candidate,c('core_evidence.csv','selected_reference.csv')),file.path(out,'weo_download_manifest.json'))
fwrite(data.table(path=inputs,sha256=vapply(inputs,digest::digest,character(1),file=TRUE,algo='sha256')),file.path(out,'macro_build_inputs_manifest.csv'))
capture.output(sessionInfo(),file=file.path(out,'R_session_info.txt'))
print(panel[,.(n=.N,complete=sum(macro_complete_net_interest_proxy)),by=.(historical_lmic_reporting_scope,selected_tier)])
