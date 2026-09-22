source('scripts/p15/activate_p15_environment.R')
suppressPackageStartupMessages(library(data.table))
out<-'experiments/p15_peer_rating_expansion_20260912'
e<-new.env();load(file.path(out,'sources/elshagi_rating_data_calc.RData'),envir=e)
d<-as.data.table(e$data.yields.daily)
map<-unique(d[,.(Country)])
map[,iso3:=countrycode::countrycode(Country,'country.name','iso3c',custom_match=c('Congo'='COG','Congo (Democratic Republic)'='COD','Kosovo'='XKX'))]
fwrite(map,file.path(out,'elshagi_country_mapping.csv'));stopifnot(!anyDuplicated(map$iso3[!is.na(map$iso3)]))
d[,iso3:=map$iso3[match(Country,map$Country)]]
dd<-d[format(Date,'%m-%d')=='01-01' & format(Date,'%Y')>='2005',.(iso3,Country,Date,Rating.Moody,Rating.SP,Rating.Fitch)]
dd[,analysis_year:=as.integer(format(Date,'%Y'))]
for(a in c('Moody','SP','Fitch'))dd[,(paste0(a,'_notch')):=fifelse(get(paste0('Rating.',a))%in%5:24,25-get(paste0('Rating.',a)),NA_real_)]
stopifnot(max(dd$analysis_year)==2017L,!anyDuplicated(dd[,.(iso3,analysis_year)]))
fwrite(dd,file.path(out,'elshagi_boy_ratings_2005_2017.csv'))
audit<-fread('experiments/p15_rating_coverage_shadow_20260912/rating_coverage_audit_country_year.csv')
z<-merge(audit,dd,by=c('iso3','analysis_year'),all.x=TRUE)
scale<-c('Aaa','Aa1','Aa2','Aa3','A1','A2','A3','Baa1','Baa2','Baa3','Ba1','Ba2','Ba3','B1','B2','B3','Caa1','Caa2','Caa3','Ca','C')
z[,current_notch:=match(rating_moodys_rating,scale)]
z[,`:=`(new_moody_candidate=is.na(current_notch)&is.finite(Moody_notch),moody_conflict=is.finite(current_notch)&is.finite(Moody_notch)&current_notch!=Moody_notch)]
z[,moody_gap:=Moody_notch-current_notch]
fwrite(z,file.path(out,'public_rating_reconciliation.csv'))
su<-z[analysis_year<=2017,.(total=.N,new_moody=sum(new_moody_candidate),overlap=sum(is.finite(current_notch)&is.finite(Moody_notch)),exact=sum(moody_gap==0,na.rm=TRUE),conflicts=sum(moody_conflict)),by=.(historical_lmic_reporting_scope,selected_tier)]
fwrite(su,file.path(out,'public_rating_reconciliation_summary.csv'));print(su)
fwrite(z[new_moody_candidate & historical_lmic_reporting_scope & selected_tier=='peer',.(country_years=.N,years=paste(analysis_year,collapse=';'),notches=paste(Moody_notch,collapse=';')),by=.(iso3,country)],file.path(out,'public_moody_recovery_candidates_by_country.csv'))
fwrite(z[moody_conflict==TRUE],file.path(out,'public_moody_conflicts.csv'))
print(z[new_moody_candidate & historical_lmic_reporting_scope & selected_tier=='peer',.(n=.N,years=paste(analysis_year,collapse=',')),by=country])
# Preserve withdrawals as missing; this source carries Iran's Fitch B+ past the
# documented 2008 withdrawal. Never treat its entire three-agency panel as verified.
print(dd[iso3=='IRN' & analysis_year>=2012])
