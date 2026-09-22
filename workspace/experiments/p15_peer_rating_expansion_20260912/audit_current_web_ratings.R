source('scripts/p15/activate_p15_environment.R');library(data.table)
out<-'experiments/p15_peer_rating_expansion_20260912'
ev<-fread(file.path(out,'countryeconomy_moody_events.csv'));setorder(ev,iso3,event_date)
scale<-c('Aaa','Aa1','Aa2','Aa3','A1','A2','A3','Baa1','Baa2','Baa3','Ba1','Ba2','Ba3','B1','B2','B3','Caa1','Caa2','Caa3','Ca','C')
ev[,notch:=match(rating,scale)]
duplicate_dates<-ev[,.(n=uniqueN(rating)),by=.(iso3,event_date)][n>1]
fwrite(duplicate_dates,file.path(out,'web_rating_same_date_conflicts.csv'))
stopifnot(nrow(duplicate_dates)==0)
z<-rbindlist(lapply(unique(ev$iso3),function(cc)rbindlist(lapply(2012:2024,function(y){
 e<-ev[iso3==cc & event_date<=as.IDate(paste0(y,'-01-01'))];if(!nrow(e))return(NULL)
 e<-tail(e,1);data.table(iso3=cc,analysis_year=y,web_moody=e$rating,web_notch=e$notch,web_event_date=e$event_date,web_source_url=e$source_url)
}))))
p<-fread('experiments/p15_rating_coverage_shadow_20260912/selected_peer_rating_audit.csv')
z<-merge(p,z,by=c('iso3','analysis_year'),all.x=TRUE)
z[,new_grade_candidate:=!current_valid_moodys & is.finite(web_notch)]
z[,known_withdrawal_conflict:=iso3=='CUB' & analysis_year==2024 & new_grade_candidate]
z[,screened_grade_candidate:=new_grade_candidate & !known_withdrawal_conflict]
z[,web_event_age:=analysis_year-as.integer(format(web_event_date,'%Y'))]
fwrite(z,file.path(out,'web_rating_peer_reconciliation.csv'))
fwrite(z[new_grade_candidate==TRUE,.(country_years=.N,years=paste(analysis_year,collapse=';'),rating_values=paste(unique(web_moody),collapse=';')),by=.(iso3,country)],file.path(out,'web_rating_candidates_by_country.csv'))
print(z[,.(peer_cases=.N,new_grade_candidates=sum(new_grade_candidate),countries=uniqueN(iso3[new_grade_candidate]),event_within5years=sum(new_grade_candidate&web_event_age<=5,na.rm=TRUE))])
print(z[new_grade_candidate==TRUE,.(n=.N,years=paste(analysis_year,collapse=',')),by=country])
