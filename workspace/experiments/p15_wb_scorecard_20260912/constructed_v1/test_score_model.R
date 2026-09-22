source('scripts/p15/activate_p15_environment.R');library(data.table)
b<-'experiments/p15_wb_scorecard_20260912/constructed_v1';source(file.path(b,'score_model.R'))
checks<-data.table(check=character(),passed=logical())
check<-function(name,ok){stopifnot(ok);checks<<-rbind(checks,data.table(check=name,passed=TRUE))}
check('every_indicator_has14_ordered_distinct_cutoffs',all(vapply(cutoffs,function(c)length(unique(c$cuts))==14L,logical(1))))
for(name in names(cutoffs)){
 cc<-sort(cutoffs[[name]]$cuts);v<-sort(c(cc-1e-8,cc,cc+1e-8));z<-indicator(v,name)
 check(paste0(name,'_direction_and_endpoints'),all(diff(z)*(if(cutoffs[[name]]$direction=='low')1 else -1) >=0))
 check(paste0(name,'_missing_propagates'),is.na(indicator(NA_real_,name)))
}
check('inflation_deflation_and_exact_boundaries',identical(inflation_index(c(-1,0,.1,1.2,1.3,2.49,2.5,24.99,25,NA)),c(15L,14L,13L,2L,1L,1L,2L,14L,15L,NA_integer_)))
check('zero_weight_interest_does_not_block_IDA',weighted_factor(data.frame(a=1L,b=1L,c=NA_integer_,d=NA_integer_),matrix(c(.5,.5,0,0),1))==1L)
check('positive_weight_missing_interest_blocks_normal_country',is.na(weighted_factor(data.frame(a=1L,b=1L,c=NA_integer_,d=NA_integer_),matrix(.25,1,4))))
check('uneven_midpoints_not_average_ordinal_positions',weighted_factor(data.frame(a=1L,b=15L),matrix(.5,1,2))==8L)
check('factor_rounding85inclusive',identical(factor_index(c(85,84.999,80,20,19.99)),c(1L,2L,2L,14L,15L)))
mat<-read_exact_matrices(b)
# Six independent published2019 examples, preserved before this construction.
ex<-fread('experiments/p15_wb_scorecard_20260912/methodology/independent_country_example_checks.csv')
for(i in 1:nrow(ex)){
 er<-mat$economic_resiliency[ex$f2[i],ex$f1[i]];gfs<-mat$government_financial_strength[er,ex$f3[i]]
 z<-match(mat$rating_midpoint[ex$f4[i],gfs],rating_labels)
 check(paste0('independent_',ex$country[i]),rating_labels[z-1]==ex$published_better[i]&rating_labels[z+1]==ex$published_worse[i])
}
# Exhaustive integer factor grid verifies endpoint property used for bounds.
g<-CJ(f1=1:15,f2=1:15,f3=1:15);g[,z:=aggregate_factors(f1,f2,f3,mat)]
check('3375combinations_are_monotonic',all(g[,all(diff(z)>=0),by=.(f1,f2)]$V1)&&all(g[order(f1,f3,f2),all(diff(z)>=0),by=.(f1,f3)]$V1)&&all(g[order(f2,f3,f1),all(diff(z)>=0),by=.(f2,f3)]$V1))
y<-fread(file.path(b,'scorecard_country_year.csv'));x<-fread(file.path(b,'inputs.csv'))
check('intervals_ordered',all(y$shadow_notch_lower<=y$shadow_notch&y$shadow_notch<=y$shadow_notch_upper,na.rm=TRUE))
check('conditional_models_never_certified_as_exact',!any(y$scorecard_complete))
check('no_future_WEO_vintage',all(as.integer(substr(x$vintage,1,4))==x$reference_year))
check('IDA_graduation_and_reentry',x[iso3=='IND'&analysis_year==2014,ida]&&!x[iso3=='IND'&analysis_year==2015,ida]&&x[iso3=='LKA'&analysis_year==2023,ida]&&!x[iso3=='LKA'&analysis_year==2022,ida])
check('late_new_IDA_not_backcast',!any(x[iso3%in%c('BLZ','SWZ','SUR'),ida]))
check('future_2024_default_excluded',x[iso3=='ARG'&analysis_year==2024,moodys_prior20_events]==3L)
fwrite(checks,file.path(b,'scorecard_checks.csv'));print(checks)
