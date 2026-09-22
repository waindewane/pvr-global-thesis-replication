# Additional diagnostics use the original engine functions but write new tables.
source('experiments/p15_peer_rating_expansion_20260912/explore_peers.R')
extra<-fread(file.path(out,'shadow_additional_predictions.csv'))
for(m in unique(extra$model))x[,(m):=extra[model==m,predicted_notch][match(paste(iso3,analysis_year),extra[model==m,paste(iso3,analysis_year)])]]
x[,fiscal_core_cascade:=fcoalesce(fiscal_ols,core_ols)]
web<-fread(file.path(out,'web_rating_peer_reconciliation.csv'))
web[iso3=='CUB' & analysis_year==2024,web_notch:=NA_real_]
x[,web_moody:=web$web_notch[match(paste(iso3,analysis_year),paste(web$iso3,web$analysis_year))]]
x[,web_then_fiscal:=fcoalesce(as.numeric(web_moody),fiscal_ols)]
sp<-c('AAA','AA+','AA','AA-','A+','A','A-','BBB+','BBB','BBB-','BB+','BB','BB-','B+','B','B-','CCC+','CCC','CCC-','CC','C')
a<-fread('experiments/p15_rating_coverage_shadow_20260912/shadow2011_annex_mapped.csv')
a[,clean:=gsub('−','-',rating_range_printed,fixed=TRUE)]
a[,snapshot_notch:=vapply(strsplit(clean,' to ',fixed=TRUE),function(s)if(all(s%in%sp))mean(match(s,sp)) else NA_real_,numeric(1))]
a[older_prediction_asterisk==TRUE,snapshot_notch:=NA_real_]
b<-fread('experiments/p15_rating_coverage_shadow_20260912/shadow2012_table7_mapped.csv')
b[,snapshot_notch:=match(sub(' .*','',rating_range_printed),sp)]
snap<-rbind(a[,.(iso3,analysis_year=2012L,snapshot_notch)],a[,.(iso3,analysis_year=2013L,snapshot_notch)],b[,.(iso3,analysis_year=2014L,snapshot_notch)])
# The 2013 paper is usable by Jan1 2014. Do not backdate its publication or carry
# its Dec2012 assessment into Jan2015, when it is more than two years old.
x[,wb_snapshots:=snap$snapshot_notch[match(paste(iso3,analysis_year),paste(snap$iso3,snap$analysis_year))]]
fwrite(snap,file.path(out,'bounded_worldbank_snapshot_inputs.csv'))
des<-CJ(sources=c('P','PIS'),rating=c('fiscal_lag2','fiscal_recent_events','fiscal_core_cascade','wb_snapshots','web_moody','web_then_fiscal'))
des[,`:=`(method=paste(sources,rating,'stop5',sep='_'),cutoff=5,width=3,skip4=FALSE,model_donors=FALSE)]
features<-fread(file.path(out,'model_feature_panel.csv'))
nv<-c('log_gdp_pc','growth3','asinh_inflation','rule_law','general_debt','fiscal_balance')
x<-merge(x,features[,c('iso3','analysis_year',nv),with=FALSE],by=c('iso3','analysis_year'),all.x=TRUE)
res<-list();k<-0
for(j in seq_len(nrow(des)))for(y in 2012:2024){
 de<-des[j]
 pool<-merge(seeds[sources==de$sources & analysis_year==y],x[,c('iso3','analysis_year',unique(des$rating)),with=FALSE],by=c('iso3','analysis_year'))
 targets<-x[analysis_year==y & historical_lmic_reporting_scope & (selected_tier%in%c('peer','no_eligible_rate')|is.finite(actual))]
 for(i in seq_len(nrow(targets))){t<-targets[i]
  if(t$selected_tier%in%c('peer','no_eligible_rate')){k<-k+1;res[[k]]<-one(t,pool,de,FALSE)}
  # Actual-rating additions are only available for the 778 target cohort and
  # cannot form a fair hidden-rating validation for the primary cohort.
  if(is.finite(t$actual) && !de$rating%in%c('web_moody','web_then_fiscal')){k<-k+1;res[[k]]<-one(t,pool,de,TRUE)}
 }
}
for(src in c('P','PIS'))for(y in 2012:2024){
 pool<-merge(seeds[sources==src & analysis_year==y],x[,c('iso3','analysis_year',nv),with=FALSE],by=c('iso3','analysis_year'))
 pool<-pool[complete.cases(pool[,nv,with=FALSE])]
 norms<-features[analysis_year<y,lapply(.SD,sd,na.rm=TRUE),.SDcols=nv]
 targets<-x[analysis_year==y & historical_lmic_reporting_scope & (selected_tier%in%c('peer','no_eligible_rate')|is.finite(actual))]
 for(i in seq_len(nrow(targets))){t<-targets[i]
  pp<-pool[iso3!=t$iso3];dist<-rep(Inf,nrow(pp))
  if(all(is.finite(as.numeric(t[,nv,with=FALSE])))){
   dif<-sweep(as.matrix(pp[,nv,with=FALSE]),2,as.numeric(t[,nv,with=FALSE]),'-')
   dist<-sqrt(rowMeans(sweep(dif,2,as.numeric(norms),'/')^2))
  }
  ok<-which(dist<=1 & ((pp$historical_income_level==t$historical_income_level)|(pp$rating_source_region==t$rating_source_region)))
  use<-if(length(ok)>=3)ok[order(dist[ok],pp$iso3[ok])][1:3] else integer()
  rr<-data.table(iso3=t$iso3,analysis_year=y,method=paste0(src,'_macro_nearest3'),mode=if(t$selected_tier%in%c('peer','no_eligible_rate'))'deployment' else 'target_rating_hidden',rule=NA_integer_,estimate=if(length(use)==3)median(pp$seed_rate[use]) else NA_real_,n_peers=length(use),target_notch=NA_real_,target_supplement_used=FALSE,primary_members=sum(pp$seed_source[use]=='primary'),ids_members=sum(pp$seed_source[use]=='ids'),secondary_members=sum(pp$seed_source[use]=='secondary'),model_donor_members=0L,member_ids=paste(sort(pp$iso3[use]),collapse=';'),member_sources=paste(paste(pp$iso3[use],pp$seed_source[use],sep=':'),collapse=';'))
  k<-k+1;res[[k]]<-rr
 }
}
q<-rbindlist(res);q<-merge(q,x[,.(iso3,analysis_year,country,historical_income_level,selected_tier,selected_rate_pct,ordinary_fallback_selection_permitted,actual)],by=c('iso3','analysis_year'))
q[,selected_peer_new:=mode=='deployment' & is.finite(estimate) & !(ordinary_fallback_selection_permitted%in%FALSE)]
fwrite(q,file.path(out,'peer_additional_predictions.csv'))
fwrite(rbind(des,data.table(method=c('P_macro_nearest3','PIS_macro_nearest3'),sources=c('P','PIS')),fill=TRUE),file.path(out,'peer_additional_designs.csv'))
