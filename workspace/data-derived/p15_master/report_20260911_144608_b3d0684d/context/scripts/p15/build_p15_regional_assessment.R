#!/usr/bin/env Rscript
suppressPackageStartupMessages({library(data.table);library(ggplot2);library(digest)})
source("R/research_governance.R")
args<-commandArgs(trailingOnly=TRUE)
out<-if(length(args))args[1] else "data-derived/p15_regional_assessment_20260908_v2"
if(dir.exists(out))stop("Refusing overwrite: ",out)
base<-Sys.getenv("P15_ANALYSIS_BASE","data-derived/p15_analysis_candidate_20260907_v1")
inputs<-c(file.path(base,c("core_evidence.csv","tier_eligibility.csv","selected_reference.csv","output_manifest.csv")),
 "data-raw/world_bank_countries.json","docs/governance/P15_REGIONAL_ASSESSMENT_PROTOCOL_2026-09-08.md",
 "docs/governance/P15_YOY_AND_EXTREME_CASE_USE_2026-09-08.md")
before<-vapply(inputs,digest,character(1),file=TRUE,algo="sha256")
p<-fread(inputs[1]);e<-fread(inputs[2]);s<-fread(inputs[3]);original<-fread(inputs[4])
w<-jsonlite::fromJSON(inputs[5])[[2]]
geo<-data.table(iso3=w$id,region_current=trimws(w$region$value))[region_current!="Aggregates"]
geo[,region:=region_current]
geo[grepl("Middle East",region),region:="Middle East & North Africa"]
geo[iso3%in%c("AFG","PAK"),region:="South Asia"]
stopifnot(!anyDuplicated(geo$iso3),!anyDuplicated(p[,.(iso3,analysis_year)]))
d<-merge(p[,.(iso3,analysis_year,country,historical_income_level,historical_lmic_reporting_scope)],geo,by="iso3",all.x=TRUE)
d[is.na(region),`:=`(region="Region unavailable",region_current="Region unavailable")]
for(t in c("primary","ids","secondary","moodys","peer")){
 a<-e[tier==t];at<-match(paste(d$iso3,d$analysis_year),paste(a$iso3,a$analysis_year))
 stopifnot(!anyNA(at));set(d,j=t,value=ifelse(a$eligible[at],a$rate_pct[at],NA_real_))
}
at<-match(paste(d$iso3,d$analysis_year),paste(s$iso3,s$analysis_year))
d[,`:=`(selected=s$selected_rate_pct[at],selected_source=s$selected_tier[at])]
d[,observed:=fcoalesce(primary,secondary)]
d[, observed_source := fcase(is.finite(primary), "primary", is.finite(secondary), "secondary", default = "missing")]
d[,no_peer:=fcoalesce(primary,ids,secondary,moodys)]
d[,no_peer_source:=fcase(is.finite(primary),"primary",is.finite(ids),"ids",is.finite(secondary),"secondary",is.finite(moodys),"moodys",default="missing")]
d[,reviewed_secondary_restriction:=(iso3=="LBN"&analysis_year%in%2020:2023)|(iso3=="BLR"&analysis_year%in%2022:2024)|(iso3=="RUS"&analysis_year==2022)]
views<-c("selected","no_peer","observed","primary","secondary","ids","moodys")
long<-rbindlist(lapply(views,function(v){z<-copy(d);z[,`:=`(view=v,rate=get(v),source=if(v%in%c("selected","observed","no_peer"))get(paste0(v,"_source")) else v)];z}))
long[,reviewed_eight_exclusion:=reviewed_secondary_restriction&source=="secondary"]
long<-rbindlist(list(cbind(scope="historical_LMIC",long[historical_lmic_reporting_scope==TRUE]),cbind(scope="all_economies",long)))
safe<-function(x,f)if(any(is.finite(x)))f(x[is.finite(x)]) else NA_real_
summary<-function(z){x<-z$rate[is.finite(z$rate)];data.table(universe=nrow(z),n=length(x),
 mean_rate=if(length(x))mean(x) else NA_real_,median_rate=if(length(x))median(x) else NA_real_,
 minimum=if(length(x))min(x) else NA_real_,maximum=if(length(x))max(x) else NA_real_,
 peer_n=sum(z$source=="peer"&is.finite(z$rate)),secondary_n=sum(z$source=="secondary"&is.finite(z$rate)))}
tab<-list(country_region_map=d[,.(iso3,country,region,region_current)][!duplicated(iso3)],
 scope_counts=p[,.(rows=.N,countries=uniqueN(iso3)),by=.(historical_income_level,historical_lmic_reporting_scope)],
 regional_levels=long[,summary(.SD),by=.(scope,region,analysis_year,view)],
 current_region_sensitivity=long[,summary(.SD),by=.(scope,region_current,analysis_year,view)],
 country_year_views=long[scope=="historical_LMIC",.(iso3,country,analysis_year,region,view,rate,source,reviewed_eight_exclusion)])
tab$regional_source_mix<-d[historical_lmic_reporting_scope==TRUE,.N,by=.(region,analysis_year,selected_source)]
# Complete year grid already exists in each country's panel; LMIC scope may enter/exit.
tr<-merge(long,long[,.(scope,view,region,iso3,analysis_year=analysis_year+1L,prior_rate=rate,prior_source=source)],
 by=c("scope","view","region","iso3","analysis_year"),all=FALSE)
tr<-tr[is.finite(rate)&is.finite(prior_rate)]
tr[,`:=`(change=rate-prior_rate,source_switched=source!=prior_source)]
tab$yoy_details<-tr[,.(scope,region,view,iso3,analysis_year,prior_rate,rate,prior_source,source,change,source_switched)]
tab$regional_yoy<-tr[,.(n=.N,mean_change=mean(change),median_change=median(change),
 switches=sum(source_switched),same_source_n=sum(!source_switched),same_source_mean=safe(change[!source_switched],mean)),by=.(scope,region,view,analysis_year)]
levels<-tab$regional_levels
prev<-levels[,.(scope,region,view,analysis_year=analysis_year+1L,prior_n=n,prior_mean=mean_rate)]
comp<-merge(merge(levels,prev,by=c("scope","region","view","analysis_year")),tab$regional_yoy,by=c("scope","region","view","analysis_year"),suffixes=c("_current","_common"),all.x=TRUE)
comp[,`:=`(raw_change=mean_rate-prior_mean,composition_component=mean_rate-prior_mean-mean_change)]
tab$regional_composition<-comp
tab$matched_view_comparisons<-rbindlist(lapply(setdiff(views,"selected"),function(v){z<-d[historical_lmic_reporting_scope==TRUE&is.finite(selected)&is.finite(get(v))];
 z[,.(n=.N,selected_mean=mean(selected),alternative_mean=mean(get(v)),mean_gap=mean(get(v)-selected),mae=mean(abs(get(v)-selected))),by=.(region,analysis_year)][,view:=v]}))
val<-d[historical_lmic_reporting_scope==TRUE&is.finite(moodys)&is.finite(observed)]
tab$rating_observed_regional<-val[,.(n=.N,countries=uniqueN(iso3),bias=mean(moodys-observed),mae=mean(abs(moodys-observed)),primary_n=sum(observed_source=="primary"),secondary_n=sum(observed_source=="secondary")),by=region]
tab$rating_observed_year<-val[,.(n=.N,bias=mean(moodys-observed),mae=mean(abs(moodys-observed))),by=.(region,analysis_year)]
filtered<-copy(long);filtered[reviewed_eight_exclusion==TRUE,rate:=NA_real_]
tab$reviewed_eight_sensitivity<-filtered[,summary(.SD),by=.(scope,region,analysis_year,view)]
checks<-data.table(check=c("211_economies_13_years","1756_LMIC_rows","142_ever_LMIC_economies","no_LMIC_unmapped_regions","eligible_observed_union_316","primary_215_secondary_251","selected_reproduced","yoy_no_country_duplicates","composition_identity","all_candidate_hashes_unchanged"),
 passed=c(nrow(p)==2743, sum(p$historical_lmic_reporting_scope)==1756,uniqueN(p[historical_lmic_reporting_scope==TRUE,iso3])==142,
 !any(d[historical_lmic_reporting_scope==TRUE,region]=="Region unavailable"),sum(is.finite(d[historical_lmic_reporting_scope==TRUE,observed]))==316,
 sum(is.finite(d[historical_lmic_reporting_scope==TRUE,primary]))==215&&sum(is.finite(d[historical_lmic_reporting_scope==TRUE,secondary]))==251,
 all(abs(d$selected-fcoalesce(d$primary,d$ids,d$secondary,d$moodys,d$peer))<1e-10,na.rm=TRUE),
 !anyDuplicated(tab$yoy_details[,.(scope,view,iso3,analysis_year)]),all(abs(comp$raw_change-comp$mean_change-comp$composition_component)<1e-10,na.rm=TRUE),
 all(vapply(original$artifact_path,digest,character(1),file=TRUE,algo="sha256")==original$sha256)))
stopifnot(all(checks$passed));tab$checks<-checks
dir.create(out,recursive=TRUE)
for(n in names(tab))fwrite(tab[[n]],file.path(out,paste0(n,".csv")))
labs_view<-c(selected="Selected ladder (with peers)",observed="Observed: primary, then secondary",moodys="Moody's implied",primary="Primary issuance",secondary="Secondary market",ids="IDS Bondholders")
cols<-c(selected="#333333",observed="#2378A0",moodys="#C47529",primary="#2378A0",secondary="#B34A48",ids="#638246")
theme_set(theme_minimal(base_size=11)+theme(panel.grid.minor=element_blank(),legend.position="bottom",legend.title=element_blank(),plot.caption=element_text(hjust=0,size=9)))
savefig<-function(name,g){ggsave(file.path(out,paste0(name,".png")),g,width=13,height=8,dpi=150,bg="white");ggsave(file.path(out,paste0(name,".pdf")),g,width=13,height=8,device="pdf")}
plotdata<-function(v){z<-copy(levels[scope=="historical_LMIC"&view%in%v]);z[n<3,mean_rate:=NA_real_];z}
g<-ggplot(plotdata(c("selected","observed","moodys")),aes(analysis_year,mean_rate,color=view))+geom_line(na.rm=TRUE)+geom_point(size=1.2,na.rm=TRUE)+facet_wrap(~region,ncol=3)+scale_color_manual(values=cols,labels=labs_view)+labs(title="Regional borrowing benchmarks: three evidence views",subtitle="Historical low- and middle-income economies, 2012-2024; equal-country means",x=NULL,y="Rate (%)",caption="Descriptive levels have changing samples. Cells with fewer than 3 countries are not plotted.\nFixed pre-July-2025 WB regions; selected ladder includes IDS and optional peers, not a pure USD market index.")
savefig("figure_1_regional_levels",g)
g<-ggplot(plotdata(c("primary","secondary","ids")),aes(analysis_year,mean_rate,color=view))+geom_line()+geom_point(size=1.2)+facet_wrap(~region,ncol=3)+scale_color_manual(values=cols,labels=labs_view)+labs(title="Primary issuance, secondary markets and IDS describe different evidence",subtitle="Equal-country means on each source's available LMIC sample",x=NULL,y="Rate (%)",caption="Primary and secondary are USD; IDS is a contractual proxy with currency qualifications. No raw currency pooling.\nFewer than 3 countries: not plotted. Extreme secondary observations remain visible; this is not a screened normal-market series.")
savefig("figure_2_observed_and_ids",g)
y<-copy(tab$regional_yoy[scope=="historical_LMIC"&view%in%c("selected","observed","moodys")]);y[n<3,mean_change:=NA_real_]
g<-ggplot(y,aes(analysis_year,mean_change,color=view))+geom_hline(yintercept=0,color="grey65")+geom_line()+geom_point(size=1.2)+facet_wrap(~region,ncol=3)+scale_color_manual(values=cols,labels=labs_view)+labs(title="Annual changes among the same countries in both years",subtitle="Equal-country changes; the common-country set differs between adjacent pairs",x=NULL,y="Change (percentage points)",caption="Fewer than 3 common countries: not plotted. Source and peer-membership changes can still contribute.\nDo not chain these comparisons into a fixed-panel level index; see source-switch counts in the supporting tables.")
savefig("figure_3_common_country_yoy",g)
cvg<-copy(levels[scope=="historical_LMIC"&view%in%c("selected","observed","moodys")]);cvg[,share:=n/universe]
g<-ggplot(cvg,aes(analysis_year,share,color=view))+geom_line()+geom_point(size=1)+facet_wrap(~region,ncol=3)+scale_color_manual(values=cols,labels=labs_view)+scale_y_continuous(labels=scales::label_percent(),limits=c(0,1))+labs(title="How much of each region is represented?",subtitle="Share of in-scope LMIC economies with an eligible rate in each view",x=NULL,y="Country coverage",caption="Broad selected coverage includes modelled values. Coverage is not the share of regional debt or proof of representativeness.\nSource: fixed P15 candidate; saved World Bank regional mapping with the documented 2025 change reversed.")
savefig("figure_4_regional_coverage",g)
manifest<-function(paths,role)pvr_manifest_rows(paths,role,basename(out),"SCHEMA-P15-REGIONAL-ASSESSMENT-V1","EST-P15-REGIONAL-DESCRIPTIVE-V1","ADM-P15-SOURCE-CLOSURE-V1","SEL-P15-NO-PROMOTION-V1","SRC-P15-RAW-CLOSURE-20260906;SRC-P15-SOURCE-CLOSURE-20260907")
fwrite(manifest(inputs,"regional_input"),file.path(out,"input_manifest.csv"))
fwrite(manifest(c("scripts/p15/build_p15_regional_assessment.R","R/research_governance.R"),"regional_code"),file.path(out,"script_manifest.csv"))
fwrite(manifest(list.files(out,full.names=TRUE),"regional_output"),file.path(out,"output_manifest.csv"))
capture.output(sessionInfo(),file=file.path(out,"environment.txt"))
stopifnot(identical(before,vapply(inputs,digest,character(1),file=TRUE,algo="sha256")))
print(checks);print(tab$rating_observed_regional)
