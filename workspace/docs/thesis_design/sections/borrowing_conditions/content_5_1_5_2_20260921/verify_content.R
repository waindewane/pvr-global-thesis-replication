suppressPackageStartupMessages({library(data.table);library(jsonlite);library(digest)})
out <- 'docs/thesis_design/sections/borrowing_conditions/content_5_1_5_2_20260921'
j <- fromJSON('data-derived/p15_master/current_run.json')
inputs <- c('data-derived/p15_master/current_run.json',file.path(out,'verify_content.R'),
 'scripts/p15/annotation_followup_20260911/build_regional_followup.R',
 'scripts/p15/build_p15_regional_assessment.R',
 'docs/thesis_design/feedback_2026-09-11/REGIONAL_ANALYSIS_DESIGN.md',
 'sources/literature_review/thesis_concepts_20260911/extracted/oecd_global_debt_report_2025.txt')
read_stage <- function(stage,file) {
 p<-file.path(j$stages[[stage]]$dir,file);inputs<<-c(inputs,p);fread(p)
}
x <- read_stage('regional','country_year_views.csv')
x[reviewed_eight_exclusion==TRUE,rate:=NA_real_]
main <- x[view=='no_peer' & is.finite(rate)]
a <- main[,.(countries=.N,mean_rate=mean(rate),median_rate=median(rate),
 q25=unname(quantile(rate,.25)),q75=unname(quantile(rate,.75))),by=analysis_year][order(analysis_year)]
r <- main[,.(countries=.N,mean_rate=mean(rate),median_rate=median(rate)),by=.(region,analysis_year)]
ref <- read_stage('regional_followup','annual_regional_rates.csv')[restriction=='recorded_secondary_holds' & view=='no_peer']
chk<-merge(r,ref,by=c('region','analysis_year'))
stopifnot(nrow(chk)==78,all(chk$countries.x==chk$countries.y),all(abs(chk$mean_rate-chk$mean_rate_pct)<1e-10))
all_prev<-x[,.(iso3,view,analysis_year=analysis_year+1L,prior_rate=rate,prior_source=source)]
matched<-merge(x[is.finite(rate)],all_prev,by=c('iso3','view','analysis_year'))[is.finite(prior_rate)]
matched[,change:=rate-prior_rate]
same<-matched[,.(countries=.N,mean_change=mean(change),median_change=median(change),
 rising=sum(change>0),source_switches=sum(source!=prior_source)),by=.(view,analysis_year)]
stable<-matched[source==prior_source,.(countries=.N,mean_change=mean(change)),by=.(view,analysis_year)]
changes<-read_stage('regional_followup','annual_common_country_changes.csv')[restriction=='recorded_secondary_holds']
c2<-matched[view=='no_peer',.(countries=.N,mean_change=mean(change)),by=.(region,analysis_year)]
c2<-merge(c2,changes[view=='no_peer'],by=c('region','analysis_year'))
stopifnot(all(c2$countries.x==c2$countries.y),all(abs(c2$mean_change-c2$mean_change_pp)<1e-10))
distribution<-matched[analysis_year==2022,.(countries=.N,iqr_2021=IQR(prior_rate),iqr_2022=IQR(rate)),by=view]
g<-read_stage('regional_followup','global_region_exclusions.csv')[restriction=='recorded_secondary_holds' & view=='no_peer']
ac<-merge(a,g[excluded_region=='none'],by='analysis_year')
stopifnot(all(ac$countries.x==ac$countries.y),all(abs(ac$mean_rate-ac$mean_rate_pct)<1e-10))
p<-read_stage('regional_followup','matched_period_contrasts.csv')[restriction=='recorded_secondary_holds' &
 view %in% c('no_peer','moodys') & coverage_requirement=='every_year_in_both_windows' &
 earlier_period=='2020_2021' & later_period=='2022_2024']
stopifnot(nrow(p)==12,all(p$mean_difference_pp>0))
map<-read_stage('regional','country_region_map.csv')
stopifnot(all(map[iso3 %in% c('AFG','PAK'),region]=='South Asia'))
# Content-specific checks after selecting the proposed claims.
stopifnot(same[view=='no_peer' & analysis_year==2022,countries]==81L,
 same[view=='no_peer' & analysis_year==2022,rising]==77L,
 stable[view=='no_peer' & analysis_year==2022,countries]==49L,
 all(same[view %in% c('primary','ids','secondary','moodys') & analysis_year==2022,mean_change]>0),
 all(distribution[iqr_2022<=iqr_2021,.N]==0))
eap<-r[region=='East Asia & Pacific',.(analysis_year,eap_rate=mean_rate)]
regional_order<-merge(r[region %in% c('Sub-Saharan Africa','Latin America & Caribbean') & analysis_year>=2014],eap,by='analysis_year')
stopifnot(nrow(regional_order)==22L,all(regional_order$mean_rate>regional_order$eap_rate))
lac<-matched[view=='no_peer' & region=='Latin America & Caribbean' & analysis_year==2024]
stopifnot(lac[which.max(change),iso3]=='BLZ',lac[iso3=='BLZ',prior_source]=='ids',
 lac[iso3=='BLZ',source]=='moodys',abs(lac[iso3=='BLZ',prior_rate]-2)<1e-10,
 median(lac$change)<0)
tables<-list(annual_rates=a,regional_rates=r,annual_matched_changes=same,
 same_source_changes=stable,regional_changes=changes[view %in% c('no_peer','moodys')],
 distribution_2022=distribution,region_omissions=g,complete_window_changes=p,
 lac_change_details=matched[view=='no_peer' & region=='Latin America & Caribbean' & analysis_year==2024,
 .(iso3,prior_rate,rate,change,prior_source,source)])
for(n in names(tables))fwrite(tables[[n]],file.path(out,paste0(n,'.csv')))
fwrite(data.table(path=inputs,sha256=vapply(inputs,digest,character(1),file=TRUE,algo='sha256')),file.path(out,'input_manifest.csv'))
fwrite(data.table(check=c('regional_means_and_counts_reproduced','regional_matched_changes_reproduced',
 'global_means_and_counts_reproduced','all_six_complete_window_increases','fixed_region_definition',
 'main_2022_sample_and_direction','separate_source_2022_increases','dispersion_widens',
 'regional_level_comparison_2014_2024','belize_largest_lac_contribution'),passed=TRUE),file.path(out,'verification.csv'))
writeLines(capture.output(sessionInfo()),file.path(out,'environment.txt'))
paths<-list.files(out,full.names=TRUE);paths<-paths[basename(paths)!='output_manifest.csv']
fwrite(data.table(path=paths,sha256=vapply(paths,digest,character(1),file=TRUE,algo='sha256')),file.path(out,'output_manifest.csv'))
print(a);print(same[view=='no_peer' & analysis_year %in% c(2022,2024)])
print(stable[view=='no_peer' & analysis_year==2022]);print(p[,.(view,region,countries,mean_difference_pp)])
