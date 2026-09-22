library(data.table)
library(jsonlite)
library(digest)
out <- 'docs/thesis_design/sections/borrowing_conditions/planning_20260921'
j <- fromJSON('data-derived/p15_master/current_run.json')
inputs <- c('data-derived/p15_master/current_run.json',
 file.path(out,'prepare_evidence.R'),
 'scripts/p15/build_p15_regional_assessment.R',
 'scripts/p15/annotation_followup_20260911/build_regional_followup.R',
 'scripts/p15/annotation_followup_20260911/build_gbohoui_context.R')
read_input <- function(path) {
  inputs <<- c(inputs,path)
  fread(path)
}
read_stage <- function(stage,file) read_input(file.path(j$stages[[stage]]$dir,file))
x <- read_stage('regional','country_year_views.csv')
x[reviewed_eight_exclusion==TRUE,rate:=NA_real_]
core <- read_input(file.path(j$candidate,'core_evidence.csv'))
x <- merge(x,core[,.(iso3,analysis_year,historical_income_level)],by=c('iso3','analysis_year'))
stopifnot(nrow(x)==1756*7,!anyDuplicated(x[,.(iso3,analysis_year,view)]))
stats <- function(z) {
 r<-z$rate[is.finite(z$rate)]
 data.table(n=length(r),mean=mean(r),median=median(r),q25=unname(quantile(r,.25)),
 q75=unname(quantile(r,.75)),iqr=IQR(r),p10=unname(quantile(r,.1)),p90=unname(quantile(r,.9)))
}
annual <- x[is.finite(rate),stats(.SD),by=.(view,analysis_year)]
registered <- read_stage('regional_followup','global_region_exclusions.csv')[restriction=='recorded_secondary_holds' & excluded_region=='none']
check <- merge(annual,registered,by=c('view','analysis_year'))
stopifnot(all(check$n==check$countries),all(abs(check$mean-check$mean_rate_pct)<1e-10))
fwrite(annual,file.path(out,'annual_levels_and_dispersion.csv'))
income <- x[is.finite(rate),stats(.SD),by=.(view,analysis_year,historical_income_level)]
fwrite(income,file.path(out,'annual_income_rates.csv'))
fwrite(x[is.finite(rate),.(n=.N,countries=uniqueN(iso3),mean=mean(rate),median=median(rate)),
 by=.(view,historical_income_level)],file.path(out,'pooled_income_context.csv'))
# Paired changes use actual same-country observations, never a chained pseudo-index.
prev<-x[,.(iso3,analysis_year=analysis_year+1L,view,prior_rate=rate,prior_source=source,
 prior_income=historical_income_level)]
pairs<-merge(x[is.finite(rate)],prev,by=c('iso3','analysis_year','view'))[is.finite(prior_rate)]
pairs[,change:=rate-prior_rate]
paired<-pairs[,.(countries=.N,mean_change=mean(change),median_change=median(change),
 source_switches=sum(source!=prior_source)),by=.(view,analysis_year)]
fwrite(paired,file.path(out,'annual_same_country_changes.csv'))
distribution<-pairs[analysis_year==2022,.(countries=.N,
 mean_2021=mean(prior_rate),mean_2022=mean(rate),median_2021=median(prior_rate),median_2022=median(rate),
 iqr_2021=IQR(prior_rate),iqr_2022=IQR(rate),
 p90_p10_2021=unname(quantile(prior_rate,.9)-quantile(prior_rate,.1)),
 p90_p10_2022=unname(quantile(rate,.9)-quantile(rate,.1))),by=view]
fwrite(distribution,file.path(out,'same_country_distribution_2021_2022.csv'))
regional_changes<-read_stage('regional_followup','annual_common_country_changes.csv')[
 restriction=='recorded_secondary_holds' & analysis_year %in% c(2022L,2024L)]
fwrite(regional_changes,file.path(out,'regional_changes_selected_years.csv'))
lac<-x[view=='no_peer' & region=='Latin America & Caribbean' &
 analysis_year %in% c(2023L,2024L) & is.finite(rate)]
lac23<-lac[analysis_year==2023L,.(iso3,rate_2023=rate)]
lac24<-lac[analysis_year==2024L,.(iso3,rate_2024=rate)]
lac_members<-merge(lac23,lac24,by='iso3',all=TRUE)
lac_members[,membership:=fifelse(is.na(rate_2023),'entering',
 fifelse(is.na(rate_2024),'leaving','both'))]
stopifnot(nrow(lac23)==19L,nrow(lac24)==15L,
 lac_members[membership=='both',.N]==14L)
fwrite(lac_members,file.path(out,'lac_membership_2023_2024.csv'))
fwrite(data.table(raw_mean_change=mean(lac24$rate_2024)-mean(lac23$rate_2023),
 same_country_mean_change=lac_members[membership=='both',mean(rate_2024-rate_2023)],
 matched_countries=14L,leaving=lac_members[membership=='leaving',.N],
 entering=lac_members[membership=='entering',.N]),file.path(out,'lac_composition_2023_2024.csv'))
region_map<-read_stage('regional','country_region_map.csv')
fwrite(region_map[region!=region_current],file.path(out,'regional_partition_differences.csv'))
models<-read_stage('gbohoui_context','model_summary.csv')
fwrite(models[indicator=='regulatory_quality' & period=='full_scope',
 .(view,specification,records,countries,slope_rate_pp,deletion_min_slope,deletion_max_slope)],
 file.path(out,'regulatory_quality_context.csv'))
fwrite(data.table(path=inputs,sha256=vapply(inputs,digest,character(1),file=TRUE,algo='sha256')),
 file.path(out,'input_manifest.csv'))
fwrite(data.table(check=c('unique_historical_scope','annual_counts_reproduced','annual_means_reproduced','lac_membership_reproduced'),passed=TRUE),file.path(out,'verification.csv'))
print(annual[view=='no_peer' & analysis_year>=2018])
print(distribution)
print(income[view %in% c('no_peer','selected') & analysis_year %in% c(2021,2022,2024)])
