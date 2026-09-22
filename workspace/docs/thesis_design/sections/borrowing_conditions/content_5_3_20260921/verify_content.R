library(data.table)
library(jsonlite)
library(digest)
out <- 'docs/thesis_design/sections/borrowing_conditions/content_5_3_20260921'
pointer <- 'data-derived/p15_master/current_run.json'
j <- fromJSON(pointer)
inputs <- c(pointer, file.path(out,'verify_content.R'))
read_input <- function(path) {
  inputs <<- c(inputs,path)
  fread(path)
}
x <- read_input(file.path(j$stages$regional$dir,'country_year_views.csv'))
x[reviewed_eight_exclusion %in% TRUE,rate:=NA_real_]
core <- read_input(file.path(j$candidate,'core_evidence.csv'))
x <- merge(x,core[,.(iso3,analysis_year,historical_income_level)],by=c('iso3','analysis_year'))
stopifnot(!anyDuplicated(x[,.(iso3,analysis_year,view)]))
x <- x[is.finite(rate)]
income <- x[,.(countries=.N,mean_rate=mean(rate),median_rate=median(rate),
  q25=unname(quantile(rate,.25)),q75=unname(quantile(rate,.75))),
  by=.(view,analysis_year,historical_income_level)]
fwrite(income,file.path(out,'annual_income_summary.csv'))
annual <- dcast(income[view=='no_peer'],analysis_year~historical_income_level,value.var='mean_rate')
stopifnot(nrow(annual)==13L,all(annual[['Lower middle income']]>annual[['Upper middle income']]))
fwrite(annual,file.path(out,'annual_income_mean_comparison.csv'))
fwrite(x[view=='no_peer' & analysis_year==2024,.N,by=.(historical_income_level,source)],
  file.path(out,'income_source_counts_2024.csv'))
fwrite(income[analysis_year==2024 & view %in% c('no_peer','primary','secondary')],
  file.path(out,'income_comparison_2024.csv'))
z <- read_input(file.path(j$stages$gbohoui_context$dir,'joined_context.csv'))
registered <- read_input(file.path(j$stages$gbohoui_context$dir,'model_summary.csv'))
d <- z[indicator=='regulatory_quality' & view=='primary' & matched_for_full_scope_model==TRUE]
fit <- lm(rate~score+factor(analysis_year)+factor(historical_income_level),data=d)
slope <- unname(coef(fit)['score'])
target <- registered[indicator=='regulatory_quality' & view=='primary' &
  period=='full_scope' & specification=='year_income']
stopifnot(nrow(d)==215L,uniqueN(d$iso3)==47L,abs(slope-target$slope_rate_pp)<1e-10)
fwrite(data.table(records=nrow(d),countries=uniqueN(d$iso3),slope=slope),
  file.path(out,'regulatory_quality_reproduction.csv'))
fwrite(d[,.(iso3,analysis_year,rate,score,historical_income_level)],
  file.path(out,'regulatory_quality_membership.csv'))
fwrite(data.table(check=c('unique_country_year_views','all_13_middle_income_mean_orderings',
  'regulatory_coefficient_reproduced','regulatory_sample_reproduced'),passed=TRUE),
  file.path(out,'verification.csv'))
fwrite(data.table(path=inputs,sha256=vapply(inputs,digest,character(1),file=TRUE,algo='sha256')),
  file.path(out,'input_manifest.csv'))
capture.output(sessionInfo(),file=file.path(out,'session_info.txt'))
outputs <- list.files(out,full.names=TRUE,pattern='\\.(csv|txt)$')
outputs <- outputs[basename(outputs)!='output_manifest.csv']
fwrite(data.table(path=outputs,sha256=vapply(outputs,digest,character(1),file=TRUE,algo='sha256')),
  file.path(out,'output_manifest.csv'))
print(income[analysis_year==2024 & view %in% c('no_peer','primary','secondary')])
print(annual[analysis_year==2019])
print(data.table(records=nrow(d),countries=uniqueN(d$iso3),slope=slope))
