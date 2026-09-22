# Input-based feasibility ceilings; never generates grades or peer estimates.
source('scripts/p15/activate_p15_environment.R')
suppressPackageStartupMessages({library(data.table);library(jsonlite);library(digest)})
e <- 'experiments/p15_wb_scorecard_20260912'
out <- file.path(e,'coverage_outlook_20260912')
inputs <- c(file.path(e,'scorecard_inputs.csv'),file.path(e,'peer/all_peer_predictions.csv'),
 'experiments/p15_peer_rating_expansion_20260912/web_rating_peer_reconciliation.csv',
 'data-derived/p15_master/current_run.json')
before <- vapply(inputs,digest,character(1),file=TRUE,algo='sha256')
x <- fread(inputs[1]);d <- fread(inputs[2]);a <- fread(inputs[3])
peer <- x[historical_lmic_reporting_scope==TRUE & selected_tier=='peer']
stopifnot(nrow(x)==2743L,x[historical_lmic_reporting_scope==TRUE,.N]==1756L,nrow(peer)==778L)
key <- function(z)paste(z$iso3,z$analysis_year)
base <- d[method=='PIS_strict' & mode=='deployment' & historical_lmic_reporting_scope==TRUE & selected_tier=='peer' & selected_peer_new==TRUE]
base_ids <- key(base);repair_ids <- key(a[screened_grade_candidate==TRUE])
stopifnot(length(base_ids)==297L,length(repair_ids)==46L)
core <- c('growth_avg7','growth_sd10','nominal_gdp_usd_bn','gdp_pc_ppp','inflation_avg7','inflation_sd10',
 'debt_gdp','debt_revenue','debt_trend_pp','fc_share_proxy_in_range')
scenarios <- CJ(gci_policy=c('gci_same_year','gci_carry_max2','gci_last_available'),
 interest_policy=c('net_interest_approximation','later_revised_gross_interest','ignore_interest_optimistic_ceiling'))
cases <- rbindlist(lapply(seq_len(nrow(scenarios)),function(i){
 g <- scenarios$gci_policy[i];ip <- scenarios$interest_policy[i]
 cols <- c(core,g,switch(ip,net_interest_approximation=c('implied_net_interest_gdp','implied_net_interest_revenue'),
 later_revised_gross_interest=c('gfs_gross_interest_gdp_latest','gfs_gross_interest_revenue_latest'),
 ignore_interest_optimistic_ceiling=character()))
 z <- copy(peer);z[,numeric_inputs_present:=Reduce(`&`,lapply(.SD,is.finite)),.SDcols=cols]
 z[,`:=`(gci_policy=g,interest_policy=ip,baseline_peer_available=key(z)%in%base_ids,
 actual_rating_reconciliation_candidate=key(z)%in%repair_ids)]
 z[,`:=`(potentially_retained=baseline_peer_available|numeric_inputs_present,
 potentially_retained_with_repairs=baseline_peer_available|numeric_inputs_present|actual_rating_reconciliation_candidate)]
 z[,.(iso3,country,analysis_year,historical_income_level,gci_policy,interest_policy,numeric_inputs_present,
 baseline_peer_available,actual_rating_reconciliation_candidate,potentially_retained,potentially_retained_with_repairs)]
}))
summary <- cases[,.(original_peer_country_years=.N,baseline_peers=sum(baseline_peer_available),
 target_input_sets=sum(numeric_inputs_present),additional_input_supported_targets=sum(numeric_inputs_present & !baseline_peer_available),
 input_based_optimistic_ceiling=sum(potentially_retained),ceiling_with_all_screened_repair_candidates=sum(potentially_retained_with_repairs)),
 by=.(gci_policy,interest_policy)]
summary[,`:=`(is_forecast=FALSE,new_shadow_ratings_generated=0L,new_peer_estimates_generated=0L,
 interpretation='Conditional input ceiling, not predicted or validated peer coverage; requires actual matching and input-policy verification')]
fwrite(summary,file.path(out,'coverage_ceiling_summary.csv'))
fwrite(cases,file.path(out,'coverage_ceiling_country_years.csv'))
cases[,period:=fifelse(analysis_year<=2017,'2012-2017',fifelse(analysis_year<=2020,'2018-2020','2021-2024'))]
fwrite(cases[,.(original_peer_country_years=.N,baseline_peers=sum(baseline_peer_available),
 input_ceiling=sum(potentially_retained),ceiling_with_repair_candidates=sum(potentially_retained_with_repairs)),
 by=.(gci_policy,interest_policy,period)],file.path(out,'coverage_ceiling_period.csv'))
fwrite(cases[,.(original_peer_country_years=.N,baseline_peers=sum(baseline_peer_available),input_ceiling=sum(potentially_retained)),
 by=.(gci_policy,interest_policy,historical_income_level)],file.path(out,'coverage_ceiling_income.csv'))
nonpeer <- x[historical_lmic_reporting_scope==TRUE & !selected_tier%in%c('peer','no_eligible_rate'),.N]
stopifnot(nonpeer==970L,all(summary$input_based_optimistic_ceiling>=297L),all(summary$input_based_optimistic_ceiling<=778L))
fwrite(data.table(assumed_peer_count=c(297L,300L,400L,500L,543L,643L,778L),
 all_lmic_country_years=1756L,unchanged_nonpeer_count=970L,total_covered=970L+c(297L,300L,400L,500L,543L,643L,778L)),
 file.path(out,'denominator_bridge.csv'))
after <- vapply(inputs,digest,character(1),file=TRUE,algo='sha256');stopifnot(identical(before,after))
fwrite(data.table(path=inputs,sha256_before=before,sha256_after=after,unchanged=before==after),file.path(out,'input_preservation.csv'))
writeLines(capture.output(sessionInfo()),file.path(out,'environment.txt'))
files <- list.files(out,full.names=TRUE);files <- files[basename(files)!='output_manifest.csv']
fwrite(data.table(path=files,sha256=vapply(files,digest,character(1),file=TRUE,algo='sha256')),file.path(out,'output_manifest.csv'))
print(summary[gci_policy!='gci_same_year']);cat('Full universe 2743; LMIC universe 1756; unchanged nonpeer 970.\n')
