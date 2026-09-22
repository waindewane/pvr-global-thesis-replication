# Bounded diagnostic functions; no rate estimation, selection or new p-values.
stat_error_metrics <- function(z) {
  stopifnot(all(is.finite(z$focal_error)),all(is.finite(z$comparator_error)))
  data.table::data.table(records=nrow(z),countries=data.table::uniqueN(z$iso3),years=if("analysis_year"%in%names(z))data.table::uniqueN(z$analysis_year) else 1L,
    focal_bias_pp=mean(z$focal_error),comparator_bias_pp=mean(z$comparator_error),
    focal_mae_pp=mean(abs(z$focal_error)),comparator_mae_pp=mean(abs(z$comparator_error)),
    focal_rmse_pp=sqrt(mean(z$focal_error^2)),comparator_rmse_pp=sqrt(mean(z$comparator_error^2)),
    focal_error_sd_pp=stats::sd(z$focal_error),comparator_error_sd_pp=stats::sd(z$comparator_error),
    improvement_pp=mean(abs(z$comparator_error)-abs(z$focal_error)),
    equal_country_improvement_pp=mean(tapply(abs(z$comparator_error)-abs(z$focal_error),z$iso3,mean)),
    equal_year_improvement_pp=if("analysis_year"%in%names(z))mean(tapply(abs(z$comparator_error)-abs(z$focal_error),z$analysis_year,mean)) else mean(abs(z$comparator_error)-abs(z$focal_error)))
}

stat_period_means <- function(d,selected="selected_gap_pp",fixed="fixed_gap_pp") {
  e<-d$commitment_year<=2021L;l<-!e
  av<-function(y,w)if(any(w))mean(y[w]) else NA_real_
  data.table::data.table(records=nrow(d),early_records=sum(e),late_records=sum(l),
    countries=data.table::uniqueN(d$iso3),early_countries=data.table::uniqueN(d$iso3[e]),late_countries=data.table::uniqueN(d$iso3[l]),
    common_countries=length(intersect(d$iso3[e],d$iso3[l])),country_years=data.table::uniqueN(paste(d$iso3,d$commitment_year)),
    early_years=data.table::uniqueN(d$commitment_year[e]),late_years=data.table::uniqueN(d$commitment_year[l]),
    selected_early_pp=av(d[[selected]],e),selected_late_pp=av(d[[selected]],l),
    fixed_early_pp=av(d[[fixed]],e),fixed_late_pp=av(d[[fixed]],l),
    selected_contrast_pp=av(d[[selected]],l)-av(d[[selected]],e),
    fixed_contrast_pp=av(d[[fixed]],l)-av(d[[fixed]],e),
    method_component_pp=(av(d[[fixed]]-d[[selected]],l)-av(d[[fixed]]-d[[selected]],e)))
}

stat_period_design <- function(d) {
  d<-data.table::copy(data.table::as.data.table(d))
  d[,late:=commitment_year>=2022L]
  d[,residualized_late:=as.numeric(late)-mean(late)]
  total_z2<-sum(d$residualized_late^2)
  effect<-mean(d$delta_ge_pp[d$late])-mean(d$delta_ge_pp[!d$late])
  details<-data.table::rbindlist(lapply(c("iso3","commitment_year"),function(cl) {
    ans<-d[,.(records=.N,early_records=sum(!late),late_records=sum(late),
      partial_leverage=sum(residualized_late^2)/total_z2,
      mean_gap_pp=mean(delta_ge_pp)),by=cl]
    data.table::setnames(ans,cl,"cluster")
    ans[,cluster:=as.character(cluster)]
    ans[,cluster_dimension:=cl]
    ans[,deleted_estimate_pp:=vapply(cluster,function(k) {
      q<-d[as.character(get(cl))!=k]
      if(!any(q$late)||all(q$late))NA_real_ else mean(q$delta_ge_pp[q$late])-mean(q$delta_ge_pp[!q$late])
    },numeric(1))]
    ans[,deletion_shift_pp:=deleted_estimate_pp-effect]
    ans[,centered_jackknife_ss:=(deleted_estimate_pp-mean(deleted_estimate_pp))^2]
    ans[,jackknife_variance_share:=centered_jackknife_ss/sum(centered_jackknife_ss)]
    ans
  }))
  concentration<-data.table::rbindlist(lapply(c("iso3","commitment_year"),function(cl) {
    ans<-d[,.(records=.N),by=c(cl,"late")]
    ans[,share:=records/sum(records),by=late]
    ans[,.(clusters=.N,max_record_share=max(share),inverse_hhi_record_share=1/sum(share^2)),by=late][,cluster_dimension:=cl]
  }))
  country<-d[,.(early_records=sum(!late),late_records=sum(late),
    early_mean_pp=if(any(!late))mean(delta_ge_pp[!late]) else NA_real_,
    late_mean_pp=if(any(late))mean(delta_ge_pp[late]) else NA_real_),by=iso3]
  country[,`:=`(common=early_records>0L&late_records>0L,
    within_weight=early_records*late_records/(early_records+late_records),within_difference_pp=late_mean_pp-early_mean_pp)]
  common<-country[common==TRUE]
  fe<-stats::weighted.mean(common$within_difference_pp,common$within_weight)
  fit<-stats::lm(delta_ge_pp~late+factor(iso3),data=d)
  fe_table<-data.table::data.table(records=nrow(d),countries=nrow(country),identifying_common_countries=nrow(common),
    identifying_common_records=sum(common$early_records+common$late_records),
    record_mean_period_contrast_pp=effect,within_country_fe_contrast_pp=fe,
    independent_lm_fe_contrast_pp=unname(stats::coef(fit)["lateTRUE"]),
    equal_common_country_contrast_pp=mean(common$within_difference_pp))
  list(cluster=details,concentration=concentration,country=country,fe=fe_table)
}
