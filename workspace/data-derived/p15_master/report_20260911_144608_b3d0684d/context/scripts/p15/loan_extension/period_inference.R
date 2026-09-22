# Exploratory inference for changes in paired loan valuation gaps.
# All outcomes and effects are grant-element percentage points.

loan_period_effect <- function(y, late) {
  stopifnot(length(y)==length(late),all(is.finite(y)),!anyNA(late))
  if(!any(late)||all(late))return(NA_real_)
  mean(y[late])-mean(y[!late])
}

loan_period_counts <- function(d) {
  data.frame(records=nrow(d),early_records=sum(!d$late),late_records=sum(d$late),
    countries=length(unique(d$iso3)),early_countries=length(unique(d$iso3[!d$late])),
    late_countries=length(unique(d$iso3[d$late])),
    common_countries=length(intersect(unique(d$iso3[!d$late]),unique(d$iso3[d$late]))),
    years=length(unique(d$commitment_year)),early_years=length(unique(d$commitment_year[!d$late])),
    late_years=length(unique(d$commitment_year[d$late])),
    early_mean_pp=mean(d$delta_ge_pp[!d$late]),late_mean_pp=mean(d$delta_ge_pp[d$late]),
    effect_pp=loan_period_effect(d$delta_ge_pp,d$late))
}

loan_period_delete_cluster <- function(d,cluster) {
  groups<-sort(unique(as.character(d[[cluster]])))
  do.call(rbind,lapply(groups,function(g) {
    z<-d[as.character(d[[cluster]])!=g,]
    data.frame(deleted_cluster=g,remaining_records=nrow(z),early_records=sum(!z$late),
      late_records=sum(z$late),estimate_pp=loan_period_effect(z$delta_ge_pp,z$late))
  }))
}

loan_period_t_result <- function(estimate,se,df) {
  if(!is.finite(se)||se<=0||!is.finite(df)||df<=0)return(data.frame(
    estimate_pp=estimate,se_pp=se,df=df,ci_low_pp=NA_real_,ci_high_pp=NA_real_,p_two_sided=NA_real_))
  critical<-stats::qt(.975,df)
  data.frame(estimate_pp=estimate,se_pp=se,df=df,
    ci_low_pp=estimate-critical*se,ci_high_pp=estimate+critical*se,
    p_two_sided=2*stats::pt(-abs(estimate/se),df))
}

loan_period_country_jackknife <- function(d) {
  counts<-loan_period_counts(d)
  deleted<-loan_period_delete_cluster(d,"iso3")
  g<-nrow(deleted)
  valid<-all(is.finite(deleted$estimate_pp))&&min(counts$early_countries,counts$late_countries)>=3
  variance<-if(valid)(g-1)/g*sum((deleted$estimate_pp-mean(deleted$estimate_pp))^2) else NA_real_
  df<-min(counts$early_countries,counts$late_countries)-1L
  list(summary=cbind(counts,method="country_delete_one_jackknife_t",
    loan_period_t_result(counts$effect_pp,sqrt(variance),df)),deletions=deleted)
}

loan_period_country_bootstrap <- function(d,repetitions=4999L,seed=20260910L) {
  stopifnot(repetitions>=99L)
  groups<-sort(unique(d$iso3));g<-length(groups)
  aggregate<-vapply(groups,function(k) {
    z<-d[d$iso3==k,]
    c(n_early=sum(!z$late),n_late=sum(z$late),
      sum_early=sum(z$delta_ge_pp[!z$late]),sum_late=sum(z$delta_ge_pp[z$late]))
  },numeric(4))
  set.seed(seed)
  sampled<-matrix(sample.int(g,g*repetitions,replace=TRUE),nrow=g)
  totals<-lapply(seq_len(4),function(i)colSums(matrix(aggregate[i,sampled],nrow=g)))
  theta<-loan_period_effect(d$delta_ge_pp,d$late)
  estimate<-totals[[4]]/totals[[2]]-totals[[3]]/totals[[1]]
  valid<-is.finite(estimate)&totals[[1]]>0&totals[[2]]>0
  draws<-data.frame(replicate=seq_len(repetitions),early_records=totals[[1]],
    late_records=totals[[2]],estimate_pp=estimate,null_centered_estimate_pp=estimate-theta,valid=valid)
  interval<-unname(stats::quantile(estimate[valid],c(.025,.975),type=7))
  p<-(1+sum(abs(estimate[valid]-theta)>=abs(theta)-1e-12))/(1+sum(valid))
  list(summary=cbind(loan_period_counts(d),method="country_pairs_bootstrap_percentile_centered_test",
    estimate_pp=theta,se_pp=stats::sd(estimate[valid]),ci_low_pp=interval[1],ci_high_pp=interval[2],
    p_two_sided=p,repetitions=repetitions,valid_repetitions=sum(valid),invalid_repetitions=sum(!valid),seed=seed),
    draws=draws)
}

loan_period_cr1_variance <- function(y,late,cluster) {
  stopifnot(length(y)==length(late),length(y)==length(cluster))
  n<-length(y);g<-length(unique(cluster))
  if(g<2||n<3||!any(late)||all(late))return(NA_real_)
  residual<-y-ifelse(late,mean(y[late]),mean(y[!late]))
  contrast_weight<-ifelse(late,1/sum(late),-1/sum(!late))
  scores<-tapply(contrast_weight*residual,cluster,sum)
  g/(g-1)*(n-1)/(n-2)*sum(scores^2)
}

loan_period_shared_year <- function(d) {
  counts<-loan_period_counts(d);y<-d$delta_ge_pp;late<-d$late
  country<-loan_period_cr1_variance(y,late,d$iso3)
  year<-loan_period_cr1_variance(y,late,d$commitment_year)
  intersection<-loan_period_cr1_variance(y,late,paste(d$iso3,d$commitment_year,sep="_"))
  two_way<-country+year-intersection
  envelope<-max(c(country,year,if(two_way>=0)two_way else NA_real_),na.rm=TRUE)
  df<-min(counts$countries-1,counts$years-1)
  methods<-c("country_CR1","year_CR1","two_way_country_year_CR1","shared_year_variance_envelope")
  variances<-c(country,year,two_way,envelope)
  do.call(rbind,lapply(seq_along(methods),function(i)cbind(counts,
    method=methods[i],variance_pp2=variances[i],
    loan_period_t_result(counts$effect_pp,if(variances[i]>=0)sqrt(variances[i]) else NA_real_,df),
    country_year_clusters=length(unique(paste(d$iso3,d$commitment_year))),
    country_variance_pp2=country,year_variance_pp2=year,intersection_variance_pp2=intersection,
    two_way_variance_pp2=two_way)))
}

loan_period_composition <- function(d) {
  common<-intersect(unique(d$iso3[d$late]),unique(d$iso3[!d$late]))
  balanced<-d[d$iso3%in%common,]
  subsets<-list(main=d,common_countries_record_weighted=balanced,reported_USD=d[d$currency=="USD",])
  if(unique(d$dataset)=="aiddata")subsets$exclude_flagged_source_imputation<-
    d[is.na(d$source_imputed_terms)|!d$source_imputed_terms,]
  if(unique(d$dataset)=="add")subsets$exclude_uncertain_zero_rates<-
    d[is.na(d$zero_rate_uncertain)|!d$zero_rate_uncertain,]
  summaries<-do.call(rbind,lapply(names(subsets),function(k)cbind(view=k,
    loan_period_counts(subsets[[k]]),weighting="equal_loan_record")))
  within<-do.call(rbind,lapply(sort(common),function(k) {
    z<-d[d$iso3==k,]
    cbind(iso3=k,loan_period_counts(z))
  }))
  extra<-loan_period_counts(balanced)
  extra$early_mean_pp<-mean(within$early_mean_pp);extra$late_mean_pp<-mean(within$late_mean_pp)
  extra$effect_pp<-mean(within$effect_pp)
  summaries<-rbind(summaries,cbind(view="common_countries_equal_country_weight",extra,weighting="equal_common_country"))
  years<-sort(unique(d$commitment_year))
  annual<-do.call(rbind,lapply(years,function(k) {
    z<-d[d$commitment_year==k,]
    data.frame(commitment_year=k,late=unique(z$late),records=nrow(z),countries=length(unique(z$iso3)),
      mean_gap_pp=mean(z$delta_ge_pp))
  }))
  extra<-loan_period_counts(d)
  extra$early_mean_pp<-mean(annual$mean_gap_pp[!annual$late]);extra$late_mean_pp<-mean(annual$mean_gap_pp[annual$late])
  extra$effect_pp<-extra$late_mean_pp-extra$early_mean_pp
  summaries<-rbind(summaries,cbind(view="equal_calendar_year_weight",extra,weighting="equal_year"))
  list(summary=summaries,country_contrasts=within,annual=annual)
}
