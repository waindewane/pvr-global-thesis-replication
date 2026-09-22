# Diagnostic analysis only. Does not select, alter or promote benchmark rates.
p15_assessment_keys <- function(x, keys, label) {
  if (!all(keys %in% names(x))) stop(label, ": missing key columns")
  if (anyNA(x[, keys, drop=FALSE]) || anyDuplicated(x[, keys, drop=FALSE]))
    stop(label, ": missing or duplicate keys")
  invisible(TRUE)
}

p15_assessment_metrics <- function(d) {
  z <- d$gap_pp
  stopifnot(length(z)>0L, all(is.finite(z)))
  corr <- function(method) if(nrow(d)>2L && sd(d$anchor_rate)>0 && sd(d$comparison_rate)>0)
    cor(d$anchor_rate,d$comparison_rate,method=method) else NA_real_
  cm <- tapply(z,d$iso3,mean)
  data.frame(n=nrow(d),countries=length(unique(d$iso3)),years=length(unique(d$analysis_year)),
    mean_gap_pp=mean(z),mae_pp=mean(abs(z)),median_abs_gap_pp=median(abs(z)),
    rmse_pp=sqrt(mean(z^2)),p90_abs_gap_pp=unname(quantile(abs(z),.9)),
    max_abs_gap_pp=max(abs(z)),within_05=mean(abs(z)<=.5),within_1=mean(abs(z)<=1),
    within_2=mean(abs(z)<=2),positive_share=mean(z>0),pearson=corr("pearson"),
    spearman=corr("spearman"),equal_country_mean_gap_pp=mean(cm),
    equal_country_mae_pp=mean(tapply(abs(z),d$iso3,mean)))
}

p15_assessment_inference <- function(d) {
  fit <- lm(gap_pp~1,data=d)
  nc <- length(unique(d$iso3)); ny <- length(unique(d$analysis_year))
  one <- function(kind) {
    df <- if(kind=="country") nc-1L else min(nc,ny)-1L
    v <- if(df>0) tryCatch(as.numeric(sandwich::vcovCL(fit,
      cluster=if(kind=="country")d$iso3 else list(d$iso3,d$analysis_year),
      type="HC1",multi0=kind!="country",fix=FALSE)[1,1]),error=function(e)NA_real_) else NA_real_
    good <- is.finite(v) && v>0
    se <- if(good)sqrt(v) else NA_real_
    mu <- mean(d$gap_pp)
    data.frame(inference=kind,mean_gap_pp=mu,se_pp=se,df=df,
      ci_low_pp=mu-qt(.975,df)*se,ci_high_pp=mu+qt(.975,df)*se,
      p_value=if(good)2*pt(-abs(mu/se),df) else NA_real_,
      inference_state=if(good)"approximate_exploratory" else "unavailable_nonpositive_covariance_or_clusters")
  }
  rbind(one("country"),one("country_year"))
}

# A bridge is an arithmetic identity, not causal attribution or a new estimate.
p15_assessment_bridges <- function(previous_rate,current_rate,previous_source_now,new_source_previous) {
  data.frame(total_change_pp=current_rate-previous_rate,
    old_source_within_change_pp=previous_source_now-previous_rate,
    source_difference_current_year_pp=current_rate-previous_source_now,
    new_source_within_change_pp=current_rate-new_source_previous,
    source_difference_previous_year_pp=new_source_previous-previous_rate,
    old_source_bridge_available=is.finite(previous_source_now),
    new_source_bridge_available=is.finite(new_source_previous))
}
