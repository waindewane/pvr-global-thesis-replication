# Use the mapping's historically applicable headline rate, not its category-rate
# column (which also contains differentiated categories for years before 2018).
loan_policy_discount_rates <- function(year, headline_rate_pct, eligible) {
  stopifnot(length(year)==length(headline_rate_pct),length(year)==length(eligible))
  rate<-as.numeric(headline_rate_pct)
  rate[is.na(eligible)|!eligible|!is.finite(year)]<-NA_real_
  if(any(is.finite(rate)&year<2018&abs(rate-10)>1e-10,na.rm=TRUE))
    stop("Pre-2018 headline DAC comparison must use the historical 10% rate")
  rate
}

# Conditional source-style loan valuation. Input rates are percentages.
loan_ge <- function(coupon_pct, maturity, first_principal, discount_pct, frequency=2) {
  if(any(!is.finite(c(coupon_pct,maturity,first_principal,discount_pct,frequency))) ||
     coupon_pct<0 || maturity<=0 || first_principal<0 || first_principal>maturity ||
     discount_pct<=-100 || frequency<=0) return(NA_real_)
  if(abs(discount_pct)<1e-9) return(-coupon_pct*(first_principal+maturity)/2)
  h<-1/frequency; logd<-log1p(discount_pct/100); period_rate<-expm1(h*logd)
  n<-1+(maturity-first_principal)/h
  average_df<-exp(-logd*first_principal)*(-expm1(-logd*n*h))/
    (n*(-expm1(-logd*h)))
  100*(1-(coupon_pct/100)*h/period_rate)*(1-average_df)
}

# Explicit payment sensitivity; principal dates start at the supplied first endpoint.
loan_explicit_schedule <- function(coupon_pct,maturity,first_principal,frequency=2) {
  stopifnot(is.finite(coupon_pct),coupon_pct>=0,maturity>0,first_principal>=0,
            first_principal<=maturity,frequency>0)
  h<-1/frequency; span<-maturity-first_principal
  n_intervals<-round(span/h)
  if(span<1e-10)principal_dates<-maturity
  else if(abs(span-n_intervals*h)<=7/365.25 && n_intervals>=1)
    principal_dates<-seq(first_principal,maturity,length.out=n_intervals+1)
  else {
    principal_dates<-seq(first_principal,maturity,by=h)
    if(tail(principal_dates,1)<maturity-1e-10)principal_dates<-c(principal_dates,maturity)
  }
  grace_dates<-if(first_principal>h)seq(h,first_principal,by=h) else numeric()
  grace_dates<-grace_dates[grace_dates<first_principal-1e-10]
  times<-sort(unique(c(grace_dates,principal_dates)))
  principal<-numeric(length(times));principal[match(principal_dates,times)]<-100/length(principal_dates)
  opening<-100-c(0,head(cumsum(principal),-1));elapsed<-diff(c(0,times))
  interest<-opening*coupon_pct/100*elapsed
  data.frame(payment_time_years=times,principal=principal,interest=interest,
             debt_service=principal+interest,opening_principal=opening)
}

loan_explicit_ge <- function(flows,discount_pct) {
  if(!is.finite(discount_pct)||discount_pct<=-100)return(NA_real_)
  100-sum(flows$debt_service/(1+discount_pct/100)^flows$payment_time_years)
}

loan_summary <- function(z) {
  if(!nrow(z))return(data.table::data.table())
  x<-z[is.finite(delta_ge_pp)]
  if(!nrow(x))return(data.table::data.table(records=0L))
  event<-x[!is.na(loan_event_id)&nzchar(loan_event_id),.(delta=mean(delta_ge_pp),
    amount_aggregated_delta=if(all(is.finite(amount_usd)&amount_usd>0))stats::weighted.mean(delta_ge_pp,amount_usd) else NA_real_),by=loan_event_id]
  goodw<-is.finite(x$amount_usd)&x$amount_usd>0
  data.table::data.table(records=nrow(x),events=if(nrow(event))nrow(event) else NA_integer_,
    records_with_event_ids=sum(!is.na(x$loan_event_id)&nzchar(x$loan_event_id)),
    countries=data.table::uniqueN(x$iso3),country_years=data.table::uniqueN(paste(x$iso3,x$commitment_year)),
    mean_market_ge_pct=mean(x$market_ge_pct),mean_reference_ge_pct=mean(x$reference_ge_pct),
    mean_delta_ge_pp=mean(x$delta_ge_pp),median_delta_ge_pp=stats::median(x$delta_ge_pp),
    q10_delta_ge_pp=as.numeric(stats::quantile(x$delta_ge_pp,.1)),q90_delta_ge_pp=as.numeric(stats::quantile(x$delta_ge_pp,.9)),
    positive_share=mean(x$delta_ge_pp>0),equal_event_mean_of_record_deltas_pp=if(nrow(event))mean(event$delta) else NA_real_,
    equal_event_mean_of_amount_aggregated_deltas_pp=if(any(is.finite(event$amount_aggregated_delta)))mean(event$amount_aggregated_delta,na.rm=TRUE) else NA_real_,
    events_with_complete_amount_weights=sum(is.finite(event$amount_aggregated_delta)),
    amount_weighted_mean_delta_ge_pp=if(any(goodw))stats::weighted.mean(x$delta_ge_pp[goodw],x$amount_usd[goodw]) else NA_real_,
    amount_weighted_records=sum(goodw))
}
