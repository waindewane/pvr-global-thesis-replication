# Explicit conditional cash flows for CRS fixed-rate EPP and annuity records.
# Percentages enter only at this interface. The reporting file does not supply
# disbursement dates: 100 is advanced on the recorded commitment date.
crs_cashflows <- function(commitment, first, final, rate_pct, frequency, repayment_type, final_tolerance_days=7L) {
 commitment <- as.Date(commitment); first <- as.Date(first); final <- as.Date(final)
 stopifnot(!anyNA(c(commitment,first,final)), first>commitment,final>=first,
  frequency %in% c(1L,2L,4L,12L),repayment_type %in% c(1L,2L),is.finite(rate_pct),rate_pct>=0,
  final_tolerance_days>=0,final_tolerance_days<=7)
 step <- 12L/frequency
 # Calendar-month dates retain the first payment's day where it exists and
 # clip to a shorter month's last day. This is not a true end-of-month rule.
 add_month <- function(date,n) {
  y <- as.integer(format(date,"%Y")); m <- as.integer(format(date,"%m")); day <- as.integer(format(date,"%d"))
  idx <- y*12L+m-1L+n; yy<-idx%/%12L; mm<-idx%%12L+1L
  start<-as.Date(sprintf("%04d-%02d-01",yy,mm)); nextidx<-idx+1L
  nextstart<-as.Date(sprintf("%04d-%02d-01",nextidx%/%12L,nextidx%%12L+1L))
  start+pmin(day,as.integer(nextstart-start))-1L
 }
 dates<-if(first<final)first else as.Date(character()); k<-1L
 while(add_month(first,k*step)<final-final_tolerance_days){dates<-c(dates,add_month(first,k*step));k<-k+1L;if(k>1200L)stop("Implausible horizon")}
 if(length(dates)==0L || tail(dates,1L)!=final) dates<-c(dates,final)
 grace<-as.Date(character());k<-1L
 while(add_month(first,-k*step)>commitment){grace<-c(grace,add_month(first,-k*step));k<-k+1L;if(k>1200L)stop("Implausible grace")}
 grace<-sort(grace); all_dates<-sort(c(grace,dates)); principal_dates<-all_dates %in% dates
 intervals<-as.numeric(all_dates-c(commitment,head(all_dates,-1L)))/365.25
 c<-rate_pct/100
 repayment_intervals<-intervals[principal_dates]
 if(repayment_type==2L) {
  # Payment solves the balance recurrence with period-specific simple accrual.
  discount<-1/cumprod(1+c*repayment_intervals)
  constant_payment<-100/sum(discount)
 }
 balance<-100;rows<-vector("list",length(all_dates));n<-length(dates)
 for(j in seq_along(all_dates)) {
  interest<-balance*c*intervals[j]
  principal<-if(!principal_dates[j])0 else if(repayment_type==1L)100/n else constant_payment-interest
  if(j==length(all_dates))principal<-balance
  stopifnot(principal>=-1e-8)
  rows[[j]]<-data.table::data.table(payment_date=as.character(all_dates[j]),
   time_years=as.numeric(all_dates[j]-commitment)/365.25,opening_balance=balance,
   principal,interest,payment=principal+interest,interval_years=intervals[j])
  balance<-balance-principal
 }
 z<-data.table::rbindlist(rows)
 stopifnot(abs(sum(z$principal)-100)<1e-7,all(z$payment>=0),all(diff(z$time_years)>0))
 z
}
crs_ge <- function(flows,discount_pct) {
 if(!is.finite(discount_pct)||discount_pct<=-100)return(NA_real_)
 100-sum(flows$payment/(1+discount_pct/100)^flows$time_years)
}
