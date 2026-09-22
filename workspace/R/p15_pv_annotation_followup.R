# Transparent hypothetical rate path, not a forecast or loan-contract reconstruction.
p15_interest_step <- function(flows, starting_rate_pct, change_pp, after_year=1) {
  stopifnot(is.finite(starting_rate_pct),starting_rate_pct>=0,is.finite(change_pp),
    is.finite(after_year),after_year>=0)
  ends <- flows$payment_time_years
  starts <- ends-flows$period_length
  later <- pmax(0,ends-pmax(starts,after_year))
  delta <- max(0,starting_rate_pct+change_pp)-starting_rate_pct
  flows$interest <- flows$interest+flows$opening_balance*delta/100*later
  flows$debt_service <- flows$principal+flows$interest
  flows
}

p15_gap_summary <- function(x) {
  dplyr::summarise(x,n=dplyr::n(),countries=dplyr::n_distinct(iso3),
    mean_gap=mean(gap),median_gap=median(gap),mean_abs_gap=mean(abs(gap)),
    positive_share=mean(gap>0),.groups="drop")
}
