# Owner-approved exact-equality extension; does not change the legacy functions.
p15_bullet_equal_schedule <- function(rate, maturity, grace) {
  args <- list(rate,maturity,grace)
  if (!all(vapply(args,function(x)is.numeric(x) && length(x)==1L && is.finite(x),logical(1))))
    stop("Finite scalar terms required")
  if (rate < 0 || maturity <= 0 || grace != maturity)
    stop("Bullet extension requires nonnegative interest and exact positive grace/maturity equality")
  f <- make_bullet_cash_flows(100, rate, maturity)
  f$payment_time_years <- pmin(f$year,maturity)
  f
}

p15_bullet_independent_pv <- function(rate,maturity,discount) {
  # Separate closed coupon sum plus final principal; no use of schedule builder.
  times <- pmin(seq_len(ceiling(maturity)),maturity)
  lengths <- c(times[1],diff(times))
  sum(rate*lengths*exp(-log1p(discount/100)*times)) +
    100*exp(-log1p(discount/100)*maturity)
}

p15_bullet_scope <- function(x) {
  with(x,historical_lmic_reporting_scope & is.finite(official_rate) & official_rate>=0 &
    is.finite(official_maturity_years) & official_maturity_years>0 &
    is.finite(official_grace_years) & official_grace_years==official_maturity_years)
}
