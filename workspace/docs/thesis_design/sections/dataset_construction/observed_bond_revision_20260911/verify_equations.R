# Run from the project root. Bounded manuscript-equation check with synthetic
# inputs; no empirical pipeline, source data or accepted method is changed.
source("R/p15_observed_secondary.R")
revision_dir <- paste0(
  "docs/thesis_design/sections/dataset_construction/",
  "observed_bond_revision_20260911"
)
cases <- data.frame(
  example = c("annual_coupon", "semiannual_coupon"),
  clean_price = c(98, 107),
  coupon_rate_pct = c(5, 4),
  frequency = c(1L, 2L)
)
quotation_date <- as.Date("2024-11-19")
maturity_date <- as.Date("2030-08-31")
results <- lapply(seq_len(nrow(cases)), function(i) {
  x <- cases[i, ]
  yield_pct <- p15_secondary_ytm_from_clean_price(
    x$clean_price, quotation_date, maturity_date,
    x$coupon_rate_pct, x$frequency
  )
  schedule <- p15_secondary_coupon_schedule(
    quotation_date, maturity_date, x$frequency
  )
  accrued_interest <- p15_secondary_accrued_interest(
    quotation_date, maturity_date, x$coupon_rate_pct, x$frequency
  )
  years_to_payment <- as.numeric(schedule$pay_dates - quotation_date) / 365.25
  cashflows <- rep(x$coupon_rate_pct / x$frequency, length(years_to_payment))
  cashflows[as.character(schedule$pay_dates) == as.character(maturity_date)] <-
    cashflows[as.character(schedule$pay_dates) == as.character(maturity_date)] + 100
  # Displayed equation uses percent yields and per-100-face prices/cash flows.
  displayed_pv <- sum(cashflows /
    (1 + yield_pct / (100 * x$frequency)) ^ (x$frequency * years_to_payment))
  data.frame(
    x, quotation_date, maturity_date, yield_pct, accrued_interest,
    target_dirty_price = x$clean_price + accrued_interest,
    displayed_pv,
    residual = displayed_pv - x$clean_price - accrued_interest
  )
})
results <- do.call(rbind, results)
stopifnot(all(is.finite(results$yield_pct)), all(abs(results$residual) < 1e-8))
write.csv(results, file.path(revision_dir, "equation_check.csv"), row.names = FALSE)
cat("Displayed bond-price equation agrees with implementation for both synthetic examples.\n")
