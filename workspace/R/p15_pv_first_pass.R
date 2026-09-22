# Candidate, normalized commitment-basis schedules. Legacy R/pvr.R is unchanged.
p15_term_reason <- function(rate, maturity, grace) {
  dplyr::case_when(
    !is.finite(rate) | !is.finite(maturity) | !is.finite(grace) ~ "missing_term_field",
    maturity <= 0 ~ "nonpositive_maturity",
    grace < 0 ~ "negative_grace",
    grace > maturity ~ "grace_exceeds_maturity",
    grace == maturity ~ "bullet_like_pair_not_equal_principal_baseline",
    rate < 0 ~ "negative_contractual_rate_review",
    TRUE ~ "usable_stylized_terms")
}

p15_pv_schedule <- function(rate, maturity, grace) {
  if (p15_term_reason(rate, maturity, grace) != "usable_stylized_terms")
    stop("Terms do not support the equal-principal baseline")
  x <- make_amortizing_cash_flows(100, rate, maturity, grace)
  x$payment_time_years <- pmin(x$year, maturity)
  x
}

p15_stream_pv <- function(flows, discount_rate_pct) {
  if (length(discount_rate_pct) != 1L || !is.finite(discount_rate_pct) || discount_rate_pct <= -100)
    stop("Invalid discount rate")
  sum(flows$debt_service / (1 + discount_rate_pct / 100)^flows$payment_time_years)
}

# Independently specified vector implementation for checks against the loop.
p15_reference_stream <- function(rate, maturity, grace) {
  ends <- pmin(seq_len(ceiling(maturity)), maturity)
  starts <- c(0, head(ends, -1))
  principal <- 100 * (pmax(ends - grace, 0) - pmax(starts - grace, 0)) / (maturity - grace)
  opening <- 100 - c(0, head(cumsum(principal), -1))
  data.frame(payment_time_years = ends, principal = principal,
    interest = opening * rate / 100 * (ends - starts),
    debt_service = principal + opening * rate / 100 * (ends - starts))
}

# Vector parser for the bounded 2024 metadata/coverage extension.
p15_parse_term_snapshot <- function(path) {
  r <- jsonlite::fromJSON(path)
  x <- r$source$data
  if (is.null(x) || !is.data.frame(x) || !nrow(x)) return(tibble::tibble())
  if (as.integer(r$pages) != 1L || nrow(x) != as.integer(r$total)) stop("Incomplete snapshot: ", path)
  get <- function(v, concept) v$id[match(concept, v$concept)]
  tibble::tibble(iso3 = vapply(x$variable, get, character(1), "Country"),
    year = as.integer(sub("YR", "", vapply(x$variable, get, character(1), "Time"))),
    creditor_id = vapply(x$variable, get, character(1), "Counterpart-Area"),
    indicator_id = vapply(x$variable, get, character(1), "Series"),
    value = as.numeric(x$value), source_file = path)
}
