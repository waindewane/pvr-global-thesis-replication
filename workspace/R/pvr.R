as_decimal_rate <- function(rate_percent) {
  rate_percent / 100
}

make_amortizing_cash_flows <- function(notional = 100, annual_rate_percent, maturity_years, grace_years) {
  stopifnot(is.numeric(notional), length(notional) == 1, notional > 0)
  stopifnot(is.numeric(annual_rate_percent), length(annual_rate_percent) == 1)
  stopifnot(is.numeric(maturity_years), length(maturity_years) == 1)
  stopifnot(is.numeric(grace_years), length(grace_years) == 1)

  maturity <- as.numeric(maturity_years)
  grace <- as.numeric(grace_years)
  periods <- as.integer(ceiling(maturity))

  if (maturity <= 0) stop("maturity_years must be positive.", call. = FALSE)
  if (grace < 0) stop("grace_years must be nonnegative.", call. = FALSE)
  if (grace >= maturity) stop("grace_years must be less than maturity_years.", call. = FALSE)

  rate <- as_decimal_rate(annual_rate_percent)
  amortization_years <- maturity - grace
  annual_principal <- notional / amortization_years
  balance <- notional

  rows <- vector("list", periods)
  for (year in seq_len(periods)) {
    period_start <- year - 1
    period_end <- min(year, maturity)
    period_length <- period_end - period_start
    amortizing_length <- max(0, period_end - max(period_start, grace))
    interest <- balance * rate * period_length
    principal <- if (amortizing_length > 0) min(annual_principal * amortizing_length, balance) else 0
    debt_service <- interest + principal
    rows[[year]] <- data.frame(
      year = year,
      period_length = period_length,
      opening_balance = balance,
      interest = interest,
      principal = principal,
      debt_service = debt_service
    )
    balance <- balance - principal
  }

  dplyr::bind_rows(rows)
}

make_bullet_cash_flows <- function(notional = 100, annual_rate_percent, maturity_years) {
  stopifnot(is.numeric(notional), length(notional) == 1, notional > 0)
  maturity <- as.numeric(maturity_years)
  periods <- as.integer(ceiling(maturity))
  if (maturity <= 0) stop("maturity_years must be positive.", call. = FALSE)

  rate <- as_decimal_rate(annual_rate_percent)
  years <- seq_len(periods)
  period_lengths <- pmin(years, maturity) - (years - 1)
  tibble::tibble(
    year = years,
    period_length = period_lengths,
    opening_balance = notional,
    interest = notional * rate * period_length,
    principal = dplyr::if_else(year == periods, notional, 0),
    debt_service = interest + principal
  )
}

present_value <- function(cash_flows, discount_rate_percent) {
  if (!"year" %in% names(cash_flows) || !"debt_service" %in% names(cash_flows)) {
    stop("cash_flows must contain year and debt_service columns.", call. = FALSE)
  }
  discount_rate <- as_decimal_rate(discount_rate_percent)
  sum(cash_flows$debt_service / ((1 + discount_rate) ^ cash_flows$year))
}

calculate_official_pvr <- function(annual_rate_percent, maturity_years, grace_years, discount_rate_percent, notional = 100) {
  flows <- make_amortizing_cash_flows(
    notional = notional,
    annual_rate_percent = annual_rate_percent,
    maturity_years = maturity_years,
    grace_years = grace_years
  )
  present_value(flows, discount_rate_percent = discount_rate_percent)
}

calculate_market_pvr <- function(market_rate_percent, market_maturity_years, discount_rate_percent = market_rate_percent, notional = 100) {
  flows <- make_bullet_cash_flows(
    notional = notional,
    annual_rate_percent = market_rate_percent,
    maturity_years = market_maturity_years
  )
  present_value(flows, discount_rate_percent = discount_rate_percent)
}
