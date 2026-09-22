#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(readr)
  library(stringr)
  library(tidyr)
})

source("R/pvr.R")

experiment_dir <- "experiments/full_ladder_ratings_integrated_status_rebuild_2026-06-01"
experiment_output_dir <- file.path(experiment_dir, "outputs")
workstream_dir <- "docs/repayment_profile_method_workstream"
output_dir <- file.path(workstream_dir, "outputs")
source_dir <- "sources/official_terms/world_bank_latest_loan_credit_snapshot_probe_20260701"
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

generated_at <- format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")
analysis_date <- as.character(Sys.Date())

path_exp <- function(file) file.path(experiment_output_dir, file)
path_out <- function(file) file.path(output_dir, file)
write_csv_na <- function(x, path) readr::write_csv(x, path, na = "")

as_decimal <- function(rate_percent) rate_percent / 100

parse_wb_date <- function(x) {
  suppressWarnings(as.Date(as.character(x), format = "%d-%b-%Y"))
}

parse_num <- function(x) {
  suppressWarnings(as.numeric(x))
}

normalize_country <- function(x) {
  key <- iconv(as.character(x), from = "", to = "ASCII//TRANSLIT")
  key <- tolower(key)
  key <- str_replace_all(key, "&", " and ")
  key <- str_replace_all(key, "[^a-z0-9]+", " ")
  key <- str_squish(key)
  dplyr::recode(
    key,
    "egypt arab rep" = "egypt arab republic of",
    "congo dem rep" = "congo democratic republic of",
    "congo rep" = "congo republic of",
    "gambia the" = "gambia the",
    "iran islamic rep" = "iran islamic republic of",
    "korea rep" = "korea republic of",
    "lao pdr" = "lao people s democratic republic",
    "russian federation" = "russia",
    "slovak republic" = "slovakia",
    "syrian arab republic" = "syria",
    "turkiye" = "turkiye",
    "venezuela rb" = "venezuela republica bolivariana de",
    "viet nam" = "viet nam",
    .default = key
  )
}

safe_weighted_mean <- function(x, w) {
  ok <- !is.na(x) & !is.na(w) & w > 0
  if (!any(ok)) return(NA_real_)
  stats::weighted.mean(x[ok], w[ok])
}

present_value_timed <- function(cash_flows, discount_rate_percent) {
  discount_rate <- as_decimal(discount_rate_percent)
  sum(cash_flows$debt_service / ((1 + discount_rate) ^ cash_flows$time_years), na.rm = TRUE)
}

make_equal_principal_frequency_flows <- function(
    notional = 100,
    annual_rate_percent,
    maturity_years,
    grace_years,
    payments_per_year = 1,
    start_time = 0) {
  maturity <- as.numeric(maturity_years)
  grace <- as.numeric(grace_years)
  if (is.na(maturity) || is.na(grace) || maturity <= 0 || grace < 0 || grace >= maturity) {
    return(tibble())
  }

  periods <- as.integer(ceiling(maturity * payments_per_year))
  rate <- as_decimal(annual_rate_percent)
  annual_principal <- notional / (maturity - grace)
  balance <- notional

  rows <- vector("list", periods)
  for (period in seq_len(periods)) {
    period_start <- (period - 1) / payments_per_year
    period_end <- min(period / payments_per_year, maturity)
    period_length <- period_end - period_start
    amortizing_length <- max(0, period_end - max(period_start, grace))
    interest <- balance * rate * period_length
    principal <- if (amortizing_length > 0) min(annual_principal * amortizing_length, balance) else 0
    rows[[period]] <- tibble(
      time_years = start_time + period_end,
      period_length = period_length,
      opening_balance = balance,
      interest = interest,
      principal = principal,
      debt_service = interest + principal
    )
    balance <- balance - principal
  }

  bind_rows(rows)
}

make_annuity_frequency_flows <- function(
    notional = 100,
    annual_rate_percent,
    maturity_years,
    grace_years,
    payments_per_year = 1,
    start_time = 0) {
  maturity <- as.numeric(maturity_years)
  grace <- as.numeric(grace_years)
  if (is.na(maturity) || is.na(grace) || maturity <= 0 || grace < 0 || grace >= maturity) {
    return(tibble())
  }

  periods <- as.integer(ceiling(maturity * payments_per_year))
  rate <- as_decimal(annual_rate_percent)
  periodic_rate <- rate / payments_per_year
  balance <- notional

  period_grid <- tibble(
    period = seq_len(periods),
    period_start = (.data$period - 1) / payments_per_year,
    period_end = pmin(.data$period / payments_per_year, maturity),
    period_length = .data$period_end - .data$period_start,
    amortizing_length = pmax(0, .data$period_end - pmax(.data$period_start, grace)),
    amortizing_period = .data$amortizing_length > 0
  )
  n_amortizing <- sum(period_grid$amortizing_period)
  if (n_amortizing <= 0) return(tibble())

  payment <- if (abs(periodic_rate) < 1e-12) {
    notional / n_amortizing
  } else {
    notional * periodic_rate / (1 - (1 + periodic_rate)^(-n_amortizing))
  }

  rows <- vector("list", periods)
  for (i in seq_len(periods)) {
    row <- period_grid[i, ]
    interest <- balance * rate * row$period_length
    principal <- if (row$amortizing_period) min(max(payment - interest, 0), balance) else 0
    rows[[i]] <- tibble(
      time_years = start_time + row$period_end,
      period_length = row$period_length,
      opening_balance = balance,
      interest = interest,
      principal = principal,
      debt_service = interest + principal
    )
    balance <- balance - principal
  }

  flows <- bind_rows(rows)
  if (nrow(flows) > 0 && abs(sum(flows$principal) - notional) > 1e-6) {
    final_row <- nrow(flows)
    flows$principal[final_row] <- flows$principal[final_row] + (notional - sum(flows$principal))
    flows$debt_service[final_row] <- flows$interest[final_row] + flows$principal[final_row]
  }
  flows
}

make_bullet_frequency_flows <- function(
    notional = 100,
    annual_rate_percent,
    maturity_years,
    payments_per_year = 1,
    start_time = 0) {
  maturity <- as.numeric(maturity_years)
  if (is.na(maturity) || maturity <= 0) return(tibble())

  periods <- as.integer(ceiling(maturity * payments_per_year))
  rate <- as_decimal(annual_rate_percent)

  tibble(
    period = seq_len(periods),
    period_start = (.data$period - 1) / payments_per_year,
    period_end = pmin(.data$period / payments_per_year, maturity),
    period_length = .data$period_end - .data$period_start,
    time_years = start_time + .data$period_end,
    opening_balance = notional,
    interest = notional * rate * .data$period_length,
    principal = if_else(.data$period == periods, notional, 0),
    debt_service = .data$interest + .data$principal
  )
}

make_step_principal_frequency_flows <- function(
    notional = 100,
    annual_rate_percent,
    maturity_years,
    step_schedule,
    payments_per_year = 1,
    start_time = 0) {
  maturity <- as.numeric(maturity_years)
  if (is.na(maturity) || maturity <= 0 || nrow(step_schedule) == 0) return(tibble())

  periods <- as.integer(ceiling(maturity * payments_per_year))
  rate <- as_decimal(annual_rate_percent)
  balance <- notional

  rows <- vector("list", periods)
  for (period in seq_len(periods)) {
    period_start <- (period - 1) / payments_per_year
    period_end <- min(period / payments_per_year, maturity)
    period_length <- period_end - period_start
    interest <- balance * rate * period_length
    scheduled_principal <- 0
    for (i in seq_len(nrow(step_schedule))) {
      overlap <- max(0, min(period_end, step_schedule$interval_end[i]) -
        max(period_start, step_schedule$interval_start[i]))
      scheduled_principal <- scheduled_principal +
        notional * step_schedule$annual_principal_pct[i] / 100 * overlap
    }
    principal <- min(scheduled_principal, balance)
    rows[[period]] <- tibble(
      time_years = start_time + period_end,
      period_length = period_length,
      opening_balance = balance,
      interest = interest,
      principal = principal,
      debt_service = interest + principal
    )
    balance <- balance - principal
  }

  flows <- bind_rows(rows)
  if (nrow(flows) > 0 && abs(sum(flows$principal) - notional) > 1e-5) {
    final_row <- nrow(flows)
    flows$principal[final_row] <- flows$principal[final_row] + (notional - sum(flows$principal))
    flows$debt_service[final_row] <- flows$interest[final_row] + flows$principal[final_row]
  }
  flows
}

pv_equal_principal_scalar <- function(
    notional = 100,
    annual_rate_percent,
    maturity_years,
    grace_years,
    discount_rate_percent,
    payments_per_year = 1,
    start_time = 0) {
  maturity <- as.numeric(maturity_years)
  grace <- as.numeric(grace_years)
  if (is.na(maturity) || is.na(grace) || maturity <= 0 || grace < 0 || grace >= maturity) {
    return(NA_real_)
  }

  periods <- as.integer(ceiling(maturity * payments_per_year))
  rate <- as_decimal(annual_rate_percent)
  discount_rate <- as_decimal(discount_rate_percent)
  annual_principal <- notional / (maturity - grace)
  balance <- notional
  pv <- 0

  for (period in seq_len(periods)) {
    period_start <- (period - 1) / payments_per_year
    period_end <- min(period / payments_per_year, maturity)
    period_length <- period_end - period_start
    amortizing_length <- max(0, period_end - max(period_start, grace))
    interest <- balance * rate * period_length
    principal <- if (amortizing_length > 0) min(annual_principal * amortizing_length, balance) else 0
    pv <- pv + (interest + principal) / ((1 + discount_rate) ^ (start_time + period_end))
    balance <- balance - principal
  }

  pv
}

pv_annuity_scalar <- function(
    notional = 100,
    annual_rate_percent,
    maturity_years,
    grace_years,
    discount_rate_percent,
    payments_per_year = 1,
    start_time = 0) {
  maturity <- as.numeric(maturity_years)
  grace <- as.numeric(grace_years)
  if (is.na(maturity) || is.na(grace) || maturity <= 0 || grace < 0 || grace >= maturity) {
    return(NA_real_)
  }

  periods <- as.integer(ceiling(maturity * payments_per_year))
  rate <- as_decimal(annual_rate_percent)
  periodic_rate <- rate / payments_per_year
  discount_rate <- as_decimal(discount_rate_percent)

  period_start <- (seq_len(periods) - 1) / payments_per_year
  period_end <- pmin(seq_len(periods) / payments_per_year, maturity)
  amortizing_length <- pmax(0, period_end - pmax(period_start, grace))
  amortizing_period <- amortizing_length > 0
  n_amortizing <- sum(amortizing_period)
  if (n_amortizing <= 0) return(NA_real_)

  payment <- if (abs(periodic_rate) < 1e-12) {
    notional / n_amortizing
  } else {
    notional * periodic_rate / (1 - (1 + periodic_rate)^(-n_amortizing))
  }

  balance <- notional
  pv <- 0
  total_principal <- 0

  for (period in seq_len(periods)) {
    period_length <- period_end[period] - period_start[period]
    interest <- balance * rate * period_length
    principal <- if (amortizing_period[period]) min(max(payment - interest, 0), balance) else 0
    if (period == periods && abs((total_principal + principal) - notional) > 1e-6) {
      principal <- principal + (notional - (total_principal + principal))
    }
    debt_service <- interest + principal
    pv <- pv + debt_service / ((1 + discount_rate) ^ (start_time + period_end[period]))
    balance <- balance - principal
    total_principal <- total_principal + principal
  }

  pv
}

pv_bullet_scalar <- function(
    notional = 100,
    annual_rate_percent,
    maturity_years,
    discount_rate_percent,
    payments_per_year = 1,
    start_time = 0) {
  maturity <- as.numeric(maturity_years)
  if (is.na(maturity) || maturity <= 0) return(NA_real_)

  periods <- as.integer(ceiling(maturity * payments_per_year))
  rate <- as_decimal(annual_rate_percent)
  discount_rate <- as_decimal(discount_rate_percent)
  pv <- 0
  for (period in seq_len(periods)) {
    period_start <- (period - 1) / payments_per_year
    period_end <- min(period / payments_per_year, maturity)
    period_length <- period_end - period_start
    interest <- notional * rate * period_length
    principal <- if (period == periods) notional else 0
    pv <- pv + (interest + principal) / ((1 + discount_rate) ^ (start_time + period_end))
  }
  pv
}

pv_step_principal_scalar <- function(
    notional = 100,
    annual_rate_percent,
    maturity_years,
    step_schedule,
    discount_rate_percent,
    payments_per_year = 1,
    start_time = 0) {
  maturity <- as.numeric(maturity_years)
  if (is.na(maturity) || maturity <= 0 || nrow(step_schedule) == 0) return(NA_real_)

  periods <- as.integer(ceiling(maturity * payments_per_year))
  rate <- as_decimal(annual_rate_percent)
  discount_rate <- as_decimal(discount_rate_percent)
  balance <- notional
  total_principal <- 0
  pv <- 0

  for (period in seq_len(periods)) {
    period_start <- (period - 1) / payments_per_year
    period_end <- min(period / payments_per_year, maturity)
    period_length <- period_end - period_start
    interest <- balance * rate * period_length
    scheduled_principal <- 0
    for (i in seq_len(nrow(step_schedule))) {
      overlap <- max(0, min(period_end, step_schedule$interval_end[i]) -
        max(period_start, step_schedule$interval_start[i]))
      scheduled_principal <- scheduled_principal +
        notional * step_schedule$annual_principal_pct[i] / 100 * overlap
    }
    principal <- min(scheduled_principal, balance)
    if (period == periods && abs((total_principal + principal) - notional) > 1e-5) {
      principal <- principal + (notional - (total_principal + principal))
    }
    pv <- pv + (interest + principal) / ((1 + discount_rate) ^ (start_time + period_end))
    balance <- balance - principal
    total_principal <- total_principal + principal
  }

  pv
}

calculate_variant_pvr <- function(
    annual_rate_percent,
    maturity_years,
    grace_years,
    discount_rate_percent,
    schedule_type,
    payments_per_year = 1,
    use_current_code = FALSE) {
  if (use_current_code) {
    return(calculate_official_pvr(
      annual_rate_percent = annual_rate_percent,
      maturity_years = maturity_years,
      grace_years = grace_years,
      discount_rate_percent = discount_rate_percent
    ))
  }

  switch(
    schedule_type,
    equal_principal = pv_equal_principal_scalar(
      annual_rate_percent = annual_rate_percent,
      maturity_years = maturity_years,
      grace_years = grace_years,
      discount_rate_percent = discount_rate_percent,
      payments_per_year = payments_per_year
    ),
    annuity = pv_annuity_scalar(
      annual_rate_percent = annual_rate_percent,
      maturity_years = maturity_years,
      grace_years = grace_years,
      discount_rate_percent = discount_rate_percent,
      payments_per_year = payments_per_year
    ),
    bullet = pv_bullet_scalar(
      annual_rate_percent = annual_rate_percent,
      maturity_years = maturity_years,
      discount_rate_percent = discount_rate_percent,
      payments_per_year = payments_per_year
    ),
    stop("Unknown schedule_type: ", schedule_type, call. = FALSE)
  )
}

resolve_horizon <- function(code, maturity_years, grace_years) {
  maturity <- as.numeric(maturity_years)
  grace <- as.numeric(grace_years)
  horizon <- switch(
    code,
    none = 0,
    fixed_2y = 2,
    fixed_3y = 3,
    fixed_5y = 5,
    fixed_7y = 7,
    grace_capped3 = min(max(grace, 0), 3),
    grace_capped5 = min(max(grace, 0), 5),
    stop("Unknown horizon code: ", code, call. = FALSE)
  )
  min(horizon, maturity)
}

disbursement_profile <- function(notional = 100, horizon_years, profile = "uniform") {
  horizon <- as.numeric(horizon_years)
  if (is.na(horizon) || horizon <= 0) {
    return(tibble(time_years = 0, amount = notional))
  }
  periods_per_year <- 2
  periods <- max(1L, as.integer(ceiling(horizon * periods_per_year)))
  times <- pmin((seq_len(periods) - 0.5) / periods_per_year, horizon)
  weights <- switch(
    profile,
    uniform = rep(1, periods),
    front_loaded = rev(seq_len(periods)),
    back_loaded = seq_len(periods),
    stop("Unknown disbursement profile: ", profile, call. = FALSE)
  )
  tibble(
    time_years = times,
    amount = notional * weights / sum(weights)
  )
}

pv_disbursements <- function(notional = 100, horizon_years, discount_rate_percent, profile = "uniform") {
  flows <- disbursement_profile(notional, horizon_years, profile)
  discount_rate <- as_decimal(discount_rate_percent)
  sum(flows$amount / ((1 + discount_rate) ^ flows$time_years), na.rm = TRUE)
}

calculate_denominator_adjusted_pvr <- function(
    annual_rate_percent,
    maturity_years,
    grace_years,
    discount_rate_percent,
    schedule_type,
    payments_per_year,
    horizon_code,
    disbursement_profile_name) {
  numerator <- calculate_variant_pvr(
    annual_rate_percent = annual_rate_percent,
    maturity_years = maturity_years,
    grace_years = grace_years,
    discount_rate_percent = discount_rate_percent,
    schedule_type = schedule_type,
    payments_per_year = payments_per_year
  )
  horizon <- resolve_horizon(horizon_code, maturity_years, grace_years)
  denominator <- pv_disbursements(100, horizon, discount_rate_percent, disbursement_profile_name)
  if (is.na(numerator) || is.na(denominator) || denominator <= 0) return(NA_real_)
  numerator / denominator * 100
}

calculate_tranched_disbursement_pvr <- function(
    annual_rate_percent,
    maturity_years,
    grace_years,
    discount_rate_percent,
    horizon_code,
    disbursement_profile_name = "uniform",
    schedule_type = "equal_principal",
    payments_per_year = 1) {
  horizon <- resolve_horizon(horizon_code, maturity_years, grace_years)
  disbursements <- disbursement_profile(100, horizon, disbursement_profile_name)

  debt_service <- lapply(seq_len(nrow(disbursements)), function(i) {
    tranche <- disbursements[i, ]
    switch(
      schedule_type,
      equal_principal = tibble(
        time_years = 0,
        debt_service = pv_equal_principal_scalar(
          notional = tranche$amount,
          annual_rate_percent = annual_rate_percent,
          maturity_years = maturity_years,
          grace_years = grace_years,
          discount_rate_percent = discount_rate_percent,
          payments_per_year = payments_per_year,
          start_time = tranche$time_years
        )
      ),
      annuity = tibble(
        time_years = 0,
        debt_service = pv_annuity_scalar(
          notional = tranche$amount,
          annual_rate_percent = annual_rate_percent,
          maturity_years = maturity_years,
          grace_years = grace_years,
          discount_rate_percent = discount_rate_percent,
          payments_per_year = payments_per_year,
          start_time = tranche$time_years
        )
      ),
      stop("Unknown tranched schedule_type: ", schedule_type, call. = FALSE)
    )
  }) |>
    bind_rows()

  if (any(is.na(debt_service$debt_service))) return(NA_real_)
  numerator <- sum(debt_service$debt_service, na.rm = TRUE)
  denominator <- pv_disbursements(100, horizon, discount_rate_percent, disbursement_profile_name)
  if (is.na(numerator) || is.na(denominator) || denominator <= 0) return(NA_real_)
  numerator / denominator * 100
}

pv_ibrd_commitment_fee <- function(horizon_years, discount_rate_percent, annual_fee_percent = 0.25) {
  horizon <- as.numeric(horizon_years)
  if (is.na(horizon) || horizon <= 4) return(0)
  disb <- disbursement_profile(100, horizon, "uniform")
  periods_per_year <- 2
  periods <- as.integer(ceiling(horizon * periods_per_year))
  discount_rate <- as_decimal(discount_rate_percent)
  fee_rate <- as_decimal(annual_fee_percent)
  pv_fee <- 0
  for (period in seq_len(periods)) {
    t0 <- (period - 1) / periods_per_year
    t1 <- min(period / periods_per_year, horizon)
    if (t1 <= 4) next
    midpoint <- (t0 + t1) / 2
    disbursed_by_start <- sum(disb$amount[disb$time_years <= t0], na.rm = TRUE)
    undisbursed <- max(0, 100 - disbursed_by_start)
    fee <- undisbursed * fee_rate * (t1 - t0)
    pv_fee <- pv_fee + fee / ((1 + discount_rate) ^ midpoint)
  }
  pv_fee
}

ida_product_specs <- tibble::tribble(
  ~product_id, ~product_label, ~maturity_years, ~grace_years, ~rate_q3_2024_usd, ~rate_q4_2024_usd,
  "regular", "IDA regular credit: 38-year final maturity, 6-year grace", 38, 6, 1.37, 1.33,
  "small_economy", "IDA small-economy credit: 40-year final maturity, 10-year grace", 40, 10, 1.34, 1.31,
  "blend", "IDA blend credit: 30-year final maturity, 5-year grace", 30, 5, 2.80, 2.71,
  "sml", "IDA Shorter Maturity Loan: 12-year final maturity, 6-year grace", 12, 6, 0.00, 0.00,
  "fifty_year", "IDA 50-year credit: 50-year final maturity, 10-year grace", 50, 10, 0.00, 0.00
)

ida_step_schedule <- function(product_id) {
  switch(
    product_id,
    regular = tibble(interval_start = 6, interval_end = 38, annual_principal_pct = 3.125),
    small_economy = tibble(
      interval_start = c(10, 20),
      interval_end = c(20, 40),
      annual_principal_pct = c(2.0, 4.0)
    ),
    blend = tibble(
      interval_start = c(5, 25),
      interval_end = c(25, 30),
      annual_principal_pct = c(3.3, 6.8)
    ),
    sml = tibble(interval_start = 6, interval_end = 12, annual_principal_pct = 100 / 6),
    fifty_year = tibble(interval_start = 10, interval_end = 50, annual_principal_pct = 2.5),
    stop("Unknown IDA product: ", product_id, call. = FALSE)
  )
}

calculate_ida_product_pvr <- function(product_id, annual_rate_percent, discount_rate_percent, payments_per_year = 1) {
  product <- ida_product_specs |> filter(.data$product_id == !!product_id)
  if (nrow(product) != 1) return(NA_real_)
  pv_step_principal_scalar(
    annual_rate_percent = annual_rate_percent,
    maturity_years = product$maturity_years,
    step_schedule = ida_step_schedule(product_id),
    discount_rate_percent = discount_rate_percent,
    payments_per_year = payments_per_year
  )
}

classify_ida_product <- function(maturity_years, grace_years) {
  if (is.na(maturity_years) || is.na(grace_years)) return(NA_character_)
  distances <- abs(ida_product_specs$maturity_years - maturity_years) +
    abs(ida_product_specs$grace_years - grace_years)
  best <- ida_product_specs$product_id[which.min(distances)]
  if (min(distances) <= 2.5) best else "other"
}

small_economy_country_keys <- normalize_country(c(
  "Belize", "Bhutan", "Cabo Verde", "Comoros", "Dominica", "Eswatini", "Fiji",
  "Grenada", "Guyana", "Kiribati", "Maldives", "Marshall Islands",
  "Micronesia, Federated States of", "Samoa", "Sao Tome and Principe",
  "Solomon Islands", "St. Lucia", "St. Vincent and the Grenadines",
  "Suriname", "Timor-Leste", "Tonga", "Tuvalu", "Vanuatu"
))

choose_ida_heuristic_product <- function(country, lending_type) {
  country_key <- normalize_country(country)
  if (!is.na(lending_type) && lending_type == "Blend") return("blend")
  if (country_key %in% small_economy_country_keys) return("small_economy")
  "regular"
}

theme_repayment <- function(base_size = 10) {
  theme_minimal(base_size = base_size) +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_line(color = "grey88", linewidth = 0.3),
      panel.grid.major.x = element_blank(),
      axis.text = element_text(color = "grey25"),
      axis.title = element_text(color = "grey25"),
      legend.position = "top",
      legend.title = element_blank(),
      plot.title = element_text(face = "bold"),
      plot.subtitle = element_text(color = "grey35"),
      plot.caption = element_text(color = "grey45", hjust = 0, size = rel(0.8)),
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA)
    )
}

pvr_rows <- read_csv(
  path_exp("p13_pvr_results_2024.csv"),
  show_col_types = FALSE,
  col_types = cols(.default = col_guess())
) |>
  filter(.data$coverage_flag == "included", .data$creditor != "Bonds") |>
  mutate(
    official_rate = as.numeric(.data$official_rate),
    official_maturity_years = as.numeric(.data$official_maturity_years),
    official_grace_years = as.numeric(.data$official_grace_years),
    discount_rate = as.numeric(.data$discount_rate),
    pvr_reported = as.numeric(.data$pvr),
    country_key = normalize_country(.data$country)
  )

benchmark_rows <- read_csv(
  path_exp("p13_best_available_benchmark_rate_2024.csv"),
  show_col_types = FALSE,
  col_types = cols(.default = col_guess())
) |>
  transmute(
    iso3 = .data$iso3,
    analysis_year = .data$analysis_year,
    income_level = .data$income_level,
    lending_type = .data$lending_type,
    selected_rate_pct = as.numeric(.data$selected_rate_pct),
    selected_source_class = .data$selected_source_class,
    selected_reliability_label = .data$selected_reliability_label,
    selected_warning_label = .data$selected_warning_label
  )

source_rows <- pvr_rows |>
  left_join(benchmark_rows, by = c("iso3", "analysis_year")) |>
  mutate(
    row_id = row_number(),
    core_creditor = .data$creditor %in% c("IBRD", "IDA", "China"),
    base_pvr_unrounded = mapply(
      calculate_variant_pvr,
      annual_rate_percent = .data$official_rate,
      maturity_years = .data$official_maturity_years,
      grace_years = .data$official_grace_years,
      discount_rate_percent = .data$discount_rate,
      MoreArgs = list(
        schedule_type = "equal_principal",
        payments_per_year = 1,
        use_current_code = TRUE
      )
    )
  )

generic_scenarios <- tibble::tribble(
  ~scenario_id, ~scenario_label, ~scenario_role, ~source_basis, ~schedule_type, ~payments_per_year, ~calculation_mode, ~horizon_code, ~disbursement_profile_name, ~applies_to_creditor,
  "current_code_recomputed", "Current code: annual equal principal after grace, nominal commitment denominator", "current_baseline", "Current R/pvr.R implementation", "equal_principal", 1L, "current_code", "none", "uniform", "all",
  "equal_principal_annual_exact_timing", "Equal principal annual schedule with exact period-end timing", "timing_convention_sensitivity", "Internal timing convention check", "equal_principal", 1L, "schedule", "none", "uniform", "all",
  "equal_principal_semiannual", "Equal principal with semiannual payments", "payment_frequency_sensitivity", "Generic repayment-frequency stress", "equal_principal", 2L, "schedule", "none", "uniform", "all",
  "equal_principal_quarterly", "Equal principal with quarterly payments", "payment_frequency_sensitivity", "Generic repayment-frequency stress", "equal_principal", 4L, "schedule", "none", "uniform", "all",
  "annuity_annual", "Annuity after grace, annual payments", "repayment_shape_sensitivity", "Generic repayment-shape stress", "annuity", 1L, "schedule", "none", "uniform", "all",
  "annuity_semiannual", "Annuity after grace, semiannual payments", "repayment_shape_sensitivity", "Generic repayment-shape stress", "annuity", 2L, "schedule", "none", "uniform", "all",
  "annuity_quarterly", "Annuity after grace, quarterly payments", "repayment_shape_sensitivity", "Generic repayment-shape stress", "annuity", 4L, "schedule", "none", "uniform", "all",
  "bullet_annual_extreme", "Bullet principal at maturity, annual coupon", "extreme_stress_test", "Deliberate non-baseline repayment-shape stress", "bullet", 1L, "schedule", "none", "uniform", "all",
  "bullet_semiannual_extreme", "Bullet principal at maturity, semiannual coupon", "extreme_stress_test", "Deliberate non-baseline repayment-shape stress", "bullet", 2L, "schedule", "none", "uniform", "all",
  "denom_uniform_2y", "Current schedule, denominator PV of uniform 2-year disbursement", "disbursement_denominator_sensitivity", "Generic commitment-versus-disbursement timing stress", "equal_principal", 1L, "denominator", "fixed_2y", "uniform", "all",
  "denom_uniform_3y", "Current schedule, denominator PV of uniform 3-year disbursement", "disbursement_denominator_sensitivity", "Generic commitment-versus-disbursement timing stress", "equal_principal", 1L, "denominator", "fixed_3y", "uniform", "all",
  "denom_uniform_5y", "Current schedule, denominator PV of uniform 5-year disbursement", "disbursement_denominator_sensitivity", "Generic commitment-versus-disbursement timing stress", "equal_principal", 1L, "denominator", "fixed_5y", "uniform", "all",
  "denom_uniform_7y", "Current schedule, denominator PV of uniform 7-year disbursement", "disbursement_denominator_sensitivity", "Generic commitment-versus-disbursement timing stress", "equal_principal", 1L, "denominator", "fixed_7y", "uniform", "all",
  "denom_grace_capped3_uniform", "Current schedule, denominator PV of uniform min(grace,3)-year disbursement", "disbursement_denominator_sensitivity", "Generic commitment-versus-disbursement timing stress", "equal_principal", 1L, "denominator", "grace_capped3", "uniform", "all",
  "denom_grace_capped5_uniform", "Current schedule, denominator PV of uniform min(grace,5)-year disbursement", "disbursement_denominator_sensitivity", "Generic commitment-versus-disbursement timing stress", "equal_principal", 1L, "denominator", "grace_capped5", "uniform", "all",
  "denom_frontloaded_5y", "Current schedule, denominator PV of front-loaded 5-year disbursement", "disbursement_profile_sensitivity", "Generic commitment-versus-disbursement timing stress", "equal_principal", 1L, "denominator", "fixed_5y", "front_loaded", "all",
  "denom_backloaded_5y", "Current schedule, denominator PV of back-loaded 5-year disbursement", "disbursement_profile_sensitivity", "Generic commitment-versus-disbursement timing stress", "equal_principal", 1L, "denominator", "fixed_5y", "back_loaded", "all",
  "tranched_uniform_2y_annual", "Uniform 2-year disbursement, each tranche repaid equal-principal annually", "tranched_disbursement_sensitivity", "Generic disbursement-linked approximation", "equal_principal", 1L, "tranched", "fixed_2y", "uniform", "all",
  "tranched_uniform_3y_annual", "Uniform 3-year disbursement, each tranche repaid equal-principal annually", "tranched_disbursement_sensitivity", "Generic disbursement-linked approximation", "equal_principal", 1L, "tranched", "fixed_3y", "uniform", "all",
  "tranched_uniform_5y_annual", "Uniform 5-year disbursement, each tranche repaid equal-principal annually", "tranched_disbursement_sensitivity", "Generic disbursement-linked approximation", "equal_principal", 1L, "tranched", "fixed_5y", "uniform", "all",
  "tranched_grace_capped5_annual", "Uniform min(grace,5)-year disbursement, each tranche repaid equal-principal annually", "tranched_disbursement_sensitivity", "Generic disbursement-linked approximation", "equal_principal", 1L, "tranched", "grace_capped5", "uniform", "all",
  "tranched_uniform_5y_semiannual", "Uniform 5-year disbursement, each tranche repaid equal-principal semiannually", "tranched_disbursement_sensitivity", "Generic disbursement-linked approximation", "equal_principal", 2L, "tranched", "fixed_5y", "uniform", "all",
  "ibrd_frontend_fee_25bp", "IBRD current schedule plus 25bp front-end fee stress", "fee_sensitivity", "IBRD product-note fee stress, not a full fee model", "equal_principal", 1L, "ibrd_frontend_fee", "none", "uniform", "IBRD",
  "ibrd_uniform_5y_commitment_fee_25bp_after4y", "IBRD current schedule plus 25bp commitment-fee stress on uniform 5-year draw", "fee_sensitivity", "IBRD product-note fee stress, not a full fee model", "equal_principal", 1L, "ibrd_commitment_fee", "fixed_5y", "uniform", "IBRD"
)

calculate_generic_for_row <- function(row, scenario) {
  if (scenario$applies_to_creditor != "all" && row$creditor != scenario$applies_to_creditor) {
    return(NA_real_)
  }

  switch(
    scenario$calculation_mode,
    current_code = calculate_variant_pvr(
      annual_rate_percent = row$official_rate,
      maturity_years = row$official_maturity_years,
      grace_years = row$official_grace_years,
      discount_rate_percent = row$discount_rate,
      schedule_type = scenario$schedule_type,
      payments_per_year = scenario$payments_per_year,
      use_current_code = TRUE
    ),
    schedule = calculate_variant_pvr(
      annual_rate_percent = row$official_rate,
      maturity_years = row$official_maturity_years,
      grace_years = row$official_grace_years,
      discount_rate_percent = row$discount_rate,
      schedule_type = scenario$schedule_type,
      payments_per_year = scenario$payments_per_year
    ),
    denominator = calculate_denominator_adjusted_pvr(
      annual_rate_percent = row$official_rate,
      maturity_years = row$official_maturity_years,
      grace_years = row$official_grace_years,
      discount_rate_percent = row$discount_rate,
      schedule_type = scenario$schedule_type,
      payments_per_year = scenario$payments_per_year,
      horizon_code = scenario$horizon_code,
      disbursement_profile_name = scenario$disbursement_profile_name
    ),
    tranched = calculate_tranched_disbursement_pvr(
      annual_rate_percent = row$official_rate,
      maturity_years = row$official_maturity_years,
      grace_years = row$official_grace_years,
      discount_rate_percent = row$discount_rate,
      horizon_code = scenario$horizon_code,
      disbursement_profile_name = scenario$disbursement_profile_name,
      schedule_type = scenario$schedule_type,
      payments_per_year = scenario$payments_per_year
    ),
    ibrd_frontend_fee = row$base_pvr_unrounded + 0.25,
    ibrd_commitment_fee = {
      horizon <- resolve_horizon(scenario$horizon_code, row$official_maturity_years, row$official_grace_years)
      row$base_pvr_unrounded + pv_ibrd_commitment_fee(horizon, row$discount_rate, annual_fee_percent = 0.25)
    },
    stop("Unknown calculation_mode: ", scenario$calculation_mode, call. = FALSE)
  )
}

generic_detail <- bind_rows(lapply(seq_len(nrow(generic_scenarios)), function(s) {
  scenario <- generic_scenarios[s, ]
  scenario_values <- lapply(seq_len(nrow(source_rows)), function(i) {
    row <- source_rows[i, ]
    tibble(
      row_id = row$row_id,
      scenario_pvr = calculate_generic_for_row(row, scenario)
    )
  }) |>
    bind_rows()

  source_rows |>
    select(
      row_id, iso3, country, analysis_year, income_level, lending_type, creditor,
      creditor_name, official_rate, official_maturity_years, official_grace_years,
      discount_rate, market_rate_source_class, selected_source_class,
      selected_reliability_label, p13_display_class, included_in_lmic_reporting_scope,
      core_creditor, pvr_reported, base_pvr_unrounded
    ) |>
    left_join(scenario_values, by = "row_id") |>
    filter(!is.na(.data$scenario_pvr)) |>
    mutate(
      scenario_id = scenario$scenario_id,
      scenario_label = scenario$scenario_label,
      scenario_role = scenario$scenario_role,
      source_basis = scenario$source_basis,
      scenario_family = "generic_formula_sensitivity"
    )
}))

ida_product_detail <- bind_rows(lapply(seq_len(nrow(ida_product_specs)), function(i) {
  product <- ida_product_specs[i, ]
  rows <- source_rows |>
    filter(.data$creditor == "IDA") |>
    mutate(
      scenario_pvr = mapply(
        calculate_ida_product_pvr,
        product_id = product$product_id,
        annual_rate_percent = .data$official_rate,
        discount_rate_percent = .data$discount_rate,
        MoreArgs = list(payments_per_year = 1)
      ),
      scenario_id = paste0("ida_product_", product$product_id, "_terms_with_ids_rate"),
      scenario_label = paste0(product$product_label, " using IDS official rate"),
      scenario_role = "ida_product_term_sensitivity",
      source_basis = "IDA 2024 quarterly term sheets; rate held at IDS row value",
      scenario_family = "ida_product_terms"
    )
  rows
})) |>
  select(
    row_id, iso3, country, analysis_year, income_level, lending_type, creditor,
    creditor_name, official_rate, official_maturity_years, official_grace_years,
    discount_rate, market_rate_source_class, selected_source_class,
    selected_reliability_label, p13_display_class, included_in_lmic_reporting_scope,
    core_creditor, pvr_reported, base_pvr_unrounded, scenario_pvr, scenario_id,
    scenario_label, scenario_role, source_basis, scenario_family
  )

ida_q3_rate_detail <- bind_rows(lapply(seq_len(nrow(ida_product_specs)), function(i) {
  product <- ida_product_specs[i, ]
  rows <- source_rows |>
    filter(.data$creditor == "IDA") |>
    mutate(
      scenario_pvr = mapply(
        calculate_ida_product_pvr,
        product_id = product$product_id,
        annual_rate_percent = product$rate_q3_2024_usd,
        discount_rate_percent = .data$discount_rate,
        MoreArgs = list(payments_per_year = 1)
      ),
      scenario_id = paste0("ida_product_", product$product_id, "_terms_q3_2024_usd_rate"),
      scenario_label = paste0(product$product_label, " using July 2024 USD term-sheet rate"),
      scenario_role = "ida_product_rate_and_term_sensitivity",
      source_basis = "IDA term sheet effective 2024-07-01; diagnostic only because IDS row may aggregate currencies/products",
      scenario_family = "ida_product_terms"
    )
  rows
})) |>
  select(
    row_id, iso3, country, analysis_year, income_level, lending_type, creditor,
    creditor_name, official_rate, official_maturity_years, official_grace_years,
    discount_rate, market_rate_source_class, selected_source_class,
    selected_reliability_label, p13_display_class, included_in_lmic_reporting_scope,
    core_creditor, pvr_reported, base_pvr_unrounded, scenario_pvr, scenario_id,
    scenario_label, scenario_role, source_basis, scenario_family
  )

ida_heuristic_map <- source_rows |>
  filter(.data$creditor == "IDA") |>
  rowwise() |>
  mutate(
    heuristic_ida_product = choose_ida_heuristic_product(.data$country, .data$lending_type),
    heuristic_product_label = ida_product_specs$product_label[match(.data$heuristic_ida_product, ida_product_specs$product_id)],
    heuristic_product_rate_q3_2024_usd = ida_product_specs$rate_q3_2024_usd[match(.data$heuristic_ida_product, ida_product_specs$product_id)],
    heuristic_basis = case_when(
      .data$lending_type == "Blend" ~ "P13 lending_type is Blend, mapped to IDA blend credit terms",
      normalize_country(.data$country) %in% small_economy_country_keys ~ "Country is in current small-economy terms list, mapped to small-economy terms",
      TRUE ~ "Fallback IDA regular-credit terms"
    )
  ) |>
  ungroup()

ida_heuristic_detail <- bind_rows(
  ida_heuristic_map |>
    rowwise() |>
    mutate(
      scenario_pvr = calculate_ida_product_pvr(
        product_id = .data$heuristic_ida_product,
        annual_rate_percent = .data$official_rate,
        discount_rate_percent = .data$discount_rate
      ),
      scenario_id = "ida_heuristic_product_terms_with_ids_rate",
      scenario_label = "Heuristic IDA product terms using IDS official rate",
      scenario_role = "ida_product_mapping_sensitivity",
      source_basis = "P13 lending type plus current IDA small-economy country list; rate held at IDS row value",
      scenario_family = "ida_product_terms"
    ) |>
    ungroup(),
  ida_heuristic_map |>
    rowwise() |>
    mutate(
      scenario_pvr = calculate_ida_product_pvr(
        product_id = .data$heuristic_ida_product,
        annual_rate_percent = .data$heuristic_product_rate_q3_2024_usd,
        discount_rate_percent = .data$discount_rate
      ),
      scenario_id = "ida_heuristic_product_terms_q3_2024_usd_rate",
      scenario_label = "Heuristic IDA product terms using July 2024 USD term-sheet rate",
      scenario_role = "ida_product_rate_and_term_sensitivity",
      source_basis = "P13 lending type plus current IDA small-economy country list; diagnostic rate from IDA 2024-07-01 term sheet",
      scenario_family = "ida_product_terms"
    ) |>
    ungroup()
) |>
  select(
    row_id, iso3, country, analysis_year, income_level, lending_type, creditor,
    creditor_name, official_rate, official_maturity_years, official_grace_years,
    discount_rate, market_rate_source_class, selected_source_class,
    selected_reliability_label, p13_display_class, included_in_lmic_reporting_scope,
    core_creditor, pvr_reported, base_pvr_unrounded, scenario_pvr, scenario_id,
    scenario_label, scenario_role, source_basis, scenario_family
  )

ibrd_term_specs <- tibble::tribble(
  ~scenario_id, ~scenario_label, ~maturity_years, ~grace_years,
  "ibrd_product_20y_final_5y_grace_with_ids_rate", "IBRD stylized 20-year final maturity, 5-year grace, IDS official rate", 20, 5,
  "ibrd_product_25y_final_5y_grace_with_ids_rate", "IBRD stylized 25-year final maturity, 5-year grace, IDS official rate", 25, 5,
  "ibrd_product_35y_final_5y_grace_with_ids_rate", "IBRD regular maximum 35-year final maturity stress, 5-year grace, IDS official rate", 35, 5,
  "ibrd_product_short_7y_final_3y_grace_with_ids_rate", "IBRD short-maturity 7-year final maturity stress, 3-year grace, IDS official rate", 7, 3,
  "ibrd_product_global_challenge_50y_final_10y_grace_with_ids_rate", "IBRD global-challenge 50-year final maturity stress, 10-year grace, IDS official rate", 50, 10
)

ibrd_product_detail <- bind_rows(lapply(seq_len(nrow(ibrd_term_specs)), function(i) {
  spec <- ibrd_term_specs[i, ]
  source_rows |>
    filter(.data$creditor == "IBRD") |>
    mutate(
      scenario_pvr = mapply(
        calculate_variant_pvr,
        annual_rate_percent = .data$official_rate,
        maturity_years = spec$maturity_years,
        grace_years = spec$grace_years,
        discount_rate_percent = .data$discount_rate,
        MoreArgs = list(
          schedule_type = "equal_principal",
          payments_per_year = 1,
          use_current_code = FALSE
        )
      ),
      scenario_id = spec$scenario_id,
      scenario_label = spec$scenario_label,
      scenario_role = "ibrd_product_term_sensitivity",
      source_basis = "IBRD Flexible Loan product note; stylized term choices, not loan-agreement schedules",
      scenario_family = "ibrd_product_terms"
    )
})) |>
  select(
    row_id, iso3, country, analysis_year, income_level, lending_type, creditor,
    creditor_name, official_rate, official_maturity_years, official_grace_years,
    discount_rate, market_rate_source_class, selected_source_class,
    selected_reliability_label, p13_display_class, included_in_lmic_reporting_scope,
    core_creditor, pvr_reported, base_pvr_unrounded, scenario_pvr, scenario_id,
    scenario_label, scenario_role, source_basis, scenario_family
  )

p13_country_key <- source_rows |>
  distinct(iso3, country, country_key)

ibrd_statement_raw <- read_csv(
  file.path(source_dir, "ibrd_statement_of_loans_latest.csv"),
  show_col_types = FALSE,
  col_types = cols(.default = col_guess())
)

ida_statement_raw <- read_csv(
  file.path(source_dir, "ida_statement_of_credits_grants_guarantees_latest.csv"),
  show_col_types = FALSE,
  col_types = cols(.default = col_guess())
)

ibrd_statement_terms <- ibrd_statement_raw |>
  transmute(
    creditor = "IBRD",
    instrument_id = .data$loan_number,
    instrument_type = .data$loan_type,
    instrument_status = .data$loan_status,
    source_country = .data$country,
    country_key = normalize_country(.data$country),
    board_approval_date = parse_wb_date(.data$board_approval_date),
    first_repayment_date = parse_wb_date(.data$first_repayment_date),
    last_repayment_date = parse_wb_date(.data$last_repayment_date),
    original_principal_amount = parse_num(.data$original_principal_amount),
    statement_rate = parse_num(.data$interest_rate),
    project_id = .data$project_id,
    project_name = .data$project_name,
    repayment_source = "World Bank Finances IBRD Statement of Loans latest snapshot"
  ) |>
  mutate(
    instrument_in_scope = TRUE,
    approval_window_calendar_2024 = .data$board_approval_date >= as.Date("2024-01-01") &
      .data$board_approval_date <= as.Date("2024-12-31"),
    approval_window_world_bank_fy2024 = .data$board_approval_date >= as.Date("2023-07-01") &
      .data$board_approval_date <= as.Date("2024-06-30"),
    observed_maturity_years = as.numeric(.data$last_repayment_date - .data$board_approval_date) / 365.25,
    observed_grace_years = as.numeric(.data$first_repayment_date - .data$board_approval_date) / 365.25,
    observed_product_id = "ibrd_flexible_or_guarantee",
    observed_product_label = .data$instrument_type
  )

ida_statement_terms <- ida_statement_raw |>
  mutate(credit_prefix = str_sub(.data$credit_number, 1, 4)) |>
  transmute(
    creditor = "IDA",
    instrument_id = .data$credit_number,
    instrument_type = case_when(
      str_detect(.data$credit_number, "^IDA[0-9]") ~ "IDA credit",
      str_detect(.data$credit_number, "^IDAE") ~ "IDA grant/other non-amortizing record",
      str_detect(.data$credit_number, "^IDAG") ~ "IDA guarantee/non-amortizing record",
      TRUE ~ "Other IDA record"
    ),
    instrument_status = .data$credit_status,
    source_country = .data$country,
    country_key = normalize_country(.data$country),
    board_approval_date = parse_wb_date(.data$board_approval_date),
    first_repayment_date = parse_wb_date(.data$first_repayment_date),
    last_repayment_date = parse_wb_date(.data$last_repayment_date),
    original_principal_amount = parse_num(.data$original_principal_amount_us_),
    statement_rate = parse_num(.data$service_charge_rate),
    project_id = .data$project_id,
    project_name = .data$project_name,
    repayment_source = "World Bank Finances IDA Statement of Credits, Grants and Guarantees latest snapshot"
  ) |>
  mutate(
    instrument_in_scope = .data$instrument_type == "IDA credit",
    approval_window_calendar_2024 = .data$board_approval_date >= as.Date("2024-01-01") &
      .data$board_approval_date <= as.Date("2024-12-31"),
    approval_window_world_bank_fy2024 = .data$board_approval_date >= as.Date("2023-07-01") &
      .data$board_approval_date <= as.Date("2024-06-30"),
    observed_maturity_years = as.numeric(.data$last_repayment_date - .data$board_approval_date) / 365.25,
    observed_grace_years = as.numeric(.data$first_repayment_date - .data$board_approval_date) / 365.25,
    observed_product_id = vapply(
      seq_along(.data$observed_maturity_years),
      function(i) classify_ida_product(.data$observed_maturity_years[i], .data$observed_grace_years[i]),
      character(1)
    ),
    observed_product_label = ida_product_specs$product_label[match(.data$observed_product_id, ida_product_specs$product_id)],
    observed_product_label = if_else(is.na(.data$observed_product_label), "Other observed IDA term pattern", .data$observed_product_label)
  )

statement_terms_all <- bind_rows(ibrd_statement_terms, ida_statement_terms) |>
  left_join(p13_country_key, by = "country_key") |>
  mutate(
    usable_repayment_dates = !is.na(.data$observed_maturity_years) &
      !is.na(.data$observed_grace_years) &
      .data$observed_maturity_years > 0 &
      .data$observed_grace_years >= 0 &
      .data$observed_grace_years < .data$observed_maturity_years,
    usable_amount = !is.na(.data$original_principal_amount) & .data$original_principal_amount > 0,
    rate_available = !is.na(.data$statement_rate) & .data$statement_rate > 0
  )

statement_terms_probe <- statement_terms_all |>
  filter(.data$approval_window_calendar_2024 | .data$approval_window_world_bank_fy2024) |>
  mutate(
    approval_window = case_when(
      .data$approval_window_calendar_2024 & .data$approval_window_world_bank_fy2024 ~ "calendar_2024_and_world_bank_fy2024",
      .data$approval_window_calendar_2024 ~ "calendar_2024_only",
      .data$approval_window_world_bank_fy2024 ~ "world_bank_fy2024_only",
      TRUE ~ "outside_windows"
    )
  ) |>
  select(
    creditor, iso3, country, source_country, instrument_id, instrument_type,
    instrument_status, board_approval_date, first_repayment_date, last_repayment_date,
    original_principal_amount, statement_rate, project_id, project_name,
    approval_window, approval_window_calendar_2024, approval_window_world_bank_fy2024,
    instrument_in_scope, usable_repayment_dates, usable_amount, rate_available,
    observed_maturity_years, observed_grace_years, observed_product_id,
    observed_product_label, repayment_source
  )

write_csv_na(
  statement_terms_probe,
  path_out("repayment_profile_wb_statement_terms_probe_2026-07-01.csv")
)

product_mix_string <- function(product_id, amount) {
  ok <- !is.na(product_id) & !is.na(amount) & amount > 0
  if (!any(ok)) return(NA_character_)
  shares <- tapply(amount[ok], product_id[ok], sum)
  shares <- sort(shares / sum(shares), decreasing = TRUE)
  paste0(names(shares), "=", sprintf("%.1f%%", 100 * as.numeric(shares)), collapse = "; ")
}

statement_country_summary <- statement_terms_all |>
  filter(
    .data$approval_window_calendar_2024,
    .data$instrument_in_scope,
    .data$usable_repayment_dates,
    .data$usable_amount,
    !is.na(.data$iso3)
  ) |>
  group_by(.data$creditor, .data$iso3, .data$country) |>
  summarise(
    statement_calendar_2024_instrument_count = n(),
    statement_calendar_2024_total_original_principal = sum(.data$original_principal_amount, na.rm = TRUE),
    statement_calendar_2024_weighted_maturity_years = safe_weighted_mean(.data$observed_maturity_years, .data$original_principal_amount),
    statement_calendar_2024_weighted_grace_years = safe_weighted_mean(.data$observed_grace_years, .data$original_principal_amount),
    statement_calendar_2024_rate_available_count = sum(.data$rate_available, na.rm = TRUE),
    statement_calendar_2024_weighted_statement_rate = safe_weighted_mean(if_else(.data$rate_available, .data$statement_rate, NA_real_), .data$original_principal_amount),
    statement_calendar_2024_product_mix = product_mix_string(.data$observed_product_id, .data$original_principal_amount),
    .groups = "drop"
  )

fy_statement_country_summary <- statement_terms_all |>
  filter(
    .data$approval_window_world_bank_fy2024,
    .data$instrument_in_scope,
    .data$usable_repayment_dates,
    .data$usable_amount,
    !is.na(.data$iso3)
  ) |>
  group_by(.data$creditor, .data$iso3, .data$country) |>
  summarise(
    statement_fy2024_instrument_count = n(),
    statement_fy2024_total_original_principal = sum(.data$original_principal_amount, na.rm = TRUE),
    statement_fy2024_weighted_maturity_years = safe_weighted_mean(.data$observed_maturity_years, .data$original_principal_amount),
    statement_fy2024_weighted_grace_years = safe_weighted_mean(.data$observed_grace_years, .data$original_principal_amount),
    statement_fy2024_product_mix = product_mix_string(.data$observed_product_id, .data$original_principal_amount),
    .groups = "drop"
  )

statement_country_summary <- statement_country_summary |>
  full_join(fy_statement_country_summary, by = c("creditor", "iso3", "country"))

write_csv_na(
  statement_country_summary,
  path_out("repayment_profile_wb_statement_country_term_summary_2026-07-01.csv")
)

calculate_statement_term_pvr <- function(creditor, product_id, maturity_years, grace_years, official_rate, discount_rate) {
  if (creditor == "IDA" && !is.na(product_id) && product_id %in% ida_product_specs$product_id) {
    return(calculate_ida_product_pvr(product_id, official_rate, discount_rate))
  }
  calculate_variant_pvr(
    annual_rate_percent = official_rate,
    maturity_years = maturity_years,
    grace_years = grace_years,
    discount_rate_percent = discount_rate,
    schedule_type = "equal_principal",
    payments_per_year = 1
  )
}

statement_mix_detail <- source_rows |>
  filter(.data$creditor %in% c("IBRD", "IDA")) |>
  inner_join(
    statement_terms_all |>
      filter(
        .data$approval_window_calendar_2024,
        .data$instrument_in_scope,
        .data$usable_repayment_dates,
        .data$usable_amount,
        !is.na(.data$iso3)
      ) |>
      select(
        creditor, iso3, instrument_id, original_principal_amount,
        observed_maturity_years, observed_grace_years, observed_product_id
      ),
    by = c("creditor", "iso3")
  ) |>
  rowwise() |>
  mutate(
    instrument_scenario_pvr = calculate_statement_term_pvr(
      creditor = .data$creditor,
      product_id = .data$observed_product_id,
      maturity_years = .data$observed_maturity_years,
      grace_years = .data$observed_grace_years,
      official_rate = .data$official_rate,
      discount_rate = .data$discount_rate
    )
  ) |>
  ungroup() |>
  group_by(.data$row_id) |>
  summarise(
    statement_instrument_count = n(),
    statement_total_original_principal = sum(.data$original_principal_amount, na.rm = TRUE),
    statement_weighted_maturity_years = safe_weighted_mean(.data$observed_maturity_years, .data$original_principal_amount),
    statement_weighted_grace_years = safe_weighted_mean(.data$observed_grace_years, .data$original_principal_amount),
    scenario_pvr = safe_weighted_mean(.data$instrument_scenario_pvr, .data$original_principal_amount),
    .groups = "drop"
  ) |>
  inner_join(source_rows, by = "row_id") |>
  mutate(
    scenario_id = "wb_statement_calendar2024_term_mix_with_ids_rate",
    scenario_label = "World Bank statement calendar-2024 observed repayment-date mix, IDS official rate",
    scenario_role = "source_mapped_repayment_term_sensitivity",
    source_basis = "World Bank Finances latest IBRD/IDA snapshots; calendar-2024 approvals; rate held at IDS row value",
    scenario_family = "world_bank_statement_terms"
  ) |>
  select(
    row_id, iso3, country, analysis_year, income_level, lending_type, creditor,
    creditor_name, official_rate, official_maturity_years, official_grace_years,
    discount_rate, market_rate_source_class, selected_source_class,
    selected_reliability_label, p13_display_class, included_in_lmic_reporting_scope,
    core_creditor, pvr_reported, base_pvr_unrounded, scenario_pvr, scenario_id,
    scenario_label, scenario_role, source_basis, scenario_family,
    statement_instrument_count, statement_total_original_principal,
    statement_weighted_maturity_years, statement_weighted_grace_years
  )

sensitivity_detail <- bind_rows(
  generic_detail,
  ida_product_detail,
  ida_q3_rate_detail,
  ida_heuristic_detail,
  ibrd_product_detail,
  statement_mix_detail
) |>
  mutate(
    delta_pvr = .data$scenario_pvr - .data$base_pvr_unrounded,
    abs_delta_pvr = abs(.data$delta_pvr),
    materiality_bucket = case_when(
      is.na(.data$abs_delta_pvr) ~ "missing",
      .data$abs_delta_pvr < 1 ~ "<1 pp",
      .data$abs_delta_pvr < 5 ~ "1-5 pp",
      .data$abs_delta_pvr < 10 ~ "5-10 pp",
      TRUE ~ ">=10 pp"
    ),
    crosses_market_parity_100 = (.data$base_pvr_unrounded < 100 & .data$scenario_pvr >= 100) |
      (.data$base_pvr_unrounded >= 100 & .data$scenario_pvr < 100),
    generated_at = generated_at
  )

write_csv_na(
  sensitivity_detail,
  path_out("repayment_profile_lockin_gate_sensitivity_detail_2026-07-01.csv")
)

summary_quantiles <- function(x, prob) {
  if (all(is.na(x))) return(NA_real_)
  as.numeric(stats::quantile(x, probs = prob, na.rm = TRUE, names = FALSE))
}

scenario_summary <- sensitivity_detail |>
  group_by(.data$scenario_id, .data$scenario_label, .data$scenario_role, .data$scenario_family, .data$source_basis) |>
  summarise(
    row_count = n(),
    core_row_count = sum(.data$core_creditor, na.rm = TRUE),
    country_count = n_distinct(.data$iso3),
    creditor_count = n_distinct(.data$creditor),
    baseline_mean_pvr = mean(.data$base_pvr_unrounded, na.rm = TRUE),
    scenario_mean_pvr = mean(.data$scenario_pvr, na.rm = TRUE),
    mean_delta_pvr = mean(.data$delta_pvr, na.rm = TRUE),
    median_delta_pvr = median(.data$delta_pvr, na.rm = TRUE),
    p10_delta_pvr = summary_quantiles(.data$delta_pvr, 0.10),
    p90_delta_pvr = summary_quantiles(.data$delta_pvr, 0.90),
    median_abs_delta_pvr = median(.data$abs_delta_pvr, na.rm = TRUE),
    p90_abs_delta_pvr = summary_quantiles(.data$abs_delta_pvr, 0.90),
    max_abs_delta_pvr = max(.data$abs_delta_pvr, na.rm = TRUE),
    rows_abs_delta_ge_1pp = sum(.data$abs_delta_pvr >= 1, na.rm = TRUE),
    rows_abs_delta_ge_5pp = sum(.data$abs_delta_pvr >= 5, na.rm = TRUE),
    rows_abs_delta_ge_10pp = sum(.data$abs_delta_pvr >= 10, na.rm = TRUE),
    rows_crossing_100 = sum(.data$crosses_market_parity_100, na.rm = TRUE),
    .groups = "drop"
  ) |>
  arrange(desc(.data$p90_abs_delta_pvr), desc(.data$median_abs_delta_pvr), .data$scenario_id)

write_csv_na(
  scenario_summary,
  path_out("repayment_profile_lockin_gate_scenario_summary_2026-07-01.csv")
)

creditor_summary <- sensitivity_detail |>
  group_by(.data$scenario_id, .data$scenario_role, .data$scenario_family, .data$creditor) |>
  summarise(
    row_count = n(),
    country_count = n_distinct(.data$iso3),
    baseline_mean_pvr = mean(.data$base_pvr_unrounded, na.rm = TRUE),
    scenario_mean_pvr = mean(.data$scenario_pvr, na.rm = TRUE),
    median_delta_pvr = median(.data$delta_pvr, na.rm = TRUE),
    p90_abs_delta_pvr = summary_quantiles(.data$abs_delta_pvr, 0.90),
    max_abs_delta_pvr = max(.data$abs_delta_pvr, na.rm = TRUE),
    rows_crossing_100 = sum(.data$crosses_market_parity_100, na.rm = TRUE),
    .groups = "drop"
  ) |>
  arrange(.data$creditor, desc(.data$p90_abs_delta_pvr), .data$scenario_id)

write_csv_na(
  creditor_summary,
  path_out("repayment_profile_lockin_gate_creditor_summary_2026-07-01.csv")
)

materiality_summary <- sensitivity_detail |>
  count(.data$scenario_id, .data$scenario_role, .data$scenario_family, .data$materiality_bucket, name = "row_count") |>
  group_by(.data$scenario_id) |>
  mutate(row_share = .data$row_count / sum(.data$row_count)) |>
  ungroup()

write_csv_na(
  materiality_summary,
  path_out("repayment_profile_lockin_gate_materiality_summary_2026-07-01.csv")
)

generic_rank_input <- sensitivity_detail |>
  filter(.data$core_creditor, .data$scenario_family == "generic_formula_sensitivity") |>
  group_by(.data$scenario_id, .data$scenario_label, .data$country) |>
  filter(n_distinct(.data$creditor) >= 2) |>
  summarise(
    creditor_count = n_distinct(.data$creditor),
    top_creditor = .data$creditor[which.min(.data$scenario_pvr)],
    lowest_pvr = min(.data$scenario_pvr, na.rm = TRUE),
    highest_pvr = max(.data$scenario_pvr, na.rm = TRUE),
    .groups = "drop"
  )

baseline_rank <- generic_rank_input |>
  filter(.data$scenario_id == "current_code_recomputed") |>
  select(country, baseline_top_creditor = top_creditor, baseline_lowest_pvr = lowest_pvr)

core_ranking_stability <- generic_rank_input |>
  left_join(baseline_rank, by = "country") |>
  mutate(top_creditor_changed = .data$top_creditor != .data$baseline_top_creditor) |>
  group_by(.data$scenario_id, .data$scenario_label) |>
  summarise(
    comparable_country_count = n(),
    top_creditor_changed_count = sum(.data$top_creditor_changed, na.rm = TRUE),
    top_creditor_changed_share = .data$top_creditor_changed_count / .data$comparable_country_count,
    countries_with_top_change = paste(.data$country[.data$top_creditor_changed], collapse = "; "),
    .groups = "drop"
  ) |>
  arrange(desc(.data$top_creditor_changed_count), .data$scenario_id)

write_csv_na(
  core_ranking_stability,
  path_out("repayment_profile_lockin_gate_core_ranking_stability_2026-07-01.csv")
)

ida_product_mapping <- ida_heuristic_map |>
  left_join(
    statement_country_summary |> filter(.data$creditor == "IDA") |> select(-creditor),
    by = c("iso3", "country")
  ) |>
  transmute(
    iso3, country, lending_type,
    ids_official_rate = official_rate,
    ids_maturity_years = official_maturity_years,
    ids_grace_years = official_grace_years,
    heuristic_ida_product,
    heuristic_product_label,
    heuristic_basis,
    heuristic_product_rate_q3_2024_usd,
    statement_calendar_2024_instrument_count,
    statement_calendar_2024_total_original_principal,
    statement_calendar_2024_weighted_maturity_years,
    statement_calendar_2024_weighted_grace_years,
    statement_calendar_2024_product_mix,
    statement_fy2024_instrument_count,
    statement_fy2024_total_original_principal,
    statement_fy2024_weighted_maturity_years,
    statement_fy2024_weighted_grace_years,
    statement_fy2024_product_mix
  )

write_csv_na(
  ida_product_mapping,
  path_out("repayment_profile_ida_product_mapping_2026-07-01.csv")
)

scenario_manifest <- bind_rows(
  generic_scenarios |>
    transmute(
      scenario_id, scenario_label, scenario_role,
      scenario_family = "generic_formula_sensitivity",
      source_basis,
      applies_to_creditor,
      calculation_mode,
      schedule_type,
      payments_per_year,
      horizon_code,
      disbursement_profile_name
    ),
  tibble(
    scenario_id = unique(c(ida_product_detail$scenario_id, ida_q3_rate_detail$scenario_id, ida_heuristic_detail$scenario_id)),
    scenario_label = unique(c(ida_product_detail$scenario_label, ida_q3_rate_detail$scenario_label, ida_heuristic_detail$scenario_label))[seq_along(unique(c(ida_product_detail$scenario_id, ida_q3_rate_detail$scenario_id, ida_heuristic_detail$scenario_id)))],
    scenario_role = "ida_product_term_sensitivity",
    scenario_family = "ida_product_terms",
    source_basis = "IDA 2024 term sheets",
    applies_to_creditor = "IDA",
    calculation_mode = "ida_product_schedule",
    schedule_type = "step_principal",
    payments_per_year = 1L,
    horizon_code = "none",
    disbursement_profile_name = "uniform"
  ),
  ibrd_term_specs |>
    transmute(
      scenario_id, scenario_label,
      scenario_role = "ibrd_product_term_sensitivity",
      scenario_family = "ibrd_product_terms",
      source_basis = "IBRD Flexible Loan product note",
      applies_to_creditor = "IBRD",
      calculation_mode = "ibrd_stylized_product_terms",
      schedule_type = "equal_principal",
      payments_per_year = 1L,
      horizon_code = "none",
      disbursement_profile_name = "uniform"
    ),
  tibble(
    scenario_id = "wb_statement_calendar2024_term_mix_with_ids_rate",
    scenario_label = "World Bank statement calendar-2024 observed repayment-date mix, IDS official rate",
    scenario_role = "source_mapped_repayment_term_sensitivity",
    scenario_family = "world_bank_statement_terms",
    source_basis = "World Bank Finances latest IBRD/IDA snapshots; calendar-2024 approvals",
    applies_to_creditor = "IBRD; IDA",
    calculation_mode = "observed_first_last_repayment_dates",
    schedule_type = "source_mapped_terms",
    payments_per_year = 1L,
    horizon_code = "none",
    disbursement_profile_name = "uniform"
  )
)

write_csv_na(
  scenario_manifest,
  path_out("repayment_profile_lockin_gate_scenario_manifest_2026-07-01.csv")
)

top_scenarios <- scenario_summary |>
  filter(.data$scenario_id != "current_code_recomputed") |>
  slice_head(n = 12)

top_sensitivity_plot <- ggplot(
  top_scenarios,
  aes(x = reorder(.data$scenario_id, .data$p90_abs_delta_pvr), y = .data$p90_abs_delta_pvr)
) +
  geom_col(fill = "#4B6F82", width = 0.72) +
  coord_flip() +
  labs(
    title = "Largest repayment-profile sensitivity movements",
    subtitle = "P90 absolute PVR-point movement versus the current annual equal-principal baseline",
    x = NULL,
    y = "P90 absolute movement, PVR points",
    caption = paste0("Generated ", analysis_date, ". Larger values mean the scenario materially changes measured concessionality.")
  ) +
  theme_repayment()

ggsave(
  path_out("repayment_profile_lockin_gate_top_sensitivity_scenarios_2026-07-01.png"),
  plot = top_sensitivity_plot,
  width = 9,
  height = 6,
  dpi = 220
)

core_plot_data <- sensitivity_detail |>
  filter(
    .data$core_creditor,
    .data$scenario_id %in% c(
      "equal_principal_semiannual",
      "annuity_semiannual",
      "bullet_annual_extreme",
      "denom_uniform_5y",
      "tranched_uniform_5y_annual",
      "wb_statement_calendar2024_term_mix_with_ids_rate",
      "ida_heuristic_product_terms_with_ids_rate",
      "ibrd_product_35y_final_5y_grace_with_ids_rate"
    )
  )

core_delta_plot <- ggplot(core_plot_data, aes(x = .data$creditor, y = .data$delta_pvr)) +
  geom_hline(yintercept = 0, color = "grey60", linewidth = 0.3) +
  geom_boxplot(outlier.alpha = 0.35, width = 0.62, fill = "#D8E3E7", color = "#284B5A") +
  facet_wrap(~scenario_id, scales = "free_y") +
  labs(
    title = "Core-creditor PVR movements under selected scenarios",
    subtitle = "Positive values raise the measured present-value cost; negative values lower it",
    x = NULL,
    y = "Change from current baseline, PVR points",
    caption = paste0("Generated ", analysis_date, ". Source-mapped World Bank statement scenario is available only where 2024 loan/credit records match P13 countries.")
  ) +
  theme_repayment(base_size = 9)

ggsave(
  path_out("repayment_profile_lockin_gate_core_creditor_delta_boxplot_2026-07-01.png"),
  plot = core_delta_plot,
  width = 11,
  height = 7,
  dpi = 220
)

all_rows_n <- nrow(source_rows)
core_rows_n <- sum(source_rows$core_creditor)
included_country_n <- n_distinct(source_rows$iso3)
statement_rows_n <- nrow(statement_mix_detail)
statement_countries_n <- n_distinct(statement_mix_detail$iso3)
generic_small <- scenario_summary |>
  filter(.data$scenario_id %in% c("equal_principal_semiannual", "annuity_semiannual", "equal_principal_quarterly")) |>
  summarise(max_p90_abs = max(.data$p90_abs_delta_pvr, na.rm = TRUE), max_cross = max(.data$rows_crossing_100, na.rm = TRUE))
disbursement_large <- scenario_summary |>
  filter(.data$scenario_role %in% c("disbursement_denominator_sensitivity", "disbursement_profile_sensitivity")) |>
  summarise(max_p90_abs = max(.data$p90_abs_delta_pvr, na.rm = TRUE), max_cross = max(.data$rows_crossing_100, na.rm = TRUE))
tranched_full <- scenario_summary |>
  filter(.data$scenario_role == "tranched_disbursement_sensitivity") |>
  summarise(max_p90_abs = max(.data$p90_abs_delta_pvr, na.rm = TRUE), max_cross = max(.data$rows_crossing_100, na.rm = TRUE))
product_large <- scenario_summary |>
  filter(.data$scenario_family %in% c("ida_product_terms", "ibrd_product_terms", "world_bank_statement_terms")) |>
  summarise(max_p90_abs = max(.data$p90_abs_delta_pvr, na.rm = TRUE), max_cross = max(.data$rows_crossing_100, na.rm = TRUE))
ida_heuristic_ids <- scenario_summary |>
  filter(.data$scenario_id == "ida_heuristic_product_terms_with_ids_rate")
wb_statement_summary <- scenario_summary |>
  filter(.data$scenario_id == "wb_statement_calendar2024_term_mix_with_ids_rate")
rank_changes <- core_ranking_stability |>
  filter(.data$scenario_id != "current_code_recomputed") |>
  summarise(max_changed = max(.data$top_creditor_changed_count, na.rm = TRUE))

memo_lines <- c(
  "# Repayment Profile Lock-In Gate Sensitivity, 2026-07-01",
  "",
  "## Question",
  "",
  "Should the current repayment-profile formula be locked in now, or does the workstream need more source-mapped testing before a project-level decision?",
  "",
  "## Inputs Used",
  "",
  paste0("- P13 included non-bond official rows tested: ", all_rows_n, " rows across ", included_country_n, " countries."),
  paste0("- Core IBRD/IDA/China rows inside that set: ", core_rows_n, " rows."),
  "- World Bank latest public snapshots saved locally: IBRD Statement of Loans latest snapshot and IDA Statement of Credits, Grants and Guarantees latest snapshot.",
  "- IDA 2024 quarterly term sheets and IBRD product notes were used only for diagnostic product-term scenarios.",
  "",
  "## Main Quantified Findings",
  "",
  paste0("- Simple payment-frequency and annuity-shape changes are modest: the largest P90 absolute movement among the semiannual/quarterly equal-principal and annuity checks is ", sprintf('%.2f', generic_small$max_p90_abs), " PVR points."),
  paste0("- Denominator-only delayed-disbursement tests are large: the largest P90 absolute movement is ", sprintf('%.2f', disbursement_large$max_p90_abs), " PVR points, with up to ", disbursement_large$max_cross, " rows crossing the 100 benchmark line."),
  paste0("- A fuller tranche approximation is much smaller because later disbursement also delays repayment: the largest P90 absolute movement among tranche scenarios is ", sprintf('%.2f', tranched_full$max_p90_abs), " PVR points."),
  paste0("- Product-term tests can be material. The broad product stress maximum is ", sprintf('%.2f', product_large$max_p90_abs), " PVR points; the more targeted IDA heuristic using IDS rates has P90 absolute movement of ", sprintf('%.2f', ida_heuristic_ids$p90_abs_delta_pvr), " PVR points."),
  paste0("- The source-mapped World Bank statement-term scenario is smaller but still useful as validation: P90 absolute movement is ", sprintf('%.2f', wb_statement_summary$p90_abs_delta_pvr), " PVR points, with a maximum row movement of ", sprintf('%.2f', wb_statement_summary$max_abs_delta_pvr), " PVR points."),
  paste0("- World Bank statement-term scenarios matched ", statement_rows_n, " P13 IBRD/IDA row-scenarios across ", statement_countries_n, " countries for calendar-2024 approvals."),
  paste0("- The maximum number of core-country top-creditor changes under generic scenarios is ", rank_changes$max_changed, "."),
  "",
  "## Research Judgment",
  "",
  "Do not lock the current formula as a final contractual repayment-profile method yet.",
  "",
  "The current formula is defensible as a clearly labelled Phase 1 baseline because it is public, scalable, and close to the existing IDS/ONE-style commitment-basis design. But the sensitivity gate shows that the economically important uncertainty is not semiannual versus annual payment frequency. The larger questions are how the paper labels the commitment-basis denominator, and whether IDA/IBRD product-specific repayment terms should accompany or replace the generic equal-principal approximation where source support exists.",
  "",
  "Recommended status: keep the current formula as the working baseline, require the disbursement and source-mapped product sensitivities beside it for any paper-facing claim, and do not promote a final method decision to docs/DECISIONS.md until the team chooses whether Phase 1 is explicitly commitment-basis or should incorporate source-specific schedules where available.",
  "",
  "## Output Files",
  "",
  "- `repayment_profile_lockin_gate_sensitivity_detail_2026-07-01.csv`",
  "- `repayment_profile_lockin_gate_scenario_summary_2026-07-01.csv`",
  "- `repayment_profile_lockin_gate_creditor_summary_2026-07-01.csv`",
  "- `repayment_profile_lockin_gate_materiality_summary_2026-07-01.csv`",
  "- `repayment_profile_lockin_gate_core_ranking_stability_2026-07-01.csv`",
  "- `repayment_profile_ida_product_mapping_2026-07-01.csv`",
  "- `repayment_profile_wb_statement_terms_probe_2026-07-01.csv`",
  "- `repayment_profile_wb_statement_country_term_summary_2026-07-01.csv`",
  "- `repayment_profile_lockin_gate_scenario_manifest_2026-07-01.csv`",
  "",
  "## Caveats",
  "",
  "- The World Bank latest snapshots are current 2026 snapshots, not frozen end-2024 snapshots. They are source-mapped diagnostics, not yet canonical raw inputs.",
  "- The statement snapshots expose first and last repayment dates but not every repayment installment, floating-rate reset, fee capitalization choice, or disbursement schedule.",
  "- IDA statement service-charge fields are not used as total borrower rates because 2024 IDA product terms can include product-specific charges that are not fully represented by a simple service-charge field.",
  "- China official lending remains outside this source-mapped World Bank product-term check; China rows are covered by generic repayment/disbursement sensitivities only."
)

writeLines(
  memo_lines,
  path_out("repayment_profile_lockin_gate_memo_2026-07-01.md")
)

artifact_manifest <- tibble::tribble(
  ~artifact, ~description,
  "repayment_profile_lockin_gate_sensitivity_detail_2026-07-01.csv", "Row-scenario PVR values and deltas for the full lock-in sensitivity gate",
  "repayment_profile_lockin_gate_scenario_summary_2026-07-01.csv", "Scenario-level quantified sensitivity summary",
  "repayment_profile_lockin_gate_creditor_summary_2026-07-01.csv", "Creditor-by-scenario quantified sensitivity summary",
  "repayment_profile_lockin_gate_materiality_summary_2026-07-01.csv", "Materiality buckets by scenario",
  "repayment_profile_lockin_gate_core_ranking_stability_2026-07-01.csv", "Core-creditor ranking stability under generic scenarios",
  "repayment_profile_ida_product_mapping_2026-07-01.csv", "IDA product-term mapping and observed statement-term cross-check",
  "repayment_profile_wb_statement_terms_probe_2026-07-01.csv", "Loan/credit-level World Bank statement probe for 2024 windows",
  "repayment_profile_wb_statement_country_term_summary_2026-07-01.csv", "Country-level IBRD/IDA statement term summary",
  "repayment_profile_lockin_gate_scenario_manifest_2026-07-01.csv", "Scenario definitions and source basis",
  "repayment_profile_lockin_gate_top_sensitivity_scenarios_2026-07-01.png", "Chart of largest scenario movements",
  "repayment_profile_lockin_gate_core_creditor_delta_boxplot_2026-07-01.png", "Core-creditor delta boxplot for selected scenarios",
  "repayment_profile_lockin_gate_memo_2026-07-01.md", "Plain-language lock-in recommendation memo"
) |>
  mutate(generated_at = generated_at)

write_csv_na(
  artifact_manifest,
  path_out("repayment_profile_lockin_gate_artifact_manifest_2026-07-01.csv")
)

message("Wrote repayment-profile lock-in gate outputs to ", output_dir)
