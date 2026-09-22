# P15 secondary price-to-yield validation helpers.

p15_secondary_repair_schema_version <- function() {
  "SCHEMA-P15-SECONDARY-REPAIR-EVIDENCE-V1"
}

p15_recompute_secondary_price_semantics <- function(crosscheck_detail) {
  required <- c(
    "crosscheck_id", "evidence_surface", "iso3", "country", "ric",
    "quote_date", "maturity_date", "coupon_rate_pct",
    "coupon_frequency_assumption", "mid_price", "direct_ytm_pct",
    "calc_ytm_if_clean_price_pct", "abs_error_clean_bps",
    "strict_plainish_subset"
  )
  missing <- setdiff(required, names(crosscheck_detail))
  if (length(missing)) {
    stop(
      "Secondary price-semantics cross-check is missing: ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }

  out <- crosscheck_detail |>
    dplyr::mutate(
      quote_date_parsed = as.Date(.data$quote_date),
      maturity_date_parsed = as.Date(.data$maturity_date),
      p15_recomputed_clean_price_ytm_pct = mapply(
        p15_secondary_ytm_from_clean_price,
        as.numeric(.data$mid_price),
        .data$quote_date_parsed,
        .data$maturity_date_parsed,
        as.numeric(.data$coupon_rate_pct),
        as.numeric(.data$coupon_frequency_assumption)
      ),
      formula_reproduction_abs_diff_bps = abs(
        .data$p15_recomputed_clean_price_ytm_pct -
          as.numeric(.data$calc_ytm_if_clean_price_pct)
      ) * 100,
      direct_ytm_abs_error_recomputed_bps = abs(
        .data$p15_recomputed_clean_price_ytm_pct -
          as.numeric(.data$direct_ytm_pct)
      ) * 100,
      formula_reproduction_pass =
        .data$formula_reproduction_abs_diff_bps <= 1e-8,
      method_decision_state = "not_evaluated",
      secondary_repair_schema_version =
        p15_secondary_repair_schema_version()
    ) |>
    dplyr::select(-"quote_date_parsed", -"maturity_date_parsed")

  if (any(is.na(out$p15_recomputed_clean_price_ytm_pct))) {
    stop("P15 formula failed on a preserved cross-check row.", call. = FALSE)
  }
  if (!all(out$formula_reproduction_pass)) {
    stop("P15 price-to-yield formula drifted from preserved evidence.", call. = FALSE)
  }
  out
}

p15_secondary_repair_validation_summary <- function(formula_detail) {
  formula_detail |>
    dplyr::group_by(
      .data$evidence_surface,
      .data$coupon_frequency_assumption,
      .data$strict_plainish_subset
    ) |>
    dplyr::summarise(
      comparison_rows = dplyr::n(),
      unique_rics = dplyr::n_distinct(.data$ric),
      formula_reproduction_failures = sum(
        !.data$formula_reproduction_pass
      ),
      median_abs_error_clean_bps = stats::median(
        .data$direct_ytm_abs_error_recomputed_bps,
        na.rm = TRUE
      ),
      mean_abs_error_clean_bps = mean(
        .data$direct_ytm_abs_error_recomputed_bps,
        na.rm = TRUE
      ),
      p90_abs_error_clean_bps = as.numeric(stats::quantile(
        .data$direct_ytm_abs_error_recomputed_bps,
        0.9,
        na.rm = TRUE
      )),
      p95_abs_error_clean_bps = as.numeric(stats::quantile(
        .data$direct_ytm_abs_error_recomputed_bps,
        0.95,
        na.rm = TRUE
      )),
      direct_in_clean_bid_ask_bracket_share_5bps = mean(
        .data$direct_in_clean_bid_ask_bracket_5bps,
        na.rm = TRUE
      ),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      direct_in_clean_bid_ask_bracket_share_5bps = ifelse(
        is.nan(.data$direct_in_clean_bid_ask_bracket_share_5bps),
        NA_real_,
        .data$direct_in_clean_bid_ask_bracket_share_5bps
      ),
      validation_authority = "descriptive_same_instrument_crosscheck",
      repair_decision_state = "not_evaluated"
    )
}
