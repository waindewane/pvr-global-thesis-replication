# Experimental scorecard components. This file does not change production data.
# The matrices are fully transcribed from ESM WP27 (March 2018), printed p28,
# which analyzes Moody's September 2013 methodology. See methodology/README.md.
# Do not describe this module as a complete November 2018 Moody's reconstruction.

.scorecard_source_file <- tryCatch(normalizePath(sys.frame(1)$ofile),
                                 error = function(e) NA_character_)
.scorecard_default_dir <- if (!is.na(.scorecard_source_file)) {
  file.path(dirname(.scorecard_source_file), "methodology")
} else file.path("experiments", "p15_wb_scorecard_20260912", "methodology")

scorecard_strength_labels <- c("VH+", "VH", "VH-", "H+", "H", "H-", "M+", "M", "M-",
                              "L+", "L", "L-", "VL+", "VL", "VL-")
scorecard_rating_labels <- c("Aaa", "Aa1", "Aa2", "Aa3", "A1", "A2", "A3", "Baa1", "Baa2",
                            "Baa3", "Ba1", "Ba2", "Ba3", "B1", "B2", "B3", "Caa1", "Caa2",
                            "Caa3", "Ca", "C")

scorecard_load_matrices <- function(path = .scorecard_default_dir) {
  read_matrix <- function(name) {
    d <- read.csv(file.path(path, paste0("esm2013_", name, ".csv")), check.names = FALSE,
                  stringsAsFactors = FALSE)
    a <- as.matrix(d[-1L]); rownames(a) <- d[[1L]]
    stopifnot(nrow(a) == 15L, ncol(a) == 15L,
              identical(colnames(a), scorecard_strength_labels),
              setequal(rownames(a), scorecard_strength_labels))
    a
  }
  list(economic_resiliency = read_matrix("economic_resiliency"),
       government_financial_strength = read_matrix("government_financial_strength"),
       rating_midpoint = read_matrix("rating_midpoint"),
       version = "esm2013_historical_matrices_2018_parity_not_fully_verified")
}

.scorecard_check_labels <- function(x, name) {
  if (any(!is.na(x) & !x %in% scorecard_strength_labels))
    stop(name, " must contain a published factor label (VH+ through VL-) or NA")
  as.character(x)
}

.scorecard_recycle <- function(...) {
  x <- list(...); n <- max(lengths(x))
  if (any(!lengths(x) %in% c(1L, n))) stop("Input lengths must be one or the common row count")
  lapply(x, rep_len, length.out = n)
}

# Factor scores are supplied labels, not inferred from macro values here.
# Higher factor strength is better; higher event-risk susceptibility is worse.
scorecard_aggregate <- function(f1, f2, f3, f4 = "M", matrices = scorecard_load_matrices()) {
  args <- .scorecard_recycle(f1, f2, f3, f4)
  args <- Map(.scorecard_check_labels, args, c("f1", "f2", "f3", "f4"))
  f1 <- args[[1L]]; f2 <- args[[2L]]; f3 <- args[[3L]]; f4 <- args[[4L]]
  get_cells <- function(a, rows, cols) a[cbind(match(rows, rownames(a)), match(cols, colnames(a)))]
  er <- get_cells(matrices$economic_resiliency, f2, f1)
  gfs <- get_cells(matrices$government_financial_strength, er, f3)
  mid <- get_cells(matrices$rating_midpoint, f4, gfs)
  rank <- match(mid, scorecard_rating_labels)
  lo <- pmax(1L, rank - 1L); hi <- pmin(length(scorecard_rating_labels), rank + 1L)
  data.frame(f1, f2, f3, f4, economic_resiliency = er, government_financial_strength = gfs,
             rating_midpoint = mid, rating_notch = rank,
             rating_range_better = scorecard_rating_labels[lo],
             rating_range_worse = scorecard_rating_labels[hi],
             matrix_version = matrices$version, stringsAsFactors = FALSE)
}

# Full enumeration certifies extrema; it does not assume unexplored matrices are
# monotonic. Strength bounds are numeric positions 1=VH+, 15=VL-. Event risk
# bounds are 1=VL-, 15=VH+, so all four dimensions share 'larger means worse'.
# Use this with externally justified factor bounds; it cannot infer missing raw
# indicator thresholds or recover precise ratings from entirely unknown factors.
scorecard_factor_bounds <- function(f1 = c(1L, 15L), f2 = c(1L, 15L),
                                    f3 = c(1L, 15L), event_risk = c(8L, 8L),
                                    matrices = scorecard_load_matrices()) {
  intervals <- list(f1, f2, f3, event_risk)
  valid <- vapply(intervals, function(x) length(x) == 2L && all(is.finite(x)) &&
                    all(x == as.integer(x)) && x[1] <= x[2] && x[1] >= 1 && x[2] <= 15,
                  logical(1))
  if (!all(valid)) stop("Each factor bound must be two ordered integers within 1..15")
  g <- expand.grid(lapply(intervals, function(x) seq.int(x[1], x[2])))
  z <- scorecard_aggregate(scorecard_strength_labels[g[[1]]], scorecard_strength_labels[g[[2]]],
                          scorecard_strength_labels[g[[3]]], rev(scorecard_strength_labels)[g[[4]]],
                          matrices)
  best <- min(z$rating_notch); worst <- max(z$rating_notch)
  data.frame(best_midpoint = scorecard_rating_labels[best], worst_midpoint = scorecard_rating_labels[worst],
             best_notch = best, worst_notch = worst, interval_width_notches = worst - best,
             distinct_midpoints = length(unique(z$rating_notch)), combinations = nrow(z),
             matrix_version = matrices$version)
}

# A rules bundle is a versioned source-backed lookup supplied explicitly. No
# intermediate indicator cutoff, internal weight or penalty has been guessed.
# Each bin is [lower, upper), except an explicitly closed upper endpoint.
scorecard_bin <- function(x, bins) {
  required <- c("lower", "upper", "lower_closed", "upper_closed", "score", "source", "verified")
  if (!all(required %in% names(bins))) stop("Incomplete bin schema")
  if (any(!bins$verified) || anyNA(bins$verified) || anyNA(bins$source) || any(!nzchar(bins$source)))
    stop("Bins must have explicit verified source provenance")
  if (any(bins$lower > bins$upper) || any(!bins$score %in% 1:15)) stop("Invalid bin bounds or scores")
  vapply(x, function(value) {
    if (is.na(value)) return(NA_integer_)
    hit <- (value > bins$lower | (bins$lower_closed & value == bins$lower)) &
      (value < bins$upper | (bins$upper_closed & value == bins$upper))
    if (sum(hit) > 1L) stop("Overlapping bins at input value ", value)
    if (!any(hit)) return(NA_integer_)
    as.integer(bins$score[hit])
  }, integer(1))
}

# Fail closed until the exact necessary 2018 tables become available. This
# prevents a data-ready row being mislabeled as a completed shadow estimate.
scorecard_2018_readiness <- function(inputs) {
  required <- c("growth_mean", "growth_sd", "wef_gci", "gdp_usd_bn", "gdppc_ppp",
                "inflation_mean", "inflation_sd", "debt_gdp", "debt_revenue",
                "interest_revenue", "interest_gdp", "debt_trend", "fc_debt_share")
  absent <- setdiff(required, names(inputs))
  ready_data <- if (length(absent)) rep(FALSE, nrow(inputs)) else
    apply(inputs[required], 1L, function(x) all(is.finite(as.numeric(x))))
  data.frame(input_metrics_complete = ready_data, scorecard_rules_complete = FALSE,
             rating_notch = NA_integer_,
             reason = paste("Unverified November 2018 full indicator bins, internal weights,",
                            "fiscal-weight variation, rounding, and automatic-adjustment tables"),
             stringsAsFactors = FALSE)
}

# Explicit integration contract for a later verified full source. Callbacks have
# no defaults because their numerical definitions are the missing source rules.
scorecard_empty_2018_bundle <- function() {
  components <- c("economic_indicator_bins", "institutional_indicator_bins",
                  "fiscal_indicator_bins", "indicator_numeric_mapping",
                  "economic_metric_weights", "institutional_metric_weights",
                  "fiscal_metric_weights_and_conditions", "factor_rounding_and_endpoints",
                  "default_trigger_lookback_and_penalty", "debt_trend_penalty",
                  "foreign_currency_debt_penalty_and_exceptions", "adjustment_order_and_caps",
                  "economic_resiliency_matrix", "government_financial_strength_matrix",
                  "event_risk_to_rating_matrix", "rating_range_mapping")
  list(method_id = "PBC_1151027", method_date = "2018-11-27",
       scenario = "wb2021_inflation_only_event_M", complete = FALSE,
       required_inputs = c("growth_mean", "growth_sd", "wef_gci", "gdp_usd_bn", "gdppc_ppp",
                           "inflation_mean", "inflation_sd", "debt_gdp", "debt_revenue",
                           "interest_revenue", "interest_gdp", "debt_trend", "fc_debt_share"),
       evidence = data.frame(component = components, verified = FALSE,
                             source_url = NA_character_, source_sha256 = NA_character_,
                             source_page = NA_character_, implementation_ref = NA_character_),
       # score_indicators(inputs): numeric data.frame, one row per input and
       # named columns matching the indicators used by the factor weight tables.
       score_indicators = NULL,
       # resolve_weights(inputs, scores): list(f1, f2, f3), each a numeric matrix
       # nrow(inputs) x ncol(scores), column names identical to names(scores).
       # Each row sums to 1. This handles source-defined varying fiscal weights.
       resolve_weights = NULL,
       # assemble_factors(inputs, scores, weights): data.frame with f1,f2,f3,f4
       # published labels; also preserve numeric initial scores and every applied
       # adjustment in additional columns for the audit trail. Implements the
       # SOURCE-DEFINED rounding, adjustment order/caps and scenario exceptions.
       assemble_factors = NULL,
       matrices = NULL)
}

scorecard_bundle_gaps <- function(bundle) {
  expected <- scorecard_empty_2018_bundle()$evidence$component
  e <- bundle$evidence
  if (!is.data.frame(e) || !all(c("component", "verified", "source_url", "source_sha256",
                                "source_page", "implementation_ref") %in% names(e)))
    return("complete_evidence_schema")
  evidenced <- !is.na(e$verified) & e$verified & !is.na(e$source_url) & nzchar(e$source_url) &
    !is.na(e$source_sha256) & grepl("^[0-9a-f]{64}$", e$source_sha256) &
    !is.na(e$source_page) & nzchar(e$source_page) &
    !is.na(e$implementation_ref) & nzchar(e$implementation_ref)
  gaps <- setdiff(expected, e$component[evidenced])
  if (!identical(bundle$method_id, "PBC_1151027") || !identical(bundle$method_date, "2018-11-27"))
    gaps <- c(gaps, "exact_method_identity")
  if (!isTRUE(bundle$complete)) gaps <- c(gaps, "bundle_not_certified_complete")
  for (name in c("score_indicators", "resolve_weights", "assemble_factors"))
    if (!is.function(bundle[[name]])) gaps <- c(gaps, name)
  if (!is.list(bundle$matrices) || !all(c("economic_resiliency", "government_financial_strength",
                                        "rating_midpoint", "version") %in% names(bundle$matrices)))
    gaps <- c(gaps, "certified_2018_matrices")
  if (!is.character(bundle$required_inputs) || !length(bundle$required_inputs))
    gaps <- c(gaps, "required_inputs")
  unique(gaps)
}

# For incomplete bundles, explicitly missing outputs permit safe integration
# into peer selection while retaining each row and its missing-rule diagnosis.
# There is deliberately no switch that disables the evidence gate.
scorecard_score_2018 <- function(inputs, bundle = scorecard_empty_2018_bundle()) {
  stopifnot(is.data.frame(inputs))
  gaps <- scorecard_bundle_gaps(bundle)
  if (length(gaps)) return(data.frame(
    rating_midpoint = rep(NA_character_, nrow(inputs)), rating_notch = rep(NA_integer_, nrow(inputs)),
    scorecard_status = rep("rules_incomplete", nrow(inputs)),
    scorecard_missing_rules = rep(paste(gaps, collapse = ";"), nrow(inputs))))
  absent <- setdiff(bundle$required_inputs, names(inputs))
  if (length(absent)) stop("Required raw-input columns absent: ", paste(absent, collapse = ", "))
  scores <- bundle$score_indicators(inputs)
  if (!is.data.frame(scores) || nrow(scores) != nrow(inputs) ||
      !all(vapply(scores, is.numeric, logical(1))) || anyDuplicated(names(scores)))
    stop("Indicator scorer must return numeric columns, one row per input, and unique names")
  weights <- bundle$resolve_weights(inputs, scores)
  for (factor in c("f1", "f2", "f3")) {
    w <- weights[[factor]]
    if (!is.matrix(w) || !is.numeric(w) || !identical(dim(w), dim(as.matrix(scores))) ||
        !identical(colnames(w), names(scores)) || anyNA(w) || any(w < 0) ||
        any(abs(rowSums(w) - 1) > 1e-10)) stop("Invalid metric weights for ", factor)
  }
  factors <- bundle$assemble_factors(inputs, scores, weights)
  if (!is.data.frame(factors) || nrow(factors) != nrow(inputs) ||
      !all(c("f1", "f2", "f3", "f4") %in% names(factors)))
    stop("Factor assembler must return one row per input and f1,f2,f3,f4 labels")
  # Any missing indicator with positive weight makes that factor missing. This
  # invariant prevents a callback from silently replacing missing data with zero.
  for (factor in c("f1", "f2", "f3")) {
    bad <- rowSums(!is.finite(as.matrix(scores)) & weights[[factor]] > 0) > 0
    factors[[factor]][bad] <- NA_character_
  }
  out <- with(factors, scorecard_aggregate(f1, f2, f3, f4, bundle$matrices))
  complete_raw <- Reduce(`&`, lapply(inputs[bundle$required_inputs], function(x) {
    if (is.numeric(x)) is.finite(x) else !is.na(x) & nzchar(as.character(x))
  }))
  out[!complete_raw, c("rating_midpoint", "rating_notch", "rating_range_better", "rating_range_worse")] <- NA
  out$scorecard_status <- ifelse(is.na(out$rating_notch), "inputs_incomplete", "scored")
  out$scorecard_missing_rules <- ""
  extra <- setdiff(names(factors), names(out))
  if (length(extra)) out <- cbind(out, factors[extra])
  out
}
