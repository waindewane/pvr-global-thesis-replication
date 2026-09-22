# Bounded retrospective check. This module never changes selected benchmarks.

p15_nested_candidates <- function(train, test, include_history = TRUE) {
  train <- as.data.frame(train)
  test <- as.data.frame(test)
  stopifnot(nrow(test) > 0L, all(train$analysis_year < min(test$analysis_year)))
  natural_history <- vapply(test$iso3, function(i) sum(train$iso3 == i), integer(1))
  fit_ok <- nrow(train) >= 10L && length(unique(train$iso3)) >= 3L &&
    length(unique(train$analysis_year)) >= 3L &&
    diff(range(train$rating_rate_pct)) > 1e-12
  pred_affine <- pred_history <- test$rating_rate_pct
  affine_state <- history_state <- "fallback_insufficient_support"
  intercept <- slope <- country_sd <- residual_sd <- shrinkage_k <- NA_real_
  warn <- character()
  guarded <- function(expr) tryCatch(withCallingHandlers(expr, warning = function(w) {
    warn <<- c(warn, conditionMessage(w)); invokeRestart("muffleWarning")
  }), error = function(e) e)
  if (fit_ok) {
    f <- guarded(lm(observed_rate_pct ~ rating_rate_pct, data = train))
    p <- if (inherits(f, "error")) f else guarded(as.numeric(predict(f, newdata = test)))
    if (!inherits(p, "error") && all(is.finite(p))) {
      pred_affine <- p
      affine_state <- "fitted"
      intercept <- unname(coef(f)[1]); slope <- unname(coef(f)[2])
    } else {
      affine_state <- "fallback_fit_failure"
      history_state <- "fallback_affine_fit_failure"
      if (inherits(p, "error")) warn <- c(warn, conditionMessage(p))
    }
    pred_history <- pred_affine
    if (include_history && affine_state == "fitted") {
      h <- guarded(p15_fit_rating_country_history(train, test))
      if (!inherits(h, "error") && all(is.finite(h$scored$history_prediction_pct))) {
        stopifnot(max(abs(h$scored$affine_prediction_pct - pred_affine)) < 1e-8)
        pred_history <- h$scored$history_prediction_pct
        country_sd <- h$country_sd; residual_sd <- h$residual_sd
        shrinkage_k <- h$shrinkage_k
        history_state <- "fitted"
      } else {
        history_state <- "fallback_fit_failure"
        if (inherits(h, "error")) warn <- c(warn, conditionMessage(h))
      }
    }
  }
  values <- list(raw = test$rating_rate_pct, affine = pred_affine)
  states <- c(raw = "original", affine = affine_state)
  if (include_history) {
    values$history <- pred_history
    states <- c(states, history = history_state)
  }
  scored <- data.table::rbindlist(lapply(names(values), function(m) {
    data.table::data.table(test, model = m, prediction_pct = values[[m]],
      fit_state = states[[m]], natural_history_rows = natural_history)
  }))
  scored[, `:=`(error_pp = prediction_pct - observed_rate_pct,
    absolute_error_pp = abs(prediction_pct - observed_rate_pct),
    squared_error_pp2 = (prediction_pct - observed_rate_pct)^2)]
  list(scored = scored, audit = data.table::data.table(
    training_rows = nrow(train), training_countries = length(unique(train$iso3)),
    training_years = length(unique(train$analysis_year)),
    training_first_year = min(train$analysis_year),
    training_last_year = max(train$analysis_year),
    test_year = unique(test$analysis_year),
    training_country_ids = paste(sort(unique(train$iso3)), collapse = ";"),
    test_country_ids = paste(sort(unique(test$iso3)), collapse = ";"),
    training_keys = paste(paste(train$iso3, train$analysis_year, sep = "_"), collapse = ";"),
    affine_state = affine_state, history_state = if (include_history) history_state else "not_fitted",
    affine_intercept = intercept, affine_slope = slope,
    country_sd = country_sd, residual_sd = residual_sd, shrinkage_k = shrinkage_k,
    warnings = paste(unique(warn), collapse = " | ")))
}

p15_nested_select <- function(inner) {
  s <- inner[, .(inner_rows = .N, inner_mae_pp = mean(absolute_error_pp)), by = model]
  stopifnot(length(unique(s$inner_rows)) == 1L)
  s[, complexity := match(model, c("raw", "affine", "history"))]
  best <- s[inner_mae_pp <= min(inner_mae_pp) + 1e-10][which.min(complexity), model]
  s[, selected := model == best]
  list(model = best, scores = s)
}

p15_run_rating_nested_check <- function(sample, outer_years = 2018:2024,
                                        outer_countries = NULL, quiet = FALSE) {
  sample <- data.table::copy(data.table::as.data.table(sample))
  data.table::setorder(sample, analysis_year, iso3)
  stopifnot(!anyDuplicated(sample[, .(iso3, analysis_year)]))
  inners <- outers <- choices <- audits <- list()
  k <- 0L
  run_fit <- function(train, test, history, branch, phase, outer_year, held_country) {
    fit <- p15_nested_candidates(train, test, history)
    fit$audit[, `:=`(branch = branch, phase = phase, outer_year = outer_year,
      held_outer_country = held_country)]
    audits[[length(audits) + 1L]] <<- fit$audit
    fit$scored
  }
  for (outer_year in outer_years) {
    test_all <- sample[analysis_year == outer_year]
    if (!is.null(outer_countries)) test_all <- test_all[iso3 %in% outer_countries]
    if (!nrow(test_all)) next
    for (branch_name in c("temporal", "country_history_withheld")) {
      held_countries <- if (branch_name == "temporal") "" else sort(test_all$iso3)
      for (held_country in held_countries) {
        prior <- sample[analysis_year < outer_year & iso3 != held_country]
        inner_list <- list()
        for (inner_year in 2015:(outer_year - 1L)) {
          inner_test <- prior[analysis_year == inner_year]
          inner_train <- prior[analysis_year < inner_year]
          if (!nrow(inner_test)) next
          if (branch_name == "temporal") {
            inner_list[[length(inner_list) + 1L]] <- run_fit(inner_train, inner_test,
              TRUE, branch_name, "inner", outer_year, held_country)
          } else {
            for (inner_country in sort(inner_test$iso3)) {
              inner_list[[length(inner_list) + 1L]] <- run_fit(
                inner_train[iso3 != inner_country], inner_test[iso3 == inner_country],
                FALSE, branch_name, "inner", outer_year, held_country)
            }
          }
        }
        inner <- data.table::rbindlist(inner_list)
        selection <- p15_nested_select(inner)
        test <- if (branch_name == "temporal") test_all else test_all[iso3 == held_country]
        outer <- run_fit(prior, test, branch_name == "temporal", branch_name,
          "outer", outer_year, held_country)
        selected <- data.table::copy(outer[model == selection$model])
        selected[, `:=`(model = "selected", selected_candidate = selection$model)]
        outer[, selected_candidate := selection$model]
        outer <- data.table::rbindlist(list(outer, selected))
        k <- k + 1L
        tag <- function(d) {
          d[, `:=`(branch = branch_name, outer_year = outer_year,
            held_outer_country = held_country)]
          d
        }
        inners[[k]] <- tag(inner)
        outers[[k]] <- tag(outer)
        choices[[k]] <- tag(selection$scores)
      }
      if (!quiet) message("Completed ", branch_name, " outer year ", outer_year)
    }
  }
  list(inner = data.table::rbindlist(inners), outer = data.table::rbindlist(outers),
    choices = data.table::rbindlist(choices), audit = data.table::rbindlist(audits))
}

p15_nested_summary <- function(d) {
  if ("branch" %in% names(d)) stopifnot(data.table::uniqueN(d$branch) == 1L)
  raw <- d[model == "raw", .(iso3, analysis_year, raw_ae = absolute_error_pp)]
  stopifnot(!anyDuplicated(raw[, .(iso3, analysis_year)]))
  d <- merge(d, raw, by = c("iso3", "analysis_year"), all.x = TRUE)
  d[, .(rows = .N, countries = data.table::uniqueN(iso3),
    years = data.table::uniqueN(analysis_year), mae_pp = mean(absolute_error_pp),
    rmse_pp = sqrt(mean(squared_error_pp2)), bias_pp = mean(error_pp),
    median_ae_pp = median(absolute_error_pp),
    mae_gain_vs_raw_pp = mean(raw_ae - absolute_error_pp),
    country_balanced_mae_pp = mean(tapply(absolute_error_pp, iso3, mean)),
    country_balanced_gain_pp = mean(tapply(raw_ae - absolute_error_pp, iso3, mean)),
    year_balanced_mae_pp = mean(tapply(absolute_error_pp, analysis_year, mean)),
    year_balanced_gain_pp = mean(tapply(raw_ae - absolute_error_pp, analysis_year, mean))),
    by = model]
}
