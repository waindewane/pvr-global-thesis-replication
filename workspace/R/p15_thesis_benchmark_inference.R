# Exploratory paired-loss inference. Rates/losses are percentage points throughout.

p15_thesis_cluster_boot <- function(loss, country, repetitions = 4999L, seed = 20260910L) {
  stopifnot(length(loss) == length(country), length(loss) > 0L,
            all(is.finite(loss)), !anyNA(country), repetitions >= 99L)
  groups <- sort(unique(as.character(country)))
  count <- vapply(groups, function(g) sum(country == g), integer(1))
  total <- vapply(groups, function(g) sum(loss[country == g]), numeric(1))
  estimate <- mean(loss)
  if (length(groups) < 2L) return(data.frame(
    estimate_pp = estimate, ci_low_pp = NA_real_, ci_high_pp = NA_real_,
    p_boot_two_sided = NA_real_, repetitions = repetitions, seed = seed))
  set.seed(seed)
  draw <- matrix(sample.int(length(groups), length(groups) * repetitions,
                            replace = TRUE), nrow = length(groups))
  denominators <- colSums(matrix(count[draw], nrow = length(groups)))
  boot <- colSums(matrix(total[draw], nrow = length(groups))) / denominators
  # Center observations, retaining unequal cluster sizes and their full histories.
  centered <- total - count * estimate
  null_boot <- colSums(matrix(centered[draw], nrow = length(groups))) / denominators
  interval <- unname(stats::quantile(boot, c(0.025, 0.975), type = 7))
  data.frame(estimate_pp = estimate, ci_low_pp = interval[1], ci_high_pp = interval[2],
             p_boot_two_sided = (1 + sum(abs(null_boot) >= abs(estimate) - 1e-12)) /
               (repetitions + 1), repetitions = repetitions, seed = seed)
}

p15_thesis_loss_rows <- function(d, focal, comparator, family, view) {
  z <- data.table::copy(d[is.finite(get(focal)) & is.finite(get(comparator)) &
                          is.finite(primary)])
  z[, .(iso3, country, analysis_year, region, family, sample_view = view,
        focal, comparator, primary, focal_rate = get(focal),
        comparator_rate = get(comparator), focal_error = get(focal) - primary,
        comparator_error = get(comparator) - primary,
        improvement_pp = abs(get(comparator) - primary) - abs(get(focal) - primary))]
}

p15_thesis_consecutive <- function(d) {
  keys <- c("iso3", "family", "sample_view", "focal", "comparator", "analysis_year")
  stopifnot(!anyDuplicated(d[, ..keys]))
  prior <- data.table::copy(d)
  prior[, analysis_year := analysis_year + 1L]
  prior <- prior[, .(iso3, family, sample_view, focal, comparator, analysis_year,
                     primary_prior = primary, focal_prior = focal_rate,
                     comparator_prior = comparator_rate,
                     focal_source_prior = focal_source)]
  z <- merge(d, prior, by = keys)
  z[, `:=`(primary_change = primary - primary_prior,
            focal_change = focal_rate - focal_prior,
            comparator_change = comparator_rate - comparator_prior)]
  z[, `:=`(level_improvement_prior_pp = abs(comparator_prior - primary_prior) -
             abs(focal_prior - primary_prior),
            change_improvement_pp = abs(comparator_change - primary_change) -
              abs(focal_change - primary_change),
            source_switch = focal_source != focal_source_prior)]
  z[, `:=`(both_endpoint_levels_closer = improvement_pp > 1e-10 &
             level_improvement_prior_pp > 1e-10,
            change_closer = change_improvement_pp > 1e-10,
            change_worse = change_improvement_pp < -1e-10)]
  z[]
}

p15_thesis_add_fallback <- function(d, name, tiers) {
  d <- data.table::copy(d)
  d[, (name) := NA_real_]
  d[, (paste0(name, "_source")) := NA_character_]
  for (tier in tiers) {
    eligible <- which(!is.finite(d[[name]]) & is.finite(d[[tier]]))
    data.table::set(d, i = eligible, j = name, value = d[[tier]][eligible])
    data.table::set(d, i = eligible, j = paste0(name, "_source"), value = tier)
  }
  d
}
