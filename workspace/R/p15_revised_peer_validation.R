# Isolated replay adapter. Reconstruct the agreed sources and matching criteria;
# retain the full-panel experiment as an independent numerical reference.
source("scripts/p15/loan_extension/benchmark_matching.R")
source("R/p15_revised_peer_matching.R")
p15_revised_one <- p15_revised_peer_match_one
p15_revised_helper_code <- c("R/p15_revised_peer_validation.R",
  "R/p15_revised_peer_matching.R", "scripts/p15/loan_extension/benchmark_matching.R")

p15_revised_peer_inputs <- function(base, reader = data.table::fread) {
  root <- base
  files <- c(file.path(base, c("core_evidence.csv", "selected_reference.csv",
    "tier_eligibility.csv", "peer_region_context.csv")),
    file.path(root, c("scorecard_country_year.csv", "all_donor_seeds.csv", "all_peer_predictions.csv")))
  core <- reader(files[1]); selected <- reader(files[2]); eligibility <- reader(files[3]); context <- reader(files[4])
  scores <- reader(files[5])[scenario == "wgi_bounded"]
  saved_seeds <- reader(files[6])[sources == "PISR", .(iso3, analysis_year, seed_rate, seed_source)]
  saved_predictions <- reader(files[7])[method == "preferred"]
  keys <- c("iso3", "analysis_year")
  stopifnot(!anyDuplicated(scores[, ..keys]))
  eligible <- p15_loan_benchmark_reference(selected, eligibility, core)$tiers
  eligible <- eligible[ordinary_cost_tier_usable %in% TRUE &
    tier %in% c("primary", "ids", "secondary", "moodys") & is.finite(benchmark_tier_rate_pct)]
  eligible[, priority := match(tier, c("primary", "ids", "secondary", "moodys"))]
  setorder(eligible, iso3, analysis_year, priority)
  seeds <- eligible[, .(seed_source = first(tier), seed_rate = first(benchmark_tier_rate_pct),
    seed_id = first(benchmark_tier_evidence_id)), by = .(iso3, analysis_year)]
  rating_variant <- core$rating_moodys_variant_id[match(paste(seeds$iso3,seeds$analysis_year),
    paste(core$iso3,core$analysis_year))]
  moodys_rows <- which(seeds$seed_source == "moodys")
  seeds[moodys_rows, seed_id := paste(analysis_year,iso3,
    rating_variant[moodys_rows],sep="::")]
  cmp <- merge(seeds, saved_seeds, by = keys, all = TRUE, suffixes = c("", "_saved"))
  stopifnot(nrow(cmp) == nrow(seeds), nrow(cmp) == nrow(saved_seeds),
    all(cmp$seed_source == cmp$seed_source_saved),
    all(is.finite(cmp$seed_rate) & abs(cmp$seed_rate - cmp$seed_rate_saved) < 1e-10),
    all(!is.na(seeds$seed_id) & nzchar(seeds$seed_id)))
  panel <- merge(core[, .(iso3, analysis_year, historical_income_level,
    historical_lmic_reporting_scope)], context[, .(iso3, analysis_year,
      rating_source_region, moodys_rating_normalized)], by = keys)
  panel <- merge(panel, scores[, .(iso3, analysis_year, shadow = shadow_notch,
    shadow_lower = shadow_notch_lower, shadow_upper = shadow_notch_upper,
    shadow_complete = scorecard_complete, shadow_status = status)], by = keys, all.x = TRUE)
  scale <- c("Aaa", "Aa1", "Aa2", "Aa3", "A1", "A2", "A3", "Baa1", "Baa2", "Baa3",
    "Ba1", "Ba2", "Ba3", "B1", "B2", "B3", "Caa1", "Caa2", "Caa3", "Ca", "C")
  panel[, notch := match(moodys_rating_normalized, scale)]
  panel[is.na(shadow_complete), shadow_complete := FALSE]
  panel[is.na(shadow_status), shadow_status := "not_supplied"]
  stopifnot(nrow(panel) == nrow(core), !anyDuplicated(panel[, ..keys]))
  list(panel = panel, seeds = seeds, saved = saved_predictions, files = files)
}

p15_revised_predictions <- function(input, hidden = FALSE, rules = "1;2;3;4;5",
    method = "current", panel = input$panel) {
  design <- data.table(method = method, scenario = "wgi_bounded", input_policy = "conditional_points",
    role = "accepted_private_working_peer", distance = "guaranteed_interval", rules = rules, caliper = 3)
  result <- vector("list", nrow(panel)); k <- 0L
  for (yr in sort(unique(panel$analysis_year))) {
    targets <- panel[analysis_year == yr]
    pool <- merge(input$seeds[analysis_year == yr, .(iso3, analysis_year, seed_rate, seed_source)],
      targets, by = c("iso3", "analysis_year"))
    for (i in seq_len(nrow(targets))) {
      k <- k + 1L
      result[[k]] <- p15_revised_one(targets[i], pool, design, hidden)
    }
  }
  rbindlist(result)
}

p15_revised_reference_check <- function(predictions, input, mode_name = "deployment") {
  a <- predictions[, .(iso3, analysis_year, estimate, rule, n_peers, member_ids)]
  b <- input$saved[mode == mode_name, .(iso3, analysis_year, estimate, rule, n_peers, member_ids)]
  z <- merge(a, b, by = c("iso3", "analysis_year"), suffixes = c("_new", "_saved"))
  stopifnot(nrow(z) == nrow(b),
    identical(is.finite(z$estimate_new), is.finite(z$estimate_saved)),
    all(abs(z$estimate_new - z$estimate_saved) < 1e-10, na.rm = TRUE),
    identical(z$rule_new, z$rule_saved), identical(z$n_peers_new, z$n_peers_saved),
    identical(z$member_ids_new, z$member_ids_saved))
  invisible(TRUE)
}

p15_revised_geography_predictions <- function(input, hidden = FALSE) {
  # Excluding geography retains Rules 2 and 4 only: no income-only/global fallback.
  # Region-only and worldwide-only are explicitly relaxed diagnostic comparators.
  rules <- c(current = "1;2;3;4;5", no_geography = "2;4", region_only = "7", worldwide_only = "8")
  rbindlist(lapply(names(rules), function(method) {
    z <- p15_revised_predictions(input, hidden, rules[[method]], method)
    if (method == "current") p15_revised_reference_check(z, input,
      if (hidden) "target_rating_hidden" else "deployment")
    target_regions <- input$panel$rating_source_region[match(paste(z$iso3, z$analysis_year),
      paste(input$panel$iso3, input$panel$analysis_year))]
    share <- vapply(seq_len(nrow(z)), function(i) {
      ids <- strsplit(z$member_ids[i], ";", fixed = TRUE)[[1]]
      if (!length(ids)) return(NA_real_)
      r <- input$panel[analysis_year == z$analysis_year[i] & iso3 %in% ids, rating_source_region]
      mean(r == target_regions[i], na.rm = TRUE)
    }, numeric(1))
    z[, .(analysis_year, iso3, method, information = if (hidden) "target_rating_hidden" else "available_rating",
      estimate, seed_count = n_peers, pool_rule = ifelse(is.finite(rule), paste0("rule_", rule), "insufficient_peers"),
      seed_ids = member_ids, same_region_share = share,
      primary_members, ids_members, secondary_members, rating_implied_members,
      target_shadow_used, estimate_depends_on_incomplete_input)]
  }))
}
