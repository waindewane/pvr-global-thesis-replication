# P15 peer-proxy evidence helpers.
#
# These functions construct target-excluding, nonrecursive peer candidates and
# preserve one relational row per target-peer membership. They do not decide
# whether peer proxies are admissible or suitable for headline use.

p15_peer_proxy_schema_version <- function() {
  "SCHEMA-P15-PEER-PROXY-EVIDENCE-V1"
}

p15_peer_safe_quantile <- function(x, probability) {
  x <- as.numeric(x)
  x <- x[is.finite(x)]
  if (!length(x)) return(NA_real_)
  as.numeric(stats::quantile(
    x, probability, names = FALSE, na.rm = TRUE, type = 7
  ))
}

p15_peer_collapse_unique <- function(x) {
  x <- sort(unique(as.character(x[!is.na(x) & as.character(x) != ""])))
  if (!length(x)) return(NA_character_)
  paste(x, collapse = ";")
}

p15_peer_validate_seed_ledger <- function(seed_ledger) {
  required <- c(
    "analysis_year", "iso3", "country", "historical_income_level",
    "positive_status_context", "seed_pool_class", "seed_priority",
    "seed_rate_pct", "seed_maturity_years", "seed_source_family",
    "seed_source_tier", "seed_source_package_id", "seed_record_locator",
    "seed_is_model_or_proxy", "seed_dependency_state"
  )
  missing <- setdiff(required, names(seed_ledger))
  if (length(missing)) {
    stop("Peer seed ledger is missing: ", paste(missing, collapse = ", "),
         call. = FALSE)
  }
  if (any(!is.finite(seed_ledger$seed_rate_pct))) {
    stop("Peer seed ledger contains non-finite rates.", call. = FALSE)
  }
  if (any(seed_ledger$seed_is_model_or_proxy %in% TRUE)) {
    stop("Peer seed ledger contains a model or proxy-derived seed.",
         call. = FALSE)
  }
  if (anyDuplicated(seed_ledger[c(
    "analysis_year", "iso3", "seed_source_family", "seed_source_tier"
  )])) {
    stop("Peer seed ledger contains duplicate source-tier country-years.",
         call. = FALSE)
  }
  invisible(TRUE)
}

p15_peer_select_country_year_seeds <- function(seed_ledger, seed_rule) {
  p15_peer_validate_seed_ledger(seed_ledger)
  eligible_classes <- switch(
    seed_rule,
    strict_observed = "strict_observed",
    broad_observed = c("strict_observed", "broad_observed"),
    strict_observed_plus_ids = c("strict_observed", "ids_contractual_proxy"),
    stop("Unknown peer seed rule: ", seed_rule, call. = FALSE)
  )

  out <- seed_ledger |>
    dplyr::filter(.data$seed_pool_class %in% eligible_classes) |>
    dplyr::arrange(
      .data$analysis_year, .data$iso3, .data$seed_priority,
      .data$seed_source_family, .data$seed_source_tier
    ) |>
    dplyr::group_by(.data$analysis_year, .data$iso3) |>
    dplyr::slice(1L) |>
    dplyr::ungroup() |>
    dplyr::mutate(seed_rule = seed_rule)

  if (anyDuplicated(out[c("analysis_year", "iso3")])) {
    stop("Selected peer seeds are not unique by country-year.",
         call. = FALSE)
  }
  if (any(out$seed_is_model_or_proxy %in% TRUE)) {
    stop("Selected peer seeds include recursive/modelled evidence.",
         call. = FALSE)
  }
  out
}

p15_build_peer_candidates <- function(grid, seed_ledger, variant_register) {
  required_grid <- c(
    "analysis_year", "iso3", "country", "country_year_id",
    "historical_income_level", "historical_lmic_reporting_scope",
    "positive_status_context"
  )
  missing_grid <- setdiff(required_grid, names(grid))
  if (length(missing_grid)) {
    stop("Peer target grid is missing: ", paste(missing_grid, collapse = ", "),
         call. = FALSE)
  }
  required_variants <- c(
    "peer_variant_id", "seed_rule", "minimum_same_income_count",
    "exclude_positive_status_context", "calculation_state"
  )
  missing_variants <- setdiff(required_variants, names(variant_register))
  if (length(missing_variants)) {
    stop("Peer variant register is missing: ",
         paste(missing_variants, collapse = ", "), call. = FALSE)
  }
  if (anyDuplicated(grid[c("analysis_year", "iso3")])) {
    stop("Peer target grid has duplicate country-years.", call. = FALSE)
  }
  computed_variants <- variant_register |>
    dplyr::filter(.data$calculation_state == "computed_candidate")
  if (!nrow(computed_variants)) {
    return(list(candidates = tibble::tibble(), members = tibble::tibble()))
  }

  candidate_parts <- list()
  member_parts <- list()
  candidate_index <- 0L
  member_index <- 0L

  for (variant_row in split(computed_variants, seq_len(nrow(computed_variants)))) {
    variant_id <- as.character(variant_row$peer_variant_id[[1]])
    seed_rule <- as.character(variant_row$seed_rule[[1]])
    minimum_count <- as.integer(variant_row$minimum_same_income_count[[1]])
    exclude_status <- isTRUE(variant_row$exclude_positive_status_context[[1]])
    selected_seeds <- p15_peer_select_country_year_seeds(
      seed_ledger, seed_rule
    )

    for (target_row in split(grid, seq_len(nrow(grid)))) {
      year <- as.integer(target_row$analysis_year[[1]])
      target_iso3 <- as.character(target_row$iso3[[1]])
      target_income <- as.character(target_row$historical_income_level[[1]])
      eligible <- selected_seeds |>
        dplyr::filter(
          .data$analysis_year == year,
          .data$iso3 != target_iso3
        )
      if (exclude_status) {
        eligible <- eligible |>
          dplyr::filter(!dplyr::coalesce(.data$positive_status_context, FALSE))
      }
      same_income <- eligible |>
        dplyr::filter(
          !is.na(.data$historical_income_level),
          .data$historical_income_level == target_income
        )
      if (nrow(same_income) >= minimum_count) {
        pool <- same_income
        pool_rule <- paste0(
          "same_historical_income_min", minimum_count,
          "_same_year_excluding_target"
        )
      } else {
        pool <- eligible
        pool_rule <- paste0(
          "global_same_year_fallback_after_income_min", minimum_count,
          "_excluding_target"
        )
      }

      candidate_index <- candidate_index + 1L
      rates <- as.numeric(pool$seed_rate_pct)
      maturities <- as.numeric(pool$seed_maturity_years)
      target_excluded <- !target_iso3 %in% pool$iso3
      nonrecursive <- !any(pool$seed_is_model_or_proxy %in% TRUE)
      candidate_parts[[candidate_index]] <- tibble::tibble(
        analysis_year = year,
        iso3 = target_iso3,
        country = as.character(target_row$country[[1]]),
        country_year_id = as.character(target_row$country_year_id[[1]]),
        historical_income_level = target_income,
        historical_lmic_reporting_scope = as.logical(
          target_row$historical_lmic_reporting_scope[[1]]
        ),
        target_positive_status_context = as.logical(
          target_row$positive_status_context[[1]]
        ),
        peer_variant_id = variant_id,
        seed_rule = seed_rule,
        minimum_same_income_count = minimum_count,
        exclude_positive_status_context = exclude_status,
        peer_pool_rule = pool_rule,
        peer_country_count = nrow(pool),
        peer_country_list = p15_peer_collapse_unique(pool$iso3),
        peer_rate_median_pct = if (length(rates)) {
          stats::median(rates, na.rm = TRUE)
        } else NA_real_,
        peer_rate_mean_pct = if (length(rates)) mean(rates, na.rm = TRUE) else NA_real_,
        peer_rate_iqr_low_pct = p15_peer_safe_quantile(rates, 0.25),
        peer_rate_iqr_high_pct = p15_peer_safe_quantile(rates, 0.75),
        peer_rate_iqr_width_pct =
          p15_peer_safe_quantile(rates, 0.75) -
          p15_peer_safe_quantile(rates, 0.25),
        peer_rate_min_pct = if (length(rates)) min(rates, na.rm = TRUE) else NA_real_,
        peer_rate_max_pct = if (length(rates)) max(rates, na.rm = TRUE) else NA_real_,
        peer_maturity_median_years = if (any(is.finite(maturities))) {
          stats::median(maturities[is.finite(maturities)], na.rm = TRUE)
        } else NA_real_,
        peer_source_family_mix = p15_peer_collapse_unique(
          pool$seed_source_family
        ),
        peer_source_package_ids = p15_peer_collapse_unique(
          pool$seed_source_package_id
        ),
        peer_predecessor_dependency_count = sum(
          grepl("predecessor", pool$seed_dependency_state), na.rm = TRUE
        ),
        peer_positive_status_context_count = sum(
          pool$positive_status_context %in% TRUE, na.rm = TRUE
        ),
        target_excluded = target_excluded,
        nonrecursive_seed_pool = nonrecursive,
        relational_provenance_complete = nrow(pool) > 0L,
        candidate_computed = nrow(pool) > 0L && all(is.finite(rates)),
        candidate_governance_state =
          "diagnostic_candidate_not_approved_for_selection_or_headline_use",
        peer_evidence_schema_version = p15_peer_proxy_schema_version()
      )

      if (nrow(pool)) {
        member_index <- member_index + 1L
        member_parts[[member_index]] <- pool |>
          dplyr::arrange(.data$iso3) |>
          dplyr::transmute(
            analysis_year = year,
            target_iso3 = target_iso3,
            target_country = as.character(target_row$country[[1]]),
            target_country_year_id = as.character(
              target_row$country_year_id[[1]]
            ),
            target_historical_income_level = target_income,
            target_positive_status_context = as.logical(
              target_row$positive_status_context[[1]]
            ),
            peer_variant_id = variant_id,
            seed_rule = seed_rule,
            peer_pool_rule = pool_rule,
            peer_iso3 = .data$iso3,
            peer_country = .data$country,
            peer_historical_income_level = .data$historical_income_level,
            peer_positive_status_context = .data$positive_status_context,
            peer_rate_pct = .data$seed_rate_pct,
            peer_maturity_years = .data$seed_maturity_years,
            peer_seed_source_family = .data$seed_source_family,
            peer_seed_source_tier = .data$seed_source_tier,
            peer_seed_source_package_id = .data$seed_source_package_id,
            peer_seed_record_locator = .data$seed_record_locator,
            peer_seed_dependency_state = .data$seed_dependency_state,
            target_excluded = .data$iso3 != target_iso3,
            recursive_seed_flag = .data$seed_is_model_or_proxy,
            member_weighting_role = "unweighted_member_of_median_pool",
            peer_membership_id = paste(
              "PEER-MEMBER", variant_id, year, target_iso3, .data$iso3,
              sep = "::"
            ),
            peer_evidence_schema_version = p15_peer_proxy_schema_version()
          )
      }
    }
  }

  candidates <- dplyr::bind_rows(candidate_parts) |>
    dplyr::arrange(.data$peer_variant_id, .data$analysis_year, .data$iso3)
  members <- dplyr::bind_rows(member_parts) |>
    dplyr::arrange(
      .data$peer_variant_id, .data$analysis_year,
      .data$target_iso3, .data$peer_iso3
    )

  stopifnot(
    nrow(candidates) == nrow(grid) * nrow(computed_variants),
    !anyDuplicated(candidates[c(
      "peer_variant_id", "analysis_year", "iso3"
    )]),
    all(candidates$target_excluded),
    all(candidates$nonrecursive_seed_pool),
    all(members$target_excluded),
    !any(members$recursive_seed_flag),
    !anyDuplicated(members[c(
      "peer_variant_id", "analysis_year", "target_iso3", "peer_iso3"
    )])
  )
  list(candidates = candidates, members = members)
}

p15_build_p13_legacy_peer <- function(targets_2024, strict_benchmarks_2024) {
  required_targets <- c(
    "analysis_year", "iso3", "country", "income_level", "lending_type",
    "included_in_lmic_reporting_scope", "has_status_gate",
    "status_or_evidence_state"
  )
  missing_targets <- setdiff(required_targets, names(targets_2024))
  if (length(missing_targets)) {
    stop("P13 legacy peer targets are missing: ",
         paste(missing_targets, collapse = ", "), call. = FALSE)
  }
  required_benchmarks <- c(
    "iso3", "country", "market_rate", "market_maturity_years",
    "market_rate_source_class"
  )
  missing_benchmarks <- setdiff(required_benchmarks, names(strict_benchmarks_2024))
  if (length(missing_benchmarks)) {
    stop("P13 strict peer seeds are missing: ",
         paste(missing_benchmarks, collapse = ", "), call. = FALSE)
  }
  if (anyDuplicated(targets_2024$iso3) || anyDuplicated(strict_benchmarks_2024$iso3)) {
    stop("P13 legacy peer inputs must be unique by iso3.", call. = FALSE)
  }

  peer_pool <- strict_benchmarks_2024 |>
    dplyr::left_join(
      targets_2024 |>
        dplyr::select(
          peer_iso3 = "iso3",
          peer_income_level = "income_level",
          peer_lending_type = "lending_type",
          peer_has_status_gate = "has_status_gate"
        ),
      by = c("iso3" = "peer_iso3")
    ) |>
    dplyr::filter(!dplyr::coalesce(.data$peer_has_status_gate, FALSE)) |>
    dplyr::transmute(
      peer_iso3 = .data$iso3,
      peer_country = .data$country,
      peer_income_level = .data$peer_income_level,
      peer_lending_type = .data$peer_lending_type,
      peer_rate_pct = as.numeric(.data$market_rate),
      peer_maturity_years = as.numeric(.data$market_maturity_years),
      peer_source_class = .data$market_rate_source_class
    )

  candidate_parts <- vector("list", nrow(targets_2024))
  member_parts <- vector("list", nrow(targets_2024))
  for (i in seq_len(nrow(targets_2024))) {
    target <- targets_2024[i, ]
    candidates <- peer_pool |>
      dplyr::filter(.data$peer_iso3 != target$iso3[[1]])
    same_income_lending <- candidates |>
      dplyr::filter(
        .data$peer_income_level == target$income_level[[1]],
        .data$peer_lending_type == target$lending_type[[1]]
      )
    same_income <- candidates |>
      dplyr::filter(.data$peer_income_level == target$income_level[[1]])
    same_lending <- candidates |>
      dplyr::filter(.data$peer_lending_type == target$lending_type[[1]])
    if (nrow(same_income_lending) >= 3L) {
      pool <- same_income_lending
      rule <- "same_income_lending_min3"
      quality <- if (nrow(pool) >= 5L) "peer_proxy_medium" else "peer_proxy_low"
    } else if (nrow(same_income) >= 5L) {
      pool <- same_income
      rule <- "same_income_min5"
      quality <- "peer_proxy_low"
    } else if (nrow(same_lending) >= 5L) {
      pool <- same_lending
      rule <- "same_lending_min5"
      quality <- "peer_proxy_low"
    } else {
      pool <- candidates
      rule <- "all_nonstatus_strict_pool_fallback"
      quality <- "peer_proxy_lowest"
    }
    rates <- pool$peer_rate_pct
    maturities <- pool$peer_maturity_years
    candidate_parts[[i]] <- tibble::tibble(
      analysis_year = 2024L,
      iso3 = target$iso3[[1]],
      country = target$country[[1]],
      income_level = target$income_level[[1]],
      lending_type = target$lending_type[[1]],
      included_in_lmic_reporting_scope = as.logical(
        target$included_in_lmic_reporting_scope[[1]]
      ),
      has_status_gate = as.logical(target$has_status_gate[[1]]),
      status_or_evidence_state = target$status_or_evidence_state[[1]],
      reliability_label = quality,
      peer_rule_version = "p11b_strict_pool_median_v1",
      peer_pool_rule = rule,
      peer_country_count = nrow(pool),
      peer_country_list = p15_peer_collapse_unique(pool$peer_iso3),
      peer_rate_median_pct = stats::median(rates, na.rm = TRUE),
      peer_rate_mean_pct = mean(rates, na.rm = TRUE),
      peer_rate_iqr_low_pct = p15_peer_safe_quantile(rates, 0.25),
      peer_rate_iqr_high_pct = p15_peer_safe_quantile(rates, 0.75),
      peer_rate_min_pct = min(rates, na.rm = TRUE),
      peer_rate_max_pct = max(rates, na.rm = TRUE),
      peer_rate_iqr_width_pct =
        p15_peer_safe_quantile(rates, 0.75) -
        p15_peer_safe_quantile(rates, 0.25),
      peer_maturity_median_years = stats::median(maturities, na.rm = TRUE),
      target_excluded = !target$iso3[[1]] %in% pool$peer_iso3,
      nonrecursive_seed_pool = TRUE,
      legacy_reconstruction_state = "p13_historical_reference_only"
    )
    member_parts[[i]] <- pool |>
      dplyr::transmute(
        analysis_year = 2024L,
        target_iso3 = target$iso3[[1]],
        peer_iso3,
        peer_country,
        peer_income_level,
        peer_lending_type,
        peer_rate_pct,
        peer_maturity_years,
        peer_source_class,
        target_excluded = .data$peer_iso3 != target$iso3[[1]],
        recursive_seed_flag = FALSE
      )
  }
  candidates <- dplyr::bind_rows(candidate_parts) |>
    dplyr::arrange(.data$iso3)
  members <- dplyr::bind_rows(member_parts) |>
    dplyr::arrange(.data$target_iso3, .data$peer_iso3)
  stopifnot(
    nrow(candidates) == nrow(targets_2024),
    all(candidates$target_excluded),
    all(members$target_excluded),
    !any(members$recursive_seed_flag)
  )
  list(candidates = candidates, members = members)
}

p15_build_peer_validation <- function(candidates, anchor_ledger) {
  required_anchors <- c(
    "analysis_year", "iso3", "anchor_family", "anchor_rate_pct",
    "anchor_maturity_years", "anchor_quality_state"
  )
  missing <- setdiff(required_anchors, names(anchor_ledger))
  if (length(missing)) {
    stop("Peer validation anchors are missing: ",
         paste(missing, collapse = ", "), call. = FALSE)
  }
  if (anyDuplicated(anchor_ledger[c(
    "analysis_year", "iso3", "anchor_family"
  )])) {
    stop("Peer validation anchors are not unique.", call. = FALSE)
  }
  candidates |>
    dplyr::inner_join(
      anchor_ledger,
      by = c("analysis_year", "iso3"),
      relationship = "many-to-many"
    ) |>
    dplyr::mutate(
      signed_gap_pp = .data$peer_rate_median_pct - .data$anchor_rate_pct,
      abs_gap_pp = abs(.data$signed_gap_pp),
      validation_authority_state =
        "descriptive_overlap_pending_VAL_02_authorization",
      validation_integrity_pass =
        .data$target_excluded & .data$nonrecursive_seed_pool
    )
}
