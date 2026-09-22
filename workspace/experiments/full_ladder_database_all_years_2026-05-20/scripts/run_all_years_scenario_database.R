suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(purrr)
  library(tibble)
  library(lubridate)
  library(readxl)
  library(httr2)
  library(jsonlite)
})

source("R/pvr.R")
source("R/ids.R")
source("R/lseg_benchmarks.R")

experiment_dir <- "experiments/full_ladder_database_all_years_2026-05-20"
data_dir <- file.path(experiment_dir, "data/lseg_v14/pvr-global-lseg/output/tables")
ids_cache_dir <- file.path(experiment_dir, "data/ids_core_all_years")
out_dir <- file.path(experiment_dir, "outputs")
dir.create(ids_cache_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

analysis_years <- 2000:2025

path_primary <- file.path(data_dir, "lseg_pvr_benchmark_candidates_with_yield_oecd_base_2000-01-01_2025-12-31.csv")
path_secondary_snap <- file.path(data_dir, "lseg_secondary_outstanding_snapshots_oecd_base_2000-01-01_2025-12-31.csv")
path_secondary_hist <- file.path(data_dir, "lseg_secondary_market_year_end_history_oecd_base_2000-01-01_2025-12-31.csv")
path_rating <- file.path(data_dir, "lseg_rating_inputs_desktop_oecd_base_2000-01-01_2025-12-31.csv")
path_risk_free_lseg <- file.path(data_dir, "lseg_risk_free_curve_history_oecd_base_2000-01-01_2025-12-31.csv")
path_country_metadata <- "data-raw/world_bank_countries.json"
path_ids_terms_2024 <- "output/tables/ids_terms_2024.csv"
path_damodaran <- "sources/literature_review/lr3_systematic_expansion_2026-05-12/official_sources/damodaran_ctryprem.xlsx"
path_damodaran_archive_dir <- file.path(experiment_dir, "data/damodaran_archive")
path_fred_dgs7 <- "sources/literature_review/lr3_systematic_expansion_2026-05-12/official_sources/fred_dgs7.csv"
dir.create(path_damodaran_archive_dir, recursive = TRUE, showWarnings = FALSE)

`%||%` <- function(x, y) if (is.null(x)) y else x

clean_num <- function(x) {
  suppressWarnings(as.numeric(as.character(x)))
}

as_logical_flag <- function(x) {
  if (is.logical(x)) return(dplyr::coalesce(x, FALSE))
  y <- toupper(trimws(as.character(x)))
  dplyr::coalesce(y %in% c("TRUE", "T", "1", "Y", "YES"), FALSE)
}

first_non_missing <- function(x) {
  x <- x[!is.na(x) & as.character(x) != ""]
  if (length(x) == 0) {
    if (is.character(x)) NA_character_ else NA
  } else {
    x[[1]]
  }
}

collapse_values <- function(x, sep = ";") {
  x <- sort(unique(stats::na.omit(as.character(x))))
  x <- x[x != ""]
  if (length(x) == 0) NA_character_ else paste(x, collapse = sep)
}

weighted_mean_or_na <- function(x, w) {
  x <- clean_num(x)
  w <- clean_num(w)
  ok <- !is.na(x) & !is.na(w) & w > 0
  if (!any(ok)) return(NA_real_)
  stats::weighted.mean(x[ok], w[ok])
}

mean_or_na <- function(x) {
  x <- clean_num(x)
  x <- x[!is.na(x)]
  if (length(x) == 0) NA_real_ else mean(x)
}

max_or_na <- function(x) {
  x <- clean_num(x)
  x <- x[!is.na(x)]
  if (length(x) == 0) NA_real_ else max(x)
}

sum_or_na <- function(x) {
  x <- clean_num(x)
  if (all(is.na(x))) NA_real_ else sum(x, na.rm = TRUE)
}

country_join_key <- function(country) {
  out <- normalize_country_name_for_join(country)
  dplyr::recode(
    out,
    "Bahamas" = "Bahamas, The",
    "Bosnia Herzegovina" = "Bosnia and Herzegovina",
    "Bosnia and Herzegovina, Fed Rep" = "Bosnia and Herzegovina",
    "Czechia" = "Czech Republic",
    "Hong Kong" = "Hong Kong SAR, China",
    "Korea South" = "Korea, Rep.",
    "Macao" = "Macao SAR, China",
    "Russia" = "Russian Federation",
    "Slovakia" = "Slovak Republic",
    "Syria" = "Syrian Arab Republic",
    "Venezuela" = "Venezuela, RB",
    "Yemen" = "Yemen, Rep.",
    .default = out
  )
}

country_key_from <- function(iso3, country) {
  iso3 <- as.character(iso3)
  country <- as.character(country)
  n <- max(length(iso3), length(country))
  iso3 <- rep_len(iso3, n)
  country <- rep_len(country, n)
  out <- country_join_key(country)
  has_iso3 <- !is.na(iso3) & iso3 != ""
  out[has_iso3] <- iso3[has_iso3]
  out
}

quality_from_issue_count <- function(issue_count, prefix) {
  dplyr::case_when(
    is.na(issue_count) ~ paste0(prefix, "_not_available"),
    issue_count >= 3 ~ paste0(prefix, "_multi_issue"),
    issue_count == 2 ~ paste0(prefix, "_two_issue"),
    issue_count == 1 ~ paste0(prefix, "_single_issue"),
    TRUE ~ paste0(prefix, "_not_available")
  )
}

status_from_issue_count <- function(issue_count) {
  dplyr::case_when(
    is.na(issue_count) | issue_count == 0 ~ "not_computed",
    issue_count == 1 ~ "computed_experimental_thin_single",
    issue_count == 2 ~ "computed_experimental_thin_two",
    issue_count >= 3 ~ "computed_experimental_multi",
    TRUE ~ "computed_experimental"
  )
}

safe_official_pvr <- function(official_rate, maturity, grace, discount_rate) {
  tryCatch(
    {
      if (any(is.na(c(official_rate, maturity, grace, discount_rate)))) {
        return(tibble(pvr = NA_real_, pvr_error = "missing_input"))
      }
      if (maturity <= 0 || grace < 0 || grace >= maturity) {
        return(tibble(pvr = NA_real_, pvr_error = "invalid_terms"))
      }
      tibble(
        pvr = round(calculate_official_pvr(
          annual_rate_percent = official_rate,
          maturity_years = maturity,
          grace_years = grace,
          discount_rate_percent = discount_rate
        ), 2),
        pvr_error = NA_character_
      )
    },
    error = function(e) tibble(pvr = NA_real_, pvr_error = conditionMessage(e))
  )
}

standard_output_columns <- function(x) {
  required <- list(
    analysis_year = NA_integer_,
    iso3 = NA_character_,
    country = NA_character_,
    country_key = NA_character_,
    income_level = NA_character_,
    lending_type = NA_character_,
    scenario_id = NA_character_,
    ladder_component = NA_character_,
    benchmark_source_tier = NA_character_,
    method_family = NA_character_,
    scenario_priority = NA_integer_,
    market_rate_pct = NA_real_,
    market_maturity_years = NA_real_,
    benchmark_quality_band = NA_character_,
    benchmark_status = NA_character_,
    benchmark_status_reason = NA_character_,
    market_rate_measure_basis = NA_character_,
    currency_basis = NA_character_,
    weighting_variable = NA_character_,
    total_weight_usd = NA_real_,
    issue_count = NA_integer_,
    included_issue_count = NA_integer_,
    eligible_issue_count = NA_integer_,
    yield_source = NA_character_,
    included_isins = NA_character_,
    included_issue_keys = NA_character_,
    source_artifact = NA_character_,
    source_file = NA_character_,
    source_url_or_path = NA_character_,
    source_retrieval_date = NA_character_,
    source_extraction_run_id = NA_character_,
    source_license_class = NA_character_,
    method_version = NA_character_,
    headline_admissible = NA_character_,
    included_in_headline_results = NA_character_,
    source_note = NA_character_,
    method_warning = NA_character_,
    decision_status = NA_character_
  )
  for (nm in names(required)) {
    if (!nm %in% names(x)) x[[nm]] <- required[[nm]]
  }
  x <- x |>
    mutate(
      source_file = coalesce(.data$source_file, basename(.data$source_artifact)),
      source_url_or_path = coalesce(.data$source_url_or_path, .data$source_artifact),
      source_retrieval_date = coalesce(.data$source_retrieval_date, "2026-05-20"),
      source_license_class = coalesce(.data$source_license_class, "mixed_local_research_inputs"),
      method_version = coalesce(.data$method_version, "experimental_all_years_2026-05-20"),
      headline_admissible = coalesce(.data$headline_admissible, "no_experimental_only"),
      included_in_headline_results = coalesce(.data$included_in_headline_results, "no")
    )
  x |> select(all_of(names(required)), everything())
}

read_excel_with_first_row_headers <- function(path, sheet) {
  raw <- readxl::read_excel(path, sheet = sheet, col_names = FALSE)
  headers <- make.names(as.character(unlist(raw[1, ])), unique = TRUE)
  out <- raw[-1, ]
  names(out) <- headers
  out
}

normalize_damodaran_spread_to_pct <- function(x) {
  x <- clean_num(x)
  scale_is_bps <- stats::median(x, na.rm = TRUE) > 1
  dplyr::case_when(
    is.na(x) ~ NA_real_,
    scale_is_bps ~ x / 100,
    TRUE ~ x * 100
  )
}

download_damodaran_archive_files <- function() {
  specs <- tibble(
    analysis_year = 2000:2024,
    suffix = sprintf("%02d", 0:24),
    extension = if_else(2000:2024 <= 2020, "xls", "xlsx")
  ) |>
    mutate(
      file_name = paste0("ctryprem", .data$suffix, ".", .data$extension),
      url = paste0("https://pages.stern.nyu.edu/~adamodar/pc/archives/", .data$file_name),
      local_path = file.path(path_damodaran_archive_dir, .data$file_name)
    )

  purrr::pwalk(specs, function(analysis_year, suffix, extension, file_name, url, local_path) {
    if (file.exists(local_path) && file.info(local_path)$size > 0) return(invisible(NULL))
    tryCatch(
      utils::download.file(url, local_path, mode = "wb", quiet = TRUE),
      error = function(e) warning("Could not download Damodaran archive file ", file_name, ": ", conditionMessage(e))
    )
  })

  specs |>
    mutate(downloaded = file.exists(.data$local_path) & file.info(.data$local_path)$size > 0)
}

parse_damodaran_archive_file <- function(path, analysis_year) {
  sheets <- readxl::excel_sheets(path)

  if ("Regional breakdown" %in% sheets) {
    x <- read_excel_with_first_row_headers(path, "Regional breakdown")
    nm <- names(x)
    country_col <- nm[stringr::str_detect(nm, "^Country$")][1]
    spread_col <- nm[stringr::str_detect(nm, "Adj.*Default.*Spread")][1]
    rating_col <- nm[stringr::str_detect(nm, "Moody|Long.Term")][1]
    region_col <- nm[stringr::str_detect(nm, "Region")][1]

    return(tibble(
      analysis_year = analysis_year,
      damodaran_country = as.character(x[[country_col]]),
      damodaran_rating = if (!is.na(rating_col)) as.character(x[[rating_col]]) else NA_character_,
      damodaran_region = if (!is.na(region_col)) as.character(x[[region_col]]) else NA_character_,
      default_spread_raw = clean_num(x[[spread_col]]),
      archive_parser = "regional_breakdown"
    ) |>
      mutate(default_spread_pct = normalize_damodaran_spread_to_pct(.data$default_spread_raw)) |>
      filter(!is.na(.data$damodaran_country), .data$damodaran_country != "", !is.na(.data$default_spread_pct)))
  }

  sheet <- sheets[1]
  x <- suppressMessages(readxl::read_excel(path, sheet = sheet, col_names = FALSE))
  header_row <- which(as.character(x[[1]]) == "Country" & stringr::str_detect(as.character(x[[2]]), "Long|Rating"))[1]
  if (is.na(header_row)) {
    warning("Could not parse Damodaran archive file: ", basename(path))
    return(tibble())
  }

  data <- x[(header_row + 1):nrow(x), ]
  names(data) <- paste0("V", seq_len(ncol(data)))
  data |>
    transmute(
      analysis_year = analysis_year,
      damodaran_country = as.character(.data$V1),
      damodaran_rating = as.character(.data$V2),
      damodaran_region = NA_character_,
      default_spread_raw = clean_num(.data$V3),
      archive_parser = "legacy_sheet"
    ) |>
    mutate(default_spread_pct = normalize_damodaran_spread_to_pct(.data$default_spread_raw)) |>
    filter(
      !is.na(.data$damodaran_country),
      .data$damodaran_country != "",
      !is.na(.data$default_spread_pct),
      !stringr::str_detect(.data$damodaran_country, "Average|Country|Adjusted|Frontier|For countries|^NA$")
    )
}

parse_ids_json_text <- function(txt) {
  raw <- jsonlite::fromJSON(txt, flatten = FALSE)
  if (is.null(raw$source$data) || length(raw$source$data) == 0) {
    return(tibble())
  }
  data <- raw$source$data
  if (nrow(data) == 0) return(tibble())

  purrr::map_dfr(seq_len(nrow(data)), function(i) {
    variables <- data$variable[[i]]
    concepts <- stats::setNames(variables$value, variables$concept)
    ids <- stats::setNames(variables$id, variables$concept)
    tibble(
      iso3 = ids[["Country"]],
      country = concepts[["Country"]],
      year = as.integer(sub("^YR", "", ids[["Time"]])),
      indicator_id = ids[["Series"]],
      indicator = concepts[["Series"]],
      creditor_id = ids[["Counterpart-Area"]],
      creditor_raw = trimws(gsub("\\s+", " ", concepts[["Counterpart-Area"]])),
      value = clean_num(data$value[[i]])
    )
  })
}

ids_page_count <- function(txt) {
  raw <- jsonlite::fromJSON(txt, flatten = FALSE)
  as.integer(raw$source$pages %||% 1L)
}

fetch_ids_counterpart_series <- function(series_id, creditor_id) {
  safe_series <- gsub("[^A-Za-z0-9]+", "_", series_id)
  cache_prefix <- file.path(ids_cache_dir, paste0("ids_", creditor_id, "_", safe_series))
  first_cache <- paste0(cache_prefix, "_page_1.json")

  fetch_page <- function(page) {
    cache_file <- paste0(cache_prefix, "_page_", page, ".json")
    if (file.exists(cache_file)) {
      return(paste(readLines(cache_file, warn = FALSE), collapse = "\n"))
    }

    url <- sprintf(
      "https://api.worldbank.org/v2/sources/6/country/all/series/%s/counterpart-area/%s/time/all",
      series_id,
      creditor_id
    )
    resp <- httr2::request(url) |>
      httr2::req_url_query(format = "json", per_page = 20000, page = page) |>
      httr2::req_timeout(90) |>
      httr2::req_perform()
    txt <- httr2::resp_body_string(resp)
    writeLines(txt, cache_file)
    txt
  }

  first_txt <- fetch_page(1L)
  pages <- ids_page_count(first_txt)
  texts <- c(first_txt, purrr::map_chr(seq.int(2L, pages), fetch_page))
  purrr::map_dfr(texts, parse_ids_json_text)
}

build_core_ids_terms_all_years <- function(country_metadata) {
  series_ids <- c("DT.INR.DPPG", "DT.MAT.DPPG", "DT.GPA.DPPG")
  counterpart_areas <- tibble(
    creditor_id = c("901", "905", "730", "BND"),
    creditor = c("IBRD", "IDA", "China", "Bondholders"),
    creditor_name = c(
      "International Bank for Reconstruction and Development",
      "International Development Association",
      "China",
      "Bondholders"
    ),
    creditor_scope = c("institution", "institution", "bilateral_or_country", "market_bondholders")
  )

  raw <- tryCatch(
    {
      purrr::map_dfr(counterpart_areas$creditor_id, function(cp) {
        purrr::map_dfr(series_ids, function(series) {
          message("Fetching/loading IDS ", cp, " ", series)
          fetch_ids_counterpart_series(series, cp)
        })
      })
    },
    error = function(e) {
      warning("IDS all-year API/cache load failed: ", conditionMessage(e))
      tibble()
    }
  )

  if (nrow(raw) == 0) {
    fallback <- readr::read_csv(path_ids_terms_2024, show_col_types = FALSE) |>
      filter(.data$creditor_id %in% counterpart_areas$creditor_id) |>
      mutate(
        ids_history_source = "local_2024_only_fallback",
        official_rate = clean_num(.data$official_rate),
        official_maturity_years = clean_num(.data$official_maturity_years),
        official_grace_years = clean_num(.data$official_grace_years),
        has_complete_terms = as.logical(.data$has_complete_terms)
      )
    return(fallback)
  }

  raw |>
    filter(.data$year %in% analysis_years) |>
    mutate(
      creditor = creditor_label(.data$creditor_id),
      term = term_label(.data$indicator_id)
    ) |>
    filter(.data$term %in% c("official_rate", "official_maturity_years", "official_grace_years")) |>
    select("iso3", "country", "year", "creditor", "creditor_id", "term", "value") |>
    group_by(.data$iso3, .data$country, .data$year, .data$creditor, .data$creditor_id, .data$term) |>
    summarise(
      value = if (all(is.na(.data$value))) NA_real_ else mean(.data$value, na.rm = TRUE),
      .groups = "drop"
    ) |>
    pivot_wider(names_from = "term", values_from = "value") |>
    left_join(counterpart_areas, by = c("creditor_id", "creditor")) |>
    left_join(
      country_metadata |> select("iso3", "income_level", "lending_type", "is_country"),
      by = "iso3"
    ) |>
    filter(.data$is_country) |>
    select(-"is_country") |>
    mutate(
      official_rate = clean_num(.data$official_rate),
      official_maturity_years = clean_num(.data$official_maturity_years),
      official_grace_years = clean_num(.data$official_grace_years),
      has_complete_terms = !is.na(.data$official_rate) &
        !is.na(.data$official_maturity_years) &
        !is.na(.data$official_grace_years) &
        .data$official_maturity_years > 0 &
        .data$official_grace_years >= 0 &
        .data$official_grace_years < .data$official_maturity_years,
      ids_history_source = "world_bank_ids_api_cached_all_years"
    ) |>
    arrange(.data$country, .data$year, .data$creditor)
}

country_metadata <- read_country_metadata(path_country_metadata) |>
  transmute(
    iso3,
    country_metadata_name = country,
    country_join = country_join_key(country),
    income_level,
    lending_type,
    is_country
  ) |>
  filter(.data$is_country)

country_year_universe <- tidyr::crossing(
  country_metadata |> select("iso3", "country" = "country_metadata_name", "income_level", "lending_type"),
  analysis_year = analysis_years
) |>
  mutate(country_key = country_key_from(.data$iso3, .data$country))

ids_terms_core <- build_core_ids_terms_all_years(country_metadata)

primary_raw <- readr::read_csv(path_primary, show_col_types = FALSE) |>
  mutate(
    IssueDate = as.Date(.data$IssueDate),
    MaturityDate = as.Date(.data$MaturityDate),
    analysis_year = as.integer(format(.data$IssueDate, "%Y")),
    CouponRate = clean_num(.data$CouponRate),
    FaceIssuedUSD = clean_num(.data$FaceIssuedUSD),
    FaceOutstandingUSD = clean_num(.data$FaceOutstandingUSD),
    IssuePrice = clean_num(.data$IssuePrice),
    MaturityStandardYield = clean_num(.data$MaturityStandardYield),
    yield_search_direct = clean_num(.data$yield_search_direct),
    yield_price_implied = clean_num(.data$yield_price_implied),
    yield_final_preference = clean_num(.data$yield_final_preference),
    original_maturity_days = clean_num(.data$original_maturity_days),
    benchmark_candidate_flag = as_logical_flag(.data$benchmark_candidate_flag),
    benchmark_hard_currency = as_logical_flag(.data$benchmark_hard_currency),
    benchmark_min_maturity_ok = as_logical_flag(.data$benchmark_min_maturity_ok),
    benchmark_exclude_like = as_logical_flag(.data$benchmark_exclude_like),
    flag_central_bank_like = as_logical_flag(.data$flag_central_bank_like),
    country = .data$oecd_country_request,
    country_join = country_join_key(.data$country),
    country_key = country_key_from(NA_character_, .data$country)
  ) |>
  filter(.data$analysis_year %in% analysis_years) |>
  left_join(country_metadata |> select("iso3", "country_join", "income_level", "lending_type"), by = "country_join") |>
  mutate(
    country_key = country_key_from(.data$iso3, .data$country),
    nonstandard_issue = classify_lseg_nonstandard_issue(
      .data$DocumentTitle,
      .data$DebtTypeDescription,
      .data$InstrumentTypeDescription,
      .data$RCSCouponTypeLeaf
    ),
    economic_issue_key = build_lseg_economic_issue_key(
      .data$country,
      .data$IssueDate,
      .data$MaturityDate,
      .data$Currency,
      .data$CouponRate,
      .data$FaceIssuedUSD
    ),
    has_final_yield = !is.na(.data$yield_final_preference),
    exclusion_or_review_flags = purrr::pmap_chr(
      list(
        .data$nonstandard_issue,
        .data$flag_central_bank_like,
        .data$benchmark_exclude_like,
        .data$IsCallable,
        .data$IsPutable,
        .data$IsSinkable,
        .data$IsConvertible,
        .data$IsPerpetualSecurity
      ),
      function(nonstandard, central_bank, exclude_like, callable, putable, sinkable, convertible, perpetual) {
        flags <- character()
        if (isTRUE(nonstandard)) flags <- c(flags, "nonstandard_coupon_or_structure")
        if (isTRUE(central_bank)) flags <- c(flags, "central_bank_like")
        if (isTRUE(exclude_like)) flags <- c(flags, "lseg_exclude_like")
        if (toupper(as.character(callable %||% "")) == "Y") flags <- c(flags, "callable_review")
        if (toupper(as.character(putable %||% "")) == "Y") flags <- c(flags, "putable_review")
        if (toupper(as.character(sinkable %||% "")) == "Y") flags <- c(flags, "sinkable_review")
        if (toupper(as.character(convertible %||% "")) == "Y") flags <- c(flags, "convertible_review")
        if (toupper(as.character(perpetual %||% "")) == "Y") flags <- c(flags, "perpetual_review")
        if (length(flags) == 0) "none" else paste(flags, collapse = ";")
      }
    )
  )

make_primary_scenario <- function(
  scenario_id,
  scenario_priority,
  allowed_currencies = c("USD", "EUR"),
  standard_only = TRUE,
  require_yield_source = NULL,
  require_min_issue_count = 1L,
  use_candidate_flag = TRUE,
  require_positive_amount = TRUE,
  min_issue_amount_usd = NULL,
  source_note_extra = ""
) {
  screened <- primary_raw |>
    mutate(
      scenario_included_row = TRUE,
      scenario_included_row = .data$scenario_included_row & .data$Currency %in% allowed_currencies,
      scenario_included_row = .data$scenario_included_row & !is.na(.data$original_maturity_days) & .data$original_maturity_days >= 365,
      scenario_included_row = if (use_candidate_flag) .data$scenario_included_row & .data$benchmark_candidate_flag else .data$scenario_included_row,
      scenario_included_row = if (standard_only) {
        .data$scenario_included_row &
          !.data$nonstandard_issue &
          !.data$flag_central_bank_like &
          !.data$benchmark_exclude_like
      } else {
        .data$scenario_included_row
      },
      scenario_included_row = if (require_positive_amount) {
        .data$scenario_included_row & !is.na(.data$FaceIssuedUSD) & .data$FaceIssuedUSD > 0
      } else {
        .data$scenario_included_row
      },
      scenario_included_row = if (!is.null(min_issue_amount_usd)) {
        .data$scenario_included_row & !is.na(.data$FaceIssuedUSD) & .data$FaceIssuedUSD >= min_issue_amount_usd
      } else {
        .data$scenario_included_row
      },
      below_materiality_threshold = if (!is.null(min_issue_amount_usd)) {
        !is.na(.data$FaceIssuedUSD) & .data$FaceIssuedUSD < min_issue_amount_usd
      } else {
        FALSE
      },
      scenario_included_row = .data$scenario_included_row & !is.na(.data$yield_final_preference),
      scenario_included_row = if (!is.null(require_yield_source)) {
        .data$scenario_included_row & .data$yield_source_final %in% require_yield_source
      } else {
        .data$scenario_included_row
      },
      scenario_row_status = case_when(
        !.data$Currency %in% allowed_currencies ~ "excluded_currency",
        is.na(.data$original_maturity_days) | .data$original_maturity_days < 365 ~ "excluded_short_maturity",
        use_candidate_flag & !.data$benchmark_candidate_flag ~ "excluded_not_lseg_candidate",
        standard_only & .data$nonstandard_issue ~ "excluded_nonstandard",
        standard_only & .data$flag_central_bank_like ~ "excluded_central_bank_like",
        standard_only & .data$benchmark_exclude_like ~ "excluded_lseg_exclude_like",
        require_positive_amount & (is.na(.data$FaceIssuedUSD) | .data$FaceIssuedUSD <= 0) ~ "excluded_missing_or_zero_amount",
        .data$below_materiality_threshold ~ "excluded_below_materiality_threshold",
        is.na(.data$yield_final_preference) ~ "excluded_missing_yield",
        !is.null(require_yield_source) & !.data$yield_source_final %in% require_yield_source ~ "excluded_other_yield_source",
        TRUE ~ "included"
      )
    )

  issue_level <- screened |>
    filter(.data$scenario_included_row) |>
    group_by(.data$analysis_year, .data$country, .data$country_key, .data$economic_issue_key) |>
    summarise(
      iso3 = first_non_missing(.data$iso3),
      income_level = first_non_missing(.data$income_level),
      lending_type = first_non_missing(.data$lending_type),
      issue_date = min(.data$IssueDate, na.rm = TRUE),
      maturity_date = max(.data$MaturityDate, na.rm = TRUE),
      currency = first_non_missing(.data$Currency),
      face_issued_usd = max_or_na(.data$FaceIssuedUSD),
      maturity_years = mean_or_na(as.numeric(difftime(.data$MaturityDate, .data$IssueDate, units = "days")) / 365.25),
      market_rate_pct = mean_or_na(.data$yield_final_preference),
      yield_source = collapse_values(.data$yield_source_final),
      row_statuses = collapse_values(.data$scenario_row_status),
      exclusion_or_review_flags = collapse_values(.data$exclusion_or_review_flags),
      included_isins = collapse_values(.data$ISIN),
      representative_titles = collapse_values(.data$DocumentTitle, sep = " || "),
      duplicate_row_count = n(),
      .groups = "drop"
    )

  issue_level |>
    group_by(.data$analysis_year, .data$country, .data$country_key) |>
    summarise(
      iso3 = first_non_missing(.data$iso3),
      income_level = first_non_missing(.data$income_level),
      lending_type = first_non_missing(.data$lending_type),
      scenario_id = scenario_id,
      ladder_component = "observed_primary_issuance",
      benchmark_source_tier = "observed_primary_issuance",
      method_family = "primary_market",
      scenario_priority = scenario_priority,
      market_rate_pct = weighted_mean_or_na(.data$market_rate_pct, .data$face_issued_usd),
      market_maturity_years = weighted_mean_or_na(.data$maturity_years, .data$face_issued_usd),
      benchmark_quality_band = quality_from_issue_count(n(), "observed_primary"),
      benchmark_status = status_from_issue_count(n()),
      benchmark_status_reason = paste0("computed_under_", scenario_id),
      market_rate_measure_basis = paste0("issue_amount_weighted_final_yield;", source_note_extra),
      currency_basis = if_else(n_distinct(.data$currency) > 1, "mixed_hard_currency", first_non_missing(.data$currency)),
      weighting_variable = "FaceIssuedUSD",
      total_weight_usd = sum_or_na(.data$face_issued_usd),
      issue_count = as.integer(n()),
      included_issue_count = as.integer(n()),
      eligible_issue_count = as.integer(n()),
      yield_source = collapse_values(.data$yield_source),
      included_isins = paste(.data$included_isins, collapse = " || "),
      included_issue_keys = paste(.data$economic_issue_key, collapse = " || "),
      first_issue_date = min(.data$issue_date, na.rm = TRUE),
      last_issue_date = max(.data$issue_date, na.rm = TRUE),
      source_artifact = path_primary,
      source_note = paste(
        "Experimental all-years LSEG v14 primary-market scenario.",
        source_note_extra,
        "Pending approvals are represented as separate scenarios, not as blockers."
      ),
      source_file = basename(path_primary),
      source_url_or_path = path_primary,
      source_extraction_run_id = collapse_values(.data$analysis_year),
      source_license_class = "LSEG_workspace_export_restricted",
      headline_admissible = if_else(n() >= 2, "pending_rule_review", "no_thin_single_or_experimental"),
      included_in_headline_results = "no",
      method_warning = "experimental_not_accepted; primary screen and yield precedence not approved for canonical outputs",
      decision_status = "experimental_pending_method_decision",
      .groups = "drop"
    ) |>
    filter(.data$issue_count >= require_min_issue_count) |>
    standard_output_columns()
}

primary_scenarios <- bind_rows(
  make_primary_scenario(
    "primary_usd_eur_final_yield_standard_all_issue_counts",
    10L,
    source_note_extra = "USD/EUR standard issue screen, final-yield preference, all issue counts."
  ),
  make_primary_scenario(
    "primary_usd_only_final_yield_standard_all_issue_counts",
    11L,
    allowed_currencies = "USD",
    source_note_extra = "USD-only standard issue screen, final-yield preference, all issue counts."
  ),
  make_primary_scenario(
    "primary_usd_eur_direct_yield_only_standard",
    12L,
    require_yield_source = "search_direct_fallback",
    source_note_extra = "USD/EUR standard issue screen, direct-yield rows only."
  ),
  make_primary_scenario(
    "primary_usd_eur_price_implied_only_standard",
    13L,
    require_yield_source = "price_implied_primary",
    source_note_extra = "USD/EUR standard issue screen, issue-price-implied rows only."
  ),
  make_primary_scenario(
    "primary_usd_eur_two_plus_standard",
    14L,
    require_min_issue_count = 2L,
    source_note_extra = "USD/EUR standard issue screen, requiring at least two economic issues."
  ),
  make_primary_scenario(
    "primary_usd_eur_multi_issue_only_standard",
    15L,
    require_min_issue_count = 3L,
    source_note_extra = "USD/EUR standard issue screen, requiring at least three economic issues."
  ),
  make_primary_scenario(
    "primary_usd_eur_final_yield_standard_issue50m_ge1",
    16L,
    min_issue_amount_usd = 50000000,
    source_note_extra = "USD/EUR standard issue screen, final-yield preference, excluding issues below USD 50 million."
  ),
  make_primary_scenario(
    "primary_usd_eur_final_yield_standard_issue50m_ge2",
    17L,
    min_issue_amount_usd = 50000000,
    require_min_issue_count = 2L,
    source_note_extra = "USD/EUR standard issue screen, final-yield preference, excluding issues below USD 50 million and requiring at least two economic issues."
  ),
  make_primary_scenario(
    "primary_usd_eur_include_nonstandard_flagged",
    18L,
    standard_only = FALSE,
    use_candidate_flag = TRUE,
    source_note_extra = "USD/EUR LSEG candidate rows with nonstandard and review-flagged rows included for sensitivity."
  ),
  make_primary_scenario(
    "primary_usd_eur_permissive_all_yielding_hard_currency",
    19L,
    standard_only = FALSE,
    use_candidate_flag = FALSE,
    source_note_extra = "Permissive USD/EUR hard-currency scenario, including rows outside the LSEG candidate flag if yield and amount exist."
  )
)

primary_row_status <- primary_raw |>
  mutate(
    baseline_status = case_when(
      !.data$benchmark_candidate_flag ~ "excluded_not_lseg_candidate",
      !.data$Currency %in% c("USD", "EUR") ~ "excluded_currency",
      is.na(.data$original_maturity_days) | .data$original_maturity_days < 365 ~ "excluded_short_maturity",
      .data$flag_central_bank_like ~ "excluded_central_bank_like",
      .data$benchmark_exclude_like ~ "excluded_lseg_exclude_like",
      is.na(.data$yield_final_preference) ~ "excluded_missing_yield",
      .data$nonstandard_issue ~ "excluded_nonstandard",
      is.na(.data$FaceIssuedUSD) | .data$FaceIssuedUSD <= 0 ~ "excluded_missing_or_zero_amount",
      TRUE ~ "included_standard_primary"
    )
  ) |>
  count(.data$analysis_year, .data$country, .data$country_key, .data$iso3, .data$income_level, .data$lending_type, .data$baseline_status, name = "rows") |>
  arrange(.data$analysis_year, .data$country, .data$baseline_status)

primary_issue_level_all_years <- primary_raw |>
  mutate(
    baseline_status = case_when(
      !.data$benchmark_candidate_flag ~ "excluded_not_lseg_candidate",
      !.data$Currency %in% c("USD", "EUR") ~ "excluded_currency",
      is.na(.data$original_maturity_days) | .data$original_maturity_days < 365 ~ "excluded_short_maturity",
      .data$flag_central_bank_like ~ "excluded_central_bank_like",
      .data$benchmark_exclude_like ~ "excluded_lseg_exclude_like",
      is.na(.data$yield_final_preference) ~ "excluded_missing_yield",
      .data$nonstandard_issue ~ "excluded_nonstandard",
      is.na(.data$FaceIssuedUSD) | .data$FaceIssuedUSD <= 0 ~ "excluded_missing_or_zero_amount",
      TRUE ~ "included_standard_primary"
    )
  ) |>
  group_by(.data$analysis_year, .data$country, .data$country_key, .data$economic_issue_key) |>
  summarise(
    iso3 = first_non_missing(.data$iso3),
    income_level = first_non_missing(.data$income_level),
    lending_type = first_non_missing(.data$lending_type),
    issue_date = min(.data$IssueDate, na.rm = TRUE),
    maturity_date = max(.data$MaturityDate, na.rm = TRUE),
    currency = first_non_missing(.data$Currency),
    coupon_rate = mean_or_na(.data$CouponRate),
    face_issued_usd = max_or_na(.data$FaceIssuedUSD),
    face_outstanding_usd = max_or_na(.data$FaceOutstandingUSD),
    issue_price = mean_or_na(.data$IssuePrice),
    maturity_years = mean_or_na(as.numeric(difftime(.data$MaturityDate, .data$IssueDate, units = "days")) / 365.25),
    yield_final_preference = mean_or_na(.data$yield_final_preference),
    yield_search_direct = mean_or_na(.data$yield_search_direct),
    yield_price_implied = mean_or_na(.data$yield_price_implied),
    yield_source_final = collapse_values(.data$yield_source_final),
    baseline_status = collapse_values(.data$baseline_status),
    nonstandard_issue = any(.data$nonstandard_issue, na.rm = TRUE),
    exclusion_or_review_flags = collapse_values(.data$exclusion_or_review_flags),
    representative_isins = collapse_values(.data$ISIN),
    representative_rics = collapse_values(.data$RIC),
    representative_titles = collapse_values(.data$DocumentTitle, sep = " || "),
    source_extraction_run_id = collapse_values(.data$run_id),
    duplicate_row_count = n(),
    .groups = "drop"
  ) |>
  arrange(.data$analysis_year, .data$country, .data$issue_date, .data$maturity_date)

secondary_snap <- readr::read_csv(path_secondary_snap, show_col_types = FALSE) |>
  mutate(
    IssueDate = as.Date(.data$IssueDate),
    MaturityDate = as.Date(.data$MaturityDate),
    CouponRate = clean_num(.data$CouponRate),
    FaceIssuedUSD = clean_num(.data$FaceIssuedUSD),
    FaceOutstandingUSD = clean_num(.data$FaceOutstandingUSD),
    yield_final_preference = clean_num(.data$yield_final_preference),
    original_maturity_days = clean_num(.data$original_maturity_days),
    remaining_maturity_years = clean_num(.data$remaining_maturity_years),
    snapshot_year = as.integer(.data$snapshot_year),
    benchmark_candidate_flag = as_logical_flag(.data$benchmark_candidate_flag),
    benchmark_exclude_like = as_logical_flag(.data$benchmark_exclude_like),
    flag_central_bank_like = as_logical_flag(.data$flag_central_bank_like),
    country = .data$oecd_country_request,
    country_join = country_join_key(.data$country),
    country_key = country_key_from(NA_character_, .data$country),
    instrument_id = .data$RIC,
    nonstandard_issue = classify_lseg_nonstandard_issue(
      .data$DocumentTitle,
      .data$DebtTypeDescription,
      .data$InstrumentTypeDescription,
      .data$RCSCouponTypeLeaf
    ),
    economic_issue_key = build_lseg_economic_issue_key(
      .data$country,
      .data$IssueDate,
      .data$MaturityDate,
      .data$Currency,
      .data$CouponRate,
      .data$FaceIssuedUSD
    )
  ) |>
  filter(.data$snapshot_year %in% analysis_years) |>
  left_join(country_metadata |> select("iso3", "country_join", "income_level", "lending_type"), by = "country_join") |>
  mutate(country_key = country_key_from(.data$iso3, .data$country))

secondary_hist <- readr::read_csv(path_secondary_hist, show_col_types = FALSE, guess_max = 1000, name_repair = "unique") |>
  mutate(
    Date = as.Date(.data$Date),
    history_year = as.integer(.data$history_year)
  ) |>
  filter(.data$history_year %in% analysis_years)

secondary_yield_cols <- names(secondary_hist)[stringr::str_detect(names(secondary_hist), "__Yield to Maturity")]

secondary_latest_yields <- secondary_hist |>
  select("Date", "history_year", all_of(secondary_yield_cols)) |>
  pivot_longer(
    cols = -c("Date", "history_year"),
    names_to = "history_column",
    values_to = "secondary_yield_pct"
  ) |>
  mutate(
    secondary_yield_pct = clean_num(.data$secondary_yield_pct),
    instrument_id = stringr::str_remove(.data$history_column, "__Yield to Maturity.*$")
  ) |>
  filter(!is.na(.data$secondary_yield_pct), !is.na(.data$Date), .data$instrument_id != "") |>
  arrange(.data$history_year, .data$instrument_id, desc(.data$Date)) |>
  group_by(.data$history_year, .data$instrument_id) |>
  slice(1) |>
  ungroup() |>
  transmute(
    analysis_year = .data$history_year,
    instrument_id,
    secondary_quote_date = .data$Date,
    secondary_yield_pct
  )

secondary_issue_level_all_years <- secondary_snap |>
  left_join(secondary_latest_yields, by = c("snapshot_year" = "analysis_year", "instrument_id")) |>
  mutate(
    baseline_status = case_when(
      !.data$benchmark_candidate_flag ~ "excluded_not_lseg_candidate",
      !.data$Currency %in% c("USD", "EUR") ~ "excluded_currency",
      is.na(.data$remaining_maturity_years) | .data$remaining_maturity_years < 1 ~ "excluded_short_residual_maturity",
      .data$flag_central_bank_like ~ "excluded_central_bank_like",
      .data$benchmark_exclude_like ~ "excluded_lseg_exclude_like",
      .data$nonstandard_issue ~ "excluded_nonstandard",
      is.na(.data$FaceOutstandingUSD) | .data$FaceOutstandingUSD <= 0 ~ "excluded_missing_or_zero_outstanding",
      is.na(.data$secondary_yield_pct) ~ "excluded_missing_direct_ytm",
      TRUE ~ "included_standard_secondary"
    )
  ) |>
  group_by(.data$snapshot_year, .data$country, .data$country_key, .data$economic_issue_key) |>
  summarise(
    analysis_year = first(.data$snapshot_year),
    iso3 = first_non_missing(.data$iso3),
    income_level = first_non_missing(.data$income_level),
    lending_type = first_non_missing(.data$lending_type),
    issue_date = min(.data$IssueDate, na.rm = TRUE),
    maturity_date = max(.data$MaturityDate, na.rm = TRUE),
    currency = first_non_missing(.data$Currency),
    coupon_rate = mean_or_na(.data$CouponRate),
    face_issued_usd = max_or_na(.data$FaceIssuedUSD),
    face_outstanding_usd = max_or_na(.data$FaceOutstandingUSD),
    remaining_maturity_years = max_or_na(.data$remaining_maturity_years),
    secondary_yield_pct = mean_or_na(.data$secondary_yield_pct),
    secondary_quote_date = if (all(is.na(.data$secondary_quote_date))) as.Date(NA) else max(.data$secondary_quote_date, na.rm = TRUE),
    baseline_status = collapse_values(.data$baseline_status),
    nonstandard_issue = any(.data$nonstandard_issue, na.rm = TRUE),
    representative_isins = collapse_values(.data$ISIN),
    representative_rics = collapse_values(.data$RIC),
    representative_titles = collapse_values(.data$DocumentTitle, sep = " || "),
    duplicate_row_count = n(),
    .groups = "drop"
  ) |>
  arrange(.data$analysis_year, .data$country, .data$remaining_maturity_years)

make_secondary_scenario <- function(
  scenario_id,
  scenario_priority,
  allowed_currencies = c("USD", "EUR"),
  standard_only = TRUE
) {
  secondary_snap |>
    left_join(secondary_latest_yields, by = c("snapshot_year" = "analysis_year", "instrument_id")) |>
    mutate(
      scenario_included_row =
        .data$Currency %in% allowed_currencies &
        !is.na(.data$remaining_maturity_years) & .data$remaining_maturity_years >= 1 &
        !is.na(.data$FaceOutstandingUSD) & .data$FaceOutstandingUSD > 0 &
        !is.na(.data$secondary_yield_pct),
      scenario_included_row = if (standard_only) {
        .data$scenario_included_row &
          .data$benchmark_candidate_flag &
          !.data$nonstandard_issue &
          !.data$flag_central_bank_like &
          !.data$benchmark_exclude_like
      } else {
        .data$scenario_included_row
      }
    ) |>
    filter(.data$scenario_included_row) |>
    group_by(.data$snapshot_year, .data$country, .data$country_key, .data$economic_issue_key) |>
    summarise(
      iso3 = first_non_missing(.data$iso3),
      income_level = first_non_missing(.data$income_level),
      lending_type = first_non_missing(.data$lending_type),
      currency = first_non_missing(.data$Currency),
      face_outstanding_usd = max_or_na(.data$FaceOutstandingUSD),
      remaining_maturity_years = max_or_na(.data$remaining_maturity_years),
      secondary_yield_pct = mean_or_na(.data$secondary_yield_pct),
      secondary_quote_date = max(.data$secondary_quote_date, na.rm = TRUE),
      included_isins = collapse_values(.data$ISIN),
      representative_rics = collapse_values(.data$RIC),
      duplicate_row_count = n(),
      .groups = "drop"
    ) |>
    group_by(.data$snapshot_year, .data$country, .data$country_key) |>
    summarise(
      analysis_year = first(.data$snapshot_year),
      iso3 = first_non_missing(.data$iso3),
      income_level = first_non_missing(.data$income_level),
      lending_type = first_non_missing(.data$lending_type),
      scenario_id = scenario_id,
      ladder_component = "secondary_market_evidence",
      benchmark_source_tier = "observed_secondary_market",
      method_family = "secondary_market",
      scenario_priority = scenario_priority,
      market_rate_pct = weighted_mean_or_na(.data$secondary_yield_pct, .data$face_outstanding_usd),
      market_maturity_years = weighted_mean_or_na(.data$remaining_maturity_years, .data$face_outstanding_usd),
      benchmark_quality_band = quality_from_issue_count(n(), "secondary_market"),
      benchmark_status = status_from_issue_count(n()),
      benchmark_status_reason = paste0("computed_under_", scenario_id),
      market_rate_measure_basis = "year_end_direct_ytm_outstanding_weighted",
      currency_basis = if_else(n_distinct(.data$currency) > 1, "mixed_hard_currency", first_non_missing(.data$currency)),
      weighting_variable = "FaceOutstandingUSD",
      total_weight_usd = sum_or_na(.data$face_outstanding_usd),
      issue_count = as.integer(n()),
      included_issue_count = as.integer(n()),
      eligible_issue_count = as.integer(n()),
      yield_source = "LSEG direct Yield to Maturity history, latest nonmissing observation in year-end window",
      included_isins = paste(.data$included_isins, collapse = " || "),
      included_issue_keys = paste(.data$economic_issue_key, collapse = " || "),
      last_quote_date = max(.data$secondary_quote_date, na.rm = TRUE),
      source_artifact = paste(path_secondary_snap, path_secondary_hist, sep = ";"),
      source_file = paste(basename(path_secondary_snap), basename(path_secondary_hist), sep = ";"),
      source_url_or_path = paste(path_secondary_snap, path_secondary_hist, sep = ";"),
      source_extraction_run_id = "LSEG_v14_secondary_history",
      source_license_class = "LSEG_workspace_export_restricted",
      headline_admissible = "no_deferred_tier",
      included_in_headline_results = "no",
      source_note = paste0(
        "Experimental all-years secondary-market scenario using direct YTM fields only; ",
        if (standard_only) "standard candidate screen." else "permissive flagged screen."
      ),
      method_warning = "experimental_not_accepted; secondary tier not approved for canonical outputs",
      decision_status = "experimental_pending_method_decision",
      .groups = "drop"
    ) |>
    standard_output_columns()
}

make_secondary_closest_tenor_scenario <- function(target_tenor_years, scenario_id, scenario_priority) {
  eligible <- secondary_issue_level_all_years |>
    filter(
      stringr::str_detect(.data$baseline_status, "included_standard_secondary"),
      !is.na(.data$secondary_yield_pct),
      !is.na(.data$remaining_maturity_years),
      !is.na(.data$face_outstanding_usd),
      .data$face_outstanding_usd > 0
    ) |>
    group_by(.data$analysis_year, .data$country, .data$country_key) |>
    mutate(
      eligible_issue_count = n(),
      tenor_distance = abs(.data$remaining_maturity_years - target_tenor_years)
    ) |>
    arrange(.data$analysis_year, .data$country_key, .data$tenor_distance, desc(.data$face_outstanding_usd)) |>
    slice(1) |>
    ungroup()

  eligible |>
    transmute(
      analysis_year,
      iso3,
      country,
      country_key,
      income_level,
      lending_type,
      scenario_id = scenario_id,
      ladder_component = "secondary_market_evidence",
      benchmark_source_tier = "observed_secondary_market",
      method_family = "secondary_market_closest_tenor",
      scenario_priority = scenario_priority,
      market_rate_pct = .data$secondary_yield_pct,
      market_maturity_years = .data$remaining_maturity_years,
      benchmark_quality_band = paste0("secondary_market_closest_", target_tenor_years, "y"),
      benchmark_status = "computed_experimental_closest_tenor",
      benchmark_status_reason = paste0("closest_available_secondary_issue_to_", target_tenor_years, "y"),
      market_rate_measure_basis = paste0("single_issue_closest_to_", target_tenor_years, "y_direct_ytm"),
      currency_basis = .data$currency,
      weighting_variable = "closest_tenor_then_largest_outstanding",
      total_weight_usd = .data$face_outstanding_usd,
      issue_count = 1L,
      included_issue_count = 1L,
      eligible_issue_count = as.integer(.data$eligible_issue_count),
      yield_source = "LSEG direct Yield to Maturity history, closest-tenor selection",
      included_isins = .data$representative_isins,
      included_issue_keys = .data$economic_issue_key,
      source_artifact = paste(path_secondary_snap, path_secondary_hist, sep = ";"),
      source_file = paste(basename(path_secondary_snap), basename(path_secondary_hist), sep = ";"),
      source_url_or_path = paste(path_secondary_snap, path_secondary_hist, sep = ";"),
      source_extraction_run_id = "LSEG_v14_secondary_history",
      source_license_class = "LSEG_workspace_export_restricted",
      headline_admissible = "no_deferred_tier",
      included_in_headline_results = "no",
      source_note = paste0("Experimental closest-tenor secondary scenario targeting ", target_tenor_years, " years."),
      method_warning = "experimental_not_accepted; secondary closest-tenor rule not approved for canonical outputs",
      decision_status = "experimental_pending_method_decision"
    ) |>
    standard_output_columns()
}

secondary_scenarios <- bind_rows(
  make_secondary_scenario("secondary_direct_ytm_standard_outstanding_weighted", 30L),
  make_secondary_scenario("secondary_direct_ytm_usd_only_standard", 31L, allowed_currencies = "USD"),
  make_secondary_scenario("secondary_direct_ytm_include_nonstandard_flagged", 32L, standard_only = FALSE),
  make_secondary_closest_tenor_scenario(7, "secondary_closest_7y_standard", 33L),
  make_secondary_closest_tenor_scenario(10, "secondary_closest_10y_standard", 34L)
)

ids_bondholder_scenario <- ids_terms_core |>
  filter(.data$creditor == "Bondholders", .data$has_complete_terms) |>
  transmute(
    analysis_year = .data$year,
    iso3,
    country,
    country_key = country_key_from(.data$iso3, .data$country),
    income_level,
    lending_type,
    scenario_id = "ids_bondholder_contractual_proxy",
    ladder_component = "ids_bondholder_proxy",
    benchmark_source_tier = "ids_bondholder_public_proxy",
    method_family = "ids_contractual_proxy",
    scenario_priority = 20L,
    market_rate_pct = .data$official_rate,
    market_maturity_years = .data$official_maturity_years,
    benchmark_quality_band = "ids_average_contractual_proxy",
    benchmark_status = "computed_experimental_proxy",
    benchmark_status_reason = "ids_bondholder_terms_available",
    market_rate_measure_basis = "ids_average_contractual_commitment_rate",
    currency_basis = "mixed_or_unspecified_ids",
    weighting_variable = NA_character_,
    total_weight_usd = NA_real_,
    issue_count = NA_integer_,
    included_issue_count = NA_integer_,
    eligible_issue_count = NA_integer_,
    yield_source = "World Bank IDS source 6 bondholder terms",
    source_artifact = paste0("World Bank IDS API/cache; ", ids_history_source),
    source_file = "World Bank IDS source 6 API/cache",
    source_url_or_path = ids_cache_dir,
    source_extraction_run_id = ids_history_source,
    source_license_class = "World_Bank_public_API",
    headline_admissible = "pending_rule_review",
    included_in_headline_results = "no",
    source_note = "Experimental IDS public fallback candidate. This is an average contractual proxy, not an issue-level market yield.",
    method_warning = "experimental_not_accepted; IDS fallback and materiality threshold not approved for canonical outputs",
    decision_status = "experimental_pending_method_decision"
  ) |>
  standard_output_columns()

fred_dgs7_annual <- readr::read_csv(path_fred_dgs7, show_col_types = FALSE) |>
  mutate(
    observation_date = as.Date(.data$observation_date),
    analysis_year = as.integer(format(.data$observation_date, "%Y")),
    DGS7 = clean_num(.data$DGS7)
  ) |>
  filter(.data$analysis_year %in% analysis_years) |>
  group_by(.data$analysis_year) |>
  summarise(risk_free_7y_pct = mean_or_na(.data$DGS7), .groups = "drop")

damodaran_regional <- read_excel_with_first_row_headers(path_damodaran, "Regional breakdown") |>
  transmute(
    damodaran_country = as.character(.data$Country),
    country_join = country_join_key(.data$damodaran_country),
    damodaran_moodys_rating = as.character(.data$Moody.s.rating),
    damodaran_region = as.character(.data$Region),
    default_spread_pct = clean_num(.data$Adj..Default.Spread) * 100
  ) |>
  filter(!is.na(.data$damodaran_country), .data$damodaran_country != "", !is.na(.data$default_spread_pct)) |>
  left_join(country_metadata |> select("iso3", "country" = "country_metadata_name", "country_join", "income_level", "lending_type"), by = "country_join") |>
  filter(!is.na(.data$iso3))

damodaran_archive_specs <- download_damodaran_archive_files()

damodaran_archive_spreads <- purrr::map2_dfr(
  damodaran_archive_specs$local_path[damodaran_archive_specs$downloaded],
  damodaran_archive_specs$analysis_year[damodaran_archive_specs$downloaded],
  parse_damodaran_archive_file
) |>
  mutate(
    country_join = country_join_key(.data$damodaran_country),
    archive_vintage_label = paste0("Damodaran ctryprem", stringr::str_sub(as.character(.data$analysis_year), 3, 4), " archive")
  ) |>
  left_join(country_metadata |> select("iso3", "country" = "country_metadata_name", "country_join", "income_level", "lending_type"), by = "country_join") |>
  filter(!is.na(.data$iso3), .data$analysis_year %in% analysis_years)

rating_implied_archive_scenario <- damodaran_archive_spreads |>
  inner_join(fred_dgs7_annual, by = "analysis_year") |>
  transmute(
    analysis_year,
    iso3,
    country,
    country_key = country_key_from(.data$iso3, .data$country),
    income_level,
    lending_type,
    scenario_id = "rating_implied_damodaran_archive_vintage_dgs7",
    ladder_component = "rating_implied_evidence",
    benchmark_source_tier = "rating_implied_damodaran_archive_vintage",
    method_family = "rating_implied_model",
    scenario_priority = 39L,
    market_rate_pct = .data$risk_free_7y_pct + .data$default_spread_pct,
    market_maturity_years = 7,
    benchmark_quality_band = "rating_implied_archive_vintage",
    benchmark_status = "computed_experimental_model",
    benchmark_status_reason = "damodaran_annual_archive_spread_plus_annual_fred_dgs7",
    market_rate_measure_basis = "annual_average_fred_dgs7_plus_damodaran_annual_archive_adjusted_default_spread",
    currency_basis = "USD_model_implied",
    weighting_variable = NA_character_,
    total_weight_usd = NA_real_,
    issue_count = NA_integer_,
    included_issue_count = NA_integer_,
    eligible_issue_count = NA_integer_,
    yield_source = paste0("FRED DGS7 annual average; ", .data$archive_vintage_label, "; rating=", .data$damodaran_rating),
    source_artifact = paste(path_fred_dgs7, path_damodaran_archive_dir, sep = ";"),
    source_file = paste0("ctryprem", stringr::str_sub(as.character(.data$analysis_year), 3, 4), ";fred_dgs7.csv"),
    source_url_or_path = paste0("https://pages.stern.nyu.edu/~adamodar/pc/archives/ctryprem", stringr::str_sub(as.character(.data$analysis_year), 3, 4), if_else(.data$analysis_year <= 2020, ".xls", ".xlsx")),
    source_extraction_run_id = .data$archive_parser,
    source_license_class = "Damodaran_public_archive_plus_FRED_public",
    headline_admissible = "no_deferred_model_tier",
    included_in_headline_results = "no",
    source_note = paste0("Damodaran archive parser=", .data$archive_parser, "; region=", .data$damodaran_region),
    method_warning = "experimental_not_accepted; annual Damodaran archive vintage improves historical spread timing but remains a model-implied fallback, not observed issuance",
    decision_status = "experimental_pending_method_decision"
  ) |>
  standard_output_columns()

rating_implied_scenario <- damodaran_regional |>
  tidyr::crossing(fred_dgs7_annual) |>
  transmute(
    analysis_year,
    iso3,
    country,
    country_key = country_key_from(.data$iso3, .data$country),
    income_level,
    lending_type,
    scenario_id = "rating_implied_damodaran_current_vintage_dgs7",
    ladder_component = "rating_implied_evidence",
    benchmark_source_tier = "rating_implied_current_vintage",
    method_family = "rating_implied_model",
    scenario_priority = 40L,
    market_rate_pct = .data$risk_free_7y_pct + .data$default_spread_pct,
    market_maturity_years = 7,
    benchmark_quality_band = "rating_implied_current_vintage",
    benchmark_status = "computed_experimental_model",
    benchmark_status_reason = "damodaran_current_vintage_country_spread_plus_annual_fred_dgs7",
    market_rate_measure_basis = "annual_average_fred_dgs7_plus_damodaran_adjusted_default_spread",
    currency_basis = "USD_model_implied",
    weighting_variable = NA_character_,
    total_weight_usd = NA_real_,
    issue_count = NA_integer_,
    included_issue_count = NA_integer_,
    yield_source = paste0("FRED DGS7 annual average; Damodaran current-vintage adjusted default spread; rating=", .data$damodaran_moodys_rating),
    source_artifact = paste(path_fred_dgs7, path_damodaran, sep = ";"),
    source_note = paste0("Region=", .data$damodaran_region),
    method_warning = "experimental_not_accepted; current Damodaran 2025/2026 vintage repeated across all years; not canonical phase 1",
    decision_status = "experimental_pending_method_decision"
  ) |>
  standard_output_columns()

damodaran_prs <- read_excel_with_first_row_headers(path_damodaran, "PRS Worksheet") |>
  transmute(
    damodaran_country = as.character(.data$Country),
    country_join = country_join_key(.data$damodaran_country),
    prs_score = clean_num(.data$PRS.Score),
    prs_default_spread_pct = clean_num(.data$Rating.based.Default.Spread) * 100
  ) |>
  filter(!is.na(.data$damodaran_country), .data$damodaran_country != "", !is.na(.data$prs_default_spread_pct)) |>
  left_join(country_metadata |> select("iso3", "country" = "country_metadata_name", "country_join", "income_level", "lending_type"), by = "country_join") |>
  filter(!is.na(.data$iso3))

shadow_prs_scenario <- damodaran_prs |>
  tidyr::crossing(fred_dgs7_annual) |>
  transmute(
    analysis_year,
    iso3,
    country,
    country_key = country_key_from(.data$iso3, .data$country),
    income_level,
    lending_type,
    scenario_id = "shadow_prs_damodaran_current_vintage_dgs7",
    ladder_component = "shadow_rating_or_fundamental_model",
    benchmark_source_tier = "shadow_prs_current_vintage",
    method_family = "shadow_prs_model",
    scenario_priority = 50L,
    market_rate_pct = .data$risk_free_7y_pct + .data$prs_default_spread_pct,
    market_maturity_years = 7,
    benchmark_quality_band = "shadow_prs_current_vintage",
    benchmark_status = "computed_experimental_model",
    benchmark_status_reason = "damodaran_prs_current_vintage_spread_plus_annual_fred_dgs7",
    market_rate_measure_basis = "annual_average_fred_dgs7_plus_damodaran_prs_rating_based_default_spread",
    currency_basis = "USD_model_implied",
    weighting_variable = NA_character_,
    total_weight_usd = NA_real_,
    issue_count = NA_integer_,
    included_issue_count = NA_integer_,
    yield_source = paste0("FRED DGS7 annual average; Damodaran PRS worksheet; PRS score=", .data$prs_score),
    source_artifact = paste(path_fred_dgs7, path_damodaran, sep = ";"),
    source_note = "Experimental shadow/PRS fallback computed only as a sensitivity database layer.",
    method_warning = "experimental_not_accepted; current Damodaran PRS vintage repeated across all years; no validation or uncertainty rule",
    decision_status = "experimental_pending_method_decision"
  ) |>
  standard_output_columns()

observed_for_peer <- bind_rows(
  primary_scenarios |> filter(.data$scenario_id == "primary_usd_eur_final_yield_standard_all_issue_counts"),
  secondary_scenarios |> filter(.data$scenario_id == "secondary_direct_ytm_standard_outstanding_weighted"),
  ids_bondholder_scenario
) |>
  filter(!is.na(.data$market_rate_pct), !is.na(.data$market_maturity_years))

peer_primary_medians <- primary_scenarios |>
  filter(.data$scenario_id == "primary_usd_eur_final_yield_standard_all_issue_counts") |>
  group_by(.data$analysis_year, .data$income_level) |>
  summarise(
    peer_rate_pct = median(.data$market_rate_pct, na.rm = TRUE),
    peer_maturity_years = median(.data$market_maturity_years, na.rm = TRUE),
    peer_count = n(),
    .groups = "drop"
  ) |>
  filter(!is.na(.data$peer_rate_pct), .data$peer_count > 0)

peer_best_medians <- observed_for_peer |>
  group_by(.data$analysis_year, .data$income_level) |>
  summarise(
    peer_rate_pct = median(.data$market_rate_pct, na.rm = TRUE),
    peer_maturity_years = median(.data$market_maturity_years, na.rm = TRUE),
    peer_count = n(),
    peer_source_mix = collapse_values(.data$scenario_id),
    .groups = "drop"
  ) |>
  filter(!is.na(.data$peer_rate_pct), .data$peer_count > 0)

peer_primary_scenario <- country_year_universe |>
  left_join(peer_primary_medians, by = c("analysis_year", "income_level")) |>
  filter(!is.na(.data$peer_rate_pct)) |>
  transmute(
    analysis_year,
    iso3,
    country,
    country_key,
    income_level,
    lending_type,
    scenario_id = "peer_income_year_median_observed_primary",
    ladder_component = "peer_proxy",
    benchmark_source_tier = "peer_proxy_primary_income_year",
    method_family = "peer_proxy",
    scenario_priority = 60L,
    market_rate_pct = .data$peer_rate_pct,
    market_maturity_years = .data$peer_maturity_years,
    benchmark_quality_band = if_else(.data$peer_count >= 3, "peer_proxy_three_plus_peers", "peer_proxy_thin_peers"),
    benchmark_status = "computed_experimental_proxy",
    benchmark_status_reason = paste0("income_year_median_primary_peer_count_", .data$peer_count),
    market_rate_measure_basis = "income_level_year_median_observed_primary_rate",
    currency_basis = "peer_median_mixed_hard_currency",
    weighting_variable = "country_peer_median",
    total_weight_usd = NA_real_,
    issue_count = as.integer(.data$peer_count),
    included_issue_count = as.integer(.data$peer_count),
    yield_source = "peer median from primary_usd_eur_final_yield_standard_all_issue_counts",
    source_artifact = "derived from primary scenario table",
    source_note = "Experimental peer proxy. Country itself is not excluded from the peer median in this exploratory database.",
    method_warning = "experimental_not_accepted; peer selection, self-exclusion, tenor normalization, and uncertainty rule not approved",
    decision_status = "experimental_pending_method_decision"
  ) |>
  standard_output_columns()

peer_best_scenario <- country_year_universe |>
  left_join(peer_best_medians, by = c("analysis_year", "income_level")) |>
  filter(!is.na(.data$peer_rate_pct)) |>
  transmute(
    analysis_year,
    iso3,
    country,
    country_key,
    income_level,
    lending_type,
    scenario_id = "peer_income_year_median_observed_plus_ids",
    ladder_component = "peer_proxy",
    benchmark_source_tier = "peer_proxy_observed_plus_ids_income_year",
    method_family = "peer_proxy",
    scenario_priority = 61L,
    market_rate_pct = .data$peer_rate_pct,
    market_maturity_years = .data$peer_maturity_years,
    benchmark_quality_band = if_else(.data$peer_count >= 3, "peer_proxy_three_plus_peers", "peer_proxy_thin_peers"),
    benchmark_status = "computed_experimental_proxy",
    benchmark_status_reason = paste0("income_year_median_observed_plus_ids_peer_count_", .data$peer_count),
    market_rate_measure_basis = "income_level_year_median_observed_primary_secondary_ids_rate",
    currency_basis = "peer_median_mixed",
    weighting_variable = "country_peer_median",
    total_weight_usd = NA_real_,
    issue_count = as.integer(.data$peer_count),
    included_issue_count = as.integer(.data$peer_count),
    yield_source = paste0("peer source mix: ", .data$peer_source_mix),
    source_artifact = "derived from primary, secondary, and IDS scenario tables",
    source_note = "Experimental peer proxy. Country itself is not excluded from the peer median in this exploratory database.",
    method_warning = "experimental_not_accepted; peer selection, self-exclusion, tenor normalization, and uncertainty rule not approved",
    decision_status = "experimental_pending_method_decision"
  ) |>
  standard_output_columns()

benchmark_scenarios <- bind_rows(
  primary_scenarios,
  ids_bondholder_scenario,
  secondary_scenarios,
  rating_implied_archive_scenario,
  rating_implied_scenario,
  shadow_prs_scenario,
  peer_primary_scenario,
  peer_best_scenario
) |>
  filter(!is.na(.data$analysis_year), .data$analysis_year %in% analysis_years) |>
  arrange(.data$analysis_year, .data$country, .data$scenario_priority, .data$scenario_id)

spread_normalized_usd_eur <- benchmark_scenarios |>
  filter(
    .data$scenario_id %in% c(
      "primary_usd_eur_final_yield_standard_all_issue_counts",
      "primary_usd_only_final_yield_standard_all_issue_counts",
      "secondary_direct_ytm_standard_outstanding_weighted",
      "secondary_direct_ytm_usd_only_standard"
    )
  ) |>
  left_join(fred_dgs7_annual, by = "analysis_year") |>
  mutate(
    market_spread_over_fred_dgs7_pct = .data$market_rate_pct - .data$risk_free_7y_pct,
    spread_warning = "diagnostic_only; subtracts USD 7-year Treasury from mixed USD/EUR scenarios where applicable and does not enter PVR discounting"
  ) |>
  select(
    analysis_year,
    iso3,
    country,
    scenario_id,
    benchmark_source_tier,
    market_rate_pct,
    risk_free_7y_pct,
    market_spread_over_fred_dgs7_pct,
    market_maturity_years,
    currency_basis,
    issue_count,
    total_weight_usd,
    spread_warning,
    source_artifact
  )

scenario_dictionary <- benchmark_scenarios |>
  distinct(
    scenario_id,
    ladder_component,
    benchmark_source_tier,
    method_family,
    scenario_priority,
    market_rate_measure_basis,
    currency_basis,
    weighting_variable,
    decision_status,
    method_warning,
    source_artifact,
    source_note
  ) |>
  arrange(.data$scenario_priority, .data$scenario_id)

ladder_rules <- tribble(
  ~ladder_variant, ~ladder_priority, ~scenario_id,
  "ladder_maximum_coverage", 10L, "primary_usd_eur_final_yield_standard_all_issue_counts",
  "ladder_maximum_coverage", 20L, "ids_bondholder_contractual_proxy",
  "ladder_maximum_coverage", 30L, "secondary_direct_ytm_standard_outstanding_weighted",
  "ladder_maximum_coverage", 40L, "rating_implied_damodaran_archive_vintage_dgs7",
  "ladder_maximum_coverage", 45L, "rating_implied_damodaran_current_vintage_dgs7",
  "ladder_maximum_coverage", 50L, "shadow_prs_damodaran_current_vintage_dgs7",
  "ladder_maximum_coverage", 60L, "peer_income_year_median_observed_plus_ids",
  "ladder_observed_first_no_model", 10L, "primary_usd_eur_final_yield_standard_all_issue_counts",
  "ladder_observed_first_no_model", 20L, "ids_bondholder_contractual_proxy",
  "ladder_observed_first_no_model", 30L, "secondary_direct_ytm_standard_outstanding_weighted",
  "ladder_primary_strict_two_plus", 10L, "primary_usd_eur_two_plus_standard",
  "ladder_primary_strict_two_plus", 20L, "ids_bondholder_contractual_proxy",
  "ladder_primary_strict_two_plus", 30L, "secondary_direct_ytm_standard_outstanding_weighted",
  "ladder_primary_strict_two_plus", 40L, "rating_implied_damodaran_archive_vintage_dgs7",
  "ladder_primary_strict_two_plus", 45L, "rating_implied_damodaran_current_vintage_dgs7",
  "ladder_primary_strict_two_plus", 50L, "shadow_prs_damodaran_current_vintage_dgs7",
  "ladder_primary_strict_two_plus", 60L, "peer_income_year_median_observed_plus_ids",
  "ladder_usd_only_primary", 10L, "primary_usd_only_final_yield_standard_all_issue_counts",
  "ladder_usd_only_primary", 20L, "ids_bondholder_contractual_proxy",
  "ladder_usd_only_primary", 30L, "secondary_direct_ytm_usd_only_standard",
  "ladder_usd_only_primary", 40L, "rating_implied_damodaran_archive_vintage_dgs7",
  "ladder_usd_only_primary", 45L, "rating_implied_damodaran_current_vintage_dgs7",
  "ladder_usd_only_primary", 50L, "shadow_prs_damodaran_current_vintage_dgs7",
  "ladder_usd_only_primary", 60L, "peer_income_year_median_observed_plus_ids",
  "ladder_primary_issue50m_ge1", 10L, "primary_usd_eur_final_yield_standard_issue50m_ge1",
  "ladder_primary_issue50m_ge1", 20L, "ids_bondholder_contractual_proxy",
  "ladder_primary_issue50m_ge1", 30L, "secondary_direct_ytm_standard_outstanding_weighted",
  "ladder_primary_issue50m_ge1", 40L, "rating_implied_damodaran_archive_vintage_dgs7",
  "ladder_primary_issue50m_ge1", 45L, "rating_implied_damodaran_current_vintage_dgs7",
  "ladder_primary_issue50m_ge1", 50L, "shadow_prs_damodaran_current_vintage_dgs7",
  "ladder_primary_issue50m_ge1", 60L, "peer_income_year_median_observed_plus_ids",
  "ladder_primary_issue50m_ge2", 10L, "primary_usd_eur_final_yield_standard_issue50m_ge2",
  "ladder_primary_issue50m_ge2", 20L, "ids_bondholder_contractual_proxy",
  "ladder_primary_issue50m_ge2", 30L, "secondary_direct_ytm_standard_outstanding_weighted",
  "ladder_primary_issue50m_ge2", 40L, "rating_implied_damodaran_archive_vintage_dgs7",
  "ladder_primary_issue50m_ge2", 45L, "rating_implied_damodaran_current_vintage_dgs7",
  "ladder_primary_issue50m_ge2", 50L, "shadow_prs_damodaran_current_vintage_dgs7",
  "ladder_primary_issue50m_ge2", 60L, "peer_income_year_median_observed_plus_ids",
  "ladder_model_after_observed", 10L, "primary_usd_eur_final_yield_standard_all_issue_counts",
  "ladder_model_after_observed", 20L, "secondary_direct_ytm_standard_outstanding_weighted",
  "ladder_model_after_observed", 30L, "ids_bondholder_contractual_proxy",
  "ladder_model_after_observed", 40L, "rating_implied_damodaran_archive_vintage_dgs7",
  "ladder_model_after_observed", 45L, "rating_implied_damodaran_current_vintage_dgs7",
  "ladder_model_after_observed", 50L, "shadow_prs_damodaran_current_vintage_dgs7",
  "ladder_model_after_observed", 60L, "peer_income_year_median_observed_plus_ids",
  "ladder_secondary_closest_7y_after_primary", 10L, "primary_usd_eur_final_yield_standard_all_issue_counts",
  "ladder_secondary_closest_7y_after_primary", 20L, "ids_bondholder_contractual_proxy",
  "ladder_secondary_closest_7y_after_primary", 30L, "secondary_closest_7y_standard",
  "ladder_secondary_closest_7y_after_primary", 40L, "rating_implied_damodaran_archive_vintage_dgs7",
  "ladder_secondary_closest_7y_after_primary", 45L, "rating_implied_damodaran_current_vintage_dgs7",
  "ladder_secondary_closest_7y_after_primary", 50L, "shadow_prs_damodaran_current_vintage_dgs7",
  "ladder_secondary_closest_7y_after_primary", 60L, "peer_income_year_median_observed_plus_ids",
  "ladder_secondary_closest_10y_after_primary", 10L, "primary_usd_eur_final_yield_standard_all_issue_counts",
  "ladder_secondary_closest_10y_after_primary", 20L, "ids_bondholder_contractual_proxy",
  "ladder_secondary_closest_10y_after_primary", 30L, "secondary_closest_10y_standard",
  "ladder_secondary_closest_10y_after_primary", 40L, "rating_implied_damodaran_archive_vintage_dgs7",
  "ladder_secondary_closest_10y_after_primary", 45L, "rating_implied_damodaran_current_vintage_dgs7",
  "ladder_secondary_closest_10y_after_primary", 50L, "shadow_prs_damodaran_current_vintage_dgs7",
  "ladder_secondary_closest_10y_after_primary", 60L, "peer_income_year_median_observed_plus_ids",
  "ladder_permissive_primary_first", 10L, "primary_usd_eur_permissive_all_yielding_hard_currency",
  "ladder_permissive_primary_first", 20L, "ids_bondholder_contractual_proxy",
  "ladder_permissive_primary_first", 30L, "secondary_direct_ytm_include_nonstandard_flagged",
  "ladder_permissive_primary_first", 40L, "rating_implied_damodaran_archive_vintage_dgs7",
  "ladder_permissive_primary_first", 45L, "rating_implied_damodaran_current_vintage_dgs7",
  "ladder_permissive_primary_first", 50L, "shadow_prs_damodaran_current_vintage_dgs7",
  "ladder_permissive_primary_first", 60L, "peer_income_year_median_observed_plus_ids"
)

selected_benchmarks_by_ladder <- benchmark_scenarios |>
  inner_join(ladder_rules, by = "scenario_id", relationship = "many-to-many") |>
  filter(!is.na(.data$market_rate_pct), !is.na(.data$market_maturity_years)) |>
  arrange(.data$ladder_variant, .data$analysis_year, .data$country_key, .data$ladder_priority, .data$scenario_priority) |>
  group_by(.data$ladder_variant, .data$analysis_year, .data$country_key) |>
  slice(1) |>
  ungroup() |>
  mutate(selection_role = "selected_experimental_ladder_rate") |>
  arrange(.data$ladder_variant, .data$analysis_year, .data$country)

not_computed_by_ladder <- tidyr::crossing(
  country_year_universe,
  ladder_variant = unique(ladder_rules$ladder_variant)
) |>
  anti_join(
    selected_benchmarks_by_ladder |> select("ladder_variant", "analysis_year", "country_key"),
    by = c("ladder_variant", "analysis_year", "country_key")
  ) |>
  mutate(
    scenario_id = "not_computed",
    ladder_component = "not_computed_status_layer",
    benchmark_source_tier = "not_computed",
    method_family = "status",
    scenario_priority = 999L,
    ladder_priority = 999L,
    market_rate_pct = NA_real_,
    market_maturity_years = NA_real_,
    benchmark_quality_band = "not_available",
    benchmark_status = "not_computed",
    benchmark_status_reason = "no_rate_available_under_ladder_variant",
    market_rate_measure_basis = NA_character_,
    currency_basis = NA_character_,
    weighting_variable = NA_character_,
    total_weight_usd = NA_real_,
    issue_count = NA_integer_,
    included_issue_count = NA_integer_,
    yield_source = NA_character_,
    source_artifact = NA_character_,
    source_note = "No experimental benchmark scenario selected under this ladder variant.",
    method_warning = "status row; country-year retained rather than silently dropped",
    decision_status = "not_applicable",
    selection_role = "not_computed_status"
  )

selected_benchmarks_complete <- bind_rows(selected_benchmarks_by_ladder, not_computed_by_ladder) |>
  arrange(.data$ladder_variant, .data$analysis_year, .data$country)

market_for_pvr <- selected_benchmarks_by_ladder |>
  filter(!is.na(.data$market_rate_pct), !is.na(.data$market_maturity_years), !is.na(.data$iso3)) |>
  transmute(
    ladder_variant,
    analysis_year,
    iso3,
    country,
    country_key,
    market_issue_name = paste0("Experimental ", .data$benchmark_source_tier, " via ", .data$scenario_id),
    market_rate = .data$market_rate_pct,
    market_maturity_years = .data$market_maturity_years,
    market_repayment_type = "bullet_assumption",
    discount_rate_concept = "country_year_market_equivalent_borrowing_rate",
    market_rate_measure_basis,
    market_rate_selection_role = "experimental_ladder_selected_benchmark",
    benchmark_cashflow_representation = "synthetic_bullet_market_reference",
    benchmark_cashflow_note = "Experimental market row uses a synthetic bullet-style reference cash flow for discounting; source tier and quality band are not accepted methodology.",
    market_rate_source_class = .data$benchmark_source_tier,
    market_rate_source_url = .data$source_artifact,
    market_rate_source_note = .data$source_note,
    scenario_id,
    benchmark_source_tier,
    benchmark_quality_band,
    benchmark_status,
    benchmark_status_reason,
    method_warning,
    decision_status,
    issue_count,
    weighting_variable,
    total_weight_usd,
    currency_basis,
    yield_source
  )

market_rows <- market_for_pvr |>
  transmute(
    ladder_variant,
    iso3,
    country,
    analysis_year,
    creditor = "Bonds",
    creditor_id = "BND",
    creditor_name = "Bondholders",
    creditor_scope = "market_bondholders",
    market_issue_name,
    market_rate,
    market_maturity_years,
    market_repayment_type,
    discount_rate_concept,
    market_rate_measure_basis,
    market_rate_selection_role,
    benchmark_cashflow_representation,
    benchmark_cashflow_note,
    official_rate = market_rate,
    official_maturity_years = market_maturity_years,
    official_grace_years = NA_real_,
    discount_rate = market_rate,
    pvr = 100,
    pvr_error = NA_character_,
    implicit_saving_per_100 = 0,
    coverage_flag = "included",
    official_terms_source = "Experimental market benchmark row",
    market_rate_source_class,
    market_rate_source_url,
    market_rate_source_note,
    scenario_id,
    benchmark_source_tier,
    benchmark_quality_band,
    benchmark_status,
    benchmark_status_reason,
    method_warning,
    decision_status,
    issue_count,
    weighting_variable,
    total_weight_usd,
    currency_basis,
    yield_source
  )

official_rows_core <- ids_terms_core |>
  filter(.data$creditor != "Bondholders") |>
  left_join(market_for_pvr, by = c("iso3", "year" = "analysis_year"), relationship = "many-to-many") |>
  rowwise() |>
  mutate(
    pvr_calc = list(safe_official_pvr(
      official_rate = .data$official_rate,
      maturity = .data$official_maturity_years,
      grace = .data$official_grace_years,
      discount_rate = .data$market_rate
    ))
  ) |>
  tidyr::unnest_wider(.data$pvr_calc) |>
  ungroup() |>
  mutate(
    ladder_variant = coalesce(.data$ladder_variant, "no_selected_ladder_rate"),
    coverage_flag = case_when(
      !.data$has_complete_terms ~ "missing_lender_terms",
      is.na(.data$market_rate) ~ "missing_market_benchmark",
      !is.na(.data$pvr_error) ~ "invalid_or_failed_pvr",
      TRUE ~ "included"
    ),
    pvr = if_else(.data$coverage_flag == "included", .data$pvr, NA_real_),
    discount_rate = if_else(.data$coverage_flag == "included", .data$market_rate, NA_real_),
    implicit_saving_per_100 = if_else(.data$coverage_flag == "included", round(100 - .data$pvr, 2), NA_real_)
  ) |>
  transmute(
    ladder_variant,
    iso3,
    country = coalesce(.data$country.x, .data$country.y),
    analysis_year = .data$year,
    creditor,
    creditor_id,
    creditor_name = coalesce(.data$creditor_name, .data$creditor),
    creditor_scope = coalesce(.data$creditor_scope, "unknown"),
    market_issue_name,
    market_rate,
    market_maturity_years,
    market_repayment_type,
    discount_rate_concept,
    market_rate_measure_basis,
    market_rate_selection_role,
    benchmark_cashflow_representation,
    benchmark_cashflow_note,
    official_rate,
    official_maturity_years,
    official_grace_years,
    discount_rate,
    pvr,
    pvr_error,
    implicit_saving_per_100,
    coverage_flag,
    official_terms_source = paste0("World Bank IDS source 6, counterpart-area ", .data$creditor_id, "; ", .data$ids_history_source),
    market_rate_source_class,
    market_rate_source_url,
    market_rate_source_note,
    scenario_id,
    benchmark_source_tier,
    benchmark_quality_band,
    benchmark_status,
    benchmark_status_reason,
    method_warning,
    decision_status,
    issue_count,
    weighting_variable,
    total_weight_usd,
    currency_basis,
    yield_source
  )

master_pvr_scenarios_all_years <- bind_rows(market_rows, official_rows_core) |>
  arrange(.data$ladder_variant, .data$analysis_year, .data$country, .data$creditor)

ids_terms_2024_all_lenders <- readr::read_csv(path_ids_terms_2024, show_col_types = FALSE) |>
  mutate(
    official_rate = clean_num(.data$official_rate),
    official_maturity_years = clean_num(.data$official_maturity_years),
    official_grace_years = clean_num(.data$official_grace_years),
    has_complete_terms = as.logical(.data$has_complete_terms)
  )

pvr_2024_all_lenders_expansion <- ids_terms_2024_all_lenders |>
  filter(.data$creditor != "Bondholders") |>
  left_join(
    market_for_pvr |> filter(.data$analysis_year == 2024),
    by = c("iso3", "year" = "analysis_year"),
    relationship = "many-to-many"
  ) |>
  rowwise() |>
  mutate(
    pvr_calc = list(safe_official_pvr(
      official_rate = .data$official_rate,
      maturity = .data$official_maturity_years,
      grace = .data$official_grace_years,
      discount_rate = .data$market_rate
    ))
  ) |>
  tidyr::unnest_wider(.data$pvr_calc) |>
  ungroup() |>
  mutate(
    ladder_variant = coalesce(.data$ladder_variant, "no_selected_ladder_rate"),
    coverage_flag = case_when(
      !.data$has_complete_terms ~ "missing_lender_terms",
      is.na(.data$market_rate) ~ "missing_market_benchmark",
      !is.na(.data$pvr_error) ~ "invalid_or_failed_pvr",
      TRUE ~ "included"
    ),
    pvr = if_else(.data$coverage_flag == "included", .data$pvr, NA_real_),
    discount_rate = if_else(.data$coverage_flag == "included", .data$market_rate, NA_real_),
    implicit_saving_per_100 = if_else(.data$coverage_flag == "included", round(100 - .data$pvr, 2), NA_real_)
  ) |>
  transmute(
    ladder_variant,
    iso3,
    country = coalesce(.data$country.x, .data$country.y),
    analysis_year = .data$year,
    creditor,
    creditor_id,
    creditor_name = coalesce(.data$creditor_name, .data$creditor),
    creditor_scope = coalesce(.data$creditor_scope, "unknown"),
    market_issue_name,
    market_rate,
    market_maturity_years,
    official_rate,
    official_maturity_years,
    official_grace_years,
    discount_rate,
    pvr,
    pvr_error,
    implicit_saving_per_100,
    coverage_flag,
    scenario_id,
    benchmark_source_tier,
    benchmark_quality_band,
    benchmark_status,
    benchmark_status_reason,
    method_warning,
    market_rate_source_class,
    market_rate_source_url,
    market_rate_source_note
  ) |>
  arrange(.data$ladder_variant, .data$country, .data$creditor)

country_year_coverage <- selected_benchmarks_complete |>
  group_by(.data$ladder_variant, .data$analysis_year, .data$benchmark_source_tier, .data$benchmark_quality_band, .data$benchmark_status) |>
  summarise(
    country_years = n(),
    country_years_with_rate = sum(!is.na(.data$market_rate_pct)),
    median_rate_pct = if (all(is.na(.data$market_rate_pct))) NA_real_ else median(.data$market_rate_pct, na.rm = TRUE),
    .groups = "drop"
  ) |>
  arrange(.data$ladder_variant, .data$analysis_year, .data$benchmark_source_tier)

pvr_coverage_summary <- master_pvr_scenarios_all_years |>
  group_by(.data$ladder_variant, .data$creditor, .data$coverage_flag, .data$benchmark_source_tier) |>
  summarise(
    rows = n(),
    countries = n_distinct(.data$country),
    years = n_distinct(.data$analysis_year),
    median_pvr = if (all(is.na(.data$pvr))) NA_real_ else median(.data$pvr, na.rm = TRUE),
    .groups = "drop"
  ) |>
  arrange(.data$ladder_variant, .data$creditor, .data$coverage_flag, .data$benchmark_source_tier)

lseg_risk_free_raw <- readr::read_csv(path_risk_free_lseg, show_col_types = FALSE) |>
  mutate(Date = as.Date(.data$Date), analysis_year = as.integer(format(.data$Date, "%Y")))

source_year_limits <- tibble(
  source_or_component = c(
    "LSEG primary candidates",
    "LSEG secondary snapshots",
    "LSEG secondary direct YTM history",
    "World Bank IDS core terms",
    "FRED DGS7",
    "LSEG risk-free curves",
    "Damodaran archived country-risk workbooks",
    "Damodaran country-risk workbook",
    "2024 IDS all-lenders expansion"
  ),
  first_year = c(
    min(primary_raw$analysis_year, na.rm = TRUE),
    min(secondary_snap$snapshot_year, na.rm = TRUE),
    min(secondary_hist$history_year, na.rm = TRUE),
    min(ids_terms_core$year, na.rm = TRUE),
    min(fred_dgs7_annual$analysis_year, na.rm = TRUE),
    min(lseg_risk_free_raw$analysis_year, na.rm = TRUE),
    min(damodaran_archive_spreads$analysis_year, na.rm = TRUE),
    NA_integer_,
    2024L
  ),
  last_year = c(
    max(primary_raw$analysis_year, na.rm = TRUE),
    max(secondary_snap$snapshot_year, na.rm = TRUE),
    max(secondary_hist$history_year, na.rm = TRUE),
    max(ids_terms_core$year, na.rm = TRUE),
    max(fred_dgs7_annual$analysis_year, na.rm = TRUE),
    max(lseg_risk_free_raw$analysis_year, na.rm = TRUE),
    max(damodaran_archive_spreads$analysis_year, na.rm = TRUE),
    NA_integer_,
    2024L
  ),
  first_year_with_complete_or_computable_data = c(
    min(primary_scenarios$analysis_year, na.rm = TRUE),
    min(secondary_scenarios$analysis_year, na.rm = TRUE),
    min(secondary_scenarios$analysis_year, na.rm = TRUE),
    min(ids_terms_core$year[ids_terms_core$has_complete_terms], na.rm = TRUE),
    min(fred_dgs7_annual$analysis_year, na.rm = TRUE),
    min(lseg_risk_free_raw$analysis_year, na.rm = TRUE),
    min(rating_implied_archive_scenario$analysis_year, na.rm = TRUE),
    NA_integer_,
    2024L
  ),
  last_year_with_complete_or_computable_data = c(
    max(primary_scenarios$analysis_year, na.rm = TRUE),
    max(secondary_scenarios$analysis_year, na.rm = TRUE),
    max(secondary_scenarios$analysis_year, na.rm = TRUE),
    max(ids_terms_core$year[ids_terms_core$has_complete_terms], na.rm = TRUE),
    max(fred_dgs7_annual$analysis_year, na.rm = TRUE),
    max(lseg_risk_free_raw$analysis_year, na.rm = TRUE),
    max(rating_implied_archive_scenario$analysis_year, na.rm = TRUE),
    NA_integer_,
    2024L
  ),
  rows = c(
    nrow(primary_raw),
    nrow(secondary_snap),
    nrow(secondary_hist),
    nrow(ids_terms_core),
    nrow(fred_dgs7_annual),
    nrow(lseg_risk_free_raw),
    nrow(damodaran_archive_spreads),
    nrow(damodaran_regional),
    nrow(ids_terms_2024_all_lenders)
  ),
  note = c(
    "Observed primary issuance scenarios use issue years 2000-2025.",
    "Secondary outstanding snapshots are only available from LSEG v14 for 2020-2025.",
    "Secondary direct YTM history is only available from LSEG v14 for 2020-2025.",
    "Fetched/cached from World Bank IDS source 6 for IBRD, IDA, China, and Bondholders; 2025 rows exist but no core creditor has complete 2025 terms in this run.",
    "Annual average of daily FRED DGS7 observations.",
    "Inventory only in this script; FRED DGS7 is used for model variants.",
    "Official Damodaran annual archive files for country risk premiums; parsed as historical default-spread sensitivity inputs.",
    "Current-vintage workbook used experimentally across all years; not a historical country spread panel.",
    "Existing canonical 2024 all-lender IDS table used only for broad experimental 2024 expansion."
  )
)

data_inventory <- tibble(
  source_name = c(
    "LSEG v14 primary candidates with yield",
    "LSEG v14 secondary outstanding snapshots",
    "LSEG v14 secondary year-end history",
    "LSEG v14 rating inputs",
    "LSEG v14 risk-free curve history",
    "World Bank IDS core all-years terms",
    "World Bank IDS all-lenders 2024 terms",
    "FRED DGS7",
    "Damodaran annual archive country-risk workbooks",
    "Damodaran country-risk workbook",
    "Country metadata"
  ),
  path = c(
    path_primary,
    path_secondary_snap,
    path_secondary_hist,
    path_rating,
    path_risk_free_lseg,
    ids_cache_dir,
    path_ids_terms_2024,
    path_fred_dgs7,
    path_damodaran_archive_dir,
    path_damodaran,
    path_country_metadata
  ),
  exists = c(
    file.exists(path_primary),
    file.exists(path_secondary_snap),
    file.exists(path_secondary_hist),
    file.exists(path_rating),
    file.exists(path_risk_free_lseg),
    dir.exists(ids_cache_dir),
    file.exists(path_ids_terms_2024),
    file.exists(path_fred_dgs7),
    dir.exists(path_damodaran_archive_dir),
    file.exists(path_damodaran),
    file.exists(path_country_metadata)
  ),
  rows_or_units = c(
    nrow(primary_raw),
    nrow(secondary_snap),
    nrow(secondary_hist),
    nrow(readr::read_csv(path_rating, show_col_types = FALSE)),
    nrow(lseg_risk_free_raw),
    nrow(ids_terms_core),
    nrow(ids_terms_2024_all_lenders),
    nrow(fred_dgs7_annual),
    nrow(damodaran_archive_spreads),
    nrow(damodaran_regional),
    nrow(country_metadata)
  ),
  years_available = c(
    paste(sort(unique(primary_raw$analysis_year)), collapse = ";"),
    paste(sort(unique(secondary_snap$snapshot_year)), collapse = ";"),
    paste(sort(unique(secondary_hist$history_year)), collapse = ";"),
    "instrument-level ratings only, no annual country panel",
    paste(sort(unique(lseg_risk_free_raw$analysis_year)), collapse = ";"),
    paste(sort(unique(ids_terms_core$year)), collapse = ";"),
    "2024",
    paste(sort(unique(fred_dgs7_annual$analysis_year)), collapse = ";"),
    paste(sort(unique(damodaran_archive_spreads$analysis_year)), collapse = ";"),
    "current workbook vintage, repeated experimentally across years",
    "static metadata"
  ),
  computed_use = c(
    "primary-market scenario variants",
    "secondary-market instrument universe",
    "secondary-market direct year-end YTM scenarios",
    "availability/audit context only in this script",
    "inventory only; FRED DGS7 used for model scenarios",
    "IDS bondholder proxy and IBRD/IDA/China PVR terms",
    "broad 2024 all-lender experimental PVR expansion",
    "risk-free input for rating-implied and PRS shadow scenarios",
    "year-specific Damodaran archive spread input for historical rating-implied scenario",
    "current-vintage default spread inputs for rating and PRS shadow scenarios",
    "country-year universe and joins"
  )
)

ladder_component_inventory <- tibble(
  ladder_component = c(
    "observed_primary_issuance",
    "thin_primary_issuance",
    "nonstandard_exclusion_layer",
    "ids_bondholder_proxy",
    "secondary_market_evidence",
    "rating_implied_evidence",
    "shadow_rating_or_fundamental_model",
    "peer_proxy",
    "not_computed_status_layer",
    "official_creditor_pvr_comparison"
  ),
  computed_in_this_experiment = c(
    "yes_multiple_scenarios",
    "yes_quality_labels_not_blocking",
    "yes_flags_and_permissive_sensitivity_scenarios",
    "yes_all_years_core_counterpart",
    "yes_2020_2025_direct_ytm_scenarios",
    "yes_current_vintage_model_sensitivity",
    "yes_prs_current_vintage_sensitivity",
    "yes_income_year_median_variants",
    "yes_for_each_ladder_variant",
    "yes_core_all_years_and_2024_all_lenders"
  ),
  available_data_summary = c(
    paste0(nrow(primary_raw), " LSEG v14 primary rows across ", n_distinct(primary_raw$analysis_year), " years."),
    paste0(sum(primary_scenarios$benchmark_quality_band == "observed_primary_single_issue"), " single-issue scenario country-years and ", sum(primary_scenarios$benchmark_quality_band == "observed_primary_two_issue"), " two-issue scenario country-years across all primary scenarios."),
    paste0(nrow(primary_row_status), " country-year-status rows record exclusions or included baseline status."),
    paste0(nrow(ids_bondholder_scenario), " complete IDS bondholder proxy country-years."),
    paste0(nrow(secondary_scenarios), " secondary scenario country-years with direct YTM."),
    paste0(nrow(rating_implied_archive_scenario), " annual-archive rating-implied country-years and ", nrow(rating_implied_scenario), " current-vintage sensitivity country-years."),
    paste0(nrow(shadow_prs_scenario), " current-vintage PRS shadow country-years."),
    paste0(nrow(peer_primary_scenario) + nrow(peer_best_scenario), " peer-proxy country-years across two variants."),
    paste0(nrow(not_computed_by_ladder), " retained not-computed country-year-ladder rows."),
    paste0(sum(master_pvr_scenarios_all_years$coverage_flag == "included", na.rm = TRUE), " included core all-years PVR rows; ", sum(pvr_2024_all_lenders_expansion$coverage_flag == "included", na.rm = TRUE), " included 2024 all-lender expansion rows.")
  ),
  still_missing_or_unapproved = c(
    "Accepted primary eligibility/yield/amount/reopening/distress rules.",
    "Accepted one-issue and two-issue headline treatment.",
    "Source-backed restructuring, default, sanctions, liability-management, and priced-out ledger.",
    "Accepted IDS fallback role and materiality amount variable.",
    "Accepted secondary-market quote-recency, tenor, distress, and weighting rule.",
    "Annual Damodaran archive is now parsed for 2000-2024, but the project still needs an approved model-tier role, vintage timing convention, and validation rule.",
    "Replicated/re-estimated coefficients, predictor sourcing, validation, and uncertainty reporting.",
    "Approved peer universe, self-exclusion, weighting, tenor/currency normalization, and uncertainty rule.",
    "Accepted status taxonomy and source failure/missing-data separation.",
    "Accepted official-creditor eligibility map for broad creditor comparisons."
  )
)

method_warnings <- tibble(
  warning_id = c(
    "experimental_boundary",
    "primary_pending",
    "nonstandard_pending",
    "secondary_pending",
    "ids_proxy_pending",
    "rating_vintage_pending",
    "shadow_validation_pending",
    "peer_proxy_pending",
    "pvr_scope_pending",
    "canonical_outputs_untouched"
  ),
  warning = c(
    "This is an experimental maximum-coverage database, not accepted methodology.",
    "Primary-market variants are computed side by side because eligibility, yield precedence, currency basis, and thin-issuance treatment are not yet approved.",
    "Permissive primary scenarios include flagged nonstandard rows for sensitivity; they should not be read as clean market borrowing costs.",
    "Secondary-market variants use direct LSEG YTM only and are limited to years available in the v14 secondary history.",
    "IDS bondholder rates are average contractual proxies, not issue-level observed yields.",
    "Rating-implied rates include an annual Damodaran archive-vintage scenario for 2000-2024 and a separate current-vintage sensitivity scenario.",
    "PRS shadow rates use current Damodaran PRS worksheet values across all years and have no project validation yet.",
    "Peer proxies use income-year medians and do not yet exclude the target country or normalize tenor/currency beyond stored labels.",
    "PVR rows use the current project valuation functions, but broad creditor eligibility is not approved.",
    "The script writes only under the experiment folder and does not overwrite canonical output/tables files."
  )
)

readr::write_csv(data_inventory, file.path(out_dir, "data_inventory.csv"))
readr::write_csv(source_year_limits, file.path(out_dir, "source_year_limits.csv"))
readr::write_csv(ladder_component_inventory, file.path(out_dir, "ladder_component_inventory.csv"))
readr::write_csv(damodaran_archive_spreads, file.path(out_dir, "damodaran_archive_country_spreads_2000_2024.csv"))
readr::write_csv(primary_row_status, file.path(out_dir, "primary_row_status_all_years.csv"))
readr::write_csv(primary_issue_level_all_years, file.path(out_dir, "primary_issue_level_all_years.csv"))
readr::write_csv(secondary_issue_level_all_years, file.path(out_dir, "secondary_issue_level_all_years.csv"))
readr::write_csv(ids_terms_core, file.path(out_dir, "ids_terms_core_all_years.csv"))
readr::write_csv(secondary_latest_yields, file.path(out_dir, "secondary_latest_direct_ytm_all_years.csv"))
readr::write_csv(scenario_dictionary, file.path(out_dir, "scenario_dictionary.csv"))
readr::write_csv(ladder_rules, file.path(out_dir, "ladder_rules.csv"))
readr::write_csv(benchmark_scenarios, file.path(out_dir, "master_benchmark_scenarios_all_years.csv"))
readr::write_csv(benchmark_scenarios, file.path(out_dir, "scenario_benchmark_country_years.csv"))
readr::write_csv(selected_benchmarks_complete, file.path(out_dir, "selected_benchmarks_by_ladder_all_years.csv"))
readr::write_csv(master_pvr_scenarios_all_years, file.path(out_dir, "master_pvr_scenarios_all_years.csv"))
readr::write_csv(master_pvr_scenarios_all_years, file.path(out_dir, "scenario_pvr_country_year_creditor.csv"))
readr::write_csv(pvr_2024_all_lenders_expansion, file.path(out_dir, "pvr_2024_all_lenders_scenario_expansion.csv"))
readr::write_csv(country_year_coverage, file.path(out_dir, "country_year_coverage.csv"))
readr::write_csv(country_year_coverage, file.path(out_dir, "scenario_coverage_by_year.csv"))
readr::write_csv(pvr_coverage_summary, file.path(out_dir, "pvr_coverage_summary.csv"))
readr::write_csv(method_warnings, file.path(out_dir, "method_warnings.csv"))
readr::write_csv(spread_normalized_usd_eur, file.path(out_dir, "spread_normalized_usd_eur_all_years.csv"))

coverage_lines <- c(
  "# Coverage And Missingness",
  "",
  "This file is generated by `scripts/run_all_years_scenario_database.R`.",
  "",
  "## Benchmark Scenario Coverage",
  "",
  paste0("- Benchmark scenario rows: ", nrow(benchmark_scenarios), "."),
  paste0("- Selected benchmark/status rows by ladder: ", nrow(selected_benchmarks_complete), "."),
  paste0("- Not-computed selected rows by ladder: ", nrow(not_computed_by_ladder), "."),
  paste0("- Primary scenario rows: ", nrow(primary_scenarios), "."),
  paste0("- IDS bondholder proxy rows: ", nrow(ids_bondholder_scenario), "."),
  paste0("- Secondary scenario rows: ", nrow(secondary_scenarios), "."),
  paste0("- Historical Damodaran-archive rating-implied sensitivity rows: ", nrow(rating_implied_archive_scenario), "."),
  paste0("- Current-vintage rating-implied sensitivity rows: ", nrow(rating_implied_scenario), "."),
  paste0("- Shadow/PRS sensitivity rows: ", nrow(shadow_prs_scenario), "."),
  paste0("- Peer-proxy rows: ", nrow(peer_primary_scenario) + nrow(peer_best_scenario), "."),
  paste0("- Primary issue-level rows: ", nrow(primary_issue_level_all_years), "."),
  paste0("- Secondary issue-level rows: ", nrow(secondary_issue_level_all_years), "."),
  "",
  "## PVR Coverage",
  "",
  paste0("- Core all-years PVR rows: ", nrow(master_pvr_scenarios_all_years), "."),
  paste0("- Core all-years included PVR rows: ", sum(master_pvr_scenarios_all_years$coverage_flag == "included", na.rm = TRUE), "."),
  paste0("- 2024 all-lender expansion rows: ", nrow(pvr_2024_all_lenders_expansion), "."),
  paste0("- 2024 all-lender included rows: ", sum(pvr_2024_all_lenders_expansion$coverage_flag == "included", na.rm = TRUE), "."),
  "",
  "## Main Remaining Gaps",
  "",
  "- Distress/default/restructuring/priced-out status is not yet sourced as a complete country-year panel.",
  "- Secondary-market evidence only exists where the LSEG v14 secondary history has direct YTM fields.",
  "- Rating model variants now include an annual Damodaran archive-vintage scenario for 2000-2024 plus a separate current-vintage sensitivity scenario.",
  "- PRS shadow model variants still use current Damodaran workbook values across all years as an explicit sensitivity assumption.",
  "- Peer proxies are mechanically filled and should be treated as weak diagnostics until a peer rule is approved."
)

readme_lines <- c(
  "# Experimental All-Years Full-Ladder Database",
  "",
  "Date: 2026-05-20",
  "",
  "Status: experimental, maximum coverage, not canonical, not approved methodology.",
  "",
  "## Purpose",
  "",
  "This experiment repeats the earlier 2024-only ladder run as an all-available-years database. Pending methodological decisions do not block computation here; instead, the script calculates multiple named scenario variants and ladder variants side by side, then carries warnings and decision-status fields through the outputs.",
  "",
  "## Main Outputs",
  "",
  "- `outputs/master_benchmark_scenarios_all_years.csv`: all computed benchmark-rate scenario rows.",
  "- `outputs/scenario_benchmark_country_years.csv`: alias for the full benchmark-scenario country-year table.",
  "- `outputs/selected_benchmarks_by_ladder_all_years.csv`: one selected benchmark or not-computed status per country-year-ladder variant.",
  "- `outputs/master_pvr_scenarios_all_years.csv`: all-years PVR results for Bonds, IBRD, IDA, and China where terms and benchmarks permit.",
  "- `outputs/scenario_pvr_country_year_creditor.csv`: alias for the full PVR country-year-creditor table.",
  "- `outputs/pvr_2024_all_lenders_scenario_expansion.csv`: 2024 broad all-lender experimental PVR expansion using the existing IDS all-lender table.",
  "- `outputs/pvr_global_all_years_master.xlsx`: Excel workbook with the main sheets and summaries.",
  "- `outputs/source_year_limits.csv`: first/last year and row count for each major source.",
  "- `outputs/damodaran_archive_country_spreads_2000_2024.csv`: parsed annual Damodaran archive spreads used for the historical rating-implied sensitivity scenario.",
  "- `outputs/primary_issue_level_all_years.csv` and `outputs/secondary_issue_level_all_years.csv`: instrument-level traceability tables.",
  "- `outputs/spread_normalized_usd_eur_all_years.csv`: diagnostic spreads over FRED DGS7 for observed USD/EUR scenarios.",
  "- `outputs/ladder_component_inventory.csv`: what each ladder component has, lacks, and computes.",
  "- `outputs/scenario_dictionary.csv`: scenario definitions and warnings.",
  "- `coverage_and_missingness.md`: compact coverage and gap summary.",
  "- `method_warnings.md`: plain-language caveats.",
  "- `agent_worklog.md`: what was done in this experiment.",
  "",
  "## Compact Result",
  "",
  paste0("- Benchmark scenario rows: ", nrow(benchmark_scenarios), "."),
  paste0("- Selected benchmark/status rows by ladder: ", nrow(selected_benchmarks_complete), "."),
  paste0("- Core all-years included PVR rows: ", sum(master_pvr_scenarios_all_years$coverage_flag == "included", na.rm = TRUE), "."),
  paste0("- 2024 all-lender included PVR rows: ", sum(pvr_2024_all_lenders_expansion$coverage_flag == "included", na.rm = TRUE), "."),
  paste0("- Historical Damodaran archive spread rows parsed: ", nrow(damodaran_archive_spreads), "."),
  "",
  "## Boundary",
  "",
  "These outputs are for learning, diagnostics, and future rule approval. They do not update `docs/DECISIONS.md`, do not overwrite canonical `output/tables/*.csv`, and should not be cited as accepted results."
)

warnings_lines <- c(
  "# Method Warnings",
  "",
  "- This is an experimental maximum-coverage database, not accepted methodology.",
  "- Pending approvals are handled by computing multiple variants, not by blocking the calculation.",
  "- Some variants are deliberately weak, especially current-vintage PRS sensitivities and peer proxies.",
  "- The preferred experimental rating-implied model variant uses annual Damodaran archive spreads for 2000-2024; the separate current-vintage rating variant remains a sensitivity assumption, not a historical spread panel.",
  "- Peer-proxy scenarios use income-year medians and do not exclude the target country from its own peer pool.",
  "- Secondary-market scenarios use direct LSEG YTM only; quote-to-yield reconstruction from prices is not attempted.",
  "- Broad all-lender PVR results for 2024 are mechanically computed for available IDS terms but creditor eligibility is not approved.",
  "- Canonical project outputs remain untouched."
)

agent_worklog_lines <- c(
  "# Agent Worklog",
  "",
  "Date: 2026-05-20",
  "",
  "## Main Codex Agent",
  "",
  "- Created the experiment-local all-years scenario database script.",
  "- Loaded LSEG v14 primary, secondary, rating, and risk-free files from the experiment folder.",
  "- Loaded or fetched World Bank IDS all-year terms for IBRD, IDA, China, and Bondholders into the experiment cache.",
  "- Computed multiple primary-market, secondary-market, IDS, rating-implied, PRS shadow, and peer-proxy benchmark scenarios.",
  "- Added the Damodaran annual archive files as an improved historical rating-implied sensitivity layer after user review flagged that the first run had used only a current-vintage workbook.",
  "- Built several ladder variants so pending rule choices become separate selected-rate outputs.",
  "- Computed core all-years PVR rows for Bonds, IBRD, IDA, and China, plus a 2024 all-lender expansion.",
  "- Wrote CSV, Markdown, and Excel outputs under the experiment folder only.",
  "",
  "## Subagents",
  "",
  "- Parfit completed a read-only inspection of LSEG v14, FRED, Damodaran, and IDS availability. It confirmed primary coverage for `2000-2025`, secondary and LSEG risk-free coverage for `2020-2025`, local IDS being `2024` only, IDS API feasibility for all years, and the need to compute cascade/order variants rather than choose a single rule prematurely.",
  "- Lagrange completed a read-only audit of likely pitfalls for the all-years run. It highlighted the need to cache IDS API responses, preserve LSEG primary key fields, avoid treating the broad LSEG base as already screened benchmark data, treat Damodaran as current-vintage rather than historical, and include traceability/status fields in the master database.",
  "- The main Codex agent incorporated those findings by adding `source_year_limits.csv`, API-cached `ids_terms_core_all_years.csv`, issue-level primary/secondary traceability outputs, named scenario variants, ladder variants, method warnings, and non-canonical status labels."
)

readr::write_lines(coverage_lines, file.path(experiment_dir, "coverage_and_missingness.md"))
readr::write_lines(readme_lines, file.path(experiment_dir, "README.md"))
readr::write_lines(warnings_lines, file.path(experiment_dir, "method_warnings.md"))
readr::write_lines(agent_worklog_lines, file.path(experiment_dir, "agent_worklog.md"))

if (requireNamespace("openxlsx", quietly = TRUE)) {
  wb <- openxlsx::createWorkbook()
  add_sheet <- function(name, data) {
    openxlsx::addWorksheet(wb, name)
    if (nrow(data) > 1000000) {
      note <- tibble(
        note = paste0("Full table has ", nrow(data), " rows and is stored as CSV. This sheet contains the first 1000000 rows."),
        full_csv = paste0("outputs/", name, ".csv")
      )
      openxlsx::writeData(wb, name, note, startRow = 1)
      openxlsx::writeData(wb, name, utils::head(data, 1000000), startRow = 4)
    } else {
      openxlsx::writeData(wb, name, data)
    }
  }
  add_sheet("README", tibble(line = readme_lines))
  add_sheet("data_inventory", data_inventory)
  add_sheet("source_year_limits", source_year_limits)
  add_sheet("scenario_dictionary", scenario_dictionary)
  add_sheet("ladder_component_inventory", ladder_component_inventory)
  add_sheet("damodaran_archive", damodaran_archive_spreads)
  add_sheet("country_year_coverage", country_year_coverage)
  add_sheet("primary_issue_level", primary_issue_level_all_years)
  add_sheet("secondary_issue_level", secondary_issue_level_all_years)
  add_sheet("spread_normalized", spread_normalized_usd_eur)
  add_sheet("benchmark_scenarios", benchmark_scenarios)
  add_sheet("selected_benchmarks", selected_benchmarks_complete)
  add_sheet("pvr_core_all_years", master_pvr_scenarios_all_years)
  add_sheet("pvr_2024_all_lenders", pvr_2024_all_lenders_expansion)
  add_sheet("pvr_coverage_summary", pvr_coverage_summary)
  add_sheet("method_warnings", method_warnings)
  openxlsx::saveWorkbook(wb, file.path(out_dir, "pvr_global_all_years_master.xlsx"), overwrite = TRUE)
} else {
  readr::write_lines(
    "openxlsx is not installed; CSV outputs are the authoritative artifacts.",
    file.path(out_dir, "xlsx_not_created.txt")
  )
}

cat("All-years experimental scenario database written to ", normalizePath(out_dir), "\n", sep = "")
cat("Benchmark scenario rows: ", nrow(benchmark_scenarios), "\n", sep = "")
cat("Selected benchmark/status rows by ladder: ", nrow(selected_benchmarks_complete), "\n", sep = "")
cat("Core all-years included PVR rows: ", sum(master_pvr_scenarios_all_years$coverage_flag == "included", na.rm = TRUE), "\n", sep = "")
cat("2024 all-lender included PVR rows: ", sum(pvr_2024_all_lenders_expansion$coverage_flag == "included", na.rm = TRUE), "\n", sep = "")
