read_country_metadata <- function(path) {
  raw <- jsonlite::fromJSON(path, flatten = TRUE)
  countries <- raw[[2]]

  countries |>
    dplyr::transmute(
      iso3 = .data$id,
      iso2 = .data$iso2Code,
      country = .data$name,
      region = .data$region.value,
      income_level = .data$incomeLevel.value,
      lending_type = .data$lendingType.value,
      is_country = !is.na(.data$capitalCity) & .data$capitalCity != "" & .data$incomeLevel.value != "Aggregates"
    )
}

read_counterpart_areas <- function(path) {
  raw <- jsonlite::fromJSON(path, flatten = TRUE)
  variables <- raw$source$concept[[1]]$variable[[1]]

  variables |>
    dplyr::transmute(
      creditor_id = .data$id,
      creditor_name = trimws(gsub("\\s+", " ", .data$value)),
      creditor_scope = dplyr::case_when(
        .data$id == "BND" ~ "market_bondholders",
        .data$id %in% c("WLD", "994", "FCW") ~ "aggregate_or_multiple",
        grepl("Multiple|World", .data$value, ignore.case = TRUE) ~ "aggregate_or_multiple",
        grepl("Bank|Fund|Corporation|Organization|Programme|Union|Commission|Facility|Association|Community|Council|Agency|UN-|International|European|African|Asian|Arab|Islamic|Nordic|OPEC|IMF|IFAD|EBRD", .data$value, ignore.case = TRUE) ~ "institution",
        TRUE ~ "bilateral_or_country"
      ),
      current_phase1_use = dplyr::case_when(
        .data$id == "BND" ~ "market_benchmark",
        .data$id %in% c("901", "905", "730") ~ "computed_creditor",
        TRUE ~ "available_metadata_only"
      )
    ) |>
    dplyr::arrange(.data$creditor_id)
}

parse_ids_data_file <- function(path) {
  raw <- jsonlite::fromJSON(path, flatten = FALSE)
  if (is.null(raw$source$data) || length(raw$source$data) == 0) {
    return(tibble::tibble())
  }

  data <- raw$source$data
  parsed <- lapply(seq_len(nrow(data)), function(i) {
    variables <- data$variable[[i]]
    concepts <- stats::setNames(variables$value, variables$concept)
    ids <- stats::setNames(variables$id, variables$concept)
    tibble::tibble(
      iso3 = ids[["Country"]],
      country = concepts[["Country"]],
      year = as.integer(sub("^YR", "", ids[["Time"]])),
      indicator_id = ids[["Series"]],
      indicator = concepts[["Series"]],
      creditor_id = ids[["Counterpart-Area"]],
      creditor_raw = trimws(gsub("\\s+", " ", concepts[["Counterpart-Area"]])),
      value = as.numeric(data$value[[i]])
    )
  })

  dplyr::bind_rows(parsed)
}

creditor_label <- function(creditor_id) {
  dplyr::case_when(
    creditor_id == "901" ~ "IBRD",
    creditor_id == "905" ~ "IDA",
    creditor_id == "730" ~ "China",
    creditor_id == "BND" ~ "Bondholders",
    TRUE ~ creditor_id
  )
}

term_label <- function(indicator_id) {
  dplyr::case_when(
    indicator_id == "DT.INR.DPPG" ~ "official_rate",
    indicator_id == "DT.MAT.DPPG" ~ "official_maturity_years",
    indicator_id == "DT.GPA.DPPG" ~ "official_grace_years",
    TRUE ~ indicator_id
  )
}

build_ids_terms <- function(raw_dir, country_metadata, counterpart_areas = NULL) {
  files <- list.files(raw_dir, pattern = "\\.json$", full.names = TRUE)
  if (length(files) == 0) stop("No IDS term files found.", call. = FALSE)

  terms_long <- purrr::map_dfr(files, parse_ids_data_file)

  terms <- terms_long |>
    dplyr::filter(.data$year == 2024) |>
    dplyr::mutate(
      creditor = creditor_label(.data$creditor_id),
      term = term_label(.data$indicator_id)
    ) |>
    dplyr::filter(.data$term %in% c("official_rate", "official_maturity_years", "official_grace_years")) |>
    dplyr::select("iso3", "country", "year", "creditor", "creditor_id", "term", "value") |>
    tidyr::pivot_wider(names_from = "term", values_from = "value") |>
    dplyr::left_join(
      country_metadata |> dplyr::select("iso3", "income_level", "lending_type", "is_country"),
      by = "iso3"
    )

  if (!is.null(counterpart_areas)) {
    terms <- terms |>
      dplyr::left_join(
        counterpart_areas |> dplyr::select("creditor_id", "creditor_name", "creditor_scope"),
        by = "creditor_id"
      )
  }

  terms |>
    dplyr::filter(.data$is_country) |>
    dplyr::select(-"is_country") |>
    dplyr::mutate(
      has_complete_terms = !is.na(.data$official_rate) &
        !is.na(.data$official_maturity_years) &
        !is.na(.data$official_grace_years) &
        .data$official_maturity_years > 0 &
        .data$official_grace_years >= 0 &
        .data$official_grace_years < .data$official_maturity_years
    ) |>
    dplyr::arrange(.data$country, .data$creditor)
}
