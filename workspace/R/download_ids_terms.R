source("R/ids.R")

download_json <- function(url, path, retries = 3) {
  if (file.exists(path) && file.info(path)$size > 0) {
    return(path)
  }

  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)

  for (attempt in seq_len(retries)) {
    try(
      utils::download.file(url, path, quiet = TRUE, mode = "wb"),
      silent = TRUE
    )
    if (file.exists(path) && file.info(path)$size > 0) {
      return(path)
    }
    Sys.sleep(attempt)
  }

  stop("Failed to download: ", url, call. = FALSE)
}

download_ids_terms <- function(
  counterpart_path = "data-raw/ids_counterpart_area.json",
  output_dir = "data-raw/ids_terms_all_lenders",
  year = 2024
) {
  counterparts <- read_counterpart_areas(counterpart_path)
  indicators <- c("DT.INR.DPPG", "DT.MAT.DPPG", "DT.GPA.DPPG")
  base_url <- "https://api.worldbank.org/v2/sources/6/country/all/series/%s/counterpart-area/%s/time/YR%s?format=json&per_page=5000"

  jobs <- expand.grid(
    indicator = indicators,
    creditor_id = counterparts$creditor_id,
    stringsAsFactors = FALSE
  )

  paths <- character(nrow(jobs))
  for (i in seq_len(nrow(jobs))) {
    indicator <- jobs$indicator[[i]]
    creditor_id <- jobs$creditor_id[[i]]
    url <- sprintf(base_url, indicator, utils::URLencode(creditor_id, reserved = TRUE), year)
    path <- file.path(output_dir, sprintf("%s__%s__YR%s.json", indicator, creditor_id, year))
    paths[[i]] <- download_json(url, path)
    if (i %% 50 == 0) {
      message("Downloaded or verified ", i, " / ", nrow(jobs), " IDS files")
    }
  }

  invisible(paths)
}

if (identical(environment(), globalenv())) {
  download_ids_terms()
}

