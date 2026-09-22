# Execute only explicitly named pure parsing definitions/expressions from the
# preserved scripts. Never source their network-capable top-level workflows.
p15_legacy_assignments <- function(path, names, envir, functions_only = FALSE) {
  expressions <- parse(path)
  available <- vapply(expressions, function(e) {
    if (is.call(e) && is.symbol(e[[1]]) && as.character(e[[1]]) %in% c("<-", "=") && is.symbol(e[[2]])) as.character(e[[2]]) else ""
  }, character(1))
  for (name in names) {
    at <- which(available == name)
    if (length(at) != 1L) stop("Ambiguous legacy definition: ", name)
    expr <- expressions[[at]]
    if (functions_only && !(is.call(expr[[3]]) && identical(expr[[3]][[1]], as.name("function")))) stop("Not a function definition: ", name)
    eval(expr, envir = envir)
  }
  invisible(envir)
}

p15_read_ids_cached_pages <- function(directory, creditor = "BND") {
  records <- list()
  for (series in c("DT_INR_DPPG", "DT_MAT_DPPG", "DT_GPA_DPPG")) {
    first <- file.path(directory, paste0("ids_", creditor, "_", series, "_page_1.json"))
    meta <- jsonlite::fromJSON(first)
    n <- as.integer(meta$pages)
    if (length(n) != 1 || is.na(n) || n < 1) stop("Invalid IDS page metadata")
    for (page in seq_len(n)) {
      path <- file.path(directory, paste0("ids_", creditor, "_", series, "_page_", page, ".json"))
      response <- jsonlite::fromJSON(path)
      if(as.integer(response$page)!=page || as.integer(response$pages)!=n || response$source$id!="6") stop("IDS page/source mismatch")
      raw <- response$source$data
      if(nrow(raw) < 1L) stop("Empty IDS page")
      get <- function(x, field, values = FALSE) x[[if(values) "value" else "id"]][match(field, x$concept)]
      records[[paste(series,page)]] <- tibble::tibble(
        iso3 = vapply(raw$variable, get, character(1), field = "Country"),
        country = vapply(raw$variable, get, character(1), field = "Country", values = TRUE),
        year = as.integer(sub("YR", "", vapply(raw$variable, get, character(1), field = "Time"))),
        indicator_id = vapply(raw$variable, get, character(1), field = "Series"),
        creditor_id = vapply(raw$variable, get, character(1), field = "Counterpart-Area"),
        value = raw$value, source_file = path, source_row = seq_len(nrow(raw)))
    }
    count <- sum(vapply(records[paste(series,seq_len(n))],nrow,integer(1)))
    if(count != as.integer(meta$total)) stop("IDS pagination row count does not match metadata")
  }
  x <- dplyr::bind_rows(records)
  if (anyDuplicated(x[c("iso3", "year", "indicator_id", "creditor_id")])) stop("Duplicate IDS source keys")
  if (any(x$creditor_id != creditor)) stop("Wrong IDS creditor category")
  if (any(is.na(x$iso3)|!nzchar(x$iso3)|is.na(x$year)|is.na(x$indicator_id))) stop("Missing IDS source keys")
  x
}
