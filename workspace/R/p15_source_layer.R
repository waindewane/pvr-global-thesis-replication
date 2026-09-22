# Functions for the P15 normalized LSEG source layer.
# These functions standardize raw history chunks without deciding whether an
# observation is an admissible benchmark rate.

p15_empty_history <- function() {
  data.frame(
    analysis_year = integer(),
    history_date = character(),
    source_subrow = integer(),
    RIC = character(),
    mid_price = numeric(),
    bid = numeric(),
    ask = numeric(),
    yield_to_maturity = numeric(),
    any_observed_measure = logical(),
    available_measure_set = character(),
    history_window_start = character(),
    history_window_end = character(),
    source_chunk_no = integer(),
    source_run_id = character(),
    history_fields_requested = character(),
    source_package_id = character(),
    source_role = character(),
    source_archive_member = character(),
    source_record_locator = character(),
    stringsAsFactors = FALSE
  )
}

p15_measure_key <- function(x) {
  key <- gsub("[^a-z0-9]", "", tolower(x))
  out <- rep(NA_character_, length(key))
  out[key %in% c("mid", "midprice")] <- "mid_price"
  out[key == "bid"] <- "bid"
  out[key == "ask"] <- "ask"
  out[key %in% c("yieldtomaturity", "tryieldtomaturity")] <- "yield_to_maturity"
  out
}

p15_parse_measure_columns <- function(column_names) {
  split_at <- regexpr("__", column_names, fixed = TRUE)
  keep <- split_at > 0L
  data.frame(
    column = column_names[keep],
    RIC = substring(column_names[keep], 1L, split_at[keep] - 1L),
    raw_measure = substring(column_names[keep], split_at[keep] + 2L),
    measure = p15_measure_key(substring(column_names[keep], split_at[keep] + 2L)),
    stringsAsFactors = FALSE
  )
}

p15_numeric_column <- function(x, column_name, n) {
  if (!column_name %in% names(x)) return(rep(NA_real_, n))
  suppressWarnings(as.numeric(x[[column_name]]))
}

p15_character_column <- function(x, column_name, n) {
  if (!column_name %in% names(x)) return(rep(NA_character_, n))
  as.character(x[[column_name]])
}

p15_integer_column <- function(x, column_name, n) {
  if (!column_name %in% names(x)) return(rep(NA_integer_, n))
  suppressWarnings(as.integer(x[[column_name]]))
}

p15_normalize_secondary_chunk <- function(
    x,
    source_package_id,
    source_role,
    source_archive_member,
    keep_rics = NULL) {
  if (!is.data.frame(x)) stop("x must be a data frame.", call. = FALSE)
  if (!nrow(x)) return(p15_empty_history())
  if (!"Date" %in% names(x)) {
    stop("Secondary-history chunk is missing Date: ", source_archive_member, call. = FALSE)
  }

  parsed <- p15_parse_measure_columns(names(x))
  parsed <- parsed[!is.na(parsed$measure), , drop = FALSE]
  rics <- unique(parsed$RIC)
  if (!is.null(keep_rics)) rics <- intersect(rics, keep_rics)
  if (!length(rics)) return(p15_empty_history())

  rows <- lapply(rics, function(ric) {
    mapping <- parsed[parsed$RIC == ric, , drop = FALSE]
    if (anyDuplicated(mapping$measure)) {
      stop("Duplicate normalized measure for RIC ", ric, " in ", source_archive_member, call. = FALSE)
    }
    lookup <- stats::setNames(mapping$column, mapping$measure)
    n <- nrow(x)
    get_measure <- function(measure) {
      column <- unname(lookup[measure])
      if (!length(column) || is.na(column)) return(rep(NA_real_, n))
      p15_numeric_column(x, column, n)
    }
    mid <- get_measure("mid_price")
    bid <- get_measure("bid")
    ask <- get_measure("ask")
    ytm <- get_measure("yield_to_maturity")
    observed <- !is.na(mid) | !is.na(bid) | !is.na(ask) | !is.na(ytm)
    measures <- intersect(
      c("mid_price", "bid", "ask", "yield_to_maturity"),
      mapping$measure
    )
    analysis_year <- p15_integer_column(x, "snapshot_year", n)
    history_date <- p15_character_column(x, "Date", n)
    source_subrow <- ave(seq_len(n), history_date, FUN = seq_along)
    data.frame(
      analysis_year = analysis_year,
      history_date = history_date,
      source_subrow = as.integer(source_subrow),
      RIC = ric,
      mid_price = mid,
      bid = bid,
      ask = ask,
      yield_to_maturity = ytm,
      any_observed_measure = observed,
      available_measure_set = paste(measures, collapse = ";"),
      history_window_start = p15_character_column(x, "history_window_start", n),
      history_window_end = p15_character_column(x, "history_window_end", n),
      source_chunk_no = p15_integer_column(x, "chunk_no", n),
      source_run_id = p15_character_column(x, "run_id", n),
      history_fields_requested = p15_character_column(x, "history_fields_requested", n),
      source_package_id = source_package_id,
      source_role = source_role,
      source_archive_member = source_archive_member,
      source_record_locator = paste(
        source_archive_member,
        ric,
        history_date,
        source_subrow,
        sep = "::"
      ),
      stringsAsFactors = FALSE
    )
  })
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

p15_validate_history_layer <- function(x) {
  required <- names(p15_empty_history())
  missing <- setdiff(required, names(x))
  if (length(missing)) {
    stop("P15 history layer is missing: ", paste(missing, collapse = ", "), call. = FALSE)
  }
  key <- paste(x$analysis_year, x$history_date, x$source_subrow, x$RIC, sep = "::")
  if (anyDuplicated(key)) {
    stop("P15 history layer has duplicate year-date-source-subrow-RIC keys.", call. = FALSE)
  }
  if (any(is.na(x$source_package_id) | !nzchar(x$source_package_id))) {
    stop("P15 history rows require source_package_id.", call. = FALSE)
  }
  if (any(is.na(x$source_record_locator) | !nzchar(x$source_record_locator))) {
    stop("P15 history rows require source_record_locator.", call. = FALSE)
  }
  invisible(TRUE)
}
