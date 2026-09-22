#!/usr/bin/env Rscript

# P15 source integration: normalize the latest broad 2012-2024 LSEG history
# capture and fill its 24 exact-match 2024 tail identifiers from the established
# complete-2024 archive. The immutable source ZIPs are never rewritten.

options(stringsAsFactors = FALSE)

root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
source(file.path(root, "R", "research_governance.R"))
source(file.path(root, "R", "p15_source_layer.R"))

if (!requireNamespace("data.table", quietly = TRUE)) {
  stop("P15 source build requires data.table.", call. = FALSE)
}
if (!requireNamespace("R.utils", quietly = TRUE)) {
  stop("P15 source build requires R.utils for gzip output.", call. = FALSE)
}

base_source_id <- "SRC-LSEG-EXT-20260623-R4"
reference_source_id <- "SRC-LSEG-RG-20260522"
build_id <- "BUILD-P15-LSEG-SOURCE-20260721"
schema_id <- "SCHEMA-P15-LSEG-HISTORY-V1"
estimator_id <- "EST-P15-SOURCE-NORMALIZATION-V1"
admissibility_id <- "ADM-P15-SOURCE-COVERAGE-V1"
selection_id <- "SEL-P15-SOURCE-PRECEDENCE-V1"

base_zip <- file.path(
  root,
  "sources/market_rates/lseg_extension_2012_2024_partial_resume_20260721",
  "pvr-global-lseg-extension-2012-2024-usd-eur_20260623_184707_partial_resume_20260721.zip"
)
reference_zip <- file.path(
  root,
  "sources/market_rates/lseg_research_grade_usd_eur_2026-05-22",
  "pvr-global-lseg-research-grade-usd-eur_20260522_143419.zip"
)
missing_path <- file.path(
  root,
  "docs/audit_first_wave/lseg_latest_resume_missing_2024_identifiers_2026-07-21.csv"
)
stopifnot(file.exists(base_zip), file.exists(reference_zip), file.exists(missing_path))

output_dir <- file.path(root, "data-derived", "p15_unified_lseg_source_2012_2024_v1")
governance_dir <- file.path(root, "docs", "governance")
tmp_dir <- file.path(root, "tmp")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(governance_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(tmp_dir, recursive = TRUE, showWarnings = FALSE)

history_output <- file.path(output_dir, "p15_secondary_history_long_2012_2024.csv.gz")
universe_output <- file.path(output_dir, "p15_instrument_year_universe_2012_2024.csv.gz")
history_tmp <- file.path(tmp_dir, "p15_secondary_history_long_2012_2024.csv")
universe_tmp <- file.path(tmp_dir, "p15_instrument_year_universe_2012_2024.csv")
unlink(c(history_output, universe_output, history_tmp, universe_tmp), force = TRUE)

pvr_assert_write_allowed(
  history_output,
  lifecycle_status = "candidate",
  root_dir = root
)

zip_inventory <- function(path) {
  info <- utils::unzip(path, list = TRUE)
  root_prefix <- sub("(^[^/]+/).*", "\\1", info$Name[[1]])
  info$relative_path <- sub(paste0("^", root_prefix), "", info$Name)
  info
}

single_member <- function(info, pattern) {
  members <- info$Name[grepl(pattern, info$Name)]
  if (length(members) != 1L) {
    stop("Expected one member for ", pattern, "; found ", length(members), call. = FALSE)
  }
  members[[1]]
}

read_member_csv <- function(zip_path, info, pattern) {
  selected <- single_member(info, pattern)
  td <- tempfile("p15_member_")
  dir.create(td, recursive = TRUE)
  on.exit(unlink(td, recursive = TRUE, force = TRUE), add = TRUE)
  utils::unzip(zip_path, files = selected, exdir = td)
  data.table::fread(
    file.path(td, selected),
    data.table = FALSE,
    check.names = FALSE,
    showProgress = FALSE
  )
}

chunk_pattern <- paste0(
  "raw/secondary_history_chunks/",
  "lseg_secondary_history_year_[0-9]{4}_chunk_[0-9]{5}_.*[.]csv$"
)
chunk_year <- function(member) as.integer(sub(".*year_([0-9]{4})_chunk_.*", "\\1", member))
chunk_number <- function(member) as.integer(sub(".*chunk_([0-9]{5})_.*", "\\1", member))

base_info <- zip_inventory(base_zip)
reference_info <- zip_inventory(reference_zip)
base_chunk_rows <- base_info[
  grepl(chunk_pattern, base_info$relative_path) & base_info$Length > 2,
  ,
  drop = FALSE
]
base_chunk_rows$analysis_year <- chunk_year(base_chunk_rows$relative_path)
base_chunk_rows$chunk_no <- chunk_number(base_chunk_rows$relative_path)
base_chunk_rows <- base_chunk_rows[
  order(base_chunk_rows$analysis_year, base_chunk_rows$chunk_no),
  ,
  drop = FALSE
]

extract_dir <- tempfile("p15_history_chunks_")
dir.create(extract_dir, recursive = TRUE)
on.exit(unlink(extract_dir, recursive = TRUE, force = TRUE), add = TRUE)

cat("Extracting broad archive once for streaming normalization...\n")
# Extracting the whole compact archive is substantially faster than asking the
# base R ZIP reader to seek separately to 7,380 named members. The temporary
# directory is deleted before the final validation read.
utils::unzip(base_zip, exdir = extract_dir)

header_written <- FALSE
base_rows_written <- 0L
base_observed_rows <- 0L
batch <- vector("list", 250L)
batch_n <- 0L

flush_batch <- function() {
  if (!batch_n) return(invisible(NULL))
  combined <- data.table::rbindlist(batch[seq_len(batch_n)], use.names = TRUE, fill = TRUE)
  data.table::fwrite(
    combined,
    history_tmp,
    append = header_written,
    col.names = !header_written,
    na = ""
  )
  header_written <<- TRUE
  batch_n <<- 0L
  batch <<- vector("list", 250L)
  invisible(NULL)
}

cat("Normalizing broad-archive chunks...\n")
for (i in seq_len(nrow(base_chunk_rows))) {
  member <- base_chunk_rows$Name[[i]]
  x <- data.table::fread(
    file.path(extract_dir, member),
    data.table = FALSE,
    check.names = FALSE,
    showProgress = FALSE
  )
  normalized <- p15_normalize_secondary_chunk(
    x,
    source_package_id = base_source_id,
    source_role = "broad_archive_base",
    source_archive_member = member
  )
  base_rows_written <- base_rows_written + nrow(normalized)
  base_observed_rows <- base_observed_rows + sum(normalized$any_observed_measure)
  batch_n <- batch_n + 1L
  batch[[batch_n]] <- normalized
  if (batch_n == length(batch)) flush_batch()
  if (i %% 500L == 0L) cat("  processed", i, "of", nrow(base_chunk_rows), "chunks\n")
}
flush_batch()

missing <- read.csv(missing_path, check.names = FALSE, stringsAsFactors = FALSE)
supplement_rics <- missing$RIC[missing$present_in_reference_2024_by_ric]
excluded_rics <- missing$RIC[!missing$present_in_reference_2024_by_ric]
stopifnot(length(supplement_rics) == 24L, length(excluded_rics) == 4L)

reference_universe <- read_member_csv(
  reference_zip,
  reference_info,
  "derived/lseg_secondary_history_universe_all_ric_years_.*[.]csv$"
)
reference_2024 <- reference_universe[
  reference_universe$snapshot_year == 2024 &
    !is.na(reference_universe$RIC) & nzchar(reference_universe$RIC),
  ,
  drop = FALSE
]
reference_2024 <- reference_2024[!duplicated(reference_2024$RIC), , drop = FALSE]
reference_2024$reference_chunk <- ceiling(seq_len(nrow(reference_2024)) / 5)
needed_reference_chunks <- unique(
  reference_2024$reference_chunk[reference_2024$RIC %in% supplement_rics]
)

reference_chunk_rows <- reference_info[
  grepl(chunk_pattern, reference_info$relative_path),
  ,
  drop = FALSE
]
reference_chunk_rows <- reference_chunk_rows[
  chunk_year(reference_chunk_rows$relative_path) == 2024,
  ,
  drop = FALSE
]
reference_chunk_rows$chunk_no <- chunk_number(reference_chunk_rows$relative_path)
reference_chunk_rows <- reference_chunk_rows[
  reference_chunk_rows$chunk_no %in% needed_reference_chunks,
  ,
  drop = FALSE
]
stopifnot(nrow(reference_chunk_rows) == length(needed_reference_chunks))
utils::unzip(reference_zip, files = reference_chunk_rows$Name, exdir = extract_dir)

supplement_list <- lapply(seq_len(nrow(reference_chunk_rows)), function(i) {
  member <- reference_chunk_rows$Name[[i]]
  x <- data.table::fread(
    file.path(extract_dir, member),
    data.table = FALSE,
    check.names = FALSE,
    showProgress = FALSE
  )
  p15_normalize_secondary_chunk(
    x,
    source_package_id = reference_source_id,
    source_role = "2024_tail_fill_price_history",
    source_archive_member = member,
    keep_rics = supplement_rics
  )
})
supplement <- data.table::rbindlist(supplement_list, use.names = TRUE, fill = TRUE)
if (!setequal(unique(supplement$RIC), supplement_rics)) {
  stop("Reference supplement did not resolve all 24 exact-match RICs.", call. = FALSE)
}
data.table::fwrite(supplement, history_tmp, append = TRUE, col.names = FALSE, na = "")

supplement_audit <- do.call(rbind, lapply(supplement_rics, function(ric) {
  x <- supplement[supplement$RIC == ric]
  source_row <- missing[missing$RIC == ric, , drop = FALSE]
  any_quote <- sum(x$any_observed_measure) > 0L
  data.frame(
    RIC = ric,
    ISIN = source_row$ISIN,
    canonical_iso3 = source_row$canonical_iso3,
    canonical_country = source_row$canonical_country,
    source_package_id = reference_source_id,
    source_archive_member = unique(x$source_archive_member)[[1]],
    history_rows = nrow(x),
    rows_with_any_price_quote = sum(!is.na(x$mid_price) | !is.na(x$bid) | !is.na(x$ask)),
    rows_with_direct_yield = sum(!is.na(x$yield_to_maturity)),
    merge_state = if (any_quote) {
      "supplemented_with_price_history"
    } else {
      "supplemented_identifier_but_no_observed_history_quote"
    },
    stringsAsFactors = FALSE
  )
}))
excluded_audit <- data.frame(
  RIC = excluded_rics,
  ISIN = missing$ISIN[match(excluded_rics, missing$RIC)],
  canonical_iso3 = missing$canonical_iso3[match(excluded_rics, missing$RIC)],
  canonical_country = missing$canonical_country[match(excluded_rics, missing$RIC)],
  source_package_id = "",
  source_archive_member = "",
  history_rows = 0L,
  rows_with_any_price_quote = 0L,
  rows_with_direct_yield = 0L,
  merge_state = "intentionally_excluded_us_treasury_strip_outside_benchmark_scope",
  stringsAsFactors = FALSE
)
tail_audit <- rbind(supplement_audit, excluded_audit)
tail_audit <- tail_audit[match(missing$RIC, tail_audit$RIC), , drop = FALSE]
write.csv(
  tail_audit,
  file.path(governance_dir, "p15_2024_tail_merge_audit.csv"),
  row.names = FALSE,
  na = ""
)

cat("Compressing normalized history layer...\n")
Sys.setFileTime(history_tmp, as.POSIXct("2026-07-21 00:00:00", tz = "UTC"))
R.utils::gzip(history_tmp, destname = history_output, overwrite = TRUE, remove = TRUE)

base_universe <- read_member_csv(
  base_zip,
  base_info,
  "derived/lseg_secondary_history_universe_all_ric_years_.*[.]csv$"
)
base_universe <- base_universe[
  !is.na(base_universe$snapshot_year) &
    !is.na(base_universe$RIC) & nzchar(base_universe$RIC),
  ,
  drop = FALSE
]
base_universe <- base_universe[
  !duplicated(paste(base_universe$snapshot_year, base_universe$RIC, sep = "::")),
  ,
  drop = FALSE
]
base_universe$p15_base_planned_chunk <- NA_integer_
for (year in sort(unique(base_universe$snapshot_year))) {
  idx <- which(base_universe$snapshot_year == year)
  base_universe$p15_base_planned_chunk[idx] <- ceiling(seq_along(idx) / 3)
}
written_key <- paste(base_chunk_rows$analysis_year, base_chunk_rows$chunk_no, sep = "::")
universe_key <- paste(
  base_universe$snapshot_year,
  base_universe$p15_base_planned_chunk,
  sep = "::"
)
base_written <- universe_key %in% written_key
is_supplement <- base_universe$snapshot_year == 2024 & base_universe$RIC %in% supplement_rics
is_excluded <- base_universe$snapshot_year == 2024 & base_universe$RIC %in% excluded_rics
base_universe$p15_history_coverage_state <- ifelse(
  base_written,
  "broad_archive_written",
  ifelse(
    is_supplement,
    "reference_2024_tail_fill_price_history",
    ifelse(is_excluded, "intentionally_excluded_us_treasury_strip", "unresolved_missing_history")
  )
)
base_universe$p15_history_source_package_id <- ifelse(
  base_written,
  base_source_id,
  ifelse(is_supplement, reference_source_id, "")
)
base_universe$p15_history_source_role <- ifelse(
  base_written,
  "broad_archive_base",
  ifelse(is_supplement, "2024_tail_fill_price_history", "not_in_p15_history")
)
base_universe$p15_available_measure_contract <- ifelse(
  base_written,
  "mid_price_bid_ask_and_direct_yield_requested",
  ifelse(is_supplement, "mid_price_bid_ask_requested_no_direct_yield", "no_history_measure")
)
base_universe$p15_source_record_locator <- paste(
  base_universe$snapshot_year,
  base_universe$RIC,
  base_universe$p15_history_source_package_id,
  sep = "::"
)
data.table::fwrite(base_universe, universe_tmp, na = "")
Sys.setFileTime(universe_tmp, as.POSIXct("2026-07-21 00:00:00", tz = "UTC"))
R.utils::gzip(universe_tmp, destname = universe_output, overwrite = TRUE, remove = TRUE)

# Remove extracted temporary members before reading the compact final product.
unlink(extract_dir, recursive = TRUE, force = TRUE)
gc()

history <- data.table::fread(history_output, showProgress = FALSE)
required_history_fields <- names(p15_empty_history())
if (!all(required_history_fields %in% names(history))) {
  stop("Final P15 history output does not satisfy its schema.", call. = FALSE)
}
raw_multirow_keys <- history[, .N, by = .(analysis_year, history_date, RIC)][N > 1L]
duplicates <- history[
  ,
  .N,
  by = .(analysis_year, history_date, source_subrow, RIC)
][N > 1L]
if (nrow(duplicates)) {
  stop("Final P15 history contains duplicate normalized observation keys.", call. = FALSE)
}
if (any(is.na(history$source_package_id) | !nzchar(history$source_package_id))) {
  stop("Final P15 history contains missing source-package IDs.", call. = FALSE)
}
unresolved_universe <- sum(base_universe$p15_history_coverage_state == "unresolved_missing_history")
if (unresolved_universe != 0L) {
  stop("P15 universe still has ", unresolved_universe, " unresolved history rows.", call. = FALSE)
}

summary <- data.frame(
  metric = c(
    "base_archive_written_chunks",
    "base_normalized_history_rows",
    "base_rows_with_any_observed_measure",
    "supplement_exact_match_identifiers",
    "supplement_identifiers_with_any_price_quote",
    "supplement_identifiers_without_history_quote",
    "supplement_rows",
    "supplement_rows_with_direct_yield",
    "intentionally_excluded_us_treasury_strips",
    "unresolved_instrument_year_history_records",
    "p15_total_history_rows",
    "p15_total_rows_with_any_observed_measure",
    "p15_unique_instrument_years",
    "p15_unique_2024_rics_in_history",
    "raw_same_date_multirow_keys_preserved"
  ),
  value = c(
    nrow(base_chunk_rows),
    base_rows_written,
    base_observed_rows,
    length(supplement_rics),
    sum(supplement_audit$rows_with_any_price_quote > 0L),
    sum(supplement_audit$rows_with_any_price_quote == 0L),
    nrow(supplement),
    sum(!is.na(supplement$yield_to_maturity)),
    length(excluded_rics),
    unresolved_universe,
    nrow(history),
    sum(history$any_observed_measure),
    nrow(base_universe),
    data.table::uniqueN(history[analysis_year == 2024, RIC]),
    nrow(raw_multirow_keys)
  ),
  stringsAsFactors = FALSE
)
summary_path <- file.path(governance_dir, "p15_source_layer_build_summary.csv")
write.csv(summary, summary_path, row.names = FALSE)

manifest_paths <- c(
  base_zip,
  reference_zip,
  history_output,
  universe_output,
  file.path(governance_dir, "p15_2024_tail_merge_audit.csv"),
  summary_path,
  file.path(root, "R", "p15_source_layer.R"),
  file.path(root, "scripts", "p15", "build_p15_unified_lseg_source_layer.R")
)
manifest_roles <- c(
  "immutable_broad_source_archive",
  "immutable_2024_tail_reference_archive",
  "normalized_secondary_history",
  "instrument_year_universe",
  "tail_merge_audit",
  "build_summary",
  "normalization_functions",
  "builder_script"
)
manifest <- do.call(rbind, lapply(seq_along(manifest_paths), function(i) {
  path <- normalizePath(manifest_paths[[i]], winslash = "/", mustWork = TRUE)
  relative <- if (startsWith(path, paste0(root, "/"))) substring(path, nchar(root) + 2L) else path
  data.frame(
    artifact_path = relative,
    artifact_role = manifest_roles[[i]],
    bytes = unname(file.info(path)$size),
    sha256 = pvr_sha256_file(path),
    build_id = build_id,
    schema_id = schema_id,
    estimator_id = estimator_id,
    admissibility_id = admissibility_id,
    selection_id = selection_id,
    source_package_ids = paste(base_source_id, reference_source_id, sep = ";"),
    lifecycle_status = "candidate",
    stringsAsFactors = FALSE
  )
}))
manifest_path <- file.path(governance_dir, "p15_source_layer_output_manifest.csv")
write.csv(manifest, manifest_path, row.names = FALSE)

cat("P15 source layer built.\n")
cat("History rows:", nrow(history), "\n")
cat("Supplement identifiers with price quotes:", sum(supplement_audit$rows_with_any_price_quote > 0L), "of 24\n")
cat("Supplement identifiers without history quotes:", sum(supplement_audit$rows_with_any_price_quote == 0L), "of 24\n")
cat("Excluded U.S. Treasury strips:", length(excluded_rics), "\n")
cat("Unresolved history records:", unresolved_universe, "\n")
cat("Output:", history_output, "\n")
