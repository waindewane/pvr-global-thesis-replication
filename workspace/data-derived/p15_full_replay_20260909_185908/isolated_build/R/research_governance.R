# Governance and manifest helpers for the candidate 2012-2024 research platform.
# These functions do not implement empirical methods. They protect destinations,
# validate version bundles, and make file-level provenance reproducible.

pvr_sha256_file <- function(path) {
  if (!file.exists(path)) stop("Cannot hash missing file: ", path, call. = FALSE)
  out <- system2(
    "shasum",
    c("-a", "256", shQuote(normalizePath(path, mustWork = TRUE))),
    stdout = TRUE,
    stderr = TRUE
  )
  if (!length(out)) stop("SHA-256 command returned no output for: ", path, call. = FALSE)
  sub("[[:space:]].*$", "", out[[1]])
}

pvr_csv_shape <- function(path) {
  if (!grepl("[.]csv$", path, ignore.case = TRUE)) {
    return(list(rows = NA_integer_, columns = NA_integer_))
  }
  header <- names(read.csv(path, nrows = 0L, check.names = FALSE))
  line_count <- system2("wc", c("-l", shQuote(path)), stdout = TRUE)
  line_count <- as.integer(strsplit(trimws(line_count[[1]]), "[[:space:]]+")[[1]][[1]])
  list(rows = max(0L, line_count - 1L), columns = length(header))
}

pvr_manifest_rows <- function(
    paths,
    artifact_role,
    build_id,
    schema_id,
    estimator_id,
    admissibility_id,
    selection_id,
    source_package_ids,
    root_dir = ".") {
  if (!length(paths)) stop("Manifest requires at least one path.", call. = FALSE)
  if (length(artifact_role) == 1L) artifact_role <- rep(artifact_role, length(paths))
  if (length(artifact_role) != length(paths)) {
    stop("artifact_role must have length one or match paths.", call. = FALSE)
  }
  required_ids <- c(
    build_id = build_id,
    schema_id = schema_id,
    estimator_id = estimator_id,
    admissibility_id = admissibility_id,
    selection_id = selection_id,
    source_package_ids = source_package_ids
  )
  if (any(is.na(required_ids) | !nzchar(trimws(required_ids)))) {
    stop("Manifest version and source-package IDs must be nonempty.", call. = FALSE)
  }

  root_dir <- normalizePath(root_dir, winslash = "/", mustWork = TRUE)
  rows <- lapply(seq_along(paths), function(i) {
    path <- paths[[i]]
    if (!file.exists(path)) stop("Manifest input is missing: ", path, call. = FALSE)
    full_path <- normalizePath(path, winslash = "/", mustWork = TRUE)
    relative_path <- if (startsWith(full_path, paste0(root_dir, "/"))) {
      substring(full_path, nchar(root_dir) + 2L)
    } else {
      full_path
    }
    shape <- pvr_csv_shape(full_path)
    data.frame(
      artifact_path = relative_path,
      artifact_role = artifact_role[[i]],
      bytes = unname(file.info(full_path)$size),
      rows = shape$rows,
      columns = shape$columns,
      sha256 = pvr_sha256_file(full_path),
      build_id = build_id,
      schema_id = schema_id,
      estimator_id = estimator_id,
      admissibility_id = admissibility_id,
      selection_id = selection_id,
      source_package_ids = source_package_ids,
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

pvr_validate_version_bundle <- function(bundle, registry) {
  required <- c(
    "estimator_id", "admissibility_id", "selection_id", "schema_id",
    "build_id", "source_package_ids"
  )
  missing_fields <- setdiff(required, names(bundle))
  if (length(missing_fields)) {
    stop("Version bundle is missing: ", paste(missing_fields, collapse = ", "), call. = FALSE)
  }
  empty_fields <- required[vapply(required, function(nm) {
    value <- bundle[[nm]]
    length(value) == 0L || all(is.na(value) | !nzchar(trimws(as.character(value))))
  }, logical(1))]
  if (length(empty_fields)) {
    stop("Version bundle has empty IDs: ", paste(empty_fields, collapse = ", "), call. = FALSE)
  }
  if (!all(c("version_id", "version_type", "lifecycle_status") %in% names(registry))) {
    stop("Version registry does not satisfy the governance contract.", call. = FALSE)
  }
  registry_ids <- registry$version_id
  single_ids <- unlist(bundle[c(
    "estimator_id", "admissibility_id", "selection_id", "schema_id", "build_id"
  )], use.names = FALSE)
  source_ids <- trimws(unlist(strsplit(bundle$source_package_ids, ";", fixed = TRUE)))
  unknown <- setdiff(c(single_ids, source_ids), registry_ids)
  if (length(unknown)) {
    stop("Unknown version/source IDs: ", paste(unknown, collapse = ", "), call. = FALSE)
  }
  invisible(TRUE)
}

pvr_is_protected_destination <- function(path, root_dir = ".") {
  root_dir <- normalizePath(root_dir, winslash = "/", mustWork = TRUE)
  absolute <- if (grepl("^/", path)) path else file.path(root_dir, path)
  absolute <- gsub("/+", "/", absolute)
  protected <- c(
    file.path(root_dir, "output", "tables"),
    file.path(root_dir, "releases", "frozen"),
    file.path(root_dir, "releases", "canonical")
  )
  any(vapply(protected, function(prefix) {
    identical(absolute, prefix) || startsWith(absolute, paste0(prefix, "/"))
  }, logical(1)))
}

pvr_validate_promotion_checklist <- function(checklist) {
  required <- c("criterion_id", "applicable", "status", "evidence", "approver")
  missing_fields <- setdiff(required, names(checklist))
  if (length(missing_fields)) {
    stop("Promotion checklist is missing: ", paste(missing_fields, collapse = ", "), call. = FALSE)
  }
  active <- checklist[checklist$applicable %in% c(TRUE, "TRUE", "yes", "YES"), , drop = FALSE]
  if (!nrow(active)) stop("Promotion checklist has no applicable criteria.", call. = FALSE)
  failed <- active$status != "passed" |
    is.na(active$evidence) | !nzchar(trimws(active$evidence))
  if (any(failed)) {
    stop(
      "Promotion checklist has unresolved criteria: ",
      paste(active$criterion_id[failed], collapse = ", "),
      call. = FALSE
    )
  }
  owner <- active[active$criterion_id == "OWNER-APPROVAL", , drop = FALSE]
  if (nrow(owner) != 1L || !nzchar(trimws(owner$approver[[1]]))) {
    stop("Promotion requires one recorded OWNER-APPROVAL with an approver.", call. = FALSE)
  }
  invisible(TRUE)
}

pvr_assert_write_allowed <- function(
    path,
    lifecycle_status,
    promotion_checklist = NULL,
    root_dir = ".") {
  lifecycle_status <- trimws(lifecycle_status)
  if (lifecycle_status == "frozen_legacy") {
    stop("Frozen legacy artifacts cannot be rewritten in place.", call. = FALSE)
  }
  if (!pvr_is_protected_destination(path, root_dir = root_dir)) return(invisible(TRUE))
  if (!lifecycle_status %in% c("canonical", "publication_ready")) {
    stop(
      "Only a promoted canonical/publication-ready build may write a protected destination.",
      call. = FALSE
    )
  }
  if (is.null(promotion_checklist)) {
    stop("Protected destination requires a promotion checklist.", call. = FALSE)
  }
  pvr_validate_promotion_checklist(promotion_checklist)
  invisible(TRUE)
}

