# Isolated replay utilities. Copies inputs; never links writable project directories.
p15_replay_safe_paths <- function(paths) {
  if (any(is.na(paths) | !nzchar(paths) | grepl("^/|(^|/)\\.\\.(/|$)", paths))) {
    stop("Replay paths must stay within the isolated project root.", call. = FALSE)
  }
  unique(paths)
}

p15_replay_compare <- function(reference, rebuilt, reference_root, rebuilt_root, numeric_tolerance = 0) {
  if (!file.exists(rebuilt)) stop("Replay did not create: ", rebuilt, call. = FALSE)
  lhs <- data.table::fread(reference)
  rhs <- data.table::fread(rebuilt)
  normalize <- function(x, root) {
    for (nm in names(x)) if (is.character(x[[nm]])) {
      data.table::set(x, j = nm, value = gsub(paste0(root, "/"), "<PROJECT>/", x[[nm]], fixed = TRUE))
    }
    x
  }
  # Preserved comparison fixtures may themselves contain the original root.
  # Both roots denote the same archived project-relative evidence path.
  lhs <- normalize(normalize(lhs, reference_root), rebuilt_root)
  rhs <- normalize(normalize(rhs, rebuilt_root), reference_root)
  exact <- isTRUE(all.equal(lhs, rhs, tolerance = 0, check.attributes = TRUE))
  same <- exact
  max_numeric_difference <- 0
  if (identical(names(lhs), names(rhs)) && nrow(lhs) == nrow(rhs)) {
    equal_columns <- vapply(names(lhs), function(nm) {
      a <- lhs[[nm]]; b <- rhs[[nm]]
      if (is.numeric(a) && is.numeric(b)) {
        if (!identical(is.na(a), is.na(b)) || !identical(is.infinite(a), is.infinite(b))) return(FALSE)
        finite <- is.finite(a) & is.finite(b)
        delta <- if (any(finite)) max(abs(a[finite] - b[finite])) else 0
        max_numeric_difference <<- max(max_numeric_difference, delta)
        return(delta <= numeric_tolerance && all(a[is.infinite(a)] == b[is.infinite(b)]))
      }
      identical(a, b)
    }, logical(1))
    same <- all(equal_columns)
  }
  data.frame(rows = nrow(rhs), columns = ncol(rhs), semantic_match = same,
    exact_values_match = exact, max_numeric_difference = max_numeric_difference,
    byte_match = identical(digest::digest(reference, algo = "sha256", file = TRUE),
                           digest::digest(rebuilt, algo = "sha256", file = TRUE)),
    comparison_rule = paste0("exact_keys_text_missingness_order; absolute_numeric_tolerance=", numeric_tolerance,
                             "; normalize_absolute_project_prefix_only"))
}

p15_replay_copy <- function(paths, from, to) {
  paths <- p15_replay_safe_paths(paths)
  for (path in paths) {
    dest <- file.path(to, path)
    dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
    if (!file.copy(file.path(from, path), dest, overwrite = FALSE)) {
      stop("Cannot stage replay input: ", path, call. = FALSE)
    }
  }
  invisible(paths)
}
