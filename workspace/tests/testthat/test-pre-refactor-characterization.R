characterization_root <- normalizePath(file.path(testthat::test_path(), "..", ".."), winslash = "/", mustWork = TRUE)
characterization_fixture_dir <- file.path(characterization_root, "tests", "fixtures", "characterization")

characterization_sha256_file <- function(path) {
  out <- system2("shasum", c("-a", "256", shQuote(path)), stdout = TRUE, stderr = TRUE)
  testthat::expect_true(length(out) > 0L, info = path)
  sub("[[:space:]].*$", "", out[[1]])
}

characterization_sha256_text <- function(text) {
  tmp <- tempfile("characterization-")
  on.exit(unlink(tmp), add = TRUE)
  writeLines(text, tmp, useBytes = TRUE)
  characterization_sha256_file(tmp)
}

characterization_csv_shape <- function(path) {
  header <- names(read.csv(path, nrows = 0L, check.names = FALSE))
  wc <- system2("wc", c("-l", shQuote(path)), stdout = TRUE)
  lines <- as.numeric(strsplit(trimws(wc[[1]]), "[[:space:]]+")[[1]][[1]])
  list(
    rows = max(0, lines - 1),
    columns = length(header),
    schema_sha256 = characterization_sha256_text(header)
  )
}

characterization_row_hash <- function(row) {
  values <- vapply(row, function(x) {
    if (length(x) == 0L || is.na(x)) return("<NA>")
    enc2utf8(as.character(x))
  }, character(1))
  characterization_sha256_text(paste(names(row), values, sep = "=", collapse = "\n"))
}

testthat::test_that("pre-refactor P8/P13/P14/combined builders remain frozen", {
  fixture <- read.csv(file.path(characterization_fixture_dir, "builder_fixtures_2026-07-18.csv"), check.names = FALSE)
  for (i in seq_len(nrow(fixture))) {
    path <- file.path(characterization_root, fixture$path[[i]])
    testthat::expect_true(file.exists(path), info = fixture$path[[i]])
    testthat::expect_equal(unname(file.info(path)$size), fixture$size_bytes[[i]], info = fixture$path[[i]])
    testthat::expect_identical(characterization_sha256_file(path), fixture$sha256[[i]], info = fixture$path[[i]])
  }
})

testthat::test_that("pre-refactor output bytes rows schemas and columns remain characterized", {
  fixture <- read.csv(file.path(characterization_fixture_dir, "output_fixtures_2026-07-18.csv"), check.names = FALSE)
  for (i in seq_len(nrow(fixture))) {
    path <- file.path(characterization_root, fixture$path[[i]])
    testthat::expect_true(file.exists(path), info = fixture$path[[i]])
    shape <- characterization_csv_shape(path)
    testthat::expect_equal(unname(file.info(path)$size), fixture$size_bytes[[i]], info = fixture$path[[i]])
    testthat::expect_identical(characterization_sha256_file(path), fixture$sha256[[i]], info = fixture$path[[i]])
    testthat::expect_equal(shape$rows, fixture$row_count[[i]], info = fixture$path[[i]])
    testthat::expect_equal(shape$columns, fixture$column_count[[i]], info = fixture$path[[i]])
    testthat::expect_identical(shape$schema_sha256, fixture$schema_sha256[[i]], info = fixture$path[[i]])
  }
})

testthat::test_that("selected structural anchor rows remain unique and unchanged", {
  fixture <- read.csv(file.path(characterization_fixture_dir, "anchor_fixtures_2026-07-18.csv"), check.names = FALSE)
  cached <- new.env(parent = emptyenv())
  for (i in seq_len(nrow(fixture))) {
    key <- fixture$path[[i]]
    if (!exists(key, envir = cached, inherits = FALSE)) {
      assign(key, read.csv(file.path(characterization_root, key), check.names = FALSE), envir = cached)
    }
    x <- get(key, envir = cached, inherits = FALSE)
    hit <- x$analysis_year == fixture$analysis_year[[i]] & x$iso3 == fixture$iso3[[i]]
    testthat::expect_equal(sum(hit, na.rm = TRUE), 1L, info = paste(key, fixture$analysis_year[[i]], fixture$iso3[[i]]))
    row <- x[which(hit)[[1]], , drop = FALSE]
    testthat::expect_identical(characterization_row_hash(row), fixture$row_sha256[[i]], info = paste(key, fixture$analysis_year[[i]], fixture$iso3[[i]]))
  }
})
