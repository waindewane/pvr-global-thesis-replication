source(testthat::test_path("..", "..", "R", "p15_replay.R"))

testthat::test_that("replay refuses paths outside its isolated workspace", {
  testthat::expect_error(p15_replay_safe_paths("../data.csv"), "isolated")
  testthat::expect_error(p15_replay_safe_paths("/tmp/data.csv"), "isolated")
  testthat::expect_identical(p15_replay_safe_paths(c("data/a.csv", "data/a.csv")), "data/a.csv")
})

testthat::test_that("replay comparison retains narratives values order and permission changes", {
  a <- tempfile(fileext = ".csv")
  b <- tempfile(fileext = ".csv")
  on.exit(unlink(c(a, b)))
  x <- data.frame(iso3 = c("AAA", "BBB"), note = c("Restructuring evidence", "Warning only"),
    allowed = c(FALSE, TRUE), rate = c(5, 7), pointer = "/oldroot/data/raw.csv")
  data.table::fwrite(x, a)
  y <- x
  y$pointer <- "/newroot/data/raw.csv"
  data.table::fwrite(y, b)
  testthat::expect_true(p15_replay_compare(a, b, "/oldroot", "/newroot")$semantic_match)
  y$note[1] <- "Distress interpretation changed"
  data.table::fwrite(y, b)
  testthat::expect_false(p15_replay_compare(a, b, "/oldroot", "/newroot")$semantic_match)
  y <- x
  y$allowed[1] <- TRUE
  data.table::fwrite(y, b)
  testthat::expect_false(p15_replay_compare(a, b, "/oldroot", "/newroot")$semantic_match)
  data.table::fwrite(x[2:1, ], b)
  testthat::expect_false(p15_replay_compare(a, b, "/oldroot", "/newroot")$semantic_match)
})
