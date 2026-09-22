source(testthat::test_path("..","..","R","p15_local_completion.R"))
source(testthat::test_path("..","..","R","p15_case_review.R"))
source(testthat::test_path("..","..","R","p15_raw_foundations.R"))

testthat::test_that("case review preserves raw rates and separates term from rate decisions", {
  root <- testthat::test_path("..","..")
  read <- function(p)tibble::as_tibble(data.table::fread(file.path(root,p)))
  p <- read("data-derived/p15_local_completion_2012_2024_20260906_v1/p15_country_year_dataset.csv")
  c <- read("data-raw/p15_case_review_20260906/ids_case_dispositions.csv")
  d <- read("data-raw/p15_case_review_20260906/ids_decision_definitions.csv")
  x <- p15_apply_ids_case_review(p,c,d)
  testthat::expect_identical(x$panel$ids_rate_pct,p$ids_rate_pct)
  testthat::expect_equal(nrow(x$cases),30L)
  k <- x$panel$iso3=="KAZ" & x$panel$analysis_year==2013
  testthat::expect_true(x$panel$ids_benchmark_proxy_candidate_permitted[k])
  testthat::expect_equal(x$panel$ids_reviewed_term_state[k],"blocked_inconsistent_terms")
  e <- x$panel$iso3=="EGY" & x$panel$analysis_year==2022
  testthat::expect_equal(x$panel$ids_rate_pct[e],.85)
  testthat::expect_false(x$panel$ids_benchmark_proxy_candidate_permitted[e])
  testthat::expect_error(p15_apply_ids_case_review(p,c[-1,],d),"lacks a disposition")
  testthat::expect_error(p15_apply_ids_case_review(p,dplyr::bind_rows(c,c[1,]),d),"duplicate")
  wrong <- c; wrong$expected_rate_pct[1] <- 5.75
  testthat::expect_error(p15_apply_ids_case_review(p,wrong,d),"no longer matches")
  wrong <- c; wrong$decision_code[1] <- "unknown"
  testthat::expect_error(p15_apply_ids_case_review(p,wrong,d),"Unknown")
  d$explanation[1] <- ""
  testthat::expect_error(p15_apply_ids_case_review(p,c,d),"rationale")
})

testthat::test_that("legacy expression reader does not execute unrequested top level code", {
  p <- tempfile(fileext=".R"); on.exit(unlink(p))
  writeLines(c("stop('must never execute')","f <- function(x) x + 1","value <- 7"),p)
  e <- new.env()
  p15_legacy_assignments(p,"f",e,functions_only=TRUE)
  testthat::expect_equal(e$f(2),3)
  testthat::expect_error(p15_legacy_assignments(p,"value",e,functions_only=TRUE),"Not a function")
  testthat::expect_error(p15_legacy_assignments(p,"missing",e),"Ambiguous")
})

testthat::test_that("replay allows machine precision but not changed decisions or rates", {
  source(testthat::test_path("..","..","R","p15_replay.R"))
  a <- tempfile(); b <- tempfile(); on.exit(unlink(c(a,b)))
  x <- data.frame(rate=5+1e-14,permission=TRUE,note="source review")
  data.table::fwrite(x,a);x$rate <- 5;data.table::fwrite(x,b)
  testthat::expect_true(p15_replay_compare(a,b,"/a","/b",1e-12)$semantic_match)
  x$rate <- 5.000001;data.table::fwrite(x,b)
  testthat::expect_false(p15_replay_compare(a,b,"/a","/b",1e-12)$semantic_match)
  x$rate <- 5;x$permission <- FALSE;data.table::fwrite(x,b)
  testthat::expect_false(p15_replay_compare(a,b,"/a","/b",1e-12)$semantic_match)
})
