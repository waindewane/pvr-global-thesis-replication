source(file.path(if (dir.exists("R")) "." else "../..", "R/p15_thesis_benchmark_inference.R"))

testthat::test_that("country bootstrap preserves unequal cluster weights and paired sign", {
  x <- c(1, 3, -2, -2, -2)
  country <- c("A", "A", "B", "B", "B")
  a <- p15_thesis_cluster_boot(x, country, repetitions = 999, seed = 10)
  b <- p15_thesis_cluster_boot(rep(x, each = 2), rep(country, each = 2),
                              repetitions = 999, seed = 10)
  testthat::expect_equal(a, b)
  testthat::expect_equal(a$estimate_pp, mean(x))
  testthat::expect_equal(a$ci_low_pp, -2)
  testthat::expect_equal(a$ci_high_pp, 2)
  reversed <- p15_thesis_cluster_boot(-x, country, repetitions = 999, seed = 10)
  testthat::expect_equal(reversed$p_boot_two_sided, a$p_boot_two_sided)
  testthat::expect_equal(reversed$estimate_pp, -a$estimate_pp)
  zero <- p15_thesis_cluster_boot(rep(0, 4), c("A", "A", "B", "B"), repetitions = 99)
  testthat::expect_equal(zero$p_boot_two_sided, 1)
})

testthat::test_that("pair samples require all three observed rates", {
  d <- data.table::data.table(iso3=c("A","B","C"), country=letters[1:3],
    analysis_year=2020, region="r", primary=c(5,NA,5), ids=c(4,4,NA), secondary=c(2,2,2))
  z <- p15_thesis_loss_rows(d, "ids", "secondary", "f", "v")
  testthat::expect_equal(z$iso3, "A")
  testthat::expect_equal(z$improvement_pp, 2)
})

testthat::test_that("consecutive joins exclude gaps and distinguish endpoint and change accuracy", {
  d <- data.table::data.table(iso3="A", country="a", analysis_year=c(2018,2019,2021),
    region="r", primary=c(5,5,5), ids=c(4,6,4), dac=c(8,8,8))
  z <- p15_thesis_loss_rows(d, "ids", "dac", "f", "v")
  z[, focal_source := c("ids","secondary","ids")]
  changes <- p15_thesis_consecutive(z)
  testthat::expect_equal(changes$analysis_year, 2019)
  testthat::expect_true(changes$both_endpoint_levels_closer)
  testthat::expect_true(changes$change_worse)
  testthat::expect_true(changes$source_switch)
  testthat::expect_equal(changes$change_improvement_pp, -2)
})

testthat::test_that("fallback withholds primary and records actual chosen lower tier", {
  d <- data.table::data.table(primary=c(1,1), ids=c(NA,3), secondary=c(2,4), peer=c(5,5))
  x <- p15_thesis_add_fallback(d, "fallback", c("ids","secondary","peer"))
  testthat::expect_equal(x$fallback, c(2,3))
  testthat::expect_equal(x$fallback_source, c("secondary","ids"))
})
