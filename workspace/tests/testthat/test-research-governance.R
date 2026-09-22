source("../../R/research_governance.R")

testthat::test_that("candidate and provisional builds cannot write protected outputs", {
  testthat::expect_error(
    pvr_assert_write_allowed("output/tables/candidate.csv", "candidate", root_dir = "../.."),
    "Only a promoted"
  )
  testthat::expect_error(
    pvr_assert_write_allowed("output/tables/provisional.csv", "provisional", root_dir = "../.."),
    "Only a promoted"
  )
  testthat::expect_true(
    pvr_assert_write_allowed("staging/build-v1/candidate.csv", "candidate", root_dir = "../..")
  )
})

testthat::test_that("frozen legacy artifacts cannot be rewritten anywhere", {
  testthat::expect_error(
    pvr_assert_write_allowed("staging/p8.csv", "frozen_legacy", root_dir = "../.."),
    "cannot be rewritten"
  )
})

testthat::test_that("promotion checklist fails closed", {
  incomplete <- data.frame(
    criterion_id = c("METHOD", "OWNER-APPROVAL"),
    applicable = TRUE,
    status = c("passed", "pending"),
    evidence = c("decision-id", ""),
    approver = c("", ""),
    stringsAsFactors = FALSE
  )
  testthat::expect_error(pvr_validate_promotion_checklist(incomplete), "unresolved")

  complete <- incomplete
  complete$status <- "passed"
  complete$evidence <- c("decision-id", "dated approval record")
  complete$approver <- c("", "project owner")
  testthat::expect_true(pvr_validate_promotion_checklist(complete))
  testthat::expect_true(pvr_assert_write_allowed(
    "output/tables/promoted.csv",
    "canonical",
    promotion_checklist = complete,
    root_dir = "../.."
  ))
})

testthat::test_that("version bundles reject missing and unknown IDs", {
  registry <- data.frame(
    version_id = c("EST-1", "ADM-1", "SEL-1", "SCH-1", "BLD-1", "SRC-1"),
    version_type = c("estimator", "admissibility", "selection", "schema", "build", "source_package"),
    lifecycle_status = rep("candidate", 6),
    stringsAsFactors = FALSE
  )
  bundle <- list(
    estimator_id = "EST-1",
    admissibility_id = "ADM-1",
    selection_id = "SEL-1",
    schema_id = "SCH-1",
    build_id = "BLD-1",
    source_package_ids = "SRC-1"
  )
  testthat::expect_true(pvr_validate_version_bundle(bundle, registry))
  bundle$selection_id <- "SEL-UNKNOWN"
  testthat::expect_error(pvr_validate_version_bundle(bundle, registry), "Unknown")
})

testthat::test_that("manifest rows include hashes shapes and version IDs", {
  temp_csv <- tempfile(fileext = ".csv")
  on.exit(unlink(temp_csv), add = TRUE)
  write.csv(data.frame(a = 1:2, b = c("x", "y")), temp_csv, row.names = FALSE)
  manifest <- pvr_manifest_rows(
    temp_csv,
    artifact_role = "test_output",
    build_id = "BLD-1",
    schema_id = "SCH-1",
    estimator_id = "EST-1",
    admissibility_id = "ADM-1",
    selection_id = "SEL-1",
    source_package_ids = "SRC-1",
    root_dir = tempdir()
  )
  testthat::expect_equal(manifest$rows, 2)
  testthat::expect_equal(manifest$columns, 2)
  testthat::expect_match(manifest$sha256, "^[0-9a-f]{64}$")
  testthat::expect_equal(manifest$selection_id, "SEL-1")
})
