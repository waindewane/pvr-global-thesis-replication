source(file.path("..","..","R","p15_current_inputs.R"))
source(file.path("..","..","R","p15_master_findings.R"))

testthat::test_that("active input routing follows the current pointer and fails closed", {
  root <- tempfile(); dir.create(root); on.exit(unlink(root,recursive=TRUE))
  candidate <- file.path(root,"candidate"); dir.create(candidate)
  stage <- file.path(root,"regional"); dir.create(stage)
  pointer <- file.path(root,"current.json")
  jsonlite::write_json(list(candidate=candidate,stages=list(regional=list(dir=stage))),
    pointer,auto_unbox=TRUE)
  testthat::expect_identical(p15_current_input("dataset",pointer),candidate)
  testthat::expect_identical(p15_current_input("regional",pointer),stage)
  testthat::expect_error(p15_current_input("missing",pointer),"unavailable")
})

testthat::test_that("review acknowledges both the results and exact note text", {
  note <- tempfile(); on.exit(unlink(note)); writeLines("Reviewed finding",note)
  registry <- data.table::data.table(analysis="dataset",note=note,status="live")
  results <- data.table::data.table(analysis="dataset",result_fingerprint="data-v1")
  review <- data.table::data.table(analysis="dataset",note=note,
    reviewed_fingerprint="data-v1",reviewed_note_sha256=digest::digest(file=note,algo="sha256"))
  testthat::expect_false(p15_master_note_alerts(registry,review,results)$review_required)
  writeLines("Unreviewed changed claim",note)
  testthat::expect_true(p15_master_note_alerts(registry,review,results)$review_required)
  registry$status <- "drafted_prose_excluded"
  testthat::expect_false(p15_master_note_alerts(registry,review,results)$review_required)
})
