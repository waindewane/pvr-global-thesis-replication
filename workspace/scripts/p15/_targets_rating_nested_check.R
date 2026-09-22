library(targets)
tar_option_set(packages = character())
list(
  tar_target(rating_nested_inputs, {
    candidate <- jsonlite::fromJSON("data-derived/p15_master/current_run.json")$candidate
    c("data-derived/p15_master/current_run.json", file.path(candidate, "core_evidence.csv"),
      file.path(candidate, "selected_reference.csv"),
      file.path(candidate, "input_manifest.csv"), file.path(candidate, "output_manifest.csv"),
      "docs/governance/P15_RATING_NESTED_CHECK_PROTOCOL_2026-09-12.md",
      "R/p15_rating_nested_check.R", "R/p15_rating_country_history_calibration.R",
      "scripts/p15/build_rating_nested_check.R", "scripts/p15/activate_p15_environment.R",
      "tests/testthat/test-p15-rating-nested-check.R",
      "R/research_governance.R", "R/p15_current_inputs.R", "renv.lock")
  }, format = "file"),
  tar_target(rating_nested_outputs, {
    stopifnot(all(file.exists(rating_nested_inputs)))
    out <- Sys.getenv("P15_RATING_NESTED_OUTPUT")
    if (!nzchar(out)) stop("Set P15_RATING_NESTED_OUTPUT to a fresh directory")
    status <- system2(file.path(R.home("bin"), "Rscript"),
      c("scripts/p15/build_rating_nested_check.R", shQuote(out)))
    if (status != 0) stop("Nested check failed")
    list.files(out, full.names = TRUE)
  }, format = "file", cue = tar_cue(mode = "always"))
)
