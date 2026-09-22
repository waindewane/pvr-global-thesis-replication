library(targets)
tar_option_set(packages=character())
list(
  tar_target(concept_illustration_inputs,
    c("scripts/p15/build_concept_illustration_20260912.R",
      "scripts/p15/_targets_concept_illustration.R", "scripts/p15/activate_p15_environment.R",
      "R/pvr.R", "R/p15_pv_first_pass.R", "R/p15_bullet_extension.R",
      "R/research_governance.R", "renv.lock"), format="file"),
  tar_target(concept_illustration_outputs, {
    stopifnot(all(file.exists(concept_illustration_inputs)))
    out <- Sys.getenv("P15_CONCEPT_ILLUSTRATION_OUTPUT", "")
    if (!nzchar(out)) stop("Set P15_CONCEPT_ILLUSTRATION_OUTPUT to a fresh directory")
    status <- system2(file.path(R.home("bin"),"Rscript"),
      c("scripts/p15/build_concept_illustration_20260912.R",shQuote(out)))
    if (status != 0) stop("Illustration build failed")
    list.files(out,full.names=TRUE)
  },format="file",cue=tar_cue(mode="always"))
)
