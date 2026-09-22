library(targets)
tar_option_set(packages=character())
list(
  tar_target(assessment_destination, {
    x<-Sys.getenv("P15_ASSESSMENT_OUTPUT")
    if(!nzchar(x))stop("Set P15_ASSESSMENT_OUTPUT to a new diagnostic output directory")
    x
  },cue=tar_cue(mode="always")),
  tar_target(assessment_inputs, {
    current<-read.csv("data-derived/p15_dataset_assessment_20260908_v4/input_manifest.csv")
    unique(c(current$artifact_path,"R/p15_dataset_assessment.R",
      "R/p15_analysis_dataset.R","R/p15_bounded_fallback_comparison.R",
      "R/research_governance.R","scripts/p15/build_p15_dataset_assessment.R"))
  },format="file"),
  tar_target(assessment_outputs, {
    stopifnot(all(file.exists(assessment_inputs)))
    status<-system2(file.path(R.home("bin"),"Rscript"),
      c("scripts/p15/build_p15_dataset_assessment.R",shQuote(assessment_destination)))
    if(status!=0)stop("Dataset assessment failed")
    list.files(assessment_destination,full.names=TRUE)
  },format="file")
)
