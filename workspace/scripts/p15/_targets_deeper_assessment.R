library(targets)
list(
  tar_target(deeper_destination,{
    x<-Sys.getenv("P15_DEEPER_ASSESSMENT_OUTPUT")
    if(!nzchar(x))stop("Set a new P15_DEEPER_ASSESSMENT_OUTPUT directory")
    x
  },cue=tar_cue(mode="always")),
  tar_target(deeper_inputs,{
    x<-read.csv("data-derived/p15_deeper_assessment_20260908_v4/input_manifest.csv")
    unique(c(x$artifact_path,"R/p15_deeper_assessment.R","R/p15_dataset_assessment.R",
      "R/research_governance.R","scripts/p15/build_p15_deeper_assessment.R"))
  },format="file"),
  tar_target(deeper_outputs,{
    stopifnot(all(file.exists(deeper_inputs)))
    status<-system2(file.path(R.home("bin"),"Rscript"),
      c("scripts/p15/build_p15_deeper_assessment.R",shQuote(deeper_destination)))
    if(status!=0)stop("Deeper assessment failed")
    list.files(deeper_destination,full.names=TRUE)
  },format="file")
)
