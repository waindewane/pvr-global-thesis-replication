library(targets)
tar_option_set(packages=character())
list(
  tar_target(annotation_inputs,c(
    "data-derived/p15_pv_interpretation_20260909_v1/analysis_rows.csv",
    "data-derived/p15_pv_interpretation_20260909_v1/available_tier_same_terms_valuations.csv",
    "data-derived/p15_pv_interpretation_20260909_v1/matched_creditor_pairs.csv",
    "data-derived/p15_pv_interpretation_20260909_v1/output_manifest.csv",
    "R/pvr.R","R/p15_pv_first_pass.R","R/p15_bullet_extension.R","R/p15_pv_interpretation.R",
    "R/p15_pv_annotation_followup.R","scripts/p15/build_p15_pv_annotation_followup.R",
    "tests/testthat/test-p15-pv-annotation-followup.R"),format="file"),
  tar_target(annotation_output,{
    stopifnot(all(file.exists(annotation_inputs)))
    out<-Sys.getenv("P15_ANNOTATION_OUTPUT")
    if(!nzchar(out))stop("Set P15_ANNOTATION_OUTPUT to a fresh directory")
    status<-system2(file.path(R.home("bin"),"Rscript"),
      c("scripts/p15/build_p15_pv_annotation_followup.R",shQuote(out)))
    if(status!=0)stop("Build failed")
    list.files(out,full.names=TRUE)
  },format="file",cue=tar_cue(mode="always"))
)
