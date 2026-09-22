library(targets)
tar_option_set(packages=character())
list(
  tar_target(pv_interpretation_destination,{
    x <- Sys.getenv("P15_INTERPRETATION_OUTPUT")
    if(!nzchar(x))stop("Set P15_INTERPRETATION_OUTPUT to a fresh directory")
    x
  },cue=tar_cue(mode="always")),
  tar_target(pv_interpretation_inputs,{
    c("data-derived/p15_bullet_extension_20260909_v1/paired_pv_for_inclusion_check.csv",
      "data-derived/p15_bullet_extension_20260909_v1/inventory_with_bullet_flags.csv",
      "data-derived/p15_bullet_extension_20260909_v1/output_manifest.csv",
      "data-derived/p15_regional_assessment_20260908_v2/country_region_map.csv",
      "data-derived/p15_analysis_candidate_20260907_v1/tier_eligibility.csv",
      "R/pvr.R","R/p15_pv_first_pass.R","R/p15_bullet_extension.R","R/p15_pv_interpretation.R",
      "scripts/p15/build_p15_pv_interpretation.R","tests/testthat/test-p15-pv-interpretation.R")
  },format="file"),
  tar_target(pv_interpretation_outputs,{
    stopifnot(all(file.exists(pv_interpretation_inputs)))
    s <- system2(file.path(R.home("bin"),"Rscript"),
      c("scripts/p15/build_p15_pv_interpretation.R",shQuote(pv_interpretation_destination)))
    if(s!=0)stop("Interpretation build failed")
    list.files(pv_interpretation_destination,full.names=TRUE)
  },format="file")
)
