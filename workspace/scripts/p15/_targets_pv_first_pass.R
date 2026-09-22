library(targets)
tar_option_set(packages=character())
list(
  tar_target(pv_destination, {
    x <- Sys.getenv("P15_PV_OUTPUT")
    if (!nzchar(x)) stop("Set P15_PV_OUTPUT to a fresh diagnostic directory")
    x
  }, cue=tar_cue(mode="always")),
  tar_target(pv_inputs, {
    x <- read.csv("data-derived/p15_pv_first_pass_20260909_v1/input_manifest.csv")
    c(x$path,"scripts/p15/build_p15_pv_first_pass.R","R/p15_pv_first_pass.R",
      "R/pvr.R","R/ids.R","R/p15_raw_foundations.R")
  }, format="file"),
  tar_target(pv_outputs, {
    stopifnot(all(file.exists(pv_inputs)))
    status <- system2(file.path(R.home("bin"),"Rscript"),
      c("scripts/p15/build_p15_pv_first_pass.R",shQuote(pv_destination)))
    if (status != 0) stop("First-pass PV replay failed")
    list.files(pv_destination,full.names=TRUE)
  }, format="file")
)
