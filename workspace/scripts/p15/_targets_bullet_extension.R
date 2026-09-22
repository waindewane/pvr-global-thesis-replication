library(targets)
tar_option_set(packages=character())
list(
  tar_target(bullet_destination, {
    x <- Sys.getenv("P15_BULLET_OUTPUT")
    if(!nzchar(x))stop("Set P15_BULLET_OUTPUT to a fresh directory")
    x
  },cue=tar_cue(mode="always")),
  tar_target(bullet_inputs, {
    p <- "data-derived/p15_pv_first_pass_20260909_v1"
    upstream <- read.csv(file.path(p,"input_manifest.csv"))
    c(upstream$path,file.path(p,c("country_year_creditor_inventory.csv","schedule_and_date_audit.csv",
      "input_manifest.csv","output_manifest.csv")),"R/pvr.R","R/p15_pv_first_pass.R",
      "R/p15_bullet_extension.R","scripts/p15/build_p15_bullet_extension.R",
      "tests/testthat/test-p15-bullet-extension.R",
      "docs/governance/P15_BULLET_EQUAL_TERMS_PRECEDENT_2026-09-09.md")
  },format="file"),
  tar_target(bullet_outputs, {
    stopifnot(all(file.exists(bullet_inputs)))
    status <- system2(file.path(R.home("bin"),"Rscript"),
      c("scripts/p15/build_p15_bullet_extension.R",shQuote(bullet_destination)))
    if(status!=0)stop("Bullet replay failed")
    list.files(bullet_destination,full.names=TRUE)
  },format="file")
)
