# One offline master entry point. Stage fingerprints provide incremental reuse.
library(targets)
source("scripts/p15/activate_p15_environment.R")
source("R/p15_master.R")
tar_option_set(packages=c("data.table","digest","jsonlite"))
list(tar_target(p15_current_research,
  p15_master_run(),format="file",cue=tar_cue(mode="always")))
