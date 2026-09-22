#!/usr/bin/env Rscript
# Run from the project root. Never downloads or promotes a release.
source("scripts/p15/activate_p15_environment.R")
targets::tar_make(script="_targets.R",store="data-derived/p15_master_targets")
