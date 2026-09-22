# Project-only locked additions; child R processes inherit the same library.
p15_lib <- file.path(getwd(), "renv/library/p15-locked")
if (dir.exists(p15_lib)) {
  .libPaths(c(p15_lib, .libPaths()))
  Sys.setenv(R_LIBS_USER = p15_lib)
}
rm(p15_lib)
