#!/usr/bin/env Rscript
# Restore exact missing locked packages to a project-only library. Existing global
# packages are never changed. Cached installation archives support later restores.
lock <- jsonlite::fromJSON("renv.lock", simplifyVector = FALSE)
lib <- file.path(getwd(), "renv/library/p15-locked")
cache <- file.path(getwd(), "renv/cache/p15-package-archives")
dir.create(lib, recursive = TRUE, showWarnings = FALSE)
dir.create(cache, recursive = TRUE, showWarnings = FALSE)
.libPaths(c(lib, .libPaths()))
equivalent_version <- function(a, b) isTRUE(package_version(a) == package_version(b))
for (p in c("renv", "base64url", "igraph", "secretbase", "targets")) {
  v <- lock$Packages[[p]]$Version
  if (requireNamespace(p, quietly = TRUE) && equivalent_version(as.character(packageVersion(p)), v)) next
  suffix <- if (Sys.info()[["sysname"]] == "Darwin" && Sys.info()[["machine"]] == "arm64" && startsWith(as.character(getRversion()),"4.3")) ".tgz" else ".tar.gz"
  archive <- file.path(cache, paste0(p, "_", v, suffix))
  url <- if (suffix == ".tgz") paste0("https://cran.r-project.org/bin/macosx/big-sur-arm64/contrib/4.3/", basename(archive)) else
    paste0("https://cran.r-project.org/src/contrib/Archive/", p, "/", basename(archive))
  if (!file.exists(archive)) download.file(url, archive, mode = "wb", quiet = TRUE)
  install.packages(archive, repos = NULL, lib = lib,
    type = if (suffix == ".tgz") "mac.binary" else "source")
}
make_audit <- function() do.call(rbind, lapply(names(lock$Packages), function(p) {
  installed <- if (requireNamespace(p, quietly = TRUE)) as.character(packageVersion(p)) else NA_character_
  data.frame(package = p, locked_version = lock$Packages[[p]]$Version,
    installed_version = installed, matches_lock = !is.na(installed) && equivalent_version(installed, lock$Packages[[p]]$Version))
}))
audit <- make_audit()
if(any(!audit$matches_lock)) {
  # Let renv resolve/install the full dependency graph on another workstation.
  # The five prebuilt additions above avoid compilation on this Mac only.
  renv::restore(project=getwd(),lockfile="renv.lock",library=lib,prompt=FALSE)
  audit <- make_audit()
}
print(audit[!audit$matches_lock, ])
stopifnot(all(audit$matches_lock), equivalent_version(as.character(getRversion()), lock$R$Version))
cat("All", nrow(audit), "locked packages and R match. Library:", lib, "\n")
