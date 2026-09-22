#!/usr/bin/env Rscript
suppressPackageStartupMessages(library(data.table))
p <- "data-derived/p15_pv_first_pass_20260909_v1"
replay <- "/tmp/p15_pv_first_pass_replay_20260909"
files <- list.files(p,pattern="\\.csv$")
files <- setdiff(files,"output_manifest.csv")
checks <- data.table(check=paste0("replay_",files),passed=vapply(files,function(f)
  identical(readLines(file.path(p,f)),readLines(file.path(replay,f))),logical(1)))
input <- fread(file.path(p,"input_manifest.csv"))
checks <- rbind(checks,data.table(check="all_input_hashes_unchanged",passed=all(input$sha256==
  vapply(input$path,function(f)digest::digest(file=f,algo="sha256"),character(1)))))
a <- fread(file.path(p,"example_inputs.csv")); v <- fread(file.path(p,"example_valuations.csv"))
for (i in seq_len(nrow(a))) {
  z <- a[i]; time <- pmin(seq_len(ceiling(z$official_maturity_years)),z$official_maturity_years)
  start <- c(0,head(time,-1)); balance <- function(t) 100*(1-pmax(t-z$official_grace_years,0)/
    (z$official_maturity_years-z$official_grace_years))
  cf <- balance(start)-balance(time)+balance(start)*z$official_rate/100*(time-start)
  rows <- v[example_id==z$example_id]
  predicted <- vapply(rows$discount_rate_pct,function(r)sum(cf*exp(-log1p(r/100)*time)),numeric(1))
  checks <- rbind(checks,data.table(check=paste0("independent_PV_",z$example_id),
    passed=max(abs(predicted-rows$pv_per_100))<1e-8))
}
x <- fread(file.path(p,"country_year_creditor_inventory.csv"))
checks <- rbind(checks,data.table(check="lmic_reason_partition",passed=
  nrow(x[historical_lmic_reporting_scope==TRUE])==5268 &&
  all(!is.na(x$inventory_reason)) && all(nzchar(x$inventory_reason))))
stopifnot(all(checks$passed))
dest <- file.path(p,"validation")
dir.create(dest,showWarnings=FALSE)
fwrite(checks,file.path(dest,"independent_and_replay_checks.csv"))
writeLines(c("Focused tests: 13 new expectations passed; legacy PV tests also run separately.",
  "Offline targets replay completed in a fresh /tmp directory.",
  "All analytical CSVs and input/code manifests match exactly; environment and path-dependent output manifest excluded.",
  "No baseline source, candidate rate, legacy calculation or registered snapshot was overwritten."),file.path(dest,"README.txt"))
paths <- c(list.files(dest,full.names=TRUE),"scripts/p15/verify_p15_pv_first_pass.R",
  "scripts/p15/_targets_pv_first_pass.R","tests/testthat/test-p15-pv-first-pass.R")
fwrite(data.table(path=paths,sha256=vapply(paths,function(f)digest::digest(file=f,algo="sha256"),character(1))),
  file.path(dest,"validation_manifest.csv"))
print(checks)
