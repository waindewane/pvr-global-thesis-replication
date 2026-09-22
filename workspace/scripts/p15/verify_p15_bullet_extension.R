#!/usr/bin/env Rscript
suppressPackageStartupMessages(library(data.table))
p <- "data-derived/p15_bullet_extension_20260909_v1"
replay <- "/tmp/p15_bullet_replay_20260909"
files <- setdiff(list.files(p,pattern="\\.csv$"),"output_manifest.csv")
checks <- data.table(check=paste0("exact_replay_",files),passed=vapply(files,function(f)
  identical(readLines(file.path(p,f)),readLines(file.path(replay,f))),logical(1)))
for(name in c("input_manifest","code_manifest","output_manifest")){
  m <- fread(file.path(p,paste0(name,".csv")))
  checks <- rbind(checks,data.table(check=paste0(name,"_hashes_intact"),passed=
    all(m$sha256==vapply(m$path,function(f)digest::digest(file=f,algo="sha256"),character(1)))))
}
x <- fread(file.path(p,"inventory_with_bullet_flags.csv"))
before <- fread("data-derived/p15_pv_first_pass_20260909_v1/country_year_creditor_inventory.csv")
checks <- rbind(checks,data.table(check="all_original_inventory_fields_unchanged",passed=
  identical(x[,names(before),with=FALSE],before)))
z <- fread(file.path(p,"inconsistent_pairs_unchanged.csv"))
checks <- rbind(checks,data.table(check="both_inconsistent_pairs_still_excluded",passed=
  nrow(z)==2 && !any(z$ordinary_scenario_with_bullet) && all(z$repayment_profile=="unavailable")))
v <- fread(file.path(p,"bullet_all_tier_valuations.csv"))
checks <- rbind(checks,data.table(check="ineligible_rates_never_valued",passed=all(is.na(v$pv_per_100[!v$usable]))))
# Independent reconstruction directly from input terms, not the new module.
a <- fread(file.path(p,"bullet_case_inputs.csv"))
for(i in seq_len(nrow(a))){
  b <- a[i]; id <- paste(b$iso3,b$analysis_year,b$creditor,sep="_")
  rows <- v[case_id==id & usable==TRUE]
  times <- pmin(seq_len(ceiling(b$official_maturity_years)),b$official_maturity_years)
  cf <- b$official_rate*c(times[1],diff(times)); cf[length(cf)] <- tail(cf,1)+100
  pred <- vapply(rows$discount_rate_pct,function(r)sum(cf/(1+r/100)^times),numeric(1))
  checks <- rbind(checks,data.table(check=paste0("independent_case_PV_",id),passed=
    max(abs(pred-rows$pv_per_100))<1e-10))
}
stopifnot(all(checks$passed))
dest <- file.path(p,"validation")
dir.create(dest,showWarnings=FALSE)
fwrite(checks,file.path(dest,"replay_and_preservation_checks.csv"))
paths <- c(file.path(dest,"replay_and_preservation_checks.csv"),"scripts/p15/verify_p15_bullet_extension.R")
fwrite(data.table(path=paths,sha256=vapply(paths,function(f)digest::digest(file=f,algo="sha256"),character(1))),
  file.path(dest,"validation_manifest.csv"))
cat(nrow(checks),"checks passed\n")
