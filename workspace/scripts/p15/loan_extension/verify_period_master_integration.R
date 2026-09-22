#!/usr/bin/env Rscript
# Independent integration checks: raw reuse, numerical preservation and current parent routing.
source("scripts/p15/activate_p15_environment.R")
source("R/p15_master.R")
source("R/p15_master_findings.R")
args <- commandArgs(TRUE)
if (length(args) < 2L) stop("Supply fresh verification CSV and the preceding 15-stage run.json")
if (file.exists(args[1])) stop("Verification output already exists")
config <- p15_master_config()
now <- jsonlite::fromJSON(file.path(config$cache_root, "current_run.json"), simplifyVector=FALSE)
old <- jsonlite::fromJSON(args[2], simplifyVector=FALSE)
checks <- data.table::data.table(check=character(), passed=logical())
check <- function(name, value) {
  if (!isTRUE(value)) stop("Period integration verification failed: ", name)
  checks <<- rbind(checks, data.table::data.table(check=name, passed=TRUE))
}
check("sixteen_registered_stages", length(now$stages)==16L)
check("previous_run_has_fifteen_stages", length(old$stages)==15L)
check("raw_cache_key_and_candidate_reused", identical(now$raw_key, old$raw_key) &&
  identical(now$candidate, old$candidate))
prior_ids <- setdiff(names(old$stages), "loan_comparisons")
prior_tables <- data.table::rbindlist(c(list(p15_master_table_changes("dataset", now$candidate,
  old$candidate)), lapply(prior_ids, function(id) p15_master_table_changes(id,
    now$stages[[id]]$dir, old$stages[[id]]$dir))))
check("all_286_earlier_tables_numerically_preserved", nrow(prior_tables)==286L &&
  !any(prior_tables$changed))
for (id in c("loan_comparisons", "loan_period_inference")) {
  stage <- now$stages[[id]]
  check(paste0(id, "_recursive_receipt_valid"),
    identical(p15_master_verify(file.path(stage$dir, "receipt.rds"))$key, stage$key))
  check(paste0(id, "_matches_registered_reference"), !any(p15_master_table_changes(id,
    stage$dir, p15_master_reference(config, id))$changed))
}
manifest <- data.table::fread(file.path(now$stages$loan_period_inference$dir, "input_manifest.csv"))
parent_csv <- file.path(now$stages$loan_comparisons$dir, "paired_comparisons.csv")
check("period_inference_consumed_current_loan_parent", normalizePath(parent_csv) %in%
  normalizePath(manifest$path))
check("historical_validation_reference_is_separate_fixed_v3_input",
  normalizePath("data-derived/p15_loan_comparisons_20260910_v3/paired_comparisons.csv") %in%
    normalizePath(manifest$path))
for (id in c("loan_comparisons", "loan_period_inference")) {
  z <- data.table::fread(file.path(now$stages[[id]]$dir, "checks.csv"))
  check(paste0(id, "_build_checks_all_pass"), all(z$passed))
}
loan <- data.table::fread(parent_csv)
check("applied_policy_rate_is_explicit", "applied_policy_rate_pct" %in% names(loan))
historical <- loan[reference=="historical_10pct_convention"]
check("historical_reference_actually_uses_ten_percent", nrow(historical)>0L &&
  all(historical$applied_policy_rate_pct==10))
old_snapshot <- data.table::fread("config/p15_analysis_source_snapshot_20260910_loan_v1.csv")
snapshot <- data.table::fread(config$analysis_snapshot_manifest)
at <- match(old_snapshot$path, snapshot$path)
check("all_953_previous_input_hashes_preserved", nrow(old_snapshot)==953L &&
  nrow(snapshot)==954L && !anyNA(at) && identical(old_snapshot$sha256, snapshot$sha256[at]))
check("one_added_input_is_historical_derived_validation_only",
  identical(setdiff(snapshot$path, old_snapshot$path),
    "data-derived/p15_loan_comparisons_20260910_v3/paired_comparisons.csv"))
check("successor_snapshot_all_hashes_valid", all(vapply(snapshot$path,
  digest::digest, character(1), file=TRUE, algo="sha256")==snapshot$sha256))
correction <- data.table::fread("data-derived/p15_loan_policy_correction_verification_20260910_v1/checks.csv")
check("independent_v3_v4_historical_correction_audit_passes", nrow(correction)==10L &&
  all(correction$passed))
checks$current_report <- now$report
checks$previous_report <- old$report
data.table::fwrite(checks, args[1])
print(checks[, .(check, passed)])
