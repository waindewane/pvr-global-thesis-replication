#!/usr/bin/env Rscript
# Additive-view verification: retain all previous results and expose only the new reference.
source("scripts/p15/activate_p15_environment.R")
source("R/p15_master.R")
source("R/p15_master_findings.R")
args <- commandArgs(TRUE)
if (length(args)<2L) stop("Supply a fresh verification CSV and the preceding 16-stage run.json")
if (file.exists(args[1])) stop("Verification output already exists")
config <- p15_master_config()
now <- jsonlite::fromJSON(file.path(config$cache_root,"current_run.json"),simplifyVector=FALSE)
old <- jsonlite::fromJSON(args[2],simplifyVector=FALSE)
checks <- data.table::data.table(check=character(),passed=logical())
check <- function(name,value) {
  if (!isTRUE(value)) stop("Standardized-reference integration verification failed: ",name)
  checks <<- rbind(checks,data.table::data.table(check=name,passed=TRUE))
}
check("sixteen_stage_names_preserved",length(now$stages)==16L &&
  identical(names(now$stages),names(old$stages)))
check("raw_cache_key_and_candidate_reused",identical(now$raw_key,old$raw_key) &&
  identical(now$candidate,old$candidate))
earlier <- setdiff(names(old$stages),c("loan_comparisons","loan_period_inference"))
check("fourteen_unaffected_analysis_caches_reused",identical(now$stages[earlier],old$stages[earlier]))
unchanged <- data.table::rbindlist(c(list(p15_master_table_changes("dataset",now$candidate,
  old$candidate)),lapply(setdiff(names(old$stages),"loan_comparisons"),function(id)
    p15_master_table_changes(id,now$stages[[id]]$dir,old$stages[[id]]$dir))))
check("297_nonloan_and_period_tables_unchanged",nrow(unchanged)==297L && !any(unchanged$changed))
for (id in c("loan_comparisons","loan_period_inference")) {
  stage <- now$stages[[id]]
  check(paste0(id,"_receipt_valid"),identical(p15_master_verify(file.path(stage$dir,"receipt.rds"))$key,stage$key))
  check(paste0(id,"_matches_registered_reference"),!any(p15_master_table_changes(id,
    stage$dir,p15_master_reference(config,id))$changed))
}
old_dir <- old$stages$loan_comparisons$dir
new_dir <- now$stages$loan_comparisons$dir
files <- list.files(old_dir,pattern="[.]csv$",full.names=FALSE)
files <- files[!grepl("manifest|environment|checks|verification|build_contract",files)]
check("eighteen_existing_loan_tables_retained",length(files)==18L && all(file.exists(file.path(new_dir,files))))
comparison_data <- function(x) {
  x <- data.table::copy(x)
  ignore <- unique(c(grep("^(build_id|schema_id|schema_version|estimator_id|admissibility_id|selection_id|source_package_ids|source_snapshot_id)$",names(x),value=TRUE),
    grep("(^|_)(path|file|directory)$",names(x),value=TRUE)))
  if (length(ignore)) x[,(ignore):=NULL]
  for (nm in intersect(c("lineage_parent_ids","parent_pair_locator"),names(x))) {
    x[,(nm):=vapply(get(nm),function(value) {
      if (is.na(value)) return(NA_character_)
      parts <- strsplit(value,";",fixed=TRUE)[[1]]
      locations <- grepl("^/|^data-derived/",parts)
      parts[locations] <- basename(parts[locations])
      paste(parts,collapse=";")
    },character(1))]
  }
  for (nm in names(x)) if (is.double(x[[nm]])) data.table::set(x,j=nm,value=signif(x[[nm]],12))
  as.data.frame(x)
}
for (f in files) {
  before <- data.table::fread(file.path(old_dir,f))
  after <- data.table::fread(file.path(new_dir,f))
  if ("reference" %in% names(after))
    after <- after[is.na(reference) | reference!="standardized_DAC_category_rule"]
  check(paste0("previous_loan_columns_retained_",f),all(names(before) %in% names(after)))
  after <- after[,names(before),with=FALSE]
  # Compare in memory: a CSV round trip would confuse literal ISO2 "NA" with missing.
  check(paste0("previous_loan_values_preserved_",f),identical(
    comparison_data(before),comparison_data(after)))
}
paired <- data.table::fread(file.path(new_dir,"paired_comparisons.csv"))
standard <- paired[reference=="standardized_DAC_category_rule"]
check("standardized_view_covers_pre2018_and_modern_records",nrow(standard)>0L &&
  any(standard$commitment_year<2018L) && any(standard$commitment_year>=2018L))
check("standardized_rates_follow_recorded_categories",all(standard$standardized_category_rate_pct %in% c(6,7,9)) &&
  all(standard$standardized_category_rate_pct==standard$group_rate_pct))
check("long_comparisons_expose_the_reference_discount_rate",
  "reference_discount_rate_pct" %in% names(paired) &&
  all(standard$reference_discount_rate_pct==standard$standardized_category_rate_pct) &&
  all(paired[reference=="fixed5",reference_discount_rate_pct]==5) &&
  all(paired[reference=="historical_10pct_convention",reference_discount_rate_pct]==10) &&
  all(paired[reference=="modern_DAC_common_reference",reference_discount_rate_pct==group_rate_pct]))
check("standardized_values_and_differences_use_explicit_new_fields",
  all(abs(standard$reference_ge_pct-standard$ge_standardized_category_pct)<1e-10) &&
  all(abs(standard$delta_ge_pp-(standard$market_ge_pct-standard$ge_standardized_category_pct))<1e-10))
historical <- paired[reference=="historical_10pct_convention"]
check("historical_ten_percent_view_remains_separate",nrow(historical)>0L &&
  all(historical$applied_policy_rate_pct==10))
manifest <- data.table::fread(file.path(now$stages$loan_period_inference$dir,"input_manifest.csv"))
check("period_inference_consumes_current_additive_loan_parent",
  normalizePath(file.path(new_dir,"paired_comparisons.csv")) %in% normalizePath(manifest$path))
snapshot <- data.table::fread(config$analysis_snapshot_manifest)
check("same_954_registered_analysis_inputs_retained",identical(config$analysis_snapshot_manifest,
  "config/p15_analysis_source_snapshot_20260910_period_v1.csv") && nrow(snapshot)==954L)
check("registered_input_hashes_still_valid",all(vapply(snapshot$path,digest::digest,
  character(1),file=TRUE,algo="sha256")==snapshot$sha256))
checks$current_report <- now$report
checks$previous_report <- old$report
data.table::fwrite(checks,args[1])
print(checks[,.(check,passed)])
