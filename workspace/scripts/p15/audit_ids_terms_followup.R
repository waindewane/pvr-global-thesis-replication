#!/usr/bin/env Rscript
# Offline diagnostic. Never alters raw inputs, case decisions, or working rates.
library(data.table)
library(jsonlite)
library(digest)
root <- "data-raw/p15_ids_terms_followup_20260906"
build_id <- paste0("p15_ids_terms_followup_",format(Sys.time(),"%Y%m%d_%H%M%S",tz="UTC"))
out <- file.path("data-derived",build_id)
stopifnot(!dir.exists(out))
dir.create(out,recursive=TRUE)
script <- "scripts/p15/audit_ids_terms_followup.R"
source_ids <- paste0("SRC-P15-IDS-XLSX-",c("A_D","E_K","N_Q","R_U"),"-20260906")
registry <- fread("docs/governance/source_package_registry.csv",colClasses="character")
pkg_for_path <- function(p) {
  id <- registry[path==p,source_package_id]
  if(length(id)) return(paste(id,collapse=";"))
  if(grepl("_bnd_extracted.csv$",p)) return(pkg_for_path(sub("_bnd_extracted.csv$",".xlsx",p)))
  if(basename(p)=="six_country_bulk_metadata.csv") return("SRC-P15-IDS-CSV-LONG-20260906")
  stop("Unregistered diagnostic input: ",p)
}
audit_sources <- fread(file.path(root,"source_manifest.csv"))
for(i in seq_len(nrow(audit_sources))) {
  registered <- registry[source_package_id==audit_sources$source_package_id[i]]
  stopifnot(nrow(registered)==1L,registered$path==audit_sources$path[i],
    registered$sha256==digest(audit_sources$path[i],algo="sha256",file=TRUE))
}
source_ids <- unique(c(source_ids,audit_sources$source_package_id,
  registry[grepl("p15_ids_review_public_snapshot_20260906|p15_case_review_20260906/ids_",path),source_package_id]))
schema_id <- "SCHEMA-P15-IDS-TERMS-FOLLOWUP-V1"
bundle <- list(estimator_id="not_applicable",admissibility_id="not_applicable",
  selection_id="not_applicable",schema_id=schema_id,build_id=build_id,
  source_package_ids=paste(source_ids,collapse=";"),release_id="not_applicable")
emit <- function(x,name) {
  x <- copy(x)
  for (n in names(bundle)) if (!n %in% names(x)) x[,(n):=bundle[[n]]]
  fwrite(x,file.path(out,paste0(name,".csv")))
}
sha <- function(p) digest(p,algo="sha256",file=TRUE)
protected <- c("data-raw/p15_case_review_20260906/ids_case_dispositions.csv",
  "data-raw/p15_case_review_20260906/ids_decision_definitions.csv",
  "data-derived/p15_full_replay_20260906_112727/isolated_build/data-derived/p15_reviewed_evidence_20260906_v1/p15_country_year_dataset.csv")
before <- vapply(protected,sha,character(1))
paths <- Sys.glob(file.path(root,"*_bnd_extracted.csv"))
stopifnot(length(paths)==4L)
x <- rbindlist(lapply(paths,fread))
stopifnot(nrow(x)==312L,!anyDuplicated(x[,.(iso3,year,series,counterpart)]),
  all(x$counterpart=="BND"),setequal(x$iso3,c("CHN","IDN","KAZ","PER","RWA","TUR")))
api_manifest <- fread("data-raw/p15_ids_review_public_snapshot_20260906/source_manifest.csv")
api_paths <- api_manifest$path
stopifnot(length(api_paths)==4L,all(vapply(api_paths,sha,character(1))==api_manifest$sha256))
api <- rbindlist(lapply(api_paths,function(path) {
  j <- fromJSON(path); stopifnot(j$pages==1L)
  d <- j$source$data
  rbindlist(lapply(seq_len(nrow(d)),function(i) {
    v <- d$variable[[i]]
    getid <- function(s) {a<-v$id[v$concept==s];stopifnot(length(a)==1L);a}
    data.table(iso3=getid("Country"),year=as.integer(sub("YR","",getid("Time"))),
      series=getid("Series"),counterpart=getid("Counterpart-Area"),api_value=d$value[i],
      api_lastupdated=j$lastupdated,api_source_file=path)
  }))
}))
stopifnot(!anyDuplicated(api[,.(iso3,year,series,counterpart)]))
comparison <- merge(x,api,by=c("iso3","year","series","counterpart"),all.x=TRUE)
stopifnot(nrow(comparison)==312L,!anyNA(comparison$api_source_file))
comparison[,difference:=value-api_value]
# Machine parsing tolerance only. Not a tolerance for grace exceeding maturity.
comparison[,match:=fifelse(is.na(value)&is.na(api_value),TRUE,
  !is.na(value)&!is.na(api_value)&abs(value-api_value)<1e-10)]
comparison[,whole_dollar_rounding_only:=!match & series=="DT.COM.DPPG.CD" &
  !is.na(value) & !is.na(api_value) & value==round(api_value,0)]
emit(comparison,"all_312_export_comparisons")
cases <- fread(protected[1])[decision_code=="retain_rate_block_inconsistent_terms"]
stopifnot(nrow(cases)==8L)
wide <- dcast(x,iso3+year~series,value.var="value")
cases <- merge(cases,wide,by.x=c("iso3","analysis_year"),by.y=c("iso3","year"),all.x=TRUE)
stopifnot(nrow(cases)==8L,!anyNA(cases$DT.MAT.DPPG),!anyNA(cases$DT.GPA.DPPG))
cases[,`:=`(maturity_match=abs(DT.MAT.DPPG-expected_maturity_years)<1e-10,
  grace_match=abs(DT.GPA.DPPG-expected_grace_years)<1e-10,
  rate_match=abs(DT.INR.DPPG-expected_rate_pct)<1e-10,
  grace_excess_years=DT.GPA.DPPG-DT.MAT.DPPG)]
cases[,grace_excess_days_approx:=grace_excess_years*365.25]
cases[,magnitude_description:=fcase(grace_excess_days_approx<1,"less_than_one_day",
  grace_excess_years>=1,"more_than_one_year",default="between_one_day_and_one_year")]
cases[,`:=`(source_cause_status="unresolved",permission_changed=FALSE)]
emit(cases,"eight_case_comparison")

# An illustrative decomposition, not identification of undisclosed loans.
rwa <- cases[iso3=="RWA" & analysis_year==2021]
main_amount <- 620000000
other_amount <- rwa$DT.COM.DPPG.CD-main_amount
stopifnot(other_amount>0)
rwa_check <- data.table(quantity=c("rate_pct","maturity_years","grace_years"),
  documented_or_assumed_main=c(5.5,10,10),
  hypothesized_other=c(5,20,21.9166),
  published=c(rwa$DT.INR.DPPG,rwa$DT.MAT.DPPG,rwa$DT.GPA.DPPG))
rwa_check[,`:=`(main_amount_usd=main_amount,unidentified_remainder_usd=other_amount)]
rwa_check[,hypothesis_value:=(main_amount*documented_or_assumed_main+
  other_amount*hypothesized_other)/(main_amount+other_amount)]
rwa_check[,`:=`(difference=hypothesis_value-published,
  implied_other=(published*(main_amount+other_amount)-main_amount*documented_or_assumed_main)/other_amount,
  claim="Illustrative close fit, not exact parity or verified underlying record membership",
  grace_caveat="Main grace=10 is a bullet illustration, not verification of every principal payment date",
  evidence="RWA_2021_ISSUE;RWA_WB_2023;official BND commitment and term snapshots")]
emit(rwa_check,"rwanda_2021_decomposition_diagnostic")
per <- fread(file.path(root,"peru_2020_documented_issues.csv"))
stopifnot(nrow(per)==6L,all(per$amount_usd>0))
per[,calendar_years_act_36525:=as.numeric(as.Date(maturity_date)-as.Date(issue_date))/365.25]
per_check <- data.table(quantity=c("commitment_usd","coupon_pct","calendar_years_act_36525"),
  reconstructed=c(sum(per$amount_usd),weighted.mean(per$coupon_pct,per$amount_usd),
    weighted.mean(per$calendar_years_act_36525,per$amount_usd)),
  ids_comparator=c(cases[iso3=="PER",DT.COM.DPPG.CD],cases[iso3=="PER",DT.INR.DPPG],
    cases[iso3=="PER",DT.MAT.DPPG]))
per_check[,`:=`(difference=reconstructed-ids_comparator,
  claim="Amount and coupon corroboration does not prove complete IDS record membership or repair terms",
  date_convention="ACT/365.25 is an explicit diagnostic only; not claimed as IDS methodology")]
emit(per_check,"peru_2020_reconstruction_diagnostic")

meta <- fread(file.path(root,"six_country_bulk_metadata.csv"))
term_notes <- meta[grepl("DT.GPA.DPPG|DT.MAT.DPPG|DT.INR.DPPG",`Series Code`)]
emit(term_notes,"matched_term_metadata")
checks <- data.table(check=c("312 unique BND observations extracted","all export values match or differ only by recorded whole-dollar rounding",
  "eight cases and all 24 published term-rate values match", "no matching term-specific metadata in bulk notes",
  "Peru amount matches and coupon rounds to IDS value", "protected inputs and current product unchanged"),
  passed=c(nrow(comparison)==312L,all(comparison$match|comparison$whole_dollar_rounding_only),
    all(cases$maturity_match&cases$grace_match&cases$rate_match),nrow(term_notes)==0L,
    per_check[quantity=="commitment_usd",difference]==0 &&
      round(per_check[quantity=="coupon_pct",reconstructed],4)==cases[iso3=="PER",DT.INR.DPPG],
    identical(before,vapply(protected,sha,character(1)))))
emit(checks,"verification")
stopifnot(all(checks$passed))

emit(data.table(path=protected,sha256_before=unname(before),
  sha256_after=unname(vapply(protected,sha,character(1)))),"protected_file_check")
inputs <- unique(c(paths,sub("_bnd_extracted.csv$",".xlsx",paths),api_paths,protected[1:2],
  file.path(root,c("ids_csv_long.zip","six_country_bulk_metadata.csv","peru_2020_documented_issues.csv"))))
manifest <- function(paths,role) rbindlist(lapply(paths,function(p) {
  tab <- if(grepl("\\.csv$",p)) fread(p) else NULL
  data.table(artifact_path=p,artifact_role=role,bytes=file.info(p)$size,
    rows=if(is.null(tab))NA_integer_ else nrow(tab),columns=if(is.null(tab))NA_integer_ else ncol(tab),
    sha256=sha(p),source_package_ids=if(role=="raw_input")pkg_for_path(p) else bundle$source_package_ids,
    estimator_id=bundle$estimator_id,admissibility_id=bundle$admissibility_id,
    selection_id=bundle$selection_id,schema_id=schema_id,build_id=build_id,
    producing_script=script,producing_script_sha256=sha(script))
}))
fwrite(manifest(inputs,"raw_input"),file.path(out,"input_manifest.csv"))
scripts <- c(script,"scripts/p15/extract_ids_terms_followup.py")
fwrite(manifest(scripts,"script"),file.path(out,"script_manifest.csv"))
fwrite(data.table(build_id=build_id,r_version=R.version.string,platform=R.version$platform,
  locale=Sys.getlocale(),timezone=Sys.timezone(),renv_lock_sha256=sha("renv.lock"),
  data_table_version=as.character(packageVersion("data.table")),
  jsonlite_version=as.character(packageVersion("jsonlite")),
  digest_version=as.character(packageVersion("digest")),
  extractor_python_and_lxml=system2("/Users/waindewane/.cache/codex-runtimes/codex-primary-runtime/dependencies/python/bin/python3",
    c("-c",shQuote("import sys,lxml; print(sys.version.split()[0],lxml.__version__)")),stdout=TRUE)),
  file.path(out,"environment_manifest.csv"))
fwrite(manifest(list.files(out,full.names=TRUE),"validation_output"),file.path(out,"output_manifest.csv"))
cat("Diagnostic output:",out,"\n")
print(checks)
