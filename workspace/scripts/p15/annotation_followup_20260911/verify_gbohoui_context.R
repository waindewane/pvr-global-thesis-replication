#!/usr/bin/env Rscript
# Independent exact-join/measurement checks and two SVD coefficient replays.
source("scripts/p15/activate_p15_environment.R")
suppressPackageStartupMessages({library(data.table);library(jsonlite);library(readxl);library(digest)})
args <- commandArgs(TRUE)
base <- if(length(args))args[1]else"data-derived/p15_gbohoui_context_20260911_v1"
out <- if(length(args)>1L)args[2]else"data-derived/p15_gbohoui_context_verification_20260911_v1"
if(dir.exists(out))stop("Verification output already exists")
input <- fread(file.path(base,"input_manifest.csv"),na.strings="")
locate <- function(suffix) {
  p <- input$artifact_path[endsWith(input$artifact_path,suffix)]
  stopifnot(length(p)==1L)
  p
}
model_path <- file.path(base,"model_summary.csv")
membership_path <- file.path(base,"model_membership.csv")
models <- fread(model_path,na.strings="")
members <- fread(membership_path,na.strings="")
core_path <- locate("/core_evidence.csv")
regional_path <- locate("/country_year_views.csv")
json_path <- locate("/originals/wgi_regulatory_quality_current_2012_2024.json")
obs_path <- locate("/originals/obs_full_timeseries_2006_2025.xlsx")
core <- fread(core_path,na.strings="")
regional <- fread(regional_path,na.strings="")
checks <- data.table(check=character(),passed=logical())
check <- function(name,value) {
  if(!isTRUE(value))stop("Context verification failed: ",name)
  checks <<- rbind(checks,data.table(check=name,passed=TRUE))
}
key <- function(iso,year)paste(iso,year,sep="::")
check("24_prespecified_models_unique",nrow(models)==24L && !anyDuplicated(models$model_id))
check("membership_unique_within_model",!anyDuplicated(members[,.(model_id,iso3,analysis_year)]))
check("observed_primary_secondary_only",setequal(unique(members$view),c("primary","secondary")))
check("no_reported_pvalues_or_confidence_intervals",!any(grepl("p_value|pvalue|p_holm|ci_low|ci_high",names(models))))

# Reconstruct covariates from original JSON/workbook, without using producer extracts.
j <- fromJSON(json_path,simplifyVector=FALSE)
w <- rbindlist(lapply(j[[2]],function(r)data.table(iso3=r$countryiso3code,
  analysis_year=as.integer(r$date),score=if(is.null(r$value))NA_real_ else r$value,
  indicator_id=r$indicator$id)))
check("WGI_original_indicator_is_estimate",all(w$indicator_id=="GOV_WGI_RQ.EST"))
w <- w[!is.na(iso3)&nzchar(iso3)]
obs <- as.data.table(read_excel(obs_path,sheet="OBS_Data_AllYears",skip=3))
o <- data.table(iso3=obs$ISO,analysis_year=as.integer(obs$Year),
  score=as.numeric(obs[["OBI (unrounded)"]])/10,source_row=seq_len(nrow(obs))+4L)
check("original_country_year_keys_unique",!anyDuplicated(w[,.(iso3,analysis_year)]) &&
  !anyDuplicated(o[,.(iso3,analysis_year)]))

for(ind in c("regulatory_quality","budget_transparency")) {
  d <- members[indicator==ind]
  src <- if(ind=="regulatory_quality")w else o
  at <- match(key(d$iso3,d$analysis_year),key(src$iso3,src$analysis_year))
  check(paste0(ind,"_exact_original_country_year_join"),!anyNA(at))
  check(paste0(ind,"_score_scale_matches_original"),all(abs(d$score-src$score[at])<1e-10))
  if(ind=="budget_transparency") {
    check("OBI_raw_score_divided_by_ten",all(abs(d$score-d$raw_score/10)<1e-10))
    check("OBI_comparable_rounds_only",all(d$analysis_year%in%c(2017L,2019L,2021L,2023L)))
    check("OBI_exact_workbook_row_provenance",all(d$source_locator==paste0("OBS_Data_AllYears!row",src$source_row[at])))
  }
}

# Reconstruct every prespecified sample from the saved upstream observed view.
v <- regional[view%in%c("primary","secondary")]
ci <- match(key(v$iso3,v$analysis_year),key(core$iso3,core$analysis_year))
check("upstream_rate_population_is_historical_LMIC",!anyNA(ci) &&
  all(core$historical_lmic_reporting_scope[ci]))
v[,historical_income_level:=core$historical_income_level[ci]]
v[,usable:=is.finite(rate) & !(reviewed_eight_exclusion%in%TRUE)]
expected_samples <- list()
for(i in seq_len(nrow(models))) {
  m <- models[i]
  src <- if(m$indicator=="regulatory_quality")w else o
  a <- v[view==m$view & usable==TRUE]
  at <- match(key(a$iso3,a$analysis_year),key(src$iso3,src$analysis_year))
  a[,score:=src$score[at]]
  a <- a[is.finite(score)]
  if(m$indicator=="budget_transparency")a <- a[analysis_year%in%c(2017L,2019L,2021L,2023L)]
  if(m$period=="modern_2018_2024")a <- a[analysis_year>=2018L]
  b <- members[model_id==m$model_id]
  setorder(a,iso3,analysis_year);setorder(b,iso3,analysis_year)
  stopifnot(identical(key(a$iso3,a$analysis_year),key(b$iso3,b$analysis_year)),
    all(abs(a$rate-b$rate_pct)<1e-10),all(a$historical_income_level==b$historical_income_level))
  expected_samples[[i]] <- data.table(model_id=m$model_id,records=nrow(a),
    countries=uniqueN(a$iso3),years=uniqueN(a$analysis_year),passed=TRUE)
}
sample_checks <- rbindlist(expected_samples)
check("all_24_samples_rates_income_and_holds_reconstructed",all(sample_checks$passed))
at <- match(models$model_id,sample_checks$model_id)
check("all_reported_sample_counts_match",all(models$records==sample_checks$records[at]) &&
  all(models$countries==sample_checks$countries[at]) && all(models$years==sample_checks$years[at]))

# SVD solves the full design matrix directly; it does not reuse lm/FWL residuals.
ids <- c("regulatory_quality::primary::full_scope::year_income",
  "budget_transparency::primary::full_scope::country_year")
replay <- rbindlist(lapply(ids,function(id) {
  d <- members[model_id==id]
  nuisance <- if(grepl("::year_income$",id))
    model.matrix(~factor(analysis_year)+factor(historical_income_level),data=d) else
    model.matrix(~factor(iso3)+factor(analysis_year),data=d)
  X <- cbind(score=d$score,nuisance)
  decomp <- svd(X)
  retained <- decomp$d>max(decomp$d)*1e-12
  beta <- decomp$v[,retained,drop=FALSE] %*%
    (crossprod(decomp$u[,retained,drop=FALSE],d$rate_pct)/decomp$d[retained])
  observed <- models[model_id==id,slope_rate_pp]
  data.table(model_id=id,producer_slope=observed,independent_svd_slope=beta[1],
    absolute_difference=abs(observed-beta[1]),design_columns=ncol(X),svd_rank=sum(retained))
}))
check("two_displayed_slopes_reproduce_by_SVD",all(replay$absolute_difference<1e-10))

dir.create(out,recursive=TRUE)
fwrite(checks,file.path(out,"checks.csv"))
fwrite(sample_checks,file.path(out,"sample_verification.csv"))
fwrite(replay,file.path(out,"slope_verification.csv"))
manifest <- function(ps)data.table(path=ps,bytes=file.info(ps)$size,
  sha256=vapply(ps,function(p)digest(file=p,algo="sha256"),character(1)))
fwrite(manifest(c(model_path,membership_path,core_path,regional_path,json_path,obs_path,
  file.path(base,"input_manifest.csv"))),file.path(out,"input_manifest.csv"))
fwrite(manifest(c("scripts/p15/annotation_followup_20260911/verify_gbohoui_context.R",
  "scripts/p15/activate_p15_environment.R","renv.lock")),file.path(out,"code_manifest.csv"))
writeLines(capture.output(sessionInfo()),file.path(out,"environment.txt"))
write_json(list(build_id=basename(out),lifecycle_status="diagnostic",release_state="private_research",
  purpose="independent_original_source_join_scale_and_selected_slope_verification",
  source_build=basename(base),new_models=FALSE),file.path(out,"verification_scope.json"),auto_unbox=TRUE,pretty=TRUE)
fwrite(manifest(list.files(out,full.names=TRUE)),file.path(out,"output_manifest.csv"))
cat(nrow(checks),"independent checks passed\n")
print(replay)
