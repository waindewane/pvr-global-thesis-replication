#!/usr/bin/env Rscript
# Prespecified descriptive contextual associations; no causal/inferential claims.
source("scripts/p15/activate_p15_environment.R")
source("R/research_governance.R")
suppressPackageStartupMessages({library(data.table); library(jsonlite); library(digest)})
args <- commandArgs(TRUE)
out <- if (length(args)) args[1] else "data-derived/p15_gbohoui_context_20260911_v1"
if (dir.exists(out)) stop("Refusing to overwrite: ", out)
state_path <- "data-derived/p15_master/current_run.json"
candidate <- Sys.getenv("P15_ANALYSIS_BASE", "")
regional <- Sys.getenv("P15_REGIONAL_BASE", "")
if (!nzchar(candidate) || !nzchar(regional)) {
  state <- fromJSON(state_path)
  if (!nzchar(candidate)) candidate <- state$candidate
  if (!nzchar(regional)) regional <- state$stages$regional$dir
}
src <- "sources/literature_review/thesis_concepts_20260911"
design <- "docs/thesis_design/feedback_2026-09-11_followup/GBOHOUI_CONTEXT_DESIGN.md"
input_paths <- c(file.path(candidate, "core_evidence.csv"),
  file.path(regional, "country_year_views.csv"),
  file.path(src, "extracted/wgi_regulatory_quality_2012_2024.csv"),
  file.path(src, "extracted/open_budget_index_2012_2024_available_rounds.csv"),
  file.path(src, "originals/wgi_regulatory_quality_current_2012_2024.json"),
  file.path(src, "originals/obs_full_timeseries_2006_2025.xlsx"),
  file.path(src, "originals/gbohoui_ouedraogo_some_2023.pdf"),
  file.path(src, "source_ledger.csv"), design)
before <- vapply(input_paths, function(p) digest(file=p, algo="sha256"), character(1))
core <- fread(input_paths[1], na.strings="")
v <- fread(input_paths[2], na.strings="")[view %in% c("primary", "secondary")]
stopifnot(!anyDuplicated(core[,.(iso3,analysis_year)]),
          !anyDuplicated(v[,.(iso3,analysis_year,view)]))
v[, registered_rate_pct := rate]
v[reviewed_eight_exclusion %in% TRUE, rate := NA_real_]
v <- merge(v, core[,.(iso3,analysis_year,historical_income_level,
  historical_lmic_reporting_scope)], by=c("iso3","analysis_year"), all.x=TRUE)
stopifnot(all(v$historical_lmic_reporting_scope %in% TRUE),
  all(v$historical_income_level %in% c("Low income","Lower middle income","Upper middle income")))
v[, rate_available := is.finite(rate)]
for (nm in c("maturity_years","issue_count","first_rate_date","last_rate_date")) {
  suffix <- switch(nm,maturity_years="market_maturity_years",issue_count="retained_quantitative_issue_count",
    first_rate_date="first_rate_date",last_rate_date="last_rate_date")
  v[, (nm) := core[[paste0("primary_usd_",suffix)]][match(paste(iso3,analysis_year),paste(core$iso3,core$analysis_year))]]
  sec <- v$view=="secondary"
  v[sec, (nm) := core[[paste0("secondary_usd_",suffix)]][match(paste(iso3,analysis_year),paste(core$iso3,core$analysis_year))]]
}
w <- fread(input_paths[3], na.strings="")
o <- fread(input_paths[4], na.strings="")
unmapped_wgi <- w[is.na(iso3) | !nzchar(iso3)]
w <- w[!is.na(iso3) & nzchar(iso3)]
stopifnot(!anyDuplicated(w[,.(iso3,year)]), !anyDuplicated(o[,.(iso3,survey_year)]))
covariates <- rbindlist(list(
  w[,.(iso3,analysis_year=year,score_country=country,indicator="regulatory_quality",
    raw_score=regulatory_quality_estimate,score=regulatory_quality_estimate,
    score_unit="one WGI estimate unit",source_row=NA_integer_,
    source_locator=paste0("GOV_WGI_RQ.EST::",iso3,"::",year),
    measurement_comparable=TRUE)],
  o[,.(iso3,analysis_year=survey_year,score_country=country,indicator="budget_transparency",
    raw_score=open_budget_index,score=open_budget_index/10,
    score_unit="ten OBI points",source_row=as.integer(source_row),
    source_locator=paste0("OBS_Data_AllYears!row",source_row),
    measurement_comparable=survey_year>=2017L)]))
stopifnot(!anyDuplicated(covariates[,.(iso3,analysis_year,indicator)]))
z <- rbindlist(lapply(c("regulatory_quality","budget_transparency"), function(ind) {
  a <- merge(v, covariates[indicator==ind], by=c("iso3","analysis_year"), all.x=TRUE)
  a[, indicator := ind]
  a[, score_available := is.finite(score)]
  a[, matched_for_full_scope_model := rate_available & score_available & measurement_comparable %in% TRUE]
  a
}))
stopifnot(nrow(z)==nrow(v)*2L,
          !anyDuplicated(z[,.(iso3,analysis_year,view,indicator)]))
safe <- function(x,f=mean) if(any(is.finite(x)))f(x[is.finite(x)]) else NA_real_
coverage_fun <- function(d) data.table(population_country_years=nrow(d),
  population_countries=uniqueN(d$iso3),eligible_rate_country_years=sum(d$rate_available),
  score_country_years=sum(d$score_available),
  matched_country_years=sum(d$rate_available & d$score_available),
  comparable_matched_country_years=sum(d$matched_for_full_scope_model),
  comparable_matched_countries=uniqueN(d[matched_for_full_scope_model==TRUE,iso3]),
  mean_score_with_rate=safe(d[rate_available==TRUE,raw_score]),
  mean_score_without_rate=safe(d[rate_available==FALSE,raw_score]))
coverage <- z[,coverage_fun(.SD),by=.(indicator,view)]
coverage_year <- z[,coverage_fun(.SD),by=.(indicator,view,analysis_year)]
coverage_income <- z[,coverage_fun(.SD),by=.(indicator,view,historical_income_level)]
coverage_region <- z[,coverage_fun(.SD),by=.(indicator,view,region)]
matched_income_context <- z[matched_for_full_scope_model==TRUE,
  .(records=.N,countries=uniqueN(iso3),mean_rate_pct=mean(rate),mean_score=mean(raw_score),
    median_maturity_years=median(as.numeric(maturity_years),na.rm=TRUE)),
  by=.(indicator,view,historical_income_level)]

# Do not print conventional lm p-values or treat deletion ranges as intervals.
controls <- list(year="factor(analysis_year)",
  year_income="factor(analysis_year) + factor(historical_income_level)",
  country_year="factor(iso3) + factor(analysis_year)")
fit_one <- function(d, specification) {
  f <- as.formula(paste("rate ~ score +", controls[[specification]]))
  fit <- lm(f, data=as.data.frame(d))
  slope <- unname(coef(fit)["score"])
  mm <- model.matrix(fit)
  nuisance <- mm[,colnames(mm)!="score",drop=FALSE]
  rx <- lm.fit(nuisance,d$score)$residuals
  ry <- lm.fit(nuisance,d$rate)$residuals
  denom <- sum(rx^2)
  fwl <- if(denom>1e-12)sum(rx*ry)/denom else NA_real_
  if(!is.finite(slope) || fit$df.residual<=0 || denom<=1e-12) slope <- NA_real_
  list(fit=fit,slope=slope,fwl=fwl,rx=rx,ry=ry,
    residualized_score_sd=sqrt(mean(rx^2)),
    score_variation_fraction=if(sum((d$score-mean(d$score))^2)>0)
      denom/sum((d$score-mean(d$score))^2) else NA_real_)
}
models <- coefficients <- members <- deletions <- residuals <- variations <- list()
k <- 0L
for (ind in c("regulatory_quality","budget_transparency")) {
  for (view_name in c("primary","secondary")) {
    for (period_name in c("full_scope","modern_2018_2024")) {
      d <- z[indicator==ind & view==view_name & matched_for_full_scope_model==TRUE]
      if(period_name=="modern_2018_2024")d <- d[analysis_year>=2018L]
      setorder(d,iso3,analysis_year)
      if(!nrow(d)) next
      country_var <- d[,.(records=.N,score_sd=if(.N>1L)sd(score) else 0,
        minimum_score=min(score),maximum_score=max(score)),by=iso3]
      country_var[,`:=`(indicator=ind,view=view_name,period=period_name)]
      variations[[length(variations)+1L]] <- country_var
      for (specification in names(controls)) {
        k <- k+1L
        id <- paste(ind,view_name,period_name,specification,sep="::")
        f <- fit_one(d,specification)
        models[[k]] <- data.table(model_id=id,indicator=ind,view=view_name,period=period_name,
          specification=specification,records=nrow(d),countries=uniqueN(d$iso3),
          years=uniqueN(d$analysis_year),first_year=min(d$analysis_year),last_year=max(d$analysis_year),
          score_unit=unique(d$score_unit),slope_rate_pp=f$slope,fwl_slope_rate_pp=f$fwl,
          residual_df=f$fit$df.residual,model_rank=f$fit$rank,
          residualized_score_rms=f$residualized_score_sd,
          residualized_score_variation_fraction=f$score_variation_fraction,
          model_state=if(is.finite(f$slope))"descriptive_estimate" else "not_identified",
          countries_with_repeated_varying_score=sum(country_var$records>=2 & country_var$score_sd>1e-8),
          information_date="retrospective_same_year_or_survey_round_not_real_time")
        coefficients[[k]] <- data.table(model_id=id,term=names(coef(f$fit)),coefficient=unname(coef(f$fit)))
        members[[k]] <- d[,.(model_id=id,indicator,view,iso3,analysis_year,country,region,
          historical_income_level,rate_pct=rate,score,raw_score,source_locator)]
        residuals[[k]] <- d[,.(model_id=id,iso3,analysis_year,
          residualized_score=f$rx,residualized_rate_pct=f$ry)]
        deletions[[k]] <- rbindlist(lapply(sort(unique(d$iso3)),function(drop_country) {
          dd <- d[iso3!=drop_country]
          ff <- tryCatch(fit_one(dd,specification),error=function(e)NULL)
          b <- if(is.null(ff))NA_real_ else ff$slope
          data.table(model_id=id,omitted_iso3=drop_country,remaining_records=nrow(dd),
            slope_rate_pp=b,change_from_full_pp=b-f$slope,
            deletion_state=if(is.finite(b))"descriptive_estimate" else "not_identified")
        }))
      }
    }
  }
}
model_summary <- rbindlist(models)
coefficient_table <- rbindlist(coefficients)
model_membership <- rbindlist(members)
model_residuals <- rbindlist(residuals)
country_deletions <- rbindlist(deletions)
country_variation <- rbindlist(variations)
deletion_summary <- country_deletions[,.(finite_deletions=sum(is.finite(slope_rate_pp)),
  deletion_min_slope=safe(slope_rate_pp,min),deletion_max_slope=safe(slope_rate_pp,max),
  largest_absolute_change=safe(abs(change_from_full_pp),max),
  most_influential_omission=omitted_iso3[which.max(abs(change_from_full_pp))]),by=model_id]
model_summary <- merge(model_summary,deletion_summary,by="model_id")
support_identity <- model_membership[,.(support=paste(paste(iso3,analysis_year),collapse=";")),
  by=.(indicator,view,model_id)][,.(supports=uniqueN(support)),by=.(indicator,view,
    period=sub("^.*::(full_scope|modern_2018_2024)::.*$","\\1",model_id))]
checks <- data.table(check=c("country_year_source_unique","all_sources_observed_usd",
 "all_rows_historical_lmic","joining_does_not_duplicate_rows","all_thirteen_years_retained_in_coverage",
 "obi_model_only_2017_2019_2021_2023","no_empty_iso3_in_models",
 "all_model_values_finite","same_support_for_each_control_specification",
 "fwl_independently_reproduces_identified_slopes","source_inputs_unchanged"),
 passed=c(!anyDuplicated(v[,.(iso3,analysis_year,view)]),
 setequal(unique(z$view),c("primary","secondary")),all(z$historical_lmic_reporting_scope),
 nrow(z)==nrow(v)*2L,identical(sort(unique(z$analysis_year)),2012:2024),
 all(model_membership[indicator=="budget_transparency",analysis_year]%in%c(2017L,2019L,2021L,2023L)),
 all(nzchar(model_membership$iso3)),all(is.finite(model_membership$rate_pct)&is.finite(model_membership$score)),
 all(support_identity$supports==1L),
 all(abs(model_summary[is.finite(slope_rate_pp),slope_rate_pp-fwl_slope_rate_pp])<1e-8),
 identical(before,vapply(input_paths,function(p)digest(file=p,algo="sha256"),character(1)))))
stopifnot(all(checks$passed))
dir.create(out,recursive=TRUE)
tables <- list(joined_context=z,coverage_overall=coverage,coverage_by_year=coverage_year,
 coverage_by_income=coverage_income,coverage_by_region=coverage_region,
 matched_income_context=matched_income_context,model_summary=model_summary,
 model_coefficients=coefficient_table,model_membership=model_membership,
 model_residuals=model_residuals,country_deletion_sensitivity=country_deletions,
 country_score_variation=country_variation,unmapped_wgi_economies=unmapped_wgi,checks=checks)
ids <- list(build_id=basename(out),schema_id="SCHEMA-P15-GBOHOUI-CONTEXT-20260911-V1",
 estimator_id="EST-P15-OBSERVED-YIELD-CONTEXT-DESCRIPTIVE-V1",
 admissibility_id="ADM-P15-EXISTING-OBSERVED-HOLDS-OBS-COMPARABLE-ROUNDS-V1",
 selection_id="SEL-P15-NO-BENCHMARK-CHANGE-CONTEXT-V1",
 source_package_ids="TC20260911-WGIRQ;TC20260911-OBSTIMESERIES;TC20260911-GBOHOUI2023;SRC-P15-CURRENT-REGIONAL")
for(nm in names(tables)) {
  tables[[nm]][,`:=`(build_id=ids$build_id,schema_id=ids$schema_id,estimator_id=ids$estimator_id,
    admissibility_id=ids$admissibility_id,selection_id=ids$selection_id,
    lifecycle_status="diagnostic",release_state="private_research")]
  fwrite(tables[[nm]],file.path(out,paste0(nm,".csv")),na="")
}
manifest <- function(paths,role)do.call(pvr_manifest_rows,c(list(paths=paths,artifact_role=role),ids))
fwrite(manifest(input_paths,"context_input"),file.path(out,"input_manifest.csv"))
code_paths <- c("scripts/p15/annotation_followup_20260911/build_gbohoui_context.R",
 "scripts/p15/activate_p15_environment.R","R/research_governance.R","renv.lock",
 file.path(src,"inspect_public_covariates.R"))
fwrite(manifest(code_paths,"context_code"),file.path(out,"code_manifest.csv"))
write_json(c(ids,list(lifecycle_status="diagnostic",release_state="private_research",
 candidate=candidate,regional=regional,design=design,
 weighting="equal observed country-years within each matched sample",
 hypotheses=c("Higher regulatory quality accompanies lower observed USD yields",
 "Higher budget transparency accompanies lower observed USD yields"),
 inference="No p-values or confidence intervals; country-deletion ranges are sensitivity only",
 wgi_vintage="2025 revision; API lastupdated 2026-03-18",
 obi_years=c(2017,2019,2021,2023),
 benchmark_change=FALSE)),file.path(out,"version_bundle.json"),auto_unbox=TRUE,pretty=TRUE)
writeLines(capture.output(sessionInfo()),file.path(out,"environment.txt"))
fwrite(manifest(list.files(out,full.names=TRUE),"context_output"),file.path(out,"output_manifest.csv"))
cat("Completed",nrow(model_summary),"prespecified descriptive models and",nrow(checks),"checks:",out,"\n")
print(model_summary[,.(indicator,view,period,specification,records,countries,years,
 slope_rate_pp,deletion_min_slope,deletion_max_slope,residualized_score_variation_fraction)])
