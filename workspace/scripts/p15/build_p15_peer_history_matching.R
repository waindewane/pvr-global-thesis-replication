#!/usr/bin/env Rscript
suppressPackageStartupMessages({library(data.table);library(digest)})
source("R/p15_bounded_fallback_comparison.R")
source("R/p15_peer_history_matching.R")
source("R/research_governance.R")
args <- commandArgs(trailingOnly=TRUE)
base <- if(length(args))args[1] else "data-derived/p15_reference_review_20260907_v1"
out <- if(length(args)>1)args[2] else "data-derived/p15_peer_history_matching_20260907_v1"
stopifnot(!dir.exists(out))
paths <- file.path(base,c("p15_country_year_dataset.csv.gz","p15_peer_options.csv.gz",
  "p15_observed_anchors.csv.gz","input_manifest.csv"))
manifest <- fread(paths[4])
context_path <- manifest$artifact_path[grepl("p15_rating_component_ledger_2012_2024.csv.gz$",manifest$artifact_path)]
stopifnot(length(context_path)==1L)
paths <- c(paths,context_path,"docs/governance/P15_HISTORY_MATCHING_PROTOCOL_2026-09-07.md","renv.lock")
hashes <- vapply(paths,digest,character(1),file=TRUE,algo="sha256")
p <- fread(paths[1]); ref <- fread(paths[2])[peer_method=="similarity_min3"]
a <- fread(paths[3])[observed_market_branch=="observed_primary" & currency=="USD" &
  is.finite(market_rate_pct) & is.finite(sovereign_spread_pct)]
c <- fread(context_path)[,.(analysis_year,iso3,moodys_rating_normalized,rating_source_region)]
c <- merge(p[,.(analysis_year,iso3,historical_income_level)],c,by=c("analysis_year","iso3"),all.x=TRUE,sort=FALSE)
stopifnot(!anyDuplicated(c[,.(analysis_year,iso3)]))
seeds <- merge(a[,.(analysis_year,iso3,market_rate_pct,source_evidence_row_id)],c,
  by=c("analysis_year","iso3"),sort=FALSE)
targets <- merge(ref,c,by=c("analysis_year","iso3"),sort=FALSE)
setorder(targets,analysis_year,iso3);setorder(seeds,analysis_year,iso3)
context <- as.data.frame(c); results <- members <- audits <- vector("list",nrow(targets))
for(i in seq_len(nrow(targets))) {
  t <- as.data.frame(targets[i]); e <- as.data.frame(seeds[analysis_year==t$analysis_year & iso3!=t$iso3])
  current <- p15_peer_similarity_pool(e,t,3L)
  baseline <- if(nrow(current$pool)>=3L) median(current$pool$market_rate_pct) else NA_real_
  stopifnot(isTRUE(all.equal(baseline,t$peer_rate_pct,tolerance=1e-12)))
  h <- p15_peer_history_pool(e,t,context)
  alt <- if(nrow(h$pool)>=3L)median(h$pool$market_rate_pct) else NA_real_
  results[[i]] <- data.table(analysis_year=t$analysis_year,iso3=t$iso3,
    historical_lmic_reporting_scope=t$historical_lmic_reporting_scope,
    last_tier_usable=t$last_tier_usable,
    ordinary_fallback_selection_permitted=t$ordinary_fallback_selection_permitted,
    observed_primary_rate_pct=t$observed_primary_rate_pct,reference_rate_pct=baseline,
    history_rate_pct=alt,reference_rule=current$rule,history_rule=h$rule,
    reference_peer_count=nrow(current$pool),history_peer_count=nrow(h$pool),
    target_history_available=h$target_history_available,history_state=h$history_state,
    reference_global=grepl("^global",current$rule),history_global=grepl("^global",h$rule))
  audits[[i]] <- as.data.table(h$audit)
  if(nrow(h$pool))members[[i]] <- data.table(analysis_year=t$analysis_year,target_iso3=t$iso3,
    peer_iso3=h$pool$iso3,peer_rate_pct=h$pool$market_rate_pct,
    source_evidence_row_id=h$pool$source_evidence_row_id,history_rule=h$rule)
}
d <- rbindlist(results); m <- rbindlist(members); audit <- rbindlist(audits)
d[,`:=`(reference_error_pp=reference_rate_pct-observed_primary_rate_pct,
  history_error_pp=history_rate_pct-observed_primary_rate_pct,
  rate_change_pp=history_rate_pct-reference_rate_pct)]
validation <- d[historical_lmic_reporting_scope & is.finite(observed_primary_rate_pct) &
  is.finite(reference_rate_pct) & is.finite(history_rate_pct)]
metrics <- function(x) data.table(n=nrow(x),reference_mean_error=mean(x$reference_error_pp),
  history_mean_error=mean(x$history_error_pp),reference_mae=mean(abs(x$reference_error_pp)),
  history_mae=mean(abs(x$history_error_pp)),reference_rmse=sqrt(mean(x$reference_error_pp^2)),
  history_rmse=sqrt(mean(x$history_error_pp^2)))
summary <- rbindlist(list(cbind(cohort="all_matched_lmic_primary",metrics(validation)),
  cbind(cohort="history_available_matched_lmic_primary",metrics(validation[target_history_available==TRUE]))))
year <- validation[,metrics(.SD),by=analysis_year]
country <- validation[,metrics(.SD),by=iso3]
deploy <- d[historical_lmic_reporting_scope & last_tier_usable]
deployment <- deploy[,.(n=.N,history_available=sum(target_history_available),
  available=sum(is.finite(history_rate_pct)),changed=sum(abs(rate_change_pp)>1e-12),
  reference_global=sum(reference_global),history_global=sum(history_global),
  mean_absolute_change=mean(abs(rate_change_pp)),max_absolute_change=max(abs(rate_change_pp)))]
independent <- m[,.(recalculated_rate=median(peer_rate_pct)),by=.(analysis_year,iso3=target_iso3)]
check <- merge(d,independent,by=c("analysis_year","iso3"))
checks <- data.table(check_id=c("all_2743_keys","current_method_reproduced_all_targets",
  "no_self_seeds","same_year_seeds","all_history_strictly_prior_three_years",
  "independent_selected_medians","reference_unchanged_without_target_history",
  "all_inputs_unchanged","blocked_not_last_tier","no_global_coverage_claim_as_validation"),
  passed=c(nrow(d)==2743&&!anyDuplicated(d[,.(analysis_year,iso3)]),TRUE,
    all(m$target_iso3!=m$peer_iso3),all(paste(m$analysis_year,m$peer_iso3)%in%paste(seeds$analysis_year,seeds$iso3)),
    all(vapply(seq_len(nrow(audit)),function(i){yrs<-as.integer(strsplit(audit$history_years[i],";",fixed=TRUE)[[1]]);all(yrs<audit$analysis_year[i]&yrs>=audit$analysis_year[i]-3)},logical(1))),
    all(abs(check$history_rate_pct-check$recalculated_rate)<1e-12),
    all(abs(d[target_history_available==FALSE]$rate_change_pp)<1e-12),
    identical(hashes,vapply(paths,digest,character(1),file=TRUE,algo="sha256")),
    !any(deploy$ordinary_fallback_selection_permitted%in%FALSE),nrow(validation)==215))
stopifnot(all(checks$passed))
dir.create(out,recursive=TRUE)
tables <- list(history_comparison=d,history_membership=m,history_pair_audit=audit,
  validation_summary=summary,validation_by_year=year,validation_by_country=country,
  validation_worst_cases=validation[order(-abs(history_error_pp))],deployment_summary=deployment,
  deployment_changes=deploy[order(-abs(rate_change_pp))],checks=checks)
bundle <- list(build_id=basename(out),schema_id="SCHEMA-P15-PEER-HISTORY-V1",
  estimator_id="EST-P15-PEER-PERSISTENCE-3Y-V1",admissibility_id="ADM-P15-PEER-HISTORY-DIAGNOSTIC-V1",
  selection_id="SEL-P15-NO-PROMOTION-V1",source_package_ids=paste(sort(unique(ref$source_package_ids)),collapse=";"))
for(n in names(tables))fwrite(tables[[n]],file.path(out,paste0(n,".csv.gz")),na="")
make_manifest <- function(p,role)do.call(pvr_manifest_rows,c(list(paths=p,artifact_role=role),bundle))
fwrite(make_manifest(paths,"unchanged_input"),file.path(out,"input_manifest.csv"))
fwrite(make_manifest(c("R/p15_peer_history_matching.R","R/p15_bounded_fallback_comparison.R",
  "R/research_governance.R","scripts/p15/build_p15_peer_history_matching.R"),"code"),file.path(out,"script_manifest.csv"))
fwrite(make_manifest(list.files(out,pattern="csv.gz$",full.names=TRUE),"diagnostic_output"),file.path(out,"output_manifest.csv"))
capture.output(sessionInfo(),file=file.path(out,"environment.txt"))
print(summary);print(deployment);print(checks)
