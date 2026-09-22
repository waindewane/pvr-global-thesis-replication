#!/usr/bin/env Rscript
# Read-only consequence analysis of the reviewed evidence and existing previews.
# Does not select a production ladder or change estimators/admissibility.
suppressPackageStartupMessages({library(data.table); library(digest)})
root <- "data-derived/p15_full_replay_20260906_112727"
base <- file.path(root,"isolated_build/data-derived/p15_reviewed_evidence_20260906_v1")
build_id <- paste0("p15_ladder_decision_brief_",format(Sys.time(),"%Y%m%d_%H%M%S",tz="UTC"))
out <- file.path("data-derived",build_id)
stopifnot(!dir.exists(out)); dir.create(out)
script <- "scripts/p15/build_ladder_decision_brief.R"
sha <- function(p) digest(p,algo="sha256",file=TRUE)
paths <- file.path(base,c("p15_country_year_dataset.csv","p15_reviewed_ladder_previews.csv",
  "p15_peer_target_detail.csv","p15_peer_membership.csv","p15_rating_validation_summary.csv"))
input_hashes <- vapply(paths,sha,character(1))
prior <- fread(file.path(root,"output_manifest.csv"))
registered <- prior[match(sub(paste0(root,"/"),"",paths,fixed=TRUE),artifact_path)]
stopifnot(nrow(registered)==length(paths),!anyNA(registered$sha256),
  identical(unname(input_hashes),registered$sha256))
p <- fread(paths[1]); x <- fread(paths[2]); peer <- fread(paths[3]); membership <- fread(paths[4])
stopifnot(nrow(p)==2743L,!anyDuplicated(p[,.(analysis_year,iso3)]),
  !any(p$selected_for_ladder|p$canonical_benchmark),!any(x$approved_selection),
  nrow(x)==4L*nrow(p),!anyDuplicated(x[,.(analysis_year,iso3,preview_variant)]))
p[,primary_either_currency_coverage:=fcoalesce(primary_usd_market_rate_pct,primary_eur_market_rate_pct)]
schema <- "SCHEMA-P15-LADDER-DECISION-BRIEF-V1"
bundle <- list(build_id=build_id,schema_id=schema,estimator_id="EST-P15-PRESERVED-FORMULAS-AND-CASE-REVIEW-V1",
  admissibility_id="ADM-P15-CASE-REVIEW-20260906-V1",selection_id="SEL-NONE-DECISION-BRIEF-V1",
  source_package_ids="SRC-P15-RAW-CLOSURE-20260906",release_id="not_applicable")
emit <- function(t,name) {
  t <- copy(as.data.table(t))
  for(n in names(bundle)) set(t,j=n,value=bundle[[n]])
  fwrite(t,file.path(out,paste0(name,".csv")))
}
scopes <- list(all_countries=rep(TRUE,nrow(p)),historical_lmic=p$historical_lmic_reporting_scope)
fields <- c(primary_usd="primary_usd_market_rate_pct",primary_eur="primary_eur_market_rate_pct",
  primary_usd_or_eur="primary_either_currency_coverage",
  secondary_usd="secondary_usd_market_rate_pct",secondary_eur="secondary_eur_market_rate_pct",
  ids_positive_source="ids_rate_pct",ids_reviewed_candidate="ids_reviewed_proxy_rate_pct",
  moodys="rating_moodys_rate_pct",fitch="rating_fitch_rate_pct",
  moodys_precedence="rating_precedence_rate_pct",agency_median="rating_agency_median_rate_pct",peer="peer_rate_pct")
coverage <- rbindlist(lapply(names(scopes),function(scope) rbindlist(lapply(names(fields),function(tier) {
  q <- p[scopes[[scope]]]
  available <- is.finite(q[[fields[[tier]]]])
  if(tier=="ids_positive_source") available <- available & q$ids_rate_pct>0
  data.table(scope=scope,tier=tier,country_years=sum(available),countries=uniqueN(q$iso3[available]),
    denominator=nrow(q),meaning="evidence availability before status-based selection; not additive")
}))))
emit(coverage,"source_coverage")
counts <- rbindlist(lapply(c("all_countries","historical_lmic"),function(scope) {
  q <- if(scope=="historical_lmic")x[historical_lmic_reporting_scope==TRUE] else copy(x)
  q[,.(country_years=.N,countries=uniqueN(iso3)),by=.(preview_variant,preview_source)][,scope:=scope]
}))
emit(counts,"selection_counts")
emit(x[historical_lmic_reporting_scope==TRUE,.(country_years=.N),
  by=.(analysis_year,preview_variant,preview_source)],"selection_counts_by_year_lmic")
a <- x[preview_variant=="ids_before_secondary__ids_case_review_applied__peer_not_selected"]
b <- x[preview_variant=="secondary_before_ids__ids_case_review_applied__peer_not_selected"]
joined <- merge(a,b,by=c("analysis_year","iso3"),suffixes=c("_ids_first","_secondary_first"))
stopifnot(nrow(joined)==nrow(p),
  identical(is.finite(joined$preview_rate_pct_ids_first),is.finite(joined$preview_rate_pct_secondary_first)))
changed <- joined[preview_source_ids_first!=preview_source_secondary_first,
  .(analysis_year,iso3,country=country_ids_first,historical_lmic_reporting_scope=historical_lmic_reporting_scope_ids_first,
    ids_rate_pct=preview_rate_pct_ids_first,secondary_rate_pct=preview_rate_pct_secondary_first,
    gap_ids_minus_secondary_pp=preview_rate_pct_ids_first-preview_rate_pct_secondary_first,
    ids_parent=parent_evidence_id_ids_first,secondary_parent=parent_evidence_id_secondary_first)]
changed <- changed[order(-abs(gap_ids_minus_secondary_pp))]
emit(changed,"ids_secondary_changed_cases")
emit(rbindlist(lapply(c("all_countries","historical_lmic"),function(scope) {
  q <- if(scope=="historical_lmic")changed[historical_lmic_reporting_scope==TRUE] else changed
  data.table(scope=scope,country_years=nrow(q),countries=uniqueN(q$iso3),
    mean_signed_gap_pp=mean(q$gap_ids_minus_secondary_pp),
    mean_absolute_gap_pp=mean(abs(q$gap_ids_minus_secondary_pp)),
    median_absolute_gap_pp=median(abs(q$gap_ids_minus_secondary_pp)),
    max_absolute_gap_pp=max(abs(q$gap_ids_minus_secondary_pp)),
    above_1pp=sum(abs(q$gap_ids_minus_secondary_pp)>1),coverage_change=0L)
})),"ids_secondary_change_summary")
full <- x[preview_variant=="ids_before_secondary__ids_case_review_applied__broad_peer_included"]
selected_peer <- merge(full[preview_source=="peer",.(analysis_year,iso3,country,
  historical_lmic_reporting_scope,preview_rate_pct,parent_evidence_id,status_review_coverage)],
  peer[,.(analysis_year,iso3,peer_pool_rule,peer_country_count,target_moodys_missing,
    rating_proximity_used,global_pool_used,peer_iqr_pp,peer_min_rate_pct,peer_max_rate_pct,thin_primary_peer_share)],
  by=c("analysis_year","iso3"))
emit(selected_peer,"peer_incremental_cases")
emit(selected_peer[historical_lmic_reporting_scope==TRUE,
  .(country_years=.N,countries=uniqueN(iso3),median_pool_size=as.numeric(median(peer_country_count)),
    median_pool_iqr_pp=median(peer_iqr_pp)),by=.(peer_pool_rule)],"peer_incremental_pool_summary_lmic")
peer_summary <- selected_peer[historical_lmic_reporting_scope==TRUE,.(country_years=.N,
  countries=uniqueN(iso3),target_moodys_missing=sum(target_moodys_missing),
  rating_proximity_used=sum(rating_proximity_used),global_pool_used=sum(global_pool_used),
  no_status_case_record=sum(status_review_coverage=="no_case_review_record_not_proof_of_no_stress"))]
emit(peer_summary,"peer_incremental_summary_lmic")
emit(full[preview_source=="no_eligible_rate"],"remaining_missing_full_preview")
overlap <- p[historical_lmic_reporting_scope==TRUE & is.finite(primary_usd_market_rate_pct) &
  is.finite(ids_reviewed_proxy_rate_pct)]
emit(overlap[,.(n=.N,countries=uniqueN(iso3),correlation=cor(primary_usd_market_rate_pct,ids_reviewed_proxy_rate_pct),
  mean_ids_minus_primary_pp=mean(ids_reviewed_proxy_rate_pct-primary_usd_market_rate_pct),
  mean_absolute_gap_pp=mean(abs(ids_reviewed_proxy_rate_pct-primary_usd_market_rate_pct)))],"ids_primary_overlap_lmic")
emit(fread(paths[5]),"rating_validation_summary")
# A simple independent first-eligible implementation must reproduce all 4 previews.
independent_ok <- TRUE
for(v in unique(x$preview_variant)) {
  q <- copy(p); setorder(q,analysis_year,iso3)
  z <- copy(x[preview_variant==v]);setorder(z,analysis_year,iso3)
  order <- if(startsWith(v,"ids_before"))c("primary","ids","secondary","moodys","peer") else
    c("primary","secondary","ids","moodys","peer")
  values <- list(primary=q$primary_usd_market_rate_pct,ids=q$ids_reviewed_proxy_rate_pct,
    secondary=q$secondary_usd_market_rate_pct,moodys=q$rating_moodys_rate_pct,peer=q$peer_rate_pct)
  chosen <- rep("no_eligible_rate",nrow(q)); rate <- rep(NA_real_,nrow(q))
  for(tier in order) {
    ok <- is.finite(values[[tier]]) & !(if(tier %in% c("primary","secondary"))
      q$observed_benchmark_selection_permitted else q$ordinary_fallback_selection_permitted) %in% FALSE
    if(tier=="moodys")ok <- ok & q$rating_moodys_available %in% TRUE
    if(tier=="peer")ok <- ok & q$peer_minimum_met %in% TRUE & grepl("broad_peer_included",v)
    take <- chosen=="no_eligible_rate" & ok
    chosen[take] <- tier;rate[take] <- values[[tier]][take]
  }
  independent_ok <- independent_ok && identical(chosen,z$preview_source) &&
    isTRUE(all.equal(rate,z$preview_rate_pct,tolerance=0))
}
stopifnot(independent_ok)
checks <- data.table(check=c("complete unique grid and four unapproved previews",
  "parent product hashes match preserved replay manifest","independent selector reproduces all four previews",
  "IDS-secondary order does not change coverage","core inputs unchanged"),passed=c(TRUE,TRUE,
    independent_ok,TRUE,identical(input_hashes,vapply(paths,sha,character(1)))))
emit(checks,"verification")
manifest <- function(files,role) data.table(artifact_path=files,artifact_role=role,
  bytes=file.info(files)$size,sha256=vapply(files,sha,character(1)),build_id=build_id,
  schema_id=schema,estimator_id=bundle$estimator_id,admissibility_id=bundle$admissibility_id,
  selection_id=bundle$selection_id,source_package_ids=bundle$source_package_ids,
  parent_build_id=basename(root),producing_script=script,producing_script_sha256=sha(script))
fwrite(manifest(c(paths,file.path(root,"output_manifest.csv")),"preserved_reviewed_input"),file.path(out,"input_manifest.csv"))
fwrite(manifest(script,"script"),file.path(out,"script_manifest.csv"))
fwrite(data.table(build_id=build_id,r_version=R.version.string,platform=R.version$platform,
  locale=Sys.getlocale(),timezone=Sys.timezone(),renv_lock_sha256=sha("renv.lock"),
  data_table_version=as.character(packageVersion("data.table")),digest_version=as.character(packageVersion("digest"))),
  file.path(out,"environment_manifest.csv"))
fwrite(manifest(list.files(out,full.names=TRUE),"decision_diagnostic_output"),file.path(out,"output_manifest.csv"))
cat("Output:",out,"\n");print(coverage[scope=="historical_lmic"])
print(fread(file.path(out,"ids_secondary_change_summary.csv"))[,1:9]);print(peer_summary)
print(head(changed,12));print(checks)
