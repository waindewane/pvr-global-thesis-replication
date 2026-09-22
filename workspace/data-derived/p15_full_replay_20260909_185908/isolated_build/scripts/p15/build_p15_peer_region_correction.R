#!/usr/bin/env Rscript
suppressPackageStartupMessages({library(data.table);library(dplyr);library(digest)})
for(f in c("research_governance","p15_local_completion","p15_ladder_preview",
  "p15_reference_review","p15_analysis_dataset","p15_bounded_fallback_comparison",
  "p15_peer_region_correction"))source(paste0("R/",f,".R"))
args <- commandArgs(TRUE)
out <- if(length(args))args[1] else "data-derived/p15_analysis_candidate_20260909_region_v1"
stopifnot(!dir.exists(out))
base <- "data-derived/p15_analysis_candidate_20260907_v1"
registered <- fread(file.path(base,"output_manifest.csv"))
stopifnot(all(vapply(registered$artifact_path,digest,character(1),file=TRUE,algo="sha256")==registered$sha256))
prior <- fread("data-derived/p15_reference_review_20260907_v1/input_manifest.csv")
context_path <- prior$artifact_path[grepl("rating_component_ledger_2012_2024.csv.gz$",prior$artifact_path)]
stopifnot(length(context_path)==1L)
inputs <- c(registered$artifact_path,context_path,"data-raw/world_bank_countries.json","renv.lock")
hashes <- vapply(inputs,digest,character(1),file=TRUE,algo="sha256")
p <- fread(file.path(base,"core_evidence.csv")); old <- copy(p)
elig <- fread(file.path(base,"tier_eligibility.csv"))
context <- fread(context_path)[,.(analysis_year,iso3,rating_source_region,moodys_rating_normalized)]
w <- jsonlite::fromJSON("data-raw/world_bank_countries.json")[[2]]
geo <- data.frame(iso3=w$id,wb_region=trimws(w$region$value))
geo <- geo[geo$wb_region!="Aggregates",]
filled <- p15_fill_peer_regions(as.data.frame(context),geo)
oldmem <- fread(file.path(base,"peer_membership.csv"))
seedcols <- c("analysis_year","peer_iso3","peer_spread_pct")
seedmeta <- unique(oldmem[,..seedcols]);stopifnot(!anyDuplicated(seedmeta[,.(analysis_year,peer_iso3)]))
message("Reconstructing original and corrected pools")
before <- p15_recompute_reference_peers(as.data.frame(p),as.data.frame(context),as.data.frame(elig),as.data.frame(seedmeta))
after <- p15_recompute_reference_peers(as.data.frame(p),filled,as.data.frame(elig),as.data.frame(seedmeta))
same <- function(a,b)identical(is.na(a),is.na(b)) && all(abs(a-b)<1e-10,na.rm=TRUE)
stopifnot(same(p$peer_rate_pct,before$detail$peer_rate_pct),
  identical(p$peer_pool_rule,before$detail$peer_pool_rule),
  identical(p$peer_country_count,before$detail$peer_country_count))
for(n in setdiff(names(after$detail),c("analysis_year","iso3")))set(p,j=n,value=after$detail[[n]])
p[,peer_candidate_state:="owner_authorized_optional_reference_region_corrected"]
p[,peer_evidence_id:=paste0(peer_evidence_id,"::REGION-20260909")]
views <- as.data.table(p15_analysis_views(as.data.frame(p)))
views[selected_tier=="peer",selected_source_package_ids:=paste0(selected_source_package_ids,";SRC-WB-COUNTRIES-LOCAL-PEER-20260909")]
setorder(views,view_id,analysis_year,iso3)
newelig <- as.data.table(p15_analysis_eligibility(as.data.frame(p)))
ref <- views[view_id=="ids_before_secondary__with_peer"]
nopeer <- views[view_id=="ids_before_secondary__without_peer"]
previous <- fread(file.path(base,"selection_variants.csv"))
delta <- merge(previous[,.(analysis_year,iso3,view_id,old_tier=selected_tier,old_rate=selected_rate_pct,old_global=selected_global_peer)],
 views[,.(analysis_year,iso3,view_id,historical_lmic_reporting_scope,new_tier=selected_tier,new_rate=selected_rate_pct,new_global=selected_global_peer)],
 by=c("analysis_year","iso3","view_id"))
delta[,`:=`(change_pp=new_rate-old_rate,rate_changed=xor(is.na(new_rate),is.na(old_rate))|fcoalesce(abs(new_rate-old_rate)>1e-10,FALSE))]
peer_delta <- merge(as.data.table(before$detail),as.data.table(after$detail),by=c("analysis_year","iso3"),suffixes=c("_before","_after"))
peer_delta[,change_pp:=peer_rate_pct_after-peer_rate_pct_before]
om <- oldmem[,.(analysis_year,target_iso3,peer_iso3)]
nm <- as.data.table(after$membership)[,.(analysis_year,target_iso3,peer_iso3)]
membership_delta <- merge(om[,old_member:=TRUE],nm[,new_member:=TRUE],by=c("analysis_year","target_iso3","peer_iso3"),all=TRUE)
membership_delta <- membership_delta[is.na(old_member)|is.na(new_member)]
stable <- setdiff(names(old),c("peer_rate_pct","peer_minimum_met","peer_pool_rule","peer_country_count",
  "peer_iqr_pp","global_pool_used","peer_candidate_state","peer_evidence_id"))
med <- as.data.table(after$membership)[used_for_estimate==TRUE,
 .(recomputed=median(peer_rate_pct),n=.N),by=.(analysis_year,iso3=target_iso3)]
mc <- merge(p,med,by=c("analysis_year","iso3"),all.x=TRUE)
checks <- data.table(check_id=c("parent_manifest_valid","2743_unique_keys","nonpeer_core_unchanged",
 "all_selected_tiers_unchanged","no_peer_rate_views_unchanged","no_self_peers","membership_unique",
 "medians_and_counts_reproduce","no_LMIC_region_missing","original_nonempty_regions_preserved",
 "inputs_unchanged"),passed=c(TRUE,nrow(p)==2743&&!anyDuplicated(p[,.(analysis_year,iso3)]),
 identical(old[,..stable],p[,..stable]),all(delta$old_tier==delta$new_tier),
 !any(delta[old_tier!="peer",rate_changed]),!any(after$membership$peer_iso3==after$membership$target_iso3),
 !anyDuplicated(as.data.table(after$membership)[,.(analysis_year,target_iso3,peer_iso3)]),
 same(mc$peer_rate_pct,mc$recomputed)&&all(mc$peer_country_count==mc$n,na.rm=TRUE),
 all(nzchar(filled$rating_source_region[match(p[historical_lmic_reporting_scope==TRUE,paste(analysis_year,iso3)],paste(filled$analysis_year,filled$iso3))])),
 identical(filled$rating_source_region[!filled$peer_region_filled],filled$rating_source_region_before[!filled$peer_region_filled]),
 identical(hashes,vapply(inputs,digest,character(1),file=TRUE,algo="sha256"))))
stopifnot(all(checks$passed))
coverage <- views[,.(country_years=.N),by=.(view_id,historical_lmic_reporting_scope,selected_tier)]
year <- views[,.(country_years=.N),by=.(view_id,analysis_year,historical_lmic_reporting_scope,selected_tier)]
contrast <- merge(ref[,.(analysis_year,iso3,historical_lmic_reporting_scope,reference_tier=selected_tier,reference_rate_pct=selected_rate_pct)],
 views[view_id=="secondary_before_ids__with_peer",.(analysis_year,iso3,comparison_tier=selected_tier,comparison_rate_pct=selected_rate_pct)],by=c("analysis_year","iso3"))
contrast <- contrast[reference_tier!=comparison_tier|abs(reference_rate_pct-comparison_rate_pct)>1e-12]
bundle <- list(build_id=basename(out),schema_id="SCHEMA-P15-ANALYSIS-V1",estimator_id="EST-P15-CLOSEST-YEAR-END-V1",
 admissibility_id="ADM-P15-SOURCE-CLOSURE-V1",selection_id="SEL-P15-REFERENCE-REGION-CORRECTED-V1",
 source_package_ids="SRC-P15-RAW-CLOSURE-20260906;SRC-P15-SOURCE-CLOSURE-20260907;SRC-WB-COUNTRIES-LOCAL-PEER-20260909")
tag <- function(d){d<-as.data.table(copy(d));for(n in names(bundle))set(d,j=n,value=bundle[[n]]);d}
tables <- list(core_evidence=p,selected_reference=ref,selected_without_peers=nopeer,selection_variants=views,
 tier_eligibility=newelig,coverage_by_tier=coverage,coverage_by_year=year,source_order_differences=contrast,
 source_case_dispositions=fread(file.path(base,"source_case_dispositions.csv")),peer_membership=after$membership,
 readiness_checks=checks,variable_dictionary=fread(file.path(base,"variable_dictionary.csv")),
 peer_region_context=filled,selection_changes=delta,peer_changes=peer_delta,membership_changes=membership_delta)
dir.create(out,recursive=TRUE)
for(n in names(tables))fwrite(tag(tables[[n]]),file.path(out,paste0(n,".csv")),na="")
manifest <- function(x,role)do.call(pvr_manifest_rows,c(list(paths=x,artifact_role=role),bundle))
fwrite(manifest(inputs,"input"),file.path(out,"input_manifest.csv"))
code <- c("R/p15_peer_region_correction.R","R/p15_bounded_fallback_comparison.R","R/p15_analysis_dataset.R",
 "R/p15_ladder_preview.R","R/p15_reference_review.R","R/p15_local_completion.R","R/research_governance.R",
 "scripts/p15/build_p15_peer_region_correction.R")
fwrite(manifest(code,"code"),file.path(out,"script_manifest.csv"))
fwrite(manifest(file.path(out,paste0(names(tables),".csv")),"candidate_output"),file.path(out,"output_manifest.csv"))
capture.output(sessionInfo(),file=file.path(out,"environment.txt"))
print(delta[view_id=="ids_before_secondary__with_peer"&historical_lmic_reporting_scope,
 .(n=.N,changed=sum(rate_changed),old_global=sum(old_global),new_global=sum(new_global),
 mean_absolute_change=mean(abs(change_pp),na.rm=TRUE)),by=old_tier]);print(checks)
