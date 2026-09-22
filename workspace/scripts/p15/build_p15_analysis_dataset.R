#!/usr/bin/env Rscript
suppressPackageStartupMessages({library(data.table);library(dplyr);library(digest)})
source("R/research_governance.R");source("R/p15_local_completion.R")
source("R/p15_ladder_preview.R");source("R/p15_reference_review.R");source("R/p15_analysis_dataset.R")
args <- commandArgs(trailingOnly=TRUE)
out <- if(length(args))args[1] else "data-derived/p15_analysis_candidate_20260907_v1"
base <- "data-derived/p15_source_closure_20260907_v1"
stopifnot(!dir.exists(out))
inputs <- c(file.path(base,c("p15_country_year_dataset.csv.gz","p15_ladder_previews.csv.gz","source_case_dispositions.csv.gz")),
  "data-derived/p15_reference_review_20260907_v1/p15_peer_options.csv.gz",
  "data-derived/p15_reference_review_20260907_v1/p15_peer_option_membership.csv.gz","renv.lock")
hashes <- vapply(inputs,digest,character(1),file=TRUE,algo="sha256")
p <- fread(inputs[1]);setorder(p,analysis_year,iso3)
views <- as.data.table(p15_analysis_views(as.data.frame(p)));setorder(views,view_id,analysis_year,iso3)
elig <- as.data.table(p15_analysis_eligibility(as.data.frame(p)))
ref <- views[view_id=="ids_before_secondary__with_peer"]
nopeer <- views[view_id=="ids_before_secondary__without_peer"]
options <- fread(inputs[4])[peer_method=="similarity_min3"]
peercheck <- merge(p[,.(analysis_year,iso3,peer_rate_pct)],options[,.(analysis_year,iso3,peer_rate_pct)],by=c("analysis_year","iso3"))
audit <- merge(views[,.(analysis_year,iso3,view_id,selected_tier,selected_rate_pct)],elig,by=c("analysis_year","iso3"),allow.cartesian=TRUE)
audit[,rank:=match(tier,c("primary","ids","secondary","moodys","peer"))]
audit[grepl("^secondary_before",view_id),rank:=match(tier,c("primary","secondary","ids","moodys","peer"))]
audit[grepl("without_peer$",view_id)&tier=="peer",eligible:=FALSE]
setorder(audit,view_id,analysis_year,iso3,rank)
independent <- audit[,.(expected_tier=if(any(eligible))tier[which(eligible)[1]] else "no_eligible_rate",
  expected_rate=if(any(eligible))rate_pct[which(eligible)[1]] else NA_real_),by=.(view_id,analysis_year,iso3)]
cmp <- merge(views,independent,by=c("view_id","analysis_year","iso3"))
checks <- data.table(check_id=c("2743_unique_keys","four_complete_views","1756_historical_LMIC_keys",
  "independent_precedence","independent_values","selected_parent_and_source_present","no_peer_in_excluded_view",
  "serbia_hold_applied","raw_IDS_serbia_retained","accepted_peer_formula_exact","no_release_promotion","input_hashes_unchanged"),
  passed=c(nrow(p)==2743&&!anyDuplicated(p[,.(analysis_year,iso3)]),nrow(views)==4*2743&&!anyDuplicated(views[,.(view_id,analysis_year,iso3)]),
    sum(p$historical_lmic_reporting_scope)==1756,all(cmp$selected_tier==cmp$expected_tier),
    identical(is.na(cmp$selected_rate_pct),is.na(cmp$expected_rate))&&all(abs(cmp$selected_rate_pct-cmp$expected_rate)<1e-12,na.rm=TRUE),
    all(!is.na(views[is.finite(selected_rate_pct)]$parent_evidence_id)&nzchar(views[is.finite(selected_rate_pct)]$parent_evidence_id))&&
      all(!is.na(views[is.finite(selected_rate_pct)]$selected_source_package_ids)&nzchar(views[is.finite(selected_rate_pct)]$selected_source_package_ids)),
    !any(nopeer$selected_tier=="peer"),all(views[iso3=="SRB"&analysis_year==2022]$selected_tier=="secondary"),
    abs(p[iso3=="SRB"&analysis_year==2022]$ids_rate_pct-3.8029)<1e-12,
    all(abs(peercheck$peer_rate_pct.x-peercheck$peer_rate_pct.y)<1e-12,na.rm=TRUE)&&identical(is.na(peercheck$peer_rate_pct.x),is.na(peercheck$peer_rate_pct.y)),
    all(views$release_state=="candidate_for_owner_review_not_canonical"),identical(hashes,vapply(inputs,digest,character(1),file=TRUE,algo="sha256"))))
stopifnot(all(checks$passed))
coverage <- views[,.(country_years=.N),by=.(view_id,historical_lmic_reporting_scope,selected_tier)]
year <- views[,.(country_years=.N),by=.(view_id,analysis_year,historical_lmic_reporting_scope,selected_tier)]
contrast <- merge(ref[,.(analysis_year,iso3,historical_lmic_reporting_scope,reference_tier=selected_tier,reference_rate_pct=selected_rate_pct)],
  views[view_id=="secondary_before_ids__with_peer",.(analysis_year,iso3,comparison_tier=selected_tier,comparison_rate_pct=selected_rate_pct)],by=c("analysis_year","iso3"))
contrast <- contrast[reference_tier!=comparison_tier|abs(reference_rate_pct-comparison_rate_pct)>1e-12]
bundle <- list(build_id=basename(out),schema_id="SCHEMA-P15-ANALYSIS-V1",estimator_id="EST-P15-CLOSEST-YEAR-END-V1",
  admissibility_id="ADM-P15-SOURCE-CLOSURE-V1",selection_id="SEL-P15-REFERENCE-VIEWS-V1",source_package_ids="SRC-P15-RAW-CLOSURE-20260906;SRC-P15-SOURCE-CLOSURE-20260907")
tag <- function(d){d<-copy(as.data.table(d));for(n in names(bundle))set(d,j=n,value=bundle[[n]]);d}
tables <- list(core_evidence=p,selected_reference=ref,selected_without_peers=nopeer,selection_variants=views,
  tier_eligibility=elig,coverage_by_tier=coverage,coverage_by_year=year,source_order_differences=contrast,
  source_case_dispositions=fread(inputs[3]),peer_membership=fread(inputs[5])[peer_method=="similarity_min3"],readiness_checks=checks)
dictionary <- rbindlist(lapply(c("core_evidence","selection_variants","tier_eligibility"),function(n){
  d<-tables[[n]];data.table(table=n,variable=names(d),type=vapply(d,function(x)class(x)[1],character(1)),
    units=ifelse(grepl("pct$|_pp$",names(d)),"percentage_points",ifelse(grepl("years$",names(d)),"years","see_variable_guide")),
    label=gsub("_"," ",names(d)))}))
tables$variable_dictionary <- dictionary
dir.create(out,recursive=TRUE)
for(n in names(tables))fwrite(tag(tables[[n]]),file.path(out,paste0(n,".csv")),na="")
manifest <- function(x,role)do.call(pvr_manifest_rows,c(list(paths=x,artifact_role=role),bundle))
fwrite(manifest(inputs,"input"),file.path(out,"input_manifest.csv"))
fwrite(manifest(c("R/p15_analysis_dataset.R","R/p15_ladder_preview.R","R/p15_reference_review.R","R/p15_local_completion.R","R/research_governance.R","scripts/p15/build_p15_analysis_dataset.R"),"code"),file.path(out,"script_manifest.csv"))
fwrite(manifest(file.path(out,paste0(names(tables),".csv")),"candidate_output"),file.path(out,"output_manifest.csv"))
capture.output(sessionInfo(),file=file.path(out,"environment.txt"))
print(coverage[view_id=="ids_before_secondary__with_peer" & historical_lmic_reporting_scope]);print(checks)
