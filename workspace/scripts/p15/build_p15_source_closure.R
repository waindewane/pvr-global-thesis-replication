#!/usr/bin/env Rscript
suppressPackageStartupMessages({library(data.table);library(digest);library(dplyr)})
source("R/research_governance.R")
source("R/p15_local_completion.R")
source("R/p15_ladder_preview.R")
source("R/p15_reference_review.R")
args <- commandArgs(trailingOnly=TRUE)
base <- "data-derived/p15_reference_review_20260907_v1"
out <- if(length(args))args[1] else "data-derived/p15_source_closure_20260907_v1"
stopifnot(!dir.exists(out))
raw <- "data-raw/p15_source_closure_20260907"
extracts <- list.files("data-derived/p15_source_closure_extract_20260907_v1",pattern="csv$",full.names=TRUE)
cells <- rbindlist(lapply(extracts,fread)); stopifnot(nrow(cells)==68L,all(cells$counterpart=="BND"))
inputs <- c(file.path(base,c("p15_country_year_dataset.csv.gz","p15_ids_secondary_cases.csv.gz",
  "p15_ladder_previews.csv.gz")),extracts,unique(cells$source_file),
  file.path(raw,c("serbia_2022_placements.csv","evidence_additions.csv")),"renv.lock")
hashes <- vapply(inputs,digest,character(1),file=TRUE,algo="sha256")
p <- fread(inputs[1]); cases <- fread(inputs[2]); before <- fread(inputs[3])
add <- fread(file.path(raw,"evidence_additions.csv")); bonds <- fread(file.path(raw,"serbia_2022_placements.csv"))
stopifnot(nrow(cases)==30L,!anyDuplicated(cases[,.(analysis_year,iso3)]),
  !anyDuplicated(cells[,.(analysis_year,iso3,series)]))
wide <- dcast(cells,iso3+analysis_year~series,value.var="value")
names(wide)[3:6] <- c("archived_commitment_usd","archived_grace_years","archived_rate_pct","archived_maturity_years")
stopifnot(identical(sort(unique(cells$series)),c("DT.COM.DPPG.CD","DT.GPA.DPPG","DT.INR.DPPG","DT.MAT.DPPG")))
review <- merge(cases[,.(analysis_year,iso3,ids_rate_pct,ids_maturity_years,ids_grace_years,
  secondary_usd_market_rate_pct,ids_benchmark_proxy_candidate_permitted,explanation_strength,source_url,review_note)],
  wide,by=c("analysis_year","iso3"),all.x=TRUE,sort=FALSE)
review <- merge(review,add,by=c("analysis_year","iso3"),all.x=TRUE,suffixes=c("_previous","_new"),sort=FALSE)
review[, archived_terms_checked := is.finite(archived_rate_pct)]
review[, current_use := "retain_labelled_contractual_proxy_not_verified_matched_USD_yield"]
review[, closure_state := "current_treatment_closed_exact_gap_attribution_not_established"]
review[, forward_ids_proxy_permitted := ids_benchmark_proxy_candidate_permitted]
review[is.na(interpretation),interpretation:=review_note]
review[is.na(evidence_strength),evidence_strength:=explanation_strength]
review[is.na(remaining_evidence),remaining_evidence:=paste0(
  "Commitment-level borrower currency fixed-or-floating terms and dates matched to secondary securities are unavailable. ",
  "Existing case-specific distinction: ",review_note)]
review[, reopening_trigger := "Specific instrument-level or provider evidence that changes rate interpretation; unrelated completion work is not expected to resolve attribution."]
review[, decision_id := "IDS-SOURCE-USE-CLOSURE-20260907"]
review[iso3=="SRB" & analysis_year==2022,`:=`(forward_ids_proxy_permitted=FALSE,
  current_use="hold_IDS_from_all_in_benchmark_preserve_as_suspected_floating_margin",
  closure_state="current_treatment_closed_strong_margin_inference",
  decision_id="IDS-SRB2022-MARGIN-20260907")]
review[iso3=="PAK" & analysis_year==2022,current_use:="retain_USD_issuance_supported_proxy_not_exact_timing_decomposition"]
review[iso3=="PER" & analysis_year==2016,current_use:="retain_contractual_proxy_EUR_evidence_not_verified_USD_equivalent"]
s <- review[iso3=="SRB" & analysis_year==2022]
reconciliation <- data.table(quantity=c("interest_pct_or_margin_pp","original_maturity_years"),
  ids_value=c(s$ids_rate_pct,s$ids_maturity_years),
  reconstructed=c(weighted.mean(bonds$margin_pp,bonds$amount_eur),weighted.mean(bonds$maturity_years,bonds$amount_eur)))
reconciliation[,matches_to_published_precision:=abs(ids_value-reconstructed)<0.00005]
stopifnot(all(reconciliation$matches_to_published_precision))
forward <- copy(p)
i <- which(forward$iso3=="SRB" & forward$analysis_year==2022);stopifnot(length(i)==1L)
forward[i,`:=`(ids_benchmark_proxy_candidate_permitted=FALSE,ids_reviewed_proxy_rate_pct=NA_real_)]
forward[, source_closure_decision_id:=NA_character_]
forward[, source_closure_current_use:=NA_character_]
forward[review,on=.(analysis_year,iso3),`:=`(source_closure_decision_id=i.decision_id,source_closure_current_use=i.current_use)]
after <- as.data.table(p15_review_previews(as.data.frame(forward)))
changes <- merge(before[,.(analysis_year,iso3,preview_variant,old_source=preview_source,old_rate_pct=preview_rate_pct)],
  after[,.(analysis_year,iso3,preview_variant,new_source=preview_source,new_rate_pct=preview_rate_pct)],
  by=c("analysis_year","iso3","preview_variant"))
changes <- changes[old_source!=new_source | abs(old_rate_pct-new_rate_pct)>1e-12]
unchanged <- setdiff(names(p),c("ids_benchmark_proxy_candidate_permitted","ids_reviewed_proxy_rate_pct","build_id","admissibility_id","schema_id"))
checks <- data.table(check_id=c("all30_dispositions","17_archived_terms_match","two_serbia_dimensions_match",
  "one_permission_change","all_source_values_unchanged","only_serbia_ids_first_previews_change","all_inputs_unchanged"),
  passed=c(nrow(review)==30L&&!anyNA(review$current_use),
    sum(review$archived_terms_checked)==17L && all(abs(review$archived_rate_pct-review$ids_rate_pct)<1e-10,na.rm=TRUE) &&
    all(abs(review$archived_maturity_years-review$ids_maturity_years)<1e-10,na.rm=TRUE) &&
    all(abs(review$archived_grace_years-review$ids_grace_years)<1e-10,na.rm=TRUE),
    all(reconciliation$matches_to_published_precision),sum(p$ids_benchmark_proxy_candidate_permitted!=forward$ids_benchmark_proxy_candidate_permitted,na.rm=TRUE)==1L,
    identical(p[,..unchanged],forward[,..unchanged]),nrow(changes)==2L&&all(changes$iso3=="SRB" & changes$analysis_year==2022 & changes$old_source=="ids" & changes$new_source=="secondary"),
    identical(hashes,vapply(inputs,digest,character(1),file=TRUE,algo="sha256"))))
stopifnot(all(checks$passed))
bundle <- list(build_id=basename(out),schema_id="SCHEMA-P15-SOURCE-CLOSURE-V1",
  estimator_id="EST-P15-CLOSEST-YEAR-END-V1",admissibility_id="ADM-P15-SOURCE-CLOSURE-V1",
  selection_id="SEL-NONE-EVIDENCE-ASSEMBLY-V1",source_package_ids="SRC-P15-SOURCE-CLOSURE-20260907")
forward[,`:=`(build_id=bundle$build_id,schema_id=bundle$schema_id,admissibility_id=bundle$admissibility_id)]
dir.create(out,recursive=TRUE)
tables <- list(source_case_dispositions=review,archived_source_cells=cells,serbia_reconciliation=reconciliation,
  p15_country_year_dataset=forward,p15_ladder_previews=after,selection_changes=changes,checks=checks)
for(n in names(tables))fwrite(tables[[n]],file.path(out,paste0(n,".csv.gz")),na="")
manifest <- function(x,role)do.call(pvr_manifest_rows,c(list(paths=x,artifact_role=role),bundle))
fwrite(manifest(inputs,"unchanged_input"),file.path(out,"input_manifest.csv"))
fwrite(manifest(c("scripts/p15/build_p15_source_closure.R","scripts/p15/extract_p15_source_closure.py",
  "R/research_governance.R","R/p15_local_completion.R","R/p15_reference_review.R","R/p15_ladder_preview.R"),"code"),file.path(out,"script_manifest.csv"))
fwrite(manifest(list.files(out,pattern="csv.gz$",full.names=TRUE),"candidate_output"),file.path(out,"output_manifest.csv"))
capture.output(sessionInfo(),file=file.path(out,"environment.txt"))
print(reconciliation);print(changes);print(checks)
