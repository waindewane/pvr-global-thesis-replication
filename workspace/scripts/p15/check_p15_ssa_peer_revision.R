#!/usr/bin/env Rscript
# Read-only audit of existing candidate data; writes a separate diagnostic bundle.
suppressPackageStartupMessages(library(data.table))
source("R/research_governance.R")
args <- commandArgs(TRUE)
out <- if(length(args))args[1] else "data-derived/p15_ssa_peer_revision_20260909_v1"
stopifnot(!dir.exists(out))
old_dir <- "data-derived/p15_regional_assessment_20260908_v2"
new_dir <- "data-derived/p15_region_correction_research_20260909_v2/regional"
base <- "data-derived/p15_analysis_candidate_20260909_region_v1"
inputs <- c(file.path(c(old_dir,new_dir),"yoy_details.csv"),
  file.path(c(old_dir,new_dir),"regional_yoy.csv"),
  file.path(base,c("peer_changes.csv","peer_membership.csv","core_evidence.csv",
    "tier_eligibility.csv","peer_region_context.csv")))
hashes <- vapply(inputs,pvr_sha256_file,character(1))
o <- fread(inputs[1]); n <- fread(inputs[2])
slice <- function(x) x[scope=="historical_LMIC" & region=="Sub-Saharan Africa" &
                        analysis_year==2024]
o <- slice(o); n <- slice(n)
old <- o[view=="selected"]; new <- n[view=="selected"]
stopifnot(!anyDuplicated(old$iso3),!anyDuplicated(new$iso3),setequal(old$iso3,new$iso3))
x <- merge(old,new,by="iso3",suffixes=c("_old","_new"))
x[,revision_pp:=change_new-change_old]
x[,contribution_to_revision_pp:=revision_pp/.N]
p <- fread(file.path(base,"core_evidence.csv"))
x[,country:=p$country[match(iso3,p$iso3)]]
peer <- fread(file.path(base,"peer_changes.csv"))
membership <- fread(file.path(base,"peer_membership.csv"))
context <- fread(file.path(base,"peer_region_context.csv"))
elig <- fread(file.path(base,"tier_eligibility.csv"))
changed <- x[abs(revision_pp)>1e-10]
pool_detail <- merge(peer[analysis_year>=2023 & iso3 %in% changed$iso3],
  context[,.(analysis_year,iso3,rating_source_region_before,rating_source_region,peer_region_filled)],
  by=c("analysis_year","iso3"))
groups <- changed[,.(n=.N,countries=paste(country,collapse="; "),
  old_2023=prior_rate_old[1],new_2023=prior_rate_new[1],old_2024=rate_old[1],new_2024=rate_new[1],
  old_change=change_old[1],new_change=change_new[1],
  contribution_to_revision_pp=sum(contribution_to_revision_pp)),by=.(revision_pp)]
views <- n[,.(countries=.N,mean_change_pp=mean(change),median_change_pp=median(change)),by=view]
decomp <- x[,.(countries=.N,old_mean_change=mean(change_old),new_mean_change=mean(change_new),
  old_contribution=sum(change_old)/nrow(x),new_contribution=sum(change_new)/nrow(x)),
  by=.(peer_in_either_year=prior_source_new=="peer"|source_new=="peer")]
seeds <- merge(elig[tier=="primary" & eligible & analysis_year>=2023],
  context[,.(analysis_year,iso3,rating_source_region)],by=c("analysis_year","iso3"))
seeds <- merge(seeds[rating_source_region=="Sub-Saharan Africa"],
  p[,.(analysis_year,iso3,country,primary_usd_retained_quantitative_issue_count,
       primary_usd_first_rate_date,primary_usd_last_rate_date)],by=c("analysis_year","iso3"))
members <- membership[analysis_year>=2023 & target_iso3 %in% c("BDI","COM")]
# Removing a recipient is not the same as removing a shared seed. Keep both explicit.
recipient_loo <- x[,.(iso3,country,mean_change_without_country=(sum(x$change_new)-change_new)/(nrow(x)-1))]
seed_loo <- members[analysis_year==2024,{
  data.table(omitted_seed=peer_iso3,remaining_pool_n=.N-1L,
    estimate_without_seed=vapply(seq_len(.N),function(i)median(peer_rate_pct[-i]),numeric(1)))
},by=target_iso3]
# Independent sum checks, not just equality between saved summary files.
checks <- data.table(check=c("same_44_countries","same_sources_both_versions",
  "no_revision_to_2023_levels","15_changed_all_peer","revision_equals_country_contributions",
  "saved_old_mean","saved_new_mean","no_2023_ssa_primary_seeds","seven_2024_ssa_primary_seeds",
  "unchanged_nonpeer_rates","representative_pool_medians","inputs_unchanged"),
  passed=c(nrow(x)==44,
    identical(x$prior_source_old,x$prior_source_new)&&identical(x$source_old,x$source_new),
    all(abs(x$prior_rate_new-x$prior_rate_old)<1e-10),
    nrow(changed)==15&&all(changed$source_new=="peer"&changed$prior_source_new=="peer"),
    abs(mean(x$change_new)-mean(x$change_old)-sum(x$contribution_to_revision_pp))<1e-10,
    abs(mean(x$change_old)-fread(inputs[3])[scope=="historical_LMIC"&region=="Sub-Saharan Africa"&view=="selected"&analysis_year==2024,mean_change])<1e-10,
    abs(mean(x$change_new)-fread(inputs[4])[scope=="historical_LMIC"&region=="Sub-Saharan Africa"&view=="selected"&analysis_year==2024,mean_change])<1e-10,
    nrow(seeds[analysis_year==2023])==0,nrow(seeds[analysis_year==2024])==7,
    all(abs(x[source_new!="peer",revision_pp])<1e-10),
    all(abs(members[,.(recalculated=median(peer_rate_pct)),by=.(analysis_year,target_iso3)]$recalculated-
      peer[match(paste(members[,.(recalculated=median(peer_rate_pct)),by=.(analysis_year,target_iso3)]$analysis_year,
        members[,.(recalculated=median(peer_rate_pct)),by=.(analysis_year,target_iso3)]$target_iso3),paste(analysis_year,iso3)),peer_rate_pct_after])<1e-10),
    identical(hashes,vapply(inputs,pvr_sha256_file,character(1)))))
stopifnot(all(checks$passed))
bundle <- list(build_id=basename(out),schema_id="SCHEMA-P15-SSA-REVISION-AUDIT-V1",
  estimator_id="EST-P15-CLOSEST-YEAR-END-V1",admissibility_id="ADM-P15-SOURCE-CLOSURE-V1",
  selection_id="SEL-P15-REFERENCE-REGION-CORRECTED-V1",
  source_package_ids="SRC-P15-RAW-CLOSURE-20260906;SRC-P15-SOURCE-CLOSURE-20260907;SRC-WB-COUNTRIES-LOCAL-PEER-20260909")
tables <- list(country_attribution=x,revision_groups=groups,view_comparisons=views,
  peer_nonpeer_decomposition=decomp,changed_peer_pools=pool_detail,
  representative_seed_membership=members,ssa_primary_seeds=seeds,
  recipient_leave_one_out=recipient_loo,seed_leave_one_out=seed_loo,checks=checks)
dir.create(out,recursive=TRUE)
for(name in names(tables)) {
  d<-copy(tables[[name]]);for(k in names(bundle))set(d,j=k,value=bundle[[k]])
  fwrite(d,file.path(out,paste0(name,".csv")),na="")
}
manifest<-function(paths,role)do.call(pvr_manifest_rows,c(list(paths=paths,artifact_role=role),bundle))
fwrite(manifest(inputs,"input"),file.path(out,"input_manifest.csv"))
fwrite(manifest(c("scripts/p15/check_p15_ssa_peer_revision.R","R/research_governance.R"),"code"),file.path(out,"code_manifest.csv"))
fwrite(manifest(file.path(out,paste0(names(tables),".csv")),"diagnostic_output"),file.path(out,"output_manifest.csv"))
capture.output(sessionInfo(),file=file.path(out,"environment.txt"))
print(groups);print(decomp);print(views);print(checks)
