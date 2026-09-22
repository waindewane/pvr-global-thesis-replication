source("R/p15_current_inputs.R")
#!/usr/bin/env Rscript
suppressPackageStartupMessages({library(data.table);library(ggplot2);library(digest)})
source("R/p15_dataset_assessment.R")
source("R/research_governance.R")
source("R/p15_analysis_dataset.R")
source("R/p15_bounded_fallback_comparison.R")
source("R/p15_revised_peer_validation.R")
args <- commandArgs(trailingOnly=TRUE)
out <- if(length(args))args[1] else file.path("data-derived",paste0("p15_assessment_",format(Sys.time(),"%Y%m%d_%H%M%S")))
if(dir.exists(out)) stop("Refusing to overwrite assessment: ",out)
base <- Sys.getenv("P15_ANALYSIS_BASE", p15_current_input("dataset"))
used <- character()
read <- function(path) { used <<- unique(c(used,path)); fread(file=path) }
p <- read(file.path(base,"core_evidence.csv"))
elig <- read(file.path(base,"tier_eligibility.csv"))
selected <- read(file.path(base,"selected_reference.csv"))
variants <- read(file.path(base,"selection_variants.csv"))
members <- read(file.path(base,"peer_membership.csv"))
case_dispositions <- read(file.path(base,"source_case_dispositions.csv"))
delivery_manifest <- read(file.path(base,"output_manifest.csv"))
context_path <- file.path(base,"peer_region_context.csv")
if(!file.exists(context_path)) {
  prior_manifest <- read("data-derived/p15_reference_review_20260907_v1/input_manifest.csv")
  context_path <- prior_manifest$artifact_path[grepl("rating_component_ledger_2012_2024.csv.gz$",prior_manifest$artifact_path)]
  stopifnot(length(context_path)==1L)
}
context <- read(context_path)[,.(analysis_year,iso3,rating_source_region,moodys_rating_normalized)]
geography_path <- "data-raw/world_bank_countries.json"
used <- c(used,geography_path)
wb <- jsonlite::fromJSON(geography_path)[[2]]
geography <- data.table(iso3=wb$id,wb_region=trimws(wb$region$value))
geography <- geography[wb_region!="Aggregates"]
p15_assessment_keys(as.data.frame(geography),"iso3","World Bank geography")
p15_assessment_keys(as.data.frame(p),c("iso3","analysis_year"),"core")
p15_assessment_keys(as.data.frame(elig),c("iso3","analysis_year","tier"),"eligibility")
p15_assessment_keys(as.data.frame(context),c("iso3","analysis_year"),"regional context")
stopifnot(nrow(p)==2743,identical(sort(unique(p$analysis_year)),2012:2024))
rebuilt_elig <- as.data.table(p15_analysis_eligibility(as.data.frame(p)))
ee <- merge(elig,rebuilt_elig,by=c("iso3","analysis_year","tier"))
stopifnot(nrow(ee)==nrow(elig),all(ee$eligible.x==ee$eligible.y),
  all(ee$exclusion_reason.x==ee$exclusion_reason.y))
frozen_hashes <- vapply(delivery_manifest$artifact_path,digest,character(1),file=TRUE,algo="sha256")
stopifnot(all(frozen_hashes==delivery_manifest$sha256))
p <- merge(p,context,by=c("iso3","analysis_year"),all.x=TRUE,sort=FALSE)
p <- merge(p,geography,by="iso3",all.x=TRUE,sort=FALSE)
case_context <- case_dispositions[,.(iso3,analysis_year,case_review_note=review_note,
  case_followup_interpretation=interpretation,case_current_use=current_use,
  case_remaining_evidence=remaining_evidence,case_source_url=source_url_new)]
p15_assessment_keys(as.data.frame(case_context),c("iso3","analysis_year"),"case dispositions")
p <- merge(p,case_context,by=c("iso3","analysis_year"),all.x=TRUE,sort=FALSE)
p[,peer_region_missing:=is.na(rating_source_region)|rating_source_region==""]
p[,region:=fifelse(peer_region_missing,wb_region,trimws(rating_source_region))]
p[is.na(region)|region=="",region:="Region unavailable"]
p[,era:=fcase(analysis_year<=2016,"2012-2016",analysis_year<=2020,"2017-2020",default="2021-2024")]
tiers <- c("primary","ids","secondary","moodys","peer")
rate_cols <- c(primary="primary_usd_market_rate_pct",ids="ids_rate_pct",secondary="secondary_usd_market_rate_pct",
  moodys="rating_moodys_rate_pct",peer="peer_rate_pct")
for(t in tiers) {
  e<-elig[tier==t]; a<-match(paste(p$iso3,p$analysis_year),paste(e$iso3,e$analysis_year))
  stopifnot(!anyNA(a),isTRUE(all.equal(p[[rate_cols[t]]],e$rate_pct[a],tolerance=1e-12,check.attributes=FALSE)))
  set(p,j=paste0(t,"_eligible"),value=e$eligible[a])
  set(p,j=t,value=ifelse(e$eligible[a],e$rate_pct[a],NA_real_))
}
p[,direct_depth:=as.integer(primary_eligible)+as.integer(secondary_eligible)]
p[,total_depth:=rowSums(.SD),.SDcols=paste0(tiers,"_eligible")]
p[,stronger_available:=primary_eligible|secondary_eligible|ids_eligible]
selcols <- c("iso3","analysis_year","selected_tier","selected_rate_pct","selected_global_peer")
p<-merge(p,selected[,..selcols],by=c("iso3","analysis_year"),all.x=TRUE,sort=FALSE)
setorder(p,iso3,analysis_year)
p[,prior_primary_count:=shift(cumsum(primary_eligible),fill=0),by=iso3]
l<-p[historical_lmic_reporting_scope==TRUE]
stopifnot(nrow(l)==1756)
summary_cov <- function(d) {
  data.table(n=nrow(d),countries=uniqueN(d$iso3),primary=sum(d$primary_eligible),ids=sum(d$ids_eligible),
    secondary=sum(d$secondary_eligible),moodys=sum(d$moodys_eligible),peer=sum(d$peer_eligible),
    any_direct=sum(d$direct_depth>0),both_direct=sum(d$direct_depth==2),
    any_primary_ids_secondary=sum(d$stronger_available),any_selected=sum(is.finite(d$selected_rate_pct)),
    selected_peer=sum(d$selected_tier=="peer"),selected_global_peer=sum(d$selected_global_peer),
    individual_case_review=sum(d$status_case_review_present %in% TRUE),
    no_case_review=sum(!d$status_case_review_present %in% TRUE),
    mean_evidence_depth=mean(d$total_depth))
}
tables <- list(coverage_overall=rbindlist(list(cbind(scope="historical_LMIC",summary_cov(l)),
  cbind(scope="outside_historical_LMIC",summary_cov(p[historical_lmic_reporting_scope==FALSE])))),
  coverage_by_year=l[,summary_cov(.SD),by=analysis_year],
  coverage_by_region=l[,summary_cov(.SD),by=region],
  coverage_by_income=l[,summary_cov(.SD),by=historical_income_level],
  coverage_by_country=l[,{d<-copy(.SD);d[,iso3:=.BY$iso3];summary_cov(d)},by=.(iso3,country)],
  selection_by_year=l[,.N,by=.(analysis_year,selected_tier)],
  eligibility_reasons=merge(elig,p[,.(iso3,analysis_year,historical_lmic_reporting_scope)],
    by=c("iso3","analysis_year"))[historical_lmic_reporting_scope==TRUE,.N,by=.(tier,exclusion_reason)],
  evidence_depth=l[,.N,by=.(direct_depth,total_depth)])
tables$eur_coverage <- rbindlist(lapply(c("primary","secondary"),function(t) {
  eur<-is.finite(l[[paste0(t,"_eur_market_rate_pct")]]) & !(l$observed_benchmark_selection_permitted %in% FALSE)
  usd<-l[[paste0(t,"_eligible")]]
  data.table(branch=t,usd=sum(usd),eur=sum(eur),both=sum(usd&eur),eur_without_usd=sum(eur&!usd),
    any_currency=sum(usd|eur),note="Separate currencies; no nominal pooling")
}))
tables$observed_quality <- rbindlist(lapply(c("primary","secondary"),function(t) {
  d<-l[l[[paste0(t,"_eligible")]]==TRUE]; pre<-paste0(t,"_usd_")
  data.table(branch=t,n=nrow(d),thin=sum(d[[paste0(pre,"thin_evidence")]]%in%TRUE),
    share_over_80pct=sum(d[[paste0(pre,"largest_issue_weight_share")]]>.8,na.rm=TRUE),
    median_issues=median(d[[paste0(pre,"retained_quantitative_issue_count")]],na.rm=TRUE),
    median_maturity=median(d[[paste0(pre,"market_maturity_years")]],na.rm=TRUE))
}))
pairs <- rbindlist(lapply(combn(tiers,2,simplify=FALSE),function(ts) {
  d<-copy(l[l[[paste0(ts[1],"_eligible")]] & l[[paste0(ts[2],"_eligible")]]])
  d[,`:=`(anchor=ts[1],comparison=ts[2],pair=paste(ts,collapse="__"),
    anchor_rate=get(ts[1]),comparison_rate=get(ts[2]))]
  d[,gap_pp:=comparison_rate-anchor_rate]
  d[,.(iso3,analysis_year,country,region,historical_income_level,era,pair,anchor,comparison,
    anchor_rate,comparison_rate,gap_pp,primary_usd_thin_evidence,secondary_usd_candidate_evidence_tier,
    primary_usd_market_maturity_years,secondary_usd_market_maturity_years,
    primary_usd_first_rate_date,primary_usd_last_rate_date,
    secondary_usd_first_rate_date,secondary_usd_last_rate_date,peer_pool_rule,global_pool_used,
    status_case_review_present,approved_status_rule_class,status_case_note,source_closure_current_use,
    ids_review_explanation,observed_case_explanation,case_review_note,case_followup_interpretation,
    case_current_use,case_remaining_evidence,case_source_url)]
}))
tables$pair_details<-pairs
tables$pair_summary<-pairs[,p15_assessment_metrics(.SD),by=.(pair,anchor,comparison)]
inf<-pairs[,p15_assessment_inference(.SD),by=pair]
inf[,p_bh:=p.adjust(p_value,method="BH"),by=inference]
tables$pair_inference<-inf
for(g in c("region","historical_income_level","analysis_year","era"))
  tables[[paste0("pairs_by_",g)]]<-pairs[,{
    d<-copy(.SD)
    if(g=="analysis_year") d[,analysis_year:=.BY$analysis_year]
    p15_assessment_metrics(d)
  },by=c("pair",g)]
tables$pairs_by_primary_thin<-pairs[anchor=="primary",p15_assessment_metrics(.SD),by=.(pair,primary_usd_thin_evidence)]
tables$pairs_by_secondary_subtype<-pairs[anchor=="secondary"|comparison=="secondary",
  p15_assessment_metrics(.SD),by=.(pair,secondary_usd_candidate_evidence_tier)]
tables$country_gaps<-pairs[,{
  d<-copy(.SD);d[,iso3:=.BY$iso3];p15_assessment_metrics(d)
},by=.(pair,iso3,country,region)]
tables$largest_discrepancies<-pairs[order(-abs(gap_pp)),head(.SD,10),by=pair]
tables$leave_one_country_out<-pairs[,{
  rbindlist(lapply(unique(iso3),function(id){z<-.SD[iso3!=id];cbind(omitted_iso3=id,p15_assessment_metrics(z))}))
},by=pair]
allfive<-l[primary_eligible&ids_eligible&secondary_eligible&moodys_eligible&peer_eligible]
tables$common_primary_sample<-rbindlist(lapply(tiers[-1],function(t){
  d<-data.frame(iso3=allfive$iso3,analysis_year=allfive$analysis_year,anchor_rate=allfive$primary,
    comparison_rate=allfive[[t]],gap_pp=allfive[[t]]-allfive$primary)
  cbind(comparison=t,p15_assessment_metrics(d))
}))
pr<-copy(pairs[anchor=="primary"]);setorder(pr,pair,iso3,analysis_year)
pr[,`:=`(prior_year=shift(analysis_year),prior_gap=shift(gap_pp)),by=.(pair,iso3)]
pr[,country_mean:=mean(gap_pp),by=.(pair,iso3)]
pr[,demeaned_gap:=gap_pp-country_mean]
pr[,prior_demeaned:=shift(demeaned_gap),by=.(pair,iso3)]
tables$consecutive_gap_persistence<-pr[analysis_year-prior_year==1,
  .(n=.N,countries=uniqueN(iso3),same_sign_share=mean(sign(gap_pp)==sign(prior_gap)),
    lag_correlation=cor(gap_pp,prior_gap),within_country_lag_correlation=cor(demeaned_gap,prior_demeaned)),by=pair]
sp<-copy(pairs[pair=="primary__secondary"])
sp[,`:=`(minimum_date_separation_days=as.integer(secondary_usd_first_rate_date-primary_usd_last_rate_date),
  maturity_gap_years=abs(secondary_usd_market_maturity_years-primary_usd_market_maturity_years))]
sp[,date_window_group:=fcase(is.na(minimum_date_separation_days),"Dates unavailable",
  minimum_date_separation_days<0,"Overlapping/reversed ranges",
  minimum_date_separation_days<=90,"0-90 days apart",default="More than 90 days apart")]
sp[,maturity_group:=fcase(is.na(maturity_gap_years),"Maturity unavailable",
  maturity_gap_years<=3,"Within 3 maturity years",default="More than 3 maturity years")]
tables$timing_context<-sp
tables$timing_group_comparison<-sp[,p15_assessment_metrics(.SD),by=.(date_window_group,maturity_group)]

# Rebuild each country's highest-priority usable donor source and revised matching.
revised <- p15_revised_peer_inputs(base, read)
replayed <- p15_revised_predictions(revised)
p15_revised_reference_check(replayed, revised)
seed <- revised$seeds[,.(analysis_year,peer_iso3=iso3,accepted_donor=seed_rate,
  expected_donor_tier=seed_source,seed_id)]
mm <- merge(members[used_for_estimate %in% TRUE],seed,by=c("analysis_year","peer_iso3"),all.x=TRUE)
peer_calc <- mm[,.(member_median=median(peer_rate_pct),member_count=.N,
  member_ids=paste(sort(peer_iso3),collapse=";")),by=.(analysis_year,iso3=target_iso3)]
pc <- merge(p,peer_calc,by=c("analysis_year","iso3"),all.x=TRUE)
rp <- merge(pc,replayed[,.(iso3,analysis_year,estimate,n_peers,replayed_members=member_ids)],
  by=c("iso3","analysis_year"))
# Moody's sources have a field locator because the original tier has no row ID.
ids_ok <- mm$peer_source_evidence_row_id==mm$seed_id
member_checks <- data.table(check_id=c("no_self_peer","all_members_current_prioritized_PISR",
  "peer_seed_identifiers","peer_medians_reproduce","peer_counts_reproduce","peer_members_unique",
  "full_method_rates_reproduced","full_method_members_reproduced","donor_tiers_reconcile"),
  passed=c(all(mm$target_iso3!=mm$peer_iso3),
    all(is.finite(mm$accepted_donor)&abs(mm$peer_rate_pct-mm$accepted_donor)<1e-10),
    all(ids_ok),
    all(is.finite(pc$member_median[is.finite(pc$peer_rate_pct)])) &&
      all(abs(pc$peer_rate_pct-pc$member_median)<1e-10,na.rm=TRUE),
    all(is.finite(pc$member_count[is.finite(pc$peer_rate_pct)])) &&
      all(pc$peer_country_count==pc$member_count,na.rm=TRUE),
    !anyDuplicated(mm[,.(analysis_year,target_iso3,peer_iso3)]),
    identical(is.finite(rp$estimate),is.finite(rp$peer_rate_pct)) &&
      all(abs(rp$estimate-rp$peer_rate_pct)<1e-10,na.rm=TRUE),
    all(rp[is.finite(estimate),member_ids==replayed_members]),
    if("peer_donor_tier"%in%names(mm))all(mm$peer_donor_tier==mm$expected_donor_tier) else TRUE))
tables$peer_membership_checks <- member_checks
tables$peer_donor_sources <- mm[,.(donor_links=.N,donor_country_years=uniqueN(paste(peer_iso3,analysis_year)),
  target_country_years=uniqueN(paste(target_iso3,analysis_year))),by=expected_donor_tier]
# Repeat the original geography-fill question using the revised algorithm.
# Preserve scores, rate sources, donor priority, rules and caliper.
filled_panel <- copy(revised$panel)
fill_at <- match(filled_panel$iso3,geography$iso3)
missing_region <- is.na(filled_panel$rating_source_region)|!nzchar(filled_panel$rating_source_region)
filled_panel[missing_region,rating_source_region:=geography$wb_region[fill_at[missing_region]]]
filled <- p15_revised_predictions(revised,panel=filled_panel)
geo_trials <- merge(p[,.(iso3,analysis_year,historical_lmic_reporting_scope,selected_tier,primary,
  accepted_peer=peer_rate_pct,region_was_missing=peer_region_missing)],
  replayed[,.(iso3,analysis_year,old_rebuilt=estimate,old_pool=rule,old_peer_count=n_peers)],
  by=c("iso3","analysis_year"))
geo_trials <- merge(geo_trials,filled[,.(iso3,analysis_year,geography_filled_peer=estimate,
  new_pool=rule,new_peer_count=n_peers)],by=c("iso3","analysis_year"))
stopifnot(identical(is.finite(geo_trials$accepted_peer),is.finite(geo_trials$old_rebuilt)),
  all(abs(geo_trials$accepted_peer-geo_trials$old_rebuilt)<1e-10,na.rm=TRUE))
geo_trials[,change_pp:=geography_filled_peer-accepted_peer]
tables$geography_sensitivity_details <- geo_trials
tables$geography_sensitivity_summary <- geo_trials[historical_lmic_reporting_scope==TRUE,
  .(n=.N,missing_region=sum(region_was_missing),changed=sum(abs(change_pp)>1e-10,na.rm=TRUE),
    mean_absolute_change=mean(abs(change_pp),na.rm=TRUE),max_absolute_change=max(abs(change_pp),na.rm=TRUE),
    old_global=sum(old_pool==8,na.rm=TRUE),new_global=sum(new_pool==8,na.rm=TRUE)),by=selected_tier]
tables$geography_validation <- rbindlist(lapply(c("accepted_peer","geography_filled_peer"),function(method){
  d<-geo_trials[historical_lmic_reporting_scope==TRUE&is.finite(primary)&is.finite(get(method))]
  d[,`:=`(gap_pp=get(method)-primary,anchor_rate=primary,comparison_rate=get(method))]
  cbind(method=method,p15_assessment_metrics(d))
}))
transport<-rbindlist(lapply(c("moodys","peer"),function(t){
  rbindlist(lapply(c("validation_primary_overlap","actual_selected_fallback"),function(cohort){
    d<-if(cohort=="validation_primary_overlap")l[primary_eligible & get(paste0(t,"_eligible"))] else l[selected_tier==t]
    d[,.(method=t,cohort,iso3,analysis_year,region,historical_income_level,rating_moodys_rating,
      rate_pct=get(t),prior_primary_count,peer_pool_rule,global_pool_used,peer_country_count,peer_iqr_pp)]
  }))
}))
tables$validation_vs_deployment<-transport[,.(n=.N,countries=uniqueN(iso3),
  prior_primary_available=sum(prior_primary_count>0),median_rate=median(rate_pct),
  moodys_missing=sum(is.na(rating_moodys_rating)|rating_moodys_rating==""),
  global_pool=sum(global_pool_used),median_peer_count=as.numeric(median(peer_country_count)),median_peer_iqr=median(peer_iqr_pp)),by=.(method,cohort)]
tables$deployment_by_region<-transport[,.N,by=.(method,cohort,region)]
tables$deployment_by_income<-transport[,.N,by=.(method,cohort,historical_income_level)]
tables$deployment_by_rating<-transport[,.N,by=.(method,cohort,rating_moodys_rating)]
tables$peer_validation_by_pool<-pairs[pair=="primary__peer",p15_assessment_metrics(.SD),by=peer_pool_rule]

tr<-copy(p);setorder(tr,iso3,analysis_year)
tr[,`:=`(previous_year=shift(analysis_year),previous_tier=shift(selected_tier),
  previous_rate=shift(selected_rate_pct),previous_lmic=shift(historical_lmic_reporting_scope)),by=iso3]
for(t in tiers) tr[[paste0(t,"_previous")]]<-tr[,shift(get(t)),by=iso3]$V1
tr<-tr[historical_lmic_reporting_scope==TRUE & previous_lmic==TRUE & analysis_year-previous_year==1 &
  is.finite(previous_rate)&is.finite(selected_rate_pct)]
tr[,`:=`(source_switch=selected_tier!=previous_tier,total_change_pp=selected_rate_pct-previous_rate)]
tables$transition_summary<-tr[,.(n=.N,countries=uniqueN(iso3),mean_change=mean(total_change_pp),
  mean_absolute_change=mean(abs(total_change_pp)),median_absolute_change=median(abs(total_change_pp)),
  p90_absolute_change=quantile(abs(total_change_pp),.9)),by=source_switch]
tables$transition_matrix<-tr[,.N,by=.(previous_tier,selected_tier)]
switches<-tr[source_switch==TRUE]
old_now<-mapply(function(t,i)switches[[t]][i],switches$previous_tier,seq_len(nrow(switches)))
new_previous<-mapply(function(t,i)switches[[paste0(t,"_previous")]][i],switches$selected_tier,seq_len(nrow(switches)))
bridge<-p15_assessment_bridges(switches$previous_rate,switches$selected_rate_pct,old_now,new_previous)
tables$source_switch_details<-cbind(switches[,.(iso3,country,analysis_year,region,previous_tier,selected_tier,previous_rate,selected_rate_pct)],bridge)
tables$variant_summary<-variants[historical_lmic_reporting_scope==TRUE,.(n=.N,
  available=sum(is.finite(selected_rate_pct)),mean_rate=mean(selected_rate_pct,na.rm=TRUE),
  median_rate=median(selected_rate_pct,na.rm=TRUE)),by=view_id]
vv<-dcast(variants[historical_lmic_reporting_scope==TRUE],iso3+analysis_year~view_id,value.var="selected_rate_pct")
vv[,source_order_gap_pp:=secondary_before_ids__with_peer-ids_before_secondary__with_peer]
tables$source_order_changes<-vv[is.finite(source_order_gap_pp)&abs(source_order_gap_pp)>1e-10]
tables$annual_tier_rates<-rbindlist(lapply(tiers,function(t) l[is.finite(get(t)),
  .(tier=t,n=.N,countries=uniqueN(iso3),mean_rate=mean(get(t)),median_rate=median(get(t))),by=analysis_year]))
balanced<-rbindlist(lapply(tiers,function(t){
  balanced_ids<-l[,.(valid_years=sum(is.finite(get(t)))),by=iso3][valid_years==13,iso3]
  if(!length(balanced_ids))return(data.table(tier=t,analysis_year=2012:2024,n=0L,mean_rate=NA_real_))
  l[iso3%in%balanced_ids,.(tier=t,n=.N,mean_rate=mean(get(t))),by=analysis_year]
}),use.names=TRUE)
tables$balanced_tier_rates<-balanced

# Link the most recent peer validation to current observed anchors by key and value.
old<-read("data-derived/p15_peer_history_matching_20260907_v1/history_comparison.csv.gz")
old<-merge(old,l[,.(iso3,analysis_year)],by=c("iso3","analysis_year"))
recon<-merge(l[,.(iso3,analysis_year,primary,peer)],old[,.(iso3,analysis_year,observed_primary_rate_pct,reference_rate_pct)],
  by=c("iso3","analysis_year"),all=TRUE)
recon[,`:=`(current_overlap=is.finite(primary)&is.finite(peer),
  old_overlap=is.finite(observed_primary_rate_pct)&is.finite(reference_rate_pct))]
tables$prior_peer_reconciliation<-recon[current_overlap|old_overlap]
tables$prior_peer_reconciliation_summary<-recon[,.(current_pairs=sum(current_overlap),
  old_pairs=sum(old_overlap),common=sum(current_overlap&old_overlap),
  changed_primary=sum(current_overlap&old_overlap&abs(primary-observed_primary_rate_pct)>1e-10),
  changed_peer=sum(current_overlap&old_overlap&abs(peer-reference_rate_pct)>1e-10))]
protocol<-"docs/governance/P15_DATASET_ASSESSMENT_PROTOCOL_2026-09-08.md"
used<-c(used,protocol,"renv.lock")
start_hashes<-vapply(used,digest,character(1),file=TRUE,algo="sha256")
checks<-rbind(member_checks,data.table(check_id=c("delivery_hashes_unchanged","LMIC_grid_1756",
  "all_ten_pairs","all_four_variants","source_order_29","old_bridge_identity","new_bridge_identity"),
  passed=c(identical(frozen_hashes,vapply(delivery_manifest$artifact_path,digest,character(1),file=TRUE,algo="sha256")),
    nrow(l)==1756,uniqueN(pairs$pair)==10,uniqueN(variants$view_id)==4,
    nrow(tables$source_order_changes)==29,
    all(abs(bridge$total_change_pp-bridge$old_source_within_change_pp-bridge$source_difference_current_year_pp)<1e-10,na.rm=TRUE),
    all(abs(bridge$total_change_pp-bridge$new_source_within_change_pp-bridge$source_difference_previous_year_pp)<1e-10,na.rm=TRUE))))
checks<-rbind(checks,data.table(check_id=c("regional_LMIC_context_complete","balanced_all_five_tiers",
  "balanced_65_rows","country_coverage_single_entity","subgroup_year_counts"),
  passed=c(all(l$region!="Region unavailable"),uniqueN(balanced$tier)==5,nrow(balanced)==65,
    all(tables$coverage_by_country$countries==1),all(tables$pairs_by_analysis_year$years==1))))
tables$checks<-checks
print(checks)
stopifnot(all(checks$passed))
dir.create(out,recursive=TRUE)
bundle<-list(build_id=basename(out),schema_id="SCHEMA-P15-ASSESSMENT-V1",estimator_id="EST-P15-DESCRIPTIVE-ASSESSMENT-V1",
  admissibility_id="ADM-P15-SOURCE-CLOSURE-V1",selection_id="SEL-P15-NO-PROMOTION-V1",
  source_package_ids=paste(unique(selected$source_package_ids),collapse=";"))
for(n in names(tables)) fwrite(tables[[n]],file.path(out,paste0(n,".csv")),na="")

theme_set(theme_minimal(base_size=11)+theme(panel.grid.minor=element_blank(),legend.position="bottom",
  plot.caption=element_text(hjust=0,size=8),strip.text=element_text(size=10)))
labels<-c(primary="Primary issuance",ids="IDS Bondholders",secondary="Secondary market",moodys="Moody's estimate",peer="Peer estimate",no_eligible_rate="No eligible rate")
fig<-function(name,g,width=9,height=5){
  ggsave(file.path(out,paste0(name,".png")),g,width=width,height=height,dpi=160,bg="white")
  ggsave(file.path(out,paste0(name,".pdf")),g,width=width,height=height,device="pdf",useDingbats=FALSE)
}
cov<-melt(tables$coverage_by_year,id.vars="analysis_year",measure.vars=tiers,variable.name="tier",value.name="n")
cov[,label:=labels[as.character(tier)]]
fig("figure_1_available_evidence",ggplot(cov,aes(analysis_year,n))+geom_line(color="#2F6B9A")+geom_point(size=1)+
  facet_wrap(~label,ncol=3)+scale_x_continuous(breaks=c(2012,2016,2020,2024))+ylim(0,max(cov$n)*1.06)+
  labs(title="Available eligible evidence by year",x=NULL,y="Historical-LMIC country-years",
    caption="Source: fixed P15 candidate, 2012-2024. Tiers overlap; these are not selected-source counts."),height=6)
vis<-pairs[anchor=="primary"];vis[,label:=labels[comparison]]
fig("figure_2_primary_comparisons",ggplot(vis,aes(anchor_rate,comparison_rate))+geom_abline(slope=1,intercept=0,color="grey60")+
  geom_point(alpha=.45,size=1.2,color="#2F6B9A")+facet_wrap(~label,ncol=2)+
  labs(title="Other evidence compared with primary issuance",x="Primary issue yield (%)",y="Comparator rate (%)",
    caption="Eligible paired LMIC country-years, 2012-2024. All extremes shown. IDS and secondary measure different objects."),height=7)
fig("figure_3_gaps_over_time",ggplot(vis,aes(analysis_year,gap_pp))+geom_hline(yintercept=0,color="grey50")+
  geom_point(alpha=.35,size=1.1,color="#2F6B9A")+facet_wrap(~label,ncol=2)+scale_x_continuous(breaks=c(2012,2016,2020,2024))+
  labs(title="Signed differences from primary issuance",x=NULL,y="Comparator minus primary (percentage points)",
    caption="All eligible paired observations. Positive means the comparator is higher; no causal timing attribution."),height=7)
cg<-tables$country_gaps[pair=="primary__moodys"&n>=4]
fig("figure_4_country_rating_gaps",ggplot(cg,aes(mean_gap_pp,reorder(country,mean_gap_pp)))+
  geom_vline(xintercept=0,color="grey65")+geom_point(color="#2F6B9A")+
  labs(title="Rating gaps differ across countries",x="Mean Moody's estimate minus primary (percentage points)",y=NULL,
    caption="Countries with at least four matched years. Descriptive means, not estimated permanent effects or confidence intervals."),height=max(5,.20*nrow(cg)+1))
infvis<-inf[inference=="country_year"&grepl("^primary__",pair)]
infvis[,label:=labels[sub("primary__","",pair)]]
fig("figure_5_mean_gap_uncertainty",ggplot(infvis,aes(mean_gap_pp,label))+
  geom_vline(xintercept=0,color="grey65")+geom_segment(aes(x=ci_low_pp,xend=ci_high_pp,yend=label),color="#2F6B9A")+
  geom_point(color="#2F6B9A",size=2)+labs(title="Overall directional differences and uncertainty",x="Comparator minus primary (percentage points)",y=NULL,
    caption="95% approximate country/year-clustered intervals; 13 year clusters. Failure to reject zero does not establish accuracy."),height=4)
rs<-copy(tables$coverage_by_region);rs[,`:=`(direct_share=any_direct/n,peer_share=selected_peer/n)]
rv<-melt(rs,id.vars="region",measure.vars=c("direct_share","peer_share"))
rv[,region:=vapply(region,function(x)paste(strwrap(x,width=30),collapse="\n"),character(1))]
fig("figure_6_regional_evidence",ggplot(rv,aes(value,region,shape=variable,color=variable))+geom_point(size=2.5)+
  scale_x_continuous(labels=scales::label_percent(),limits=c(0,1))+
  scale_color_manual(values=c("#2F6B9A","#B45A4A"),labels=c("Any primary/secondary evidence","Peer selected"))+
  scale_shape_discrete(labels=c("Any primary/secondary evidence","Peer selected"))+
  labs(title="Direct evidence and peer dependence by region",x="Share of historical-LMIC country-years",y=NULL,color=NULL,shape=NULL,
    caption="Source-vintage regional labels; unknown region retained. These two shares are not complementary."),height=5)

make_manifest<-function(paths,role)do.call(pvr_manifest_rows,c(list(paths=paths,artifact_role=role),bundle))
fwrite(make_manifest(used,"assessment_input"),file.path(out,"input_manifest.csv"))
code<-c("R/p15_dataset_assessment.R","scripts/p15/build_p15_dataset_assessment.R","R/research_governance.R","R/p15_analysis_dataset.R","R/p15_bounded_fallback_comparison.R")
code<-unique(c(code,p15_revised_helper_code))
fwrite(make_manifest(code,"assessment_code"),file.path(out,"script_manifest.csv"))
fwrite(make_manifest(list.files(out,pattern="[.](csv|png|pdf)$",full.names=TRUE),"diagnostic_output"),file.path(out,"output_manifest.csv"))
capture.output(sessionInfo(),file=file.path(out,"environment.txt"))
stopifnot(identical(start_hashes,vapply(used,digest,character(1),file=TRUE,algo="sha256")))
print(tables$pair_summary);print(tables$coverage_overall);print(checks)
if(!all(checks$passed))stop("Assessment has failed evidence checks; inspect output, do not claim complete")
