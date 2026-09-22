# Accepted private working peer layer. Caller supplies a completed legacy baseline.
# No active pointer, experimental predictions, regressions or network requests.
for (.p15_peer_file in c('research_governance','p15_local_completion','p15_ladder_preview',
    'p15_reference_review','p15_analysis_dataset','p15_revised_peer_scorecard','p15_revised_peer_matching'))
  source(paste0('R/', .p15_peer_file, '.R'))
rm(.p15_peer_file)

p15_build_revised_peer_candidate <- function(base_candidate, out,
    config_path='config/p15_revised_peer_20260912_v1.json') {
  suppressPackageStartupMessages({library(data.table);library(dplyr);library(digest);library(jsonlite)})
  if(dir.exists(out))stop('Fresh revised-peer output directory required: ',out)
  pvr_assert_write_allowed(out,'candidate')
  config <- p15_revised_peer_config(config_path)
  base <- base_candidate
  used <- character()
  read <- function(path) { used <<- unique(c(used,path));fread(path) }
  old <- read(file.path(base,'core_evidence.csv'));p<-copy(old)
  oldviews <- read(file.path(base,'selection_variants.csv'))
  oldref <- read(file.path(base,'selected_reference.csv'))
  oldmem <- read(file.path(base,'peer_membership.csv'))
  old_manifest <- read(file.path(base,'output_manifest.csv'))
  stopifnot(all(vapply(old_manifest$artifact_path,digest,character(1),file=TRUE,algo='sha256')==old_manifest$sha256))
  context <- read(file.path(base,'peer_region_context.csv'))
  case_dispositions <- read(file.path(base,'source_case_dispositions.csv'))
  dictionary <- read(file.path(base,'variable_dictionary.csv'))
  used <- unique(c(used,p15_revised_peer_input_files(config_path),'renv.lock'))
  input_hashes <- vapply(used,digest,character(1),file=TRUE,algo='sha256')
  key <- function(x)paste(x$analysis_year,x$iso3)
  same <- function(a,b)identical(is.na(a),is.na(b)) && all(abs(a-b)<1e-9,na.rm=TRUE)
  stopifnot(nrow(p)>0L,!anyDuplicated(key(p)),!anyDuplicated(key(context)),all(p$analysis_year %in% config$years))
  u <- p15_revised_peer_score_inputs(config,p[,.(iso3,analysis_year)])
  matrices <- p15_peer_read_exact_matrices(config$paths)
  # Endpoint bounds are valid only for monotone published matrices.
  for(nm in names(matrices)) {
    mm<-matrices[[nm]][if(nm=='rating_midpoint')rev(p15_peer_strength_labels) else p15_peer_strength_labels,,drop=FALSE]
    vv<-matrix(match(mm,if(nm=='rating_midpoint')p15_peer_rating_labels else p15_peer_strength_labels),15)
    stopifnot(all(apply(vv,1,diff)>=0),all(apply(vv,2,diff)>=0))
  }
  score <- p15_revised_peer_bounded_scores(u,matrices,TRUE)
  result <- p15_revised_peer_compute(p,oldref,context,score,config,hidden_validation=TRUE)
  all_predictions <- result$predictions
  pred <- all_predictions[mode=='deployment']
  seed <- result$seeds
  rules <- p15_revised_peer_rule_mapping()
  pred <- pred[match(key(p),key(pred))]
  stopifnot(nrow(pred)==nrow(p),identical(key(p),key(pred)),!anyDuplicated(key(pred)))
  comparison <- copy(pred[,.(iso3,analysis_year,country,historical_income_level,historical_lmic_reporting_scope,
    selected_tier,selected_rate_pct,ordinary_fallback_selection_permitted,new_peer_rate_pct=estimate,
    new_peer_rule=rule,new_peer_count=n_peers,peer_available,peer_eligible_for_selection,selected_peer_new,
    primary_members,ids_members,secondary_members,rating_implied_members,donor_shadow_members,target_shadow_used,
    estimate_depends_on_incomplete_input,member_ids,member_sources,member_rates)])
  comparison[,`:=`(old_peer_rate_pct=p$peer_rate_pct,old_peer_rule=p$peer_pool_rule,old_peer_count=p$peer_country_count,
    candidate_selected_tier=selected_tier,candidate_selected_rate_pct=selected_rate_pct)]
  comparison[,old_peer_available:=is.finite(old_peer_rate_pct)]
  comparison[,coverage_transition:=fifelse(old_peer_available&peer_available,'both',fifelse(old_peer_available,'old_only',fifelse(peer_available,'new_only','neither')))]
  comparison[,rate_change_pp:=new_peer_rate_pct-old_peer_rate_pct]
  comparison[selected_tier %in% c('peer','no_eligible_rate'),`:=`(candidate_selected_tier=fifelse(selected_peer_new,'peer','no_eligible_rate'),
    candidate_selected_rate_pct=fifelse(selected_peer_new,new_peer_rate_pct,NA_real_))]
  scale <- c('Aaa','Aa1','Aa2','Aa3','A1','A2','A3','Baa1','Baa2','Baa3','Ba1','Ba2','Ba3','B1','B2','B3','Caa1','Caa2','Caa3','Ca','C')
  # Authoritative donor values come from the saved full-precision seeds; strings are only a membership ledger.
  mem <- rbindlist(lapply(which(pred$peer_available),function(i) {
    z<-pred[i];ids<-strsplit(z$member_ids,';',fixed=TRUE)[[1]]
    extract<-function(s) { a<-strsplit(s,';',fixed=TRUE)[[1]];sub('^[^:]*:','',a) }
    data.table(analysis_year=z$analysis_year,target_iso3=z$iso3,peer_iso3=ids,
      peer_source=extract(z$member_sources),peer_donor_tier=extract(z$member_sources),encoded_rate=as.numeric(extract(z$member_rates)),
      peer_rating_basis=extract(z$member_rating_basis),encoded_matching_notch=as.numeric(replace(extract(z$member_notches),extract(z$member_notches)=='NA',NA_character_)),
      peer_rule_number=z$rule,peer_pool_rule=rules$current_label[match(z$rule,rules$rule)])
  }))
  if(!nrow(mem))mem<-data.table(analysis_year=integer(),target_iso3=character(),peer_iso3=character(),
    peer_source=character(),peer_donor_tier=character(),encoded_rate=numeric(),peer_rating_basis=character(),
    encoded_matching_notch=numeric(),peer_rule_number=integer(),peer_pool_rule=character())
  seed_at <- match(paste(mem$analysis_year,mem$peer_iso3),paste(seed$analysis_year,seed$iso3))
  core_at <- match(paste(mem$analysis_year,mem$peer_iso3),key(p))
  context_at <- match(paste(mem$analysis_year,mem$peer_iso3),key(context))
  score_at <- match(paste(mem$analysis_year,mem$peer_iso3),key(score))
  stopifnot(!anyNA(seed_at),!anyNA(core_at),!anyNA(context_at),!anyNA(score_at),all(mem$peer_source==seed$seed_source[seed_at]))
  mem[,peer_rate_pct:=seed$seed_rate[seed_at]]
  stopifnot(same(mem$peer_rate_pct,mem$encoded_rate))
  mem[,`:=`(peer_method='similarity_min3_rules1to5_radius3_mixed_donors_gross_interest_bounds',
    peer_income=p$historical_income_level[core_at],peer_region=context$rating_source_region[context_at],
    peer_rating=context$moodys_rating_normalized[context_at],peer_spread_pct=NA_real_,peer_maturity_years=NA_real_,
    peer_source_evidence_row_id=NA_character_,peer_source_package_ids=NA_character_,peer_currency_basis=NA_character_,
    peer_timing_basis=NA_character_,peer_rate_evidence_kind=NA_character_,used_for_estimate=TRUE)]
  # Original primary spreads are source facts, retained only where an exact primary donor key exists.
  oldprimary <- unique(oldmem[,.(analysis_year,peer_iso3,peer_spread_pct,peer_maturity_years)])
  stopifnot(!anyDuplicated(oldprimary[,.(analysis_year,peer_iso3)]))
  old_at<-match(paste(mem$analysis_year,mem$peer_iso3),paste(oldprimary$analysis_year,oldprimary$peer_iso3))
  for(tier in c('primary','ids','secondary','moodys')) {
    ii<-which(mem$peer_source==tier);if(!length(ii))next;at<-core_at[ii]
    if(tier %in% c('primary','secondary')) {
      prefix<-paste0(tier,'_usd_')
      set(mem,i=ii,j='peer_source_evidence_row_id',value=p[[paste0(prefix,'source_evidence_row_id')]][at])
      set(mem,i=ii,j='peer_source_package_ids',value=p[[paste0(prefix,'source_package_ids')]][at])
      set(mem,i=ii,j='peer_currency_basis',value='USD')
      set(mem,i=ii,j='peer_timing_basis',value=if(tier=='primary')'annual_issue_flow' else 'around_year_end_stock')
      set(mem,i=ii,j='peer_rate_evidence_kind',value=if(tier=='primary')'observed_primary_rate' else 'observed_or_repaired_secondary_rate')
      # The legacy peer maturity/spread columns describe primary donors only.
      if(tier=='primary') {
        set(mem,i=ii,j='peer_maturity_years',value=p$primary_usd_market_maturity_years[at])
        set(mem,i=ii,j='peer_spread_pct',value=oldprimary$peer_spread_pct[old_at[ii]])
      }
    } else if(tier=='ids') {
      set(mem,i=ii,j='peer_source_evidence_row_id',value=p$ids_evidence_id[at])
      set(mem,i=ii,j='peer_source_package_ids',value=p$ids_source_package_id[at])
      set(mem,i=ii,j='peer_currency_basis',value='IDS_currency_composition_unknown')
      set(mem,i=ii,j='peer_timing_basis',value='annual_new_commitment_terms')
      set(mem,i=ii,j='peer_rate_evidence_kind',value='reported_BND_contractual_proxy_not_verified_USD_yield')
    } else {
      set(mem,i=ii,j='peer_source_evidence_row_id',value=paste(p$analysis_year[at],p$iso3[at],p$rating_moodys_variant_id[at],sep='::'))
      set(mem,i=ii,j='peer_source_package_ids',value=p$source_package_ids[at])
      set(mem,i=ii,j='peer_currency_basis',value='USD_model_implied')
      set(mem,i=ii,j='peer_timing_basis',value='BOY_rating_and_annual_reference_inputs')
      set(mem,i=ii,j='peer_rate_evidence_kind',value='model_generated_original_Moodys_rating_implied_rate')
    }
  }
  mem[,peer_observed_notch:=match(peer_rating,scale)]
  mem[,peer_shadow_used:=!is.finite(peer_observed_notch)&is.finite(score$shadow_notch[score_at])]
  mem[,`:=`(peer_matching_notch=ifelse(peer_shadow_used,score$shadow_notch[score_at],peer_observed_notch),
    peer_matching_notch_lower=ifelse(peer_shadow_used,score$shadow_notch_lower[score_at],peer_observed_notch),
    peer_matching_notch_upper=ifelse(peer_shadow_used,score$shadow_notch_upper[score_at],peer_observed_notch))]
  stopifnot(same(mem$peer_matching_notch,mem$encoded_matching_notch))
  mem[,c('encoded_rate','encoded_matching_notch'):=NULL]
  source_id <- config$source_snapshot_id
  join_unique <- function(s)paste(sort(unique(unlist(strsplit(s[!is.na(s)&nzchar(s)],';',fixed=TRUE)))),collapse=';')
  agg <- mem[,.(peer_recomputed_rate_pct=median(peer_rate_pct),peer_recomputed_count=.N,
    peer_iqr_pp=IQR(peer_rate_pct),peer_donor_sources=join_unique(peer_source),
    peer_donor_source_package_ids=join_unique(peer_source_package_ids),
    peer_donor_timing_bases=join_unique(peer_timing_basis),peer_donor_currency_bases=join_unique(peer_currency_basis)),by=.(analysis_year,iso3=target_iso3)]
  ai<-match(key(p),key(agg));stopifnot(same(pred$estimate,agg$peer_recomputed_rate_pct[ai]))
  p[,`:=`(peer_rate_pct=pred$estimate,peer_minimum_met=pred$peer_available,
    peer_pool_rule=ifelse(is.na(pred$rule),'no_group_min3_by_rule5',rules$current_label[match(pred$rule,rules$rule)]),
    peer_country_count=as.integer(pred$n_peers),peer_iqr_pp=agg$peer_iqr_pp[ai],
    rating_proximity_used=!is.na(pred$rule)&pred$rule<=4L,global_pool_used=FALSE,
    peer_candidate_state=ifelse(pred$peer_available,'owner_authorized_private_mixed_donor_peer','no_group_min3_by_rule5'),
    peer_evidence_id=paste('P15-PEER-REVISED-GROSS-BOUNDS-V1',analysis_year,iso3,sep='::'),
    peer_rule_number=pred$rule,peer_primary_donor_count=pred$primary_members,peer_ids_donor_count=pred$ids_members,
    peer_secondary_donor_count=pred$secondary_members,peer_rating_implied_donor_count=pred$rating_implied_members,
    peer_shadow_donor_count=pred$donor_shadow_members,peer_target_shadow_used=pred$target_shadow_used,
    peer_depends_on_conditional_scorecard=pred$estimate_depends_on_incomplete_input,
    peer_source_package_ids=ifelse(pred$peer_available,paste(agg$peer_donor_source_package_ids[ai],source_id,sep=';'),source_id),
    peer_donor_sources=agg$peer_donor_sources[ai],peer_donor_timing_bases=agg$peer_donor_timing_bases[ai],
    peer_donor_currency_bases=agg$peer_donor_currency_bases[ai],
    peer_currency_basis=ifelse(!pred$peer_available,NA_character_,ifelse(pred$ids_members>0,'mixed_USD_and_IDS_currency_composition_unknown','USD_observed_or_model_implied')),
    peer_timing_basis=ifelse(pred$peer_available,'same_year_heterogeneous_donor_timing_see_peer_donor_timing_bases',NA_character_),
    peer_evidence_subtype=ifelse(!pred$peer_available,NA_character_,ifelse(pred$rating_implied_members>0,
      'similarity_min3_rules1to5_radius3_including_model_implied_donors','similarity_min3_rules1to5_radius3_observed_and_contractual_donors')))]
  # Stock selection functions preserve all eligibility decisions. Their historical primary-only peer descriptions are updated here.
  views <- as.data.table(p15_analysis_views(as.data.frame(p)));setorder(views,view_id,analysis_year,iso3)
  vi<-match(key(views),key(p));ii<-which(views$selected_tier=='peer')
  for(pair in list(c('selected_currency_basis','peer_currency_basis'),c('selected_timing_basis','peer_timing_basis'),
   c('selected_source_package_ids','peer_source_package_ids'),c('selected_evidence_subtype','peer_evidence_subtype'))) {
   set(views,i=ii,j=pair[1],value=p[[pair[2]]][vi[ii]])
  }
  views[selected_tier=='peer',`:=`(selected_global_peer=FALSE,selected_thin_evidence=NA,
    selected_first_rate_date=NA_character_,selected_last_rate_date=NA_character_)]
  elig <- as.data.table(p15_analysis_eligibility(as.data.frame(p)))
  ref <- views[view_id=='ids_before_secondary__with_peer'];nopeer<-views[view_id=='ids_before_secondary__without_peer']
  ri<-match(key(p),key(ref));stopifnot(same(ref$selected_rate_pct[ri],comparison$candidate_selected_rate_pct),identical(ref$selected_tier[ri],comparison$candidate_selected_tier))
  delta <- merge(oldviews[,.(analysis_year,iso3,view_id,old_tier=selected_tier,old_rate=selected_rate_pct)],
    views[,.(analysis_year,iso3,view_id,historical_lmic_reporting_scope,new_tier=selected_tier,new_rate=selected_rate_pct)],
    by=c('analysis_year','iso3','view_id'))
  delta[,`:=`(change_pp=new_rate-old_rate,rate_changed=xor(is.na(new_rate),is.na(old_rate))|fcoalesce(abs(new_rate-old_rate)>1e-9,FALSE))]
  changed_core <- c('peer_rate_pct','peer_minimum_met','peer_pool_rule','peer_country_count','peer_iqr_pp','rating_proximity_used','global_pool_used','peer_candidate_state','peer_evidence_id')
  stable <- setdiff(names(old),changed_core)
  mi<-match(paste(mem$analysis_year,mem$target_iso3),key(p))
  checks <- data.table(check_id=c('baseline_manifest_valid','unique_full_baseline_keys','nonpeer_core_columns_unchanged',
    'all_four_views_generated','without_peer_rates_and_tiers_unchanged','stronger_selected_rates_and_tiers_unchanged',
    'selected_peer_logic_matches_independent_last_tier_application','no_self_donors','unique_country_year_donors',
    'donor_source_rates_match_full_precision_seeds','membership_medians_reproduce_peer_rates',
    'all_available_groups_have_minimum_three','only_rules_1_to_5','no_global_peer_used',
    'donor_maturity_and_spread_missing_when_not_primary','IDS_peer_currency_label_correct','model_donor_subtype_label_correct',
    'score_intervals_computed_from_sources','full_panel_deployment_and_unique_hidden_validation','all_registered_inputs_unchanged'),
    passed=c(TRUE,nrow(p)==nrow(old)&&!anyDuplicated(key(p)),identical(old[,..stable],p[,..stable]),
      nrow(views)==nrow(p)*4L&&!anyDuplicated(views[,.(analysis_year,iso3,view_id)]),
      !any(delta[grepl('without_peer',view_id),rate_changed|old_tier!=new_tier]),
      !any(delta[!old_tier%in%c('peer','no_eligible_rate'),rate_changed|old_tier!=new_tier]),
      same(ref$selected_rate_pct[ri],comparison$candidate_selected_rate_pct),
      !any(mem$peer_iso3==mem$target_iso3),!anyDuplicated(mem[,.(analysis_year,target_iso3,peer_iso3)]),TRUE,
      same(p$peer_rate_pct,agg$peer_recomputed_rate_pct[ai]),all(p[peer_minimum_met==TRUE,peer_country_count]>=config$minimum_donors),
      all(na.omit(p$peer_rule_number)%in%config$rules),!any(p$global_pool_used),
      all(is.na(mem[peer_source!='primary',peer_maturity_years]))&&all(is.na(mem[peer_source!='primary',peer_spread_pct])),
      all(p[peer_ids_donor_count>0,grepl('IDS_currency_composition_unknown',peer_currency_basis)]),
      all(p[peer_rating_implied_donor_count>0,grepl('model_implied',peer_evidence_subtype)]),
      nrow(score)==nrow(p)&&!anyDuplicated(key(score)),
      nrow(pred)==nrow(p)&&!anyDuplicated(all_predictions[,.(iso3,analysis_year,method,mode)]),
      identical(input_hashes,vapply(used,digest,character(1),file=TRUE,algo='sha256'))))
  stopifnot(all(checks$passed))
  coverage<-views[,.(country_years=.N),by=.(view_id,historical_lmic_reporting_scope,selected_tier)]
  year<-views[,.(country_years=.N),by=.(view_id,analysis_year,historical_lmic_reporting_scope,selected_tier)]
  contrast<-merge(ref[,.(analysis_year,iso3,historical_lmic_reporting_scope,reference_tier=selected_tier,reference_rate_pct=selected_rate_pct)],
    views[view_id=='secondary_before_ids__with_peer',.(analysis_year,iso3,comparison_tier=selected_tier,comparison_rate_pct=selected_rate_pct)],by=c('analysis_year','iso3'))
  contrast<-contrast[reference_tier!=comparison_tier|abs(reference_rate_pct-comparison_rate_pct)>1e-12]
  bundle<-list(build_id=config$version,schema_id='SCHEMA-P15-ANALYSIS-MIXED-PEER-V2',
    estimator_id='EST-P15-CLOSEST-YEAR-END-PEER-PISR-GROSS-BOUNDS-V1',admissibility_id='ADM-P15-SOURCE-CLOSURE-V1',
    selection_id='SEL-P15-PRIVATE-RULES1TO5-PISR-RADIUS3-V1',source_package_ids=paste(old$source_package_ids[1],config$source_snapshot_id,sep=';'))
  tag<-function(d){d<-as.data.table(copy(d));for(n in names(bundle))set(d,j=n,value=bundle[[n]]);d}
  extra<-rbindlist(list(data.table(table='core_evidence',variable=setdiff(names(p),names(old))),
    data.table(table='peer_membership',variable=setdiff(names(mem),names(oldmem)))))
  extra[,`:=`(type='see_table_schema',units='see_label',label=gsub('_',' ',variable,fixed=TRUE))]
  dictionary<-unique(rbindlist(list(dictionary,extra),fill=TRUE),by=c('table','variable'))
  dictionary[table=='peer_membership'&variable%in%c('peer_maturity_years','peer_spread_pct'),label:='Legacy primary donor field; explicitly missing for nonprimary donors']
  dictionary[table=='core_evidence'&variable=='peer_rate_pct',label:='Median across at least three distinct eligible same-year P/IDS/S/rating-implied donors under Rules 1-5, radius 3; actual ratings precede conditional score intervals; peer selection remains last in ladder']
  tables<-list(core_evidence=p,selected_reference=ref,selected_without_peers=nopeer,selection_variants=views,tier_eligibility=elig,
    coverage_by_tier=coverage,coverage_by_year=year,source_order_differences=contrast,source_case_dispositions=case_dispositions,
    peer_membership=mem,readiness_checks=checks,variable_dictionary=dictionary,peer_region_context=context,
    selection_changes=delta,peer_changes=comparison,peer_rule_mapping=rules,peer_donor_seeds=seed,peer_score_intervals=score,
    scorecard_country_year=score,all_donor_seeds=seed,all_peer_predictions=all_predictions,peer_scorecard_inputs=u,
    revised_peer_design=result$design)
  dir.create(out,recursive=TRUE)
  for(n in names(tables))fwrite(tag(tables[[n]]),file.path(out,paste0(n,'.csv')),na='')
  manifest<-function(paths,role)do.call(pvr_manifest_rows,c(list(paths=paths,artifact_role=role),bundle))
  fwrite(manifest(used,'input'),file.path(out,'input_manifest.csv'))
  code<-setdiff(p15_revised_peer_dependency_paths(config_path),p15_revised_peer_input_files(config_path))
  fwrite(manifest(code,'code'),file.path(out,'script_manifest.csv'))
  fwrite(manifest(file.path(out,paste0(names(tables),'.csv')),'private_working_candidate_output'),file.path(out,'output_manifest.csv'))
  capture.output(sessionInfo(),file=file.path(out,'environment.txt'))
  jsonlite::write_json(list(state='owner_authorized_private_working_candidate_not_public_release',baseline_candidate=normalizePath(base),
    config=config_path,output_candidate=normalizePath(out),version=bundle,coverage=list(all_panel_available=sum(is.finite(p$peer_rate_pct)),
    LMIC_available=sum(p$historical_lmic_reporting_scope&is.finite(p$peer_rate_pct)),LMIC_selected_peer=nrow(ref[historical_lmic_reporting_scope==TRUE&selected_tier=='peer']),
    all_selected_peer=nrow(ref[selected_tier=='peer'])),
    source_note='Scorecard reconstructed from hashed prepared source snapshots; donors and matching reconstructed from provided baseline; no saved experimental predictions or active pointer'),
    file.path(out,'integration_manifest.json'),pretty=TRUE,auto_unbox=TRUE)
  invisible(list(candidate=normalizePath(out),checks=checks,coverage=coverage))
}
