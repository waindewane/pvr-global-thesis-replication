# Offline, versioned full-panel peer build. Run from the repository root.
source('scripts/p15/activate_p15_environment.R')
suppressPackageStartupMessages({library(data.table);library(jsonlite);library(digest)})
v1 <- 'experiments/p15_wb_scorecard_20260912/constructed_v1'
v2 <- 'experiments/p15_wb_scorecard_20260912/full_panel_v2'
out <- file.path(v2,'results');dir.create(out,recursive=TRUE,showWarnings=FALSE)
manifest <- function(paths) data.table(path=paths,sha256=vapply(paths,digest,character(1),file=TRUE,algo='sha256'),bytes=file.info(paths)$size)
frozen_ptr <- fromJSON(file.path(v2,'snapshot/current_run.json'))
preserved_dirs <- unique(c(frozen_ptr$candidate,frozen_ptr$report,
  vapply(frozen_ptr$stages,function(z)z$dir,character(1)),v1))
preserved_paths <- sort(unique(c('data-derived/p15_master/current_run.json',
  unlist(lapply(preserved_dirs,list.files,full.names=TRUE,recursive=TRUE,pattern='\\.(csv|json|R|md|txt)$')),
  list.files('R',full.names=TRUE,pattern='\\.R$'),
  list.files('scripts/p15',full.names=TRUE,recursive=TRUE,pattern='\\.(R|py)$'),
  '_targets.R','renv.lock')))
preserved_before <- manifest(preserved_paths)
preservation_file <- file.path(v2,'snapshot/preserved_baseline_manifest.csv')
if(file.exists(preservation_file)) {
  previous <- fread(preservation_file)
  stopifnot(identical(previous$path,preserved_before$path),
    identical(previous$sha256,preserved_before$sha256),all(previous$bytes==preserved_before$bytes))
} else fwrite(preserved_before,preservation_file)
source(file.path(v2,'build_scores.R'))
source(file.path(v2,'prepare_peer_inputs.R'))
source(file.path(v2,'peer_functions.R'))
designs <- data.table(method=c('current','preferred','observed_donors'),
  sources=c('P','PISR','PIS'),scenario=c('observed_only','wgi_bounded','wgi_bounded'),
  input_policy=c('observed_only','conditional_points','conditional_points'),
  rules=c('1;2;3;4;5;6;7;8','1;2;3;4;5','1;2;3;4;5'),
  distance=c('point','guaranteed_interval','guaranteed_interval'),
  role=c('production_all_panel_replay','owner_preferred_candidate','observed_donor_sensitivity'),caliper=3)
fwrite(designs,file.path(out,'designs.csv'))
pp <- list();k <- 0L
for(j in seq_len(nrow(designs))) {
  de <- designs[j];cat('Full-panel method:',de$method,'\n');flush.console()
  xx <- select_shadow(x,de$scenario,de$input_policy)
  stopifnot(nrow(xx)==2743L)
  for(y in 2012:2024) {
    pool <- merge(seeds[sources==de$sources & analysis_year==y,.(iso3,analysis_year,seed_rate,seed_source)],
      xx[analysis_year==y],by=key)
    targets <- xx[analysis_year==y]
    for(i in seq_len(nrow(targets))) {
      t <- targets[i]
      k <- k+1L;pp[[k]] <- one(t,pool,de,FALSE)
      if(t$historical_lmic_reporting_scope && is.finite(t$actual)) {
        k <- k+1L;pp[[k]] <- one(t,pool,de,TRUE)
      }
    }
  }
}
p <- rbindlist(pp)
p <- merge(p,x[,.(iso3,analysis_year,country,historical_income_level,historical_lmic_reporting_scope,
  selected_tier,selected_rate_pct,peer_pool_rule,ordinary_fallback_selection_permitted,actual)],by=key)
# Availability, admissibility, and selection are three distinct fields.
p[,peer_available:=is.finite(estimate)]
p[,peer_eligible_for_selection:=peer_available & !(ordinary_fallback_selection_permitted %in% FALSE)]
p[,selected_peer_new:=mode=='deployment' & selected_tier %in% c('peer','no_eligible_rate') & peer_eligible_for_selection]
stopifnot(!anyDuplicated(p[,.(iso3,analysis_year,method,mode)]),
  all(p[mode=='deployment',.N,by=method]$N==2743L),all(p[peer_available==TRUE,n_peers]>=3L),
  all(p$primary_members+p$ids_members+p$secondary_members+p$rating_implied_members==p$n_peers))
for(i in seq_len(nrow(p))) {
  ids <- strsplit(p$member_ids[i],';',fixed=TRUE)[[1]]
  stopifnot(!p$iso3[i] %in% ids,!anyDuplicated(ids))
}
stopifnot(all(p[method!='preferred',rating_implied_members]==0L),
  all(p[method!='current' & peer_available,rule]<=5L))
dep <- p[mode=='deployment']
# Compare actual stored old peer values over every row, including stronger tiers.
baseline <- merge(dep[method=='current'],core[,.(iso3,analysis_year,
  saved_peer_rate=peer_rate_pct,saved_peer_rule=peer_pool_rule,saved_peer_count=peer_country_count)],by=key)
stopifnot(nrow(baseline)==2743L,
  identical(is.finite(baseline$estimate),is.finite(baseline$saved_peer_rate)),
  all(abs(baseline[peer_available==TRUE,estimate-saved_peer_rate])<1e-10),
  all(rule_map$current_label[baseline$rule[baseline$peer_available]]==baseline$saved_peer_rule[baseline$peer_available]),
  all(baseline$n_peers[baseline$peer_available]==baseline$saved_peer_count[baseline$peer_available]))
bm <- saved_members[used_for_estimate %in% TRUE,.(saved_ids=paste(sort(peer_iso3),collapse=';')),
  by=.(iso3=target_iso3,analysis_year)]
bm <- merge(baseline[peer_available==TRUE],bm,by=key,all.x=TRUE)
stopifnot(all(!is.na(bm$saved_ids)),all(bm$member_ids==bm$saved_ids))
fwrite(baseline[,.(iso3,analysis_year,saved_peer_rate,replayed_peer_rate=estimate,
  saved_peer_rule,replayed_rule=rule,saved_peer_count,replayed_count=n_peers)],file.path(out,'all_panel_baseline_replay.csv'))
# Reproduce the approved diagnostic on exactly its original cases.
diagnostic_path <- file.path(v2,'snapshot/gross_interest_uncertainty_check.csv')
diagnostic <- fread(diagnostic_path)
setnames(diagnostic,'year','analysis_year')
check_diag <- merge(diagnostic,p[method=='preferred',.(iso3,analysis_year,mode,recomputed=estimate)],by=c(key,'mode'))
stopifnot(nrow(check_diag)==nrow(diagnostic),
  identical(is.finite(check_diag$new),is.finite(check_diag$recomputed)),
  all(abs(check_diag[is.finite(new),new-recomputed])<1e-9))
fwrite(p,file.path(out,'all_peer_predictions.csv'))
preferred <- dep[method=='preferred']
stopifnot(preferred[selected_tier=='peer' & historical_lmic_reporting_scope, sum(selected_peer_new)]==731L)
comparison <- merge(core[,.(iso3,analysis_year,old_peer_rate_pct=peer_rate_pct,
  old_peer_rule=peer_pool_rule,old_peer_count=peer_country_count)],
  preferred[,.(iso3,analysis_year,country,historical_income_level,historical_lmic_reporting_scope,
    selected_tier,selected_rate_pct,ordinary_fallback_selection_permitted,
    new_peer_rate_pct=estimate,new_peer_rule=rule,new_peer_count=n_peers,
    peer_available,peer_eligible_for_selection,selected_peer_new,primary_members,ids_members,
    secondary_members,rating_implied_members,donor_shadow_members,target_shadow_used,
    estimate_depends_on_incomplete_input,member_ids,member_sources,member_rates)],by=key)
comparison[,old_peer_available:=is.finite(old_peer_rate_pct)]
comparison[,coverage_transition:=fifelse(old_peer_available & peer_available,'both',
  fifelse(old_peer_available,'old_only',fifelse(peer_available,'new_only','neither')))]
comparison[,rate_change_pp:=new_peer_rate_pct-old_peer_rate_pct]
comparison[,`:=`(candidate_selected_tier=selected_tier,candidate_selected_rate_pct=selected_rate_pct)]
comparison[selected_tier %in% c('peer','no_eligible_rate'),`:=`(
  candidate_selected_tier=fifelse(selected_peer_new,'peer','no_eligible_rate'),
  candidate_selected_rate_pct=fifelse(selected_peer_new,new_peer_rate_pct,NA_real_))]
hi <- !comparison$selected_tier %in% c('peer','no_eligible_rate')
stopifnot(identical(comparison$candidate_selected_tier[hi],comparison$selected_tier[hi]),
  identical(comparison$candidate_selected_rate_pct[hi],comparison$selected_rate_pct[hi]),
  !any(comparison[ordinary_fallback_selection_permitted %in% FALSE,selected_peer_new]))
fwrite(comparison,file.path(out,'country_year_comparison.csv'))
fwrite(comparison[coverage_transition!='both'],file.path(out,'coverage_changes.csv'))
fwrite(comparison[selected_tier=='peer' & !selected_peer_new],file.path(out,'lost_selected_peers.csv'))
# Append candidate fields without overwriting the baseline selection or its metadata.
cs <- merge(sel,comparison[,.(iso3,analysis_year,candidate_peer_rate_pct=new_peer_rate_pct,
  candidate_peer_rule=new_peer_rule,candidate_peer_count=new_peer_count,peer_available,
  peer_eligible_for_selection,selected_peer_new,candidate_selected_tier,candidate_selected_rate_pct)],by=key,all.x=TRUE)
cs[,candidate_authority:='isolated_full_panel_v2_not_promoted']
fwrite(cs,file.path(out,'selected_reference_candidate.csv'))
source(file.path(v2,'summarize.R'))
preserved_after <- manifest(preserved_paths,'preserved_baseline')
# prepare_peer_inputs defines a manifest with roles; compare only stable fields.
stopifnot(identical(preserved_before,preserved_after[,.(path,sha256,bytes)]))
fwrite(data.table(path=preserved_before$path,sha256_before=preserved_before$sha256,
  sha256_after=preserved_after$sha256,unchanged=TRUE),file.path(out,'preservation_check.csv'))
checks <- data.table(check=c('full_panel_2743_rows_per_method','v1_shadow_bounds_exact_replay',
  'conservative_intervals_contain_v1_intervals','all_stored_baseline_peer_rates_rules_counts_replayed',
  'all_stored_baseline_memberships_replayed','prior_conservative_diagnostic_replayed',
  '731_selected_LMIC_peers_reproduced','three_distinct_donors','target_excluded','one_source_per_donor',
  'rating_implied_donors_only_in_preferred_design','first_qualifying_rule_from_unchanged_matching_function',
  'rules_6_to_8_excluded_from_new_designs','all_stronger_selected_tiers_unchanged',
  'fallback_status_holds_preserved','all_baseline_code_and_empirical_files_preserved'),passed=TRUE)
fwrite(checks,file.path(out,'checks.csv'))
code_paths <- c(list.files(v2,full.names=TRUE,pattern='\\.(R|py)$'),
  list.files(file.path(v2,'snapshot'),full.names=TRUE,pattern='\\.R$'),'renv.lock')
fwrite(manifest(code_paths,'code_or_environment'),file.path(out,'code_manifest.csv'))
source_paths <- unique(c(inputs,file.path(v1,c('inputs.csv','scorecard_country_year.csv',
  'economic_resiliency.csv','government_financial_strength.csv','rating_midpoint.csv')),
  diagnostic_path,'scripts/p15/loan_extension/benchmark_matching.R',
  'scripts/p15/loan_extension/loan_valuation.R','R/p15_peer_geography_validation.R'))
fwrite(manifest(source_paths,'input_snapshot'),file.path(out,'input_manifest.csv'))
writeLines(capture.output(sessionInfo()),file.path(out,'r_session_info.txt'))
write_json(list(version='P15-FULL-PANEL-PEER-20260912-V2',authority='isolated_candidate',
  baseline_run_key=frozen_ptr$run_key,years=2012:2024,rows=2743,
  recipe='Rules 1-5; three notches; P>I>S>existing rating-implied donors; conservative gross-interest intervals',
  valuation_rerun=FALSE,production_promoted=FALSE),file.path(out,'run_manifest.json'),pretty=TRUE,auto_unbox=TRUE)
output_paths <- list.files(out,full.names=TRUE,pattern='\\.(csv|json|txt|md)$')
output_paths <- output_paths[basename(output_paths)!='output_manifest.csv']
fwrite(manifest(output_paths,'output'),file.path(out,'output_manifest.csv'))
report_status <- system2('python3',shQuote(file.path(v2,'package_report.py')))
stopifnot(report_status==0L)
cat('Completed full-panel build;',nrow(checks),'checks passed. No valuation rerun or production promotion.\n')
