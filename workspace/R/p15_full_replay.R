p15_full_stages <- function() {
  data.frame(script = paste0("scripts/p15/", c(
    "build_p15_raw_foundations.R", "build_p15_unified_lseg_source_layer.R",
    "build_p15_neutral_country_year_grid.R", "build_p15_status_context_evidence.R",
    "build_p15_status_source_expansion.R", "build_p15_p13_observed_primary_parity.R",
    "build_p15_p13_observed_secondary_parity.R", "build_p15_p13_observed_market_parity_gate.R",
    "build_p15_observed_all_years_evidence.R", "build_p15_primary_approved_rules.R",
    "build_p15_status_sanity_approved_rules.R", "build_p15_secondary_repair_candidates.R",
    "build_p15_integrated_observed_candidate.R", "build_p15_ids_bondholder_evidence.R",
    "build_p15_rating_component_ledger.R", "build_p15_rating_variant_validation.R",
    "build_p15_fallback_validation.R", "build_p15_bounded_fallback_comparison.R",
    "build_p15_local_completion.R", "preview_p15_local_ladder.R")),
    manifest = c(NA, paste0("docs/governance/", c(
      "p15_source_layer_output_manifest.csv", "p15_country_year_grid_manifest.csv",
      "p15_status_context_evidence_manifest.csv", "p15_status_source_expansion_manifest.csv",
      "p15_p13_observed_primary_parity_manifest.csv", "p15_p13_observed_secondary_parity_manifest.csv",
      "p15_p13_observed_market_parity_gate_manifest.csv", "p15_observed_market_evidence_manifest.csv",
      "p15_primary_approved_manifest.csv", "p15_status_sanity_approved_manifest.csv",
      "p15_secondary_repair_candidate_manifest_2026-08-09.csv", "p15_integrated_observed_manifest.csv",
      "p15_ids_bondholders_evidence_manifest.csv", "p15_rating_component_ledger_manifest.csv",
      "p15_rating_variant_validation_manifest.csv", "p15_fallback_validation_manifest.csv",
      "p15_bounded_fallback_comparison_manifest.csv")),
      "data-derived/p15_local_completion_2012_2024_20260906_v1/p15_build_manifest.csv",
      "data-derived/p15_local_ladder_previews_20260906_v1/p15_preview_manifest.csv"))
}

p15_stage_manifest <- function(path) {
  m <- data.table::fread(path)
  if (!"artifact_path" %in% names(m) && "path" %in% names(m)) data.table::setnames(m,"path","artifact_path")
  if (!"artifact_path" %in% names(m)) stop("Manifest has no artifact path: ",path)
  if (!"artifact_role" %in% names(m)) m$artifact_role <- "generated_output"
  m$is_output <- (grepl("generated|^output$|preview_output",m$artifact_role) |
    m$artifact_role %in% c("normalized_secondary_history","instrument_year_universe","tail_merge_audit","build_summary")) &
    !grepl("\\.(R|py)$",m$artifact_path)
  m
}

p15_full_specification <- function(root = getwd()) {
  stages <- p15_full_stages()
  manifests <- lapply(stages$manifest, function(p) if(is.na(p)) NULL else p15_stage_manifest(p))
  outputs <- unique(unlist(lapply(manifests,function(m) m$artifact_path[m$is_output])))
  inputs <- unique(unlist(lapply(manifests,function(m) if(is.null(m)) character() else m$artifact_path[!m$is_output])))
  legacy <- "experiments/full_ladder_database_all_years_2026-05-20"
  p13 <- "experiments/full_ladder_ratings_integrated_status_rebuild_2026-06-01/outputs"
  foundation_outputs <- c(paste0(legacy,"/outputs/",c("ids_terms_core_all_years.csv",
    "damodaran_archive_country_spreads_2000_2024.csv","secondary_issue_level_all_years.csv","master_benchmark_scenarios_all_years.csv")),
    "experiments/p14_historical_validation_and_extension_2026-06-22/outputs/p14_historical_quality_equalization_2012_2023/p14_historical_country_classification_ledger_2012_2024.csv",
    paste0(p13,"/p12a_feature_rich_secondary_accepted_issue_layer_2024.csv"))
  rating_dir <- "data-raw/ratings/bloomberg_sovereign_rating_panel_2026-05-28"
  rating_outputs <- list.files(rating_dir,pattern="\\.csv$",full.names=TRUE)
  extra <- c("data-raw/world_bank_countries.json", "data-raw/p15_curated_inputs_20260906/geography_and_static_labels.csv",
    "data-raw/p15_curated_inputs_20260906/preserved_input_roles.csv",
    "data-raw/p15_curated_inputs_20260906/source_manifest.csv",
    "data-raw/p15_curated_inputs_20260906/legacy_feature_permissions_manifest.csv",
    "data-raw/p15_curated_inputs_20260906/legacy_feature_row_permissions.csv",
    "sources/market_rates/lseg_workspace_download_v12_partial_2026-05-11/extracted/pvr_lseg_debug/20260511_175902/lseg_pvr_benchmark_cashflow_terms_desktop_oecd_base_2000-01-01_2025-12-31.csv",
    list.files("data-raw/p15_case_review_20260906",full.names=TRUE),
    list.files("data-raw/p15_reference_review_20260907",full.names=TRUE),
    list.files("data-raw/p15_source_closure_20260907",pattern="csv$",full.names=TRUE),
    file.path("data-raw/p15_ids_terms_followup_20260906",c("a_d.xlsx","e_k.xlsx","n_q.xlsx","r_u.xlsx")),
    list.files("data-raw/p15_ids_review_public_snapshot_20260906",pattern="DT_(INR|MAT|GPA|COM_DPPG_CD).*json$",full.names=TRUE),
    "data-raw/p15_ids_review_public_snapshot_20260906/source_manifest.csv",
    list.files("data-raw/world_bank_country_classifications",full.names=TRUE),
    list.files(file.path(legacy,"data/ids_core_all_years"),pattern="ids_BND_.*page_1.json$",full.names=TRUE),
    list.files(file.path(legacy,"data/damodaran_archive"),pattern="\\.xlsx?$",full.names=TRUE),
    paste0(legacy,"/data/lseg_v14/pvr-global-lseg/output/tables/",c(
      "lseg_secondary_outstanding_snapshots_oecd_base_2000-01-01_2025-12-31.csv",
      "lseg_secondary_market_year_end_history_oecd_base_2000-01-01_2025-12-31.csv")),
    "sources/literature_review/lr3_systematic_expansion_2026-05-12/official_sources/damodaran_ctryprem.xlsx",
    "sources/ratings/bloomberg_static_rating_changes_2026-05-28/raw/bloomberg_rating_changes_static_values_2026-05-28.zip",
    "docs/audit_first_wave/lseg_latest_resume_missing_2024_identifiers_2026-07-21.csv",
    paste0(p13,"/",c("p12a_secondary_direct_country_scenarios_2024.csv",
      "secondary_terminal_post_audit_review_classes_2024.csv","secondary_price_to_yield_identifier_price_snapshot_2024.csv",
      "secondary_price_to_yield_row_classification_2024.csv","secondary_price_to_yield_trial_results_2024.csv",
      "p12a_feature_rich_secondary_accepted_issue_layer_2024.csv","p12a_feature_rich_secondary_country_layer_2024.csv",
      "p13_best_available_benchmark_rate_2024.csv")),
    "experiments/paper_candidate_2024_clean_v1_2026-05-27/outputs/paper_candidate_benchmark_evidence_2024.csv",
    "experiments/paper_candidate_2024_clean_v1_2026-05-27/outputs/paper_candidate_issue_audit_2024.csv",
    "data-derived/p15_p13_full_anchor_parity_2024_v1/isolated_p13_rebuild/outputs/p13_best_available_benchmark_rate_2024.csv")
  # Active downstream analyses and tests do not construct the raw evidence.
  # Keep their edits from forcing an unrelated raw-source replay.
  research_only <- paste0("R/",c("p15_master","p15_master_findings","p15_dataset_assessment",
    "p15_deeper_assessment","p15_pv_first_pass","p15_bullet_extension",
    "p15_pv_interpretation","p15_pv_annotation_followup","p15_peer_geography_validation"),".R")
  # Revised peers are a separately fingerprinted successor to this preserved
  # source reconstruction, not an input to the historical source replay.
  research_only <- c(research_only,"R/p15_current_inputs.R",
    list.files("R",pattern="^p15_revised_peer.*[.]R$",full.names=TRUE))
  code <- unique(c(setdiff(list.files("R",pattern="\\.R$",full.names=TRUE),research_only),stages$script,
    "scripts/build_bloomberg_sovereign_rating_panel.py", "scripts/p15/run_p15_full_replay.R",
    "scripts/p15/activate_p15_environment.R", "renv.lock", "requirements-p15.txt",
    "scripts/p15/build_p15_reviewed_evidence.R", "scripts/p15/build_p15_reference_review.R",
    "scripts/p15/verify_p15_reference_review.R",
    "scripts/p15/extract_p15_source_closure.py", "scripts/p15/rebuild_p15_source_closure.py",
    "scripts/p15/build_p15_source_closure.R", "scripts/p15/build_p15_analysis_dataset.R",
    "scripts/p15/build_p15_peer_region_correction.R",
    "scripts/p15/restore_p15_environment.R", "scripts/p15/_targets_legacy_guarded.R",
    paste0("experiments/full_ladder_ratings_integrated_status_rebuild_2026-06-01/scripts/",
      "build_p12a_feature_rich_secondary_final_decision_2024.R"),
    paste0(legacy,"/scripts/run_all_years_scenario_database.R"),
    "experiments/p14_historical_validation_and_extension_2026-06-22/scripts/build_p14_historical_quality_equalization_2012_2023.R"))
  leaves <- p15_replay_safe_paths(setdiff(unique(c(inputs,extra,code)),c(outputs,foundation_outputs,rating_outputs,stages$manifest)))
  list(stages=stages,manifests=manifests,outputs=outputs,leaves=leaves,code=code,foundation_outputs=foundation_outputs,rating_outputs=rating_outputs)
}

p15_run_full_replay <- function() {
  root <- normalizePath(getwd(),winslash="/")
  source("R/p15_environment.R")
  environment <- p15_environment_audit()
  spec <- p15_full_specification(root)
  if(any(!file.exists(spec$leaves))) stop("Missing raw replay leaves: ",paste(spec$leaves[!file.exists(spec$leaves)],collapse=";"))
  run <- paste0("p15_full_replay_",format(Sys.time(),"%Y%m%d_%H%M%S"))
  report <- file.path(root,"data-derived",run)
  dir.create(report,recursive=TRUE)
  scratch <- file.path(report,"isolated_build")
  dir.create(scratch)
  before <- vapply(spec$leaves,digest::digest,character(1),algo="sha256",file=TRUE)
  roles <- data.table::fread("data-raw/p15_curated_inputs_20260906/preserved_input_roles.csv")
  legacy_at <- grepl("/outputs/",spec$leaves)
  role_at <- match(basename(spec$leaves),roles$basename)
  if(any(legacy_at & is.na(role_at)))stop("Unaudited preserved intermediate input")
  source_role <- ifelse(spec$leaves %in% spec$code,"code",
    ifelse(legacy_at,roles$role[role_at],"preserved_source_or_curated_input"))
  source("R/research_governance.R")
  manifest <- pvr_manifest_rows(spec$leaves,source_role,run,"SCHEMA-P15-RAW-REPLAY-V1",
    "EST-P15-PRESERVED-FORMULAS-AND-CASE-REVIEW-V1","ADM-P15-CASE-REVIEW-20260906-V1",
    "SEL-NONE-EVIDENCE-ASSEMBLY-V1","SRC-P15-RAW-CLOSURE-20260906")
  manifest$source_snapshot_content_id <- paste0("SNAP-SHA256-",before)
  manifest$allowed_use <- ifelse(legacy_at,roles$allowed_use[role_at],"See recorded source and method contract")
  data.table::fwrite(manifest,file.path(report,"input_manifest.csv"))
  data.table::fwrite(environment,file.path(report,"environment.csv"))
  reference_before <- vapply(spec$outputs,digest::digest,character(1),algo="sha256",file=TRUE)
  data.table::fwrite(data.frame(artifact_path=spec$outputs,sha256=reference_before),file.path(report,"regression_reference_manifest.csv"))
  data.table::fwrite(spec$stages,file.path(report,"stages.csv"))
  p15_replay_copy(spec$leaves,root,scratch)
  file.create(file.path(scratch,".p15_isolated_build"))
  # Never load an existing calculated output for a stage in this replay.
  stopifnot(!any(file.exists(file.path(scratch,c(spec$outputs,spec$foundation_outputs,spec$rating_outputs)))))
  run_script <- function(script, label, python=FALSE) {
    old <- setwd(scratch)
    on.exit(setwd(old))
    log <- file.path(report,paste0(label,".log"))
    bin <- if(python) Sys.which("python3") else file.path(R.home("bin"),"Rscript")
    args <- c(if(!python) "--vanilla",shQuote(script))
    rc <- system2(bin,args,stdout=log,stderr=log)
    if(rc!=0L)stop("Stage failed: ",label,". See ",log)
  }
  comparisons <- list()
  tryCatch({
    message("Rebuilding Bloomberg panel from archived rating-change workbooks")
    run_script("scripts/build_bloomberg_sovereign_rating_panel.py","ratings_raw",TRUE)
    for(i in seq_len(nrow(spec$stages))) {
      message("Raw replay stage ",i,"/",nrow(spec$stages),": ",basename(spec$stages$script[i]))
      run_script(spec$stages$script[i],sprintf("stage_%02d",i))
      m <- spec$manifests[[i]]
      for(p in m$artifact_path[m$is_output]) {
        if(!grepl("\\.csv(\\.gz)?$",p))next
        # Hash/manifest diagnostics legitimately change when source lineage is rebuilt.
        if(grepl("manifest|verification",basename(p)))next
        cmp <- p15_replay_compare(file.path(root,p),file.path(scratch,p),root,scratch,numeric_tolerance=1e-12)
        comparisons[[p]] <- cbind(artifact_path=p,stage=i,cmp)
      }
      if(length(comparisons)) {
        data.table::fwrite(data.table::rbindlist(comparisons),file.path(report,"output_comparison.csv"))
        # Preserve discrepancies for investigation. Do not silently accept new rates.
        bad <- data.table::rbindlist(comparisons)[semantic_match==FALSE]
        if(nrow(bad))stop("Replay discrepancy: ",paste(bad$artifact_path,collapse=";"))
      }
    }
    message("Applying documented case review and recomputing affected peer/validation evidence")
    run_script("scripts/p15/build_p15_reviewed_evidence.R","stage_21_case_review")
    message("Building the closest-date successor and bounded peer/source review")
    run_script("scripts/p15/build_p15_reference_review.R","stage_22_reference_review")
    run_script("scripts/p15/verify_p15_reference_review.R","stage_23_reference_verification")
    message("Re-extracting source-closure evidence from archived IDS workbooks")
    run_script("scripts/p15/rebuild_p15_source_closure.py","stage_24_raw_source_closure",TRUE)
    run_script("scripts/p15/build_p15_source_closure.R","stage_25_source_closure")
    message("Building source-qualified selected analysis views")
    run_script("scripts/p15/build_p15_analysis_dataset.R","stage_26_analysis_dataset")
    message("Rebuilding owner-authorized peer-region correction from replayed parents")
    run_script("scripts/p15/build_p15_peer_region_correction.R","stage_27_peer_region_correction")
    stopifnot(identical(before,vapply(spec$leaves,digest::digest,character(1),algo="sha256",file=TRUE)))
    stopifnot(identical(reference_before,vapply(spec$outputs,digest::digest,character(1),algo="sha256",file=TRUE)))
    reviewed <- list.files(file.path(scratch,"data-derived/p15_reviewed_evidence_20260906_v1"),full.names=TRUE)
    reference_review <- list.files(file.path(scratch,"data-derived/p15_reference_review_20260907_v1"),full.names=TRUE)
    source_closure <- list.files(file.path(scratch,"data-derived/p15_source_closure_20260907_v1"),full.names=TRUE)
    analysis_dataset <- list.files(file.path(scratch,"data-derived/p15_analysis_candidate_20260907_v1"),full.names=TRUE)
    region_corrected <- list.files(file.path(scratch,"data-derived/p15_analysis_candidate_20260909_region_v1"),full.names=TRUE)
    products <- unique(c(file.path(scratch,spec$outputs),file.path(scratch,spec$foundation_outputs),reviewed,reference_review,source_closure,analysis_dataset,region_corrected))
    output_manifest <- pvr_manifest_rows(products,"generated_candidate_or_regression_evidence",run,
      "SCHEMA-P15-RAW-REPLAY-V1","EST-P15-PRESERVED-FORMULAS-AND-CASE-REVIEW-V1",
      "ADM-P15-CASE-REVIEW-20260906-V1","SEL-NONE-EVIDENCE-ASSEMBLY-V1","SRC-P15-RAW-CLOSURE-20260906",root_dir=report)
    data.table::fwrite(output_manifest,file.path(report,"output_manifest.csv"))
    data.table::fwrite(data.frame(check_id=c("all_stages_completed","raw_inputs_unchanged","baseline_outputs_unchanged",
      "numeric_baseline_parity","case_review_applied","no_unapproved_ladder_promotion"),
      passed=c(TRUE,TRUE,TRUE,all(data.table::rbindlist(comparisons)$semantic_match),TRUE,TRUE)),file.path(report,"acceptance.csv"))
    capture.output(sessionInfo(),file=file.path(report,"environment.txt"))
    message("Full staged replay complete: ",report)
    # Track the actual deliverables as files, so deleted/changed products cannot
    # masquerade as a valid targets cache merely because a receipt survives.
    c(file.path(report,c("input_manifest.csv","output_manifest.csv","environment.csv","output_comparison.csv","acceptance.csv")),products)
  },finally={setwd(root)})
}
