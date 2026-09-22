# Reporting is intentionally separate from calculation. It never edits research prose.
p15_master_csv_signature <- function(path) {
  x<-data.table::fread(path)
  ignore<-grep("^(build_id|schema_id|schema_version|estimator_id|admissibility_id|selection_id|source_package_ids|source_snapshot_id)$",names(x),value=TRUE)
  ignore<-unique(c(ignore,grep("(^|_)(path|file|directory)$",names(x),value=TRUE)))
  if(length(ignore))x[,(ignore):=NULL]
  # Preserve the table/record identity in compound provenance while ignoring the
  # cache directory. Exact source and output locations remain in the manifests.
  for(n in intersect(c("lineage_parent_ids","parent_pair_locator"),names(x))) {
    values<-vapply(x[[n]],function(value) {
      if(is.na(value))return(NA_character_)
      parts<-strsplit(value,";",fixed=TRUE)[[1]]
      locations<-grepl("^/|^data-derived/",parts)
      parts[locations]<-basename(parts[locations])
      paste(parts,collapse=";")
    },character(1))
    data.table::set(x,j=n,value=values)
  }
  for(n in names(x))if(is.double(x[[n]]))data.table::set(x,j=n,value=signif(x[[n]],12))
  digest::digest(as.data.frame(x),algo="sha256")
}
p15_master_table_changes <- function(id,current,previous) {
  paths<-function(d)if(dir.exists(d))list.files(d,pattern="[.]csv$",full.names=FALSE) else character()
  files<-union(paths(current),paths(previous))
  files<-files[!grepl("manifest|environment|checks|verification|build_contract",files)]
  data.table::rbindlist(lapply(files,function(f) {
    a<-file.path(previous,f);b<-file.path(current,f)
    old<-if(file.exists(a))p15_master_csv_signature(a) else NA_character_
    new<-if(file.exists(b))p15_master_csv_signature(b) else NA_character_
    data.table::data.table(analysis=id,table=f,changed=!identical(old,new),previous=previous,current=current)
  }))
}
p15_master_result_fingerprint <- function(dir) {
  files<-list.files(dir,pattern="[.]csv$",full.names=TRUE)
  files<-files[!grepl("manifest|environment|checks|verification|build_contract",basename(files))]
  digest::digest(setNames(vapply(files,p15_master_csv_signature,character(1)),basename(files)),algo="sha256")
}
p15_master_note_alerts <- function(registry,review,results) {
  z<-merge(registry,results,by="analysis",all.x=TRUE,sort=FALSE)
  z<-merge(z,review,by=c("analysis","note"),all.x=TRUE,sort=FALSE)
  z[,review_required:=status=="live" & (is.na(reviewed_fingerprint)|result_fingerprint!=reviewed_fingerprint)]
  z
}
p15_master_markdown_table <- function(x,digits=3) {
  x<-as.data.frame(x)
  if(!nrow(x))return("No observations in this slice.")
  for(n in names(x)) {
    if(is.numeric(x[[n]]))x[[n]]<-format(round(x[[n]],digits),trim=TRUE,scientific=FALSE)
    x[[n]]<-gsub("|","/",as.character(x[[n]]),fixed=TRUE)
  }
  c(paste0("| ",paste(names(x),collapse=" | ")," |"),
    paste0("| ",paste(rep("---",ncol(x)),collapse=" | ")," |"),
    apply(x,1,function(r)paste0("| ",paste(r,collapse=" | ")," |")))
}
p15_master_report <- function(config,raw,done) {
  pointer<-file.path(config$cache_root,"current_run.json")
  previous<-if(file.exists(pointer))jsonlite::fromJSON(pointer,simplifyVector=FALSE) else NULL
  changes<-data.table::rbindlist(lapply(names(done),function(id) {
    old<-if(!is.null(previous$stages[[id]]))previous$stages[[id]]$dir else p15_master_reference(config,id)
    p15_master_table_changes(id,done[[id]]$dir,old)
  }),fill=TRUE)
  changes<-data.table::rbindlist(list(p15_master_table_changes("dataset",file.path(raw$dir,"candidate"),
    if(!is.null(previous))previous$candidate else config$reference_candidate),changes))
  dirs<-c(dataset=file.path(raw$dir,"candidate"),vapply(done,`[[`,character(1),"dir"))
  results<-data.table::data.table(analysis=names(dirs),directory=unname(dirs),
    result_fingerprint=vapply(dirs,p15_master_result_fingerprint,character(1)))
  registry<-data.table::fread(config$notes_registry)
  stopifnot(all(file.exists(registry$note)),all(registry$analysis %in% results$analysis))
  review<-data.table::fread(config$notes_review_receipt,colClasses="character")
  alerts<-p15_master_note_alerts(registry,review,results)
  report_key<-p15_master_key(c(config$config_path,config$analysis_registry,config$version_registry,config$notes_registry,
    config$notes_review_receipt,"R/p15_master_findings.R",unique(registry$note)),
    list(raw=raw$key,stages=lapply(done,`[[`,"key"),previous=previous$run_key))
  out<-file.path(config$cache_root,paste0("report_",format(Sys.time(),"%Y%m%d_%H%M%S"),"_",substr(report_key,1,8)))
  stopifnot(!dir.exists(out));dir.create(out)
  data.table::fwrite(changes,file.path(out,"changed_tables.csv"))
  data.table::fwrite(results,file.path(out,"current_outputs.csv"))
  data.table::fwrite(alerts,file.path(out,"notes_review.csv"))
  read<-function(id,file)data.table::fread(file.path(dirs[[id]],file))
  selected<-read("dataset","selected_reference.csv")
  selected<-selected[historical_lmic_reporting_scope==TRUE]
  annual<-read("deeper","annual_composition.csv")
  regional<-read("regional","regional_yoy.csv")
  pools<-read("deeper","shared_pool_summary.csv")
  modern<-read("official_policy","by_era.csv")
  standardized_benchmark<-read("official_policy","paired_summary.csv")
  pv<-read("pv_interpretation","summary_period.csv")
  finding<-c("# Current numerical findings — generated, not an interpretation",
    "",paste0("Configuration: `",config$version,"`. The dataset includes restricted vendor evidence and is used for internal research; it is not a public release."),
    "","Borrowing rates are percentages; differences between rates are percentage points. The selected view follows the accepted order of eligible available tiers, including an optional weak peer fallback.",
    "These tables keep the population and comparison basis visible. A change in a generated number does not automatically justify changing the thesis argument.",
    "","## Selected evidence in historical low- and middle-income country-years","",
    p15_master_markdown_table(selected[,.N,by=selected_tier]),
    "","## Annual movements: changing membership versus continuing countries","",
    "Equal-country means; the peer-inclusive series is not a homogeneous observed-market time series.","",
    p15_master_markdown_table(annual[,.(analysis_year,common_n,total_mean_change,common_country_change)]),
    "","## Regional comparisons in 2022 and 2024","",
    "Selected includes peers; no_peer excludes them; observed uses primary or secondary evidence. Each row holds countries constant only within that year-on-year comparison.","",
    p15_master_markdown_table(regional[scope=="historical_LMIC" & analysis_year %in% c(2022,2024) & view %in% c("selected","no_peer","observed"),.(region,view,analysis_year,n,mean_change)]),
    "","## Shared peer information","",
    paste0("The selected peer recipients share ",sum(pools$unique_pools)," year-specific membership sets. Repeated estimates are not independent new market observations."),
    "","## Policy comparisons by period","",
    "Absolute discrepancy is measured against observed primary issuance on matched samples. IDS is source consistency, not independent predictive performance. Grouped DAC-rate scenarios are not a reconstruction of every creditor's actual reporting.","",
    p15_master_markdown_table(modern[policy=="dac_modern_2018",.(tier,
      period=data.table::fifelse(era=="2017-2020","2018-2020",era),n,tier_mae,policy_mae,mean_improvement)]),
    "","## Standardized benchmark comparison over 2012-2024","",
    "These existing matched comparisons apply the 9/7/6% rule to each year's recorded DAC category throughout the full period. They are distinct from the historical-headline series that uses 10% before 2018. Positive mean improvement means a smaller absolute discrepancy from observed primary issuance. The rows have different overlap samples, and their descriptive improvements do not inherit statistical conclusions from the modern-only analysis.","",
    p15_master_markdown_table(standardized_benchmark[policy=="dac_group976",
      .(source=data.table::fifelse(tier=="moodys","rating-implied",tier),n,countries,
        tier_mae,policy_mae,mean_improvement)]),
    "","## Present-value scenarios by period","",
    "These value the same stylized repayments using different discount rates. Recorded interest is held constant; these are not verified floating-rate contracts or cash savings.","",
    p15_master_markdown_table(pv[,.(evidence_view,period,n,mean_market_ge,mean_policy_ge,mean_difference)]),
    "","All full tables, figures, case-level outputs and exact paths are indexed in `current_outputs.csv` beside this note.")
  if("benchmark_inference" %in% names(dirs)) {
    inference<-read("benchmark_inference","paired_loss_inference.csv")
    changes_detail<-read("benchmark_inference","consecutive_summary.csv")
    display_tier<-function(x)data.table::fifelse(x=="moodys","rating-implied",x)
    finding<-c(finding,"","## Additional paired benchmark comparisons","",
      "Each row compares absolute discrepancies from the same observed primary issuance rate. Positive improvement means the focal source is closer on average. The September 11 statistical audit recommends emphasizing descriptive losses and sample support: only seven modern year clusters are available, and shared model inputs and changing coverage complicate inference. The original country intervals, adjusted p-values and country/year sensitivities remain in the full numerical tables; they are not conclusive population evidence or a basis for automatic ladder reordering.","",
      p15_master_markdown_table(inference[sample_view=="full_validation" &
        family %in% c("modern_tier_vs_dac","modern_pairwise_tiers"),
        .(family,focal=display_tier(focal),comparator=display_tier(comparator),n,countries,
          focal_mae_pp,comparator_mae_pp,improvement_pp=estimate_pp)]),
      "","## Annual levels and changes answer different questions","",
      "Each row uses consecutive observed borrower-years and withholds primary issuance from the fallback. Better annual rate levels can coexist with a less accurate year-on-year change. Endpoint errors and change errors below use exactly the same transitions; shared endpoints repeat when consecutive transitions overlap.","",
      p15_master_markdown_table(changes_detail[sample_view=="full_validation",
        .(source=display_tier(focal),n_transitions,countries,focal_endpoint_mae_pp,dac_endpoint_mae_pp,
          focal_change_mae_pp,dac_change_mae_pp,both_levels_closer_but_change_worse_n)]))
  }
  if("repayment_influence" %in% names(dirs)) {
    influence<-read("repayment_influence","sensitivity_summary.csv")
    finding<-c(finding,"","## Influence on creditor-ranking reversals","",
      "A reversal means the creditor with higher recorded interest has the higher modeled grant-element analogue under the same borrower-year discount rate. Existing restrictions have already been applied. Omitting reviewed IDS proxies or Brazil 2020's unresolved zero-interest context is an influence diagnostic, not an accepted exclusion. These comparisons change the sample and do not re-estimate repayment terms.","",
      p15_master_markdown_table(influence[,.(view,n_pairs,n_reversals,median_reversal_abs_ge_gap,
        mean_reversal_abs_ge_gap)]),
      "","The full case audit, recorded restrictions and country/pair omission results remain beside these summary tables.")
  }
  if("loan_comparisons" %in% names(dirs)) {
    loan_coverage<-read("loan_comparisons","sample_coverage.csv")
    loan_summary<-read("loan_comparisons","comparison_summary.csv")
    loan_repayment<-read("loan_comparisons","repayment_reconciliation_summary.csv")
    finding<-c(finding,"","## External loan comparisons on matched records","",
      "The new loan exercise uses the original MPG archive, later AidData records and the African Debt Database. Each difference values identical normalized repayments with a borrower benchmark and the stated reference rate. Main non-peer views exclude records whose selected rate is only a peer estimate; the full selected view retains them as a sensitivity. These samples do not change the platform ladder.","",
      p15_master_markdown_table(loan_coverage[main_cohort==TRUE,
        .(dataset,cohort,source_records,source_eligible,matched_records,nonpeer_matched,peer_only_matched)]),
      "","Grant elements are percentages of normalized principal; differences are percentage points. Means below give each source record equal weight. Event and amount weighting, individual tiers, borrower scope and source-quality flags are separate full-table views. MPG and broad ADD comparisons require explicit common-USD scenarios; source currency and constant-rate assumptions remain visible in the loan records. The modern DAC comparison is a common reference-rate exercise, not certification of each loan's actual ODA reporting.","",
      p15_master_markdown_table(loan_summary[main_cohort==TRUE &
        benchmark_view %in% c("non_peer","all_selected") &
        reference %in% c("fixed5","modern_DAC_common_reference"),
        .(dataset,cohort,benchmark_view,reference,records,countries,
          mean_reference_ge_pct,mean_market_ge_pct,mean_delta_ge_pp)]),
      "","## Standardized category-rate comparison across available years","",
      "The standardized comparison applies the same 9/7/6% category rule throughout each dataset's available years. The rate follows the borrower's recorded category, so a category change can still change that country's rate. Applying the rule before 2018 is an analytical comparison, not a claim about the historical headline ODA convention. These means retain equal record weights and the same normalized loans and market benchmarks.","",
      p15_master_markdown_table(loan_summary[main_cohort==TRUE &
        benchmark_view %in% c("non_peer","all_selected") &
        reference=="standardized_DAC_category_rule",
        .(dataset,cohort,benchmark_view,records,countries,
          mean_reference_ge_pct,mean_market_ge_pct,mean_delta_ge_pp)]),
      "","The historically labelled comparison remains separate and uses 10% before 2018. The original policy fields and their values are retained alongside explicit standardized-category fields. The fixed-5% comparison and modern-DAC period inference remain unchanged.",
      "","## MPG repayment reconstruction at the same 5% rate","",
      "World Bank total horizons are reconstructed from the archived dates, with single principal endpoints treated as bullets. This table changes the repayment definition while holding the discount rate at 5%; its difference must remain separate from the borrower-rate effect above. Original source fields and the two agreement-anchor timing alternatives remain in the case outputs.","",
      p15_master_markdown_table(loan_repayment[dataset=="mpg",
        .(creditor,records,mean_source_saved5_pct,mean_reconstructed5_pct,
          mean_reconstruction_delta_pp,mean_explicit_at5_delta_pp)]))
  }
  if("loan_period_inference" %in% names(dirs)) {
    period_primary<-read("loan_period_inference","primary_country_inference.csv")
    period_year<-read("loan_period_inference","shared_year_inference.csv")
    period_composition<-read("loan_period_inference","composition_sensitivities.csv")
    finding<-c(finding,"","## Modern loan comparisons across periods","",
      "Each gap is the grant element using the borrower benchmark minus the grant element of the same loan using the modern DAC reference. The contrast compares the mean gap in 2022-2024 with 2018-2021; AidData's later records end in 2023. The non-peer main cohort is fixed by the preceding loan analysis. The two periods contain different loan records, so the contrast is descriptive and can reflect borrowing terms, countries, benchmark tiers and market conditions.","",
      "The main description reports effect sizes and actual support. The original country-jackknife intervals and adjusted p-values are preserved in the full tables, but are not emphasized here after the statistical audit of small time samples and uneven country contributions.","",
      p15_master_markdown_table(period_primary[,.(dataset,records,early_records,late_records,
        countries,common_countries,early_mean_pp,late_mean_pp,effect_pp)]),
      "","Shared-year uncertainty is a separate sensitivity. AidData has only six observed calendar years and two later years; ADD has seven and three. The saved variance envelope cannot guarantee reliable small-sample inference or cover arbitrary persistent global shocks across different countries and years. These comparisons do not identify a causal effect or propagate all benchmark, loan-term and selection uncertainty.","",
      p15_master_markdown_table(period_year[method=="shared_year_variance_envelope",
        .(dataset,years,early_years,late_years,effect_pp)]),
      "","Composition and currency sensitivities help assess what is driving the difference. Changing weights changes the target mean; restricting to common countries changes the sample. These are not additional independent replications.","",
      p15_master_markdown_table(period_composition[view %in% c("main",
        "common_countries_record_weighted","common_countries_equal_country_weight",
        "reported_USD","exclude_flagged_source_imputation","exclude_uncertain_zero_rates"),
        .(dataset,view,records,countries,early_mean_pp,late_mean_pp,effect_pp)]))
  }
  if("statistical_review" %in% names(dirs)) {
    error_metrics<-read("statistical_review","benchmark_error_metrics.csv")
    stable<-read("statistical_review","tier_stability_summary.csv")
    annual_loans<-read("statistical_review","existing_loan_annual_summary.csv")
    fixed<-read("statistical_review","fixed_method_period_contrasts.csv")
    finding<-c(finding,"","## Error size, signed direction and tail sensitivity","",
      "MAE measures the average size of a discrepancy; signed errors can cancel and RMSE gives more weight to large discrepancies. Rows below retain each tier's own matched primary/DAC sample and cannot independently rank tiers against one another.","",
      p15_master_markdown_table(error_metrics[scenario=="modern2018_2024",
        .(tier,records,countries,focal_bias_pp,focal_mae_pp,comparator_mae_pp,focal_rmse_pp,comparator_rmse_pp)]),
      "","## Stable tier availability across every modern year","",
      "Availability means existing eligible and ordinary-usable evidence; it does not mean the hierarchy selected that tier. This table uses countries within historical LMIC scope in every 2018-2024 year. Stable tier labels do not guarantee stable timing, maturity or a large primary-validation panel.","",
      p15_master_markdown_table(stable[scope=="historical_LMIC_every_year",
        .(tier,countries_in_scope,available_all_seven,selected_all_seven,primary_validation_all_seven)]),
      "","## Annual loan gaps without concealing small yearly samples","",
      p15_master_markdown_table(annual_loans[reference=="modern_DAC_common_reference"&benchmark_view=="non_peer",
        .(dataset,commitment_year,records,countries,mean_delta_ge_pp)]),
      "","## Fixed benchmark methods on identical loan support","",
      "Selected and fixed-method contrasts within each row use the same loan records, repayments, policy rates and equal-record weights. Early and late portfolios remain different loans. The method-specific rows have different supports; the all-three intersection is saved separately. ADD retains a positive contrast under each fixed method. AidData's direction changes with support and method, so its period comparison should not be used as independent temporal corroboration.","",
      p15_master_markdown_table(fixed[support=="method_available_on_main_nonpeer_loans",
        .(dataset,tier,early_records,late_records,common_countries,selected_contrast_pp,fixed_contrast_pp,method_component_pp)]),
      "","The September 11 audit is descriptive and adds no p-values. Country and year clustering estimate covariance, not identified economic shocks or fixed effects. All preserved inference remains accessible in its original tables.")
  }
  if("regional_followup" %in% names(dirs)) {
    period_rates<-read("regional_followup","regional_period_rates.csv")
    finding<-c(finding,"","## Regional borrowing-rate levels and periods","",
      "These are descriptive means of each country's available period rates, then equal-country means within regions. Main views exclude peer-only selected rates. Stable tier availability, common-country contrasts and individual sources are saved separately; issuance and year-end secondary yields have different timing.","",
      p15_master_markdown_table(period_rates[restriction=="recorded_secondary_holds"&view=="no_peer",
        .(region,period,countries,country_years,mean_rate_pct)]))
  }
  if("crs_application" %in% names(dirs)) {
    crs_summary<-read("crs_application","comparison_summary.csv")
    crs_period<-read("crs_application","period_summary.csv")
    finding<-c(finding,"","## OECD CRS dated-term application","",
      "Eligible government financing records have usable dated repayments, matched borrower benchmarks and reported numeric interest held constant in the calculation. Each financing record is counted once within a view; source activities and annual records are shown separately. CRS currency is provider reporting currency; loan denomination is unverified, so this is a conditional USD-reference comparison. It does not replicate each provider's official ODA calculation. The full eligibility and exclusion tables retain the wider source denominator.","",
      p15_master_markdown_table(crs_summary[benchmark_view=="non_peer",
        .(reference,financing_records,source_activities,countries,providers,mean_reference_ge_pct,mean_market_ge_pct,mean_delta_ge_pp)]),"",
      p15_master_markdown_table(crs_period[benchmark_view=="non_peer"&reference=="standardized_DAC_category_rule",
        .(period,financing_records,countries,providers,mean_delta_ge_pp)]))
  }
  if("crs_companions" %in% names(dirs)) {
    geography<-read("crs_companions","geographic_coverage.csv")
    overlap<-read("crs_companions","hard_WB_overlap_summary.csv")
    finding<-c(finding,"","## CRS geographic coverage and identified World Bank overlap","",
      "Country and provider coverage, strict government and schedule sensitivities, and constant-provider annual views are available in the companion tables. Identified World Bank records overlapping earlier sources are linked for traceability; the source samples are not independent replications.","",
      p15_master_markdown_table(geography),"",p15_master_markdown_table(overlap))
  }
  if("crs_modern_summary" %in% names(dirs)) {
    crs_modern<-read("crs_modern_summary","modern_summary.csv")
    crs_modern_period<-read("crs_modern_summary","period_summary.csv")
    finding<-c(finding,"","## CRS application in the central 2018-2024 period","",
      "This supplement reuses the completed loan calculations and fixes the modern reporting period. The standardized 9/7/6% comparison is the main analytical reference; 5% is a separate comparator. Main views exclude peer-only benchmarks. CRS reports provider accounting currency rather than verified loan denomination, and numeric interest is held constant, so these remain conditional USD-reference valuations.","",
      p15_master_markdown_table(crs_modern[benchmark_view=="non_peer",
        .(weighting,financing_records,countries,providers,mean_market_ge_pct,
          mean_standardized_ge_pct,mean_market_minus_standardized_ge_pp,
          mean_market_pvr_per100,mean_standardized_pvr_per100)]),"",
      "PVR per 100 means discounted modeled repayments per 100 of normalized lending. It is not nominal repayments, a budget saving or cash actually transferred. Amount weighting changes the target average and does not improve representativeness by itself.","",
      p15_master_markdown_table(crs_modern_period[benchmark_view=="non_peer" &
        weighting=="financing_record_equal",.(period,financing_records,countries,providers,
          mean_market_minus_standardized_ge_pp)]))
  }
  if("gbohoui_context" %in% names(dirs)) {
    context_models<-read("gbohoui_context","model_summary.csv")
    finding<-c(finding,"","## Institutional context in observed borrowing yields","",
      "These prespecified descriptive models use observed primary issuance or secondary USD yields, with no rating-implied or peer outcomes. Regulatory-quality coefficients are yield percentage points per WGI estimate unit, not per point on a 0-100 index. Every specification within a source and period uses the same matched observations. Year/income and country/year effects summarize different variation; neither establishes a causal effect.","",
      p15_master_markdown_table(context_models[indicator=="regulatory_quality" &
        specification %in% c("year_income","country_year"),
        .(view,period,specification,records,countries,years,slope_rate_pp,
          deletion_min_slope,deletion_max_slope)]),"",
      "Country-deletion ranges are sensitivity results, not confidence intervals. Observed borrowers are selective, revisions affect the historical WGI estimates, and within-country models retain little score variation. Budget-transparency models are preserved in supporting tables: only four comparable survey rounds are available, and several within-country signs change under country deletion. No p-values, causal explanation of regional gaps or new benchmark tier are claimed.")
  }
  if("panel_disbursement" %in% names(dirs)) {
    disbursement<-read("panel_disbursement","summary_by_period.csv")
    finding<-c(finding,"","## Aggregate-panel disbursement identity check","",
      "All 1,498 eligible borrower-year-creditor scenarios retain their original rates and repayment schedules. Six illustrative draw profiles shift each identical full-tenor tranche and its lending inflow together. Under a flat discount rate, the same discount factor multiplies both present values, so their ratio, ratio-based grant-element analogue and corresponding market-minus-policy gap are algebraically unchanged. This ratio divides by discounted advances; discounted repayments per 100 nominal committed do change. Direct cash-flow replay verifies the identity for every case, including fractional maturities and the 21 approved bullets.","",
      p15_master_markdown_table(disbursement[profile_id=="uniform_3y",
        .(reference,period,scenarios,countries,bullet_scenarios,
          mean_baseline_pvr_pct,mean_tranche_pvr_pct,maximum_absolute_pvr_change_pp)]),"",
      "This closes a specific modeled panel check. It does not establish that actual disbursement dates, fixed contractual final maturities, fees, changing rates or tranche terms are harmless. The component present values change even when the ratio does not. Earlier years retain the standardized category reference, not a claim to historical headline ODA accounting; the separate timing bridge must not be described as a disbursement effect.")
  }
  writeLines(finding,file.path(out,"CURRENT_FINDINGS.md"))
  pending<-alerts[review_required==TRUE]
  lines<-c("# Master refresh report","",paste0("Configuration: ",config$version),
    "","The raw-to-candidate build and the registered active analyses completed successfully. Existing snapshots and historical experiments were preserved.",
    "",paste0(sum(changes$changed)," of ",nrow(changes)," research tables have changed contents after removing run-location metadata, compared with the previous successful run (or the registered comparison snapshots on the first run)."),
    paste0(nrow(pending)," note/analysis links still require explicit review. An unchanged rerun does not clear these notices."),
    "","## Changed tables","",p15_master_markdown_table(changes[changed==TRUE,.(analysis,table)]),
    "","## Notes requiring review","",p15_master_markdown_table(pending[,.(analysis,note)]),
    "","## What this does and does not establish","",
    "- Raw/source hashes, preserved case decisions and baseline parity are checked by the raw replay; cached outputs are hash-verified before reuse.",
    "- The independent present-value checks and three-block arithmetic decomposition run on current inputs. The older separate replay comparison is not claimed by that check stage.",
    "- Figures are rebuilt by their owning stage. Table comparisons ignore run identifiers and filesystem locations, round numeric values to 12 significant digits, and retain substantive rates, selections and peer membership.",
    "- Historical calibration, alternative-method and old-versus-new correction experiments remain fixed evidence; they are not silently recalculated with a different sample.",
    "- Alerts cover the registered notes, not every Markdown sentence in the repository. Unregistered notes are not certified current.",
    "- No finding, method or canonical dataset is automatically promoted; interpretation requires explicit review and methodological decisions retain their owner-approval requirements.",
    "","See `CURRENT_FINDINGS.md`, `current_outputs.csv`, `changed_tables.csv` and `notes_review.csv` in this report directory.")
  writeLines(lines,file.path(out,"REPORT.md"))
  state<-list(configuration=config$version,run_key=report_key,report=normalizePath(out),
    candidate=dirs[["dataset"]],raw_key=raw$key,stages=lapply(done,function(r)list(key=r$key,dir=r$dir)))
  jsonlite::write_json(state,file.path(out,"run.json"),auto_unbox=TRUE,pretty=TRUE)
  source("R/research_governance.R")
  bundle<-list(build_id=basename(out),schema_id="SCHEMA-P15-MASTER-REFRESH-V1",
    estimator_id="EST-P15-REGISTERED-STAGE-REFRESH-V1",admissibility_id="ADM-P15-SOURCE-CLOSURE-V1",
    selection_id="SEL-P15-REFERENCE-REGION-CORRECTED-V1",
    source_package_ids="SRC-P15-RAW-CLOSURE-20260906;SRC-P15-SOURCE-CLOSURE-20260907;SRC-WB-COUNTRIES-LOCAL-PEER-20260909")
  manifest<-function(paths,role)do.call(pvr_manifest_rows,c(list(paths=paths,artifact_role=role),bundle))
  inputs<-c(config$config_path,config$raw_snapshot_manifest,config$analysis_snapshot_manifest,
    config$analysis_registry,config$version_registry,config$notes_registry,config$notes_review_receipt,
    file.path(raw$dir,"receipt.rds"),vapply(done,function(r)file.path(r$dir,"receipt.rds"),character(1)))
  master_code<-c("R/p15_master.R","R/p15_master_findings.R","R/research_governance.R",
    "_targets.R","scripts/p15/run_p15_master.R","renv.lock")
  # Keep the reviewed prose/config/code as it stood in this run. Editing a live
  # review receipt or note must not destroy the evidence behind an older report.
  stage_files<-unique(unlist(lapply(done,function(r)r$inputs$path)))
  context<-unique(c(inputs[!grepl("receipt[.]rds$",inputs)],master_code,registry$note,
    stage_files[grepl("[.](R|py|md)$|renv[.]lock$",stage_files)]))
  archived<-vapply(context,function(p) {
    if(grepl("^/|[.][.]/",p))stop("Context snapshot requires a project-relative path: ",p)
    dest<-file.path(out,"context",p);dir.create(dirname(dest),recursive=TRUE,showWarnings=FALSE)
    stopifnot(file.copy(p,dest));normalizePath(dest)
  },character(1))
  data.table::fwrite(data.table::data.table(original_path=context,archived_path=archived),file.path(out,"context_index.csv"))
  input_paths<-ifelse(inputs %in% context,archived[match(inputs,context)],inputs)
  data.table::fwrite(manifest(input_paths,"registered_inputs_and_parent_receipts"),file.path(out,"input_manifest.csv"))
  data.table::fwrite(manifest(archived[match(master_code,context)],"master_code"),file.path(out,"code_manifest.csv"))
  jsonlite::write_json(c(bundle,list(release_state="private_working_candidate_not_canonical",
    analysis_source_manifest=config$analysis_snapshot_manifest,stage_estimators="inherited_without_method_change")),
    file.path(out,"build_contract.json"),auto_unbox=TRUE,pretty=TRUE)
  writeLines(capture.output(sessionInfo()),file.path(out,"environment.txt"))
  data.table::fwrite(manifest(list.files(out,full.names=TRUE,recursive=TRUE),"master_report_output"),file.path(out,"output_manifest.csv"))
  # Publish a current pointer only after every calculation and report succeeded.
  tmp<-paste0(pointer,".tmp");jsonlite::write_json(state,tmp,auto_unbox=TRUE,pretty=TRUE)
  stopifnot(file.rename(tmp,pointer))
  message("MASTER COMPLETE: ",file.path(out,"REPORT.md"),"; pending note reviews: ",nrow(pending))
  c(pointer,list.files(out,full.names=TRUE))
}
