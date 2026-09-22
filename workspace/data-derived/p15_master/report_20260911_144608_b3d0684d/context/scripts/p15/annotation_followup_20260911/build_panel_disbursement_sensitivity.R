#!/usr/bin/env Rscript
# Identical full-tenor tranche shifts: an identity check, not contract validation.
source("scripts/p15/activate_p15_environment.R")
source("R/research_governance.R")
source("R/pvr.R")
source("R/p15_pv_first_pass.R")
source("R/p15_bullet_extension.R")
suppressPackageStartupMessages({library(data.table); library(jsonlite); library(digest); library(tibble)})
args <- commandArgs(TRUE)
out <- if (length(args)) args[1] else "data-derived/p15_panel_disbursement_20260911_v1"
if (dir.exists(out)) stop("Choose a fresh output directory: ", out)
parent <- Sys.getenv("P15_BULLET_BASE", "")
if (!nzchar(parent)) parent <- fromJSON("data-derived/p15_master/current_run.json")$stages$bullet$dir
design <- "docs/thesis_design/feedback_2026-09-11_followup/PANEL_DISBURSEMENT_DESIGN.md"
legacy_path <- "docs/repayment_profile_method_workstream/scripts/build_repayment_profile_lockin_gate_2026_07_01.R"
input_paths <- c(file.path(parent, c("paired_pv_for_inclusion_check.csv",
  "inventory_with_bullet_flags.csv", "output_manifest.csv")), design, legacy_path)
input_hashes <- vapply(input_paths, function(p) digest(file=p, algo="sha256"), character(1))
registered <- fread(input_paths[3])
registered_paths <- if ("artifact_path" %in% names(registered)) registered$artifact_path else registered$path
registered_paths <- normalizePath(registered_paths, mustWork=TRUE)
stopifnot(all(input_hashes[1:2] == registered$sha256[
  match(normalizePath(input_paths[1:2]), registered_paths)]))
p <- fread(input_paths[1], na.strings="")
inv <- fread(input_paths[2], na.strings="")
keys <- c("iso3", "analysis_year", "creditor")
stopifnot(!anyDuplicated(p[,..keys]), !anyDuplicated(inv[,..keys]))
terms <- c(keys,"country","historical_income_level","historical_lmic_reporting_scope",
  "official_rate","official_maturity_years","official_grace_years","term_state",
  "ordinary_scenario_with_bullet","selected_purpose_hold","selected_currency_basis",
  "selected_timing_basis","parent_evidence_id")
x <- merge(p, inv[,..terms], by=keys, all.x=TRUE, sort=FALSE)
x[, case_id := paste(iso3,analysis_year,creditor,sep="::")]
x[, `:=`(fractional_maturity=abs(official_maturity_years-round(official_maturity_years))>1e-10,
  fractional_grace=abs(official_grace_years-round(official_grace_years))>1e-10,
  period=ifelse(analysis_year>=2018L,"modern_2018_2024","earlier_2012_2017"),
  policy_reference_interpretation=ifelse(analysis_year>=2018L,
    "modern_category_reference_same_repayments","standardized_category_reference_not_historical_headline_ODA"))]
stopifnot(nrow(x)==1498L, sum(x$modern_dac_comparison)==762L,
  sum(x$bullet_extension_case)==21L, all(x$ordinary_scenario_with_bullet),
  all(x$historical_lmic_reporting_scope), !any(x$selected_purpose_hold),
  all(is.finite(x$market_pv_per_100)),all(is.finite(x$policy_pv_per_100)))

# Evaluate only these preserved function definitions, never the old producer.
legacy <- new.env(parent=globalenv())
legacy$as_decimal <- function(z) as.numeric(z)/100
legacy_names <- c("resolve_horizon","disbursement_profile","pv_equal_principal_scalar")
for (e in parse(legacy_path)) {
  if (is.call(e) && identical(e[[1]], as.name("<-")) &&
      is.symbol(e[[2]]) && as.character(e[[2]]) %in% legacy_names) eval(e, legacy)
}
stopifnot(all(vapply(legacy_names, exists, logical(1), envir=legacy, inherits=FALSE)))
profiles <- data.table(profile_id=c("uniform_2y","uniform_3y","uniform_5y",
  "uniform_grace_capped5","front_loaded_5y","back_loaded_5y"),
  horizon_code=c("fixed_2y","fixed_3y","fixed_5y","grace_capped5","fixed_5y","fixed_5y"),
  weight_profile=c(rep("uniform",4L),"front_loaded","back_loaded"))
make_draws <- function(maturity, grace, horizon_code, weight_profile) {
  h <- min(maturity, switch(horizon_code, fixed_2y=2, fixed_3y=3,
    fixed_5y=5, grace_capped5=min(max(grace,0),5)))
  if (h<=0) return(data.table(tranche=1L,draw_time_years=0,weight=1,horizon_years=h))
  n <- max(1L, as.integer(ceiling(2*h)))
  w <- switch(weight_profile, uniform=rep(1,n), front_loaded=rev(seq_len(n)), back_loaded=seq_len(n))
  data.table(tranche=seq_len(n),draw_time_years=pmin((seq_len(n)-0.5)/2,h),
    weight=w/sum(w),horizon_years=h)
}
flows_all <- replays <- details <- draws_all <- grid_checks <- legacy_bridge <- list()
k <- 0L
for (i in seq_len(nrow(x))) {
  a <- x[i]
  f <- if(a$bullet_extension_case) p15_bullet_equal_schedule(a$official_rate,
    a$official_maturity_years,a$official_grace_years) else p15_pv_schedule(a$official_rate,
    a$official_maturity_years,a$official_grace_years)
  flows_all[[i]] <- cbind(data.table(case_id=a$case_id),as.data.table(f))
  market_replay <- p15_stream_pv(f,a$selected_rate_pct)
  policy_replay <- p15_stream_pv(f,a$dac_rate_pct)
  replays[[i]] <- data.table(case_id=a$case_id,principal_total=sum(f$principal),
    accepted_final_payment_time=tail(f$payment_time_years,1),
    market_replay_pv_per_100=market_replay,policy_replay_pv_per_100=policy_replay,
    market_stored_error=market_replay-a$market_pv_per_100,
    policy_stored_error=policy_replay-a$policy_pv_per_100)
  for (reference in c("market","policy")) {
    r <- if(reference=="market")a$selected_rate_pct else a$dac_rate_pct
    exact <- if(reference=="market")market_replay else policy_replay
    ceil_pv <- sum(f$debt_service/(1+r/100)^f$year)
    old_exact <- if(a$bullet_extension_case)NA_real_ else legacy$pv_equal_principal_scalar(
      annual_rate_percent=a$official_rate,maturity_years=a$official_maturity_years,
      grace_years=a$official_grace_years,discount_rate_percent=r)
    legacy_bridge[[length(legacy_bridge)+1L]] <- data.table(case_id=a$case_id,
      reference=reference,discount_rate_pct=r,fractional_maturity=a$fractional_maturity,
      bullet_extension_case=a$bullet_extension_case,accepted_actual_end_pv_per_100=exact,
      whole_year_end_pv_per_100=ceil_pv,actual_minus_whole_year_end_pv=exact-ceil_pv,
      legacy_exact_end_helper_pv=old_exact,
      legacy_exact_helper_minus_accepted=old_exact-exact,
      interpretation=if(a$bullet_extension_case)"old_tranche_helper_did_not_cover_bullet" else
        "current_case_timing_bridge_not_delay_effect_or_old_sample_reestimate")
  }
  for (j in seq_len(nrow(profiles))) {
    s <- profiles[j]
    d <- make_draws(a$official_maturity_years,a$official_grace_years,s$horizon_code,s$weight_profile)
    ld <- legacy$disbursement_profile(100,
      legacy$resolve_horizon(s$horizon_code,a$official_maturity_years,a$official_grace_years),s$weight_profile)
    k <- k+1L
    draws_all[[k]] <- cbind(data.table(case_id=a$case_id,profile_id=s$profile_id),d)
    grid_checks[[k]] <- data.table(case_id=a$case_id,profile_id=s$profile_id,
      weight_sum_error=abs(sum(d$weight)-1),legacy_grid_rows_match=nrow(d)==nrow(ld),
      legacy_grid_max_time_error=max(abs(d$draw_time_years-ld$time_years)),
      legacy_grid_max_weight_error=max(abs(100*d$weight-ld$amount)))
    for (reference in c("market","policy")) {
      r <- if(reference=="market")a$selected_rate_pct else a$dac_rate_pct
      baseline <- if(reference=="market")a$market_pv_per_100 else a$policy_pv_per_100
      factor <- sum(d$weight/(1+r/100)^d$draw_time_years)
      numerator <- baseline*factor
      inflow <- 100*factor
      # Direct Cartesian cash-flow calculation, independent of stored-PV factoring.
      shifted_times <- outer(f$payment_time_years,d$draw_time_years,"+")
      shifted_amounts <- outer(f$debt_service,d$weight,"*")
      direct_numerator <- sum(shifted_amounts/(1+r/100)^shifted_times)
      direct_ratio <- 100*direct_numerator/inflow
      ratio <- 100*numerator/inflow
      details[[length(details)+1L]] <- data.table(case_id=a$case_id,iso3=a$iso3,
        analysis_year=a$analysis_year,creditor=a$creditor,selected_tier=a$selected_tier,
        period=a$period,modern_dac_comparison=a$modern_dac_comparison,
        repayment_profile=a$repayment_profile,bullet_extension_case=a$bullet_extension_case,
        fractional_maturity=a$fractional_maturity,fractional_grace=a$fractional_grace,
        profile_id=s$profile_id,reference=reference,discount_rate_pct=r,
        horizon_years=d$horizon_years[1],tranches=nrow(d),
        first_draw_years=min(d$draw_time_years),last_draw_years=max(d$draw_time_years),
        final_repayment_years=max(f$payment_time_years)+max(d$draw_time_years),
        baseline_repayments_pv_per_100=baseline,baseline_inflows_pv_per_100=100,
        draw_discount_factor=factor,tranche_repayments_pv_per_100=numerator,
        tranche_inflows_pv_per_100=inflow,repayments_pv_change_per_100=numerator-baseline,
        inflows_pv_change_per_100=inflow-100,baseline_pvr_pct=baseline,tranche_pvr_pct=ratio,
        pvr_change_pp=ratio-baseline,baseline_grant_element_pct=100-baseline,
        tranche_grant_element_pct=100-ratio,grant_element_change_pp=baseline-ratio,
        direct_shifted_repayments_pv=direct_numerator,direct_shifted_pvr_pct=direct_ratio,
        direct_minus_factored_repayments_pv=direct_numerator-numerator,
        direct_ratio_minus_baseline_pp=direct_ratio-baseline)
    }
  }
}
f <- rbindlist(flows_all); replay <- rbindlist(replays); z <- rbindlist(details)
draws <- rbindlist(draws_all); grids <- rbindlist(grid_checks); bridge <- rbindlist(legacy_bridge)
pair_cols <- c("case_id","profile_id","iso3","analysis_year","creditor","selected_tier",
  "period","modern_dac_comparison","bullet_extension_case","fractional_maturity","fractional_grace")
gap <- merge(z[reference=="market", c(.SD,list(market_baseline_ge=baseline_grant_element_pct,
  market_tranche_ge=tranche_grant_element_pct)),.SDcols=pair_cols],
  z[reference=="policy",.(case_id,profile_id,policy_baseline_ge=baseline_grant_element_pct,
    policy_tranche_ge=tranche_grant_element_pct)],by=c("case_id","profile_id"))
gap[,`:=`(baseline_market_minus_policy_ge_pp=market_baseline_ge-policy_baseline_ge,
  tranche_market_minus_policy_ge_pp=market_tranche_ge-policy_tranche_ge)]
gap[,gap_change_pp:=tranche_market_minus_policy_ge_pp-baseline_market_minus_policy_ge_pp]
summarize <- function(d) data.table(scenarios=nrow(d),countries=uniqueN(d$iso3),
  modern_scenarios=sum(d$modern_dac_comparison),bullet_scenarios=sum(d$bullet_extension_case),
  fractional_maturity_scenarios=sum(d$fractional_maturity),
  mean_baseline_repayments_pv=mean(d$baseline_repayments_pv_per_100),
  mean_tranche_repayments_pv=mean(d$tranche_repayments_pv_per_100),
  mean_tranche_inflows_pv=mean(d$tranche_inflows_pv_per_100),
  mean_repayments_pv_change=mean(d$repayments_pv_change_per_100),
  mean_inflows_pv_change=mean(d$inflows_pv_change_per_100),
  mean_baseline_pvr_pct=mean(d$baseline_pvr_pct),mean_tranche_pvr_pct=mean(d$tranche_pvr_pct),
  maximum_absolute_pvr_change_pp=max(abs(d$pvr_change_pp)),
  maximum_absolute_direct_pvr_error=max(abs(d$direct_ratio_minus_baseline_pp)))
summary_all <- z[,summarize(.SD),by=.(profile_id,reference)]
summary_period <- z[,summarize(.SD),by=.(profile_id,reference,period)]
summary_year <- z[,summarize(.SD),by=.(profile_id,reference,analysis_year)]
summary_creditor <- z[,summarize(.SD),by=.(profile_id,reference,creditor)]
summary_tier <- z[,summarize(.SD),by=.(profile_id,reference,selected_tier)]
gap_summary <- gap[,.(scenarios=.N,countries=uniqueN(iso3),
  mean_baseline_gap_pp=mean(baseline_market_minus_policy_ge_pp),
  mean_tranche_gap_pp=mean(tranche_market_minus_policy_ge_pp),
  maximum_absolute_gap_change_pp=max(abs(gap_change_pp))),by=.(profile_id,period,creditor)]
coverage <- inv[,.(inventory_rows=.N,eligible_current_rows=sum(historical_lmic_reporting_scope &
  ordinary_scenario_with_bullet)),by=.(analysis_year,creditor,historical_lmic_reporting_scope,
  term_state,use_reason_with_bullet)]
support <- x[,.(scenarios=.N,countries=uniqueN(iso3),modern=sum(modern_dac_comparison),
  bullet=sum(bullet_extension_case),fractional_maturity=sum(fractional_maturity),
  fractional_grace=sum(fractional_grace)),by=.(analysis_year,creditor,selected_tier)]
checks <- data.table(check=c("1498_baselines_preserved","762_modern_rows_retained",
  "21_exact_equal_bullets_retained","terms_join_unique_and_complete","eligible_inventory_exactly_matches_baseline",
  "all_six_profiles_for_every_case_and_both_references","all_parent_rates_preserved",
  "baseline_cash_flow_pv_reproduced","principal_repaid_in_full","actual_final_maturity_inherited",
  "draw_weights_sum_one","draw_grid_matches_preserved_legacy_functions","discounted_inflows_positive",
  "identical_full_tenor_shift_matches_direct_cash_flows","pvr_invariant_all_cases",
  "grant_element_invariant_all_cases","market_policy_gap_invariant_all_cases",
  "fractional_case_ratios_invariant","bullet_case_ratios_invariant",
  "legacy_exact_end_helper_matches_current_nonbullet_schedule","whole_year_timing_difference_zero_for_integer_maturity",
  "inputs_unchanged"),passed=c(
  isTRUE(all.equal(p,x[,names(p),with=FALSE],check.attributes=FALSE)),
  sum(x$modern_dac_comparison)==762L,sum(x$bullet_extension_case)==21L,
  nrow(x)==nrow(p) && !anyDuplicated(x$case_id) && all(is.finite(x$official_rate)),
  setequal(x$case_id,inv[historical_lmic_reporting_scope & ordinary_scenario_with_bullet,
    paste(iso3,analysis_year,creditor,sep="::")]),
  nrow(z)==1498L*6L*2L && !anyDuplicated(z[,.(case_id,profile_id,reference)]),
  all(z[reference=="market",discount_rate_pct]==x$selected_rate_pct[match(z[reference=="market",case_id],x$case_id)]) &&
    all(z[reference=="policy",discount_rate_pct]==x$dac_rate_pct[match(z[reference=="policy",case_id],x$case_id)]),
  max(abs(c(replay$market_stored_error,replay$policy_stored_error)))<1e-9,
  max(abs(replay$principal_total-100))<1e-9,
  max(abs(replay$accepted_final_payment_time-x$official_maturity_years[match(replay$case_id,x$case_id)]))<1e-9,
  max(grids$weight_sum_error)<1e-12,
  all(grids$legacy_grid_rows_match) && max(grids$legacy_grid_max_time_error)<1e-12 &&
    max(grids$legacy_grid_max_weight_error)<1e-10,
  all(z$tranche_inflows_pv_per_100>0),max(abs(z$direct_minus_factored_repayments_pv))<1e-9,
  max(abs(z$pvr_change_pp))<1e-9 && max(abs(z$direct_ratio_minus_baseline_pp))<1e-9,
  max(abs(z$grant_element_change_pp))<1e-9,max(abs(gap$gap_change_pp))<1e-9,
  max(abs(z[fractional_maturity | fractional_grace,direct_ratio_minus_baseline_pp]))<1e-9,
  max(abs(z[bullet_extension_case==TRUE,direct_ratio_minus_baseline_pp]))<1e-9,
  max(abs(bridge[bullet_extension_case==FALSE,legacy_exact_helper_minus_accepted]))<1e-9,
  max(abs(bridge[fractional_maturity==FALSE,actual_minus_whole_year_end_pv]))<1e-9,
  identical(input_hashes,vapply(input_paths,function(p)digest(file=p,algo="sha256"),character(1)))))
if (!all(checks$passed)) { print(checks[passed==FALSE]); stop("Panel disbursement checks failed") }
tables <- list(baseline_cases=x,inventory_coverage=coverage,support_by_year_creditor_tier=support,
  accepted_cash_flows=f,baseline_replay_checks=replay,profile_definitions=profiles,
  disbursement_tranches=draws,profile_grid_checks=grids,tranche_valuations=z,
  paired_grant_element_checks=gap,summary_overall=summary_all,summary_by_period=summary_period,
  summary_by_year=summary_year,summary_by_creditor=summary_creditor,summary_by_tier=summary_tier,
  gap_summary_by_period_creditor=gap_summary,legacy_timing_bridge=bridge,checks=checks)
ids <- list(build_id=basename(out),schema_id="SCHEMA-P15-PANEL-DISBURSEMENT-V1",
  estimator_id="EST-P15-IDENTICAL-TRANCHE-SHIFT-V1",
  admissibility_id="ADM-P15-INHERITED-AGGREGATE-BULLET-1498-V1",
  selection_id="SEL-P15-NO-CHANGE-DISBURSEMENT-DIAGNOSTIC-V1",
  source_package_ids="SRC-P15-CURRENT-BULLET-PANEL;SRC-P15-LEGACY-TRANCHE-CODE-20260701")
dir.create(out,recursive=TRUE)
for(nm in names(tables)) {
  tables[[nm]][,`:=`(build_id=ids$build_id,schema_id=ids$schema_id,estimator_id=ids$estimator_id,
    admissibility_id=ids$admissibility_id,selection_id=ids$selection_id,
    lifecycle_status="diagnostic",release_state="private_research")]
  fwrite(tables[[nm]],file.path(out,paste0(nm,".csv")),na="")
}
manifest <- function(paths,role)do.call(pvr_manifest_rows,c(list(paths=paths,artifact_role=role),ids))
fwrite(manifest(input_paths,"panel_disbursement_input"),file.path(out,"input_manifest.csv"))
code_paths <- c("scripts/p15/annotation_followup_20260911/build_panel_disbursement_sensitivity.R",
  "scripts/p15/activate_p15_environment.R","R/research_governance.R","R/pvr.R",
  "R/p15_pv_first_pass.R","R/p15_bullet_extension.R",legacy_path,"renv.lock")
fwrite(manifest(code_paths,"panel_disbursement_code"),file.path(out,"code_manifest.csv"))
write_json(c(ids,list(parent_bullet=parent,design=design,lifecycle_status="diagnostic",
  release_state="private_research",notional=100,profiles=profiles$profile_id,
  ratio_denominator="PV of the same disbursements at the same flat rate",
  repayment_timing="Identical accepted full-tenor schedule from each tranche date",
  statistical_inference="none; algebraic identity verified numerically",
  fixed_terminal_maturity_model=FALSE,actual_contract_validation=FALSE,
  benchmark_or_schedule_change=FALSE,tolerance=1e-9)),
  file.path(out,"version_bundle.json"),auto_unbox=TRUE,pretty=TRUE)
writeLines(capture.output(sessionInfo()),file.path(out,"environment.txt"))
fwrite(manifest(list.files(out,full.names=TRUE),"panel_disbursement_output"),file.path(out,"output_manifest.csv"))
cat("Completed",nrow(z),"case/profile/reference checks and",nrow(checks),"assertions:",out,"\n")
print(summary_all)
