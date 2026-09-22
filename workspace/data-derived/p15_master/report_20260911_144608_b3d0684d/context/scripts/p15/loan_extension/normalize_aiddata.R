#!/usr/bin/env Rscript
# Normalize public AidData loan records for separately executed conditional valuation.
# No source data, benchmark, or accepted platform selection rule is modified.
source("scripts/p15/activate_p15_environment.R")
suppressPackageStartupMessages({library(data.table); library(readxl); library(jsonlite); library(digest)})
args <- commandArgs(trailingOnly = TRUE)
out <- if (length(args)) args[[1]] else "data-derived/p15_aiddata_loan_normalization_20260910_v1"
stopifnot(!dir.exists(out)); dir.create(out, recursive = TRUE)
source_path <- "sources/literature_review/loan_extension_feasibility_20260910/aiddata/AidDatas_CLG_LMIC_Dataset_v1.0.xlsx"
x <- as.data.table(read_excel(source_path, sheet = "CLG-LMIC 1.0_Records", col_types = "text"))
x[, source_row := .I + 1L] # Excel row, including the header at row 1.
number <- function(v) suppressWarnings(as.numeric(v))
id <- function(v) ifelse(is.na(v), NA_character_, format(number(v), scientific = FALSE, trim = TRUE))
x[, `:=`(year = number(Commitment_Year), rate = number(Interest_at_T0),
  mat = number(Maturity), grace = number(Grace_Period), amount = number(Amount_Original_Currency))]
x[, source_screen := Recommended_for_Aggregates == "Yes" & Flow_Type == "Loan" & year >= 2015 & year <= 2023 &
  Original_Currency == "USD" & Level_of_Public_Liability %in% c("Central government debt", "Central government-guaranteed debt") &
  is.finite(rate) & rate >= 0 & is.finite(mat) & mat > 0 & is.finite(grace) & grace >= 0 & grace <= mat &
  is.finite(amount) & amount > 0]
q <- x[source_screen == TRUE]
stopifnot(nrow(q) == 419L, sum(q$Interest_Rate_Type == "Fixed Interest Rate") == 215L)
q[, record_id := id(AidData_Record_ID)]
safe <- function(v) fifelse(is.na(v), "", v)
q[, source_comment_text := paste(safe(Staff_Comments), safe(Narrative_Description), sep = "\n")]
term_pattern <- "(estimat|imput|default|assum).{0,180}(interest rate|maturity|grace period|borrowing terms)|(interest rate|maturity|grace period|borrowing terms).{0,180}(estimat|imput|default)"
q[, source_terms_estimation_screen := grepl(term_pattern, source_comment_text, ignore.case = TRUE, perl = TRUE)]
q[, source_ids_terms_imputation_screen := source_terms_estimation_screen &
  grepl("Debtor Reporting System|International Debt Statistics|weighted average (grace|maturity|interest)", source_comment_text, ignore.case = TRUE)]
q[, source_default_term_screen := grepl("(maturity|grace period).{0,80}(by default|default)|set the (maturity|grace).{0,100}default", source_comment_text, ignore.case = TRUE)]
q[, source_guarantee_assumed_screen := grepl("assumes.{0,150}(sovereign|guarantee)|assumed.{0,150}(sovereign|guarantee)", source_comment_text, ignore.case = TRUE)]
q[, source_disbursement_record_screen := grepl("\\[Disbursed\\]|makes.*loan disbursement|disburses|disbursed", Title, ignore.case = TRUE)]
q[, country_agree := !is.na(DRA_Country_of_Inc_ISO3) & Country_of_Activity_ISO3 == DRA_Country_of_Inc_ISO3]
q[, sovereign_country_resolution := fifelse(country_agree,
  "activity_and_direct_borrower_incorporation_agree_with_source_public_liability", "requires_source_field_review")]
# These country exceptions are resolved from exact fields/narratives already in the workbook.
# PAK records explicitly name the Government of Pakistan as guarantor (source assumption flagged).
q[record_id %in% c("54234", "92636", "92637", "92635", "53675"),
  sovereign_country_resolution := "PAK_named_sovereign_guarantor_source_assumption_preserved"]
# A Chinese contractor intermediates on-lending to Zambia in the source narrative.
q[record_id %in% c("53037", "92430"),
  sovereign_country_resolution := "ZMB_named_indirect_government_recipient_and_explicit_onlending"]
q[record_id == "56970", sovereign_country_resolution := "ZMB_named_ministry_of_finance_guarantor_source_assumption_preserved"]
q[record_id == "73171", sovereign_country_resolution := "ZMB_named_direct_government_borrower_despite_missing_incorporation"]
q[record_id %in% c("101901", "96280"),
  sovereign_country_resolution := "named_RWA_EGY_government_indirect_borrower_via_AGTF_intermediary"]
# Missing incorporation may still be resolved from an explicitly named government recipient.
q[sovereign_country_resolution == "requires_source_field_review" &
  mapply(function(c, a) grepl(c, a, fixed = TRUE), safe(Country_of_Activity), safe(Direct_Receiving_Agencies)) &
  Level_of_Public_Liability == "Central government debt",
  sovereign_country_resolution := "named_direct_government_recipient_and_source_central_liability"]
normalized <- q[, .(dataset = "aiddata", loan_id = paste0("aiddata_", record_id),
  loan_event_id = paste0("aiddata_event_", id(Loan_Event_ID)), iso3 = Country_of_Activity_ISO3,
  commitment_year = as.integer(year), creditor = "China", currency = Original_Currency,
  interest_rate_pct = rate, maturity_years = mat, grace_years = grace,
  amount_usd = amount, amount_original = amount,
  interest_type = fcase(Interest_Rate_Type == "Fixed Interest Rate", "fixed",
    Interest_Rate_Type == "Variable Interest Rate", "variable_at_origination", default = "unknown_type_at_origination"),
  normalization_eligible = sovereign_country_resolution != "requires_source_field_review",
  exclusion_reason = fifelse(sovereign_country_resolution == "requires_source_field_review", "sovereign_country_not_resolved_from_source", ""),
  source_row, source_path = source_path, source_sheet = "CLG-LMIC 1.0_Records",
  source_original_maturity = Maturity, source_original_grace = Grace_Period,
  source_saved_ge_pct = number(Grant_Element_IMF),
  schedule_basis = "AidData_IMF_formula_total_maturity_including_grace_principal_at_grace_endpoint",
  first_principal_years = grace, repayment_frequency = 2L,
  first_principal_basis = "source_formula_endpoint_convention_not_contract_payment_verification",
  currency_basis = "original_currency_USD_no_conversion",
  loan_scope = Level_of_Public_Liability,
  source_record_id = record_id, source_parent_id = id(Parent_ID), source_event_id = id(Loan_Event_ID),
  source_event_tranche = Loan_Event_Tranche, source_lender = Funding_Agencies,
  source_amount_nominal_usd = number(Amount_Nominal_USD),
  source_adjusted_amount_usd = number(Adjusted_Amount_Nominal_USD),
  source_syndicated_amount_usd = number(Syndicated_Loan_Amount_Nominal_USD),
  source_syndicated_share = number(Syndicated_Loan_Share), source_amount_estimated = Amount_Estimated,
  source_commitment_date = Commitment_Date, source_commitment_date_estimated = Commitment_Date_Estimated,
  source_activity_country = Country_of_Activity, source_activity_iso3 = Country_of_Activity_ISO3,
  source_borrower_incorporation_iso3 = DRA_Country_of_Inc_ISO3,
  source_direct_borrower = Direct_Receiving_Agencies, source_indirect_borrower = Indirect_Receiving_Agencies,
  source_guarantor = Guarantor, sovereign_country_resolution,
  source_management_fee_pct = number(Management_Fee), source_commitment_fee_pct = number(Commitment_Fee),
  source_insurance_fee_pct = number(Insurance_Fee_Percent),
  source_insurance_fee_nominal_usd = number(Insurance_Fee_Nominal_USD),
  source_terms_estimation_screen, source_ids_terms_imputation_screen, source_default_term_screen,
  source_guarantee_assumed_screen, source_disbursement_record_screen,
  source_agreement_available = Original_Agreement, source_title = Title,
  source_short_term = Short_Term, source_rescue = Rescue, source_refinancing = Refinancing,
  source_quality_score = number(Source_Quality_Score), source_loan_detail_score = number(Loan_Detail_Score))]
normalized[, source_imputed_terms := source_ids_terms_imputation_screen | source_default_term_screen]
normalized[, source_imputed_terms_basis := "text_screen_for_IDS_borrowed_terms_or_default_assumptions_not_exhaustive_contract_classification"]
normalized[, original_fixed_usd_screen := interest_type == "fixed"]
normalized[, interest_scenario := fifelse(original_fixed_usd_screen, "fixed_coupon_baseline",
  fifelse(interest_type == "variable_at_origination", "floating_coupon_held_at_origination_sensitivity", "unknown_type_coupon_held_at_origination_sensitivity"))]
normalized[, fees_present := is.finite(source_management_fee_pct) | is.finite(source_commitment_fee_pct) |
  is.finite(source_insurance_fee_pct) | is.finite(source_insurance_fee_nominal_usd)]
normalized[, positive_fee_present := fcoalesce(source_management_fee_pct > 0, FALSE) |
  fcoalesce(source_commitment_fee_pct > 0, FALSE) | fcoalesce(source_insurance_fee_pct > 0, FALSE) |
  fcoalesce(source_insurance_fee_nominal_usd > 0, FALSE)]
normalized[, fee_treatment := "source_IMF_comparison_excludes_fees_all_in_cost_not_claimed"]
normalized[source_record_id == "53037", `:=`(normalization_eligible = FALSE,
  exclusion_reason = "origination_comparability_hold_disbursement_only_terms_IDS_imputed_agreement_unconfirmed")]
# Source formula, rather than a reconstruction of contractual repayment dates.
aiddata_source_ge <- function(coupon_pct, maturity, grace, discount_pct = 5, frequency = 2) {
  d <- discount_pct / 100; r <- coupon_pct / 100
  period_rate <- expm1(log1p(d) / frequency)
  count <- 1 + frequency * (maturity - grace)
  mean_df <- ((1 + d)^(-grace) - (1 + d)^(-maturity - 1 / frequency)) /
    (count * (1 - (1 + d)^(-1 / frequency)))
  100 * (1 - r / (frequency * period_rate)) * (1 - mean_df)
}
normalized[, source_formula_ge_5_unclipped_pct := aiddata_source_ge(interest_rate_pct, maturity_years, grace_years)]
normalized[, source_formula_ge_5_clipped_pct := pmin(100, pmax(0, source_formula_ge_5_unclipped_pct))]
normalized[, source_formula_ge_5_error_pp := source_formula_ge_5_clipped_pct - source_saved_ge_pct]
normalized[, `:=`(as_source_ge5_unbounded = source_formula_ge_5_unclipped_pct,
  as_source_ge5_bounded = source_formula_ge_5_clipped_pct)]
normalized[, source_formula_fractional_payment_count := 1 + 2 * (maturity_years - grace_years)]
normalized[, source_formula_fractional_grid := abs(2 * maturity_years - round(2 * maturity_years)) > 1e-8 |
  abs(2 * grace_years - round(2 * grace_years)) > 1e-8]
normalized[, source_formula_clipping_changed_value := abs(source_formula_ge_5_unclipped_pct - source_formula_ge_5_clipped_pct) > 1e-8]
normalized[, event_tranche_key := paste(loan_event_id, fifelse(is.na(source_event_tranche), "unspecified", source_event_tranche), sep = ":")]
normalized[, event_record_count := .N, by = loan_event_id]
normalized[, event_tranche_record_count := .N, by = event_tranche_key]
economic_key <- c("loan_event_id", "source_event_tranche", "source_lender", "currency", "interest_rate_pct", "maturity_years", "grace_years", "amount_original")
normalized[, same_lender_economic_key_duplicates := .N > 1L, by = economic_key]
normalized[, apparent_duplicate_resolution := fifelse(same_lender_economic_key_duplicates, "unresolved", "not_flagged")]
normalized[source_record_id %in% c("92567", "110056"),
  apparent_duplicate_resolution := "distinct_BOC_Panama_vs_Beijing_Liaoning_contributions_documented_in_source_comments"]
# Retain each distinct creditor contribution. Do not weight repeated whole-syndicate amounts.
normalized[, amount_weight_basis := "record_Chinese_creditor_contribution_original_USD_not_syndicated_facility_total"]
normalized[, event_equal_weight := 1 / .N, by = loan_event_id]
event_audit <- normalized[, .(record_count = .N, tranches = uniqueN(source_event_tranche), lenders = uniqueN(source_lender),
  terms_combinations = uniqueN(paste(interest_rate_pct, maturity_years, grace_years)),
  included_contribution_sum_usd = sum(amount_usd),
  distinct_source_syndicated_totals = paste(sort(unique(na.omit(source_syndicated_amount_usd))), collapse = ";"),
  same_lender_economic_duplicates = any(same_lender_economic_key_duplicates),
  source_row_ids = paste(source_record_id, collapse = ";"),
  scope_note = "sum_covers_inspected_contributions_only_not_necessarily_whole_facility"), by = loan_event_id]
meta <- list(build_id = basename(out), schema_id = "SCHEMA-P15-AIDDATA-LOAN-NORMALIZATION-V1",
  estimator_id = "EST-P15-AIDDATA-SOURCE-GE-REPLAY-V1", admissibility_id = "ADM-P15-AIDDATA-CONDITIONAL-LOANS-V1",
  selection_id = "SEL-P15-AIDDATA-LATER-CENTRAL-USD-TERMS-V1", lifecycle_status = "diagnostic", release_state = "private_working_analysis")
write_table <- function(z, filename) {
  z <- copy(z)
  for (nm in names(meta)) set(z, j = nm, value = meta[[nm]])
  z[, lineage_parent_ids := paste0("loan_extension_20260910_aiddata_clg_lmic_v1_zip;", source_path)]
  fwrite(z, file.path(out, filename), na = "")
}
write_table(normalized, "normalized_loans.csv")
write_table(event_audit, "loan_event_audit.csv")
write_table(q[, .(source_record_id = record_id, source_row, source_title = Title, source_staff_comments = Staff_Comments,
  source_narrative = Narrative_Description, sovereign_country_resolution, source_terms_estimation_screen,
  source_ids_terms_imputation_screen, source_default_term_screen)], "source_qualification_audit.csv")
summary <- normalized[, .(records = .N, eligible = sum(normalization_eligible), events = uniqueN(loan_event_id),
  source_term_estimation_flags = sum(source_terms_estimation_screen), IDS_term_imputation_flags = sum(source_ids_terms_imputation_screen),
  default_term_flags = sum(source_default_term_screen), source_disbursement_record_flags = sum(source_disbursement_record_screen),
  fractional_grid = sum(source_formula_fractional_grid), clipped_5pct_values = sum(source_formula_clipping_changed_value),
  positive_fee_records = sum(positive_fee_present), max_source_GE_replay_error_pp = max(abs(source_formula_ge_5_error_pp))), by = interest_scenario]
write_table(summary, "normalization_summary.csv")
checks <- data.table(check = c("419_source_screen_records_retained", "215_fixed_source_records_retained", "unique_record_ids",
  "positive_horizons_and_compatible_grace", "same_currency_original_and_USD_amount", "source_GE_all_finite_reproduced_0_0002pp",
  "two_fixed_country_exceptions_resolved", "no_unresolved_sovereign_country", "53037_hold_preserved", "apparent_same_lender_duplicates_resolved"),
  passed = c(nrow(normalized) == 419, sum(normalized$original_fixed_usd_screen) == 215, !anyDuplicated(normalized$loan_id),
    all(normalized$maturity_years > 0 & normalized$grace_years >= 0 & normalized$grace_years <= normalized$maturity_years),
    all(normalized$amount_usd == normalized$amount_original), all(is.finite(normalized$source_formula_ge_5_error_pp)) & max(abs(normalized$source_formula_ge_5_error_pp)) < 0.0002,
    all(normalized[source_record_id %in% c("53037", "73171"), iso3] == "ZMB"),
    !any(normalized$sovereign_country_resolution == "requires_source_field_review"),
    normalized[source_record_id == "53037", !normalization_eligible], !any(normalized$apparent_duplicate_resolution == "unresolved")))
fwrite(checks, file.path(out, "checks.csv")); stopifnot(all(checks$passed))
manifest <- function(paths) data.table(path = paths, sha256 = vapply(paths, function(p) digest(file = p, algo = "sha256"), character(1)), bytes = file.info(paths)$size)
fwrite(manifest(source_path), file.path(out, "input_manifest.csv"))
fwrite(manifest(c("scripts/p15/loan_extension/normalize_aiddata.R", "scripts/p15/activate_p15_environment.R", "renv.lock")), file.path(out, "code_manifest.csv"))
write_json(c(meta, list(source_snapshot_id = "loan_extension_20260910_aiddata_clg_lmic_v1_zip",
  amount_unit = "USD", rate_unit = "percentage_points", source_formula = "endpoint_inclusive_semiannual_effective_annual_discount_closed_form",
  limitations = "Origination coupon held constant; source terms may be imputed; fees omitted; public liability source assumed in some cases; no contract audit; row/event units differ.")), file.path(out, "version_bundle.json"), pretty = TRUE, auto_unbox = TRUE)
writeLines(capture.output(sessionInfo()), file.path(out, "environment.txt"))
fwrite(manifest(list.files(out, full.names = TRUE)), file.path(out, "output_manifest.csv"))
print(summary); cat("AidData normalization written:", out, "\n")
