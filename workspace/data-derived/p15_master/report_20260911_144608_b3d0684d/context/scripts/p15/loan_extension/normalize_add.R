#!/usr/bin/env Rscript
# Owner-authorized exploratory ADD loan normalization, 10 September 2026.
# No valuation occurs here. Every external loan row in 2015--2024 is retained.
source("scripts/p15/activate_p15_environment.R")
suppressPackageStartupMessages({library(data.table);library(readxl);library(jsonlite);library(digest);library(countrycode)})

add_num <- function(x) suppressWarnings(as.numeric(gsub(",", "", x, fixed=TRUE)))
add_date <- function(x) as.Date(x,format="%m/%d/%Y")

add_wb_date_audit <- function(add, raw_dir) {
  raw <- rbindlist(lapply(c("IDA","IBRD"),function(kind) {
    path <- list.files(raw_dir,pattern=paste0("^",kind,".*csv$"),full.names=TRUE)
    stopifnot(length(path)==1L)
    d <- fread(path,colClasses="character",na.strings="")
    data.table(wb_raw_path=path,wb_raw_row=seq_len(nrow(d)),
      creditor=paste0("WB-",kind),
      iso3=countrycode(d[["Country Code"]],"iso2c","iso3c",warn=FALSE),
      wb_id=d[[if(kind=="IDA")"Credit Number" else "Loan Number"]],
      wb_currency=d[["Currency of Commitment"]],
      amount_original=add_num(d[["Original Principal Amount"]]),
      issue_date=add_date(d[["Agreement Signing Date"]]),
      board_date=add_date(d[["Board Approval Date"]]),
      effective_date=add_date(d[["Effective Date (Most Recent)"]]),
      first_date=add_date(d[["First Repayment Date"]]),
      last_date=add_date(d[["Last Repayment Date"]]),
      wb_raw_rate=add_num(d[[if(kind=="IDA")"Service Charge Rate" else "Interest Rate"]]),
      wb_status=d[[if(kind=="IDA")"Credit Status" else "Loan Status"]])
  }),fill=TRUE)
  d <- add[CreditorName_short %in% c("WB-IDA","WB-IBRD"),
    .(source_row,iso3=ISO3,creditor=CreditorName_short,currency=Currency,
      amount_original=add_num(Amount_m)*1e6,
      issue_date=as.Date(add_num(issue_date),origin="1899-12-30"),
      source_maturity=add_num(maturity),source_grace=add_num(grace),
      source_interest=add_num(interest),source_year=add_num(year))]
  # Matching excludes the repayment dates being validated. The amounts are
  # rounded to the nearest original-currency unit only to absorb Excel precision.
  raw[,amount_key:=round(amount_original)]
  d[,amount_key:=round(amount_original)]
  candidates <- merge(d,raw,by=c("iso3","creditor","issue_date","amount_key"),
    all=FALSE,allow.cartesian=TRUE,suffixes=c("_add","_raw"))
  candidates <- candidates[!is.na(iso3)&!is.na(issue_date)&is.finite(amount_key)]
  candidates <- candidates[creditor=="WB-IBRD" | (!is.na(currency)&currency==wb_currency)]
  candidates[,candidate_count:=.N,by=source_row]
  candidates[,raw_match_count:=uniqueN(source_row),by=.(wb_raw_path,wb_raw_row)]
  candidates[,identity_unique:=candidate_count==1L & raw_match_count==1L]
  candidates[,`:=`(raw_first_last_years=as.numeric(last_date-first_date)/365.25,
    raw_effective_first_years=as.numeric(first_date-effective_date)/365.25,
    raw_signing_first_years=as.numeric(first_date-issue_date)/365.25,
    raw_effective_last_years=as.numeric(last_date-effective_date)/365.25)]
  candidates[,`:=`(maturity_error_years=source_maturity-raw_first_last_years,
    grace_error_years=source_grace-raw_effective_first_years)]
  candidates[,`:=`(maturity_span_agrees=is.finite(maturity_error_years)&abs(maturity_error_years)<1e-8,
    grace_anchor_agrees=is.finite(grace_error_years)&abs(grace_error_years)<1e-8)]
  candidates[]
}

normalize_add <- function(source_path,raw_dir) {
  a <- as.data.table(read_excel(source_path,col_types="text"))
  a[,source_row:=.I]
  audit <- add_wb_date_audit(a,raw_dir)
  # The reporting layer will distinguish exact overlap checks from the
  # source-level reconstruction applied outside the older raw-data vintage.
  d <- a[add_num(year)>=2015 & add_num(year)<=2024 &
    instrument_type %in% c("Multilateral Loan","Paris Club Loan","non-Paris Club Loan","China Loan")]
  u <- audit[identity_unique==TRUE]
  stopifnot(!anyDuplicated(u$source_row))
  x <- d[,.(dataset="add",loan_id=sprintf("add_2026may_a_r%05d",source_row),
    loan_event_id=NA_character_,iso3=ISO3,commitment_year=as.integer(add_num(year)),
    creditor=CreditorName_short,currency=Currency,interest_rate_pct=add_num(interest),
    maturity_years=add_num(maturity),grace_years=add_num(grace),
    amount_usd=add_num(Amount_musd)*1e6,amount_original=add_num(Amount_m)*1e6,
    interest_type=structure,source_row,source_excel_row=source_row+1L,
    source_path=source_path,source_original_maturity=add_num(maturity),
    source_original_grace=add_num(grace),source_saved_ge_pct=NA_real_,
    borrower_type=BorrowerType,borrower_agency=BorrowerAgency,
    creditor_group=CreditorGroup,creditor_agency=CreditorAgency,
    creditor_agency_type=CreditorAgencyType,instrument_type,
    original_source=source,source_original_interest_type=structure,
    source_reference_rate=reference_rate,source_margin=margin,
    source_issue_date=as.character(as.Date(add_num(issue_date),origin="1899-12-30")),
    source_maturity_date=as.character(as.Date(add_num(maturity_date),origin="1899-12-30")))]
  x[,`:=`(world_bank=creditor %in% c("WB-IDA","WB-IBRD"),
    first_principal_payment_years=grace_years,
    loan_scope=fifelse(borrower_type=="Central government","central_government","other_borrower_guarantee_not_established"),
    central_scope_eligible=borrower_type=="Central government")]
  # Table 2's pooled maturity description does not match the WB component.
  # The 1,550 exact raw-overlap spans and the cited MPG construction establish
  # that this component is first-to-last principal span. Restore total horizon.
  x[world_bank==TRUE,maturity_years:=source_original_maturity+source_original_grace]
  x[,`:=`(schedule_basis=fifelse(world_bank,
    "wb_total_horizon_source_first_to_last_span_plus_effective_to_first_grace;equal_principal_assumption",
    "add_reported_total_horizon_and_commitment_to_first_principal;equal_principal_assumption"),
    schedule_frequency_assumed=2L,
    schedule_anchor=fifelse(world_bank,"effective_date_from_source_construction","commitment_date_from_source_definition"),
    currency_basis=fifelse(currency=="USD","reported_USD_contract;USD_common_numeraire_scenario",
      "reported_non_USD_contract;USD_common_numeraire_scenario_without_FX_or_currency_curve_conversion"),
    source_rate_timing_basis=fifelse(world_bank,
      "reported_current_WB_interest_or_service_charge;constant_snapshot_rate_scenario",
      "ADD_reported_commitment_rate_or_commitment_year_reference_average_plus_margin"))]
  x[is.na(currency)|!nzchar(currency),currency_basis:=
    "currency_not_reported;USD_common_numeraire_scenario_only"]
  x[,interest_type:=fcase(source_original_interest_type=="Fixed","reported_fixed",
    source_original_interest_type=="Variable","floating_fixed_equivalent_at_commitment_year_reference_average",
    world_bank,"wb_reported_interest_or_service_charge_constant_scenario",
    creditor=="IMF","imf_original_facility_rate_constant_scenario",
    default="reported_rate_type_unspecified_constant_scenario")]
  match_cols <- c("source_row","wb_id","wb_raw_path","wb_raw_row","wb_raw_rate","wb_currency","wb_status",
    "effective_date","first_date","last_date","raw_effective_last_years","raw_signing_first_years",
    "maturity_error_years","grace_error_years","maturity_span_agrees","grace_anchor_agrees")
  x <- merge(x,u[,..match_cols],by="source_row",all.x=TRUE,sort=FALSE)
  setorder(x,source_row)
  x[,`:=`(wb_unique_raw_identity_match=!is.na(wb_id),
    wb_raw_vintage_disagreement=(!is.na(maturity_error_years)&abs(maturity_error_years)>1e-8)|
      (!is.na(grace_error_years)&abs(grace_error_years)>1e-8),
    wb_source_definition_reconstruction=world_bank,
    source_grace_gt_reported_maturity=is.finite(source_original_grace)&
      is.finite(source_original_maturity)&source_original_grace>source_original_maturity)]
  x[,wb_schedule_evidence:=fcase(!world_bank,"not_world_bank",
    wb_unique_raw_identity_match & maturity_span_agrees & grace_anchor_agrees,"individual_raw_dates_cross_validated",
    wb_unique_raw_identity_match & wb_raw_vintage_disagreement,"uniform_source_definition;older_raw_dates_differ",
    wb_unique_raw_identity_match,"uniform_source_definition;individual_raw_dates_incomplete",
    default="uniform_source_definition;outside_or_unmatched_to_2019_raw_vintage")]
  # A zero is preserved as supplied. The IDA codebook explicitly permits zero
  # placeholders for multiple-rate loans. The 2022 terms sheet independently
  # supports zero charges for SML/50-year credits; a pattern is not a loan-ID link.
  x[,ida_zero_product_pattern:=fcase(creditor=="WB-IDA" & interest_rate_pct==0 &
    commitment_year>=2022 & abs(source_original_maturity-5.5)<0.02 &
    source_original_grace>=4 & source_original_grace<=7,"shorter_maturity_loan_compatible_unverified_product_id",
    creditor=="WB-IDA" & interest_rate_pct==0 & commitment_year>=2022 &
    abs(source_original_maturity-39.5)<0.02 & source_original_grace>=8 & source_original_grace<=12,
    "fifty_year_credit_compatible_unverified_product_id",default=NA_character_)]
  x[,`:=`(zero_rate_uncertain=world_bank & is.finite(interest_rate_pct) &
      interest_rate_pct==0 & is.na(ida_zero_product_pattern),
    official_USD_view=!is.na(currency)&currency=="USD")]
  x[,zero_rate_quality_usable:=!zero_rate_uncertain]
  x[,exclusion_reason:=""]
  add_reason <- function(flag,reason) {
    i <- which(!is.na(flag)&flag)
    if(length(i))set(x,i=i,j="exclusion_reason",value=ifelse(nzchar(x$exclusion_reason[i]),
      paste(x$exclusion_reason[i],reason,sep=";"),reason))
  }
  add_reason(!is.finite(x$interest_rate_pct),"missing_interest_rate")
  add_reason(is.finite(x$interest_rate_pct)&x$interest_rate_pct<0,"negative_interest_rate")
  add_reason(!is.finite(x$source_original_maturity),"missing_maturity")
  add_reason(!is.finite(x$source_original_grace),"missing_grace")
  add_reason(is.finite(x$maturity_years)&x$maturity_years<=0,"nonpositive_total_horizon")
  add_reason(is.finite(x$grace_years)&x$grace_years<0,"negative_first_principal_time")
  add_reason(is.finite(x$grace_years)&is.finite(x$maturity_years)&x$grace_years>x$maturity_years,
    "first_principal_after_total_horizon")
  add_reason(!is.finite(x$amount_usd)|x$amount_usd<=0,"missing_or_nonpositive_committed_amount_usd")
  add_reason(!is.na(x$wb_id)&grepl("^IDA[DHBEG]",x$wb_id),"raw_identity_is_grant_or_guarantee_not_ordinary_loan")
  add_reason(is.na(x$iso3)|!grepl("^[A-Z]{3}$",x$iso3),"missing_or_invalid_country_identifier")
  x[,normalization_eligible:=!nzchar(exclusion_reason)]
  x[,`:=`(normalization_quality_eligible=normalization_eligible & zero_rate_quality_usable &
    !wb_raw_vintage_disagreement,
    benchmark_country_interpretation=fifelse(central_scope_eligible,
      "reported_central_government_borrower","sovereign_rate_is_only_a_country_context_scenario"),
    record_unit="source_workbook_row;economic_loan_event_id_not_provided",
    repayment_schedule_observed=FALSE,
    fees_fully_observed=FALSE,
    source_snapshot_id="LEF-ADD-2026MAY-XLSX-20260910")]
  list(raw=a,loans=x,audit=audit)
}

if (sys.nframe()==0L) {
  args <- commandArgs(TRUE)
  out <- if(length(args))args[1] else "data-derived/p15_add_loan_normalization_20260910_v1"
  stopifnot(!dir.exists(out))
  base <- "sources/literature_review/loan_extension_feasibility_20260910"
  z <- normalize_add(file.path(base,"other_data/add_2026may_a.xlsx"),file.path(base,"mpg/extracted/input"))
  date_summary <- z$audit[identity_unique==TRUE,.(records=.N,maturity_comparable=sum(is.finite(maturity_error_years)),
    maturity_agree=sum(maturity_span_agrees),grace_comparable=sum(is.finite(grace_error_years)),
    grace_agree=sum(grace_anchor_agrees),max_maturity_error=max(abs(maturity_error_years),na.rm=TRUE),
    max_grace_error=max(abs(grace_error_years),na.rm=TRUE)),by=.(creditor,post_2014=source_year>=2015)]
  x <- z$loans
  checks <- data.table(check=c("source_69849_rows","all_5142_later_external_loans_retained",
    "unique_stable_row_ids","wb_unique_raw_identity_matches_2222","wb_maturity_agrees_1550_of_1559",
    "all_pre2015_comparable_maturity_spans_agree","wb_reconstructed_horizon_identity",
    "eligible_rows_have_valid_numeric_terms","all_49_ida_grace_gt_span_retained_reconstructed",
    "no_imputed_missing_interest","no_fx_currency_relabel","no_present_value_calculated"),
    passed=c(nrow(z$raw)==69849L,nrow(x)==5142L,!anyDuplicated(x$loan_id),
      nrow(z$audit[identity_unique==TRUE])==2222L,
      sum(z$audit[identity_unique==TRUE,maturity_span_agrees])==1550L &&
        sum(is.finite(z$audit[identity_unique==TRUE,maturity_error_years]))==1559L,
      all(z$audit[identity_unique==TRUE & source_year<2015 & is.finite(maturity_error_years),maturity_span_agrees]),
      all(abs(x[world_bank & is.finite(maturity_years),maturity_years-source_original_maturity-source_original_grace])<1e-10),
      all(x[normalization_eligible==TRUE,is.finite(interest_rate_pct)&maturity_years>0&grace_years>=0&grace_years<=maturity_years]),
      nrow(x[creditor=="WB-IDA"&source_grace_gt_reported_maturity==TRUE])==49L &&
        all(x[creditor=="WB-IDA"&source_grace_gt_reported_maturity==TRUE,normalization_eligible]),
      sum(is.finite(x$interest_rate_pct))==sum(is.finite(add_num(z$raw$interest[z$raw$source_row %in% x$source_row]))),
      identical(x$currency,z$raw$Currency[match(x$source_row,z$raw$source_row)]),TRUE))
  stopifnot(all(checks$passed))
  dir.create(out,recursive=TRUE)
  meta <- list(build_id=basename(out),schema_id="SCHEMA-P15-ADD-LOAN-NORMALIZATION-V1",
    estimator_id="EST-P15-ADD-WB-SOURCE-SPAN-RECONSTRUCTION-V1",
    admissibility_id="ADM-P15-EXPLORATORY-NUMERIC-TERMS-SEPARATE-QUALITY-FLAGS-V1",
    selection_id="SEL-P15-ADD-2015-2024-EXTERNAL-LOANS-V1",lifecycle_status="diagnostic",
    release_state="private_exploratory_normalization_no_valuation")
  write_table <- function(d,name) {
    d <- copy(d);for(nm in names(meta))set(d,j=nm,value=meta[[nm]])
    d[,lineage_parent_ids:=paste(c(file.path(base,"other_data/add_2026may_a.xlsx"),
      "scripts/p15/loan_extension/normalize_add.R"),collapse=";")]
    fwrite(d,file.path(out,paste0(name,".csv")),na="")
  }
  write_table(x,"normalized_loans")
  write_table(z$audit,"wb_raw_identity_and_date_audit")
  write_table(date_summary,"wb_source_definition_validation")
  write_table(x[,.(records=.N,eligible=sum(normalization_eligible),
    central_eligible=sum(normalization_eligible&central_scope_eligible),
    usd_eligible=sum(normalization_eligible&official_USD_view),
    central_usd_eligible=sum(normalization_eligible&central_scope_eligible&official_USD_view),
    quality_eligible=sum(normalization_quality_eligible),
    unresolved_zero_rate_eligible=sum(normalization_eligible&zero_rate_uncertain),
    countries=uniqueN(iso3),source_grace_gt_span=sum(source_grace_gt_reported_maturity)),
    by=.(creditor_group,creditor)],"normalization_summary")
  write_table(x[,.(records=.N),by=.(normalization_eligible,exclusion_reason)],"exclusion_summary")
  write_table(x[zero_rate_uncertain | !is.na(ida_zero_product_pattern)],"ida_zero_rate_classification")
  write_table(checks,"checks")
  manifest <- function(paths)data.table(path=paths,bytes=file.info(paths)$size,
    sha256=vapply(paths,function(p)digest(file=p,algo="sha256"),character(1)))
  inputs <- c(file.path(base,"other_data",c("add_2026may_a.xlsx","add_2025_paper.pdf",
    "add_2025_paper.txt","ida_terms_20221001.pdf","ida_codebook_web_extract.json")),
    list.files(file.path(base,"mpg/extracted/input"),pattern="^(IBRD|IDA).*csv$",full.names=TRUE),
    file.path(base,"mpg/extracted/dofiles/Chinese_World_Bank_Lending_Terms_Database_Creation_20200228_FINAL.do"))
  fwrite(manifest(inputs),file.path(out,"input_manifest.csv"))
  fwrite(manifest(c("scripts/p15/loan_extension/normalize_add.R","scripts/p15/activate_p15_environment.R","renv.lock")),
    file.path(out,"code_manifest.csv"))
  write_json(c(meta,list(source_snapshot_ids=c("LEF-ADD-2026MAY-XLSX-20260910",
    "SRC-MPG-2020-REPLICATION-ZIP-20260910","LEF-ADD-2025-PAPER-20260910",
    "LEF-IDA-TERMS-20221001-20260910","LEF-IDA-CODEBOOK-WEB-20260910"),
    normalization_rule="For WB only: total horizon equals source maturity plus source grace; preserve effective-to-first endpoint. Other sources use reported total horizon. No source rates imputed.",
    quality_rule="Unresolved zero-rate IDA rows remain numerically eligible but carry an exclusion flag for the separate quality view. Non-central borrowers have separate scope flags.",
    limitations="No economic loan event IDs in ADD; no full repayment schedule; constant snapshot/commitment-reference rate scenario; no FX or currency discount-curve adjustment.")),
    file.path(out,"version_bundle.json"),auto_unbox=TRUE,pretty=TRUE)
  writeLines(capture.output(sessionInfo()),file.path(out,"environment.txt"))
  fwrite(manifest(list.files(out,full.names=TRUE)),file.path(out,"output_manifest.csv"))
  cat("ADD normalization:",nrow(x),"retained;",sum(x$normalization_eligible),"numerically eligible;",out,"\n")
}
