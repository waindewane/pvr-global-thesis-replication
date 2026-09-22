#!/usr/bin/env Rscript
# Read-only source audit: inspect published inputs and reproduce their saved 5% formula.
source("scripts/p15/activate_p15_environment.R")
suppressPackageStartupMessages({library(data.table);library(jsonlite);library(digest);library(readxl)})
args <- commandArgs(TRUE)
out <- if(length(args)) args[1] else "data-derived/p15_mpg_archive_inspection_20260910_v1"
stopifnot(!dir.exists(out));dir.create(out,recursive=TRUE)
base <- "sources/literature_review/loan_extension_feasibility_20260910/mpg/extracted"
source_id <- "SRC-MPG-2020-REPLICATION-ZIP-20260910"
script <- "scripts/p15/loan_extension/inspect_mpg_archive.R"
manifest <- fread(file.path(dirname(base),"extracted_manifest.csv"))
stopifnot(all(vapply(manifest$path,function(p)digest(file=p,algo="sha256"),character(1))==manifest$sha256))
x <- fread(file.path(base,"Full Dataset.csv"),colClasses="character",na.strings="",strip.white=FALSE)
x[, source_row:=.I]
numeric_cols <- c("year","amount","originalamount","interest","maturity","grace","grantelement","grantelement_b")
for(nm in numeric_cols)set(x,j=nm,value=suppressWarnings(as.numeric(x[[nm]])))
x[, creditor:=fcase(source=="China","China",type_d=="Loan: IDA","IDA",grepl("IBRD",type_d),"IBRD",default="WB grants")]
x[, `:=`(paper_population=country!="China",loan=type=="Loan",within_benchmark_period=year>=2012 & year<=2024)]
x[, numeric_terms_complete:=is.finite(interest)&is.finite(maturity)&is.finite(grace)]
x[, `:=`(nonnegative_terms=numeric_terms_complete & interest>=0 & maturity>=0 & grace>=0,
  literal_horizon_compatible=numeric_terms_complete & interest>=0 & maturity>grace & grace>=0,
  grace_ge_maturity=numeric_terms_complete & grace>=maturity,
  repayment_definition=ifelse(source=="World Bank","maturity_is_first_to_last_repayment_span;grace_effective_to_first","maturity_and_grace_from_aiddata"),
  source_interest_rate_pct=100*interest)]
# This reproduces their original expression only; it is not a repaired repayment model.
d <- sqrt(1.05)-1
x[, formula_replayed:=NA_real_]
x[numeric_terms_complete & maturity!=grace,formula_replayed:=(1-(interest/2)/d)*(1-((1/(1+d)^(2*grace)-1/(1+d)^(2*maturity))/(d*(2*maturity-2*grace))))]
x[, formula_difference:=formula_replayed-grantelement]
x[, numeric_agreement:=is.finite(formula_difference)&abs(formula_difference)<1e-6]
metadata <- list(build_id=basename(out),schema_id="SCHEMA-P15-MPG-SOURCE-INSPECTION-V1",estimator_id="EST-P15-MPG-SAVED-FORMULA-AUDIT-V1",admissibility_id="ADM-P15-SOURCE-INSPECTION-NO-PROMOTION-V1",selection_id="SEL-P15-SOURCE-INSPECTION-NO-SELECTION-V1",source_package_ids=source_id,lifecycle_status="diagnostic",release_state="source_inspection_not_accepted_valuation")
write_table <- function(z,name,parents=file.path(base,"Full Dataset.csv")) {
 z<-copy(as.data.table(z));for(nm in names(metadata))set(z,j=nm,value=metadata[[nm]])
 z[,lineage_parent_ids:=paste(c(parents,script),collapse=";")]
 fwrite(z,file.path(out,paste0(name,".csv")),na="")
}
write_table(x,"source_record_audit")
write_table(x[,.(records=.N,countries=uniqueN(country),loans=sum(loan),complete_loan_terms=sum(loan & numeric_terms_complete),literal_horizon_compatible_loans=sum(loan & literal_horizon_compatible),loan_grace_ge_maturity=sum(loan&grace_ge_maturity),saved_loan_ge=sum(loan&is.finite(grantelement))),by=.(paper_population,source,creditor,year)],"coverage_by_creditor_year")
write_table(x[paper_population & loan,.(loans=.N,complete_terms=sum(numeric_terms_complete),saved_ge=sum(is.finite(grantelement)),grace_ge_maturity=sum(grace_ge_maturity),formula_compared=sum(is.finite(formula_difference)),max_abs_formula_difference=max(abs(formula_difference),na.rm=TRUE)),by=.(source,creditor,within_benchmark_period)],"loan_inspection_summary")
write_table(x[loan & numeric_terms_complete & (grace_ge_maturity | !is.finite(grantelement)),.(id,country,countrycode,year,creditor,source_interest_rate_pct,maturity,grace,grantelement,repayment_definition)],"schedule_definition_flags")
raw_summaries <- list();raw_fields<-list();currencies<-list()
for(kind in c("IBRD","IDA")) {
 file<-list.files(file.path(base,"input"),pattern=paste0("^",kind,".*csv$"),full.names=TRUE)
 z<-fread(file,colClasses="character",na.strings="")
 yr<-as.integer(format(as.Date(z[["Agreement Signing Date"]],format="%m/%d/%Y"),"%Y"))
 missing_year<-is.na(yr);yr[missing_year]<-as.integer(format(as.Date(z[["Board Approval Date"]][missing_year],format="%m/%d/%Y"),"%Y"))
 # Preserve the reported calendar year; Stata's numeric date implementation is inspected separately.
 raw_summaries[[kind]]<-data.table(input=basename(file),lending_year=yr)[,.(records=.N),by=.(input,lending_year)]
 raw_fields[[kind]]<-data.table(input=basename(file),field=names(z))
 currencies[[kind]]<-data.table(input=basename(file),currency=z[["Currency of Commitment"]],lending_year=yr)[,.(records=.N),by=.(input,currency,lending_year)]
}
wb_inputs <- list.files(file.path(base,"input"),pattern="^(IBRD|IDA).*csv$",full.names=TRUE)
write_table(rbindlist(raw_summaries),"original_wb_inputs_year_coverage",wb_inputs)
write_table(rbindlist(raw_fields),"original_wb_inputs_fields",wb_inputs)
write_table(rbindlist(currencies),"original_wb_inputs_currencies",wb_inputs)
aidfile<-file.path(base,"input","GlobalChineseOfficialFinanceDataset_v1.0.xlsx")
sheets<-excel_sheets(aidfile)
aid_fields<-rbindlist(lapply(sheets,function(sh)data.table(sheet=sh,field=names(read_excel(aidfile,sheet=sh,n_max=0,.name_repair="minimal")))) )
write_table(aid_fields,"original_aiddata_fields",aidfile)
checks<-data.table(check=c("archive_member_hashes","unique_source_ids","published_population_reconciles","published_china_complete_terms_reconciles","source_interest_in_fraction_units","saved_five_percent_formula_reproduces","no_benchmark_or_market_valuation_input"),passed=c(TRUE,!anyDuplicated(x$id),nrow(x[paper_population == TRUE])==7312,nrow(x[paper_population & source=="China" & loan & numeric_terms_complete])==268,max(x$interest,na.rm=TRUE)<1,all(abs(x$formula_difference[is.finite(x$formula_difference)])<1e-6),TRUE))
fwrite(checks,file.path(out,"checks.csv"));stopifnot(all(checks$passed))
files<-unique(c(manifest$path,file.path(dirname(base),"extracted_manifest.csv"),file.path(dirname(base),"archive_retrieval_and_inventory.json")))
make_manifest<-function(paths)data.table(path=paths,sha256=vapply(paths,function(p)digest(file=p,algo="sha256"),character(1)),bytes=file.info(paths)$size,source_snapshot_id=source_id)
fwrite(make_manifest(files),file.path(out,"input_manifest.csv"))
fwrite(make_manifest(c(script,"scripts/p15/activate_p15_environment.R","renv.lock")),file.path(out,"code_manifest.csv"))
write_json(c(metadata,list(inspected_utc=format(Sys.time(),"%Y-%m-%dT%H:%M:%S%z"),formula_scope="source_expression_only_no_market_revaluation",warnings="Source maturity meanings differ; complete numbers do not establish valid economic schedules; no inferred loan currency")),file.path(out,"version_bundle.json"),pretty=TRUE,auto_unbox=TRUE)
writeLines(capture.output(sessionInfo()),file.path(out,"environment.txt"))
fwrite(make_manifest(list.files(out,full.names=TRUE)),file.path(out,"output_manifest.csv"))
cat("Saved source inspection:",out,";",nrow(checks),"checks passed\n")
