#!/usr/bin/env Rscript
# Illustrative arithmetic only; no empirical loans or benchmark data are read.
source("scripts/p15/activate_p15_environment.R")
source("R/research_governance.R")
source("R/pvr.R")
source("R/p15_pv_first_pass.R")
source("R/p15_bullet_extension.R")
args <- commandArgs(TRUE)
out <- if (length(args)) args[1] else "data-derived/p15_concept_illustration_20260912_v1"
if (dir.exists(out)) stop("Refusing to overwrite: ", out)

schedule <- data.frame(time_years=c(0,1,2), advance=c(100,0,0),
                       principal=c(0,0,100), interest=c(0,5,5))
schedule$repayment <- schedule$principal + schedule$interest
rates <- c(3,5,10)
values <- 5/(1+rates/100) + 105/(1+rates/100)^2
table <- data.frame(discount_rate_pct=rates, advance=100, nominal_repayments=110,
                    repayment_pv=values, pvr_ratio=values/100,
                    pvr_per_100=values, grant_equivalent=100-values,
                    grant_element_pct=100-values)
production <- p15_bullet_equal_schedule(5,2,2)
actual <- vapply(rates, function(r) p15_stream_pv(production,r), numeric(1))
exact <- c(1101500/10609, 100, 11050/121)
checks <- data.frame(check=c("scheduled_payments_match_current_function",
  "direct_formula_matches_current_pv_function", "independent_rational_arithmetic",
  "per_100_and_grant_element_identity", "contractual_payment_sum_fixed"),
  passed=c(identical(as.numeric(production$debt_service),c(5,105)),
    max(abs(values-actual))<1e-10, max(abs(values-exact))<1e-10,
    all(abs(table$pvr_per_100+table$grant_element_pct-100)<1e-10),
    sum(schedule$repayment)==110))
stopifnot(all(checks$passed))

ids <- list(build_id="P15-CONCEPT-ILLUSTRATION-20260912-V1",
  schema_id="SCH-P15-CONCEPT-ILLUSTRATION-V1",
  estimator_id="EST-P15-ILLUSTRATIVE-PROMISED-PAYMENT-PV-V1",
  admissibility_id="ADM-ILLUSTRATIVE-NO-EMPIRICAL-SAMPLE",
  selection_id="SEL-NONE-ILLUSTRATIVE-REFERENCES",
  source_package_ids="SRC-AUTHOR-SYNTHETIC-LOAN-20260912")
dir.create(out,recursive=TRUE)
for (n in c("schedule","table","checks")) {
  d <- get(n)
  for (k in names(ids)) d[[k]] <- ids[[k]]
  d$lifecycle_status <- "diagnostic"
  d$release_state <- "private_research"
  write.csv(d,file.path(out,paste0(n,".csv")),row.names=FALSE,na="")
}
manifest <- function(paths,role) do.call(pvr_manifest_rows,
  c(list(paths=paths,artifact_role=role),ids))
code <- c("scripts/p15/build_concept_illustration_20260912.R",
  "scripts/p15/_targets_concept_illustration.R", "scripts/p15/activate_p15_environment.R",
  "R/pvr.R", "R/p15_pv_first_pass.R", "R/p15_bullet_extension.R",
  "R/research_governance.R", "renv.lock")
write.csv(manifest(code,"illustration_input_and_code"),
          file.path(out,"input_manifest.csv"),row.names=FALSE)
write.csv(manifest(code,"illustration_code"),
          file.path(out,"code_manifest.csv"),row.names=FALSE)
jsonlite::write_json(c(ids,list(lifecycle_status="diagnostic",
  release_state="private_research", synthetic=TRUE, empirical_outputs_changed=FALSE,
  assumptions="100 at time zero; annual interest 5; principal 100 at year 2; no fees/default; flat illustrative reference",
  policy_note="3, 5 and 10 percent are illustrative rates, not changes to accepted reference conventions")),
  file.path(out,"version_bundle.json"),pretty=TRUE,auto_unbox=TRUE)
writeLines(capture.output(sessionInfo()),file.path(out,"environment.txt"))
write.csv(manifest(list.files(out,full.names=TRUE),"illustration_output"),
          file.path(out,"output_manifest.csv"),row.names=FALSE)
print(table)
