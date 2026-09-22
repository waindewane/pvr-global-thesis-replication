# Run from the project root. Verify existing summaries without changing any analysis.
pointer <- jsonlite::fromJSON("data-derived/p15_master/current_run.json")
folder <- pointer$stages$statistical_review$dir
details <- read.csv(file.path(folder, "benchmark_error_details.csv"))
published <- read.csv(file.path(folder, "benchmark_error_metrics.csv"))
checks <- lapply(seq_len(nrow(published)), function(i) {
  row <- published[i, ]
  z <- details[details$scenario == row$scenario & details$tier == row$tier, ]
  stopifnot(!anyDuplicated(z[c("iso3", "analysis_year")]))
  e <- z$focal_rate - z$primary
  p <- z$comparator_rate - z$primary
  advantage <- abs(p) - abs(e)
  values <- c(records=nrow(z), countries=length(unique(z$iso3)),
    years=length(unique(z$analysis_year)), focal_bias_pp=mean(e),
    comparator_bias_pp=mean(p), focal_mae_pp=mean(abs(e)),
    comparator_mae_pp=mean(abs(p)), focal_rmse_pp=sqrt(mean(e^2)),
    comparator_rmse_pp=sqrt(mean(p^2)), improvement_pp=mean(advantage),
    equal_country_improvement_pp=mean(tapply(advantage,z$iso3,mean)),
    equal_year_improvement_pp=mean(tapply(advantage,z$analysis_year,mean)))
  discrepancy <- max(abs(values - as.numeric(row[1,names(values)])))
  stopifnot(discrepancy < 1e-10, max(abs(e-z$focal_error)) < 1e-10,
            max(abs(p-z$comparator_error)) < 1e-10)
  data.frame(scenario=row$scenario,tier=row$tier,quantities_checked=length(values),
             maximum_absolute_difference=discrepancy,passed=TRUE)
})
write.csv(do.call(rbind,checks),
 "docs/thesis_design/sections/benchmark_evaluation/evaluation_design_20260920/metric_verification.csv",row.names=FALSE)
cat("Verified",nrow(published),"scenario/method summaries and their country-year keys.\n")
