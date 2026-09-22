# Run from the project root. Read-only checks of the existing numerical evidence
# used in the Chapter 8 conclusion; this script does not alter analytical outputs.
run <- jsonlite::fromJSON("data-derived/p15_master/current_run.json")
crs <- read.csv(file.path(run$stages$crs_modern_summary$dir, "modern_summary.csv"))
main <- subset(crs, benchmark_view == "non_peer" & weighting == "financing_record_equal")
stopifnot(nrow(main) == 1L)
accuracy <- read.csv(file.path(run$stages$benchmark_inference$dir, "paired_loss_inference.csv"))
modern <- subset(accuracy, family == "modern_tier_vs_dac" & sample_view == "full_validation")
checks <- data.frame(
  check = c("Main CRS sample", "Main CRS mean difference, rounded", "Four modern benchmark comparisons"),
  observed = c(as.character(main$financing_records),
               as.character(round(main$mean_market_minus_standardized_ge_pp, 2)),
               as.character(nrow(modern))),
  passed = c(main$financing_records == 1274,
             round(main$mean_market_minus_standardized_ge_pp, 2) == -5.64,
             nrow(modern) == 4L && setequal(modern$focal, c("ids", "secondary", "moodys", "peer")))
)
for (i in seq_len(nrow(modern))) {
  checks <- rbind(checks, data.frame(
    check = paste("Modern MAE lower than standardized reference:", modern$focal[i]),
    observed = paste(modern$focal_mae_pp[i], "<", modern$comparator_mae_pp[i]),
    passed = modern$focal_mae_pp[i] < modern$comparator_mae_pp[i]
  ))
}
write.csv(checks, "docs/thesis_design/sections/discussion_and_conclusion/prose_20260922/evidence_checks.csv", row.names = FALSE)
stopifnot(all(checks$passed))
cat(nrow(checks), "evidence checks passed.\n")
