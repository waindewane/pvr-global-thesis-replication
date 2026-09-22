source("scripts/p15/activate_p15_environment.R")
library(targets)
experiment_dir <- "experiments/p15_peer_rating_expansion_20260912"
stage_names <- c("build_features.R", "fit_models.R", "audit_public_ratings.R",
 "audit_current_web_ratings.R", "explore_peers.R", "additional_models.R",
 "additional_peers.R", "leakage_check.R", "uncertainty.R", "summarize_and_value.R")
run_offline_exploration <- function(script_files, snapshot_manifest) {
 stopifnot(all(file.exists(script_files)), file.exists(snapshot_manifest))
 for (script in script_files) {
  status <- system2(file.path(R.home("bin"), "Rscript"), shQuote(script))
  if (status != 0L) stop("Exploration stage failed: ", script)
 }
 file.path(experiment_dir, c("all_peer_coverage.csv", "all_peer_rule_counts.csv",
   "strict_peer_validation.csv", "crs_diagnostic_valuation_summary.csv"))
}
list(
 tar_target(exploration_scripts, file.path(experiment_dir, stage_names), format="file"),
 tar_target(exploration_snapshot, file.path(experiment_dir, "production_input_manifest.csv"), format="file"),
 tar_target(exploration_tables, run_offline_exploration(exploration_scripts, exploration_snapshot), format="file")
)
