#!/usr/bin/env Rscript
# Offline, isolated replay of case evidence -> status rules -> observed integration
# -> assembled dataset -> previews. Market/IDS/rating upstream leaves remain cached.
suppressPackageStartupMessages({library(data.table); library(digest)})
source("R/p15_replay.R")
source("R/research_governance.R")
root <- normalizePath(getwd(), winslash = "/")
run_id <- paste0("p15_case_aware_replay_", format(Sys.time(), "%Y%m%d_%H%M%S"))
report_dir <- file.path(root, "data-derived", run_id)
if (dir.exists(report_dir)) stop("Replay report already exists; retry with a new run ID")
dir.create(report_dir, recursive = TRUE)
stages <- data.frame(
  script = paste0("scripts/p15/", c("build_p15_status_context_evidence.R",
    "build_p15_status_source_expansion.R", "build_p15_status_sanity_approved_rules.R",
    "build_p15_integrated_observed_candidate.R", "build_p15_local_completion.R",
    "preview_p15_local_ladder.R")),
  manifest = c(paste0("docs/governance/", c("p15_status_context_evidence_manifest.csv",
    "p15_status_source_expansion_manifest.csv", "p15_status_sanity_approved_manifest.csv",
    "p15_integrated_observed_manifest.csv")),
    "data-derived/p15_local_completion_2012_2024_20260906_v1/p15_build_manifest.csv",
    "data-derived/p15_local_ladder_previews_20260906_v1/p15_preview_manifest.csv"))
manifests <- lapply(stages$manifest, fread)
outputs <- unique(unlist(lapply(manifests, function(m)
  m$artifact_path[grepl("generated.*output|preview_output", m$artifact_role)])))
inputs <- unique(unlist(lapply(manifests, function(m)
  m$artifact_path[!grepl("generated.*output|preview_output", m$artifact_role)])))
# All stage products (including their manifests) must be built, never seeded.
leaves <- setdiff(inputs, c(outputs, stages$manifest))
code <- c(list.files("R", pattern = "\\.R$", full.names = TRUE), stages$script,
  "scripts/p15/replay_p15_case_aware_build.R", "renv.lock",
  "docs/DECISIONS.md", "docs/governance/source_package_registry.csv")
staged <- p15_replay_safe_paths(unique(c(leaves, code)))
if (any(!file.exists(staged))) stop("Missing replay inputs: ", paste(staged[!file.exists(staged)], collapse = ";"))
original_hashes <- vapply(staged, digest, character(1), algo = "sha256", file = TRUE)
input_manifest <- pvr_manifest_rows(staged,
  ifelse(staged %in% code, "code_environment_or_decision", "cached_leaf_input"),
  build_id = run_id, schema_id = "SCHEMA-P15-CASE-REPLAY-V1",
  estimator_id = "EST-P15-UNCHANGED-REPLAY", admissibility_id = "ADM-P15-STATUS-SANITY-FORWARD-V1",
  selection_id = "SEL-NONE-REPLAY-ONLY",
  source_package_ids = paste(c("SRC-BOC-BOE-DEBT-2025-20260630",
    "SRC-PARIS-CLUB-AGREEMENTS-20260630", "SRC-P13-DISTRESS-STATUS-20240601",
    "SRC-P14-HIST-STATUS-20260630", "SRC-UCDP-ORGANIZED-VIOLENCE-CY-26.1-20260721",
    "SRC-BFTU-CROSS-BORDER-DFBCBFB-20260721"), collapse = ";"))
fwrite(input_manifest, file.path(report_dir, "p15_replay_input_manifest.csv"))
fwrite(stages, file.path(report_dir, "p15_replay_stages.csv"))
scratch <- tempfile("p15_case_replay_")
dir.create(scratch)
checks <- list()
tryCatch({
  p15_replay_copy(staged, root, scratch)
  stopifnot(!any(file.exists(file.path(scratch, outputs))))
  for (i in seq_len(nrow(stages))) {
    message("Replaying stage ", i, "/", nrow(stages), ": ", basename(stages$script[i]))
    old <- setwd(scratch)
    status <- tryCatch(system2(file.path(R.home("bin"), "Rscript"),
      c("--vanilla", shQuote(stages$script[i])),
      stdout = file.path(report_dir, paste0("stage_", i, ".log")),
      stderr = file.path(report_dir, paste0("stage_", i, ".log"))),
      finally = setwd(old))
    if (status != 0L) stop("Replay stage failed; inspect stage_", i, ".log")
    m <- manifests[[i]]
    stage_outputs <- m$artifact_path[grepl("generated.*output|preview_output", m$artifact_role)]
    for (p in stage_outputs) {
      checks[[p]] <- cbind(artifact_path = p, stage = i,
        p15_replay_compare(file.path(root, p), file.path(scratch, p), root, scratch))
    }
    result <- rbindlist(checks)
    fwrite(result, file.path(report_dir, "p15_replay_output_comparison.csv"))
    if (any(!result$semantic_match)) stop("Replay changed data or case judgments; inspect output comparison")
  }
  panel <- fread(file.path(scratch,
    "data-derived/p15_local_completion_2012_2024_20260906_v1/p15_country_year_dataset.csv"))
  cases <- fread(file.path(scratch,
    "data-derived/p15_status_sanity_approved_2012_2024_v1/p15_approved_status_rule_contract_2012_2024.csv.gz"))
  stopifnot(nrow(cases) == 226L, all(nzchar(cases$case_level_note)))
  joined <- merge(cases, panel, by = c("analysis_year", "iso3"), suffixes = c("_case", "_panel"))
  stopifnot(nrow(joined) == nrow(cases),
    identical(joined$case_level_note, joined$status_case_note),
    identical(joined$ordinary_fallback_selection_permitted_case,
              joined$ordinary_fallback_selection_permitted_panel),
    identical(joined$observed_benchmark_selection_permitted_case,
              joined$observed_benchmark_selection_permitted_panel))
  fwrite(cases, file.path(report_dir, "p15_replayed_case_judgments.csv"), na = "")
  summary <- data.table(check = c("all_stage_outputs_match", "case_notes_and_permissions_survive",
    "source_files_unchanged", "full_market_raw_replay_complete", "locked_targets_environment_verified"),
    passed = c(TRUE, TRUE, identical(original_hashes,
      vapply(staged, digest, character(1), algo = "sha256", file = TRUE)), FALSE, FALSE),
    meaning = c("Exact table values and order; only absolute project prefixes normalized",
      "226 case records replayed through to the country-year dataset",
      "Original input/code/decision file hashes unchanged",
      "Still cached market issue/repair, grid, IDS, rating and validation leaves",
      "This checks the installed R environment; renv/targets closure remains open"))
  fwrite(summary, file.path(report_dir, "p15_replay_acceptance_checks.csv"))
  stopifnot(all(summary$passed[1:3]))
  capture.output(sessionInfo(), file = file.path(report_dir, "p15_replay_environment.txt"))
  # Preserve the actual code/decision inputs, not just hashes of mutable paths.
  old <- setwd(scratch)
  tryCatch(utils::tar(file.path(report_dir, "p15_replay_code_decision_snapshot.tar.gz"),
    files = intersect(staged, code), compression = "gzip", tar = "internal"),
    finally = setwd(old))
  artifacts <- list.files(report_dir, full.names = TRUE)
  output_manifest <- pvr_manifest_rows(artifacts, rep("diagnostic_replay_report", length(artifacts)),
    build_id = run_id, schema_id = "SCHEMA-P15-CASE-REPLAY-V1",
    estimator_id = "EST-P15-UNCHANGED-REPLAY", admissibility_id = "ADM-P15-STATUS-SANITY-FORWARD-V1",
    selection_id = "SEL-NONE-REPLAY-ONLY", source_package_ids = unique(input_manifest$source_package_ids))
  fwrite(output_manifest, file.path(report_dir, "p15_replay_output_manifest.csv"))
  cat("Case-aware replay passed. Report:", report_dir, "\n")
}, finally = {
  setwd(root)
  unlink(scratch, recursive = TRUE)
})
