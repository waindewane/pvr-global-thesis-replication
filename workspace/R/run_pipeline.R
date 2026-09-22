legacy_override <- Sys.getenv("PVR_ALLOW_LEGACY_TOP_LEVEL_WRITE", unset = "")
if (!identical(legacy_override, "YES-I-UNDERSTAND")) {
  stop(
    paste(
      "R/run_pipeline.R is temporarily disabled by PIPE-01.",
      "It invokes the frozen P8 2024 route and can overwrite shared output/tables",
      "with results that differ from _targets.R, P13, and P14.",
      "No official build product has been selected yet (PIPE-03).",
      "See docs/AUDIT_ACTION_REGISTER_2026-07-17.md and docs/PROJECT_STATUS.md.",
      "For an explicitly authorized legacy reproduction only, set",
      "PVR_ALLOW_LEGACY_TOP_LEVEL_WRITE=YES-I-UNDERSTAND in that isolated run."
    ),
    call. = FALSE
  )
}
warning("PIPE-01 override active: running the frozen P8 2024 route.", call. = FALSE)

source("experiments/full_ladder_ratings_integrated_status_rebuild_2026-06-01/scripts/build_p8_canonical_outputs_2024.R")
