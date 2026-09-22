#!/usr/bin/env Rscript
# Mechanical reconciliation of the explicit 2026-09-06 completion decisions.
# Never touches scientific inputs, frozen outputs, or the original task inventory.
library(data.table)
note <- "docs/governance/P15_BUCKETS_1_2_RESEARCH_DECISIONS_2026-09-06.md"
proof <- "data-derived/p15_bucket_completion_acceptance_20260906"
run <- "data-derived/p15_full_replay_20260906_112727"
stopifnot(file.exists(file.path(proof,"independent_run_comparisons.csv")))
stopifnot(all(fread("data-derived/p15_full_replay_20260906_113432/acceptance.csv")$passed))
registry <- "docs/governance/source_package_registry.csv"
reg <- fread(registry,colClasses="character")
add_source <- function(id,path,owner,access,vintage,scope,route,role) {
  stopifnot(file.exists(path))
  row <- data.table(source_package_id=id,path=path,
    sha256=digest::digest(path,algo="sha256",file=TRUE),source_owner=owner,
    access_class=access,acquisition_or_capture_date="2026-09-06",source_vintage=as.character(vintage),
    scope=scope,extraction_route=route,immutable_state="preserved_immutable",approved_role=role)
  if(id %in% reg$source_package_id) {
    old <- reg[source_package_id==id]
    stopifnot(nrow(old)==1L,identical(old$sha256,row$sha256),identical(old$path,row$path))
    reg <<- rbind(reg[source_package_id!=id],row,use.names=TRUE)
  } else reg <<- rbind(reg,row,use.names=TRUE)
}
add_source("SRC-P15-RAW-CLOSURE-20260906",file.path(run,"input_manifest.csv"),
  "Project source-closure manifest","restricted_private","preserved source vintages recorded per leaf",
  "2012-2024 candidate evidence raw/source/code closure","isolated local build",
  "Exact input inventory; not a new observation source or ladder promotion")
docs <- fread("data-raw/p15_case_review_20260906/source_manifest.csv")
for(i in which(!is.na(docs$sha256)&nzchar(docs$sha256))) {
  add_source(docs$source_snapshot_id[i],docs$artifact_path[i],"Public source owner; URL in document_sources.csv",
    "public","document-specific date and locator in case ledger","bounded IDS and collateral-bond review",
    "direct source-owner download",paste("Corroboration:",docs$support[i]))
}
api <- fread("data-raw/p15_ids_review_public_snapshot_20260906/source_manifest.csv",colClasses="character")
for(i in seq_len(nrow(api))) add_source(paste0("SRC-P15-IDS-AUDIT-",sub("\\.json$","",basename(api$path[i])),"-20260906"),
  api$path[i],"World Bank International Debt Statistics","public",api$source_lastupdated[i],
  "14 countries; 2012-2024; Bondholders counterpart BND","source-6 multidimensional API",
  "Independent audit only; does not replace the registered calculation snapshot")
curated <- c("data-raw/p15_curated_inputs_20260906/geography_and_static_labels.csv",
  "data-raw/p15_curated_inputs_20260906/legacy_feature_row_permissions.csv",
  "data-raw/p15_curated_inputs_20260906/preserved_input_roles.csv",
  "data-raw/p15_case_review_20260906/ids_case_dispositions.csv",
  "data-raw/p15_case_review_20260906/ids_decision_definitions.csv",
  "data-raw/p15_case_review_20260906/observed_issue_dispositions.csv")
for(p in curated) add_source(paste0("SRC-P15-CURATED-",toupper(sub("\\.csv$","",basename(p))),"-20260906"),
  p,"Project documented curation","restricted_private","2026-09-06 curation of preserved evidence",
  "Explicit keys permissions and explanations","source-backed manual decision input",
  paste("Candidate build input; see",note))
stopifnot(!anyDuplicated(reg$source_package_id))
fwrite(reg,registry)

path <- "docs/governance/audit_master_status_ledger_2026-08-09.csv"
ledger <- fread(path,colClasses="character")
amend <- function(id,status,basis,interpretation,next_action,evidence=note) {
  at <- which(ledger$task_id==id)
  stopifnot(length(at)==1L)
  set(ledger,at,"current_status",status)
  set(ledger,at,"status_basis",basis)
  set(ledger,at,"current_interpretation",interpretation)
  set(ledger,at,"next_action",next_action)
  set(ledger,at,"completion_evidence",paste(evidence,proof,sep="; "))
  set(ledger,at,"last_reconciled","2026-09-06")
}
amend("PIPE-06","complete","Normal targets entrypoint completes the declared current candidate evidence build",
  "Current product is reviewed evidence, not a final selected or canonical ladder.",
  "Extend this same graph when final selection is approved.","_targets.R; R/p15_full_replay.R")
amend("PIPE-07","complete","Direct replay runner invokes the same function as targets; old runner formally deprecated",
  "R/run_pipeline.R remains guarded historical code; it is not a supported current-product runner.",
  "Keep wrappers non-divergent.","scripts/p15/run_p15_full_replay.R; docs/governance/P15_LOCAL_DATASET_GUIDE_2026-09-06.md")
amend("PIPE-09","complete","Complete raw-to-reviewed execution succeeds with operating-system network denial",
  "Uses explicit local source and curated inputs; no source refresh or alternative method on network failure.",
  "Repeat offline checks when adding a source or stage.",paste(note,"data-derived/p15_full_replay_20260906_113432/acceptance.csv",sep="; "))
amend("PIPE-10","partial","Locked environment and required source/code paths are checked; isolated destination and case guards enforced",
  "Current evidence runs are guarded; a single complete schema/method preflight for the eventual promoted ladder remains broader.",
  "Retain the evidence preflight and add final-selection schema and method-authority checks before release.")
amend("PIPE-11","partial","Successful full runs record exact raw/code input hashes environment output hashes and dated acceptance receipts",
  "Failed development runs retain stage logs and input manifests; uniform structured failure receipts remain to standardize.",
  "Add uniform failed-run status receipts at the release-hardening gate; preserve current successful-run contracts.")
amend("PIPE-12","complete","Independent full targets builds and direct offline wrapper have identical substantive output hashes",
  "166 tables match per repeat; volatile run paths and manifests intentionally record their own execution identity.",
  "Require the same proof after final-selection integration.")
amend("REP-03","partial","Full orchestration records exact leaf hashes including source archives and manually curated decisions",
  "Every source of the current product is inventoried; universal native manifests in every historical standalone builder are not claimed.",
  "Use the full orchestrated route; retain broader standalone-builder manifest work as applicable.")
amend("REP-04","partial","All 64 locked R packages and R4.3.3 checked; Python dependencies pinned; full scripts and environment recorded",
  "Missing packages restored in a project-only library on this Mac; an independent fresh-machine restoration is not tested.",
  "Test a fresh-machine restore before portable final-release acceptance.")
amend("REP-17","partial","Source-only isolated builds including OS-network-denied run reproduce the current candidate",
  "Private source closure and code copies are explicit; independently provisioned clean-clone environment recovery remains broader.",
  "Perform final artifact-bundle and fresh-environment recovery after selection and release packaging.")
amend("IDS-04","partial","All 30 initial anomalies have source-backed dispositions; newly identified Angola collateral issue is held",
  "Bounded source-quality treatment review is complete; this does not identify every cause or review every large source-to-source gap.",
  "Reopen held cases only on recorded new evidence; separately assess wider discrepancies during ladder validation.")
amend("IDS-05","decision_recorded","IDS contractual proxy role retained with explicit case-specific admissibility",
  "Eight inconsistent term pairs do not automatically invalidate interest; 22 all-country benchmark holds preserve raw values.",
  "Use reviewed eligibility in the final IDS-versus-secondary ordering review.")
amend("TERM-11","partial","Rate use and repayment-profile use separated; eight inconsistent pairs blocked and ambiguous case profiles held",
  "Ordered aggregate grace and maturity do not identify bond cash flows or settle bullet versus amortizing structure.",
  "Validate an actual PVR term object before PVR use; no term clipping or automatic schedule inference.")
amend("STAT-06","partial","Raw-to-reviewed replay preserves existing case explanations and status permissions",
  "The new case holds propagate to anchors peers validation and unapproved previews; selected/PVR consumers remain unclosed.",
  "Apply these same permissions to final selection and PVR consumers.")
amend("TEST-06","partial","Country-year and historical joins pass complete tests and three raw rebuild comparisons",
  "Current evidence grid and case joins verified; future final-selection and PVR joins remain to test.",
  "Retain current join tests and extend to final consumers.")
amend("LAD-06","pending_prerequisite","Four reviewed deterministic previews supplement the eight preserved pre-review alternatives",
  "Source review is implemented; exact final hierarchy and rating/peer promotion still require their substantive gates.",
  "Review the concrete source-order and fallback consequences; then implement the approved selection register.")
stopifnot(nrow(ledger)==292L,!anyDuplicated(ledger$task_id))
fwrite(ledger,path)
fwrite(ledger[,.N,by=current_status][order(current_status)],file.path(proof,"master_ledger_status_counts.csv"))

path <- "docs/governance/p15_current_method_decision_registry_2026-08-09.csv"
m <- fread(path)
rows <- data.table(item_id=c("CURRENT-IDS-02","CURRENT-PRI-09","CURRENT-PIPE-01"),
  register_task=c("IDS-04;IDS-05;TERM-11","PRI-05;PRI-06;STAT-06","PIPE-06;PIPE-07;PIPE-09;PIPE-12"),
  topic=c("Case-specific IDS source and term use","Angola collateral-financing restriction","Current evidence-product reproducibility"),
  current_state="implemented_candidate_not_promoted",
  governing_direction=c("Preserve all raw IDS values; eight inconsistent term pairs block repayment use but not automatically rates; 22 benchmark holds reflect individual source or comparability evidence.",
    "Hold Angola 2024 sole USD primary issue XS2965710598 as collateral/TRS rather than ordinary cash-raising issuance; preserve the source and secondary anchor.",
    "Rebuild raw evidence and explicit case judgments through one isolated targets/direct function with locked environment and exact source/output hashes."),
  implementation_or_next_gate=c("Use reviewed eligibility in the final ladder review; reopen holds only with the specified missing evidence.",
    "Peer seeds and rating validation recomputed; changed source identity or added issues requires fresh aggregation and review.",
    "Final source ordering and thesis release remain separate approvals."),
  supersedes_or_qualifies=c("Replaces temporary undifferentiated low-rate holds in the older preview only; no universal low-rate deletion.",
    "Qualifies primary amount and ordinary-issuance interpretation; does not change P13 baseline parity or invent a replacement rate.",
    "Supersedes the six-stage cached-input boundary; remaining historical fixtures are explicitly comparison-only."),
  decision_source=paste("docs/DECISIONS.md",note,sep=";"))
for(i in seq_len(nrow(rows))) {
  m <- m[item_id!=rows$item_id[i]]
  m <- rbind(m,rows[i],use.names=TRUE)
}
stopifnot(!anyDuplicated(m$item_id))
fwrite(m,path)
cat("Registered",nrow(reg),"source packages and reconciled",nrow(ledger),"parent tasks.\n")
