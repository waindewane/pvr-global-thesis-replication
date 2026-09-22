# Bounded master orchestration: explicit snapshots, immutable stage outputs, note alerts.
p15_master_config <- function(path=Sys.getenv("P15_MASTER_CONFIG","config/p15_master.json")) {
  x<-jsonlite::fromJSON(path);x$config_path<-path;x
}
p15_master_hashes <- function(paths) {
  paths<-sort(unique(paths));stopifnot(length(paths)>0,all(file.exists(paths)))
  data.frame(path=paths,sha256=vapply(paths,digest::digest,character(1),file=TRUE,algo="sha256"))
}
p15_master_key <- function(files,parents=list())digest::digest(list(p15_master_hashes(files),parents),algo="sha256")
p15_master_manifest_paths <- function(path) {
  m<-data.table::fread(path);column<-intersect(c("artifact_path","path"),names(m))[1]
  if(is.na(column))stop("No path column: ",path)
  as.character(m[[column]])
}
p15_master_verify <- function(receipt) {
  m<-readRDS(receipt)
  hashes<-p15_master_hashes(m$manifest$path)
  if(!identical(hashes,m$manifest))stop("Cached output changed or missing: ",receipt,". Restore or use a fresh cache; never silently accept it.")
  if(!is.null(m$external_outputs)) {
    if(!identical(p15_master_hashes(m$external_outputs$path),m$external_outputs))
      stop("Raw replay provenance changed or missing: ",receipt)
  }
  m
}
p15_master_receipt <- function(out,id,key,inputs) {
  files<-list.files(out,full.names=TRUE,recursive=TRUE)
  files<-files[!grepl("receipt.rds$|execution.log$",files)]
  r<-list(id=id,key=key,dir=normalizePath(out),release_state="working_private_candidate_not_canonical",
    manifest=p15_master_hashes(files),inputs=p15_master_hashes(inputs))
  data.table::fwrite(r$inputs,file.path(out,"master_input_manifest.csv"))
  r$manifest<-p15_master_hashes(c(files,file.path(out,"master_input_manifest.csv")))
  saveRDS(r,file.path(out,"receipt.rds"));r
}
p15_master_reference <- function(config,id) {
  if(!is.null(config$reference_stages[[id]]))return(config$reference_stages[[id]])
  if(id=="dac")return(config$reference_dac)
  if(id=="peer_geography_validation")return(config$reference_peer_geography)
  file.path(config$reference_research,id)
}
p15_master_external <- function(config,id) {
  ref<-p15_master_reference(config,id)
  if(id=="pv_three_block")return(character())
  input_manifest<-file.path(ref,c("input_manifest.csv","input_code_manifest.csv"))
  input_manifest<-input_manifest[file.exists(input_manifest)]
  if(!length(input_manifest))stop("No registered input manifest for ",id)
  paths<-p15_master_manifest_paths(input_manifest[1])
  dynamic<-c(config$reference_candidate,config$reference_research,config$reference_dac,
    config$cache_root,unlist(config$reference_stages,use.names=FALSE),
    "data-derived/p15_analysis_candidate_20260907_v1","data-derived/p15_policy_comparison_20260908_v2",
    "data-derived/p15_reference_review_20260907_v1/input_manifest.csv")
  dynamic<-unique(c(dynamic,normalizePath(dynamic,mustWork=FALSE)))
  # Derived parent outputs may be recorded with absolute or relative paths.
  # Their current contents are tracked through parent keys, not pinned as sources.
  paths<-paths[!Reduce(`|`,lapply(dynamic,function(p)paths==p | startsWith(paths,paste0(p,"/"))))]
  # A pinned input manifest may itself reference files that its consumer verifies.
  more<-unlist(lapply(paths[grepl("manifest[.]csv$",paths)],function(p) {
    m<-data.table::fread(p);col<-intersect(c("artifact_path","path"),names(m))[1]
    if(is.na(col))return(character());as.character(m[[col]])
  }))
  # The DAC source register also records an unavailable download. Its status is
  # preserved in that register; it is not an input consumed by the calculation.
  more<-as.character(more)
  sort(unique(c(paths,more[file.exists(more)])))
}
p15_master_code <- function(config,id,script) {
  ref<-p15_master_reference(config,id)
  manifest<-file.path(ref,c("script_manifest.csv","code_manifest.csv"))
  manifest<-manifest[file.exists(manifest)]
  code<-if(length(manifest))p15_master_manifest_paths(manifest[1]) else character()
  if(id=="pv_three_block")code<-c("R/pvr.R","R/p15_pv_first_pass.R","R/p15_bullet_extension.R","R/p15_pv_interpretation.R")
  # Current validation and routing helpers are real dependencies of these stages.
  extra <- unlist(config$extra_stage_code[[id]],use.names=FALSE)
  sort(unique(c(code,script,extra,"R/p15_current_inputs.R","R/p15_master.R","renv.lock","scripts/p15/activate_p15_environment.R")))
}
p15_master_raw_files <- function(spec,snapshot_manifest) {
  # The legacy replay enumerates most R helpers broadly. This new helper is only
  # sourced by the downstream inference stage and is fingerprinted there; it
  # cannot affect raw construction. Do not copy/rebuild raw archives for its edits.
  downstream_only<-c("R/p15_thesis_benchmark_inference.R", "R/p15_current_inputs.R",
    spec$leaves[grepl("^R/p15_revised_peer",spec$leaves)])
  setdiff(unique(c(spec$leaves,spec$outputs,snapshot_manifest)),downstream_only)
}
p15_master_legacy_raw <- function(config) {
  source("R/p15_replay.R");source("R/p15_full_replay.R");source("R/p15_environment.R")
  p15_environment_audit()
  spec<-p15_full_specification()
  lock<-data.table::fread(config$raw_snapshot_manifest)
  lock<-lock[artifact_role!="code"]
  if(any(vapply(lock$artifact_path,digest::digest,character(1),file=TRUE,algo="sha256")!=lock$sha256))
    stop("A registered raw snapshot changed. Register a new snapshot/configuration; do not replace existing sources in place.")
  files<-p15_master_raw_files(spec,config$raw_snapshot_manifest)
  key<-p15_master_key(files)
  out<-file.path(config$cache_root,paste0("raw_",substr(key,1,20)))
  receipt<-file.path(out,"receipt.rds")
  if(file.exists(receipt))return(p15_master_verify(receipt))
  if(dir.exists(out))stop("Incomplete raw cache: ",out)
  message("MASTER: rebuilding raw evidence and preserved case decisions")
  products<-p15_run_full_replay()
  accept<-products[grepl("/acceptance.csv$",products)]
  stopifnot(length(accept)==1L,all(data.table::fread(accept)$passed))
  rawroot<-file.path(dirname(accept),"isolated_build")
  candidate<-file.path(rawroot,"data-derived/p15_analysis_candidate_20260909_region_v1")
  dir.create(out,recursive=TRUE)
  target<-file.path(out,"candidate");dir.create(target)
  stopifnot(all(file.copy(list.files(candidate,full.names=TRUE),target)))
  # Rebase provenance, not numerical data: downstream scripts must validate/read
  # this raw replay's files, never similarly named predecessor files in the repo.
  for(f in list.files(target,pattern="manifest[.]csv$",full.names=TRUE)) {
    m<-data.table::fread(f)
    if(!"artifact_path" %in% names(m))next
    m[,artifact_path:=vapply(artifact_path,function(p) {
      if(startsWith(p,"data-derived/p15_analysis_candidate_20260909_region_v1/"))
        normalizePath(file.path(target,basename(p)),mustWork=TRUE)
      else normalizePath(file.path(rawroot,p),mustWork=TRUE)
    },character(1))]
    data.table::fwrite(m,f)
  }
  writeLines(normalizePath(rawroot),file.path(out,"raw_replay_root.txt"))
  r<-p15_master_receipt(out,"raw",key,files)
  linked<-unlist(lapply(list.files(target,pattern="manifest[.]csv$",full.names=TRUE),p15_master_manifest_paths))
  r$external_outputs<-p15_master_hashes(unique(c(products,linked)))
  saveRDS(r,file.path(out,"receipt.rds"))
  r
}
p15_master_raw <- function(config) {
  # The source/status reconstruction is preserved. The accepted peer layer is
  # recomputed from its declared inputs before any current analysis sees the core.
  baseline <- p15_master_legacy_raw(config)
  if (is.null(config$revised_peer_config)) return(baseline)
  source("R/p15_revised_peer_candidate.R")
  files <- p15_revised_peer_dependency_paths(config$revised_peer_config)
  key <- p15_master_key(files, list(observed_source_parent=baseline$key))
  out <- file.path(config$cache_root,paste0("raw_revised_peer_",substr(key,1,20)))
  receipt <- file.path(out,"receipt.rds")
  if (file.exists(receipt)) return(p15_master_verify(receipt))
  if (dir.exists(out)) stop("Incomplete revised-peer build: ",out)
  dir.create(out,recursive=TRUE)
  message("MASTER: rebuilding accepted full-panel model-assisted peers")
  p15_build_revised_peer_candidate(file.path(baseline$dir,"candidate"),
    file.path(out,"candidate"),config_path=config$revised_peer_config)
  p15_master_receipt(out,"raw",key,c(files,file.path(baseline$dir,"receipt.rds")))
}
p15_master_stage <- function(config,row,raw,done) {
  id<-row$id;script<-row$script
  deps<-if(nzchar(row$dependencies))strsplit(row$dependencies,";",fixed=TRUE)[[1]] else character()
  stopifnot(all(deps %in% names(done)))
  external<-p15_master_external(config,id);code<-p15_master_code(config,id,script)
  files<-unique(c(external,code))
  parents<-c(list(raw=raw$key),lapply(done[deps],function(x)x$key))
  key<-p15_master_key(files,parents)
  out<-file.path(config$cache_root,paste0(id,"_",substr(key,1,20)))
  receipt<-file.path(out,"receipt.rds")
  if(file.exists(receipt))return(p15_master_verify(receipt))
  if(dir.exists(out))stop("Incomplete stage cache: ",out,". See execution log; use a fresh cache after correcting the cause.")
  message("MASTER: running ",id)
  settings<-c(P15_MASTER_ACTIVE="true",P15_ANALYSIS_BASE=file.path(raw$dir,"candidate"),
    P15_PV_INDEPENDENT_ONLY="true")
  variable<-c(assessment="P15_ASSESSMENT_BASE",deeper="P15_DEEPER_BASE",
    regional="P15_REGIONAL_BASE",policy="P15_POLICY_BASE",dac="P15_DAC_BASE",
    official_policy="P15_OFFICIAL_POLICY_BASE",pv_first="P15_PV_FIRST_BASE",
    bullet="P15_BULLET_BASE",pv_interpretation="P15_PV_INTERPRETATION_BASE",
    pv_followup="P15_PV_FOLLOWUP_BASE",loan_comparisons="P15_LOAN_BASE",
    benchmark_inference="P15_BENCHMARK_INFERENCE_BASE",loan_period_inference="P15_LOAN_PERIOD_BASE",
    crs_application="P15_CRS_BASE")
  for(d in intersect(names(done),names(variable)))settings[variable[d]]<-done[[d]]$dir
  if("regional" %in% names(done))settings["P15_REGION_MAP"]<-file.path(done$regional$dir,"country_region_map.csv")
  if(id=="pv_three_block")settings["P15_PV_CHECK_OUTPUT"]<-normalizePath(dirname(out)) |> file.path(basename(out))
  previous<-Sys.getenv(names(settings),unset=NA_character_)
  on.exit({Sys.unsetenv(names(settings));v<-previous[!is.na(previous)];if(length(v))do.call(Sys.setenv,as.list(v))},add=TRUE)
  do.call(Sys.setenv,as.list(settings))
  # Stage scripts require a nonexistent destination. Logs live beside it.
  log<-paste0(out,".log")
  rc<-system2(file.path(R.home("bin"),"Rscript"),c(shQuote(script),shQuote(out)),stdout=log,stderr=log)
  if(rc!=0L)stop("Analysis failed: ",id,"; log: ",log)
  # Refuse newly introduced unregistered dependencies rather than silently reuse
  # a result next time after an input unknown to the master changes.
  manifests<-file.path(out,c("input_manifest.csv","code_manifest.csv","script_manifest.csv"))
  actual<-unique(unlist(lapply(manifests[file.exists(manifests)],p15_master_manifest_paths)))
  actual<-normalizePath(actual,mustWork=TRUE)
  allowed<-normalizePath(files,mustWork=TRUE)
  parent_dirs<-c(raw$dir,vapply(done[deps],`[[`,character(1),"dir"))
  in_parent<-Reduce(`|`,lapply(parent_dirs,function(p)startsWith(actual,paste0(p,"/"))))
  unknown<-actual[!actual %in% allowed & !in_parent]
  if(length(unknown))stop("Unregistered dependencies for ",id,": ",paste(unknown,collapse=", "))
  # Numerical checks written by scripts are enforced, not merely displayed.
  for(f in list.files(out,pattern="^(checks|verification_checks)[.]csv$",full.names=TRUE)) {
    x<-data.table::fread(f);if("passed" %in% names(x)&&!all(x$passed))stop("Failed checks: ",f)
  }
  p15_master_receipt(out,id,key,files)
}
p15_master_run <- function(config=p15_master_config()) {
  dir.create(config$cache_root,recursive=TRUE,showWarnings=FALSE)
  snapshot<-data.table::fread(config$analysis_snapshot_manifest)
  if(!identical(p15_master_hashes(snapshot$path),as.data.frame(snapshot[,.(path,sha256)]))) {
    # Compare values rather than row names generated by data.frame().
    current<-p15_master_hashes(snapshot$path)
    if(!identical(current$path,snapshot$path)||!identical(current$sha256,snapshot$sha256))
      stop("A registered analysis source changed. Register a new snapshot; do not overwrite it.")
  }
  raw<-p15_master_raw(config)
  registry<-data.table::fread(config$analysis_registry,na.strings=NULL)
  done<-list()
  for(i in seq_len(nrow(registry)))done[[registry$id[i]]]<-p15_master_stage(config,registry[i],raw,done)
  # Report logic is separate from calculation cache keys: editing notes never rebuilds data.
  source("R/p15_master_findings.R")
  p15_master_report(config,raw,done)
}
