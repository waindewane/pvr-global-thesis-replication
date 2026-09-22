library(targets)
tar_option_set(packages=character())
root <- Sys.getenv("P15_REGION_RUN_ROOT","data-derived/p15_region_correction_research_20260909_v2")
candidate <- Sys.getenv("P15_REGION_CANDIDATE","data-derived/p15_analysis_candidate_20260909_region_v1")
run <- function(script,folder){
  settings <- c(P15_ANALYSIS_BASE=candidate,P15_ASSESSMENT_BASE=file.path(root,"assessment"),
    P15_REGIONAL_BASE=file.path(root,"regional"),P15_REGION_MAP=file.path(root,"regional","country_region_map.csv"),
    P15_POLICY_BASE=file.path(root,"policy"),
    P15_PV_FIRST_BASE=file.path(root,"pv_first"),P15_BULLET_BASE=file.path(root,"bullet"),
    P15_PV_INTERPRETATION_BASE=file.path(root,"pv_interpretation"))
  do.call(Sys.setenv,as.list(settings))
  out <- file.path(root,folder)
  if(dir.exists(out))stop("Fresh stage path required: ",out)
  dir.create(root,recursive=TRUE,showWarnings=FALSE)
  code <- system2(file.path(R.home("bin"),"Rscript"),c(script,shQuote(out)))
  if(code!=0L)stop("Stage failed: ",script)
  list.files(out,full.names=TRUE)
}
list(
 tar_target(correction_inputs,{
   m<-data.table::fread("data-derived/p15_analysis_candidate_20260907_v1/output_manifest.csv")
   context<-data.table::fread("data-derived/p15_reference_review_20260907_v1/input_manifest.csv")
   context_path<-context$artifact_path[grepl("rating_component_ledger_2012_2024.csv.gz$",context$artifact_path)]
   stopifnot(length(context_path)==1L)
   c(m$artifact_path,context_path,"data-raw/world_bank_countries.json",list.files("R",pattern="^p15_.*[.]R$",full.names=TRUE),
    list.files("scripts/p15",pattern="^build_p15_.*[.]R$",full.names=TRUE),"renv.lock")
 },format="file"),
 tar_target(corrected_candidate,{
   stopifnot(all(file.exists(correction_inputs)))
   if(!dir.exists(candidate)) {
     code<-system2(file.path(R.home("bin"),"Rscript"),c("scripts/p15/build_p15_peer_region_correction.R",shQuote(candidate)))
     if(code!=0L)stop("Candidate correction failed")
   }
   m<-data.table::fread(file.path(candidate,"output_manifest.csv"))
   stopifnot(all(vapply(m$artifact_path,function(p)digest::digest(file=p,algo="sha256"),character(1))==m$sha256))
   list.files(candidate,full.names=TRUE)
 },format="file"),
 tar_target(assessment,{corrected_candidate;run("scripts/p15/build_p15_dataset_assessment.R","assessment")},format="file"),
 tar_target(deeper,{assessment;run("scripts/p15/build_p15_deeper_assessment.R","deeper")},format="file"),
 tar_target(regional,{corrected_candidate;run("scripts/p15/build_p15_regional_assessment.R","regional")},format="file"),
 tar_target(peer_geography_validation,{regional;run("scripts/p15/build_p15_peer_geography_validation.R","peer_geography_validation")},format="file"),
 tar_target(policy,{regional;run("scripts/p15/build_p15_policy_comparison.R","policy")},format="file"),
 tar_target(official_policy,{policy;run("scripts/p15/build_p15_official_policy_comparison.R","official_policy")},format="file"),
 tar_target(pv_first,{corrected_candidate;run("scripts/p15/build_p15_pv_first_pass.R","pv_first")},format="file"),
 tar_target(bullet,{pv_first;run("scripts/p15/build_p15_bullet_extension.R","bullet")},format="file"),
 tar_target(pv_interpretation,{bullet;regional;run("scripts/p15/build_p15_pv_interpretation.R","pv_interpretation")},format="file"),
 tar_target(pv_followup,{pv_interpretation;run("scripts/p15/build_p15_pv_annotation_followup.R","pv_followup")},format="file")
)
