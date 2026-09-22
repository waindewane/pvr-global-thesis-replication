library(targets)
tar_option_set(packages=character())
list(
  tar_target(peer_description_inputs,{
    base<-Sys.getenv("P15_ANALYSIS_BASE",Sys.getenv("P15_PEER_DESCRIPTION_BASE",""))
    if(!nzchar(base))base<-jsonlite::fromJSON("data-derived/p15_master/current_run.json")$candidate
    c("data-derived/p15_master/current_run.json",file.path(base,c("core_evidence.csv",
      "selected_reference.csv","peer_membership.csv","peer_region_context.csv","tier_eligibility.csv",
      "output_manifest.csv","peer_score_intervals.csv")),"scripts/p15/summarise_peer_groups_20260912.R",
      "scripts/p15/activate_p15_environment.R","R/p15_current_inputs.R","R/research_governance.R","renv.lock")
  },format="file"),
  tar_target(peer_description_outputs,{
    stopifnot(all(file.exists(peer_description_inputs)))
    out<-Sys.getenv("P15_PEER_DESCRIPTION_OUTPUT","")
    if(!nzchar(out))stop("Set P15_PEER_DESCRIPTION_OUTPUT to a fresh directory")
    if(nzchar(Sys.getenv("P15_PEER_DESCRIPTION_BASE"))) Sys.setenv(P15_ANALYSIS_BASE=Sys.getenv("P15_PEER_DESCRIPTION_BASE"))
    status<-system2(file.path(R.home("bin"),"Rscript"),
      c("scripts/p15/summarise_peer_groups_20260912.R",shQuote(out)))
    if(status!=0)stop("Peer description failed")
    list.files(out,full.names=TRUE)
  },format="file",cue=tar_cue(mode="always"))
)
