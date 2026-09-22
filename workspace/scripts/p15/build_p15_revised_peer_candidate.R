#!/usr/bin/env Rscript
source("scripts/p15/activate_p15_environment.R")
source("R/p15_revised_peer_candidate.R")
args <- commandArgs(TRUE)
if(length(args)<2L||length(args)>3L)stop("Usage: build_p15_revised_peer_candidate.R <base_candidate> <fresh_out> [config]")
config <- if(length(args)==3L)args[3] else "config/p15_revised_peer_20260912_v1.json"
result <- p15_build_revised_peer_candidate(args[1],args[2],config)
print(result$checks)
cat("Completed revised private peer candidate:",result$candidate,"\n")
