#!/usr/bin/env Rscript
# One-time curation snapshot: preserve old case permissions, not calculated rates.
library(data.table)
library(digest)
parent <- "experiments/full_ladder_ratings_integrated_status_rebuild_2026-06-01/outputs/p12a_feature_rich_secondary_final_decision_2024.csv"
out <- "data-raw/p15_curated_inputs_20260906/legacy_feature_row_permissions.csv"
if(file.exists(out))stop("Curated snapshot already exists; create a new version to change it")
x <- fread(parent)
drop <- c("central_rate_pct","lower_yield_bound_pct","upper_yield_bound_pct",
  "v12_bid_yield","v12_ask_yield","v12_pricing_mid_yield","v12_mean_yield",
  "v12_yield_to_maturity_1","v12_yield_to_maturity_2","v14_yield_search_direct","v14_yield_price_implied",
  "v12_bid_ask_yield_range_bps","v12_ytm_vs_v14_yield_search_abs_diff_pct","v12_pricing_mid_vs_v14_yield_search_abs_diff_pct")
x[,(intersect(drop,names(x))):=NULL]
fwrite(x,out,na="")
fwrite(data.table(source_snapshot_id="SRC-P15-LEGACY-FEATURE-PERMISSIONS-20260906",artifact_path=c(parent,out),
  sha256=vapply(c(parent,out),digest,character(1),algo="sha256",file=TRUE)),
  "data-raw/p15_curated_inputs_20260906/legacy_feature_permissions_manifest.csv")
