#!/usr/bin/env Rscript
# Independent replay from the upstream source; no analysis helper is sourced.
args <- commandArgs(trailingOnly = TRUE)
out <- if (length(args)) args[1] else "data-derived/p15_thesis_benchmark_inference_20260910_v2"
read <- function(name) read.csv(file.path(out, paste0(name, ".csv")), check.names = FALSE)
manifest <- read("input_manifest")
input <- manifest$artifact_path[basename(manifest$artifact_path) == "comparison_inputs.csv"]
stopifnot(length(input) == 1L)
d <- read.csv(input, check.names = FALSE)
summary <- read("paired_loss_inference")
modern <- d[d$analysis_year >= 2018 & is.finite(d$dac_modern_2018), ]
selected <- subset(summary, sample_view == "full_validation" &
                             family %in% c("modern_tier_vs_dac", "modern_pairwise_tiers"))
for (i in seq_len(nrow(selected))) {
  row <- selected[i, ]
  keep <- is.finite(modern[[row$focal]]) & is.finite(modern[[row$comparator]])
  z <- modern[keep, ]
  focal_loss <- abs(z[[row$focal]] - z$primary)
  comparator_loss <- abs(z[[row$comparator]] - z$primary)
  stopifnot(nrow(z) == row$n, length(unique(z$iso3)) == row$countries,
            abs(mean(focal_loss) - row$focal_mae_pp) < 1e-11,
            abs(mean(comparator_loss) - row$comparator_mae_pp) < 1e-11,
            abs(mean(comparator_loss-focal_loss) - row$estimate_pp) < 1e-11)
}
ids <- d[is.finite(d$ids), ]
ids_summary <- read("primary_ids_signed_summary")
stopifnot(nrow(ids) == 189L,
  abs(mean(ids$primary-ids$ids)-ids_summary$primary_minus_ids_mean_pp)<1e-11,
  abs(mean(abs(ids$primary-ids$ids))-ids_summary$mae_pp)<1e-11)

# Independent row-expansion bootstrap verifies the weighted cluster implementation.
row <- subset(selected, focal == "secondary" & comparator == "moodys")
z <- modern[is.finite(modern$secondary) & is.finite(modern$moodys), ]
z <- z[order(z$iso3, z$analysis_year), ]
loss <- abs(z$moodys-z$primary)-abs(z$secondary-z$primary)
groups <- sort(unique(z$iso3))
set.seed(row$seed)
draws <- replicate(row$repetitions, {
  sampled <- sample(groups, length(groups), replace = TRUE)
  index <- unlist(lapply(sampled, function(g) which(z$iso3 == g)), use.names = FALSE)
  mean(loss[index])
})
ci <- unname(quantile(draws, c(.025,.975)))
centered_p <- (1+sum(abs(draws-mean(loss))>=abs(mean(loss))-1e-12))/(length(draws)+1)
stopifnot(abs(ci[1]-row$ci_low_pp)<1e-11, abs(ci[2]-row$ci_high_pp)<1e-11,
          abs(centered_p-row$p_boot_two_sided)<1e-11)

change <- read("consecutive_details")
direct <- subset(change, sample_view == "full_validation" & focal == "secondary")
for (i in seq_len(nrow(direct))) {
  row <- direct[i, ]
  endpoints <- modern[modern$iso3==row$iso3 &
                      modern$analysis_year %in% c(row$analysis_year-1,row$analysis_year), ]
  endpoints <- endpoints[order(endpoints$analysis_year), ]
  stopifnot(nrow(endpoints)==2L, all(is.finite(endpoints$secondary)))
  advantage <- abs(endpoints$dac_modern_2018-endpoints$primary)-
               abs(endpoints$secondary-endpoints$primary)
  change_gain <- abs(diff(endpoints$dac_modern_2018)-diff(endpoints$primary))-
                 abs(diff(endpoints$secondary)-diff(endpoints$primary))
  stopifnot(all(advantage>1e-10)==row$both_endpoint_levels_closer,
            abs(change_gain-row$change_improvement_pp)<1e-11)
}
restricted <- read("recorded_secondary_use_restrictions")
stopifnot(!any(restricted$present_in_primary_validation))
a <- subset(summary, sample_view == "full_validation")
b <- subset(summary, sample_view == "recorded_secondary_use_sensitivity")
a$sample_view <- b$sample_view <- NULL
rownames(a) <- rownames(b) <- NULL
stopifnot(isTRUE(all.equal(a,b)))
for (name in c("input_manifest", "code_manifest", "output_manifest")) {
  m <- read(name)
  hashes <- vapply(m$artifact_path, digest::digest, character(1), file=TRUE, algo="sha256")
  stopifnot(all(hashes == m$sha256))
}
cat("PASS: 10 original-input modern loss comparisons, signed IDS mean, independent\n")
cat("row-expanded bootstrap, consecutive endpoint/change arithmetic, identical\n")
cat("restriction sensitivity, and input/code/output hashes.\n")
