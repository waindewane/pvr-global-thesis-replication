library(data.table)
library(jsonlite)
library(digest)
j <- fromJSON("data-derived/p15_master/current_run.json")
out <- "docs/thesis_design/sections/benchmark_evaluation/source_comparability_prose_20260920"
path <- file.path(j$stages$benchmark_inference$dir, "paired_loss_details.csv")
d <- fread(path)[family == "modern_pairwise_tiers" & sample_view == "full_validation" & focal != "ids"]
stopifnot(all(d$analysis_year %in% 2018:2024), !anyDuplicated(d[,.(focal,comparator,iso3,analysis_year)]),
 all(abs(d$focal_error-(d$focal_rate-d$primary))<1e-10),
 all(abs(d$comparator_error-(d$comparator_rate-d$primary))<1e-10))
z <- d[, .(country_years=.N,countries=uniqueN(iso3),
 first_mae=mean(abs(focal_error)),second_mae=mean(abs(comparator_error)),
 first_median_absolute_error=median(abs(focal_error)),second_median_absolute_error=median(abs(comparator_error))),by=.(focal,comparator)]
z <- z[match(c("secondary_moodys","moodys_peer","secondary_peer"),paste(focal,comparator,sep="_"))]
# Verify medians independently against sorted middle observations, including even n.
midpoint <- function(x) { x <- sort(abs(x)); mean(x[c(floor((length(x)+1)/2),ceiling((length(x)+1)/2))]) }
for (i in seq_len(nrow(z))) {
 f <- z$focal[i]; co <- z$comparator[i]; rows <- d[focal==f & comparator==co]
 stopifnot(abs(midpoint(rows$focal_error)-z$first_median_absolute_error[i])<1e-12,
 abs(midpoint(rows$comparator_error)-z$second_median_absolute_error[i])<1e-12)
}
prior <- fread("docs/thesis_design/sections/benchmark_evaluation/source_comparability_20260920/paired_accuracy_comparisons.csv")
a <- merge(z,prior,by=c("focal","comparator"))
stopifnot(nrow(a)==3,all(a$country_years==a$n),all(a$countries.x==a$countries.y),
 all(abs(a$first_mae-a$focal_mae_pp)<1e-10),all(abs(a$second_mae-a$comparator_mae_pp)<1e-10))
fwrite(z,file.path(out,"paired_accuracy_with_medians.csv"))
fwrite(data.table(check=c("matched keys and error definitions verified", "six medians verified from ordered errors", "MAEs and sample counts agree with existing inference output"),passed=TRUE),file.path(out,"verification.csv"))
manifest <- data.table(path=c("data-derived/p15_master/current_run.json",path,
 "docs/thesis_design/sections/benchmark_evaluation/source_comparability_20260920/paired_accuracy_comparisons.csv",
 file.path(out,"prepare_median_comparison.R")))
manifest[,sha256:=vapply(path,digest,character(1),file=TRUE,algo="sha256")]
fwrite(manifest,file.path(out,"input_manifest.csv"))
print(z)
