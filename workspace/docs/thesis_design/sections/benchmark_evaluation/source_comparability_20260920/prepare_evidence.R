library(data.table)
library(jsonlite)
library(digest)
j <- fromJSON("data-derived/p15_master/current_run.json")
out <- "docs/thesis_design/sections/benchmark_evaluation/source_comparability_20260920"
paths <- c(file.path(j$stages$assessment$dir,c("pair_details.csv","pair_summary.csv")),
 file.path(j$stages$benchmark_inference$dir,c("paired_loss_details.csv","paired_loss_inference.csv")),
 file.path(j$stages$deeper$dir,c("timing_maturity_sensitivity.csv","tail_influence.csv")))
pairs <- fread(paths[1]); summary <- fread(paths[2])
loss <- fread(paths[3])[family=="modern_pairwise_tiers" & sample_view=="full_validation"]
inf <- fread(paths[4])[family=="modern_pairwise_tiers" & sample_view=="full_validation"]
source("R/p15_thesis_benchmark_inference.R")
actual <- loss[, c(list(n=.N,countries=uniqueN(iso3),focal_mae_pp=mean(abs(focal_rate-primary)),
 comparator_mae_pp=mean(abs(comparator_rate-primary))),
 p15_thesis_cluster_boot(improvement_pp,iso3,seed=first(bootstrap_seed))),by=.(focal,comparator)]
a <- merge(actual,inf,by=c("focal","comparator"))
for(v in c("n","countries","focal_mae_pp","comparator_mae_pp","estimate_pp","ci_low_pp","ci_high_pp","p_boot_two_sided"))
 stopifnot(max(abs(a[[paste0(v,".x")]]-a[[paste0(v,".y")]]))<1e-10)
fwrite(inf,file.path(out,"paired_accuracy_comparisons.csv"))
p <- pairs[pair %in% c("primary__ids","primary__secondary")]
stopifnot(all(abs(p$gap_pp-(p$comparison_rate-p$anchor_rate))<1e-10))
z <- p[,.(n=.N,countries=uniqueN(iso3),mean_gap_pp=mean(gap_pp),mae_pp=mean(abs(gap_pp)),
 rmse_pp=sqrt(mean(gap_pp^2)),within_1=mean(abs(gap_pp)<=1)),by=pair]
a <- merge(z,summary,by="pair")
for(v in c("n","countries","mean_gap_pp","mae_pp","rmse_pp","within_1"))
 stopifnot(max(abs(a[[paste0(v,".x")]]-a[[paste0(v,".y")]]))<1e-10)
fwrite(z,file.path(out,"observed_source_gaps.csv"))
s <- copy(p[pair=="primary__secondary"])
s[,close_dates:=as.numeric(as.IDate(secondary_usd_first_rate_date)-as.IDate(primary_usd_last_rate_date)) %between% c(0,90)]
s[,close_maturity:=abs(secondary_usd_market_maturity_years-primary_usd_market_maturity_years)<=3]
timing <- s[,.(sample="all",n=.N,bias=mean(gap_pp),mae=mean(abs(gap_pp))),by=.(close_dates,close_maturity)]
a <- merge(timing,fread(paths[5])[sample=="all"],by=c("sample","close_dates","close_maturity"))
stopifnot(nrow(a)==4,all(a$n.x==a$n.y),all(abs(a$mae.x-a$mae.y)<1e-10),all(abs(a$bias.x-a$bias.y)<1e-10))
fwrite(timing,file.path(out,"timing_maturity_groups.csv"))
s <- s[order(-abs(gap_pp))]
tail <- fread(paths[6])[pair=="primary__secondary" & removed_n==2]
stopifnot(abs(sum(s$gap_pp[1:2]^2)/sum(s$gap_pp^2)-tail$removed_squared_share)<1e-10)
fwrite(tail,file.path(out,"largest_gap_influence.csv"))
fwrite(s[1:2,.(iso3,analysis_year,anchor_rate,comparison_rate,gap_pp,primary_usd_last_rate_date,secondary_usd_first_rate_date)],file.path(out,"largest_gap_cases.csv"))
manifest <- data.table(path=c("data-derived/p15_master/current_run.json",paths,"R/p15_thesis_benchmark_inference.R","scripts/p15/build_p15_deeper_assessment.R","scripts/p15/build_p15_dataset_assessment.R","scripts/p15/build_p15_thesis_benchmark_inference.R"))
manifest[,sha256:=vapply(path,digest,character(1),file=TRUE,algo="sha256")]
fwrite(manifest,file.path(out,"evidence_manifest.csv"))
fwrite(data.table(check=c("six paired MAEs and country bootstrap results reproduced", "observed-source summaries reproduced", "four timing/maturity groups reproduced", "two-largest-gap squared contribution reproduced"),passed=TRUE),file.path(out,"verification.csv"))
print(z)
print(timing)

vpath <- file.path(j$candidate,"selection_variants.csv")
v <- fread(vpath)[historical_lmic_reporting_scope==TRUE]
v <- dcast(v,iso3+analysis_year~view_id,value.var="selected_rate_pct")
v[,gap:=secondary_before_ids__with_peer-ids_before_secondary__with_peer]
changed <- v[is.finite(gap)&abs(gap)>1e-10]
order_path <- file.path(j$stages$assessment$dir,"source_order_changes.csv")
recorded <- fread(order_path)
a <- merge(changed,recorded,by=c("iso3","analysis_year"))
stopifnot(nrow(a)==29,all(abs(a$gap-a$source_order_gap_pp)<1e-10),
 identical(is.finite(v$ids_before_secondary__with_peer),is.finite(v$secondary_before_ids__with_peer)))
fwrite(changed[,.(changed_country_years=.N,countries=uniqueN(iso3),mean_absolute_change_pp=mean(abs(gap)),mean_signed_change_pp=mean(gap),total_available=sum(is.finite(v$gap)))],file.path(out,"selection_order_summary.csv"))
extra <- data.table(path=c(vpath,order_path))
extra[,sha256:=vapply(path,digest,character(1),file=TRUE,algo="sha256")]
fwrite(rbind(manifest,extra),file.path(out,"evidence_manifest.csv"))
checks <- fread(file.path(out,"verification.csv"))
fwrite(rbind(checks,data.table(check="IDS/secondary order changes and unchanged availability reproduced",passed=TRUE)),file.path(out,"verification.csv"))
