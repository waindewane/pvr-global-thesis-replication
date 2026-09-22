# Prepare section 4.3 displays and verify rank calculations from current inputs.
library(data.table)
j <- jsonlite::fromJSON("data-derived/p15_master/current_run.json")
out <- "docs/thesis_design/sections/benchmark_evaluation/accuracy_20260920"
m <- fread(file.path(j$stages$statistical_review$dir,"benchmark_error_metrics.csv"))
d <- fread(file.path(j$stages$statistical_review$dir,"benchmark_error_details.csv"))
b <- fread(file.path(j$stages$benchmark_inference$dir,"paired_loss_inference.csv"))
v <- fread(file.path(j$stages$benchmark_inference$dir,"paired_loss_country_year_sensitivity.csv"))
source_order <- c("ids","secondary","moodys","peer")
modern <- m[scenario=="modern2018_2024"]
modern[,reduction_percent:=100*improvement_pp/comparator_mae_pp]
setorder(modern,tier);modern <- modern[match(source_order,modern$tier)]
ci <- b[family=="modern_tier_vs_dac"&sample_view=="full_validation"]
check <- merge(modern,ci,by.x="tier",by.y="focal")
stopifnot(nrow(check)==4,all(abs(check$improvement_pp-check$estimate_pp)<1e-10),
 all(check$records==check$n),all(check$countries.x==check$countries.y),
 all(abs(check$reduction_percent-100*check$relative_mae_reduction)<1e-10))
fwrite(modern[,.(tier,records,countries,focal_mae_pp,comparator_mae_pp,reduction_percent)],file.path(out,"modern_accuracy_table.csv"))
fwrite(modern[,.(tier,focal_bias_pp,comparator_bias_pp,focal_rmse_pp,comparator_rmse_pp,
 improvement_pp,equal_country_improvement_pp,equal_year_improvement_pp)],file.path(out,"modern_complementary_metrics.csv"))
fwrite(ci[,.(focal,n,countries,estimate_pp,ci_low_pp,ci_high_pp,p_holm_within_family)],file.path(out,"modern_country_bootstrap.csv"))
fwrite(v[family=="modern_tier_vs_dac"&sample_view=="full_validation"&inference=="country_year",
 .(focal,mean_gap_pp,ci_low_pp,ci_high_pp,p_holm_within_family)],file.path(out,"modern_country_year_sensitivity.csv"))
fwrite(m[scenario!="modern2018_2024",.(scenario,tier,records,countries,focal_mae_pp,comparator_mae_pp,improvement_pp)],file.path(out,"historical_comparisons.csv"))
early <- d[scenario=="standardized2012_2017"]
historic <- d[scenario=="historical10_2012_2017"]
setorder(early,tier,iso3,analysis_year);setorder(historic,tier,iso3,analysis_year)
stopifnot(identical(early[,.(tier,iso3,analysis_year,primary,focal_rate)],historic[,.(tier,iso3,analysis_year,primary,focal_rate)]))
model <- fread(file.path(j$stages$deeper$dir,"model_common_details.csv"))
rank <- rbindlist(lapply(c("moodys","peer"),function(method_name) {
 model[,.(method=method_name,n=.N,spearman=cor(primary,get(method_name),method="spearman")),by=analysis_year]
}))
saved <- fread(file.path(j$stages$deeper$dir,"within_year_rank.csv"))
check_rank <- merge(rank,saved,by=c("analysis_year","method"))
stopifnot(nrow(check_rank)==26,all(check_rank$n.x==check_rank$n.y),all(abs(check_rank$spearman.x-check_rank$spearman.y)<1e-12))
rank_summary <- rank[n>=8,.(first_year=min(analysis_year),last_year=max(analysis_year),years=.N,
 country_years=sum(n),mean_spearman=mean(spearman)),by=method]
saved_summary <- fread(file.path(j$stages$deeper$dir,"within_year_rank_summary.csv"))
check_summary <- merge(rank_summary,saved_summary,by="method")
stopifnot(all(abs(check_summary$mean_spearman.x-check_summary$mean_spearman.y)<1e-12),all(check_summary$country_years==check_summary$n))
fwrite(rank_summary,file.path(out,"rank_summary.csv"))
fwrite(data.table(check=c("modern point estimates and supports agree across outputs", "early standardized and historical rates use identical cases", "all annual rank correlations reproduced", "rank summary reproduced"),passed=TRUE),file.path(out,"verification.csv"))
print(modern[,.(tier,records,countries,focal_mae_pp,comparator_mae_pp,reduction_percent)])
print(rank_summary)
