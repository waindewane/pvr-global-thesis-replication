source("R/p15_current_inputs.R")
#!/usr/bin/env Rscript
suppressPackageStartupMessages({library(dplyr);library(tidyr);library(purrr);library(data.table)})
source("R/pvr.R");source("R/p15_pv_first_pass.R");source("R/p15_bullet_extension.R")
source("R/p15_pv_interpretation.R");source("R/p15_pv_annotation_followup.R")
args <- commandArgs(TRUE)
out <- if(length(args))args[1] else file.path("data-derived",paste0("p15_pv_followup_",format(Sys.time(),"%Y%m%d_%H%M%S")))
if(dir.exists(out)&&length(list.files(out)))stop("Fresh output directory required")
dir.create(out,recursive=TRUE,showWarnings=FALSE)
save_csv <- function(x,n)fwrite(x,file.path(out,paste0(n,".csv")),na="")
p <- Sys.getenv("P15_PV_INTERPRETATION_BASE", p15_current_input("pv_interpretation"))
paths <- file.path(p,c("analysis_rows.csv","available_tier_same_terms_valuations.csv",
  "matched_creditor_pairs.csv","output_manifest.csv"))
x <- as_tibble(fread(paths[1])) |> mutate(gap=policy_pv_minus_market_pv)
alt <- as_tibble(fread(paths[2]))
pairs <- as_tibble(fread(paths[3]))
manifest <- fread(paths[4])
manifest$path <- normalizePath(manifest$path,mustWork=TRUE)
at <- match(normalizePath(paths[1:3],mustWork=TRUE),manifest$path)
stopifnot(!anyNA(at))
stopifnot(all(vapply(paths[1:3],function(f)digest::digest(file=f,algo="sha256"),character(1))==
  manifest$sha256[at]))
keys <- c("iso3","analysis_year","creditor")
stopifnot(nrow(x)>0L && all(x$analysis_year %in% 2018:2024),!anyDuplicated(x[keys]),!anyDuplicated(alt[c(keys,"tier")]))
moodys <- alt |> filter(tier=="moodys") |> select(all_of(keys),moodys_rate_pct=rate_pct,moodys_ge=market_ge)
mx <- inner_join(x,moodys,by=keys) |> mutate(moodys_gap=moodys_ge-policy_grant_element_analogue_pct)
paired <- mx |> filter(selected_tier %in% c("primary","ids"))
rate_views <- bind_rows(
  x |> filter(selected_tier %in% c("primary","ids")) |> mutate(view="selected_primary_ids"),
  x |> filter(selected_tier=="moodys") |> mutate(view="selected_moodys_fallback"),
  mx |> mutate(view="moodys_every_available_case",gap=moodys_gap),
  paired |> mutate(view="paired_primary_ids",gap=policy_pv_minus_market_pv),
  paired |> mutate(view="paired_moodys",gap=moodys_gap))
save_csv(rate_views |> select(view,all_of(keys),country,region,period,selected_tier,gap),"rating_comparison_cases")
save_csv(rate_views |> group_by(view,period) |> p15_gap_summary(),"rating_comparison_period")
save_csv(rate_views |> group_by(view,region,period) |> p15_gap_summary(),"rating_comparison_region_period")
save_csv(paired |> group_by(period) |> summarise(n=n(),countries=n_distinct(iso3),
  mean_moodys_minus_observed_ge=mean(moodys_ge-market_grant_element_analogue_pct),
  mean_abs_moodys_minus_observed_ge=mean(abs(moodys_ge-market_grant_element_analogue_pct)),.groups="drop"),
  "rating_same_case_difference")
# Each country-creditor first receives equal weight within a period, then pairs across periods.
common <- rate_views |> group_by(view,iso3,creditor,region,period) |>
  summarise(period_gap=mean(gap),years=n(),.groups="drop") |>
  pivot_wider(names_from=period,values_from=c(period_gap,years)) |>
  filter(is.finite(period_gap_2018_2021),is.finite(period_gap_2022_2024)) |>
  mutate(change=period_gap_2022_2024-period_gap_2018_2021)
save_csv(common,"rating_common_borrower_creditor_cases")
save_csv(common |> group_by(view) |> summarise(n=n(),countries=n_distinct(iso3),
  early_gap=mean(period_gap_2018_2021),late_gap=mean(period_gap_2022_2024),change=mean(change),.groups="drop"),
  "rating_common_borrower_creditor_summary")

rev <- pairs |> filter(interest_ranking_reversed) |>
  separate_wider_delim(pair,"__",names=c("creditor_a","creditor_b"),cols_remove=FALSE) |>
  left_join(x |> select(all_of(keys),country,region,official_rate,official_grace_years,
    market_grant_element_analogue_pct),by=c("iso3","analysis_year","creditor_a"="creditor")) |>
  rename(rate_a=official_rate,grace_a=official_grace_years,ge_a=market_grant_element_analogue_pct) |>
  left_join(x |> select(all_of(keys),official_rate,official_grace_years,market_grant_element_analogue_pct),
    by=c("iso3","analysis_year","creditor_b"="creditor")) |>
  rename(rate_b=official_rate,grace_b=official_grace_years,ge_b=market_grant_element_analogue_pct) |>
  mutate(abs_ge_gap=abs(ge_difference_a_minus_b),abs_interest_gap=abs(interest_difference_a_minus_b)) |>
  arrange(desc(abs_ge_gap))
stopifnot(nrow(rev)==sum(pairs$interest_ranking_reversed),max(abs(rev$ge_a-rev$ge_b-rev$ge_difference_a_minus_b))<1e-8)
save_csv(rev,"reversal_magnitude_cases")
save_csv(rev |> summarise(n=n(),median_abs_ge_gap=median(abs_ge_gap),mean_abs_ge_gap=mean(abs_ge_gap),
  max_abs_ge_gap=max(abs_ge_gap),over_1=sum(abs_ge_gap>1),over_5=sum(abs_ge_gap>5),over_10=sum(abs_ge_gap>10),
  interest_gap_below_point_one=sum(abs_interest_gap<.1)),"reversal_magnitude_summary")
other <- inner_join(alt,alt,by=c("iso3","analysis_year","tier"),suffix=c("_a","_b"),relationship="many-to-many") |>
  filter(creditor_a<creditor_b) |> mutate(pair=paste(creditor_a,creditor_b,sep="__")) |>
  inner_join(rev |> select(iso3,analysis_year,pair,interest_difference_a_minus_b),by=c("iso3","analysis_year","pair")) |>
  mutate(ge_gap=market_ge_a-market_ge_b,reversal_persists=ge_gap*interest_difference_a_minus_b>1e-8)
save_csv(other |> select(iso3,analysis_year,pair,tier,ge_gap,reversal_persists),"reversal_alternative_benchmarks")
save_csv(other |> group_by(tier) |> summarise(n=n(),reversal_persists=sum(reversal_persists),
  median_abs_ge_gap=median(abs(ge_gap)),.groups="drop"),"reversal_alternative_summary")

ib <- x |> filter(creditor=="IBRD")
ibviews <- p15_interpretation_views(ib)
for(g in list(c("evidence_view","analysis_year"),c("evidence_view","region"),
  c("evidence_view","region","period"),c("evidence_view","period"),c("selected_tier","period"))){
  z <- if(!"evidence_view" %in% g)filter(ibviews,evidence_view=="all_selected") else ibviews
  save_csv(z |>
    group_by(across(all_of(g))) |> p15_gap_summary(),paste0("ibrd_",paste(g,collapse="_")))
}
ibcommon <- ibviews |> group_by(evidence_view,iso3,region,period) |>
  summarise(period_gap=mean(gap),years=n(),.groups="drop") |>
  pivot_wider(names_from=period,values_from=c(period_gap,years)) |>
  filter(is.finite(period_gap_2018_2021),is.finite(period_gap_2022_2024)) |>
  mutate(change=period_gap_2022_2024-period_gap_2018_2021)
save_csv(ibcommon,"ibrd_common_country_period_cases")
save_csv(ibcommon |> group_by(evidence_view,region) |> summarise(countries=n(),early_gap=mean(period_gap_2018_2021),
  late_gap=mean(period_gap_2022_2024),change=mean(change),.groups="drop"),"ibrd_common_country_region")
save_csv(ib |> group_by(iso3,country,region) |> summarise(years=n(),mean_gap=mean(gap),
  positive_share=mean(gap>0),min_gap=min(gap),max_gap=max(gap),.groups="drop") |> filter(years>=3),
  "ibrd_persistent_country_patterns")
# Regional difference from the overall mean of the same year is a descriptive timing check.
ibcenter <- ib |> group_by(analysis_year) |> mutate(year_centered_gap=gap-mean(gap)) |> ungroup()
save_csv(ibcenter |> group_by(region) |> summarise(n=n(),countries=n_distinct(iso3),
  raw_mean=mean(gap),mean_year_centered_gap=mean(year_centered_gap),.groups="drop"),"ibrd_year_centered_regions")

steps <- map_dfr(seq_len(nrow(ib)),function(i){a<-ib[i,]
  f<-p15_analysis_schedule(a$official_rate,a$official_maturity_years,a$official_grace_years)
  map_dfr(c(-1,0,1),function(delta){s<-p15_interest_step(f,a$official_rate,delta)
    market<-100-p15_stream_pv(s,a$selected_rate_pct); policy<-100-p15_stream_pv(s,a$dac_rate_pct)
    tibble(iso3=a$iso3,analysis_year=a$analysis_year,period=a$period,selected_tier=a$selected_tier,
      change_after_year_one_pp=delta,starting_rate_pct=a$official_rate,
      later_rate_pct=max(0,a$official_rate+delta),zero_floor_used=a$official_rate+delta<0,
      market_ge=market,policy_ge=policy,gap=market-policy,
      market_ge_change=market-a$market_grant_element_analogue_pct,
      gap_change=market-policy-a$gap)
  })})
stopifnot(max(abs(steps$gap_change[steps$change_after_year_one_pp==0]))<1e-8,
  all(steps$market_ge_change[steps$change_after_year_one_pp==1]<0),
  all(steps$market_ge_change[steps$change_after_year_one_pp== -1]>0))
save_csv(steps,"ibrd_interest_step_cases")
save_csv(steps |> group_by(change_after_year_one_pp,period) |> summarise(n=n(),floor_cases=sum(zero_floor_used),
  mean_market_ge_change=mean(market_ge_change),mean_gap_change=mean(gap_change),
  mean_abs_gap_change=mean(abs(gap_change)),mean_gap=mean(gap),.groups="drop"),"ibrd_interest_step_summary")
files_manifest <- function(ps)tibble(path=ps,sha256=vapply(ps,function(f)digest::digest(file=f,algo="sha256"),character(1)))
save_csv(files_manifest(paths),"input_manifest")
save_csv(files_manifest(c("R/pvr.R","R/p15_pv_first_pass.R","R/p15_bullet_extension.R","R/p15_pv_interpretation.R",
  "R/p15_pv_annotation_followup.R","scripts/p15/build_p15_pv_annotation_followup.R",
  "tests/testthat/test-p15-pv-annotation-followup.R")),"code_manifest")
writeLines(capture.output(sessionInfo()),file.path(out,"environment.txt"))
writeLines(c(paste0("build_id: ",out),paste0("parent_input: ",p),
  "release_state: diagnostic_thesis_analysis_not_canonical","schema_id: P15-PV-ANNOTATION-FOLLOWUP-1",
  "estimator_id: inherited_PV_and_descriptive_matched_samples_and_hypothetical_interest_step",
  "admissibility_id: unchanged_modern_762_rows","selection_id: unchanged_reference",
  "Interest steps are +/-1 pp after year one, floored at zero, not forecasts or calibrated uncertainty bounds."),
  file.path(out,"build_contract.txt"))
save_csv(files_manifest(list.files(out,full.names=TRUE)),"output_manifest")
cat("Saved",out,"\n")
