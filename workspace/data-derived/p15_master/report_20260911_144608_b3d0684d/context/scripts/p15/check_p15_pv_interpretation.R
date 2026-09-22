#!/usr/bin/env Rscript
suppressPackageStartupMessages({library(data.table);library(dplyr);library(tidyr);library(purrr)})
source("R/pvr.R");source("R/p15_pv_first_pass.R");source("R/p15_bullet_extension.R")
source("R/p15_pv_interpretation.R")
out <- Sys.getenv("P15_PV_CHECK_OUTPUT","data-derived/p15_pv_interpretation_followup_20260909_v2")
if(dir.exists(out)&&length(list.files(out)))stop("Fresh follow-up directory required")
dir.create(out,recursive=TRUE)
p <- Sys.getenv("P15_PV_INTERPRETATION_BASE","data-derived/p15_pv_interpretation_20260909_v1")
replay <- Sys.getenv("P15_PV_INTERPRETATION_REPLAY","/tmp/p15_pv_interpretation_replay_20260909_v1")
save_csv <- function(x,n)fwrite(x,file.path(out,paste0(n,".csv")),na="")
x <- as_tibble(fread(file.path(p,"analysis_rows.csv")))
checks <- tibble(check=character(),passed=logical())
check <- function(name,value){stopifnot(isTRUE(value));checks <<- bind_rows(checks,tibble(check=name,passed=value))}
files <- list.files(p,pattern="\\.csv$",full.names=FALSE)
normalize_replay <- function(lines){
  root<-Sys.getenv("P15_REGION_REPLAY_ROOT")
  if(nzchar(root))lines<-gsub(root,Sys.getenv("P15_REGION_RUN_ROOT"),lines,fixed=TRUE)
  lines
}
independent_only <- identical(Sys.getenv("P15_PV_INDEPENDENT_ONLY"),"true")
for(f in if(independent_only)character() else setdiff(files,"output_manifest.csv")) {
  # Manifests contain location-dependent hashes; check each against its own files
  # below. Compare the numerical research tables directly across independent runs.
  if(nzchar(Sys.getenv("P15_REGION_REPLAY_ROOT")) && grepl("manifest",f))next
  check(paste0("replay_",f),identical(readLines(file.path(p,f)),normalize_replay(readLines(file.path(replay,f)))))
}
for(m in c("input_manifest.csv","code_manifest.csv","output_manifest.csv")){
  z <- fread(file.path(p,m));for(i in seq_len(nrow(z)))check(paste0("hash_",z$path[i]),
    digest::digest(file=z$path[i],algo="sha256")==z$sha256[i])
}
# Independently compute each fixed-rate schedule from payment times/principal.
independent_pv <- function(i,r){
  a<-x[i,];M<-a$official_maturity_years;G<-a$official_grace_years
  t<-sort(unique(c(seq_len(floor(M)),M)));t<-t[t>0]
  if(G==M){principal<-ifelse(t==M,100,0)}else{
    # Principal is allocated evenly over the actual post-grace duration.
    start<-c(0,head(t,-1));principal<-100*pmax(0,t-pmax(start,G))/(M-G)
  }
  balance<-100-c(0,head(cumsum(principal),-1))
  interest<-balance*a$official_rate/100*diff(c(0,t))
  sum((principal+interest)/(1+r/100)^t)
}
pv <- vapply(seq_len(nrow(x)),function(i)independent_pv(i,x$selected_rate_pct[i]),numeric(1))
check("independent_762_PV",max(abs(pv-x$market_pv_per_100))<1e-8)
check("762_unique_cases",nrow(x)==762L&&!anyDuplicated(x[c("iso3","analysis_year","creditor")]))
check("288_primary_IDS",sum(x$selected_tier %in% c("primary","ids"))==288L)
check("594_nonpeer",sum(x$selected_tier!="peer")==594L)

# Six possible update orders separate the interest-rate part from maturity/grace.
# This is arithmetic attribution of synthetic schedules, not a causal effect.
y <- inner_join(x,x |> mutate(analysis_year=analysis_year+1),
  by=c("iso3","analysis_year","creditor"),suffix=c("","_old"))
orders <- list(c(1,2,3),c(1,3,2),c(2,1,3),c(2,3,1),c(3,1,2),c(3,2,1))
three <- map_dfr(seq_len(nrow(y)),function(i){a<-y[i,]
  val<-function(s){
    r<-if(s[1])a$selected_rate_pct else a$selected_rate_pct_old
    cpn<-if(s[2])a$official_rate else a$official_rate_old
    M<-if(s[3])a$official_maturity_years else a$official_maturity_years_old
    G<-if(s[3])a$official_grace_years else a$official_grace_years_old
    100-p15_stream_pv(p15_analysis_schedule(cpn,M,G),r)
  }
  contributions<-sapply(orders,function(o){s<-rep(FALSE,3);v<-val(s);d<-numeric(3)
    for(j in o){s[j]<-TRUE;w<-val(s);d[j]<-w-v;v<-w};d})
  d<-rowMeans(contributions)
  tibble(iso3=a$iso3,analysis_year=a$analysis_year,creditor=a$creditor,
    selected_tier=a$selected_tier,previous_tier=a$selected_tier_old,
    market_ge_change=val(rep(TRUE,3))-val(rep(FALSE,3)),benchmark_component=d[1],
    official_interest_component=d[2],maturity_grace_component=d[3],
    old_official_rate=a$official_rate_old,new_official_rate=a$official_rate,
    old_maturity=a$official_maturity_years_old,new_maturity=a$official_maturity_years,
    old_grace=a$official_grace_years_old,new_grace=a$official_grace_years)
})
check("three_block_adds_exactly",max(abs(three$market_ge_change-three$benchmark_component-
  three$official_interest_component-three$maturity_grace_component))<1e-8)
save_csv(three,"three_block_case_decomposition")
save_csv(three |> group_by(creditor,analysis_year) |> summarise(n=n(),
  official_rate_increases=sum(new_official_rate>old_official_rate),
  across(c(market_ge_change,benchmark_component,official_interest_component,maturity_grace_component,
    old_official_rate,new_official_rate),mean),
  .groups="drop"),"three_block_creditor_year")
save_csv(three |> filter(selected_tier==previous_tier) |> group_by(creditor,analysis_year) |> summarise(n=n(),
  across(c(market_ge_change,benchmark_component,official_interest_component,maturity_grace_component),mean),
  .groups="drop"),"three_block_same_tier")
# Does the pooled matched annual movement survive omitting any one country?
loo <- map_dfr(sort(unique(three$analysis_year)),function(yr){z<-three |> filter(analysis_year==yr)
  map_dfr(unique(z$iso3),function(c)z |> filter(iso3!=c) |>
    summarise(analysis_year=yr,omitted_country=c,n=n(),mean_ge_change=mean(market_ge_change)))})
save_csv(loo,"matched_annual_leave_one_country_out")
save_csv(loo |> group_by(analysis_year) |> summarise(min_change=min(mean_ge_change),max_change=max(mean_ge_change),
  .groups="drop"),"matched_annual_influence_ranges")
save_csv(x |> filter(creditor=="IBRD") |> summarise(n=n(),mean_difference=mean(policy_pv_minus_market_pv),
  mean_absolute_difference=mean(abs(policy_pv_minus_market_pv)),median_absolute_difference=median(abs(policy_pv_minus_market_pv))),
  "ibrd_cancellation_check")
save_csv(checks,"verification_checks")
manifest <- function(ps)tibble(path=ps,sha256=vapply(ps,function(f)digest::digest(file=f,algo="sha256"),character(1)))
input_paths <- file.path(p,c("analysis_rows.csv","input_manifest.csv","code_manifest.csv","output_manifest.csv"))
code_paths <- c("scripts/p15/check_p15_pv_interpretation.R","R/pvr.R",
  "R/p15_pv_first_pass.R","R/p15_bullet_extension.R","R/p15_pv_interpretation.R")
save_csv(manifest(input_paths),"input_manifest")
save_csv(manifest(code_paths),"code_manifest")
save_csv(manifest(c(input_paths,code_paths)),"input_code_manifest")
writeLines(capture.output(sessionInfo()),file.path(out,"environment.txt"))
writeLines(c(paste0("build_id: ",out),paste0("parent_input: ",p),
  paste0("separate_archived_replay_comparison_run: ",!independent_only),
  "release_state: diagnostic_thesis_analysis_not_canonical",
  "schema_id: P15-PV-INTERPRETATION-FOLLOWUP-1",
  "estimator_id: exact_six_order_three_block_accounting_decomposition",
  "admissibility_id: unchanged_inherited_762_modern_rows",
  "selection_id: unchanged_reference"),file.path(out,"build_contract.txt"))
save_csv(manifest(list.files(out,full.names=TRUE)),"output_manifest")
print(three |> filter(analysis_year==2023) |> group_by(creditor) |> summarise(n=n(),
  across(c(market_ge_change,benchmark_component,official_interest_component,maturity_grace_component),mean)))
cat("Passed",nrow(checks),"checks.\n")
