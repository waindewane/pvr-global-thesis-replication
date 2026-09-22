# From project root. Recount coverage from current records; do not change the dataset.
library(data.table)
j <- jsonlite::fromJSON("data-derived/p15_master/current_run.json")
out <- "docs/thesis_design/sections/benchmark_evaluation/coverage_20260920"
p <- fread(file.path(j$candidate,"core_evidence.csv"))
e <- fread(file.path(j$candidate,"tier_eligibility.csv"))
s <- fread(file.path(j$candidate,"selected_reference.csv"))
context <- fread(file.path(j$candidate,"peer_region_context.csv"))
wb <- jsonlite::fromJSON("data-raw/world_bank_countries.json")[[2]]
geo <- data.table(iso3=wb$id,wb_region=trimws(wb$region$value))[wb_region!="Aggregates"]
stopifnot(!anyDuplicated(p[,.(iso3,analysis_year)]),!anyDuplicated(s[,.(iso3,analysis_year)]))
d <- p[historical_lmic_reporting_scope==TRUE,.(iso3,analysis_year,historical_income_level)]
d <- merge(d,s[,.(iso3,analysis_year,selected_tier,selected_rate_pct,missing_result_state)],by=c("iso3","analysis_year"))
d <- merge(d,context[,.(iso3,analysis_year,rating_source_region)],by=c("iso3","analysis_year"))
d <- merge(d,geo,by="iso3",all.x=TRUE)
d[,region:=fifelse(is.na(rating_source_region)|rating_source_region=="",wb_region,trimws(rating_source_region))]
tiers <- c("primary","ids","secondary","moodys","peer")
for (tier_name in tiers) {
  x <- e[tier==tier_name,.(iso3,analysis_year,eligible)]
  setnames(x,"eligible",tier_name)
  d <- merge(d,x,by=c("iso3","analysis_year"))
}
stopifnot(nrow(d)==1756L)
summarise <- function(x) data.table(n=nrow(x),countries=uniqueN(x$iso3),
 primary=sum(x$primary),ids=sum(x$ids),secondary=sum(x$secondary),moodys=sum(x$moodys),peer=sum(x$peer),
 any_direct=sum(x$primary|x$secondary),both_direct=sum(x$primary&x$secondary),
 any_primary_ids_secondary=sum(x$primary|x$ids|x$secondary),
 any_selected=sum(is.finite(x$selected_rate_pct)),selected_peer=sum(x$selected_tier=="peer"))
checks <- list()
for (group in c("overall","analysis_year","historical_income_level","region")) {
  actual <- if(group=="overall") summarise(d) else d[,summarise(.SD),by=group]
  filename <- switch(group,overall="coverage_overall.csv",analysis_year="coverage_by_year.csv",historical_income_level="coverage_by_income.csv",region="coverage_by_region.csv")
  expected <- fread(file.path(j$stages$assessment$dir,filename))
  if(group=="overall") expected <- expected[scope=="historical_LMIC"] else {
    setorderv(actual,group);setorderv(expected,group)
    stopifnot(identical(actual[[group]],expected[[group]]))
  }
  cols <- setdiff(names(actual),group)
  for(col in cols) stopifnot(all(actual[[col]]==expected[[col]]))
  checks[[group]] <- data.table(table=filename,rows=nrow(actual),quantities=length(cols),passed=TRUE)
}
source_table <- rbindlist(lapply(tiers,function(t) data.table(source=t,eligible_country_years=sum(d[[t]]),selected_country_years=sum(d$selected_tier==t))))
source_table[,cumulative_selected:=cumsum(selected_country_years)]
source_table[,cumulative_percent:=100*cumulative_selected/nrow(d)]
annual <- d[,.(country_years=.N,primary=sum(selected_tier=="primary"),ids=sum(selected_tier=="ids"),secondary=sum(selected_tier=="secondary"),rating_implied=sum(selected_tier=="moodys"),peer=sum(selected_tier=="peer"),no_rate=sum(!is.finite(selected_rate_pct))),by=analysis_year][order(analysis_year)]
stopifnot(all(rowSums(annual[,.(primary,ids,secondary,rating_implied,peer,no_rate)])==annual$country_years))
groups <- rbindlist(lapply(c("historical_income_level","region"),function(g) {
  ans <- d[,.(country_years=.N,primary_ids_secondary=sum(primary|ids|secondary),nonpeer=sum(selected_tier%in%tiers[1:4]),peer_selected=sum(selected_tier=="peer"),selected=sum(is.finite(selected_rate_pct))),by=g]
  setnames(ans,g,"group");ans[,dimension:=g];ans
}))
for(col in c("primary_ids_secondary","nonpeer","peer_selected","selected")) groups[,(paste0(col,"_percent")):=100*get(col)/country_years]
fwrite(source_table,file.path(out,"source_contributions.csv"))
fwrite(annual,file.path(out,"annual_selected_sources.csv"))
fwrite(groups,file.path(out,"coverage_composition.csv"))
fwrite(rbindlist(checks),file.path(out,"verification.csv"))
fwrite(d[!is.finite(selected_rate_pct),.N,by=.(analysis_year,missing_result_state)],file.path(out,"uncovered_states.csv"))
print(source_table);print(groups);print(annual)
cat("Verified overall, annual, income and regional coverage against current outputs.\n")
