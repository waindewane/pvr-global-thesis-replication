source("R/p15_current_inputs.R")
#!/usr/bin/env Rscript
source("scripts/p15/activate_p15_environment.R")
suppressPackageStartupMessages({library(data.table);library(jsonlite);library(digest);library(ggplot2)})
args <- commandArgs(TRUE)
out <- if(length(args)) args[1] else file.path("data-derived",paste0("p15_regional_followup_",format(Sys.time(),"%Y%m%d_%H%M%S")))
stopifnot(!dir.exists(out))
state <- fromJSON("data-derived/p15_master/current_run.json")
base <- Sys.getenv("P15_REGIONAL_BASE", state$stages$regional$dir)
design <- "docs/thesis_design/feedback_2026-09-11/REGIONAL_ANALYSIS_DESIGN.md"
inputs <- c(file.path(base,c("country_year_views.csv","regional_levels.csv","regional_yoy.csv")),design)
before <- vapply(inputs,function(p)digest(file=p,algo="sha256"),character(1))
x <- fread(inputs[1],na.strings="")
stopifnot(!anyDuplicated(x[,.(iso3,analysis_year,view)]))
raw <- copy(x)[,restriction:="as_registered"]
screened <- copy(x)[,restriction:="recorded_secondary_holds"]
screened[reviewed_eight_exclusion==TRUE,rate:=NA_real_]
z <- rbindlist(list(raw,screened))
annual <- z[,.(population_countries=.N,countries=sum(is.finite(rate)),
 mean_rate_pct=if(any(is.finite(rate)))mean(rate,na.rm=TRUE) else NA_real_,
 median_rate_pct=if(any(is.finite(rate)))median(rate,na.rm=TRUE) else NA_real_),
 by=.(restriction,view,region,analysis_year)]
z[,period:=fcase(analysis_year<=2017,"2012_2017",analysis_year<=2019,"2018_2019",
 analysis_year<=2021,"2020_2021",default="2022_2024")]
period_lengths <- c(`2012_2017`=6L,`2018_2019`=2L,`2020_2021`=2L,`2022_2024`=3L)
country_period <- z[is.finite(rate),.(observed_years=.N,mean_rate_pct=mean(rate),
 sources=paste(sort(unique(source)),collapse=";")),by=.(restriction,view,region,iso3,period)]
country_period[,complete_window:=observed_years==period_lengths[period]]
period <- country_period[,.(countries=.N,country_years=sum(observed_years),
 mean_rate_pct=mean(mean_rate_pct),median_rate_pct=median(mean_rate_pct),
 complete_countries=sum(complete_window)),by=.(restriction,view,region,period)]
contrasts <- list(c("2018_2019","2020_2021"),c("2020_2021","2022_2024"),c("2018_2019","2022_2024"))
matched <- rbindlist(lapply(contrasts,function(pair){
 a <- country_period[period==pair[1],.(restriction,view,region,iso3,
 earlier_mean=mean_rate_pct,earlier_years=observed_years,earlier_complete=complete_window,earlier_sources=sources)]
 b <- country_period[period==pair[2],.(restriction,view,region,iso3,
 later_mean=mean_rate_pct,later_years=observed_years,later_complete=complete_window,later_sources=sources)]
 q <- merge(a,b,by=c("restriction","view","region","iso3"))
 q[,`:=`(earlier_period=pair[1],later_period=pair[2],difference_pp=later_mean-earlier_mean)]
 rbindlist(list(copy(q)[,coverage_requirement:="at_least_one_year_per_window"],
 q[earlier_complete&later_complete][,coverage_requirement:="every_year_in_both_windows"]))
}))
matched_summary <- matched[,.(countries=.N,earlier_country_years=sum(earlier_years),
 later_country_years=sum(later_years),earlier_mean_pct=mean(earlier_mean),later_mean_pct=mean(later_mean),
 mean_difference_pp=mean(difference_pp),median_difference_pp=median(difference_pp),
 countries_with_unchanged_source_set=sum(earlier_sources==later_sources)),
 by=.(restriction,view,region,earlier_period,later_period,coverage_requirement)]
prior <- z[,.(restriction,view,region,iso3,analysis_year=analysis_year+1L,prior_rate=rate,prior_source=source)]
yoy <- merge(z[is.finite(rate)],prior,by=c("restriction","view","region","iso3","analysis_year"))[
 is.finite(prior_rate)]
yoy[,`:=`(change_pp=rate-prior_rate,source_switch=source!=prior_source)]
yoy_summary <- yoy[,.(countries=.N,mean_change_pp=mean(change_pp),median_change_pp=median(change_pp),
 source_switches=sum(source_switch),same_source_countries=sum(!source_switch),
 same_source_mean_change_pp=if(any(!source_switch))mean(change_pp[!source_switch]) else NA_real_),
 by=.(restriction,view,region,analysis_year)]
regions <- sort(unique(z$region))
global <- rbindlist(lapply(c("none",regions),function(excluded){
 q <- z[is.finite(rate)&(excluded=="none"|region!=excluded)]
 a <- q[,.(countries=.N,regions=uniqueN(region),mean_rate_pct=mean(rate),median_rate_pct=median(rate)),
 by=.(restriction,view,analysis_year)]
 a[,excluded_region:=excluded]
 a
}))
global_base <- global[excluded_region=="none",.(restriction,view,analysis_year,all_regions_mean_pct=mean_rate_pct)]
global <- merge(global,global_base,by=c("restriction","view","analysis_year"))
global[,difference_from_all_regions_pp:=mean_rate_pct-all_regions_mean_pct]
equal_region <- annual[countries>0,.(represented_regions=.N,mean_rate_pct=mean(mean_rate_pct),
 min_regional_countries=min(countries),max_regional_countries=max(countries)),by=.(restriction,view,analysis_year)]
registered <- fread(inputs[2])[scope=="historical_LMIC"]
replay <- merge(annual[restriction=="as_registered"],registered,by=c("view","region","analysis_year"))
registered_yoy <- fread(inputs[3])[scope=="historical_LMIC"]
replay_yoy <- merge(yoy_summary[restriction=="as_registered"],registered_yoy,
 by=c("view","region","analysis_year"))
same_numeric <- function(a,b) all((is.na(a)&is.na(b))|(!is.na(a)&!is.na(b)&abs(a-b)<1e-10))
checks <- data.table(check=c("unique_source_country_year_rows","all_13_years_present",
 "raw_annual_counts_reproduce_registered","raw_annual_means_reproduce_registered",
 "raw_common_country_changes_reproduce_registered","holds_change_only_flagged_rates",
 "matched_country_period_identity","complete_windows_have_all_years",
 "global_omissions_use_existing_regions","source_inputs_unchanged"),
 passed=c(!anyDuplicated(x[,.(iso3,analysis_year,view)]),identical(sort(unique(x$analysis_year)),2012:2024),
 nrow(replay)==nrow(registered)&&all(replay$countries==replay$n),
 same_numeric(replay$mean_rate_pct,replay$mean_rate),
 nrow(replay_yoy)==nrow(registered_yoy)&&same_numeric(replay_yoy$mean_change_pp,replay_yoy$mean_change),
 same_numeric(raw[reviewed_eight_exclusion==FALSE,rate],screened[reviewed_eight_exclusion==FALSE,rate]),
 all(abs(matched$difference_pp-(matched$later_mean-matched$earlier_mean))<1e-10),
 all(matched[coverage_requirement=="every_year_in_both_windows",earlier_complete&later_complete]),
 setequal(setdiff(unique(global$excluded_region),"none"),regions),
 identical(before,vapply(inputs,function(p)digest(file=p,algo="sha256"),character(1)))))
stopifnot(all(checks$passed))
dir.create(out,recursive=TRUE)
tables <- list(annual_regional_rates=annual,country_period_rates=country_period,
 regional_period_rates=period,matched_period_country_details=matched,
 matched_period_contrasts=matched_summary,annual_country_change_details=yoy,
 annual_common_country_changes=yoy_summary,global_region_exclusions=global,
 equal_region_annual_means=equal_region,checks=checks)
for(nm in names(tables))fwrite(tables[[nm]],file.path(out,paste0(nm,".csv")),na="")
fig <- annual[restriction=="recorded_secondary_holds"&view%in%c("no_peer","primary","moodys")]
fig <- copy(fig)[,display_rate:=fifelse(countries>=3,mean_rate_pct,NA_real_)]
fig[,evidence:=factor(view,levels=c("no_peer","primary","moodys"),
 labels=c("Available benchmark, excluding peers","Primary issuance","Rating-implied"))]
theme_set(theme_minimal(base_size=11)+theme(panel.grid.minor=element_blank(),
 legend.position="bottom",legend.title=element_blank(),plot.caption=element_text(hjust=0,size=9),
 strip.text=element_text(face="bold")))
g <- ggplot(fig,aes(analysis_year,display_rate,color=evidence,linetype=evidence))+
 geom_line(linewidth=.65,na.rm=TRUE)+geom_point(size=1,na.rm=TRUE)+facet_wrap(~region,ncol=3)+
 scale_color_manual(values=c("#174A68","#478A7F","#B36A35"))+
 scale_linetype_manual(values=c("solid","dotted","longdash"))+
 scale_x_continuous(breaks=c(2012,2016,2020,2024))+
 labs(title="Borrowing-rate evidence across regions",subtitle="Equal-country means in each source's available sample, 2012-2024",
 x=NULL,y="Benchmark rate (%)",caption=paste("Historical low- and middle-income economies; existing secondary-use restrictions applied.",
 "Calendar-year references retain different timing: issuance during the year, year-end secondary quotes, and annual rating estimates.",
 "Lines break where fewer than three countries have a rate. The samples differ between lines; country counts are supplied separately.",sep="\n"))
ggsave(file.path(out,"regional_borrowing_rates.png"),g,width=12.5,height=7.8,dpi=160,bg="white")
ggsave(file.path(out,"regional_borrowing_rates.pdf"),g,width=12.5,height=7.8,bg="white")
g2 <- ggplot(fig,aes(analysis_year,countries,color=evidence,linetype=evidence))+
 geom_line(linewidth=.65)+facet_wrap(~region,ncol=3)+scale_color_manual(values=c("#174A68","#478A7F","#B36A35"))+
 scale_linetype_manual(values=c("solid","dotted","longdash"))+
 scale_x_continuous(breaks=c(2012,2016,2020,2024))+
 labs(title="Countries represented in the regional rate comparisons",subtitle="Coverage corresponding to the borrowing-rate figure",x=NULL,y="Countries")
ggsave(file.path(out,"regional_country_coverage.png"),g2,width=12.5,height=7.8,dpi=160,bg="white")
ggsave(file.path(out,"regional_country_coverage.pdf"),g2,width=12.5,height=7.8,bg="white")
manifest <- function(paths)data.table(path=paths,sha256=vapply(paths,function(p)digest(file=p,algo="sha256"),character(1)),bytes=file.info(paths)$size)
fwrite(manifest(inputs),file.path(out,"input_manifest.csv"))
fwrite(manifest(c("scripts/p15/annotation_followup_20260911/build_regional_followup.R",
 "scripts/p15/activate_p15_environment.R","renv.lock")),file.path(out,"code_manifest.csv"))
write_json(list(build_id=basename(out),lifecycle_status="diagnostic",schema_id="SCHEMA-P15-REGIONAL-ANNOTATION-20260911-V1",
 estimator_id="EST-P15-REGIONAL-MATCHED-DESCRIPTIVE-20260911-V1",admissibility_id="ADM-P15-INHERITED-REGIONAL-SECONDARY-HOLDS-V1",
 selection_id="SEL-P15-UNCHANGED-REFERENCE-DIAGNOSTIC-SUBSETS-V1",source_parent=base,
 weights="equal countries within years; equal country-period means across countries; alternative equal-region mean identified separately",
 periods=names(period_lengths),interpretation="descriptive samples, not causal regional effects"),file.path(out,"version_bundle.json"),auto_unbox=TRUE,pretty=TRUE)
writeLines(capture.output(sessionInfo()),file.path(out,"environment.txt"))
fwrite(manifest(list.files(out,full.names=TRUE)),file.path(out,"output_manifest.csv"))
cat("Regional follow-up complete:",out,";",nrow(checks),"checks passed\n")
