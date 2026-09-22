source("R/p15_current_inputs.R")
#!/usr/bin/env Rscript
suppressPackageStartupMessages({library(data.table);library(xml2);library(countrycode);library(digest)})
source("R/research_governance.R")
args<-commandArgs(trailingOnly=TRUE);out<-if(length(args))args[1] else file.path("data-derived",paste0("p15_dac_",format(Sys.time(),"%Y%m%d_%H%M%S")))
if(dir.exists(out))stop("Refusing overwrite")
src<-"sources/official_terms/oecd_dac_lists_20260908"
manifest<-fread(file.path(src,"source_manifest.csv"))
ok<-manifest[retrieval_state=="downloaded"]
stopifnot(all(vapply(ok$artifact_path,digest,character(1),file=TRUE,algo="sha256")==ok$sha256))
periods<-c("2012-13","2014-17","2018-19","2020","2021","2022-23","2024")
lo<-c(2012,2014,2018,2020,2021,2022,2024);hi<-c(2013,2017,2019,2020,2021,2023,2024)
groups<-c("LDCs","Other LICs","LMICs","UMICs")
parse_pdf<-function(period){
  file<-file.path(src,paste0("dac_",period,".pdf"));tmp<-tempfile(fileext=".html")
  stopifnot(system2("pdftotext",c("-bbox",shQuote(file),shQuote(tmp)))==0)
  doc<-read_xml(tmp);xml_ns_strip(doc);w<-xml_find_all(doc,".//word")
  z<-data.table(text=xml_text(w),x=as.numeric(xml_attr(w,"xMin")),y=as.numeric(xml_attr(w,"yMin")))
  top<-z[text=="Afghanistan",min(y)];bottom<-z[text=="Futuna",min(y)]
  first<-z[abs(y-top)<1][order(x)]
  anchors<-c("Afghanistan",if(period=="2012-13")"Kenya" else "Democratic","Armenia","Albania")
  starts<-vapply(anchors,function(a)first[text==a,x][1],numeric(1))
  stopifnot(length(starts)==4,!anyNA(starts),all(diff(starts)>0))
  z<-z[y>=top-0.2&y<=bottom+0.2&!grepl("^[0-9]+$",text)]
  z[,column:=findInterval(x,starts-1)]
  stopifnot(all(z$column%in%1:4))
  z[,line:=round(y,1)];setorder(z,column,line,x)
  a<-z[,.(source_name=paste(text,collapse=" ")),by=.(column,line)]
  a[,`:=`(source_name=gsub("[0-9*]+$|^\\*","",source_name),dac_group=groups[column],source_locator=paste0("PDF p1 column ",column," y=",line),source_file=file)]
  a[,.(source_name,dac_group,source_locator,source_file)]
}
lists<-rbindlist(lapply(seq_along(periods),function(i){
  per<-periods[i]
  if(i<=4)a<-parse_pdf(per) else {
    file<-file.path(src,paste0("dac_",per,".csv"));raw<-as.data.table(read.csv(file,fileEncoding="latin1"))
    a<-raw[,.(source_name=RecipientNameE,dac_group=GroupNameE,source_locator=paste0("CSV row ",seq_len(.N)+1L,"; RecipientCode=",RecipientCode),source_file=file)]
  }
  a[,`:=`(period=per,valid_from=lo[i],valid_through=hi[i])];a
}))
lists[,iso3:=countrycode(source_name,"country.name","iso3c",warn=FALSE)]
aliases<-c("Kosovo"="XKX","West Bank and Gaza Strip"="PSE","St. Helena"="SHN","Saint Helena"="SHN","St. Kitts-Nevis"="KNA","St. Vincent and Grenadines"="VCT","Korea, Dem. Rep."="PRK","Congo, Dem. Rep."="COD","Congo, Rep."="COG")
aliases<-c(aliases,"Micronesia"="FSM")
for(n in names(aliases))lists[source_name==n,iso3:=aliases[[n]]]
if(anyNA(lists$iso3)){print(lists[is.na(iso3)]);stop("Unresolved source country")}
stopifnot(!anyDuplicated(lists[,.(period,iso3)]),all(lists$dac_group%in%groups))
lists[,group_rate_pct:=c(9,9,7,6)[match(dac_group,groups)]]
lists[,source_url:=manifest$url[match(source_file,manifest$artifact_path)]]
lists[,source_sha256:=manifest$sha256[match(source_file,manifest$artifact_path)]]
annual<-lists[,.(analysis_year=seq.int(valid_from,valid_through)),by=names(lists)]
base<-Sys.getenv("P15_ANALYSIS_BASE", p15_current_input("dataset"))
policy_base<-Sys.getenv("P15_POLICY_BASE", p15_current_input("policy"))
p<-fread(file.path(base,"core_evidence.csv"))
grid<-unique(p[,.(iso3,country,analysis_year,historical_income_level,historical_lmic_reporting_scope)])
map<-merge(grid,annual,by=c("iso3","analysis_year"),all.x=TRUE)
map[,dac_eligible:=!is.na(dac_group)]
map[dac_eligible==FALSE,`:=`(dac_group="not_on_dac_list",source_locator="absent from complete effective recipient list")]
for(i in seq_along(periods))map[!dac_eligible&analysis_year>=lo[i]&analysis_year<=hi[i],`:=`(period=periods[i],valid_from=lo[i],valid_through=hi[i],source_file=file.path(src,paste0("dac_",periods[i],if(i<=4)".pdf" else ".csv"))) ]
map[dac_eligible==FALSE,`:=`(source_url=manifest$url[match(source_file,manifest$artifact_path)],source_sha256=manifest$sha256[match(source_file,manifest$artifact_path)])]
map[,`:=`(historical_headline_new_loan_rate_pct=ifelse(dac_eligible,ifelse(analysis_year<2018,10,group_rate_pct),NA_real_),
  grant_equivalent_new_loan_rate_pct=ifelse(dac_eligible&analysis_year>=2015,group_rate_pct,NA_real_),
  regime=fcase(analysis_year<=2014,"legacy_cash_flow",analysis_year<=2017,"dual_reporting_transition",default="grant_equivalent_headline"),
  applicability="new bilateral official-sector commitment in stated year; not existing-loan replay",
  source_snapshot_id="SRC-OECD-DAC-LISTS-20260908")]
old<-fread(file.path(policy_base,"policy_mapping.csv"))
diff<-merge(old,map,by=c("iso3","analysis_year"),suffixes=c("_old","_official"),all.x=TRUE)
diff[,assignment_change:=fcase(!dac_eligible,"not_dac_eligible",group976!=group_rate_pct,"category_rate_changed",default="same_group_rate")]
counts<-lists[,.(recipients=.N),by=.(period,dac_group)]
checks<-data.table(check=c("full_2743_grid","unique_country_year","source_hashes_present","all_primary215_joined","ineligible_rates_missing","transition_dual_visible"),passed=c(nrow(map)==2743,!anyDuplicated(map[,.(iso3,analysis_year)]),!anyNA(map$source_sha256),nrow(diff)==215&&!anyNA(diff$dac_eligible),all(is.na(map[dac_eligible==FALSE,group_rate_pct])),all(map[analysis_year%in%2015:2017,regime]=="dual_reporting_transition")))
stopifnot(all(checks$passed));dir.create(out,recursive=TRUE)
for(n in c("lists","annual","map","diff","counts","checks"))fwrite(get(n),file.path(out,paste0(n,".csv")))
mm<-function(paths,role)pvr_manifest_rows(paths,role,basename(out),"SCHEMA-P15-DAC-MAPPING-V1","EST-P15-OFFICIAL-DAC-LIST-V1","ADM-P15-DAC-ELIGIBILITY-V1","SEL-P15-NO-PROMOTION-V1","SRC-OECD-DAC-LISTS-20260908")
fwrite(mm(c(ok$artifact_path,file.path(src,"source_manifest.csv"),file.path(base,"core_evidence.csv"),file.path(policy_base,"policy_mapping.csv")),"input"),file.path(out,"input_manifest.csv"))
fwrite(mm(c("scripts/p15/build_oecd_dac_mapping.R","R/research_governance.R"),"code"),file.path(out,"script_manifest.csv"))
fwrite(mm(list.files(out,full.names=TRUE),"output"),file.path(out,"output_manifest.csv"))
capture.output(sessionInfo(),file=file.path(out,"environment.txt"))
print(counts);print(diff[,.(n=.N),by=assignment_change]);print(diff[assignment_change!="same_group_rate",.(iso3,analysis_year,group976,dac_group,group_rate_pct,assignment_change)])
