#!/usr/bin/env Rscript
library(data.table);library(xml2);library(countrycode);library(digest)
args<-commandArgs(trailingOnly=TRUE);out<-if(length(args))args[1] else "data-derived/p15_official_dac_mapping_20260908_v1"
src<-"sources/official_terms/oecd_dac_lists_20260908"
m<-fread(file.path(out,"map.csv"));l<-fread(file.path(out,"lists.csv"));checks<-list()
add<-function(n,v)checks[[length(checks)+1L]]<<-data.table(check=n,passed=isTRUE(v))
# Independent PDF-versus-CSV check for the machine-readable source periods.
pdf_rows<-rbindlist(lapply(c("2021","2022-23","2024"),function(per){
 tmp<-tempfile();system2("pdftotext",c("-bbox",shQuote(file.path(src,paste0("dac_",per,".pdf"))),shQuote(tmp)))
 doc<-read_xml(tmp);xml_ns_strip(doc);a<-xml_find_all(doc,".//word")
 z<-data.table(text=xml_text(a),x=as.numeric(xml_attr(a,"xMin")),y=as.numeric(xml_attr(a,"yMin")))
 top<-z[text=="Afghanistan",min(y)];first<-z[abs(y-top)<1]
 an<-c("Afghanistan","Democratic",if(per=="2021")"Armenia" else "Algeria","Albania")
 starts<-sapply(an,function(a)first[text==a,x][1]);stopifnot(!anyNA(starts))
 z[,col:=findInterval(x,starts-1)];z<-z[y>=top-.2&col%in%1:4]
 bottom<-z[text=="Futuna"|text=="Futuna*",min(y)]
 if(per=="2024")bottom<-z[col==4&text=="Strip",min(y)]
 z<-z[y<=bottom+.2]
 if(per=="2024")z<-z[col==4|y<=z[col==1&text=="Zambia",min(y)]+.2]
 z<-z[!grepl("^[0-9]+$|^\\((L|LM|UM|H)\\)$",text)]
 z[,line:=round(y,1)];setorder(z,col,line,x)
 r<-z[,.(source_name=paste(text,collapse=" ")),by=.(col,line)]
 # The 2024 left-footnote block starts below Zambia; no country words are removed.
 r[,source_name:=gsub("[0-9*]","",source_name)]
 r[,source_name:=trimws(gsub("\\([ LMUH]*\\)","",source_name))];r<-r[nzchar(source_name)]
 r[,iso3:=countrycode(source_name,"country.name","iso3c",warn=FALSE)]
 r[source_name=="Micronesia",iso3:="FSM"];r[source_name=="Kosovo",iso3:="XKX"]
 if(anyNA(r$iso3)){print(r[is.na(iso3)]);stop("PDF check unresolved country")}
 r[,.(period=per,iso3,pdf_group=c("LDCs","Other LICs","LMICs","UMICs")[col])]
}))
cmp<-merge(l[period%in%c("2021","2022-23","2024"),.(period,iso3,csv_group=dac_group)],pdf_rows,by=c("period","iso3"),all=TRUE)
cmp[,same:=!is.na(csv_group)&!is.na(pdf_group)&csv_group==pdf_group]
print(cmp[same==FALSE])
add("csv_pdf_all_agree_except_documented_palau_reinstatement",all(cmp$same|(cmp$period=="2022-23"&cmp$iso3=="PLW"&cmp$csv_group=="UMICs"&is.na(cmp$pdf_group))))
# Individually inspected old PDF assignments and non-membership, not generated expectations.
cases<-data.table(iso3=c("KEN","TJK","JOR","PRY","GTM","ARM","JOR","PLW","PAN","ATG","ROU","RUS"),analysis_year=c(2012,2014,2016,2017,2020,2021,2022,2022,2022,2022,2014,2019),expected=c("Other LICs","Other LICs","UMICs","LMICs","LMICs","LMICs","UMICs","UMICs","UMICs","not_on_dac_list","not_on_dac_list","not_on_dac_list"))
cases<-merge(cases,m[,.(iso3,analysis_year,dac_group)],by=c("iso3","analysis_year"),all.x=TRUE)
for(i in seq_len(nrow(cases)))add(paste0("source_case_",cases$iso3[i],"_",cases$analysis_year[i]),cases$expected[i]==cases$dac_group[i])
add("all_211_by_13_keys",nrow(m)==211*13&&!anyDuplicated(m[,.(iso3,analysis_year)]))
add("dac_rate_rules",all(m[dac_eligible==TRUE,group_rate_pct]==c(9,9,7,6)[match(m[dac_eligible==TRUE,dac_group],c("LDCs","Other LICs","LMICs","UMICs"))]))
add("new_ge_not_before_2015",all(is.na(m[analysis_year<2015,grant_equivalent_new_loan_rate_pct])))
add("headline_legacy_before2018",all(m[dac_eligible==TRUE&analysis_year<2018,historical_headline_new_loan_rate_pct]==10))
for(f in c("input_manifest.csv","script_manifest.csv","output_manifest.csv")){a<-fread(file.path(out,f));add(f,all(vapply(a$artifact_path,digest,character(1),file=TRUE,algo="sha256")==a$sha256))}
r<-rbindlist(checks);print(r);stopifnot(all(r$passed))
fwrite(r,file.path(out,"independent_verification.csv"));fwrite(cmp,file.path(out,"csv_pdf_verification.csv"));fwrite(cases,file.path(out,"inspected_case_verification.csv"))
