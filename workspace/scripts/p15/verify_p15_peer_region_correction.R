#!/usr/bin/env Rscript
suppressPackageStartupMessages({library(data.table);library(digest)})
args<-commandArgs(TRUE)
raw_replay<-if(length(args))args[1] else ""
new<-"data-derived/p15_analysis_candidate_20260909_region_v1"
root<-"data-derived/p15_region_correction_research_20260909_v2"
replay<-"/tmp/p15_region_research_replay_20260909"
out<-if(length(args)>1)args[2] else "data-derived/p15_region_correction_verification_20260909_v1"
stopifnot(!dir.exists(out));dir.create(out,recursive=TRUE)
checks<-data.table(check=character(),passed=logical())
ck<-function(n,v){checks<<-rbind(checks,data.table(check=n,passed=isTRUE(v)));if(!isTRUE(v))stop(n)}
save<-function(x,n)fwrite(x,file.path(out,paste0(n,".csv")),na="")
norm<-function(x){gsub(replay,root,x,fixed=TRUE)}
stages<-c("assessment","deeper","regional","policy","official_policy","pv_first","bullet","pv_interpretation","pv_followup")
for(stage in stages){
 path<-file.path(root,stage)
 for(f in list.files(path,pattern="[.]csv$")) {
   if(grepl("manifest",f))next
   ck(paste("replay",stage,f),identical(readLines(file.path(path,f)),norm(readLines(file.path(replay,stage,f)))))
 }
 for(m in list.files(path,pattern="manifest[.]csv$",full.names=TRUE)){
   z<-fread(m);col<-if("artifact_path"%in%names(z))"artifact_path" else "path"
   ck(paste("hash",stage,basename(m)),all(vapply(z[[col]],digest,character(1),file=TRUE,algo="sha256")==z$sha256))
 }
}
for(m in c("input_manifest.csv","script_manifest.csv","output_manifest.csv")){
 z<-fread(file.path(new,m));ck(paste("candidate",m),all(vapply(z$artifact_path,digest,character(1),file=TRUE,algo="sha256")==z$sha256))
}
if(nzchar(raw_replay)) {
 rr<-file.path(raw_replay,"isolated_build",new)
 for(f in setdiff(list.files(new,pattern="[.]csv$"),c("input_manifest.csv","script_manifest.csv","output_manifest.csv")))
   ck(paste("raw_replay",f),identical(readLines(file.path(new,f)),readLines(file.path(rr,f))))
 ck("full_raw_replay_acceptance",all(fread(file.path(raw_replay,"acceptance.csv"))$passed))
}
old<-"data-derived/p15_analysis_candidate_20260907_v1"
a<-fread(file.path(old,"core_evidence.csv"));b<-fread(file.path(new,"core_evidence.csv"))
bundle<-c("build_id","schema_id","estimator_id","admissibility_id","selection_id","source_package_ids")
allowed<-c(bundle,"peer_rate_pct","peer_minimum_met","peer_pool_rule","peer_country_count","peer_iqr_pp",
 "global_pool_used","peer_candidate_state","peer_evidence_id")
keep<-setdiff(names(a),allowed)
ck("all_nonpeer_core_fields_unchanged",identical(a[,..keep],b[,..keep]))
for(f in c("selected_reference.csv","selected_without_peers.csv")){
 aa<-fread(file.path(old,f));bb<-fread(file.path(new,f))
 ck(paste(f,"same_keys_and_tiers"),identical(aa[,.(analysis_year,iso3,selected_tier)],bb[,.(analysis_year,iso3,selected_tier)]))
 ck(paste(f,"unchanged_stronger_rates"),identical(aa[selected_tier!="peer",selected_rate_pct],bb[selected_tier!="peer",selected_rate_pct]))
}
changes<-fread(file.path(new,"selection_changes.csv"))[view_id=="ids_before_secondary__with_peer"&historical_lmic_reporting_scope]
save(changes[rate_changed==TRUE][order(-abs(change_pp))],"changed_selected_LMIC_cases")
save(changes[old_tier=="peer",.(n=.N,changed=sum(rate_changed),mean_abs_change=mean(abs(change_pp)),
 max_abs_change=max(abs(change_pp)),old_global=sum(old_global),new_global=sum(new_global))],"selected_peer_impact")
compare<-function(oldfile,newfile,keys,values,name){
 aa<-fread(oldfile);bb<-fread(newfile)
 stopifnot(!anyDuplicated(aa[,..keys]),!anyDuplicated(bb[,..keys]))
 z<-merge(aa[,c(keys,values),with=FALSE],bb[,c(keys,values),with=FALSE],by=keys,suffixes=c("_before","_after"),all=TRUE)
 for(v in values)z[,(paste0(v,"_change")):=get(paste0(v,"_after"))-get(paste0(v,"_before"))]
 save(z,name);z
}
compare("data-derived/p15_deeper_assessment_20260908_v4/annual_composition.csv",file.path(root,"deeper/annual_composition.csv"),
 "analysis_year",c("total_mean_change","common_country_change"),"annual_findings_changes")
compare("data-derived/p15_regional_assessment_20260908_v2/regional_yoy.csv",file.path(root,"regional/regional_yoy.csv"),
 c("scope","region","view","analysis_year"),c("n","mean_change"),"regional_yoy_changes")
compare("data-derived/p15_official_policy_comparison_20260908_v1/paired_summary.csv",file.path(root,"official_policy/paired_summary.csv"),
 c("tier","policy"),c("n","tier_mae","mean_improvement"),"policy_findings_changes")
compare("data-derived/p15_pv_interpretation_20260909_v1/summary_period.csv",file.path(root,"pv_interpretation/summary_period.csv"),
 c("evidence_view","period"),c("n","mean_difference"),"pv_period_changes")
compare("data-derived/p15_pv_interpretation_20260909_v1/summary_creditor.csv",file.path(root,"pv_interpretation/summary_creditor.csv"),
 c("evidence_view","creditor"),c("n","mean_difference","mean_market_ge"),"pv_creditor_changes")
pv<-compare("data-derived/p15_pv_interpretation_20260909_v1/analysis_rows.csv",file.path(root,"pv_interpretation/analysis_rows.csv"),
 c("iso3","analysis_year","creditor","selected_tier"),c("selected_rate_pct","market_pv_per_100","policy_pv_per_100","policy_pv_minus_market_pv"),"pv_case_changes")
ck("pv_policy_values_unchanged",all(abs(pv$policy_pv_per_100_change)<1e-10))
ck("pv_nonpeer_values_unchanged",all(abs(pv[selected_tier!="peer",market_pv_per_100_change])<1e-10))
save(pv[,.(n=.N,changed=sum(abs(market_pv_per_100_change)>1e-10),
 mean_abs_pv_change=mean(abs(market_pv_per_100_change)),max_abs_pv_change=max(abs(market_pv_per_100_change)))],"pv_impact_summary")
# Independent direct summation of each modern repayment schedule, no project PV helper.
x<-fread(file.path(root,"pv_interpretation/analysis_rows.csv"))
for(i in seq_len(nrow(x))){
 z<-x[i];M<-z$official_maturity_years;G<-z$official_grace_years
 t<-sort(unique(c(seq_len(floor(M)),M)));t<-t[t>0];start<-c(0,head(t,-1))
 principal<-if(G==M)ifelse(t==M,100,0) else 100*pmax(0,t-pmax(start,G))/(M-G)
 balance<-100-c(0,head(cumsum(principal),-1));payments<-principal+balance*z$official_rate/100*(t-start)
 ck(paste("independent_PV",z$iso3,z$analysis_year,z$creditor),
  abs(sum(payments/(1+z$selected_rate_pct/100)^t)-z$market_pv_per_100)<1e-8)
}
save(checks,"checks")
save(data.table(path=c("scripts/p15/verify_p15_peer_region_correction.R","scripts/p15/_targets_peer_region_correction.R"),
 sha256=vapply(c("scripts/p15/verify_p15_peer_region_correction.R","scripts/p15/_targets_peer_region_correction.R"),digest,character(1),file=TRUE,algo="sha256")),"code_manifest")
cat(nrow(checks),"checks passed. Raw replay:",raw_replay,"\n")
