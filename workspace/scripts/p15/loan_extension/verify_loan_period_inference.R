#!/usr/bin/env Rscript
source("scripts/p15/activate_p15_environment.R")
suppressPackageStartupMessages({library(data.table);library(sandwich);library(digest)})
args<-commandArgs(TRUE)
out<-if(length(args))args[1] else "data-derived/p15_loan_period_inference_20260910_v1"
x<-fread(file.path(out,"analysis_rows.csv"))
primary<-fread(file.path(out,"primary_country_inference.csv"))
shared<-fread(file.path(out,"shared_year_inference.csv"))
boot<-fread(file.path(out,"country_bootstrap_inference.csv"))
draws<-fread(file.path(out,"country_bootstrap_draws.csv"))
checks<-list()
for(k in c("aiddata","add")) {
 d<-as.data.frame(x[dataset==k]);d$late_indicator<-as.integer(d$late)
 fit<-lm(delta_ge_pp~late_indicator,data=d)
 p<-primary[dataset==k]
 checks[[paste(k,"OLS contrast")]]<-abs(coef(fit)[2]-p$estimate_pp)<1e-10
 groups<-sort(unique(d$iso3))
 delete<-vapply(groups,function(g)coef(lm(delta_ge_pp~late_indicator,data=d[d$iso3!=g,]))[2],numeric(1))
 jack_se<-sqrt((length(groups)-1)*mean((delete-mean(delete))^2))
 checks[[paste(k,"jackknife OLS recalculation")]]<-abs(jack_se-p$se_pp)<1e-10
 matrix_country<-vcovCL(fit,cluster=d$iso3,type="HC1",cadjust=TRUE,fix=FALSE)[2,2]
 matrix_year<-vcovCL(fit,cluster=d$commitment_year,type="HC1",cadjust=TRUE,fix=FALSE)[2,2]
 matrix_two_way<-vcovCL(fit,cluster=data.frame(country=d$iso3,year=d$commitment_year),
    type="HC1",cadjust=TRUE,fix=FALSE)[2,2]
 checks[[paste(k,"independent sandwich country variance")]]<-
   abs(matrix_country-shared[dataset==k&method=="country_CR1",variance_pp2])<1e-10
 checks[[paste(k,"independent sandwich year variance")]]<-
   abs(matrix_year-shared[dataset==k&method=="year_CR1",variance_pp2])<1e-10
 checks[[paste(k,"independent sandwich two-way variance")]]<-
   abs(matrix_two_way-shared[dataset==k&method=="two_way_country_year_CR1",variance_pp2])<1e-10
 # Recreate the first 25 draws by selecting whole source histories explicitly,
 # instead of using the builder's preaggregated country sums.
 seed<-boot[dataset==k,seed];set.seed(seed)
 sampled<-matrix(sample.int(length(groups),length(groups)*4999L,replace=TRUE),nrow=length(groups))
 reconstructed<-vapply(seq_len(25L),function(j) {
   z<-do.call(rbind,lapply(groups[sampled[,j]],function(g)d[d$iso3==g,]))
   mean(z$delta_ge_pp[z$late])-mean(z$delta_ge_pp[!z$late])
 },numeric(1))
 checks[[paste(k,"explicit whole-country bootstrap draws")]]<-
   max(abs(reconstructed-draws[dataset==k&replicate<=25L,estimate_pp]))<1e-10
}
checks[["primary two-test Holm correction"]]<-
 all(abs(primary$p_holm_two_datasets-p.adjust(primary$p_two_sided,"holm"))<1e-14)
for(file in c("input_manifest.csv","code_manifest.csv","output_manifest.csv")) {
 m<-fread(file.path(out,file))
 checks[[paste(file,"hashes")]]<-all(vapply(m$path,function(p)digest(file=p,algo="sha256"),character(1))==m$sha256)
}
check_table<-data.table(check=names(checks),passed=unlist(checks,use.names=FALSE))
print(check_table)
stopifnot(all(check_table$passed))
cat(nrow(check_table),"independent inference checks passed\n")
