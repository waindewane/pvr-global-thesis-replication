# Conditional quantitative adaptation; analyst judgments are never certified.
# Exact numerical tables: PBC_1151027 (27 November 2018), pp4–6,9,14,18.
# World Bank WPS9649 pp5–6/21 supplies seven-year windows and event risk M.
strength_labels <- c('VH+','VH','VH-','H+','H','H-','M+','M','M-','L+','L','L-','VL+','VL','VL-')
rating_labels <- c('Aaa','Aa1','Aa2','Aa3','A1','A2','A3','Baa1','Baa2','Baa3','Ba1','Ba2','Ba3','B1','B2','B3','Caa1','Caa2','Caa3','Ca','C')
midpoints <- c(92.5,82.5,77.5,72.5,67.5,62.5,57.5,52.5,47.5,42.5,37.5,32.5,27.5,22.5,10.5)
# Every cutoff includes its lower endpoint. Indicator strength order: 1 best.
low_good <- function(x,cuts) {z<-findInterval(x,cuts)+1L;z[!is.finite(x)]<-NA_integer_;z}
high_good <- function(x,cuts) {z<-15L-findInterval(x,sort(cuts));z[!is.finite(x)]<-NA_integer_;z}
factor_index <- function(z) high_good(z,c(20,25,30,35,40,45,50,55,60,65,70,75,80,85))
inflation_index <- function(x) {
 z<-rep(NA_integer_,length(x));ok<-is.finite(x)
 z[ok & x>=1.3 & x<2.5]<-1L
 lo<-c(1.2,1.1,1,.9,.8,.7,.6,.5,.4,.3,.2,.1,0)
 hi<-c(2.5,3,3.5,4,5,6,8,10,12.5,15,17.5,20,22.5)
 for(j in 1:13){z[ok & x>=lo[j] & x<if(j==1)1.3 else lo[j-1]]<-j+1L
   z[ok & x>=hi[j] & x<if(j==13)25 else hi[j+1]]<-j+1L}
 z[ok & (x<0|x>=25)]<-15L;z
}
cutoffs <- list(
 growth=list(direction='high',cuts=c(4.5,4,3.5,3,2.75,2.5,2.25,2,1.75,1.5,1.25,1,.75,.5)),
 growth_sd=list(direction='low',cuts=c(1.44,1.66,1.76,1.96,2.11,2.20,2.29,2.49,2.64,2.85,3.14,3.36,3.72,3.95)),
 gci=list(direction='high',cuts=c(4.98,4.61,4.52,4.45,4.39,4.31,4.26,4.22,4.1,4.03,3.95,3.9,3.84,3.75)),
 gdp=list(direction='high',cuts=c(1000,500,400,300,250,200,175,150,125,100,75,50,25,10)),
 gdppc=list(direction='high',cuts=c(35175,30130,25918,24045,20402,18001,16297,13587,11863,10656,8577,7708,5919,4320)),
 inflation_sd=list(direction='low',cuts=c(1.2,1.4,1.7,2,2.1,2.5,2.6,2.7,3.1,3.4,3.6,3.8,4.5,5.6)),
 debt_gdp=list(direction='low',cuts=c(30,35,40,45,50,55,60,65,70,80,90,100,120,140)),
 debt_revenue=list(direction='low',cuts=c(120,140,160,180,200,220,240,260,280,320,360,400,480,560)),
 interest_gdp=list(direction='low',cuts=c(1.5,1.75,2,2.25,2.5,2.75,3,3.25,3.5,4,4.5,5,6,7)),
 interest_revenue=list(direction='low',cuts=c(6,7,8,9,10,11,12,13,14,16,18,20,24,28)),
 ge=list(direction='high',cuts=c(1.14,1.01,.85,.48,.34,.25,.11,-.01,-.10,-.17,-.35,-.41,-.50,-.72)),
 rl=list(direction='high',cuts=c(.98,.81,.64,.48,.26,.06,-.08,-.15,-.29,-.35,-.45,-.57,-.71,-.82)),
 cc=list(direction='high',cuts=c(1.03,.82,.56,.32,.13,-.06,-.19,-.29,-.39,-.44,-.58,-.64,-.79,-.91)))
indicator <- function(x,name){c<-cutoffs[[name]];if(c$direction=='high')high_good(x,c$cuts) else low_good(x,c$cuts)}
weighted_factor <- function(indices,weights) {
 m<-matrix(midpoints[as.matrix(indices)],nrow=nrow(indices));w<-as.matrix(weights)
 stopifnot(identical(dim(m),dim(w)),all(abs(rowSums(w)-1)<1e-10),all(w>=0))
 m[w==0]<-0;factor_index(rowSums(m*w))
}
read_exact_matrices <- function(base) {
 lapply(setNames(c('economic_resiliency','government_financial_strength','rating_midpoint'),
                c('economic_resiliency','government_financial_strength','rating_midpoint')),function(nm){
  d<-read.csv(file.path(base,paste0(nm,'.csv')),check.names=FALSE);m<-as.matrix(d[-1]);rownames(m)<-d[[1]]
  stopifnot(nrow(m)==15,ncol(m)==15,identical(colnames(m),strength_labels));m})
}
aggregate_factors <- function(f1,f2,f3,matrices) {
 n<-max(length(f1),length(f2),length(f3));f1<-rep_len(f1,n);f2<-rep_len(f2,n);f3<-rep_len(f3,n)
 er<-matrices$economic_resiliency[cbind(strength_labels[f2],strength_labels[f1])]
 gfs<-matrices$government_financial_strength[cbind(er,strength_labels[f3])]
 match(matrices$rating_midpoint[cbind(rep('M',n),gfs)],rating_labels)
}
