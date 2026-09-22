# Diagnostic only: compare fixed peer designs without selecting a new rule.
p15_geography_pool <- function(seeds,target,method) {
  minimum <- 3L
  if(method=="current")return(p15_peer_similarity_pool(seeds,target,minimum))
  same_income <- !is.na(seeds$historical_income_level) &
    seeds$historical_income_level==target$historical_income_level[[1]]
  same_region <- !is.na(seeds$rating_source_region) & nzchar(seeds$rating_source_region) &
    seeds$rating_source_region==target$rating_source_region[[1]]
  tn <- p15_rating_notch_number(target$moodys_rating_normalized[[1]])
  sn <- p15_rating_notch_number(seeds$moodys_rating_normalized)
  close <- is.finite(sn)&is.finite(tn)&abs(sn-tn)<=3L
  rules <- switch(method,
    no_geography=list(same_income_rating3=same_income&close,rating3=close,
      same_income=same_income,global=rep(TRUE,nrow(seeds))),
    region_only=list(same_region=same_region),
    worldwide_only=list(global=rep(TRUE,nrow(seeds))),
    stop("Unknown diagnostic method"))
  for(rule in names(rules)) {
    pool <- seeds[rules[[rule]] %in% TRUE,,drop=FALSE]
    if(nrow(pool)>=minimum)return(list(pool=pool,rule=paste0(rule,"_min3")))
  }
  list(pool=seeds[FALSE,,drop=FALSE],rule="insufficient_peers")
}

p15_geography_predictions <- function(panel,mask_target_rating=FALSE) {
  stopifnot(!anyDuplicated(panel[c("analysis_year","iso3")]))
  methods <- c("current","no_geography","region_only","worldwide_only")
  result <- vector("list",nrow(panel)*length(methods)); k<-0L
  for(i in seq_len(nrow(panel))) {
    target <- panel[i,,drop=FALSE]
    # Target removed BEFORE any match, count, or median calculation.
    seeds <- panel[panel$analysis_year==target$analysis_year & panel$iso3!=target$iso3 &
      panel$primary_eligible,,drop=FALSE]
    if(mask_target_rating)target$moodys_rating_normalized <- NA_character_
    for(method in methods) {
      pick<-p15_geography_pool(seeds,target,method);pool<-pick$pool
      k<-k+1L
      result[[k]]<-data.frame(analysis_year=target$analysis_year,iso3=target$iso3,
        method=method,information=if(mask_target_rating)"target_rating_hidden" else "available_rating",
        estimate=if(nrow(pool)>=3)median(pool$primary_usd_market_rate_pct) else NA_real_,
        seed_count=nrow(pool),pool_rule=pick$rule,
        seed_ids=paste(sort(pool$iso3),collapse=";"),
        same_region_share=if(nrow(pool))mean(pool$rating_source_region==target$rating_source_region,na.rm=TRUE) else NA_real_)
    }
  }
  do.call(rbind,result)
}

p15_geography_metrics <- function(x,truth="actual") {
  e<-x$estimate-x[[truth]]
  data.frame(n=length(e),countries=length(unique(x$iso3)),years=length(unique(x$analysis_year)),
    bias=mean(e),mae=mean(abs(e)),median_ae=median(abs(e)),rmse=sqrt(mean(e^2)),
    within_1pp=mean(abs(e)<=1),within_2pp=mean(abs(e)<=2))
}

p15_geography_paired <- function(x,a,b) {
  # Same target observations on both sides. Positive benefit means A is closer.
  aa<-x[x$method==a & is.finite(x$estimate),]
  bb<-x[x$method==b & is.finite(x$estimate),]
  keys<-c("iso3","analysis_year","information")
  z<-merge(aa,bb,by=keys,suffixes=c("_a","_b"))
  stopifnot(!anyDuplicated(z[keys]),all(z$actual_a==z$actual_b))
  z$benefit<-abs(z$estimate_b-z$actual_b)-abs(z$estimate_a-z$actual_a)
  z$method_a<-a;z$method_b<-b
  z
}

p15_geography_inference <- function(x) {
  # Conditional paired-score uncertainty. Not a fresh independent validation set.
  ncountry<-length(unique(x$iso3));nyear<-length(unique(x$analysis_year))
  se<-lower<-upper<-p<-NA_real_
  if(nrow(x)>=3 && ncountry>=5 && nyear>=5 && stats::sd(x$benefit)>0) {
    fit<-stats::lm(benefit~1,data=x)
    v<-as.numeric(sandwich::vcovCL(fit,cluster=list(x$iso3,x$analysis_year),type="HC1",multi0=TRUE)[1,1])
    if(is.finite(v)&&v>0) {
      se<-sqrt(v);df<-min(ncountry,nyear)-1L
      lower<-mean(x$benefit)-stats::qt(.975,df)*se
      upper<-mean(x$benefit)+stats::qt(.975,df)*se
      p<-2*stats::pt(abs(mean(x$benefit)/se),df,lower.tail=FALSE)
    }
  }
  data.frame(n=nrow(x),countries=ncountry,years=nyear,mae_a=mean(abs(x$estimate_a-x$actual_a)),
    mae_b=mean(abs(x$estimate_b-x$actual_b)),benefit=mean(x$benefit),
    country_balanced_benefit=mean(tapply(x$benefit,x$iso3,mean)),
    share_a_closer=mean(x$benefit>1e-10),share_tied=mean(abs(x$benefit)<=1e-10),
    se=se,ci_lower=lower,ci_upper=upper,p_two_sided=p)
}
