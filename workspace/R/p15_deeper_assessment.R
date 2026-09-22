# Fixed-data diagnostics only: no selected rate is changed.
p15_deep_boot_mean <- function(value, country, B=1999L, seed=20260908L) {
  stopifnot(length(value)==length(country),all(is.finite(value)),!anyNA(country))
  if(length(unique(country))<2L) return(data.frame(mean=if(length(value))mean(value) else NA_real_,
    low=NA_real_,high=NA_real_,n=length(value),countries=length(unique(country)),bootstrap_replicates=0L))
  sums<-tapply(value,country,sum);counts<-table(country)[names(sums)]
  set.seed(seed)
  ix<-matrix(sample.int(length(sums),length(sums)*B,replace=TRUE),nrow=length(sums))
  draws<-colSums(matrix(sums[ix],nrow=length(sums)))/
    colSums(matrix(as.numeric(counts)[ix],nrow=length(sums)))
  data.frame(mean=mean(value),low=unname(quantile(draws,.025)),high=unname(quantile(draws,.975)),
    n=length(value),countries=length(sums),bootstrap_replicates=B)
}

p15_deep_sign_disagreement <- function(selected_change, within_change, tolerance=1e-10) {
  ok<-is.finite(selected_change)&is.finite(within_change)&
    abs(selected_change)>tolerance&abs(within_change)>tolerance
  ifelse(ok,sign(selected_change)!=sign(within_change),NA)
}

p15_deep_pool_removal <- function(rates, minimum=3L) {
  stopifnot(length(rates)>=minimum,all(is.finite(rates)))
  original<-median(rates)
  removed<-vapply(seq_along(rates),function(i)median(rates[-i]),numeric(1))
  data.frame(seed_index=seq_along(rates),original_rate=original,deleted_seed_rate=rates,
    remaining_count=length(rates)-1L,minimum_still_met=length(rates)-1L>=minimum,
    arithmetic_median_after_removal=removed,change_pp=removed-original)
}

p15_deep_composition <- function(prior, current, prior_ids, current_ids) {
  stopifnot(length(prior)==length(prior_ids),length(current)==length(current_ids),
    !anyDuplicated(prior_ids),!anyDuplicated(current_ids))
  common<-intersect(prior_ids,current_ids)
  if(!length(common))stop("No common countries")
  a<-prior[match(common,prior_ids)];b<-current[match(common,current_ids)]
  data.frame(previous_n=length(prior),current_n=length(current),common_n=length(common),
    total_mean_change=mean(current)-mean(prior),common_country_change=mean(b-a),
    current_composition=mean(current)-mean(b),prior_composition=mean(a)-mean(prior))
}
