select_shadow <- function(panel,scenario_name,input_policy) {
  panel <- copy(panel)
  panel[,`:=`(shadow=NA_real_,shadow_lower=NA_real_,shadow_upper=NA_real_,shadow_complete=FALSE,shadow_status='not_supplied')]
  if(input_policy=='observed_only')return(panel)
  z <- sc[scenario==scenario_name]
  at <- match(paste(panel$iso3,panel$analysis_year),paste(z$iso3,z$analysis_year))
  allowed <- !is.na(at)&is.finite(z$shadow_notch[at])
  if(input_policy=='complete_only')allowed <- allowed & (z$scorecard_complete[at]%in%TRUE)
  panel[allowed,`:=`(shadow=z$shadow_notch[at[allowed]],shadow_lower=z$shadow_notch_lower[at[allowed]],
    shadow_upper=z$shadow_notch_upper[at[allowed]],shadow_complete=z$scorecard_complete[at[allowed]],shadow_status=z$status[at[allowed]])]
  panel
}
one <- function(t,p,de,hidden=FALSE) {
  p <- p[iso3!=t$iso3]
  stopifnot(!anyDuplicated(p$iso3))
  tn <- if(hidden)NA_real_ else t$notch
  target_shadow <- !is.finite(tn)&is.finite(t$shadow)
  if(target_shadow)tn <- t$shadow
  pn <- p$notch;ps <- !is.finite(pn)&is.finite(p$shadow);pn[ps]<-p$shadow[ps]
  tl <- tu <- tn;pl <- pu <- pn
  if(target_shadow){tl<-t$shadow_lower;tu<-t$shadow_upper}
  pl[ps]<-p$shadow_lower[ps];pu[ps]<-p$shadow_upper[ps]
  dist <- switch(de$distance,point=abs(pn-tn),
    guaranteed_interval=pmax(abs(pl-tu),abs(pu-tl)),possible_interval=pmax(pl-tu,tl-pu,0))
  rat <- is.finite(dist)&dist<=de$caliper
  inc <- p$historical_income_level==t$historical_income_level;inc[is.na(inc)]<-FALSE
  reg <- p$rating_source_region==t$rating_source_region;reg[is.na(reg)]<-FALSE
  masks <- list(inc&reg&rat,inc&rat,reg&rat,rat,inc&reg,inc,reg,rep(TRUE,nrow(p)))
  matching_notches<-rep(tn,8L)
  if(de$distance=='possible_interval' && all(is.finite(c(tl,tu)))) {
    # Pairwise possibility alone need not imply a common feasible target grade.
    # Search all interval endpoints where membership can change, maximizing donor
    # count within each rule; ties use proximity to the supplied midpoint, then
    # the lower grade position. This is an optimistic, conditional sensitivity.
    qs<-sort(unique(c(tl,tu,tn,pl-de$caliper,pu+de$caliper)))
    qs<-qs[is.finite(qs)&qs>=tl&qs<=tu]
    restrictions<-list(inc&reg,inc,reg,rep(TRUE,nrow(p)))
    for(ri in 1:4) {
      candidates<-lapply(qs,function(q)is.finite(pl)&is.finite(pu)&pl<=q+3&pu>=q-3&restrictions[[ri]])
      nc<-vapply(candidates,sum,integer(1))
      win<-order(-nc,abs(qs-tn),qs)[[1]]
      masks[[ri]]<-candidates[[win]];matching_notches[[ri]]<-qs[[win]]
    }
  }
  counts <- vapply(masks,sum,integer(1));search<-as.integer(strsplit(de$rules,';',fixed=TRUE)[[1]])
  eligible <- search[counts[search]>=3L];rule<-if(length(eligible))eligible[[1]] else NA_integer_
  use <- if(is.finite(rule))masks[[rule]] else rep(FALSE,nrow(p))
  if(is.finite(rule))stopifnot(sum(use)>=3L,!any(counts[search[seq_len(match(rule,search)-1L)]]>=3L))
  incomplete_members <- ps & !p$shadow_complete
  target_conditional <- target_shadow && !t$shadow_complete
  # Ratings only enter Rules 1–4. Rule 5 does not depend on either shadow grade.
  rating_rule <- is.finite(rule) && rule<=4
  data.table(iso3=t$iso3,analysis_year=t$analysis_year,method=de$method,scenario=de$scenario,
    input_policy=de$input_policy,design_role=de$role,distance=de$distance,mode=if(hidden)'target_rating_hidden' else 'deployment',
    rule=rule,estimate=if(sum(use)>=3L)median(p$seed_rate[use]) else NA_real_,n_peers=sum(use),
    target_notch=tn,matching_target_notch=if(is.finite(rule))matching_notches[[rule]] else tn,target_shadow_used=target_shadow,target_shadow_status=t$shadow_status,
    target_shadow_complete=if(target_shadow)t$shadow_complete else NA,
    donor_shadow_members=sum(ps[use]),incomplete_shadow_donor_members=sum(incomplete_members[use]),
    estimate_depends_on_incomplete_input=rating_rule&&(target_conditional||any(incomplete_members[use])),
    primary_members=sum(p$seed_source[use]=='primary'),ids_members=sum(p$seed_source[use]=='ids'),
    secondary_members=sum(p$seed_source[use]=='secondary'),rating_implied_members=sum(p$seed_source[use]=='moodys'),
    member_ids=paste(sort(p$iso3[use]),collapse=';'),
    member_sources=paste(paste(p$iso3[use],p$seed_source[use],sep=':'),collapse=';'),
    member_rates=paste(paste(p$iso3[use],sprintf('%.12g',p$seed_rate[use]),sep=':'),collapse=';'),
    member_rating_basis=paste(paste(p$iso3[use],ifelse(ps[use],p$shadow_status[use],'observed_or_missing'),sep=':'),collapse=';'),
    member_notches=paste(paste(p$iso3[use],sprintf('%.8g',pn[use]),sep=':'),collapse=';'),
    candidate_counts=paste(counts,collapse=';'))
}
