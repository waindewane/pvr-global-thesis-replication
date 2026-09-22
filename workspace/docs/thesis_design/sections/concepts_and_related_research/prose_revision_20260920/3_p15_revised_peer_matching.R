p15_revised_peer_select_shadow <- function(panel, scorecard, scenario_name='wgi_bounded', input_policy='conditional_points') {
  panel <- data.table::copy(panel)
  panel[,`:=`(shadow=NA_real_,shadow_lower=NA_real_,shadow_upper=NA_real_,shadow_complete=FALSE,shadow_status='not_supplied')]
  if(input_policy=='observed_only')return(panel)
  z <- scorecard[scenario==scenario_name]
  at <- match(paste(panel$iso3,panel$analysis_year),paste(z$iso3,z$analysis_year))
  allowed <- !is.na(at)&is.finite(z$shadow_notch[at])
  if(input_policy=='complete_only')allowed <- allowed & (z$scorecard_complete[at]%in%TRUE)
  panel[allowed,`:=`(shadow=z$shadow_notch[at[allowed]],shadow_lower=z$shadow_notch_lower[at[allowed]],
    shadow_upper=z$shadow_notch_upper[at[allowed]],shadow_complete=z$scorecard_complete[at[allowed]],shadow_status=z$status[at[allowed]])]
  panel
}
p15_revised_peer_match_one <- function(t,p,de,hidden=FALSE) {
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
  data.table::data.table(iso3=t$iso3,analysis_year=t$analysis_year,method=de$method,scenario=de$scenario,
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

p15_revised_peer_rule_mapping <- function() {
  data.table::data.table(rule=1:8,
    current_label=c('same_income_region_rating3_min3','same_income_rating3_min3',
      'same_region_rating3_min3','rating3_min3','same_income_region_min3',
      'same_income_min3','same_region_min3','global_min3'),
    meaning=c('Same income, region, and rating within 3 notches',
      'Same income and rating within 3 notches','Same region and rating within 3 notches',
      'Rating within 3 notches','Same income and region','Same income','Same region','Any eligible donor'),
    permitted_in_revised_method=c(rep(TRUE,5),rep(FALSE,3)))
}

# The chosen donor is the first eligible source in P > IDS > secondary > Moody's.
# Ordinary-cost secondary holds are explicit, versioned inherited source decisions.
p15_revised_peer_donor_seeds <- function(core, config) {
  eligible <- data.table::as.data.table(p15_analysis_eligibility(as.data.frame(core)))
  z <- eligible[tier %in% config$donor_priority & eligible %in% TRUE & is.finite(rate_pct)]
  held <- config$secondary_ordinary_holds
  for(i in seq_len(nrow(held)))
    z <- z[!(tier=='secondary' & iso3==held$iso3[i] & analysis_year %in% unlist(held$years[i]))]
  z[, priority := match(tier, config$donor_priority)]
  data.table::setorder(z,iso3,analysis_year,priority)
  seeds <- z[, .(sources='PISR',seed_source=tier[[1L]],seed_rate=rate_pct[[1L]]),by=.(iso3,analysis_year)]
  stopifnot(!anyDuplicated(seeds[,.(iso3,analysis_year)]),all(is.finite(seeds$seed_rate)),all(seeds$seed_rate > -100))
  seeds
}

# Every target country-year is computed. Hidden-rating diagnostics are additional
# observations and never enter the selected dataset.
p15_revised_peer_compute <- function(core, selected, context, scorecard, config, hidden_validation=TRUE) {
  key <- c('iso3','analysis_year')
  x <- merge(core[,.(iso3,analysis_year,country,historical_income_level,historical_lmic_reporting_scope,
      ordinary_fallback_selection_permitted)],
    selected[,.(iso3,analysis_year,selected_tier,selected_rate_pct,peer_pool_rule)],by=key)
  x <- merge(x,context[,.(iso3,analysis_year,rating_source_region,moodys_rating_normalized)],by=key)
  stopifnot(nrow(x)==nrow(core),!anyDuplicated(x[,..key]))
  x[,notch:=match(moodys_rating_normalized,p15_peer_rating_labels)]
  core_at<-match(paste(x$iso3,x$analysis_year),paste(core$iso3,core$analysis_year))
  stopifnot(identical(x$notch,match(core$rating_moodys_rating[core_at],p15_peer_rating_labels)))
  seeds <- p15_revised_peer_donor_seeds(core,config)
  x <- merge(x,seeds[seed_source=='primary',.(iso3,analysis_year,actual=seed_rate)],by=key,all.x=TRUE)
  xx <- p15_revised_peer_select_shadow(x,scorecard,config$scenario,config$input_policy)
  de <- data.table::data.table(method='preferred',sources='PISR',scenario=config$scenario,input_policy=config$input_policy,
    rules=paste(config$rules,collapse=';'),distance=config$distance,role='owner_authorized_private_working_method',caliper=config$caliper)
  predictions<-list();k<-0L
  for(y in sort(unique(xx$analysis_year))) {
    pool<-merge(seeds[analysis_year==y,.(iso3,analysis_year,seed_rate,seed_source)],xx[analysis_year==y],by=key)
    targets<-xx[analysis_year==y]
    for(i in seq_len(nrow(targets))) {
      t<-targets[i];k<-k+1L;predictions[[k]]<-p15_revised_peer_match_one(t,pool,de,FALSE)
      if(hidden_validation && isTRUE(t$historical_lmic_reporting_scope) && is.finite(t$actual)) {
        k<-k+1L;predictions[[k]]<-p15_revised_peer_match_one(t,pool,de,TRUE)
      }
    }
  }
  pred<-data.table::rbindlist(predictions)
  pred<-merge(pred,x[,.(iso3,analysis_year,country,historical_income_level,historical_lmic_reporting_scope,
    selected_tier,selected_rate_pct,peer_pool_rule,ordinary_fallback_selection_permitted,actual)],by=key)
  pred[,peer_available:=is.finite(estimate)]
  pred[,peer_eligible_for_selection:=peer_available & !(ordinary_fallback_selection_permitted %in% FALSE)]
  pred[,selected_peer_new:=mode=='deployment' & selected_tier %in% c('peer','no_eligible_rate') & peer_eligible_for_selection]
  list(predictions=pred,seeds=seeds,design=de,matching_context=xx)
}
