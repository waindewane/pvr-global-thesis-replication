# Rebuild the bounded scorecard from frozen v1 inputs. No source refresh.
source(file.path(v2,'snapshot/score_model.R'))
u <- fread(file.path(v1,'inputs.csv'))
mat <- read_exact_matrices(v1)
bounded_scores <- function(u, conservative_interest=TRUE) {
  n <- nrow(u)
  ei <- data.frame(growth=indicator(u$growth_avg7,'growth'),
    growth_sd=indicator(u$growth_sd10,'growth_sd'),gci=indicator(u$gci_carry_max2,'gci'),
    gdp=indicator(u$nominal_gdp_usd_bn,'gdp'),gdppc=indicator(u$gdp_pc_ppp,'gdppc'))
  ew <- matrix(rep(c(.25,.125,.125,.25,.25),each=n),ncol=5)
  elo <- ehi <- ei
  elo$gci[is.na(elo$gci)] <- 1L; ehi$gci[is.na(ehi$gci)] <- 15L
  f1lo <- weighted_factor(elo,ew); f1hi <- weighted_factor(ehi,ew)
  inst <- data.frame(ge=indicator(u$government_effectiveness,'ge'),
    rl=indicator(u$rule_law,'rl'),cc=indicator(u$corruption,'cc'),
    level=inflation_index(u$inflation_avg7),vol=indicator(u$inflation_sd10,'inflation_sd'))
  iw <- matrix(rep(c(.375,.1875,.1875,.125,.125),each=n),ncol=5)
  f2lo <- weighted_factor(inst,iw); f2hi <- pmin(15L,f2lo+3L)
  fi <- data.frame(debt_gdp=indicator(u$debt_gdp,'debt_gdp'),
    debt_revenue=indicator(u$debt_revenue,'debt_revenue'),
    interest_gdp=indicator(u$interest_gdp,'interest_gdp'),
    interest_revenue=indicator(u$interest_revenue,'interest_revenue'))
  if(conservative_interest) {
    gross <- u$interest_source %in% 'GFS_general_government_revised_gross'
    fi$interest_gdp[!gross] <- NA_integer_; fi$interest_revenue[!gross] <- NA_integer_
  }
  fw <- matrix(.25,n,4)
  exception <- u$concessional_weight_exception %in% TRUE
  fw[exception,] <- matrix(rep(c(.5,.5,0,0),each=sum(exception)),ncol=4)
  reserve <- u$iso3 %in% c('JPN','CHE','GBR','USA','DEU','FRA')
  fw[reserve,] <- matrix(rep(c(.05,.05,.45,.45),each=sum(reserve)),ncol=4)
  flo <- fhi <- fi
  for(k in c('interest_gdp','interest_revenue')) {
    flo[[k]][is.na(flo[[k]])] <- 1L; fhi[[k]][is.na(fhi[[k]])] <- 15L
  }
  f3lo <- pmin(15L,weighted_factor(flo,fw)+u$debt_trend_penalty)
  f3hi <- pmin(15L,weighted_factor(fhi,fw)+pmin(6L,u$debt_trend_penalty+ifelse(u$debt_gdp<25,3L,6L)))
  lower <- aggregate_factors(f1lo,f2lo,f3lo,mat)
  upper <- aggregate_factors(f1hi,f2hi,f3hi,mat)
  data.table(iso3=u$iso3,analysis_year=u$analysis_year,scenario='wgi_bounded',
    shadow_notch=(lower+upper)/2,shadow_notch_lower=lower,shadow_notch_upper=upper,
    scorecard_complete=FALSE,status=if(conservative_interest)
      'conditional_score_interval_gross_interest_uncertainty' else 'conditional_score_interval_v1_net_proxy')
}
old_scores <- bounded_scores(u,FALSE)
saved_scores <- fread(file.path(v1,'scorecard_country_year.csv'))[scenario=='wgi_bounded']
setorder(saved_scores,iso3,analysis_year);setorder(old_scores,iso3,analysis_year)
for(nm in c('iso3','analysis_year','shadow_notch','shadow_notch_lower','shadow_notch_upper'))
  stopifnot(isTRUE(all.equal(old_scores[[nm]],saved_scores[[nm]],tolerance=1e-12)))
new_scores <- bounded_scores(u,TRUE);setorder(new_scores,iso3,analysis_year)
stopifnot(nrow(new_scores)==2743L,
  identical(is.finite(new_scores$shadow_notch),is.finite(old_scores$shadow_notch)))
ok <- is.finite(old_scores$shadow_notch)
stopifnot(all(new_scores$shadow_notch_lower[ok]<=old_scores$shadow_notch_lower[ok]),
  all(new_scores$shadow_notch_upper[ok]>=old_scores$shadow_notch_upper[ok]))
fwrite(new_scores,file.path(v2,'results/scorecard_country_year.csv'))
score_changes <- merge(old_scores[,.(iso3,analysis_year,old_lower=shadow_notch_lower,old_upper=shadow_notch_upper)],
  new_scores[,.(iso3,analysis_year,new_lower=shadow_notch_lower,new_upper=shadow_notch_upper)],by=c('iso3','analysis_year'))
fwrite(score_changes,file.path(v2,'results/score_interval_changes.csv'))
