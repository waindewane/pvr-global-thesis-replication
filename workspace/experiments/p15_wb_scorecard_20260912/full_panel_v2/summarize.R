# Full-panel comparisons distinguish availability, eligibility, and selection.
scoped <- rbindlist(list(copy(comparison)[,scope:='All country-years'],
  copy(comparison[historical_lmic_reporting_scope==TRUE])[,scope:='LMIC country-years'],
  copy(comparison[historical_lmic_reporting_scope & selected_tier=='peer'])[,scope:='Original 778 LMIC peer selections']))
coverage_stats <- function(d) list(country_years=nrow(d),countries=uniqueN(d$iso3),
  old_peer_available=sum(d$old_peer_available),new_peer_available=sum(d$peer_available),
  both=sum(d$coverage_transition=='both'),lost=sum(d$coverage_transition=='old_only'),
  gained=sum(d$coverage_transition=='new_only'),neither=sum(d$coverage_transition=='neither'),
  new_peer_eligible=sum(d$peer_eligible_for_selection),
  old_peer_selected=sum(d$selected_tier=='peer'),new_peer_selected=sum(d$selected_peer_new),
  old_any_selected=sum(is.finite(d$selected_rate_pct)),new_any_selected=sum(is.finite(d$candidate_selected_rate_pct)))
coverage <- scoped[,coverage_stats(.SD),by=scope]
fwrite(coverage,file.path(out,'coverage_summary.csv'))
fwrite(scoped[,coverage_stats(.SD),by=.(scope,analysis_year)],file.path(out,'coverage_by_year.csv'))
tier_coverage <- rbindlist(lapply(unique(comparison$selected_tier),function(tier) {
  d <- comparison[selected_tier==tier]
  cbind(data.table(selected_tier=tier),as.data.table(coverage_stats(d)))
}))
fwrite(tier_coverage,file.path(out,'coverage_by_current_tier.csv'))
fwrite(comparison[,coverage_stats(.SD),by=historical_income_level],file.path(out,'coverage_by_income.csv'))
pair_stats <- function(d) list(n=nrow(d),countries=uniqueN(d$iso3),
  old_mean_pct=mean(d$old_peer_rate_pct),new_mean_pct=mean(d$new_peer_rate_pct),
  mean_change_pp=mean(d$rate_change_pp),median_change_pp=median(d$rate_change_pp),
  mean_absolute_change_pp=mean(abs(d$rate_change_pp)),
  p10_change_pp=as.numeric(quantile(d$rate_change_pp,.1)),
  p90_change_pp=as.numeric(quantile(d$rate_change_pp,.9)),
  min_change_pp=min(d$rate_change_pp),max_change_pp=max(d$rate_change_pp),
  lower=sum(d$rate_change_pp < -1e-10),unchanged=sum(abs(d$rate_change_pp)<=1e-10),
  higher=sum(d$rate_change_pp > 1e-10),correlation=cor(d$old_peer_rate_pct,d$new_peer_rate_pct))
pair <- scoped[coverage_transition=='both']
paired_summary <- pair[,pair_stats(.SD),by=scope]
fwrite(paired_summary,file.path(out,'paired_rate_summary.csv'))
paired_year <- pair[,pair_stats(.SD),by=.(scope,analysis_year)]
fwrite(paired_year,file.path(out,'paired_rate_changes_by_year.csv'))
fwrite(comparison[coverage_transition=='both',pair_stats(.SD),by=selected_tier],file.path(out,'paired_rate_changes_by_current_tier.csv'))
fwrite(comparison[coverage_transition=='both',pair_stats(.SD),by=historical_income_level],file.path(out,'paired_rate_changes_by_income.csv'))
big <- copy(comparison[coverage_transition=='both']);big[,absolute_change_pp:=abs(rate_change_pp)];setorder(big,-absolute_change_pp)
fwrite(big,file.path(out,'paired_cases_by_absolute_change.csv'))
rules <- rbindlist(lapply(unique(scoped$scope),function(s) {
  d <- scoped[scope==s]
  rbind(data.table(scope=s,version='old',rule=match(d$old_peer_rule,rule_map$current_label),available=d$old_peer_available),
    data.table(scope=s,version='new',rule=d$new_peer_rule,available=d$peer_available))
}))
rules <- merge(CJ(scope=unique(scoped$scope),version=c('old','new'),rule=1:8),
  rules[available==TRUE,.(country_years=.N),by=.(scope,version,rule)],all.x=TRUE,by=c('scope','version','rule'))
rules[is.na(country_years),country_years:=0L]
rules <- merge(rules,rule_map,by='rule');setorder(rules,scope,rule,version)
fwrite(rules,file.path(out,'rule_counts.csv'))
source_stats <- comparison[peer_available==TRUE,.(groups=.N,groups_with_rating_implied=sum(rating_implied_members>0),
  groups_majority_rating_implied=sum(rating_implied_members>new_peer_count/2),
  groups_all_rating_implied=sum(rating_implied_members==new_peer_count),
  groups_no_primary=sum(primary_members==0),median_group_size=as.numeric(median(new_peer_count))),
  by=.(historical_lmic_reporting_scope,selected_tier)]
fwrite(source_stats,file.path(out,'donor_composition_summary.csv'))
fwrite(dep[,.(rows=.N,available=sum(peer_available),eligible=sum(peer_eligible_for_selection),
  selected=sum(selected_peer_new)),by=.(method,historical_lmic_reporting_scope,selected_tier)],
  file.path(out,'method_scope_comparison.csv'))
# Rate validation remains distinct from ordinary peer values for rated targets.
val <- p[mode=='target_rating_hidden']
fwrite(val,file.path(out,'hidden_rating_validation_cases.csv'))
fwrite(val[peer_available==TRUE,.(n=.N,countries=uniqueN(iso3),mae_pp=mean(abs(estimate-actual)),
  bias_pp=mean(estimate-actual),rmse_pp=sqrt(mean((estimate-actual)^2))),by=.(method,historical_income_level)],
  file.path(out,'hidden_rating_validation_by_income.csv'))
fwrite(val[peer_available==TRUE,.(n=.N,countries=uniqueN(iso3),mae_pp=mean(abs(estimate-actual)),
  bias_pp=mean(estimate-actual),rmse_pp=sqrt(mean((estimate-actual)^2))),by=method],file.path(out,'hidden_rating_validation_summary.csv'))
validation_pairs <- merge(val[peer_available==TRUE],
  val[method=='current' & peer_available,.(iso3,analysis_year,baseline_estimate=estimate)],by=c('iso3','analysis_year'))
fwrite(validation_pairs[,.(n=.N,countries=uniqueN(iso3),
  baseline_mae_pp=mean(abs(baseline_estimate-actual)),candidate_mae_pp=mean(abs(estimate-actual))),
  by=method],file.path(out,'hidden_rating_validation_same_cases.csv'))
# Year-to-year comparisons use the same countries for both methods and dates.
yr <- merge(scoped[coverage_transition=='both',.(scope,iso3,analysis_year,
  old_now=old_peer_rate_pct,new_now=new_peer_rate_pct)],
  scoped[coverage_transition=='both',.(scope,iso3,analysis_year=analysis_year+1L,
  old_prev=old_peer_rate_pct,new_prev=new_peer_rate_pct)],by=c('scope','iso3','analysis_year'))
yr[,`:=`(old_change=old_now-old_prev,new_change=new_now-new_prev)]
fwrite(yr,file.path(out,'paired_annual_changes.csv'))
annual <- yr[,.(n=.N,old_mean_change_pp=mean(old_change),new_mean_change_pp=mean(new_change),
  individual_direction_reversals=sum(old_change*new_change < -1e-10)),by=.(scope,analysis_year)]
annual[,mean_direction_reversed:=old_mean_change_pp*new_mean_change_pp < -1e-10]
setorder(annual,scope,analysis_year)
fwrite(annual,file.path(out,'paired_annual_change_summary.csv'))
# Compact static diagnostic; paired means avoid old/new sample-composition bias.
png(file.path(out,'full_panel_comparison.png'),width=1600,height=750,res=150)
par(mfrow=c(1,2),mar=c(4.5,4.3,3,1),bty='l',las=1)
q <- comparison[coverage_transition=='both']
plot(q$old_peer_rate_pct,q$new_peer_rate_pct,pch=16,cex=.45,col=adjustcolor('#245A81',alpha.f=.2),
  xlab='Previous peer rate (%)',ylab='Revised peer rate (%)',main='Matched country-years')
abline(0,1,lty=2,col='#888888')
y <- paired_year[scope=='All country-years'];setorder(y,analysis_year)
plot(y$analysis_year,y$old_mean_pct,type='o',pch=16,col='#777777',
  ylim=range(c(y$old_mean_pct,y$new_mean_pct)),xlab='Year',ylab='Mean peer rate (%)',main='Same cases within each year')
lines(y$analysis_year,y$new_mean_pct,type='o',pch=16,col='#245A81')
legend('topleft',legend=c('Previous','Revised'),col=c('#777777','#245A81'),lty=1,pch=16,bty='n')
dev.off()
print(coverage);print(paired_summary);print(annual[scope=='All country-years'])
