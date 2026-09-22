# Focused correctness tests for substantive boundary cases.
source('scripts/p15/activate_p15_environment.R')
library(data.table)
expr<-parse('experiments/p15_wb_scorecard_20260912/constructed_v1/build_peer_candidate.R')
for(e in expr)if(is.call(e)&&as.character(e[[1]])=='<-'&&is.symbol(e[[2]])&&as.character(e[[2]])%in%c('one','select_shadow'))eval(e)
t<-data.table(iso3='AAA',analysis_year=2024L,notch=14,shadow=8,shadow_lower=7,shadow_upper=9,
  shadow_complete=TRUE,shadow_status='adapted_fixture',historical_income_level='Low income',rating_source_region='Region A')
p<-data.table(iso3=c('AAA','BBB','CCC','DDD'),analysis_year=2024L,notch=14,
  shadow=14,shadow_lower=14,shadow_upper=14,shadow_complete=TRUE,shadow_status='adapted_fixture',
  historical_income_level='Low income',rating_source_region='Region A',seed_rate=c(99,6,7,8),
  seed_source=c('primary','primary','ids','secondary'))
d<-data.table(method='unit_fixture',scenario='unit_fixture_not_empirical',input_policy='complete_only',
  role='unit_test',caliper=3,rules='1;2;3;5',distance='point')
z<-one(t,p,d)
stopifnot(z$rule==1L,z$estimate==7,z$n_peers==3L,!z$target_shadow_used,z$target_notch==14,
  z$primary_members==1L,z$ids_members==1L,z$secondary_members==1L,z$member_ids=='BBB;CCC;DDD')
# Two foreign donors cannot qualify, even with the target as a fourth raw row.
stopifnot(!is.finite(one(t,p[iso3!='DDD'],d)$estimate))
# Hidden grade uses scorecard, so the rating pools fail and Rule 5 is used.
z<-one(t,p,d,hidden=TRUE);stopifnot(z$rule==5L,z$target_notch==8,z$target_shadow_used)
# Rule 4 can qualify only in the explicitly permissive design.
t2<-copy(t);t2[,`:=`(historical_income_level='Lower middle income',rating_source_region='Region B')]
stopifnot(!is.finite(one(t2,p,d)$estimate))
d4<-copy(d);d4[,rules:='1;2;3;4;5'];stopifnot(one(t2,p,d4)$rule==4L)
# Incomplete inputs are flagged only when their rating enters the selected rule.
p2<-copy(p);p2[,`:=`(notch=NA_real_,shadow_complete=FALSE)]
z<-one(t,p2,d);stopifnot(z$estimate_depends_on_incomplete_input)
z<-one(t,p2,d,hidden=TRUE);stopifnot(z$rule==5L,!z$estimate_depends_on_incomplete_input)
# Wide intervals can pass midpoint but fail guaranteed proximity.
t3<-copy(t);t3[,`:=`(notch=NA_real_,shadow=14,shadow_lower=11,shadow_upper=17)]
p3<-copy(p);p3[,rating_source_region:='Region B']
stopifnot(one(t3,p3,d)$rule==2L)
di<-copy(d);di[,distance:='guaranteed_interval'];stopifnot(one(t3,p3,di)$rule==2L)
t3[,shadow_lower:=10];stopifnot(!is.finite(one(t3,p3,di)$estimate))
di[,distance:='possible_interval'];stopifnot(one(t3,p3,di)$rule==2L)
# Pairwise possible matches must share one feasible target grade.
t4<-copy(t3);t4[,`:=`(shadow=11,shadow_lower=1,shadow_upper=21,historical_income_level='Other',rating_source_region='Other')]
p4<-copy(p);p4[,notch:=c(14,1,11,21)]
d4[,distance:='possible_interval']
stopifnot(!is.finite(one(t4,p4,d4)$estimate))
# Complete-only filtering never activates a merely finite, incomplete grade.
sc<-data.table(iso3=c('AAA','BBB'),analysis_year=2024L,scenario='fixture',shadow_notch=c(14,13),
  shadow_notch_lower=c(12,11),shadow_notch_upper=c(16,15),scorecard_complete=c(TRUE,FALSE),status='adapted_fixture')
xx<-data.table(iso3=c('AAA','BBB','CCC'),analysis_year=2024L)
a<-select_shadow(xx,'fixture','complete_only');b<-select_shadow(xx,'fixture','conditional_points')
stopifnot(is.finite(a$shadow[1]),!is.finite(a$shadow[2]),is.finite(b$shadow[2]),!is.finite(b$shadow[3]))
fwrite(data.table(check=c('actual_grade_preferred','three_sources_one_country_each','target_removed_before_count',
  'hidden_target_grade_filled','rating_only_rule_optional','conditional_grade_dependence_tracked',
  'interval_proximity_boundaries','complete_only_input_gate'),passed=TRUE),
  'experiments/p15_wb_scorecard_20260912/constructed_v1/matching_checks.csv')
cat('Eight boundary-condition test groups passed.\n')

# Rating-implied member counts are explicit; they do not increase distinct issuers.
p5<-copy(p);p5[iso3=='DDD',seed_source:='moodys']
z<-one(t,p5,d);stopifnot(z$rating_implied_members==1L,z$n_peers==3L,z$estimate==7)
# Wider caliper activates an otherwise failed rating match, without changing rule5.
p6<-copy(p);p6[,notch:=18];t6<-copy(t);t6[,rating_source_region:='Elsewhere']
stopifnot(!is.finite(one(t6,p6,d)$estimate));d6<-copy(d);d6[,caliper:=5]
stopifnot(one(t6,p6,d6)$rule==2L)
cat('Rating-implied source and caliper5 checks passed.\n')
