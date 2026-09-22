from pathlib import Path
b=Path(__file__).resolve().parent
s=(b.parent/'build_peer_candidate.R').read_text()
needle="x <- merge(x,seeds[sources=='P',.(iso3,analysis_year,actual=seed_rate)],by=key,all.x=TRUE)"
add="""# Owner-authorized donor-source sensitivity: existing usable Moody's-implied
# rates only. Shadow grades do not manufacture new donor rates.
ord<-c('primary','ids','secondary','moodys')
z<-copy(seed_ref[ordinary_cost_tier_usable%in%TRUE & tier%in%ord & is.finite(benchmark_tier_rate_pct)])
z[,priority:=match(tier,ord)];setorder(z,iso3,analysis_year,priority)
extra<-z[,.(sources='PISR',seed_source=first(tier),seed_rate=first(benchmark_tier_rate_pct)),by=.(iso3,analysis_year)]
seeds<-rbind(seeds,extra,fill=TRUE)
fwrite(seeds,file.path(out,'all_donor_seeds.csv'))
"""
assert needle in s;s=s.replace(needle,add+needle)
a=s.index('for(scenario_name in unique(sc$scenario))');z=s.index('stopifnot(!anyDuplicated(designs$method))',a)
s=s[:a]+"""designs[,caliper:=3]
for(src in c('PIS','PISR'))for(rs in c('1;2;3;5','1;2;3;4;5','1;2;3;4;5;6;7')) {
 nm<-paste(src,if(rs=='1;2;3;5')'strict' else if(rs=='1;2;3;4;5')'stop5' else 'stop7',sep='_')
 if(!nm%in%designs$method)designs<-rbind(designs,data.table(method=nm,sources=src,scenario='observed_only',
 input_policy='observed_only',rules=rs,distance='point',role='observed_donor_or_matching_sensitivity',caliper=3))
}
for(sn in unique(sc$scenario))for(src in c('PIS','PISR'))for(rs in c('1;2;3;5','1;2;3;4;5')) {
 for(dist in if(sn=='wgi_bounded')c('guaranteed_interval','possible_interval') else 'point'){
 nm<-paste(src,sn,if(rs=='1;2;3;5')'strict' else 'stop5',dist,sep='__')
 designs<-rbind(designs,data.table(method=nm,sources=src,scenario=sn,input_policy='conditional_points',rules=rs,
 distance=dist,role=if(dist=='possible_interval')'optimistic_bound_not_deployable' else 'conditional_scorecard_experiment',caliper=3))
 }
}
for(src in c('PIS','PISR'))designs<-rbind(designs,data.table(method=paste0(src,'__wgi_legacy_hold__strict__caliper5'),
 sources=src,scenario='wgi_legacy_hold',input_policy='conditional_points',rules='1;2;3;5',distance='point',
 role='wider_rating_match_sensitivity',caliper=5))
"""+s[z:]
s=s.replace('dist<=3','dist<=de$caliper').replace('pl-3,pu+3','pl-de$caliper,pu+de$caliper').replace('pl-3','pl-de$caliper').replace('pu+3','pu+de$caliper').replace('pl-3','pl-de$caliper')
s=s.replace('pl-tu,tl-pu,0','pl-tu,tl-pu,0').replace('pl-3','pl-de$caliper').replace('pl-3','pl-de$caliper')
s=s.replace('qs<-sort(unique(c(tl,tu,tn,pl-3,pu+3)))','qs<-sort(unique(c(tl,tu,tn,pl-de$caliper,pu+de$caliper)))')
s=s.replace('pl-3','pl-de$caliper').replace('pu+3','pu+de$caliper').replace('pl<=q+3,pu>=q-3','pl<=q+de$caliper,pu>=q-de$caliper')
s=s.replace("secondary_members=sum(p$seed_source[use]=='secondary'),","secondary_members=sum(p$seed_source[use]=='secondary'),rating_implied_members=sum(p$seed_source[use]=='moodys'),")
s=s.replace('p$primary_members+p$ids_members+p$secondary_members==p$n_peers','p$primary_members+p$ids_members+p$secondary_members+p$rating_implied_members==p$n_peers')
s=s.replace("'rating_implied_donors_excluded'","'rating_implied_donors_only_in_authorized_PISR_sensitivity'")
s=s.replace("fwrite(checks,file.path(out,'checks.csv'))","stopifnot(all(p[!grepl('^PISR',method),rating_implied_members]==0))\nfwrite(checks,file.path(out,'checks.csv'))")
s=s.replace("'experiments/p15_wb_scorecard_20260912/build_peer_candidate.R',","'experiments/p15_wb_scorecard_20260912/constructed_v1/build_peer_candidate.R',\n  'experiments/p15_wb_scorecard_20260912/constructed_v1/score_model.R',\n  'experiments/p15_wb_scorecard_20260912/constructed_v1/build.R',")
s=s.replace("else 'No complete shadow grades: peer figures are observed-rating controls, not new scorecard estimates'", "else if(any(is.finite(shadow_notch)))'Conditional shadow scenarios are built; no exact-method certification' else 'No finite shadow estimates'")
s=s.replace("'experiments/p15_wb_scorecard_20260912/peer/test_matching.R',", "'experiments/p15_wb_scorecard_20260912/constructed_v1/test_matching.R',")
s=s.replace('secondary=sum(secondary_members),shadow_matched_members=', 'secondary=sum(secondary_members),rating_implied=sum(rating_implied_members),shadow_matched_members=')
(b/'build_peer_candidate.R').write_text(s)
