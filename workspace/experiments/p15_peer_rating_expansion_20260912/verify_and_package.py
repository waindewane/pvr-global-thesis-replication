from pathlib import Path
import csv, json, hashlib, shutil, zipfile, sys
import numpy as np
import pandas as pd

repo = Path('/Users/waindewane/Claude and Codex/Miscellaneous/pvr-global/exports/thesis-replication-20260922/workspace')
p = repo / 'experiments/p15_peer_rating_expansion_20260912'
outputs = Path('/Users/waindewane/Documents/Codex/2026-09-12/referenced-chatgpt-conversation-this-is-an/outputs')
def digest(f):
    h = hashlib.sha256()
    with f.open('rb') as stream:
        for block in iter(lambda: stream.read(1048576), b''): h.update(block)
    return h.hexdigest()
def read(name): return pd.read_csv(p / name)
checks = []
def check(name, passed, detail):
    checks.append(dict(check=name, passed=bool(passed), detail=str(detail)))

preserved = []
for row in csv.DictReader((p/'production_input_manifest.csv').open()):
    f = repo / row['path']
    current = digest(f)
    preserved.append(dict(path=row['path'], before_sha256=row['sha256'], after_sha256=current, unchanged=current==row['sha256']))
pd.DataFrame(preserved).to_csv(p/'production_preservation_check.csv',index=False)
check('production_preserved', len(preserved)==9 and all(r['unchanged'] for r in preserved), '9 original pointer/data/valuation hashes')
ptr=json.loads((repo/'data-derived/p15_master/current_run.json').read_text())
core=pd.read_csv(Path(ptr['candidate'])/'core_evidence.csv',low_memory=False)
sel=pd.read_csv(Path(ptr['candidate'])/'selected_reference.csv',low_memory=False)
mem=pd.read_csv(Path(ptr['candidate'])/'peer_membership.csv',low_memory=False)
scope=core[['iso3','analysis_year','historical_lmic_reporting_scope']].merge(sel[['iso3','analysis_year','selected_tier','selected_rate_pct','peer_pool_rule']],on=['iso3','analysis_year'])
scope=scope[scope.historical_lmic_reporting_scope]
check('historical_scope',len(core)==2743 and len(scope)==1756, f'{len(core)} total; {len(scope)} historical LMIC')
selected=scope[scope.selected_tier=='peer'].copy()
mapping=read('peer_rule_mapping.csv').set_index('current_label').rule
selected['rule']=selected.peer_pool_rule.map(mapping)
counts=selected.rule.value_counts().reindex(range(1,9),fill_value=0).tolist()
check('selected_rule_counts',counts==[0,0,0,1,72,339,146,220] and selected.iso3.nunique()==90,counts)
dep=read('all_peer_deployment_cases.csv')
check('deployment_unique',not dep.duplicated(['iso3','analysis_year','method']).any(),f'{len(dep)} records, {dep.method.nunique()} methods')
check('deployment_universe',dep.method.nunique()==38 and dep.groupby('method').size().eq(786).all(),'778 existing peers and 8 no-rate cases per method')
base=dep[(dep.method=='current')&(dep.selected_tier=='peer')]
check('baseline_rates',len(base)==778 and np.allclose(base.estimate,base.selected_rate_pct,atol=1e-10,rtol=0),'778 estimates exactly reproduced')
b=base.merge(selected[['iso3','analysis_year','rule']],on=['iso3','analysis_year'],suffixes=('_replay','_production'))
check('baseline_rule_mapping',b.rule_replay.eq(b.rule_production).all(),'All 778 numbered rules match current labels')
mm=mem[mem.used_for_estimate & (mem.peer_method=='similarity_min3')].groupby(['target_iso3','analysis_year']).peer_iso3.agg(lambda a:';'.join(sorted(a)))
check('baseline_membership',all(r.member_ids==mm.loc[(r.iso3,r.analysis_year)] for r in base.itertuples()),'Every selected baseline donor set matches production')
finite=dep[dep.estimate.notna()]
def valid_members(frame):
    for row in frame.itertuples():
        ids=row.member_ids.split(';') if isinstance(row.member_ids,str) else []
        if len(ids)!=row.n_peers or len(set(ids))!=len(ids) or row.iso3 in ids or len(ids)<3: return False
        if row.primary_members+row.ids_members+row.secondary_members!=len(ids): return False
    return True
check('all_donor_constraints',valid_members(finite),'At least 3 distinct countries, target absent, source counts reconcile')
check('no_unauthorized_selection',not dep[dep.selected_tier!='peer'].selected_peer_new.any(),'No previously excluded/no-rate case obtains a selected rate')
cov=read('all_peer_coverage.csv').set_index('method')
check('cutoff_losses',int(cov.loc['P_stop5','lost_current_peers'])==705 and int(cov.loc['drop8','lost_current_peers'])==220,'Stop 5 loses 705; drop 8 loses 220')
check('headline_coverage',int(cov.loc['PIS_fiscal_ols_stop5','retained_current_peers'])==643 and int(cov.loc['PIS_fiscal_no_rule4','retained_current_peers'])==543 and int(cov.loc['PIS_biennial_stop5','retained_current_peers'])==646,'643 / 543 / 646')
losses=read('all_lost_peers_by_country.csv')
loss_ok=all(len(str(r.years).split(';'))==r.country_years for r in losses.itertuples())
totals=losses.groupby('method').country_years.sum().reindex(cov.index,fill_value=0)
check('loss_list_reconciles',loss_ok and totals.eq(cov.lost_current_peers).all(),'All per-country year lists sum to option totals')
feat=read('model_feature_panel.csv')
check('macro_grid_unique',len(feat)==4220 and feat.iso3.nunique()==211 and not feat.duplicated(['iso3','analysis_year']).any(),'211 countries x 20 years, 2005–2024')
check('macro_lags',feat.macro_year.eq(feat.analysis_year-1).all(),'Main features end in t-1; revisions separately disclosed')
sc=['Aaa','Aa1','Aa2','Aa3','A1','A2','A3','Baa1','Baa2','Baa3','Ba1','Ba2','Ba3','B1','B2','B3','Caa1','Caa2','Caa3','Ca','C']
f=feat[feat.analysis_year>=2012]
expected=f.rating_moodys_rating.map({k:i+1 for i,k in enumerate(sc)})
check('training_labels_match_current',np.allclose(f.rating_notch,expected,equal_nan=True),'2012–2024 recognized Moody labels align with current core')
sh=read('shadow_predictions.csv')
check('model_prediction_keys',not sh.duplicated(['iso3','analysis_year','model']).any() and len(sh)==2743*5,'Five main models, all current country-years')
check('training_precedes_prediction',sh.training_last_year.lt(sh.analysis_year).all(),'Maximum training year < target year')
check('model_score_bounds',sh.predicted_notch.dropna().between(1,21).all(),'Continuous score restricted to 1–21')
v=read('strict_peer_validation_cases.csv'); vs=read('strict_peer_validation.csv').set_index('method')
check('strict_validation_keys',not v.duplicated(['iso3','analysis_year','method']).any() and valid_members(v),'Distinct tests and target excluded from every donor list')
vv=v[v.method=='PIS_fiscal_ols_stop5']
check('strict_validation_headline',len(vv)==215 and vv.iso3.nunique()==47 and abs((vv.estimate-vv.actual).abs().mean()-1.1945915)<1e-6,'215 cases / 47 countries; MAE 1.194592')
check('strict_validation_summaries',all(abs((g.estimate-g.actual).abs().mean()-vs.loc[m,'mae_a'])<1e-9 for m,g in v.groupby('method')),'Independently recomputed absolute errors')
code=(p/'leakage_check.R').read_text()
check('strict_training_exclusion_code','analysis_year<y & fold!=f & iso3!=cc' in code and 'analysis_year<ay & fold!=f & iso3!=cc' in code,'Reviewed annual and biennial donor fits omit full target history; target fits use country folds')
check('low_income_test_support',(vv.historical_income_level=='Low income').sum()==5,'Only five low-income validation country-years')
web=read('web_rating_peer_reconciliation.csv')
check('actual_rating_candidates',web.new_grade_candidate.sum()==47 and web.screened_grade_candidate.sum()==46,'47 possible additions, Cuba 2024 rejected, 46 candidates')
valuation=read('crs_diagnostic_valuation_cases.csv')
old=valuation[valuation.method=='current']
check('cashflow_baseline_replay',len(old)==881 and old.ge_change.abs().max()<1e-9,'881 finite current peer-valued CRS records reproduced')
cs=read('crs_diagnostic_valuation_summary.csv')
cs=cs[(cs.method=='PIS_fiscal_ols_stop5')&(cs.period=='2018-2024')].iloc[0]
check('modern_valuation_headline',int(cs.current_records)==256 and int(cs.retained)==254 and abs(cs.mean_abs_ge_change_retained-5.986)<.001,'254 of 256 retained, 5.986 GE points mean absolute change')

source_rows=[]
for mf in sorted(p.glob('*manifest.json')):
    data=json.loads(mf.read_text())
    rows=data if isinstance(data,list) else [data]
    for r in rows:
        if not isinstance(r,dict) or not r.get('file') or not r.get('sha256'): continue
        f=p/'sources'/r['file']
        if not f.exists(): f=p/r['file']
        source_rows.append(dict(manifest=mf.name,file=str(f.relative_to(p)),url=r.get('url',''),source_snapshot_id='sha256:'+r['sha256'],sha256=r['sha256'],bytes=r.get('bytes',''),hash_matches=f.exists() and digest(f)==r['sha256']))
pd.DataFrame(source_rows).to_csv(p/'source_snapshot_manifest.csv',index=False)
check('source_hashes',len(source_rows)>50 and all(r['hash_matches'] for r in source_rows),f'{len(source_rows)} source snapshots checked')
pages=[json.loads((p/'sources'/f'dbnomics_weo_macro_page{i}.json').read_text()) for i in [1,2]]
docs=[r for x in pages for r in x['series']['docs']]
check('weo_pagination',len(docs)==1568 and len({r['series_code'] for r in docs})==1568 and pages[0]['series']['num_found']==1568,'1,568 unique series across both pages')
weo=read('weo_macro_long.csv')
check('weo_observations',len(weo)==39200 and not weo.duplicated(['iso3','year','indicator']).any(),'39,200 records; 8 subjects x 196 countries x 25 years')
check('failed_pdf_not_used',not (p/'sources/elshagi2022.pdf').read_bytes().startswith(b'%PDF') and (p/'sources/elshagi2022_author.pdf').read_bytes().startswith(b'%PDF'),'Econstor response is HTML; valid author PDF used instead')

sources=finite[(finite.method=='PIS_fiscal_ols_stop5') & finite.selected_peer_new]
composition=pd.DataFrame([dict(method='PIS_fiscal_ols_stop5',country_years=len(sources),total_donor_memberships=sources.n_peers.sum(),primary_memberships=sources.primary_members.sum(),ids_memberships=sources.ids_members.sum(),secondary_memberships=sources.secondary_members.sum(),groups_without_primary=(sources.primary_members==0).sum(),groups_with_any_ids=(sources.ids_members>0).sum(),groups_with_any_secondary=(sources.secondary_members>0).sum(),groups_with_modelled_donor_rating=(sources.model_donor_members>0).sum())])
composition.to_csv(p/'fiscal_pis_source_composition.csv',index=False)
affected=selected[selected.rule>=6].copy()
affected['lost_if_only_rule8_removed']=affected.rule.eq(8)
affected=affected.merge(core[['iso3','analysis_year','country']],on=['iso3','analysis_year'])
affected[['iso3','country','analysis_year','rule','peer_pool_rule','selected_rate_pct','lost_if_only_rule8_removed']].sort_values(['country','analysis_year']).to_csv(outputs/'original_cutoff_affected_country_years.csv',index=False)
pd.DataFrame(checks).to_csv(p/'verification_checks.csv',index=False)
print(pd.DataFrame(checks).to_string(index=False))
print(composition.to_string(index=False))
if not all(r['passed'] for r in checks): sys.exit(1)

for name in ['source_claim_audit.csv','verification_checks.csv','production_preservation_check.csv']:
    shutil.copy2(p/name,outputs/name)
names=[f for f in sorted(p.iterdir()) if f.is_file() and f.suffix in ['.csv','.json','.md','.txt'] and not f.name.endswith('_run_log.txt') and f.name not in ['model_feature_panel.csv','macro_raw_long.csv','macro_fallback_long.csv','weo_macro_long.csv','elshagi_boy_ratings_2005_2017.csv']]
with zipfile.ZipFile(outputs/'peer_rating_expansion_tables.zip','w',zipfile.ZIP_DEFLATED) as z:
    for f in names: z.write(f,f.name)
print('Packaged',len(names),'files; checks:',len(checks),'passed')
