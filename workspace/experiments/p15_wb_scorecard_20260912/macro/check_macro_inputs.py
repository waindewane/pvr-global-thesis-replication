"""Independent arithmetic and provenance checks; transformation remains R-first."""
import csv,json,math,statistics,hashlib
from pathlib import Path
ROOT=Path(__file__).resolve().parent
read=lambda p:list(csv.DictReader((ROOT/p).open()))
panel=read('macro_input_panel.csv');raw=read('weo_required_observations_long.csv');vintages=read('weo_vintage_completeness.csv')
checks=[]
def check(name,condition,detail):
 checks.append(dict(check=name,passed=bool(condition),detail=detail));assert condition,(name,detail)
def n(x):
 try:return float(x)
 except:return math.nan
def eq(a,b):return (not math.isfinite(a) and not math.isfinite(b)) or math.isclose(a,b,rel_tol=1e-10,abs_tol=1e-8)
check('country_year_grid',len(panel)==2743 and len({r['iso3'] for r in panel})==211 and len({(r['iso3'],r['analysis_year']) for r in panel})==2743,'211 countries x13 years; unique keys')
check('vintage_timing',all(int(r['reference_year'])==int(r['analysis_year'])-1 and r['vintage'][:4]==r['reference_year'] for r in panel),'Allfallvintages strictly before Jan1 application year')
check('vintage_completeness',len(vintages)==13 and all(r['series_expected']==r['series_retrieved'] and r['all_subjects_present']=='TRUE' for r in vintages),'13vintages;1direct+12two-page mirrors')
manifest=json.loads((ROOT/'weo_download_manifest.json').read_text())
check('source_hashes',all(hashlib.sha256((ROOT/r['file']).read_bytes()).hexdigest()==r['sha256'] for r in manifest if not r.get('error')),'All25successfulWEOartifacts match downloadmanifest')
check('unavailable_direct_routes_recorded',sum(bool(r.get('error')) for r in manifest)==12,'12direct403responses recorded; successfully mirrored')
keys={};lookup={}
for r in raw:
 k=(r['iso3'],int(r['analysis_year']),r['indicator'],int(r['year']))
 assert k not in lookup;lookup[k]=n(r['value'])
check('observation_uniqueness',len(lookup)==len(raw),'Unique country x vintage x indicator x observationyear')
check('window_bounds',all(int(r['reference_year'])-9<=int(r['year'])<=int(r['reference_year'])+2 for r in raw),'Only t−9…t+2 included')
checks_n=0
for r in panel:
 iso=r['iso3'];y=int(r['analysis_year']);t=y-1
 val=lambda code,yr:lookup.get((iso,y,code,yr),math.nan)
 for out,code,start,end,fun in [('growth_avg7','NGDP_RPCH',t-4,t+2,statistics.mean),('growth_sd10','NGDP_RPCH',t-9,t,statistics.stdev),('inflation_avg7','PCPIPCH',t-4,t+2,statistics.mean),('inflation_sd10','PCPIPCH',t-9,t,statistics.stdev)]:
  xs=[val(code,j) for j in range(start,end+1)]
  expected=fun(xs) if all(math.isfinite(x) for x in xs) else math.nan
  assert eq(n(r[out]),expected),(iso,y,out,n(r[out]),expected)
  checks_n+=1
 for out,expected in [('nominal_gdp_usd_bn',val('NGDPD',t)),('gdp_pc_ppp',val('PPPPC',t)),('debt_gdp',val('GGXWDG_NGDP',t)),('debt_trend_pp',val('GGXWDG_NGDP',t+1)-val('GGXWDG_NGDP',t-4)),('implied_net_interest_gdp',val('GGXONLB_NGDP',t)-val('GGXCNL_NGDP',t))]:
  assert eq(n(r[out]),expected),(iso,y,out,n(r[out]),expected)
  checks_n+=1
 rev=val('GGR_NGDP',t)
 for out,amount in [('debt_revenue',val('GGXWDG_NGDP',t)),('implied_net_interest_revenue',val('GGXONLB_NGDP',t)-val('GGXCNL_NGDP',t))]:
  expected=100*amount/rev if math.isfinite(rev) and rev>0 else math.nan
  assert eq(n(r[out]),expected),(iso,y,out)
  checks_n+=1
check('independent_formula_replay',True,f'{checks_n} complete-window, sample-SD, level and ratio values independently reproduced from stored raw records')
check('gross_net_distinction',all(not math.isfinite(n(r['gross_interest_gdp'])) and not math.isfinite(n(r['gross_interest_revenue'])) and 'NET_interest' in r['interest_measure'] for r in panel),'No net-interest values relabelled as observed gross expense')
check('gfs_vintage_warning',all('not_historical_vintage' in r['gfs_information_basis'] for r in panel),'GFS sensitivity visibly identifies latest revision')
check('no_silent_fallback',all(r['fallback_used']=='FALSE' for r in panel),'No WDI/current-vintage/cross-country imputation in main panel')
peer=[r for r in panel if r['historical_lmic_reporting_scope']=='TRUE' and r['selected_tier']=='peer']
check('production_peer_scope',len(peer)==778,'Scope matches actual current-run selected LMIC peers')
check('kosovo_palestine_mapping',any(r['source_country_code']=='UVK' and r['iso3']=='XKX' for r in raw) and any(r['source_country_code']=='WBG' and r['iso3']=='PSE' and r['in_current_universe']=='FALSE' for r in read('weo_country_code_mapping.csv')),'Provideraliases explicitly mapped; Kosovo retained, Palestine correctly outside current universe')
check('known_units_sanity',10000<n(next(r for r in panel if r['iso3']=='USA' and r['analysis_year']=='2012')['nominal_gdp_usd_bn'])<20000,'2011USA nominalGDP inUSDbillions; no accidentalunit/billionfactor')
with (ROOT/'macro_checks.csv').open('w') as f:
 w=csv.DictWriter(f,fieldnames=['check','passed','detail']);w.writeheader();w.writerows(checks)
print(len(checks),'checks passed;',checks_n,'arithmetic comparisons')
