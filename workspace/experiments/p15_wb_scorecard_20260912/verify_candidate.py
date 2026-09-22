"""Independent checks of supplemental inputs, reported counts, and preservation."""
import csv, hashlib, json, math
from pathlib import Path
import openpyxl

ROOT = Path('experiments/p15_wb_scorecard_20260912')
def read(path):
    with open(path, newline='') as f:
        return list(csv.DictReader(f))
def num(row, key):
    try: return float(row[key])
    except (ValueError, TypeError, KeyError): return float('nan')
def finite(row, key): return math.isfinite(num(row,key))
checks=[]
def check(name, passed, detail):
    checks.append(dict(check=name,passed=bool(passed),detail=detail))
    if not passed: raise AssertionError((name,detail))

x=read(ROOT/'scorecard_inputs.csv')
keys={(r['iso3'],r['analysis_year']) for r in x}
check('complete_current_country_year_grid',len(x)==len(keys)==2743 and len({r['iso3'] for r in x})==211,'2743 unique country-years / 211 countries')
peers=[r for r in x if r['historical_lmic_reporting_scope']=='TRUE' and r['selected_tier']=='peer']
check('selected_lmic_peer_denominator',len(peers)==778 and len({r['iso3'] for r in peers})==90,'778 selected LMIC peers / 90 countries')

w=openpyxl.load_workbook(ROOT/'supplemental/wef_gci_2007_2017.xlsx',read_only=True,data_only=True)
rows=w['Data'].iter_rows(values_only=True)
next(rows);next(rows);header=next(rows)
raw_gci={}
for row in rows:
    if row[3]=='GCI' and row[7]=='Value':
        for code,value in zip(header[8:],row[8:]):
            if isinstance(code,str) and len(code)==3 and code.isalpha() and isinstance(value,(int,float)):
                raw_gci[(code,int(row[2][:4]))]=value
gci_checks=0
for row in x:
    iso,t=row['iso3'],int(row['reference_year'])
    avail=[(year,value) for (code,year),value in raw_gci.items() if code==iso and year<=t]
    last=max(avail) if avail else None
    for col,max_age in [('gci_same_year',0),('gci_carry_max2',2),('gci_last_available',100)]:
        expected=last[1] if last and t-last[0]<=max_age else float('nan')
        actual=num(row,col)
        assert (math.isnan(expected) and math.isnan(actual)) or abs(expected-actual)<1e-11,(iso,t,col)
        gci_checks+=1
check('gci_original_workbook_independent_reconstruction',True,f'{gci_checks} current/carry/stale comparisons; original legacy scale preserved')

raw=json.load(open(ROOT/'supplemental/wdi_ppg_external_debt.json'))
debt={(r['countryiso3code'],int(r['date'])):r['value'] for r in raw[1]}
nratios=0;outliers=0
for row in x:
    key=(row['iso3'],int(row['reference_year']))
    d=debt.get(key)
    den=num(row,'debt_gdp')/100*num(row,'nominal_gdp_usd_bn')*1e9
    expected=100*d/den if d is not None and den>0 else float('nan')
    actual=num(row,'fc_share_external_ppg_proxy')
    assert (math.isnan(expected) and math.isnan(actual)) or math.isclose(expected,actual,rel_tol=1e-12,abs_tol=1e-12),key
    if math.isfinite(expected):
        nratios+=1
        if not 0<=expected<=100:
            outliers+=1
            assert not finite(row,'fc_share_proxy_in_range')
check('external_ppg_ratio_units_and_no_silent_clipping',True,f'{nratios} finite ratios; {outliers} invalid share proxies flagged')

audit=read(ROOT/'input_scenario_country_years.csv')
summary=read(ROOT/'input_scenario_coverage_income.csv')
summary_year=read(ROOT/'input_scenario_coverage_year.csv')
base=['growth_avg7','growth_sd10','inflation_avg7','inflation_sd10','nominal_gdp_usd_bn',
      'gdp_pc_ppp','debt_gdp','debt_revenue','debt_trend_pp','fc_share_proxy_in_range']
policies={
 'strict_historical_gross_current_gci':['gross_interest_gdp','gross_interest_revenue','gci_same_year'],
 'net_interest_current_gci':['implied_net_interest_gdp','implied_net_interest_revenue','gci_same_year'],
 'net_interest_gci_carry_max2':['implied_net_interest_gdp','implied_net_interest_revenue','gci_carry_max2'],
 'net_interest_stale_gci_hold_diagnostic':['implied_net_interest_gdp','implied_net_interest_revenue','gci_last_available'],
 'latest_gfs_gross_gci_carry_max2':['gfs_gross_interest_gdp_latest','gfs_gross_interest_revenue_latest','gci_carry_max2']}
lookup={(r['iso3'],r['analysis_year']):r for r in x}
for row in audit:
    inp=lookup[(row['iso3'],row['analysis_year'])]
    expected=all(finite(inp,c) for c in base+policies[row['input_scenario']])
    assert expected==(row['input_metrics_complete']=='TRUE')
for table,group in [(summary,'historical_income_level'),(summary_year,'analysis_year')]:
    for row in table:
        z=[r for r in peers if r[group]==row[group]]
        ok=[r for r in z if all(finite(r,c) for c in base+policies[row['input_scenario']])]
        assert int(row['selected_peer_country_years'])==len(z)
        assert int(row['complete_input_country_years'])==len(ok)
        if 'data_complete_countries' in row:
            assert int(row['data_complete_countries'])==len({r['iso3'] for r in ok})
check('input_availability_vs_rating_counts',True,f'{len(audit)} row scenarios and {len(summary)+len(summary_year)} grouped summaries independently verified')

shadow=read(ROOT/'scorecard_country_year.csv')
check('no_invented_shadow_grades',len(shadow)==2743 and all(r['scorecard_complete']=='FALSE' and not finite(r,'shadow_notch') for r in shadow),'Zero grades issued while full2018 numerical rules unavailable')
for row in x:
    if finite(row,'boc_bond_bank_last_positive_year'):
        assert num(row,'boc_bond_bank_last_positive_year')<=num(row,'reference_year')
check('default_history_excludes_future_years',True,'Historical stock-proxy dates cannot exceed reference year')

preserved=read(ROOT/'peer/production_preservation_check.csv')
for row in preserved:
    digest=hashlib.sha256(Path(row['path']).read_bytes()).hexdigest()
    assert digest==row['sha256_before']==row['sha256_after']
check('production_and_seed_hashes_unchanged',True,f'{len(preserved)} exact input hashes')
for row in read(ROOT/'supplemental/input_manifest.csv'):
    assert hashlib.sha256(Path(row['path']).read_bytes()).hexdigest()==row['sha256']
check('supplemental_source_hashes',True,'All supplemental input snapshots match their receipts')
for path in ['peer/checks.csv','macro/macro_checks.csv','methodology/component_check_results.csv']:
    items=read(ROOT/path)
    values=[r.get('passed',r.get('pass',r.get('status',''))) for r in items]
    assert all(str(v).lower() in ('true','pass','passed') for v in values),(path,values)
check('component_check_receipts',True,'Macro, scoring components and final peer integration receipts all pass')

with open(ROOT/'independent_checks.csv','w',newline='') as f:
    writer=csv.DictWriter(f,fieldnames=['check','passed','detail']);writer.writeheader();writer.writerows(checks)
print(json.dumps({'checks_passed':len(checks),'gci_values_verified':gci_checks,'input_scenarios_verified':len(audit)}))
