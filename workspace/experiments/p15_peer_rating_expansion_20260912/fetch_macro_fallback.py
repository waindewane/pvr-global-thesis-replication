import sys,json,concurrent.futures,csv
from pathlib import Path
sys.path.insert(0,str(Path(__file__).parent))
from fetch_new import root,fetch
tasks={}
for c in ['GOV_WGI_RL.EST','GOV_WGI_GE.EST','GOV_WGI_CC.EST','GOV_WGI_PV.EST']:
 tasks['wdi_'+c+'.json']=f'https://api.worldbank.org/v2/country/all/indicator/{c}?format=json&per_page=20000&date=2000:2024'
 tasks['meta_'+c+'.json']=f'https://api.worldbank.org/v2/indicator/{c}?format=json&per_page=100'
tasks['dbnomics_weo_debt.json']='https://api.db.nomics.world/v22/series/IMF/WEO:2024-10?limit=1000&observations=1&dimensions=%7B%22weo-subject-code%22%3A%5B%22GGXWDG_NGDP%22%5D%7D'
tasks['wb_basu_bundles.json']='https://openknowledge.worldbank.org/server/api/core/items/1de6fb75-8fdf-5409-a131-f77ff6387f63/bundles?size=100'
if __name__=='__main__':
 with concurrent.futures.ThreadPoolExecutor(max_workers=4) as ex:results=list(ex.map(fetch,tasks.items()))
 (root/'macro_fallback_manifest.json').write_text(json.dumps(results,indent=2));print(json.dumps(results,indent=2))
 rows=[]
 for name in tasks:
  p=root/'sources'/name
  if not p.exists() or not name.startswith('wdi_'):continue
  d=json.loads(p.read_text())
  if not(isinstance(d,list) and len(d)>1):print('INVALID',name);continue
  assert int(d[0]['pages'])==1
  for r in d[1] or []:rows.append(dict(provider='WB',indicator=name[4:-5],iso3=r['countryiso3code'],year=int(r['date']),value=r['value'],country=r['country']['value']))
 with (root/'macro_fallback_long.csv').open('w') as f:
  w=csv.DictWriter(f,fieldnames=['provider','indicator','iso3','year','value','country']);w.writeheader();w.writerows(rows)
