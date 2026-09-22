import sys,json,concurrent.futures,csv,urllib.parse
from pathlib import Path
sys.path.insert(0,str(Path(__file__).parent))
from fetch_new import root,fetch
subjects=['GGXWDG_NGDP','GGXCNL_NGDP','NGDPDPC','NGDPD','NGDP_RPCH','PCPIPCH','BCA_NGDP','GGR_NGDP','GGXONLB_NGDP']
q=urllib.parse.urlencode({'limit':1000,'observations':1,'dimensions':json.dumps({'weo-subject':subjects})})
tasks={'dbnomics_weo_macro_page1.json':'https://api.db.nomics.world/v22/series/IMF/WEO:2024-10?'+q,
'dbnomics_weo_macro_page2.json':'https://api.db.nomics.world/v22/series/IMF/WEO:2024-10?'+q+'&offset=1000'}
if __name__=='__main__':
 with concurrent.futures.ThreadPoolExecutor(max_workers=2) as ex:results=list(ex.map(fetch,tasks.items()))
 (root/'weo_mirror_manifest.json').write_text(json.dumps(results,indent=2));print(json.dumps(results,indent=2))
 rows=[];series=[]
 for name in tasks:
  p=root/'sources'/name
  if not p.exists():continue
  d=json.loads(p.read_text());print(name,d.get('errors'),d['series']['num_found']);series+=d['series']['docs']
 assert len(series)==d['series']['num_found']
 for s in series:
  code=s['dimensions']['weo-subject'];iso=s['dimensions']['weo-country']
  assert code in subjects
  for y,v in zip(s['period'],s['value']):
   if 2000<=int(y)<=2024:rows.append(dict(provider='IMF_WEO2024Oct_DBnomics',indicator=code,iso3=iso,year=int(y),value=None if v=='NA' else v,country=''))
 with (root/'weo_macro_long.csv').open('w') as f:
  w=csv.DictWriter(f,fieldnames=['provider','indicator','iso3','year','value','country']);w.writeheader();w.writerows(rows)
 print(len(rows),'observations')
