from pathlib import Path
import sys,json,concurrent.futures,csv,urllib.request,hashlib,datetime,time
sys.path.insert(0,str(Path(__file__).parent))
from fetch_new import root,fetch
codes=['NY.GDP.PCAP.CD','NY.GDP.MKTP.CD','NY.GNP.PCAP.CD','NY.GDP.MKTP.KD.ZG','FP.CPI.TOTL.ZG','BN.CAB.XOKA.GD.ZS','FI.RES.TOTL.MO','DT.DOD.DECT.EX.ZS','DT.DOD.DECT.GN.ZS','GC.DOD.TOTL.GD.ZS','GC.BAL.CASH.GD.ZS','RL.EST','GE.EST','CC.EST','PV.EST','GC.XPN.INTP.RV.ZS']
tasks={}
for c in codes:
 tasks['wdi_'+c+'.json']=f'https://api.worldbank.org/v2/country/all/indicator/{c}?format=json&per_page=20000&date=2000:2024'
 tasks['meta_'+c+'.json']=f'https://api.worldbank.org/v2/indicator/{c}?format=json&per_page=100'
for c in ['GGXWDG_NGDP','GGXCNL_NGDP','NGDPDPC','NGDPD','NGDP_RPCH','PCPIPCH','BCA_NGDP']:
 tasks['imf_'+c+'.json']=f'https://www.imf.org/external/datamapper/api/v1/{c}'
if __name__=='__main__':
 results=[]
 with concurrent.futures.ThreadPoolExecutor(max_workers=4) as ex:
  for x in ex.map(fetch,tasks.items()):
   results.append(x);print(x['file'],x.get('bytes',x.get('error')),flush=True)
 (root/'macro_source_manifest.json').write_text(json.dumps(results,indent=2))
 rows=[]
 for c in codes:
  p=root/'sources'/('wdi_'+c+'.json')
  if not p.exists():continue
  d=json.loads(p.read_text())
  if not(isinstance(d,list) and len(d)>1): print('INVALID',c,d);continue
  assert int(d[0]['pages'])==1,(c,d[0])
  for r in d[1] or []:
   rows.append(dict(provider='WB',indicator=c,iso3=r['countryiso3code'],year=int(r['date']),value=r['value'],country=r['country']['value']))
 for c in ['GGXWDG_NGDP','GGXCNL_NGDP','NGDPDPC','NGDPD','NGDP_RPCH','PCPIPCH','BCA_NGDP']:
  p=root/'sources'/('imf_'+c+'.json')
  if not p.exists():continue
  d=json.loads(p.read_text())
  for country,v in d.get('values',{}).get(c,{}).items():
   for y,vv in v.items():
    if 2000<=int(y)<=2024:rows.append(dict(provider='IMF',indicator=c,iso3=country,year=int(y),value=vv,country=''))
 with (root/'macro_raw_long.csv').open('w') as f:
  w=csv.DictWriter(f,fieldnames=['provider','indicator','iso3','year','value','country']);w.writeheader();w.writerows(rows)
 print('Total macro rows',len(rows),flush=True)
