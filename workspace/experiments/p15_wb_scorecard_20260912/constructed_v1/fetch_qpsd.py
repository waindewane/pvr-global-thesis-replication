from pathlib import Path
import urllib.request,json,hashlib,csv
b=Path(__file__).resolve().parent;s=b/'sources';receipts=[];values={}
for name,indicator in [('fc','DP.DOD.DECF.CR.GG'),('total','DP.DOD.DECT.CR.GG')]:
 u=f'https://api.worldbank.org/v2/country/all/indicator/{indicator}?source=20&format=json&per_page=30000&date=2011Q1:2023Q4'
 p=s/f'qpsd_{name}.json'
 if not p.exists():p.write_bytes(urllib.request.urlopen(u,timeout=45).read())
 raw=p.read_bytes();d=json.loads(raw);print(name,d[0],flush=True);assert d[0]['pages']==1
 receipts.append(dict(url=u,file=str(p),sha256=hashlib.sha256(raw).hexdigest()))
 for r in d[1]:
  if r['date'].endswith('Q4') and r['value'] is not None:
   key=(r['countryiso3code'],int(r['date'][:4]));values.setdefault(key,{})[name]=r['value']
rows=[]
for (i,y),v in sorted(values.items()):
 if len(v)==2 and v['total']>0:rows.append(dict(iso3=i,reference_year=y,qpsd_fc_share=100*v['fc']/v['total'],**v))
with (b/'qpsd_fc_share.csv').open('w') as f:
 w=csv.DictWriter(f,fieldnames=['iso3','reference_year','qpsd_fc_share','fc','total']);w.writeheader();w.writerows(rows)
(b/'qpsd_manifest.json').write_text(json.dumps(receipts,indent=2));print('paired Q4 observations',len(rows))
