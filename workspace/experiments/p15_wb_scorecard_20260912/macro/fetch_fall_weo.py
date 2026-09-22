"""Acquire immutable IMF fall vintages; data transformation is in build_macro_inputs.R."""
from pathlib import Path
import urllib.request,urllib.parse,json,hashlib,datetime,concurrent.futures,time
ROOT=Path(__file__).resolve().parent
S=ROOT/'sources';S.mkdir(exist_ok=True)
SUBJECTS=['NGDP_RPCH','PCPIPCH','NGDPD','PPPPC','GGXWDG_NGDP','GGXCNL_NGDP','GGXONLB_NGDP','GGR_NGDP']
def fetch(url,p):
 existed=p.exists()
 if not existed:
  for attempt in range(3):
   try:
    req=urllib.request.Request(url,headers={'User-Agent':'Mozilla/5.0 (academic economic research)'})
    with urllib.request.urlopen(req,timeout=55) as r:b=r.read()
    p.write_bytes(b);break
   except Exception:
    if attempt==2:raise
    time.sleep(2)
 b=p.read_bytes()
 return {'url':url,'file':str(p.relative_to(ROOT)),'bytes':len(b),'sha256':hashlib.sha256(b).hexdigest(),'retrieved_utc':datetime.datetime.fromtimestamp(p.stat().st_mtime,datetime.timezone.utc).isoformat(),'downloaded_this_call':not existed}
def run(y):
 name=f"weo{'sep' if y==2011 else 'oct'}{y}all.xls"
 url=f'https://www.imf.org/-/media/files/publications/weo/weo-database/{y}/{name}'
 records=[]
 try:
  rec=fetch(url,S/name);rec.update(vintage_year=y,vintage_month=9 if y==2011 else 10,format='IMF_TSV')
  b=(S/name).read_bytes()
  if not b.startswith(b'WEO Country Code\t'):raise ValueError('Not expected IMF TSV header')
  records.append(rec);print(y,'direct',len(b),flush=True)
 except Exception as e:
  records.append({'url':url,'vintage_year':y,'format':'IMF_TSV','error':str(e)})
  if y==2011:return records
  offset=0
  while True:
   q=urllib.parse.urlencode({'limit':1000,'observations':1,'dimensions':json.dumps({'weo-subject':SUBJECTS}),'offset':offset})
   u=f'https://api.db.nomics.world/v22/series/IMF/WEO:{y}-10?'+q
   try:
    r=fetch(u,S/f'weo_{y}_10_offset{offset}.json');r.update(vintage_year=y,vintage_month=10,format='DBnomics_JSON');records.append(r)
    d=json.loads((ROOT/r['file']).read_text());n=d['series']['num_found'];got=len(d['series']['docs']);assert got>0
    offset+=got
    if offset>=n:assert offset==n;break
   except Exception as e:records.append({'url':u,'vintage_year':y,'format':'DBnomics_JSON','error':str(e)});break
  print(y,'mirror',offset,flush=True)
 return records
if __name__=='__main__':
 with concurrent.futures.ThreadPoolExecutor(max_workers=3) as ex:results=list(ex.map(run,range(2011,2024)))
 records=sum(results,[])
 (ROOT/'weo_download_manifest.json').write_text(json.dumps(records,indent=2))
 print('Finished',len(records),'requests',flush=True)
