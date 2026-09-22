from pathlib import Path
import urllib.parse,json
from fetch_fall_weo import ROOT,S,fetch
D={'FREQ':['A'],'REF_SECTOR':['S13'],'UNIT_MEASURE':['XDC_R_B1GQ'],'CLASSIFICATION':['G24__Z','G1__Z']}
results=[];offset=0
while True:
 q=urllib.parse.urlencode({'dimensions':json.dumps(D),'observations':1,'limit':1000,'offset':offset})
 u='https://api.db.nomics.world/v22/series/IMF/GFSMAB?'+q
 r=fetch(u,S/f'gfs_general_gov_interest_revenue_{offset}.json');results.append(r)
 d=json.loads((ROOT/r['file']).read_text());n=d['series']['num_found'];got=len(d['series']['docs']);offset+=got
 print(n,got,offset,flush=True)
 if offset>=n:assert offset==n;break
 assert got>0
(ROOT/'gfs_interest_manifest.json').write_text(json.dumps(results,indent=2))
