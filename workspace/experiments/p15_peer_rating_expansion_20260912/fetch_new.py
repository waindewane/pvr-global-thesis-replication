from pathlib import Path
import urllib.request,json,hashlib,concurrent.futures,datetime
root=Path('/Users/waindewane/Claude and Codex/Miscellaneous/pvr-global/exports/thesis-replication-20260922/workspace/experiments/p15_peer_rating_expansion_20260912')
out=root/'sources';out.mkdir(parents=True,exist_ok=True)
tasks={
'finance_prosperity2024_ch2.xlsx':'https://thedocs.worldbank.org/en/doc/57f0b48cbef5dd12b2c6cc9e7e5024a5-0430012024/related/Finance-Prosperity-2024-Chapter-2-data-and-figures.xlsx',
'wb_basu_record.json':'https://openknowledge.worldbank.org/server/api/core/items/1de6fb75-8fdf-5409-a131-f77ff6387f63',
'wb_other_record.json':'https://openknowledge.worldbank.org/server/api/core/items/1a842a12-3c03-5bf9-94b5-09e6c6dbe32d'}
def fetch(item):
 name,url=item;p=out/name
 try:
  downloaded=not p.exists()
  if downloaded:
   req=urllib.request.Request(url,headers={'User-Agent':'Mozilla/5.0'})
   with urllib.request.urlopen(req,timeout=60) as r: b=r.read()
   p.write_bytes(b)
  b=p.read_bytes()
  return dict(file=name,url=url,bytes=len(b),sha256=hashlib.sha256(b).hexdigest(),retrieved_utc=datetime.datetime.fromtimestamp(p.stat().st_mtime,datetime.timezone.utc).isoformat(),downloaded_this_call=downloaded)
 except Exception as e:return dict(file=name,url=url,error=str(e))
if __name__=='__main__':
 with concurrent.futures.ThreadPoolExecutor(max_workers=3) as ex:results=list(ex.map(fetch,tasks.items()))
 (root/'new_source_manifest.json').write_text(json.dumps(results,indent=2));print(json.dumps(results,indent=2))
