from pathlib import Path
import urllib.request,json,hashlib,datetime,concurrent.futures,subprocess
r=Path(__file__).resolve().parent
urls={'bank_canada2017_7_official.pdf':'https://www.bankofcanada.ca/wp-content/uploads/2017/05/sdp2017-7.pdf','bank_canada2021_16.pdf':'https://www.bankofcanada.ca/wp-content/uploads/2021/11/sdp2021-16.pdf'}
def get(item):
 name,url=item;p=r/name;d=dict(file=name,url=url)
 try:
  if not p.exists():p.write_bytes(urllib.request.urlopen(url,timeout=45).read())
  b=p.read_bytes();d.update(bytes=len(b),sha256=hashlib.sha256(b).hexdigest(),pdf_signature=b.startswith(b'%PDF'),retrieved_utc=datetime.datetime.fromtimestamp(p.stat().st_mtime,datetime.timezone.utc).isoformat())
  if b.startswith(b'%PDF'):subprocess.run(['pdftotext','-layout',str(p),str(p.with_suffix('.txt'))],check=True)
 except Exception as e:d['error']=str(e)
 print(d,flush=True);return d
with concurrent.futures.ThreadPoolExecutor(max_workers=3) as ex:d=list(ex.map(get,urls.items()))
(r/'bank_canada_download_manifest.json').write_text(json.dumps(d,indent=2))
