from pathlib import Path
import urllib.request, concurrent.futures,hashlib,json,pdfplumber,csv
b=Path(__file__).resolve().parent;s=b/'sources'
u={
'wef_gci_2019.xlsx':'https://www3.weforum.org/docs/WEF_GCI_4.0_2019_Dataset.xlsx',
'ida_fy12.pdf':'https://documents1.worldbank.org/curated/en/527521468339906853/pdf/879650BR0IDA0S0IC0disclosed05080140.pdf',
'ida_fy13.pdf':'https://documents1.worldbank.org/curated/en/331101468325187617/pdf/759260BR0IDA0S00Disclosed0308020130.pdf',
'ida_graduates.html':'https://ida.worldbank.org/en/about/borrowing-countries/ida-graduates',
'hipc.html':'https://www.imf.org/en/about/factsheets/sheets/2023/debt-relief-under-the-heavily-indebted-poor-countries-initiative-hipc',
'wef2020.html':'https://www.weforum.org/publications/the-global-competitiveness-report-2020/in-full/'}
def fetch(kv):
 name,url=kv
 try:
  if (s/name).exists():data=(s/name).read_bytes()
  else:
   data=urllib.request.urlopen(url,timeout=45).read();(s/name).write_bytes(data)
  return dict(file=name,url=url,bytes=len(data),sha256=hashlib.sha256(data).hexdigest(),status='saved_snapshot')
 except Exception as e:return dict(file=name,url=url,error=str(e))
res=list(concurrent.futures.ThreadPoolExecutor(6).map(fetch,u.items()));(b/'source_manifest.json').write_text(json.dumps(res,indent=2));print(res,flush=True)
p=pdfplumber.open(b.parent/'source_intake_20260912/sources/moodys_sovereign_bond_ratings_2018.pdf')
texts=[page.filter(lambda c:c.get('object_type')!='char' or c['size']<13).extract_text(x_tolerance=1,y_tolerance=2) for page in p.pages]
(s/'moodys2018_filtered.txt').write_text('\n\f\n'.join(texts))
labels='VH+ VH VH- H+ H H- M+ M M- L+ L L- VL+ VL VL-'.split()
checks=[]
for name,idx,allowed,section in [('economic_resiliency',4,labels,None),('government_financial_strength',5,labels,'As a final step'),('rating_midpoint',5,'Aaa Aa1 Aa2 Aa3 A1 A2 A3 Baa1 Baa2 Baa3 Ba1 Ba2 Ba3 B1 B2 B3 Caa1 Caa2 Caa3 Ca C'.split(),'Government Financial Strength')]:
 t=texts[idx]
 if name=='government_financial_strength':t=t.split(section)[0]
 if name=='rating_midpoint':t=t.rsplit(section,1)[1]
 rows=[]
 for line in t.splitlines():
  parts=line.split()
  if len(parts)>=16 and parts[0] in labels and all(v in allowed for v in parts[1:16]):rows.append(parts[:16])
 assert len(rows)==15,(name,len(rows));assert len(set(r[0] for r in rows))==15
 with (b/(name+'.csv')).open('w') as f:
  w=csv.writer(f);w.writerow(['row']+labels);w.writerows(rows)
 with (b.parent/'methodology'/('esm2013_'+name+'.csv')).open() as f:old={r[0]:r[1:] for r in list(csv.reader(f))[1:]}
 changes=[(r[0],labels[j],old[r[0]][j],v) for r in rows for j,v in enumerate(r[1:]) if old[r[0]][j]!=v]
 checks.append(dict(matrix=name,source_page=idx+1,cells=225,differences_from_esm=changes));print(checks[-1],flush=True)
(b/'matrix_verification.json').write_text(json.dumps(checks,indent=2))
for name in ['ida_fy12','ida_fy13']:
 if (s/(name+'.pdf')).exists():
  with pdfplumber.open(s/(name+'.pdf')) as p:(s/(name+'.txt')).write_text('\n\f\n'.join(a.extract_text() or '' for a in p.pages))
