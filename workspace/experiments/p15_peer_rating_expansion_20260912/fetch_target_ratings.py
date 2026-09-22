import sys,json,concurrent.futures,csv,re
from pathlib import Path
from lxml import html
sys.path.insert(0,str(Path(__file__).parent))
from fetch_new import root,fetch
links=list(csv.DictReader((root/'countryeconomy_target_links.csv').open()))
tasks={'countryeconomy_'+r['iso3']+'.html':r['url'] for r in links}
with concurrent.futures.ThreadPoolExecutor(max_workers=3) as ex:results=list(ex.map(fetch,tasks.items()))
(root/'countryeconomy_source_manifest.json').write_text(json.dumps(results,indent=2))
rows=[];counts=[]
for r in links:
 p=root/'sources'/('countryeconomy_'+r['iso3']+'.html')
 if not p.exists():continue
 doc=html.fromstring(p.read_bytes())
 tables=doc.xpath('//table[starts-with(@id,"tb0_")]')
 n=0
 for table in tables:
  # Page structure explicitly places Moody's long-term foreign-currency series
  # first. Verify table headers; later currency/type columns are never pooled.
  headers=' '.join(table.xpath('./thead//text()'))
  assert 'Long term Rating' in headers and 'Foreign currency' in headers
  assert table.xpath('ancestor::div[@id="moodys"]'), 'Rating table must belong to Moody agency tab'
  for i,tr in enumerate(table.xpath('./tbody/tr'),1):
   cells=tr.xpath('./td')
   if len(cells)<2:continue
   date=' '.join(cells[0].itertext()).strip();value=' '.join(cells[1].itertext()).strip()
   rating=re.match(r'^(Aaa|Aa[123]|A[123]|Baa[123]|Ba[123]|B[123]|Caa[123]|Ca|C|WR|NR)(?:\s|$)',value)
   grade=rating.group(1) if rating else ''
   if grade and re.match(r'^\d{4}-\d{2}-\d{2}$',date) and not date.startswith('0000'):
    rows.append(dict(iso3=r['iso3'],country=r['country'],event_date=date,rating=grade,raw_text=value,source_url=r['url'],source_table=table.get('id'),source_row=i));n+=1
 counts.append(dict(iso3=r['iso3'],country=r['country'],moodys_dated_rows=n))
for name,data in [('countryeconomy_moody_events.csv',rows),('countryeconomy_parse_counts.csv',counts)]:
 with (root/name).open('w') as f:
  w=csv.DictWriter(f,fieldnames=list(data[0]));w.writeheader();w.writerows(data)
print('Countries',len(links),'events',len(rows),'errors',sum('error'in r for r in results))
