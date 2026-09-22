from pathlib import Path
import json, hashlib, shutil, base64, datetime
from urllib.parse import urljoin
from bs4 import BeautifulSoup
from PIL import Image
from reportlab.pdfgen import canvas
from reportlab.lib.utils import ImageReader
root=Path('/Users/waindewane/Claude and Codex/Miscellaneous/pvr-global/exports/thesis-replication-20260922/workspace')
exp=root/'experiments/p15_wb_scorecard_20260912/source_intake_20260912'
work=Path('work/wb_scorecard_note/received_sources')
out=Path('outputs/moodys_source_capture')
imgs=exp/'sources/default_report_images';imgs.mkdir(exist_ok=True)
meta=json.loads((work/'default_report_browser_capture.json').read_text())
manifest=[]
for f in meta['figures']:
 imeta=f['images'][0];old=Path('/Users/waindewane/Downloads')/Path(imeta['src']).name
 assert old.exists(),old
 name=f"image_{f['index']+1:02d}.png"
 shutil.copy2(old,imgs/name)
 with Image.open(old) as image:
  image.load();w,h=image.size
 manifest.append({'image_index':f['index']+1,'filename':name,'caption':f['text'],'alt':imeta['alt'],'source_url':urljoin('https://www.moodys.com',imeta['src']),'source_relative_url':imeta['src'],'bytes':old.stat().st_size,'sha256':hashlib.sha256(old.read_bytes()).hexdigest(),'width':w,'height':h})
assert len(manifest)==28
(imgs/'manifest.json').write_text(json.dumps(manifest,indent=2))
soup=BeautifulSoup((work/'default_report_full_dom.html').read_text(),'html.parser')
for n in soup.find_all('button'):
 if n.get_text(strip=True).isdigit():n.unwrap()
 else:n.decompose()
for n in soup.find_all(['script','style','svg','iframe','input']):n.decompose()
lookup={m['source_relative_url']:m for m in manifest}
for n in soup.find_all(True):
 for a in list(n.attrs):
  if a not in ['href','src','alt','colspan','rowspan','id']:del n.attrs[a]
 if n.name=='a' and n.get('href'):n['href']=urljoin('https://www.moodys.com',n['href'])
 if n.name=='img':
  m=lookup[n['src']];n['src']='data:image/png;base64,'+base64.b64encode((imgs/m['filename']).read_bytes()).decode()
style='''body{font:17px/1.6 Georgia,serif;max-width:1100px;margin:40px auto;padding:0 24px;color:#17212b;background:white}h1,h2,h3,h4,h5{font-family:Arial,sans-serif;line-height:1.25}h2{margin-top:2em}h5{font-size:17px}img{max-width:100%;height:auto;background:white}figure{margin:24px 0}table{width:100%;table-layout:fixed}td{vertical-align:top;padding:8px}a{color:#185c91}.capture-note{padding:16px;background:#edf4fa;border-left:4px solid #185c91;font:15px/1.5 Arial,sans-serif}'''
html='<!doctype html><html lang="en"><head><meta charset="utf-8"><title>Moody’s sovereign default report — offline capture</title><style>'+style+'</style></head><body><h1>Sovereign default and recovery rates, 1983–2023</h1><p class="capture-note">Moody’s Ratings, 11 April 2024. Private offline research capture saved on 12 September 2026 from the user’s accessible report. All 28 report images are embedded; this is a local rendering, not a publisher-supplied PDF. External reference links still require internet access. The separate Excel supplement was not acquired.</p>'+str(soup)+'</body></html>'
(out/'moodys_default_report_offline.html').write_text(html)
# A faithful white-background PDF of the source images supports inspection of tables.
c=canvas.Canvas(str(out/'moodys_default_report_figures.pdf'))
c.setTitle('Moody’s sovereign default report: captured figures and appendix tables')
for m in manifest:
 w,h=m['width'],m['height'];scale=560/w;pw=608;ph=h*scale+66
 c.setPageSize((pw,ph));c.setFillColorRGB(1,1,1);c.rect(0,0,pw,ph,fill=1,stroke=0)
 c.setFillColorRGB(.1,.15,.2);c.setFont('Helvetica',9)
 label=f"Exhibit {m['image_index']}" if m['image_index']<=20 else f"Appendix image {m['image_index']}"
 c.drawString(24,ph-20,'Moody\'s sovereign default study | '+label)
 c.drawImage(ImageReader(str(imgs/m['filename'])),24,30,width=560,height=h*scale,mask='auto')
 c.setFont('Helvetica',8);c.drawString(24,14,'Captured source image; see the complete report for captions and notes.');c.showPage()
c.save()
receipt={'retrieved_utc':datetime.datetime.now(datetime.timezone.utc).isoformat(),'images_verified':28,'images_expected':28,'all_images_decode':True,'offline_html_images_embedded':28,'offline_html_external_image_dependencies':0,'excel_supplement_acquired':False,'production_changed':False,'source_text_characters':len((work/'default_report_full_text.txt').read_text()),'manifest_file':'sources/default_report_images/manifest.json','offline_html_sha256':hashlib.sha256((out/'moodys_default_report_offline.html').read_bytes()).hexdigest(),'files_total_bytes':sum(m['bytes'] for m in manifest)}
(exp/'image_retry_receipt.json').write_text(json.dumps(receipt,indent=2))
print(json.dumps(receipt,indent=2))
