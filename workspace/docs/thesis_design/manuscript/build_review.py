#!/usr/bin/env python3
"""Build the complete supervisor-review copy without accepting new prose."""
from pathlib import Path
import datetime,hashlib,json,re,subprocess,sys
from build_draft import polish_tex
BASE=Path(__file__).resolve().parent
ROOT=BASE.parents[2]
OUT=BASE/'complete_review'
OUT.mkdir(exist_ok=True)
def sha(p): return hashlib.sha256(p.read_bytes()).hexdigest()
def run(args,**kw): return subprocess.run(args,check=True,text=True,**kw)
accepted=json.loads((BASE/'accepted_sections.json').read_text())
review=json.loads((BASE/'review_sections.json').read_text())
sections=accepted['sections']+review['sections']
sections.sort(key=lambda x:x['chapter'])
chapters=accepted['chapters']|review['chapters']
accepted_ref_keys=set(); refs={}; variants={}; blocks=[]; records=[]; figures=set(); chapter=None
for s in sections:
 p=ROOT/s['path']
 if 'accepted_sha256' in s and sha(p)!=s['accepted_sha256']:
  raise SystemExit('Accepted source changed without approval: '+s['path'])
 records.append({'path':s['path'],'sha256':sha(p),'status':'accepted' if 'accepted_sha256' in s else 'review'})
 text=p.read_text();notes=[]
 def note(m): notes.append(m[0]);return ''
 text=re.sub(r'^\[\^[^\]]+\]:[^\n]*(?:\n[ \t]+[^\n]+)*',note,text,flags=re.M)
 parts=re.split(r'\n#+ References[^\n]*\n',text,maxsplit=1)
 if len(parts)>1:
  for ref in re.split(r'\n\s*\n',parts[1].strip()):
   if not ref.strip():continue
   key=re.match(r'^(.+?\. (?:\d{4}|n\.d\.)(?:-[a-z])?)(?:\.| )',ref)
   if not key:raise ValueError(ref)
   key=key[1]
   variants.setdefault(key,[]).append({'source':s['path'],'entry':ref})
   # Prefer accepted source's bibliography over an introduction duplicate.
   if key not in refs or ('accepted_sha256' in s and key not in accepted_ref_keys):refs[key]=ref.replace(r'\[DGS7\]', '[DGS7]')
   if 'accepted_sha256' in s:accepted_ref_keys.add(key)
 if chapter!=s['chapter']:
  chapter=s['chapter']
  prefix='\\clearpage\n\\pagenumbering{arabic}\n' if chapter==1 else '\\Needspace{8\\baselineskip}\n'
  blocks.append(prefix+'\\setcounter{section}{'+str(chapter-1)+'}\n\n# '+chapters[str(chapter)]+'\n')
 body=re.sub(r'^# \d+\. [^\n]+\n','',parts[0])
 body=re.sub(r'^(##)\s+\d+\.\d+\s+',r'\1 ',body,flags=re.M)
 lines=body.splitlines();out=[];i=0
 while i<len(lines):
  line=lines[i]
  if line.startswith('**') and line.endswith('**') and i+2<len(lines) and lines[i+2].startswith('|'):
   out+=['Table: '+re.sub(r'^Table(?:\s+\d+)?[.:]?\s*','',line[2:-2]),''];i+=2;continue
  out.append(line);i+=1
 body='\n'.join(out)
 def fig(m):
  loc=m[2].strip('<>');fp=Path(loc) if Path(loc).is_absolute() else p.parent/loc
  fp=fp.resolve();figures.add(fp)
  return '!['+m[1]+']('+str(fp.relative_to(ROOT)).replace(' ','%20')+'){width=100%}'
 body=re.sub(r'!\[([^\]]+)\]\((<[^>]+>|[^)]+)\)',fig,body)
 # Uniform author-date punctuation; sources/claim content remain unchanged.
 body=re.sub(r'\((World Bank), (n\.d\.-[abc]|2025)',r'(\1 \2',body)
 blocks.append(body.strip()+'\n\n'+'\n\n'.join(notes))
abstract=ROOT/review['abstract']
front='\\clearpage\n\\phantomsection\\addcontentsline{toc}{section}{List of tables}\n\\listoftables\n\\clearpage\n\\phantomsection\\addcontentsline{toc}{section}{List of figures}\n\\listoffigures\n\\clearpage\n\n# Abstract {.unnumbered}\n\n'+abstract.read_text()+'\n\\clearpage\n\n'+(BASE/'abbreviations.md').read_text()+'\n'
body=front+'\n\n'.join(blocks)
body=body.replace('(Damodaran, n.d.)','(Damodaran n.d.)').replace('(Pinheiro and Bates, n.d.)','(Pinheiro and Bates n.d.)').replace('(World Bank n.d.-b, 2025)','(World Bank n.d.-b; World Bank 2025)').replace('(OECD n.d.-c; n.d.-e)','(OECD n.d.-c; OECD n.d.-e)')
for key in refs:
 if key.startswith('Morris, Scott,') and 'morris-parks-gardner-china-world-bank-data-code.zip' not in refs[key]:
  refs[key]+=' [Data and replication code](https://www.cgdev.org/sites/default/files/morris-parks-gardner-china-world-bank-data-code.zip).'
body+='\n\n\\clearpage\n\n# References {.unnumbered}\n\n\\begingroup\n\\small\n\\setstretch{1.15}\n\\setlength{\\parskip}{3pt}\n\n'+'\n\n'.join(refs[k] for k in sorted(refs,key=str.casefold))+'\n\n\\endgroup\n'
md=OUT/'Thesis_Review.md';md.write_text(body)
meta={'author':accepted['author'],'date':'','geometry':['a4paper','margin=1in'],'linestretch':1.5,'fontsize':'10pt','documentclass':'article','toc':True,'toc-title':'Contents','colorlinks':False}
(OUT/'metadata.json').write_text(json.dumps(meta,indent=2)+'\n')
header=(BASE/'header.tex').read_text().replace('\\setcounter{section}{1}','\\setcounter{section}{0}')
header+='''
\\usepackage{fancyhdr}
\\pagestyle{fancy}
\\fancyhf{}
\\fancyfoot[R]{\\thepage}
\\renewcommand{\\headrulewidth}{0pt}
\\fancypagestyle{plain}{\\fancyhf{}\\fancyfoot[R]{\\thepage}\\renewcommand{\\headrulewidth}{0pt}}
\\makeatletter
\\renewcommand\\footnotesize{\\@setfontsize\\footnotesize{10}{12}}
\\AtBeginEnvironment{footnote}{\\setstretch{1}}
\\makeatother
\\brokenpenalty=10000
\\usepackage{pdfpages}
\\AtBeginDocument{\\hypersetup{pdftitle={Measuring Borrower-Side Concessionality: Constructing a Sovereign Market-Benchmark Dataset, 2012--2024}}}
'''
(OUT/'header.tex').write_text(header)
cover=ROOT/review['cover']
if not cover.exists():raise SystemExit('Official cover not ready: '+str(cover))
before='\\includepdf[pages=1,pagecommand={\\thispagestyle{empty}}]{'+str(cover)+'}\n\\pagenumbering{roman}\n'
(OUT/'front.tex').write_text(before)
tex=OUT/'Thesis_Review.tex'
run(['pandoc',str(md),'-f','markdown+tex_math_single_backslash','-t','latex','--standalone','--number-sections','--metadata-file',str(OUT/'metadata.json'),'--include-in-header',str(OUT/'header.tex'),'--include-before-body',str(OUT/'front.tex'),'-o',str(tex)],cwd=ROOT)
polish_tex(tex)
rendered=tex.read_text()
# Short list entries retain the full, source-attributed captions in the body.
rendered=rendered.replace(r'\caption{Source of the selected borrowing-rate reference by year.',r'\caption[Source of the selected borrowing-rate reference by year]{Source of the selected borrowing-rate reference by year.')
rendered=re.sub(r'\\caption\{(Annual differences between borrower-specific and\s+standardized grant elements\.)',lambda m:r'\caption[Annual differences between borrower-specific and standardized grant elements]{'+m[1],rendered)
rendered=rendered.replace('\\[','\\begin{equation}').replace('\\]','\\end{equation}')
tex.write_text(rendered)
with (OUT/'compile_stdout.txt').open('w') as f:
 for _ in range(3):run(['xelatex','-interaction=nonstopmode','-halt-on-error','-output-directory',str(OUT),str(tex)],cwd=ROOT,stdout=f,stderr=subprocess.STDOUT)
audit={'built_utc':datetime.datetime.now(datetime.timezone.utc).isoformat(),'purpose':'Complete supervisor-review copy. Review prose is not implicitly accepted.','sources':records,'abstract':{'path':str(abstract.relative_to(ROOT)),'sha256':sha(abstract)},'cover':{'path':str(cover.relative_to(ROOT)),'sha256':sha(cover)},'figures':[{'path':str(p.relative_to(ROOT)),'sha256':sha(p)} for p in sorted(figures)],'reference_count':len(refs),'assembly':{str(p.relative_to(ROOT)):sha(p) for p in [BASE/'build_review.py',BASE/'build_draft.py',BASE/'review_sections.json',BASE/'header.tex',BASE/'abbreviations.md',BASE/'accepted_sections.json']},'outputs':{p.name:sha(p) for p in [md,tex,OUT/'Thesis_Review.pdf']}}
(OUT/'build_manifest.json').write_text(json.dumps(audit,indent=2)+'\n')
(OUT/'reference_variants.json').write_text(json.dumps(variants,indent=2,ensure_ascii=False)+'\n')
print('Built complete review:',len(records),'sources;',len(refs),'references;',OUT/'Thesis_Review.pdf')
