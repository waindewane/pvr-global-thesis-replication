#!/usr/bin/env python3
"""Assemble only explicitly accepted reader-facing sections; compile with XeLaTeX."""
from pathlib import Path
import argparse,datetime,hashlib,json,re,shutil,subprocess,sys
BASE=Path(__file__).resolve().parent
ROOT=BASE.parents[2]
MANIFEST=BASE/'accepted_sections.json'
def sha(p):return hashlib.sha256(p.read_bytes()).hexdigest()
def run(args,**kw):return subprocess.run(args,check=True,text=True,**kw)

def polish_tex(path):
 text=path.read_text().replace(r'\begin{figure}',r'\begin{figure}[H]')
 text=re.sub(r'\\textbf\{Figure\. ([^}]+)\}\s*\\begin\{figure\}\[H\](.*?)\\caption\{',lambda m:r'\begin{figure}[H]'+m[2]+r'\caption{'+m[1]+'. ',text,flags=re.S)
 def table(match):
  content=match[1]
  start=content.index('\\toprule')
  pre=content[:start]
  cap=re.search(r'\\caption\{(.*?)\}\\tabularnewline',pre,re.S)
  spec=pre[:cap.start()].strip() if cap else pre.strip()
  head=content[start:].split('\\endfirsthead')[0] if '\\endfirsthead' in content else content[start:].split('\\endhead')[0]
  body=content.split('\\endlastfoot',1)[1]
  if not cap: caption="Construction of Peru's 2024 primary borrowing-rate reference"
  else:caption=cap[1]
  n=len(re.findall(r'\\real\{',spec))
  # Compact tables may use natural-width l/r/c columns, with no width fractions.
  if n==0:widths=[]
  elif n==5 and 'Financing records with usable terms' in caption:widths=[.30,.15,.27,.13,.15]
  elif n==6:widths=[.27,.17,.14,.14,.14,.14]
  elif n==7 and 'Paired comparisons' in caption:widths=[.24,.12,.12,.13,.13,.13,.13]
  elif n==7:widths=[.13,.17,.14,.14,.14,.14,.14]
  elif n==4 and 'Sources and estimation' in caption:widths=[.16,.29,.26,.29]
  elif n==4 and 'Availability' in caption:widths=[.24,.25,.26,.25]
  elif n==3 and 'Rating-implied' in caption:widths=[.48,.20,.32]
  elif n==2:widths=[.16,.84]
  else:widths=[1/n]*n
  it=iter(widths);spec=re.sub(r'\\real\{[0-9.]+\}',lambda _:r'\real{'+str(next(it))+'}',spec)
  reserve = '\\Needspace{13\\baselineskip}\n' if caption=='Borrowing rates by income group, 2024' else ''
  return reserve+'\\begin{table}[H]\n\\centering\n\\small\\setstretch{1.05}\\setlength{\\tabcolsep}{4pt}\n\\caption{'+caption+'}\n\\begin{tabular}'+spec+'\n'+head+body+'\\bottomrule\n\\end{tabular}\n\\end{table}'
 text=re.sub(r'\\begin\{longtable\}\[\](.*?)\\end\{longtable\}',table,text,flags=re.S)
 # Keep the Chapter 6 sample table's explanatory note with its display.
 text=re.sub(r'(\\begin\{table\}\[H\](?:(?!\\end\{table\}).)*?\\caption\{Financing records with usable terms(?:(?!\\end\{table\}).)*?)\\end\{table\}\s*\n(\\emph\{Notes:\}.*?)(?=\n\s*\n)',
  lambda m:m[1]+'\\par\\medskip\n\\begin{minipage}{\\linewidth}\n\\normalsize\\setstretch{1.5}\\raggedright\n'+m[2]+'\n\\end{minipage}\n\\end{table}',text,flags=re.S)
 path.write_text(text)

def main():
 ap=argparse.ArgumentParser();ap.add_argument('--accept',action='append',default=[],help='Update approval hash for an existing section only after owner locks it.');ap.add_argument('--no-pdf',action='store_true');ap.add_argument('--add',help='New owner-accepted section path');ap.add_argument('--chapter',type=int);ap.add_argument('--chapter-title');args=ap.parse_args()
 m=json.loads(MANIFEST.read_text());known={s['path']:s for s in m['sections']}
 if args.add:
  if not args.chapter:raise SystemExit('--add requires --chapter')
  rel=str(Path(args.add).resolve().relative_to(ROOT))
  if rel in known:raise SystemExit('Already registered; use --accept')
  if str(args.chapter) not in m['chapters']:
   if not args.chapter_title:raise SystemExit('New chapter requires --chapter-title')
   m['chapters'][str(args.chapter)]=args.chapter_title
  m['sections'].append({'chapter':args.chapter,'path':rel,'accepted_sha256':sha(ROOT/rel)})
  m['subtitle']='Working thesis draft · Chapters '+str(min(int(c) for c in m['chapters']))+'–'+str(max(int(c) for c in m['chapters']))
 for name in args.accept:
  rel=str(Path(name).resolve().relative_to(ROOT))
  if rel not in known:raise SystemExit('New sections require explicit addition to accepted_sections.json: '+rel)
  known[rel]['accepted_sha256']=sha(ROOT/rel)
 if args.accept or args.add:MANIFEST.write_text(json.dumps(m,indent=2)+'\n')
 for s in m['sections']:
  if sha(ROOT/s['path'])!=s['accepted_sha256']:raise SystemExit('Accepted text changed. Review/lock it, then run --accept '+s['path'])
 blocks=[];refs={};record=[];figure_paths=set();chapter=None;tables=0
 for s in m['sections']:
  p=ROOT/s['path'];text=p.read_text();snapshot=BASE/'snapshots'/sha(p)/p.name;snapshot.parent.mkdir(parents=True,exist_ok=True);snapshot.write_bytes(p.read_bytes());record.append({'path':s['path'],'sha256':sha(p)})
  if chapter!=s['chapter']:
   chapter=s['chapter'];blocks.append(('\\clearpage\n\n' if chapter==2 else '\\Needspace{8\\baselineskip}\n\n')+'\\setcounter{section}{'+str(chapter-1)+'}\n\n# '+m['chapters'][str(chapter)]+'\n')
  # Preserve footnote definitions even when older source files put them below references.
  notes=[]
  def pullnote(match):notes.append(match.group(0));return ''
  text=re.sub(r'^\[\^[^\]]+\]:[^\n]*(?:\n[ \t]+[^\n]+)*',pullnote,text,flags=re.M)
  parts=re.split(r'\n#+ References[^\n]*\n',text,maxsplit=1)
  body=re.sub(r'^# \d+\. [^\n]+\n', '', parts[0])
  if len(parts)>1:
   for ref in re.split(r'\n\s*\n',parts[1].strip()):
    if not ref.strip():continue
    # Same author/year is one cited work in this draft. Existing n.d. suffixes stay fixed.
    key=re.match(r'^(.+?\. (?:\d{4}|n\.d\.)(?:-[a-z])?)(?:\.| )',ref)
    if not key:raise ValueError('Unrecognized reference: '+ref)
    refs.setdefault(key.group(1),ref.replace(r'\[DGS7\]', '[DGS7]'))
  body=re.sub(r'^(##)\s+\d+\.\d+\s+',r'\1 ',body,flags=re.M)
  # Format captions and table bodies without changing cell content.
  lines=body.splitlines();out=[];i=0
  while i<len(lines):
   line=lines[i]
   if line.startswith('**') and line.endswith('**') and i+2<len(lines) and lines[i+2].startswith('|'):
    title=re.sub(r'^Table(?:\s+\d+)?[.:]?\s*','',line[2:-2]);out.append('Table: '+title);out.append('');i+=2;continue
   out.append(line);i+=1
  body='\n'.join(out)
  # Uncaptioned pipe tables receive their preceding descriptive caption only; require manual review otherwise.
  figure_paths.update(Path(a).resolve() for a in re.findall(r'!\[[^]]+\]\(<([^>]+)>\)',body))
  body=re.sub(r'!\[([^]]+)\]\(<([^>]+)>\)',lambda a:'!['+a[1]+']('+str(Path(a[2]).relative_to(ROOT)).replace(' ','%20')+'){width=100%}',body)
  blocks.append(body.strip()+'\n\n'+'\n\n'.join(notes))
 blocks.append('\\clearpage\n\n# References {.unnumbered}\n\n\\begingroup\n\\small\n\\setstretch{1.15}\n\\setlength{\\parskip}{2pt}\n\n'+ '\n\n'.join(refs[k] for k in sorted(refs,key=str.casefold))+'\n\n\\endgroup')
 body='\n\n'.join(blocks)+'\n'
 metadata={'title':m['title'],'subtitle':m['subtitle'],'author':m['author'],'date':m['date'],'geometry':['a4paper','margin=1in'],'linestretch':1.5,'fontsize':'10pt','documentclass':'article','toc':True,'toc-title':'Contents','colorlinks':False}
 (BASE/'metadata.json').write_text(json.dumps(metadata,indent=2)+'\n')
 md=BASE/'thesis_draft.md';md.write_text(body)
 tex=BASE/'thesis_draft.tex'
 run(['pandoc',str(md),'-f','markdown+tex_math_single_backslash','-t','latex','--standalone','--number-sections','--metadata-file',str(BASE/'metadata.json'),'--include-in-header',str(BASE/'header.tex'),'-o',str(tex)],cwd=ROOT)
 polish_tex(tex)
 if not args.no_pdf:
  log=BASE/'build'/'compile_stdout.txt'
  with log.open('w') as f:
   for _ in range(2):run(['xelatex','-interaction=nonstopmode','-halt-on-error','-output-directory',str(BASE/'build'),str(tex)],cwd=ROOT,stdout=f,stderr=subprocess.STDOUT)
  shutil.copy2(BASE/'build'/'thesis_draft.pdf',BASE/'thesis_draft.pdf')
 audit={'assembly_inputs':{p.name:sha(p) for p in [MANIFEST,BASE/'header.tex',BASE/'build_draft.py']},'figure_inputs':[{'path':str(p.relative_to(ROOT)),'sha256':sha(p)} for p in sorted(figure_paths)],'built_utc':datetime.datetime.now(datetime.timezone.utc).isoformat(),'source_files':record,'reference_count':len(refs),'format_reference':{'path':m['format_reference'],'sha256':sha(ROOT/m['format_reference'])},'outputs':{x.name:sha(x) for x in [md,tex]+([] if args.no_pdf else [BASE/'thesis_draft.pdf'])}}
 (BASE/'build_manifest.json').write_text(json.dumps(audit,indent=2)+'\n')
 print('Built',len(record),'accepted files;',len(refs),'references;',BASE/'thesis_draft.pdf')
if __name__=='__main__':main()
