#!/usr/bin/env python3
"""Rebase archived path metadata, then refresh only provenance hashes.
Never alters rates, repayment terms, inclusion choices or numerical formulas.
Run on a disposable replication workspace, never on the research source tree.
"""
from pathlib import Path
import csv,gzip,hashlib,io,json,sys
root=Path(sys.argv[1]).resolve();old=sys.argv[2].rstrip('/');changed=set()
index=root.parent/'file_manifest.json'
paths=root.parent/'package_paths.json'
selected=[root/x['path'] for x in json.loads(index.read_text())] if index.exists() else [root/x for x in json.loads(paths.read_text())]
files=[p for p in selected if p.is_file() and p.suffix in ('.R','.py','.csv','.json','.md','.txt','.gz')]
for p in files:
 try:s=gzip.decompress(p.read_bytes()).decode() if p.suffix=='.gz' else p.read_text()
 except UnicodeError:continue
 t=s.replace(str(root),'__REPLICATION_NEW_ROOT__').replace(old,str(root)).replace('__REPLICATION_NEW_ROOT__',str(root))
 if t!=s:
  if p.suffix=='.gz':p.write_bytes(gzip.compress(t.encode(),mtime=0))
  else:p.write_text(t)
  changed.add(str(p.relative_to(root)))
def sha(p):return hashlib.sha256(p.read_bytes()).hexdigest()
for iteration in range(15):
 updates=0
 for p in files:
  if p.suffix=='.csv' and any(x in p.name for x in ['manifest','snapshot','receipt']):
   try:
    s=p.read_text();f=io.StringIO(s);rd=csv.DictReader(f);fields=rd.fieldnames or []
    pathkey=next((k for k in ['artifact_path','path','local_path'] if k in fields),None)
    if pathkey is None or 'sha256' not in fields:continue
    rows=list(rd);dirty=False
    for row in rows:
     q=Path(row[pathkey]);q=q if q.is_absolute() else root/q
     if q.is_file() and q!=p and q.is_relative_to(root):
      h=sha(q)
      if row['sha256']!=h:row['sha256']=h;dirty=True
    if dirty:
     z=io.StringIO(newline='');w=csv.DictWriter(z,fieldnames=fields);w.writeheader();w.writerows(rows);p.write_text(z.getvalue());updates+=1;changed.add(str(p.relative_to(root)))
   except (ValueError,UnicodeError,csv.Error):pass
  elif p.suffix=='.json' and p.parent.name=='config':
   x=json.loads(p.read_text());dirty=[False]
   def walk(a):
    if isinstance(a,dict):
     if 'path' in a and 'sha256' in a and isinstance(a['path'],str):
      q=Path(a['path']);q=q if q.is_absolute() else root/q
      if q.is_file() and q!=p:
       h=sha(q)
       if a['sha256']!=h:a['sha256']=h;dirty[0]=True
     for v in a.values():walk(v)
    elif isinstance(a,list):
     for v in a:walk(v)
   walk(x)
   if dirty[0]:p.write_text(json.dumps(x,indent=2)+'\n');updates+=1;changed.add(str(p.relative_to(root)))
 if not updates:break
(root/'relocation_record.json').write_text(json.dumps({'provenance_only':True,'old_root':old,'new_root':str(root),'iterations':iteration+1,'changed':sorted(changed)},indent=2)+'\n')
print('Rebased metadata in',len(changed),'files; last pass updated',updates)
