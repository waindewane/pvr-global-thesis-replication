#!/usr/bin/env python3
"""Fetch the complete private snapshot, restore dependencies and reproduce the thesis."""
from pathlib import Path
import argparse,hashlib,io,json,os,shutil,subprocess,tarfile
ROOT=Path(__file__).resolve().parent
WORK=ROOT/'workspace'
REPO='waindewane/pvr-global-thesis-replication'
TAG='thesis-2026-09-22'
def run(args,cwd=ROOT):
 env=os.environ.copy();env['PYTHONPATH']=str(ROOT/'tools/python-packages')+os.pathsep+env.get('PYTHONPATH','')
 subprocess.run([str(a) for a in args],cwd=cwd,env=env,check=True)
def sha(p):
 h=hashlib.sha256()
 with p.open('rb') as f:
  for b in iter(lambda:f.read(1024*1024),b''):h.update(b)
 return h.hexdigest()
class Parts(io.RawIOBase):
 def __init__(self,paths):self.paths=iter(paths);self.f=None
 def read(self,n=-1):
  if n<0:raise ValueError('Streaming reads require a bounded length')
  blocks=[];left=n
  while left:
   if self.f is None:
    try:self.f=next(self.paths).open('rb')
    except StopIteration:break
   b=self.f.read(left)
   if not b:self.f.close();self.f=None;continue
   blocks.append(b);left-=len(b)
  return b''.join(blocks)
def prepare():
 if (ROOT/'prepared.json').exists():
  state=json.loads((ROOT/'prepared.json').read_text())
  if state['workspace']!=str(WORK):raise SystemExit('Workspace moved: remove prepared.json and run prepare again.')
  print('Snapshot already prepared.');return
 assets=json.loads((ROOT/'release_assets.json').read_text());downloads=ROOT/'downloads';downloads.mkdir(exist_ok=True)
 files=[]
 for row in assets['parts']+assets.get('extras',[]):
  p=downloads/row['name']
  if not p.exists():run(['gh','release','download',TAG,'--repo',REPO,'--pattern',p.name,'--dir',downloads])
  if sha(p)!=row['sha256']:raise SystemExit('Asset hash mismatch: '+p.name)
  files.append(p)
 print('Verified all archive parts. Extracting complete snapshot…',flush=True)
 groups=[files[:len(assets['parts'])]]+[[p] for p in files[len(assets['parts']):]]
 for group in groups:
  with tarfile.open(fileobj=Parts(group),mode='r|gz') as t:
   for m in t:
    target=(ROOT/m.name).resolve()
    if not target.is_relative_to(WORK.resolve()) or not (m.isfile() or m.isdir()):raise SystemExit('Unsafe archive member: '+m.name)
    if m.isdir():target.mkdir(parents=True,exist_ok=True);continue
    target.parent.mkdir(parents=True,exist_ok=True)
    with t.extractfile(m) as src,target.open('wb') as dst:shutil.copyfileobj(src,dst)
 for row in json.loads((ROOT/'file_manifest.json').read_text()):
  if sha(WORK/row['path'])!=row['sha256']:raise SystemExit('Snapshot file hash mismatch: '+row['path'])
 run(['python3',ROOT/'tools/relocate.py',WORK,assets['archive_workspace']])
 (ROOT/'prepared.json').write_text(json.dumps({'workspace':str(WORK),'snapshot':TAG},indent=2)+'\n')
 print('Complete input snapshot ready.')
def restore():
 prepare()
 run(['Rscript','--vanilla',ROOT/'tools/restore.R'],cwd=WORK)
 run(['python3','-m','pip','install','--target',ROOT/'tools/python-packages','-r',WORK/'requirements-p15.txt'])
def calculations():
 run(['Rscript','--vanilla',ROOT/'tools/check_inputs.R'],cwd=WORK)
 run(['Rscript','--vanilla','-e',"source('scripts/p15/activate_p15_environment.R'); targets::tar_make(script='../tools/replication_targets.R')"],cwd=WORK)
def displays():run(['Rscript','--vanilla',ROOT/'tools/build_displays.R'],cwd=WORK)
def pdf():run(['python3',WORK/'docs/thesis_design/manuscript/build_review.py'],cwd=WORK)
def verify():run(['Rscript','--vanilla',ROOT/'tools/verify_results.R'],cwd=WORK)
if __name__=='__main__':
 p=argparse.ArgumentParser(description=__doc__);p.add_argument('command',choices=['prepare','restore','run','displays','pdf','verify','all']);a=p.parse_args()
 if a.command=='prepare':prepare()
 elif a.command=='restore':restore()
 else:
  prepare()
  if a.command in ('run','all'):calculations()
  if a.command in ('verify','all'):verify()
  if a.command in ('displays','all'):displays()
  if a.command in ('pdf','all'):pdf()
