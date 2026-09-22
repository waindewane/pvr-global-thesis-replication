from pathlib import Path
import csv,json,hashlib,datetime,sys,subprocess
b=Path(__file__).resolve().parent;repo=b.parents[2]
def sha(p):
 h=hashlib.sha256()
 with p.open('rb') as f:
  for a in iter(lambda:f.read(1024*1024),b''):h.update(a)
 return h.hexdigest()
def manifest(paths,name,role):
 rows=[dict(path=str(p.relative_to(repo)),role=role,bytes=p.stat().st_size,sha256=sha(p)) for p in sorted(set(paths))]
 with (b/name).open('w') as f:
  w=csv.DictWriter(f,fieldnames=['path','role','bytes','sha256']);w.writeheader();w.writerows(rows)
 return rows
# Check every immutable production/seed input against the pre-run receipt.
for r in csv.DictReader((b/'peer_final/production_input_manifest.csv').open()):assert sha(repo/r['path'])==r['sha256'],r['path']
# No stale completion, source omission or incorrect headline output.
rules={r['method']:r for r in csv.DictReader((b/'report_rules.csv').open())}
cover={r['method']:r for r in csv.DictReader((b/'report_coverage.csv').open())}
for m,r in rules.items():assert sum(int(r.get(str(i),0)) for i in range(1,9))==int(cover[m]['covered'])
assert int(cover['PIS__wgi_bounded__stop5__guaranteed_interval']['covered'])==638
assert int(cover['PISR__wgi_bounded__stop5__guaranteed_interval']['covered'])==732
for p in [b/'scorecard_checks.csv',b/'matching_checks.csv',b/'peer_final/checks.csv']:
 assert all(r['passed'].lower()=='true' for r in csv.DictReader(p.open()))
inputs=[b.parent/'scorecard_inputs.csv',repo/'experiments/p15_peer_rating_expansion_20260912/model_feature_panel.csv',
 b.parent/'supplemental/wef_legacy_gci_long.csv',b.parent/'source_intake_20260912/sources/moodys_sovereign_bond_ratings_2018.pdf',
 b.parent/'source_intake_20260912/sources/default_report_images/image_02.png',
 repo/'experiments/p15_rating_coverage_shadow_20260912/sources/wb2021_wps9649.pdf']
inputs+=list((b/'sources').glob('*'))
inputs += [b/n for n in ['historical_eligibility.csv','historical_eligibility_sources.csv','qpsd_fc_share.csv','gci4_2017_2019.csv','default_events.csv','economic_resiliency.csv','government_financial_strength.csv','rating_midpoint.csv']]
manifest(inputs,'source_input_manifest.csv','preserved_or_curated_research_input')
manifest(list(b.glob('*.R'))+list(b.glob('*.py'))+[repo/'renv.lock'],'code_manifest.csv','experiment_code_and_environment')
files=[p for p in b.iterdir() if p.is_file() and p.suffix in ['.csv','.md','.json','.txt'] and p.name not in ['output_manifest.csv','run_manifest.json']]
files += [p for p in (b/'peer_final').iterdir() if p.is_file()]
manifest(files,'output_manifest.csv','conditional_research_output')
ptr=json.load((repo/'data-derived/p15_master/current_run.json').open())
r=dict(schema_version='P15-CONDITIONAL-SCORECARD-CONSTRUCTED-V1',run_id='p15_wb_scorecard_20260912_constructed_v1',
 authority='diagnostic_owner_authorized_exploration_not_promoted',completed_utc=datetime.datetime.now(datetime.timezone.utc).isoformat(),
 production_pointer=ptr,production_unchanged_receipt='peer_final/production_preservation_check.csv',
 quantitative_scorecard_implemented=True,exact_authors_dataset_replication=False,scenario_count=5,peer_designs=34,
 reporting_denominator_current_lmic_peers=778,source_input_manifest='source_input_manifest.csv',code_manifest='code_manifest.csv',
 output_manifest='output_manifest.csv',output_manifest_sha256=sha(b/'output_manifest.csv'),
 python_version=sys.version,r_environment='r_session_info.txt',
 assumptions=['moderate event risk','conditional judgments','revised public data','source-labelled fiscal proxies','input ranges are not prediction intervals'],
 instruction_scope='construct isolated scorecard; explore rating-implied donors and matching; no sovereign-rating regression or production promotion')
(b/'run_manifest.json').write_text(json.dumps(r,indent=2))
print('Final source/code/output manifests saved; all checks and headline totals reconcile; production hashes unchanged.')
