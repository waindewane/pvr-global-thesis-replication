"""Exact source extraction and explicitly curated historical eligibility; no model fitting."""
from pathlib import Path
import csv,json,openpyxl
b=Path(__file__).resolve().parent
# World Bank FY12 annex, printed p10: all 81 IDA-only/blend entries, irrespective of income.
africa='AGO BEN BFA BDI CPV CMR CAF TCD COM COD COG CIV ETH ERI GMB GHA GIN GNB KEN LSO LBR MDG MWI MLI MRT MOZ NER NGA RWA STP SEN SLE SOM SDN TZA TGO UGA ZMB ZWE'.split()
eap='KHM KIR LAO MHL FSM MNG MMR PNG WSM SLB TLS TON TUV VUT VNM'.split()
eca='ARM BIH GEO XKX KGZ MDA TJK UZB'.split()
lac='BOL DMA GRD GUY HTI HND NIC LCA VCT'.split()
mena='DJI YEM'.split();sa='AFG BGD BTN IND MDV NPL PAK LKA'.split()
ida=set(africa+eap+eca+lac+mena+sa);assert len(ida)==81
hipc=set('AFG BEN BOL BFA BDI CMR CAF TCD COM COG COD CIV ETH GMB GHA GIN GNB GUY HTI HND LBR MDG MWI MLI MRT MOZ NIC NER RWA STP SEN SLE TZA TGO UGA ZMB SOM SDN ERI'.split());assert len(hipc)==39
# Changes take effect during the named calendar year; all grades apply January 1 next year.
grads={'AGO':2014,'ARM':2014,'BIH':2014,'GEO':2014,'IND':2014,'BOL':2017,'LKA':2017,'VNM':2017,'MDA':2020,'MNG':2020}
entries={'SSD':2012,'SYR':2016,'FJI':2019}
rows=list(csv.DictReader((b.parent/'scorecard_inputs.csv').open()))
flags=[]
for r in rows:
 i=r['iso3'];t=int(r['reference_year']);active=i in ida
 if i in grads and t>=grads[i]:active=False
 if i in entries and t>=entries[i]:active=True
 if i=='LKA' and t>=2022:active=True
 flags.append(dict(iso3=i,analysis_year=r['analysis_year'],reference_year=t,ida=active,hipc=i in hipc,
   concessional_weight_exception=active or i in hipc,source='WB FY12/FY13 eligibility annexes; IDA graduates and dated reentry notices; IMF HIPC 2023 roster',
   historical_status_basis='year-end eligibility, retrospective reconstruction; HIPC eligible roster treated as structural category'))
with (b/'historical_eligibility.csv').open('w') as f:
 w=csv.DictWriter(f,fieldnames=flags[0]);w.writeheader();w.writerows(flags)
w=openpyxl.load_workbook(b/'sources/wef_gci_2019.xlsx',read_only=True,data_only=True)
rs=list(w['Data'].iter_rows(values_only=True));isos=rs[2];gci=[]
for r in rs:
 if r[2]=='GCI4' and r[9]=='SCORE':
  for i,v in zip(isos[10:],r[10:]):
   if not isinstance(i,str) or len(i)!=3 or i in ['LIC','LMC','UMC','HIC','AVG']:continue
   try:v=float(v)
   except (ValueError,TypeError):continue
   gci.append(dict(iso3=i,year=int(str(r[1])[:4]),gci4=v,first_publication_year=2018 if str(r[1]).startswith('2017') else int(r[1]),
    note='2017 backcast released 2018; GCI4 is not a directly comparable legacy GCI'))
with (b/'gci4_2017_2019.csv').open('w') as f:
 w=csv.DictWriter(f,fieldnames=gci[0]);w.writeheader();w.writerows(gci)
print('IDA FY12 countries',len(ida),'GCI4 numeric country observations',len(gci))
