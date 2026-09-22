"""Package the completed, checked candidate as a readable report and tables."""
from pathlib import Path
import csv, shutil, sys
v=Path('experiments/p15_wb_scorecard_20260912/full_panel_v2')
p=v/'results'
def rows(name):
    with (p/name).open() as f: return list(csv.DictReader(f))
def table(headers,data):
    return '\n'.join(['| '+' | '.join(headers)+' |','| '+' | '.join(['---']*len(headers))+' |']+['| '+' | '.join(map(str,r))+' |' for r in data])
def f(x,d=2): return f'{float(x):,.{d}f}'
coverage=rows('coverage_summary.csv');paired=rows('paired_rate_summary.csv');cases=rows('country_year_comparison.csv')
rules=rows('rule_counts.csv');scope=['All country-years','LMIC country-years','Original 778 LMIC peer selections']
newrules={(r['scope'],r['rule']):r['country_years'] for r in rules if r['version']=='new'}
oldrules={(r['scope'],r['rule']):r['country_years'] for r in rules if r['version']=='old'}
meanings={r['rule']:r['meaning'] for r in rules}
annual=rows('paired_annual_change_summary.csv')
checks=rows('checks.csv');preserve=rows('preservation_check.csv')
assert all(r['passed']=='TRUE' for r in checks)
assert all(r['unchanged']=='TRUE' for r in preserve)
lines=['# Full-panel peer benchmark comparison',
'Completed isolated candidate, 12 September 2026. All quantities below are country-years, not numbers of countries. Rates and changes are in percentage points unless stated otherwise.',
'## What is now complete',
'The revised peer benchmark is calculated for every one of the 2,743 country-years, irrespective of the currently selected tier. It produces 2,668 available peer values. The historical low-/middle-income (LMIC) panel contains 1,756 country-years; 1,706 have a revised peer value. The preferred candidate still retains 731 of the original 778 LMIC peer selections. The calculation includes 1,552 usable comparison values for country-years currently using stronger tiers, including 967 in the LMIC scope. They were outside the earlier ordinary peer calculation; they are additional comparison values, not additional selected benchmarks.',
'All old peer values were read from the actual current-run core evidence and independently reproduced, including rules, donor counts and memberships. The old global fallback provided a peer value for every panel row. Consequently, the new method cannot expand availability relative to that baseline; it removes 75 weakly matched values.',
'## Coverage',
table(['Scope','Panel rows','Old peer available','New peer available','Lost','Gained'],[[r['scope'],r['country_years'],r['old_peer_available'],r['new_peer_available'],r['lost'],r['gained']] for r in coverage]),
'Availability and selection are deliberately separate. Nine available revised peer values remain ineligible for selection because existing ordinary-fallback status restrictions apply (eight are LMIC). All 1,555 stronger selected rates are unchanged. Hypothetical total selected-rate coverage is 2,662/2,743 globally, compared with 2,733 before, and 1,701/1,756 for LMICs, compared with 1,748 before. No currently unserved country-year gains an eligible selected rate.',
'Of the 75 lost peer values, 71 had been selected: 47 in LMIC country-years and 24 outside that scope. Three losses are unused comparison values for Sri Lanka 2012–2014, which retains its primary/IDS selections. The remaining loss is Venezuela 2024, whose peer value was already blocked from selection.',
'## Rules and implementation',
'The source priority is primary → IDS → secondary → existing eligible Moody’s-implied rate. Each donor contributes one rate; the target is excluded; at least three distinct countries are required. The benchmark is the median of the first qualifying group. Rules 1–5 are retained; Rules 6–8 are removed. Actual ratings take precedence over shadow intervals. Three notches applies to Rules 1–4 only. When an interval is involved, every possible grade pairing must be within three notches. Rule 5 uses income and region without a rating restriction.',
'The accepted interest adjustment is applied to targets and donors across the full panel: net interest is not treated as observed gross interest. Unknown gross-interest indicators span the full scoring range, retaining the published zero weights for IDA/HIPC. Rebuilding the older treatment exactly reproduces all 2,743 previous bounded-score rows; the conservative build exactly reproduces the prior 731-case diagnostic. The scorecard continues to provide 2,287 finite conditional intervals across the full panel.',
table(['Rule','Thesis criterion','New: full panel','New: LMIC panel','Old: selected LMIC peers','New: selected LMIC peers'],[[i,meanings[i],newrules[scope[0],i],newrules[scope[1],i],oldrules[scope[2],i],newrules[scope[2],i]] for i in map(str,range(1,9))]),
'The machine-readable rule mapping preserves every original `peer_pool_rule` label. The source-controlled alternative excluding rating-implied donors is also calculated across the full panel; with the same conservative interest treatment, it retains 634 of the 778 original LMIC peer selections. The earlier 638 figure used the less conservative interest treatment.',
'## Changes on identical country-years',
table(['Scope','Paired cases','Old mean rate (%)','New mean rate (%)','Mean change','Mean absolute change','Lower / unchanged / higher'],[[r['scope'],r['n'],f(r['old_mean_pct']),f(r['new_mean_pct']),f(r['mean_change_pp']),f(r['mean_absolute_change_pp']),f"{r['lower']} / {r['unchanged']} / {r['higher']}"] for r in paired]),
'These means use identical old/new observations. The small global mean change does not imply small individual changes: positive and negative changes offset. Among retained LMIC peer selections, the average rate rises from 5.98% to 7.04%. That is a material change for the later valuation exercise.',
'Within each year, the accompanying figure compares the same countries under both methods. The year-to-year table is stricter: it retains only countries with both methods available at both adjacent dates. Neither is a single fixed-country sample across all 13 years.',
'![Full-panel peer comparison](full_panel_comparison.png)',
table(['Year versus prior year','Matched LMIC countries','Old mean annual change','New mean annual change','Average direction reversed'],[[r['analysis_year'],r['n'],f(r['old_mean_change_pp']),f(r['new_mean_change_pp']),'Yes' if r['mean_direction_reversed']=='TRUE' else 'No'] for r in annual if r['scope']=='LMIC country-years']),
'The 2022 rise survives and becomes larger: +1.93 to +3.66 points across the same 129 LMIC countries. However, the average direction reverses in 2023 (−0.31 to +0.22) and 2024 (+1.27 to −0.44), as well as some earlier years. This is evidence of sensitivity in peer-rate patterns, not yet evidence that a selected-rate or valuation conclusion reverses.',
'Large changes are visible in the case file. For example, Belize, Ecuador and Suriname 2022 each move from a 5.60% peer reference to 16.47%; five of their six donors provide rating-implied rates. Their currently selected rating-implied tier is unchanged. These cases illustrate why an expanded donor pool must remain visibly model-assisted.',
'## Exact losses',
'Every country-year is listed in `coverage_changes.csv`; selected losses are separately listed in `lost_selected_peers.csv`. The following list groups the 47 LMIC selected losses by country:',
]
from collections import defaultdict
g=defaultdict(list)
for r in cases:
    if r['historical_lmic_reporting_scope']=='TRUE' and r['selected_tier']=='peer' and r['selected_peer_new']=='FALSE':g[r['country']].append(r['analysis_year'])
lines += [table(['Country','Years'],[[k,', '.join(sorted(val))] for k,val in sorted(g.items())]),
'## Validation, preservation and remaining work',
'All 16 build checks pass, including complete old-value replay, prior conservative-diagnostic replay, target exclusion, donor uniqueness, minimum group size, Rule 1–5 restriction, and unchanged stronger selections. The separate pipeline completed successfully. Hash checks confirm that all 1,138 enumerated baseline code and empirical files remain unchanged. Earlier outputs are preserved in their versioned locations; previous scoring/peer code is also copied into the candidate snapshot. This is not a new full-project backup.',
'Validation with hidden target ratings remains separate from ordinary peer values for rated countries. Its cases and same-case error comparison are included. It does not establish accuracy for every unrated low-income country. The rate-valued donor pool uses existing eligible rating-implied rates; shadow grades do not create additional donor rates. Three donors therefore need not represent three independent observed prices. Fixed moderate event risk, incomplete judgmental adjustments, retrospectively revised inputs and conditional score intervals remain limitations. The intervals are not statistical confidence intervals.',
'The next stage is a separately versioned integration and empirical replay, with a broader code checkpoint and explicit old/new results for every affected analysis. This build supplies the full peer layer and a hypothetical selected-reference comparison, but does not replace the production core, rewrite thesis results or rerun loan valuations. Directional stability should be evaluated, not imposed as a condition for retaining the new method.',
'## Files',
'`country_year_comparison.csv` contains every country-year and both values, rules, source counts, memberships, availability, eligibility and hypothetical selection. `coverage_summary.csv`, `coverage_by_year.csv`, `coverage_by_current_tier.csv`, `rule_counts.csv`, and the paired-change files provide the corresponding aggregate views. `README.md` documents the offline build and the meaning of each layer.']
report='\n\n'.join(lines)+'\n'
(v/'REPORT.md').write_text(report.replace('(full_panel_comparison.png)', '(results/full_panel_comparison.png)'))
if len(sys.argv)>1:
    dest=Path(sys.argv[1]);dest.mkdir(parents=True,exist_ok=True)
    names=['country_year_comparison.csv','coverage_summary.csv','coverage_by_year.csv','coverage_by_current_tier.csv','coverage_by_income.csv','rule_counts.csv','rule_mapping.csv','paired_rate_summary.csv','paired_rate_changes_by_year.csv','paired_rate_changes_by_current_tier.csv','paired_rate_changes_by_income.csv','paired_annual_change_summary.csv','paired_cases_by_absolute_change.csv','coverage_changes.csv','lost_selected_peers.csv','method_scope_comparison.csv','hidden_rating_validation_same_cases.csv','checks.csv','full_panel_comparison.png']
    for name in names:shutil.copy2(p/name,dest/name)
    (dest/'report.md').write_text(report);shutil.copy2(v/'README.md',dest/'README.md')
    print('Packaged',len(names)+2,'files in',dest)
