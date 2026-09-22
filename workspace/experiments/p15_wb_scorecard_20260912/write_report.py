"""Package the verified separate build; never describe unavailable ratings as results."""
import csv, json, shutil, zipfile
from pathlib import Path
ROOT=Path('experiments/p15_wb_scorecard_20260912')
OUT=Path('/Users/waindewane/Documents/Codex/2026-09-12/referenced-chatgpt-conversation-this-is-an/outputs')
OUT.mkdir(parents=True,exist_ok=True)
def rows(p):
    with open(p,newline='') as f:return list(csv.DictReader(f))
def write(p,data):
    with open(p,'w',newline='') as f:
        w=csv.DictWriter(f,fieldnames=list(data[0]));w.writeheader();w.writerows(data)

loss=rows(ROOT/'peer/lost_peer_country_years.csv')
strict=[r for r in loss if r['method']=='PIS_strict']
stop5=[r for r in loss if r['method']=='P_stop5']
assert len(strict)==481 and len(stop5)==705
loss_countries=len({r['iso3'] for r in strict})
keep=['iso3','country','analysis_year','historical_income_level','selected_rate_pct','peer_pool_rule']
write(OUT/'wb_scorecard_expanded_pool_lost_country_years.csv',[{k:r[k] for k in keep} for r in strict])
write(OUT/'wb_scorecard_original_stop5_lost_country_years.csv',[{k:r[k] for k in keep} for r in stop5])
complete=rows(ROOT/'input_scenario_coverage_income.csv')
totals={s:sum(int(r['complete_input_country_years']) for r in complete if r['input_scenario']==s)
        for s in {r['input_scenario'] for r in complete}}
assert totals=={'strict_historical_gross_current_gci':0,'net_interest_current_gci':148,
 'net_interest_gci_carry_max2':202,'net_interest_stale_gci_hold_diagnostic':290,
 'latest_gfs_gross_gci_carry_max2':26}

report=f'''# Scorecard reconstruction and peer-reference build

12 September 2026 · Separate research build · Existing thesis results preserved

The question is whether a published scorecard can supply credible missing credit ratings, allowing countries to be matched on credit risk as well as income and region. A second, independent change is to let each donor supply its eligible primary rate, otherwise its IDS rate, otherwise its secondary-market rate. Rating-implied rates remain excluded from donors. No regression was fitted or substituted in this build.

**The data preparation, verified scorecard aggregation components, peer selection and valuation comparisons are built and tested. The full World Bank/Moody’s shadow-rating calculation is not complete.** The exact November 2018 numerical scoring tables remain inaccessible, and the available public inputs also contain material gaps. Consequently this build produces **zero new shadow ratings**. This is an explicit unavailable state, not evidence that a completed scorecard would fail to find peers.

## What is now built

- A panel of **2,743 country-years**, covering the existing 211-country universe in 2012–2024, with frozen source files and explicit missingness.
- All **13 historical fall IMF WEO vintages** needed for the January 1 convention: September 2011, then October 2012–2023. Forward-looking windows use forecasts from the relevant vintage, not later realized values.
- Legacy World Economic Forum competitiveness data, the World Bank’s external public-and-publicly-guaranteed debt data, genuine IMF GFS gross interest where available, and a separately identified Bank of Canada–Bank of England default-stock history proxy.
- All **675 cells** of the historical Moody’s aggregation matrices reproduced in ESM Working Paper 27. They exactly reproduce the indicative ranges in **six independently published August 2019 country examples**. This verifies those six paths, not every cell against the unavailable 2018 original. [ESM paper](https://www.esm.europa.eu/system/files/document/wp27final.pdf).
- A scoring interface that requires explicit evidence for indicator thresholds, conditional weights, rounding and adjustments. Missing rules cannot silently generate ratings. The peer engine already accepts point grades, completeness flags, provenance and justified grade intervals.
- Full donor membership, benchmark-selection controls, hidden-target validation and loan-valuation comparisons. Each country contributes one eligible rate; at least three distinct countries are required; the borrower is excluded; the median uses the first qualifying group.

The broader donor pool and shadow matching are separate modules. A shadow grade would help decide *which countries are comparable*. The borrowing-rate observation still has to come from primary issuance, IDS or secondary pricing. Existing actual Moody’s grades take precedence over shadow grades; higher selected benchmark tiers are preserved.

## Why the precise World Bank reconstruction is unfinished

The World Bank authors explicitly reference Moody’s 2018 methodology. Their Appendix D lists inputs and sources, but does not supply all indicator bins, internal weights, rounding conventions and adjustment schedules. Table 1 maps rating symbols to numbers; it is not the missing scoring algorithm. [World Bank WPS9649](https://documents1.worldbank.org/curated/en/565681620234717531/pdf/Economic-Governance-Improvements-and-Sovereign-Financing-Costs-in-Developing-Countries.pdf).

The exact document is **Moody’s, Sovereign Bond Ratings, 27 November 2018, PBC_1151027**. Its identity is corroborated by a contemporary Moody’s report. Both existing browser sessions reached its sign-in page. Public issuer reports, ESM/MNB reconstructions, regulator copies, methodology-change references and replication-file searches supplied useful pieces, but no complete verified 2018 rule set. [Exact methodology link](https://www.moodys.com/research/doc--PBC_1151027).

The accessible indexed November 2019 method explicitly changes indicators, scoring ranges, weights and score granularity. Substituting it would therefore produce a different model. [Moody’s November 2019 methodology](https://ratings.moodys.com/api/rmc-documents/63168).

Access to the exact 2018 document would unlock the scoring implementation, **but would not resolve the input gaps below**. It also would not reproduce the authors’ undocumented manual input selections automatically.

## What the actual input downloads show

These counts use the **778 country-years currently selected at the peer tier**, across 90 historically low- or middle-income countries. They count complete numerical input sets under the stated assumptions, **not completed ratings and not newly rescued peer groups**.

| Input policy | Low income | Lower middle income | Upper middle income | Total |
|---|---:|---:|---:|---:|
| Historical gross interest and contemporaneous legacy competitiveness | 0 | 0 | 0 | **0** |
| Net-interest approximation; contemporaneous competitiveness | 84 | 47 | 17 | **148** |
| Net-interest approximation; competitiveness carried at most two years | 109 | 71 | 22 | **202** |
| Net-interest approximation; last competitiveness value held without age limit | 148 | 110 | 32 | **290** |
| Later-revised GFS gross interest; competitiveness carried at most two years | 4 | 20 | 2 | **26** |

All rows also require complete macro windows, debt ratios and a usable external-PPG-debt share proxy. They do not certify an exact Moody’s default-history input or complete rules. The final stale-value row for net interest is a diagnostic, not a recommended extension.

Four findings explain the limits:

1. **Interest expense:** WEO primary balance minus overall balance equals **net** interest expense, after subtracting interest revenue. It does not establish gross interest expense. Complete macro coverage allowing this approximation is 522 of 778; the approximation itself is observed in 557, including 26 negative values. Genuine general-government GFS gross interest exists for 129 peer country-years, with matching revenue for 128, but this is a later-revised snapshot. Zeros and negative values were not fabricated or floored. [Original IMF WEO data and subject definitions](https://www.imf.org/-/media/files/publications/weo/weo-database/2011/weosep2011all.xls).
2. **Competitiveness:** the downloaded legacy index covers 2007–2017 on a 1–7 scale. WEF introduced a different index in 2018 on a 0–100 scale; changing units alone would not make it the same variable. A two-year carry of the final 2017 observation reaches reference year 2019, hence benchmark year 2020 under our timing convention. It cannot provide current information for 2021–2024. [Legacy dataset](https://www3.weforum.org/docs/GCR2017-2018/GCI_Dataset_2007-2017.xlsx), [WEF methodology change](https://www.weforum.org/press/2018/10/changing-nature-of-competitiveness-poses-challenges-for-future-of-the-global-economy/).
3. **Foreign-currency debt:** external PPG debt is not the same concept as foreign-currency general-government debt. The numerator also includes broader public and guaranteed obligations. Of 580 calculable peer-country-year ratios, 46 exceed the meaningful 0–100% share range; these are flagged rather than silently capped. The 534 remaining ratios are still proxies. [World Bank indicator definition](https://api.worldbank.org/v2/indicator/DT.DOD.DPPG.CD?format=json).
4. **Default history:** the BoC–BoE source provides historical default stocks, not the authors’ Moody’s default-event series. Bond/bank histories are kept separate from official-loan arrears. Only observations through the reference year are used; absence means no recorded stock, not certified absence of every default. [BoC–BoE methodology](https://www.bankofcanada.ca/2025/10/staff-analytical-note-2025-24/).

There is also a choice about the model itself. The World Bank study removes governance indicators from institutional strength to avoid a mechanical relationship with its CPIA regressors, giving inflation the full institutional weight. It fixes event risk at Moderate. Those choices serve that paper’s research design; they are not automatically the best choices for matching borrowing conditions. A full-governance variant deserves separate evaluation once the underlying rules are available. [World Bank methodology discussion](https://documents1.worldbank.org/curated/en/565681620234717531/pdf/Economic-Governance-Improvements-and-Sovereign-Financing-Costs-in-Developing-Countries.pdf).

## What changes from donor expansion alone

The following are recomputed controls using the current actual-rating inputs. **They do not include newly constructed shadow ratings.** Within the 778 selected peer cases, most target ratings remain missing in the current archive. Missing in this archive does not prove that a sovereign was unrated by every agency.

| Thesis rule | Current `peer_pool_rule` | Current primary pool | Primary, stop after 5 | Expanded sources, stop after 5 | Expanded sources, Rules 1–3 and 5 |
|---|---|---:|---:|---:|---:|
| 1: income + region + rating | same_income_region_rating3_min3 | 0 | 0 | 0 | 0 |
| 2: income + rating | same_income_rating3_min3 | 0 | 0 | 0 | 0 |
| 3: region + rating | same_region_rating3_min3 | 0 | 0 | 0 | 0 |
| 4: rating only | rating3_min3 | 1 | 1 | 1 | excluded |
| 5: income + region | same_income_region_min3 | 72 | 72 | 296 | 297 |
| 6: income only | same_income_min3 | 339 | excluded | excluded | excluded |
| 7: region only | same_region_min3 | 146 | excluded | excluded | excluded |
| 8: any eligible country | global_min3 | 220 | excluded | excluded | excluded |
| **Retained peer country-years** | | **778** | **73** | **297** | **297** |
| **Lose current peer estimate** | | **0** | **705** | **481** | **481** |

Keeping only Rules 1–3 and 5 gives **72** groups with primary donors, **215** with primary→IDS, and **297** with primary→IDS→secondary. These cover 22, 44 and 61 countries, respectively. All 297 expanded-source groups use Rule 5. Of the original 312 low-income peer country-years, 46 survive; 154 of 276 lower-middle-income and 97 of 190 upper-middle-income cases survive.

The 481 losses span **{loss_countries} countries**, with every country/year listed in the supplied CSV. Missing groups remain recorded as unavailable; no country-year is deleted. The existing eight LMIC country-years without an eligible benchmark remain unavailable. No higher-tier rate changes. The expanded strict control therefore has 970 stronger-tier estimates, 297 peer estimates and 489 unavailable references within the 1,756 LMIC country-years.

The retained groups contain 1,437 donor memberships: **643 primary, 573 IDS and 221 secondary**. Fifty-eight groups contain no primary donor. Source expansion changes 267 retained peer rates, by an average absolute **1.274 percentage points**. IDS and secondary observations expand support but preserve their different timing and measurement properties.

## Validation and valuation consequences

Hiding each validation borrower’s grade and removing its own rate produces 144 common test country-years across 35 countries. On those same cases, mean absolute rate error is **1.307 points** for the strict expanded pool and **1.300 points** for the current method. The paired gain is −0.0068 points, with a descriptive country/year-clustered 95% interval of −0.222 to 0.208. **Source expansion alone shows no demonstrated accuracy improvement.** Comparing its error on 144 cases with the current method’s 1.419 error on all 215 cases would confuse a sample change with improvement. Shadow-rating accuracy cannot yet be evaluated because no new grades were produced.

The cutoff and source changes would require recomputing any downstream results that include peer estimates. The separate replay already shows their effect for the currently finite historical-LMIC peer-valued CRS records:

| Period | Current peer-valued records | Retained with strict expanded pool | Lose valuation |
|---|---:|---:|---:|
| 2012–2017 | 625 | 133 | 492 |
| 2018–2024 | 256 | 109 | 147 |

For the 109 retained modern records, borrower grant element changes by **−1.99 percentage points on average**, or **−3.74 points with commitment weights**; the mean absolute change is **6.01 points**. These describe surviving cases. The 147 missing valuations are a separate coverage loss. Broader MPG, AidData and ADD applications are replayed in distinct tables; overlapping datasets are not pooled as independent loans. Existing non-peer valuations remain unchanged. For clarity, the established modern non-peer CRS count of 1,274 is global; imposing historical-LMIC scope gives 1,266.

## Additional published scorecard checked

The **Bank of Canada 2017 methodology** is a useful new lead: its full public paper supplies numerical bins and an indicative-rating matrix. It nevertheless requires qualitative monetary-policy credibility in the initial score, several additional historical institutional datasets, and judgments about some boundary cases. Its displayed matrix also compresses the lower tail: it jumps from B− to C and includes N/A combinations. Therefore it cannot simply be treated as a complete Moody’s-equivalent 21-notch panel. The 2021 update changes parts of the method but also retains qualitative assessments. No climate adjustments or alternative Bank of Canada ratings were implemented. [2017 paper](https://www.bankofcanada.ca/wp-content/uploads/2017/05/sdp2017-7.pdf), [2021 update](https://www.bankofcanada.ca/wp-content/uploads/2021/11/sdp2021-16.pdf).

## Assessment and next dependency

The scorecard route remains worth investigating, and the work completed here substantially reduces the effort needed to finish it. It is **not yet a defensible completed replacement** for the thesis benchmarks. The obstacle is both source access and input quality, not the five low-income primary-rate validation cases discussed earlier. For example, the two-year competitiveness/net-interest input policy already has 109 complete low-income input sets; those still need a verified scoring algorithm and rating validation.

The next concrete dependency is the exact 2018 methodology or the authors’ original scoring workbook. Once available, the versioned rules bundle can be completed, tested against published examples, and applied first to input-supported cases. Net versus gross interest, competitiveness age, default history, governance treatment and event-risk assumptions must remain separate sensitivities. The existing matching and valuation engine can then evaluate genuinely new grades immediately.

For now, retain the requested scorecard-first direction and the exclusion of rating-implied donor rates. Do not install a guessed scorecard, a regression substitute or the source-expansion control as the accepted method merely because it runs. The numerical controls are reviewable research results; production selection and valuation files remain unchanged.

## Verification and saved material

Fourteen macro checks include 30,173 independently reproduced calculations. Seventeen scoring-component checks and six published-country comparisons pass. Seventeen peer integration checks and eight boundary-test groups pass. Ten additional independent checks verify 8,229 competitiveness assignments, 13,715 input scenarios, ratio units, grouped counts, source hashes and production preservation. These checks establish implementation correctness within the stated scope; they do not validate unavailable ratings.

The accompanying table archive includes exact country-year losses, all matching controls and memberships, input availability by year/income, scorecard build status, source receipts, validation and valuation comparisons. It is a private research package because some baseline inputs derive from restricted sources. The repository experiment is `experiments/p15_wb_scorecard_20260912/`; its README documents offline reproduction and the precise remaining source requirements.
'''
(ROOT/'REPORT.md').write_text(report)
(OUT/'wb_scorecard_build_report.md').write_text(report)
shutil.copy2(ROOT/'input_scenario_coverage_income.csv',OUT/'wb_scorecard_input_coverage.csv')
shutil.copy2(ROOT/'input_scenario_country_years.csv',OUT/'wb_scorecard_input_country_years.csv')
archive=OUT/'wb_scorecard_build_tables.zip'
with zipfile.ZipFile(archive,'w',zipfile.ZIP_DEFLATED) as z:
    for base in [ROOT,ROOT/'peer',ROOT/'macro',ROOT/'methodology',ROOT/'supplemental']:
        for p in sorted(base.glob('*.csv')):
            z.write(p,str(p.relative_to(ROOT)))
    for p in [ROOT/'REPORT.md',ROOT/'README.md',ROOT/'OWNER_INSTRUCTION.md',ROOT/'methodology/README.md',
              ROOT/'peer/BASELINE_RESULTS.md',ROOT/'peer/INDEPENDENT_REVIEW.md',
              ROOT/'methodology/exact_2018_access_receipt.json',ROOT/'macro/README.md',
              ROOT/'macro/method_search/boc2017_published_tables.json']:
        if p.exists():z.write(p,str(p.relative_to(ROOT)))
    for p in (ROOT/'macro/method_search').glob('*.md'):z.write(p,str(p.relative_to(ROOT)))
    for p in (ROOT/'methodology').glob('boc*.md'):z.write(p,str(p.relative_to(ROOT)))
print(json.dumps({'report':str(OUT/'wb_scorecard_build_report.md'),'archive':str(archive),
                  'lost_expanded_country_years':len(strict),'lost_expanded_countries':loss_countries}))
