from pathlib import Path
import csv,json,hashlib,shutil,zipfile
repo=Path('/Users/waindewane/Claude and Codex/Miscellaneous/pvr-global/exports/thesis-replication-20260922/workspace')
p=repo/'experiments/p15_peer_rating_expansion_20260912'
outputs=Path('/Users/waindewane/Documents/Codex/2026-09-12/referenced-chatgpt-conversation-this-is-an/outputs')
def read(n):return list(csv.DictReader((p/n).open()))
def table(headers,rows):
 return '\n'.join(['| '+' | '.join(headers)+' |','| '+' | '.join(['---']*len(headers))+' |']+['| '+' | '.join(map(str,r))+' |' for r in rows])
cov={r['method']:r for r in read('all_peer_coverage.csv')}
valid={r['method']:r for r in read('strict_peer_validation.csv')}
rules=read('all_peer_rule_counts.csv')
rule_names=['Income + region + rating','Income + rating','Region + rating','Rating alone','Income + region','Income alone','Region alone','All eligible global issuers']
mapping=read('peer_rule_mapping.csv')
main=['current','P_stop5','PIS_stop5','P_fiscal_ols_stop5','PIS_fiscal_ols_stop5','PIS_fiscal_no_rule4','PIS_biennial_stop5']
rt=table(['Thesis rule','Matching criteria','Current','P, stop 5','PIS, stop 5','Shadow P','Shadow PIS','Shadow PIS, omit 4','Biennial PIS'],[[i+1,rule_names[i]]+[sum(int(r['country_years']) for r in rules if r['method']==m and r['rule']==str(i+1)) for m in main] for i in range(8)])
labels={'current':'Current rules 1–8','drop8':'Remove only rule 8','P_stop5':'Primary, stop after rule 5','PIS_stop5':'PIS, stop after rule 5','P_fiscal_ols_stop5':'Fiscal shadow, primary, stop 5','PIS_fiscal_ols_stop5':'Fiscal shadow, PIS, stop 5','PIS_fiscal_no_rule4':'Fiscal shadow, PIS, rules 1–3 and 5','PIS_biennial_stop5':'Fiscal shadow, PIS, biennial','PIS_fiscal_lag2_stop5':'Fiscal shadow, PIS, two-year macro lag','PIS_fiscal_recent_events_stop5':'Fiscal shadow, PIS, recent rating events','PIS_fiscal_core_cascade_stop5':'Fiscal shadow then core model, PIS','PIS_fiscal_ridge_stop5':'Fiscal ridge model, PIS','PIS_fiscal_tree_stop5':'Fiscal tree model, PIS','PIS_core_ols_stop5':'Core economic model, PIS','PIS_external_ridge_stop5':'External-data model, PIS','PIS_macro_nearest3':'Direct economic-distance matching, PIS','PIS_wb_snapshots_stop5':'Bounded published WB snapshots, PIS','PIS_web_moody_stop5':'Public Moody reconciliation candidates, PIS','PIS_web_then_fiscal_stop5':'Public Moody candidates then fiscal shadow, PIS'}
ct=table(['Option','Retained / 778','Lost','Countries retaining peers','Changed retained rates'],[[labels[m],cov[m]['retained_current_peers'],cov[m]['lost_current_peers'],cov[m]['countries'],cov[m]['changed_retained']] for m in labels])
vmethods=['current','P_stop5','PIS_stop5','P_fiscal_ols_stop5','PIS_fiscal_ols_stop5','PIS_fiscal_no_rule4','PIS_biennial_stop5','PIS_fiscal_lag2_stop5','PIS_fiscal_recent_events_stop5','PIS_fiscal_ridge_stop5','PIS_fiscal_tree_stop5']
vt=table(['Option','Test country-years','Current MAE on those cases','Alternative MAE','Improvement','95% descriptive interval for improvement'],[[labels[m],valid[m]['n'],f"{float(valid[m]['mae_b']):.3f}",f"{float(valid[m]['mae_a']):.3f}",f"{float(valid[m]['benefit']):.3f}",(f"{float(valid[m]['ci_lower']):.3f} to {float(valid[m]['ci_upper']):.3f}" if valid[m]['ci_lower'] else '—')] for m in vmethods])
cr=read('crs_diagnostic_valuation_summary.csv')
crt=table(['Option','Retained / 256 modern peer-valued loans','Lost','Mean GE change on retained loans','Mean absolute GE change'],[[labels[m],r['retained'],r['lost'],f"{float(r['mean_ge_change_retained']):+.2f}",f"{float(r['mean_abs_ge_change_retained']):.2f}"] for m in ['drop8','P_fiscal_ols_stop5','PIS_fiscal_ols_stop5','PIS_fiscal_no_rule4','PIS_biennial_stop5'] for r in cr if r['method']==m and r['period']=='2018-2024'])
sources=[
('Finance and Prosperity 2024, chapter 2 workbook','Country-level S&P rating changes for 2012–2023','Downloaded all 16 sheets. Sheet 2.3 contains four group medians: two tercile groups in 2012 and 2023, based on 49 EMDEs. No individual-country rating panel.','No rating observations added.','https://thedocs.worldbank.org/en/doc/57f0b48cbef5dd12b2c6cc9e7e5024a5-0430012024/related/Finance-Prosperity-2024-Chapter-2-data-and-figures.xlsx'),
('Basu et al., WPS 6641 (2013)','Continuous actual/shadow panel between July 2008 and December 2012','The supplied UUID is the correct paper. It compares two snapshots, July 2008 and December 2012. Table 7 reports 27 shadow predictions for countries without S&P ratings; not necessarily unrated by all agencies.','Existing extracted table used only for Jan 2014 in the bounded-snapshot diagnostic.','https://openknowledge.worldbank.org/entities/publication/1de6fb75-8fdf-5409-a131-f77ff6387f63'),
('Second new World Bank UUID','Later shadow/governance/sub-sovereign data extension','Actually The Ghost of a Rating Downgrade, about borrowing-cost effects in 20 countries over 1998–2015.','No shadow panel.','https://openknowledge.worldbank.org/entities/publication/1a842a12-3c03-5bf9-94b5-09e6c6dbe32d'),
('Ratha, De and Mohapatra, WPS 4269 (2007)','Ready-to-use historical shadow dataset','A historical cross-section with 55 unrated-country predictions, not annual 2012–2024 ratings.','Methodological precedent; too old for a two-year carry into this sample.','https://openknowledge.worldbank.org/entities/publication/ddc75ebf-3865-5aff-b6ce-7a0c9e6d08b4'),
('Canuto, Mohapatra and Ratha, Economic Premise 63 (2011)','Reusable shadow dataset','Annex has 47 country rows, including five explicitly older predictions. Agency-model ranges are not statistical confidence intervals. The author website’s two apparent data links resolve to broken local-Word paths.','Fresh, finite ranges used at their midpoint only in Jan 2012/2013; old and one-sided rows excluded.','https://documents1.worldbank.org/curated/en/125821468332693455/pdf/638890BRI0Econ000public00BOX361532B.pdf'),
('De, Mohapatra and Ratha, WPS 9401 (2020)','Broad actual + shadow + relative panel','26 emerging/frontier economies, quarterly 1998–2017, focused on actual and relative ratings. Repository bundles inspected in the preceding audit contain the paper, not a broad shadow panel.','No missing-country shadow observations added.','https://openknowledge.worldbank.org/entities/publication/b71cefd1-0c5a-563b-bead-bca68b87b99f'),
('Abate et al., WPS 9649 (2021)','Biennial 132-country shadow ratings extensible through 2024','Genuine scorecard-based estimates for 2006, 2008, 2010, 2012, 2014, 2016 and 2018. A ready-to-download observation file was not found in the repository or linked sources checked.','A serious model precedent, not data available for direct merging. Our fitted models are separate and do not replicate this scorecard.','https://documents1.worldbank.org/curated/en/565681620234717531/pdf/Economic-Governance-Improvements-and-Sovereign-Financing-Costs-in-Developing-Countries.pdf'),
('El-Shagi and von Schweinitz, SJPE (2022)','New independent literature-discovery lead','Downloaded author-linked replication RData at a fixed Git commit and the author PDF. File has 1,114,907 daily records across 140 country labels, including yields; the paper describes rating announcements for 138 countries collected in October 2017.','35 possible Moody additions among 2012–2017 selected peers, reduced to 33 after excluding implausible Mozambique observations. Conflicts and stale withdrawals prevent automatic replacement.','https://github.com/gvschweinitz/ES_21_Why-they-keep-missing-An-empirical-investigation-of-sovereign-bond-ratings-and-their-timing'),
('Countryeconomy current foreign-currency Moody tables','Could complete the replication source beyond 2017','Downloaded the 41 listed country pages relevant to current selected peers; parsed 135 dated Moody grade rows, separately from local-currency and short-term columns.','47 possible additions, including one contradicted by Cuba’s known withdrawal: 46 remaining reconciliation candidates, not 46 certified recoveries.','https://countryeconomy.com/ratings'),
('WDI and WGI','Public macroeconomic inputs','Saved raw API responses and metadata; resolved renamed GOV_WGI codes and blank ISO3 identifiers. Historical governance values are the 2025 revision.','Actual predictors and coverage checks for all 211 thesis economies.','https://datacatalog.worldbank.org/search/dataset/0038026/worldwide-governance-indicators'),
('IMF WEO October 2024','Public fiscal and macro inputs','Direct IMF endpoints returned 403. Retrieved the explicitly dated IMF dataset through DBnomics: 1,568 series across two fully reconciled pages, 39,200 year observations for 2000–2024.','General government debt and fiscal balance, plus same-concept macro substitutions. Fiscal quantities are percent of GDP.','https://www.imf.org/en/publications/weo/weo-database/2024/october/download-entire-database'),
('MPRA 127543 and other climate adjustments','Climate-adjusted ratings','Excluded in accordance with the user’s instruction.','No climate variables, adjustments or predictions used.','https://mpra.ub.uni-muenchen.de/127543/1/MPRA_paper_127543.pdf')]
with (p/'source_claim_audit.csv').open('w') as f:
 w=csv.writer(f);w.writerow(['source','claim_or_lead','verified_contents','use_in_exploration','url']);w.writerows(sources)
st=table(['Source or lead','What inspection established','Use here'],[[f'[{a}]({u})',c,d] for a,b,c,d,u in sources])
mt=table(['Thesis rule','Current peer_pool_rule'],[[r.get('rule',r.get('thesis_rule')),r.get('current_label',r.get('peer_pool_rule'))] for r in mapping])
report=f'''# Peer ratings and benchmark expansion: separate empirical exploration

12 September 2026. Current run: `b3d0684d4f8aae7eb15a0958c3fb823008b627d61fe832ce6d7ea12334a7df46`.

## What this investigation changes in the assessment

There is a feasible and empirically promising alternative to the present broad peer fallback: use a modest economic/fiscal model to estimate missing rating positions, use those positions only to select peers, and continue to take observed donor rates as the benchmark. The evidence now supports building and reviewing such a candidate. It does not support treating all modelled ratings as accurate, filling every missing country-year, or adopting a large opaque model because it maximizes coverage.

This pass downloaded actual data, fit seven rating specifications, compared 38 peer configurations including baselines, tested a two-year update cycle and additional information lags, checked indirect leakage in validation, perturbed model errors, and replayed existing loan cash flows. All production inputs and outputs remain unchanged. This is a research experiment, not an adopted methodology or revised thesis result.

The simple fiscal model supplies estimates for **692 of the 778 current selected peer country-years**. With Rules 1–5 it retains **504** peer references using primary donor rates, or **643** using the primary → IDS → secondary donor priority. Omitting rating-only Rule 4 leaves **543** in the latter design. In strict masked-rating tests, the fiscal PIS design lowers average absolute primary-rate error from **1.419 to 1.195 percentage points**, on the same 215 country-years. The gain survives removing the test country’s history from every donor model as well as its own prediction.

My preferred direction is a repaired actual-rating layer plus a simple, separately identified shadow score, with **Rules 1–3 and 5 as the stricter peer definition**. Keep the additional rating-only cases from Rule 4 as a clearly labelled sensitivity. Report primary-only donor results alongside any mixed-source expansion. Do not use Rules 6–8 to force a matched peer estimate into every remaining case. This recommendation is not implemented.

## The unchanged baseline and exact rule mapping

The current data contain 2,743 country-years in 2012–2024, of which 1,756 are in the historical low-/middle-income reporting scope. Their selected tiers are primary 215, IDS 229, secondary 72, Moody’s-implied 454, peer 778, and no eligible rate 8. Thus the 778 cases below are selected peer **country-years**, not countries or bond issues. Ninety distinct countries occur among them.

{mt}

Rules 1–4 use a distance of at most three Moody’s notches. All rules require at least three distinct donor countries; the target is excluded; the first qualifying pool supplies the median of all its eligible members. Categories match exactly. Rule 5 ignores ratings.

The current counts are Rules 1–3: zero each; Rule 4: one; Rule 5: 72; Rule 6: 339; Rule 7: 146; Rule 8: 220. Removing Rules 6–8 therefore removes **705** current estimates and retains 73. Removing Rule 8 alone removes **220** and retains 558. Complete country/year loss lists for these and all new options are in the accompanying tables.

Two distinctions remain essential. First, stopping after Rule 5 does **not** require every peer to share both income and region: Rules 2–4 can qualify earlier. Second, 776 selected peer cases lack the current Moody’s input; this is not proof that 776 were unrated by every agency. The selection ladder uses Moody’s-implied rates before peers, which itself concentrates missing Moody’s observations in the peer category.

## Source verification: what is usable and what was overstated

{st}

The [full source-claim table](source_claim_audit.csv) records the supplied claim separately from what was found. The 2024 workbook’s “2012–2023” caption is particularly misleading as a data lead: its rating observations are group medians, not an annual set of country changes. There is no way to recover individual starting ratings from them.

The 2021 World Bank scorecard remains a substantive precedent. Its institutional component deliberately removes WGI and relies on inflation, while event risk is fixed at “medium”; some other adjustments are neutral. These choices serve its governance-regression question and are not automatically the best design for peer matching. Inputs include WEO, competitiveness data, default history and foreign-currency debt information, with some manual additions. Its reported 2018 comparison puts 61% of estimates within two actual-rating notches. Reproducing and extending that specific model requires its scoring machinery and input decisions, rather than merely downloading WDI. The published historical panel ends in 2018. A two-year carry could cover an intervening year, not the entire 2020–2024 gap.

The [author’s website](https://dilipratha.com/) did not reveal a hidden annual panel: its two apparent shadow-data links point to broken local Word-file paths. Commercial leads from the preceding investigation—CountryRisk.io, Continuum Economics, and S&P’s separate country-risk products—remain possibilities, but complete licensed 2012–2024 shadow histories were not obtained or established. No provider was contacted, no account was purchased, and no commercial coverage was assumed.

The economics literature skill was used as a supplementary search. OpenAlex returned eight records; Semantic Scholar was rate-limited. Ordinary source-owner searches produced the useful replication-data lead. This is a documented search result, not proof that no other dataset exists.

## Actual ratings: useful recoveries, but real errors in public alternatives

The current Bloomberg input was archived on 28 May 2026 as eight “Rating Changes” workbooks. It contains Moody’s, Fitch and S&P, although the accepted main rating reference and peer matching use Moody’s. Recorded source dates start in January 2000 and extend into May 2026; the reconstruction restricts events to 2000–2025 and takes beginning-of-year states for the thesis’s 2012–2024 sample. A captured initial state is carried through years without changes. When that initial state is missing, the code only uses the previous grade from a first captured event if that event occurs in the target year. Thus infrequent changes can explain missing early history, but are not a problem after a valid initial state is captured. The record span does not establish complete country coverage.

The downloaded SJPE replication package is materially useful because it permits direct historical comparisons. It suggests 35 Moody additions among current selected peers in 2012–2017. Two are Mozambique observations that translate to Aa3 before the first B1 entry in the current web history, and are excluded from the fill-only diagnostic. There are also 104 disagreements among 520 overlapping Moody country-years across the full 2012–2017 comparison. Some disagreements may originate in the thesis extract; others may reflect definitions or errors in the alternative. They need reconciliation, not voting or blind replacement.

The source explicitly builds histories by carrying rating announcements forward. It carries Iran’s Fitch rating into 2012–2017 even though the earlier audit documented withdrawal in 2008. Current web histories have their own problems: the Mozambique Fitch table contains alternating B+/AA− entries; undated entries also occur. The public Moody table for Cuba carries a grade after withdrawal. [ECLAC’s 2024 rating table](https://repositorio.cepal.org/server/api/core/bitstreams/af1c6fc2-d3fd-466f-9443-9684dcdc16f7/content) explicitly records Cuba’s rating withdrawal on 7 December 2023.

The current web extraction finds 47 potential Moody additions across 11 current peer countries. Rejecting Cuba 2024 leaves **46 candidates**. Bangladesh contributes 2012–2021 and Cambodia all 2012–2024; together these are 23 cases. The remaining candidates are Angola, Armenia, Cuba’s earlier years, Georgia, Guatemala, Honduras, Morocco, Papua New Guinea and Senegal. Dates and grades are in `web_rating_peer_reconciliation.csv`. Their source is public reported actual ratings, not shadow estimates. Agency/type/date and withdrawal checks are still required before promotion.

For illustration, filling only these screened target-grade candidates raises strict primary coverage from 73 to 116, or strict PIS coverage from 297 to 326. These figures leave the existing selected-tier order fixed; fully repairing actual ratings could move some cases into the higher Moody’s-implied tier, so they are not final post-repair tier counts. Combining the candidate fills with the fiscal shadow model leaves PIS coverage at 643 but changes some matches. This is worth doing for accuracy, even when headline coverage barely changes.

## The models actually estimated

The exercise estimates a numeric position on the current 21-notch Moody’s scale. It does not assign an official rating, estimate a default probability, or turn a shadow rating directly into an interest rate. This is a literature-inspired model in the [Cantor–Packer](https://www.newyorkfed.org/research/staff_reports/research_papers/9608.html) tradition, not a replication of the World Bank scorecard or a claim to match an agency committee’s process.

The core OLS model uses log GDP per capita, log total GDP, a trailing three-year growth average, a trailing five-year growth standard deviation, transformed inflation, and WGI rule of law. The fiscal model adds general-government gross debt/GDP and fiscal balance/GDP. Ridge regression uses the same fiscal inputs with a fixed penalty. A shallow tree provides a nonlinear comparison. An external-data model adds current-account balance and reserve import coverage. Two further fiscal tests use a two-year macro lag or restrict training labels to events no more than five years old. The specifications were not tuned to maximize retained peer counts.

The data distinction matters. General-government debt is available for 694 current peer cases, whereas WDI central-government debt is available for only 98. They are different concepts and are not silently substituted. Current-account data cover 621 cases, reserve coverage 500, and the full external model can predict only 475. Requiring more predictors can reduce practical coverage substantially. The core model predicts 738 cases and the fiscal model 692. Adding the core model only where fiscal inputs are absent produces a separate cascade sensitivity, not hidden imputation of fiscal quantities.

All predictors end before the target rating year. The main specification uses economic information through year t−1. Training ratings run from 2005 through t−1, with ten deterministic country folds: a target’s entire country history is omitted from its training fold. All transformations and complete-case requirements are explicit. The current labels are the thesis’s existing Moody’s series, so input errors remain a limitation even with honest holdouts.

These are retrospective data. WDI/WGI include later revisions, and WEO is the October 2024 vintage retrieved through [DBnomics](https://db.nomics.world/IMF/WEO%3A2024-10). Historical variable dates do not establish that all data were publicly available on 1 January. The two-year-lag test addresses information delay but does not turn revised inputs into a true vintage backtest.

On 890 LMIC country-years with observed ratings and complete fiscal inputs, the main model’s mean absolute error is **1.85 notches**, with **61.3%** within two and **80.6%** within three. Its mean error is −0.43 notches: estimates are somewhat more favourable than actual grades on average. The 90th percentile absolute error is 3.88 notches. These are errors of continuous score predictions. Good rank information does not imply reliable exact grades, and a ±3 matching window cannot ignore this uncertainty.

## What happens to the numbered peer rules

“P” uses current eligible same-year primary rates. “PIS” chooses one rate per donor-country/year, in order primary → IDS → secondary; it never counts three sources from one country as three peers. Higher selected tiers and existing exclusion rules are retained. A valid current Moody grade takes precedence over a shadow estimate. The model fills missing target and donor matching grades, while the peer benchmark remains a median of donor rates.

{rt}

The final three columns use the fiscal OLS score. “Omit 4” searches Rules 1, 2, 3 and 5; it is stricter than the user’s original stop-after-5 proposal. “Biennial” retains an even-year model score in the following odd year. Numbers are selected LMIC peer country-years. Unique-country counts overlap across rows and should not be added.

{ct}

No option adds an eligible selected rate to the eight currently excluded/no-rate cases. Under the fiscal PIS stop-5 design, total selected LMIC rates would therefore be 970 non-peer plus 643 peer = **1,613**, compared with 1,748 currently. The stricter version omitting Rule 4 gives **1,513** total selected rates. Missing cases remain in the dataset with no peer group; countries and observations are not deleted.

The mixed-source option depends substantially on IDS. Across its 643 selected groups there are 5,023 donor memberships: 1,731 primary (34.5%), 2,620 IDS (52.2%) and 672 secondary (13.4%). Ninety-six groups contain no primary donor. These are repeated memberships, not independent observations. Source diversity expands coverage, while differences in currency, borrower scope and measurement remain consequential.

Rules 1–3 rise from zero to 491 with fiscal PIS matching. That is a substantive improvement in the matching description. However, 105 cases still first qualify on estimated rating alone. Omitting Rule 4 loses 100 net cases, because five can then qualify at Rule 5. The cost is concentrated in lower-income economies:

{table(['Income group','Current peer cases','Fiscal PIS rules 1–5','Fiscal PIS rules 1–3 and 5'],[['Low income',312,212,116],['Lower middle income',276,260,256],['Upper middle income',190,171,171]])}

The strict choice therefore improves comparability at a material cost to low-income coverage. This must be visible in the thesis rather than hidden by a global fallback.

Published World Bank snapshots alone offer a smaller bridge: with stale and one-sided 2011 entries excluded and a bounded publication-aware use of the 2012 assessment, primary stop-5 coverage becomes 129 and PIS becomes 363. This does not create a continuous 2012–2024 shadow panel. Averaging the printed agency ranges is only a diagnostic convention, not a claim that the authors supplied those exact point estimates.

The preceding exploration also tested the user's additive ideas. Combining incomplete income and region pools only after Rule 7 fails retains 587 cases, rescues 29 former global cases, and preserves all 558 existing Rules 1–7 estimates. Averaging the income and region medians after Rule 5 retains 467; its masked-rating error improves from 1.464 to 1.248 points on 200 identical cases. That is a useful alternative, with explicit equal-weighting and overlapping-pool choices. These earlier tests use different support from the new shadow tests and should not be ranked by their unpaired MAEs. Widening the existing rating window to five or seven notches leaves selected stop-5 coverage at 73: it cannot solve missing ratings. The new fiscal model addresses that missing-information constraint, which is why its assessment changes the earlier, more cautious recommendation.

## Does the change improve the rates, not just coverage?

The test set consists of the 215 LMIC country-years with eligible primary rates, across 47 countries. Each target’s own rate is removed from the donor pool, its rating is hidden, and its entire rating history is removed from every fitted model that could inform the prediction, including donor models. The target’s actual primary rate is used only to evaluate the result. Actual ratings of other donor countries remain available. This simulates missing target-rating information more credibly than a random split of country-year rows.

{vt}

MAE and improvements are in percentage points of interest. Each row compares identical cases within that row; its denominator is shown. Intervals use country and year clustering with finite-sample t critical values. They are descriptive, conditional on reused donors, revised macro data and this exploratory comparison. They are not adjusted for searching across specifications. Actual-rating reconciliation variants are not presented as fair hidden-rating tests, since they restore the information the test is meant to hide.

The fiscal improvement is not an artefact of mixing IDS or secondary rates: it also appears in the primary-only model. In 2018–2024, fiscal PIS MAE falls from 1.578 to 1.287 on 140 identical cases. A separate initial comparison restricting six main alternatives to common support also gives similar relative rankings; the stricter donor-history exclusion above is the validation used for conclusions.

The tree illustrates why coverage is an inadequate objective. It retains 711 PIS peer references but its strict rate MAE is about 1.45, worse than the current 1.42. Direct nearest-neighbour matching on economic fundamentals was also tested, requiring a shared income group or region, three donors within a fixed standardized distance, and the closest three rates. It retains 555 PIS cases but gives no accuracy gain (MAE about 1.45 versus 1.41 on its matched cases). These particular nonlinear and direct-distance alternatives are not preferred.

The most important external-validity limitation is the test population: 139 observations are upper-middle income, 71 lower-middle income, and only **five** low income. Rated and issuing countries need not behave like countries with neither ratings nor market access. Honest country holdouts reduce leakage; they do not solve that selection problem or certify rates for never-observed borrowers.

## Biennial updates and uncertainty

The biennial proposal is feasible as a timing convention when a consistent underlying model or dataset exists. In the main fiscal model’s odd-year overlap, annual rating MAE is 1.876 and the carried biennial MAE 1.896. Peer coverage moves from 643 to 646; strict rate MAE is 1.171. This is not evidence that updating less frequently is inherently better: the differences are small, use the same exploratory sample, and can reflect smoothing.

As a persistence benchmark, 74.1% of 668 observed-rating comparisons across an intervening year are unchanged and 91.2% move by at most one notch. The reconstructed actual series itself may contain stale grades, so these figures can overstate persistence. “Carry forward unless there is a change” is implementable only if intervening actual changes and withdrawals are separately observed. Otherwise it means a bounded assumption, not knowledge that the rating stayed constant.

In 100 sensitivity draws, empirical out-of-country rating errors are applied to shadow components, with one country shock shared across its years. Current observed grades stay fixed. For fiscal PIS, the middle 80% of simulated retained counts is approximately **624–668**, around a point count of 643. Of the 643 point-retained cases, **540** retain a group in at least 90% of draws. The median 10th–90th percentile rate-band width is **1.31 percentage points**. These are perturbation diagnostics, not calibrated posterior or confidence intervals for unrated countries. They show that group existence can be reasonably stable while the chosen rate remains uncertain.

## Downstream valuation implications

The main non-peer benchmarks and results are unchanged in this exploration. If a peer redesign were adopted, selected benchmark tables, broad coverage and composition summaries, peer-inclusive country/scenario analyses and peer-valued loans would need to be regenerated. The underlying primary, IDS and secondary rate extraction need not be changed merely to change matching. A later actual-rating repair would additionally affect the higher rating-implied tier and its selection dependencies.

The separate replay uses the existing CRS cash flows and first reproduces current grant elements numerically. There are 881 existing finite peer-valued CRS loan records: 625 in 2012–2017 and 256 in 2018–2024. The modern-period diagnostic is:

{crt}

Grant-element changes are percentage points and compare retained loans with their own current value. They must not be confused with a comparison of changing sample averages. The fiscal PIS design changes 607 retained country-year rates, by 1.16 percentage points in mean absolute terms. Its modern retained loans have an average absolute grant-element change of about six points. Thus a change would be methodologically and numerically material for peer-inclusive results, even though the present main non-peer comparison is insulated.

## Practical decision

I would pursue the following sequence as one controlled candidate revision, rather than choose a coverage-maximizing rule now:

1. Reconcile actual Moody’s history first, including country coverage, rating type, initial state and withdrawals. Use the public and replication files to identify exact cases; retain disputed cases explicitly. Extend to other agencies only with the same checks.
2. Keep a simple fiscal shadow score for matching where actual grades remain unavailable. Preserve its model status and prediction uncertainty. The evidence here is sufficient to justify this extension; it is not yet a completed replacement rating database.
3. Make Rules 1–3 and 5 the stricter proposed peer category, requiring three distinct donors and retaining missing groups. Treat rating-only Rule 4 as a separate sensitivity, and remove Rules 6–8 from the matched-peer definition. If “same income and same region” is literally required for every donor, that is a different, still stricter rule than either ladder and should be stated directly.
4. Preserve one donor rate per country. Compare primary-only with primary → IDS → secondary pools; disclose source composition and retain the existing exclusions. IDS contractual rates and secondary yields are not interchangeable merely because both are percentages.
5. Rebuild the affected benchmark and valuation outputs only after selecting the candidate and documenting its definitions. Maintain the non-peer main results and show peer additions as model-dependent evidence.

This is a stronger direction than retaining unrestricted global peers, and more constructive than deleting 705 estimates without examining alternatives. The statistical gain is meaningful but not large enough to eliminate uncertainty or justify universal coverage. The main unresolved judgment is how much model dependence and donor-source heterogeneity the thesis should allow in its optional weakest benchmark.

## Reproducibility and files

The source downloads, metadata, extraction code, economic panel, model predictions, fixed country folds, country-year memberships, strict validation cases and valuation replay are saved separately under this experiment. `source_claim_audit.csv` lists source claims and URLs. `all_peer_deployment_cases.csv` contains every option’s country-year result; `all_lost_peers_by_country.csv` lists all lost years by country. `strict_peer_validation.csv` is the preferred accuracy table; earlier validation tables remain diagnostic intermediate comparisons.

The production manifest checks the current pointer plus eight current-run artifacts. The verification receipt and manifests accompany the final outputs. The manuscript, accepted method, source snapshots already used in production and current-run files were not replaced. Code and private vendor-derived comparisons remain in the private research workspace.
'''
(p/'REPORT.md').write_text(report)
outputs.mkdir(exist_ok=True)
(outputs/'peer_rating_expansion_report.md').write_text(report)
shutil.copy2(p/'all_peer_deployment_cases.csv',outputs/'peer_rating_options_country_years.csv')
shutil.copy2(p/'all_lost_peers_by_country.csv',outputs/'peer_rating_options_lost_years.csv')
print('Report words',len(report.split()))
