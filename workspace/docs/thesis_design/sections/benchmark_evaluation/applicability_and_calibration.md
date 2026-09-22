## 4.5 Applicability of estimated rates and the calibration evidence

This section examines how far accuracy among observed bond issuers supports the use of estimated rates for other borrowers. It also assesses whether corrections fitted to observed issuance should replace the original rating-implied formula. The evaluation samples differ substantially from the country-years in which estimates supply the selected reference. Low-income borrowers account for only 5 of 214 country-years in the peer evaluation over 2012–2024, compared with 276 of the 731 country-years in which peer rates are selected. Their borrowing conditions are therefore sparsely represented in the direct accuracy evidence.

This thesis estimates a common correction by regressing observed issuance rates on the original rating-implied rates, with an intercept and slope. A further adjustment uses the country's earlier discrepancies between this corrected rate and observed issuance, when available. Fitting and selection between corrections use earlier years, separately from the year being evaluated.[^calibration-design] A second design removes the evaluated country's entire history from fitting and selection to test whether a correction learned from other borrowers remains useful.

**Table. Rating-implied accuracy with and without fitted corrections, 2018–2024**

| Estimation procedure | MAE | Reduction from original MAE |
|---|---:|---:|
| Original rating-implied formula | 1.098 | — |
| Correction using available country history | 0.838 | 23.7% |
| Common correction with the evaluated country's history withheld | 1.005 | 8.5% |

*Notes:* All rows evaluate the same 130 country-years in 40 countries during 2018–2024. MAE is in interest-rate percentage points relative to primary issuance. Unlike the policy comparison in Section 4.3, this exercise does not require a DAC reference. *Source:* Calculations from the constructed dataset.

The correction using available country history improves average accuracy in every evaluation year. With that history withheld, the common correction still lowers pooled MAE by 8.5%, but raises annual MAE in 2019 from 1.149 to 1.330 points and in 2021 from 0.801 to 0.990. These results support calibration among observed issuers, particularly when their earlier borrowing provides information for the correction. Withholding that information does not, however, establish performance for borrowers whose issuance rates remain unobserved.[^calibration-no-history]

Of the 269 country-years in which a rating-implied rate is selected during 2018–2024, 241—about 90%—lack the earlier matched observations needed for a country adjustment. This thesis retains the original Treasury-plus-spread formula because the issuer-based corrections have not been validated for that wider population. This is a cautious choice about transferring a fitted correction, not a finding that the original formula minimizes pooled validation error. The calibrated estimates are reported as alternatives in this evaluation but are not used in the main analysis.

For peer estimates, this thesis examines missing rating information by recomputing the rates after withholding the target country's recorded rating, using estimated creditworthiness and the matching rules in Chapter 3. Across 209 identical country-years in 46 countries during 2012–2024, MAE rises from 1.12 to 1.33 percentage points. Five otherwise evaluable country-years lose a peer estimate. The comparison shows weaker approximation when matching cannot use the recorded rating, a condition relevant to many borrowers needing peer estimates. However, it still contains only five low-income country-years, leaving limited direct evidence for that group.

[^calibration-design]: For each evaluation year, candidates are compared on earlier years using still earlier observations for fitting. The selected candidate is then refitted on all preceding observations. This separates model selection from evaluation, following the principle discussed by Cawley and Talbot (2010, p. 2080). The exercise remains retrospective, rather than a previously untouched confirmation sample. The country adjustment partially pools earlier country discrepancies, using variance components estimated with a random-intercept model in `nlme` (Pinheiro and Bates, n.d.).

[^calibration-no-history]: Thirteen evaluation observations in thirteen countries have no earlier matched issuance and rating-implied observation in the dataset. In this small group, the common correction increases MAE from 0.94 to 1.19 points. This is limited accompanying evidence about borrowers lacking history, not a general finding for all such borrowers.

# References cited in this subsection

Cawley, Gavin C., and Nicola L. C. Talbot. 2010. “On Over-fitting in Model Selection and Subsequent Selection Bias in Performance Evaluation.” *Journal of Machine Learning Research* 11: 2079–2107. [Publisher page](https://www.jmlr.org/papers/v11/cawley10a.html).

Pinheiro, José, and Douglas Bates. n.d. “Linear Mixed-Effects Models: lme.” *R nlme package documentation*. Accessed 20 September 2026. [Documentation](https://stat.ethz.ch/R-manual/R-devel/library/nlme/html/lme.html).
