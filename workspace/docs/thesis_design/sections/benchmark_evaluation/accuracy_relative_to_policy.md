## 4.3 Approximation accuracy relative to policy rates

This section examines whether borrower-specific rates approximate observed primary issuance more closely than the differentiated DAC discount rates. During 2018–2024, all four alternatives—IDS bondholder terms, secondary yields, rating-implied rates and peer estimates—have lower mean absolute error than the DAC rates on their respective matched samples.

For secondary yields, rating-implied rates and peer estimates, the reduction in mean absolute error is approximately 30–34%, equivalent to 0.50–0.58 interest-rate percentage points. The larger reduction for IDS partly reflects its overlap with primary issuance in the underlying bond borrowing. Close agreement between these two sources therefore provides less independent evidence about approximation accuracy than the results for the estimated rates.

**Table. Approximation of primary issuance rates relative to DAC references, 2018–2024**

| Alternative reference | Country-years | Countries | Alternative MAE | DAC MAE | Reduction in MAE |
|---|---:|---:|---:|---:|---:|
| IDS bondholder terms | 127 | 37 | 0.573 | 1.654 | 65.4% |
| Secondary yields | 117 | 32 | 1.110 | 1.662 | 33.2% |
| Rating-implied rates | 126 | 38 | 1.108 | 1.686 | 34.3% |
| Peer estimates | 136 | 41 | 1.161 | 1.661 | 30.1% |

*Notes:* MAE is in interest-rate percentage points. Each row compares the alternative and the DAC rate against the same primary observations. The samples differ between rows, so the table does not establish an accuracy ranking between methods. *Source:* Calculations from the constructed dataset using the evaluation design in Section 4.1.

The DAC rates exceed primary issuance rates on average by 0.43–0.63 percentage points across these matched samples. Among the alternatives, secondary yields are lower than primary issuance by 0.34 points on average, while rating-implied and peer rates are higher by 0.10 and 0.27 points, respectively. The IDS difference is close to zero, at −0.02 points. The small average difference for rating-implied rates nevertheless coexists with an MAE above one percentage point, as overestimates and underestimates offset one another in the signed average.

All four alternatives also reduce RMSE relative to their DAC comparison, although the improvement for secondary yields is much smaller under this measure. Their RMSE is 1.98 percentage points, against 2.12 for DAC. The improvement is therefore less pronounced when larger errors receive greater weight. Rating-implied and peer rates have RMSEs of 1.45 and 1.50 points, against DAC values of approximately 2.1 in their respective samples.

Giving each country, or each year, equal total weight leaves the reduction in mean absolute error positive for all four alternatives during 2018–2024. Under the country-bootstrap procedure described in Section 4.1, the 95% confidence intervals for these reductions are above zero for all four alternatives. The corresponding tests remain statistically significant at the 5% level after applying Holm’s (1979) correction for the four comparisons.

Accounting additionally for related observations across countries in the same year weakens that statistical evidence. Only IDS remains statistically significant at the 5% level after the same multiple-comparison correction, although the estimated average reductions in error are unchanged. With only seven calendar years, the evidence for improvement across a broader range of borrowing conditions remains limited, as discussed in Section 4.1.

Over the full 2012–2024 period, all four alternatives have lower MAE than the standardized 9/7/6% reference. The earlier years are less uniform. During 2012–2017, IDS, secondary yields and peer estimates improve on that reference, but rating-implied rates have a slightly higher MAE, at 1.26 rather than 1.16 percentage points. Their better approximation during 2018–2024 therefore does not hold throughout the study period. Against the historical 10% convention, all four alternatives show much larger reductions of 3.01–3.73 points in 2012–2017. Because these two earlier-period comparisons retain exactly the same observations and borrowing-rate estimates, the larger gains against 10% result from changing the policy reference.

Rating-implied rates reproduce the ordering of countries from lower to higher observed borrowing costs more closely than peer estimates in every year included in this comparison. This thesis measures that agreement using Spearman rank correlations with primary issuance, calculated on the same countries for both methods within each year. The summary includes years with at least eight matched countries, a reporting threshold chosen for this comparison, leaving 189 country-years over 2014–2024. The equally weighted average of the eleven annual correlations is 0.78 for rating-implied rates and 0.60 for peer estimates. Both methods distinguish relatively higher- and lower-cost issuers within this sample. This finding concerns relative positions across countries, separately from the size of errors in their estimated rates.

# References cited in this subsection

Holm, S. 1979. “A Simple Sequentially Rejective Multiple Test Procedure.” *Scandinavian Journal of Statistics* 6: 65–70. [Article record](https://www.jstor.org/stable/4615733). [Implementation and methodological reference in the R documentation](https://stat.ethz.ch/R-manual/R-devel/library/stats/html/p.adjust.html).
