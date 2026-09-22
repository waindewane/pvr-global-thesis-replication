## 4.4 Source comparability and differences between methods

This section examines whether some sources approximate observed issuance more closely than others, and why the sources can nevertheless give different rates for the same country and year. It also assesses how these differences affect the choice of a borrower-specific reference rate.

**Table. Paired comparisons of approximation accuracy, 2018–2024**

| Methods compared | Country-years | Countries | First: MAE | Second: MAE | First: median | Second: median |
|---|---:|---:|---:|---:|---:|---:|
| Secondary / rating-implied | 107 | 29 | 1.115 | 1.093 | 0.769 | 0.937 |
| Rating-implied / peer | 126 | 38 | 1.108 | 1.136 | 0.936 | 0.875 |
| Secondary / peer | 117 | 32 | 1.110 | 1.131 | 0.779 | 0.865 |

*Notes:* All error measures are in interest-rate percentage points relative to primary issuance. “Median” denotes the median absolute error, which is less sensitive to unusually large discrepancies than MAE. “First” and “second” follow the order of methods named in each row. Both methods use identical observations within a row, but samples differ between rows. *Source:* Calculations from the constructed dataset using the evaluation design in Section 4.1.

None of the three paired MAE comparisons establishes an accuracy advantage significant at the 5% level under the country-bootstrap procedure. For secondary minus rating-implied MAE, the 95% interval extends from −0.23 to +0.35 percentage points, permitting either method to have the lower average error. Secondary yields nevertheless have lower median absolute errors than either estimate on their respective matched samples. Peer estimates also have a lower median than rating-implied rates, despite a slightly higher MAE. The relative performance therefore depends partly on sensitivity to large discrepancies.

Across 189 overlapping country-years in 41 countries during 2012–2024, IDS rates are 0.13 percentage points below primary issuance on average, with a mean absolute difference of 0.58 points. About 81% of the paired rates differ by no more than one point. Their overlapping bond borrowing helps explain this agreement, although differences in contractual interest versus issue yields and in the commitments covered prevent treating them as interchangeable observations, as discussed in Chapter 3.

Across 150 overlapping country-years in 36 countries during 2012–2024, secondary yields are 0.37 percentage points below primary issuance on average, with a mean absolute difference of 1.02 points. These comparisons use all eligible source overlaps, without requiring a DAC reference. Primary rates describe financing raised during the year, whereas secondary yields describe outstanding bonds around year-end. Changes in market conditions between those dates and differences in maturity can therefore contribute to the discrepancies.

In the 17 country-years with both relatively close observation dates and similar aggregate maturities, the mean absolute rate difference is 0.50 points.[^comparability-timing] This smaller gap does not isolate the contribution of timing or maturity, because the countries and bonds also differ. Closer dates alone do not consistently coincide with smaller discrepancies.

The two largest primary–secondary gaps account for approximately 60% of the sum of squared differences. Omitting them reduces RMSE from 1.81 to 1.14 points, compared with a smaller fall in MAE from 1.02 to 0.90 points. The full sample remains the main comparison, since the size of a discrepancy alone does not establish that an observation is erroneous.

The selection order in Chapter 3 consequently rests on the nature and timing of the borrowing evidence as well as approximation accuracy. Reversing the order of IDS and secondary yields changes the selected rate in 29 country-years across 12 countries, without changing the total coverage of 1,701 country-years. Among those affected observations, the mean absolute change is 2.60 percentage points. The order therefore affects relatively few selections but can substantially change the reference rate in those cases.

[^comparability-timing]: This thesis defines close dates for this diagnostic as an interval of 0–90 days from the last primary issue to the earliest secondary observation, and similar maturities as a difference of no more than three years between the aggregate maturities. These cutoffs do not change benchmark eligibility or match individual bonds across sources.
