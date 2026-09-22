## Peer-based borrowing-rate approximations

For countries without a usable borrowing-rate reference from the preceding sources, this thesis estimates a rate from a group of selected countries in the same year. The estimate is the median of their rates, with the group selected according to income category, geographical region and creditworthiness. The IMF–World Bank debt-management manual illustrates using a comparable sovereign's credit spread to approximate borrowing costs when direct evidence is unavailable (Balibek et al. 2019, Annex L, 59–60). This thesis applies that principle across the annual dataset through an explicit procedure for selecting comparison countries and combining their rates.

Each comparison country contributes one eligible rate. Its primary issuance rate is used when available, followed by IDS bondholder terms, secondary-market yields and an existing rating-implied rate. The country whose rate is being estimated is excluded, and peer estimates are not used as inputs to other peer estimates. The pool therefore combines observed borrowing information with rating-implied estimates. The differences in timing and currency coverage described for the individual sources also carry into the peer comparison, including the unidentified currency composition of IDS bondholder terms.

Income matching requires the same World Bank income category: low, lower-middle, upper-middle or high income. It uses the retrospective income-data-year classifications described earlier. Regional matching requires membership in the same geographical group. Countries within these categories are not ranked by how close their income levels or geographical locations are. High-income countries can enter groups that do not require an income match, or groups formed for high-income borrowers.

Creditworthiness matching uses the available Moody's rating at the beginning of the year. When that rating is missing from the dataset, this thesis estimates creditworthiness using the scorecard-based approach implemented by Abate, Brown, Sienaert and Thomas (2021, 4–6 and Appendix D), drawing on Moody's published sovereign-rating methodology. Abate and coauthors used the scorecard to estimate ratings for developing countries with incomplete actual-rating coverage. This thesis adapts their approach to identify comparable borrowers. The estimated grades are used for matching, rather than to create an additional rating-implied borrowing-rate series.

The scorecard assigns economic indicators to strength categories using published thresholds. For example, average growth above 4.5% receives the strongest growth category, whereas growth below 0.5% receives the weakest. Growth contributes 25% of the economic-strength calculation, with growth variability, competitiveness, economic size and income per person supplying the remainder. Governance and inflation indicators determine institutional strength, and debt and interest-payment indicators determine fiscal strength. Moody's combination tables translate these factor scores, together with the event-risk assessment, into a grade on its credit-rating scale (Moody's Investors Service 2018, 4–6, 9, 14 and 18).

This thesis retains governance indicators in the institutional assessment. Abate and coauthors excluded them because their subsequent regressions used related governance measures to explain the estimated ratings, creating a mechanical connection between the variables. That concern does not require their exclusion from the peer-matching calculation. The treatment of event risk follows their simplifying assumption of a moderate level for every country. It therefore does not reproduce each country's agency assessment of political, liquidity, banking and external vulnerability risks (Abate et al. 2021, 5–6).

When gross interest expenditure is unavailable and the relevant indicator has a positive weight, the calculation allows its score to vary from the strongest to the weakest category. It carries these possibilities, together with bounds on adjustments for default history and foreign-currency debt exposure, through the combination tables to obtain lower and upper grade bounds. These bounds describe uncertainty within the adopted calculation and remain conditional on its assumptions, including moderate event risk. An available actual rating always takes priority over the estimated range.

The matching procedure permits a creditworthiness difference of at most three rating steps. When an estimated range is used for either country, the most distant pairing within the two ranges must still satisfy that limit. The calculation then selects all members of the first group in the following table containing at least three eligible countries. Each group is assessed against the same pool of available countries. The conditions change between groups, rather than adding countries one by one to the preceding group.

**Peer-group selection criteria**

| Search order | Conditions required for inclusion |
|---:|---|
| 1 | Same income category, same region and creditworthiness within three steps |
| 2 | Same income category and creditworthiness within three steps |
| 3 | Same region and creditworthiness within three steps |
| 4 | Creditworthiness within three steps |
| 5 | Same income category and same region |

*Creditworthiness is assessed using an available actual rating or the estimated grade range. At least three distinct comparison countries are required in every group.*

The sequence balances similarity with the availability of comparison rates. The first group matches all three characteristics. Subsequent groups allow particular characteristics to differ when the earlier conditions produce too few countries. The fifth group permits matching on income and region without a creditworthiness condition. If none of the five groups qualifies, no peer estimate is calculated. The matching sequence, three-step limit and minimum of three countries are this thesis's choices for implementing the comparison, rather than requirements of the cited scorecard or debt-management manual.

The estimate is the unweighted median of all rates in the selected group. Each country contributes once, without additional weights for economic size, number of bonds or closeness within the permitted rating distance. Any weighting used to construct that country's primary or IDS rate is retained. The median limits the influence of individual extreme rates, and the minimum of three countries reduces reliance on a single comparator. Three is a minimum group size, not a requirement to select exactly three countries or the three closest matches.

During 2012–2024, peer estimates supply the selected reference for 731 low- and middle-income country-years across 89 countries. In 582 of these cases, matching uses an estimated target grade; two use a recorded rating, and 147 use income and region without a creditworthiness restriction. These counts concern cases in which a peer estimate is selected, after the preceding rate sources have been considered. The full low- and middle-income panel contains 903 country-years with recorded ratings out of 1,756. The annual distribution of the selected peer references across matching groups is shown below.

**Selected peer references by matching group, 2012–2024**

| Year | Income + region + grade | Income + grade | Region + grade | Grade only | Income + region | Total |
| --- | ---: | ---: | ---: | ---: | ---: | ---: |
| 2012 | 2 | 21 | 5 | 27 | 21 | 76 |
| 2013 | 4 | 19 | 14 | 17 | 19 | 73 |
| 2014 | 11 | 17 | 11 | 9 | 17 | 65 |
| 2015 | 15 | 23 | 2 | 10 | 13 | 63 |
| 2016 | 13 | 21 | 8 | 8 | 13 | 63 |
| 2017 | 20 | 18 | 5 | 4 | 11 | 58 |
| 2018 | 24 | 19 | 1 | 1 | 11 | 56 |
| 2019 | 14 | 24 | 1 | 3 | 10 | 52 |
| 2020 | 22 | 16 | 1 | 2 | 7 | 48 |
| 2021 | 21 | 16 | 1 | 3 | 5 | 46 |
| 2022 | 24 | 12 | 1 | 3 | 4 | 44 |
| 2023 | 20 | 17 | 0 | 1 | 5 | 43 |
| 2024 | 17 | 12 | 1 | 3 | 11 | 44 |
| 2012–2024 | 207 | 235 | 51 | 91 | 147 | 731 |

*Counts are low- and middle-income borrower-years using a peer rate as their selected reference. Grade matches use actual ratings or estimated ranges under the three-step condition. Source: this thesis's calculations.*

The selected groups contain an average of 6.72 countries and a median of five. The middle half contains four to eight countries. Group sizes range from three to 26, with a standard deviation of 4.50. These statistics describe the group used for each borrower-year, so a group shared by several borrowers is counted for each use.

The following table shows the rate sources contributing to those groups. Across the 731 selected borrower-years, there are 4,915 contributions from comparison countries. A country appearing in several borrowers' groups contributes separately to each. Rating-implied rates account for 2,867 of these contributions and enter 697 selected groups, including 71 composed entirely of rating-implied rates. Their use is therefore a substantial part of the peer estimates' empirical basis.

**Rate sources contributing to selected peer groups, 2012–2024**

| Year | Selected borrower-years | Primary | IDS | Secondary | Rating-implied | Total contributions |
| --- | ---: | ---: | ---: | ---: | ---: | ---: |
| 2012 | 76 | 84 | 212 | 0 | 378 | 674 |
| 2013 | 73 | 22 | 188 | 0 | 288 | 498 |
| 2014 | 65 | 55 | 126 | 2 | 288 | 471 |
| 2015 | 63 | 55 | 141 | 21 | 281 | 498 |
| 2016 | 63 | 40 | 112 | 22 | 302 | 476 |
| 2017 | 58 | 82 | 34 | 12 | 182 | 310 |
| 2018 | 56 | 79 | 59 | 18 | 151 | 307 |
| 2019 | 52 | 56 | 55 | 32 | 121 | 264 |
| 2020 | 48 | 51 | 36 | 26 | 176 | 289 |
| 2021 | 46 | 56 | 44 | 24 | 149 | 273 |
| 2022 | 44 | 42 | 13 | 30 | 186 | 271 |
| 2023 | 43 | 29 | 21 | 43 | 219 | 312 |
| 2024 | 44 | 82 | 16 | 28 | 146 | 272 |
| 2012–2024 | 731 | 733 | 1,057 | 258 | 2,867 | 4,915 |

*Each contribution is one comparison country's rate entering one target country's peer group. Repeated use of the same country-year is counted separately. The source counts sum to contributions, not to selected borrower-years or to additive effects on the median. Source: this thesis's calculations.*

Shared comparison countries and rating-implied inputs mean that different peer estimates can depend on the same underlying information. The dataset retains the member countries, their rate sources and the dispersion of their rates for examining particular comparisons. The benchmark-evaluation chapter assesses accuracy against observed issuance rates, excluding the target country from its own peer group. It separately examines matching with an available target rating and matching when that rating is withheld to assess the use of estimated creditworthiness. Because these tests require observed issuance, they provide less direct evidence about accuracy for countries without bond-market observations.

## References cited in this subsection

Abate, Girum, Michael Brown, Alex Sienaert, and Mark Thomas. 2021. *Economic Governance Improvements and Sovereign Financing Costs in Developing Countries*. Policy Research Working Paper 9649. Washington, DC: World Bank. May. [Paper](https://documents1.worldbank.org/curated/en/565681620234717531/pdf/Economic-Governance-Improvements-and-Sovereign-Financing-Costs-in-Developing-Countries.pdf).

Balibek, Emre, Tobias Haque, Diego Rivetti, and Miriam Tamene. 2019. *Medium-Term Debt Management Strategy: Analytical Tool Manual*. Technical Notes and Manuals 2019/002. International Monetary Fund. [Manual](https://doi.org/10.5089/9781498314961.005).

Moody's Investors Service. 2018. *Sovereign Bond Ratings*. Rating Methodology. 27 November. Document PBC_1151027.
