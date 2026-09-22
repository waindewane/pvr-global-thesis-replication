## Scope and contents of the dataset

This thesis constructs a country-year dataset of sovereign borrowing benchmarks for 2012–2024. Each record represents one economy in one calendar year. Newly issued bonds provide evidence of borrowing conditions at issuance, but an issuance-based rate requires both an eligible bond issue and sufficiently complete information about its terms. The dataset therefore combines issuance yields with yields on outstanding bonds, reported interest rates on new external debt commitments, and estimates based on sovereign ratings or comparable borrowers. This structure supports the evaluation of benchmark coverage and comparability, followed by an assessment of how borrower-specific discount rates change the measured concessionality of official loans.

The dataset follows a fixed roster of 211 economies for all thirteen years, producing 2,743 country-year records even when no borrowing benchmark is available. The roster contains the non-aggregate entries with a recorded capital city in the archived World Bank country metadata, including territories that satisfy that rule (World Bank n.d.-d). The coverage denominator includes the 1,756 country-years classified as low, lower-middle or upper-middle income in the historical series. High-income and unclassified years remain in the dataset but are excluded from that denominator; missing borrowing-rate observations remain included. Income groups are drawn from the World Bank’s historical classification workbook and supplemented with World Bank classifications processed by Our World in Data (World Bank, n.d.-b, 2025). Classifications are assigned to the year of the income data on which they are based. For example, the group attached to 2024 reflects the classification published in July 2025 using 2024 income data. This provides a retrospective grouping for the analysis, including the income criterion used to identify comparable borrowers. It does not reproduce the classifications already in force during each year.

The following table summarises the five benchmark sources and estimation methods. Primary and secondary bond yields are constructed separately for US dollars and euros. The main reference selection uses dollar primary and secondary bond yields and dollar-based rating estimates. The International Debt Statistics (IDS) Bondholders series provides an additional proxy based on reported contractual interest rates; its aggregate does not identify the underlying currency composition. Each source-specific rate remains separately identifiable, together with its source, relevant dates and recorded limitations. Where an application requires one benchmark per country-year, a stated selection order determines which eligible reference is used. The alternatives remain available for comparison.

**Table. Sources and estimation methods for sovereign borrowing benchmarks**

| Benchmark | Underlying information | Rate and aggregation | Timing and currency |
|----|----|----|----|
| Primary issuance | LSEG records of sovereign bond issues | Mean issue yield, weighted by the amount issued | Issues during the calendar year; separate dollar and euro series |
| IDS Bondholders | World Bank reports of new public and publicly guaranteed external debt commitments to bondholders | Mean contractual interest rate, weighted by commitment amount | New commitments during the year; currency composition unidentified in the aggregate |
| Secondary market | LSEG quotations for outstanding sovereign bonds | Mean bond yield, weighted by the recorded face amount outstanding | Eligible quotations closest to year-end; separate dollar and euro series |
| Rating-implied estimate | Moody’s sovereign ratings, Damodaran’s archived sovereign default spreads and the seven-year US Treasury yield from FRED | Treasury yield plus the sovereign default spread associated with the borrower’s rating | Rating at 1 January, annual mean Treasury yield and year-specific spread archive; dollar reference |
| Peer approximation | One eligible primary, IDS, secondary or rating-implied rate per comparison country, in that priority order | Median rate of at least three other countries in the first qualifying comparison group | Same-year rates; groups use income, region and actual or estimated creditworthiness. Currency coverage follows the contributing sources |

*Sources and notes:* Author’s compilation and calculations from the sources listed, including archived LSEG extracts (LSEG 2026). Moody’s ratings are drawn from Bloomberg rating-change records (Bloomberg 2026). The IDS definition and amount weighting follow the World Bank’s indicator metadata (World Bank, n.d.-a, “Short definition”). Peer rates approximate borrowing conditions using other countries’ observations and therefore provide a less borrower-specific basis than evidence drawn from the country itself. The following subsections explain the eligibility rules, timing conventions, estimation procedures and selection order.

# References cited in this subsection

Bloomberg. 2026. *Sovereign Rating Changes*. Static-value Excel exports from the Rating Changes view, archived May 28, 2026.

LSEG. 2026. *Sovereign Bond Issue Terms and Market Quotations*. Archived data extracts acquired May–July 2026; observations for 2012–2024.

World Bank. 2025. *World Bank’s Income Classification*. Dataset, with major processing by Our World in Data. <https://ourworldindata.org/grapher/world-bank-income-groups>.

World Bank. n.d.-a. *Average Interest on New External Debt Commitments (%): DT.INR.DPPG*. International Debt Statistics, metadata glossary. Accessed 10 September 2026. <https://databank.worldbank.org/metadataglossary/international-debt-statistics/series/DT.INR.DPPG>.

World Bank. n.d.-b. *World Bank Analytical Classifications*. OGHIST Excel workbook, Country Analytical History sheet; archived copy retrieved June 30, 2026. Accessed 11 September 2026. <https://datahelpdesk.worldbank.org/knowledgebase/articles/906519-world-bank-country-and-lending-groups>.

World Bank. n.d.-d. *Country API Metadata*. Archived country and economy metadata used to define the fixed reporting roster. [Country metadata](https://api.worldbank.org/v2/country?format=json&per_page=400).
