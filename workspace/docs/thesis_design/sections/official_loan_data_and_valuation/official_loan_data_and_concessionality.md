This thesis examines how the concessionality of official loans changes when their repayments are discounted at borrower-specific rates instead of standardized policy rates. This chapter sets out the loan data, repayment assumptions and comparisons underlying the findings in Chapter 7, using the borrowing-rate evidence developed in Chapters 3–5.

## 6.1 Official-loan sources and the usable samples

This thesis uses loan records from the OECD Creditor Reporting System (CRS) to compare borrower-specific and standardized measures of concessionality across bilateral and multilateral creditors. CRS provides the broadest creditor and geographical coverage among the datasets examined here. Its reported interest rates, repayment dates, frequency and repayment type allow the calculation of the amounts due and when they fall due (OECD 2024; OECD n.d.-d). The sample includes new loans to recipient governments with positive commitments and sufficiently complete, consistent terms to reconstruct those payments.[^ch6_crs_records] Incomplete reporting limits coverage across providers and years. After matching the records to borrower-specific rates and excluding loans supported only by peer estimates, the sample contains 2,686 financing records across 85 countries and 22 providers. Of these, 1,274 fall in the central comparison period, 2018–2024.

Morris, Parks and Gardner (2020, 8–9) compare Chinese, IDA and IBRD lending by discounting repayments at a common 5% rate. This thesis recalculates concessionality using borrower-specific rates for usable records from 2012–2014, the years shared by their study and this thesis’s benchmark dataset. Later Chinese lending is examined using AidData’s records through 2023, restricted here to fixed-rate US-dollar loans to central governments or with sovereign guarantees (AidData 2025). The African Debt Database adds official lending to African central governments through 2024, covering Chinese and other bilateral and multilateral creditors (Manger et al. 2025, 11–12; African Debt Database 2026). Together, these records extend the comparisons to later borrowing conditions and different creditor mixes.[^ch6_source_overlap]

The World Bank’s International Debt Statistics (IDS) also provide average interest rates, maturities and grace periods by borrowing country, year and creditor (World Bank n.d.-c). For China, IBRD and IDA, this thesis uses those averages to compare the concessionality of average lending terms, following ONE’s *Priced Out* approach (Harcourt, Haro Ruiz and Rivera 2026; ONE Data n.d.). Each observation describes a repayment schedule constructed from average terms, whose interpretation differs from an individual-loan result as explained in Section 6.2.

**Table. Financing records with usable terms and a borrowing benchmark other than peers**

| Data source | Years | Observation | Number | Countries |
|---|---|---|---:|---:|
| OECD CRS | 2012–2024 | Financing record | 2,686 | 85 |
| Morris, Parks and Gardner | 2012–2014 | Loan record | 462 | 65 |
| AidData | 2015–2023 | Loan record | 164 | 37 |
| African Debt Database | 2015–2024 | Loan record | 1,624 | 29 |
| IDS average terms: China, IBRD and IDA | 2018–2024 | Country-year-creditor scenario | 594 | 81 |

*Notes:* The DAC-reference comparison includes 448 of the 462 Morris–Parks–Gardner records because it also requires recipient eligibility. The IDS row shows the central-period subset. Counts across sources and observation types should not be added together. *Source:* This thesis’s calculations from the sources described above and the borrowing benchmarks constructed in Chapter 3.

The borrower benchmarks primarily describe dollar borrowing, so loan currency affects the interpretation of the resulting concessionality estimates. CRS expresses commitment amounts in dollars for reporting, but the extract used here leaves contractual currency unidentified. The Morris–Parks–Gardner and ADD records include dollar loans alongside other or unidentified currencies. The calculations normalize each advance to 100 units, retain its repayment proportions and timing, and apply the primarily dollar-based borrowing reference to those payments. For non-dollar loans, this assumes a common currency without converting actual future payments at projected exchange rates. AidData’s sample consists of dollar-denominated loans, and a separate calculation restricted to 499 dollar-denominated ADD records provides a comparison within that currency. The CRS extract cannot support the same restriction.

## 6.2 Reconstructing repayment schedules

The concessionality comparison depends on both the amounts repaid and their timing, so this thesis reconstructs a payment schedule from the reported loan terms. Following the present-value approach described by Scott (2017, sections 1.2–1.4), each schedule is valued under the alternative discount rates. The calculation assumes the full loan is advanced at the outset and holds the recorded interest rate or service charge constant. For variable-rate loans, this is a simplifying assumption about future interest payments. In CRS, the reported repayment dates, frequency and repayment type determine whether principal is repaid in equal instalments or through a constant combined principal-and-interest payment (OECD n.d.-d). Interest is calculated on the outstanding balance and is also paid during grace periods. The advance is placed on the recorded commitment date. These schedules include principal and the interest or service charge represented in the rate input, excluding separately reported fees and actual staged disbursements.

For the Morris–Parks–Gardner, AidData and ADD records, this thesis models equal principal repayments twice yearly. The calculation builds on the grant-element approach used by Morris, Parks and Gardner (2020, replication code) and the repayment convention described by AidData (2025, 28–29). Using that calculation for ADD is this thesis’s adaptation. Some World Bank records in these sources label the period from first to final principal repayment as maturity. This thesis includes the preceding grace period when reconstructing total loan duration, using archived dates or the source’s repayment-span definition. In the Morris–Parks–Gardner comparison, the effect of this reconstruction is assessed separately at their original 5% discount rate before changing to borrower-specific rates.

Following ONE’s method, the IDS comparison uses each country-year-creditor’s average terms to construct annual equal-principal repayments after grace, with interest on the remaining balance (ONE Data n.d., “The debt service schedule”). The resulting schedule describes a loan with those average terms. Its calculated grant element need not equal the average grant element of the actual loans because the present-value calculation is nonlinear (ONE Data n.d., note 8). The loan-record and average-term results therefore remain separate. Chapter 7 reports sensitivity to alternative interest-payment assumptions.

## 6.3 Discount rates and matching loans to borrowing benchmarks

To measure the effect of borrower-specific discounting, this thesis values each repayment schedule using both the eligible borrowing benchmark for the recipient’s commitment year and the standardized reference. The annual borrower rate applies throughout the schedule, with the loan terms and weights held identical across the two valuations. For bond-derived references, the quoted percentage is used numerically as the effective annual discount rate, without a compounding conversion. These are retrospective annual-reference comparisons: rates can include information recorded after commitment, and no loan-specific discount curve is fitted to the payment dates. The main loan comparisons exclude peer-only estimates. Additional results include those estimates or restrict the sample to primary issuance and IDS bondholder rates. Since these restrictions also change the loans represented, direct comparisons between rate methods use only loans for which both methods are available.

The main reference follows the DAC recipient categories applicable in the commitment year: 9% for least-developed and other low-income countries, 7% for lower-middle-income countries and 6% for upper-middle-income countries (OECD DAC 2014, Annex 2; OECD DAC n.d.). The central comparison covers 2018–2024, when grant-equivalent reporting became the headline ODA standard (OECD 2019). Applying the same category rule before 2018 permits a consistent comparison over 2012–2024, with those earlier values interpreted under the later framework. Separate calculations use the former DAC reference of 10% before 2018 and the common 5% rate used by Morris, Parks and Gardner (OECD DAC 2014, Annex 2, paragraph 3; Morris, Parks and Gardner 2020, 8–9). These references provide a common basis for assessing loan terms across creditors. The calculations measure concessionality rather than determining each loan’s contribution to official ODA statistics.

## 6.4 Summarizing the comparisons and assessing uncertainty

Chapter 7 reports the borrower-rate grant element minus the standardized-reference grant element, in percentage points. A positive difference means that the same financing appears more concessional relative to the borrower’s borrowing conditions. The present value of repayments per 100 advanced provides the equivalent presentation introduced in Chapter 2, following ONE (ONE Data n.d.). Equal-record averages describe the financing records represented in the sample, and commitment-weighted averages give larger loans greater weight. Medians help assess the influence of extreme observations on the mean. The IDS comparison instead averages country-year-creditor scenarios. AidData also permits averaging across identified financing events, so that events with several records do not automatically receive more weight. Annual results accompany comparisons between 2018–2021 and 2022 onward. Since these periods contain different loans, comparisons retaining common countries, creditors or benchmark methods help assess the contribution of changing sample composition.

For AidData and ADD, approximate country-based 95% intervals assess the difference between the earlier and later period averages. The calculation repeatedly omits one country’s entire set of loans and recalculates that difference, allowing for related observations from the same borrower. Resampling whole countries provides an additional check (Cameron and Miller 2015, section II.F). These intervals hold the observed years, loan terms and borrowing-rate estimates fixed. The six or seven years available provide limited evidence about whether the period difference would recur under other borrowing conditions. CRS results are reported descriptively, with weighting and sample-composition checks.

[^ch6_crs_records]: CRS sometimes divides a loan among activities (Manger et al. 2025, 11). This thesis combines World Bank entries with a shared loan identifier, year and compatible terms. Other records retain the CRS activity unit.

[^ch6_source_overlap]: The datasets partly share underlying loans, including ADD records drawn from CRS and AidData (Manger et al. 2025, 11–12). Results remain separate, and agreement between sources is interpreted with that overlap in mind.

## References

African Debt Database. 2026. *African Debt Database*. Version 2026May-a. Accessed 10 September 2026. [Dataset](https://africandebtdatabase.com/downloads/ADD_v2026May_a.xlsx).

AidData. 2025. *China's Loans and Grants to Low- and Middle-Income Countries Dataset*, Version 1.0. Released 18 November 2025. Accompanying *Field Definitions*, 28–29. [Dataset and documentation](https://www.aiddata.org/data/chinas-loans-and-grants-to-low-and-middle-income-countries-dataset-1-0).

Cameron, A. Colin, and Douglas L. Miller. 2015. “A Practitioner’s Guide to Cluster-Robust Inference.” *Journal of Human Resources* 50(2): 317–372. [Author-hosted paper](https://cameron.econ.ucdavis.edu/research/Cameron_Miller_JHR_2015.pdf).

Harcourt, Sara, Miguel Haro Ruiz, and Jorge Rivera. 2026. “Priced Out: The Rising Cost of Borrowing for Low- and Lower-Middle-Income Countries.” ONE Data, 14 April. [Article](https://data.one.org/analysis/priced-out-borrowing-costs-developing-countries).

Manger, Mark S., David Mihalyi, Ugo Panizza, Niccolò Rescia, Christoph Trebesch, and Ka Lok Wong. 2025. *Africa's Domestic Debt Boom: Evidence from the African Debt Database*. CEPR Discussion Paper 20747. October. [Paper](https://africandebtdatabase.com/downloads/ADD_2025_debt_07Oct2025.pdf).

Morris, Scott, Brad Parks, and Alysha Gardner. 2020. *Chinese and World Bank Lending Terms: A Systematic Comparison Across 157 Countries and 15 Years*. CGD Policy Paper 170. April. [Paper](https://www.cgdev.org/publication/chinese-and-world-bank-lending-terms-systematic-comparison). [Data and replication code](https://www.cgdev.org/sites/default/files/morris-parks-gardner-china-world-bank-data-code.zip).

OECD Development Assistance Committee (OECD DAC). 2014. *Final Communiqué of the 2014 DAC High Level Meeting*. DCD/DAC(2014)69/FINAL. [Communiqué](https://one.oecd.org/document/DCD/DAC%282014%2969/FINAL/en/pdf).

OECD Development Assistance Committee (OECD DAC). n.d. *DAC Lists of ODA Recipients*. Historical lists applicable to 2012–2024 flows. Accessed 8 September 2026. [Historical lists](https://webfs.oecd.org/oda/DAClists/).

OECD. 2019. “Development Aid Drops in 2018, Especially to Neediest Countries.” Press release, 10 April. Accessed 13 September 2026. [Release](https://www.oecd.org/en/about/news/press-releases/2019/04/development-aid-drops-in-2018-especially-to-neediest-countries.html).

OECD. 2024. “Development Finance Statistics: Resources for Reporting.” 30 October. Accessed 14 September 2026. [Reporting documentation](https://www.oecd.org/en/data/insights/data-explainers/2024/10/resources-for-reporting-development-finance-statistics.html).

OECD. n.d.-d. *DAC Tables and CRS Codebook*. Excel workbook, “CRS bulk data – codebook” sheet. Copy downloaded 25 May 2026. [Codebook](https://webfs.oecd.org/oda/DataCollection/Resources/DAC-tables-CRS-codebook.xlsx).

ONE Data. n.d. “Priced Out: Methodology.” Accessed 14 September 2026. [Methodology](https://docs.one.org/methodologies/priced-out/).

Scott, Simon. 2017. *The Grant Element Method of Measuring the Concessionality of Loans and Debt Relief*. OECD Development Centre Working Papers 339. [DOI](https://doi.org/10.1787/19e4b706-en).

World Bank. n.d.-c. *International Debt Statistics*. Dataset, creditor-specific terms on new external debt commitments, observations for 2012–2024. [Dataset](https://datacatalog.worldbank.org/search/dataset/0038015/international-debt-statistics).
