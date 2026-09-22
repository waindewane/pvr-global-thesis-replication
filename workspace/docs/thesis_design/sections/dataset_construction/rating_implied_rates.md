## Rating-implied borrowing rates

Sovereign credit ratings provide a basis for estimating borrowing rates, including for countries without observed bond yields. The estimate combines a US Treasury reference rate with a default spread associated with the borrower’s rating. The spread represents the additional yield associated with sovereign credit risk. This approach follows ONE’s *Priced Out* methodology, which adds a rating-based spread to the annual average seven-year US Treasury yield (ONE Data n.d., “The discount rate”). For country $i$ in year $t$, the calculation is

$$
\widehat{r}_{it}^{\mathrm{rating}}
= \overline{y}_{t}^{\mathrm{US},7}
+ s_t(R_{i,\mathrm{1\ January}\ t}),
$$

where $\overline{y}_{t}^{\mathrm{US},7}$ is the annual average seven-year Treasury yield and $s_t(R_{i,\mathrm{1\ January}\ t})$ is the spread assigned to the country’s rating at the beginning of that year. The Treasury yield and resulting estimate are expressed in per cent, and the spread in percentage points.

The Treasury component is the arithmetic mean of the available daily observations in FRED’s DGS7 series (Board of Governors of the Federal Reserve System 2026). The seven-year maturity follows ONE’s choice of a reference closer to the typical duration of emerging-market sovereign bonds than a ten-year yield (ONE Data n.d., n. 14). It supplies a common dollar reference for the country estimates; it is not matched separately to the maturity of each bond or official loan.

The country component uses the Moody’s foreign-currency sovereign rating in force on 1 January, reconstructed from Bloomberg’s rating-change records (Bloomberg 2026). That rating label is matched to the corresponding group in Damodaran’s historical default-spread tables, whose sovereign grades are based on local-currency ratings (Damodaran n.d.). The calculation therefore transfers spreads between foreign- and local-currency rating categories, which need not coincide for a given country. For each year, the calculation takes the median default spread among country entries carrying the same rating in the relevant annual archive. The median supplies one spread per rating group where individual entries differ. An estimate requires both a Moody’s rating and a matching spread; ratings from other agencies do not substitute for a missing Moody’s observation.

This construction produces a retrospective annual estimate. The rating is fixed at the start of the year, while the Treasury component reflects observations throughout the year and the spread comes from the historical table assigned to that year.[^rating-vintage] Countries with the same rating therefore receive the same estimate within a year. Differences in their borrowing conditions that the rating does not capture remain outside this approximation.

This thesis retains the Treasury-plus-spread formula as the main rating-based reference, without a fitted correction. Section 4.5 evaluates corrections using observed issuance and explains this choice: their gains are strongest when earlier observations are available for the same borrower, whereas most country-years relying on rating estimates lack that history.

# References cited in this subsection

Bloomberg. 2026. *Sovereign Rating Changes*. Static-value Excel exports from the Rating Changes view, archived May 28, 2026.

Board of Governors of the Federal Reserve System. 2026. *Market Yield on U.S. Treasury Securities at 7-Year Constant Maturity, Quoted on an Investment Basis \[DGS7\]*. Daily data retrieved from FRED, Federal Reserve Bank of St. Louis; snapshot of May 12, 2026, using observations for 2012–2024. <https://fred.stlouisfed.org/series/DGS7>.

Damodaran, Aswath. n.d. *Country Risk Premiums: Historical Spreadsheets*. Annual archive files labelled 2012–2024, ctryprem12.xls through ctryprem24.xlsx. Accessed 12 September 2026. <https://pages.stern.nyu.edu/~adamodar/New_Home_Page/dataarchived.html>.

ONE Data. n.d. *Priced Out Methodology*. Accessed 12 September 2026. <https://docs.one.org/methodologies/priced-out/>.

[^rating-vintage]: The historical spread data are assigned to years according to the labels of the annual releases. The observation dates of individual spreads are not consistently stated, and some releases contain updates from the following year. Together with the annual Treasury average, this prevents interpreting the estimates as information available on 1 January.
