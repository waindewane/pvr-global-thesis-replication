## Observed bond evidence

The primary and secondary benchmarks are constructed from LSEG bond records, which link issue terms and instrument characteristics to dated yield and price observations (LSEG 2026). The collection and reconciliation of individual bond records build on the OECD’s *Global Debt Report 2025* (OECD 2025, Annex 3.A, pp. 134–136). The OECD provides the security-level collection precedent and the secondary maturity range cited below. The precise annual sampling window, reconciliation thresholds and source-selection rules are this thesis’s implementation choices.

The primary series uses the reported original yield to maturity to account for the issue price and the timing of promised interest and principal payments. Using the coupon rate alone would omit the effect of issuing above or below face value. The main analysis uses the USD bond benchmarks; EUR yields are retained separately because the two currencies have different underlying interest-rate curves. Both currency series require an original maturity of at least one year and a recorded face amount issued equivalent to at least USD 50 million. Provisions allowing early redemption by the issuer (issuer calls) or the retirement of principal before final maturity (sinking funds) can alter repayment timing. The primary series includes otherwise eligible fixed-rate bonds with these provisions when their original issue yield is reported, using that reported yield directly without independently valuing those repayment features. To focus on new medium- and long-term central-government borrowing on fixed-rate terms, the main primary aggregate excludes Treasury bills, central-bank issuance, debt-exchange or restructuring issues, and floating-rate, index-linked and other nonstandard structures.

Before aggregation, repeated records are reconciled using the borrower, issue and maturity dates, currency, coupon and recorded issue amount. Records representing the same underlying issue contribute only one issuance weight, using the median of their finite reported original yields. If their reported yields differ by more than 0.25 percentage points and the discrepancy remains unresolved, that issue is excluded from the average. Reopenings or tranches that cannot confidently be identified as the same issue remain separate. For the dollar series, the country-year primary rate is

$$
r^{P}_{ct} = \frac{\sum_{i\in I_{ct}} A_i y_i}{\sum_{i\in I_{ct}} A_i},
$$

where $I_{ct}$ contains the eligible issues of country $c$ in year $t$, $y_i$ is the original issue yield in percent, and $A_i$ is the recorded dollar face amount issued. The euro series is calculated separately using the source’s dollar-equivalent issue amounts. The weighting describes the average yield on the financing volume represented in the sample. The number of issues, their maturities and the largest issue’s weight are retained alongside the rate. A year with only one issue remains usable, but its rate describes that particular borrowing operation. The accompanying results identify years in which one issue accounts for at least 80% of the aggregate weight, because their average is dominated by the terms of that issue.

The secondary series describes borrowing conditions around year-end using quotations: recorded market prices or yields for bonds that have already been issued. Its construction has two steps: obtain a yield to maturity for each eligible bond, then average those yields into a country-level rate. For a given country, year and currency, the benchmark averages the eligible yields reported by LSEG. Only when no eligible reported yields are available is the benchmark calculated from bond prices instead. Price-derived yields are therefore not added to an average already based on reported yields.

To calculate a yield from a price, the bond’s remaining coupon and principal payments are discounted at the rate that makes their present value equal to its current price, including accrued interest. This defines yield to maturity (FINRA 2022). The implementation uses the reported mid-price, treated as a clean price excluding interest accrued since the preceding coupon payment. Adding that accrued interest gives the price used in the calculation, following the standard present-value relationship set out by Djatschenko (2020, sec. 2.4, eq. 1):

$$
P_i^{\mathrm{clean}} + AI_i
= \sum_{j=1}^{N_i}
\frac{CF_{ij}}{\left(1 + y_i/(100 f_i)\right)^{f_i\tau_{ij}}}.
$$

Here, prices, accrued interest $AI_i$ and payments $CF_{ij}$ are expressed per 100 units of face value. The payments comprise the remaining coupons and principal repayment of 100 at maturity. There are $N_i$ remaining payment dates; $f_i$ is the number of coupon payments per year. The time from the quotation date to payment $j$, $\tau_{ij}$, is measured in years as actual days divided by 365.25. The yield $y_i$ is a nominal annual percentage compounded at the coupon frequency and is solved numerically. Yields enter the country averages in their quoted form, without conversion to a common effective annual convention. This calculation is used only where the recorded prices and instrument terms support a fixed-coupon repayment schedule. Coupon dates are reconstructed backwards from maturity, and accrued interest is approximated from the elapsed fraction of the regular coupon period. The quotation date serves as the valuation date.

For each quoted instrument and year, the date rule selects the reported yield or complete price quotation closest to 31 December within a window extending 31 calendar days before and seven days after that date. The earlier observation is chosen in a tie. For the secondary benchmark, this thesis restricts remaining maturity to 2–15 years, matching the maturity range used in the OECD’s secondary-market comparison (OECD 2025, fig. 3.7, panel A, p. 121). The restriction limits differences arising from combining bonds close to repayment with much longer-term debt. Remaining maturity is measured at year-end for reported yields and at the quotation date for yields calculated from prices. To make yields more comparable across bonds with different contractual features, the secondary series excludes bonds with early-redemption, principal-retirement or conversion provisions, including callable, putable, sinking-fund and convertible bonds, as well as other nonstandard instruments. Its instrument scope is therefore narrower than that of the primary series.

Both secondary routes weight the eligible bond yields by recorded face amounts outstanding. For a given currency, the resulting country-year rate is

$$
r^{S}_{ct} = \frac{\sum_{i\in J_{ct}} W_i y_i}{\sum_{i\in J_{ct}} W_i},
$$

where $J_{ct}$ contains the eligible issues from the selected route and $W_i$ is the dollar-equivalent face amount outstanding recorded in the archived LSEG extracts acquired in 2026. These instrument attributes are reused with historical quotations, and their own observation dates are not retained. The year attached to a quotation therefore does not date its outstanding-balance weight. Multiple identifiers representing the same issue are consolidated before weighting. The secondary average therefore uses recorded amounts rather than reconstructed historical year-end balances. Differences from the primary rate can reflect both changes in borrowing conditions and differences in the instruments and weights represented.

# References cited in this subsection

Djatschenko, Wadim. 2020. ‘BondValuation: An R Package for Fixed Coupon Bond Analysis’. *The R Journal* 11 (2): 124–41. <https://doi.org/10.32614/RJ-2019-055>.

FINRA. 2022. *Understanding Bond Yield and Return*. <https://www.finra.org/investors/insights/bond-yield-return>.

LSEG. 2026. *Sovereign Bond Issue Terms and Market Quotations*. Archived data extracts acquired May–July 2026; observations for 2012–2024.

OECD. 2025. *Global Debt Report 2025: Financing Growth in a Challenging Debt Market Environment*. OECD. <https://www.oecd.org/en/publications/global-debt-report-2025_8ee42b13-en.html>.
