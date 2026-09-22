# Verification — 22 September 2026

The package was tested in an isolated export of the research project on macOS with R 4.3.3 and the locked scientific package versions. A separate GitHub clone was used to test downloading, extracting, checking and relocating the complete release snapshot.

| Check | Result |
|---|---|
| Private repository visibility | Confirmed with GitHub |
| Release download and file checksums | All archive parts and all 4,250 snapshot files verified |
| Registered input and code dependencies | All 1,421 present in the separately downloaded workspace; raw snapshot hashes verified |
| Raw construction replay | All 20 construction stages and subsequent source-review steps completed; all six acceptance gates passed |
| Raw comparison fixtures | All 153 comparisons passed; exact keys, text, missingness and order, with the existing absolute numerical tolerance of 1e-12 |
| Accepted peer-method reconstruction | Completed from the packaged source features, matching rules and rebuilt source evidence |
| Empirical analysis | All 27 stages completed |
| Numerical output comparisons | All 497 table comparisons passed at tolerance 1e-10, including compressed tables |
| Orchestration | The packaged `targets` entry point completed successfully |
| Focused calculation tests | 9 repayment tests and 22 revised-peer tests passed, without warnings or skips |
| Manuscript displays | All 16 preparation/check scripts completed; all four regenerated PNG figures are byte-identical to the submitted paper’s figures |
| Manuscript build | 55 pages, 55 references; extracted PDF text identical to the submitted paper |
| Original research files | Original active-run pointer and submitted PDF unchanged |

Detailed records are in [verification/](verification/). Numerical table comparisons check schemas, row counts and numeric/logical columns in row order. File-path and hash metadata are expected to change when the workspace is relocated. The raw construction replay additionally checks text and ordering under its existing comparison rules.

The rebuilt PDF may have a different binary hash because PDF generation records build metadata. The original sent attachment is preserved unmodified at `paper/Thesis.pdf`, SHA-256 `9dd59a52819eb6bd714b8bc846ca825ea7ce6274cbb3c7d4eb2e25fe081698a3`.

The clean calculation and package tests used the existing matching R installation on the same Mac. The lockfile and restoration commands are supplied for installation elsewhere; a fresh operating-system installation was not tested. Historical classifications, manually reviewed source decisions and prepared macroeconomic source features remain explicit frozen inputs, with their underlying source records and preparation code included.
