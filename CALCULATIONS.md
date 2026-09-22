# From source files to the paper

The entry point is `reproduce.py`; the scientific stage runner is `tools/run_all.R`. It uses the existing benchmark-construction code and the dependency order in `workspace/config/p15_analysis_registry.csv`.

## Benchmark construction

The raw rebuild constructs historical source inputs, reconciles primary and secondary observations, reconstructs IDS bondholder terms and rating-implied estimates, and checks the preserved 2024 reference. It then applies the September 2026 source/status decisions and accepted revised peer method to the full 2012–2024 panel. The peer method uses the recorded matching rules, three-notch rating distance, at least three distinct donor countries and the established donor-source priority.

The accepted configuration is `workspace/config/p15_revised_peer_20260912_v1.json`. Curated decisions and historical classification inputs are frozen with the source snapshot. Their upstream construction code and source material are included in the relevant `experiments/` folders.

## Analysis stages and chapters

| Paper content | Principal stages |
|---|---|
| Chapter 3: construction and coverage | Raw benchmark build; `peer_description`; manuscript construction checks |
| Chapter 4: benchmark evaluation | `assessment`, `deeper`, `benchmark_inference`, `peer_geography_validation`, `rating_nested`, `rating_gap_influence` |
| Chapter 5: country, regional and time patterns | `regional`, `regional_followup`, `gbohoui_context` |
| Chapter 6: loan records and repayment calculations | `dac`, `official_policy`, `loan_comparisons`, `crs_application`, `source_overlap_currency` |
| Chapter 7: concessionality comparisons | `policy`, `pv_first`, `bullet`, `pv_interpretation`, `pv_followup`, `pv_three_block`, `repayment_influence`, `loan_period_inference`, `statistical_review`, `crs_companions`, `crs_modern_summary`, `panel_disbursement` |
| Chapter 8: interpretation | Chapter 7 outputs and the source-attributed literature in the manuscript |

The stage IDs are file-navigation labels. The paper explains the substantive comparisons and the sources from which the methods were adopted or adapted.

## Tables and figures

`tools/build_displays.R` runs the manuscript preparation/check scripts in chapter order. These scripts expose the underlying counts and numerical tables for the paper’s 17 tables and regenerate its four figures. The final table wording and rounding are preserved in the reader-facing Markdown; the checks reconstruct their numerical basis from the analysis outputs.

| Figure | Script under `workspace/docs/thesis_design/sections/` |
|---|---|
| Annual benchmark-source coverage | `benchmark_evaluation/coverage_prose_20260920/build_coverage_figure.R` |
| Overall borrowing conditions | `borrowing_conditions/prose_5_1_5_2_20260921/build_figures.R` |
| Regional borrowing conditions | The same borrowing-conditions figure script |
| Annual concessionality comparisons | `concessionality_results/prose_20260922/prepare_and_check.R` |

`workspace/docs/thesis_design/manuscript/build_review.py` assembles the complete paper, bibliography, cover, tables and figures. Its inclusion lists retain the original drafting-status metadata; that metadata does not affect the scientific calculations.
