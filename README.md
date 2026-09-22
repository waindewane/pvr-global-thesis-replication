# Sovereign borrowing benchmarks and borrower-side concessionality, 2012–2024

Replication package for Markus Wane’s bachelor’s thesis, **Measuring Borrower-Side Concessionality: Constructing a Sovereign Market-Benchmark Dataset, 2012–2024**, as sent to the supervisor on 22 September 2026.

[Read the thesis](paper/Thesis.pdf) · [Download the complete input snapshot](https://github.com/waindewane/pvr-global-thesis-replication/releases/tag/thesis-2026-09-22) · [Calculation map](CALCULATIONS.md) · [Verification record](VERIFICATION.md)

This repository contains the calculation code, original source files, curated inputs, comparison fixtures, manuscript sources and figure-generation code. All required data are attached to the release in this same repository. Large data files are stored as release assets rather than Git history. No additional data-access service is required.

## Reproduce

Install **R 4.3.3**, **Python 3.9 or later**, Git and the GitHub CLI. For the paper PDF, also install Pandoc and a TeX distribution with XeLaTeX. Dependency installation needs internet access; the scientific calculations use the included source snapshots without downloading new observations. Allow approximately 15 GB of free disk space for extraction and the isolated rebuild.

```sh
gh auth login
gh repo clone waindewane/pvr-global-thesis-replication
cd pvr-global-thesis-replication
python3 reproduce.py prepare
python3 reproduce.py restore
python3 reproduce.py all
```

`prepare` downloads and checks every archive part, extracts all inputs, checks every file against its SHA-256 manifest, and adjusts recorded folder paths for your machine. `restore` installs the locked R packages and Python dependencies. `all` rebuilds the benchmarks, runs all 27 analysis stages, compares results with the thesis snapshot, regenerates the displays and compiles the paper.

The results are written under `workspace/replication-results/`. The regenerated PDF is `workspace/docs/thesis_design/manuscript/complete_review/Thesis_Review.pdf`. The originally sent PDF remains unchanged at `paper/Thesis.pdf`.

Individual commands are available:

```sh
python3 reproduce.py run       # benchmarks and all 27 analysis stages
python3 reproduce.py verify    # compare rebuilt numerical tables with the thesis snapshot
python3 reproduce.py displays  # regenerate manuscript tables, checks and four figures
python3 reproduce.py pdf       # compile the complete manuscript
```

Keep a prepared workspace in the same folder. If moving to another location, clone again and prepare there. An interrupted calculation retains logs; a completed stage can be reused after its input and output hashes have been checked. Incomplete output directories are deliberately rejected rather than silently reused.

## What the files represent

- `workspace/R/` and `workspace/scripts/p15/`: benchmark construction and empirical analysis.
- `workspace/data-raw/`, `workspace/sources/` and archived source folders under `workspace/experiments/`: source data and supporting source artifacts.
- `workspace/config/`: input registries, source decisions, the accepted peer-estimation configuration and stage dependencies.
- `workspace/data-derived/`: preserved tables used as regression checks and prepared inputs with their provenance. The rebuild starts from source snapshots and curated decisions; these comparison tables are not substituted for newly calculated results.
- `workspace/docs/thesis_design/sections/`: manuscript text and scripts that prepare its tables and figures.
- `tools/`: portable packaging, environment restoration, orchestration and comparison utilities.
- `environment.lock`: the scientific environment plus the packages used to produce figures. `workspace/renv.lock` retains the original scientific lockfile.
- `file_manifest.json`: complete release snapshot, file sizes and SHA-256 checksums.

Manually reviewed source corrections and prepared historical classifications are explicit inputs, with their source records and construction code included. They are preserved rather than replaced with new downloads or fresh judgment calls. This reproduces the evidence used in the thesis.

The scientific methods and accepted decisions are unchanged. Portability adjustments replace absolute folder paths and refresh the associated provenance hashes. The release preserves the submitted paper as a research snapshot, without promoting it to a new dataset version.
