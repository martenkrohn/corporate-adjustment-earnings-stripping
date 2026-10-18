# Corporate Adjustment Beyond Leverage

**Evidence from European Firms under Earnings-Stripping Rules**

Master's thesis in Economics · Arne Marten Krohn · University of Münster · 2026

An empirical study of how European firms adjust their financing, operating earnings volatility, and cash holdings when exposed to earnings-based restrictions on interest deductibility. The project combines consolidated firm accounts with country-level policy, macroeconomic, institutional, and pandemic data in a modular R analysis.

**[Read the thesis](thesis/corporate-adjustment-beyond-leverage.pdf)** · **[Explore the analysis](R/code)** · **[Reproduce the research](docs/REPRODUCIBILITY.md)** · **[Data availability](DATA_AVAILABILITY.md)**

## Research question

Do earnings-stripping rules induce corporate adjustment beyond leverage?

The study compares firms headquartered in countries newly exposed to the ATAD-era policy environment with firms in countries that already had an interest-limitation regime classified as equally effective. The comparison group therefore also faced interest-limitation rules.

## Main findings

| Outcome | Preferred estimate | p-value | Interpretation |
| --- | ---: | ---: | --- |
| Debt-to-assets | −2.05 percentage points | 0.0068 | The clearest evidence of adjustment: lower leverage, accompanied by higher equity and lower debt components. |
| Adjusted forward earnings volatility | −1.96 percentage points | 0.0722 | Suggestive evidence of more stable subsequent operating earnings; sensitive to specification and pre-period dynamics. |
| Cash/assets | −0.39 percentage points | 0.5720 | No affirmative evidence of an increase in average cash buffers. |

These are conditional relative changes from separate outcome-specific models. They should not be read as universal policy effects. The volatility measure is 100 times a three-year forward standard deviation of adjusted EBIT/assets; it does not directly observe managerial risk choices. Estimates and p-values are transcribed from the submitted results, especially thesis Table 6 ([source table](R/results/submitted/tables/07_control_sets.tex)).

## Data and empirical design

- **Firm panel:** consolidated Orbis accounts for European non-financial firms, with accounting years 2012–2023 retained in construction.
- **Descriptive baseline:** 26,212 firm-year observations, 2,794 firms, and 22 countries over 2014–2023. The preferred leverage model uses 24,851 observations from 2,655 firms and 20 countries; the risk and cash samples differ.
- **Estimation:** difference-in-differences with firm and year fixed effects, time-varying firm and country controls, and standard errors clustered by headquarters country.
- **Timing:** 2019 for leverage and cash; a 2017 analytical cutoff for the forward-looking risk measure, whose windows span years t through t+2.
- **Diagnostics:** event studies, joint pre-trend tests, propensity-score matching, ATT reweighting, alternative controls, pandemic sensitivity, and policy-regime and firm-level heterogeneity.

The strongest finding is the direction of the leverage response across the selected checks. Country-level treatment, 20 clusters in the preferred models, imperfect pre-period dynamics, and overlap with the pandemic limit causal interpretation. See [methodology and limitations](docs/METHODOLOGY.md).

## What this project demonstrates

- **Data engineering in R:** importing split Excel exports, reshaping accounting panels, validating firm-year keys, harmonizing missing values and accounting signs, and merging country-year inputs.
- **Applied econometrics:** fixed-effects panel models, dynamic diagnostics, matching and weighting, and interpretation of uncertainty across specifications.
- **Research software organization:** one entry point, centralized configuration, numbered analysis stages, reusable helpers, a package lockfile, and frozen-source checksum checks.
- **Research communication:** a complete thesis, preserved submitted tables and figures, and a clear distinction between findings, suggestive patterns, and data limitations.

## Repository guide

```text
thesis/                    Thesis PDF with a portfolio cover
R/RUN_ALL.R                Analysis entry point
R/CHECK_INPUTS.R           Dependency-free input availability check
R/code/                   Numbered analysis stages, 00–08
R/functions/              Import, estimation, matching, and export helpers
R/data/raw/               Country-level inputs and source records
R/renv.lock               Reference R package versions
R/results/submitted/      Original submitted tables and figures
docs/                     Methodology, reproduction, and packaging notes
references/               Bibliographic records from the source archive
```

Start with [the leverage specification](R/code/03_leverage_analysis.R), [data preparation](R/code/01_prepare_data.R), and [robustness design](R/code/06_robustness.R) to see the empirical choices explicitly.

## Running the analysis

Reference environment: **R 4.5.1**. Main packages include `dplyr`, `tidyr`, `readr`, `readxl`, `fixest`, `ggplot2`, and `renv`.

This is a research portfolio with restricted-data dependencies. A fresh clone is sufficient to read the thesis and code; a complete empirical rerun additionally requires the licensed Orbis exports and two frozen pandemic source snapshots described in [DATA_AVAILABILITY.md](DATA_AVAILABILITY.md).

From the repository root, check input availability without installing packages:

```sh
Rscript --vanilla R/CHECK_INPUTS.R
```

After supplying the required inputs, open `R/thesis_analysis.Rproj` in RStudio and follow [the reproduction instructions](docs/REPRODUCIBILITY.md). Fresh analysis output goes to `R/results/generated/`; the submitted artifacts are preserved separately.

## Citation and reuse

Krohn, Arne Marten (2026). *Corporate Adjustment Beyond Leverage: Evidence from European Firms under Earnings-Stripping Rules*. Master's thesis, University of Münster.

Machine-readable citation metadata is in [CITATION.cff](CITATION.cff). No open-source license has been granted in this repository. Third-party sources retain their own terms; access to the firm-level data must be obtained separately.
