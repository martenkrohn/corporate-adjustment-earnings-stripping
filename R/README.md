# Empirical analysis

Open `thesis_analysis.Rproj` in RStudio. Before running models, read the [data availability note](../DATA_AVAILABILITY.md) and [reproduction guide](../docs/REPRODUCIBILITY.md).

`CHECK_INPUTS.R` checks external-file availability without installing packages. `RUN_ALL.R` runs the same preflight, prepares the project library, and runs the numbered analysis stages in a clean session.

| Path | Purpose |
| --- | --- |
| `code/00_config.R` | Paths, country groups, sample windows, and thresholds. |
| `code/01_prepare_data.R` | Import, cleaning, merges, variables, and samples. |
| `code/02_descriptive_outputs.R` | Descriptive and sample-construction tables. |
| `code/03_leverage_analysis.R` | Capital-structure models and event studies. |
| `code/04_risk_analysis.R` | Forward earnings-volatility models. |
| `code/05_cash_analysis.R` | Cash-holdings models. |
| `code/06_robustness.R` | Matching, ATT weighting, heterogeneity, and COVID sensitivity. |
| `code/07_investment_feasibility.R` | Data-availability audits for additional outcomes. |
| `code/08_export_results.R` | Result registries and output generation. |
| `functions/` | Reusable data, estimation, matching, sensitivity, and export algorithms. |

Submitted artifacts are at `results/submitted/`. A fresh run writes to the ignored `results/generated/` directory.
