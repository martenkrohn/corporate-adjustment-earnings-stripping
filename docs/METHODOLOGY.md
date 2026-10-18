# Methodology and interpretation

## Research design

The thesis studies financing, operating earnings volatility, and liquidity as three potential responses to earnings-stripping rules. The policy contrast is between countries newly exposed to the ATAD-era environment and countries with a pre-2019 interest-limitation regime classified as equally effective.

Models include firm fixed effects, year fixed effects, and outcome-specific time-varying firm and country controls. Treatment is assigned by headquarters country. Standard errors are clustered at that country level; the preferred full-control models retain 20 countries.

| Design element | Leverage | Operational risk | Cash holdings |
| --- | --- | --- | --- |
| Main outcome | Long-term debt plus short-term loans/debt, divided by assets | Adjusted forward three-year volatility of EBIT/assets, multiplied by 100 | Cash and cash equivalents divided by assets |
| Common cutoff | 2019 | 2017 analytical cutoff | 2019 |
| Event-study reference year | 2018 | 2016 | 2018 |
| Preferred observations | 24,851 | 18,562 | 22,570 |
| Preferred firms | 2,655 | 2,183 | 2,427 |

The operational-risk cutoff is an analytical timing convention for overlapping forward windows, not a statement that every country's law changed in 2017. The last observable forward-window base year is 2021 because accounting data end in 2023.

Continuous constructed variables are winsorized at pooled 1st and 99th percentiles. Adjusted earnings use a country–two-digit-NACE–year peer benchmark; the code uses a leave-one-out benchmark when at least two observations are available and the firm's own value for a singleton cell. See `R/functions/data_helpers.R` for the exact construction.

## Robustness strategy

The analysis compares alternative firm and country control sets, 1:1 nearest-neighbor propensity-score matching without replacement using a 0.001 caliper, and ATT inverse-propensity weights. Leverage and cash matching use 2018 covariates; risk matching uses 2016 covariates.

Further checks vary pandemic-year support and add mortality and stringency controls. Risk models align pandemic controls with the forward outcome window. Heterogeneity analyses consider pre-reform policy regimes, pre-treatment operational risk, and an accounting-based restriction-pressure proxy.

## Evidence boundaries

- The comparison group was already regulated. The estimates are relative changes under a specific policy contrast.
- Headquarters-country exposure does not reveal which consolidated group entities face a binding statutory cap.
- Twenty country clusters limit the reliability of conventional clustered inference.
- Event-study diagnostics do not establish parallel trends. The debt-to-assets joint pre-trend p-value is 0.1959, but some individual pre-period terms and debt-component dynamics are less reassuring.
- Adjusted risk has a baseline joint pre-trend p-value of 0.0836; matched and reweighted risk models show stronger evidence of differential pre-period dynamics. Its negative estimate is suggestive.
- Country-specific shocks and the pandemic overlap may still influence the estimates.
- Outcome-specific complete cases and forward-window support produce different samples, limiting direct cross-outcome comparisons.
- Investment flows and daily market-based risk are not estimated because the required source variables are unavailable. Fixed-asset stocks are not substituted for investment flows.

The thesis, especially Chapters 5–9 and the appendix diagnostics, is the authoritative account of the design and its limitations.
