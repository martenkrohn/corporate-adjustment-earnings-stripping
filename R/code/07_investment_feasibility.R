# STEP 7 — Check whether additional outcomes are feasible.
#
# This is an evidence boundary, not an estimation step. The helpers inspect
# both raw Orbis column names and the analysis-ready data for:
# - a direct capital-expenditure or fixed-asset-additions flow; and
# - daily market returns, prices, beta, or market-volatility data.
# Stock measures such as fixed assets are not treated as investment flows.
# The resulting audits explain why investment and market-based risk effects are
# not estimated when the required source variables are absent.

investment_audit_data <- analysis_state$samples$leverage |>
  dplyr::mutate(post_2019 = as.integer(.data$year >= 2019L))
# Each audit returns candidate columns, coverage summaries, and a decision row.
investment_audit <- investment_feasibility_audit(investment_audit_data, analysis_config$paths$orbis)
market_risk_audit <- market_risk_availability_audit(analysis_config$paths$orbis)
analysis_state$investment <- investment_audit
analysis_state$market_risk <- market_risk_audit
