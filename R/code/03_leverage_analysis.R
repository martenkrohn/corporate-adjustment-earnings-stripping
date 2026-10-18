# STEP 3 — Estimate capital-structure effects.
#
# This file is the readable specification of the leverage analysis. The shared
# estimators in `estimation_helpers.R` apply the choices declared here without
# selecting outcomes, controls, timing, or reference years on their own.

leverage_firm_controls <- c("tangibility", "roa_ebitda", "log_assets", "growth_options")
leverage_country_controls <- c(
  "corp_tax_rate", "gdp_growth", "inflation", "interest_rate",
  "rule_of_law_pct", "regulatory_quality_pct", "corruption_pct"
)

leverage_spec <- list(
  analysis_id = "leverage",
  outcomes = tibble::tribble(
    ~model_id, ~outcome, ~label,
    "baseline_dta", "dta", "DtA",
    "baseline_ln_equity", "ln_equity", "ln_Equity",
    "baseline_ln_current_debt", "ln_current_debt", "ln_CurrentDebt",
    "baseline_ln_longterm_debt", "ln_longterm_debt", "ln_LongtermDebt"
  ),
  firm_controls = leverage_firm_controls,
  country_controls = leverage_country_controls,
  period = "post",
  post_start = 2019L,
  reference_year = 2018L,
  fixed_effects = c("bv_d_id_number", "year"),
  cluster_variable = "country_iso_code",
  preferred_control_set = "full_usable",
  control_metadata_style = "control_set",
  report_event_control_groups = TRUE,
  control_sets = list(
    firm_only = leverage_firm_controls,
    no_interest = c(
      leverage_firm_controls, "corp_tax_rate", "gdp_growth", "inflation",
      "rule_of_law_pct", "regulatory_quality_pct", "corruption_pct"
    ),
    full_usable = c(leverage_firm_controls, leverage_country_controls)
  )
)

# Estimate every outcome under each control set. The event studies use the
# preferred full-control specification and omit 2018.
leverage_results <- run_analysis_specification(
  analysis_state$samples$leverage,
  leverage_spec
)

# Preserve both the visible specification and the structured results for the
# robustness and export steps.
analysis_state$specifications <- list(leverage = leverage_spec)
analysis_state$baseline <- list(leverage = leverage_results)
