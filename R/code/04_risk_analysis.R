# STEP 4 — Estimate operational-risk effects.
#
# The forward-looking risk design uses the 2017 announcement benchmark and
# omits 2016 from the event study. These choices are deliberately visible here.

risk_data <- analysis_state$samples$risk |>
  dplyr::mutate(reform_2017 = as.integer(.data$year >= 2017L))

risk_firm_controls <- c("log_assets", "profitability", "growth", "log_sales")
risk_country_controls <- c(
  "corp_tax_rate", "gdp_growth", "inflation", "interest_rate",
  "rule_of_law_pct", "regulatory_quality_pct", "corruption_pct"
)

risk_spec <- list(
  analysis_id = "risk",
  outcomes = tibble::tribble(
    ~model_id, ~outcome, ~label,
    "risk_earnvol", "earnvol", "EarnVol",
    "risk_unadj_earnvol", "unadj_earnvol", "UnadjEarnVol"
  ),
  firm_controls = risk_firm_controls,
  country_controls = risk_country_controls,
  period = "reform_2017",
  post_start = 2017L,
  reference_year = 2016L,
  fixed_effects = c("bv_d_id_number", "year"),
  cluster_variable = "country_iso_code",
  preferred_control_set = "full_usable",
  control_metadata_style = "control_set_id",
  report_event_control_groups = FALSE,
  control_sets = list(
    firm_only = risk_firm_controls,
    macro_institutional = c(
      risk_firm_controls, "corp_tax_rate", "gdp_growth", "inflation", "interest_rate"
    ),
    full_usable = c(risk_firm_controls, risk_country_controls)
  )
)

risk_results <- run_analysis_specification(risk_data, risk_spec)

analysis_state$samples$risk <- risk_data
analysis_state$specifications$risk <- risk_spec
analysis_state$baseline$risk <- risk_results
