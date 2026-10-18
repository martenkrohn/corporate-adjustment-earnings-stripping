# STEP 5 — Estimate cash-holdings effects.
#
# The cash design uses the 2019 implementation benchmark and omits 2018 from
# the event study. The outcome and all control sequences are defined here.

cash_data <- analysis_state$samples$cash |>
  dplyr::mutate(post_2019 = as.integer(.data$year >= 2019L))

cash_firm_controls <- c("log_assets", "profitability", "tangibility", "dta", "growth")
cash_country_controls <- c(
  "corp_tax_rate", "gdp_growth", "inflation", "interest_rate",
  "rule_of_law_pct", "regulatory_quality_pct", "corruption_pct"
)

cash_spec <- list(
  analysis_id = "cash",
  outcomes = tibble::tribble(
    ~model_id, ~outcome, ~label,
    "cash_assets_main", "cash_assets", "CashAssets"
  ),
  firm_controls = cash_firm_controls,
  country_controls = cash_country_controls,
  period = "post_2019",
  post_start = 2019L,
  reference_year = 2018L,
  fixed_effects = c("bv_d_id_number", "year"),
  cluster_variable = "country_iso_code",
  preferred_control_set = "full_usable",
  control_metadata_style = "control_set_id",
  report_event_control_groups = FALSE,
  control_sets = list(
    firm_only = cash_firm_controls,
    macro_tax_rate = c(
      cash_firm_controls, "corp_tax_rate", "gdp_growth", "inflation", "interest_rate"
    ),
    full_usable = c(cash_firm_controls, cash_country_controls)
  )
)

cash_results <- run_analysis_specification(cash_data, cash_spec)

analysis_state$samples$cash <- cash_data
analysis_state$specifications$cash <- cash_spec
analysis_state$baseline$cash <- cash_results
