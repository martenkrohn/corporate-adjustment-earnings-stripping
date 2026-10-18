# STEP 6 — Run robustness and heterogeneity analyses.
#
# This file declares the retained variants and research choices. Shared helper
# files implement matching, estimation, classification, and COVID algorithms.

analysis_ids <- c("leverage", "risk", "cash")

# A. Observable comparability -------------------------------------------------
# Both designs use firm controls measured in the last pre-treatment year:
# nearest-neighbour propensity-score matching is 1:1, without replacement,
# with a 0.001 caliper; ATT weighting assigns treated firms weight 1 and control
# firms p/(1-p).
matching_variant_registry <- function(anchor_year) {
  suffix <- as.character(anchor_year)
  tibble::tribble(
    ~variant_id, ~variant_label, ~weight_col, ~method, ~distance, ~ratio, ~replacement, ~caliper, ~exact_cols, ~note,
    paste0("psm_nn_1_1_norepl_", suffix),
    paste0(anchor_year, " nearest-neighbor 1:1 PS match, no replacement"),
    paste0("psm_nn_1_1_norepl_", suffix, "_weight"),
    "nearest", "pscore", 1L, FALSE, 0.001, list(character()),
    "Nearest-neighbor propensity-score matching, 1:1 without replacement, caliper 0.001.",
    paste0("ipw_att_", suffix),
    paste0(anchor_year, " propensity-score ATT reweighted sample"),
    paste0("ipw_att_", suffix, "_weight"),
    "ipw", "pscore", NA_integer_, NA, NA_real_, list(character()),
    paste0(
      "ATT inverse-propensity weighting using ", anchor_year,
      " covariates; treated firms weight 1 and control firms weight p/(1-p)."
    )
  )
}

matching_specs <- list(
  leverage = list(
    analysis_id = "leverage", anchor_year = 2018L,
    covariates = analysis_state$specifications$leverage$firm_controls,
    variants = matching_variant_registry(2018L)
  ),
  risk = list(
    analysis_id = "risk", anchor_year = 2016L,
    covariates = analysis_state$specifications$risk$firm_controls,
    variants = matching_variant_registry(2016L)
  ),
  cash = list(
    analysis_id = "cash", anchor_year = 2018L,
    covariates = analysis_state$specifications$cash$firm_controls,
    variants = matching_variant_registry(2018L)
  )
)

matching_results <- lapply(analysis_ids, function(id) {
  run_matching_analysis(
    analysis_state$samples[[id]],
    analysis_state$specifications[[id]],
    matching_specs[[id]]
  )
})
names(matching_results) <- analysis_ids

# B. Policy-regime heterogeneity ---------------------------------------------
# Each treated-country group is compared with all baseline control countries.
policy_regime_variants <- tibble::tibble(
  regime_id = c("no_ilr", "only_tcr", "pre_existing_esr"),
  regime_label = c(
    "No pre-reform interest-limitation rule",
    "Only pre-reform thin-capitalization rule",
    "Pre-reform earnings-stripping rule not equally effective"
  ),
  treated_countries = list(
    c("CY", "EE", "LU", "MT", "NL", "SE"),
    c("BE", "BG", "HR", "CZ", "HU", "LT"),
    c("DK", "FI", "IT", "PT")
  ),
  paper_benchmark = c(-0.811, -0.964, -0.383)
)

regime_specs <- lapply(analysis_ids, function(id) {
  list(analysis_id = id, variants = policy_regime_variants)
})
names(regime_specs) <- analysis_ids

regime_results <- lapply(analysis_ids, function(id) {
  run_policy_regime_analysis(
    analysis_state$samples[[id]],
    analysis_state$specifications[[id]],
    regime_specs[[id]]
  )
})
names(regime_results) <- analysis_ids

# C. Firm-level heterogeneity -------------------------------------------------
# Classifications use only pre-treatment information. Their years, minimum
# support, EBITDA threshold, pressure share, and admitted countries are copied
# explicitly from the central configuration into this step's visible registry.
heterogeneity_settings <- list(
  included_countries = analysis_config$heterogeneity$included_countries,
  risk_anchor_years = analysis_config$heterogeneity$risk_anchor_years,
  risk_min_anchors = analysis_config$heterogeneity$risk_min_anchors,
  pressure_years = analysis_config$heterogeneity$pressure_years,
  pressure_min_years = analysis_config$heterogeneity$pressure_min_years,
  ebitda_threshold = analysis_config$heterogeneity$ebitda_threshold,
  high_pressure_share = analysis_config$heterogeneity$high_pressure_share
)
operational_classification <- build_operational_risk_classification(
  analysis_state$panel,
  analysis_state$samples$leverage,
  heterogeneity_settings
)
pressure_classification <- build_restriction_pressure_classification(
  analysis_state$panel,
  analysis_state$samples$leverage,
  heterogeneity_settings
)
operational_heterogeneity <- run_operational_risk_heterogeneity(
  analysis_state$samples$leverage,
  operational_classification,
  analysis_state$specifications$leverage$control_sets$full_usable
)
pressure_heterogeneity <- run_restriction_pressure_heterogeneity(
  analysis_state$samples$leverage,
  pressure_classification,
  analysis_state$specifications$leverage$control_sets$full_usable
)

# D. COVID sensitivity --------------------------------------------------------
# Estimate the preferred model without pandemic controls, with deaths, with
# Oxford stringency, and with both. Risk uses matching forward control windows;
# leverage and cash use contemporaneous country-year controls.
covid_variants <- tibble::tribble(
  ~variant_id, ~variant_label, ~display_order, ~covid_controls,
  "baseline", "Main model", 1L, list(character()),
  "deaths", "Confirmed deaths per million", 2L, list("deaths"),
  "stringency", "Oxford stringency", 3L, list("stringency"),
  "deaths_stringency", "Deaths and Oxford stringency", 4L, list(c("deaths", "stringency"))
)

covid_country_mapping <- stats::setNames(
  c(
    "BEL", "BGR", "CYP", "CZE", "DEU", "DNK", "EST", "ESP", "FIN", "FRA", "GRC",
    "HRV", "HUN", "ITA", "LTU", "LUX", "MLT", "NLD", "PRT", "SWE", "SVN", "SVK"
  ),
  c(
    "BE", "BG", "CY", "CZ", "DE", "DK", "EE", "ES", "FI", "FR", "GR",
    "HR", "HU", "IT", "LT", "LU", "MT", "NL", "PT", "SE", "SI", "SK"
  )
)
covid_annual <- read_covid_annual_inputs(
  analysis_config$paths$covid_deaths,
  analysis_config$paths$covid_stringency,
  covid_country_mapping
)
validate_covid_source_record(
  analysis_config$paths$covid_deaths_source_record,
  analysis_config$paths$covid_deaths_raw,
  analysis_config$paths$covid_deaths
)
validate_covid_source_record(
  analysis_config$paths$covid_stringency_source_record,
  analysis_config$paths$covid_stringency_raw,
  analysis_config$paths$covid_stringency
)
covid_risk_windows <- build_covid_risk_windows(covid_annual$deaths, covid_annual$stringency)
covid_results <- run_covid_sensitivity(
  analysis_state$samples,
  covid_annual,
  covid_risk_windows,
  analysis_state$baseline,
  analysis_state$specifications,
  covid_variants
)

# Preserve every result and supporting classification for Step 8.
analysis_state$robustness <- list(
  matching = matching_results,
  regimes = regime_results,
  operational_classification = operational_classification,
  pressure_classification = pressure_classification,
  operational_heterogeneity = operational_heterogeneity,
  pressure_heterogeneity = pressure_heterogeneity,
  covid_annual = covid_annual,
  covid = covid_results
)
