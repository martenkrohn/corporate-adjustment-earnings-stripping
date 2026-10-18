# STEP 0 — Define the analysis design.
#
# This script contains no estimation. It gathers all paths, sample windows,
# country groups, cleaning rules, and heterogeneity thresholds in one object.
# Later scripts read `analysis_config` instead of repeating these choices.

if (!exists("ANALYSIS_ROOT", inherits = FALSE)) {
  stop("ANALYSIS_ROOT must be set by RUN_ALL.R.", call. = FALSE)
}

# `analysis_path()` builds portable paths below the detected package root.
analysis_path <- function(...) file.path(ANALYSIS_ROOT, ...)

# Main fields:
# - `paths`: portable locations below the analysis-package root;
# - `cleaning`: accounting-year and firm-screening rules;
# - `countries`: treatment, control, and excluded country groups;
# - `samples`: outcome-specific time windows and support requirements;
# - `heterogeneity`: pre-treatment classification thresholds.
analysis_config <- list(
  package_version = "2026-08-24",
  random_seed = 20260824L,
  export_model_csvs = isTRUE(EXPORT_MODEL_CSVS),
  keep_temporary_data = FALSE,
  paths = list(
    generated_models = analysis_path("results", "generated", "models"),
    generated_tables = analysis_path("results", "generated", "tables"),
    generated_figures = analysis_path("results", "generated", "figures"),
    orbis = c(
      analysis_path("data", "raw", "orbis", "Export 11_02_2026 11_19.xlsx"),
      analysis_path("data", "raw", "orbis", "Export 11_02_2026 11_19_part_2.xlsx")
    ),
    policy_panel = analysis_path("data", "raw", "policy", "policy_panel.csv"),
    policy_classification = analysis_path("data", "raw", "policy", "atad_classification.csv"),
    macro_panel = analysis_path("data", "raw", "macro", "macro_panel.csv"),
    covid_deaths = analysis_path("data", "raw", "covid", "covid_country_year.csv"),
    covid_deaths_raw = analysis_path("data", "raw", "covid", "owid-covid-data.csv"),
    covid_deaths_source_record = analysis_path("data", "raw", "covid", "covid_source_record.csv"),
    covid_stringency = analysis_path("data", "raw", "covid", "oxford_stringency_country_year.csv"),
    covid_stringency_raw = analysis_path("data", "raw", "covid", "OxCGRT_timeseries_StringencyIndex_v1.csv"),
    covid_stringency_source_record = analysis_path("data", "raw", "covid", "oxford_stringency_source_record.csv")
  ),
  cleaning = list(
    year_window = c(2012L, 2023L), remove_financials = TRUE,
    excluded_nace_2d = c(64L, 65L, 66L), harmonize_expense_signs = TRUE,
    enforce_positive_assets = TRUE, raw_winsorize = FALSE
  ),
  countries = list(
    control = c("FR", "DE", "GR", "SK", "SI", "ES"),
    treatment = c("BE", "BG", "HR", "CY", "CZ", "DK", "EE", "FI", "HU", "IT", "LT", "LU", "MT", "NL", "PT", "SE"),
    excluded = c("AT", "IE", "LV", "PL", "RO", "UK")
  ),
  samples = list(
    leverage = list(year_start = 2014L, year_end = 2023L, pre_period_end = 2018L,
                    post_period_start = 2019L, min_pre_periods = 1L, min_post_periods = 1L,
                    complete_case_vars = character()),
    risk = list(year_start = 2012L, year_end = 2023L, pre_period_end = 2016L,
                post_period_start = 2017L, min_pre_periods = 1L, min_post_periods = 1L,
                complete_case_vars = c("earnvol", "unadj_earnvol")),
    cash = list(year_start = 2014L, year_end = 2023L, pre_period_end = 2018L,
                post_period_start = 2019L, min_pre_periods = 1L, min_post_periods = 1L,
                complete_case_vars = character())
  ),
  heterogeneity = list(
    included_countries = c("BE", "BG", "CZ", "DE", "DK", "ES", "FI", "FR", "GR", "HR", "HU", "IT", "LT", "LU", "MT", "NL", "PT", "SE", "SI", "SK"),
    risk_anchor_years = 2012:2014, risk_min_anchors = 2L,
    pressure_years = 2014:2016, pressure_min_years = 2L,
    ebitda_threshold = 0.30, high_pressure_share = 0.50
  )
)

# Fix the random seed and prepare only the output folders requested for this
# run. Model CSVs are omitted in the default configuration.
set.seed(analysis_config$random_seed)
output_directories <- c("generated_tables", "generated_figures")
if (analysis_config$export_model_csvs) {
  output_directories <- c("generated_models", output_directories)
} else if (dir.exists(analysis_config$paths$generated_models)) {
  unlink(analysis_config$paths$generated_models, recursive = TRUE, force = TRUE)
}
ensure_output_directories(analysis_config$paths[output_directories])
