# STEP 1 — Build the analysis-ready firm-year data.
#
# The detailed import and transformation rules live in `data_helpers.R`.
# This orchestration script deliberately stays short: it calls those helpers
# in the order required to move from frozen raw inputs to analysis samples.

required_inputs <- unlist(analysis_config$paths[c(
  "orbis", "policy_panel", "policy_classification", "macro_panel",
  "covid_deaths", "covid_deaths_raw", "covid_deaths_source_record",
  "covid_stringency", "covid_stringency_raw", "covid_stringency_source_record"
)], use.names = FALSE)
missing_inputs <- required_inputs[!file.exists(required_inputs)]
if (length(missing_inputs) > 0L) stop(sprintf("Required raw inputs are missing: %s", paste(missing_inputs, collapse = ", ")), call. = FALSE)

# Data flow:
# 1. reshape the wide Orbis workbooks into one firm-year panel;
# 2. apply accounting-data cleaning and sample restrictions;
# 3. merge country-year policy and macroeconomic variables;
# 4. construct outcomes, controls, lags, and treatment indicators.
imported <- import_orbis(analysis_config$paths$orbis)
cleaned <- clean_orbis(imported$data, analysis_config$cleaning)
policy_panel <- read_policy_input(analysis_config$paths$policy_panel, cleaned$data)
macro_panel <- read_macro_input(analysis_config$paths$macro_panel)
merged_panel <- merge_policy_macro(cleaned$data, policy_panel, macro_panel)
constructed <- construct_variables(merged_panel)

# Create separate leverage, operational-risk, and cash samples because their
# timing windows and missing-data requirements differ. Each sample also gets
# the common treatment/control-country classification.
make_analysis_sample <- function(sample_id) {
  sampled <- apply_sample_definition(constructed$data, analysis_config$samples[[sample_id]])
  list(data = classify_countries(sampled$data, analysis_config$countries), filter_log = sampled$filter_log)
}

sample_outputs <- lapply(c("leverage", "risk", "cash"), make_analysis_sample)
names(sample_outputs) <- c("leverage", "risk", "cash")
analysis_samples <- lapply(sample_outputs, `[[`, "data")

# `analysis_state` is the shared in-memory handoff to Steps 2–8. Besides the
# data, it retains observation counts, filtering logs, missingness, and source
# diagnostics so the exported tables can document the full sample construction.
analysis_state <- list(
  panel = constructed$data,
  samples = analysis_samples,
  audits = list(
    import_counts = imported$counts, import_filter = imported$filter_log,
    clean_counts = cleaned$counts, clean_filter = cleaned$filter_log,
    variable_counts = constructed$counts, variable_missingness = constructed$missingness,
    debt_source = constructed$debt_source_summary, risk_window = constructed$risk_window_summary,
    sample_filter = sample_outputs$leverage$filter_log
  )
)
