# STEP 2 — Produce descriptive and sample-construction tables.
#
# The helper prepares the underlying data frames. This script visibly maps each
# one to the filename, caption, and LaTeX label required by the thesis package.

descriptive_artifacts <- build_descriptive_artifacts(
  analysis_state$samples$leverage,
  analysis_state$audits,
  analysis_config$paths$policy_classification
)

descriptive_table_registry <- list(
  "03_sample_flow.tex" = list(
    data = descriptive_artifacts$sample_flow,
    caption = "Sample restrictions and attrition",
    label = "tab:data_sample_flow"
  ),
  "03_variable_summary.tex" = list(
    data = descriptive_artifacts$variable_summary,
    caption = "Main variable availability and descriptive statistics",
    label = "tab:data_variable_summary"
  ),
  "A03_policy_changes.tex" = list(
    data = descriptive_artifacts$policy_changes,
    caption = "Earnings-stripping-rule design in the baseline countries",
    label = "tab:appendix_policy_changes"
  ),
  "A03_country_composition.tex" = list(
    data = descriptive_artifacts$country_composition,
    caption = "Country-level sample composition",
    label = "tab:appendix_data_country_composition"
  ),
  "A03_pre_reform_balance.tex" = list(
    data = descriptive_artifacts$pre_reform_balance,
    caption = "Pre-reform treated-control differences in firm-level means",
    label = "tab:appendix_data_pre_reform_balance"
  )
)

descriptive_tables <- purrr::imap_chr(descriptive_table_registry, function(spec, filename) {
  write_latex_table(
    spec$data,
    file.path(analysis_config$paths$generated_tables, filename),
    spec$caption,
    spec$label
  )
})
