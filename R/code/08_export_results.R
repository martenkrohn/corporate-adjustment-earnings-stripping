# STEP 8 — Convert in-memory results into regenerable artifacts.
#
# Earlier steps retain rich R objects because tables and figures need more than
# a single coefficient. This script first builds named data-frame registries,
# then writes the thesis-facing LaTeX tables and PDF figures. Machine-readable
# model CSVs are written only when `EXPORT_MODEL_CSVS` is TRUE in `RUN_ALL.R`.
# Nothing in this script overwrites the frozen `results/submitted` directory.

generated_models <- analysis_config$paths$generated_models

# Collect baseline and event-study results in a named export registry.
exports <- list(
  "panel_imported_counts.csv" = analysis_state$audits$import_counts,
  "panel_cleaned_filter_log.csv" = analysis_state$audits$clean_filter,
  "sample_ready_main_filter_log.csv" = analysis_state$audits$sample_filter,
  "analysis_ready_baseline_counts.csv" = audit_counts_snapshot(analysis_state$samples$leverage, "analysis_ready_baseline"),
  "capital_structure_baseline_table.csv" = model_table(leverage_results, "treat:post") |> dplyr::filter(.data$control_set == "full_usable"),
  "capital_structure_validation_control_sets.csv" = model_table(leverage_results, "treat:post"),
  "capital_structure_event_study_coefficients.csv" = leverage_results$event_study$coefficients,
  "capital_structure_event_study_fit_stats.csv" = leverage_results$event_study$fit_stats,
  "capital_structure_event_study_pretrend_tests.csv" = leverage_results$event_study$pretrend_tests,
  "capital_structure_event_study_sample_summary.csv" = leverage_results$sample_summary,
  "risk_taking_coefficients.csv" = risk_results$coefficients,
  "risk_taking_fit_stats.csv" = risk_results$fit_stats,
  "risk_taking_main_table.csv" = model_table(risk_results, "treat:reform_2017") |> dplyr::filter(.data$control_set_id == "full_usable"),
  "risk_taking_event_study_coefficients.csv" = risk_results$event_study$coefficients,
  "risk_taking_event_study_fit_stats.csv" = risk_results$event_study$fit_stats,
  "risk_taking_event_study_pretrend_tests.csv" = risk_results$event_study$pretrend_tests,
  "cash_holdings_coefficients.csv" = cash_results$coefficients,
  "cash_holdings_fit_stats.csv" = cash_results$fit_stats,
  "cash_holdings_main_table.csv" = model_table(cash_results, "treat:post_2019") |> dplyr::filter(.data$control_set_id == "full_usable"),
  "cash_holdings_event_study_coefficients.csv" = cash_results$event_study$coefficients,
  "cash_holdings_event_study_fit_stats.csv" = cash_results$event_study$fit_stats,
  "cash_holdings_event_study_pretrend_tests.csv" = cash_results$event_study$pretrend_tests
)

# Add matching and weighting diagnostics for each outcome family.
for (id in c("leverage", "risk", "cash")) {
  item <- matching_results[[id]]
  prefix <- switch(id, leverage = "leverage_robustness", risk = "risk_taking_robustness",
                   cash = "cash_holdings_robustness")
  exports[[paste0(prefix, "_design_summary.csv")]] <- item$design_summary
  exports[[paste0(prefix, "_balance_diagnostics.csv")]] <- item$balance_diagnostics
  suffix <- if (id == "cash") "_matching" else ""
  exports[[paste0(prefix, suffix, "_coefficients.csv")]] <- item$coefficients
  exports[[paste0(prefix, suffix, "_fit_stats.csv")]] <- item$fit_stats
  exports[[paste0(prefix, suffix, "_event_study_coefficients.csv")]] <- item$event_study$coefficients
  exports[[paste0(prefix, suffix, "_event_study_fit_stats.csv")]] <- item$event_study$fit_stats
  exports[[paste0(prefix, suffix, "_event_study_pretrend_tests.csv")]] <- item$event_study$pretrend_tests
  exports[[paste0(prefix, if (id == "cash") "_matching_table.csv" else "_table.csv")]] <-
    model_table(item, switch(id, leverage = "treat:post", risk = "treat:reform_2017", cash = "treat:post_2019"))
}

# Add estimates split by countries' pre-reform policy regimes.
for (id in c("leverage", "risk", "cash")) {
  item <- regime_results[[id]]
  prefix <- switch(id, leverage = "leverage_policy_regime", risk = "risk_taking_regime",
                   cash = "cash_policy_regime")
  exports[[paste0(prefix, "_coefficients.csv")]] <- item$coefficients
  exports[[paste0(prefix, "_fit_stats.csv")]] <- item$fit_stats
  exports[[paste0(prefix, "_map.csv")]] <- item$regime_map
  exports[[paste0(prefix, if (id == "risk") "_table.csv" else "_heterogeneity.csv")]] <-
    model_table(item, switch(id, leverage = "treat:post", risk = "treat:reform_2017", cash = "treat:post_2019"))
}

# Add firm-level heterogeneity results and their support diagnostics.
operational_maps <- list(
  "leverage_firm_heterogeneity_classified_baseline.csv" = operational_heterogeneity$classified_baseline,
  "leverage_firm_heterogeneity_coefficients.csv" = operational_heterogeneity$coefficients,
  "leverage_firm_heterogeneity_country_support.csv" = operational_classification$country_support,
  "leverage_firm_heterogeneity_equality_tests.csv" = operational_heterogeneity$equality_tests,
  "leverage_firm_heterogeneity_event_pretrend_tests.csv" = operational_heterogeneity$dynamic$pretrend_tests,
  "leverage_firm_heterogeneity_fit_stats.csv" = operational_heterogeneity$fit_stats,
  "leverage_firm_heterogeneity_group_support.csv" = operational_classification$group_support,
  "leverage_firm_heterogeneity_marginal_effects.csv" = operational_heterogeneity$marginal_effects,
  "leverage_firm_heterogeneity_model_support.csv" = operational_heterogeneity$model_support
)
pressure_maps <- list(
  "leverage_restriction_pressure_classified_baseline.csv" = pressure_heterogeneity$classified_baseline,
  "leverage_restriction_pressure_coefficients.csv" = pressure_heterogeneity$coefficients,
  "leverage_restriction_pressure_country_support.csv" = pressure_classification$country_support,
  "leverage_restriction_pressure_equality_test.csv" = pressure_heterogeneity$equality_test,
  "leverage_restriction_pressure_event_pretrend_tests.csv" = pressure_heterogeneity$dynamic$pretrend_tests,
  "leverage_restriction_pressure_fit_stats.csv" = pressure_heterogeneity$fit_stats,
  "leverage_restriction_pressure_group_support.csv" = pressure_classification$group_support,
  "leverage_restriction_pressure_marginal_effects.csv" = pressure_heterogeneity$marginal_effects,
  "leverage_restriction_pressure_model_support.csv" = pressure_heterogeneity$model_support
)
exports <- c(exports, operational_maps, pressure_maps)

# Add COVID sensitivity and feasibility-audit outputs.
exports <- c(exports, list(
  "covid_combined_results.csv" = covid_results$combined_results,
  "covid_control_coefficients.csv" = covid_results$control_coefficients,
  "covid_coverage_audit.csv" = covid_coverage_audit(covid_annual$deaths),
  "covid_existing_exclusions.csv" = covid_results$existing_exclusions,
  "covid_fit_stats.csv" = covid_results$fit_stats,
  "covid_source_record.csv" = readr::read_csv(analysis_config$paths$covid_deaths_source_record, show_col_types = FALSE),
  "covid_specification_registry.csv" = covid_results$specification_registry,
  "covid_stringency_coverage_audit.csv" = covid_coverage_audit(covid_annual$stringency),
  "covid_stringency_source_record.csv" = readr::read_csv(analysis_config$paths$covid_stringency_source_record, show_col_types = FALSE),
  "covid_treatment_effects.csv" = covid_results$treatment_effects,
  "investment_analysis_candidate_columns.csv" = investment_audit$data_candidates,
  "investment_direct_candidate_value_summary.csv" = investment_audit$direct_candidate_summary,
  "investment_feasibility_summary.csv" = investment_audit$feasibility_summary,
  "investment_raw_candidate_columns.csv" = investment_audit$raw_candidates,
  "investment_raw_orbis_headers.csv" = investment_audit$raw_headers,
  "investment_variable_availability.csv" = investment_audit$variable_availability,
  "market_risk_feasibility.csv" = market_risk_audit$feasibility_summary
))

# Model CSVs are optional because they are used only by the author-side audit.
if (analysis_config$export_model_csvs) {
  purrr::iwalk(exports, ~ write_generated_model(.x, .y, generated_models))
  message("Machine-readable model CSV export: enabled.")
} else {
  message("Machine-readable model CSV export: disabled (tables and figures are still generated).")
}

# Render every active thesis table directly from the fresh in-memory results.
variable_definitions <- tibble::tribble(
  ~Variable, ~Definition,
  "Debt-to-assets", "Total debt divided by total assets",
  "Log equity", "Natural logarithm of shareholder equity",
  "Log current debt", "Natural logarithm of current debt",
  "Log long-term debt", "Natural logarithm of long-term debt",
  "Adjusted earnings volatility", "Forward-window volatility of adjusted earnings",
  "Unadjusted earnings volatility", "Forward-window volatility of unadjusted earnings",
  "Cash/assets", "Cash and cash equivalents divided by total assets"
)
table_sources <- list(
  "04_variable_definitions.tex" = variable_definitions,
  "06_capital_structure_results.tex" = exports[["capital_structure_baseline_table.csv"]],
  "06_cash_holdings_results.tex" = exports[["cash_holdings_main_table.csv"]],
  "06_operational_risk_results.tex" = exports[["risk_taking_main_table.csv"]],
  "07_control_sets.tex" = dplyr::bind_rows(exports[["capital_structure_validation_control_sets.csv"]],
                                             exports[["risk_taking_coefficients.csv"]],
                                             exports[["cash_holdings_coefficients.csv"]]),
  "07_covid.tex" = covid_results$combined_results,
  "07_firm_heterogeneity.tex" = dplyr::bind_rows(operational_heterogeneity$marginal_effects,
                                                  pressure_heterogeneity$marginal_effects),
  "07_matching.tex" = dplyr::bind_rows(exports[["leverage_robustness_table.csv"]],
                                        exports[["risk_taking_robustness_table.csv"]],
                                        exports[["cash_holdings_robustness_matching_table.csv"]]),
  "07_policy_regimes.tex" = dplyr::bind_rows(exports[["leverage_policy_regime_heterogeneity.csv"]],
                                              exports[["risk_taking_regime_table.csv"]],
                                              exports[["cash_policy_regime_heterogeneity.csv"]]),
  "A06_baseline_pretrend_diagnostics.tex" = dplyr::bind_rows(
    leverage_results$event_study$pretrend_tests, risk_results$event_study$pretrend_tests,
    cash_results$event_study$pretrend_tests),
  "A06_event_study_cash.tex" = cash_results$event_study$coefficients,
  "A06_event_study_leverage.tex" = leverage_results$event_study$coefficients,
  "A06_event_study_risk.tex" = risk_results$event_study$coefficients,
  "A07_control_sets_cash.tex" = cash_results$coefficients,
  "A07_control_sets_leverage.tex" = leverage_results$coefficients,
  "A07_control_sets_risk.tex" = risk_results$coefficients,
  "A07_covid_cash.tex" = dplyr::filter(covid_results$combined_results, .data$analysis_id == "cash"),
  "A07_covid_leverage.tex" = dplyr::filter(covid_results$combined_results, .data$analysis_id == "leverage"),
  "A07_covid_risk.tex" = dplyr::filter(covid_results$combined_results, .data$analysis_id == "risk"),
  "A07_firm_heterogeneity_leverage.tex" = dplyr::bind_rows(
    operational_heterogeneity$marginal_effects, operational_heterogeneity$equality_tests,
    pressure_heterogeneity$marginal_effects, pressure_heterogeneity$equality_test),
  "A07_matching_cash.tex" = exports[["cash_holdings_robustness_matching_table.csv"]],
  "A07_matching_dynamic_pretrends.tex" = dplyr::bind_rows(
    matching_results$leverage$event_study$pretrend_tests,
    matching_results$risk$event_study$pretrend_tests,
    matching_results$cash$event_study$pretrend_tests),
  "A07_matching_leverage.tex" = exports[["leverage_robustness_table.csv"]],
  "A07_matching_risk.tex" = exports[["risk_taking_robustness_table.csv"]],
  "A07_regimes_cash.tex" = exports[["cash_policy_regime_heterogeneity.csv"]],
  "A07_regimes_leverage.tex" = exports[["leverage_policy_regime_heterogeneity.csv"]],
  "A07_regimes_risk.tex" = exports[["risk_taking_regime_table.csv"]]
)

# Keep thesis-facing captions and labels explicit. A fresh run writes these
# artifacts only to results/generated; promotion to results/submitted and
# Thesis/assets is a separate, author-approved step after result verification.
table_metadata <- list(
  "04_variable_definitions.tex" = list(
    caption = "Definitions of main outcomes and firm-level controls",
    label = "tab:data_variable_definitions"
  ),
  "06_capital_structure_results.tex" = list(
    caption = "Capital-structure regressions",
    label = "tab:results_capital_structure"
  ),
  "06_cash_holdings_results.tex" = list(
    caption = "Cash-holdings regression",
    label = "tab:results_cash"
  ),
  "06_operational_risk_results.tex" = list(
    caption = "Baseline earnings-volatility regressions",
    label = "tab:results_operational_risk"
  ),
  "07_control_sets.tex" = list(
    caption = "Treatment-effect sensitivity across control sets",
    label = "tab:robustness_control_sets"
  ),
  "07_covid.tex" = list(
    caption = "COVID-period sensitivity: timing restrictions and pandemic controls",
    label = "tab:robustness_covid"
  ),
  "07_firm_heterogeneity.tex" = list(
    caption = "Firm-level heterogeneity in the debt-to-assets response",
    label = "tab:robustness_firm_heterogeneity"
  ),
  "07_matching.tex" = list(
    caption = "Matching and ATT-reweighting estimates and diagnostics",
    label = "tab:robustness_matching"
  ),
  "07_policy_regimes.tex" = list(
    caption = "Treatment effects by pre-reform policy regime",
    label = "tab:robustness_regimes"
  ),
  "A06_baseline_pretrend_diagnostics.tex" = list(
    caption = "Joint pre-trend tests for baseline event studies",
    label = "tab:appendix_baseline_pretrends"
  ),
  "A06_event_study_cash.tex" = list(
    caption = "Cash-holdings event-study regression",
    label = "tab:appendix_event_cash"
  ),
  "A06_event_study_leverage.tex" = list(
    caption = "Capital-structure event-study regressions",
    label = "tab:appendix_event_leverage"
  ),
  "A06_event_study_risk.tex" = list(
    caption = "Earnings-volatility event-study regressions",
    label = "tab:appendix_event_risk"
  ),
  "A07_control_sets_cash.tex" = list(
    caption = "Cash/assets regressions across control sets",
    label = "tab:appendix_robustness_control_cash"
  ),
  "A07_control_sets_leverage.tex" = list(
    caption = "Debt-to-assets regressions across control sets",
    label = "tab:appendix_robustness_control_leverage"
  ),
  "A07_control_sets_risk.tex" = list(
    caption = "Adjusted earnings-volatility regressions across control sets",
    label = "tab:appendix_robustness_control_risk"
  ),
  "A07_covid_cash.tex" = list(
    caption = "Cash/assets regressions for COVID-period sensitivity",
    label = "tab:appendix_robustness_covid_cash"
  ),
  "A07_covid_leverage.tex" = list(
    caption = "Debt-to-assets regressions for COVID-period sensitivity",
    label = "tab:appendix_robustness_covid_leverage"
  ),
  "A07_covid_risk.tex" = list(
    caption = "Adjusted earnings-volatility regressions for COVID-period sensitivity",
    label = "tab:appendix_robustness_covid_risk"
  ),
  "A07_firm_heterogeneity_leverage.tex" = list(
    caption = "Debt-to-assets regressions by pre-reform exposure proxy",
    label = "tab:appendix_robustness_firm_heterogeneity"
  ),
  "A07_matching_cash.tex" = list(
    caption = "Cash/assets regressions after matching and reweighting",
    label = "tab:appendix_robustness_matching_cash"
  ),
  "A07_matching_dynamic_pretrends.tex" = list(
    caption = "Joint pre-trend tests after matching and ATT reweighting",
    label = "tab:appendix_robustness_matching_dynamic_pretrends"
  ),
  "A07_matching_leverage.tex" = list(
    caption = "Debt-to-assets regressions after matching and reweighting",
    label = "tab:appendix_robustness_matching_leverage"
  ),
  "A07_matching_risk.tex" = list(
    caption = "Adjusted earnings-volatility regressions after matching and reweighting",
    label = "tab:appendix_robustness_matching_risk"
  ),
  "A07_regimes_cash.tex" = list(
    caption = "Cash/assets regressions by pre-reform policy regime",
    label = "tab:appendix_robustness_regime_cash"
  ),
  "A07_regimes_leverage.tex" = list(
    caption = "Debt-to-assets regressions by pre-reform policy regime",
    label = "tab:appendix_robustness_regime_leverage"
  ),
  "A07_regimes_risk.tex" = list(
    caption = "Adjusted earnings-volatility regressions by pre-reform policy regime",
    label = "tab:appendix_robustness_regime_risk"
  )
)

missing_table_metadata <- setdiff(names(table_sources), names(table_metadata))
if (length(missing_table_metadata)) {
  stop("Missing thesis table metadata for: ", paste(missing_table_metadata, collapse = ", "), call. = FALSE)
}
purrr::iwalk(table_sources, function(data, filename) {
  metadata <- table_metadata[[filename]]
  write_latex_table(data, file.path(analysis_config$paths$generated_tables, filename),
                    caption = metadata$caption,
                    label = metadata$label)
})

figure_specs <- list(
  "capital-structure-event-study-1.pdf" = list("leverage", leverage_results$event_study$coefficients),
  "operational-risk-event-study-1.pdf" = list("risk", risk_results$event_study$coefficients),
  "cash-holdings-event-study-1.pdf" = list("cash", cash_results$event_study$coefficients)
)
purrr::iwalk(figure_specs, function(spec, filename) {
  plot <- event_study_plot(spec[[2L]], spec[[1L]])
  ggplot2::ggsave(file.path(analysis_config$paths$generated_figures, filename),
                  plot = plot, width = 8, height = 5.5, units = "in")
})
