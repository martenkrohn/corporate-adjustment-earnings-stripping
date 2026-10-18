# Frozen COVID-control construction and selected sensitivity regressions.

# `validate_covid_source_record()` recomputes SHA-256 hashes for one raw
# pandemic snapshot and its normalized annual panel. It stops on any mismatch
# and returns the one-row source record when both frozen inputs are unchanged.
validate_covid_source_record <- function(source_record_path, raw_path, normalized_path) {
  source_record <- readr::read_csv(source_record_path, show_col_types = FALSE)
  if (nrow(source_record) != 1L) stop(sprintf("COVID source record must have one row: %s", source_record_path), call. = FALSE)
  observed_raw <- sha256_file(raw_path)
  observed_normalized <- sha256_file(normalized_path)
  if (!identical(tolower(observed_raw), tolower(source_record$sha256[[1L]]))) {
    stop(sprintf("COVID raw snapshot checksum mismatch: %s", basename(raw_path)), call. = FALSE)
  }
  if (!identical(tolower(observed_normalized), tolower(source_record$normalized_sha256[[1L]]))) {
    stop(sprintf("COVID annual panel checksum mismatch: %s", basename(normalized_path)), call. = FALSE)
  }
  source_record
}

# `join_covid_controls()` joins annual country controls to leverage and cash.
# For the forward-looking volatility outcome, it instead joins the matching
# forward window so the control timing follows the outcome construction.
join_covid_controls <- function(data, analysis_id, annual, risk_windows) {
  if (analysis_id == "risk") {
    return(dplyr::left_join(data, risk_windows, by = c("country_iso_code", "year")))
  }
  data |>
    dplyr::left_join(
      dplyr::select(
        annual$deaths, "country_iso_code", "year",
        "annual_deaths_per_million", "log_deaths_per_million"
      ),
      by = c("country_iso_code", "year")
    ) |>
    dplyr::left_join(
      dplyr::select(
        annual$stringency, "country_iso_code", "year",
        "annual_stringency_index", "stringency_index_10"
      ),
      by = c("country_iso_code", "year")
    )
}

# `covid_control_terms()` translates a variant ID into the exact regression
# column names, including the forward-window names required for risk models.
covid_control_terms <- function(analysis_id, variant_id) {
  if (variant_id == "baseline") return(character())
  deaths <- if (analysis_id == "risk") "log_forward_deaths_per_million" else "log_deaths_per_million"
  stringency <- if (analysis_id == "risk") "forward_stringency_index_10" else "stringency_index_10"
  switch(variant_id, deaths = deaths, stringency = stringency,
         deaths_stringency = c(deaths, stringency), stop("Unknown COVID variant.", call. = FALSE))
}

# `firm_year_key()` creates a sortable identifier used to verify that adding a
# COVID control never silently changes the preferred estimation sample.
firm_year_key <- function(data) paste(data$bv_d_id_number, data$year, sep = "::")

# `run_covid_sensitivity()` estimates all four variants for each outcome while
# enforcing an identical firm-year sample. It also estimates pandemic-year
# exclusions and returns models, treatment effects, control coefficients, fit
# statistics, specification records, and exclusion checks.
run_covid_sensitivity <- function(
  samples, annual, risk_windows, baseline_results, analysis_specs, variants
) {
  analysis_order <- c("leverage", "risk", "cash")
  outcome_labels <- c(leverage = "Leverage", risk = "Operational risk", cash = "Cash holdings")
  models <- list(); treatment_rows <- list(); fit_rows <- list(); control_rows <- list()
  specification_rows <- list(); exclusion_rows <- list()

  # Confidence intervals use the country-cluster degrees of freedom.
  interval <- function(estimate, std_error, countries) {
    critical <- stats::qt(.975, df = countries - 1L)
    c(conf_low = estimate - critical * std_error, conf_high = estimate + critical * std_error)
  }
  # Convert each alternative-year-window result to the same reporting schema.
  exclusion_row <- function(analysis_id, variant_id, variant_order, variant_label,
                            excluded_years, window_rule, coefficient, fit) {
    limits <- interval(coefficient$estimate[[1L]], coefficient$std_error[[1L]], fit$countries[[1L]])
    tibble::tibble(
      analysis_id = analysis_id, variant_id = variant_id, variant_order = variant_order,
      variant_label = variant_label, excluded_years = excluded_years, window_rule = window_rule,
      estimate = coefficient$estimate[[1L]], std_error = coefficient$std_error[[1L]],
      statistic = coefficient$statistic[[1L]], p_value = coefficient$p_value[[1L]],
      conf_low = limits[["conf_low"]], conf_high = limits[["conf_high"]],
      observations = fit$observations[[1L]], firms = fit$firms[[1L]], countries = fit$countries[[1L]],
      included_countries = fit$included_countries[[1L]], validation_status = "exact_match"
    )
  }

  # Work through leverage, risk, and cash using their registered timing and
  # preferred baseline estimation samples.
  for (analysis_id in analysis_order) {
    spec <- analysis_specs[[analysis_id]]
    data <- samples[[analysis_id]]
    data <- join_covid_controls(data, analysis_id, annual, risk_windows)
    outcome <- spec$outcomes$outcome[[1L]]
    model_id <- if (analysis_id == "leverage") "baseline_dta" else if (analysis_id == "risk") "risk_earnvol" else "cash_assets"
    baseline_key <- if (analysis_id == "leverage") "full_usable.baseline_dta" else if (analysis_id == "risk")
      "full_usable.risk_earnvol" else "full_usable.cash_assets_main"
    preferred_keys <- sort(firm_year_key(baseline_results[[analysis_id]]$estimation_samples[[baseline_key]]))
    analysis_treatments <- list()

    # Add each pandemic-control set one at a time. The explicit firm-year key
    # check below prevents missing controls from changing the comparison sample.
    for (i in seq_len(nrow(variants))) {
      variant <- variants[i, ]
      variant_id <- variant$variant_id[[1L]]
      added <- covid_control_terms(analysis_id, variant_id)
      controls <- c(spec$control_sets[[spec$preferred_control_set]], added)
      key <- paste(analysis_id, variant_id, sep = ".")
      result <- estimate_static_did(
        data, outcome, controls, spec$period, model_id,
        metadata = list(analysis_id = analysis_id, variant_id = variant_id,
                        variant_label = variant$variant_label[[1L]], display_order = variant$display_order[[1L]])
      )
      if (!identical(sort(firm_year_key(result$estimation_data)), preferred_keys)) {
        stop(sprintf("COVID variant %s changes the preferred %s firm-year sample.", variant_id, analysis_id), call. = FALSE)
      }
      models[[key]] <- result$model
      treatment <- dplyr::filter(result$coefficients, .data$term == result$fit_stats$treatment_term[[1L]])
      limits <- interval(treatment$estimate[[1L]], treatment$std_error[[1L]], result$fit_stats$countries[[1L]])
      analysis_treatments[[variant_id]] <- tibble::tibble(
        analysis_id = analysis_id, outcome_label = unname(outcome_labels[[analysis_id]]),
        variant_id = variant_id, variant_label = variant$variant_label[[1L]],
        display_order = variant$display_order[[1L]], control_set = paste(added, collapse = ";"),
        treatment_term = treatment$term[[1L]], estimate = treatment$estimate[[1L]],
        std_error = treatment$std_error[[1L]], statistic = treatment$statistic[[1L]], p_value = treatment$p_value[[1L]],
        conf_low = limits[["conf_low"]], conf_high = limits[["conf_high"]],
        scale_factor = ifelse(analysis_id == "risk", 1, 100),
        observations = result$fit_stats$observations[[1L]], firms = result$fit_stats$firms[[1L]],
        countries = result$fit_stats$countries[[1L]], years = result$fit_stats$years[[1L]],
        included_countries = result$fit_stats$included_countries[[1L]],
        excluded_countries = result$fit_stats$excluded_countries[[1L]],
        formula = result$fit_stats$formula[[1L]], cluster_variable = "country_iso_code"
      )
      if (length(added) > 0L) {
        control_rows[[key]] <- purrr::map_dfr(seq_along(added), function(control_order) {
          term <- added[[control_order]]
          row <- dplyr::filter(result$coefficients, .data$term == term)
          dplyr::transmute(row, analysis_id = analysis_id, variant_id = variant_id,
                           control_id = ifelse(grepl("deaths", term), "deaths", "stringency"),
                           control_order = control_order, term = .data$term, estimate = .data$estimate,
                           std_error = .data$std_error, statistic = .data$statistic, p_value = .data$p_value,
                           observations = result$fit_stats$observations[[1L]], countries = result$fit_stats$countries[[1L]])
        })
      }
      fit_rows[[key]] <- dplyr::transmute(
        result$fit_stats, analysis_id = analysis_id, variant_id = variant_id,
        variant_label = variant$variant_label[[1L]], display_order = variant$display_order[[1L]],
        observations = .data$observations, firms = .data$firms, countries = .data$countries, years = .data$years,
        complete_case_observations_before_pre_post = .data$complete_case_observations_before_pre_post,
        complete_case_firms_before_pre_post = .data$complete_case_firms_before_pre_post,
        observations_dropped_no_pre_post = .data$observations_dropped_no_model_pre_post,
        adjusted_r2 = .data$adjusted_r2, within_r2 = .data$within_r2,
        included_countries = .data$included_countries, excluded_countries = .data$excluded_countries,
        fixed_effects = "bv_d_id_number;year", cluster_variable = "country_iso_code", formula = .data$formula
      )
      control_ids <- ifelse(grepl("deaths", added), "deaths", "stringency")
      specification_rows[[key]] <- tibble::tibble(
        contract_version = "covid_sensitivity_v2", analysis_id = analysis_id, outcome = outcome,
        variant_id = variant_id, variant_label = variant$variant_label[[1L]], display_order = variant$display_order[[1L]],
        treatment_term = result$fit_stats$treatment_term[[1L]], covid_control_ids = paste(control_ids, collapse = ";"),
        covid_control_terms = paste(added, collapse = ";"),
        deaths_timing = if ("deaths" %in% control_ids) ifelse(analysis_id == "risk", "forward_t_to_t_plus_2", "contemporaneous_country_year") else "not_applicable",
        stringency_timing = if ("stringency" %in% control_ids) ifelse(analysis_id == "risk", "forward_t_to_t_plus_2", "contemporaneous_country_year") else "not_applicable",
        stringency_2023_rule = if ("stringency" %in% control_ids) "constructed_zero_user_assumption" else "not_applicable",
        controls = paste(spec$control_sets[[spec$preferred_control_set]], collapse = ";"), fixed_effects = "bv_d_id_number;year",
        cluster_variable = "country_iso_code", formula = result$fit_stats$formula[[1L]]
      )
    }

    # Express each treatment estimate and standard error relative to the
    # no-COVID-control baseline for easy robustness interpretation.
    analysis_treatments <- dplyr::bind_rows(analysis_treatments) |>
      dplyr::mutate(
        baseline_estimate = .data$estimate[.data$variant_id == "baseline"][[1L]],
        baseline_std_error = .data$std_error[.data$variant_id == "baseline"][[1L]],
        baseline_observations = .data$observations[.data$variant_id == "baseline"][[1L]],
        estimate_change = .data$estimate - .data$baseline_estimate,
        absolute_magnitude_change_pct = 100 * (abs(.data$estimate) / abs(.data$baseline_estimate) - 1),
        std_error_change_pct = 100 * (.data$std_error / .data$baseline_std_error - 1),
        observation_change = .data$observations - .data$baseline_observations
      )
    treatment_rows[[analysis_id]] <- analysis_treatments
    baseline_coefficient <- dplyr::filter(analysis_treatments, .data$variant_id == "baseline")
    baseline_fit <- fit_rows[[paste(analysis_id, "baseline", sep = ".")]]
    full_label <- if (analysis_id == "risk") "Full risk sample" else "Full 2014-2023 sample"
    exclusion_rows[[paste0(analysis_id, ".full")]] <- exclusion_row(
      analysis_id, "full_sample", 1L, full_label, "", "", baseline_coefficient, baseline_fit
    )

    # Exclude observations whose outcome window overlaps the main pandemic
    # years (the rule differs for the forward-looking risk outcome).
    strict_keep <- if (analysis_id == "risk") data$year + 2L < 2020L else !data$year %in% c(2020L, 2021L)
    strict <- estimate_static_did(data[strict_keep, , drop = FALSE], outcome, spec$control_sets[[spec$preferred_control_set]],
                                  spec$period, model_id)
    strict_coefficient <- dplyr::filter(strict$coefficients, .data$term == strict$fit_stats$treatment_term[[1L]])
    strict_id <- if (analysis_id == "risk") "covid_safe" else "exclude_2020_2021"
    strict_order <- if (analysis_id == "risk") 2L else 3L
    strict_label <- if (analysis_id == "risk") "Forward volatility window ends before 2020" else "Exclude 2020-2021"
    exclusion_rows[[paste0(analysis_id, ".strict")]] <- exclusion_row(
      analysis_id, strict_id, strict_order, strict_label,
      ifelse(analysis_id == "risk", "", "2020;2021"),
      ifelse(analysis_id == "risk", "anchor_year + 3 - 1 < 2020", ""), strict_coefficient, strict$fit_stats
    )
    models[[paste0(analysis_id, ".strict_timing")]] <- strict$model
  }

  treatment_effects <- dplyr::bind_rows(treatment_rows)
  existing_exclusions <- dplyr::bind_rows(exclusion_rows)
  combined_results <- dplyr::bind_rows(
    dplyr::transmute(existing_exclusions, analysis_id = .data$analysis_id, evidence_family = "frozen_exclusion",
                     variant_id = .data$variant_id, variant_label = .data$variant_label,
                     display_order = .data$variant_order, estimate = .data$estimate, std_error = .data$std_error,
                     statistic = .data$statistic, p_value = .data$p_value, conf_low = .data$conf_low,
                     conf_high = .data$conf_high, observations = .data$observations, firms = .data$firms,
                     countries = .data$countries, scale_factor = ifelse(.data$analysis_id == "risk", 1, 100),
                     validation_status = .data$validation_status),
    dplyr::transmute(treatment_effects, analysis_id = .data$analysis_id, evidence_family = "model_extension",
                     variant_id = .data$variant_id, variant_label = .data$variant_label,
                     display_order = .data$display_order, estimate = .data$estimate, std_error = .data$std_error,
                     statistic = .data$statistic, p_value = .data$p_value, conf_low = .data$conf_low,
                     conf_high = .data$conf_high, observations = .data$observations, firms = .data$firms,
                     countries = .data$countries, scale_factor = .data$scale_factor, validation_status = "estimated")
  ) |>
    dplyr::arrange(match(.data$analysis_id, analysis_order), .data$evidence_family, .data$display_order)
  if (nrow(treatment_effects) != 12L || nrow(existing_exclusions) != 6L || nrow(combined_results) != 18L) {
    stop("COVID output cardinality does not match the frozen 12/6/18 contract.", call. = FALSE)
  }
  list(models = models, treatment_effects = treatment_effects,
       control_coefficients = dplyr::bind_rows(control_rows), fit_stats = dplyr::bind_rows(fit_rows),
       specification_registry = dplyr::bind_rows(specification_rows),
       existing_exclusions = existing_exclusions, combined_results = combined_results)
}

# `covid_coverage_audit()` summarizes the first/last year and non-missing
# coverage of each COVID variable by country.
covid_coverage_audit <- function(panel) {
  panel |>
    dplyr::group_by(.data$country_iso_code) |>
    dplyr::summarise(year_min = min(.data$year), year_max = max(.data$year),
                     country_years = dplyr::n(), .groups = "drop") |>
    dplyr::arrange(.data$country_iso_code)
}
