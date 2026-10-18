# Estimation helpers for the linear analysis package.

# `tidy_fixest_coefficients()` converts a fixest coefficient matrix into a
# stable data frame with term, estimate, standard error, statistic, and p-value.
tidy_fixest_coefficients <- function(model) {
  table <- as.data.frame(fixest::coeftable(model))
  table$term <- rownames(table)
  rownames(table) <- NULL
  table |>
    dplyr::rename(
      estimate = `Estimate`,
      std_error = `Std. Error`,
      statistic = `t value`,
      p_value = `Pr(>|t|)`
    ) |>
    dplyr::select("term", "estimate", "std_error", "statistic", "p_value")
}

# `extract_fixest_stat()` safely extracts a named fit statistic and returns
# `NA` if that statistic is unavailable for the fitted specification.
extract_fixest_stat <- function(model, type) {
  value <- fixest::fitstat(model, type)
  as.numeric(value[[1L]])
}

# `filter_model_pre_post_support()` removes firms that do not contribute at
# least one usable observation on both sides of the specified reform indicator.
# It returns the filtered data and before/after support counts.
filter_model_pre_post_support <- function(data, period_variable) {
  data |>
    dplyr::group_by(.data$bv_d_id_number) |>
    dplyr::mutate(
      has_pre = any(.data[[period_variable]] == 0L, na.rm = TRUE),
      has_post = any(.data[[period_variable]] == 1L, na.rm = TRUE)
    ) |>
    dplyr::ungroup() |>
    dplyr::filter(.data$has_pre, .data$has_post) |>
    dplyr::select(-dplyr::all_of(c("has_pre", "has_post")))
}

# `cluster_count()` reports the number of country clusters actually used by a
# fitted model, which determines the finite-sample inference degrees of freedom.
cluster_count <- function(model) {
  as.integer(attr(summary(model)$cov.scaled, "G"))
}

# `model_sample_summary()` records usable observations, firms, countries, and
# years for one outcome before the regression's complete-case filtering.
model_sample_summary <- function(data, outcome, period_variable, model_id) {
  values <- data[[outcome]]
  tibble::tibble(
    model_id = model_id, outcome = outcome,
    observations = nrow(data), firms = dplyr::n_distinct(data$bv_d_id_number),
    countries = dplyr::n_distinct(data$country_iso_code), years = dplyr::n_distinct(data$year),
    treatment_observations = sum(data$treat == 1L), control_observations = sum(data$treat == 0L),
    pre_observations = sum(data[[period_variable]] == 0L), post_observations = sum(data[[period_variable]] == 1L),
    outcome_mean_treatment = mean(values[data$treat == 1L], na.rm = TRUE),
    outcome_mean_control = mean(values[data$treat == 0L], na.rm = TRUE)
  )
}

# `estimate_static_did()` fits the static difference-in-differences model with
# firm and year fixed effects and country-clustered standard errors. It applies
# complete-case and pre/post-support rules and returns the model, tidy
# coefficients, fit/sample diagnostics, and exact estimation data.
estimate_static_did <- function(
  data, outcome, controls, period_variable, model_id,
  treatment_term = paste0("treat:", period_variable), weight_variable = NULL,
  metadata = list()
) {
  # Complete cases are defined jointly over the outcome, treatment timing,
  # controls, fixed-effect identifiers, cluster, and optional weight.
  model_variables <- unique(c(
    outcome, "treat", period_variable, controls, "bv_d_id_number", "year", "country_iso_code", weight_variable
  ))
  assert_required_columns(data, model_variables, model_id)
  estimation_data <- data |>
    dplyr::select(dplyr::all_of(model_variables)) |>
    tidyr::drop_na()
  complete_case_observations <- nrow(estimation_data)
  complete_case_firms <- dplyr::n_distinct(estimation_data$bv_d_id_number)
  if (!is.null(weight_variable)) estimation_data <- dplyr::filter(estimation_data, .data[[weight_variable]] > 0)
  estimation_data <- filter_model_pre_post_support(estimation_data, period_variable)
  if (nrow(estimation_data) == 0L) stop(sprintf("Estimation sample for %s is empty.", model_id), call. = FALSE)
  # The treatment interaction is the coefficient of interest. Firm and year
  # fixed effects absorb time-invariant firm factors and common annual shocks.
  formula_text <- sprintf(
    "%s ~ %s + %s | bv_d_id_number + year",
    outcome, treatment_term, paste(controls, collapse = " + ")
  )
  model <- if (is.null(weight_variable)) {
    fixest::feols(stats::as.formula(formula_text), data = estimation_data, cluster = ~country_iso_code)
  } else {
    fixest::feols(stats::as.formula(formula_text), data = estimation_data,
                  weights = stats::as.formula(paste0("~", weight_variable)), cluster = ~country_iso_code)
  }
  # Package both estimates and enough sample/fit metadata to audit the model.
  included <- sort(unique(estimation_data$country_iso_code))
  excluded <- sort(setdiff(unique(data$country_iso_code), included))
  coefficient_table <- tidy_fixest_coefficients(model) |>
    dplyr::mutate(model_id = model_id, outcome = outcome, .before = 1)
  fit <- tibble::tibble(
    model_id = model_id, outcome = outcome, treatment_term = treatment_term,
    observations = stats::nobs(model), firms = dplyr::n_distinct(estimation_data$bv_d_id_number),
    complete_case_observations_before_pre_post = complete_case_observations,
    complete_case_firms_before_pre_post = complete_case_firms,
    observations_dropped_no_model_pre_post = complete_case_observations - stats::nobs(model),
    firms_dropped_no_model_pre_post = complete_case_firms - dplyr::n_distinct(estimation_data$bv_d_id_number),
    countries = length(included), included_countries = paste(included, collapse = ";"),
    excluded_countries = paste(excluded, collapse = ";"),
    years = dplyr::n_distinct(estimation_data$year),
    weighted_observations = if (is.null(weight_variable)) NA_real_ else sum(estimation_data[[weight_variable]]),
    adjusted_r2 = extract_fixest_stat(model, "ar2"), within_r2 = extract_fixest_stat(model, "wr2"),
    controls = paste(controls, collapse = "; "), formula = formula_text
  )
  sample_summary <- model_sample_summary(estimation_data, outcome, period_variable, model_id)
  for (name in names(metadata)) {
    fit[[name]] <- metadata[[name]]
    coefficient_table[[name]] <- metadata[[name]]
    sample_summary[[name]] <- metadata[[name]]
  }
  list(
    model = model, coefficients = coefficient_table, fit_stats = fit,
    sample_summary = sample_summary,
    estimation_data = estimation_data
  )
}

# `make_event_pretrend_test()` tests whether all available pre-reform event-time
# coefficients are jointly zero. It returns a one-row test record, including an
# explicit status when no testable lead coefficient exists.
make_event_pretrend_test <- function(model, coefficients, model_id, outcome, reference_year) {
  terms <- coefficients |>
    dplyr::filter(.data$event_year < reference_year) |>
    dplyr::pull("term")
  terms <- intersect(terms, names(stats::coef(model)))
  tested_years <- coefficients$event_year[match(terms, coefficients$term)]
  if (length(terms) == 0L) {
    return(tibble::tibble(model_id = model_id, outcome = outcome, reference_year = reference_year,
                          tested_years = "", tested_terms = "", statistic = NA_real_, df = 0L,
                          p_value = NA_real_, method = "joint chi-square test of pre-reform treated-year coefficients"))
  }
  b <- stats::coef(model)[terms]
  variance <- stats::vcov(model)[terms, terms, drop = FALSE]
  rank <- qr(variance)$rank
  statistic <- tryCatch(as.numeric(t(b) %*% solve(variance, b)),
                        error = function(e) tryCatch(as.numeric(t(b) %*% qr.solve(variance, b)), error = function(e2) NA_real_))
  tibble::tibble(
    model_id = model_id, outcome = outcome, reference_year = reference_year,
    tested_years = paste(tested_years, collapse = ";"), tested_terms = paste(terms, collapse = ";"),
    statistic = statistic, df = rank,
    p_value = ifelse(is.na(statistic), NA_real_, stats::pchisq(statistic, rank, lower.tail = FALSE)),
    method = "joint chi-square test of pre-reform treated-year coefficients"
  )
}

# `estimate_event_study()` replaces the static treatment interaction with
# treatment-by-year indicators, using the stated reference year. It returns the
# model, confidence intervals, fit statistics, pre-trend test, and exact sample.
estimate_event_study <- function(
  data, outcome, controls, period_variable, post_start, reference_year, model_id,
  weight_variable = NULL, metadata = list(), firm_controls = NULL, country_controls = NULL
) {
  # Use the same complete-case and pre/post-support contract as the static model.
  variables <- unique(c(outcome, "treat", period_variable, controls,
                        "bv_d_id_number", "year", "country_iso_code", weight_variable))
  assert_required_columns(data, variables, paste(model_id, "event study"))
  estimation_data <- data |>
    dplyr::select(dplyr::all_of(variables)) |>
    tidyr::drop_na()
  complete_case_observations <- nrow(estimation_data)
  complete_case_firms <- dplyr::n_distinct(estimation_data$bv_d_id_number)
  if (!is.null(weight_variable)) estimation_data <- dplyr::filter(estimation_data, .data[[weight_variable]] > 0)
  estimation_data <- filter_model_pre_post_support(estimation_data, period_variable)
  if (nrow(estimation_data) == 0L) stop(sprintf("Event-study sample for %s is empty.", model_id), call. = FALSE)
  # `i(year, treat)` estimates one treated-country difference for each year;
  # the omitted reference year is inserted below as an explicit zero row.
  formula_text <- sprintf(
    "%s ~ i(year, treat, ref = %s) + %s | bv_d_id_number + year",
    outcome, reference_year, paste(controls, collapse = " + ")
  )
  model <- if (is.null(weight_variable)) {
    fixest::feols(stats::as.formula(formula_text), data = estimation_data, cluster = ~country_iso_code)
  } else {
    fixest::feols(stats::as.formula(formula_text), data = estimation_data,
                  weights = stats::as.formula(paste0("~", weight_variable)), cluster = ~country_iso_code)
  }
  coefficients <- tidy_fixest_coefficients(model) |>
    dplyr::filter(grepl("::", .data$term) & grepl(":treat", .data$term)) |>
    dplyr::mutate(
      event_year = as.integer(stringr::str_extract(.data$term, "[0-9]{4}")),
      reference_year = reference_year,
      relative_year = .data$event_year - post_start,
      period = dplyr::case_when(.data$event_year < reference_year ~ "pre",
                                .data$event_year >= post_start ~ "post", TRUE ~ "other"),
      conf_low = .data$estimate - 1.96 * .data$std_error,
      conf_high = .data$estimate + 1.96 * .data$std_error,
      model_id = model_id, outcome = outcome, .before = 1
    )
  reference <- tibble::tibble(
    model_id = model_id, outcome = outcome,
    term = sprintf("year::%s:treat", reference_year), estimate = 0, std_error = NA_real_,
    statistic = NA_real_, p_value = NA_real_, event_year = reference_year,
    reference_year = reference_year, relative_year = reference_year - post_start,
    period = "reference", conf_low = 0, conf_high = 0
  )
  coefficients <- dplyr::bind_rows(coefficients, reference) |> dplyr::arrange(.data$event_year)
  # Test all observable leads jointly, then attach sample and fit diagnostics.
  pretrend <- make_event_pretrend_test(model, coefficients, model_id, outcome, reference_year)
  included <- sort(unique(estimation_data$country_iso_code))
  excluded <- sort(setdiff(unique(data$country_iso_code), included))
  fit <- tibble::tibble(
    model_id = model_id, outcome = outcome, reference_year = reference_year, post_period_start = post_start,
    observations = stats::nobs(model), firms = dplyr::n_distinct(estimation_data$bv_d_id_number),
    complete_case_observations_before_pre_post = complete_case_observations,
    complete_case_firms_before_pre_post = complete_case_firms,
    observations_dropped_no_model_pre_post = complete_case_observations - stats::nobs(model),
    firms_dropped_no_model_pre_post = complete_case_firms - dplyr::n_distinct(estimation_data$bv_d_id_number),
    countries = length(included), included_countries = paste(included, collapse = ";"),
    excluded_countries = paste(excluded, collapse = ";"),
    years = dplyr::n_distinct(estimation_data$year),
    weighted_observations = if (is.null(weight_variable)) NA_real_ else sum(estimation_data[[weight_variable]]),
    adjusted_r2 = extract_fixest_stat(model, "ar2"), within_r2 = extract_fixest_stat(model, "wr2"),
    controls = paste(controls, collapse = "; "), formula = formula_text
  )
  if (!is.null(firm_controls)) fit$firm_controls <- paste(firm_controls, collapse = "; ")
  if (!is.null(country_controls)) fit$country_controls <- paste(country_controls, collapse = "; ")
  for (name in names(metadata)) {
    coefficients[[name]] <- metadata[[name]]
    pretrend[[name]] <- metadata[[name]]
    fit[[name]] <- metadata[[name]]
  }
  list(model = model, coefficients = coefficients, pretrend_test = pretrend,
       fit_stats = fit, estimation_data = estimation_data)
}

# `control_set_label()` converts internal control-set IDs into readable labels
# used in tables and machine-readable outputs.
control_set_label <- function(control_set_id) {
  labels <- c(
    firm_only = "Firm controls only",
    no_interest = "Firm plus macro/institutional controls excluding the interest rate",
    macro_institutional = "Firm plus macro/tax-rate controls",
    macro_tax_rate = "Firm plus macro/tax-rate controls",
    full_usable = "Full currently usable controls"
  )
  unname(labels[[control_set_id]])
}

# `paper_observations_for_model()` stores the thesis-table observation count
# associated with each preferred model so generated diagnostics can display
# the reference count alongside the newly estimated count.
paper_observations_for_model <- function(model_id) {
  values <- c(
    baseline_dta = 27730,
    baseline_ln_equity = 26483,
    baseline_ln_current_debt = 23636,
    baseline_ln_longterm_debt = 23128,
    risk_earnvol = 20101,
    risk_unadj_earnvol = 20101
  )
  if (model_id %in% names(values)) as.numeric(values[[model_id]]) else NA_real_
}

# `run_analysis_specification()` executes a complete specification supplied by
# a numbered analysis script. The helper chooses no outcomes, timing, controls,
# or reference years itself; those research choices must be visible to readers
# where the specification is defined.
run_analysis_specification <- function(data, spec) {
  required_fields <- c(
    "analysis_id", "outcomes", "firm_controls", "country_controls", "period",
    "post_start", "reference_year", "control_sets", "control_metadata_style",
    "report_event_control_groups", "fixed_effects", "cluster_variable",
    "preferred_control_set"
  )
  missing_fields <- setdiff(required_fields, names(spec))
  if (length(missing_fields)) {
    stop(sprintf("Analysis specification is missing: %s", paste(missing_fields, collapse = ", ")), call. = FALSE)
  }
  if (!spec$period %in% names(data)) {
    stop(sprintf("Timing variable '%s' is absent from the %s data.", spec$period, spec$analysis_id), call. = FALSE)
  }
  if (!identical(spec$fixed_effects, c("bv_d_id_number", "year")) ||
      !identical(spec$cluster_variable, "country_iso_code")) {
    stop("The shared estimator currently supports firm/year fixed effects and country clustering only.", call. = FALSE)
  }
  if (!spec$preferred_control_set %in% names(spec$control_sets)) {
    stop(sprintf("Preferred control set '%s' is not registered.", spec$preferred_control_set), call. = FALSE)
  }
  # Static models cover every registered outcome × control-set combination.
  static_outputs <- list()
  for (control_id in names(spec$control_sets)) {
    for (i in seq_len(nrow(spec$outcomes))) {
      row <- spec$outcomes[i, ]
      key <- paste(control_id, row$model_id[[1L]], sep = ".")
      paper_observations <- paper_observations_for_model(row$model_id[[1L]])
      metadata <- list(
        outcome_label = row$label[[1L]],
        paper_observations = paper_observations,
        observations_vs_paper = NA_real_
      )
      if (identical(spec$control_metadata_style, "control_set")) {
        metadata$control_set <- control_id
      } else {
        metadata$control_set_id <- control_id
        metadata$control_set_label <- control_set_label(control_id)
      }
      static_outputs[[key]] <- estimate_static_did(
        data, row$outcome[[1L]], spec$control_sets[[control_id]], spec$period, row$model_id[[1L]],
        metadata = metadata
      )
      static_outputs[[key]]$fit_stats$observations_vs_paper <-
        static_outputs[[key]]$fit_stats$observations - paper_observations
    }
  }
  # Dynamic models use only the preferred full-control specification.
  event_outputs <- list()
  for (i in seq_len(nrow(spec$outcomes))) {
    row <- spec$outcomes[i, ]
    paper_observations <- paper_observations_for_model(row$model_id[[1L]])
    event_outputs[[row$model_id[[1L]]]] <- estimate_event_study(
      data, row$outcome[[1L]], spec$control_sets[[spec$preferred_control_set]], spec$period,
      spec$post_start, spec$reference_year, row$model_id[[1L]],
      metadata = list(
        outcome_label = row$label[[1L]], paper_observations = paper_observations,
        observations_vs_paper = NA_real_
      ),
      firm_controls = if (isTRUE(spec$report_event_control_groups)) spec$firm_controls else NULL,
      country_controls = if (isTRUE(spec$report_event_control_groups)) spec$country_controls else NULL
    )
    event_outputs[[row$model_id[[1L]]]]$fit_stats$observations_vs_paper <-
      event_outputs[[row$model_id[[1L]]]]$fit_stats$observations - paper_observations
  }
  list(
    specification = spec,
    models = purrr::map(static_outputs, "model"),
    estimation_samples = purrr::map(static_outputs, "estimation_data"),
    coefficients = purrr::map_dfr(static_outputs, "coefficients"),
    fit_stats = purrr::map_dfr(static_outputs, "fit_stats"),
    sample_summary = purrr::map_dfr(static_outputs, "sample_summary"),
    event_study = list(
      models = purrr::map(event_outputs, "model"),
      coefficients = purrr::map_dfr(event_outputs, "coefficients"),
      pretrend_tests = purrr::map_dfr(event_outputs, "pretrend_test"),
      fit_stats = purrr::map_dfr(event_outputs, "fit_stats")
    )
  )
}

# `run_matching_analysis()` builds the outcome-specific propensity-score design,
# reports overlap and covariate balance, applies each retained weighting scheme,
# and re-estimates the preferred static and dynamic specifications. Its return
# object keeps both design diagnostics and model results.
run_matching_analysis <- function(data, spec, matching_spec) {
  analysis_id <- matching_spec$analysis_id
  anchor_year <- matching_spec$anchor_year
  variants <- matching_spec$variants
  covariates <- matching_spec$covariates
  # Propensity scores use only firm controls measured in the last pre-period.
  design <- make_matching_design(data, covariates, anchor_year, variants)
  outcomes <- if (analysis_id == "cash") spec$outcomes[1L, ] else spec$outcomes
  static <- list(); dynamic <- list()
  # Apply each design once and estimate both its static and event-study model.
  for (i in seq_len(nrow(variants))) {
    variant <- variants[i, ]
    variant_data <- apply_matching_variant(data, design, variant)
    for (j in seq_len(nrow(outcomes))) {
      outcome <- outcomes[j, ]
      key <- paste(variant$variant_id[[1L]], outcome$model_id[[1L]], sep = ".")
      output_model_id <- if (analysis_id == "cash") "cash_assets" else outcome$model_id[[1L]]
      paper_observations <- paper_observations_for_model(output_model_id)
      metadata <- list(variant_id = variant$variant_id[[1L]], variant_label = variant$variant_label[[1L]],
                       outcome_label = outcome$label[[1L]])
      if (analysis_id == "cash") metadata$scale <- "ratio_pp"
      if (analysis_id == "risk") {
        metadata$paper_observations <- paper_observations
        metadata$observations_vs_paper <- NA_real_
      }
      static[[key]] <- estimate_static_did(
        variant_data, outcome$outcome[[1L]], spec$control_sets[[spec$preferred_control_set]],
        spec$period, output_model_id, weight_variable = "analysis_weight", metadata = metadata
      )
      dynamic[[key]] <- estimate_event_study(
        variant_data, outcome$outcome[[1L]], spec$control_sets[[spec$preferred_control_set]],
        spec$period, spec$post_start, spec$reference_year, output_model_id,
        weight_variable = "analysis_weight", metadata = metadata
      )
      if (analysis_id == "risk") {
        static[[key]]$fit_stats$observations_vs_paper <-
          static[[key]]$fit_stats$observations - paper_observations
        dynamic[[key]]$fit_stats$observations_vs_paper <-
          dynamic[[key]]$fit_stats$observations - paper_observations
      }
    }
  }
  list(
    design = design,
    design_summary = matching_design_summary(design, anchor_year, variants),
    balance_diagnostics = matching_balance_diagnostics(design, covariates, anchor_year, variants),
    models = purrr::map(static, "model"),
    coefficients = purrr::map_dfr(static, "coefficients"),
    fit_stats = purrr::map_dfr(static, "fit_stats"),
    event_study = list(
      models = purrr::map(dynamic, "model"),
      coefficients = purrr::map_dfr(dynamic, "coefficients"),
      pretrend_tests = purrr::map_dfr(dynamic, "pretrend_test"),
      fit_stats = purrr::map_dfr(dynamic, "fit_stats")
    )
  )
}

# `run_policy_regime_analysis()` subsets an outcome sample to each registered
# policy regime, estimates the preferred static model, and returns the models,
# coefficient/fit tables, exact samples, and country-to-regime map.
run_policy_regime_analysis <- function(data, spec, regime_spec) {
  analysis_id <- regime_spec$analysis_id
  outcome <- spec$outcomes[1L, ]
  variants <- regime_spec$variants
  outputs <- list()
  # Every regime-specific sample retains all baseline control countries and only
  # the treated countries assigned to that regime.
  for (i in seq_len(nrow(variants))) {
    variant <- variants[i, ]
    subset <- dplyr::filter(data, .data$treat == 0L | .data$country_iso_code %in% unlist(variant$treated_countries))
    output_model_id <- if (analysis_id == "cash") "cash_assets" else outcome$model_id[[1L]]
    metadata <- list(
      regime_id = variant$regime_id[[1L]], regime_label = variant$regime_label[[1L]],
      outcome_label = outcome$label[[1L]]
    )
    if (analysis_id == "cash") metadata$scale <- "ratio_pp"
    if (analysis_id == "risk") {
      metadata$paper_observations <- paper_observations_for_model(output_model_id)
      metadata$observations_vs_paper <- NA_real_
    }
    outputs[[variant$regime_id[[1L]]]] <- estimate_static_did(
      subset, outcome$outcome[[1L]], spec$control_sets[[spec$preferred_control_set]], spec$period,
      output_model_id, metadata = metadata
    )
    if (analysis_id == "risk") {
      outputs[[variant$regime_id[[1L]]]]$fit_stats$observations_vs_paper <-
        outputs[[variant$regime_id[[1L]]]]$fit_stats$observations - paper_observations_for_model(output_model_id)
    }
  }
  list(
    regime_map = tidyr::unnest_longer(variants, "treated_countries", values_to = "country_iso_code") |>
      dplyr::select("regime_id", "regime_label", "country_iso_code", "paper_benchmark"),
    models = purrr::map(outputs, "model"),
    coefficients = purrr::map_dfr(outputs, "coefficients"),
    fit_stats = purrr::map_dfr(outputs, "fit_stats")
  )
}

# `linear_combination()` computes an estimate, clustered standard error, test
# statistic, and p-value for a named weighted sum of fitted coefficients.
linear_combination <- function(model, weights, test_id, label) {
  coefficients <- stats::coef(model)
  if (!all(names(weights) %in% names(coefficients))) {
    return(tibble::tibble(test_id = test_id, label = label, estimate = NA_real_, std_error = NA_real_,
                          statistic = NA_real_, df = cluster_count(model) - 1L, p_value = NA_real_,
                          conf_low = NA_real_, conf_high = NA_real_, tested_terms = paste(names(weights), collapse = ";")))
  }
  vector <- stats::setNames(rep(0, length(coefficients)), names(coefficients))
  vector[names(weights)] <- weights
  estimate <- sum(vector * coefficients)
  standard_error <- sqrt(as.numeric(t(vector) %*% stats::vcov(model) %*% vector))
  degrees <- cluster_count(model) - 1L
  statistic <- estimate / standard_error
  critical <- stats::qt(.975, degrees)
  tibble::tibble(
    test_id = test_id, label = label, estimate = estimate, std_error = standard_error,
    statistic = statistic, df = degrees,
    p_value = 2 * stats::pt(abs(statistic), degrees, lower.tail = FALSE),
    conf_low = estimate - critical * standard_error, conf_high = estimate + critical * standard_error,
    tested_terms = paste(names(weights), collapse = ";")
  )
}

# `joint_linear_test()` performs a Wald test for several coefficient
# restrictions and returns the F statistic, numerator/denominator degrees of
# freedom, p-value, and the tested terms.
joint_linear_test <- function(model, restrictions, test_id, label) {
  coefficients <- stats::coef(model)
  terms <- unique(unlist(lapply(restrictions, names), use.names = FALSE))
  clusters <- cluster_count(model)
  if (!all(terms %in% names(coefficients))) {
    return(tibble::tibble(test_id = test_id, label = label, statistic = NA_real_, df1 = NA_integer_,
                          df2 = clusters - 1L, p_value = NA_real_, tested_terms = paste(terms, collapse = ";"),
                          method = "country-clustered Wald F test"))
  }
  matrix <- do.call(rbind, lapply(restrictions, function(weights) {
    row <- stats::setNames(rep(0, length(coefficients)), names(coefficients)); row[names(weights)] <- weights; row
  }))
  estimate <- as.numeric(matrix %*% coefficients)
  variance <- matrix %*% stats::vcov(model) %*% t(matrix)
  rank <- qr(variance)$rank
  statistic <- tryCatch(as.numeric(t(estimate) %*% qr.solve(variance, estimate)) / rank, error = function(e) NA_real_)
  tibble::tibble(test_id = test_id, label = label, statistic = statistic, df1 = rank, df2 = clusters - 1L,
                 p_value = stats::pf(statistic, rank, clusters - 1L, lower.tail = FALSE),
                 tested_terms = paste(terms, collapse = ";"), method = "country-clustered Wald F test")
}

# `build_operational_risk_classification()` averages pre-treatment volatility
# over the configured anchor years, assigns firms to quartiles using the
# baseline leverage sample, and returns firm classifications plus support audits.
build_operational_risk_classification <- function(panel, leverage_data, settings) {
  # The classification universe is fixed by the baseline leverage firms and the
  # countries admitted to the heterogeneity analysis.
  universe <- leverage_data |>
    dplyr::filter(.data$country_iso_code %in% settings$included_countries) |>
    dplyr::distinct(.data$bv_d_id_number, .data$country_iso_code, .data$treat)
  if (anyDuplicated(universe$bv_d_id_number)) stop("Firm treatment or country assignment is not stable.", call. = FALSE)
  anchors <- panel |>
    dplyr::semi_join(universe, by = "bv_d_id_number") |>
    dplyr::filter(.data$year %in% settings$risk_anchor_years) |>
    dplyr::select("bv_d_id_number", "country_iso_code", "year", "treat", "earnvol")
  # Average only valid strictly pre-announcement volatility anchors by firm.
  firm_scores <- anchors |>
    dplyr::group_by(.data$bv_d_id_number) |>
    dplyr::summarise(
      strict_pre_announcement_risk = ifelse(sum(!is.na(.data$earnvol)) > 0L, mean(.data$earnvol, na.rm = TRUE), NA_real_),
      valid_risk_anchors = sum(!is.na(.data$earnvol)),
      observed_anchor_years = paste(sort(.data$year[!is.na(.data$earnvol)]), collapse = ";"),
      .groups = "drop"
    ) |>
    dplyr::right_join(universe, by = "bv_d_id_number") |>
    dplyr::mutate(
      valid_risk_anchors = tidyr::replace_na(.data$valid_risk_anchors, 0L),
      observed_anchor_years = tidyr::replace_na(.data$observed_anchor_years, ""),
      classification_status = dplyr::case_when(
        .data$valid_risk_anchors >= settings$risk_min_anchors ~ "classified",
        .data$valid_risk_anchors == 1L ~ "excluded_one_valid_anchor",
        TRUE ~ "excluded_no_valid_anchor"
      )
    )
  # Quartiles are pooled across eligible firms with deterministic tie ordering.
  classification <- firm_scores |>
    dplyr::filter(.data$classification_status == "classified") |>
    dplyr::arrange(.data$strict_pre_announcement_risk, .data$bv_d_id_number) |>
    dplyr::mutate(
      operational_risk_quartile = dplyr::ntile(.data$strict_pre_announcement_risk, 4L),
      operational_risk_group = paste0("Q", .data$operational_risk_quartile),
      high_operational_risk = as.integer(.data$operational_risk_quartile == 4L),
      classifier = "mean earnvol at 2012-2014 anchors; every forward window ends by 2016",
      quartile_rule = "pooled deterministic ntile; ties ordered by firm identifier"
    )
  list(
    classification = classification,
    variable_availability = tidyr::expand_grid(anchor_year = settings$risk_anchor_years) |>
      dplyr::left_join(
        anchors |>
          dplyr::group_by(anchor_year = .data$year) |>
          dplyr::summarise(rows = dplyr::n(), firms = dplyr::n_distinct(.data$bv_d_id_number),
                           nonmissing_earnvol_rows = sum(!is.na(.data$earnvol)),
                           firms_with_earnvol = dplyr::n_distinct(.data$bv_d_id_number[!is.na(.data$earnvol)]), .groups = "drop"),
        by = "anchor_year"
      ) |>
      dplyr::mutate(forward_window_end = .data$anchor_year + 2L,
                    strictly_pre_announcement = .data$forward_window_end <= 2016L),
    group_support = classification |>
      dplyr::group_by(.data$operational_risk_quartile, .data$operational_risk_group) |>
      dplyr::summarise(firms = dplyr::n(), treated_firms = sum(.data$treat == 1L),
                       control_firms = sum(.data$treat == 0L), countries = dplyr::n_distinct(.data$country_iso_code),
                       risk_mean = mean(.data$strict_pre_announcement_risk),
                       risk_median = stats::median(.data$strict_pre_announcement_risk), .groups = "drop"),
    country_support = classification |>
      dplyr::count(.data$operational_risk_quartile, .data$operational_risk_group,
                   .data$country_iso_code, .data$treat, name = "firms"),
    missingness_selection = firm_scores |>
      dplyr::count(.data$classification_status, name = "firms") |>
      dplyr::mutate(universe_firms = nrow(universe), share_of_universe = .data$firms / .data$universe_firms)
  )
}

# `build_restriction_pressure_classification()` measures how often a firm's
# pre-treatment interest burden exceeds the configured EBITDA threshold and
# assigns low/high restriction-pressure groups. It also returns support audits.
build_restriction_pressure_classification <- function(panel, leverage_data, settings) {
  # Start from the same baseline-firm universe and configured pre-reform years.
  universe <- leverage_data |>
    dplyr::filter(.data$country_iso_code %in% settings$included_countries) |>
    dplyr::distinct(.data$bv_d_id_number, .data$country_iso_code, .data$treat)
  # A firm-year is under accounting pressure when net financial expense exceeds
  # the configured share of non-negative EBITDA; invalid inputs remain explicit.
  annual <- panel |>
    dplyr::semi_join(universe, by = "bv_d_id_number") |>
    dplyr::filter(.data$year %in% settings$pressure_years) |>
    dplyr::select(
      "bv_d_id_number", "country_iso_code", "year", "treat",
      "financial_expenses_pos", "financial_revenue_th_usd",
      "ebitda_th_usd", "total_assets_th_usd"
    ) |>
    dplyr::mutate(
      invalid_reason = dplyr::case_when(
        is.na(.data$financial_expenses_pos) ~ "missing_financial_expense",
        is.na(.data$financial_revenue_th_usd) ~ "missing_financial_revenue",
        .data$financial_revenue_th_usd < 0 ~ "negative_financial_revenue",
        is.na(.data$ebitda_th_usd) ~ "missing_ebitda",
        is.na(.data$total_assets_th_usd) | .data$total_assets_th_usd <= 0 ~ "invalid_total_assets",
        TRUE ~ "valid"
      ),
      valid_pressure_year = .data$invalid_reason == "valid",
      accounting_nfe = dplyr::if_else(.data$valid_pressure_year,
                                      pmax(.data$financial_expenses_pos - .data$financial_revenue_th_usd, 0), NA_real_),
      ebitda_capacity = dplyr::if_else(.data$valid_pressure_year,
                                      settings$ebitda_threshold * pmax(.data$ebitda_th_usd, 0), NA_real_),
      pressure_condition = dplyr::if_else(.data$valid_pressure_year,
                                          as.integer(.data$accounting_nfe > .data$ebitda_capacity), NA_integer_)
    )
  # Collapse valid firm-years to the share of observed years under pressure.
  firm_scores <- annual |>
    dplyr::group_by(.data$bv_d_id_number) |>
    dplyr::summarise(
      valid_pressure_years = sum(.data$valid_pressure_year),
      pressure_years = sum(.data$pressure_condition == 1L, na.rm = TRUE),
      pressure_share = ifelse(.data$valid_pressure_years > 0L, .data$pressure_years / .data$valid_pressure_years, NA_real_),
      observed_pressure_years = paste(sort(.data$year[.data$valid_pressure_year]), collapse = ";"), .groups = "drop"
    ) |>
    dplyr::right_join(universe, by = "bv_d_id_number") |>
    dplyr::mutate(
      valid_pressure_years = tidyr::replace_na(.data$valid_pressure_years, 0L),
      pressure_years = tidyr::replace_na(.data$pressure_years, 0L),
      observed_pressure_years = tidyr::replace_na(.data$observed_pressure_years, ""),
      classification_status = dplyr::case_when(
        .data$valid_pressure_years >= settings$pressure_min_years ~ "classified",
        .data$valid_pressure_years == 1L ~ "excluded_one_valid_year",
        TRUE ~ "excluded_no_valid_year"
      )
    )
  # The high group must meet the configured majority-of-years threshold.
  classification <- firm_scores |>
    dplyr::filter(.data$classification_status == "classified") |>
    dplyr::mutate(
      high_restriction_pressure = as.integer(.data$pressure_share >= settings$high_pressure_share),
      restriction_pressure_group = dplyr::if_else(.data$high_restriction_pressure == 1L, "High pressure", "Low pressure"),
      classifier = "accounting_30pct_majority",
      interpretation = "accounting_exposure_proxy_not_statutory_bindingness"
    )
  list(
    classification = classification, annual = annual,
    year_availability = annual |>
      dplyr::group_by(.data$year) |>
      dplyr::summarise(rows = dplyr::n(), firms = dplyr::n_distinct(.data$bv_d_id_number),
                       valid_rows = sum(.data$valid_pressure_year),
                       pressure_rows = sum(.data$pressure_condition == 1L, na.rm = TRUE), .groups = "drop"),
    invalid_reasons = dplyr::count(annual, .data$year, .data$invalid_reason, name = "rows"),
    group_support = classification |>
      dplyr::group_by(.data$high_restriction_pressure, .data$restriction_pressure_group) |>
      dplyr::summarise(firms = dplyr::n(), treated_firms = sum(.data$treat == 1L), control_firms = sum(.data$treat == 0L),
                       countries = dplyr::n_distinct(.data$country_iso_code), mean_pressure_share = mean(.data$pressure_share), .groups = "drop"),
    country_support = classification |>
      dplyr::count(.data$high_restriction_pressure, .data$restriction_pressure_group,
                   .data$country_iso_code, .data$treat, name = "firms"),
    missingness_selection = firm_scores |>
      dplyr::count(.data$classification_status, name = "firms") |>
      dplyr::mutate(universe_firms = nrow(universe), share_of_universe = .data$firms / .data$universe_firms)
  )
}

# `heterogeneity_model_data()` joins a firm classification to the leverage panel
# and retains only observations with complete classification columns.
heterogeneity_model_data <- function(leverage_data, classification, columns, controls) {
  leverage_data |>
    dplyr::inner_join(dplyr::select(classification, "bv_d_id_number", dplyr::all_of(columns)), by = "bv_d_id_number") |>
    dplyr::select(dplyr::all_of(c("dta", "treat", "post", controls, "bv_d_id_number", "year", "country_iso_code", columns))) |>
    tidyr::drop_na() |>
    filter_model_pre_post_support("post") |>
    dplyr::arrange(.data$bv_d_id_number, .data$year)
}

# `run_operational_risk_heterogeneity()` interacts treatment timing with the
# operational-risk quartiles. It returns group marginal effects, pairwise and
# joint equality tests, model/sample support, and group-specific event studies.
run_operational_risk_heterogeneity <- function(leverage_data, classification, controls) {
  # Q1 is the reference group; added interactions measure how Q2–Q4 differ.
  data <- heterogeneity_model_data(
    leverage_data, classification$classification,
    c("operational_risk_quartile", "operational_risk_group", "strict_pre_announcement_risk"),
    controls
  ) |>
    dplyr::mutate(treat_post = .data$treat * .data$post)
  for (quartile in 2:4) {
    data[[paste0("post_risk_q", quartile)]] <- data$post * as.integer(data$operational_risk_quartile == quartile)
    data[[paste0("treat_post_risk_q", quartile)]] <- data$treat * data[[paste0("post_risk_q", quartile)]]
  }
  interaction_terms <- c("treat_post", paste0("post_risk_q", 2:4), paste0("treat_post_risk_q", 2:4))
  formula_text <- sprintf("dta ~ %s | bv_d_id_number + year", paste(c(interaction_terms, controls), collapse = " + "))
  model <- fixest::feols(stats::as.formula(formula_text), data = data, cluster = ~country_iso_code)
  baseline <- estimate_static_did(data, "dta", controls, "post", "classified_sample_average")
  # Recover one total treatment effect per quartile from the interaction model.
  marginal <- purrr::map_dfr(1:4, function(quartile) {
    weights <- c(treat_post = 1)
    if (quartile > 1L) weights[[paste0("treat_post_risk_q", quartile)]] <- 1
    linear_combination(model, weights, paste0("q", quartile, "_effect"),
                       paste0("Treatment effect for operational-risk Q", quartile)) |>
      dplyr::mutate(operational_risk_quartile = quartile, .before = 1)
  })
  equality <- dplyr::bind_rows(
    linear_combination(model, c(treat_post_risk_q4 = 1), "q4_vs_q1", "Difference between Q4 and Q1 treatment effects"),
    joint_linear_test(model, lapply(2:4, function(q) stats::setNames(1, paste0("treat_post_risk_q", q))),
                      "all_quartiles_equal", "Joint equality of Q1-Q4 treatment effects") |>
      dplyr::mutate(estimate = NA_real_, std_error = NA_real_, conf_low = NA_real_, conf_high = NA_real_)
  )
  # Estimate separate annual paths while keeping all groups in one common model.
  dynamic <- run_explicit_heterogeneity_event(
    data, group_values = 1:4, group_variable = "operational_risk_quartile",
    group_prefix = "risk_q", reference_year = 2018L, pretrend_years = 2014:2017,
    controls = controls
  )
  list(
    static_model = model, classified_baseline_model = baseline$model,
    coefficients = tidy_fixest_coefficients(model),
    classified_baseline = dplyr::filter(baseline$coefficients, .data$term == "treat:post"),
    marginal_effects = marginal, equality_tests = equality,
    fit_stats = dplyr::bind_rows(
      tibble::tibble(model_id = "leverage_operational_risk_quartiles", observations = stats::nobs(model),
                     firms = dplyr::n_distinct(data$bv_d_id_number), countries = cluster_count(model),
                     adjusted_r2 = extract_fixest_stat(model, "ar2"), within_r2 = extract_fixest_stat(model, "wr2"), formula = formula_text),
      baseline$fit_stats
    ),
    model_support = data |> dplyr::group_by(.data$operational_risk_quartile, .data$treat) |>
      dplyr::summarise(observations = dplyr::n(), firms = dplyr::n_distinct(.data$bv_d_id_number),
                       countries = dplyr::n_distinct(.data$country_iso_code), .groups = "drop"),
    dynamic = dynamic
  )
}

# `run_explicit_heterogeneity_event()` estimates one event-time path per firm
# group in a common fixed-effects model, then returns group coefficients,
# confidence intervals, fit statistics, and incremental pre-trend tests.
run_explicit_heterogeneity_event <- function(data, group_values, group_variable, group_prefix,
                                             reference_year, pretrend_years, controls) {
  # Construct explicit treated-year and incremental group-year interactions.
  years <- 2014:2023
  groups_after_reference <- setdiff(group_values, group_values[[1L]])
  for (year in setdiff(years, reference_year)) {
    data[[paste0("treated_year_", year)]] <- data$treat * as.integer(data$year == year)
    for (group in groups_after_reference) {
      data[[paste0(group_prefix, group, "_year_", year)]] <- as.integer(data[[group_variable]] == group) * as.integer(data$year == year)
      data[[paste0("treated_", group_prefix, group, "_year_", year)]] <- data$treat * data[[paste0(group_prefix, group, "_year_", year)]]
    }
  }
  dynamic_terms <- unlist(lapply(setdiff(years, reference_year), function(year) {
    c(paste0("treated_year_", year),
      unlist(lapply(groups_after_reference, function(group) c(
        paste0(group_prefix, group, "_year_", year), paste0("treated_", group_prefix, group, "_year_", year)
      ))))
  }), use.names = FALSE)
  formula_text <- sprintf("dta ~ %s | bv_d_id_number + year", paste(c(dynamic_terms, controls), collapse = " + "))
  model <- fixest::feols(stats::as.formula(formula_text), data = data, cluster = ~country_iso_code)
  # Combine the reference-group path with each group's incremental coefficient.
  event <- tidyr::expand_grid(group = group_values, event_year = years) |>
    purrr::pmap_dfr(function(group, event_year) {
      if (event_year == reference_year) {
        return(tibble::tibble(group = group, event_year = event_year, estimate = 0, std_error = NA_real_,
                              statistic = NA_real_, df = cluster_count(model) - 1L, p_value = NA_real_,
                              conf_low = 0, conf_high = 0, tested_terms = ""))
      }
      weights <- stats::setNames(1, paste0("treated_year_", event_year))
      if (group != group_values[[1L]]) weights[[paste0("treated_", group_prefix, group, "_year_", event_year)]] <- 1
      linear_combination(model, weights, paste0(group_prefix, group, "_", event_year),
                         paste0("Group ", group, " effect in ", event_year)) |>
        dplyr::mutate(group = group, event_year = event_year, .before = 1)
    }) |>
    dplyr::mutate(reference_year = reference_year,
                  period = dplyr::case_when(.data$event_year < reference_year ~ "pre",
                                            .data$event_year == reference_year ~ "reference", TRUE ~ "post"))
  # Report total pre-trends by group, incremental group differences, and one
  # joint test covering all incremental pre-period coefficients where relevant.
  pretrends <- purrr::map_dfr(group_values, function(group) {
    restrictions <- lapply(pretrend_years, function(year) {
      weights <- stats::setNames(1, paste0("treated_year_", year))
      if (group != group_values[[1L]]) weights[[paste0("treated_", group_prefix, group, "_year_", year)]] <- 1
      weights
    })
    joint_linear_test(model, restrictions, paste0(group_prefix, group, "_pretrend"), paste0("Group ", group, " pretrend")) |>
      dplyr::mutate(group = group, .before = 1)
  })
  incremental <- purrr::map_dfr(groups_after_reference, function(group) {
    restrictions <- lapply(pretrend_years, function(year) {
      stats::setNames(1, paste0("treated_", group_prefix, group, "_year_", year))
    })
    joint_linear_test(model, restrictions, paste0(group_prefix, group, "_incremental_pretrend"),
                      paste0("Incremental group ", group, " pretrend")) |>
      dplyr::mutate(group = group, .before = 1)
  })
  joint_incremental <- if (length(groups_after_reference) > 1L) {
    restrictions <- lapply(
      unlist(lapply(groups_after_reference, function(group) {
        paste0("treated_", group_prefix, group, "_year_", pretrend_years)
      }), use.names = FALSE),
      function(term) stats::setNames(1, term)
    )
    joint_linear_test(model, restrictions, "all_incremental_pretrends",
                      "Joint incremental pretrend heterogeneity") |>
      dplyr::mutate(group = NA_integer_, .before = 1)
  } else {
    tibble::tibble()
  }
  pretrends <- dplyr::bind_rows(pretrends, incremental, joint_incremental)
  list(model = model, coefficients = tidy_fixest_coefficients(model), event_coefficients = event,
       pretrend_tests = pretrends,
       fit_stats = tibble::tibble(model_id = paste0("dynamic_", group_prefix), observations = stats::nobs(model),
                                  firms = dplyr::n_distinct(data$bv_d_id_number), countries = cluster_count(model),
                                  adjusted_r2 = extract_fixest_stat(model, "ar2"), within_r2 = extract_fixest_stat(model, "wr2"),
                                  formula = formula_text))
}

# `run_restriction_pressure_heterogeneity()` estimates and compares leverage
# effects for low- and high-pressure firms. It returns marginal effects, their
# equality test, support diagnostics, and group-specific dynamic estimates.
run_restriction_pressure_heterogeneity <- function(leverage_data, classification, controls) {
  # Low-pressure firms are the reference; the triple interaction is the
  # high-minus-low difference in the treatment effect.
  data <- heterogeneity_model_data(
    leverage_data, classification$classification,
    c("pressure_share", "high_restriction_pressure", "restriction_pressure_group"),
    controls
  ) |>
    dplyr::mutate(
      treat_post = .data$treat * .data$post,
      post_high_pressure = .data$post * .data$high_restriction_pressure,
      treat_post_high_pressure = .data$treat * .data$post * .data$high_restriction_pressure
    )
  formula_text <- sprintf("dta ~ treat_post + post_high_pressure + treat_post_high_pressure + %s | bv_d_id_number + year",
                          paste(controls, collapse = " + "))
  model <- fixest::feols(stats::as.formula(formula_text), data = data, cluster = ~country_iso_code)
  baseline <- estimate_static_did(data, "dta", controls, "post", "restriction_pressure_classified_average")
  # Recover readable low- and high-pressure total effects.
  marginal <- dplyr::bind_rows(
    linear_combination(model, c(treat_post = 1), "low_pressure_effect", "Treatment effect for low-pressure firms") |>
      dplyr::mutate(high_restriction_pressure = 0L, .before = 1),
    linear_combination(model, c(treat_post = 1, treat_post_high_pressure = 1), "high_pressure_effect",
                       "Treatment effect for high-pressure firms") |>
      dplyr::mutate(high_restriction_pressure = 1L, .before = 1)
  )
  dynamic <- run_explicit_heterogeneity_event(
    data, group_values = 0:1, group_variable = "high_restriction_pressure",
    group_prefix = "pressure_", reference_year = 2018L, pretrend_years = 2014:2017,
    controls = controls
  )
  list(
    static_model = model, classified_baseline_model = baseline$model,
    coefficients = tidy_fixest_coefficients(model),
    classified_baseline = dplyr::filter(baseline$coefficients, .data$term == "treat:post"),
    marginal_effects = marginal,
    equality_test = linear_combination(model, c(treat_post_high_pressure = 1), "high_vs_low_pressure",
                                       "Difference between high- and low-pressure treatment effects"),
    fit_stats = dplyr::bind_rows(
      tibble::tibble(model_id = "leverage_restriction_pressure", observations = stats::nobs(model),
                     firms = dplyr::n_distinct(data$bv_d_id_number), countries = cluster_count(model),
                     adjusted_r2 = extract_fixest_stat(model, "ar2"), within_r2 = extract_fixest_stat(model, "wr2"), formula = formula_text),
      baseline$fit_stats
    ),
    model_support = data |> dplyr::group_by(.data$high_restriction_pressure, .data$treat) |>
      dplyr::summarise(observations = dplyr::n(), firms = dplyr::n_distinct(.data$bv_d_id_number),
                       countries = dplyr::n_distinct(.data$country_iso_code), .groups = "drop"),
    dynamic = dynamic
  )
}
