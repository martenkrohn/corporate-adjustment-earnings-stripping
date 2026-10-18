# Observable-comparability algorithms. The selected designs and their settings
# are declared in `code/06_robustness.R` and passed into these helpers.

# `safe_weighted_mean()` computes a weighted mean after dropping missing or
# non-positive weights. It returns `NA` rather than failing if nothing remains.
safe_weighted_mean <- function(x, weight) {
  keep <- !is.na(x) & !is.na(weight) & weight > 0
  if (!any(keep)) return(NA_real_)
  sum(x[keep] * weight[keep]) / sum(weight[keep])
}

# `safe_weighted_variance()` is the matching weighted-variance counterpart.
# It is used only for balance statistics and safely handles tiny empty groups.
safe_weighted_variance <- function(x, weight) {
  keep <- !is.na(x) & !is.na(weight) & weight > 0
  if (sum(keep) < 2L || sum(weight[keep]) <= 0) return(NA_real_)
  center <- safe_weighted_mean(x[keep], weight[keep])
  sum(weight[keep] * (x[keep] - center)^2) / sum(weight[keep])
}

# `make_matching_anchor_data()` keeps one complete firm observation in the
# anchor year and estimates treatment propensity from the supplied covariates.
# It returns the firm-level cross-section with bounded propensity scores.
make_matching_anchor_data <- function(data, covariates, anchor_year) {
  required <- unique(c("bv_d_id_number", "country_iso_code", "treat", "year", covariates))
  assert_required_columns(data, required, sprintf("%s matching input", anchor_year))
  anchor <- data |>
    dplyr::filter(.data$year == anchor_year) |>
    dplyr::select(dplyr::all_of(required)) |>
    dplyr::distinct(.data$bv_d_id_number, .keep_all = TRUE) |>
    tidyr::drop_na(dplyr::all_of(c("bv_d_id_number", "treat", covariates)))
  if (nrow(anchor) == 0L || !all(c(0L, 1L) %in% unique(anchor$treat))) {
    stop(sprintf("The %s matching anchor must contain treated and control firms.", anchor_year), call. = FALSE)
  }
  propensity_model <- stats::glm(
    stats::reformulate(covariates, response = "treat"),
    data = anchor,
    family = stats::binomial()
  )
  anchor |>
    dplyr::mutate(
      pscore = pmin(pmax(as.numeric(stats::fitted(propensity_model)), 1e-6), 1 - 1e-6),
      matching_anchor_year = anchor_year,
      matching_covariates = paste(covariates, collapse = "; ")
    )
}

# `make_no_replacement_ps_weights()` greedily pairs the closest treated and
# control firms inside the caliper, using each firm at most once. Its numeric
# output is a 1/0 inclusion weight aligned with the anchor-data rows.
make_no_replacement_ps_weights <- function(anchor, caliper = 0.001) {
  # Compute all treated-control propensity-score distances and retain only
  # candidate pairs inside the allowed caliper.
  treated <- dplyr::filter(anchor, .data$treat == 1L)
  controls <- dplyr::filter(anchor, .data$treat == 0L)
  distances <- abs(outer(treated$pscore, controls$pscore, "-"))
  candidates <- which(!is.na(distances) & distances <= caliper, arr.ind = TRUE)
  weights <- rep(0, nrow(anchor))
  if (nrow(candidates) == 0L) return(weights)
  candidates <- tibble::tibble(
    treated_row = candidates[, "row"],
    control_row = candidates[, "col"],
    distance = distances[candidates]
  ) |>
    dplyr::arrange(.data$distance)
  used_treated <- rep(FALSE, nrow(treated))
  used_controls <- rep(FALSE, nrow(controls))
  selected_treated <- character()
  selected_controls <- character()
  # Starting with the closest pair, accept a match only if neither firm has
  # already been used. This enforces 1:1 matching without replacement.
  for (i in seq_len(nrow(candidates))) {
    tr <- candidates$treated_row[[i]]
    co <- candidates$control_row[[i]]
    if (!used_treated[[tr]] && !used_controls[[co]]) {
      used_treated[[tr]] <- TRUE
      used_controls[[co]] <- TRUE
      selected_treated <- c(selected_treated, treated$bv_d_id_number[[tr]])
      selected_controls <- c(selected_controls, controls$bv_d_id_number[[co]])
    }
  }
  weights[anchor$bv_d_id_number %in% c(selected_treated, selected_controls)] <- 1
  weights
}

# `make_matching_design()` combines propensity scores with both retained
# designs. It appends nearest-neighbour and ATT-IPW weights to the anchor data.
make_matching_design <- function(data, covariates, anchor_year, variants) {
  anchor <- make_matching_anchor_data(data, covariates, anchor_year)
  anchor[[variants$weight_col[[1L]]]] <- make_no_replacement_ps_weights(anchor, variants$caliper[[1L]])
  anchor[[variants$weight_col[[2L]]]] <- ifelse(anchor$treat == 1L, 1, anchor$pscore / (1 - anchor$pscore))
  anchor
}

# `matching_design_summary()` reports the available and retained treated and
# control firms, their weighted mass, and the largest control weight by design.
matching_design_summary <- function(design, anchor_year, variants) {
  purrr::pmap_dfr(variants, function(variant_id, variant_label, weight_col, method, distance,
                                    ratio, replacement, caliper, exact_cols, note) {
    weight <- design[[weight_col]]
    included <- !is.na(weight) & weight > 0
    tibble::tibble(
      variant_id = variant_id, variant_label = variant_label, note = note,
      method = method, distance = distance, ratio = ratio,
      replacement = replacement, caliper = caliper,
      exact_cols = paste(unlist(exact_cols, use.names = FALSE), collapse = "; "),
      anchor_year = anchor_year,
      matching_covariates = unique(design$matching_covariates)[[1L]],
      anchor_pool_firms = nrow(design),
      anchor_pool_treated_firms = sum(design$treat == 1L),
      anchor_pool_control_firms = sum(design$treat == 0L),
      included_firms = sum(included),
      included_treated_firms = sum(included & design$treat == 1L),
      included_control_firms = sum(included & design$treat == 0L),
      weighted_treated_firm_mass = sum(weight[included & design$treat == 1L], na.rm = TRUE),
      weighted_control_firm_mass = sum(weight[included & design$treat == 0L], na.rm = TRUE),
      max_control_weight = if (any(included & design$treat == 0L)) max(weight[included & design$treat == 0L]) else NA_real_
    )
  })
}

# `matching_balance_diagnostics()` compares weighted treated/control means for
# every matching covariate and returns standardized mean differences before
# and after matching or weighting.
matching_balance_diagnostics <- function(design, covariates, anchor_year, variants) {
  # Include the unweighted anchor pool so balance improvements remain visible.
  variants <- dplyr::bind_rows(
    tibble::tibble(
      variant_id = paste0("unweighted_", anchor_year, "_anchor"),
      variant_label = paste0("Unweighted ", anchor_year, " anchor pool"),
      weight_col = "unweighted_weight"
    ),
    dplyr::select(variants, "variant_id", "variant_label", "weight_col")
  )
  design$unweighted_weight <- 1
  purrr::pmap_dfr(variants, function(variant_id, variant_label, weight_col) {
    purrr::map_dfr(covariates, function(covariate) {
      weight <- design[[weight_col]]
      value <- design[[covariate]]
      treated <- design$treat == 1L & !is.na(weight) & weight > 0
      control <- design$treat == 0L & !is.na(weight) & weight > 0
      treated_mean <- safe_weighted_mean(value[treated], weight[treated])
      control_mean <- safe_weighted_mean(value[control], weight[control])
      # Standardize the weighted mean gap by pooled within-group dispersion.
      pooled_sd <- sqrt((safe_weighted_variance(value[treated], weight[treated]) +
                           safe_weighted_variance(value[control], weight[control])) / 2)
      tibble::tibble(
        variant_id = variant_id, variant_label = variant_label, covariate = covariate,
        treated_mean = treated_mean, control_mean = control_mean,
        standardized_mean_difference = ifelse(is.na(pooled_sd) || pooled_sd == 0, NA_real_,
                                              (treated_mean - control_mean) / pooled_sd),
        treated_firms = sum(treated), control_firms = sum(control)
      )
    })
  })
}

# `apply_matching_variant()` joins one selected firm-level weight back to every
# available firm-year observation and drops firms with zero or missing weight.
apply_matching_variant <- function(data, design, variant) {
  weights <- design |>
    dplyr::transmute(
      bv_d_id_number = .data$bv_d_id_number,
      analysis_weight = as.numeric(.data[[variant$weight_col[[1L]]]])
    ) |>
    dplyr::filter(!is.na(.data$analysis_weight), .data$analysis_weight > 0)
  dplyr::inner_join(data, weights, by = "bv_d_id_number")
}
