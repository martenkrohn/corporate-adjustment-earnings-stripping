# Data preparation helpers for the linear thesis analysis package.
# Adapted from the approved development sources in R/R/core/data and samples.

# `assert_required_columns()` stops immediately when an input lacks a variable
# required by the next transformation. On success it returns invisibly.
assert_required_columns <- function(data, required_cols, context = "data") {
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0L) {
    stop(sprintf("%s is missing required columns: %s", context, paste(missing_cols, collapse = ", ")), call. = FALSE)
  }
  invisible(TRUE)
}

# `assert_unique_firm_year()` verifies that each firm-year key occurs once.
# This protects later lags, joins, and fixed-effects models from duplicate rows.
assert_unique_firm_year <- function(data, firm_col = "bv_d_id_number", year_col = "year") {
  assert_required_columns(data, c(firm_col, year_col), "firm-year panel")
  duplicates <- data |>
    dplyr::count(.data[[firm_col]], .data[[year_col]], name = "n") |>
    dplyr::filter(.data$n > 1L)
  if (nrow(duplicates) > 0L) {
    stop(sprintf("Firm-year keys are not unique (%s duplicate keys).", nrow(duplicates)), call. = FALSE)
  }
  invisible(TRUE)
}

# `audit_counts_snapshot()` returns a one-row record of observations, firms,
# countries, and years at a named processing stage.
audit_counts_snapshot <- function(data, stage) {
  tibble::tibble(
    stage = stage,
    observations = nrow(data),
    firms = if ("bv_d_id_number" %in% names(data)) dplyr::n_distinct(data$bv_d_id_number) else NA_integer_,
    countries = if ("country_iso_code" %in% names(data)) dplyr::n_distinct(data$country_iso_code) else NA_integer_,
    years = if ("year" %in% names(data)) dplyr::n_distinct(data$year) else NA_integer_
  )
}

# `audit_missingness_snapshot()` returns missing counts and shares for selected
# variables, labelled with the processing stage at which they were measured.
audit_missingness_snapshot <- function(data, stage, vars = names(data)) {
  vars <- intersect(vars, names(data))
  tibble::tibble(variable = vars) |>
    dplyr::mutate(
      stage = stage,
      missing_n = purrr::map_int(.data$variable, ~ sum(is.na(data[[.x]]))),
      missing_share = purrr::map_dbl(.data$variable, ~ mean(is.na(data[[.x]])))
    ) |>
    dplyr::select("stage", "variable", "missing_n", "missing_share")
}

# `new_filter_log()` creates the correctly typed empty attrition table used to
# record how each cleaning or sample rule changes the data.
new_filter_log <- function() {
  tibble::tibble(
    step = character(), reason = character(),
    obs_before = integer(), obs_after = integer(),
    firms_before = integer(), firms_after = integer(),
    countries_before = integer(), countries_after = integer()
  )
}

# `append_filter_log()` adds one before/after row with observation, firm, and
# country counts plus a human-readable reason for the filter.
append_filter_log <- function(log, step, before, after, reason) {
  firm_count <- function(x) if ("bv_d_id_number" %in% names(x)) dplyr::n_distinct(x$bv_d_id_number) else NA_integer_
  country_count <- function(x) if ("country_iso_code" %in% names(x)) dplyr::n_distinct(x$country_iso_code) else NA_integer_
  dplyr::bind_rows(log, tibble::tibble(
    step = step, reason = reason,
    obs_before = nrow(before), obs_after = nrow(after),
    firms_before = firm_count(before), firms_after = firm_count(after),
    countries_before = country_count(before), countries_after = country_count(after)
  ))
}

# `import_orbis()` reads the staged wide Orbis exports, harmonizes firm IDs and
# annual column names, stacks workbook parts, reshapes years into rows, converts
# measures to numeric, checks unique keys, and returns data plus import audits.
import_orbis <- function(orbis_files) {
  # Local helpers handle workbook layout, ID detection, numeric text, and the
  # duplicate names that readxl assigns to repeated metadata columns.
  pick_results_sheet <- function(path) {
    sheets <- readxl::excel_sheets(path)
    if (length(sheets) == 1L) return(sheets[[1L]])
    likely <- sheets[stringr::str_detect(stringr::str_to_lower(sheets), "result|results|data|export|table|sheet2")]
    if (length(likely) > 0L) likely[[1L]] else sheets[[2L]]
  }

  detect_bvd_id_col <- function(x) {
    if ("bv_d_id_number" %in% x) return("bv_d_id_number")
    if ("bvd_id_number" %in% x) return("bvd_id_number")
    idx <- which(stringr::str_detect(stringr::str_to_lower(x), "bv.?d.*id|bvd.*id"))
    if (length(idx) == 0L) return(NA_character_)
    x[idx][which.min(nchar(x[idx]))]
  }

  clean_numeric <- function(x) {
    if (is.numeric(x)) return(x)
    y <- stringr::str_trim(as.character(x))
    y[stringr::str_to_lower(y) %in% c("", "n.a.", "n.a", "n/a", "na", "null", "-", "--")] <- NA_character_
    suppressWarnings(as.numeric(stringr::str_replace_all(y, "[,[:space:]]+", "")))
  }

  collapse_readxl_duplicates <- function(data) {
    duplicate_cols <- names(data)[
      stringi::stri_detect_regex(names(data), "_\\d+$") &
        !stringi::stri_detect_regex(names(data), "_(19\\d{2}|20\\d{2})$")
    ]
    if (length(duplicate_cols) == 0L) return(data)
    mapping <- tibble::tibble(
      column = duplicate_cols,
      base = stringi::stri_replace_last_regex(duplicate_cols, "_\\d+$", "")
    ) |>
      dplyr::group_by(.data$base) |>
      dplyr::filter(dplyr::n() >= 2L) |>
      dplyr::ungroup()
    for (base in unique(mapping$base)) {
      current <- mapping$column[mapping$base == base]
      data[[base]] <- purrr::reduce(data[current], dplyr::coalesce)
      data <- dplyr::select(data, -dplyr::all_of(current))
    }
    data
  }

  # Read and standardize one workbook part while it is still in wide format.
  read_one <- function(path) {
    extension <- tolower(tools::file_ext(path))
    data <- if (extension %in% c("csv", "txt")) {
      readr::read_csv(path, show_col_types = FALSE)
    } else if (extension %in% c("xlsx", "xls")) {
      suppressMessages(readxl::read_excel(path, sheet = pick_results_sheet(path), .name_repair = "unique"))
    } else {
      stop(sprintf("Unsupported Orbis file extension: %s", extension), call. = FALSE)
    }
    data <- data |>
      dplyr::filter(dplyr::if_any(dplyr::everything(), ~ !is.na(.x))) |>
      janitor::clean_names() |>
      dplyr::select(-dplyr::any_of(c("x1", "x_1", "unnamed_0", "unnamed_1", "dot_dot_dot_1")))
    id_col <- detect_bvd_id_col(names(data))
    if (is.na(id_col)) stop(sprintf("Could not detect BvD ID column in %s.", basename(path)), call. = FALSE)
    data |>
      dplyr::rename(bv_d_id_number = dplyr::all_of(id_col)) |>
      dplyr::mutate(bv_d_id_number = as.character(.data$bv_d_id_number)) |>
      collapse_readxl_duplicates()
  }

  # Read all staged parts and join only columns not already supplied by an
  # earlier part, using the stable BvD firm identifier as the key.
  paths <- unique(orbis_files)
  if (length(paths) == 0L || any(!file.exists(paths))) stop("One or more Orbis input files are missing.", call. = FALSE)
  imported <- purrr::map(paths, read_one)
  log <- new_filter_log()
  wide <- imported[[1L]]
  if (length(imported) > 1L) {
    for (i in 2:length(imported)) {
      before <- wide
      next_data <- imported[[i]]
      new_cols <- setdiff(names(next_data), names(wide))
      wide <- dplyr::full_join(wide, dplyr::select(next_data, "bv_d_id_number", dplyr::all_of(new_cols)), by = "bv_d_id_number")
      log <- append_filter_log(log, paste0("merge_export_", i), before, wide,
                               sprintf("Merged %s and added %s new columns.", basename(paths[[i]]), length(new_cols)))
    }
  }
  # Annual Orbis columns encode the year in their name. Pivot them to long
  # firm-year form and then widen economic variables into ordinary columns.
  year_cols <- names(wide)[stringr::str_detect(names(wide), "_20\\d{2}$")]
  if (length(year_cols) == 0L) stop("No year-suffixed Orbis columns were found.", call. = FALSE)
  before <- wide
  wide <- dplyr::mutate(wide, dplyr::across(dplyr::all_of(year_cols), clean_numeric))
  log <- append_filter_log(log, "coerce_year_columns_numeric", before, wide, "Converted year-suffixed fields to numeric.")
  panel <- wide |>
    tidyr::pivot_longer(
      cols = dplyr::all_of(year_cols), names_to = c("variable", "year"),
      names_pattern = "^(.*)_(20\\d{2})$", values_to = "value"
    ) |>
    dplyr::mutate(year = as.integer(.data$year)) |>
    tidyr::pivot_wider(names_from = "variable", values_from = "value") |>
    dplyr::relocate("bv_d_id_number", "year")
  id_like <- c("bv_d_id_number", "year", "company_name_latin_alphabet", "country_iso_code",
               "nace_rev_2_core_code_4_digits", "consolidation_code")
  measure_cols <- setdiff(names(panel), id_like)
  before <- panel
  panel <- dplyr::mutate(panel, dplyr::across(dplyr::all_of(measure_cols), clean_numeric))
  log <- append_filter_log(log, "coerce_measure_columns_numeric", before, panel, "Coerced non-ID fields to numeric where possible.")
  assert_required_columns(panel, c("bv_d_id_number", "country_iso_code", "year", "total_assets_th_usd"), "Orbis panel")
  assert_unique_firm_year(panel)
  list(data = panel, filter_log = log, counts = audit_counts_snapshot(panel, "panel_imported"),
       missingness = audit_missingness_snapshot(panel, "panel_imported", c("country_iso_code", "year", "total_assets_th_usd")))
}

# `clean_orbis()` applies the documented raw-data contract: numeric parsing,
# valid IDs/years, 2012–2023 support, exclusion of NACE 64–66, plausible signs,
# expense-sign harmonization, and positive assets. It returns data and logs.
clean_orbis <- function(data, cleaning) {
  # These local helpers standardize numeric text and define the approved
  # winsorization mechanism (raw-level winsorization remains disabled below).
  parse_numeric <- function(x) {
    if (is.numeric(x)) return(x)
    y <- stringr::str_trim(as.character(x))
    y[stringr::str_to_lower(y) %in% c("", "n.a.", "n.a", "n/a", "na", "null", "-", "--")] <- NA_character_
    suppressWarnings(as.numeric(stringr::str_replace_all(y, "[,[:space:]]+", "")))
  }
  winsor <- function(x, lower, upper) {
    if (!is.numeric(x) || all(is.na(x))) return(x)
    limits <- stats::quantile(x, c(lower, upper), na.rm = TRUE, type = 7)
    pmin(pmax(x, limits[[1L]]), limits[[2L]])
  }
  nonnegative <- intersect(names(data), c(
    "total_assets_th_usd", "operating_revenue_turnover_th_usd", "number_of_employees",
    "intangible_assets_th_usd", "tangible_fixed_assets_th_usd", "net_property_plant_equipment_th_usd",
    "of_which_cash_and_cash_equivalent_th_usd", "total_liabilities_th_usd", "current_liabilities_th_usd",
    "non_current_liabilities_th_usd", "long_term_debt_th_usd", "loans_short_term_debt_th_usd",
    "total_long_term_interest_bearing_debt_th_usd", "current_portion_of_long_term_debt_th_usd",
    "loans_borrowings_th_usd", "total_shareholders_funds_and_liabilities_th_usd"
  ))
  # Parse monetary columns before applying any row filters.
  log <- new_filter_log()
  before <- data
  numeric_candidates <- names(data)[stringr::str_detect(names(data), "(_th_usd$|_usd$|employees$|^year$)")]
  data <- dplyr::mutate(data, dplyr::across(dplyr::all_of(numeric_candidates), parse_numeric))
  log <- append_filter_log(log, "parse_numeric", before, data, "Converted monetary and core numeric fields to numeric.")
  # Require usable identifiers and the configured raw accounting-year window.
  before <- data
  data <- dplyr::filter(data, !is.na(.data$bv_d_id_number), !is.na(.data$year))
  log <- append_filter_log(log, "drop_missing_id_year", before, data, "Dropped rows with missing firm ID or year.")
  before <- data
  data <- dplyr::filter(data, .data$year >= cleaning$year_window[[1L]], .data$year <= cleaning$year_window[[2L]])
  log <- append_filter_log(log, "restrict_raw_year_window", before, data,
                           sprintf("Kept raw data years %s-%s.", cleaning$year_window[[1L]], cleaning$year_window[[2L]]))
  # Remove financial-sector firms because their balance sheets are not
  # comparable to those of non-financial corporations in this design.
  if (isTRUE(cleaning$remove_financials)) {
    before <- data
    data <- data |>
      dplyr::mutate(nace_2d = stringr::str_sub(as.character(.data$nace_rev_2_core_code_4_digits), 1L, 2L)) |>
      dplyr::filter(!.data$nace_2d %in% as.character(cleaning$excluded_nace_2d))
    log <- append_filter_log(log, "exclude_financial_sector", before, data, "Removed NACE 64-66 firms.")
  }
  # Treat negative balance-sheet levels as invalid rather than economic values.
  before <- data
  data <- dplyr::mutate(data, dplyr::across(dplyr::all_of(nonnegative), ~ dplyr::if_else(!is.na(.x) & .x < 0, NA_real_, .x)))
  log <- append_filter_log(log, "plausibility_non_negative", before, data, "Set implausible negative level variables to NA.")
  # Preserve both accounting-sign and positive-magnitude expense versions.
  if (isTRUE(cleaning$harmonize_expense_signs)) {
    before <- data
    data <- data |>
      dplyr::mutate(
        financial_expenses_pos = dplyr::if_else(is.na(.data$financial_expenses_th_usd), NA_real_, abs(.data$financial_expenses_th_usd)),
        income_tax_expense_pos = dplyr::if_else(is.na(.data$income_tax_expense_th_usd), NA_real_, abs(.data$income_tax_expense_th_usd)),
        financial_expenses_th_usd = dplyr::if_else(is.na(.data$financial_expenses_th_usd), NA_real_, -abs(.data$financial_expenses_th_usd)),
        income_tax_expense_th_usd = dplyr::if_else(is.na(.data$income_tax_expense_th_usd), NA_real_, -abs(.data$income_tax_expense_th_usd))
      )
    log <- append_filter_log(log, "harmonize_expense_signs", before, data, "Standardized expense signs and created positive-magnitude copies.")
  }
  if (isTRUE(cleaning$enforce_positive_assets)) {
    before <- data
    data <- dplyr::filter(data, !is.na(.data$total_assets_th_usd), .data$total_assets_th_usd > 0)
    log <- append_filter_log(log, "enforce_positive_assets", before, data, "Dropped non-positive total assets.")
  }
  if (isTRUE(cleaning$raw_winsorize)) {
    stop("The approved thesis cleaning contract has raw-level winsorization disabled.", call. = FALSE)
  }
  assert_unique_firm_year(data)
  list(data = data, filter_log = log, counts = audit_counts_snapshot(data, "panel_cleaned"),
       missingness = audit_missingness_snapshot(data, "panel_cleaned"))
}

# `read_policy_input()` reads and standardizes the country-year policy panel.
# If the optional file is empty, it returns only fallback country-year keys so
# the later merge remains explicit rather than inventing policy information.
read_policy_input <- function(path, fallback_country_year = NULL) {
  if (!file.exists(path) || file.info(path)$size == 0L) {
    if (is.null(fallback_country_year)) return(tibble::tibble(country_iso_code = character(), year = integer()))
    return(dplyr::distinct(fallback_country_year, .data$country_iso_code, .data$year))
  }
  data <- readr::read_csv(path, show_col_types = FALSE, name_repair = "minimal") |> janitor::clean_names()
  if (!"country_iso_code" %in% names(data)) {
    alias <- intersect(c("country", "iso2", "country_code"), names(data))
    if (length(alias) > 0L) data <- dplyr::rename(data, country_iso_code = dplyr::all_of(alias[[1L]]))
  }
  assert_required_columns(data, c("country_iso_code", "year"), "policy input")
  dplyr::mutate(data, country_iso_code = toupper(as.character(.data$country_iso_code)), year = as.integer(.data$year))
}

# `read_macro_input()` reads the required macro panel, standardizes country and
# year keys, and stops if the file or merge keys are unavailable.
read_macro_input <- function(path) {
  if (!file.exists(path) || file.info(path)$size == 0L) stop("Macro input is missing or empty.", call. = FALSE)
  data <- readr::read_csv(path, show_col_types = FALSE, name_repair = "minimal") |> janitor::clean_names()
  assert_required_columns(data, c("country_iso_code", "year"), "macro input")
  dplyr::mutate(data, country_iso_code = toupper(as.character(.data$country_iso_code)), year = as.integer(.data$year))
}

# `merge_policy_macro()` left-joins policy and macro variables to every
# firm-year row, normalizes yes/no policy fields, and exposes consistently named
# tax, governance, treatment, and post-period variables.
merge_policy_macro <- function(data, policy, macro) {
  # Convert textual policy encodings into numeric indicators before merging.
  parse_yes_no <- function(x) {
    value <- stringr::str_to_lower(stringr::str_trim(as.character(x)))
    dplyr::case_when(value %in% c("yes", "y", "true", "1") ~ 1,
                     value %in% c("no", "n", "false", "0", "x") ~ 0, TRUE ~ NA_real_)
  }
  parse_carry <- function(x) {
    value <- stringr::str_to_lower(stringr::str_trim(as.character(x)))
    dplyr::case_when(value %in% c("", "na", "n/a") ~ NA_real_,
                     value %in% c("no", "n", "false", "0", "x") ~ 0, TRUE ~ 1)
  }
  if (!"inflation" %in% names(macro) && "inflation_hicp" %in% names(macro)) macro$inflation <- macro$inflation_hicp
  for (column in c("rule_of_law", "regulatory_quality", "control_of_corruption",
                   "rule_of_law_score", "regulatory_quality_score", "control_of_corruption_score")) {
    if (!column %in% names(macro)) macro[[column]] <- NA_real_
  }
  if (nrow(policy) == 0L) policy <- dplyr::distinct(data, .data$country_iso_code, .data$year)
  policy <- policy |>
    dplyr::mutate(
      loss_offset = if ("carry_forward_interest" %in% names(policy)) parse_carry(.data$carry_forward_interest) else NA_real_,
      anti_tax_avoidance = if ("ebitda_rule" %in% names(policy)) parse_yes_no(.data$ebitda_rule) else NA_real_,
      group_taxation = if ("escape_group" %in% names(policy)) parse_yes_no(.data$escape_group) else NA_real_,
      equity_allowance = if ("escape_standalone" %in% names(policy)) parse_yes_no(.data$escape_standalone) else NA_real_,
      treat_policy = if ("treat" %in% names(policy)) as.integer(.data$treat) else NA_integer_,
      post_policy = if ("post" %in% names(policy)) as.integer(.data$post) else NA_integer_
    )
  # Both joins are many-firm-to-one-country-year joins; no firm rows are dropped.
  merged <- data |>
    dplyr::left_join(policy, by = c("country_iso_code", "year")) |>
    dplyr::left_join(macro, by = c("country_iso_code", "year"))
  existing_treat <- if ("treat" %in% names(merged)) suppressWarnings(as.integer(merged$treat)) else rep(NA_integer_, nrow(merged))
  existing_post <- if ("post" %in% names(merged)) suppressWarnings(as.integer(merged$post)) else rep(NA_integer_, nrow(merged))
  merged |>
    dplyr::mutate(
      corp_tax_rate = .data$cit_rate,
      rule_of_law_pct = dplyr::coalesce(.data$rule_of_law_score, .data$rule_of_law),
      regulatory_quality_pct = dplyr::coalesce(.data$regulatory_quality_score, .data$regulatory_quality),
      corruption_pct = dplyr::coalesce(.data$control_of_corruption_score, .data$control_of_corruption),
      loss_offset_proxy = .data$loss_offset, anti_tax_avoidance_proxy = .data$anti_tax_avoidance,
      group_taxation_proxy = .data$group_taxation, equity_allowance_proxy = .data$equity_allowance,
      treat = dplyr::coalesce(.data$treat_policy, existing_treat),
      post = dplyr::coalesce(.data$post_policy, existing_post)
    )
}

# `construct_variables()` creates the thesis outcomes and controls from the
# cleaned accounting items. It constructs debt components, cash/assets,
# profitability and growth controls, firm lags, forward earnings-volatility
# measures, and winsorized analysis variables; it returns data and diagnostics.
construct_variables <- function(data) {
  # Local helpers create safe logarithms, cap analysis variables at their 1st
  # and 99th percentiles, and calculate complete forward volatility windows.
  log_positive <- function(x) { out <- rep(NA_real_, length(x)); ok <- !is.na(x) & x > 0; out[ok] <- log(x[ok]); out }
  winsor <- function(x) {
    if (!is.numeric(x) || all(is.na(x))) return(x)
    limits <- stats::quantile(x, c(.01, .99), na.rm = TRUE, type = 7)
    if (anyNA(limits)) x else pmin(pmax(x, limits[[1L]]), limits[[2L]])
  }
  forward_sd <- function(x, year, window = 3L) {
    out <- rep(NA_real_, length(x))
    for (i in seq_along(x)) {
      idx <- match(year[[i]] + seq.int(0L, window - 1L), year)
      if (!anyNA(idx) && all(!is.na(x[idx]))) out[[i]] <- stats::sd(x[idx])
    }
    out
  }
  source_col <- function(name) {
    winsorized <- paste0(name, "_w")
    if (winsorized %in% names(data)) data[[winsorized]] else data[[name]]
  }
  # Prefer an approved pre-existing `_w` source when available; otherwise use
  # the corresponding cleaned raw accounting item.
  data$total_assets_src <- source_col("total_assets_th_usd")
  data$total_liabilities_src <- source_col("total_liabilities_th_usd")
  data$turnover_src <- source_col("operating_revenue_turnover_th_usd")
  data$cash_src <- source_col("of_which_cash_and_cash_equivalent_th_usd")
  data$ebit_src <- source_col("earnings_before_interest_tax_ebit_th_usd")
  data$ebitda_src <- source_col("ebitda_th_usd")
  data$tangibility_src <- source_col("tangible_fixed_assets_th_usd")
  data$employees_src <- source_col("number_of_employees")
  data$lt_interest_bearing_src <- source_col("total_long_term_interest_bearing_debt_th_usd")
  data$st_debt_src <- source_col("loans_short_term_debt_th_usd")
  data$lt_debt_src <- source_col("long_term_debt_th_usd")
  data$current_portion_lt_src <- source_col("current_portion_of_long_term_debt_th_usd")
  data$total_equity_src <- source_col("total_shareholders_equity_th_usd")
  data$shareholders_funds_src <- source_col("shareholders_funds_th_usd")
  data$net_profit_src <- source_col("net_profit_th_usd")
  analysis_vars <- c(
    "dta", "dta_paper", "leverage_ratio", "cash_assets", "profitability", "roa_ebitda", "tangibility",
    "growth", "ln_growth", "growth_options", "log_assets", "employees_log", "sales_log", "log_sales",
    "sales_log_lag", "log_sales_lag", "lag_log_sales", "growth_lag", "lag_growth", "profitability_sales",
    "profitability_sales_lag", "lag_profitability_sales", "cash_flow_rate_proxy", "earnvol", "unadj_earnvol",
    "ln_equity", "ln_current_debt", "ln_longterm_debt", "volatility", "log_dta", "log_profitability", "log_earnvol"
  )
  # First construct contemporaneous ratios and the leave-one-out country-
  # industry-year earnings benchmark used to adjust operational performance.
  out <- data |>
    dplyr::arrange(.data$bv_d_id_number, .data$year) |>
    dplyr::mutate(
      nace_2d = stringr::str_sub(as.character(.data$nace_rev_2_core_code_4_digits), 1L, 2L),
      ebit_assets_raw = .data$ebit_src / .data$total_assets_src,
      roa_ebitda_raw = .data$ebitda_src / .data$total_assets_src,
      sales_log = log_positive(.data$turnover_src / 1000), log_sales = .data$sales_log,
      depreciation_proxy = dplyr::if_else(!is.na(.data$ebitda_src) & !is.na(.data$ebit_src) & .data$ebitda_src >= .data$ebit_src,
                                           .data$ebitda_src - .data$ebit_src, NA_real_),
      cash_flow_proxy_num = .data$net_profit_src + .data$depreciation_proxy,
      profitability_sales = dplyr::if_else(.data$turnover_src > 0, .data$ebit_src / .data$turnover_src, NA_real_)
    ) |>
    dplyr::group_by(.data$country_iso_code, .data$nace_2d, .data$year) |>
    dplyr::mutate(
      earnvol_benchmark_n = sum(!is.na(.data$ebit_assets_raw)),
      earnvol_benchmark_sum = sum(.data$ebit_assets_raw, na.rm = TRUE),
      earnvol_benchmark_mean = dplyr::case_when(
        is.na(.data$ebit_assets_raw) ~ NA_real_,
        .data$earnvol_benchmark_n >= 2L ~ (.data$earnvol_benchmark_sum - .data$ebit_assets_raw) / (.data$earnvol_benchmark_n - 1L),
        .data$earnvol_benchmark_n == 1L ~ .data$ebit_assets_raw,
        TRUE ~ NA_real_
      ),
      earnvol_benchmark_type = dplyr::case_when(
        is.na(.data$ebit_assets_raw) ~ NA_character_,
        .data$earnvol_benchmark_n >= 2L ~ "leave_one_out",
        .data$earnvol_benchmark_n == 1L ~ "singleton_including_firm",
        TRUE ~ NA_character_
      ),
      ebit_assets_adj = .data$ebit_assets_raw - .data$earnvol_benchmark_mean
    ) |>
    dplyr::ungroup() |>
    dplyr::group_by(.data$country_iso_code, .data$year) |>
    dplyr::mutate(
      roa_country_year_mean = mean(.data$roa_ebitda_raw, na.rm = TRUE),
      roa_country_year_mean = dplyr::if_else(is.nan(.data$roa_country_year_mean), NA_real_, .data$roa_country_year_mean),
      roa_country_year_adj = .data$roa_ebitda_raw - .data$roa_country_year_mean
    ) |>
    dplyr::ungroup() |>
    # Within each firm, construct debt components, ratios, consecutive-year
    # lags, growth, cash-flow capacity, and three-year forward volatility.
    dplyr::group_by(.data$bv_d_id_number) |>
    dplyr::mutate(
      debt_source = dplyr::if_else(!is.na(.data$lt_debt_src) | !is.na(.data$st_debt_src), "lt_st_broad", NA_character_),
      debt_paper_num = dplyr::if_else(.data$debt_source == "lt_st_broad",
                                      dplyr::coalesce(.data$lt_debt_src, 0) + dplyr::coalesce(.data$st_debt_src, 0), NA_real_),
      current_debt_num = dplyr::coalesce(.data$st_debt_src, .data$current_portion_lt_src),
      longterm_debt_num = dplyr::coalesce(.data$lt_interest_bearing_src, .data$lt_debt_src),
      equity_num = dplyr::coalesce(.data$total_equity_src, .data$shareholders_funds_src),
      dta = .data$debt_paper_num / .data$total_assets_src, dta_paper = .data$dta,
      leverage_ratio = .data$total_liabilities_src / .data$total_assets_src,
      cash_assets = .data$cash_src / .data$total_assets_src,
      profitability = .data$ebit_assets_raw, roa_ebitda = .data$ebitda_src / .data$total_assets_src,
      tangibility = .data$tangibility_src / .data$total_assets_src,
      sales_log_lag = dplyr::if_else(.data$year - dplyr::lag(.data$year) == 1L, dplyr::lag(.data$sales_log), NA_real_),
      log_sales_lag = .data$sales_log_lag, lag_log_sales = .data$sales_log_lag,
      growth = .data$sales_log - .data$sales_log_lag, ln_growth = .data$growth,
      growth_lag = dplyr::if_else(.data$year - dplyr::lag(.data$year) == 1L, dplyr::lag(.data$growth), NA_real_),
      lag_growth = .data$growth_lag,
      profitability_sales_lag = dplyr::if_else(.data$year - dplyr::lag(.data$year) == 1L,
                                               dplyr::lag(.data$profitability_sales), NA_real_),
      lag_profitability_sales = .data$profitability_sales_lag,
      tangible_fixed_assets_lag = dplyr::if_else(.data$year - dplyr::lag(.data$year) == 1L,
                                                 dplyr::lag(.data$tangibility_src), NA_real_),
      cash_flow_rate_proxy = dplyr::if_else(.data$tangible_fixed_assets_lag > 0,
                                            .data$cash_flow_proxy_num / .data$tangible_fixed_assets_lag, NA_real_),
      log_assets = log_positive(.data$total_assets_src / 1000), employees_log = log1p(.data$employees_src),
      earnvol = 100 * forward_sd(.data$ebit_assets_adj, .data$year),
      unadj_earnvol = 100 * forward_sd(.data$ebit_assets_raw, .data$year),
      volatility = 100 * forward_sd(.data$roa_country_year_adj, .data$year)
    ) |>
    dplyr::ungroup() |>
    # Country-industry-year average growth proxies for growth opportunities.
    dplyr::group_by(.data$country_iso_code, .data$nace_2d, .data$year) |>
    dplyr::mutate(growth_options = mean(.data$growth, na.rm = TRUE)) |>
    dplyr::ungroup() |>
    # Finish log components, winsorize only the registered analysis variables,
    # and replace infinite transformations with missing values.
    dplyr::mutate(
      growth_options = dplyr::if_else(is.nan(.data$growth_options), NA_real_, .data$growth_options),
      ln_equity = log_positive(.data$equity_num / 1000),
      ln_current_debt = log_positive(.data$current_debt_num / 1000),
      ln_longterm_debt = log_positive(.data$longterm_debt_num / 1000),
      log_dta = log_positive(.data$dta), log_profitability = log_positive(.data$profitability),
      log_earnvol = log_positive(.data$earnvol),
      dplyr::across(dplyr::all_of(analysis_vars), winsor),
      dplyr::across(dplyr::all_of(analysis_vars), ~ dplyr::if_else(is.infinite(.x), NA_real_, .x))
    )
  assert_unique_firm_year(out)
  list(
    data = out,
    counts = audit_counts_snapshot(out, "panel_variables"),
    missingness = audit_missingness_snapshot(out, "panel_variables", c(analysis_vars, "debt_source")),
    debt_source_summary = out |> dplyr::filter(!is.na(.data$debt_source)) |> dplyr::count(.data$debt_source, name = "observations") |>
      dplyr::mutate(share = .data$observations / sum(.data$observations)),
    risk_window_summary = out |> dplyr::group_by(.data$year) |>
      dplyr::summarise(observations = dplyr::n(), earnvol_available = sum(!is.na(.data$earnvol)),
                       unadj_earnvol_available = sum(!is.na(.data$unadj_earnvol)),
                       leave_one_out_cells = sum(.data$earnvol_benchmark_type == "leave_one_out", na.rm = TRUE),
                       singleton_cells = sum(.data$earnvol_benchmark_type == "singleton_including_firm", na.rm = TRUE), .groups = "drop")
  )
}

# `apply_sample_definition()` enforces an outcome's year range, any required
# complete cases, and minimum firm-level pre/post observations. It returns the
# filtered sample together with a transparent attrition log.
apply_sample_definition <- function(data, sample) {
  # Apply broad year and complete-case rules first.
  log <- new_filter_log()
  before <- data
  out <- dplyr::filter(data, .data$year >= sample$year_start, .data$year <= sample$year_end)
  log <- append_filter_log(log, "restrict_year_window", before, out,
                           sprintf("Kept years %s-%s.", sample$year_start, sample$year_end))
  complete_vars <- intersect(sample$complete_case_vars, names(out))
  if (length(complete_vars) > 0L) {
    before <- out
    out <- tidyr::drop_na(out, dplyr::all_of(complete_vars))
    log <- append_filter_log(log, "complete_case_filter", before, out,
                             sprintf("Kept rows complete for: %s.", paste(complete_vars, collapse = ", ")))
  }
  # Retain firms that identify a within-firm before/after comparison.
  before <- out
  out <- out |>
    dplyr::group_by(.data$bv_d_id_number) |>
    dplyr::mutate(n_pre = sum(.data$year <= sample$pre_period_end), n_post = sum(.data$year >= sample$post_period_start)) |>
    dplyr::ungroup() |>
    dplyr::filter(.data$n_pre >= sample$min_pre_periods, .data$n_post >= sample$min_post_periods) |>
    dplyr::select(-dplyr::all_of(c("n_pre", "n_post")))
  log <- append_filter_log(log, "require_pre_post_support", before, out,
                           sprintf("Required at least %s pre and %s post observations.", sample$min_pre_periods, sample$min_post_periods))
  list(data = out, filter_log = log)
}

# `classify_countries()` reconciles the configured and policy-file treatment
# assignments, constructs the default 2019 post indicator, removes excluded
# countries, and adds a country-year identifier.
classify_countries <- function(data, country_set) {
  # Build the configured assignment and then let explicit policy-file values
  # take precedence where they are available.
  configured <- dplyr::case_when(
    data$country_iso_code %in% country_set$treatment ~ 1L,
    data$country_iso_code %in% country_set$control ~ 0L,
    TRUE ~ NA_integer_
  )
  policy_treat <- if ("treat" %in% names(data)) suppressWarnings(as.integer(data$treat)) else NA_integer_
  policy_post <- if ("post" %in% names(data)) suppressWarnings(as.integer(data$post)) else NA_integer_
  mismatch <- sum(!is.na(policy_treat) & !is.na(configured) & policy_treat != configured)
  if (mismatch > 0L) warning(sprintf("Policy and configured treatment disagree in %s rows; policy assignment takes precedence.", mismatch), call. = FALSE)
  data |>
    dplyr::mutate(
      treat = dplyr::coalesce(policy_treat, configured),
      post = dplyr::coalesce(policy_post, as.integer(.data$year >= 2019L)),
      country_year = interaction(.data$country_iso_code, .data$year, drop = TRUE)
    ) |>
    dplyr::filter(!is.na(.data$treat), .data$country_iso_code %in% c(country_set$control, country_set$treatment))
}

# `read_covid_annual_inputs()` reads the normalized deaths and stringency
# panels, verifies unique and complete 2012–2023 country-year support, and
# returns both panels in a named list.
read_covid_annual_inputs <- function(deaths_path, stringency_path, country_mapping) {
  deaths <- readr::read_csv(deaths_path, show_col_types = FALSE)
  stringency <- readr::read_csv(stringency_path, show_col_types = FALSE)
  assert_required_columns(deaths, c("country_iso_code", "year", "annual_deaths_per_million", "log_deaths_per_million"), "COVID deaths panel")
  assert_required_columns(stringency, c("country_iso_code", "year", "annual_stringency_index", "stringency_index_10"), "Oxford stringency panel")
  expected <- tidyr::expand_grid(country_iso_code = names(country_mapping), year = 2012:2023)
  for (item in list(deaths = deaths, stringency = stringency)) {
    keys <- dplyr::distinct(item, .data$country_iso_code, .data$year)
    if (nrow(dplyr::anti_join(expected, keys, by = c("country_iso_code", "year"))) > 0L || anyDuplicated(keys)) {
      stop("A frozen COVID annual panel has incomplete or duplicate country-year support.", call. = FALSE)
    }
  }
  list(deaths = deaths, stringency = stringency)
}

# `build_covid_risk_windows()` converts annual pandemic measures into three-year
# forward exposures aligned with the forward earnings-volatility outcome. It
# returns only country-years with a complete three-year window.
build_covid_risk_windows <- function(deaths, stringency) {
  # Expand each base year to t, t+1, and t+2 before joining annual exposures.
  requested <- tidyr::expand_grid(country_iso_code = sort(unique(deaths$country_iso_code)), year = 2012:2023, offset = 0:2) |>
    dplyr::mutate(exposure_year = .data$year + .data$offset) |>
    dplyr::left_join(dplyr::select(deaths, "country_iso_code", exposure_year = "year",
                                   "annual_deaths_per_million"), by = c("country_iso_code", "exposure_year")) |>
    dplyr::left_join(dplyr::select(stringency, "country_iso_code", exposure_year = "year",
                                   "annual_stringency_index", "source_observed_days", "calendar_days"),
                     by = c("country_iso_code", "exposure_year")) |>
    dplyr::filter(.data$exposure_year <= 2023L)
  # Aggregate deaths as a total and stringency as a calendar-day weighted mean.
  requested |>
    dplyr::group_by(.data$country_iso_code, .data$year) |>
    dplyr::summarise(
      window_years = dplyr::n(),
      forward_deaths_per_million = sum(.data$annual_deaths_per_million, na.rm = TRUE),
      log_forward_deaths_per_million = log1p(.data$forward_deaths_per_million),
      weighted_stringency_sum = sum(.data$annual_stringency_index * .data$calendar_days, na.rm = TRUE),
      window_calendar_days = sum(.data$calendar_days, na.rm = TRUE),
      forward_stringency_index = .data$weighted_stringency_sum / .data$window_calendar_days,
      forward_stringency_index_10 = .data$forward_stringency_index / 10,
      .groups = "drop"
    ) |>
    dplyr::filter(.data$window_years == 3L)
}

# `sha256_file()` returns a lowercase SHA-256 checksum using the digest package;
# missing files return `NA`.
sha256_file <- function(path) {
  if (!file.exists(path)) return(NA_character_)
  digest::digest(file = path, algo = "sha256", serialize = FALSE)
}
