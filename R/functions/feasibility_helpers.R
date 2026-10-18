# Direct investment-flow availability audit. Estimation branches are
# intentionally absent because the frozen Orbis export has no usable flow.

# `investment_direct_patterns()` returns conservative name patterns for true
# capital-expenditure, additions, purchase, or acquisition flow variables.
investment_direct_patterns <- function() {
  c("capex", "capital.*expenditure", "capital.*expenditures",
    "additions?.*(fixed|tangible|property|plant|equipment|ppe)",
    "(fixed|tangible|property|plant|equipment|ppe).*additions?",
    "purchases?.*(fixed|tangible|property|plant|equipment|ppe)",
    "acquisitions?.*(fixed|tangible|property|plant|equipment|ppe)")
}

# `market_risk_availability_audit()` scans raw headers for daily prices,
# returns, beta, or market-volatility fields. It returns the matched headers
# and an explicit feasible/infeasible decision; it estimates no model.
market_risk_availability_audit <- function(raw_files) {
  headers <- read_raw_orbis_headers(raw_files)
  patterns <- c(
    "daily.*(stock|share|market).*(return|price)",
    "(stock|share|market).*(return|price).*daily",
    "market.*beta", "equity.*beta", "stock.*volatility", "share.*volatility"
  )
  candidates <- headers |>
    dplyr::filter(matches_any_pattern(.data$base_column, patterns)) |>
    dplyr::arrange(.data$raw_file, .data$sheet, .data$base_column)
  feasible <- nrow(candidates) > 0L
  list(
    raw_headers = headers,
    candidates = candidates,
    feasibility_summary = tibble::tibble(
      feasible = feasible,
      status = if (feasible) "candidate_market_series_header_found" else "infeasible_no_daily_market_series_header",
      matched_header_count = nrow(candidates),
      matched_headers = paste(unique(candidates$base_column), collapse = ";"),
      decision = if (feasible) {
        "Validate frequency, coverage, and construction before estimating market-based risk outcomes."
      } else {
        "Do not estimate market-based risk outcomes from the staged Orbis exports."
      }
    )
  )
}

# `investment_stock_patterns()` identifies fixed-asset balance-sheet stocks,
# which are documented but deliberately not treated as investment flows.
investment_stock_patterns <- function() {
  c("tangible_fixed_assets", "net_property_plant_equipment", "property_plant_equipment", "ppe")
}

# `matches_any_pattern()` performs case-insensitive matching of each name in
# `x` against any regular expression in `patterns`.
matches_any_pattern <- function(x, patterns) {
  purrr::map_lgl(x, ~ any(stringr::str_detect(.x, stringr::regex(paste(patterns, collapse = "|"), ignore_case = TRUE))))
}

# `strip_year_suffix()` maps wide annual names such as `sales_2020` to their
# common base name so one economic field is classified only once.
strip_year_suffix <- function(x) stringr::str_replace(x, "_20\\d{2}$", "")

# `pick_results_sheet_for_headers()` chooses the likely data sheet from a raw
# workbook. It prefers recognizable result/data names and otherwise uses the
# second sheet, matching the structure of the staged Orbis exports.
pick_results_sheet_for_headers <- function(path) {
  sheets <- readxl::excel_sheets(path)
  if (length(sheets) == 1L) return(sheets[[1L]])
  likely <- sheets[stringr::str_detect(stringr::str_to_lower(sheets), "result|results|data|export|table|sheet2")]
  if (length(likely) > 0L) likely[[1L]] else sheets[[2L]]
}

# `read_raw_orbis_headers()` reads column names only, cleans them, and removes
# annual suffixes. It returns one row per raw column with file and sheet origin.
read_raw_orbis_headers <- function(raw_files) {
  purrr::map_dfr(raw_files[file.exists(raw_files)], function(path) {
    extension <- tolower(tools::file_ext(path))
    if (extension %in% c("xlsx", "xls")) {
      sheet <- pick_results_sheet_for_headers(path)
      raw_names <- names(suppressMessages(readxl::read_excel(path, sheet = sheet, n_max = 0, .name_repair = "unique")))
    } else {
      sheet <- NA_character_
      raw_names <- names(suppressMessages(readr::read_csv(path, n_max = 0, show_col_types = FALSE)))
    }
    clean <- janitor::make_clean_names(raw_names)
    tibble::tibble(raw_file = basename(path), sheet = sheet, raw_column = raw_names,
                   clean_column = clean, base_column = strip_year_suffix(clean))
  })
}

# `candidate_column_table()` classifies supplied names as direct investment
# flows or stock-only measures and returns only names relevant to the audit.
candidate_column_table <- function(columns, source) {
  base <- strip_year_suffix(columns)
  tibble::tibble(
    source = source, column = columns, base_column = base,
    candidate_type = dplyr::case_when(
      matches_any_pattern(base, investment_direct_patterns()) ~ "direct_capex_or_additions_candidate",
      matches_any_pattern(base, investment_stock_patterns()) ~ "stock_only_not_capex",
      TRUE ~ NA_character_
    )
  ) |>
    dplyr::filter(!is.na(.data$candidate_type)) |>
    dplyr::distinct()
}

# `candidate_value_summary()` measures missingness, signs, and ranges for each
# direct-flow candidate. A typed empty table is returned when none exist.
candidate_value_summary <- function(data, columns) {
  columns <- intersect(columns, names(data))
  if (length(columns) == 0L) {
    return(tibble::tibble(
      variable = character(), non_missing = integer(), missing_share = double(),
      negative_share_nonmissing = double(), positive_share_nonmissing = double(),
      zero_share_nonmissing = double(), min = double(), median = double(), max = double()
    ))
  }
  purrr::map_dfr(columns, function(variable) {
    value <- data[[variable]]
    observed <- sum(!is.na(value)); denominator <- max(observed, 1L)
    tibble::tibble(
      variable = variable, non_missing = observed, missing_share = mean(is.na(value)),
      negative_share_nonmissing = sum(!is.na(value) & value < 0) / denominator,
      positive_share_nonmissing = sum(!is.na(value) & value > 0) / denominator,
      zero_share_nonmissing = sum(!is.na(value) & value == 0) / denominator,
      min = if (observed > 0L) min(value, na.rm = TRUE) else NA_real_,
      median = if (observed > 0L) stats::median(value, na.rm = TRUE) else NA_real_,
      max = if (observed > 0L) max(value, na.rm = TRUE) else NA_real_
    )
  })
}

# `investment_feasibility_audit()` combines raw-header and analysis-data checks.
# It returns candidate inventories, value coverage, year/country support, and a
# decision that prevents stock measures from being mislabelled as investment.
investment_feasibility_audit <- function(data, raw_files) {
  raw_headers <- read_raw_orbis_headers(raw_files)
  raw_candidates <- candidate_column_table(raw_headers$base_column, "raw_orbis_headers") |>
    dplyr::distinct(.data$source, .data$base_column, .data$candidate_type)
  data_candidates <- candidate_column_table(names(data), "analysis_ready_data")
  direct_names <- data_candidates |>
    dplyr::filter(.data$candidate_type == "direct_capex_or_additions_candidate") |>
    dplyr::pull("column") |>
    unique()
  values <- candidate_value_summary(data, direct_names)
  viable <- values |>
    dplyr::filter(.data$non_missing > 0L) |>
    dplyr::mutate(
      priority = dplyr::case_when(
        stringr::str_detect(.data$variable, stringr::regex("capex|capital.*expenditure", ignore_case = TRUE)) ~ 1L,
        stringr::str_detect(.data$variable, stringr::regex("addition", ignore_case = TRUE)) ~ 2L,
        TRUE ~ 3L
      )
    ) |>
    dplyr::arrange(.data$priority, dplyr::desc(.data$non_missing), .data$variable)
  feasible <- nrow(viable) > 0L || ("investment" %in% names(data) && any(!is.na(data$investment)))
  selected <- if (nrow(viable) > 0L) viable$variable[[1L]] else if (feasible) "investment" else NA_character_
  controls <- c("log_sales_lag", "cash_flow_rate_proxy", "profitability_sales_lag", "growth_lag",
                "corp_tax_rate", "gdp_growth", "inflation", "interest_rate",
                "rule_of_law_pct", "regulatory_quality_pct", "corruption_pct")
  availability_names <- unique(c("bv_d_id_number", "country_iso_code", "year", "treat", "post", "post_2019",
                                 "investment", "tangible_fixed_assets_th_usd",
                                 "net_property_plant_equipment_th_usd", "tangible_fixed_assets_lag", controls))
  list(
    raw_headers = raw_headers,
    raw_candidates = raw_candidates,
    data_candidates = data_candidates,
    direct_candidate_summary = values,
    feasibility_summary = tibble::tibble(
      feasible = feasible,
      status = if (feasible) "feasible_direct_capex_or_additions_candidate_available" else
        "infeasible_no_direct_capex_or_fixed_asset_additions_field",
      selected_source = selected,
      direct_candidates_in_analysis_data = paste(direct_names, collapse = ";"),
      direct_candidates_with_values = paste(values$variable[values$non_missing > 0L], collapse = ";"),
      stock_only_candidates_in_analysis_data = paste(data_candidates$column[data_candidates$candidate_type == "stock_only_not_capex"], collapse = ";"),
      direct_candidates_in_raw_headers = paste(raw_candidates$base_column[raw_candidates$candidate_type == "direct_capex_or_additions_candidate"], collapse = ";"),
      stock_only_candidates_in_raw_headers = paste(raw_candidates$base_column[raw_candidates$candidate_type == "stock_only_not_capex"], collapse = ";"),
      decision = if (feasible) "A direct-flow candidate requires substantive validation before estimation." else
        "Do not estimate investment effects with the current export; only stock variables are available."
    ),
    variable_availability = tibble::tibble(variable = availability_names) |>
      dplyr::mutate(
        present = .data$variable %in% names(data),
        non_missing = purrr::map_int(.data$variable, ~ if (.x %in% names(data)) sum(!is.na(data[[.x]])) else 0L),
        missing = nrow(data) - .data$non_missing,
        missing_share = .data$missing / nrow(data)
      ),
    year_support = data |>
      dplyr::group_by(.data$year) |>
      dplyr::summarise(observations = dplyr::n(), firms = dplyr::n_distinct(.data$bv_d_id_number),
                       treatment_observations = sum(.data$treat == 1L), control_observations = sum(.data$treat == 0L),
                       positive_lagged_tangible_assets = sum(!is.na(.data$tangible_fixed_assets_lag) & .data$tangible_fixed_assets_lag > 0),
                       .groups = "drop"),
    country_support = data |>
      dplyr::group_by(.data$country_iso_code, .data$treat) |>
      dplyr::summarise(observations = dplyr::n(), firms = dplyr::n_distinct(.data$bv_d_id_number),
                       pre_observations = sum(.data$year <= 2018L), post_observations = sum(.data$year >= 2019L),
                       positive_lagged_tangible_assets = sum(!is.na(.data$tangible_fixed_assets_lag) & .data$tangible_fixed_assets_lag > 0),
                       .groups = "drop")
  )
}
