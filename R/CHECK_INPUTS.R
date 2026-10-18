# Base-R input availability check. No package installation or estimation.

check_analysis_inputs <- function(analysis_root) {
  relative_paths <- c(
    "data/raw/orbis/Export 11_02_2026 11_19.xlsx",
    "data/raw/orbis/Export 11_02_2026 11_19_part_2.xlsx",
    "data/raw/policy/policy_panel.csv",
    "data/raw/policy/atad_classification.csv",
    "data/raw/macro/macro_panel.csv",
    "data/raw/covid/covid_country_year.csv",
    "data/raw/covid/owid-covid-data.csv",
    "data/raw/covid/covid_source_record.csv",
    "data/raw/covid/oxford_stringency_country_year.csv",
    "data/raw/covid/OxCGRT_timeseries_StringencyIndex_v1.csv",
    "data/raw/covid/oxford_stringency_source_record.csv"
  )
  available <- file.exists(file.path(analysis_root, relative_paths))
  cat(paste0(ifelse(available, "[OK]      ", "[MISSING] "), relative_paths), sep = "\n")
  if (any(!available)) {
    cat("\nExternal inputs are required. See DATA_AVAILABILITY.md in the repository root.\n")
    return(FALSE)
  }
  cat("\nAll required input files are present. Source hashes are checked by the analysis.\n")
  TRUE
}

if (sys.nframe() == 0L) {
  script_argument <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
  analysis_root <- if (length(script_argument)) {
    dirname(normalizePath(sub("^--file=", "", script_argument[[1L]]), mustWork = TRUE))
  } else {
    getwd()
  }
  available <- check_analysis_inputs(analysis_root)
  quit(save = "no", status = if (available) 0L else 1L)
}
