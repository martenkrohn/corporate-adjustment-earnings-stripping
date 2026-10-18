# One-click entry point for the thesis analysis package.

# Change this to TRUE temporarily when machine-readable model CSVs are needed for an audit.
EXPORT_MODEL_CSVS <- FALSE

# Find the package reliably whether this file is sourced or run with Rscript.
locate_analysis_root <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  script_arg <- grep("^--file=", args, value = TRUE)
  frame_files <- unlist(lapply(sys.frames(), function(frame) {
    if (!is.null(frame$ofile)) as.character(frame$ofile) else character()
  }), use.names = FALSE)
  candidates <- c(
    if (length(script_arg)) dirname(sub("^--file=", "", script_arg[[1L]])) else character(),
    dirname(frame_files), getwd(), file.path(getwd(), "R")
  )
  candidates <- unique(normalizePath(candidates, winslash = "/", mustWork = FALSE))
  hit <- candidates[
    file.exists(file.path(candidates, "ANALYSIS_PACKAGE_MARKER")) &
      file.exists(file.path(candidates, "code", "00_config.R"))
  ]
  if (!length(hit)) stop(
    "Analysis-package root not found. Open the packaged thesis_analysis.Rproj and run RUN_ALL.R with Source; do not paste its lines into a console rooted in the development repository.",
    call. = FALSE
  )
  hit[[1L]]
}

ANALYSIS_ROOT <- locate_analysis_root()
assign("ANALYSIS_ROOT", ANALYSIS_ROOT, envir = .GlobalEnv)
setwd(ANALYSIS_ROOT)

# Portfolio preflight: report external inputs before installing packages.
source(file.path(ANALYSIS_ROOT, "CHECK_INPUTS.R"), local = .GlobalEnv)
if (!check_analysis_inputs(ANALYSIS_ROOT)) {
  stop("Supply the external inputs listed in DATA_AVAILABILITY.md before running the analysis.", call. = FALSE)
}

if (getRversion() < "4.5.1") {
  message("Using R ", getRversion(), " (reference environment: R 4.5.1).")
}

# A clean worker process receives the already prepared project-library path and
# does not need to activate renv again. The interactive parent activates renv
# quietly so recoverable bootstrap diagnostics do not alarm normal users.
worker_library <- Sys.getenv("THESIS_ANALYSIS_PROJECT_LIBRARY", unset = "")
worker_mode <- nzchar(worker_library)
if (worker_mode) {
  project_library <- normalizePath(worker_library, winslash = "/", mustWork = TRUE)
} else {
  activation_error <- NULL
  invisible(capture.output(
    withCallingHandlers(
      tryCatch(
        suppressMessages(source(file.path(ANALYSIS_ROOT, "renv", "activate.R"), local = .GlobalEnv)),
        error = function(error) activation_error <<- error
      ),
      warning = function(condition) invokeRestart("muffleWarning")
    ),
    type = "output"
  ))
  if (!requireNamespace("renv", quietly = TRUE)) {
    detail <- if (is.null(activation_error)) "unknown bootstrap failure" else conditionMessage(activation_error)
    stop("Could not initialize the private R package library: ", detail, call. = FALSE)
  }
  project_library <- renv::paths$library(project = ANALYSIS_ROOT)
}
.libPaths(c(project_library, .Library))

# Install runtime packages that are still absent from the private project
# library. Check the complete lockfile closure, not just packages called
# directly by the analysis: the parent R session may have a dependency (for
# example `cli`) in its user library, but the clean worker intentionally cannot
# see that library.
direct_runtime_packages <- c(
  "dplyr", "purrr", "readr", "readxl", "stringr", "stringi", "tidyr",
  "tibble", "janitor", "yaml", "ggplot2", "fixest", "digest"
)
lockfile <- renv::lockfile_read(file.path(ANALYSIS_ROOT, "renv.lock"))
locked_runtime_packages <- setdiff(names(lockfile$Packages), "renv")
required_packages <- union(direct_runtime_packages, locked_runtime_packages)
installed_in_project <- function() {
  if (!dir.exists(project_library)) return(character())
  rownames(utils::installed.packages(lib.loc = project_library, noCache = TRUE))
}
missing_packages <- setdiff(required_packages, installed_in_project())
installed_runtime_packages <- length(missing_packages) > 0L
install_error <- NULL
if (length(missing_packages)) {
  if (worker_mode) {
    stop("The clean analysis process could not find required project packages: ",
         paste(missing_packages, collapse = ", "), call. = FALSE)
  }
  message("Preparing the private R package library (first run only)...")
  invisible(capture.output(
    withCallingHandlers(
      tryCatch(
        suppressMessages(renv::install(
          packages = missing_packages,
          project = ANALYSIS_ROOT,
          prompt = FALSE,
          dependencies = "strong",
          transactional = FALSE
        )),
        error = function(error) install_error <<- error
      ),
      warning = function(condition) invokeRestart("muffleWarning")
    ),
    type = "output"
  ))
}
missing_packages <- setdiff(required_packages, installed_in_project())
if (length(missing_packages)) {
  detail <- if (is.null(install_error)) "" else paste0(" ", conditionMessage(install_error))
  stop(
    "Missing runtime packages after automatic installation: ",
    paste(missing_packages, collapse = ", "),
    ".", detail,
    call. = FALSE
  )
}

# RStudio can preload namespaces from outside the project. Always run the
# analysis in a clean child process; on the first run this also avoids asking
# the user to restart after package installation.
run_analysis_here <- worker_mode
if (!worker_mode) {
  if (installed_runtime_packages) {
    message("Private package library ready; starting the analysis in a clean R session...")
  } else {
    message("Starting the analysis in a clean R session...")
  }
  Sys.setenv(THESIS_ANALYSIS_PROJECT_LIBRARY = project_library)
  rscript <- file.path(R.home("bin"), if (.Platform$OS.type == "windows") "Rscript.exe" else "Rscript")
  worker_status <- tryCatch(
    system2(rscript, c("--vanilla", shQuote(file.path(ANALYSIS_ROOT, "RUN_ALL.R")))),
    finally = Sys.unsetenv("THESIS_ANALYSIS_PROJECT_LIBRARY")
  )
  if (!identical(worker_status, 0L)) {
    stop("The clean analysis process exited with status ", worker_status, ".", call. = FALSE)
  }
}

if (run_analysis_here) {
# Load reusable helpers before executing the numbered analysis scripts.
function_files <- sort(list.files(file.path(ANALYSIS_ROOT, "functions"), pattern = "[.]R$", full.names = TRUE))
invisible(lapply(function_files, source, local = .GlobalEnv))

# Mirror console output to a run log for a compact repeatability record.
log_path <- file.path(ANALYSIS_ROOT, "results", "generated", "run.log")
dir.create(dirname(log_path), recursive = TRUE, showWarnings = FALSE)
started <- Sys.time()
status <- "FAILED"
run_error <- NULL
log_connection <- file(log_path, open = "wt", encoding = "UTF-8")

sink(log_connection, split = TRUE)
sink(log_connection, type = "message")

cat("Thesis analysis started:", format(started, tz = "UTC"), "UTC\n")
cat("Root:", ANALYSIS_ROOT, "\n")
# Run the analysis in a fixed, readable order and report any failure clearly.
tryCatch({
  for (script in sprintf("%02d_%s.R", 0:8, c(
    "config", "prepare_data", "descriptive_outputs", "leverage_analysis", "risk_analysis",
    "cash_analysis", "robustness", "investment_feasibility", "export_results"
  ))) {
    cat("\n--- Running", script, "---\n")
    source(file.path(ANALYSIS_ROOT, "code", script), local = .GlobalEnv)
  }
  status <- "COMPLETED"
}, error = function(error) {
  cat("\nERROR:", conditionMessage(error), "\n")
  run_error <<- error
}, finally = {
  ended <- Sys.time()
  cat("\nStatus:", status, "\nFinished:", format(ended, tz = "UTC"), "UTC\n")
  cat("Elapsed minutes:", round(as.numeric(difftime(ended, started, units = "mins")), 2), "\n\n")
  print(sessionInfo())
})
sink(type = "message")
sink()
close(log_connection)
if (!is.null(run_error)) stop(run_error)
}
