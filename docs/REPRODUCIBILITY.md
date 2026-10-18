# Reproducing the empirical analysis

## Read without running

The thesis PDF and submitted tables and figures can be read without an R installation. Submitted outputs are preserved at `R/results/submitted/`. No TeX compilation is required to read the thesis.

## Prepare inputs

Use R 4.5.1 as the reference environment. RStudio is optional but provides a convenient project entry point.

Supply the two licensed Orbis workbooks and the two frozen pandemic snapshots described in [DATA_AVAILABILITY.md](../DATA_AVAILABILITY.md). From the repository root, run:

```sh
Rscript --vanilla R/CHECK_INPUTS.R
```

This check uses base R, reports all required inputs, and exits with status 1 when files are missing. It does not install dependencies or run models. The analysis entry point runs the same availability check before initializing its package library.

## Restore the reference packages

Open `R/thesis_analysis.Rproj`, or start R with `R/` as the working directory. Then run:

```r
source("renv/activate.R")
renv::restore(prompt = FALSE)
```

The activation script may first download `renv`; restoration requires network access and may require platform-specific build tools if matching package binaries are unavailable. `renv.lock` records the reference R and package versions. Restore it explicitly for a version-pinned run: the existing `RUN_ALL.R` fallback installs missing packages by name and does not itself guarantee those exact versions.

## Run

From the R project:

```r
source("RUN_ALL.R")
```

Alternatively, from the repository root:

```sh
Rscript --vanilla R/RUN_ALL.R
```

The entry point locates the analysis root, uses a private project package library, starts a clean worker session, loads helper functions, and runs stages 00–08 in order. It checks the frozen COVID source hashes during the robustness stage. It does not download the research inputs automatically.

## Inspect outputs

- `R/results/generated/tables/`: newly generated LaTeX table fragments.
- `R/results/generated/figures/`: newly generated PDF figures.
- `R/results/generated/run.log`: completion status, timing, and R session information.
- `R/results/generated/models/`: optional machine-readable audit tables when `EXPORT_MODEL_CSVS <- TRUE` is set in `RUN_ALL.R`.

Fresh outputs are ignored by Git. The original submitted results are separate artifacts; this portfolio does not claim that the fresh exporter reproduces their final presentation byte for byte. Compare estimates, sample sizes, and diagnostics against the submitted tables and thesis.

## Checks performed when preparing this portfolio

The packaging review covers source-file inventory and hashes, text extraction from the thesis and literature, input schemas and frozen-source hashes, R syntax parsing, local documentation links, and the input-availability check. See the [validation record](VALIDATION.md) for the results. The packaging process does not rerun the empirical models, install the reference package environment, compile TeX files, or visually inspect images or figures. Original submitted results are reported as submitted results, not as independently replicated estimates.
