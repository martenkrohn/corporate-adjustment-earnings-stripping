# Data availability

The submitted analysis used frozen local inputs. This portfolio preserves the analysis code, country-level inputs, source records, and aggregate submitted results. The original firm-level Orbis exports are excluded.

## Included inputs

| Directory under `R/data/raw/` | Files | Role |
| --- | --- | --- |
| `policy/` | `policy_panel.csv`, `atad_classification.csv` | Country-year policy controls and treatment/regime classification. |
| `macro/` | `macro_panel.csv`, `macro_panel_source_summary.csv` | Macroeconomic, corporate tax, and institutional controls and source summary. |
| `covid/` | `covid_country_year.csv`, `covid_source_record.csv` | Annual pandemic mortality controls and frozen-source provenance. |
| `covid/` | `oxford_stringency_country_year.csv`, `oxford_stringency_source_record.csv` | Annual policy stringency controls and frozen-source provenance. |

These are country-level research inputs. Each normalized COVID panel contains 264 country-year rows covering 22 countries and 2012–2023. The original source records retain URLs, retrieval dates, sizes, and SHA-256 hashes. The macro source summary documents missing coverage rather than claiming a fully observed panel.

## Inputs required for a full rerun

Place the following files at their exact expected paths, relative to the repository root:

| Path | Requirement |
| --- | --- |
| `R/data/raw/orbis/Export 11_02_2026 11_19.xlsx` | First original licensed Orbis workbook. |
| `R/data/raw/orbis/Export 11_02_2026 11_19_part_2.xlsx` | Second original licensed Orbis workbook. |
| `R/data/raw/covid/owid-covid-data.csv` | Frozen OWID source snapshot, 98,391,483 bytes. |
| `R/data/raw/covid/OxCGRT_timeseries_StringencyIndex_v1.csv` | Frozen Oxford source snapshot, 2,527,943 bytes. |

The Orbis workbooks contain the consolidated-account exports used by the thesis. They require authorized access to the data provider; a newly downloaded sample is not guaranteed to reproduce the original estimates. The schema, consolidation codes, accounting units, and selection rules are described in thesis Chapter 4 and implemented in `R/functions/data_helpers.R`.

The pandemic snapshots are omitted to keep the portfolio compact. Their annual analysis panels are included, but the unchanged source-validation code also requires the raw snapshots and checks their SHA-256 hashes. Downloading a current OWID file from the recorded URL may yield different bytes. Exact reproduction requires the matching archived snapshot; the checksum should not be bypassed.

The Oxford source record identifies the upstream commit `e7f66ee39654293b5c068efd2f195bd591dc27f6`. Source URLs and hashes are available in the two included records:

- [OWID source record](R/data/raw/covid/covid_source_record.csv)
- [Oxford source record](R/data/raw/covid/oxford_stringency_source_record.csv)

All four omitted raw inputs are covered by `.gitignore`. Keep restored local data out of version control.

## Data and source attribution

The thesis attributes firm accounts to Orbis/Bureau van Dijk; GDP, inflation, and governance indicators to the World Bank; corporate income tax rates to the OECD; long-term bond yields to Eurostat; mortality data to Our World in Data; and pandemic stringency to the Oxford COVID-19 Government Response Tracker. Provider names describe the frozen thesis inputs, not a claim about current provider offerings.

Third-party data and literature are not relicensed by this repository. Bibliographic records and the thesis reference list provide attribution. Copies of third-party literature PDFs remain in the local source archive.
