# Portfolio validation record

Prepared on 2026-10-18.

| Check | Result |
| --- | --- |
| Original archive inventory | 182 files recorded with sizes and SHA-256 hashes. |
| Original archive preservation | All 182 source-file hashes unchanged after packaging. |
| Unmodified copied artifacts | 63 files verified against their original hashes. |
| Portfolio PDF | 83 pages: new cover plus 82 scientific pages with unchanged page content streams. |
| PDF structure and personal details | No extra hidden page objects or attachments; original cover's address, student number, and university email absent from extracted text. |
| Literature | 55 BibTeX records combined; no duplicate citation keys. Literature PDFs were processed for extractable text only. |
| Country-year inputs | Unique country-year keys in the macro, policy, mortality, and stringency panels. |
| Frozen COVID sources | All four raw/normalized hashes agree with their source records. |
| R syntax | All 18 R scripts parse successfully with R 4.5.1. |
| Input preflight: original archive | All 11 required input files detected. |
| Input preflight: curated portfolio | Exactly four documented external files reported missing; nonzero exit as intended. |
| Analysis entry point | Stops at the preflight before package setup when external inputs are absent. |
| Git exclusions | Orbis workbooks, raw pandemic snapshots, generated results, and private package libraries ignored. |
| Documentation | Local Markdown links resolve; citation YAML parses. |

No full empirical rerun or independent numerical replication was performed. No packages were installed, no TeX files were compiled, and no images or figures were visually inspected. Image-only literature content was not reviewed. The submitted aggregate results are preserved as evidence of the submitted analysis.
