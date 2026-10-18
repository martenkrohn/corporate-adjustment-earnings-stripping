# Portfolio packaging notes

This repository is a curated copy of the submitted thesis archive. The original archive is preserved separately and unchanged.

## Included

- The scientific thesis text, appendix, and references in a portfolio PDF.
- Original numbered R stages, reusable helpers, project settings, and `renv` files.
- Country-level policy, macroeconomic, mortality, and stringency inputs with source records.
- Original submitted aggregate tables and PDF figures, copied without modification.
- Bibliographic records combined from the original literature folders.

## Deliberate omissions

- Licensed firm-level Orbis exports.
- Two bulky pandemic source snapshots; their annual panels, provenance, and checksums are retained.
- Third-party literature PDFs; bibliographic records remain available.
- The submission photo.
- The original submission cover containing personal contact and examination details, and the two final non-scientific pages. These pages were excluded from the portfolio PDF; a new text-only cover provides the title, author, university, and year. The scientific pages in between are retained.

The original archive inventory, byte sizes, hashes, and inclusion decisions are recorded in [source_manifest.csv](source_manifest.csv). No image or figure was visually reviewed during packaging.

## Code changes

The empirical specifications and helper functions are copied unchanged. `R/RUN_ALL.R` has one packaging addition: it invokes the new base-R `CHECK_INPUTS.R` before dependency initialization, so a fresh clone reports its missing external inputs immediately. The original plain-text usage notes are replaced by documentation appropriate to this curated repository.

## Rights and visibility

This repository is intended to start as private. No open-source license is selected on the author's behalf. Source attribution and third-party terms remain separate from repository visibility. No GitHub Pages site or other public deployment is part of this portfolio.
