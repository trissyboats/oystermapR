## R CMD check results

0 errors | 0 warnings | 3 notes

### Note 1: Package suggested but not available for checking: 'oce'

`oce` is listed in `Suggests` and is used only in `read_rdi_adcp()`, whose
examples are wrapped in `\dontrun{}`. The package is available on CRAN but is
not available on all check platforms. The dependency is genuinely optional and
the package functions correctly without it.

### Note 2: unable to verify current time

Standard infrastructure note on some check servers; not related to package code.

### Note 3: HTML validation of the manual (local macOS only)

`checking HTML version of manual` reports "<main> is not recognized" and similar
for every generated help page. It comes from the system `tidy` on macOS being
older than the HTML5 that R's help pages use; it is not caused by package
content and does not appear for packages checked on CRAN's Linux machines.

## Test environments

- Local macOS, R version 4.4.2 (2024-10-31), `R CMD check --as-cran` on the built tarball
- win-builder (R-release and R-devel): both completed with no ERRORs and no WARNINGs

## Submission notes

This is version 1.6.0, an update to the version currently on CRAN.

This submission replaces an earlier, unprocessed submission of the same version. The only difference is documentation wording: the weighting is now described as rank-reciprocal weighting rather than "AHP", because no pairwise-comparison matrices or consistency ratio are computed. No code behaviour changed.

**New, and additive.** `predict_oyster()` gains `weighting = c("rank",
"evidence")`; the default is `"rank"`, which is the existing behaviour, so
results from 1.5.0 are reproduced exactly and no existing call changes meaning.
The new arguments are appended after the existing ones, so positional calls are
unaffected. Fifteen functions are added and five S3 `print()` methods:

- evidence weighting: `evidence_coverage()`, `evidence_weights()`,
  `evidence_suitability()`, `reliability_from_replicates()`,
  `reliability_from_perturbation()`, `confidence_score()`,
  `weight_table_md()`, and `layer_reproducibility()`, which measures
  reliability from a survey's overlapping lines;
- lineage tolerance profiles: `as_lineage_distances()`, `lineage_signal()`,
  `fit_lineages()`, `lineage_occurrence()`, `rank_source_populations()`,
  `lineage_fits_as_list()`, `lineage_fits_from_list()`.

There are no new dependencies: the new code uses only `stats`, `utils` and
`cli`, which were already imported.

**Changed behaviour of existing functions.**

- `update_species_tolerances()` gives different results, and this is a
  correction. It fitted a private copy of the scoring curve that differed from
  the one `predict_oyster()` uses (variables were weighted by rank where scoring
  weights by 1/rank, and one shoulder was held fixed where the returned curve
  scales it), and it returned `NA` for every posterior standard deviation and
  credible interval. It now fits the deployed curve and returns real intervals;
  `NEWS.md` gives the details and recommends refitting multi-variable
  updates. New optional arguments `profile` and `prior` are appended and default
  to `NULL`; the returned list gains elements and existing ones keep their names.
- `export_geotiff()` writes seven bands rather than five. The five existing
  bands are unchanged and keep their positions; bands 6 and 7 carry
  per-location confidence and an insufficient-data mask, and are `NA` / `0`
  under the default weighting.
- `export_qml_style()` writes a second `.qml` file alongside the first, styling
  the new band 7. Its return value is unchanged apart from an added attribute.
- `check_exclusions()` (used by `predict_oyster()`) is corrected in two ways.
  `predict_oyster()` no longer stops for two species (`crassostrea_iredalei`,
  `saccostrea_cucullata`) on data without a date column, where an `NA` season
  left `NA` in an exclusion flag; and `exclusion_reason` now names each excluded
  row's own value instead of every flagged row's at once.
- `generate_summary_pdf()` no longer fails in a non-UTF-8 locale: typographic
  characters get plain-ASCII stand-ins there. Output in a UTF-8 locale is
  unchanged.

Reverse dependencies: none on CRAN.
