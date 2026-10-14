# oystermapR 1.6.0

## Documentation

The weighting is now described accurately as rank-reciprocal (each variable's weight is the inverse of its importance rank for the species). Earlier text called it "AHP-weighted", but no pairwise-comparison matrices or consistency ratio were ever computed. No behaviour changes. The `area_summary()` example's slow second call now runs under `\donttest{}` to keep example time short.

## Evidence weighting

`predict_oyster()` gains `weighting = c("rank", "evidence")`. The default is
unchanged, so every existing call returns exactly what it returned in 1.5.0.

The problem it addresses: the engine renormalises its rank weights over
whichever variables happen to be present, so a survey measuring four of a
species' seventeen scored variables returns a confident number on [0, 1] with
nothing on it to say the rest of the evidence base was never observed. Absence
is indistinguishable from irrelevance. Two further consequences follow, both
measurable — a variable that does not vary still absorbs weight, and a variable
that does not reproduce is weighted identically to one that does.

Under `weighting = "evidence"` each variable's weight becomes
`prior x reliability x spread`, normalised. Reliability multiplies the
biological prior and never replaces it: weighting on measured reliability alone
would promote whatever the instrument happens to measure well, which is
circular. Coverage — the share of the species' total prior weight the survey
observed — is reported separately and never multiplied into the per-cell score,
which would put every location below any class threshold on a low-coverage
survey.

New exported functions:

* `evidence_coverage()` — share of the species' total prior weight observed.
  Weight-based, unlike the count-based `data_completeness` already returned, so
  missing a rank-1 variable costs six times what missing a rank-6 one does.
  `attempted_failed` prices an instrument failure in the same units.
* `reliability_from_replicates()` — reliability from real repeat coverage, with
  a confidence interval and an `established` flag.
* `layer_reproducibility()` — measures reliability for every layer at once from
  a survey's own overlapping lines: bins the survey to a grid, pairs the lines
  crossing each cell, and passes the pairs to `reliability_from_replicates()`.
  Returns a named vector ready for `predict_oyster(reliability = )`, named as
  the scored layer (`depth`, not `depth_m`), with `NA` for layers with too few
  overlaps to test. Needs a line/pass column and at least two lines.
* `reliability_from_perturbation()` — the same statistic where there is no
  repeat coverage, by injecting the measured per-observation error. `derive`
  propagates depth error into quantities computed from depth.
* `evidence_weights()` — the weight derivation as an auditable table, with
  provenance per layer.
* `confidence_score()` — per-location confidence from weighted components.
* `evidence_suitability()` — re-weights existing `score_*` columns and attaches
  coverage. Scoring curves are untouched.
* `weight_table_md()` — weight derivation and coverage ledger as markdown, for
  a methods section.

Plus `print()` methods for `evidence_weights` and `evidence_coverage`.

## Lineage tolerance profiles and inverse lineage mapping

Tolerance profiles for populations or lineages, linked by genetic relatedness,
and maps of where each is more likely to occur. Bring-your-own-data: occurrence
records carrying a lineage identifier, and a pairwise genetic distance matrix.
`lineage_walkthrough.R` runs the whole workflow on simulated data.

* `as_lineage_distances()` — validates a distance matrix (matrix, `dist`, data
  frame or CSV).
* `lineage_signal()` — Mantel test of whether related lineages have more
  similar fitted tolerances. Pooling is refused below six fitted lineages, and
  when the signal is absent lineages are fitted alone.
* `fit_lineages()` — one profile per lineage via the existing Bayesian update.
  Where signal is shown, a Gaussian-process prior decaying with genetic
  distance lets thin lineages borrow from relatives, and lineages with no
  records get a profile from relatedness alone (`source = "borrowed"`).
* `lineage_occurrence()` — per-lineage suitability and a posterior across
  lineages at every location, with an ambiguity measure and an `"Ambiguous"`
  class where the environment does not tell lineages apart.
* `rank_source_populations()` — ranks lineages as sources for each site, with
  an optional discount for genetic distance from the locally native lineage
  and optional posterior draws.
* `lineage_fits_as_list()` / `lineage_fits_from_list()` — reduce a fitted model
  to plain data (profile table, calibration, distances, signal test) that
  survives JSON, and rebuild a scoring-ready model from it. The SaaS stores
  models this way.

`update_species_tolerances()` gains `profile` (fit and cache under
`"<species>::<profile>"`) and `prior` (per-variable prior means and SDs). Both
default to `NULL`, which leaves behaviour unchanged, and the result now also
carries `tolerances`.

## Fixes to `update_species_tolerances()`

The Bayesian update fitted a private copy of the scoring curve that differed
from the one `predict_oyster()` uses, and reported no uncertainty. All of the
following are corrected; results change.

* **It now fits the curve it deploys.** The likelihood calls the same
  `.score_numeric()` as the maps, with shoulders scaled to the candidate optimum
  exactly as the returned tolerance object carries them. Before, the lower
  shoulder was held fixed while the returned curve scaled it.
* **Variables are weighted by 1/rank, as in scoring.** The likelihood weighted
  by rank, so with more than one fitted variable a rank-5 variable counted five
  times a rank-1 one -- the priorities inverted. With one variable this cancels,
  which is why it went unnoticed. On data generated by the deployed scorer the
  old fit's McFadden R-squared was 0.02-0.05 (barely better than no model); it is
  now 0.55-0.69, and the heavily weighted variable's optimum is recovered to
  within about 0.1 degrees at the lower bound.
* **Posterior SDs and credible intervals are numbers.** In the MAP + Laplace
  path `sqrt(diag(cov))` dropped the parameter names, so the by-name lookups
  returned `NA` for every `posterior_sd_*` and `ci95_*`.
* **A non-finite Hessian no longer errors.** A MAP estimate on the optimiser's
  bound could crash the update with "missing value where TRUE/FALSE needed".
* **Bound-hitting estimates are flagged.** `updated_params[[v]]$at_bound_omin`
  and `at_bound_omax` say when an estimate sits on (or outside) the search box;
  its Laplace curvature is meaningless there, so the prior SD is reported
  instead, with a warning.

**If you have custom tolerance envelopes fitted on more than one variable,
refit them.** They were fitted under the inverted weights.

`seafloormapRcore` carries the only other copy of this code (it cannot run on
its own -- it needs a species pack that supplies `get_species_tolerances()`) and
has the same corrections.

## Fixes to `check_exclusions()` (and so `predict_oyster()`)

* **`predict_oyster()` no longer fails for `crassostrea_iredalei` and
  `saccostrea_cucullata` on a survey without a date column.** With no date,
  `season` is `NA` and the seasonal checks are meant to be skipped, but
  `season == "winter"` is then `NA`, `NA & TRUE` is `NA`, and any cell colder
  than the species' winter minimum (18 degrees for these two) left `NA` in the
  flag. The assignment that records the reason then stopped with "NAs are not
  allowed in subscripted assignments". The other fifteen species have winter
  minimums below the temperatures in typical data, which hid it. Every species
  is now scored on the example survey in the tests.
* **`exclusion_reason` names each row's own value.** The reason for an excluded
  row was built from every flagged row's temperature (or oxygen) at once, so
  with two cells too cold both rows read "temp_too_cold (2.0 < 7.0); temp_too_cold
  (3.0 < 7.0)". Each row now lists only its own. This affects the temperature,
  seasonal and oxygen reasons; the salinity reason was already per row.

## Fix: `generate_summary_pdf()` in a non-UTF-8 locale

The PDF device cannot draw an em dash or a middle dot in a C or other non-UTF-8
locale and stopped with "conversion failure ... in 'mbcsToSbcs'" -- including
under `R CMD check` on some machines. In such a locale those characters now get
plain-ASCII stand-ins; output in a UTF-8 locale is unchanged.

## Insufficient data is a class

A location below `confidence_floor` (default 0.5) is returned as
`"Insufficient data"` rather than handed a class it cannot support. The
continuous score is still returned for it. Consequences elsewhere:

* `area_summary()` counts those locations toward the surveyed area — that
  ground was surveyed — and never toward the suitable area, which remains
  High + Moderate. They are excluded from patch analysis.
* `export_geotiff()` now writes seven bands. Band 6 is per-location
  `confidence`; band 7 is the `insufficient_mask`. Unclassified locations are
  held out of the band 1 interpolation rather than asserting a suitability
  into the surface.
* `export_qml_style()` writes a companion `<name>_insufficient.qml` styling
  band 7 as a grey overlay, so unclassified ground reads in QGIS as a class
  rather than as a hole in the map.
* Locations the model declined to classify are no longer eligible as top
  introduction sites.

## Notes

* `R/evidence_weights.R` is vendored from `seafloormapRcore`. oystermapR does
  not import the core package — it carries its own copy of the engine — so fix
  bugs in the core copy first and re-vendor rather than editing in place.
* Reliability has no default. A layer with no measured reliability receives no
  weight, which is what keeps the method honest: nothing enters the score on
  the strength of an argument alone.

# oystermapR 1.5.0

## Ocean acidification scoring (pH + aragonite saturation)

* **pH scoring** added to all 17 species tolerance profiles with species-specific
  thresholds. *Ostrea edulis* (most sensitive): optimal 7.9–8.3; poor below 7.6.
  Cupped/Pacific species slightly more tolerant; brackish *C. iredalei* lowest thresholds.
* **`calculate_aragonite()`** — new exported function. Computes aragonite saturation
  state (Ω_arag) from pH, total alkalinity, temperature, and salinity with no
  external package dependencies. Uses Lueker et al. (2000) K1/K2, Dickson (1990) KB,
  Millero (1995) KW, Mucci (1983) Ksp_arag, Uppstrom (1974) [B_T], Riley & Tongudai
  (1967) [Ca²⁺]. Validated: T=12 °C, S=35, pH=8.1, TA=2300 µmol/kg → Ω≈2.54.
* **Aragonite auto-calculation**: if `ph` and `alkalinity` columns are present in the
  survey data, `predict_oyster()` automatically computes `omega_aragonite` before
  scoring — no preprocessing step required.
* **Omega_aragonite scoring** added to all 17 species tolerance profiles.
* **Sample data updated**: `inst/extdata/sample_survey.csv` and
  `inst/extdata/example_bay_ctd.csv` now include `ph` and `alkalinity` columns with
  realistic values for their respective locations.

## Visualisation and spatial tools

* **`plot_tolerance(species, variable)`** — new exported function. Draws the
  suitability scoring curve for any species/variable combination directly from
  the tolerance specification, no dataset required. Colour-coded zone backgrounds
  (green = optimal, orange = poor/acceptable, red = excluded). For seasonal
  variables (temperature), pass `season = "all"` to overlay all four curves.
  Falls back to base R `plot()` if ggplot2 is not installed.

* **`area_summary(result)`** — new exported function. Converts point-based
  suitability output to fine-scale area estimates in m² (primary unit) and ha
  (secondary). Designed for restoration ecology and science where sub-hectare
  precision matters. Key features:
  - Auto-estimates survey cell size from median nearest-neighbour spacing;
    accepts explicit `cell_size_m` for known grids (ROV, AUV, ADCP tracklines).
  - Per-class breakdown: n_points, area m², area ha, % of total, % of suitable
    area, mean and median suitability.
  - Totals: surveyed area, suitable (High + Moderate) area, % suitable.
  - Contiguous patch analysis (`terra::patches()`) identifies individual
    High/Moderate habitat patches by size, with a `viable` flag against a
    configurable minimum area threshold (default 100 m² = 0.01 ha, aligned
    with OSPAR oyster reef restoration guidance). Largest/smallest/median patch
    size printed to console.
  - Cell size cap at 500 m prevents inflated estimates from sparse transect
    spacing on large offshore surveys.

* **Note:** `compare_species()` and `sensitivity_analysis()` were already
  implemented in `batch_compare.R` and `variable_importance.R` respectively.

## Variable impact diagnostic (`variable_impact()`)

* **`variable_impact(result, species)`** — new exported function. Returns a ranked
  summary table showing for each environmental variable: species importance rank,
  normalised weight (%), mean score across non-excluded points, net weighted
  contribution to the suitability score, and data coverage (% of points with data).
  Sort by `"mean_contribution"` (default, highest-impact variables first),
  `"mean_score"` (lowest-scoring = likely bottlenecks), or `"pct_coverage"`
  (sparsest data layers). Primary diagnostic for QA and survey planning.

## Tolerance improvements (all 17 species)

* **Dissolved oxygen added to `scored`** for all 17 species (rank 5). DO was
  previously only used as a hard exclusion threshold. Sites are now credited for
  better oxygen conditions within the acceptable range — a site at 9 mg/L scores
  higher than one at 5 mg/L. Species-specific optimal ranges applied (e.g.
  O. edulis 6–10 mg/L; C. iredalei 4–8 mg/L).
* **Salinity added to `scored`** for 6 species that lacked it (ostrea_edulis,
  magallana_gigas, crassostrea_angulata, ostrea_stentina, ostrea_lurida,
  ostrea_angasi). Salinity was exclusion-only for these species; sites at
  optimal salinity now score higher than sites at merely acceptable salinity.
* **`dissolved_oxygen` and `salinity` added to `col_aliases`** — these variables
  are now recognised from standard column names (e.g. `do`, `do_mgl`, `sal`,
  `salinity_psu`) and properly matched to the scored entries.
* **O. edulis depth `optimal_min` corrected** from 0 m to 2 m with
  `poor_min = 0`. O. edulis is a subtidal species; intertidal sites (0–2 m) now
  score in the "poor" rather than "optimal" zone, consistent with published habitat
  assessments (Pogoda et al. 2023).

## Data completeness overlay

* **`n_layers_scored`** — new integer output column from `predict_oyster()`. Counts
  the number of environmental variables that contributed to the suitability score at
  each location. Use alongside `data_completeness` (fraction) to identify points where
  the score rests on few data inputs.
* **GeoTIFF band 5**: `export_geotiff()` now writes `n_layers_scored` as a fifth raster
  band. Load it as a thematic overlay in QGIS to visualise data coverage spatially
  alongside the suitability heatmap.

# oystermapR 1.4.0

## New species (9 added; 14 total)

* `crassostrea_virginica` — Eastern Oyster (NW Atlantic); high data quality.
* `saccostrea_glomerata` — Sydney Rock Oyster (E Australia); medium data quality.
* `magallana_sikamea` — Kumamoto Oyster (Japan / Pacific NW); low data quality.
* `magallana_ariakensis` — Suminoe Oyster (E Asia); low data quality.
* `crassostrea_hongkongensis` — Hong Kong Oyster (S China Sea); high data quality;
  note on historical misidentification as *C. plicatula* / *C. rivularis* included
  in species metadata.
* `crassostrea_nippona` — Iwagaki Oyster (Japan / Korea); medium data quality.
* `crassostrea_belcheri` — Tropical Rock Oyster (SE Asia); low data quality.
* `ostrea_chilensis` — Chilean Oyster (S Chile / New Zealand); low data quality.
* `ostrea_denselamellosa` — Korean Flat Oyster (E Asia); low data quality.

## New sensor readers (3 added)

* `read_nortek_aquadopp()` — reads Nortek Aquadopp ASCII export (ENU or beam
  files); handles moored (fixed lat/lon) and vessel-mounted (GPS in file) deployments.
* `read_rdi_adcp()` — reads Teledyne RDI binary PD0 files via `oce::read.adp.rdi()`;
  falls back to WinRiver ASCII CSV with `VelEast_binN` columns.
* `read_aanderaa_csv()` — reads Aanderaa Data Studio CSV exports; auto-detects
  speed unit (cm/s vs m/s); handles "Horisontal Speed" column name variant.

## Vignette

* Added `example-bay-survey` vignette: complete eight-step pipeline using simulated
  Example Bay data (ADCP, single-beam soundings, CTD), covering data ingest,
  QC, suitability prediction, risk scoring, and GeoTIFF export.
* Added three example data files to `inst/extdata/`:
  `example_bay_adcp.csv`, `example_bay_soundings.xyz`, `example_bay_ctd.csv`.

## Citation and documentation

* `inst/REFERENCES.md` expanded: added citations for all 9 new species; added
  Horn (1981) for terrain derivative methods; confirmed every referenced source
  in the package has a corresponding entry.
* Fixed pre-existing omission of *Ostrea lurida* from the species table in the
  bundled user-guide PDF.

---

# oystermapR 1.2.0

## Live data integration

* `oystermapR_live_config()` — stores credentials for CMEMS, ICES, EMODnet, and
  FSA in a session-scoped environment; credentials are never written to disk.
* `fetch_live_environmental_data()` — pulls salinity, temperature, current speed,
  chlorophyll, HAB presence, and substrate from external APIs and merges the
  result onto an existing survey dataframe by rounded lat/lon grid keys.
* CMEMS integration updated to copernicusmarine CLI v2.x: credentials via
  environment variables, removed `--force-download` / `--output-format csv`,
  `--log-level` changed to `WARN`; all output is NetCDF parsed with ncdf4.
* Multi-candidate dataset ID fallback: NWS north-west shelf regional product
  preferred; falls back to global 0.083-degree product automatically.

## Sensor ingest (new readers)

* `read_soundings_xyz()` — reads bathymetric sounding XYZ files; derives slope
  and rugosity via `terra::terrain`.
* `read_sonar_tif()` updated to normalise backscatter intensity to
  `substrate_hardness` in the range [0, 1]; reprojects to WGS84 if needed.

## Bug fixes

* `qc_survey_data()` — fixed crash when `apply_flags = TRUE`: `cli::cli_inform`
  bullet-prefix argument was incorrectly passed as a named function argument
  instead of a named element of the message character vector.
* `devtools::document()` warnings resolved: non-ASCII characters replaced with
  `\\uXXXX` escapes in R source; roxygen `#'` lines now use plain ASCII to
  avoid unknown-macro warnings in compiled `.Rd` files.

---

# oystermapR 1.1.0

## Season-aware scoring and tidal correction

* `detect_season()` / `add_season_column()` — automatic season detection from
  a datetime column; used internally by `predict_oyster()` to apply
  season-specific tolerance weights.
* `auto_tidal_correct()` / `correct_to_chart_datum()` — corrects depth readings
  to chart datum using harmonic tidal prediction (nodal factors, UKHO-style
  harmonics).
* `add_intertidal_flag()` — flags survey points that fall within the intertidal
  zone based on tidal range.

## Habitat and risk modules

* `score_hab_risk()` — HAB (Harmful Algal Bloom) risk scoring with optional live
  ICES biotoxin data integration.
* `score_anthropogenic_disturbance()` — disturbance scoring incorporating
  shipping density, aquaculture lease proximity, and dredging history layers.
* `score_wave_exposure()` / `score_sediment_stability()` — fetch-based wave
  exposure and Shields criterion sediment stability scoring.
* `add_shellfish_classification()` — appends EU/UK shellfish harvesting
  classification zone (A/B/C/Prohibited) from spatial polygon layers.

## Reporting

* `generate_summary_pdf()` — produces a self-contained four-page A4 PDF using
  only `grDevices::pdf()` and `ggplot2`; no LaTeX or pandoc dependency.

## Validation

* `validate_against_records()` now returns Brier score and F1 alongside AUC,
  TSS, sensitivity, and specificity.
* `spatial_block_cv()` spatial block cross-validation added.

---

# oystermapR 1.0.0

Initial CRAN release.

## Core prediction pipeline

* `predict_oyster()` — AHP-weighted suitability scoring from tabular sensor data,
  with automatic column name matching, season detection, hard exclusion checks,
  and optional GeoTIFF + contour export for QGIS.
* `check_exclusions()` / `score_locations()` — modular exclusion and scoring
  steps for programmatic use.
* `export_geotiff()` / `export_qml_style()` — raster and QGIS colour-ramp export.

## Species supported

Ostrea edulis, Magallana gigas, Crassostrea angulata, Ostrea stentina,
Ostrea lurida. See `list_species()`.

## Sensor ingest

* `read_nortek_adcp()` — Nortek Signature 500 ADCP CSV parser.
* `read_sonar_tif()` — Ping 3DSS / BioBase bathymetric raster reader.
* `merge_sensor_data()` / `ingest_sensors()` — spatial merge of multi-sensor datasets.

## Validation and diagnostics

* `validate_against_records()` — AUC, TSS, Brier score, F1, sensitivity/specificity
  vs known presence/absence records.
* `spatial_block_cv()` — spatial block cross-validation (Roberts et al. 2017).
* `permutation_importance()` — variable importance by AUC drop.
* `sensitivity_analysis()` — partial dependence curves.

## Bayesian tolerance updating

* `update_species_tolerances()` — MAP + Laplace or RWMH MCMC updating of
  optimal-range parameters from field observations.
* `get_tolerance_posteriors()`, `save_tolerance_update()`,
  `load_tolerance_update()`, `reset_tolerance_update()`.

## Risk and disturbance modules

* `score_predation_risk()`, `score_hab_risk()`,
  `score_anthropogenic_disturbance()`, `score_wave_exposure()`,
  `score_sediment_stability()`, `add_shellfish_classification()`.

## Larval connectivity

* `score_larval_connectivity()` — hybrid union-find Gaussian kernel + optional
  OpenDrift/FVCOM connectivity matrix scoring.
* `parse_opendrift_connectivity()` — converts OpenDrift CSV output to
  connectivity matrix format.

## Multi-species and seasonal comparison

* `compare_species()`, `compare_surveys()`, `composite_seasonal()`.
