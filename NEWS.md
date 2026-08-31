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
