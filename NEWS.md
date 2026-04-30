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
