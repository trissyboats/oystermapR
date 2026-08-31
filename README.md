# oystermapR <img src="https://img.shields.io/badge/version-1.5.0-blue" align="right"/>

> Predict and map oyster growth suitability from environmental survey data

`oystermapR` takes tabular sensor data — from ADCPs, CTDs, bathymetric sonar, or standard CSV files — applies species-specific AHP-weighted scoring rules, and returns per-location suitability scores alongside a five-band GeoTIFF heatmap ready to load in QGIS.

---

## Supported species

| Key | Species | Common name | Region |
|-----|---------|-------------|--------|
| `ostrea_edulis` | *Ostrea edulis* | European Flat Oyster | NE Atlantic / Mediterranean |
| `magallana_gigas` | *Magallana gigas* | Pacific Oyster | Global aquaculture |
| `crassostrea_angulata` | *Crassostrea angulata* | Portuguese Oyster | Iberian Peninsula |
| `ostrea_stentina` | *Ostrea stentina* | Denticulate Flat Oyster | Mediterranean / W Africa |
| `ostrea_lurida` | *Ostrea lurida* | Olympia Oyster | Pacific NW (N America) |
| `crassostrea_virginica` | *Crassostrea virginica* | Eastern Oyster | NW Atlantic |
| `saccostrea_glomerata` | *Saccostrea glomerata* | Sydney Rock Oyster | E Australia |
| `magallana_sikamea` | *Magallana sikamea* | Kumamoto Oyster | Japan / Pacific NW |
| `magallana_ariakensis` | *Magallana ariakensis* | Suminoe Oyster | E Asia |
| `crassostrea_hongkongensis` | *Crassostrea hongkongensis* | Hong Kong Oyster | S China Sea |
| `crassostrea_nippona` | *Crassostrea nippona* | Iwagaki Oyster | Japan / Korea |
| `crassostrea_belcheri` | *Crassostrea belcheri* | Tropical Rock Oyster | SE Asia |
| `ostrea_chilensis` | *Ostrea chilensis* | Chilean Oyster | S Chile / New Zealand |
| `ostrea_denselamellosa` | *Ostrea denselamellosa* | Korean Flat Oyster | E Asia |
| `ostrea_angasi` | *Ostrea angasi* | Angasi Oyster | S Australia |
| `saccostrea_cucullata` | *Saccostrea cucullata* | Rock Oyster | Indian Ocean / W Pacific |
| `crassostrea_iredalei` | *Crassostrea iredalei* | Slipper Oyster | SE Asia (brackish) |

---

## Installation

```r
# From CRAN (once accepted)
install.packages("oystermapR")

# Development version from GitHub
devtools::install_github("trissyboats/oystermapR")
```

Requires R >= 4.1.0. Core dependencies: `dplyr`, `terra`, `sf`, `cli`, `rlang`.

---

## Quick start

```r
library(oystermapR)

# Load your survey data
df <- read.csv("my_survey.csv")

# Run the prediction
result <- predict_oyster(
  data           = df,
  species        = "ostrea_edulis",
  output_geotiff = "oyster_suitability.tif",
  verbose        = TRUE
)

# Inspect high-suitability sites
subset(result, suitability_class == "High")

# Export matching QGIS colour ramp style
export_qml_style("oyster_suitability.tif")
```

The output dataframe contains `suitability` (0–1), `suitability_class` (High / Moderate / Low / Very Low / Excluded), `data_completeness` (fraction of variables scored), `n_layers_scored` (integer count of variables that contributed at each point), and per-variable component scores (`score_depth`, `score_temperature`, etc.).

---

## Input data

Your CSV needs at minimum `lat`, `lon`, and `date`. Any environmental columns present are scored automatically — missing variables are skipped and their weights redistributed. Column names are matched case-insensitively against the aliases below.

| Variable | Recognised column names |
|----------|------------------------|
| Temperature (°C) | `temperature`, `temp`, `temp_c` |
| Salinity (PSU) | `salinity`, `sal`, `salinity_psu`, `psu`, `sal_psu` |
| Dissolved oxygen (mg/L) | `dissolved_oxygen`, `do`, `do_mgl`, `oxygen`, `dissolved_o2`, `do_mg_l` |
| pH | `ph`, `ph_nbs`, `ph_total`, `seawater_ph` |
| Total alkalinity (µmol/kg) | `alkalinity`, `total_alkalinity`, `ta`, `talk`, `alk_umol_kg` |
| Aragonite saturation (Ω) | `omega_aragonite`, `omega_arag`, `aragonite_saturation`, `arag_sat`, `omega` |
| Depth (m) | `depth`, `depth_m` |
| Current velocity (m/s) | `current_velocity`, `velocity`, `current` |
| Shear stress (N/m²) | `shear_stress`, `tau`, `bed_shear` |
| Chlorophyll-a (µg/L) | `chlorophyll_a`, `chla`, `chlorophyll` |
| Turbidity (NTU) | `turbidity`, `ntu`, `turb` |
| Slope (degrees) | `slope`, `slope_deg` |
| Substrate hardness | `substrate_hardness`, `hardness` |

A sample dataset is included at `inst/extdata/sample_survey.csv`.

---

## Ocean acidification scoring

pH and aragonite saturation state (Ω_arag) are scored for all 17 species. If your data contains `ph` and `alkalinity` columns, `predict_oyster()` automatically computes `omega_aragonite` before scoring — no preprocessing required.

```r
# Aragonite auto-calculated from pH + alkalinity columns
result <- predict_oyster(df, "ostrea_edulis")

# Manual calculation (no external packages)
df$omega_aragonite <- calculate_aragonite(
  pH          = df$ph,
  alkalinity  = df$alkalinity,    # µmol/kg
  temperature = df$temperature,   # °C
  salinity    = df$salinity       # PSU
)
```

`calculate_aragonite()` implements the full carbonate chemistry using Lueker et al. (2000) K1/K2, Dickson (1990) KB, Millero (1995) KW, Mucci (1983) Ksp_arag, Uppstrom (1974) [B_T], and Riley & Tongudai (1967) [Ca²⁺]. No external package dependencies.

---

## Diagnostics and QA

### Variable impact

`variable_impact()` returns a ranked table showing which environmental variables contribute most to the suitability score across a dataset. Use it to identify limiting factors and data gaps.

```r
impact <- variable_impact(result, "ostrea_edulis")
#>   variable          rank  norm_weight_pct  mean_score  mean_contribution  n_points  pct_coverage
#>   temperature          1            24.1        0.82               0.198       842          99.5
#>   depth                2            18.7        0.91               0.170       842         100.0
#>   dissolved_oxygen     5            12.4        0.68               0.084       511          60.7
#>   ...
```

Sort by `"mean_score"` to find bottlenecks (variables scoring poorly across the dataset), or by `"pct_coverage"` to find sparsely observed layers.

### Data layer coverage

`n_layers_scored` in the result dataframe records how many variables contributed to the suitability score at each location. Low values indicate points where the score rests on few observations. GeoTIFF band 5 maps this spatially in QGIS.

---

## Fine-scale area analysis

`area_summary()` converts point-based scores into habitat area estimates at sub-hectare resolution — designed for restoration ecology where the difference between 200 m² and 800 m² matters.

```r
s <- area_summary(result, cell_size_m = 10, viable_area_m2 = 100)

# Per-class breakdown in m²
s$class_summary

# Total suitable area
s$total["suitable_area_m2"]
s$total["pct_suitable"]

# Contiguous patches — flag those meeting OSPAR minimum viable area
s$patches[s$patches$viable, ]
```

Cell size is auto-estimated from median nearest-neighbour point spacing when `cell_size_m = NULL`. The `viable_area_m2` threshold (default 100 m² = 0.01 ha) aligns with OSPAR oyster reef restoration guidance.

---

## Tolerance visualisation

`plot_tolerance()` draws the mathematical scoring curve for any species/variable combination directly from the tolerance specification — no dataset needed. Useful for QA, stakeholder communication, and report figures.

```r
# Scoring curve with colour-coded zones (optimal = green, excluded = red)
plot_tolerance("ostrea_edulis", "temperature")

# All four seasons overlaid
plot_tolerance("ostrea_edulis", "temperature", season = "all")

# Ocean acidification variables
plot_tolerance("ostrea_edulis", "ph")
plot_tolerance("magallana_gigas", "omega_aragonite")

# Compare DO tolerance: European flat vs brackish slipper oyster
plot_tolerance("ostrea_edulis",       "dissolved_oxygen")
plot_tolerance("crassostrea_iredalei","dissolved_oxygen")
```

For data-driven partial dependence curves from an actual survey, use `sensitivity_analysis()` instead.

---

## Sensor readers

```r
# Nortek Signature 500 ADCP
adcp <- read_nortek_adcp("adcp_export.csv")

# Nortek Aquadopp (moored or vessel-mounted)
adcp2 <- read_nortek_aquadopp("aquadopp_export.csv")

# Teledyne RDI binary PD0 (requires oce)
adcp3 <- read_rdi_adcp("transect.pd0")

# Ping 3DSS / BioBase bathymetric raster
bathy <- read_sonar_tif("bathymetry.tif")

# Bathymetric sounding XYZ point cloud
bathy2 <- read_soundings_xyz("soundings.xyz")

# Any standard CSV export (auto column matching)
ctd <- read_generic_csv("ctd_cast.csv")

# Merge multiple sensor sources onto a common spatial grid
survey <- merge_sensor_data(adcp = adcp, bathy = bathy2, ctd = ctd)
```

---

## Validation and diagnostics

```r
# Validate against known presence/absence records
val <- validate_against_records(result, records)
val$auc      # ROC-AUC
val$tss      # True Skill Statistic
val$f1       # F1 score
val$brier    # Brier score

# Spatial block cross-validation (avoids inflated AUC from autocorrelation)
cv <- spatial_block_cv(result, records, n_blocks = 5)

# Permutation variable importance (AUC drop per variable)
imp <- permutation_importance(result, records)

# Partial dependence curve for a single variable (data-driven)
sensitivity_analysis(result, records, variable = "temperature")

# Dataset-level weighted contribution summary (no records needed)
variable_impact(result, "ostrea_edulis")

# Multi-species comparison at the same grid
compare_species(survey_clean, species = c("ostrea_edulis", "magallana_gigas"))
```

---

## Bayesian tolerance updating

Update species tolerance parameters from your own field observations:

```r
fit <- update_species_tolerances(
  records     = field_data,
  species     = "ostrea_edulis",
  update_vars = c("temperature", "salinity", "depth")
)

# Subsequent predict_oyster() calls use the updated parameters automatically
result2 <- predict_oyster(df, "ostrea_edulis")

# Persist across sessions
save_tolerance_update("ostrea_edulis")
```

---

## Additional modules

| Function | Purpose |
|----------|---------|
| `variable_impact()` | Ranked variable contribution table for QA and survey planning |
| `area_summary()` | Fine-scale area estimates in m² with contiguous patch analysis |
| `plot_tolerance()` | Tolerance scoring curve from species parameters (no data required) |
| `calculate_aragonite()` | In-house aragonite saturation (Ω_arag) from pH and alkalinity |
| `score_wave_exposure()` | JONSWAP wave height from fetch and wind speed |
| `score_sediment_stability()` | Shields parameter sediment mobility analysis |
| `score_larval_connectivity()` | Hybrid Gaussian kernel + OpenDrift connectivity matrix |
| `score_predation_risk()` | Starfish, crab, and snail predation pressure |
| `score_hab_risk()` | Harmful algal bloom risk (PSP/ASP/DSP/AZP) |
| `score_anthropogenic_disturbance()` | Bottom trawling, anchor damage, dredging |
| `add_shellfish_classification()` | UK/EU harvesting area classification (A/B/C) |
| `compare_species()` | Side-by-side suitability across multiple species |
| `composite_seasonal()` | Merge summer/winter surveys into a composite score |
| `generate_report()` | Export a formatted PDF or HTML report |

---

## QGIS workflow

```r
# Export five-band GeoTIFF + contour lines
predict_oyster(df, "ostrea_edulis", output_geotiff = "suitability.tif")

# Export matching colour ramp style file
export_qml_style("suitability.tif")
```

Load `suitability.tif` in QGIS, then drag the `.qml` file onto the layer to apply the standard oystermapR colour scheme instantly.

GeoTIFF bands:

| Band | Name | Description |
|------|------|-------------|
| 1 | `suitability` | Continuous suitability score [0, 1] |
| 2 | `excluded_mask` | 1 = hard-excluded (failed exclusion threshold) |
| 3 | `n_observations` | Number of survey points per raster cell |
| 4 | `dist_to_nearest_m` | Distance to nearest survey point (m) |
| 5 | `n_layers_scored` | Number of environmental variables that contributed to the score |

---

## Citation

If you use `oystermapR` in published work, please cite:

> Tucker T. (2026). *oystermapR: Predict and Map Oyster Growth Suitability from Environmental Data*. R package version 1.5.0. https://github.com/trissyboats/oystermapR

---

## License

GPL-3 © T Tucker

Free for research, education, and non-commercial use. Commercial entities
wishing to embed oystermapR in a proprietary product should contact
tristantucker48@gmail.com to discuss a commercial licence.
