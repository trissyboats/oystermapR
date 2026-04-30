# oystermapR <img src="https://img.shields.io/badge/version-1.0.0-blue" align="right"/>

> Predict and map oyster growth suitability from environmental survey data

`oystermapR` takes tabular sensor data — from ADCPs, CTDs, bathymetric sonar, or standard CSV files — applies species-specific AHP-weighted scoring rules, and returns per-location suitability scores alongside a GeoTIFF heatmap ready to load in QGIS.

---

## Supported species

| Key | Species | Common name |
|-----|---------|-------------|
| `ostrea_edulis` | *Ostrea edulis* | European Flat Oyster |
| `magallana_gigas` | *Magallana gigas* | Pacific Oyster |
| `crassostrea_angulata` | *Crassostrea angulata* | Portuguese Oyster |
| `ostrea_stentina` | *Ostrea stentina* | Denticulate Flat Oyster |
| `ostrea_lurida` | *Ostrea lurida* | Olympia Oyster |

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

The output dataframe contains `suitability` (0–1), `suitability_class` (High / Moderate / Low / Very Low / Excluded), per-variable component scores, and the original columns.

---

## Input data

Your CSV needs at minimum `lat`, `lon`, and `date`. Any environmental columns present are scored automatically — missing variables are skipped and their weights redistributed.

| Variable | Recognised column names |
|----------|------------------------|
| Temperature (°C) | `temperature`, `temp`, `temp_c` |
| Salinity (PSU) | `salinity`, `sal`, `salinity_psu` |
| Dissolved oxygen (mg/L) | `dissolved_oxygen`, `do`, `oxygen` |
| Depth (m) | `depth`, `depth_m` |
| Current velocity (m/s) | `current_velocity`, `velocity`, `current` |
| Shear stress (N/m²) | `shear_stress`, `tau`, `bed_shear` |
| Chlorophyll-a (µg/L) | `chlorophyll_a`, `chla`, `chlorophyll` |
| Turbidity (NTU) | `turbidity`, `ntu`, `turb` |
| Slope (degrees) | `slope`, `slope_deg` |
| Substrate hardness | `substrate_hardness`, `hardness` |

A sample dataset is included at `inst/extdata/sample_survey.csv`.

---

## Sensor readers

```r
# Nortek Signature 500 ADCP
adcp <- read_nortek_adcp("adcp_export.csv")

# Ping 3DSS / BioBase bathymetric raster
bathy <- read_sonar_tif("bathymetry.tif")

# Merge multiple sensor sources onto a common spatial grid
survey <- merge_sensor_data(adcp = adcp, bathy = bathy)
```

---

## Validation and diagnostics

```r
# Validate against known presence/absence records
val <- validate_against_records(result, records)
val$auc      # ROC-AUC
val$tss      # True Skill Statistic

# Spatial block cross-validation (avoids inflated AUC from spatial autocorrelation)
cv <- spatial_block_cv(result, records, n_blocks = 5)
cv$mean_auc

# Variable importance
imp <- permutation_importance(result, records)

# Partial dependence curve for a single variable
sensitivity_analysis(result, records, variable = "temperature")
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
| `score_wave_exposure()` | JONSWAP wave height from fetch and wind speed |
| `score_sediment_stability()` | Shields parameter mobility analysis |
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
# Export GeoTIFF + contour lines
predict_oyster(df, "ostrea_edulis", output_geotiff = "suitability.tif")

# Export matching colour ramp style file
export_qml_style("suitability.tif")
```

Load `suitability.tif` in QGIS, then drag the `.qml` file onto the layer to apply the yellow → orange → red suitability colour ramp instantly.

---

## Citation

If you use `oystermapR` in published work, please cite:

> Tucker T. (2026). *oystermapR: Predict and Map Oyster Growth Suitability from Environmental Data*. R package version 1.0.0. https://github.com/trissyboats/oystermapR

---

## License

GPL-3 © T Tucker

Free for research, education, and non-commercial use. Commercial entities
wishing to embed oystermapR in a proprietary product should contact
tristantucker48@gmail.com to discuss a commercial licence.

