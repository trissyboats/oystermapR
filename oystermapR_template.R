# =============================================================================
# oystermapR — Kilchoan Script
# =============================================================================
# Replace file paths and species name with your own data.
# The package adapts automatically to whatever columns are present —
# you don't need every variable; missing ones are skipped.
#
# Supported variables (column names recognised automatically):
#   temperature, salinity, dissolved_oxygen, depth, current_velocity,
#   shear_stress, chlorophyll_a, turbidity, roughness, slope,
#   substrate_hardness, sediment_type, benthic_communities, biotope,
#   fishing_intensity, ph, alkalinity, omega_aragonite
#
# Supported species (pass to species = "..."):
#   ostrea_edulis, magallana_gigas, crassostrea_virginica,
#   saccostrea_glomerata, magallana_sikamea, magallana_ariakensis,
#   crassostrea_hongkongensis, crassostrea_nippona, crassostrea_belcheri,
#   crassostrea_angulata, ostrea_stentina, ostrea_lurida, ostrea_angasi,
#   ostrea_chilensis, ostrea_denselamellosa, crassostrea_iredalei,
#   crassostrea_belcheri
# =============================================================================

library(oystermapR)


# =============================================================================
# STEP 1 — Load your sensor data
# =============================================================================
# Each loader returns a dataframe with lat, lon, and the variables that
# sensor provides. Use only the loaders relevant to your equipment.

# --- Nortek Signature 500 ADCP (current velocity, shear stress) --------------
# NOTE: currently set up for the SAMS boat platform, use python program to combine 
# gps and ADCP into specialized csv

adcp <- read_nortek_adcp(
  file        = "path/to/your_adcp.csv",
  spatial_res = 3,    # decimal places for lat/lon grid binning
                      # 3 ≈ 100 m cells, 2 ≈ 1 km cells, etc
  verbose     = TRUE
)


# these sensors likely arent relevant for your use case but kept in for posterity
# --- Nortek Aquadopp (moored or vessel-mounted) ------------------------------
# adcp <- read_nortek_aquadopp("path/to/aquadopp.csv", verbose = TRUE)

# --- Teledyne RDI ADCP (binary PD0 or WinRiver ASCII) -----------------------
# adcp <- read_rdi_adcp("path/to/rdi.pd0", verbose = TRUE)

# --- Aanderaa current meter --------------------------------------------------
# current <- read_aanderaa_csv("path/to/aanderaa.csv", verbose = TRUE)

# --- Single-beam echosounder XYZ point cloud (depth, slope, roughness) -------
bathy <- read_soundings_xyz(
  file          = "path/to/soundings.xyz",
  spatial_res   = 3,
  min_soundings = 3,  # minimum pings per cell; raise for noisier surveys
  verbose       = TRUE
)

# --- Sidescan sonar GeoTIFF (substrate hardness 0-1) ------------------------
# sonar <- read_sonar_tif("path/to/backscatter.tif", spatial_res = 3)
# in this case this would be to the geotifs from xtf files, will send a 
# converter I made also

# --- CTD / water quality CSV (temperature, salinity, chlorophyll, pH, etc.) --
# read_generic_csv handles most tabular exports. Column names are matched
# automatically when they follow standard conventions. 
ctd <- read_generic_csv(
  file        = "path/to/ctd.csv",
  spatial_res = 3,
  verbose     = TRUE
)

# If your CTD exports non-standard headers, supply a col_map to recalibrate:
# ctd <- read_generic_csv(
#   "ysi_export.csv",
#   col_map = c(lat = "GPS_Lat", lon = "GPS_Lon",
#               temperature = "Temp_C", salinity = "Sal_PSU",
#               ph = "pH_total", alkalinity = "TA_umolkg")
# )


# =============================================================================
# STEP 2 — Merge all sensor datasets onto a common grid
# =============================================================================
# Add or remove arguments to match whichever loaders you used above.
# Any number of dataframes can be passed; missing columns get NA.

survey <- merge_sensor_data(
  adcp  = adcp,
  bathy = bathy,
  ctd   = ctd
  # sonar = sonar  # uncomment if sidescan was collected
)

cat("Merged survey:", nrow(survey), "grid cells,",
    ncol(survey), "variables\n")


# =============================================================================
# STEP 3 — Quality control
# =============================================================================
# Flags range failures, statistical outliers, and cross-variable anomalies.
# apply_flags = TRUE replaces bad values with NA so they are skipped downstream.

survey_clean <- qc_survey_data(
  df          = survey,
  apply_flags = TRUE,
  verbose     = TRUE
)


# =============================================================================
# STEP 4 — Predict suitability
# =============================================================================
# Generalized AHP-weighted scoring for your target species.
# omega_aragonite is calculated automatically if ph + alkalinity are present.
# if lower ammounts of prior model weight are present (limited sensors,use
# reliability weighting)

result <- predict_oyster(
  data    = survey_clean,
  species = "ostrea_edulis",
  verbose = TRUE
)

# Quick overview of the result
table(result$suitability_class)
summary(result$suitability)

# The result contains:
#   suitability         — score in [0, 1]
#   suitability_class   — High / Moderate / Low / Very Low / Excluded
#   data_completeness   — fraction of variables that were scored (0–1)
#   n_layers_scored     — integer count of variables that contributed
#   score_<variable>    — per-variable component score (one column each)
#   omega_aragonite     — auto-calculated if ph + alkalinity were present


# =============================================================================
# STEP 5 — Optional risk modules
# =============================================================================

result <- score_wave_exposure(result, verbose = FALSE)
result <- score_hab_risk(result, verbose = FALSE)
# result <- score_sediment_stability(result, verbose = FALSE)
# result <- score_predation_risk(result, verbose = FALSE)
# result <- score_anthropogenic_disturbance(result, verbose = FALSE)


# =============================================================================
# STEP 6 — Variable impact diagnostic
# =============================================================================
# Shows which variables are driving (or bottlenecking) the suitability scores.
# Use to identify limiting factors and plan future survey priorities.

impact <- variable_impact(result, "ostrea_edulis")
print(impact)

# Variables scoring below 0.5 are likely site limiters
impact[impact$mean_score < 0.5, c("variable", "norm_weight_pct", "mean_score")]

# Variables with sparse data — consider adding sensors next survey
impact[impact$pct_coverage < 80, c("variable", "pct_coverage")]
# depending on survey coverage by area (combining different areas into one possibly
# without uniform coverage it might throw a flag here just to be aware)


# =============================================================================
# STEP 7 — Fine-scale habitat area summary
# =============================================================================
# Converts point scores to area estimates (m² and ha).
# Identifies contiguous habitat patches and flags viable ones (>= 100 m²
# by default, the OSPAR oyster restoration minimum).

s <- area_summary(result, verbose = TRUE)

# Per-class breakdown
s$class_summary[, c("class", "area_m2", "area_ha", "pct_total_area",
                    "mean_suitability")]

# Total surveyed and suitable area
s$total[c("surveyed_area_m2", "suitable_area_m2", "pct_suitable", "cell_size_m")]

# Individual habitat patches (High + Moderate suitability)
head(s$patches)

# Patches meeting the OSPAR 100 m² threshold
viable <- s$patches[s$patches$viable, ]
cat(nrow(viable), "viable patches; largest:", round(max(viable$area_m2)), "m²\n")

# If you know your survey grid resolution, supply it explicitly:
# s <- area_summary(result, cell_size_m = 5)   # 5 m ROV grid
# s <- area_summary(result, cell_size_m = 25)  # 25 m ADCP tracklines


# =============================================================================
# STEP 8 — Tolerance curve visualisation
# =============================================================================
# Draws the scoring function for any species/variable combination.
# Useful for QA, stakeholder reporting, and publication figures.

plot_tolerance("ostrea_edulis", "temperature")
plot_tolerance("ostrea_edulis", "temperature", season = "all")  # all seasons
plot_tolerance("ostrea_edulis", "depth")
plot_tolerance("ostrea_edulis", "salinity")
plot_tolerance("ostrea_edulis", "ph")
plot_tolerance("ostrea_edulis", "omega_aragonite")
plot_tolerance("ostrea_edulis", "dissolved_oxygen")

# Save a plot:
# p <- plot_tolerance("ostrea_edulis", "temperature")
# ggplot2::ggsave("temperature_tolerance.png", p, width = 8, height = 5)


# =============================================================================
# STEP 9 — Export GeoTIFF for QGIS
# =============================================================================
# Writes a 5-band GeoTIFF and matching QGIS colour-ramp style (.qml).
# Load in QGIS: Layer → Add Raster Layer, then right-click → Load Layer Style.

export_geotiff(
  df         = result,
  path       = "suitability_output.tif",
  resolution = 0.001,   # degrees; ~100 m at mid-latitudes
  contours   = TRUE     # also writes a contour shapefile
)

export_qml_style("suitability_output.tif")

# GeoTIFF bands:
#   Band 1 — suitability score [0, 1]
#   Band 2 — excluded mask (1 = hard-excluded)
#   Band 3 — survey point count per cell
#   Band 4 — distance to nearest survey point (m)  ← mask beyond ~3× point spacing
#   Band 5 — n_layers_scored (data coverage overlay)

# additionally, can output the combined final table combined from all imports
# and enter into the Qgis plugin also attached for rapid mapping


# =============================================================================
# STEP 10 — Bayesian tolerance updating (optional)
# =============================================================================
# If you have presence/absence records from the loch, update the species
# tolerances from your own field data to improve future model predictions.

# records <- data.frame(
#   lat      = c(...),  # decimal degrees
#   lon      = c(...),
#   presence = c(...),  # 1 = oyster observed, 0 = absent
#   temperature = c(...),
#   salinity    = c(...),
#   depth       = c(...)
# )
#
# update_species_tolerances(
#   records,
#   "ostrea_edulis",
#   update_vars = c("temperature", "salinity", "depth"),
#   min_records = 20L,
#   verbose     = TRUE
# )
#
# # Save the updated tolerances so future sessions use them
# save_tolerance_update("ostrea_edulis", path = "my_tolerance_updates/")
#
# # In a new session, reload them before running predict_oyster()
# load_tolerance_update("ostrea_edulis", path = "my_tolerance_updates/")
#
# # Check what the posterior tolerances look like
# get_tolerance_posteriors("ostrea_edulis")
# plot_tolerance("ostrea_edulis", "temperature")  # updated curve


# =============================================================================
# STEP 11 — Validation against observed records (optional)
# =============================================================================
# If you have independent presence/absence records, validate the model's
# predictions against them.

# val <- validate_against_records(
#   result  = result,
#   records = records,
#   species = "ostrea_edulis"
# )
# print(val)  # AUC, TSS, Brier score, F1, sensitivity, specificity


# =============================================================================
# FURTHER READING
# =============================================================================
# Full function documentation:  ?predict_oyster, ?area_summary, etc.
# Variable importance detail:   ?variable_impact
# Bayesian updating:            ?update_species_tolerances
# Vignette (complete walkthrough with example data):
#   vignette("example-bay-survey", package = "oystermapR")
# CRAN page:
#   https://cran.r-project.org/web/packages/oystermapR/index.html
