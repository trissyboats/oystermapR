#' oystermapR: Predict and Map Oyster Growth Suitability
#'
#' @description
#' `oystermapR` provides a rule-based, AHP-weighted suitability scoring
#' pipeline for predicting where conditions are most likely to support oyster
#' growth. Users supply a tabular dataset of environmental and physical
#' measurements (collected in the field or from remote sensing), specify a
#' target species, and receive:
#'
#' - Per-location suitability scores \[0, 1\]
#' - Categorical suitability classes (High / Moderate / Low / Very Low / Excluded)
#' - Per-variable component scores for diagnosis and reporting
#' - A GeoTIFF raster file for heatmap visualisation in QGIS
#' - A QGIS colour-ramp style file (.qml)
#' - Contour lines for overlay mapping
#' - A formatted PDF or HTML report
#'
#' @section Supported species:
#' | Key | Latin name | Common name | Region |
#' |---|---|---|---|
#' | `ostrea_edulis` | *Ostrea edulis* | European flat oyster | NE Atlantic, North Sea, Mediterranean |
#' | `magallana_gigas` | *Magallana gigas* | Pacific oyster | Cosmopolitan (introduced) |
#' | `crassostrea_angulata` | *Crassostrea angulata* | Portuguese oyster | Iberian Peninsula, W Atlantic |
#' | `ostrea_stentina` | *Ostrea stentina* | Denticulate flat oyster | Mediterranean, Mar Menor |
#' | `ostrea_lurida` | *Ostrea lurida* | Olympia oyster | NE Pacific: Alaska to Baja California |
#'
#' Use [list_species()] to see all species with their data quality ratings.
#'
#' @section Typical workflow:
#' ```r
#' library(oystermapR)
#'
#' # Load your survey data
#' df <- read.csv("my_survey.csv")
#'
#' # Run the prediction
#' result <- predict_oyster(
#'   data           = df,
#'   species        = "ostrea_edulis",
#'   output_geotiff = "oyster_suitability.tif",
#'   verbose        = TRUE
#' )
#'
#' # Inspect high-suitability sites
#' subset(result, suitability_class == "High")
#'
#' # Export matching QGIS colour style
#' export_qml_style("oyster_suitability.tif")
#' ```
#'
#' @section Sensor compatibility:
#' The package is designed around data collectable with:
#' - **Nortek Signature 500 ADCP** \u2014 current velocity, shear stress, turbidity
#'   proxy (read with [read_nortek_adcp()])
#' - **Ping 3DSS iDX450 PRO Bathymetric Sonar** \u2014 depth, slope, roughness
#'   (read with [read_sonar_tif()])
#' - **Lowrance + BioBase** \u2014 depth, bottom hardness, vegetation, sidescan
#' - **Salinity/temperature probes** \u2014 temperature, salinity, dissolved oxygen
#'
#' @section Validation and model diagnostics:
#' Validation functions for research-grade use:
#' - [validate_against_records()]: AUC, TSS, Brier score, F1, sensitivity,
#'   specificity vs known presence/absence records.
#' - [spatial_block_cv()]: Spatial block cross-validation (Roberts et al. 2017)
#'   \u2014 avoids inflated AUC from spatially autocorrelated data.
#' - [permutation_importance()]: Variable importance by AUC drop after
#'   permuting each scored variable.
#' - [sensitivity_analysis()]: Partial dependence curve \u2014 suitability vs a
#'   single variable while others are held at their median.
#'
#' @section Bayesian tolerance updating:
#' Tolerance parameters (optimal ranges) can be updated from field observations:
#' - [update_species_tolerances()]: MAP + Laplace approximation (fast) or
#'   RWMH MCMC (full posterior). Sequential \u2014 posterior becomes next prior.
#' - [get_tolerance_posteriors()]: Retrieve updated parameters with 95% CIs.
#' - [save_tolerance_update()] / [load_tolerance_update()]: Persist across sessions.
#' - [reset_tolerance_update()]: Revert to built-in prior parameters.
#'
#' @section Risk and disturbance modules:
#' Optional scored overlays (all `fetch_live = FALSE` by default):
#' - [score_predation_risk()]: Starfish, crab, snail predation pressure.
#' - [score_hab_risk()]: Harmful algal bloom risk (PSP/ASP/DSP/AZP).
#' - [score_anthropogenic_disturbance()]: Bottom trawling (ICES VMS SAR),
#'   anchor damage, dredging/extraction.
#' - [score_wave_exposure()]: JONSWAP wave height from fetch and wind speed.
#' - [score_sediment_stability()]: Shields parameter mobility analysis.
#' - [add_shellfish_classification()]: UK/EU harvesting area classification
#'   (Class A/B/C/Prohibited; EC Regulation 854/2004).
#'
#' @section Live data integration:
#' Set credentials once; all live fetches are off by default:
#' ```r
#' oystermapR_live_config(cmems_user = "...", cmems_password = "...")
#' result_live <- fetch_live_environmental_data(survey)
#' ```
#'
#' @section Multi-species comparison:
#' - [compare_species()]: Compare suitability across multiple species at the
#'   same locations.
#' - [compare_surveys()]: Compare two survey datasets for the same species.
#' - [composite_seasonal()]: Merge summer/winter surveys into a composite score.
#'
#' @references
#' Roberts D.R. et al. (2017) Cross-validation strategies for data with
#'   temporal, spatial, hierarchical, or phylogenetic structure. Ecography
#'   40:913-929. \doi{10.1111/ecog.02881}
#'
#' Breiman L. (2001) Random Forests. Machine Learning 45:5-32.
#'
#' Gelman A. et al. (2013) Bayesian Data Analysis, 3rd ed. CRC Press.
#'
#' Eigaard O.R. et al. (2017) The footprint of bottom trawling in European
#'   waters. ICES J Mar Sci 74:1700-1710.
#'
#' Hasselmann K. et al. (1973) Measurements of wind-wave growth and swell
#'   decay during the Joint North Sea Wave Project (JONSWAP). Dtsch
#'   Hydrogr Z Suppl A8.
#'
#' @keywords internal
#' @importFrom dplyr case_when recode filter coalesce full_join group_by summarise mutate select left_join across all_of first n
#' @importFrom cli cli_inform cli_warn cli_abort cli_h2 cli_h3 cli_rule cli_code
#' @importFrom rlang `%||%`
#' @importFrom utils read.csv browseURL install.packages
#' @importFrom stats sd quantile median rnorm lm coef var predict complete.cases dnorm optim runif
#' @importFrom tools file_ext
"_PACKAGE"

# Suppress R CMD check NOTEs for bare column names used in dplyr pipelines.
# These are legitimate column names passed as symbols, not missing global vars.
utils::globalVariables(c(
  # ingest_sensors / merge_sensor_data
  "lat_bin", "lon_bin", "._lat_bin", "._lon_bin",
  "lat", "lon", "date", "n_obs", "min_obs", "spatial_res",
  # validate / spatial_block_cv
  "suitability", "suitability_class", "presence",
  "fold_id", "block_id", "block_row", "block_col",
  # connectivity / larval dispersal
  "cluster_id", "cluster_size", "gap_m",
  "larval_cluster_id", "larval_cluster_size",
  "source_quality_score", "nearest_source_km",
  # survey_compare
  "survey_name", "mean_suit", "n_sites",
  # variable importance
  "variable", "importance", "score_drop",
  # seasonal / compare
  "season", "weight_summer",
  # misc dplyr .data mask
  ".data"
))
