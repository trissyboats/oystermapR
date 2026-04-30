pkgname <- "oystermapR"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
base::assign(".ExTimings", "oystermapR-Ex.timings", pos = 'CheckExEnv')
base::cat("name\tuser\tsystem\telapsed\n", file=base::get(".ExTimings", pos = 'CheckExEnv'))
base::assign(".format_ptime",
function(x) {
  if(!is.na(x[4L])) x[1L] <- x[1L] + x[4L]
  if(!is.na(x[5L])) x[2L] <- x[2L] + x[5L]
  options(OutDec = '.')
  format(x[1L:3L], digits = 7L)
},
pos = 'CheckExEnv')

### * </HEADER>
library('oystermapR')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("add_intertidal_flag")
### * add_intertidal_flag

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: add_intertidal_flag
### Title: Add an intertidal zone flag to a survey dataframe
### Aliases: add_intertidal_flag

### ** Examples

## Not run: 
##D survey <- auto_tidal_correct(survey, datetime_col = "date")
##D survey <- add_intertidal_flag(survey)
##D 
##D # Check intertidal coverage
##D table(survey$intertidal_zone)
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("add_intertidal_flag", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("add_shellfish_classification")
### * add_shellfish_classification

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: add_shellfish_classification
### Title: Add shellfish water quality classification to scored result
### Aliases: add_shellfish_classification

### ** Examples

## Not run: 
##D # Option 1: manual column already in data
##D result <- add_shellfish_classification(result, class_col = "water_class")
##D 
##D # Option 2: separate classified areas dataframe
##D areas <- data.frame(lat = c(51.5), lon = c(-4.2), shellfish_class = c("B"))
##D result <- add_shellfish_classification(result, classified_areas = areas)
##D 
##D # Option 3: live fetch (internet required)
##D result <- add_shellfish_classification(result, fetch_live = TRUE)
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("add_shellfish_classification", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("add_suitability_ci")
### * add_suitability_ci

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: add_suitability_ci
### Title: Add bootstrap confidence intervals to suitability scores
### Aliases: add_suitability_ci

### ** Examples

## Not run: 
##D result <- predict_oyster(survey, "ostrea_edulis")
##D tol    <- get_species_tolerances("ostrea_edulis")
##D result <- add_suitability_ci(result, tol,
##D                              uncertainty = c(temperature = 0.3,
##D                                              salinity    = 0.5))
##D # Columns suit_ci_lower, suit_ci_upper, suit_ci_width now present
##D generate_report(result, "report.html")   # map rings scale with CI width
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("add_suitability_ci", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("analyse_connectivity")
### * analyse_connectivity

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: analyse_connectivity
### Title: Analyse spatial connectivity of suitable habitat cells
### Aliases: analyse_connectivity

### ** Examples

## Not run: 
##D result <- predict_oyster(survey, "ostrea_edulis")
##D result <- analyse_connectivity(result, gap_m = 500)
##D 
##D # Large well-connected patches are the best restoration targets
##D good_patches <- subset(result,
##D   connectivity_class == "large" & suitability_class == "High")
##D 
##D # Count patches
##D table(result$connectivity_class)
##D 
##D # Visualise \u2014 patch_id maps to distinct colours in QGIS
##D export_geotiff(result, "connectivity.tif")
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("analyse_connectivity", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("assess_gear_feasibility")
### * assess_gear_feasibility

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: assess_gear_feasibility
### Title: Assess gear deployment feasibility at survey locations
### Aliases: assess_gear_feasibility

### ** Examples

## Not run: 
##D result <- predict_oyster(survey, "ostrea_edulis")
##D result <- assess_gear_feasibility(result)
##D 
##D # Sites where longline is feasible AND suitability is High
##D longline_sites <- subset(result,
##D   gear_longline_feasible & suitability_class == "High")
##D 
##D # Restoration reef sites (no farming licence needed)
##D reef_sites <- subset(result, gear_restoration_reef_feasible)
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("assess_gear_feasibility", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("auto_tidal_correct")
### * auto_tidal_correct

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: auto_tidal_correct
### Title: Automatically predict and apply tidal correction from harmonic
###   constituents
### Aliases: auto_tidal_correct

### ** Examples

## Not run: 
##D # Automatic correction \u2014 finds nearest port, predicts heights, corrects depths
##D survey_corrected <- auto_tidal_correct(survey, datetime_col = "date")
##D 
##D # Tighten the distance threshold (only trust ports within 40 km)
##D survey_corrected <- auto_tidal_correct(survey, max_port_dist_km = 40)
##D 
##D # See which port was selected and inspect predicted heights
##D survey_corrected$tidal_height_pred_m
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("auto_tidal_correct", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("check_exclusions")
### * check_exclusions

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: check_exclusions
### Title: Check exclusion criteria for all rows in a dataset
### Aliases: check_exclusions

### ** Examples

tol <- get_species_tolerances("ostrea_edulis")
df <- data.frame(
  lat = 51.5, lon = -2.5,
  temperature = 8, salinity = 28, dissolved_oxygen = 7
)
check_exclusions(df, tol)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("check_exclusions", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("classify_substrate_from_backscatter")
### * classify_substrate_from_backscatter

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: classify_substrate_from_backscatter
### Title: Classify seabed substrate hardness from near-seabed ADCP
###   backscatter
### Aliases: classify_substrate_from_backscatter

### ** Examples

## Not run: 
##D # ADCP data with near-seabed backscatter column
##D survey <- classify_substrate_from_backscatter(survey,
##D             backscatter_col = "bottom_backscatter_db",
##D             is_sv = TRUE)
##D 
##D # The substrate_hardness_index column feeds directly into predict_oyster()
##D # as the 'substrate_hardness' variable
##D names(survey)[names(survey) == "substrate_hardness_index"] <- "substrate_hardness"
##D result <- predict_oyster(survey, "ostrea_edulis")
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("classify_substrate_from_backscatter", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("compare_species")
### * compare_species

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: compare_species
### Title: Compare suitability across multiple oyster species
### Aliases: compare_species

### ** Examples

## Not run: 
##D # Compare all supported species
##D comparison <- compare_species(survey)
##D 
##D # Compare only well-characterised species
##D comparison <- compare_species(survey, min_data_quality = "high")
##D 
##D # Compare specific species and export per-species heatmaps
##D comparison <- compare_species(
##D   survey,
##D   species    = c("ostrea_edulis", "magallana_gigas"),
##D   output_dir = "comparison_maps/"
##D )
##D 
##D # Find sites suitable for both O. edulis AND M. gigas
##D both_high <- subset(comparison, n_species_high >= 2)
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("compare_species", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("compare_surveys")
### * compare_surveys

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: compare_surveys
### Title: Compare suitability scores across multiple surveys or monitoring
###   years
### Aliases: compare_surveys

### ** Examples

## Not run: 
##D r2022 <- predict_oyster(survey_2022, "ostrea_edulis")
##D r2023 <- predict_oyster(survey_2023, "ostrea_edulis")
##D r2024 <- predict_oyster(survey_2024, "ostrea_edulis")
##D 
##D comp <- compare_surveys(
##D   surveys = list("2022" = r2022, "2023" = r2023, "2024" = r2024)
##D )
##D 
##D comp$summary        # mean scores by year
##D comp$change         # year-on-year change
##D 
##D # Pass to generate_report() for a full comparative HTML report
##D generate_report(r2024, "monitoring_report.html",
##D                 title = "Kames Bay 3-Year Monitoring")
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("compare_surveys", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("composite_seasonal")
### * composite_seasonal

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: composite_seasonal
### Title: Combine multi-season survey results into a composite suitability
###   score
### Aliases: composite_seasonal

### ** Examples

## Not run: 
##D spring <- predict_oyster(survey_spring, "ostrea_edulis")
##D summer <- predict_oyster(survey_summer, "ostrea_edulis")
##D autumn <- predict_oyster(survey_autumn, "ostrea_edulis")
##D 
##D composite <- composite_seasonal(
##D   surveys = list(spring = spring, summer = summer, autumn = autumn),
##D   method  = "min"
##D )
##D 
##D generate_report(composite, "composite_report.html",
##D                 title = "Year-round Suitability Assessment")
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("composite_seasonal", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("correct_to_chart_datum")
### * correct_to_chart_datum

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: correct_to_chart_datum
### Title: Correct survey depths to Chart Datum (LAT)
### Aliases: correct_to_chart_datum

### ** Examples

## Not run: 
##D # Survey conducted around high water at Oban \u2014 tidal height ~3.1 m above CD
##D survey_corrected <- correct_to_chart_datum(survey, tidal_height_m = 3.1)
##D 
##D # Per-row heights from a tide gauge CSV matched to survey timestamps
##D tide_series <- read.csv("oban_tide_gauge.csv")
##D # (match to survey rows by timestamp \u2014 user responsibility)
##D survey_corrected <- correct_to_chart_datum(
##D   survey,
##D   tidal_height_m = tide_series$height_m
##D )
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("correct_to_chart_datum", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("detect_season")
### * detect_season

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: detect_season
### Title: Detect meteorological season from date and latitude
### Aliases: detect_season

### ** Examples

detect_season("2024-01-15", lat = 51.5)   # "winter" (UK)
detect_season("2024-07-15", lat = 51.5)   # "summer"
detect_season("2024-01-15", lat = -33.9)  # "summer" (Sydney)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("detect_season", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("estimate_chlorophyll_from_backscatter")
### * estimate_chlorophyll_from_backscatter

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: estimate_chlorophyll_from_backscatter
### Title: Estimate chlorophyll-a concentration from ADCP acoustic
###   backscatter
### Aliases: estimate_chlorophyll_from_backscatter

### ** Examples

## Not run: 
##D # Default calibration (300 kHz Nortek Signature)
##D adcp <- read_nortek_adcp("survey.csv")
##D adcp <- estimate_chlorophyll_from_backscatter(
##D           adcp, backscatter_col = "amp_mean_dB",
##D           frequency_khz = 300)
##D 
##D # Custom calibration from water samples
##D adcp <- estimate_chlorophyll_from_backscatter(
##D           adcp, "amp_mean_dB", frequency_khz = 300,
##D           calibration = c(a = 0.041, b = -0.52))
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("estimate_chlorophyll_from_backscatter", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("export_contours")
### * export_contours

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: export_contours
### Title: Export optional contour lines as a GeoPackage for QGIS
### Aliases: export_contours

### ** Examples

## Not run: 
##D export_geotiff(result, "oyster_heatmap.tif")
##D export_contours("oyster_heatmap.tif")
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("export_contours", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("export_geotiff")
### * export_geotiff

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: export_geotiff
### Title: Export suitability scores as a smooth interpolated GeoTIFF
###   heatmap for QGIS
### Aliases: export_geotiff

### ** Examples

## Not run: 
##D result <- predict_oyster("survey.csv", "ostrea_edulis")
##D 
##D # Standard export \u2014 smooth heatmap with auto radius and contours
##D export_geotiff(result, "oyster_heatmap.tif")
##D 
##D # Finer resolution for a small harbour survey
##D export_geotiff(result, "oyster_heatmap.tif", resolution = 0.0001)
##D 
##D # Override IDW radius explicitly (e.g. sparse transect data)
##D export_geotiff(result, "oyster_heatmap.tif", idw_max_dist = 0.005)
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("export_geotiff", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("fetch_live_environmental_data")
### * fetch_live_environmental_data

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: fetch_live_environmental_data
### Title: Fetch live environmental data for a survey extent
### Aliases: fetch_live_environmental_data

### ** Examples

## Not run: 
##D oystermapR_live_config(cmems_user = "u", cmems_password = "p")
##D live <- fetch_live_environmental_data(survey, sources = c("ices_hab", "ices_vms"))
##D result <- score_hab_risk(result, hab_data = live$ices_hab, species = "ostrea_edulis")
##D result <- score_anthropogenic_disturbance(result, trawling_data = live$ices_vms)
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("fetch_live_environmental_data", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("generate_report")
### * generate_report

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: generate_report
### Title: Generate a formatted survey report from predict_oyster() results
### Aliases: generate_report

### ** Examples

## Not run: 
##D result <- predict_oyster(survey, "ostrea_edulis",
##D                          output_geotiff = "kames_bay.tif")
##D 
##D # Standard HTML report (recommended \u2014 Plotly charts, no LaTeX needed)
##D generate_report(result, "kames_bay_report.html",
##D                 title  = "Kames Bay Oyster Habitat Assessment",
##D                 author = "T. Tucker")
##D 
##D # With validation results embedded
##D val  <- validate_against_records(result, nbn_records)
##D generate_report(result, "kames_bay_validated.html",
##D                 validation = val)
##D 
##D # With competition adjustment (when comparing species)
##D comp <- compare_species(survey,
##D           species = c("ostrea_edulis", "magallana_gigas"))
##D generate_report(result, "kames_bay_competition.html",
##D                 comparison = comp)
##D 
##D # PDF report (requires LaTeX / tinytex)
##D generate_report(result, "kames_bay_report.pdf",
##D                 title = "Kames Bay Oyster Habitat Assessment")
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("generate_report", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("get_species_tolerances")
### * get_species_tolerances

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: get_species_tolerances
### Title: Get species tolerance parameters
### Aliases: get_species_tolerances

### ** Examples

tol <- get_species_tolerances("ostrea_edulis")
tol$exclusions$temperature



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("get_species_tolerances", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("get_tolerance_posteriors")
### * get_tolerance_posteriors

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: get_tolerance_posteriors
### Title: Retrieve current posterior tolerance parameters for a species
### Aliases: get_tolerance_posteriors

### ** Examples

## Not run: 
##D # After calling update_species_tolerances()
##D post <- get_tolerance_posteriors("ostrea_edulis")
##D post$temperature  # updated optimal_min, optimal_max with CIs
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("get_tolerance_posteriors", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("identify_resilient_sites")
### * identify_resilient_sites

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: identify_resilient_sites
### Title: Identify climate-resilient locations
### Aliases: identify_resilient_sites

### ** Examples

## Not run: 
##D proj      <- project_suitability(result, tol)
##D resilient <- identify_resilient_sites(proj, result, min_class = "Moderate")
##D nrow(resilient)  # sites that stay Moderate+ even under RCP8.5
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("identify_resilient_sites", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("interpolate_survey")
### * interpolate_survey

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: interpolate_survey
### Title: Interpolate survey measurements to a regular grid before scoring
### Aliases: interpolate_survey

### ** Examples

## Not run: 
##D # Auto method \u2014 kriging for dense variables, IDW for sparse
##D grid <- interpolate_survey(survey, resolution_m = 100)
##D result <- predict_oyster(grid, "ostrea_edulis")
##D 
##D # Force IDW (fast, no gstat needed)
##D grid <- interpolate_survey(survey, resolution_m = 200, method = "idw")
##D 
##D # Inspect which method was used per variable
##D unique(grid$interp_method_temperature)
##D 
##D # Kriging variance \u2014 high values = uncertain interpolation
##D hist(grid$krige_var_temperature)
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("interpolate_survey", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("list_species")
### * list_species

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: list_species
### Title: List all supported species
### Aliases: list_species

### ** Examples

list_species()



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("list_species", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("load_tolerance_update")
### * load_tolerance_update

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: load_tolerance_update
### Title: Load saved Bayesian tolerance updates into session cache
### Aliases: load_tolerance_update

### ** Examples

## Not run: 
##D load_tolerance_update("ostrea_edulis")
##D # Now predict_oyster() uses the updated tolerances automatically
##D result <- predict_oyster(survey, "ostrea_edulis")
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("load_tolerance_update", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("merge_sensor_data")
### * merge_sensor_data

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: merge_sensor_data
### Title: Merge multiple sensor datasets into a single survey table
### Aliases: merge_sensor_data

### ** Examples

## Not run: 
##D # Single run per sensor
##D adcp    <- read_nortek_adcp("adcp.csv")
##D biobase <- read_generic_csv("biobase.csv")
##D survey  <- merge_sensor_data(adcp = adcp, biobase = biobase)
##D 
##D # Multiple ADCP runs (different survey days) \u2014 automatically stacked
##D adcp1  <- read_nortek_adcp("survey_jan.csv")
##D adcp2  <- read_nortek_adcp("survey_feb.csv")
##D survey <- merge_sensor_data(adcp = list(adcp1, adcp2), biobase = biobase)
##D 
##D result <- predict_oyster(survey, "ostrea_edulis", output_geotiff = "map.tif")
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("merge_sensor_data", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("oystermapR_help")
### * oystermapR_help

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: oystermapR_help
### Title: Print an interactive getting-started guide for oystermapR
### Aliases: oystermapR_help

### ** Examples

oystermapR_help()                  # full guide
oystermapR_help("sensors")         # sensor ingestion only
oystermapR_help("qgis")            # QGIS output only



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("oystermapR_help", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("oystermapR_live_config")
### * oystermapR_live_config

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: oystermapR_live_config
### Title: Configure live data API credentials for oystermapR
### Aliases: oystermapR_live_config

### ** Examples

## Not run: 
##D oystermapR_live_config(
##D   cmems_user     = "myusername",
##D   cmems_password = "mypassword"
##D )
##D # Then use fetch_live = TRUE in any supporting function
##D score_hab_risk(result, species = "ostrea_edulis", fetch_live = TRUE)
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("oystermapR_live_config", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("permutation_importance")
### * permutation_importance

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: permutation_importance
### Title: Permutation variable importance for suitability model
### Aliases: permutation_importance

### ** Examples

## Not run: 
##D records <- read.csv("nbn_ostrea_edulis.csv")
##D result  <- predict_oyster(survey, "ostrea_edulis")
##D 
##D imp <- permutation_importance(result, records, n_permutations = 100)
##D print(imp)
##D # Variable        Importance  Rank
##D # temperature     0.183       1
##D # salinity        0.091       2
##D # depth           0.045       3
##D # ...
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("permutation_importance", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("predict_oyster")
### * predict_oyster

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: predict_oyster
### Title: Predict oyster growth suitability from environmental survey data
### Aliases: predict_oyster

### ** Examples

## Not run: 
##D # Using a CSV file
##D result <- predict_oyster(
##D   data    = "my_survey.csv",
##D   species = "ostrea_edulis",
##D   output_geotiff = "oyster_suitability.tif"
##D )
##D 
##D # Using a dataframe
##D df <- read.csv("survey_data.csv")
##D result <- predict_oyster(df, species = "ostrea_edulis", verbose = TRUE)
##D 
##D # View top locations
##D subset(result, suitability_class == "High")
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("predict_oyster", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("project_suitability")
### * project_suitability

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: project_suitability
### Title: Project suitability under future climate scenarios
### Aliases: project_suitability

### ** Examples

## Not run: 
##D result <- predict_oyster(survey, "ostrea_edulis")
##D tol    <- get_species_tolerances("ostrea_edulis")
##D 
##D # Project under all three UKCP18-aligned scenarios
##D proj <- project_suitability(result, tol)
##D 
##D # Mean suitability change by scenario
##D proj$summary
##D 
##D # High-risk cells: currently High but drops to Low under RCP8.5
##D vulnerable <- subset(proj$rcp85,
##D   result$suitability_class == "High" & suitability_class == "Low")
##D 
##D # Custom scenario (e.g. local downscaled projection)
##D proj2 <- project_suitability(result, tol, scenarios = NULL,
##D   custom_deltas = list(
##D     ukcp18_p95 = c(temperature = 3.2, salinity = -0.8)))
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("project_suitability", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("qc_survey_data")
### * qc_survey_data

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: qc_survey_data
### Title: Run automated quality control on raw survey data
### Aliases: qc_survey_data

### ** Examples

## Not run: 
##D # Run QC before modelling
##D survey_qc <- qc_survey_data(survey, datetime_col = "timestamp")
##D 
##D # Inspect flagged rows
##D flagged <- subset(survey_qc, qc_status %in% c("review","fail"))
##D View(flagged)
##D 
##D # Apply flags (replace flagged values with NA) before scoring
##D survey_clean <- qc_survey_data(survey, apply_flags = TRUE)
##D result <- predict_oyster(survey_clean, "ostrea_edulis")
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("qc_survey_data", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("read_generic_csv")
### * read_generic_csv

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: read_generic_csv
### Title: Read a generic sensor CSV with flexible column mapping
### Aliases: read_generic_csv

### ** Examples

## Not run: 
##D # Lowrance BioBase export (automatic column matching)
##D biobase <- read_generic_csv("biobase_export.csv")
##D 
##D # With explicit column mapping
##D probe <- read_generic_csv(
##D   "ctd_data.csv",
##D   col_map = c(lat="Lat", lon="Lon", temperature="Temp", salinity="Sal",
##D               dissolved_oxygen="DO_mgl")
##D )
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("read_generic_csv", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("read_nortek_adcp")
### * read_nortek_adcp

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: read_nortek_adcp
### Title: Read and process a Nortek Signature 500 ADCP CSV file
### Aliases: read_nortek_adcp

### ** Examples

## Not run: 
##D adcp <- read_nortek_adcp("S104456A008_AWE_Melfort_merged.csv")
##D print(adcp)
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("read_nortek_adcp", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("read_sonar_tif")
### * read_sonar_tif

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: read_sonar_tif
### Title: Read a bathymetric or sidescan GeoTIFF and convert to oystermapR
###   variables
### Aliases: read_sonar_tif

### ** Examples

## Not run: 
##D bathy_df   <- read_sonar_tif("kames_bay_bathy.tif",    type = "bathy")
##D sidescan_df <- read_sonar_tif("kames_bay_sidescan.tif", type = "sidescan")
##D survey <- merge_sensor_data(bathy = bathy_df, sidescan = sidescan_df, adcp = adcp_df)
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("read_sonar_tif", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("read_soundings_xyz")
### * read_soundings_xyz

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: read_soundings_xyz
### Title: Read a bathymetric XYZ point cloud and derive depth, slope and
###   roughness
### Aliases: read_soundings_xyz

### ** Examples

## Not run: 
##D bathy <- read_soundings_xyz("kames_bay_soundings.xyz")
##D survey <- merge_sensor_data(adcp = adcp_data, bathy = bathy)
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("read_soundings_xyz", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("reset_tolerance_update")
### * reset_tolerance_update

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: reset_tolerance_update
### Title: Reset Bayesian tolerance updates for a species
### Aliases: reset_tolerance_update

### ** Examples

## Not run: 
##D reset_tolerance_update("ostrea_edulis")
##D reset_tolerance_update("all")
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("reset_tolerance_update", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("save_tolerance_update")
### * save_tolerance_update

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: save_tolerance_update
### Title: Save Bayesian tolerance updates to disk
### Aliases: save_tolerance_update

### ** Examples

## Not run: 
##D save_tolerance_update("ostrea_edulis")
##D save_tolerance_update("all", path = "data/bayes_updates/")
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("save_tolerance_update", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("score_anthropogenic_disturbance")
### * score_anthropogenic_disturbance

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: score_anthropogenic_disturbance
### Title: Score anthropogenic disturbance at survey locations
### Aliases: score_anthropogenic_disturbance

### ** Examples

## Not run: 
##D # Manual ICES VMS data (downloaded from ICES data portal)
##D vms <- read.csv("ices_vms_sar_2022.csv")  # columns: lat, lon, sar, gear_type
##D result <- score_anthropogenic_disturbance(result, trawling_data = vms)
##D 
##D # Live ICES VMS fetch
##D result <- score_anthropogenic_disturbance(result, fetch_live = TRUE)
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("score_anthropogenic_disturbance", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("score_disease_risk")
### * score_disease_risk

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: score_disease_risk
### Title: Score locations for disease and parasite risk
### Aliases: score_disease_risk

### ** Examples

## Not run: 
##D result <- predict_oyster(survey, "ostrea_edulis")
##D 
##D # Basic risk scoring (temperature-driven only)
##D result <- score_disease_risk(result, "ostrea_edulis")
##D 
##D # With known Bonamia-positive site locations
##D infected <- data.frame(lat = c(55.8, 56.1), lon = c(-5.2, -5.4))
##D result <- score_disease_risk(result, "ostrea_edulis",
##D                               known_sites = infected)
##D 
##D # Sites with high ecological suitability but also high disease risk
##D flagged <- subset(result,
##D   suitability_class == "High" & disease_risk_class %in% c("High","Critical"))
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("score_disease_risk", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("score_economic_viability")
### * score_economic_viability

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: score_economic_viability
### Title: Score economic viability for aquaculture or restoration
### Aliases: score_economic_viability

### ** Examples

## Not run: 
##D result <- predict_oyster(survey, "ostrea_edulis")
##D result <- assess_gear_feasibility(result)
##D result <- analyse_connectivity(result)
##D 
##D harbours <- data.frame(
##D   name = c("Tarbert","Portavadie"),
##D   lat  = c(55.865, 55.875),
##D   lon  = c(-5.425, -5.300)
##D )
##D 
##D result <- score_economic_viability(result, harbours = harbours,
##D                                     target = "aquaculture")
##D 
##D # Best aquaculture candidates
##D subset(result, viability_class %in% c("Good","Excellent"))
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("score_economic_viability", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("score_hab_risk")
### * score_hab_risk

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: score_hab_risk
### Title: Score harmful algal bloom risk at survey locations
### Aliases: score_hab_risk

### ** Examples

## Not run: 
##D # Manual data
##D hab <- data.frame(lat=52.1, lon=-4.5, date="2022-07-15",
##D                   genus="Alexandrium", toxin="PSP", closure_days=14)
##D result <- score_hab_risk(result, hab_data=hab, species="ostrea_edulis")
##D 
##D # Live ICES fetch
##D result <- score_hab_risk(result, fetch_live=TRUE,
##D                          date_range=c("2015-01-01","2024-12-31"))
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("score_hab_risk", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("score_larval_connectivity")
### * score_larval_connectivity

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: score_larval_connectivity
### Title: Score larval dispersal connectivity at survey locations
### Aliases: score_larval_connectivity

### ** Examples

## Not run: 
##D result <- predict_oyster(survey, "ostrea_edulis")
##D 
##D # Route 1 only \u2014 built-in dispersal kernel
##D result <- score_larval_connectivity(result, species = "ostrea_edulis")
##D 
##D # Route 1 with custom dispersal distance (e.g. strong tidal excursion)
##D result <- score_larval_connectivity(result, species = "ostrea_edulis",
##D                                     tidal_excursion_km = 10)
##D 
##D # Route 1 with manual dispersal override
##D result <- score_larval_connectivity(result, dispersal_km = 8)
##D 
##D # Route 2 \u2014 external connectivity matrix from OpenDrift
##D cm <- read.csv("opendrift_connectivity.csv")
##D # Required columns: source_lat, source_lon, dest_lat, dest_lon, weight
##D result <- score_larval_connectivity(result,
##D            species             = "ostrea_edulis",
##D            connectivity_matrix = cm)
##D 
##D # Inspect isolated patches \u2014 need artificial seeding
##D isolated <- subset(result,
##D   larval_connectivity_class == "Isolated" & suitability_class == "High")
##D 
##D # Strong candidates: high suitability AND high connectivity
##D targets <- subset(result,
##D   suitability_class == "High" & larval_connectivity_class == "Highly connected")
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("score_larval_connectivity", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("score_predation_risk")
### * score_predation_risk

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: score_predation_risk
### Title: Score predation and bioturbation pressure at survey locations
### Aliases: score_predation_risk

### ** Examples

## Not run: 
##D # Manual data
##D pred <- data.frame(lat=51.5, lon=-4.1, species="Asterias rubens", density=3)
##D result <- score_predation_risk(result, predator_data=pred, species="ostrea_edulis")
##D 
##D # Live EMODnet fetch
##D result <- score_predation_risk(result, fetch_live=TRUE)
##D 
##D # Depth-proxy only (no predator data)
##D result <- score_predation_risk(result)
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("score_predation_risk", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("score_sediment_stability")
### * score_sediment_stability

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: score_sediment_stability
### Title: Score sediment stability and mobility at survey locations
### Aliases: score_sediment_stability

### ** Examples

## Not run: 
##D # With current velocity and substrate type columns
##D result <- score_sediment_stability(result, current_col="current_ms", substrate_col="substrate")
##D 
##D # After score_wave_exposure() \u2014 wave_hs_m column already present
##D result <- score_wave_exposure(result, fetch_km=15)
##D result <- score_sediment_stability(result)
##D 
##D # With measured grain size
##D result$d50_mm <- c(0.25, 2.5, 15.0, 0.08)
##D result <- score_sediment_stability(result)
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("score_sediment_stability", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("score_settlement")
### * score_settlement

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: score_settlement
### Title: Score locations for spat settlement suitability
### Aliases: score_settlement

### ** Examples

## Not run: 
##D adult_result     <- predict_oyster(survey, "ostrea_edulis")
##D settlement_result <- score_settlement(survey, "ostrea_edulis")
##D 
##D # Identify cells suitable for both adult survival AND natural recruitment
##D combined <- merge(adult_result, settlement_result[, c("lat","lon",
##D               "settlement_suitability","settlement_class")],
##D               by = c("lat","lon"))
##D combined$dual_suitable <- combined$suitability >= 0.6 &
##D                           combined$settlement_suitability >= 0.6
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("score_settlement", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("score_wave_exposure")
### * score_wave_exposure

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: score_wave_exposure
### Title: Score wave exposure from fetch or measured wave height
### Aliases: score_wave_exposure

### ** Examples

## Not run: 
##D # Fetch column in result
##D result$fetch_km <- c(2.5, 8.0, 25.0, 45.0)
##D result <- score_wave_exposure(result, fetch_col = "fetch_km")
##D 
##D # Uniform fetch for a sheltered sea loch
##D result <- score_wave_exposure(result, fetch_km = 3.5)
##D 
##D # Measured wave height column
##D result <- score_wave_exposure(result, wave_height_col = "hs_m")
##D 
##D # Depth proxy only (coarse)
##D result <- score_wave_exposure(result)
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("score_wave_exposure", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("sensitivity_analysis")
### * sensitivity_analysis

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: sensitivity_analysis
### Title: Partial dependence / sensitivity analysis for a scored variable
### Aliases: sensitivity_analysis

### ** Examples

## Not run: 
##D result <- predict_oyster(survey, "ostrea_edulis")
##D 
##D # Temperature response curve
##D temp_pd <- sensitivity_analysis(result, "ostrea_edulis", "temperature")
##D plot(temp_pd$x, temp_pd$suitability, type="l",
##D      xlab="Temperature (\u00b0C)", ylab="Suitability",
##D      main="Partial dependence: temperature")
##D 
##D # Salinity response (summer)
##D sal_pd <- sensitivity_analysis(result, "ostrea_edulis", "salinity",
##D                                season = "summer")
##D 
##D # ggplot2 version
##D library(ggplot2)
##D ggplot(temp_pd, aes(x, suitability)) +
##D   geom_line(colour="steelblue", linewidth=1.2) +
##D   geom_ribbon(aes(ymin=0, ymax=suitability), alpha=0.2, fill="steelblue") +
##D   labs(x="Temperature (\u00b0C)", y="Suitability [0-1]") +
##D   theme_minimal()
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("sensitivity_analysis", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("smooth_suitability")
### * smooth_suitability

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: smooth_suitability
### Title: Apply Gaussian kernel smoothing to suitability scores
### Aliases: smooth_suitability

### ** Examples

## Not run: 
##D result <- predict_oyster(survey, "ostrea_edulis")
##D 
##D # Smooth with 300 m bandwidth (good for dense ADCP surveys)
##D result_smooth <- smooth_suitability(result, bandwidth_m = 300)
##D 
##D # Compare raw vs smoothed
##D plot(result$suitability_raw, result$suitability,
##D      xlab = "Raw", ylab = "Smoothed", pch = 20)
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("smooth_suitability", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("spatial_block_cv")
### * spatial_block_cv

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: spatial_block_cv
### Title: Spatial block cross-validation for suitability model evaluation
### Aliases: spatial_block_cv

### ** Examples

## Not run: 
##D records <- read.csv("nbn_ostrea_edulis.csv")
##D result  <- predict_oyster(survey, "ostrea_edulis")
##D 
##D # Standard validation
##D std_val <- validate_against_records(result, records)
##D std_val$auc  # may be optimistically high
##D 
##D # Spatial block CV
##D sp_cv <- spatial_block_cv(result, records, n_blocks = 5)
##D sp_cv$auc_mean  # more honest estimate of transferability
##D 
##D # Compare \u2014 if sp_cv$auc_mean << std_val$auc, model is spatially
##D # autocorrelated and transfers less well than initially apparent.
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("spatial_block_cv", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("stack_surveys")
### * stack_surveys

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: stack_surveys
### Title: Stack multiple survey runs from the same sensor type
### Aliases: stack_surveys

### ** Examples

## Not run: 
##D # Stack two ADCP runs before merging with other sensors
##D adcp_mon <- read_nortek_adcp("survey_monday.csv")
##D adcp_thu <- read_nortek_adcp("survey_thursday.csv")
##D adcp_all <- stack_surveys(adcp_mon, adcp_thu)
##D 
##D survey <- merge_sensor_data(adcp = adcp_all, bathy = bathy_df)
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("stack_surveys", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("tidal_port_info")
### * tidal_port_info

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: tidal_port_info
### Title: Look up approximate tidal range for major European survey ports
### Aliases: tidal_port_info

### ** Examples

tidal_port_info("oban")
tidal_port_info("brest")



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("tidal_port_info", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("validate_against_records")
### * validate_against_records

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: validate_against_records
### Title: Validate suitability predictions against known presence/absence
###   records
### Aliases: validate_against_records

### ** Examples

## Not run: 
##D # Load known flat oyster records (e.g. from NBN Atlas CSV export)
##D records <- read.csv("nbn_ostrea_edulis.csv")
##D 
##D # Run prediction on same area
##D result <- predict_oyster(survey, "ostrea_edulis")
##D 
##D # Validate
##D val <- validate_against_records(result, records,
##D                                  presence_col = "presence")
##D val$auc           # overall discrimination power
##D val$tss           # skill score
##D val$roc_df        # data for custom ggplot
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("validate_against_records", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
### * <FOOTER>
###
cleanEx()
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
