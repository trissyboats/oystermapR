# =============================================================================
#  oystermapR — guided walkthrough
#  T Tucker (tristantucker48@gmail.com) — package version 1.5.0
# =============================================================================
#
#  WHAT THIS IS
#  ------------
#  A single runnable script that takes you through the whole oystermapR
#  pipeline: survey CSV in -> QC -> species-specific AHP-weighted scoring ->
#  suitability table, diagnostics, area/patch statistics, a five-band GeoTIFF
#  for QGIS, and a PDF/HTML report.
#
#  It runs end to end on the sample survey bundled with the package, so you can
#  execute it once as-is to see what comes out, then point the CONFIG block at
#  your own data.
#
#  It is written to be run SECTION BY SECTION (Ctrl+Enter / Cmd+Enter through
#  it), not sourced in one go — most sections print something worth reading.
#  If you do want it non-interactively: source("oystermapR_walkthrough.R").
#
#  RUNTIME
#  -------
#  Sections 0-8 on the 200-row sample: well under a minute.
#  Scoring is a per-row loop in R, so budget roughly a second per 1,000 rows.
#  For surveys beyond ~50k points, thin or grid the data first (see Section 2).
#
#  WHAT YOU NEED INSTALLED
#  -----------------------
#  Required : dplyr, terra, sf, cli, rlang        (terra/sf need GDAL/GEOS/PROJ;
#                                                  on macOS: brew install gdal proj geos)
#  Optional : ggplot2      -> plots + PDF summary        (Sections 1, 6, 8)
#             rmarkdown,
#             knitr        -> reports                    (Section 8)
#             plotly, scales, htmlwidgets, leaflet
#                          -> interactive HTML report    (Section 8, HTML only)
#             tinytex      -> PDF report via LaTeX        (Section 8, PDF only)
#
#  Nothing below needs an internet connection except the install lines in
#  Section 0 and the (commented-out) live-data call in Section 9.
# =============================================================================


# -----------------------------------------------------------------------------
# 0. INSTALL AND LOAD
# -----------------------------------------------------------------------------
# Uncomment whichever install line applies. oystermapR is not on CRAN yet
# (submitted), so GitHub or the local tarball are the routes for now.

# install.packages(c("dplyr", "terra", "sf", "cli", "rlang"))
# install.packages(c("ggplot2", "rmarkdown", "knitr"))          # optional extras
#
# remotes::install_github("trissyboats/oystermapR")             # dev version
# install.packages("oystermapR_1.5.0.tar.gz", repos = NULL, type = "source")

library(oystermapR)

# Quick check that the optional bits are present. Nothing fails if they are
# not — the affected sections just say so and skip.
have <- function(p) requireNamespace(p, quietly = TRUE)
cat("ggplot2:", have("ggplot2"), " rmarkdown:", have("rmarkdown"),
    " knitr:", have("knitr"), "\n")

# Built-in help index — worth a look, it lists every entry point by theme.
# oystermapR_help()


# -----------------------------------------------------------------------------
# 1. CONFIG — the only block you normally need to edit
# -----------------------------------------------------------------------------

SPECIES <- "ostrea_edulis"          # see list_species() in Section 2

# Input. Either a path to your CSV, or leave as-is to use the bundled sample.
DATA_PATH <- system.file("extdata", "sample_survey.csv", package = "oystermapR")
# DATA_PATH <- "~/path/to/my_survey.csv"

OUT_DIR <- file.path(getwd(), "oystermapR_output")
dir.create(OUT_DIR, showWarnings = FALSE, recursive = TRUE)

# Raster cell size for the GeoTIFF, in DECIMAL DEGREES (the package works in
# EPSG:4326). 0.0002 deg is roughly 22 m of latitude. Rule of thumb: set it to
# about the mean spacing of your survey points — finer than that and you are
# interpolating into empty space, coarser and you throw away resolution.
RESOLUTION <- 0.0002

cat("Species:", SPECIES, "\nInput:  ", DATA_PATH, "\nOutput: ", OUT_DIR, "\n")


# -----------------------------------------------------------------------------
# 2. PICK A SPECIES, AND SEE WHAT IT WANTS
# -----------------------------------------------------------------------------
# 17 species are supported. Keys are snake_case latin names; get_species_
# tolerances() also resolves latin or common names.

list_species()

tol <- get_species_tolerances(SPECIES)
cat("\n", tol$latin_name, " (", tol$common_name, ")\n", sep = "")
cat("Data quality of the tolerance set:", tol$data_quality, "\n")

# Two blocks drive everything:
#   $exclusions — hard gates. Fail one and the location is scored 0 / "Excluded",
#                 regardless of how good everything else is.
#   $scored     — the graded variables, each with a rank. Ranks become AHP
#                 weights, normalised over the variables PRESENT IN YOUR DATA.
str(tol$exclusions, max.level = 2)
sort(vapply(tol$scored, function(x) x$rank, numeric(1)))   # variable -> rank

# IMPORTANT CONSEQUENCE, worth internalising before you read any map:
# weights are renormalised over present variables only. If you supply three
# layers, those three carry 100% of the weight between them. A weak or
# uncalibrated layer that happens to be one of few present ends up dominating
# the score. Section 6 shows you what actually drove the result.

# Tolerance curves, straight from the parameters — no data needed.
if (have("ggplot2")) {
  print(plot_tolerance(SPECIES, "temperature"))
  print(plot_tolerance(SPECIES, "salinity"))
  print(plot_tolerance(SPECIES, "temperature", season = "all"))  # seasonal overrides
}


# -----------------------------------------------------------------------------
# 3. LOAD THE SURVEY
# -----------------------------------------------------------------------------
# Minimum columns: lat, lon, date. Everything else is optional — any recognised
# environmental column present gets scored, anything missing is skipped and its
# weight redistributed. Column names are matched case-insensitively against a
# table of aliases (temp/temp_c/temperature, sal/psu/salinity, do/do_mgl/... ).
# See the README's "Input data" table for the full alias list.

survey <- read.csv(DATA_PATH, stringsAsFactors = FALSE)
cat("Rows:", nrow(survey), " Columns:", ncol(survey), "\n")
str(survey, give.attr = FALSE, list.len = 25)

# Which of the recognised variables did we actually get?
recognised <- c("temperature", "salinity", "dissolved_oxygen", "ph", "alkalinity",
                "omega_aragonite", "depth", "current_velocity", "shear_stress",
                "chlorophyll_a", "turbidity", "slope", "substrate_hardness")
present <- intersect(tolower(names(survey)), recognised)
cat("Scoreable layers present:", paste(present, collapse = ", "), "\n")

# --- If your data is raw sensor output rather than a tidy CSV ----------------
# There are readers for the common instruments, and merge_sensor_data() puts
# them on a common grid. Uncomment and adapt:
#
# adcp  <- read_nortek_adcp("adcp_export.csv")      # or read_nortek_aquadopp(),
# ctd   <- read_generic_csv("ctd_cast.csv")         #    read_rdi_adcp() (needs oce)
# bathy <- read_soundings_xyz("soundings.xyz")      # or read_sonar_tif("bathy.tif")
# survey <- merge_sensor_data(adcp = adcp, ctd = ctd, bathy = bathy)

# --- If your survey is very large --------------------------------------------
# Grid it before scoring rather than scoring every ping. interpolate_survey()
# will krige or IDW onto a regular grid at a stated resolution:
#
# survey <- interpolate_survey(survey, resolution_m = 25, method = "auto")


# -----------------------------------------------------------------------------
# 4. QC
# -----------------------------------------------------------------------------
# Physical plausibility ranges plus IQR outlier detection plus (if you have a
# timestamp) rate-of-change limits. Returns the same dataframe with qc_status
# and one qc_flag_<var> column per variable. Nothing is removed unless you ask.
#
# datetime_col must name your timestamp column — "date" in the sample.

survey_qc <- qc_survey_data(survey, datetime_col = "date", verbose = TRUE)

table(survey_qc$qc_status)
# subset(survey_qc, qc_status == "fail")            # inspect before deciding

# Two ways to act on the flags. Pick one:
survey_clean <- subset(survey_qc, qc_status != "fail")            # drop bad rows
# survey_clean <- qc_survey_data(survey, datetime_col = "date",
#                                apply_flags = TRUE)              # NA the bad values
cat("Retained", nrow(survey_clean), "of", nrow(survey_qc), "rows\n")


# -----------------------------------------------------------------------------
# 5. RUN THE MODEL
# -----------------------------------------------------------------------------
# predict_oyster() does: exclusion check -> per-variable scoring -> AHP-weighted
# aggregation -> (optionally) rasterise + contour. With verbose = TRUE it prints
# a class breakdown and the top five spatially distinct candidate sites.
#
# If your data has ph + alkalinity but no omega_aragonite, the aragonite
# saturation state is computed internally first (Lueker/Mucci constants, no
# external dependencies) and scored.

tif_path <- file.path(OUT_DIR, paste0(SPECIES, "_suitability.tif"))

result <- predict_oyster(
  data           = survey_clean,
  species        = SPECIES,
  output_geotiff = tif_path,
  resolution     = RESOLUTION,
  contours       = TRUE,        # also writes <name>_contours.gpkg
  verbose        = TRUE
)

# The returned dataframe is your input plus:
#   suitability        0-1 continuous score
#   suitability_class  High >= 0.70 | Moderate >= 0.45 | Low >= 0.20 | Very Low | Excluded
#   excluded           TRUE if a hard exclusion threshold was breached
#   limiting_factors   which variables held the score down at that point
#   data_completeness  fraction of the species' scored variables observed there
#   n_layers_scored    how many variables actually contributed
#   score_<var>        the per-variable component scores
table(result$suitability_class)
summary(result$suitability)

head(result[order(-result$suitability),
            c("lat", "lon", "suitability", "suitability_class",
              "n_layers_scored", "limiting_factors")], 10)

# Read n_layers_scored alongside suitability, always. A 0.8 resting on two
# layers is a different claim from a 0.8 resting on nine.
table(result$n_layers_scored)

write.csv(result, file.path(OUT_DIR, paste0(SPECIES, "_result.csv")),
          row.names = FALSE)


# -----------------------------------------------------------------------------
# 6. DIAGNOSTICS — what actually drove the score
# -----------------------------------------------------------------------------
# Do this before you believe the map.

# (a) Weighted contribution per variable across the dataset.
#     norm_weight_pct  = share of weight after renormalising over present layers
#     mean_score       = how well the survey scores on that variable (low = bottleneck)
#     pct_coverage     = how much of the survey actually observed it
impact <- variable_impact(result, SPECIES)
print(impact)

# Sort differently to answer different questions:
# variable_impact(result, SPECIES, sort_by = "mean_score")     # what's limiting?
# variable_impact(result, SPECIES, sort_by = "pct_coverage")   # what's missing?

# (b) Habitat area and contiguous patches, in m2. viable_area_m2 = 100 follows
#     OSPAR minimum-viable-patch guidance for oyster reef restoration.
areas <- area_summary(result, cell_size_m = NULL, viable_area_m2 = 100)
print(areas$class_summary)
print(areas$total)
print(head(areas$patches[order(-areas$patches$area_m2), ], 10))

# (c) Sanity check: drop your weakest layer and re-run. If the map changes
#     completely, the result rests on that layer and you should say so.
#     Example — if substrate_hardness is uncalibrated:
# no_sub <- predict_oyster(survey_clean[, setdiff(names(survey_clean),
#                          "substrate_hardness")], SPECIES, verbose = TRUE)
# table(no_sub$suitability_class)


# -----------------------------------------------------------------------------
# 7. QGIS OUTPUTS
# -----------------------------------------------------------------------------
# Section 5 already wrote the GeoTIFF and contours. Add the colour ramp:

export_qml_style(tif_path)

# Contours at a different interval, if you want them coarser/finer:
# export_contours(tif_path, interval = 0.1)

# In QGIS: Layer > Add Raster Layer > <species>_suitability.tif, then drag the
# matching .qml onto the layer in the Layers panel to apply the ramp.
#
# GeoTIFF bands:
#   1 suitability        continuous 0-1
#   2 excluded_mask      1 = hard-excluded
#   3 n_observations     survey points per cell
#   4 dist_to_nearest_m  distance to nearest real observation  <- interpolation trust
#   5 n_layers_scored    variables contributing per cell       <- evidence depth
#   6 confidence         per-location confidence (evidence weighting only)
#   7 insufficient_mask  1 = the model declined to classify this location
#
# Bands 3, 4, 6 and 7 are the honest-map bands. Style band 4 and mask anything
# beyond a few point-spacings before showing the map to anyone; band 1 will
# happily interpolate a confident-looking score across water nobody surveyed.
#
# export_qml_style() writes two files: the band 1 colour ramp, and a companion
# <name>_insufficient.qml styling band 7 as a grey overlay. Load the raster
# twice and drop one style on each.

cat("\nWritten to", OUT_DIR, ":\n"); print(list.files(OUT_DIR))


# -----------------------------------------------------------------------------
# 8. REPORTS
# -----------------------------------------------------------------------------
# Two flavours. The one-page PDF summary needs only ggplot2; the full report
# needs rmarkdown + knitr (+ plotly/leaflet/scales/htmlwidgets for HTML,
# or tinytex for PDF).

if (have("ggplot2")) {
  generate_summary_pdf(
    result,
    output  = file.path(OUT_DIR, paste0(SPECIES, "_summary.pdf")),
    species = SPECIES,
    title   = "Oyster Suitability Summary",
    author  = "",
    open    = FALSE
  )
}

if (have("rmarkdown") && have("knitr")) {
  generate_report(
    result,
    output  = file.path(OUT_DIR, paste0(SPECIES, "_report.html")),
    title   = "Oyster Suitability Assessment",
    author  = "",
    species = SPECIES,
    top_n   = 5L,
    open    = FALSE
  )
}


# =============================================================================
# 9. BEYOND THE CORE PIPELINE — all optional, all commented out
# =============================================================================
# Everything above is the spine. These are the modules you reach for when the
# question demands them.

# --- Validation against known presence/absence -------------------------------
# records needs lat, lon and a presence column (1/0 or TRUE/FALSE).
# records <- read.csv("oyster_observations.csv")
# val <- validate_against_records(result, records)
# c(auc = val$auc, tss = val$tss, f1 = val$f1, brier = val$brier)
#
# Plain AUC is inflated by spatial autocorrelation. Block it:
# cv  <- spatial_block_cv(result, records, n_blocks = 5)
# imp <- permutation_importance(result, records)          # AUC drop per variable
# sensitivity_analysis(result, SPECIES, variable = "temperature")  # partial dependence

# --- Evidence weighting (1.6.0) ----------------------------------------------
# The default weighting renormalises 1/rank over PRESENT variables, which is
# the trap flagged in Section 2. Evidence weighting replaces the weight, not
# the curves: w = prior x reliability x spread, with the share of the species'
# evidence base you actually observed reported alongside the score instead of
# renormalised away. Reliability is measured, never assumed -- a layer with no
# measured reliability gets no weight at all.
#
# rel <- c(
#   depth = reliability_from_replicates(depth_pass1, depth_pass2),
#   slope = reliability_from_replicates(slope_pass1, slope_pass2)
# )
# ev <- predict_oyster(survey_clean, SPECIES, weighting = "evidence",
#                      reliability = rel)
# attr(ev, "evidence_weights")    # the derivation, layer by layer
# attr(ev, "evidence_coverage")   # how much of the evidence base you have
# cat(weight_table_md(ev), sep = "\n")   # both, as markdown for a methods section
#
# ev$suitability_rank / ev$suitability_class_rank keep what rank weighting said,
# so the two are directly comparable. Locations below confidence_floor come back
# as "Insufficient data" rather than being handed a class they cannot support.

# --- Uncertainty and smoothing -----------------------------------------------
# result <- add_suitability_ci(result, tol, n_boot = 200)   # bootstrap CI columns
# result <- smooth_suitability(result, bandwidth_m = 500)   # Gaussian spatial smooth

# --- Multi-species comparison on the same grid -------------------------------
# cmp <- compare_species(survey_clean,
#                        species = c("ostrea_edulis", "magallana_gigas"),
#                        output_dir = OUT_DIR)

# --- Seasonality --------------------------------------------------------------
# survey_clean <- add_season_column(survey_clean, date_col = "date")
# comp <- composite_seasonal(list(summer = res_summer, winter = res_winter),
#                             method = "min")   # "min" = worst season limits the site

# --- Risk and exposure modules -----------------------------------------------
# score_wave_exposure()             JONSWAP significant wave height from fetch + wind
# score_sediment_stability()        Shields parameter sediment mobility
# score_larval_connectivity()       Gaussian kernel, or an OpenDrift/FVCOM matrix
# score_predation_risk()            starfish / crab / drilling snail pressure
# score_disease_risk()              Bonamia, Marteilia, MSX/Dermo
# score_hab_risk()                  PSP / ASP / DSP / AZP bloom risk
# score_anthropogenic_disturbance() trawling, anchoring, dredging
# add_shellfish_classification()    UK/EU harvesting area class A/B/C
# assess_gear_feasibility()         which culture method the site can carry
# score_economic_viability()        cost/return layer

# --- Tidal correction ---------------------------------------------------------
# Bathymetry referenced to instantaneous water level, not chart datum, is the
# single most common silent error in this kind of survey.
# survey_clean <- auto_tidal_correct(survey_clean, datetime_col = "date",
#                                    depth_col = "depth")  # nearest port auto-matched
# tidal_port_info()

# --- Bayesian tolerance updating ---------------------------------------------
# Refit the tolerance parameters from your own field observations. Subsequent
# predict_oyster() calls then use YOUR parameters automatically.
# fit <- update_species_tolerances(records, SPECIES,
#                                  update_vars = c("temperature", "salinity", "depth"))
# save_tolerance_update(SPECIES)      # persist across sessions
# get_tolerance_posteriors(SPECIES)   # inspect
# reset_tolerance_update(SPECIES)     # back to published priors

# --- Live environmental data (needs network + httr/jsonlite) ------------------
# live <- fetch_live_environmental_data(survey_clean, sources = "ices")

# --- Climate projection -------------------------------------------------------
# proj <- project_suitability(result, tol, scenarios = c("rcp45", "rcp85"),
#                             horizon = "2050s")     # UKCP18-aligned deltas
# identify_resilient_sites(proj, baseline = result, min_class = "Moderate")


# =============================================================================
#  THREE THINGS TO KNOW BEFORE TRUSTING A MAP
# =============================================================================
#  1. Weights renormalise over PRESENT variables. Few layers -> each carries a
#     lot. Run variable_impact() every time; if one weak layer holds most of the
#     weight, that is the result, not the map. Where you have repeat coverage to
#     measure reliability from, weighting = "evidence" is the mode that says so
#     rather than leaving you to notice.
#  2. Bands 3-5 of the GeoTIFF are not decoration. Band 1 interpolates into
#     unsurveyed water and looks just as confident there.
#  3. The tolerance sets are literature priors, not fits to your system. For a
#     site you have observations for, validate_against_records() first and
#     update_species_tolerances() second.
#
#  Questions: tristantucker48@gmail.com | github.com/trissyboats/oystermapR
# =============================================================================
