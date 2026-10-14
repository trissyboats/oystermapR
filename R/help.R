# =============================================================================
# oystermapR \u2014 User Guide
# =============================================================================

#' Print an interactive getting-started guide for oystermapR
#'
#' @description
#' Prints a step-by-step workflow guide to the console, covering sensor data
#' ingestion, spatial merging, suitability prediction, and QGIS export.
#' Run this any time you need a reminder of the available functions and
#' typical usage patterns.
#'
#' @param topic Character. Optionally focus on a specific topic:
#'   `"sensors"`, `"merge"`, `"tides"`, `"predict"`, `"weighting"`,
#'   `"qgis"`, `"species"`. Default `NULL` prints the full guide.
#'
#' @return Called for its side-effect (console output). Returns `invisible(NULL)`.
#' @export
#' @examples
#' oystermapR_help()                  # full guide
#' oystermapR_help("sensors")         # sensor ingestion only
#' oystermapR_help("weighting")       # rank vs evidence weighting
#' oystermapR_help("qgis")            # QGIS output only
oystermapR_help <- function(topic = NULL) {

  valid_topics <- c("sensors", "merge", "tides", "predict", "weighting",
                    "qgis", "species")

  if (!is.null(topic)) {
    topic <- tolower(trimws(topic))
    if (!topic %in% valid_topics) {
      cli::cli_warn(c(
        "Unknown topic {.val {topic}}.",
        "i" = "Valid topics: {.val {valid_topics}}",
        "i" = "Showing full guide instead."
      ))
      topic <- NULL
    }
  }

  show_all     <- is.null(topic)

  # ---- Header ----------------------------------------------------------------
  cli::cli_rule(left = "oystermapR  \u2014  Oyster Suitability Mapping")
  cli::cli_inform(c(
    "Predict and map where oyster growth is most likely to succeed.",
    "Feed in your survey data, pick a species, get a scored table and a",
    "GeoTIFF heatmap ready to open in QGIS.",
    " "
  ))

  # ---- Overview (always shown) -----------------------------------------------
  if (show_all) {
    cli::cli_h2("Typical workflow")
    cli::cli_inform(c(
      "  1.  Read each sensor CSV     \u2192  read_nortek_adcp() / read_generic_csv()",
      "                                  read_soundings_xyz() / read_sonar_tif()",
      "  2.  Stack repeated surveys   \u2192  stack_surveys()           (if > 1 run)",
      "  3.  Merge all sensors        \u2192  merge_sensor_data()",
      "  3b. Correct depths to CD     \u2192  auto_tidal_correct()      (recommended)",
      "  4.  Run the model            \u2192  predict_oyster()",
      "  5.  Weight on measured evidence \u2192 predict_oyster(weighting = \"evidence\")",
      "                                  (optional; the default is rank weighting)",
      "  6.  Open outputs in QGIS     \u2192  .tif  +  .qml  +  _contours.gpkg",
      " "
    ))
    cli::cli_inform(
      "Run {.code oystermapR_help(\"sensors\")} etc. for detail on each step."
    )
    cli::cli_rule()
  }

  # ---- STEP 1: Sensors -------------------------------------------------------
  if (show_all || topic == "sensors") {
    cli::cli_h2("Step 1 \u2014 Reading sensor data")

    cli::cli_h3("Nortek Signature 500 ADCP")
    cli::cli_inform(c(
      "Reads the raw merged Nortek CSV, auto-detects sidelobe-contaminated",
      "velocity bins, and computes current velocity + shear stress.",
      " "
    ))
    cli::cli_code(
      'adcp <- read_nortek_adcp("S104456A008_AWE_Melfort_merged.csv")'
    )
    cli::cli_inform(c(
      " ",
      "Key arguments:",
      "  {.arg max_plausible_speed}  Speed threshold for sidelobe detection (default 1.5 m/s).",
      "                             Increase to 2.5 for open-coast surveys.",
      "  {.arg spatial_res}         Grid resolution in decimal places (default 4 = ~11 m).",
      "  {.arg min_obs}             Min ensembles per cell to keep (default 5).",
      " "
    ))

    cli::cli_h3("Generic CSV  (CTD probe / Lowrance BioBase / any logger)")
    cli::cli_inform(
      "Auto-maps common column names; or supply your own mapping."
    )
    cli::cli_code(c(
      '# Auto column detection',
      'biobase <- read_generic_csv("biobase_export.csv")',
      '',
      '# Explicit column mapping',
      'probe <- read_generic_csv(',
      '  "ctd_data.csv",',
      '  col_map = c(lat         = "Latitude",',
      '              lon         = "Longitude",',
      '              temperature = "Temp_C",',
      '              salinity    = "Sal_PSU",',
      '              dissolved_oxygen = "DO_mgl")',
      ')'
    ))
    cli::cli_inform(" ")

    cli::cli_h3("Bathymetric XYZ point cloud  (e.g. Ping 3DSS export)")
    cli::cli_inform(
      "Derives depth, slope, and rugosity from raw soundings."
    )
    cli::cli_code(
      'bathy <- read_soundings_xyz("kames_bay_soundings.xyz")'
    )
    cli::cli_inform(" ")

    cli::cli_h3("GeoTIFF raster  (bathymetric DEM or sidescan mosaic)")
    cli::cli_code(c(
      'bathy_df    <- read_sonar_tif("bathy.tif",    type = "bathy")',
      'sidescan_df <- read_sonar_tif("sidescan.tif", type = "sidescan")'
    ))
    cli::cli_inform(c(
      " ",
      "  {.val \"bathy\"}    \u2192 depth, slope, roughness",
      "  {.val \"sidescan\"} \u2192 substrate_hardness (backscatter normalised 0\u20131)",
      " "
    ))

    cli::cli_rule()
  }

  # ---- STEP 2 / 3: Merge -----------------------------------------------------
  if (show_all || topic == "merge") {
    cli::cli_h2("Steps 2\u20133 \u2014 Stacking and merging datasets")

    cli::cli_h3("Single run per sensor (most common)")
    cli::cli_code(c(
      'survey <- merge_sensor_data(',
      '  adcp     = adcp,',
      '  bathy    = bathy_df,',
      '  biobase  = biobase_df',
      ')'
    ))
    cli::cli_inform(" ")

    cli::cli_h3("Multiple runs of the same sensor across different survey days")
    cli::cli_inform(
      "Use {.fn stack_surveys} first, or pass a list directly."
    )
    cli::cli_code(c(
      '# Option A: stack explicitly',
      'adcp_all <- stack_surveys(adcp_jan, adcp_feb, adcp_march)',
      'survey   <- merge_sensor_data(adcp = adcp_all, bathy = bathy_df)',
      '',
      '# Option B: pass a list \u2014 auto-stacked inside merge_sensor_data()',
      'survey <- merge_sensor_data(',
      '  adcp  = list(adcp_jan, adcp_feb, adcp_march),',
      '  bathy = bathy_df',
      ')'
    ))
    cli::cli_inform(c(
      " ",
      "Overlapping cells are averaged across runs (noise reduction).",
      "Non-overlapping cells are all kept (extends coverage).",
      " ",
      "Not every sensor needs to cover the same area. Cells that only",
      "one sensor reached will carry NA for the missing variables \u2014",
      "{.fn predict_oyster} skips missing variables automatically.",
      " "
    ))

    cli::cli_rule()
  }

  # ---- Tides -----------------------------------------------------------------
  if (show_all || topic == "tides") {
    cli::cli_h2("Tidal height correction")

    cli::cli_inform(c(
      "Survey depths are recorded relative to the water surface at the time",
      "of survey. Without correction, a site surveyed at high water appears",
      "deeper than the same site at low water. Correcting to Chart Datum (CD)",
      "removes this bias before the depth variable is scored.",
      " ",
      "Formula: depth_CD = depth_surveyed + tidal_height_above_CD",
      " "
    ))

    cli::cli_h3("Automatic correction (recommended)")
    cli::cli_inform(c(
      "{.fn auto_tidal_correct} finds the nearest standard port (from 31",
      "UK/European ports), predicts tidal heights for each survey timestamp",
      "using embedded 5-constituent harmonic models (M2, S2, N2, K1, O1),",
      "and applies the correction row-by-row. Accuracy is typically \u00b10.3\u20130.5 m.",
      " "
    ))
    cli::cli_code(c(
      '# Automatic \u2014 finds nearest port, predicts heights, corrects depths',
      'survey <- auto_tidal_correct(survey, datetime_col = "date")',
      '',
      '# Tighten the distance threshold (default is 75 km)',
      'survey <- auto_tidal_correct(survey, max_port_dist_km = 40)',
      '',
      '# Inspect the predicted tidal heights that were used',
      'range(survey$tidal_height_pred_m)'
    ))
    cli::cli_inform(c(
      " ",
      "Nearest port and predicted height range are printed automatically.",
      " "
    ))

    cli::cli_h3("Manual correction (if you have official tide table values)")
    cli::cli_code(c(
      '# Single reading from a tide table (e.g. 1.4 m at survey time)',
      'survey <- correct_to_chart_datum(survey, tidal_height_m = 1.4)',
      '',
      '# Per-row heights from a tide gauge time series',
      'survey <- correct_to_chart_datum(survey,',
      '  tidal_height_m = tide_gauge_df$height_m)',
      '',
      '# Port datum reference (sanity check on your tidal height value)',
      'tidal_port_info("oban")'
    ))
    cli::cli_inform(c(
      " ",
      "UK official tidal heights: {.url https://easytide.ukho.gov.uk}",
      " "
    ))

    cli::cli_rule()
  }

  # ---- STEP 4: Predict -------------------------------------------------------
  if (show_all || topic == "predict") {
    cli::cli_h2("Step 4 \u2014 Running the suitability model")

    cli::cli_code(c(
      'result <- predict_oyster(',
      '  data           = survey,',
      '  species        = "ostrea_edulis",',
      '  output_geotiff = "kames_bay_suitability.tif",',
      '  verbose        = TRUE',
      ')'
    ))
    cli::cli_inform(c(
      " ",
      "What you get back:",
      "  {.field suitability}        Weighted score 0\u20131 for each location.",
      "  {.field suitability_class}  \"High\" / \"Moderate\" / \"Low\" / \"Very Low\" / \"Excluded\".",
      "  {.field excluded}           TRUE if the location fails a hard-stop limit",
      "                             (e.g. lethal temperature, critically low oxygen).",
      "  {.field exclusion_reason}   Which variable caused the exclusion.",
      "  {.field score_<variable>}   Per-variable component scores.",
      "  {.field n_layers_scored}    How many variables contributed here.",
      "  {.field data_completeness}  Fraction of the species' variables observed.",
      "  {.field limiting_factors}   Which variables held the score down.",
      "  {.field season}             Detected season from date + latitude.",
      " ",
      "Console output includes:",
      "  \u2022 Suitability class breakdown (High / Moderate / ...)",
      "  \u2022 Top 5 spatially distinct sites for introducing oysters,",
      "    each with lat/lon, score, and patch radius in metres.",
      " ",
      "Key arguments:",
      "  {.arg species}         Species key, latin name, or common name.",
      "                        Run {.code list_species()} to see all options.",
      "  {.arg output_geotiff}  File path for the GeoTIFF, or FALSE to skip.",
      "  {.arg resolution}      Raster cell size in degrees (default 0.0002 \u2248 22 m).",
      "  {.arg contours}        Auto-export contour lines as GeoPackage (default TRUE).",
      "  {.arg verbose}         Print per-variable scoring detail (default FALSE).",
      "  {.arg weighting}       \"rank\" (default) or \"evidence\".",
      "                        See {.code oystermapR_help(\"weighting\")}.",
      " "
    ))

    cli::cli_h3("Filtering results in R")
    cli::cli_code(c(
      '# View top locations',
      'subset(result, suitability_class == "High")',
      '',
      '# Export just the scored table to CSV',
      'write.csv(result, "scored_survey.csv", row.names = FALSE)',
      '',
      '# Retrieve top 5 intro sites as a dataframe',
      'top5 <- oystermapR:::.top_introduction_sites(result)'
    ))
    cli::cli_inform(" ")

    cli::cli_rule()
  }

  # ---- STEP 5: Evidence weighting --------------------------------------------
  if (show_all || topic == "weighting") {
    cli::cli_h2("Step 5 \u2014 Rank weighting vs evidence weighting")

    cli::cli_inform(c(
      "By default predict_oyster() weights each variable by 1/rank and",
      "normalises those weights over the variables PRESENT IN YOUR DATA.",
      " ",
      "That renormalisation is the thing to understand before you read any map",
      "it produces: a survey measuring 4 of a species' 17 scored variables gets",
      "a confident number on 0\u20131 with nothing on it to say the rest was never",
      "observed. Absence and irrelevance look identical. Two more consequences,",
      "both measurable:",
      "  \u2022 A variable that does not vary still absorbs weight. Depth scoring 1.0",
      "    at every cell of a 6\u201310 m survey took a third of the weight and",
      "    diluted the layers that actually discriminated.",
      "  \u2022 A variable that does not reproduce is weighted the same as one that",
      "    does, however badly its two independent estimates agree.",
      " "
    ))

    cli::cli_h3("What evidence weighting does instead")
    cli::cli_inform(c(
      "  weight  =  (1 / rank)  x  reliability  x  spread,  normalised",
      " ",
      "  {.field prior}        1/rank from the published tolerance profile.",
      "  {.field reliability}  Correlation between two independent estimates of",
      "                       that layer, measured on your own survey.",
      "  {.field spread}       Population SD of the layer's SCORE.",
      " ",
      "Reliability MULTIPLIES the biological prior and never replaces it \u2014",
      "weighting on measured reliability alone promotes whatever the instrument",
      "measures well, which is circular. A layer with no measured reliability",
      "gets no weight at all: nothing enters the score on the strength of an",
      "argument alone. A layer that scores the same everywhere also gets no",
      "weight \u2014 it says nothing about where WITHIN THIS SURVEY conditions",
      "differ. Zero weight means \"did not discriminate here\", never",
      "\"does not matter\".",
      " "
    ))

    cli::cli_h3("Measuring reliability")
    cli::cli_code(c(
      '# Where the survey has repeat coverage \u2014 line against line, pass',
      '# against pass \u2014 measure it directly:',
      'rel <- c(',
      '  depth = reliability_from_replicates(depth_pass1, depth_pass2),',
      '  slope = reliability_from_replicates(slope_pass1, slope_pass2)',
      ')',
      '',
      '# Where a layer has no repeat coverage, inject its MEASURED error.',
      '# sigma = between-pass disagreement / sqrt(2), NOT the within-cell',
      '# standard error (which ignores errors systematic within a pass).',
      'rel["bpi"] <- reliability_from_perturbation(',
      '  depth_grid, sigma = 0.308 / sqrt(2),',
      '  derive = function(z) bpi(z, scale = 10),   # propagate through it',
      '  seed   = 1                                 # weights must reproduce',
      ')'
    ))
    cli::cli_inform(" ")

    cli::cli_h3("Running it")
    cli::cli_code(c(
      'ev <- predict_oyster(',
      '  data             = survey,',
      '  species          = "ostrea_edulis",',
      '  weighting        = "evidence",',
      '  reliability      = rel,',
      '  attempted_failed = "shear_stress"   # instrumented, unusable data',
      ')',
      '',
      'attr(ev, "evidence_weights")     # the derivation, layer by layer',
      'attr(ev, "evidence_coverage")    # how much of the evidence base you have',
      'cat(weight_table_md(ev), sep = "\\n")   # both, as markdown for a methods section'
    ))
    cli::cli_inform(c(
      " ",
      "Extra columns you get back:",
      "  {.field suitability_rank}   What rank weighting would have said.",
      "  {.field suitability_class_rank}  ... and its class, for comparison.",
      "  {.field confidence}         Per-location confidence 0\u20131. Defaults to the",
      "                             share of the evidence weight observed there.",
      "  {.field suitability_class}  May also take the value \"Insufficient data\".",
      " ",
      "Coverage is reported once at survey level and never multiplied into the",
      "per-cell score \u2014 doing that would push every location below any class",
      "threshold on a low-coverage survey. Locations below {.arg confidence_floor}",
      "(default 0.5) come back as \"Insufficient data\" rather than being handed a",
      "class they cannot support. The continuous score is still returned for them.",
      " ",
      "Those locations count toward the surveyed area in area_summary() \u2014 that",
      "ground WAS surveyed \u2014 but never toward the suitable area, and they are",
      "dropped from patch analysis and from the top-sites list. A location the",
      "model declined to classify cannot be recommended as a site.",
      " ",
      "The default is unchanged: {.code weighting = \"rank\"} reproduces every",
      "result from 1.5.0 exactly.",
      " "
    ))

    cli::cli_rule()
  }

  # ---- STEP 6: QGIS ----------------------------------------------------------
  if (show_all || topic == "qgis") {
    cli::cli_h2("Step 6 \u2014 Opening outputs in QGIS")

    cli::cli_inform(c(
      "Four files are written automatically when output_geotiff is set:",
      " "
    ))
    cli::cli_inform(c(
      "  {.file <name>.tif}             Raster heatmap \u2014 open this first.",
      "  {.file <name>.qml}             QGIS colour style (orange \u2192 red ramp).",
      "                                Auto-applied when the .qml sits next to the .tif.",
      "  {.file <name>_insufficient.qml} Grey overlay for band 7 \u2014 load the raster a",
      "                                second time and apply this to it.",
      "  {.file <name>_contours.gpkg}   Contour lines at 0.1 suitability intervals.",
      " "
    ))

    cli::cli_h3("The seven bands")
    cli::cli_inform(c(
      "  1  suitability        Continuous score 0\u20131. The heatmap.",
      "  2  excluded_mask      1 = hard-excluded by a threshold.",
      "  3  n_observations     Survey points per cell.",
      "  4  dist_to_nearest_m  Distance to the nearest real observation.",
      "  5  n_layers_scored    Variables contributing per cell.",
      "  6  confidence         Per-location confidence (evidence weighting only).",
      "  7  insufficient_mask  1 = the model declined to classify this location.",
      " ",
      "Bands 3, 4, 6 and 7 say how much to trust band 1. Band 1 will happily",
      "interpolate a confident-looking score across water nobody surveyed \u2014 style",
      "band 4 and mask beyond a few point-spacings before showing anyone the map.",
      " "
    ))

    cli::cli_h3("Loading in QGIS")
    cli::cli_inform(c(
      "  1. Layer \u2192 Add Layer \u2192 Add Raster Layer \u2192 select the .tif",
      "     The orange heatmap style loads automatically.",
      "  2. Drag the _contours.gpkg onto the map.",
      "     Right-click the layer \u2192 Properties \u2192 Labels \u2192 label with the {.field level} field.",
      "  3. Add a basemap: Web \u2192 QuickMapServices \u2192 Google Satellite.",
      " "
    ))

    cli::cli_h3("Re-exporting with different settings")
    cli::cli_code(c(
      '# Finer resolution (slower but sharper)',
      'export_geotiff(result, "map_fine.tif", resolution = 0.0001)',
      '',
      '# Wider IDW radius (fills more gaps between transect lines)',
      'export_geotiff(result, "map_wide.tif", idw_max_dist = 0.008)',
      '',
      '# Contour lines only (from an existing .tif)',
      'export_contours("kames_bay_suitability.tif", interval = 0.1)'
    ))
    cli::cli_inform(" ")

    cli::cli_rule()
  }

  # ---- Species ---------------------------------------------------------------
  if (show_all || topic == "species") {
    cli::cli_h2("Species reference")

    cli::cli_inform(c(
      "List all supported species and their scoring parameters:"
    ))
    cli::cli_code(c(
      'list_species()',
      '',
      '# Full tolerance parameters for a species',
      'get_species_tolerances("ostrea_edulis")'
    ))
    cli::cli_inform(c(
      " ",
      "Currently supported:",
      "  {.val ostrea_edulis}   Ostrea edulis \u2014 European Flat Oyster",
      "                        Tolerances from Pogoda et al. 2023 (Aquatic Conservation).",
      " ",
      "Scoring variables (ranked by ecological importance):",
      "  Rank 1  temperature         \u2014 primary driver; hard exclusion limits apply",
      "  Rank 1  salinity            \u2014 primary driver; hard exclusion limits apply",
      "  Rank 1  dissolved_oxygen    \u2014 hard exclusion below 4 mg/L",
      "  Rank 2  depth               \u2014 optimal 1\u201310 m",
      "  Rank 3  current_velocity    \u2014 food delivery and sediment clearance",
      "  Rank 3  chlorophyll_a       \u2014 food availability",
      "  Rank 4  substrate_hardness  \u2014 spat settlement surface",
      "  Rank 4  slope               \u2014 stability for shell development",
      "  Rank 5  turbidity           \u2014 feeding and light interference",
      "  Rank 5  roughness           \u2014 microhabitat complexity",
      "  Rank 6  sediment_type       \u2014 categorical; penalises deep mud",
      "  Rank 6  benthic_communities \u2014 existing ecological context",
      " ",
      "Missing variables are skipped; weights are re-normalised across",
      "whatever variables are present in your dataset.",
      " "
    ))

    cli::cli_rule()
  }

  # ---- Footer ----------------------------------------------------------------
  if (show_all) {
    cli::cli_inform(c(
      "Quick reference:",
      "  {.code oystermapR_help(\"sensors\")}  \u2014 reading raw sensor files",
      "  {.code oystermapR_help(\"merge\")}    \u2014 combining sensor datasets",
      "  {.code oystermapR_help(\"tides\")}    \u2014 tidal height correction",
      "  {.code oystermapR_help(\"predict\")}  \u2014 running the model",
      "  {.code oystermapR_help(\"qgis\")}     \u2014 QGIS output and visualisation",
      "  {.code oystermapR_help(\"species\")}  \u2014 species and scoring variables",
      " "
    ))
  }

  invisible(NULL)
}
