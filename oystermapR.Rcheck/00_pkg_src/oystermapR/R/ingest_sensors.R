# =============================================================================
# Sensor Data Ingestion for oystermapR
#
# Functions for reading raw sensor outputs and combining them into a single
# spatially-averaged dataset ready for predict_oyster().
#
# Supported sensors:
#   read_nortek_adcp()   \u2014 Nortek Signature 500 (merged CSV format)
#   read_generic_csv()   \u2014 Generic CSV with flexible column mapping
#                          (Lowrance BioBase, CTD probes, bathymetric sonar, etc.)
#   merge_sensor_data()  \u2014 Spatially combine outputs from multiple sensors
# =============================================================================


# =============================================================================
# NORTEK ADCP READER
# =============================================================================

#' Read and process a Nortek Signature 500 ADCP CSV file
#'
#' @description
#' Reads a raw merged Nortek Signature 500 ADCP CSV, automatically detects
#' and excludes depth bins contaminated by acoustic sidelobe interference,
#' computes current velocity and estimated bed shear stress from the deepest
#' clean bin, and returns a spatially averaged dataframe ready to merge with
#' other sensor data via [merge_sensor_data()].
#'
#' **Sidelobe detection:** Each bin is tested independently. A bin is flagged
#' as contaminated if more than `sidelobe_threshold` fraction of its speed
#' readings exceed `max_plausible_speed`. Once a bin is flagged, all deeper
#' bins are also flagged (contamination propagates downward).
#'
#' **Shear stress accuracy:** Bed shear stress (`tau = rho * Cd * U_bed^2`)
#' is most accurate when computed from near-bed velocity. If near-bed bins are
#' contaminated and only near-surface bins remain, a warning is issued and the
#' `shear_stress_quality` column is set to `"low"`. If two or more clean bins
#' exist, the deepest clean bin is used and quality is `"moderate"` or
#' `"good"`.
#'
#' @param file Character. Path to the Nortek merged CSV file.
#' @param spatial_res Integer. Decimal places for lat/lon binning (default `4`,
#'   \u224811 m cells at 56\u00b0N). Reduce to `3` (~111 m) for coarser surveys.
#' @param min_obs Integer. Minimum number of raw ensembles per spatial cell
#'   to include in output (default `5`). Cells with fewer observations are
#'   dropped as unreliable.
#' @param max_plausible_speed Numeric. Speed in m/s above which a reading is
#'   considered a sidelobe spike (default `1.5` m/s \u2014 appropriate for sheltered
#'   coastal/loch environments). Increase to `2.5` for open coast surveys.
#' @param sidelobe_threshold Numeric. Fraction of readings in a bin that must
#'   exceed `max_plausible_speed` before the bin is flagged as contaminated
#'   (default `0.10`, i.e. 10%).
#' @param rho Numeric. Seawater density in kg/m\u00b3 (default `1025`).
#' @param Cd Numeric. Drag coefficient for bed shear stress calculation
#'   (default `0.002`, typical for mixed sandy/rocky coastal seabed).
#' @param verbose Logical. Print processing summary (default `TRUE`).
#'
#' @return A dataframe with one row per spatial cell containing:
#'   - `lat`, `lon` \u2014 cell centroid coordinates
#'   - `date` \u2014 earliest observation date in cell (character, `"YYYY-MM-DD"`)
#'   - `current_velocity` \u2014 mean speed from deepest clean bin (m/s)
#'   - `current_velocity_sd` \u2014 standard deviation within cell (m/s)
#'   - `current_velocity_p95` \u2014 95th percentile speed (m/s)
#'   - `shear_stress` \u2014 estimated bed shear stress (N/m\u00b2)
#'   - `shear_stress_quality` \u2014 `"good"`, `"moderate"`, or `"low"`
#'   - `n_ensembles` \u2014 raw ensembles averaged into this cell
#'   - `bins_used` \u2014 which velocity bins contributed (e.g. `"bin1"`)
#'   - `bins_excluded` \u2014 which bins were removed as contaminated
#'
#' @export
#' @examples
#' \dontrun{
#' adcp <- read_nortek_adcp("S104456A008_AWE_Melfort_merged.csv")
#' print(adcp)
#' }
read_nortek_adcp <- function(file,
                             spatial_res          = 4L,
                             min_obs              = 5L,
                             max_plausible_speed  = 1.5,
                             sidelobe_threshold   = 0.10,
                             rho                  = 1025,
                             Cd                   = 0.002,
                             verbose              = TRUE) {

  if (!file.exists(file)) cli::cli_abort("File not found: {.file {file}}")

  if (verbose) cli::cli_inform("Reading Nortek ADCP data from {.file {file}}...")
  raw <- utils::read.csv(file, stringsAsFactors = FALSE)

  n_raw <- nrow(raw)
  if (verbose) cli::cli_inform("  {n_raw} ensembles loaded.")

  # ---- Detect bin structure ---------------------------------------------------
  # Find all VelE_binN columns to determine how many bins are present
  vel_e_cols <- sort(grep("^VelE_bin", names(raw), value = TRUE))
  vel_n_cols <- sort(grep("^VelN_bin", names(raw), value = TRUE))
  n_bins     <- length(vel_e_cols)

  if (n_bins == 0) {
    cli::cli_abort(c(
      "No velocity bin columns found.",
      "i" = "Expected columns like {.val VelE_bin1}, {.val VelN_bin1}, etc.",
      "i" = "Column names found: {.val {names(raw)}}"
    ))
  }
  if (verbose) cli::cli_inform("  {n_bins} velocity bin{?s} detected.")

  # ---- Compute speed magnitude per bin ----------------------------------------
  speed_matrix <- matrix(NA_real_, nrow = n_raw, ncol = n_bins)
  bin_labels   <- paste0("bin", seq_len(n_bins))

  for (i in seq_len(n_bins)) {
    e <- suppressWarnings(as.numeric(raw[[vel_e_cols[i]]]))
    n <- suppressWarnings(as.numeric(raw[[vel_n_cols[i]]]))
    speed_matrix[, i] <- sqrt(e^2 + n^2)
  }
  colnames(speed_matrix) <- bin_labels

  # ---- Sidelobe detection -----------------------------------------------------
  contaminated <- .detect_sidelobe_bins(
    speed_matrix         = speed_matrix,
    max_plausible_speed  = max_plausible_speed,
    sidelobe_threshold   = sidelobe_threshold
  )

  clean_bins  <- which(!contaminated)
  dirty_bins  <- which(contaminated)

  if (length(clean_bins) == 0) {
    cli::cli_abort(c(
      "All {n_bins} bin{?s} flagged as sidelobe-contaminated.",
      "i" = "Try increasing {.arg max_plausible_speed} (currently {max_plausible_speed} m/s).",
      "i" = "Or check that the CSV contains valid Nortek velocity data."
    ))
  }

  # ---- Shear stress quality assessment ----------------------------------------
  # Quality tiers based on which clean bin is the deepest:
  #   "good"     \u2014 deepest clean bin is in the lower half of the profile
  #   "moderate" \u2014 deepest clean bin is in the upper half but not bin1
  #   "low"      \u2014 only bin1 (surface) is clean
  deepest_clean   <- max(clean_bins)
  shear_quality   <- ifelse(
    deepest_clean == 1, "low",
    ifelse(deepest_clean <= ceiling(n_bins / 2), "moderate", "good")
  )

  # ---- Build warnings ---------------------------------------------------------
  if (length(dirty_bins) > 0 && verbose) {
    cli::cli_warn(c(
      "!" = "Sidelobe contamination detected in {length(dirty_bins)} of {n_bins} bin{?s}:",
      " " = "  Contaminated: {.val {bin_labels[dirty_bins]}}",
      " " = "  Clean:        {.val {bin_labels[clean_bins]}}",
      " " = "  Velocity computed from deepest clean bin: {.val {bin_labels[deepest_clean]}}"
    ))
  }

  if (shear_quality == "low" && verbose) {
    cli::cli_warn(c(
      "!" = "Shear stress accuracy is LOW.",
      " " = "  Only near-surface bin ({.val {bin_labels[1]}}) is free of contamination.",
      " " = "  Bed shear stress requires near-bed velocity \u2014 using surface velocity",
      " " = "  will UNDERESTIMATE true bed shear stress.",
      " " = "  Treat {.field shear_stress} values as lower bounds only.",
      "i" = "  To improve: increase ADCP blanking distance or reduce bin size in",
      "i" = "  Nortek deployment settings to get cleaner near-bed bins."
    ))
  } else if (shear_quality == "moderate" && verbose) {
    cli::cli_warn(c(
      "!" = "Shear stress accuracy is MODERATE.",
      " " = "  Deepest clean bin ({.val {bin_labels[deepest_clean]}}) is in the upper",
      " " = "  half of the water column profile.",
      " " = "  Values are better than surface-only but may underestimate bed stress."
    ))
  }

  # ---- Use deepest clean bin for primary velocity + shear ---------------------
  primary_speed <- speed_matrix[, deepest_clean]

  # Clip any residual spikes even in clean bins
  primary_speed[primary_speed > max_plausible_speed] <- NA_real_

  # ---- Extract date -----------------------------------------------------------
  date_col <- .find_col_any(raw, c("adcp_time_utc", "adcp_time_synced_utc", "datetime",
                                   "time_utc", "timestamp", "time"))
  if (!is.null(date_col)) {
    raw$date_str <- substr(raw[[date_col]], 1, 10)
  } else {
    raw$date_str <- NA_character_
    cli::cli_warn("No time column found; date will be NA in output.")
  }

  # ---- Spatial binning --------------------------------------------------------
  raw$lat_bin   <- round(as.numeric(raw$gps_lat), spatial_res)
  raw$lon_bin   <- round(as.numeric(raw$gps_lon), spatial_res)
  raw$spd_clean <- primary_speed
  raw$tau       <- rho * Cd * primary_speed^2

  result <- raw |>
    dplyr::group_by(lat_bin, lon_bin) |>
    dplyr::summarise(
      lat                  = mean(as.numeric(gps_lat),  na.rm = TRUE),
      lon                  = mean(as.numeric(gps_lon),  na.rm = TRUE),
      date                 = dplyr::first(stats::na.omit(date_str)),
      current_velocity     = mean(spd_clean, na.rm = TRUE),
      current_velocity_sd  = stats::sd(spd_clean, na.rm = TRUE),
      current_velocity_p95 = unname(stats::quantile(spd_clean, 0.95, na.rm = TRUE)),
      shear_stress         = mean(tau, na.rm = TRUE),
      n_ensembles          = dplyr::n(),
      .groups = "drop"
    ) |>
    dplyr::filter(n_ensembles >= min_obs,
                  !is.na(current_velocity)) |>
    dplyr::mutate(
      shear_stress_quality = shear_quality,
      bins_used            = bin_labels[deepest_clean],
      bins_excluded        = if (length(dirty_bins) > 0) {
                               paste(bin_labels[dirty_bins], collapse = ", ")
                             } else {
                               "none"
                             }
    )

  if (verbose) {
    cli::cli_inform(c(
      "v" = "ADCP processing complete.",
      "i" = "  Spatial cells: {nrow(result)} (>= {min_obs} ensembles each)",
      "i" = "  Velocity bin used: {bin_labels[deepest_clean]} (deepest clean)",
      "i" = "  current_velocity: mean={round(mean(result$current_velocity,na.rm=TRUE),3)} m/s, max={round(max(result$current_velocity,na.rm=TRUE),3)} m/s",
      "i" = "  shear_stress: mean={round(mean(result$shear_stress,na.rm=TRUE),5)} N/m\u00b2 (quality: {shear_quality})"
    ))
  }

  result
}


#' Internal: detect sidelobe-contaminated bins
#'
#' @param speed_matrix Numeric matrix, rows = ensembles, cols = bins (shallow to deep).
#' @param max_plausible_speed Numeric. Speed threshold in m/s.
#' @param sidelobe_threshold Numeric. Contamination fraction threshold [0,1].
#' @return Logical vector, length = n_bins. TRUE = contaminated.
#' @keywords internal
.detect_sidelobe_bins <- function(speed_matrix, max_plausible_speed, sidelobe_threshold) {

  n_bins       <- ncol(speed_matrix)
  contaminated <- logical(n_bins)

  for (i in seq_len(n_bins)) {
    spd         <- speed_matrix[, i]
    valid       <- spd[!is.na(spd)]
    if (length(valid) == 0) {
      contaminated[i] <- TRUE
      next
    }
    frac_bad    <- mean(valid > max_plausible_speed)
    contaminated[i] <- frac_bad > sidelobe_threshold
  }

  # Propagate contamination downward: once a bin is bad, all deeper ones are too
  first_bad <- which(contaminated)
  if (length(first_bad) > 0) {
    contaminated[min(first_bad):n_bins] <- TRUE
  }

  contaminated
}


# =============================================================================
# GENERIC SENSOR CSV READER
# =============================================================================

#' Read a generic sensor CSV with flexible column mapping
#'
#' @description
#' Reads any sensor CSV (Lowrance BioBase, CTD/probe logger, bathymetric sonar,
#' sidescan export, etc.) and maps its columns to the standard oystermapR
#' variable names. Performs spatial averaging at a specified resolution.
#'
#' @param file Character. Path to the CSV file.
#' @param col_map Named character vector mapping oystermapR variable names to
#'   column names in the CSV. Only variables you want to extract need to be
#'   listed. Example:
#'   ```r
#'   col_map = c(
#'     lat                 = "Latitude",
#'     lon                 = "Longitude",
#'     depth               = "Depth_m",
#'     substrate_hardness  = "Hardness",
#'     temperature         = "Temp_C"
#'   )
#'   ```
#'   If `col_map` is `NULL` (default), the function attempts automatic column
#'   name matching using built-in synonym lists (case-insensitive).
#' @param spatial_res Integer. Decimal places for spatial binning (default `4`).
#' @param min_obs Integer. Minimum observations per cell (default `3`).
#' @param categorical_cols Character vector. Column names (after mapping) that
#'   should be treated as categorical \u2014 mode (most common value) is returned
#'   instead of mean. Default: `c("sediment_type", "benthic_communities",
#'   "biotope", "fishing_intensity")`.
#' @param verbose Logical. Print a summary after loading (default `TRUE`).
#'
#' @return A spatially averaged dataframe using oystermapR standard column names.
#'
#' @export
#' @examples
#' \dontrun{
#' # Lowrance BioBase export (automatic column matching)
#' biobase <- read_generic_csv("biobase_export.csv")
#'
#' # With explicit column mapping
#' probe <- read_generic_csv(
#'   "ctd_data.csv",
#'   col_map = c(lat="Lat", lon="Lon", temperature="Temp", salinity="Sal",
#'               dissolved_oxygen="DO_mgl")
#' )
#' }
read_generic_csv <- function(file,
                             col_map         = NULL,
                             spatial_res     = 4L,
                             min_obs         = 3L,
                             categorical_cols = c("sediment_type",
                                                  "benthic_communities",
                                                  "biotope",
                                                  "fishing_intensity"),
                             verbose         = TRUE) {

  if (!file.exists(file)) cli::cli_abort("File not found: {.file {file}}")

  raw <- utils::read.csv(file, stringsAsFactors = FALSE)
  if (verbose) cli::cli_inform("Reading {.file {file}}: {nrow(raw)} rows, {ncol(raw)} columns.")

  # ---- Apply column mapping ---------------------------------------------------
  if (is.null(col_map)) {
    raw <- .auto_map_columns(raw, verbose = verbose)
  } else {
    # User-supplied map: rename matching columns
    for (target_name in names(col_map)) {
      src <- col_map[[target_name]]
      if (src %in% names(raw)) {
        names(raw)[names(raw) == src] <- target_name
      } else {
        cli::cli_warn("Column {.val {src}} not found in {.file {file}}; {.val {target_name}} will be NA.")
      }
    }
  }

  # ---- Check for required coordinate columns ----------------------------------
  if (!all(c("lat", "lon") %in% names(raw))) {
    cli::cli_abort(c(
      "Could not find latitude/longitude columns.",
      "i" = "Supply a {.arg col_map} with {.val lat} and {.val lon} entries.",
      "i" = "Columns available: {.val {names(raw)}}"
    ))
  }

  raw$lat <- suppressWarnings(as.numeric(raw$lat))
  raw$lon <- suppressWarnings(as.numeric(raw$lon))
  raw     <- raw[!is.na(raw$lat) & !is.na(raw$lon), ]

  # ---- Date column ------------------------------------------------------------
  if ("date" %in% names(raw)) {
    raw$date <- as.character(as.Date(raw$date))
  }

  # ---- Spatial binning --------------------------------------------------------
  raw$lat_bin <- round(raw$lat, spatial_res)
  raw$lon_bin <- round(raw$lon, spatial_res)

  # Identify numeric vs categorical columns (from oystermapR variables present)
  oyster_vars <- c("temperature", "salinity", "dissolved_oxygen", "depth",
                   "current_velocity", "shear_stress", "chlorophyll_a",
                   "turbidity", "slope", "roughness", "substrate_hardness",
                   "sediment_type", "benthic_communities", "biotope",
                   "fishing_intensity")
  present_vars <- intersect(oyster_vars, names(raw))
  num_vars     <- setdiff(present_vars, categorical_cols)
  cat_vars     <- intersect(present_vars, categorical_cols)

  # Numeric: force to numeric
  for (v in num_vars) {
    raw[[v]] <- suppressWarnings(as.numeric(raw[[v]]))
  }

  # Build grouping expressions
  agg_list <- list(
    lat  = ~ mean(lat,  na.rm = TRUE),
    lon  = ~ mean(lon,  na.rm = TRUE),
    n_obs = ~ dplyr::n()
  )
  if ("date" %in% names(raw)) {
    agg_list$date <- ~ dplyr::first(stats::na.omit(date))
  }

  # Group and summarise
  result <- raw |>
    dplyr::group_by(lat_bin, lon_bin) |>
    dplyr::summarise(
      lat   = mean(lat,  na.rm = TRUE),
      lon   = mean(lon,  na.rm = TRUE),
      n_obs = dplyr::n(),
      dplyr::across(dplyr::all_of(num_vars),
                    ~ mean(.x, na.rm = TRUE)),
      dplyr::across(dplyr::all_of(cat_vars),
                    ~ .mode(.x)),
      .groups = "drop"
    ) |>
    dplyr::filter(n_obs >= min_obs) |>
    dplyr::select(-lat_bin, -lon_bin)

  if ("date" %in% names(raw)) {
    # Add date back
    date_lookup <- raw |>
      dplyr::mutate(lat_bin = round(lat, spatial_res),
                    lon_bin = round(lon, spatial_res)) |>
      dplyr::group_by(lat_bin, lon_bin) |>
      dplyr::summarise(date = dplyr::first(stats::na.omit(date)), .groups = "drop")
    result <- dplyr::left_join(
      result,
      date_lookup |>
        dplyr::mutate(lat = round(lat_bin, spatial_res),
                      lon = round(lon_bin, spatial_res)) |>
        dplyr::select(lat, lon, date),
      by = c("lat", "lon")
    )
  }

  if (verbose) {
    cli::cli_inform(c(
      "v" = "{nrow(result)} spatial cells after averaging.",
      "i" = "Variables extracted: {.val {present_vars}}"
    ))
  }

  result
}


#' Mode helper (most common value in a vector)
#' @keywords internal
.mode <- function(x) {
  x <- x[!is.na(x) & x != ""]
  if (length(x) == 0) return(NA_character_)
  tbl <- table(x)
  names(tbl)[which.max(tbl)]
}

#' Automatic column name matching for common sensor exports
#' @keywords internal
.auto_map_columns <- function(df, verbose = TRUE) {
  lwr <- tolower(names(df))

  # Synonym map: target name -> recognised source names (lowercase)
  synonyms <- list(
    lat                 = c("lat", "latitude", "gps_lat", "y", "lat_dd", "latitude_dd"),
    lon                 = c("lon", "lng", "longitude", "gps_lon", "x", "lon_dd",
                            "longitude_dd", "long"),
    date                = c("date", "datetime", "timestamp", "time", "adcp_time_utc",
                            "adcp_time_synced_utc", "time_utc", "survey_date"),
    temperature         = c("temperature", "temp", "temp_c", "water_temp",
                            "temperature_c", "sst"),
    salinity            = c("salinity", "sal", "salinity_psu", "psu", "sal_psu"),
    dissolved_oxygen    = c("dissolved_oxygen", "do", "do_mgl", "oxygen",
                            "do_mg_l", "o2_mgl"),
    depth               = c("depth", "depth_m", "depth_ft", "water_depth",
                            "bottom_depth"),
    current_velocity    = c("current_velocity", "velocity", "current", "u_mean",
                            "speed", "flow_speed"),
    shear_stress        = c("shear_stress", "tau", "bed_shear", "shear",
                            "bottom_stress"),
    chlorophyll_a       = c("chlorophyll_a", "chla", "chl_a", "chlorophyll",
                            "chl", "chloro"),
    turbidity           = c("turbidity", "ntu", "turb", "turbidity_ntu"),
    slope               = c("slope", "slope_deg", "gradient"),
    roughness           = c("roughness", "rugosity", "rugosity_index",
                            "roughness_index"),
    substrate_hardness  = c("substrate_hardness", "hardness", "bottom_hardness",
                            "hard", "hardness_index"),
    sediment_type       = c("sediment_type", "sediment", "substrate_type",
                            "substrate", "bottom_type", "sed_type"),
    benthic_communities = c("benthic_communities", "benthic", "community",
                            "benthic_community", "bio_community"),
    biotope             = c("biotope", "biotopes", "habitat", "habitat_type"),
    fishing_intensity   = c("fishing_intensity", "fishing", "fishing_observed",
                            "trawling", "fishing_activity")
  )

  mapped <- character(0)
  for (target in names(synonyms)) {
    match_idx <- which(lwr %in% synonyms[[target]])
    if (length(match_idx) > 0 && !target %in% names(df)) {
      names(df)[match_idx[1]] <- target
      mapped <- c(mapped, target)
    }
  }

  if (verbose && length(mapped) > 0) {
    cli::cli_inform("  Auto-mapped columns: {.val {mapped}}")
  }

  df
}


# =============================================================================
# MULTI-SURVEY STACKER
# =============================================================================

#' Stack multiple survey runs from the same sensor type
#'
#' @description
#' Combines two or more dataframes from the same sensor (e.g. an ADCP run on
#' Monday and again on Thursday) by row-binding them and re-averaging
#' overlapping cells onto a common spatial grid. Non-overlapping cells are
#' kept as-is.
#'
#' This is the right tool when you have repeated surveys of the same area \u2014
#' rather than picking one run, overlapping cells are averaged across all runs
#' which improves noise reduction. Non-overlapping cells (different survey
#' extents) are preserved.
#'
#' After stacking, pass the result into [merge_sensor_data()] alongside your
#' other sensor datasets as usual.
#'
#' @param ... Two or more dataframes of the same sensor type. Each must contain
#'   `lat` and `lon` columns.
#' @param spatial_res Integer. Decimal places for the common spatial grid
#'   (default `4`, \u224811 m at 56\u00b0N).
#' @param verbose Logical. Print a summary (default `TRUE`).
#'
#' @return A single spatially averaged dataframe combining all input surveys.
#'
#' @export
#' @examples
#' \dontrun{
#' # Stack two ADCP runs before merging with other sensors
#' adcp_mon <- read_nortek_adcp("survey_monday.csv")
#' adcp_thu <- read_nortek_adcp("survey_thursday.csv")
#' adcp_all <- stack_surveys(adcp_mon, adcp_thu)
#'
#' survey <- merge_sensor_data(adcp = adcp_all, bathy = bathy_df)
#' }
stack_surveys <- function(..., spatial_res = 4L, verbose = TRUE) {

  dfs <- list(...)
  if (length(dfs) < 2) cli::cli_abort("Supply at least 2 dataframes to stack.")

  for (i in seq_along(dfs)) {
    if (!is.data.frame(dfs[[i]])) {
      cli::cli_abort("Argument {i} is not a dataframe. Each argument must be a sensor dataframe.")
    }
    if (!all(c("lat", "lon") %in% names(dfs[[i]]))) {
      cli::cli_abort("Dataframe {i} is missing {.val lat} or {.val lon} columns.")
    }
  }

  n_rows_in <- sapply(dfs, nrow)
  if (verbose) {
    cli::cli_inform(
      "Stacking {length(dfs)} survey{?s}: {paste(n_rows_in, collapse=' + ')} = {sum(n_rows_in)} rows total."
    )
  }

  combined <- do.call(rbind, lapply(dfs, function(d) {
    # Normalise: keep only columns present across all datasets (or fill NA)
    d
  }))

  # ---- Spatial re-averaging ---------------------------------------------------
  combined$._lat_bin <- round(combined$lat, spatial_res)
  combined$._lon_bin <- round(combined$lon, spatial_res)

  # Columns to aggregate
  skip_cols <- c("._lat_bin", "._lon_bin", "lat", "lon")
  cat_flag  <- c("sediment_type", "benthic_communities", "biotope",
                 "fishing_intensity", "shear_stress_quality",
                 "bins_used", "bins_excluded", "date", "season")

  all_cols  <- setdiff(names(combined), skip_cols)
  num_cols  <- setdiff(all_cols, cat_flag)
  cat_cols  <- intersect(all_cols, cat_flag)

  for (v in num_cols) combined[[v]] <- suppressWarnings(as.numeric(combined[[v]]))

  result <- combined |>
    dplyr::group_by(._lat_bin, ._lon_bin) |>
    dplyr::summarise(
      lat = mean(lat, na.rm = TRUE),
      lon = mean(lon, na.rm = TRUE),
      dplyr::across(dplyr::all_of(intersect(num_cols, names(combined))),
                    ~ mean(.x, na.rm = TRUE)),
      dplyr::across(dplyr::all_of(intersect(cat_cols, names(combined))),
                    ~ .mode(.x)),
      .groups = "drop"
    ) |>
    dplyr::select(-`._lat_bin`, -`._lon_bin`)

  if (verbose) {
    n_overlap <- sum(n_rows_in) - nrow(result)
    cli::cli_inform(c(
      "v" = "{nrow(result)} unique spatial cells after stacking.",
      "i" = "  {n_overlap} overlapping cell{?s} averaged across survey runs."
    ))
  }

  result
}


# =============================================================================
# MULTI-SENSOR SPATIAL MERGE
# =============================================================================

#' Merge multiple sensor datasets into a single survey table
#'
#' @description
#' Takes any number of sensor dataframes (from [read_nortek_adcp()],
#' [read_generic_csv()], or custom sources) and merges them spatially into a
#' single table ready for [predict_oyster()].
#'
#' Each dataset is first rounded to a common spatial resolution grid. Datasets
#' are then joined using the grid cell key \u2014 cells that exist in multiple
#' datasets are merged; cells that exist in only one dataset carry `NA` for
#' variables from other sensors.
#'
#' @param ... Two or more dataframes to merge. Each must contain `lat` and `lon`
#'   columns. Pass them as named arguments for cleaner messages (e.g.
#'   `adcp = adcp_df, biobase = biobase_df`).
#'
#'   **Multiple runs of the same sensor:** If you have several surveys from the
#'   same sensor type, pass them as a list and they will be automatically
#'   stacked via [stack_surveys()] before merging:
#'   ```r
#'   merge_sensor_data(
#'     adcp    = list(adcp_run1, adcp_run2, adcp_run3),
#'     bathy   = bathy_df,
#'     biobase = biobase_df
#'   )
#'   ```
#' @param spatial_res Integer. Decimal places for the common spatial grid
#'   (default `4`, \u224811 m at 56\u00b0N). Must be the same or coarser than the
#'   resolution used when reading each individual sensor.
#' @param date_source Character. Name of the dataset to take the `date` column
#'   from (default `NULL` \u2014 uses the first dataset that contains a `date`
#'   column).
#' @param verbose Logical. Print a merge summary (default `TRUE`).
#'
#' @return A single dataframe with all oystermapR-compatible columns populated
#'   from their respective sensor sources. Columns that weren't available from
#'   any sensor will be absent from the result \u2014 [predict_oyster()] handles
#'   missing columns gracefully.
#'
#' @export
#' @examples
#' \dontrun{
#' # Single run per sensor
#' adcp    <- read_nortek_adcp("adcp.csv")
#' biobase <- read_generic_csv("biobase.csv")
#' survey  <- merge_sensor_data(adcp = adcp, biobase = biobase)
#'
#' # Multiple ADCP runs (different survey days) \u2014 automatically stacked
#' adcp1  <- read_nortek_adcp("survey_jan.csv")
#' adcp2  <- read_nortek_adcp("survey_feb.csv")
#' survey <- merge_sensor_data(adcp = list(adcp1, adcp2), biobase = biobase)
#'
#' result <- predict_oyster(survey, "ostrea_edulis", output_geotiff = "map.tif")
#' }
merge_sensor_data <- function(...,
                              spatial_res = 4L,
                              date_source = NULL,
                              verbose     = TRUE) {

  datasets   <- list(...)
  data_names <- ...names()
  if (is.null(data_names)) data_names <- paste0("dataset_", seq_along(datasets))
  data_names[data_names == ""] <- paste0("dataset_", which(data_names == ""))

  if (length(datasets) < 2) {
    cli::cli_abort("Supply at least 2 sensor datasets to merge.")
  }

  # ---- Unwrap list-of-dataframes: auto-stack multiple runs of same sensor -----
  datasets <- lapply(seq_along(datasets), function(i) {
    d  <- datasets[[i]]
    nm <- data_names[i]
    if (is.list(d) && !is.data.frame(d)) {
      if (!all(sapply(d, is.data.frame))) {
        cli::cli_abort("Dataset {.val {nm}}: when passing a list, all elements must be dataframes.")
      }
      if (verbose) cli::cli_inform("  {nm}: auto-stacking {length(d)} survey run{?s}...")
      do.call(stack_surveys, c(d, list(spatial_res = spatial_res, verbose = verbose)))
    } else {
      d
    }
  })

  if (verbose) cli::cli_inform("Merging {length(datasets)} sensor dataset{?s}...")

  # ---- Add grid key to each dataset -----------------------------------------
  keyed <- lapply(seq_along(datasets), function(i) {
    df   <- datasets[[i]]
    name <- data_names[i]

    if (!all(c("lat", "lon") %in% names(df))) {
      cli::cli_abort("Dataset {.val {name}} is missing {.val lat} or {.val lon} columns.")
    }

    df$._lat_key <- round(df$lat, spatial_res)
    df$._lon_key <- round(df$lon, spatial_res)

    # Drop internal diagnostic columns that shouldn't pollute the output
    drop_cols <- intersect(names(df),
                           c("lat_bin", "lon_bin", "n_obs", "n_ensembles",
                             "current_velocity_sd", "current_velocity_p95",
                             "shear_stress_quality", "bins_used", "bins_excluded",
                             "heading_deg"))
    df <- df[, !names(df) %in% drop_cols]

    if (verbose) {
      vars <- setdiff(names(df), c("lat", "lon", "date", "._lat_key", "._lon_key"))
      cli::cli_inform("  {name}: {nrow(df)} cells, variables: {.val {vars}}")
    }
    df
  })

  # ---- Full join across all datasets -----------------------------------------
  merged <- keyed[[1]]
  for (i in seq_along(keyed)[-1]) {
    next_df <- keyed[[i]]

    # Identify columns to bring in (exclude ones already present, except key + coords)
    new_cols <- setdiff(names(next_df),
                        c(names(merged), "._lat_key", "._lon_key", "lat", "lon"))

    # Always include lat/lon so rows unique to next_df get coordinates via coalesce
    join_df  <- next_df[, c("._lat_key", "._lon_key", "lat", "lon", new_cols), drop = FALSE]

    merged <- dplyr::full_join(merged, join_df,
                               by = c("._lat_key", "._lon_key"))

    # Re-fill lat/lon from whichever dataset has it for each row
    if ("lat.x" %in% names(merged)) {
      merged$lat <- dplyr::coalesce(merged$lat.x, merged$lat.y)
      merged$lon <- dplyr::coalesce(merged$lon.x, merged$lon.y)
      merged <- merged[, !names(merged) %in% c("lat.x", "lat.y", "lon.x", "lon.y")]
    }
  }

  # ---- Handle date column -----------------------------------------------------
  date_cols <- grep("^date", names(merged), value = TRUE)
  if (length(date_cols) > 1) {
    # Collapse multiple date columns into one
    merged$date <- apply(merged[, date_cols, drop = FALSE], 1,
                         function(r) dplyr::first(stats::na.omit(r)))
    merged <- merged[, !names(merged) %in% setdiff(date_cols, "date")]
  }

  # ---- Clean up key columns ---------------------------------------------------
  merged <- merged[, !names(merged) %in% c("._lat_key", "._lon_key")]

  # Reorder: lat/lon/date first
  priority_cols <- intersect(c("lat", "lon", "date"), names(merged))
  other_cols    <- setdiff(names(merged), priority_cols)
  merged        <- merged[, c(priority_cols, other_cols)]

  if (verbose) {
    filled <- sapply(merged, function(x) mean(!is.na(x))) * 100
    oyster_vars <- c("temperature", "salinity", "dissolved_oxygen", "depth",
                     "current_velocity", "shear_stress", "chlorophyll_a",
                     "turbidity", "slope", "roughness", "substrate_hardness",
                     "sediment_type", "benthic_communities", "biotope")
    present  <- intersect(oyster_vars, names(merged))
    missing  <- setdiff(oyster_vars, names(merged))

    cli::cli_inform(c(
      "v" = "Merged dataset: {nrow(merged)} spatial cells.",
      "i" = "Variables present ({length(present)}): {.val {present}}",
      "i" = "Variables absent ({length(missing)}):  {.val {missing}}",
      "i" = "(Absent variables will be skipped by predict_oyster())"
    ))
  }

  merged
}
