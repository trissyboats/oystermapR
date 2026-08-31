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
#'   approx.11 m cells at 56degrees N). Reduce to `3` (~111 m) for coarser surveys.
#' @param min_obs Integer. Minimum number of raw ensembles per spatial cell
#'   to include in output (default `5`). Cells with fewer observations are
#'   dropped as unreliable.
#' @param max_plausible_speed Numeric. Speed in m/s above which a reading is
#'   considered a sidelobe spike (default `1.5` m/s -- appropriate for sheltered
#'   coastal/loch environments). Increase to `2.5` for open coast surveys.
#' @param sidelobe_threshold Numeric. Fraction of readings in a bin that must
#'   exceed `max_plausible_speed` before the bin is flagged as contaminated
#'   (default `0.10`, i.e. 10%).
#' @param rho Numeric. Seawater density in kg/m^3 (default `1025`).
#' @param Cd Numeric. Drag coefficient for bed shear stress calculation
#'   (default `0.002`, typical for mixed sandy/rocky coastal seabed).
#' @param verbose Logical. Print processing summary (default `TRUE`).
#'
#' @return A dataframe with one row per spatial cell containing:
#'   - `lat`, `lon` -- cell centroid coordinates
#'   - `date` -- earliest observation date in cell (character, `"YYYY-MM-DD"`)
#'   - `current_velocity` -- mean speed from deepest clean bin (m/s)
#'   - `current_velocity_sd` -- standard deviation within cell (m/s)
#'   - `current_velocity_p95` -- 95th percentile speed (m/s)
#'   - `shear_stress` -- estimated bed shear stress (N/m^2)
#'   - `shear_stress_quality` -- `"good"`, `"moderate"`, or `"low"`
#'   - `n_ensembles` -- raw ensembles averaged into this cell
#'   - `bins_used` -- which velocity bins contributed (e.g. `"bin1"`)
#'   - `bins_excluded` -- which bins were removed as contaminated
#'
#' @export
#' @examples
#' adcp_f <- system.file("extdata", "example_bay_adcp.csv", package = "oystermapR")
#' adcp <- read_nortek_adcp(adcp_f, verbose = FALSE)
#' head(adcp[, c("lat", "lon", "current_velocity")])
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
#' @param sidelobe_threshold Numeric. Contamination fraction threshold \[0,1\].
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
#'   should be treated as categorical -- mode (most common value) is returned
#'   instead of mean. Default: `c("sediment_type", "benthic_communities",
#'   "biotope", "fishing_intensity")`.
#' @param verbose Logical. Print a summary after loading (default `TRUE`).
#'
#' @return A spatially averaged dataframe using oystermapR standard column names.
#'
#' @export
#' @examples
#' ctd_f <- system.file("extdata", "example_bay_ctd.csv", package = "oystermapR")
#' ctd <- read_generic_csv(ctd_f, verbose = FALSE)
#' head(ctd[, c("lat", "lon", "temperature", "salinity")])
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
#' This is the right tool when you have repeated surveys of the same area --
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
#'   (default `4`, approx.11 m at 56degrees N).
#' @param verbose Logical. Print a summary (default `TRUE`).
#'
#' @return A single spatially averaged dataframe combining all input surveys.
#'
#' @export
#' @examples
#' adcp_f <- system.file("extdata", "example_bay_adcp.csv", package = "oystermapR")
#' run1 <- read_nortek_adcp(adcp_f, verbose = FALSE)
#' run2 <- read_nortek_adcp(adcp_f, verbose = FALSE)
#' adcp_all <- stack_surveys(run1, run2)
#' nrow(adcp_all)
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
#' are then joined using the grid cell key -- cells that exist in multiple
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
#'   (default `4`, approx.11 m at 56degrees N). Must be the same or coarser than the
#'   resolution used when reading each individual sensor.
#' @param date_source Character. Name of the dataset to take the `date` column
#'   from (default `NULL` -- uses the first dataset that contains a `date`
#'   column).
#' @param verbose Logical. Print a merge summary (default `TRUE`).
#'
#' @return A single dataframe with all oystermapR-compatible columns populated
#'   from their respective sensor sources. Columns that weren't available from
#'   any sensor will be absent from the result -- [predict_oyster()] handles
#'   missing columns gracefully.
#'
#' @export
#' @examples
#' adcp_f <- system.file("extdata", "example_bay_adcp.csv", package = "oystermapR")
#' ctd_f  <- system.file("extdata", "example_bay_ctd.csv",  package = "oystermapR")
#' adcp   <- read_nortek_adcp(adcp_f, verbose = FALSE)
#' ctd    <- read_generic_csv(ctd_f,  verbose = FALSE)
#' survey <- merge_sensor_data(adcp = adcp, ctd = ctd)
#' head(survey[, c("lat", "lon", "current_velocity", "temperature")])
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


# =============================================================================
# read_nortek_aquadopp()
# Nortek Aquadopp Profiler ASCII export (AquaPro software)
# =============================================================================

#' Read a Nortek Aquadopp Profiler ASCII export
#'
#' @description
#' Reads the ASCII velocity export produced by Nortek's **AquaPro** software
#' from an Aquadopp Profiler deployment. The function accepts two common export
#' layouts:
#'
#' - **ENU CSV** -- a single file with named columns for east, north, and up
#'   velocity per depth bin (e.g. `East_1`, `North_1`, `East_2`, ...) plus
#'   optional time, temperature, and pressure columns.
#' - **Separate beam files** -- the companion `.sen` (sensor) file providing
#'   time, temperature, and pressure, and the `.v1`/`.v2` files providing
#'   beam velocities. Supply the `.sen` path to `file` and set
#'   `beam_files = c("survey.v1", "survey.v2", "survey.v3")`.
#'
#' Because Aquadopp instruments are often moored at a single location rather
#' than vessel-mounted, GPS coordinates may not be embedded in the export. In
#' that case supply `lat` and `lon` directly; all output rows receive that
#' fixed position.
#'
#' @param file Character. Path to the AquaPro ENU CSV export, or path to the
#'   `.sen` file when using separate beam files.
#' @param beam_files Character vector. Paths to `.v1`, `.v2`, `.v3` beam
#'   velocity files (optional). If `NULL` (default) the function reads velocity
#'   from named columns in `file`.
#' @param lat Numeric. Fixed latitude for moored deployments (decimal degrees).
#'   Required when GPS columns are absent from the export.
#' @param lon Numeric. Fixed longitude for moored deployments (decimal degrees).
#'   Required when GPS columns are absent from the export.
#' @param spatial_res Integer. Decimal places for lat/lon binning (default `4`).
#'   For moored deployments all ensembles share one position and this parameter
#'   has no practical effect.
#' @param min_obs Integer. Minimum ensembles per grid cell (default `5`).
#' @param max_plausible_speed Numeric. Upper speed threshold for outlier
#'   removal in m/s (default `2.5`; Aquadopp profilers can measure stronger
#'   tidal flows than vessel-mounted ADCPs).
#' @param rho Numeric. Water density in kg/m^3 (default `1025`).
#' @param Cd Numeric. Drag coefficient for bed shear stress (default `0.002`).
#' @param verbose Logical. Print progress messages (default `TRUE`).
#'
#' @return A dataframe with columns `lat`, `lon`, `current_speed`,
#'   `bed_shear_stress`, `shear_stress_quality`, and optionally `temperature`,
#'   `pressure_dbar`, and `date` -- ready for [merge_sensor_data()].
#'
#' @export
#' @examples
#' \dontrun{
#' # Moored deployment (fixed position)
#' adcp <- read_nortek_aquadopp(
#'   "harbour_mouth_aqd.csv",
#'   lat = 56.512, lon = -5.403
#' )
#'
#' # Vessel-mounted with GPS columns in the file
#' adcp <- read_nortek_aquadopp("transect_enu.csv")
#'
#' survey <- merge_sensor_data(adcp = adcp, bathy = bathy)
#' }
read_nortek_aquadopp <- function(file,
                                  beam_files          = NULL,
                                  lat                 = NULL,
                                  lon                 = NULL,
                                  spatial_res         = 4L,
                                  min_obs             = 5L,
                                  max_plausible_speed = 2.5,
                                  rho                 = 1025,
                                  Cd                  = 0.002,
                                  verbose             = TRUE) {

  if (!file.exists(file)) cli::cli_abort("File not found: {.file {file}}")
  if (verbose) cli::cli_inform("Reading Nortek Aquadopp data from {.file {file}}...")

  # ---- Read primary file ------------------------------------------------------
  raw <- utils::read.csv(file, stringsAsFactors = FALSE, check.names = FALSE)
  n_raw <- nrow(raw)
  if (verbose) cli::cli_inform("  {n_raw} ensembles loaded.")

  # ---- Detect velocity columns ------------------------------------------------
  # Support AquaPro ENU layout: East_1/North_1/Up_1 ... East_N/North_N/Up_N
  # Also handle VelEast_1, VelNorth_1 variants
  east_cols  <- sort(grep("^(East|VelEast|Vel_E|U)_?[0-9]+$",  names(raw), value = TRUE, ignore.case = TRUE))
  north_cols <- sort(grep("^(North|VelNorth|Vel_N|V)_?[0-9]+$", names(raw), value = TRUE, ignore.case = TRUE))

  # ---- Optionally read beam files (.v1/.v2/.v3) -------------------------------
  if (!is.null(beam_files) && length(east_cols) == 0) {
    if (verbose) cli::cli_inform("  Reading {length(beam_files)} beam file(s)...")
    beam_list <- lapply(beam_files, function(bf) {
      if (!file.exists(bf)) {
        cli::cli_warn("Beam file not found: {.file {bf}} -- skipping.")
        return(NULL)
      }
      utils::read.table(bf, header = FALSE, stringsAsFactors = FALSE)
    })
    beam_list <- Filter(Negate(is.null), beam_list)
    if (length(beam_list) >= 2) {
      # Beams 1 and 2 approximate East and North in standard Aquadopp orientation
      b1 <- beam_list[[1]]; b2 <- beam_list[[2]]
      n_bins_beam <- ncol(b1)
      for (i in seq_len(n_bins_beam)) {
        raw[[paste0("East_",  i)]]  <- suppressWarnings(as.numeric(b1[, i]))
        raw[[paste0("North_", i)]]  <- suppressWarnings(as.numeric(b2[, i]))
      }
      east_cols  <- paste0("East_",  seq_len(n_bins_beam))
      north_cols <- paste0("North_", seq_len(n_bins_beam))
    }
  }

  if (length(east_cols) == 0 || length(north_cols) == 0) {
    cli::cli_abort(c(
      "No velocity bin columns detected.",
      "i" = "Expected ENU columns like {.val East_1}, {.val North_1}, {.val East_2} ...",
      "i" = "Or use {.arg beam_files} to supply separate .v1/.v2/.v3 files.",
      "i" = "Columns found: {.val {names(raw)}}"
    ))
  }

  n_bins <- min(length(east_cols), length(north_cols))
  if (verbose) cli::cli_inform("  {n_bins} velocity depth bin{?s} detected.")

  # ---- Compute speed magnitude ------------------------------------------------
  speed_matrix <- matrix(NA_real_, nrow = n_raw, ncol = n_bins)
  for (i in seq_len(n_bins)) {
    e <- suppressWarnings(as.numeric(raw[[east_cols[i]]]))
    n <- suppressWarnings(as.numeric(raw[[north_cols[i]]]))
    speed_matrix[, i] <- sqrt(e^2 + n^2)
  }

  # Clip outliers
  speed_matrix[speed_matrix > max_plausible_speed] <- NA_real_

  # Use deepest bin with non-NA data for near-bed velocity
  valid_per_bin  <- colSums(!is.na(speed_matrix))
  usable_bins    <- which(valid_per_bin >= max(1, n_raw * 0.10))
  deepest_usable <- if (length(usable_bins) > 0) max(usable_bins) else 1

  primary_speed  <- speed_matrix[, deepest_usable]
  shear_quality  <- ifelse(deepest_usable == 1, "low",
                    ifelse(deepest_usable <= ceiling(n_bins / 2), "moderate", "good"))

  if (verbose && deepest_usable < n_bins) {
    cli::cli_inform(c(
      "i" = "Deepest usable bin: {deepest_usable} of {n_bins}.",
      "i" = "Shear stress quality: {.val {shear_quality}}."
    ))
  }

  # ---- GPS / fixed position ---------------------------------------------------
  gps_lat_col <- .find_col_any(raw, c("gps_lat", "latitude", "lat", "Lat", "GPS_Lat"))
  gps_lon_col <- .find_col_any(raw, c("gps_lon", "longitude", "lon", "Lon", "GPS_Lon"))

  if (!is.null(gps_lat_col) && !is.null(gps_lon_col)) {
    raw$lat_src <- suppressWarnings(as.numeric(raw[[gps_lat_col]]))
    raw$lon_src <- suppressWarnings(as.numeric(raw[[gps_lon_col]]))
  } else {
    if (is.null(lat) || is.null(lon)) {
      cli::cli_abort(c(
        "No GPS columns found in file and no fixed position supplied.",
        "i" = "For moored deployments supply {.arg lat} and {.arg lon}.",
        "i" = "Columns found: {.val {names(raw)}}"
      ))
    }
    raw$lat_src <- lat
    raw$lon_src <- lon
    if (verbose) cli::cli_inform("  Fixed position: lat = {lat}, lon = {lon}")
  }

  # ---- Optional sensor columns -----------------------------------------------
  temp_col <- .find_col_any(raw, c("temperature", "temp", "Temp", "Temperature"))
  pres_col <- .find_col_any(raw, c("pressure", "Pressure", "pres_dbar", "Press", "depth"))

  # ---- Date ------------------------------------------------------------------
  date_col <- .find_col_any(raw, c("datetime", "time", "Time", "Date", "timestamp",
                                    "Month Day Year Hour Minute Second"))
  raw$date_str <- if (!is.null(date_col)) substr(raw[[date_col]], 1, 10) else NA_character_

  # ---- Spatial binning -------------------------------------------------------
  raw$lat_bin   <- round(raw$lat_src, spatial_res)
  raw$lon_bin   <- round(raw$lon_src, spatial_res)
  raw$spd_clean <- primary_speed
  raw$tau       <- rho * Cd * primary_speed^2
  if (!is.null(temp_col)) raw$temperature_src <- suppressWarnings(as.numeric(raw[[temp_col]]))
  if (!is.null(pres_col)) raw$pressure_src    <- suppressWarnings(as.numeric(raw[[pres_col]]))

  agg <- raw |>
    dplyr::group_by(lat_bin, lon_bin) |>
    dplyr::summarise(
      lat                  = mean(lat_src,        na.rm = TRUE),
      lon                  = mean(lon_src,        na.rm = TRUE),
      current_speed        = mean(spd_clean,      na.rm = TRUE),
      bed_shear_stress     = mean(tau,            na.rm = TRUE),
      shear_stress_quality = shear_quality,
      n_ensembles          = dplyr::n(),
      date                 = stats::na.omit(date_str)[1],
      .groups = "drop"
    )

  if (!is.null(temp_col)) {
    temp_agg <- raw |>
      dplyr::group_by(lat_bin, lon_bin) |>
      dplyr::summarise(temperature = mean(temperature_src, na.rm = TRUE), .groups = "drop")
    agg <- dplyr::left_join(agg, temp_agg, by = c("lat_bin", "lon_bin"))
  }

  if (!is.null(pres_col)) {
    pres_agg <- raw |>
      dplyr::group_by(lat_bin, lon_bin) |>
      dplyr::summarise(pressure_dbar = mean(pressure_src, na.rm = TRUE), .groups = "drop")
    agg <- dplyr::left_join(agg, pres_agg, by = c("lat_bin", "lon_bin"))
  }

  result <- agg[agg$n_ensembles >= min_obs, ]
  result <- result[, setdiff(names(result), c("lat_bin", "lon_bin"))]

  if (verbose) {
    cli::cli_inform(c(
      "v" = "Aquadopp output: {nrow(result)} grid cell{?s} (>= {min_obs} ensembles each)."
    ))
  }
  result
}


# =============================================================================
# read_rdi_adcp()
# Teledyne RDI WorkHorse / Sentinel V -- binary PD0 format via oce
# =============================================================================

#' Read a Teledyne RDI ADCP binary file (PD0 format)
#'
#' @description
#' Reads the binary **PD0** output file produced by Teledyne RDI instruments
#' (WorkHorse II, Sentinel V, StreamPro, Ocean Surveyor, Long Ranger) via the
#' the `oce` package's `read.adp.rdi()` parser. The function
#' extracts east/north velocity profiles, applies sidelobe contamination
#' detection, and derives current speed and bed shear stress -- producing
#' output in the same format as [read_nortek_adcp()].
#'
#' **Optional dependency:** reading RDI binary (PD0) files requires the `oce`
#' package (`install.packages("oce")`). If `oce` is absent the function will
#' stop with an informative message. ASCII WinRiver II exports (`.txt`/`.csv`)
#' do not require `oce`.
#'
#' **GPS:** vessel-mounted deployments embed GPS in the binary file and are
#' handled automatically. For moored deployments supply `lat` and `lon`.
#'
#' @param file Character. Path to the RDI binary `.000` / `.PD0` / `.pd0`
#'   file, or to an ASCII WinRiver II export (`.txt` or `.csv`) with columns
#'   `VelEast_binN` and `VelNorth_binN`.
#' @param lat Numeric. Fixed latitude for moored deployments (decimal degrees).
#' @param lon Numeric. Fixed longitude for moored deployments (decimal degrees).
#' @param spatial_res Integer. Decimal places for lat/lon binning (default `4`).
#' @param min_obs Integer. Minimum ensembles per grid cell (default `5`).
#' @param max_plausible_speed Numeric. Upper speed threshold in m/s for
#'   sidelobe / spike removal (default `2.0`).
#' @param sidelobe_threshold Numeric. Fraction of ensembles exceeding
#'   `max_plausible_speed` above which a bin is considered sidelobe-contaminated
#'   (default `0.10`).
#' @param rho Numeric. Water density in kg/m^3 (default `1025`).
#' @param Cd Numeric. Drag coefficient for bed shear stress (default `0.002`).
#' @param verbose Logical. Print progress messages (default `TRUE`).
#'
#' @return A dataframe with columns `lat`, `lon`, `current_speed`,
#'   `bed_shear_stress`, `shear_stress_quality`, and `n_ensembles` -- ready
#'   for [merge_sensor_data()].
#'
#' @seealso [read_nortek_adcp()], [read_nortek_aquadopp()], [merge_sensor_data()]
#' @export
#' @examples
#' \dontrun{
#' # Vessel-mounted WorkHorse II (GPS embedded in binary)
#' adcp <- read_rdi_adcp("survey_2024.000")
#'
#' # Moored Sentinel V (fixed position)
#' adcp <- read_rdi_adcp("mooring_2024.000", lat = 52.41, lon = -9.20)
#'
#' survey <- merge_sensor_data(adcp = adcp, bathy = bathy)
#' }
read_rdi_adcp <- function(file,
                           lat                 = NULL,
                           lon                 = NULL,
                           spatial_res         = 4L,
                           min_obs             = 5L,
                           max_plausible_speed = 2.0,
                           sidelobe_threshold  = 0.10,
                           rho                 = 1025,
                           Cd                  = 0.002,
                           verbose             = TRUE) {

  if (!file.exists(file)) cli::cli_abort("File not found: {.file {file}}")

  # ---- Try binary PD0 via oce ------------------------------------------------
  use_oce <- grepl("\\.(000|pd0|PD0|rdi|RDI)$", file, perl = TRUE) ||
             !grepl("\\.(csv|txt|CSV|TXT)$", file, perl = TRUE)

  if (use_oce) {
    if (!requireNamespace("oce", quietly = TRUE)) {
      cli::cli_abort(c(
        "The {.pkg oce} package is required to read RDI binary files.",
        "i" = "Install it with: {.code install.packages('oce')}",
        "i" = "Alternatively, export your data to ASCII from WinRiver II and",
        "i" = "supply a .csv or .txt file with {.val VelEast_binN} columns."
      ))
    }

    if (verbose) cli::cli_inform("Reading RDI binary via {.pkg oce}: {.file {file}}...")

    adp <- oce::read.adp.rdi(file)

    # Extract ENU velocity array: [ensemble, bin, beam] where beams 1=E, 2=N
    v <- adp[["v"]]  # dimensions: [time, distance, beam]
    if (is.null(v) || length(dim(v)) < 3) {
      cli::cli_abort(c(
        "Could not extract velocity array from RDI file.",
        "i" = "oce returned an object without a valid {.val v} slot.",
        "i" = "Try exporting to ASCII from WinRiver II and use a .csv file."
      ))
    }

    n_ens  <- dim(v)[1]
    n_bins <- dim(v)[2]
    if (verbose) cli::cli_inform("  {n_ens} ensembles, {n_bins} depth bin{?s}.")

    # Columns: beam 1 = East, beam 2 = North
    speed_matrix <- matrix(NA_real_, nrow = n_ens, ncol = n_bins)
    for (i in seq_len(n_bins)) {
      e <- v[, i, 1]; n <- v[, i, 2]
      speed_matrix[, i] <- sqrt(e^2 + n^2)
    }

    # GPS from oce object
    gps_lat_vec <- tryCatch(adp[["latitude"]],  error = function(e) NULL)
    gps_lon_vec <- tryCatch(adp[["longitude"]], error = function(e) NULL)
    time_vec    <- tryCatch(adp[["time"]],       error = function(e) NULL)

    raw_lat <- if (!is.null(gps_lat_vec) && length(gps_lat_vec) == n_ens) {
      gps_lat_vec
    } else if (!is.null(lat)) {
      rep(lat, n_ens)
    } else {
      cli::cli_abort(c(
        "No GPS data in binary file and no fixed position supplied.",
        "i" = "Supply {.arg lat} and {.arg lon} for moored deployments."
      ))
    }

    raw_lon <- if (!is.null(gps_lon_vec) && length(gps_lon_vec) == n_ens) {
      gps_lon_vec
    } else {
      rep(lon, n_ens)
    }

    date_str <- if (!is.null(time_vec)) {
      format(as.POSIXct(time_vec, origin = "1970-01-01", tz = "UTC"), "%Y-%m-%d")
    } else {
      rep(NA_character_, n_ens)
    }

  } else {
    # ---- ASCII WinRiver II fallback --------------------------------------------
    if (verbose) cli::cli_inform("Reading RDI ASCII export: {.file {file}}...")
    raw_csv <- utils::read.csv(file, stringsAsFactors = FALSE)
    n_ens   <- nrow(raw_csv)

    east_cols  <- sort(grep("^VelEast_|^East_bin",  names(raw_csv), value = TRUE, ignore.case = TRUE))
    north_cols <- sort(grep("^VelNorth_|^North_bin", names(raw_csv), value = TRUE, ignore.case = TRUE))

    if (length(east_cols) == 0) {
      cli::cli_abort(c(
        "No velocity bin columns found in ASCII file.",
        "i" = "Expected columns like {.val VelEast_bin1}, {.val VelNorth_bin1}.",
        "i" = "Columns found: {.val {names(raw_csv)}}"
      ))
    }

    n_bins       <- min(length(east_cols), length(north_cols))
    speed_matrix <- matrix(NA_real_, nrow = n_ens, ncol = n_bins)
    for (i in seq_len(n_bins)) {
      e <- suppressWarnings(as.numeric(raw_csv[[east_cols[i]]]))
      n <- suppressWarnings(as.numeric(raw_csv[[north_cols[i]]]))
      speed_matrix[, i] <- sqrt(e^2 + n^2)
    }

    lat_col <- .find_col_any(raw_csv, c("gps_lat", "latitude", "lat"))
    lon_col <- .find_col_any(raw_csv, c("gps_lon", "longitude", "lon"))
    raw_lat <- if (!is.null(lat_col)) suppressWarnings(as.numeric(raw_csv[[lat_col]])) else rep(lat, n_ens)
    raw_lon <- if (!is.null(lon_col)) suppressWarnings(as.numeric(raw_csv[[lon_col]])) else rep(lon, n_ens)

    date_col <- .find_col_any(raw_csv, c("datetime", "time", "timestamp"))
    date_str <- if (!is.null(date_col)) substr(raw_csv[[date_col]], 1, 10) else rep(NA_character_, n_ens)
    if (verbose) cli::cli_inform("  {n_ens} ensembles, {n_bins} depth bin{?s}.")
  }

  # ---- Shared processing (identical to read_nortek_adcp) ---------------------
  speed_matrix[speed_matrix > max_plausible_speed] <- NA_real_

  contaminated   <- .detect_sidelobe_bins(speed_matrix, max_plausible_speed, sidelobe_threshold)
  clean_bins     <- which(!contaminated)
  if (length(clean_bins) == 0) clean_bins <- seq_len(ncol(speed_matrix))
  deepest_clean  <- max(clean_bins)
  n_bins_total   <- ncol(speed_matrix)
  shear_quality  <- ifelse(deepest_clean == 1, "low",
                    ifelse(deepest_clean <= ceiling(n_bins_total / 2), "moderate", "good"))
  primary_speed  <- speed_matrix[, deepest_clean]
  tau            <- rho * Cd * primary_speed^2

  df <- data.frame(
    lat_src   = raw_lat,
    lon_src   = raw_lon,
    spd_clean = primary_speed,
    tau       = tau,
    date_str  = date_str,
    stringsAsFactors = FALSE
  )

  df$lat_bin <- round(df$lat_src, spatial_res)
  df$lon_bin <- round(df$lon_src, spatial_res)

  result <- df |>
    dplyr::group_by(lat_bin, lon_bin) |>
    dplyr::summarise(
      lat                  = mean(lat_src,   na.rm = TRUE),
      lon                  = mean(lon_src,   na.rm = TRUE),
      current_speed        = mean(spd_clean, na.rm = TRUE),
      bed_shear_stress     = mean(tau,       na.rm = TRUE),
      shear_stress_quality = shear_quality,
      n_ensembles          = dplyr::n(),
      date                 = stats::na.omit(date_str)[1],
      .groups = "drop"
    )

  result <- result[result$n_ensembles >= min_obs, ]
  result <- result[, setdiff(names(result), c("lat_bin", "lon_bin"))]

  if (verbose) {
    cli::cli_inform(c(
      "v" = "RDI output: {nrow(result)} grid cell{?s} (>= {min_obs} ensembles each)."
    ))
  }
  result
}


# =============================================================================
# read_aanderaa_csv()
# Aanderaa RCM series -- Aanderaa Data Studio CSV export
# =============================================================================

#' Read an Aanderaa RCM current meter CSV export
#'
#' @description
#' Reads the CSV file exported by **Aanderaa Data Studio** from an Aanderaa
#' Recording Current Meter (RCM Blue 5450, SeaGuard RCM, or compatible model).
#' The export typically contains time, current speed, current direction, and
#' optionally temperature, pressure/depth, salinity, and dissolved oxygen.
#'
#' Because Aanderaa instruments are moored at a fixed location, latitude and
#' longitude must be supplied by the user via `lat` and `lon`.
#'
#' Current speed is returned as `current_speed` (m/s). If the export uses
#' cm/s (common in older instrument firmware), set `speed_unit = "cm/s"` or
#' leave as `"auto"` and the function will detect units from the column header
#' or by range-checking the data.
#'
#' @param file Character. Path to the Aanderaa Data Studio CSV export.
#' @param lat Numeric. Site latitude in decimal degrees (required).
#' @param lon Numeric. Site longitude in decimal degrees (required).
#' @param speed_unit Character. Units of the current speed column.
#'   `"auto"` (default) detects from the column header (looks for `cm/s`) or
#'   infers from data range; `"m/s"` and `"cm/s"` override auto-detection.
#' @param depth_from_pressure Logical. If `TRUE` and a pressure column is
#'   present, approximate depth is derived as `pressure_dbar / 1.025` (default
#'   `FALSE`; use only when a depth column is absent).
#' @param verbose Logical. Print progress messages (default `TRUE`).
#'
#' @return A one-row dataframe (site-averaged) with columns `lat`, `lon`,
#'   `current_speed`, and any additional variables present in the file
#'   (`temperature`, `salinity`, `dissolved_oxygen`, `pressure_dbar`, `depth`)
#'   -- ready for [merge_sensor_data()].
#'
#' @seealso [read_rdi_adcp()], [read_nortek_aquadopp()], [merge_sensor_data()]
#' @export
#' @examples
#' \dontrun{
#' # Moored RCM Blue at Strangford Lough narrows
#' rcm <- read_aanderaa_csv(
#'   "strangford_rcm_2024.csv",
#'   lat = 54.398,
#'   lon = -5.558
#' )
#' survey <- merge_sensor_data(adcp = rcm, bathy = bathy)
#' }
read_aanderaa_csv <- function(file,
                               lat                  = NULL,
                               lon                  = NULL,
                               speed_unit           = c("auto", "m/s", "cm/s"),
                               depth_from_pressure  = FALSE,
                               verbose              = TRUE) {

  if (!file.exists(file)) cli::cli_abort("File not found: {.file {file}}")
  if (is.null(lat) || is.null(lon)) {
    cli::cli_abort(c(
      "{.arg lat} and {.arg lon} are required for Aanderaa moored deployments.",
      "i" = "Aanderaa instruments do not log GPS; supply the deployment coordinates."
    ))
  }
  speed_unit <- match.arg(speed_unit)

  if (verbose) cli::cli_inform("Reading Aanderaa RCM data from {.file {file}}...")

  raw <- utils::read.csv(file, stringsAsFactors = FALSE, check.names = FALSE)
  if (verbose) cli::cli_inform("  {nrow(raw)} records loaded.")

  col_names_lower <- tolower(names(raw))

  # ---- Current speed ----------------------------------------------------------
  spd_col <- .find_col_any(raw, c(
    "Current Speed", "CurrentSpeed", "current_speed", "Speed", "speed",
    "Horisontal Speed", "HorisontalSpeed"  # Aanderaa Data Studio spelling
  ))
  if (is.null(spd_col)) {
    cli::cli_abort(c(
      "Could not find a current speed column.",
      "i" = "Columns found: {.val {names(raw)}}"
    ))
  }
  spd_raw <- suppressWarnings(as.numeric(raw[[spd_col]]))

  # Auto-detect cm/s: Aanderaa Data Studio can label as "Current Speed (cm/s)"
  if (speed_unit == "auto") {
    header_has_cms <- grepl("cm/s", names(raw)[spd_col], ignore.case = TRUE) ||
                      grepl("cm.s", names(raw)[spd_col], ignore.case = TRUE)
    range_suggests_cms <- stats::median(spd_raw, na.rm = TRUE) > 5  # >5 m/s implausible
    speed_unit <- if (header_has_cms || range_suggests_cms) "cm/s" else "m/s"
    if (verbose) cli::cli_inform("  Speed unit detected: {.val {speed_unit}}")
  }
  spd_ms <- if (speed_unit == "cm/s") spd_raw / 100 else spd_raw
  spd_ms[spd_ms < 0 | spd_ms > 5] <- NA_real_  # clip implausible

  # ---- Optional columns -------------------------------------------------------
  temp_col <- .find_col_any(raw, c("Temperature", "Temp", "temperature", "temp"))
  sal_col  <- .find_col_any(raw, c("Salinity", "salinity", "Sal", "sal",
                                    "Conductivity", "conductivity"))
  do_col   <- .find_col_any(raw, c("Dissolved Oxygen", "DO", "dissolved_oxygen",
                                    "DissolvedOxygen", "oxygen", "O2"))
  dep_col  <- .find_col_any(raw, c("depth", "Depth", "depth_m"))
  pre_col  <- .find_col_any(raw, c("Pressure", "pressure", "press", "Press",
                                    "pressure_dbar", "Pressure (dbar)"))

  out <- data.frame(
    lat           = lat,
    lon           = lon,
    current_speed = mean(spd_ms, na.rm = TRUE),
    n_records     = sum(!is.na(spd_ms)),
    stringsAsFactors = FALSE
  )

  if (!is.null(temp_col))
    out$temperature     <- mean(suppressWarnings(as.numeric(raw[[temp_col]])), na.rm = TRUE)
  if (!is.null(sal_col))
    out$salinity        <- mean(suppressWarnings(as.numeric(raw[[sal_col]])),  na.rm = TRUE)
  if (!is.null(do_col))
    out$dissolved_oxygen <- mean(suppressWarnings(as.numeric(raw[[do_col]])),  na.rm = TRUE)
  if (!is.null(pre_col)) {
    pres_vals <- mean(suppressWarnings(as.numeric(raw[[pre_col]])), na.rm = TRUE)
    out$pressure_dbar   <- pres_vals
    if (depth_from_pressure && is.null(dep_col))
      out$depth <- pres_vals / 1.025
  }
  if (!is.null(dep_col))
    out$depth           <- mean(suppressWarnings(as.numeric(raw[[dep_col]])),  na.rm = TRUE)

  if (verbose) {
    vars_out <- setdiff(names(out), c("lat", "lon", "n_records"))
    cli::cli_inform(c(
      "v" = "Aanderaa output: 1 site-averaged record.",
      "i" = "Variables: {.val {vars_out}}",
      "i" = "Based on {out$n_records} valid speed readings."
    ))
  }
  out
}
