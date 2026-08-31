# =============================================================================
# Sonar / Raster Data Ingestion for oystermapR
#
# Functions for reading bathymetric and sidescan survey outputs and deriving
# the physical variables needed by predict_oyster().
#
# read_soundings_xyz()  \u2014 Raw XYZ point cloud (e.g. from Ping 3DSS sonar)
#                         \u2192 depth, slope, roughness
# read_sonar_tif()      \u2014 GeoTIFF raster (bathymetry DEM or sidescan mosaic)
#                         \u2192 depth + slope + roughness (bathy)
#                         \u2192 substrate_hardness (sidescan backscatter)
# =============================================================================


#' Read a bathymetric XYZ point cloud and derive depth, slope and roughness
#'
#' @description
#' Reads a plain-text XYZ soundings file (comma or space delimited, optional
#' `#`-prefixed header), spatially averages onto a regular grid, and derives:
#'
#' - **`depth`** -- mean depth per cell (metres, positive downward)
#' - **`roughness`** -- rugosity index per cell, derived from intra-cell depth
#'   variance using the surface-area approximation
#'   `rugosity = sqrt(1 + (depth_sd / cell_size_m)^2)`. Values near 1.0 are
#'   flat; higher values indicate more complex relief.
#' - **`slope`** -- maximum downslope gradient in degrees, computed by finite
#'   differences between neighbouring grid cells (Horn 1981 method).
#'
#' @param file Character. Path to the `.xyz` (or `.csv`) soundings file.
#'   Expected columns: longitude, latitude, depth (in that order, or named).
#'   Lines beginning with `#` are treated as comments.
#' @param lon_col,lat_col,depth_col Character or integer. Column names or
#'   positions for longitude, latitude, and depth. If `NULL` (default), the
#'   function tries common names automatically.
#' @param spatial_res Integer. Decimal places for lat/lon binning (default `4`,
#'   approx.11 m at 56degrees N). The rugosity calculation uses the physical cell size
#'   implied by this resolution.
#' @param min_soundings Integer. Minimum soundings per cell to include in output
#'   (default `10`). Cells with fewer soundings produce unreliable statistics.
#' @param depth_positive Logical. If `TRUE` (default), depth values are already
#'   positive-downward. Set to `FALSE` if your sonar records depth as negative
#'   (elevation-style) -- values will be negated.
#' @param verbose Logical. Print processing summary (default `TRUE`).
#'
#' @return A dataframe with columns `lat`, `lon`, `depth`, `slope`,
#'   `roughness`, and `n_soundings` -- ready for [merge_sensor_data()].
#'
#' @export
#' @examples
#' bathy_f <- system.file("extdata", "example_bay_soundings.xyz", package = "oystermapR")
#' bathy <- read_soundings_xyz(bathy_f, verbose = FALSE)
#' head(bathy[, c("lat", "lon", "depth")])
read_soundings_xyz <- function(file,
                               lon_col        = NULL,
                               lat_col        = NULL,
                               depth_col      = NULL,
                               spatial_res    = 4L,
                               min_soundings  = 10L,
                               depth_positive = TRUE,
                               verbose        = TRUE) {

  if (!file.exists(file)) cli::cli_abort("File not found: {.file {file}}")

  # ---- Read file --------------------------------------------------------------
  if (verbose) cli::cli_inform("Reading soundings from {.file {file}}...")

  # Skip comment lines, auto-detect delimiter
  raw_lines <- readLines(file, warn = FALSE)
  comment_lines <- grep("^#", raw_lines)
  header_line   <- if (length(comment_lines) > 0) raw_lines[max(comment_lines)] else NULL
  data_lines    <- raw_lines[!seq_along(raw_lines) %in% comment_lines]

  # Detect delimiter from first data line
  first_data <- data_lines[1]
  delim <- if (grepl(",", first_data)) "," else " "

  # Parse header for column names (strip leading #)
  col_names <- NULL
  if (!is.null(header_line)) {
    clean_header <- trimws(sub("^#+\\s*", "", header_line))
    col_names    <- trimws(strsplit(clean_header, delim)[[1]])
  }

  # Read into matrix
  con <- textConnection(data_lines)
  raw <- utils::read.table(con, sep = delim, header = FALSE,
                           stringsAsFactors = FALSE, fill = TRUE)
  close(con)
  if (!is.null(col_names) && length(col_names) == ncol(raw)) {
    names(raw) <- col_names
  }

  n_raw <- nrow(raw)
  if (verbose) cli::cli_inform("  {n_raw} soundings loaded.")

  # ---- Identify columns -------------------------------------------------------
  lwr <- tolower(names(raw))

  .pick_col <- function(user_arg, candidates) {
    if (!is.null(user_arg)) {
      if (is.numeric(user_arg)) return(user_arg)
      idx <- which(names(raw) == user_arg)
      if (length(idx)) return(idx[1])
    }
    idx <- which(lwr %in% tolower(candidates))
    if (length(idx)) return(idx[1])
    NULL
  }

  lon_idx   <- .pick_col(lon_col,   c("longitude", "lon", "long", "x", "easting"))
  lat_idx   <- .pick_col(lat_col,   c("latitude",  "lat", "y",    "northing"))
  depth_idx <- .pick_col(depth_col, c("depth_m", "depth", "z", "elevation", "elev", "d"))

  if (is.null(lon_idx) || is.null(lat_idx) || is.null(depth_idx)) {
    cli::cli_abort(c(
      "Could not identify lon/lat/depth columns automatically.",
      "i" = "Columns found: {.val {names(raw)}}",
      "i" = "Supply {.arg lon_col}, {.arg lat_col}, {.arg depth_col} explicitly."
    ))
  }

  lon   <- suppressWarnings(as.numeric(raw[[lon_idx]]))
  lat   <- suppressWarnings(as.numeric(raw[[lat_idx]]))
  depth <- suppressWarnings(as.numeric(raw[[depth_idx]]))

  # Negate if elevation-style (negative = below surface)
  if (!depth_positive) depth <- -depth

  # Remove NAs / clearly bad values
  good <- !is.na(lon) & !is.na(lat) & !is.na(depth) & depth > 0 & depth < 500
  lon  <- lon[good];  lat <- lat[good];  depth <- depth[good]

  # ---- Physical cell size for rugosity ----------------------------------------
  mid_lat      <- mean(lat)
  cell_size_lat <- (10^(-spatial_res)) * 111000          # metres N-S
  cell_size_lon <- (10^(-spatial_res)) * 111000 * cos(mid_lat * pi / 180)
  cell_size_m   <- sqrt(cell_size_lat * cell_size_lon)  # geometric mean

  # ---- Spatial binning --------------------------------------------------------
  lat_bin <- round(lat, spatial_res)
  lon_bin <- round(lon, spatial_res)
  keys    <- paste(lat_bin, lon_bin, sep = "_")

  # Aggregate per cell
  cell_lats   <- tapply(lat,   keys, mean)
  cell_lons   <- tapply(lon,   keys, mean)
  cell_means  <- tapply(depth, keys, mean)
  cell_sds    <- tapply(depth, keys, function(x) if (length(x) > 1) sd(x) else 0)
  cell_counts <- tapply(depth, keys, length)

  # Filter minimum soundings
  keep <- cell_counts >= min_soundings
  cell_lats   <- cell_lats[keep]
  cell_lons   <- cell_lons[keep]
  cell_means  <- cell_means[keep]
  cell_sds    <- cell_sds[keep]
  cell_counts <- cell_counts[keep]

  # ---- Roughness (rugosity index) ---------------------------------------------
  # rugosity = sqrt(1 + (depth_sd / cell_size_m)^2)
  # Conceptually: ratio of actual surface relief to flat plane within cell
  roughness_vec <- sqrt(1 + (cell_sds / cell_size_m)^2)

  # ---- Slope (finite differences, Horn 1981) ----------------------------------
  # Build a lookup of cell mean depths by (lat_key, lon_key)
  lat_keys_num <- as.numeric(sub("_.*", "", names(cell_means)))
  lon_keys_num <- as.numeric(sub(".*_", "", names(cell_means)))
  depth_lookup <- setNames(as.numeric(cell_means),
                            paste(round(lat_keys_num, spatial_res),
                                  round(lon_keys_num, spatial_res), sep = "_"))

  step <- 10^(-spatial_res)

  slope_vec <- vapply(seq_along(cell_means), function(i) {
    la <- lat_keys_num[i]
    lo <- lon_keys_num[i]

    get_d <- function(dlat, dlon) {
      k <- paste(round(la + dlat, spatial_res), round(lo + dlon, spatial_res), sep = "_")
      v <- depth_lookup[k]
      if (is.null(v) || is.na(v)) NA_real_ else v
    }

    # 3x3 neighbourhood
    nw <- get_d( step, -step); n <- get_d( step, 0); ne <- get_d( step,  step)
    w  <- get_d(    0, -step);                        e  <- get_d(    0,  step)
    sw <- get_d(-step, -step); s <- get_d(-step, 0); se <- get_d(-step,  step)

    # Horn's formula: dz/dx and dz/dy
    dzdx_vals <- c(nw, w, sw, ne, e, se)
    dzdy_vals <- c(nw, n, ne, sw, s, se)

    if (sum(!is.na(dzdx_vals)) < 2 || sum(!is.na(dzdy_vals)) < 2) return(NA_real_)

    # Simplified finite difference if neighbours missing
    east_val  <- if (!is.na(e))  e  else if (!is.na(ne)) ne else if (!is.na(se)) se else NA
    west_val  <- if (!is.na(w))  w  else if (!is.na(nw)) nw else if (!is.na(sw)) sw else NA
    north_val <- if (!is.na(n))  n  else if (!is.na(ne)) ne else if (!is.na(nw)) nw else NA
    south_val <- if (!is.na(s))  s  else if (!is.na(se)) se else if (!is.na(sw)) sw else NA

    dzdx <- if (!is.na(east_val) && !is.na(west_val))
               (east_val - west_val) / (2 * cell_size_lon)
            else NA_real_

    dzdy <- if (!is.na(north_val) && !is.na(south_val))
               (north_val - south_val) / (2 * cell_size_lat)
            else NA_real_

    if (is.na(dzdx) && is.na(dzdy)) return(NA_real_)
    dzdx <- if (is.na(dzdx)) 0 else dzdx
    dzdy <- if (is.na(dzdy)) 0 else dzdy

    grad_mag <- sqrt(dzdx^2 + dzdy^2)
    atan(grad_mag) * 180 / pi   # degrees
  }, numeric(1))

  # ---- Assemble output --------------------------------------------------------
  result <- data.frame(
    lat         = as.numeric(cell_lats),
    lon         = as.numeric(cell_lons),
    depth       = round(as.numeric(cell_means), 3),
    slope       = round(slope_vec, 3),
    roughness   = round(roughness_vec, 4),
    n_soundings = as.integer(cell_counts),
    stringsAsFactors = FALSE
  )
  result <- result[!is.na(result$depth), ]

  if (verbose) {
    cli::cli_inform(c(
      "v" = "Soundings processed: {nrow(result)} spatial cells.",
      "i" = "  depth:     {round(min(result$depth,na.rm=TRUE),1)} \u2013 {round(max(result$depth,na.rm=TRUE),1)} m  (mean {round(mean(result$depth,na.rm=TRUE),1)} m)",
      "i" = "  slope:     mean {round(mean(result$slope,na.rm=TRUE),1)}\u00b0,  p95 {round(quantile(result$slope,0.95,na.rm=TRUE),1)}\u00b0",
      "i" = "  roughness: mean {round(mean(result$roughness,na.rm=TRUE),3)},  p95 {round(quantile(result$roughness,0.95,na.rm=TRUE),3)}"
    ))
  }

  result
}


#' Read a bathymetric or sidescan GeoTIFF and convert to oystermapR variables
#'
#' @description
#' Reads a single-band GeoTIFF raster using `terra` and returns a spatially
#' averaged dataframe suitable for [merge_sensor_data()].
#'
#' Two modes are supported, selected via the `type` argument:
#'
#' **`"bathy"`** -- Bathymetric DEM (e.g. from Ping 3DSS or post-processed
#' soundings). Extracts:
#' - `depth` -- raster cell value (metres)
#' - `slope` -- computed using [terra::terrain()]
#' - `roughness` -- Terrain Ruggedness Index via [terra::terrain()]
#'
#' **`"sidescan"`** -- Sidescan backscatter mosaic (normalised 0--1 or raw DN).
#' Extracts:
#' - `substrate_hardness` -- backscatter intensity normalised to \[0, 1\].
#'   High backscatter -> hard/coarse substrate (rock, gravel, shell);
#'   low backscatter -> soft substrate (mud, fine sand).
#'
#' @param file Character. Path to the GeoTIFF file (`.tif`).
#' @param type Character. Either `"bathy"` or `"sidescan"`.
#' @param spatial_res Integer. Decimal places for output lat/lon grid
#'   (default `4`). The raster is resampled to this resolution before
#'   converting to a dataframe.
#' @param band Integer. Raster band to read (default `1`).
#' @param nodata_val Numeric. Value to treat as no-data (default `NA`).
#'   Override if your raster uses a sentinel (e.g. `-9999`).
#' @param depth_positive Logical. For `type = "bathy"`: if `TRUE` (default),
#'   raster values are already positive-downward. Set `FALSE` if stored as
#'   negative elevation.
#' @param verbose Logical (default `TRUE`).
#'
#' @return A dataframe with `lat`, `lon`, and the derived variable columns
#'   ready for [merge_sensor_data()].
#'
#' @export
#' @examples
#' \dontrun{
#' bathy_df   <- read_sonar_tif("kames_bay_bathy.tif",    type = "bathy")
#' sidescan_df <- read_sonar_tif("kames_bay_sidescan.tif", type = "sidescan")
#' survey <- merge_sensor_data(bathy = bathy_df, sidescan = sidescan_df, adcp = adcp_df)
#' }
read_sonar_tif <- function(file,
                           type           = c("bathy", "sidescan"),
                           spatial_res    = 4L,
                           band           = 1L,
                           nodata_val     = NA,
                           depth_positive = TRUE,
                           verbose        = TRUE) {

  type <- match.arg(type)

  if (!file.exists(file)) cli::cli_abort("File not found: {.file {file}}")
  if (!requireNamespace("terra", quietly = TRUE)) {
    cli::cli_abort("Package {.pkg terra} required. Install: {.code install.packages('terra')}")
  }

  if (verbose) cli::cli_inform("Reading {type} TIF: {.file {file}}...")

  r <- terra::rast(file, lyrs = band)

  # Replace sentinel no-data value if specified
  if (!is.na(nodata_val)) {
    r[r == nodata_val] <- NA
  }

  # ---- Reproject to WGS84 if needed ------------------------------------------
  if (!terra::is.lonlat(r)) {
    if (verbose) cli::cli_inform("  Reprojecting from {terra::crs(r, describe=TRUE)$name} to WGS84...")
    r <- terra::project(r, "EPSG:4326", method = "bilinear")
  }

  if (type == "bathy") {

    # ---- Flip sign if elevation-style ------------------------------------------
    if (!depth_positive) r <- r * -1

    # Mask clearly invalid depths
    r[r <= 0 | r > 500] <- NA

    # ---- Derive slope and roughness using terra::terrain -----------------------
    if (verbose) cli::cli_inform("  Computing slope and terrain ruggedness index (TRI)...")

    slope_r <- terra::terrain(r, v = "slope",    unit = "degrees", neighbors = 8)
    tri_r   <- terra::terrain(r, v = "TRI",      neighbors = 8)

    # Convert TRI to rugosity index (TRI in metres \u2192 dimensionless ratio)
    # Use mean cell resolution in metres for normalisation
    res_m   <- mean(terra::res(r)) * 111000
    rugosity_r <- 1 + (tri_r / res_m)
    rugosity_r <- terra::clamp(rugosity_r, lower = 1, upper = 10, values = TRUE)

    # ---- Aggregate to output spatial_res grid ----------------------------------
    target_res <- 10^(-spatial_res)
    if (mean(terra::res(r)) < target_res) {
      factor_n <- round(target_res / mean(terra::res(r)))
      if (factor_n > 1) {
        r         <- terra::aggregate(r,         fact = factor_n, fun = "mean",  na.rm = TRUE)
        slope_r   <- terra::aggregate(slope_r,   fact = factor_n, fun = "mean",  na.rm = TRUE)
        rugosity_r<- terra::aggregate(rugosity_r,fact = factor_n, fun = "mean",  na.rm = TRUE)
      }
    }

    # ---- Convert to dataframe --------------------------------------------------
    df_depth <- as.data.frame(r,         xy = TRUE, na.rm = TRUE)
    df_slope <- as.data.frame(slope_r,   xy = TRUE, na.rm = FALSE)
    df_rug   <- as.data.frame(rugosity_r,xy = TRUE, na.rm = FALSE)

    names(df_depth)[3] <- "depth"
    names(df_slope)[3] <- "slope"
    names(df_rug)[3]   <- "roughness"

    result <- df_depth
    result$slope     <- df_slope$slope[match(
                           paste(round(df_depth$x, 6), round(df_depth$y, 6)),
                           paste(round(df_slope$x, 6), round(df_slope$y, 6)))]
    result$roughness <- df_rug$roughness[match(
                           paste(round(df_depth$x, 6), round(df_depth$y, 6)),
                           paste(round(df_rug$x, 6), round(df_rug$y, 6)))]

    names(result)[1:2] <- c("lon", "lat")
    result <- result[, c("lat", "lon", "depth", "slope", "roughness")]
    result <- result[!is.na(result$depth), ]

    if (verbose) {
      cli::cli_inform(c(
        "v" = "Bathy TIF processed: {nrow(result)} cells.",
        "i" = "  depth:     {round(min(result$depth,na.rm=TRUE),1)} \u2013 {round(max(result$depth,na.rm=TRUE),1)} m",
        "i" = "  slope:     mean {round(mean(result$slope,na.rm=TRUE),1)}\u00b0",
        "i" = "  roughness: mean {round(mean(result$roughness,na.rm=TRUE),3)}"
      ))
    }

  } else {
    # ---- Sidescan: backscatter \u2192 substrate_hardness ---------------------------

    # Normalise to [0, 1] if not already
    vals  <- terra::values(r, mat = FALSE)
    v_min <- min(vals, na.rm = TRUE)
    v_max <- max(vals, na.rm = TRUE)

    if (v_max > 1 || v_min < 0) {
      if (verbose) cli::cli_inform("  Normalising backscatter from [{round(v_min,1)}, {round(v_max,1)}] to [0, 1]...")
      r <- (r - v_min) / (v_max - v_min)
    }

    # Aggregate to output resolution
    target_res <- 10^(-spatial_res)
    if (mean(terra::res(r)) < target_res) {
      factor_n <- round(target_res / mean(terra::res(r)))
      if (factor_n > 1) {
        r <- terra::aggregate(r, fact = factor_n, fun = "mean", na.rm = TRUE)
      }
    }

    df    <- as.data.frame(r, xy = TRUE, na.rm = TRUE)
    names(df) <- c("lon", "lat", "substrate_hardness")
    result <- df[, c("lat", "lon", "substrate_hardness")]
    result <- result[!is.na(result$substrate_hardness), ]

    if (verbose) {
      cli::cli_inform(c(
        "v" = "Sidescan TIF processed: {nrow(result)} cells.",
        "i" = "  substrate_hardness: mean={round(mean(result$substrate_hardness),3)},",
        "i" = "    p05={round(quantile(result$substrate_hardness,0.05),3)},",
        "i" = "    p95={round(quantile(result$substrate_hardness,0.95),3)}"
      ))
    }
  }

  result
}
