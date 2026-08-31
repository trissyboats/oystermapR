#' Export suitability scores as a smooth interpolated GeoTIFF heatmap for QGIS
#'
#' @description
#' Takes the scored point dataset from [predict_oyster()] and produces a
#' **smooth, continuous raster heatmap** using Inverse Distance Weighting (IDW)
#' interpolation -- the same technique used by systems like Lowrance BioBase.
#' The result looks like a fluid heat surface over your survey area rather than
#' isolated point squares.
#'
#' The output GeoTIFF contains five bands:
#' 1. **suitability** -- IDW-interpolated score \[0, 1\]. Use this for the heatmap.
#' 2. **excluded_mask** -- 1 where a survey point was hard-excluded, 0 otherwise.
#' 3. **n_observations** -- how many survey points fell within each raster cell
#'    (data density diagnostic).
#' 4. **dist_to_nearest_m** -- distance in metres from each cell centre to the
#'    nearest survey point (use as a QGIS transparency mask to fade extrapolated areas).
#' 5. **n_layers_scored** -- number of environmental variables that contributed to
#'    the suitability score at each survey point (data completeness overlay).
#'
#' @param df A dataframe processed by [predict_oyster()], containing columns
#'   `lat`, `lon`, `suitability`, `excluded`.
#' @param path Character. Output `.tif` file path.
#' @param resolution Numeric. Raster cell size in decimal degrees. Default
#'   `0.0002` approx. 22 m -- matches the typical 4-decimal-place survey grid. For
#'   large estuary surveys increase to `0.001` (100 m); for very detailed
#'   harbour surveys decrease to `0.0001` (~11 m).
#' @param idw_power Numeric. IDW distance decay exponent (default `2`). Higher
#'   values make the surface honour nearby points more tightly and decay faster
#'   with distance -- producing sharper gradients. Lower values (e.g. 1) give
#'   broader, smoother spread.
#' @param idw_max_dist Numeric or `NULL`. Maximum search radius in degrees for
#'   IDW interpolation. If `NULL` (default), the radius is automatically set to
#'   5x the median nearest-neighbour spacing between survey points -- so the
#'   heatmap fills gaps between transect lines but does not bleed onto land or
#'   beyond the survey boundary. Override with an explicit value (e.g. `0.003`
#'   approx. 330 m) if needed.
#' @param buffer Numeric. Padding in degrees added around the survey extent
#'   before building the raster grid (default `0.001` approx. 110 m). Keeps the
#'   heatmap from clipping at the outermost survey points without extending far
#'   beyond the survey boundary.
#' @param contours Logical. If `TRUE` (default), automatically export contour
#'   lines as a GeoPackage alongside the GeoTIFF using [export_contours()].
#' @param crs Character. CRS string (default `"EPSG:4326"` -- WGS84). Use a
#'   projected CRS (e.g. `"EPSG:32630"`) if your coordinates are in metres.
#' @param overwrite Logical. Overwrite existing file (default `TRUE`).
#'
#' @return The output file path (invisibly). Also writes a `.qml` QGIS style
#'   file alongside the `.tif` automatically.
#'
#' @export
#' @examples
#' sample_csv <- system.file("extdata", "sample_survey.csv", package = "oystermapR")
#' result <- predict_oyster(sample_csv, "ostrea_edulis", verbose = FALSE)
#' out_tif <- file.path(tempdir(), "oyster_heatmap.tif")
#' export_geotiff(result, out_tif)
#' file.exists(out_tif)
export_geotiff <- function(df,
                           path,
                           resolution   = 0.0002,
                           idw_power    = 2,
                           idw_max_dist = NULL,
                           buffer       = 0.001,
                           contours     = TRUE,
                           crs          = "EPSG:4326",
                           overwrite    = TRUE) {

  # ---- Validate ---------------------------------------------------------------
  required_cols <- c("lat", "lon", "suitability", "excluded")
  missing_cols  <- setdiff(required_cols, names(df))
  if (length(missing_cols) > 0) {
    cli::cli_abort(c(
      "Required columns missing from dataframe.",
      "i" = "Run {.fn predict_oyster} before {.fn export_geotiff}.",
      "x" = "Missing: {.val {missing_cols}}"
    ))
  }
  if (!requireNamespace("terra", quietly = TRUE)) {
    cli::cli_abort("Package {.pkg terra} required. Install: {.code install.packages('terra')}")
  }

  # ---- Build output dir -------------------------------------------------------
  out_dir <- dirname(path)
  if (!dir.exists(out_dir) && out_dir != ".") dir.create(out_dir, recursive = TRUE)

  # ---- Excluded points score 0 so exclusion zones appear in surface -----------
  df$suitability[df$excluded] <- 0

  # ---- Auto-calculate IDW search radius if not supplied -----------------------
  if (is.null(idw_max_dist)) {
    n_pts    <- nrow(df)
    # Sample up to 300 points to estimate nearest-neighbour spacing
    samp_idx <- if (n_pts > 300) sample.int(n_pts, 300) else seq_len(n_pts)
    s_lon    <- df$lon[samp_idx]
    s_lat    <- df$lat[samp_idx]
    nn_dists <- vapply(seq_along(samp_idx), function(i) {
      dx <- s_lon[i] - s_lon[-i]
      dy <- s_lat[i] - s_lat[-i]
      min(sqrt(dx^2 + dy^2))
    }, numeric(1))
    med_nn       <- stats::median(nn_dists, na.rm = TRUE)
    idw_max_dist <- max(0.001, min(0.03, med_nn * 5))
    cli::cli_inform(
      "  Auto IDW radius: {round(idw_max_dist, 5)}\u00b0 (~{round(idw_max_dist*111000)} m)  [5x median point spacing]"
    )
  }

  # ---- Build raster template --------------------------------------------------
  ext <- terra::ext(
    min(df$lon) - buffer,
    max(df$lon) + buffer,
    min(df$lat) - buffer,
    max(df$lat) + buffer
  )
  r_template <- terra::rast(ext, resolution = resolution, crs = crs)

  # ---- IDW interpolation \u2014 Band 1: suitability --------------------------------
  cli::cli_inform("Interpolating suitability surface (IDW power={idw_power}, radius={round(idw_max_dist,5)}\u00b0)...")

  suit_vals <- .idw_interpolate(
    pts_lon  = df$lon,
    pts_lat  = df$lat,
    values   = df$suitability,
    rast     = r_template,
    power    = idw_power,
    max_dist = idw_max_dist
  )

  suit_rast        <- r_template
  terra::values(suit_rast) <- suit_vals
  names(suit_rast) <- "suitability"

  # ---- Band 2: Excluded mask (rasterised directly \u2014 not interpolated) ---------
  df$excluded_int <- as.integer(df$excluded)
  pts_excl  <- terra::vect(df, geom = c("lon", "lat"), crs = crs)
  excl_rast <- terra::rasterize(pts_excl, r_template,
                                field = "excluded_int",
                                fun   = max, na.rm = TRUE)
  names(excl_rast) <- "excluded_mask"

  # ---- Band 3: Observation density --------------------------------------------
  df$obs_n <- 1L
  pts_cnt  <- terra::vect(df, geom = c("lon", "lat"), crs = crs)
  cnt_rast <- terra::rasterize(pts_cnt, r_template,
                               field = "obs_n",
                               fun   = sum, na.rm = TRUE)
  names(cnt_rast) <- "n_observations"

  # ---- Band 4: Distance-to-nearest-survey-point (uncertainty indicator) ------
  # Cells close to actual survey points are reliable; cells far away are
  # interpolated and less certain. Load this band in QGIS as a transparency
  # mask to fade out extrapolated areas. Units: metres.
  cli::cli_inform("Computing uncertainty layer (distance to nearest survey point)...")
  dist_vals <- .distance_to_nearest_m(
    pts_lon  = df$lon,
    pts_lat  = df$lat,
    rast     = r_template
  )
  dist_rast        <- r_template
  terra::values(dist_rast) <- dist_vals
  names(dist_rast) <- "dist_to_nearest_m"

  # ---- Band 5: Data layers used (n_layers_scored per point) ------------------
  # Shows how many environmental variables contributed to the score at each
  # survey point. Low values flag data-sparse locations. Load in QGIS alongside
  # the suitability surface to identify areas where the score rests on few inputs.
  if ("n_layers_scored" %in% names(df)) {
    df$n_layers_int <- as.integer(df$n_layers_scored)
    pts_lyr  <- terra::vect(df, geom = c("lon", "lat"), crs = crs)
    lyr_rast <- terra::rasterize(pts_lyr, r_template,
                                 field = "n_layers_int",
                                 fun   = min, na.rm = TRUE)  # min = most data-sparse cell
    names(lyr_rast) <- "n_layers_scored"
  } else {
    lyr_rast <- r_template
    terra::values(lyr_rast) <- NA_real_
    names(lyr_rast) <- "n_layers_scored"
  }

  # ---- Stack and write --------------------------------------------------------
  stack <- c(suit_rast, excl_rast, cnt_rast, dist_rast, lyr_rast)

  terra::writeRaster(stack,
                     filename  = path,
                     overwrite = overwrite,
                     datatype  = "FLT4S",
                     gdal      = c("COMPRESS=LZW", "TILED=YES"))

  # Auto-export matching QGIS style
  qml_path <- export_qml_style(path)

  cell_m <- round(resolution * 111000)
  n_cells_used <- sum(!is.na(suit_vals))
  cli::cli_inform(c(
    "v" = "GeoTIFF exported:  {.file {path}}",
    "v" = "QGIS style:        {.file {qml_path}}",
    "i" = "Interpolated cells: {n_cells_used} of {terra::ncell(r_template)}",
    "i" = "Cell size: {resolution}\u00b0 (~{cell_m} m) | Survey points: {nrow(df)}",
    "i" = "In QGIS: Layer > Add Raster Layer. The orange heatmap style is pre-applied."
  ))

  # ---- Auto-export contour lines ----------------------------------------------
  if (contours) {
    tryCatch(
      export_contours(path),
      error = function(e) cli::cli_warn("Contour export skipped: {conditionMessage(e)}")
    )
  }

  invisible(path)
}


#' Internal IDW interpolation (no external dependencies)
#'
#' @description
#' Vectorised Inverse Distance Weighting using base-R matrix operations.
#' Computes a weighted mean of point values for every cell centre in a raster
#' template, using only points within `max_dist` degrees of each cell.
#'
#' @param pts_lon,pts_lat Numeric vectors of point coordinates.
#' @param values Numeric vector of values to interpolate (same length as pts_*).
#' @param rast A `SpatRaster` template defining the output grid.
#' @param power Numeric. Distance decay exponent.
#' @param max_dist Numeric. Search radius in same units as coordinates.
#' @return Numeric vector of interpolated values, one per raster cell.
#' @keywords internal
.idw_interpolate <- function(pts_lon, pts_lat, values, rast, power = 2, max_dist = 0.02) {

  # Cell centres of the output raster
  xy      <- terra::xyFromCell(rast, seq_len(terra::ncell(rast)))
  grid_x  <- xy[, 1]
  grid_y  <- xy[, 2]
  n_cells <- length(grid_x)
  n_pts   <- length(pts_lon)

  # For large grids, chunk processing to stay within memory (~500 MB budget)
  # Each chunk is chunk_size cells; each requires a chunk_size x n_pts distance matrix
  bytes_per_chunk <- 500e6  # 500 MB
  chunk_size      <- max(1L, floor(bytes_per_chunk / (n_pts * 8)))
  chunk_size      <- min(chunk_size, n_cells)

  result <- numeric(n_cells)

  idx_start <- 1L
  while (idx_start <= n_cells) {
    idx_end  <- min(idx_start + chunk_size - 1L, n_cells)
    idx      <- idx_start:idx_end

    cx <- grid_x[idx]
    cy <- grid_y[idx]

    # Distance matrix: rows = cells, cols = survey points
    dx       <- outer(cx, pts_lon, "-")
    dy       <- outer(cy, pts_lat, "-")
    dist_mat <- sqrt(dx^2 + dy^2)

    # Mask points beyond search radius
    dist_mat[dist_mat > max_dist] <- NA

    # Check for exact coincidences (dist == 0) \u2014 return that value directly
    zero_mask <- dist_mat == 0
    zero_mask[is.na(zero_mask)] <- FALSE

    # IDW weights  (1 / d^p)
    w_mat <- 1 / dist_mat^power   # NA where dist > max_dist or dist = 0

    # Weighted sum / weight sum
    vals_mat <- matrix(values, nrow = length(idx), ncol = n_pts, byrow = TRUE)
    w_sum    <- rowSums(w_mat, na.rm = TRUE)
    wv_sum   <- rowSums(w_mat * vals_mat, na.rm = TRUE)

    chunk_result <- ifelse(w_sum > 0, wv_sum / w_sum, NA_real_)

    # Override with exact-match values where applicable
    exact_rows <- which(rowSums(zero_mask) > 0)
    for (r in exact_rows) {
      chunk_result[r] <- values[which(zero_mask[r, ])[1]]
    }

    result[idx] <- chunk_result
    idx_start   <- idx_end + 1L
  }

  result
}


#' Internal: distance (metres) from each raster cell to the nearest survey point
#'
#' @description
#' For each cell centre, finds the minimum Euclidean distance to any survey
#' point and converts to approximate metres. Used as the uncertainty band.
#' Chunked to stay within memory budget.
#' @keywords internal
.distance_to_nearest_m <- function(pts_lon, pts_lat, rast) {

  xy      <- terra::xyFromCell(rast, seq_len(terra::ncell(rast)))
  grid_x  <- xy[, 1]
  grid_y  <- xy[, 2]
  n_cells <- length(grid_x)
  n_pts   <- length(pts_lon)

  # Chunked: ~200 MB budget
  chunk_size <- max(1L, floor(200e6 / (n_pts * 8)))
  chunk_size <- min(chunk_size, n_cells)

  result    <- numeric(n_cells)
  idx_start <- 1L

  while (idx_start <= n_cells) {
    idx_end  <- min(idx_start + chunk_size - 1L, n_cells)
    idx      <- idx_start:idx_end
    dx       <- outer(grid_x[idx], pts_lon, "-")
    dy       <- outer(grid_y[idx], pts_lat, "-")
    dist_deg <- sqrt(dx^2 + dy^2)
    min_deg  <- apply(dist_deg, 1, min, na.rm = TRUE)
    # Convert degrees to approximate metres using mid-latitude
    mid_lat    <- mean(pts_lat)
    m_per_deg  <- 111000 * cos(mid_lat * pi / 180)
    result[idx] <- min_deg * m_per_deg
    idx_start  <- idx_end + 1L
  }

  result
}


#' Export optional contour lines as a GeoPackage for QGIS
#'
#' @description
#' Derives contour lines from the IDW suitability raster and saves them as a
#' vector GeoPackage (`.gpkg`). Load alongside the heatmap in QGIS for the
#' depth-contour look shown in BioBase outputs.
#'
#' @param tif_path Character. Path to the GeoTIFF produced by [export_geotiff()].
#' @param interval Numeric. Contour interval in suitability units (default `0.1`,
#'   producing lines at 0.1, 0.2, ... 0.9).
#' @param gpkg_path Character. Output `.gpkg` path. Defaults to same directory
#'   as the `.tif` with `_contours.gpkg` suffix.
#'
#' @return The `.gpkg` file path (invisibly).
#' @export
#' @examples
#' sample_csv <- system.file("extdata", "sample_survey.csv", package = "oystermapR")
#' result <- predict_oyster(sample_csv, "ostrea_edulis", verbose = FALSE)
#' out_tif <- file.path(tempdir(), "oyster_heatmap.tif")
#' export_geotiff(result, out_tif)
#' export_contours(out_tif)
export_contours <- function(tif_path,
                            interval  = 0.1,
                            gpkg_path = NULL) {

  if (!requireNamespace("terra", quietly = TRUE)) {
    cli::cli_abort("Package {.pkg terra} required.")
  }

  if (is.null(gpkg_path)) {
    gpkg_path <- sub("\\.tif$", "_contours.gpkg", tif_path, ignore.case = TRUE)
  }

  r        <- terra::rast(tif_path, lyrs = 1)
  contours <- terra::as.contour(r, levels = seq(interval, 1 - interval, by = interval))

  terra::writeVector(contours, gpkg_path, overwrite = TRUE)

  cli::cli_inform(c(
    "v" = "Contour lines exported: {.file {gpkg_path}}",
    "i" = "{length(contours)} contour features at interval {interval}",
    "i" = "In QGIS: drag the .gpkg onto the map \u2014 label with the 'level' field."
  ))

  invisible(gpkg_path)
}


#' Export a QGIS colour style (.qml) matching the BioBase orange/red heatmap
#'
#' @description
#' Writes a QGIS Layer Style file (`.qml`) using a yellow -> orange -> red ->
#' dark red colour ramp, closely matching the BioBase/Lowrance heatmap style.
#' The file is automatically applied when its name matches the `.tif` -- no
#' manual styling needed in QGIS.
#'
#' @param tif_path Character. Path to the `.tif` from [export_geotiff()].
#' @return The `.qml` file path (invisibly).
#' @export
export_qml_style <- function(tif_path) {

  qml_path <- sub("\\.tif$", ".qml", tif_path, ignore.case = TRUE)

  qml_content <- '<?xml version="1.0" encoding="UTF-8"?>
<qgis version="3.28" styleCategories="AllStyleCategories">
  <pipe>
    <rasterrenderer opacity="0.85" alphaBand="-1" band="1" type="singlebandpseudocolor" nodataColor="">
      <rastershader>
        <colorrampshader colorRampType="INTERPOLATED" clip="0" classificationMode="1" labelPrecision="2">
          <item value="0"    color="#ffffff" label="No data"          alpha="0"/>
          <item value="0.05" color="#ffffcc" label="Very Low (0.05)"  alpha="210"/>
          <item value="0.20" color="#fed976" label="Low (0.20)"       alpha="225"/>
          <item value="0.40" color="#fd8d3c" label="Moderate (0.40)"  alpha="235"/>
          <item value="0.65" color="#e31a1c" label="High (0.65)"      alpha="245"/>
          <item value="0.85" color="#bd0026" label="Very High (0.85)" alpha="250"/>
          <item value="1.00" color="#800026" label="Peak (1.00)"      alpha="255"/>
        </colorrampshader>
      </rastershader>
    </rasterrenderer>
    <brightnesscontrast brightness="0" contrast="5" gamma="1"/>
    <huesaturation colorizeOn="0" saturation="15"/>
  </pipe>
  <blendMode>0</blendMode>
  <layerGeometryType>2</layerGeometryType>
</qgis>'

  writeLines(qml_content, qml_path)
  invisible(qml_path)
}
