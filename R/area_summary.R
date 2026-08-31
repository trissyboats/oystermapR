# =============================================================================
# Fine-scale area summary for restoration and scientific reporting
# =============================================================================
#
# Converts a suitability point dataset into area estimates at sub-hectare
# resolution. Primary unit is m2 so that small restoration plots, reef
# patches, and experimental sites are reported at ecologically meaningful
# scales rather than being rounded to coarse hectare values.
#
# Contiguous patch analysis uses terra::patches() on a rasterised grid,
# enabling minimum-viable-area filtering directly relevant to oyster reef
# restoration guidelines (e.g. the 0.01 ha OSPAR minimum restoration unit).
# =============================================================================

#' Fine-scale habitat area summary for restoration and science
#'
#' @description
#' Converts the point-based output of [predict_oyster()] into estimated areas
#' per suitability class at sub-hectare resolution. Designed for restoration
#' ecology and scientific reporting where the difference between 200 m2 and
#' 800 m2 is operationally significant.
#'
#' The function:
#' 1. Estimates survey point spacing from the median nearest-neighbour distance.
#' 2. Computes a cell area in m2 for each point (Voronoi approximation: each
#'    point is assumed to represent one cell of that area).
#' 3. Aggregates by suitability class.
#' 4. Optionally rasterises the result via [terra] and runs connected-component
#'    labelling to identify and size contiguous habitat patches -- critical for
#'    assessing whether areas meet minimum viable restoration unit thresholds.
#'
#' @param result A dataframe returned by [predict_oyster()], containing at
#'   minimum columns `lat`, `lon`, `suitability_class`, `suitability`,
#'   and `excluded`.
#' @param cell_size_m Numeric or `NULL`. Survey point spacing in metres.
#'   If `NULL` (default), estimated automatically from the median nearest-
#'   neighbour distance of the survey points (capped at 500 m). Provide an
#'   explicit value if you know your survey grid resolution (e.g. `5` for a
#'   5 m AUV grid, `25` for a 25 m ADCP trackline spacing).
#' @param viable_area_m2 Numeric. Minimum viable area threshold in m2 for a
#'   contiguous patch to be flagged as restoration-relevant (default `100`,
#'   = 0.01 ha, aligned with OSPAR oyster reef restoration guidelines).
#'   Patches below this threshold are reported but flagged `viable = FALSE`.
#' @param patch_analysis Logical. If `TRUE` (default), runs contiguous patch
#'   analysis using [terra]. Patches are identified within "High" and "Moderate"
#'   suitability zones. Set to `FALSE` to skip (faster for large datasets).
#' @param classes Character vector. Which suitability classes to include in
#'   the main summary. Default: all five classes.
#' @param verbose Logical. Print the summary table to the console (default `TRUE`).
#'
#' @return A named list with three elements:
#' \describe{
#'   \item{`class_summary`}{Dataframe -- one row per suitability class with
#'     columns: `class`, `n_points`, `area_m2`, `area_ha`, `pct_total_area`,
#'     `pct_suitable_area` (High + Moderate = 100%), `mean_suitability`,
#'     `median_suitability`.}
#'   \item{`total`}{Named numeric vector: `surveyed_area_m2`,
#'     `surveyed_area_ha`, `suitable_area_m2`, `suitable_area_ha`,
#'     `pct_suitable`, `cell_size_m` (estimated or supplied),
#'     `n_points_total`.}
#'   \item{`patches`}{Dataframe of contiguous habitat patches (High + Moderate)
#'     sorted by area descending. Columns: `patch_id`, `class_dominant`,
#'     `n_cells`, `area_m2`, `area_ha`, `mean_suitability`, `viable`.
#'     `NULL` if `patch_analysis = FALSE`.}
#' }
#'
#' @export
#' @examples
#' sample_csv <- system.file("extdata", "sample_survey.csv", package = "oystermapR")
#' result <- predict_oyster(sample_csv, "ostrea_edulis", verbose = FALSE)
#'
#' # Automatic cell size estimation
#' s <- area_summary(result)
#'
#' # Known grid spacing, 500 m2 minimum viable patch
#' s2 <- area_summary(result, cell_size_m = 25, viable_area_m2 = 500)
#'
#' # Access the patch table and class summary
#' s$patches
#' s$class_summary
#' s$total["suitable_area_m2"]
area_summary <- function(result,
                         cell_size_m    = NULL,
                         viable_area_m2 = 100,
                         patch_analysis = TRUE,
                         classes        = c("High", "Moderate", "Low",
                                            "Very Low", "Excluded"),
                         verbose        = TRUE) {

  # ---- Validate ---------------------------------------------------------------
  required <- c("lat", "lon", "suitability_class", "suitability", "excluded")
  missing  <- setdiff(required, names(result))
  if (length(missing) > 0) {
    cli::cli_abort(c(
      "Required columns missing from {.arg result}.",
      "i" = "Run {.fn predict_oyster} first.",
      "x" = "Missing: {.val {missing}}"
    ))
  }

  # ---- Estimate cell size from median nearest-neighbour distance --------------
  if (is.null(cell_size_m)) {
    cell_size_m <- .estimate_cell_size_m(result$lon, result$lat)
    if (verbose) {
      cli::cli_inform(c(
        "i" = "Cell size estimated from median point spacing: {round(cell_size_m, 1)} m"
      ))
    }
  }

  cell_area_m2 <- cell_size_m^2

  # ---- Class summary ----------------------------------------------------------
  class_levels <- c("High", "Moderate", "Low", "Very Low", "Excluded")
  result$suitability_class <- factor(result$suitability_class,
                                     levels = class_levels)

  cs_rows <- lapply(class_levels, function(cls) {
    sub   <- result[!is.na(result$suitability_class) &
                      result$suitability_class == cls, ]
    n_pts <- nrow(sub)
    area  <- n_pts * cell_area_m2
    data.frame(
      class           = cls,
      n_points        = n_pts,
      area_m2         = round(area, 1),
      area_ha         = round(area / 10000, 4),
      mean_suitability   = if (n_pts > 0) round(mean(sub$suitability, na.rm = TRUE), 3) else NA_real_,
      median_suitability = if (n_pts > 0) round(stats::median(sub$suitability, na.rm = TRUE), 3) else NA_real_,
      stringsAsFactors = FALSE
    )
  })
  cs <- do.call(rbind, cs_rows)
  cs <- cs[cs$class %in% classes, ]

  total_area_m2   <- sum(cs$area_m2)
  suitable_area_m2 <- sum(cs$area_m2[cs$class %in% c("High", "Moderate")])

  cs$pct_total_area    <- round(cs$area_m2 / total_area_m2 * 100, 2)
  cs$pct_suitable_area <- round(
    ifelse(cs$class %in% c("High", "Moderate"),
           cs$area_m2 / pmax(suitable_area_m2, 1) * 100, NA_real_), 2)

  # Reorder columns
  cs <- cs[, c("class", "n_points", "area_m2", "area_ha",
               "pct_total_area", "pct_suitable_area",
               "mean_suitability", "median_suitability")]
  rownames(cs) <- NULL

  # ---- Total summary vector ---------------------------------------------------
  total_vec <- c(
    surveyed_area_m2  = round(total_area_m2,    1),
    surveyed_area_ha  = round(total_area_m2 / 10000, 4),
    suitable_area_m2  = round(suitable_area_m2, 1),
    suitable_area_ha  = round(suitable_area_m2 / 10000, 4),
    pct_suitable      = round(suitable_area_m2 / pmax(total_area_m2, 1) * 100, 2),
    cell_size_m       = round(cell_size_m, 1),
    n_points_total    = nrow(result)
  )

  # ---- Patch analysis ---------------------------------------------------------
  patches_df <- NULL

  if (patch_analysis && requireNamespace("terra", quietly = TRUE)) {
    patches_df <- tryCatch(
      .run_patch_analysis(result, cell_size_m, viable_area_m2),
      error = function(e) {
        cli::cli_warn("Patch analysis failed: {conditionMessage(e)}")
        NULL
      }
    )
  } else if (patch_analysis) {
    cli::cli_warn(c(
      "!" = "{.pkg terra} not available; patch analysis skipped.",
      "i" = "Install with {.code install.packages('terra')}."
    ))
  }

  # ---- Print summary ----------------------------------------------------------
  if (verbose) {
    cli::cli_h2("Area Summary")
    cli::cli_inform(c(
      "i" = "Total surveyed area:  {format(round(total_area_m2), big.mark=',')} m2  ({round(total_area_m2/10000, 3)} ha)",
      "i" = "Suitable area (High + Moderate): {format(round(suitable_area_m2), big.mark=',')} m2  ({round(suitable_area_m2/10000, 3)} ha)  [{round(total_vec['pct_suitable'], 1)}%]",
      "i" = "Cell size used: {round(cell_size_m, 1)} m  ({round(cell_area_m2)} m2 per point)"
    ))
    cli::cli_text("")
    cli::cli_text("Per-class breakdown:")
    for (i in seq_len(nrow(cs))) {
      r <- cs[i, ]
      if (r$n_points == 0) next
      cli::cli_inform(
        "  {r$class}: {format(r$area_m2, big.mark=',')} m2  ({r$area_ha} ha)  [{r$pct_total_area}% of survey]  mean suit. {r$mean_suitability}"
      )
    }
    if (!is.null(patches_df) && nrow(patches_df) > 0) {
      cli::cli_text("")
      n_viable <- sum(patches_df$viable, na.rm = TRUE)
      cli::cli_inform(c(
        "i" = "Contiguous suitable patches: {nrow(patches_df)} total,  {n_viable} meet viable threshold ({viable_area_m2} m2)",
        "i" = "Largest patch: {format(round(max(patches_df$area_m2)), big.mark=',')} m2  ({round(max(patches_df$area_m2)/10000, 4)} ha)"
      ))
    }
  }

  invisible(list(
    class_summary = cs,
    total         = total_vec,
    patches       = patches_df
  ))
}


# ---- Internal helpers --------------------------------------------------------

#' Estimate cell size (m) from median nearest-neighbour spacing
#' @keywords internal
.estimate_cell_size_m <- function(lon, lat) {
  n <- length(lon)
  # Sample up to 400 points for speed
  idx   <- if (n > 400) sample.int(n, 400) else seq_len(n)
  s_lon <- lon[idx]
  s_lat <- lat[idx]

  nn_dists_deg <- vapply(seq_along(idx), function(i) {
    dx <- s_lon[i] - s_lon[-i]
    dy <- s_lat[i] - s_lat[-i]
    min(sqrt(dx^2 + dy^2))
  }, numeric(1))

  med_deg   <- stats::median(nn_dists_deg, na.rm = TRUE)
  mid_lat   <- mean(lat, na.rm = TRUE)
  m_per_deg <- 111000 * cos(mid_lat * pi / 180)

  # Cap at 500 m to avoid inflating area estimates from sparse tracklines
  min(med_deg * m_per_deg, 500)
}


#' Contiguous patch analysis via terra
#' @keywords internal
.run_patch_analysis <- function(result, cell_size_m, viable_area_m2) {
  # Only High and Moderate are "suitable" for patch purposes
  suitable <- result[result$suitability_class %in% c("High", "Moderate") &
                       !result$excluded, ]

  if (nrow(suitable) < 3) {
    cli::cli_inform("i" = "Too few suitable points for patch analysis (n < 3).")
    return(NULL)
  }

  # Convert cell size to degrees (approx)
  mid_lat    <- mean(result$lat, na.rm = TRUE)
  m_per_deg  <- 111000 * cos(mid_lat * pi / 180)
  res_deg    <- cell_size_m / m_per_deg

  # Build raster template over suitable extent
  ext_r <- terra::ext(
    min(suitable$lon) - res_deg,
    max(suitable$lon) + res_deg,
    min(suitable$lat) - res_deg,
    max(suitable$lat) + res_deg
  )
  r_tmpl <- terra::rast(ext_r, resolution = res_deg, crs = "EPSG:4326")

  # Rasterise suitability scores
  pts_v <- terra::vect(suitable, geom = c("lon", "lat"), crs = "EPSG:4326")
  suit_r <- terra::rasterize(pts_v, r_tmpl, field = "suitability",
                              fun = mean, na.rm = TRUE)

  # Binary suitable/not (1 = suitable cell)
  binary_r <- !is.na(suit_r)

  # Connected patches
  patched <- terra::patches(binary_r, directions = 8, zeroAsNA = TRUE)

  # Patch statistics
  n_patches <- max(terra::values(patched), na.rm = TRUE)
  if (is.na(n_patches) || n_patches == 0) return(NULL)

  cell_area_m2 <- cell_size_m^2

  patch_rows <- lapply(seq_len(n_patches), function(pid) {
    mask      <- patched == pid
    n_cells   <- sum(terra::values(mask), na.rm = TRUE)
    area_m2   <- n_cells * cell_area_m2

    # Extract suitability values in this patch
    suit_vals <- terra::values(suit_r)[terra::values(mask)]
    suit_vals <- suit_vals[!is.na(suit_vals)]
    mn_suit   <- if (length(suit_vals) > 0) round(mean(suit_vals), 3) else NA_real_

    # Dominant class
    dom_class <- if (!is.na(mn_suit) && mn_suit >= 0.70) "High" else "Moderate"

    data.frame(
      patch_id         = pid,
      class_dominant   = dom_class,
      n_cells          = n_cells,
      area_m2          = round(area_m2, 1),
      area_ha          = round(area_m2 / 10000, 5),
      mean_suitability = mn_suit,
      viable           = area_m2 >= viable_area_m2,
      stringsAsFactors = FALSE
    )
  })

  patches_df <- do.call(rbind, patch_rows)
  patches_df <- patches_df[order(-patches_df$area_m2), ]
  rownames(patches_df) <- NULL
  patches_df
}
