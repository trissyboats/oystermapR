# =============================================================================
# Habitat Utility Functions for oystermapR
#
# add_intertidal_flag()  \u2014 derive intertidal/subtidal classification from
#                          chart-datum depth and local tidal range
# =============================================================================


#' Add an intertidal zone flag to a survey dataframe
#'
#' @description
#' Classifies each survey location as intertidal, subtidal, or supratidal
#' based on its depth below Chart Datum (CD) and the tidal range at the
#' nearest harmonic reference port. Chart Datum is approximately Lowest
#' Astronomical Tide (LAT), so:
#'
#' - **Subtidal:**    `depth > 0` (always submerged)
#' - **Intertidal:**  `-MHWS_above_CD <= depth <= 0`
#'   (exposed at some states of the tide; depth is at or above CD)
#' - **Supratidal:**  `depth < -MHWS_above_CD`
#'   (above the highest astronomical tide; excluded from scoring)
#'
#' MHWS above CD is approximated from the harmonic constituents of the
#' nearest port as: `Z0 + 1.1 * (M2_H + S2_H)` \u2014 the 10% factor accounts
#' for minor constituents not included in the 5-constituent model.
#'
#' This flag is used internally by [score_locations()] to give *Magallana
#' gigas* (Pacific oyster) full depth scores for intertidal cells, reflecting
#' its strong intertidal ecology in NW European waters.
#'
#' @param df Dataframe with `lat`, `lon`, and `depth` (chart-datum corrected)
#'   columns. Typically the output of [auto_tidal_correct()] or
#'   [correct_to_chart_datum()].
#' @param max_port_dist_km Numeric. Maximum distance to nearest harmonic port
#'   in km (default 75). If the survey centroid is further than this from all
#'   ports, the function falls back to a conservative intertidal window of 6 m
#'   above CD and issues a warning.
#' @param depth_col Character. Name of the chart-datum depth column
#'   (default `"depth"`).
#' @param verbose Logical. Print the nearest port name and derived MHWS
#'   (default `TRUE`).
#'
#' @return The input dataframe with an added `intertidal_zone` column:
#'   - `"subtidal"`: depth > 0 m CD
#'   - `"intertidal"`: 0 m CD >= depth >= -MHWS_above_CD
#'   - `"supratidal"`: depth < -MHWS_above_CD
#'
#' @export
#' @examples
#' \dontrun{
#' survey <- auto_tidal_correct(survey, datetime_col = "date")
#' survey <- add_intertidal_flag(survey)
#'
#' # Check intertidal coverage
#' table(survey$intertidal_zone)
#' }
add_intertidal_flag <- function(df,
                                 max_port_dist_km = 75,
                                 depth_col        = "depth",
                                 verbose          = TRUE) {

  if (!depth_col %in% names(df)) {
    cli::cli_abort(c(
      "Depth column {.val {depth_col}} not found.",
      "i" = "Run {.fn auto_tidal_correct} or {.fn correct_to_chart_datum} first."
    ))
  }

  depth <- as.numeric(df[[depth_col]])

  # \u2500\u2500 Find nearest harmonic port \u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500
  lat_col <- .find_col_any(df, c("lat", "latitude"))
  lon_col <- .find_col_any(df, c("lon", "longitude"))

  if (!is.null(lat_col) && !is.null(lon_col)) {
    centroid_lat <- mean(df[[lat_col]], na.rm = TRUE)
    centroid_lon <- mean(df[[lon_col]], na.rm = TRUE)

    dists <- vapply(names(.harmonic_ports), function(nm) {
      p <- .harmonic_ports[[nm]]
      .haversine_km(centroid_lat, centroid_lon, p$lat, p$lon)
    }, numeric(1))

    nearest_nm   <- names(dists)[which.min(dists)]
    nearest_dist <- min(dists)
    nearest_port <- .harmonic_ports[[nearest_nm]]
  } else {
    nearest_dist <- Inf
  }

  # \u2500\u2500 Derive MHWS above CD from harmonic constituents \u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500
  # MHWS \u2248 Z0 + 1.1 * (M2_H + S2_H)
  # The 1.1 factor adds ~10% for N2 and other minor constituents.
  FALLBACK_MHWS <- 6.0  # conservative fallback (m above CD)

  if (nearest_dist <= max_port_dist_km) {
    mhws_above_cd <- nearest_port$Z0 +
                     1.1 * (nearest_port$M2[1] + nearest_port$S2[1])
    mhws_above_cd <- max(mhws_above_cd, 0.5)   # sanity floor

    if (verbose) {
      cli::cli_inform(c(
        "i" = paste0("Intertidal flag: using {.val {nearest_nm}} ",
                     "({round(nearest_dist,1)} km from survey centroid)"),
        " " = "MHWS above CD: {round(mhws_above_cd, 2)} m"
      ))
    }
  } else {
    mhws_above_cd <- FALLBACK_MHWS
    cli::cli_warn(c(
      "!" = paste0("Survey centroid is {round(nearest_dist,1)} km from nearest port ",
                   "(threshold {max_port_dist_km} km)."),
      "i" = "Using fallback MHWS of {FALLBACK_MHWS} m above CD.",
      "i" = "Increase {.arg max_port_dist_km} or use {.arg mhws_override} for accuracy."
    ))
  }

  # \u2500\u2500 Classify zones \u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500
  zone <- dplyr::case_when(
    is.na(depth)                        ~ NA_character_,
    depth > 0                           ~ "subtidal",
    depth >= -mhws_above_cd             ~ "intertidal",
    TRUE                                ~ "supratidal"
  )

  df$intertidal_zone   <- zone
  df$mhws_above_cd_ref <- mhws_above_cd   # expose for QA

  if (verbose) {
    n_sub   <- sum(zone == "subtidal",    na.rm = TRUE)
    n_inter <- sum(zone == "intertidal",  na.rm = TRUE)
    n_supra <- sum(zone == "supratidal",  na.rm = TRUE)
    cli::cli_inform(c(
      " " = paste0("Subtidal: {n_sub}  |  Intertidal: {n_inter}  |  ",
                   "Supratidal: {n_supra}")
    ))
  }

  df
}
