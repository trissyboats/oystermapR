# =============================================================================
# Spatial smoothing of suitability scores
# =============================================================================

#' Apply Gaussian kernel smoothing to suitability scores
#'
#' @description
#' Individual CTD or ADCP casts are point measurements subject to instrument
#' noise, micro-scale patchiness, and tidal state variability. `smooth_suitability()`
#' applies a Gaussian distance-weighted kernel to the suitability scores, replacing
#' each cell's score with a weighted mean of all cells within `bandwidth_m` metres.
#' The result is a spatially coherent surface that better reflects broad habitat
#' quality rather than single-cast noise.
#'
#' The smoothed score is computed as:
#' \deqn{\hat{s}_i = \frac{\sum_j w_{ij} \cdot s_j}{\sum_j w_{ij}}}
#' where \eqn{w_{ij} = \exp(-d_{ij}^2 / (2\sigma^2))} and \eqn{\sigma} is the
#' Gaussian bandwidth in metres. Excluded cells (suitability = 0, excluded flag)
#' are down-weighted but included so that unsuitable barriers are preserved.
#'
#' @param result Dataframe from [predict_oyster()] with `lat`, `lon`,
#'   `suitability`, and `excluded` columns.
#' @param bandwidth_m Numeric. Gaussian kernel bandwidth (1 sigma) in metres.
#'   Default 500 m. Increase for sparser surveys; decrease to preserve fine detail.
#' @param max_radius_m Numeric. Maximum search radius for neighbours in metres.
#'   Cells further than this are ignored (default `3 * bandwidth_m`).
#' @param smooth_excluded Logical. If FALSE (default), excluded cells do not
#'   contribute to neighbours' smoothed scores (hard barriers are preserved).
#' @param verbose Logical. Print summary (default TRUE).
#'
#' @return Input dataframe with `suitability_raw` (original) and
#'   `suitability` (smoothed, overwrites original) columns, plus a
#'   `suitability_class` column reclassified from the smoothed score.
#'
#' @export
#' @examples
#' \dontrun{
#' result <- predict_oyster(survey, "ostrea_edulis")
#'
#' # Smooth with 300 m bandwidth (good for dense ADCP surveys)
#' result_smooth <- smooth_suitability(result, bandwidth_m = 300)
#'
#' # Compare raw vs smoothed
#' plot(result$suitability_raw, result$suitability,
#'      xlab = "Raw", ylab = "Smoothed", pch = 20)
#' }
smooth_suitability <- function(result,
                                bandwidth_m    = 500,
                                max_radius_m   = NULL,
                                smooth_excluded = FALSE,
                                verbose        = TRUE) {

  required <- c("lat", "lon", "suitability")
  missing  <- setdiff(required, names(result))
  if (length(missing) > 0)
    cli::cli_abort("result is missing columns: {paste(missing, collapse=', ')}.")

  if (!"excluded" %in% names(result)) result$excluded <- FALSE
  result$excluded <- as.logical(result$excluded)
  result$excluded[is.na(result$excluded)] <- FALSE

  if (is.null(max_radius_m)) max_radius_m <- 3 * bandwidth_m

  sigma   <- bandwidth_m
  n       <- nrow(result)

  # Convert lat/lon to approximate metres (equirectangular, sufficient for
  # survey-scale distances < 100 km)
  lat_mid  <- mean(result$lat, na.rm = TRUE)
  m_per_deg_lat <- 111320
  m_per_deg_lon <- 111320 * cos(lat_mid * pi / 180)

  x <- result$lon * m_per_deg_lon
  y <- result$lat * m_per_deg_lat

  result$suitability_raw <- result$suitability
  smoothed <- numeric(n)

  for (i in seq_len(n)) {
    dx   <- x - x[i]
    dy   <- y - y[i]
    dist <- sqrt(dx^2 + dy^2)

    in_radius <- dist <= max_radius_m

    if (!smooth_excluded) in_radius <- in_radius & !result$excluded

    if (sum(in_radius) == 0) {
      smoothed[i] <- result$suitability[i]
      next
    }

    d_sub <- dist[in_radius]
    s_sub <- result$suitability_raw[in_radius]
    w     <- exp(-(d_sub^2) / (2 * sigma^2))
    smoothed[i] <- sum(w * s_sub) / sum(w)
  }

  smoothed <- pmin(1, pmax(0, smoothed))

  result$suitability <- smoothed
  result$suitability_class <- cut(
    smoothed,
    breaks = c(-Inf, 0.3, 0.5, 0.7, Inf),
    labels = c("Very Low", "Low", "Moderate", "High"),
    right  = TRUE
  )
  result$suitability_class <- as.character(result$suitability_class)
  result$suitability_class[result$excluded] <- "Excluded"

  if (verbose) {
    delta <- result$suitability - result$suitability_raw
    cli::cli_inform(c(
      "i" = paste0("Smoothed {n} cells | bandwidth = ", bandwidth_m,
                   " m | max radius = ", max_radius_m, " m"),
      " " = paste0("Mean absolute change: ", round(mean(abs(delta), na.rm=TRUE), 4)),
      " " = paste0("Max change: ", round(max(abs(delta), na.rm=TRUE), 4))
    ))
  }

  result
}
