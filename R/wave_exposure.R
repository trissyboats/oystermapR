# =============================================================================
# Wave exposure and effective fetch scoring
# =============================================================================
#
# Wave energy at a coastal site is determined by:
#  1. Effective fetch \u2014 the unobstructed distance over which wind acts on water
#  2. Wind speed and duration
#  3. Local bathymetry (depth-limited wave breaking)
#
# Depth alone is a poor proxy for wave energy \u2014 a 5 m site in a sheltered
# sea loch has negligible wave energy, while a 5 m site on an exposed
# headland may experience significant wave height > 3 m. This is one of
# the most common errors in simple habitat suitability models.
#
# Fetch \u2192 Wave Height:
#  The JONSWAP growth curve (Hasselmann et al. 1973) is the standard
#  empirical relationship between fetch, wind speed, and significant wave
#  height (Hs) for fetch-limited conditions:
#
#    Hs = 0.0248 * (U^2/g) * (g*F/U^2)^0.42
#
#  where: U = wind speed (m/s), F = fetch (m), g = 9.81 m/s\u00b2
#  (JONSWAP empirical coefficients from Hasselmann et al. 1973)
#
#  Depth limiting: in shallow water, Hs is capped at ~0.6 * depth (wave
#  breaking criterion; Battjes & Janssen 1978).
#
# Oyster-relevant wave thresholds:
#  Hs < 0.5 m  \u2014 negligible wave exposure (sheltered)
#  Hs 0.5-1.5 m \u2014 moderate; suitable for most subtidal gear with anchoring
#  Hs 1.5-2.5 m \u2014 exposed; longline and raft culture marginal
#  Hs > 2.5 m  \u2014 severely exposed; essentially all aquaculture gear unsuitable
#  O. edulis on hard substrate can tolerate up to ~1.5 m Hs in adults;
#  spat bags and flat oyster trays fail above ~0.8 m Hs.
#
# References:
#  Hasselmann et al. (1973): Measurements of wind-wave growth and swell decay.
#   Dtsch. Hydrogr. Z. Suppl. A8, No. 12.
#  Battjes & Janssen (1978): Energy loss and set-up due to breaking in random waves.
#   ICCE 1978, ASCE.
#  Thomas (1997): An explicit approach to fetch determination. Aquacultural Engineering.
#  Falconer et al. (2013): Modelling support for siting of shellfish farms. AEI.

#' Score wave exposure from fetch or measured wave height
#'
#' @description
#' Produces a wave exposure score \[0,1\] and significant wave height estimate
#' for each survey location. High wave exposure reduces gear feasibility and
#' can destabilise restoration reef structures and spat bags.
#'
#' **Input options:**
#' - `fetch_km`: effective fetch in kilometres (distance to nearest land or
#'   obstruction in the prevailing wind direction). Most useful for estuarine,
#'   sea loch, or enclosed bay sites. Can be measured from GIS (e.g. in QGIS
#'   using the Fetch tool) or estimated from chart inspection.
#' - `wave_height_m`: directly measured or modelled significant wave height
#'   (Hs). If supplied, `fetch_km` is ignored for Hs computation.
#' - Both absent: wave exposure is estimated from depth alone (coarse proxy;
#'   a warning is issued).
#'
#' @param result Dataframe from [predict_oyster()]. Must contain `lat`, `lon`.
#'   Optional columns used if present: `depth_m`, `fetch_km`, `wave_height_m`.
#' @param fetch_col Character. Name of a column in `result` containing fetch
#'   values in km. Alternative to supplying a scalar `fetch_km`.
#' @param wave_height_col Character. Name of a column in `result` containing
#'   measured/modelled Hs in metres.
#' @param fetch_km Numeric scalar. Uniform fetch in km applied to all rows
#'   (overridden by `fetch_col` if both supplied).
#' @param wave_height_m Numeric scalar. Uniform Hs in metres applied to all
#'   rows (overridden by `wave_height_col` if both supplied).
#' @param wind_speed_ms Numeric. Design wind speed in m/s used for JONSWAP
#'   calculation (default 12 m/s = moderate gale, Beaufort 6, typical design
#'   condition for coastal aquaculture siting).
#' @param depth_limit Logical. Apply depth-limited wave breaking cap
#'   (Hs_max = 0.6 * depth_m). Default TRUE.
#' @param verbose Logical. Default TRUE.
#'
#' @return `result` with additional columns:
#'   - `wave_hs_m`: significant wave height (m) — computed or supplied
#'   - `wave_exposure_score` \[0,1\]: 0 = fully sheltered, 1 = severely exposed
#'   - `wave_exposure_class`: "Sheltered" / "Moderate" / "Exposed" / "Severe"
#'   - `wave_exposure_note`: gear implication flag
#'   - `wave_source`: method used ("measured", "jonswap_fetch", "depth_proxy")
#'
#' @export
#' @examples
#' \dontrun{
#' # Fetch column in result
#' result$fetch_km <- c(2.5, 8.0, 25.0, 45.0)
#' result <- score_wave_exposure(result, fetch_col = "fetch_km")
#'
#' # Uniform fetch for a sheltered sea loch
#' result <- score_wave_exposure(result, fetch_km = 3.5)
#'
#' # Measured wave height column
#' result <- score_wave_exposure(result, wave_height_col = "hs_m")
#'
#' # Depth proxy only (coarse)
#' result <- score_wave_exposure(result)
#' }
score_wave_exposure <- function(result,
                                 fetch_col       = NULL,
                                 wave_height_col = NULL,
                                 fetch_km        = NULL,
                                 wave_height_m   = NULL,
                                 wind_speed_ms   = 12,
                                 depth_limit     = TRUE,
                                 verbose         = TRUE) {

  if (!all(c("lat","lon") %in% names(result)))
    cli::cli_abort("result must contain 'lat' and 'lon' columns.")

  g <- 9.81  # m/s\u00b2
  U <- wind_speed_ms

  n <- nrow(result)
  hs_vec    <- rep(NA_real_, n)
  method    <- rep("depth_proxy", n)

  # \u2500\u2500 Source 1: measured / modelled Hs \u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500
  if (!is.null(wave_height_col) && wave_height_col %in% names(result)) {
    hs_vec <- as.numeric(result[[wave_height_col]])
    method[!is.na(hs_vec)] <- "measured"
    if (verbose) cli::cli_inform("Wave height: using measured column '{wave_height_col}'.")
  } else if (!is.null(wave_height_m)) {
    hs_vec[] <- wave_height_m
    method   <- rep("measured", n)
    if (verbose) cli::cli_inform("Wave height: using supplied scalar Hs = {wave_height_m} m.")
  }

  # \u2500\u2500 Source 2: JONSWAP from fetch \u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500
  fetch_m <- rep(NA_real_, n)

  if (!is.null(fetch_col) && fetch_col %in% names(result)) {
    fetch_m <- as.numeric(result[[fetch_col]]) * 1000
    if (verbose) cli::cli_inform("Wave height: computing Hs from fetch column '{fetch_col}' via JONSWAP.")
  } else if (!is.null(fetch_km)) {
    fetch_m[] <- fetch_km * 1000
    if (verbose) cli::cli_inform("Wave height: computing Hs from scalar fetch = {fetch_km} km via JONSWAP.")
  }

  need_jonswap <- is.na(hs_vec) & !is.na(fetch_m) & fetch_m > 0
  if (any(need_jonswap)) {
    # JONSWAP significant wave height formula (Hasselmann et al. 1973)
    # Hs = 0.0248 * (U^2/g) * (g*F/U^2)^0.42
    F_j       <- fetch_m[need_jonswap]
    dim_fetch <- (g * F_j) / (U^2)
    hs_vec[need_jonswap] <- 0.0248 * (U^2 / g) * (dim_fetch^0.42)
    method[need_jonswap] <- "jonswap_fetch"
  }

  # \u2500\u2500 Source 3: depth proxy (fallback) \u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500
  still_na <- is.na(hs_vec)
  if (any(still_na)) {
    if (verbose)
      cli::cli_warn(c(
        "{sum(still_na)} row(s) have no fetch or measured wave height.",
        "i" = "Using depth as a coarse wave exposure proxy (unreliable for enclosed sites).",
        "i" = "Supply fetch_col or wave_height_col for accurate estimates."
      ))
    if ("depth_m" %in% names(result)) {
      d <- abs(result$depth_m[still_na])
      d[is.na(d)] <- 10
      # Rough empirical: very shallow open coast ~0.5 m Hs; deep open ocean ~3+ m
      # This proxy is deliberately conservative \u2014 it only avoids complete nulls
      hs_vec[still_na] <- pmin(0.08 * pmax(d, 0.5)^0.6, 3.0)
    } else {
      hs_vec[still_na] <- 1.0  # neutral default
    }
    method[still_na] <- "depth_proxy"
  }

  # \u2500\u2500 Depth limiting (wave breaking) \u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500
  if (depth_limit && "depth_m" %in% names(result)) {
    d_abs <- abs(result$depth_m)
    d_abs[is.na(d_abs)] <- Inf
    hs_cap <- 0.6 * d_abs
    hs_vec <- pmin(hs_vec, hs_cap)
  }

  result$wave_hs_m <- round(hs_vec, 3)
  result$wave_source <- method

  # \u2500\u2500 Wave exposure score [0,1] \u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500
  # Breakpoints:
  #  Hs = 0.0 m \u2192 score = 0.0 (fully sheltered)
  #  Hs = 0.5 m \u2192 score = 0.15
  #  Hs = 1.0 m \u2192 score = 0.40
  #  Hs = 1.5 m \u2192 score = 0.60
  #  Hs = 2.5 m \u2192 score = 0.85
  #  Hs >= 4.0 m \u2192 score = 1.0

  result$wave_exposure_score <- pmin(
    ifelse(hs_vec < 0.5,  hs_vec / 0.5 * 0.15,
    ifelse(hs_vec < 1.0,  0.15 + (hs_vec - 0.5) / 0.5 * 0.25,
    ifelse(hs_vec < 1.5,  0.40 + (hs_vec - 1.0) / 0.5 * 0.20,
    ifelse(hs_vec < 2.5,  0.60 + (hs_vec - 1.5) / 1.0 * 0.25,
    ifelse(hs_vec < 4.0,  0.85 + (hs_vec - 2.5) / 1.5 * 0.15,
                          1.0))))),
    1.0
  )

  result$wave_exposure_class <- dplyr::case_when(
    result$wave_hs_m < 0.5  ~ "Sheltered",
    result$wave_hs_m < 1.5  ~ "Moderate",
    result$wave_hs_m < 2.5  ~ "Exposed",
    TRUE                    ~ "Severe"
  )

  result$wave_exposure_note <- dplyr::case_when(
    result$wave_exposure_class == "Sheltered" ~ "All gear types viable from a wave exposure perspective.",
    result$wave_exposure_class == "Moderate"  ~ "Most gear viable; longlines require appropriate anchoring specification.",
    result$wave_exposure_class == "Exposed"   ~
      paste0("Hs ~", round(result$wave_hs_m, 1),
             " m. Longline/raft marginal; bottom culture and intertidal rack require robust mooring."),
    result$wave_exposure_class == "Severe"    ~
      paste0("Hs ~", round(result$wave_hs_m, 1),
             " m. Commercial gear deployment not viable; restoration reef substrate anchoring required."),
    TRUE ~ NA_character_
  )

  if (verbose) {
    tab <- table(result$wave_exposure_class)
    for (cl in names(tab))
      cli::cli_inform("  Wave exposure {cl}: {tab[cl]} site{?s} (median Hs {round(median(result$wave_hs_m[result$wave_exposure_class==cl],na.rm=TRUE),2)} m)")
  }

  result
}


# \u2500\u2500 Utility: estimate fetch from a bearing \u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500

#' Estimate effective fetch from a set of lat/lon points and a bearing
#'
#' @description
#' Simple great-circle ray-tracing to estimate how far open water extends
#' from a point in a given direction, up to `max_fetch_km`. This is a
#' lightweight GIS-free approximation — for precise fetch, use QGIS or
#' the `fetchR` R package.
#'
#' @param lat Numeric. Site latitude (decimal degrees).
#' @param lon Numeric. Site longitude (decimal degrees).
#' @param bearing_deg Numeric. Prevailing wind direction to check (0 = North).
#' @param land_polygons SpatialPolygons or sf POLYGON object representing
#'   land. If NULL, returns max_fetch_km (fully open ocean assumed).
#' @param max_fetch_km Numeric. Maximum fetch to search (default 200 km).
#' @param step_km Numeric. Ray step size (default 1 km).
#'
#' @return Numeric. Estimated fetch in km.
#' @export
estimate_fetch <- function(lat, lon, bearing_deg,
                            land_polygons  = NULL,
                            max_fetch_km   = 200,
                            step_km        = 1) {

  if (is.null(land_polygons)) return(max_fetch_km)

  has_sf <- requireNamespace("sf", quietly = TRUE)
  if (!has_sf)
    cli::cli_abort("estimate_fetch() with land polygons requires the {.pkg sf} package.")

  bearing_rad <- bearing_deg * pi / 180
  n_steps     <- ceiling(max_fetch_km / step_km)

  for (k in seq_len(n_steps)) {
    d_km <- k * step_km
    # Approximate lat/lon displacement (equirectangular, acceptable at survey scales)
    dlat <- (d_km * cos(bearing_rad)) / 111.32
    dlon <- (d_km * sin(bearing_rad)) / (111.32 * cos(lat * pi / 180))
    pt   <- sf::st_point(c(lon + dlon, lat + dlat))
    pt_sf <- sf::st_sfc(pt, crs = 4326)

    if (any(sf::st_intersects(pt_sf, land_polygons, sparse = FALSE))) {
      return(d_km - step_km)
    }
  }
  max_fetch_km
}
