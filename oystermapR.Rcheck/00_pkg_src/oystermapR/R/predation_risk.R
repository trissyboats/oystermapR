# =============================================================================
# Predation and bioturbation pressure scoring
# =============================================================================
#
# Key predators of O. edulis and M. gigas spat and juveniles:
#  - Asterias rubens (common starfish): primary predator of subtidal oysters,
#    particularly devastating for newly deployed spat bags and restoration reefs
#  - Carcinus maenas (green shore crab): dominant intertidal/shallow subtidal
#  - Ocenebra erinaceus (sting winkle / oyster drill): native UK; bores
#    through shell of juveniles < 30 mm
#  - Urosalpinx cinerea (American oyster drill): invasive; SE England only
#  - Nucella lapillus (dog whelk): minor at restoration densities
#
# Predation risk is scored from:
#  1. Direct predator occurrence/density data (manually supplied or EMODnet)
#  2. Substrate rugosity proxy (high rugosity = more refugia = lower risk)
#  3. Depth proxy (starfish most abundant 0-30 m, drills intertidal-5 m)
#
# References:
#  Kamermans et al. (2004): Combined effects of copper and predation on O. edulis.
#  Kamermans & Smaal (2002): Oyster culture and predation control in the Netherlands.
#  Zwerschke et al. (2021): Starfish predation on O. edulis restoration reefs.

#' Score predation and bioturbation pressure at survey locations
#'
#' @description
#' Produces a predation risk index (0 = negligible, 1 = severe) based on
#' predator occurrence data, substrate rugosity, and depth. The output is
#' intended as an **overlay** on ecological suitability \u2014 high predation risk
#' does not reduce the habitat suitability score directly but is flagged in
#' a separate column for management planning (e.g. cage exclusion, deep
#' subtidal placement to avoid intertidal drills).
#'
#' **Input options (in priority order):**
#' 1. `predator_data` \u2014 manually supplied dataframe with predator records
#'    (see Details).
#' 2. `fetch_live = TRUE` \u2014 queries EMODnet Biology for *Asterias rubens* and
#'    *Carcinus maenas* occurrence records within the survey extent. Requires
#'    internet access and the `httr` package.
#' 3. Neither \u2014 returns a depth-proxy-only risk score with a warning.
#'
#' @section predator_data format:
#' A dataframe with columns:
#' - `lat`, `lon` \u2014 coordinates
#' - `species` \u2014 species name or AphiaID (character)
#' - `density` (optional) \u2014 relative abundance / density index; if absent,
#'   each record is treated as presence-only (density = 1)
#' - `date` (optional) \u2014 survey date (used to filter to relevant season)
#'
#' @param result Dataframe from [predict_oyster()] with `lat`, `lon`, and
#'   optionally `depth_m` and `substrate` columns.
#' @param predator_data Dataframe of predator records (see Details), or NULL.
#' @param fetch_live Logical. Query EMODnet Biology API (default FALSE).
#' @param match_radius_m Numeric. Radius in metres within which predator
#'   records are aggregated per survey cell (default 1000 m).
#' @param species Character. Target oyster species \u2014 affects which predators
#'   are most relevant (`"ostrea_edulis"` or `"magallana_gigas"`).
#' @param verbose Logical. Default TRUE.
#'
#' @return `result` with additional columns:
#'   - `predation_risk` [0,1]: composite risk index
#'   - `predation_risk_class`: "Low" / "Moderate" / "High" / "Severe"
#'   - `predation_risk_note`: text flag for management planning
#'   - `n_predator_records`: number of predator records within match_radius_m
#'
#' @export
#' @examples
#' \dontrun{
#' # Manual data
#' pred <- data.frame(lat=51.5, lon=-4.1, species="Asterias rubens", density=3)
#' result <- score_predation_risk(result, predator_data=pred, species="ostrea_edulis")
#'
#' # Live EMODnet fetch
#' result <- score_predation_risk(result, fetch_live=TRUE)
#'
#' # Depth-proxy only (no predator data)
#' result <- score_predation_risk(result)
#' }
score_predation_risk <- function(result,
                                  predator_data   = NULL,
                                  fetch_live      = FALSE,
                                  match_radius_m  = 1000,
                                  species         = "ostrea_edulis",
                                  verbose         = TRUE) {

  if (!all(c("lat","lon") %in% names(result)))
    cli::cli_abort("result must contain 'lat' and 'lon' columns.")

  # \u2500\u2500 Fetch live predator data \u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500
  if (fetch_live && is.null(predator_data)) {
    if (verbose) cli::cli_inform("Fetching predator records from EMODnet Biology...")
    bbox <- c(
      lon_min = min(result$lon, na.rm=TRUE), lon_max = max(result$lon, na.rm=TRUE),
      lat_min = min(result$lat, na.rm=TRUE), lat_max = max(result$lat, na.rm=TRUE)
    )
    predator_data <- tryCatch(
      .fetch_emodnet_predators(bbox, verbose),
      error = function(e) {
        cli::cli_warn("EMODnet predator fetch failed: {conditionMessage(e)}. Using depth proxy only.")
        NULL
      }
    )
    if (!is.null(predator_data)) {
      # Standardise EMODnet column names
      if (!"density" %in% names(predator_data)) predator_data$density <- 1
      if (!"species" %in% names(predator_data) && "scientificName" %in% names(predator_data))
        predator_data$species <- predator_data$scientificName
    }
  }

  # \u2500\u2500 Predator species weights by target oyster species \u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500
  # Weights reflect relative damage caused to each oyster species life stage
  predator_weights <- list(
    ostrea_edulis  = c("Asterias rubens"  = 1.0,
                       "Carcinus maenas"  = 0.8,
                       "Ocenebra erinaceus" = 0.7,
                       "Urosalpinx cinerea" = 0.9,
                       "Nucella lapillus" = 0.3),
    magallana_gigas = c("Asterias rubens" = 0.6,
                        "Carcinus maenas" = 0.5,
                        "Ocenebra erinaceus" = 0.4,
                        "Urosalpinx cinerea" = 0.7,
                        "Nucella lapillus"  = 0.2)
  )
  sp_weights <- predator_weights[[species]]
  if (is.null(sp_weights)) sp_weights <- predator_weights[["ostrea_edulis"]]

  lat_mid   <- mean(result$lat, na.rm=TRUE)
  m_per_lat <- 111320
  m_per_lon <- 111320 * cos(lat_mid * pi / 180)
  radius_deg_lat <- match_radius_m / m_per_lat
  radius_deg_lon <- match_radius_m / m_per_lon

  result$n_predator_records  <- 0L
  result$predation_pressure  <- 0.0

  # \u2500\u2500 Score predator pressure per cell \u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500
  if (!is.null(predator_data) &&
      all(c("lat","lon") %in% names(predator_data)) &&
      nrow(predator_data) > 0) {

    if (!"density" %in% names(predator_data)) predator_data$density <- 1
    if (!"species" %in% names(predator_data)) predator_data$species <- "unknown"

    for (i in seq_len(nrow(result))) {
      dlat <- abs(predator_data$lat - result$lat[i])
      dlon <- abs(predator_data$lon - result$lon[i])
      in_r <- dlat <= radius_deg_lat & dlon <= radius_deg_lon

      if (!any(in_r)) next

      nearby <- predator_data[in_r, , drop=FALSE]
      result$n_predator_records[i] <- nrow(nearby)

      # Weighted density sum
      pressure <- 0
      for (j in seq_len(nrow(nearby))) {
        sp   <- nearby$species[j]
        dens <- as.numeric(nearby$density[j])
        if (is.na(dens)) dens <- 1
        # Match species name (partial, case-insensitive)
        wt_idx <- which(vapply(names(sp_weights), function(n)
          grepl(n, sp, ignore.case=TRUE), logical(1)))
        wt <- if (length(wt_idx) > 0) max(sp_weights[wt_idx]) else 0.5
        pressure <- pressure + wt * dens
      }
      result$predation_pressure[i] <- pressure
    }

    if (verbose)
      cli::cli_inform(paste0("Predation: ", sum(result$n_predator_records > 0),
                             " cell(s) with nearby predator records."))
  } else {
    if (verbose)
      cli::cli_warn(c(
        "No predator data supplied \u2014 using depth proxy only.",
        "i" = "Supply predator_data or set fetch_live = TRUE for a more complete estimate."
      ))
  }

  # \u2500\u2500 Depth proxy for starfish abundance \u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500
  # Asterias rubens peaks at 0-30 m, negligible below 50 m
  depth_risk <- rep(0.5, nrow(result))  # default if no depth column
  if ("depth_m" %in% names(result)) {
    d <- abs(result$depth_m)
    depth_risk <- ifelse(d <= 5,  0.7,
                  ifelse(d <= 15, 0.9,
                  ifelse(d <= 30, 0.6,
                  ifelse(d <= 50, 0.3, 0.1))))
  }

  # \u2500\u2500 Substrate rugosity proxy \u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500
  # High rugosity = more refuge = lower predation risk
  rugosity_factor <- rep(1.0, nrow(result))
  if ("substrate" %in% names(result)) {
    sub <- tolower(as.character(result$substrate))
    rugosity_factor <- ifelse(grepl("hard|reef|rock|shell|gravel", sub), 0.7,
                       ifelse(grepl("mixed", sub), 0.85,
                       ifelse(grepl("sand",  sub), 1.0, 1.1)))  # soft = more exposure
  }

  # \u2500\u2500 Composite risk \u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500
  # Normalise predation_pressure to [0,1] (95th percentile = 1.0)
  max_pressure <- quantile(result$predation_pressure, 0.95, na.rm=TRUE)
  if (max_pressure > 0) {
    norm_pressure <- pmin(result$predation_pressure / max_pressure, 1.0)
  } else {
    norm_pressure <- rep(0, nrow(result))
  }

  # If we have real predator data, weight it 60%; depth proxy 40%
  has_records <- result$n_predator_records > 0
  w_rec   <- if (any(has_records)) 0.60 else 0.0
  w_depth <- 1.0 - w_rec

  result$predation_risk <- pmin(
    (w_rec * norm_pressure + w_depth * depth_risk) * rugosity_factor,
    1.0
  )

  result$predation_risk_class <- dplyr::case_when(
    result$predation_risk < 0.25 ~ "Low",
    result$predation_risk < 0.50 ~ "Moderate",
    result$predation_risk < 0.75 ~ "High",
    TRUE                         ~ "Severe"
  )

  result$predation_risk_note <- dplyr::case_when(
    result$predation_risk_class == "Low"      ~ "Minimal predation management likely required.",
    result$predation_risk_class == "Moderate" ~ "Monitor predator populations; consider seasonal exclusion cages.",
    result$predation_risk_class == "High"     ~ "Predator exclusion cages or elevated culture strongly recommended.",
    result$predation_risk_class == "Severe"   ~ "Active predator control essential; restoration likely to fail without intervention.",
    TRUE ~ NA_character_
  )

  # Clean up intermediate column
  result$predation_pressure <- NULL

  result
}
