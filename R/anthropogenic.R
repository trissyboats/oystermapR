# =============================================================================
# Anthropogenic disturbance scoring
# =============================================================================
#
# Physical disturbance from commercial bottom trawling is one of the primary
# anthropogenic pressures on benthic habitats and is the dominant reason why
# many historically suitable oyster reef sites cannot be restored without
# regulatory protection.
#
# Data source: ICES VMS (Vessel Monitoring System) swept-area ratio data.
# The swept-area ratio (SAR) = total area swept by bottom-contact gear per
# unit seabed area per year. SAR > 1 means the entire seabed area was swept
# at least once per year on average.
#
# SAR data available from:
#  - ICES Data Portal (aggregated, publicly available, c-square resolution)
#    https://www.ices.dk/data/data-portals/Pages/VMS.aspx
#  - EMODnet Human Activities portal (annual, 0.05\u00b0 c-square grid)
#    https://www.emodnet-humanactivities.eu/
#  - ICES WGSFD (Working Group on Spatial Fisheries Data) \u2014 annual reports
#
# Additional disturbance sources (accepted as manual input):
#  - Anchor damage: recreational boating density in marina approaches
#  - Dredging/extraction: licensed marine aggregate extraction areas
#  - Cable/pipeline corridors: Ofgem/BEIS cable landing areas
#
# References:
#  Eigaard et al. (2017): The footprint of bottom trawling. ICES JMS.
#  Hiddink et al. (2017): Global analysis of depletion and recovery of seabed
#   biota after bottom trawling. PNAS.
#  ICES WGSFD (2022): Spatial Fisheries Data Working Group report.

#' Score anthropogenic disturbance at survey locations
#'
#' @description
#' Calculates a disturbance index \[0,1\] from bottom trawling intensity,
#' dredging activity, and anchor damage. The primary data input is the
#' ICES swept-area ratio (SAR), which can be supplied manually or fetched
#' live from the ICES VMS data portal.
#'
#' This output is primarily relevant to the gear feasibility and economic
#' viability modules. Very high trawling intensity (SAR > 0.5/yr) constitutes
#' a hard gate for bottom culture and restoration reef deployment — physical
#' gear deployment is not viable on actively trawled ground regardless of
#' ecological suitability.
#'
#' **Input options:**
#' 1. `trawling_data` — ICES VMS SAR data, manually downloaded as CSV/dataframe.
#' 2. `fetch_live = TRUE` — queries ICES VMS GeoServer endpoint.
#' 3. Neither — returns zero disturbance with a warning.
#'
#' @section trawling_data format:
#' A dataframe with columns:
#' - `lat`, `lon` — centroid of c-square or fishing location
#' - `sar` — swept-area ratio (numeric; dimensionless ratio per year)
#' - `gear_type` (optional) — "otter", "beam", "dredge", "seine" etc.
#'   Dredge gear has higher impact per SAR unit.
#'
#' @section Additional disturbance inputs:
#' - `anchor_data`: dataframe with `lat`, `lon`, `density` (vessel anchoring
#'   density index) for recreational/commercial anchorage areas.
#' - `dredge_areas`: dataframe with `lat`, `lon` identifying licensed
#'   aggregate extraction areas (all sites within the polygon receive score 1.0).
#'
#' @param result Dataframe from [predict_oyster()]. Must contain `lat`, `lon`.
#' @param trawling_data Dataframe of ICES VMS SAR data (see Details), or NULL.
#' @param anchor_data Dataframe of anchoring density data, or NULL.
#' @param dredge_areas Dataframe of dredging/extraction area centroids, or NULL.
#' @param fetch_live Logical. Query ICES VMS GeoServer (default FALSE).
#' @param match_radius_m Numeric. Spatial matching radius in metres (default
#'   5000 m, consistent with ICES c-square ~0.05° resolution).
#' @param verbose Logical. Default TRUE.
#'
#' @return `result` with additional columns:
#'   - `disturbance_sar`: swept-area ratio at or near each site (NA if no data)
#'   - `disturbance_score` \[0,1\]: composite anthropogenic disturbance index
#'   - `disturbance_class`: "Negligible" / "Low" / "Moderate" / "High" / "Active"
#'   - `disturbance_gear_gate`: logical — TRUE if SAR exceeds gear deployment
#'     threshold (used by `assess_gear_feasibility()`)
#'   - `disturbance_note`: management flag
#'
#' @export
#' @examples
#' \dontrun{
#' # Manual ICES VMS data (downloaded from ICES data portal)
#' vms <- read.csv("ices_vms_sar_2022.csv")  # columns: lat, lon, sar, gear_type
#' result <- score_anthropogenic_disturbance(result, trawling_data = vms)
#'
#' # Live ICES VMS fetch
#' result <- score_anthropogenic_disturbance(result, fetch_live = TRUE)
#' }
score_anthropogenic_disturbance <- function(result,
                                             trawling_data  = NULL,
                                             anchor_data    = NULL,
                                             dredge_areas   = NULL,
                                             fetch_live     = FALSE,
                                             match_radius_m = 5000,
                                             verbose        = TRUE) {

  if (!all(c("lat","lon") %in% names(result)))
    cli::cli_abort("result must contain 'lat' and 'lon' columns.")

  # \u2500\u2500 Live fetch \u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500
  if (fetch_live && is.null(trawling_data)) {
    if (verbose) cli::cli_inform("Fetching ICES VMS swept-area ratio data...")
    bbox <- c(
      lon_min = min(result$lon, na.rm=TRUE), lon_max = max(result$lon, na.rm=TRUE),
      lat_min = min(result$lat, na.rm=TRUE), lat_max = max(result$lat, na.rm=TRUE)
    )
    trawling_data <- tryCatch(
      .fetch_ices_vms(bbox, verbose),
      error = function(e) {
        cli::cli_warn("ICES VMS fetch failed: {conditionMessage(e)}. Setting disturbance = 0.")
        NULL
      }
    )
    if (!is.null(trawling_data)) {
      # Normalise ICES VMS field names
      if (!"sar" %in% names(trawling_data)) {
        sar_col <- intersect(c("SAR","SweptAreaRatio","swept_area_ratio","sar_all"),
                             names(trawling_data))
        if (length(sar_col) > 0) {
          trawling_data$sar <- as.numeric(trawling_data[[sar_col[1]]])
        } else {
          cli::cli_warn("Could not identify SAR column in ICES VMS response.")
          trawling_data <- NULL
        }
      }
    }
  }

  lat_mid   <- mean(result$lat, na.rm=TRUE)
  m_per_lat <- 111320
  m_per_lon <- 111320 * cos(lat_mid * pi / 180)
  rad_lat   <- match_radius_m / m_per_lat
  rad_lon   <- match_radius_m / m_per_lon

  result$disturbance_sar   <- NA_real_
  result$trawl_score       <- 0.0

  # \u2500\u2500 SAR-based trawling disturbance \u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500
  if (!is.null(trawling_data) &&
      all(c("lat","lon","sar") %in% names(trawling_data)) &&
      nrow(trawling_data) > 0) {

    trawling_data$sar <- as.numeric(trawling_data$sar)

    # Gear impact multiplier \u2014 dredge > beam > otter > seine
    gear_impact <- c(dredge=1.4, beam=1.2, otter=1.0, seine=0.8)
    if (!"gear_type" %in% names(trawling_data))
      trawling_data$gear_type <- "otter"

    for (i in seq_len(nrow(result))) {
      dlat <- abs(trawling_data$lat - result$lat[i])
      dlon <- abs(trawling_data$lon - result$lon[i])
      in_r <- dlat <= rad_lat & dlon <= rad_lon

      if (!any(in_r)) next

      nearby <- trawling_data[in_r, , drop=FALSE]
      # Take maximum SAR within radius (conservative)
      max_row  <- nearby[which.max(nearby$sar), , drop=FALSE]
      raw_sar  <- max_row$sar[1]
      result$disturbance_sar[i] <- raw_sar

      gt  <- tolower(as.character(max_row$gear_type[1]))
      gi  <- if (!is.na(gt) && any(grepl(gt, names(gear_impact), fixed=TRUE))) {
               gear_impact[grep(gt, names(gear_impact), fixed=TRUE)[1]]
             } else {
               1.0
             }

      # SAR scoring: 0 \u2192 0, 0.5 \u2192 0.6, 1.0 \u2192 0.85, >= 3 \u2192 1.0
      result$trawl_score[i] <- pmin(
        ifelse(raw_sar <= 0,   0,
        ifelse(raw_sar <= 0.1, 0.2 * raw_sar / 0.1,
        ifelse(raw_sar <= 0.5, 0.2 + 0.4 * (raw_sar - 0.1) / 0.4,
        ifelse(raw_sar <= 1.0, 0.6 + 0.25 * (raw_sar - 0.5) / 0.5,
        ifelse(raw_sar <= 3.0, 0.85 + 0.15 * (raw_sar - 1.0) / 2.0,
               1.0))))) * gi,
        1.0
      )
    }

    n_active <- sum(!is.na(result$disturbance_sar) & result$disturbance_sar > 0.5)
    if (verbose)
      cli::cli_inform(paste0(
        "Trawling: ", n_active, " site(s) with SAR > 0.5 (active trawling)."
      ))
  } else {
    if (is.null(trawling_data) && !fetch_live)
      cli::cli_warn(c(
        "No trawling data supplied \u2014 disturbance score set to 0.",
        "i" = "Supply trawling_data (ICES VMS SAR) or use fetch_live = TRUE."
      ))
  }

  # \u2500\u2500 Anchor damage \u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500
  anchor_score <- rep(0.0, nrow(result))
  if (!is.null(anchor_data) &&
      all(c("lat","lon") %in% names(anchor_data)) &&
      nrow(anchor_data) > 0) {

    if (!"density" %in% names(anchor_data)) anchor_data$density <- 1
    for (i in seq_len(nrow(result))) {
      dlat <- abs(anchor_data$lat - result$lat[i])
      dlon <- abs(anchor_data$lon - result$lon[i])
      in_r <- dlat <= rad_lat & dlon <= rad_lon
      if (!any(in_r)) next
      anchor_score[i] <- pmin(sum(anchor_data$density[in_r], na.rm=TRUE) / 10, 1.0)
    }
  }

  # \u2500\u2500 Dredge/extraction areas \u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500
  dredge_score <- rep(0.0, nrow(result))
  if (!is.null(dredge_areas) &&
      all(c("lat","lon") %in% names(dredge_areas)) &&
      nrow(dredge_areas) > 0) {

    for (i in seq_len(nrow(result))) {
      dlat <- abs(dredge_areas$lat - result$lat[i])
      dlon <- abs(dredge_areas$lon - result$lon[i])
      if (any(dlat <= rad_lat & dlon <= rad_lon))
        dredge_score[i] <- 1.0  # Hard: any extraction = maximum disturbance
    }
  }

  # \u2500\u2500 Composite \u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500
  result$disturbance_score <- pmin(
    pmax(result$trawl_score, anchor_score * 0.7, dredge_score),
    1.0
  )
  result$trawl_score <- NULL

  result$disturbance_class <- dplyr::case_when(
    result$disturbance_score < 0.10 ~ "Negligible",
    result$disturbance_score < 0.30 ~ "Low",
    result$disturbance_score < 0.55 ~ "Moderate",
    result$disturbance_score < 0.80 ~ "High",
    TRUE                            ~ "Active"
  )

  # Gear gate: SAR > 0.5/yr is the threshold above which physical gear
  # deployment is not viable (Hiddink et al. 2017; OSPAR background)
  result$disturbance_gear_gate <- !is.na(result$disturbance_sar) &
                                    result$disturbance_sar > 0.5

  result$disturbance_note <- dplyr::case_when(
    result$disturbance_class == "Negligible" ~ "No significant anthropogenic disturbance recorded.",
    result$disturbance_class == "Low"        ~ "Low disturbance history; minimal management required.",
    result$disturbance_class == "Moderate"   ~ "Moderate trawling/anchor pressure; confirm access rights before deployment.",
    result$disturbance_class == "High"       ~ paste0(
      "High disturbance (SAR ~", round(result$disturbance_sar, 2), "/yr). ",
      "Regulatory closure or fishery exclusion zone likely required for restoration."),
    result$disturbance_class == "Active"     ~ paste0(
      "Active trawling (SAR ~", round(result$disturbance_sar, 2), "/yr). ",
      "Gear deployment not viable without statutory protection. Bottom culture impossible."),
    TRUE ~ NA_character_
  )

  result
}
