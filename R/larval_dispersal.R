# =============================================================================
# Mechanistic larval dispersal and inter-patch connectivity scoring
# =============================================================================
#
# OVERVIEW
# --------
# Oyster larvae are planktonic during their pelagic larval duration (PLD).
# During this window they are advected by tidal currents and can settle on
# any suitable habitat within effective dispersal range. Whether a given site
# can self-sustain or depends on upstream larval supply is therefore a function
# of both local habitat quality AND connectivity to source populations.
#
# This module provides two complementary routes:
#
# ROUTE 1 \u2014 Gap-threshold union-find (built-in, no external data)
# ----------------------------------------------------------------
# Builds a dispersal network from the suitability map alone. Two suitable
# patches are considered capable of exchanging larvae if they are within the
# species' effective dispersal distance (derived from PLD \u00d7 mean tidal
# excursion). Union-Find groups all inter-reachable patches into dispersal
# clusters. Within each cluster, a connectivity score is computed from:
#   - n_larval_sources: number of suitable source patches within range
#   - source_quality_score: distance-weighted suitability of those sources
#   - cluster_size: total patches in dispersal network
#
# Kernel model: Gaussian decay kernel
#   w(d) = exp(\u2212(d / sigma)^2)
#   where sigma = dispersal_km / 2 (so w = e^{\u22121} \u2248 0.37 at dispersal_km/2)
#
# This encodes that nearby sources contribute more to settlement success
# than distant sources at the edge of the dispersal range \u2014 consistent with
# empirical settlement curves (Cowen & Sponaugle 2009; Siegel et al. 2008).
#
# ROUTE 2 \u2014 Connectivity matrix (optional; from particle tracking or literature)
# -------------------------------------------------------------------------------
# Accepts an external connectivity matrix as a dataframe of
# (source_lat, source_lon, dest_lat, dest_lon, weight) pairs \u2014 the output of
# particle tracking models (OpenDrift, FVCOM, ROMS) or a published empirical
# matrix (e.g. Robins et al. 2017 for the Irish Sea; Ayata et al. 2010 for
# the NE Atlantic). The matrix weights replace the Gaussian kernel for matched
# source\u2013destination pairs.
#
# HYBRID BEHAVIOUR
# ----------------
# When a connectivity_matrix is provided but only partially covers the survey
# area, Route 2 is used where matrix coverage exists and Route 1 (union-find)
# fills gaps. Each row records which source was used in larval_source column.
#
# SPECIES PELAGIC LARVAL DURATION (PLD) LOOKUP
# ---------------------------------------------
# Species             | PLD range  | Larval type      | Reference
# --------------------|------------|------------------|----------------------------
# Ostrea edulis       |  1\u20137 days  | lecithotrophic   | Laing & Walker (2003);
#                     |            |                  | Woolmer et al. (2009);
#                     |            |                  | Pogoda et al. (2019)
# Magallana gigas     | 14\u201328 days | planktotrophic   | Ruesink et al. (2005);
#                     |            |                  | Bayne (2017)
# Crassostrea angulata| 14\u201321 days | planktotrophic   | Flores-Vergara et al. (2004)
# Ostrea stentina     |  2\u20137 days  | lecithotrophic   | Sendra (2022);
#                     |            |                  | Gonz\u00e1lez-Wang\u00fcemert et al. (2019)
# Ostrea lurida       |  3\u201310 days | lecithotrophic   | Trimble et al. (2009);
#                     |            |                  | Kimbro et al. (2019)
#
# Effective dispersal distance:
#   dispersal_km \u2248 PLD_mean_days \u00d7 tidal_excursion_km_per_day
#
# Default tidal_excursion_km = 5 km/day (equivalent to mean current ~0.06 m/s
# \u00d7 24 h \u2014 typical for sheltered coastal sites in NW Europe / NE Pacific).
# Exposed or estuarine sites with strong tidal forcing may need 8\u201315 km/day.
#
# References:
#  Cowen R.K. & Sponaugle S. (2009) Larval dispersal and marine population
#    connectivity. Annual Review of Marine Science 1:443\u2013466.
#  Siegel D.A. et al. (2008) The stochastic nature of larval connectivity among
#    nearshore marine populations. PNAS 105:8974\u20138979.
#  Robins P.E. et al. (2017) Empirical and modelled larval dispersal for oysters.
#    J Appl Ecol 54:1699\u20131710. doi:10.1111/1365-2664.12854
#  Ayata S.D. et al. (2010) Dispersal in a nutrient-limited ocean. Ecology
#    Letters 13:1547\u20131558.
#  Woolmer A.P. et al. (2009) European flat oyster Ostrea edulis L. stock
#    assessment and potential for restoration in the Firth of Clyde, Scotland.
#    J Shellfish Res 28:107\u2013116.
#  Trimble A.C. et al. (2009) Restoring the Olympia oyster Ostrea lurida to
#    Puget Sound. Journal of Shellfish Research 28:43\u201353.
#  Laing I. & Walker P. (2003) Status of Ostrea edulis in England and Wales.
#    Hydrobiologia 188:29\u201342.


# \u2500\u2500 Species PLD lookup \u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500

.pld_params <- list(
  ostrea_edulis = list(
    pld_min_days  = 1,
    pld_max_days  = 7,
    pld_mean_days = 4,
    larval_type   = "lecithotrophic",
    settlement_note = paste0(
      "Lecithotrophic larvae (yolk-sac fed). Very limited active dispersal; ",
      "settlement overwhelmingly within 5 km of source reef. ",
      "Strong self-recruitment \u2014 isolated reefs may fail without artificial seeding."
    )
  ),
  magallana_gigas = list(
    pld_min_days  = 14,
    pld_max_days  = 28,
    pld_mean_days = 21,
    larval_type   = "planktotrophic",
    settlement_note = paste0(
      "Planktotrophic larvae (feeding in water column). Long PLD enables ",
      "connectivity over 50\u2013200 km. High inter-annual variability in dispersal ",
      "direction driven by residual currents."
    )
  ),
  crassostrea_angulata = list(
    pld_min_days  = 14,
    pld_max_days  = 21,
    pld_mean_days = 17,
    larval_type   = "planktotrophic",
    settlement_note = paste0(
      "Planktotrophic larvae; comparable dispersal to M. gigas. Estuarine ",
      "distribution concentrates larvae in riverine outflow plumes."
    )
  ),
  ostrea_stentina = list(
    pld_min_days  = 2,
    pld_max_days  = 7,
    pld_mean_days = 4,
    larval_type   = "lecithotrophic",
    settlement_note = paste0(
      "Lecithotrophic larvae; similar to O. edulis. Mar Menor is an enclosed ",
      "lagoon \u2014 dispersal is strongly constrained by tidal inlet hydrodynamics."
    )
  ),
  ostrea_lurida = list(
    pld_min_days  = 3,
    pld_max_days  = 10,
    pld_mean_days = 6,
    larval_type   = "lecithotrophic",
    settlement_note = paste0(
      "Lecithotrophic larvae; relatively short PLD. Pacific coast residual ",
      "currents are predominantly southward \u2014 northward recovery of reefs ",
      "from southern source populations is limited."
    )
  )
)


# \u2500\u2500 Haversine distance in km (vectorised) \u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500
.haversine_km_vec <- function(lat1, lon1, lat2, lon2) {
  R    <- 6371.0
  phi1 <- lat1 * pi / 180; phi2 <- lat2 * pi / 180
  dphi <- (lat2 - lat1) * pi / 180
  dlam <- (lon2 - lon1) * pi / 180
  a    <- sin(dphi/2)^2 + cos(phi1)*cos(phi2)*sin(dlam/2)^2
  2 * R * atan2(sqrt(a), sqrt(1 - a))
}


# \u2500\u2500 Main function \u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500

#' Score larval dispersal connectivity at survey locations
#'
#' @description
#' Estimates the larval connectivity of each survey location using one or both
#' of two approaches:
#'
#' **Route 1 — Built-in gap-threshold union-find (no external data needed):**
#' Patches above `min_suitability` that lie within the species' effective
#' dispersal kernel distance are grouped into dispersal clusters using
#' union-find. Within each cluster, a connectivity score is derived from the
#' number, quality, and distance-weighted contribution of potential larval
#' sources (Gaussian decay kernel, sigma = dispersal_km / 2).
#'
#' **Route 2 — External connectivity matrix (particle tracking / literature):**
#' When `connectivity_matrix` is supplied (a dataframe of source → destination
#' pairs with weights), those weights replace the Gaussian kernel for matched
#' pairs. Rows not covered by the matrix fall back to Route 1. The matrix can
#' come from:
#' - **OpenDrift** particle tracking simulations (export as source/destination
#'   settlement density CSV)
#' - **FVCOM / ROMS** model connectivity matrices
#' - **Published empirical matrices** (e.g. Robins et al. 2017)
#'
#' @section Dispersal distance:
#' If `dispersal_km` is not supplied, the effective kernel distance is derived
#' from the species' mean pelagic larval duration (PLD) multiplied by
#' `tidal_excursion_km` (mean daily tidal dispersal distance):
#'
#' ```
#' dispersal_km = PLD_mean_days × tidal_excursion_km
#' ```
#'
#' PLD values are literature-derived:
#' | Species | PLD | Larval type | Default dispersal |
#' |---|---|---|---|
#' | *O. edulis* | 1–7 days | lecithotrophic | ~20 km |
#' | *M. gigas* | 14–28 days | planktotrophic | ~105 km |
#' | *C. angulata* | 14–21 days | planktotrophic | ~85 km |
#' | *O. stentina* | 2–7 days | lecithotrophic | ~20 km |
#' | *O. lurida* | 3–10 days | lecithotrophic | ~30 km |
#'
#' @param result Dataframe from [predict_oyster()] with `lat`, `lon`,
#'   `suitability` columns.
#' @param species Character. Species key (e.g. `"ostrea_edulis"`). Used to
#'   look up pelagic larval duration for dispersal kernel. Required if
#'   `dispersal_km` is not supplied.
#' @param dispersal_km Numeric. Override for the effective dispersal kernel
#'   radius in km. When supplied, `species` PLD look-up is skipped.
#' @param pld_days Numeric. Override for the mean pelagic larval duration in
#'   days. Takes precedence over species lookup; ignored if `dispersal_km` is
#'   supplied.
#' @param tidal_excursion_km Numeric. Mean daily tidal dispersal distance in km
#'   (default 5 km/day approx. mean current 0.06 m/s × 24 h). Increase for highly
#'   tidal or estuarine sites (8–15 km/day). Ignored if `dispersal_km` is
#'   supplied.
#' @param min_suitability Numeric. Minimum suitability score for a patch to
#'   qualify as a potential larval source or sink (default 0.40). Unsuitable
#'   patches still receive a connectivity score reflecting their proximity to
#'   sources.
#' @param connectivity_matrix Dataframe or NULL. External connectivity weights
#'   (Route 2). Must contain columns:
#'   - `source_lat`, `source_lon`: coordinates of the larval source
#'   - `dest_lat`, `dest_lon`: coordinates of the destination
#'   - `weight`: connectivity weight \[0, 1\] (e.g. settlement probability or
#'     proportional larval flux). Higher = stronger connection.
#'   OpenDrift export tip: `\dontrun{write.csv(connectivity_matrix, "cm.csv")}`.
#' @param matrix_match_radius_deg Numeric. Spatial matching tolerance in decimal
#'   degrees for linking matrix entries to result rows (default 0.10 approx. 10 km).
#' @param verbose Logical. Print dispersal parameters and connectivity summary
#'   (default TRUE).
#'
#' @return `result` with additional columns:
#'   - `larval_dispersal_km`: effective kernel distance used
#'   - `larval_pld_days`: PLD used (species default or override)
#'   - `larval_type`: "lecithotrophic" or "planktotrophic"
#'   - `n_larval_sources`: suitable patches within dispersal range
#'   - `source_quality_score` \[0,1\]: Gaussian-weighted quality of nearby sources
#'   - `larval_cluster_id`: dispersal network cluster ID (integer; NA if
#'     below min_suitability threshold and no sources nearby)
#'   - `larval_cluster_size`: number of suitable patches in dispersal network
#'   - `nearest_source_km`: km to nearest suitable source patch
#'   - `larval_connectivity_score` \[0,1\]: composite dispersal connectivity score
#'   - `larval_connectivity_class`: "Highly connected" / "Connected" /
#'     "Low connectivity" / "Isolated"
#'   - `larval_connectivity_note`: plain-language ecological interpretation
#'   - `larval_source`: "union_find", "matrix", or "matrix+union_find"
#'
#' @export
#' @references
#' Cowen R.K. & Sponaugle S. (2009) Annual Review of Marine Science 1:443–466.
#' Robins P.E. et al. (2017) J Applied Ecology 54:1699–1710. \doi{10.1111/1365-2664.12854}
#' Siegel D.A. et al. (2008) PNAS 105:8974–8979.
#' Woolmer A.P. et al. (2009) J Shellfish Research 28:107–116.
#' Trimble A.C. et al. (2009) J Shellfish Research 28:43–53.
#'
#' @examples
#' \dontrun{
#' result <- predict_oyster(survey, "ostrea_edulis")
#'
#' # Route 1 only — built-in dispersal kernel
#' result <- score_larval_connectivity(result, species = "ostrea_edulis")
#'
#' # Route 1 with custom dispersal distance (e.g. strong tidal excursion)
#' result <- score_larval_connectivity(result, species = "ostrea_edulis",
#'                                     tidal_excursion_km = 10)
#'
#' # Route 1 with manual dispersal override
#' result <- score_larval_connectivity(result, dispersal_km = 8)
#'
#' # Route 2 — external connectivity matrix from OpenDrift
#' cm <- read.csv("opendrift_connectivity.csv")
#' # Required columns: source_lat, source_lon, dest_lat, dest_lon, weight
#' result <- score_larval_connectivity(result,
#'            species             = "ostrea_edulis",
#'            connectivity_matrix = cm)
#'
#' # Inspect isolated patches — need artificial seeding
#' isolated <- subset(result,
#'   larval_connectivity_class == "Isolated" & suitability_class == "High")
#'
#' # Strong candidates: high suitability AND high connectivity
#' targets <- subset(result,
#'   suitability_class == "High" & larval_connectivity_class == "Highly connected")
#' }
score_larval_connectivity <- function(result,
                                       species              = NULL,
                                       dispersal_km         = NULL,
                                       pld_days             = NULL,
                                       tidal_excursion_km   = 5.0,
                                       min_suitability      = 0.40,
                                       connectivity_matrix  = NULL,
                                       matrix_match_radius_deg = 0.10,
                                       verbose              = TRUE) {

  # \u2500\u2500 Input validation \u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500
  required <- c("lat", "lon", "suitability")
  missing  <- setdiff(required, names(result))
  if (length(missing) > 0)
    cli::cli_abort("result is missing required columns: {paste(missing, collapse=', ')}.")

  if (is.null(dispersal_km) && is.null(species) && is.null(pld_days))
    cli::cli_abort(c(
      "Must supply at least one of: {.arg species}, {.arg dispersal_km}, {.arg pld_days}.",
      "i" = "Use species = \"ostrea_edulis\" for automatic PLD lookup,",
      "i" = "or dispersal_km = 15 to set the kernel distance directly."
    ))

  # \u2500\u2500 Resolve dispersal kernel distance \u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500
  pld_entry   <- NULL
  pld_used    <- NA_real_
  larval_type <- "unknown"

  if (!is.null(dispersal_km)) {
    # Direct override \u2014 ignore species/pld
    if (!is.numeric(dispersal_km) || dispersal_km <= 0)
      cli::cli_abort("{.arg dispersal_km} must be a positive number.")
    kern_km     <- dispersal_km
    pld_used    <- NA_real_
    larval_type <- if (!is.null(species)) {
      sp_key <- tolower(species)
      if (sp_key %in% names(.pld_params)) .pld_params[[sp_key]]$larval_type else "unknown"
    } else "unknown"

  } else if (!is.null(pld_days)) {
    # Manual PLD override
    if (!is.numeric(pld_days) || pld_days <= 0)
      cli::cli_abort("{.arg pld_days} must be a positive number.")
    kern_km     <- pld_days * tidal_excursion_km
    pld_used    <- pld_days

  } else {
    # Species lookup
    sp_key <- tolower(trimws(species))
    # Handle alternative keys (e.g. "crassostrea_gigas" \u2192 "magallana_gigas")
    if (sp_key == "crassostrea_gigas") sp_key <- "magallana_gigas"
    if (!sp_key %in% names(.pld_params))
      cli::cli_abort(c(
        "No PLD data for species {.val {sp_key}}.",
        "i" = "Supported keys: {paste(names(.pld_params), collapse=', ')}",
        "i" = "Or supply {.arg dispersal_km} directly."
      ))
    pld_entry   <- .pld_params[[sp_key]]
    pld_used    <- pld_entry$pld_mean_days
    kern_km     <- pld_used * tidal_excursion_km
    larval_type <- pld_entry$larval_type
  }

  sigma_km <- kern_km / 2   # Gaussian kernel half-width (e^{-1} \u2248 0.37 at kern_km/2)

  if (verbose) {
    cli::cli_h2("Larval Dispersal Connectivity")
    cli::cli_inform(c(
      " " = "Dispersal kernel: {round(kern_km, 1)} km (sigma = {round(sigma_km, 1)} km)",
      " " = if (!is.na(pld_used))
              paste0("PLD: ", round(pld_used, 1), " days \u00d7 tidal excursion ",
                     round(tidal_excursion_km, 1), " km/day")
            else
              paste0("Dispersal supplied directly: ", round(kern_km, 1), " km"),
      " " = paste0("Larval type: ", larval_type),
      " " = paste0("Min suitability threshold for source: ", min_suitability),
      " " = paste0("Matrix route: ", if (!is.null(connectivity_matrix)) "enabled" else "disabled")
    ))
  }

  n    <- nrow(result)
  lats <- result$lat
  lons <- result$lon
  suit <- result$suitability

  # \u2500\u2500 Identify source patches (suitable cells above threshold) \u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500
  is_source <- !is.na(suit) & suit >= min_suitability
  src_idx   <- which(is_source)
  n_src     <- length(src_idx)

  # \u2500\u2500 Initialise output vectors \u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500
  n_sources_vec  <- integer(n)
  src_qual_vec   <- numeric(n)
  nearest_km_vec <- rep(NA_real_, n)
  conn_source    <- rep("union_find", n)

  # \u2500\u2500 Route 2 \u2014 Connectivity matrix \u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500
  matrix_covered <- logical(n)

  if (!is.null(connectivity_matrix)) {
    # Validate matrix columns
    req_cm <- c("source_lat", "source_lon", "dest_lat", "dest_lon", "weight")
    missing_cm <- setdiff(req_cm, names(connectivity_matrix))
    if (length(missing_cm) > 0)
      cli::cli_abort(c(
        "{.arg connectivity_matrix} is missing columns: {paste(missing_cm, collapse=', ')}.",
        "i" = "Required: source_lat, source_lon, dest_lat, dest_lon, weight"
      ))

    cm_weight <- as.numeric(connectivity_matrix$weight)
    if (any(!is.finite(cm_weight) | cm_weight < 0 | cm_weight > 1, na.rm = TRUE))
      cli::cli_warn(c(
        "!" = "Some connectivity_matrix weight values are outside [0, 1] or non-finite.",
        "i" = "These rows will be ignored."
      ))
    cm_valid <- !is.na(cm_weight) & is.finite(cm_weight) & cm_weight >= 0 & cm_weight <= 1
    connectivity_matrix <- connectivity_matrix[cm_valid, ]
    cm_weight <- cm_weight[cm_valid]

    r2_deg <- matrix_match_radius_deg

    if (verbose)
      cli::cli_inform(c("i" = "Connectivity matrix: {nrow(connectivity_matrix)} valid source-destination pairs."))

    # For each result row, find matrix entries where dest \u2248 result location
    for (i in seq_len(n)) {
      dest_match <- (abs(connectivity_matrix$dest_lat - lats[i]) <= r2_deg) &
                    (abs(connectivity_matrix$dest_lon - lons[i]) <= r2_deg)
      if (!any(dest_match)) next

      cm_sub      <- connectivity_matrix[dest_match, ]
      sub_weights <- cm_weight[dest_match]

      # Further filter: only from source patches that are suitable in result
      # Match matrix sources to result rows to check their suitability
      src_suit_weights <- vapply(seq_len(nrow(cm_sub)), function(k) {
        src_lat <- cm_sub$source_lat[k]
        src_lon <- cm_sub$source_lon[k]
        # Find nearest result row to this source point
        d2 <- (lats - src_lat)^2 + (lons - src_lon)^2
        j  <- which.min(d2)
        if (sqrt(d2[j]) > r2_deg) return(0)   # no matching result cell
        if (is.na(suit[j]) || suit[j] < min_suitability) return(0)
        sub_weights[k] * suit[j]               # weight \u00d7 source suitability
      }, numeric(1))

      if (sum(src_suit_weights) == 0) next

      n_sources_vec[i]  <- sum(src_suit_weights > 0)
      src_qual_vec[i]   <- min(1, sum(src_suit_weights))  # cap at 1
      matrix_covered[i] <- TRUE
      conn_source[i]    <- "matrix"

      # Nearest source distance
      dist_to_srcs <- .haversine_km_vec(lats[i], lons[i],
                                         cm_sub$source_lat, cm_sub$source_lon)
      nearest_km_vec[i] <- min(dist_to_srcs, na.rm = TRUE)
    }

    n_matrix <- sum(matrix_covered)
    n_fallback <- n - n_matrix
    if (verbose)
      cli::cli_inform(c(
        "i" = "Matrix route: {n_matrix} row{?s} scored from connectivity matrix.",
        "i" = if (n_fallback > 0)
                paste0(n_fallback, " row(s) not covered by matrix \u2014 using union-find fallback.")
              else
                "All rows covered by matrix."
      ))
  }

  # \u2500\u2500 Route 1 \u2014 Gap-threshold union-find + Gaussian kernel \u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500
  # Applied to all rows not covered by the matrix

  needs_route1 <- !matrix_covered

  if (any(needs_route1) && n_src > 0) {

    # For each result row (not matrix-covered), compute distance to every source
    # and accumulate: n_sources, Gaussian-weighted quality score, nearest dist
    for (i in which(needs_route1)) {
      if (n_src == 0) break

      dist_km <- .haversine_km_vec(lats[i], lons[i], lats[src_idx], lons[src_idx])
      within  <- dist_km <= kern_km & src_idx != i  # exclude self

      if (any(within)) {
        d_in     <- dist_km[within]
        s_in     <- suit[src_idx[within]]
        # Gaussian kernel weights
        w_gauss  <- exp(-(d_in / sigma_km)^2)
        contrib  <- w_gauss * s_in

        n_sources_vec[i]  <- sum(within)
        src_qual_vec[i]   <- min(1, sum(contrib))
        nearest_km_vec[i] <- min(d_in)
      } else {
        # No sources within range; record nearest source distance anyway
        # Exclude self to avoid a spurious 0-km distance for source sites
        others <- dist_km[src_idx != i]
        if (length(others) > 0) nearest_km_vec[i] <- min(others)
      }

      if (matrix_covered[i] && conn_source[i] == "matrix") {
        conn_source[i] <- "matrix+union_find"
      }
    }
  }

  # \u2500\u2500 Union-find \u2014 dispersal cluster assignment \u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500
  # Cluster all SUITABLE patches that are within kern_km of each other.
  # Unsuitable patches (below min_suitability) are not assigned to clusters
  # but can still receive connectivity scores from nearby source clusters.

  cluster_id   <- rep(NA_integer_, n)
  cluster_size <- rep(NA_integer_, n)

  if (n_src >= 2) {

    # Build union-find over source indices only
    parent_s <- seq_len(n_src)
    rank_s   <- integer(n_src)

    find_s <- function(i) {
      while (parent_s[i] != i) {
        parent_s[i] <<- parent_s[parent_s[i]]
        i <- parent_s[i]
      }
      i
    }
    union_s <- function(i, j) {
      ri <- find_s(i); rj <- find_s(j)
      if (ri == rj) return(invisible(NULL))
      if (rank_s[ri] < rank_s[rj])       parent_s[ri] <<- rj
      else if (rank_s[ri] > rank_s[rj])  parent_s[rj] <<- ri
      else { parent_s[rj] <<- ri; rank_s[ri] <<- rank_s[ri] + 1L }
    }

    # Connect pairs within dispersal kernel distance
    for (a in seq_len(n_src - 1)) {
      d_ab <- .haversine_km_vec(lats[src_idx[a]], lons[src_idx[a]],
                                 lats[src_idx[(a+1):n_src]],
                                 lons[src_idx[(a+1):n_src]])
      hits <- which(d_ab <= kern_km) + a
      for (b in hits) union_s(a, b)
    }

    # Resolve roots and create sequential cluster IDs
    roots      <- vapply(seq_len(n_src), find_s, integer(1))
    root_map   <- setNames(seq_along(unique(roots)), unique(roots))
    patch_cids <- root_map[as.character(roots)]
    patch_szs  <- as.integer(table(patch_cids)[as.character(patch_cids)])

    cluster_id[src_idx]   <- patch_cids
    cluster_size[src_idx] <- patch_szs

    # Assign non-source cells to the nearest source cluster (if within 1.5\u00d7 kern)
    non_src_idx <- which(!is_source)
    if (length(non_src_idx) > 0 && n_src > 0) {
      for (i in non_src_idx) {
        d_to_src <- .haversine_km_vec(lats[i], lons[i], lats[src_idx], lons[src_idx])
        j_near   <- which.min(d_to_src)
        if (d_to_src[j_near] <= kern_km * 1.5) {
          cluster_id[i]   <- patch_cids[j_near]
          cluster_size[i] <- patch_szs[j_near]
        }
      }
    }

  } else if (n_src == 1) {
    cluster_id[src_idx]   <- 1L
    cluster_size[src_idx] <- 1L
  }

  # \u2500\u2500 Composite connectivity score [0, 1] \u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500
  #
  # score = 0.50 * source_quality_score    (weighted larval supply)
  #       + 0.30 * cluster_size_score      (network size effect)
  #       + 0.20 * proximity_score         (penalty for distance to nearest source)
  #
  # cluster_size_score = 1 - exp(- cluster_size / 5)  (saturates at ~20 patches)
  # proximity_score    = max(0, 1 - nearest_km / (kern_km * 2))

  csize_score <- ifelse(is.na(cluster_size) | cluster_size == 0,
                        0,
                        pmin(1, 1 - exp(-cluster_size / 5)))

  prox_score  <- ifelse(is.na(nearest_km_vec),
                        0,
                        pmax(0, 1 - nearest_km_vec / (kern_km * 2)))

  conn_score  <- pmin(1, pmax(0,
    0.50 * src_qual_vec +
    0.30 * csize_score  +
    0.20 * prox_score
  ))

  # \u2500\u2500 Classification \u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500
  conn_class <- dplyr::case_when(
    conn_score >= 0.60                         ~ "Highly connected",
    conn_score >= 0.35                         ~ "Connected",
    conn_score >= 0.10                         ~ "Low connectivity",
    TRUE                                       ~ "Isolated"
  )

  # \u2500\u2500 Settlement note \u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500
  sp_note <- if (!is.null(pld_entry)) pld_entry$settlement_note else
             paste0("Effective dispersal kernel: ", round(kern_km, 1), " km.")

  conn_note <- dplyr::case_when(
    conn_class == "Highly connected" ~ paste0(
      "Well within larval supply network (", round(kern_km, 1), " km kernel). ",
      "Natural recruitment likely if upstream reefs are reproductively active. ",
      sp_note),
    conn_class == "Connected" ~ paste0(
      "Moderate larval supply. Natural recruitment possible but may be ",
      "stochastic. Artificial seeding increases establishment probability. ",
      sp_note),
    conn_class == "Low connectivity" ~ paste0(
      "Limited larval supply. Site is near the margin of the dispersal network. ",
      "Restoration success likely requires seeded spat or broodstock transplant. ",
      sp_note),
    conn_class == "Isolated" ~ paste0(
      "No connected source reefs within dispersal range (", round(kern_km, 1), " km). ",
      "Self-seeding is unlikely. Artificial seeding with pathogen-screened spat ",
      "is required for establishment. ", sp_note),
    TRUE ~ NA_character_
  )

  # \u2500\u2500 Write back \u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500
  result$larval_dispersal_km        <- round(kern_km, 2)
  result$larval_pld_days            <- if (!is.na(pld_used)) round(pld_used, 1) else NA_real_
  result$larval_type                <- larval_type
  result$n_larval_sources           <- n_sources_vec
  result$source_quality_score       <- round(src_qual_vec, 4)
  result$larval_cluster_id          <- cluster_id
  result$larval_cluster_size        <- cluster_size
  result$nearest_source_km          <- round(nearest_km_vec, 3)
  result$larval_connectivity_score  <- round(conn_score, 4)
  result$larval_connectivity_class  <- conn_class
  result$larval_connectivity_note   <- conn_note
  result$larval_source              <- conn_source

  # \u2500\u2500 Summary \u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500
  if (verbose) {
    tab <- table(conn_class)
    cli::cli_h3("Larval Connectivity Summary")
    for (cl in names(tab))
      cli::cli_inform("  {cl}: {tab[cl]} site{?s}")
    n_isolated <- sum(conn_class == "Isolated", na.rm = TRUE)
    n_high_suit_isolated <- sum(conn_class == "Isolated" &
                                  !is.na(suit) & suit >= 0.6, na.rm = TRUE)
    if (n_high_suit_isolated > 0)
      cli::cli_warn(c(
        "!" = paste0(n_high_suit_isolated,
                     " high-suitability site{?s} {is/are} isolated from larval supply."),
        "i" = "These are candidate sites for priority artificial seeding."
      ))
    if (!is.null(connectivity_matrix)) {
      n_mat <- sum(conn_source %in% c("matrix","matrix+union_find"))
      n_uf  <- sum(conn_source == "union_find")
      cli::cli_inform(c(
        "i" = "Connectivity source \u2014 matrix: {n_mat} | union-find: {n_uf}"
      ))
    }
  }

  result
}


# \u2500\u2500 Utility: build connectivity matrix from OpenDrift CSV \u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500

#' Parse an OpenDrift particle tracking output into a connectivity matrix
#'
#' @description
#' Converts the output of an OpenDrift settlement simulation (a CSV file with
#' particle origin and final position) into the connectivity matrix format
#' expected by `score_larval_connectivity(connectivity_matrix = ...)`.
#'
#' **OpenDrift setup:**
#' Run OpenDrift (`OceanDrift` or `OpenOil` adapted for larvae) seeding particles
#' at each source reef location. Export a CSV with at minimum:
#' - `origin_lat`, `origin_lon` — seed position
#' - `final_lat`, `final_lon` — settlement position (stranded or settled)
#' - `status` — filter to `"stranded"` or `"active"` as appropriate
#'
#' The function bins particles into a grid and computes the fraction of particles
#' from each source bin that arrive in each destination bin.
#'
#' @param opendrift_csv Character. Path to OpenDrift settlement CSV file. Must
#'   contain columns `origin_lat`, `origin_lon`, `final_lat`, `final_lon`.
#' @param bin_deg Numeric. Grid cell size in decimal degrees for binning
#'   (default 0.05 approx. 5 km at mid-latitudes).
#' @param status_col Character or NULL. Name of a status column to filter on.
#'   If NULL (default), all rows are used.
#' @param status_keep Character. Value(s) of `status_col` to include
#'   (default `c("stranded","settled")`).
#' @param min_weight Numeric. Minimum connectivity weight to include in output
#'   (default 0.005 = 0.5 percent). Filters very weak connections.
#' @param verbose Logical. Default TRUE.
#'
#' @return A dataframe with columns `source_lat`, `source_lon`, `dest_lat`,
#'   `dest_lon`, `weight` — ready for passing to
#'   [score_larval_connectivity()].
#'
#' @export
#' @examples
#' \dontrun{
#' # After running OpenDrift simulation and exporting CSV:
#' cm <- parse_opendrift_connectivity("opendrift_output.csv", bin_deg = 0.05)
#'
#' result <- predict_oyster(survey, "ostrea_edulis")
#' result <- score_larval_connectivity(result,
#'             species             = "ostrea_edulis",
#'             connectivity_matrix = cm)
#' }
parse_opendrift_connectivity <- function(opendrift_csv,
                                          bin_deg     = 0.05,
                                          status_col  = NULL,
                                          status_keep = c("stranded","settled"),
                                          min_weight  = 0.005,
                                          verbose     = TRUE) {

  if (!file.exists(opendrift_csv))
    cli::cli_abort("File not found: {.path {opendrift_csv}}")

  df <- utils::read.csv(opendrift_csv, stringsAsFactors = FALSE)

  req <- c("origin_lat","origin_lon","final_lat","final_lon")
  miss <- setdiff(req, names(df))
  if (length(miss) > 0)
    cli::cli_abort(c(
      "CSV missing required columns: {paste(miss, collapse=', ')}.",
      "i" = "Expected: origin_lat, origin_lon, final_lat, final_lon"
    ))

  if (!is.null(status_col)) {
    if (!status_col %in% names(df))
      cli::cli_warn("status_col '{status_col}' not found in CSV; using all rows.")
    else {
      df <- df[df[[status_col]] %in% status_keep, ]
      if (verbose)
        cli::cli_inform("After status filter: {nrow(df)} particle{?s} retained.")
    }
  }

  df <- df[complete.cases(df[, req]), ]
  n_particles <- nrow(df)

  if (n_particles == 0)
    cli::cli_abort("No valid rows after filtering. Check status_col and status_keep.")

  # Bin to grid
  bin_lat <- function(x) round(floor(x / bin_deg) * bin_deg + bin_deg / 2, 6)
  bin_lon <- function(x) round(floor(x / bin_deg) * bin_deg + bin_deg / 2, 6)

  df$src_lat_bin  <- bin_lat(df$origin_lat)
  df$src_lon_bin  <- bin_lon(df$origin_lon)
  df$dest_lat_bin <- bin_lat(df$final_lat)
  df$dest_lon_bin <- bin_lon(df$final_lon)

  # Count particles per source bin (denominator)
  src_counts <- table(paste(df$src_lat_bin, df$src_lon_bin, sep = "_"))

  # Count source\u2192destination pairs
  pair_key    <- paste(df$src_lat_bin, df$src_lon_bin,
                        df$dest_lat_bin, df$dest_lon_bin, sep = "_")
  pair_counts <- table(pair_key)

  # Build output dataframe
  keys <- strsplit(names(pair_counts), "_")
  cm_rows <- lapply(seq_along(pair_counts), function(k) {
    parts    <- keys[[k]]
    src_lat  <- as.numeric(parts[1])
    src_lon  <- as.numeric(parts[2])
    dest_lat <- as.numeric(parts[3])
    dest_lon <- as.numeric(parts[4])
    src_key  <- paste(src_lat, src_lon, sep = "_")
    n_src    <- as.integer(src_counts[src_key])
    weight   <- as.numeric(pair_counts[k]) / n_src
    data.frame(source_lat = src_lat, source_lon = src_lon,
               dest_lat   = dest_lat, dest_lon   = dest_lon,
               weight     = weight, stringsAsFactors = FALSE)
  })
  cm <- do.call(rbind, cm_rows)

  # Filter weak connections
  cm <- cm[cm$weight >= min_weight, ]

  if (verbose) {
    cli::cli_inform(c(
      "v" = "OpenDrift connectivity matrix built.",
      "i" = "  Particles used: {n_particles}",
      "i" = "  Unique source bins: {length(unique(paste(cm$source_lat, cm$source_lon)))}",
      "i" = "  Unique dest bins:   {length(unique(paste(cm$dest_lat,   cm$dest_lon)))}",
      "i" = "  Source-destination pairs: {nrow(cm)} (weight >= {min_weight})"
    ))
  }

  cm
}
