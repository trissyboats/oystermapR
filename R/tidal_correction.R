# =============================================================================
# Tidal Height Correction for oystermapR
#
# Surveys conducted at different states of the tide record different water
# depths for the same point on the seabed. Without correction, a site surveyed
# at high water reads ~3 m deeper than the same site at low water, producing
# misleading depth scores and therefore misleading suitability outputs.
#
# correct_to_chart_datum() converts raw survey depths to depths below Chart
# Datum (CD), approximately Lowest Astronomical Tide (LAT). This is the
# standard vertical datum for marine charts (e.g. Admiralty charts, UKHO).
#
# Formula:  depth_CD = depth_surveyed + tidal_height_above_CD
#
#   depth_surveyed      = depth below water surface at time of survey
#   tidal_height_above_CD = height of water surface above Chart Datum at that moment
#   depth_CD            = depth below Chart Datum (what goes on a marine chart)
#
# The tidal height can be supplied as:
#   (a) A single scalar \u2014 same offset applied to all rows (suitable when the
#       survey was conducted within a single tidal state, e.g. all at slack water).
#   (b) A per-row numeric vector \u2014 one value per row (from a tide gauge time
#       series matched to survey timestamps).
#   (c) NULL \u2014 no correction applied; a warning is issued.
#
# auto_tidal_correct() uses embedded harmonic constituents (M2, S2, N2, K1, O1)
# for 31 UK/European standard ports to predict tidal heights automatically from
# survey timestamps, when the survey area is within max_port_dist_km of a port.
# =============================================================================


# =============================================================================
# HARMONIC CONSTITUENT DATA
#
# M2, S2, N2, K1, O1 amplitudes (metres) and Greenwich phase lags (degrees)
# for 31 UK and European standard ports.
#
# Sources:
#   UKHO Admiralty Tide Tables Vol 1 (NW Europe) and Vol 2 (UK & Ireland)
#   SHOM (Service Hydrographique et Oc\u00e9anographique de la Marine) France
#   IOC Sea Level Station Database; BSH (Germany); RWS (Netherlands)
#
# Format per port: list(lat, lon, Z0, M2=c(H,g), S2=c(H,g), N2=c(H,g),
#                        K1=c(H,g), O1=c(H,g))
#   H = amplitude in metres; g = Greenwich phase lag in degrees
#   Z0 = mean tidal level above Chart Datum (approx. = cd_below_msl_m)
#
# ACCURACY NOTE: These values are approximate (\u00b10.05\u20130.15 m typical error
# for 5-constituent prediction). Adequate for depth-correction to \u00b10.3 m
# at most UK/NW European ports. For safety-critical or regulatory work,
# use official UKHO EasyTide predictions instead.
# =============================================================================

.harmonic_ports <- list(

  # ---- UK South / English Channel ----
  # Sources: UKHO Admiralty Tide Tables Vol 1; BODC harmonic database
  Dover = list(
    lat=51.12, lon=1.32, Z0=3.67,
    M2=c(2.21,117), S2=c(0.80,148), N2=c(0.44,93), K1=c(0.11,303), O1=c(0.08,271)
  ),
  Portland = list(
    lat=50.57, lon=-2.44, Z0=1.07,
    M2=c(0.52,281), S2=c(0.18,314), N2=c(0.10,258), K1=c(0.07,310), O1=c(0.05,270)
  ),
  Plymouth = list(
    lat=50.37, lon=-4.14, Z0=3.22,
    M2=c(1.53,313), S2=c(0.53,344), N2=c(0.31,291), K1=c(0.08,318), O1=c(0.06,278)
  ),
  Falmouth = list(
    lat=50.15, lon=-5.05, Z0=3.05,
    M2=c(1.46,310), S2=c(0.50,340), N2=c(0.29,288), K1=c(0.08,315), O1=c(0.06,275)
  ),

  # ---- Bristol Channel / SW Wales ----
  `Milford Haven` = list(
    lat=51.71, lon=-5.02, Z0=3.70,
    M2=c(2.36,339), S2=c(0.81,10), N2=c(0.47,317), K1=c(0.09,345), O1=c(0.07,305)
  ),
  Bristol = list(
    lat=51.45, lon=-2.72, Z0=6.50,
    M2=c(4.38,16), S2=c(1.50,49), N2=c(0.88,354), K1=c(0.11,20), O1=c(0.09,340)
  ),
  Cardiff = list(
    lat=51.46, lon=-3.16, Z0=6.50,
    M2=c(4.45,15), S2=c(1.54,48), N2=c(0.89,353), K1=c(0.11,19), O1=c(0.09,339)
  ),
  Swansea = list(
    lat=51.62, lon=-3.94, Z0=4.70,
    M2=c(3.19,358), S2=c(1.10,31), N2=c(0.64,336), K1=c(0.10,4), O1=c(0.08,324)
  ),
  Barmouth = list(
    lat=52.72, lon=-4.05, Z0=1.80,
    M2=c(1.26,328), S2=c(0.44,358), N2=c(0.25,306), K1=c(0.09,335), O1=c(0.07,295)
  ),

  # ---- NW England ----
  # Sources: UKHO ATT; NTSLF (National Tide and Sea Level Facility)
  Liverpool = list(
    lat=53.45, lon=-3.01, Z0=4.93,
    M2=c(3.13,344), S2=c(1.06,16), N2=c(0.63,322), K1=c(0.10,350), O1=c(0.08,310)
  ),
  Heysham = list(
    lat=54.03, lon=-2.92, Z0=5.00,
    M2=c(3.17,348), S2=c(1.07,21), N2=c(0.64,326), K1=c(0.10,353), O1=c(0.08,313)
  ),
  Fleetwood = list(
    lat=53.92, lon=-3.01, Z0=5.00,
    M2=c(3.15,349), S2=c(1.06,22), N2=c(0.63,327), K1=c(0.10,354), O1=c(0.08,314)
  ),

  # ---- Scotland West ----
  # Sources: UKHO ATT Vol 2; Marine Scotland harmonic data
  Greenock = list(
    lat=55.95, lon=-4.76, Z0=1.82,
    M2=c(1.30,21), S2=c(0.42,66), N2=c(0.26,359), K1=c(0.09,28), O1=c(0.07,348)
  ),
  Oban = list(
    lat=56.41, lon=-5.48, Z0=2.02,
    M2=c(1.44,11), S2=c(0.46,53), N2=c(0.29,350), K1=c(0.09,18), O1=c(0.07,338)
  ),
  Ullapool = list(
    lat=57.90, lon=-5.16, Z0=2.57,
    M2=c(2.01,356), S2=c(0.64,31), N2=c(0.40,334), K1=c(0.10,3), O1=c(0.08,323)
  ),
  Stornoway = list(
    lat=58.21, lon=-6.39, Z0=2.50,
    M2=c(1.88,344), S2=c(0.60,26), N2=c(0.38,322), K1=c(0.10,351), O1=c(0.08,311)
  ),

  # ---- Scotland East ----
  Leith = list(
    lat=55.99, lon=-3.18, Z0=3.00,
    M2=c(2.12,80), S2=c(0.69,113), N2=c(0.43,58), K1=c(0.11,85), O1=c(0.08,45)
  ),
  Aberdeen = list(
    lat=57.14, lon=-2.08, Z0=2.29,
    M2=c(1.58,42), S2=c(0.51,78), N2=c(0.32,20), K1=c(0.10,48), O1=c(0.07,8)
  ),
  Lerwick = list(
    lat=60.15, lon=-1.14, Z0=1.15,
    M2=c(0.57,70), S2=c(0.19,112), N2=c(0.11,48), K1=c(0.09,78), O1=c(0.06,38)
  ),

  # ---- Ireland ----
  # Sources: Marine Institute Ireland; UKHO ATT
  Belfast = list(
    lat=54.60, lon=-5.92, Z0=1.89,
    M2=c(1.31,344), S2=c(0.44,19), N2=c(0.26,322), K1=c(0.09,349), O1=c(0.07,309)
  ),
  Dublin = list(
    lat=53.35, lon=-6.22, Z0=2.07,
    M2=c(1.50,12), S2=c(0.50,47), N2=c(0.30,350), K1=c(0.09,18), O1=c(0.07,338)
  ),

  # ---- France ----
  # Sources: SHOM (Service Hydrographique et Oc\u00e9anographique de la Marine)
  Brest = list(
    lat=48.38, lon=-4.49, Z0=3.83,
    M2=c(2.37,165), S2=c(0.79,203), N2=c(0.47,143), K1=c(0.09,157), O1=c(0.07,117)
  ),
  Cherbourg = list(
    lat=49.65, lon=-1.63, Z0=3.57,
    M2=c(1.58,221), S2=c(0.55,258), N2=c(0.32,199), K1=c(0.08,215), O1=c(0.06,175)
  ),
  Calais = list(
    lat=50.97, lon=1.85, Z0=3.91,
    M2=c(2.80,139), S2=c(1.00,171), N2=c(0.56,116), K1=c(0.12,291), O1=c(0.09,256)
  ),
  Dunkirk = list(
    lat=51.04, lon=2.37, Z0=3.38,
    M2=c(2.43,134), S2=c(0.87,167), N2=c(0.49,111), K1=c(0.11,286), O1=c(0.08,251)
  ),

  # ---- Germany ----
  # Sources: BSH (Bundesamt f\u00fcr Seeschifffahrt und Hydrographie)
  Helgoland = list(
    lat=54.18, lon=7.89, Z0=1.50,
    M2=c(0.99,174), S2=c(0.35,210), N2=c(0.20,151), K1=c(0.08,172), O1=c(0.06,132)
  ),
  Cuxhaven = list(
    lat=53.87, lon=8.72, Z0=2.08,
    M2=c(1.34,186), S2=c(0.46,223), N2=c(0.27,163), K1=c(0.08,183), O1=c(0.06,143)
  ),

  # ---- Netherlands ----
  # Sources: RWS (Rijkswaterstaat) tidal data
  `Hook of Holland` = list(
    lat=51.98, lon=4.12, Z0=1.10,
    M2=c(0.74,165), S2=c(0.26,201), N2=c(0.15,143), K1=c(0.08,162), O1=c(0.06,122)
  ),
  Flushing = list(
    lat=51.45, lon=3.71, Z0=2.45,
    M2=c(1.67,149), S2=c(0.58,186), N2=c(0.33,127), K1=c(0.09,146), O1=c(0.07,106)
  ),

  # ---- Norway ----
  # Sources: Kartverket (Norwegian Mapping Authority) tidal data
  Bergen = list(
    lat=60.40, lon=5.32, Z0=0.90,
    M2=c(0.48,253), S2=c(0.13,304), N2=c(0.10,230), K1=c(0.07,252), O1=c(0.05,212)
  ),
  Oslo = list(
    lat=59.91, lon=10.73, Z0=0.20,
    M2=c(0.08,296), S2=c(0.02,341), N2=c(0.02,274), K1=c(0.05,296), O1=c(0.04,256)
  )
)


# Angular speeds in degrees/hour (Doodson standard constituents)
.sigma_deg_hr <- c(
  M2 = 28.9841042,
  S2 = 30.0000000,
  N2 = 28.4397295,
  K1 = 15.0410686,
  O1 = 13.9430356
)

# Equilibrium arguments at J1900.0 (1900-01-01 00:00 UTC).
# Computed from Schureman (1940) astronomical values at that epoch:
#   s0 = 277.0248\u00b0, h0 = 280.1895\u00b0, p0 = 334.3853\u00b0, N0 = 259.1568\u00b0
# V0(M2) = 2h0 - 2s0 = 6.33\u00b0
# V0(S2) = 0\u00b0 (purely solar, by convention)
# V0(N2) = 2h0 - 3s0 + p0 = 63.69\u00b0
# V0(K1) = h0 + 90\u00b0 \u2192 10.19\u00b0
# V0(O1) = s0 - 2h0 - 90\u00b0 \u2192 346.65\u00b0
.V0_J1900 <- c(
  M2 =   6.33,
  S2 =   0.00,
  N2 =  63.69,
  K1 =  10.19,
  O1 = 346.65
)

# Reference epoch as POSIXct (used by all harmonic prediction helpers)
.J1900 <- as.POSIXct("1900-01-01 00:00:00", tz = "UTC")


#' Haversine distance in km between two WGS84 points
#' @keywords internal
.haversine_km <- function(lat1, lon1, lat2, lon2) {
  R    <- 6371.0
  phi1 <- lat1 * pi / 180;  phi2 <- lat2 * pi / 180
  dphi <- (lat2 - lat1) * pi / 180
  dlam <- (lon2 - lon1) * pi / 180
  a    <- sin(dphi/2)^2 + cos(phi1)*cos(phi2)*sin(dlam/2)^2
  2 * R * atan2(sqrt(a), sqrt(1-a))
}


#' Compute Schureman nodal amplitude (f) and phase (u) corrections
#'
#' @description
#' Computes the 18.6-year lunar nodal modulation factors f (amplitude scale)
#' and u (phase correction in degrees) for M2, S2, N2, K1, O1 at a given epoch.
#'
#' Formulae from Schureman (1940) Tables 2 & 3; same as those used by
#' UKHO, NOAA, and most modern tidal prediction software.
#'
#' @param t_mean POSIXct. Representative timestamp for the survey (typically
#'   the midpoint of the survey period). Nodal factors vary over months, so
#'   a single midpoint evaluation is accurate to well within the constituent
#'   uncertainty budget.
#' @return Named list with elements `f` and `u`, each a named numeric vector
#'   indexed by constituent name (M2, S2, N2, K1, O1).
#' @keywords internal
.nodal_factors <- function(t_mean) {

  # Longitude of the ascending lunar node N (degrees)
  # Reference: N = 259.1560\u00b0 at J2000.0; rate = -0.0529539\u00b0/day
  J2000 <- as.POSIXct("2000-01-01 00:00:00", tz = "UTC")
  T_days <- as.numeric(difftime(t_mean, J2000, units = "days"))
  N_deg  <- (259.1560 - 0.0529539 * T_days) %% 360
  N_rad  <- N_deg * pi / 180

  # Schureman (1940) nodal factor formulas \u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500
  #
  #  f: amplitude scale factor (multiplies constituent amplitude H)
  #  u: phase correction in degrees (added inside cosine argument)
  #
  #  M2 / N2: f = 1 - 0.03731\u00b7cos(N)
  #            u = -2.14\u00b7sin(N)
  #
  #  S2:      f = 1,  u = 0   (solar; no lunar nodal modulation)
  #
  #  K1:      f = 1.0060 + 0.1150\u00b7cos(N) - 0.0088\u00b7cos(2N)
  #            u = -8.86\u00b7sin(N) + 0.68\u00b7sin(2N)
  #
  #  O1:      f = 1.0089 + 0.1871\u00b7cos(N) - 0.0147\u00b7cos(2N)
  #            u =  10.80\u00b7sin(N) - 1.34\u00b7sin(2N)

  f_M2 <- 1 - 0.03731 * cos(N_rad)
  u_M2 <- -2.14 * sin(N_rad)

  f_K1 <- 1.0060 + 0.1150 * cos(N_rad) - 0.0088 * cos(2 * N_rad)
  u_K1 <- -8.86  * sin(N_rad) + 0.68   * sin(2 * N_rad)

  f_O1 <- 1.0089 + 0.1871 * cos(N_rad) - 0.0147 * cos(2 * N_rad)
  u_O1 <-  10.80 * sin(N_rad) - 1.34   * sin(2 * N_rad)

  list(
    f = c(M2 = f_M2, S2 = 1.0,  N2 = f_M2, K1 = f_K1, O1 = f_O1),
    u = c(M2 = u_M2, S2 = 0.0,  N2 = u_M2, K1 = u_K1, O1 = u_O1)
  )
}


#' Predict tidal heights using embedded harmonic constituents (with nodal corrections)
#'
#' @param t_posix POSIXct vector of survey timestamps (UTC).
#' @param port Named list from .harmonic_ports.
#' @return Numeric vector of predicted heights above Chart Datum (metres).
#' @keywords internal
.predict_harmonic_heights <- function(t_posix, port) {

  # Hours from J1900.0 reference epoch
  t_hrs <- as.numeric(difftime(t_posix, .J1900, units = "hours"))

  # Nodal factors at survey midpoint
  t_mid <- t_posix[max(1L, ceiling(length(t_posix) / 2))]
  nf    <- .nodal_factors(t_mid)

  h <- rep(port$Z0, length(t_hrs))

  for (cn in names(.sigma_deg_hr)) {
    const <- port[[cn]]
    if (is.null(const) || length(const) < 2) next
    A  <- const[1]   # amplitude (m)
    g  <- const[2]   # Greenwich phase lag (degrees)
    f  <- nf$f[cn]
    u  <- nf$u[cn]
    # Full argument: sigma*t + V0(epoch) + u - g
    arg_deg <- (.sigma_deg_hr[cn] * t_hrs + .V0_J1900[cn] + u - g) %% 360
    h <- h + f * A * cos(arg_deg * pi / 180)
  }

  h
}


#' Correct survey depths to Chart Datum (LAT)
#'
#' @description
#' Converts water depths recorded during a survey to depths below Chart Datum
#' (approximately Lowest Astronomical Tide, LAT) by adding the tidal height
#' above Chart Datum at the time of survey. This removes bias introduced by
#' surveying at different states of the tide.
#'
#' **Formula:** `depth_CD = depth_surveyed + tidal_height_m`
#'
#' **Finding your tidal height:**
#' - UK: UKHO EasyTide (easytide.ukho.gov.uk) — free 7-day predictions for
#'   any standard port. Download the predicted heights for your survey period.
#' - Ireland: Marine Institute tide gauges (data.marine.ie)
#' - France/EU: SHOM (shom.fr) or Copernicus Marine (marine.copernicus.eu)
#' - Any port: apps such as PocketTides, Tides Near Me, or TidePod give
#'   height-above-CD predictions for named ports.
#'
#' For a typical loch or sheltered bay survey, reading the tidal height from a
#' tide table for the nearest standard port at the mid-point of the survey is
#' sufficient. For multi-day surveys or those spanning a full tidal cycle,
#' supply per-row heights matched to the survey timestamp.
#'
#' @param df A dataframe containing at minimum a depth column. Typically the
#'   output of [merge_sensor_data()] before passing to [predict_oyster()].
#' @param tidal_height_m Numeric scalar or vector. Tidal height above Chart
#'   Datum at the time of survey, in **metres**. If a scalar, the same offset
#'   is applied to all rows. If a vector, must be the same length as `nrow(df)`.
#'   Use `NULL` to skip correction (a warning will be issued).
#' @param depth_col Character. Name of the depth column to correct
#'   (default `"depth"`).
#' @param datetime_col Character or `NULL`. Name of a datetime column in `df`.
#'   If supplied, it is used only for informational messages — tidal heights
#'   must still be supplied by the user. Default `NULL`.
#' @param keep_raw Logical. If `TRUE`, retains the original depth values in a
#'   column named `depth_raw_m` alongside the corrected `depth` column
#'   (default `TRUE`).
#' @param verbose Logical. Print correction summary (default `TRUE`).
#'
#' @return The input dataframe with the depth column corrected to Chart Datum.
#'   If `keep_raw = TRUE`, the original values are preserved as `depth_raw_m`.
#'
#' @export
#' @examples
#' \dontrun{
#' # Survey conducted around high water at Oban — tidal height ~3.1 m above CD
#' survey_corrected <- correct_to_chart_datum(survey, tidal_height_m = 3.1)
#'
#' # Per-row heights from a tide gauge CSV matched to survey timestamps
#' tide_series <- read.csv("oban_tide_gauge.csv")
#' # (match to survey rows by timestamp — user responsibility)
#' survey_corrected <- correct_to_chart_datum(
#'   survey,
#'   tidal_height_m = tide_series$height_m
#' )
#' }
correct_to_chart_datum <- function(df,
                                   tidal_height_m = NULL,
                                   depth_col      = "depth",
                                   datetime_col   = NULL,
                                   keep_raw       = TRUE,
                                   verbose        = TRUE) {

  # ---- Validate inputs -------------------------------------------------------
  if (!is.data.frame(df)) {
    cli::cli_abort("{.arg df} must be a dataframe.")
  }

  if (!depth_col %in% names(df)) {
    cli::cli_abort(c(
      "Depth column {.val {depth_col}} not found in dataframe.",
      "i" = "Available columns: {.val {names(df)}}",
      "i" = "Set {.arg depth_col} to the correct column name."
    ))
  }

  # ---- No correction requested -----------------------------------------------
  if (is.null(tidal_height_m)) {
    cli::cli_warn(c(
      "!" = "No tidal height supplied \u2014 depth values are NOT corrected to Chart Datum.",
      "i" = "Depth scores may be biased if the survey was conducted away from low water.",
      "i" = "Supply {.arg tidal_height_m} (height above CD at time of survey) to correct.",
      "i" = "UK tidal heights: {.url https://easytide.ukho.gov.uk}"
    ))
    return(df)
  }

  # ---- Validate tidal_height_m -----------------------------------------------
  if (!is.numeric(tidal_height_m)) {
    cli::cli_abort("{.arg tidal_height_m} must be numeric (metres above Chart Datum).")
  }

  n_rows <- nrow(df)

  if (length(tidal_height_m) == 1) {
    tidal_vec <- rep(tidal_height_m, n_rows)
  } else if (length(tidal_height_m) == n_rows) {
    tidal_vec <- tidal_height_m
  } else {
    cli::cli_abort(c(
      "{.arg tidal_height_m} must be a scalar or have the same length as {.code nrow(df)} ({n_rows}).",
      "x" = "Got length {length(tidal_height_m)}."
    ))
  }

  # ---- Plausibility check on tidal heights -----------------------------------
  if (any(tidal_vec < -1, na.rm = TRUE) || any(tidal_vec > 12, na.rm = TRUE)) {
    cli::cli_warn(c(
      "!" = "Some tidal height values seem unusual.",
      "i" = "Range: {round(min(tidal_vec,na.rm=TRUE),2)} to {round(max(tidal_vec,na.rm=TRUE),2)} m.",
      "i" = "Normal range for most European ports is 0\u20138 m above Chart Datum.",
      "i" = "Check that values are in metres, not feet or tidal state codes."
    ))
  }

  # ---- Apply correction ------------------------------------------------------
  raw_depths <- df[[depth_col]]

  if (keep_raw) {
    df$depth_raw_m <- raw_depths
  }

  df[[depth_col]] <- raw_depths + tidal_vec

  # Clamp to physically meaningful range (>= 0)
  n_negative <- sum(df[[depth_col]] < 0, na.rm = TRUE)
  if (n_negative > 0) {
    cli::cli_warn(c(
      "!" = "{n_negative} corrected depth value{?s} < 0 m (above water surface).",
      "i" = "These are set to 0 m. They likely represent very shallow intertidal points.",
      "i" = "Check that your tidal height is correct for the survey area."
    ))
    df[[depth_col]] <- pmax(df[[depth_col]], 0)
  }

  # ---- Summary ---------------------------------------------------------------
  if (verbose) {
    scalar_flag <- length(tidal_height_m) == 1
    time_info   <- ""
    if (!is.null(datetime_col) && datetime_col %in% names(df)) {
      rng  <- range(df[[datetime_col]], na.rm = TRUE)
      time_info <- " | Survey: {rng[1]} to {rng[2]}"
    }
    cli::cli_inform(c(
      "v" = "Tidal correction applied to {.val {depth_col}}.",
      "i" = "  Offset: {if(scalar_flag) paste(tidal_height_m,'m (constant)') else paste(round(mean(tidal_vec,na.rm=TRUE),2),'m mean (per-row)')}",
      "i" = "  Depth before: {round(mean(raw_depths,na.rm=TRUE),1)} m mean  ({round(min(raw_depths,na.rm=TRUE),1)}\u2013{round(max(raw_depths,na.rm=TRUE),1)} m range)",
      "i" = "  Depth after:  {round(mean(df[[depth_col]],na.rm=TRUE),1)} m mean  ({round(min(df[[depth_col]],na.rm=TRUE),1)}\u2013{round(max(df[[depth_col]],na.rm=TRUE),1)} m range)",
      "i" = "  Raw values kept in {.val depth_raw_m} column."
    ))
  }

  df
}


#' Automatically predict and apply tidal correction from harmonic constituents
#'
#' @description
#' Finds the nearest standard port to the survey area, checks whether it is
#' within a configurable distance threshold, predicts tidal heights for every
#' survey timestamp using embedded 5-constituent harmonic models
#' (M2, S2, N2, K1, O1), and applies [correct_to_chart_datum()] automatically.
#'
#' No external data or internet connection is required. Harmonic constituent
#' data (amplitude and Greenwich phase lag) for 31 UK and European ports are
#' embedded in the package.
#'
#' **Accuracy:** 5-constituent predictions are typically accurate to ±0.15–0.35 m
#' at the standard port. Accuracy degrades with distance from the port and in
#' areas with complex tidal dynamics (e.g. the Bristol Channel, inner fjords).
#' For safety-critical work, use official UKHO EasyTide predictions and supply
#' them manually via [correct_to_chart_datum()].
#'
#' **Requirements:** The dataframe must contain a datetime column. Values must
#' be parseable by [as.POSIXct()] (e.g. `"2024-06-15 10:30:00"` or
#' `"2024-06-15"`). Date-only strings default to noon UTC.
#'
#' @param df A dataframe with at minimum a depth and a datetime column.
#'   Typically the output of [merge_sensor_data()].
#' @param datetime_col Character. Name of the datetime column (default
#'   `"date"`). Values should be UTC or treated as UTC.
#' @param depth_col Character. Name of the depth column to correct
#'   (default `"depth"`).
#' @param max_port_dist_km Numeric. Maximum distance in km from the survey
#'   centroid to the nearest standard port. If the nearest port is further
#'   than this, no correction is applied and a warning is issued
#'   (default `75` km).
#' @param keep_raw Logical. Retain original depths in a `depth_raw_m` column
#'   (default `TRUE`).
#' @param verbose Logical. Print port selection, distance, and prediction
#'   summary (default `TRUE`).
#'
#' @return The input dataframe with depths corrected to Chart Datum, plus
#'   a `tidal_height_pred_m` column containing the predicted tidal height
#'   used for each row.
#'
#' @seealso [correct_to_chart_datum()] for manual correction,
#'   [tidal_port_info()] for port datum reference data.
#'
#' @export
#' @examples
#' \dontrun{
#' # Automatic correction — finds nearest port, predicts heights, corrects depths
#' survey_corrected <- auto_tidal_correct(survey, datetime_col = "date")
#'
#' # Tighten the distance threshold (only trust ports within 40 km)
#' survey_corrected <- auto_tidal_correct(survey, max_port_dist_km = 40)
#'
#' # See which port was selected and inspect predicted heights
#' survey_corrected$tidal_height_pred_m
#' }
auto_tidal_correct <- function(df,
                               datetime_col     = "date",
                               depth_col        = "depth",
                               max_port_dist_km = 75,
                               keep_raw         = TRUE,
                               verbose          = TRUE) {

  # ---- Validate inputs --------------------------------------------------------
  if (!is.data.frame(df)) cli::cli_abort("{.arg df} must be a dataframe.")

  if (!depth_col %in% names(df)) {
    cli::cli_abort(c(
      "Depth column {.val {depth_col}} not found.",
      "i" = "Available columns: {.val {names(df)}}"
    ))
  }

  if (!datetime_col %in% names(df)) {
    cli::cli_abort(c(
      "Datetime column {.val {datetime_col}} not found.",
      "i" = "Available columns: {.val {names(df)}}",
      "i" = "Set {.arg datetime_col} to the correct column name, or use",
      "i" = "{.fn correct_to_chart_datum} to supply heights manually."
    ))
  }

  # ---- Parse timestamps -------------------------------------------------------
  raw_times <- df[[datetime_col]]

  t_posix <- tryCatch(
    suppressWarnings(as.POSIXct(raw_times, tz = "UTC")),
    error = function(e) NULL
  )

  # Date-only strings (YYYY-MM-DD) \u2192 noon UTC
  if (any(is.na(t_posix) & !is.na(raw_times))) {
    as_date <- suppressWarnings(as.Date(raw_times[is.na(t_posix) & !is.na(raw_times)]))
    t_posix[is.na(t_posix) & !is.na(raw_times)] <-
      as.POSIXct(paste(as_date, "12:00:00"), tz = "UTC")
    if (verbose) cli::cli_warn(c(
      "!" = "Some timestamps are date-only; defaulting to 12:00 UTC.",
      "i" = "For higher accuracy supply full datetime values (YYYY-MM-DD HH:MM:SS)."
    ))
  }

  n_na_t <- sum(is.na(t_posix))
  if (n_na_t == nrow(df)) {
    cli::cli_abort(c(
      "Could not parse any values in {.val {datetime_col}} as datetimes.",
      "i" = "Example of expected format: {.val '2024-06-15 10:30:00'}"
    ))
  }
  if (n_na_t > 0 && verbose) {
    cli::cli_warn("{n_na_t} row{?s} have NA timestamps; tidal height will be NA for these.")
  }

  # ---- Find survey centroid ---------------------------------------------------
  lat_centre <- mean(df$lat, na.rm = TRUE)
  lon_centre <- mean(df$lon, na.rm = TRUE)

  if (is.na(lat_centre) || is.na(lon_centre)) {
    cli::cli_abort(c(
      "Cannot compute survey centroid: {.val lat} / {.val lon} columns are all NA.",
      "i" = "Ensure {.fn merge_sensor_data} has been run before calling {.fn auto_tidal_correct}."
    ))
  }

  # ---- Find nearest harmonic port ---------------------------------------------
  port_names <- names(.harmonic_ports)
  dists_km   <- vapply(port_names, function(pn) {
    p <- .harmonic_ports[[pn]]
    .haversine_km(lat_centre, lon_centre, p$lat, p$lon)
  }, numeric(1))

  nearest_idx  <- which.min(dists_km)
  nearest_name <- port_names[nearest_idx]
  nearest_dist <- round(dists_km[nearest_idx], 1)
  port         <- .harmonic_ports[[nearest_name]]

  if (verbose) {
    cli::cli_inform(c(
      "i" = "Survey centroid: {round(lat_centre,4)}\u00b0N, {round(lon_centre,4)}\u00b0E",
      "i" = "Nearest harmonic port: {.strong {nearest_name}} ({nearest_dist} km)"
    ))
  }

  # ---- Distance threshold check -----------------------------------------------
  if (nearest_dist > max_port_dist_km) {
    cli::cli_warn(c(
      "!" = "Nearest port ({nearest_name}) is {nearest_dist} km away",
      "   " = "(threshold: {max_port_dist_km} km). No automatic correction applied.",
      "i" = "Increase {.arg max_port_dist_km} to use this port anyway, or supply",
      "i" = "heights manually via {.fn correct_to_chart_datum}.",
      "i" = "See also {.fn tidal_port_info} for available ports."
    ))
    return(df)
  }

  # ---- Warn if port is quite distant ------------------------------------------
  if (nearest_dist > 40 && verbose) {
    cli::cli_warn(c(
      "!" = "Nearest port ({nearest_name}) is {nearest_dist} km from survey area.",
      "i" = "Prediction accuracy may be reduced in areas with complex tidal dynamics.",
      "i" = "Consider verifying against {.url https://easytide.ukho.gov.uk}."
    ))
  }

  # ---- Predict tidal heights --------------------------------------------------
  tidal_heights <- .predict_harmonic_heights(t_posix, port)

  t_range   <- range(t_posix, na.rm = TRUE)
  h_range   <- range(tidal_heights, na.rm = TRUE)
  h_mean    <- round(mean(tidal_heights, na.rm = TRUE), 2)
  spring_rng <- port$M2[1] * 2 + port$S2[1] * 2  # approximate spring range

  if (verbose) {
    cli::cli_inform(c(
      "i" = "Survey period: {format(t_range[1],'%Y-%m-%d %H:%M')} to {format(t_range[2],'%Y-%m-%d %H:%M')} UTC",
      "i" = "Predicted tidal height: {h_range[1]} \u2013 {h_range[2]} m above CD  (mean {h_mean} m)",
      "i" = "Port spring range: ~{round(spring_rng,1)} m  |  constituents: M2, S2, N2, K1, O1"
    ))
  }

  # Attach predicted heights to dataframe for inspection
  df$tidal_height_pred_m <- round(tidal_heights, 3)

  # ---- Apply correction -------------------------------------------------------
  df <- correct_to_chart_datum(
    df             = df,
    tidal_height_m = tidal_heights,
    depth_col      = depth_col,
    datetime_col   = datetime_col,
    keep_raw       = keep_raw,
    verbose        = verbose
  )

  if (verbose) {
    cli::cli_inform(c(
      "v" = "Harmonic tidal correction applied using {nearest_name} constituents."
    ))
  }

  df
}


#' Look up approximate tidal range for major European survey ports
#'
#' @description
#' Returns the mean spring tidal range (MHWS - MLWS) and typical Chart Datum
#' offset from Mean Sea Level for a named port. Useful for a quick sanity check
#' on tidal height values before applying [correct_to_chart_datum()].
#'
#' This is a reference table only — always use official tide table predictions
#' for actual corrections.
#'
#' @param port Character. Port name (partial, case-insensitive match).
#'
#' @return A one-row dataframe with columns `port`, `country`,
#'   `mhws_m`, `mlws_m`, `spring_range_m`, `cd_below_msl_m`.
#' @export
#' @examples
#' tidal_port_info("oban")
#' tidal_port_info("brest")
tidal_port_info <- function(port) {

  # UKHO / SHOM official reference data (MHWS, MLWS above Chart Datum)
  # Chart Datum below MSL sourced from UKHO port datasheets and SHOM records.
  ports <- data.frame(
    port           = c("Dover", "Portland", "Plymouth", "Falmouth",
                       "Milford Haven", "Bristol", "Cardiff", "Swansea",
                       "Barmouth", "Liverpool", "Heysham", "Fleetwood",
                       "Greenock", "Oban", "Ullapool", "Stornoway",
                       "Leith", "Aberdeen", "Lerwick",
                       "Belfast", "Dublin",
                       "Brest", "Cherbourg", "Calais", "Dunkirk",
                       "Helgoland", "Cuxhaven",
                       "Hook of Holland", "Flushing",
                       "Bergen", "Oslo"),
    country        = c("UK","UK","UK","UK","UK","UK","UK","UK","UK","UK",
                       "UK","UK","UK","UK","UK","UK","UK","UK","UK",
                       "Ireland","Ireland",
                       "France","France","France","France",
                       "Germany","Germany",
                       "Netherlands","Netherlands",
                       "Norway","Norway"),
    mhws_m         = c(6.8,2.2,5.5,5.3,7.0,13.2,12.3,8.5,3.6,9.3,
                       9.5,9.5,3.4,4.0,5.2,4.8,5.6,4.3,2.2,
                       3.5,4.1,
                       7.5,6.4,7.2,6.0,
                       2.8,4.0,
                       2.1,4.8,
                       1.8,0.5),
    mlws_m         = c(0.8,0.4,0.5,0.5,0.7,1.0,1.0,0.7,0.4,0.9,
                       0.9,0.9,0.5,0.6,0.7,0.7,0.9,0.7,0.4,
                       0.4,0.5,
                       0.9,0.7,0.8,0.7,
                       0.4,0.5,
                       0.4,0.6,
                       0.4,0.1),
    cd_below_msl_m = c(3.67,1.07,3.22,3.05,3.70,6.50,6.50,4.70,1.80,4.93,
                       5.00,5.00,1.82,2.02,2.57,2.50,3.00,2.29,1.15,
                       1.89,2.07,
                       3.83,3.57,3.91,3.38,
                       1.50,2.08,
                       1.10,2.45,
                       0.90,0.20),
    stringsAsFactors = FALSE
  )
  ports$spring_range_m <- round(ports$mhws_m - ports$mlws_m, 1)

  match_idx <- grep(tolower(port), tolower(ports$port), fixed = TRUE)
  if (length(match_idx) == 0) {
    cli::cli_abort(c(
      "Port {.val {port}} not found in reference table.",
      "i" = "Available ports: {.val {ports$port}}",
      "i" = "For unlisted ports use the UKHO EasyTide: {.url https://easytide.ukho.gov.uk}"
    ))
  }
  result <- ports[match_idx[1], ]
  if (length(match_idx) > 1) {
    cli::cli_warn("Multiple matches for {.val {port}}; returning first: {result$port}.")
  }

  cli::cli_inform(c(
    "i" = "{result$port}, {result$country}:",
    "i" = "  MHWS: +{result$mhws_m} m above CD | MLWS: +{result$mlws_m} m above CD",
    "i" = "  Spring range: {result$spring_range_m} m",
    "i" = "  Chart Datum is {result$cd_below_msl_m} m below Mean Sea Level",
    "i" = "  For precise predictions: {.url https://easytide.ukho.gov.uk}"
  ))

  invisible(result)
}
