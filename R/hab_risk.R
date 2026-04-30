# =============================================================================
# Harmful Algal Bloom (HAB) risk scoring
# =============================================================================
#
# HABs cause shellfish toxin accumulation (PSP, ASP, DSP, AZP) and direct
# mortality at extreme densities. The primary genera affecting European oysters:
#
#  Alexandrium spp.    \u2014 PSP (paralytic shellfish poisoning); common UK/Ireland
#  Pseudo-nitzschia    \u2014 ASP (amnesic shellfish poisoning / domoic acid)
#  Dinophysis spp.     \u2014 DSP (diarrhetic shellfish poisoning); OA toxins
#  Azadinium/Azaspiracid \u2014 AZP; Irish/Scottish waters
#  Karenia mikimotoides \u2014 direct mortality; bleaching events
#
# Risk drivers:
#  1. Historical HAB closure frequency from ICES HAB / CEFAS biotoxin records
#  2. Dissolved inorganic nitrogen (eutrophication driver)
#  3. Summer thermal stratification index (warm surface layer traps cells)
#
# For restoration: HAB risk primarily indicates monitoring requirements.
# For aquaculture: HAB closure frequency directly determines operational viability.
#
# References:
#  Gallacher et al. (2019): Predicting HABs at Scottish shellfish farms. ICES.
#  ICES WGHABD (2022): Working Group on Harmful Algal Bloom Dynamics. CM 2022.
#  Trainer et al. (2020): Parallels and contrasts in the seasonality of HABs. Harmful Algae.

#' Score harmful algal bloom risk at survey locations
#'
#' @description
#' Produces a HAB risk index (0 = negligible, 1 = severe) based on historical
#' bloom event frequency and, where available, nutrient and stratification data.
#'
#' **Input options (in priority order):**
#' 1. `hab_data` — manually supplied dataframe of historical HAB events
#'    (see Details).
#' 2. `fetch_live = TRUE` — queries the ICES HAB database for event records
#'    within the survey extent and a specified date range. Requires internet
#'    access and the `httr` package.
#' 3. Neither — returns a fixed background-level risk (0.1) with a warning.
#'
#' @section hab_data format:
#' A dataframe with columns:
#' - `lat`, `lon` — event coordinates (decimal degrees)
#' - `date` — event date (Date or character "YYYY-MM-DD")
#' - `genus` (optional) — bloom genus (e.g. "Alexandrium", "Pseudo-nitzschia")
#' - `toxin` (optional) — toxin class (PSP, ASP, DSP, AZP) — affects severity weight
#' - `closure_days` (optional) — number of days the area was closed; default 1
#'   per event if absent
#'
#' @param result Dataframe from [predict_oyster()]. Must contain `lat`, `lon`.
#'   Optional: `din_ug_l` (dissolved inorganic nitrogen µg/L) and
#'   `temp_surface_c` / `temp_bottom_c` for stratification index.
#' @param hab_data Dataframe of historical HAB events (see Details), or NULL.
#' @param fetch_live Logical. Query ICES HAB database (default FALSE).
#' @param date_range Character vector length 2 `c("YYYY-MM-DD","YYYY-MM-DD")`.
#'   Date range for live fetch or for filtering manually supplied `hab_data`.
#'   Default: last 10 years.
#' @param match_radius_m Numeric. Radius in metres for event aggregation
#'   (default 5000 m = 5 km, consistent with ICES monitoring station spacing).
#' @param species Character. Target species — affects toxin severity weights
#'   (`"ostrea_edulis"` or `"magallana_gigas"`).
#' @param verbose Logical. Default TRUE.
#'
#' @return `result` with additional columns:
#'   - `hab_risk` \[0,1\]: composite risk index
#'   - `hab_risk_class`: "Low" / "Moderate" / "High" / "Critical"
#'   - `hab_closure_days_per_year`: estimated days/year with shellfish closures
#'   - `hab_dominant_toxin`: most frequently recorded toxin class at the site
#'   - `hab_risk_note`: management flag
#'
#' @export
#' @examples
#' \dontrun{
#' # Manual data
#' hab <- data.frame(lat=52.1, lon=-4.5, date="2022-07-15",
#'                   genus="Alexandrium", toxin="PSP", closure_days=14)
#' result <- score_hab_risk(result, hab_data=hab, species="ostrea_edulis")
#'
#' # Live ICES fetch
#' result <- score_hab_risk(result, fetch_live=TRUE,
#'                          date_range=c("2015-01-01","2024-12-31"))
#' }
score_hab_risk <- function(result,
                            hab_data      = NULL,
                            fetch_live    = FALSE,
                            date_range    = NULL,
                            match_radius_m = 5000,
                            species       = "ostrea_edulis",
                            verbose       = TRUE) {

  if (!all(c("lat","lon") %in% names(result)))
    cli::cli_abort("result must contain 'lat' and 'lon' columns.")

  if (is.null(date_range)) {
    end_date   <- Sys.Date()
    start_date <- end_date - 365 * 10
    date_range <- as.character(c(start_date, end_date))
  }

  n_years <- as.numeric(difftime(as.Date(date_range[2]),
                                  as.Date(date_range[1]),
                                  units = "days")) / 365.25

  # \u2500\u2500 Live fetch \u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500
  if (fetch_live && is.null(hab_data)) {
    if (verbose) cli::cli_inform("Fetching ICES HAB event records...")
    bbox <- c(
      lon_min = min(result$lon, na.rm=TRUE), lon_max = max(result$lon, na.rm=TRUE),
      lat_min = min(result$lat, na.rm=TRUE), lat_max = max(result$lat, na.rm=TRUE)
    )
    hab_data <- tryCatch(
      .fetch_ices_hab(bbox, date_range, verbose),
      error = function(e) {
        cli::cli_warn("ICES HAB fetch failed: {conditionMessage(e)}. Using background risk.")
        NULL
      }
    )
    if (!is.null(hab_data)) {
      # Standardise ICES HAB field names
      if (!"lat" %in% names(hab_data) && "latitude" %in% names(hab_data))
        hab_data$lat <- hab_data$latitude
      if (!"lon" %in% names(hab_data) && "longitude" %in% names(hab_data))
        hab_data$lon <- hab_data$longitude
      if (!"closure_days" %in% names(hab_data))
        hab_data$closure_days <- 1
      if (!"toxin" %in% names(hab_data))
        hab_data$toxin <- "unknown"
    }
  }

  # \u2500\u2500 Toxin severity weights per species \u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500
  # O. edulis filters more slowly and may accumulate PSP less rapidly than C. gigas,
  # but is similarly affected by DSP. ASP has similar impacts on both.
  toxin_severity <- c(
    PSP     = 1.0,  # Paralytic \u2014 complete closure, no depuration threshold
    ASP     = 0.9,  # Amnesic \u2014 domoic acid; oysters can be cleared but slow
    DSP     = 0.8,  # Diarrhetic \u2014 OA toxins; regular UK closures
    AZP     = 0.9,  # Azaspiracid \u2014 Irish/Scottish waters; persistent
    unknown = 0.6
  )

  lat_mid    <- mean(result$lat, na.rm=TRUE)
  m_per_lat  <- 111320
  m_per_lon  <- 111320 * cos(lat_mid * pi / 180)
  rad_lat    <- match_radius_m / m_per_lat
  rad_lon    <- match_radius_m / m_per_lon

  result$hab_closure_days_per_year <- 0.0
  result$hab_dominant_toxin        <- NA_character_
  result$hab_raw_score             <- 0.0

  # \u2500\u2500 Score from HAB event data \u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500
  if (!is.null(hab_data) &&
      all(c("lat","lon") %in% names(hab_data)) &&
      nrow(hab_data) > 0) {

    if (!"closure_days" %in% names(hab_data)) hab_data$closure_days <- 1
    if (!"toxin" %in% names(hab_data))        hab_data$toxin <- "unknown"

    for (i in seq_len(nrow(result))) {
      dlat <- abs(hab_data$lat - result$lat[i])
      dlon <- abs(hab_data$lon - result$lon[i])
      in_r <- dlat <= rad_lat & dlon <= rad_lon

      if (!any(in_r)) next

      nearby <- hab_data[in_r, , drop=FALSE]

      total_closure_days <- sum(as.numeric(nearby$closure_days), na.rm=TRUE)
      result$hab_closure_days_per_year[i] <- total_closure_days / max(n_years, 1)

      toxin_tab <- table(nearby$toxin)
      result$hab_dominant_toxin[i] <- names(which.max(toxin_tab))

      # Weighted score: closure days/year normalised, then toxin-weighted
      txn <- result$hab_dominant_toxin[i]
      txn_wt <- if (!is.na(txn) && txn %in% names(toxin_severity)) {
                  toxin_severity[txn]
                } else {
                  toxin_severity["unknown"]
                }

      # 30 closure days/year \u2192 raw score 1.0 (about 4 weeks \u2014 common threshold
      # for declaring a site operationally unviable for aquaculture)
      result$hab_raw_score[i] <- pmin(
        (result$hab_closure_days_per_year[i] / 30) * txn_wt,
        1.0
      )
    }

    if (verbose)
      cli::cli_inform(paste0(
        "HAB: ", sum(result$hab_closure_days_per_year > 0),
        " cell(s) with recorded events; max ",
        round(max(result$hab_closure_days_per_year), 1),
        " closure days/yr."
      ))
  } else {
    if (verbose)
      cli::cli_warn(c(
        "No HAB event data supplied \u2014 applying background risk level (0.10).",
        "i" = "Supply hab_data or set fetch_live = TRUE for site-specific estimates."
      ))
    result$hab_raw_score <- 0.10
  }

  # \u2500\u2500 Nutrient loading proxy (eutrophication driver) \u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500
  nutrient_risk <- rep(0, nrow(result))
  if ("din_ug_l" %in% names(result)) {
    din <- result$din_ug_l
    # DIN > 50 \u00b5g/L = elevated eutrophication risk; > 200 = high
    nutrient_risk <- pmin(ifelse(din < 50, 0,
                          ifelse(din < 100, (din - 50) / 100,
                          ifelse(din < 200, 0.5 + (din - 100) / 200,
                                 1.0))), 1.0)
    nutrient_risk[is.na(nutrient_risk)] <- 0
  }

  # \u2500\u2500 Stratification index \u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500
  strat_risk <- rep(0, nrow(result))
  if ("temp_surface_c" %in% names(result) && "temp_bottom_c" %in% names(result)) {
    strat <- result$temp_surface_c - result$temp_bottom_c
    strat[is.na(strat)] <- 0
    # > 4\u00b0C stratification = surface bloom conditions
    strat_risk <- pmin(pmax(strat - 2, 0) / 6, 1.0)
  }

  # \u2500\u2500 Composite HAB risk \u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500
  w_hist <- if (any(result$hab_raw_score > 0.1)) 0.65 else 0.40
  w_nutr <- if (any(nutrient_risk > 0)) 0.20 else 0
  w_str  <- if (any(strat_risk  > 0)) 0.15 else 0
  w_sum  <- w_hist + w_nutr + w_str
  if (w_sum == 0) w_sum <- 1

  result$hab_risk <- pmin(
    (w_hist * result$hab_raw_score +
     w_nutr * nutrient_risk +
     w_str  * strat_risk) / w_sum,
    1.0
  )

  result$hab_raw_score <- NULL

  result$hab_risk_class <- dplyr::case_when(
    result$hab_risk < 0.20 ~ "Low",
    result$hab_risk < 0.45 ~ "Moderate",
    result$hab_risk < 0.70 ~ "High",
    TRUE                   ~ "Critical"
  )

  result$hab_risk_note <- dplyr::case_when(
    result$hab_risk_class == "Low"      ~ "No significant HAB history; standard monitoring.",
    result$hab_risk_class == "Moderate" ~ paste0(
      round(result$hab_closure_days_per_year, 0),
      " estimated closure days/yr. Enhanced biotoxin monitoring recommended."),
    result$hab_risk_class == "High"     ~ paste0(
      round(result$hab_closure_days_per_year, 0),
      " estimated closure days/yr. Site may be operationally marginal for aquaculture."),
    result$hab_risk_class == "Critical" ~ paste0(
      round(result$hab_closure_days_per_year, 0),
      " estimated closure days/yr. Aquaculture viability severely compromised; confirm with regulatory authority."),
    TRUE ~ NA_character_
  )

  result
}
