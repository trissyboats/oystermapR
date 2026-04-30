# =============================================================================
# Disease and parasite risk layer
# =============================================================================
#
# References:
#  Culloty & Mulcahy (2007): Bonamia ostreae in O. edulis \u2014 temperature dependence
#  ICES WGOS (2022): European flat oyster disease risk assessment framework
#  Garcia et al. (2011): OsHV-1 temperature thresholds for M. gigas mortality events
#  Burge et al. (2014): Climate change and marine disease
#  EFSA (2015): Bonamia ostreae and B. exitiosa \u2014 Scientific Opinion

# \u2500\u2500 Internal risk parameter tables \u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500

.bonamia_params <- list(
  # Temperature drives Bonamia proliferation rate inside haemocytes
  # Risk increases above 10\u00b0C; peak transmission 15-20\u00b0C; low below 8\u00b0C
  temp_risk = list(
    cold_safe   = 8.0,    # below this: near-zero proliferation
    onset       = 10.0,   # transmission begins
    peak_low    = 15.0,   # peak transmission window start
    peak_high   = 20.0,   # peak transmission window end
    high_thresh = 22.0    # sustained high temps reduce parasite viability slightly
  ),
  # Salinity \u2014 Bonamia tolerates typical estuarine to full marine range
  # Lower salinity (<28 PSU) reduces transmission slightly
  sal_risk = list(
    low_safe  = 20.0,
    onset     = 25.0,
    full_risk = 30.0
  ),
  # Known infected site proximity penalty (qualitative, user-supplied)
  # Applied as a multiplier to the composite risk score
  proximity_multiplier = list(
    within_1km  = 2.0,
    within_5km  = 1.5,
    within_20km = 1.2,
    beyond_20km = 1.0
  )
)

.oshv1_params <- list(
  # OsHV-1 microvariant (\u00b5Var) \u2014 causes Pacific oyster mortality syndrome (POMS)
  # Mortality events consistently occur when seawater >16\u00b0C for several days
  # Below 12\u00b0C: viral replication negligible
  temp_risk = list(
    cold_safe   = 12.0,
    onset       = 14.0,
    peak_low    = 16.0,
    peak_high   = 24.0,
    high_thresh = 28.0
  ),
  # OsHV-1 risk also scales with turbidity/organic matter (stress amplifier)
  turbidity_amplifier = list(
    low_ntu  = 5.0,   # below: base risk only
    high_ntu = 30.0   # above: 1.3\u00d7 multiplier (stress weakens immune response)
  )
)


#' Score locations for disease and parasite risk
#'
#' @description
#' Assesses the environmental risk of two key bivalve pathogens at each survey
#' location:
#'
#' - **Bonamia ostreae** (*Ostrea edulis* only) — an intracellular haplosporidian
#'   parasite that is the primary constraint on flat oyster restoration in northern
#'   Europe. Risk is temperature-driven: transmission peaks 15-20°C and is
#'   negligible below 8°C. The function also accepts a `known_sites` dataframe of
#'   confirmed infected locations to apply a proximity multiplier.
#'
#' - **OsHV-1 microvariant** (*Magallana gigas*) — a herpesvirus causing Pacific
#'   Oyster Mortality Syndrome (POMS). Mortality events occur when seawater
#'   exceeds 16°C for sustained periods. High turbidity amplifies risk by
#'   increasing larval stress.
#'
#' Risk scores are returned on a 0-1 scale (0 = negligible, 1 = highest
#' environmental risk) and classified as Low / Moderate / High / Critical.
#' They are deliberately kept separate from the suitability score — a High
#' suitability site with High disease risk is a meaningful regulatory red flag.
#'
#' @param result Dataframe from [predict_oyster()] with `lat`, `lon`, and
#'   environmental variable columns.
#' @param species Character. `"ostrea_edulis"` (Bonamia risk) or
#'   `"magallana_gigas"` (OsHV-1 risk).
#' @param known_sites Dataframe or NULL. Known infected/positive sites with
#'   `lat` and `lon` columns. When supplied, a proximity multiplier is applied.
#'   For *O. edulis* these are Bonamia-positive sites; for *M. gigas* OsHV-1
#'   outbreak sites.
#' @param temp_col Character. Name of temperature column in `result`
#'   (default `"temperature"`).
#' @param salinity_col Character. Name of salinity column (default `"salinity"`).
#'   Used for Bonamia only.
#' @param turbidity_col Character. Name of turbidity column (default
#'   `"turbidity"`). Used for OsHV-1 amplification only.
#' @param verbose Logical. Print risk summary (default TRUE).
#'
#' @return Input dataframe with additional columns:
#'   `disease_risk_score` \[0-1\], `disease_risk_class` (Low/Moderate/High/Critical),
#'   `disease_agent` (pathogen name), and `disease_risk_note` (plain-language
#'   interpretation).
#'
#' @export
#' @examples
#' \dontrun{
#' result <- predict_oyster(survey, "ostrea_edulis")
#'
#' # Basic risk scoring (temperature-driven only)
#' result <- score_disease_risk(result, "ostrea_edulis")
#'
#' # With known Bonamia-positive site locations
#' infected <- data.frame(lat = c(55.8, 56.1), lon = c(-5.2, -5.4))
#' result <- score_disease_risk(result, "ostrea_edulis",
#'                               known_sites = infected)
#'
#' # Sites with high ecological suitability but also high disease risk
#' flagged <- subset(result,
#'   suitability_class == "High" & disease_risk_class %in% c("High","Critical"))
#' }
score_disease_risk <- function(result,
                                species       = "ostrea_edulis",
                                known_sites   = NULL,
                                temp_col      = "temperature",
                                salinity_col  = "salinity",
                                turbidity_col = "turbidity",
                                verbose       = TRUE) {

  species <- tolower(gsub("crassostrea_gigas", "magallana_gigas", species))

  if (!species %in% c("ostrea_edulis", "magallana_gigas"))
    cli::cli_abort(c(
      "Disease risk scoring is implemented for 'ostrea_edulis' and 'magallana_gigas'.",
      "i" = "Got: {.val {species}}"
    ))

  if (!temp_col %in% names(result))
    cli::cli_abort("Temperature column {.val {temp_col}} not found in result.")

  temp <- result[[temp_col]]
  n    <- nrow(result)

  if (species == "ostrea_edulis") {
    risk <- .bonamia_risk(result, temp, temp_col, salinity_col, known_sites)
    agent <- "Bonamia ostreae"
  } else {
    risk <- .oshv1_risk(result, temp, turbidity_col, known_sites)
    agent <- "OsHV-1 microvariant"
  }

  risk_score <- pmin(1, pmax(0, risk))

  risk_class <- dplyr::case_when(
    is.na(risk_score)    ~ NA_character_,
    risk_score >= 0.75   ~ "Critical",
    risk_score >= 0.50   ~ "High",
    risk_score >= 0.25   ~ "Moderate",
    TRUE                 ~ "Low"
  )

  risk_note <- dplyr::case_when(
    risk_class == "Critical" ~ paste0(
      "Critical ", agent, " risk. Environmental conditions are highly favourable ",
      "for disease transmission. Restocking strongly discouraged without prior ",
      "pathogen screening and regulatory clearance."),
    risk_class == "High" ~ paste0(
      "High ", agent, " risk. Conditions support disease transmission. ",
      "Consider seasonal timing (avoid peak summer) and pre-stocking health checks."),
    risk_class == "Moderate" ~ paste0(
      "Moderate ", agent, " risk. Conditions periodically suitable for transmission. ",
      "Monitor during warm periods; health surveillance recommended."),
    TRUE ~ paste0(
      "Low ", agent, " risk. Current temperature regime limits disease transmission.")
  )

  result$disease_risk_score <- round(risk_score, 4)
  result$disease_risk_class <- risk_class
  result$disease_agent      <- agent
  result$disease_risk_note  <- risk_note

  if (verbose) {
    tbl <- table(risk_class)
    cli::cli_h2("Disease Risk \u2014 {agent}")
    cli::cli_inform(c(
      " " = paste0("Critical: ", tbl["Critical"] %||% 0,
                   " | High: ",     tbl["High"]     %||% 0,
                   " | Moderate: ", tbl["Moderate"] %||% 0,
                   " | Low: ",      tbl["Low"]      %||% 0),
      "i" = paste0("Mean risk score: ",
                   round(mean(risk_score, na.rm = TRUE), 3))
    ))

    n_flagged <- sum(
      !is.na(result$suitability_class) &
      result$suitability_class %in% c("High","Moderate") &
      risk_class %in% c("High","Critical"),
      na.rm = TRUE
    )
    if (n_flagged > 0)
      cli::cli_warn(c(
        "!" = paste0(n_flagged, " location{?s} combine High/Moderate suitability ",
                     "with High/Critical disease risk."),
        "i" = "Filter with: subset(result, suitability_class %in% c('High','Moderate') & disease_risk_class %in% c('High','Critical'))"
      ))
  }

  result
}


# \u2500\u2500 Internal helpers \u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500

.bonamia_risk <- function(result, temp, temp_col, salinity_col, known_sites) {
  p <- .bonamia_params

  # Temperature component [0-1]
  temp_risk <- dplyr::case_when(
    is.na(temp)                          ~ NA_real_,
    temp <= p$temp_risk$cold_safe        ~ 0.0,
    temp <= p$temp_risk$onset            ~
      (temp - p$temp_risk$cold_safe) /
      (p$temp_risk$onset - p$temp_risk$cold_safe) * 0.2,
    temp <= p$temp_risk$peak_low         ~
      0.2 + (temp - p$temp_risk$onset) /
      (p$temp_risk$peak_low - p$temp_risk$onset) * 0.5,
    temp <= p$temp_risk$peak_high        ~ 0.9,
    temp <= p$temp_risk$high_thresh      ~
      0.9 - (temp - p$temp_risk$peak_high) /
      (p$temp_risk$high_thresh - p$temp_risk$peak_high) * 0.1,
    TRUE                                  ~ 0.8
  )

  # Salinity component \u2014 scales base risk down at low salinities
  sal_factor <- rep(1.0, nrow(result))
  if (salinity_col %in% names(result)) {
    sal <- result[[salinity_col]]
    sal_factor <- dplyr::case_when(
      is.na(sal)                           ~ 1.0,
      sal <= p$sal_risk$low_safe           ~ 0.3,
      sal <= p$sal_risk$onset              ~
        0.3 + (sal - p$sal_risk$low_safe) /
        (p$sal_risk$onset - p$sal_risk$low_safe) * 0.4,
      sal <= p$sal_risk$full_risk          ~
        0.7 + (sal - p$sal_risk$onset) /
        (p$sal_risk$full_risk - p$sal_risk$onset) * 0.3,
      TRUE                                  ~ 1.0
    )
  }

  base_risk <- temp_risk * sal_factor

  # Proximity to known infected sites
  if (!is.null(known_sites) && nrow(known_sites) > 0) {
    prox_mult <- .proximity_multiplier(result, known_sites,
                                       .bonamia_params$proximity_multiplier)
    base_risk <- pmin(1, base_risk * prox_mult)
  }

  base_risk
}


.oshv1_risk <- function(result, temp, turbidity_col, known_sites) {
  p <- .oshv1_params

  temp_risk <- dplyr::case_when(
    is.na(temp)                         ~ NA_real_,
    temp <= p$temp_risk$cold_safe       ~ 0.0,
    temp <= p$temp_risk$onset           ~
      (temp - p$temp_risk$cold_safe) /
      (p$temp_risk$onset - p$temp_risk$cold_safe) * 0.15,
    temp <= p$temp_risk$peak_low        ~
      0.15 + (temp - p$temp_risk$onset) /
      (p$temp_risk$peak_low - p$temp_risk$onset) * 0.55,
    temp <= p$temp_risk$peak_high       ~ 0.95,
    temp <= p$temp_risk$high_thresh     ~
      0.95 - (temp - p$temp_risk$peak_high) /
      (p$temp_risk$high_thresh - p$temp_risk$peak_high) * 0.05,
    TRUE                                 ~ 0.9
  )

  # Turbidity amplifier \u2014 high suspended sediment stresses spat
  turb_mult <- rep(1.0, nrow(result))
  if (turbidity_col %in% names(result)) {
    turb <- result[[turbidity_col]]
    lo   <- p$turbidity_amplifier$low_ntu
    hi   <- p$turbidity_amplifier$high_ntu
    turb_mult <- ifelse(
      is.na(turb) | turb <= lo, 1.0,
      ifelse(turb >= hi, 1.3,
             1.0 + 0.3 * (turb - lo) / (hi - lo))
    )
  }

  base_risk <- pmin(1, temp_risk * turb_mult)

  if (!is.null(known_sites) && nrow(known_sites) > 0) {
    prox_mult <- .proximity_multiplier(result, known_sites,
                                       .bonamia_params$proximity_multiplier)
    base_risk <- pmin(1, base_risk * prox_mult)
  }

  base_risk
}


.proximity_multiplier <- function(result, known_sites, mult_table) {
  # Returns a multiplier vector based on distance to nearest known site
  lat_mid <- mean(result$lat, na.rm = TRUE)
  m_lon   <- 111320 * cos(lat_mid * pi / 180)
  m_lat   <- 111320

  mult <- rep(mult_table$beyond_20km, nrow(result))

  for (i in seq_len(nrow(result))) {
    dx   <- (result$lon[i] - known_sites$lon) * m_lon
    dy   <- (result$lat[i] - known_sites$lat) * m_lat
    dkm  <- min(sqrt(dx^2 + dy^2)) / 1000

    mult[i] <- if      (dkm <= 1)  mult_table$within_1km
               else if (dkm <= 5)  mult_table$within_5km
               else if (dkm <= 20) mult_table$within_20km
               else                mult_table$beyond_20km
  }
  mult
}
