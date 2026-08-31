# =============================================================================
# In-house aragonite saturation state calculator
# =============================================================================
#
# Computes the aragonite saturation state (Omega_aragonite) from field
# measurements of pH, total alkalinity, temperature, and salinity.
# No external carbonate chemistry packages are required; all equilibrium
# constants are implemented from primary literature.
#
# Equilibrium constants used:
#   K1, K2  -- Lueker et al. (2000) Geochim Cosmochim Acta 64(4):575-590
#              (on Total pH scale; valid 2-35 degC, 19-43 PSU)
#   KB      -- Dickson (1990) Deep-Sea Res 37(5):755-766
#   KW      -- Millero (1995) Geochim Cosmochim Acta 59(4):661-677
#   Ksp_arag -- Mucci (1983) Am J Sci 283(7):780-799
#   [B_T]   -- Uppstrom (1974) Deep-Sea Res 21:161-162
#   [Ca2+]  -- Riley & Tongudai (1967) Chem Geol 2:263-269
# =============================================================================


#' Calculate aragonite saturation state from seawater carbonate chemistry
#'
#' @description
#' Computes Omega_aragonite (aragonite saturation state) from pH, total
#' alkalinity, temperature, and salinity using in-house equilibrium constant
#' expressions. No external carbonate chemistry packages are required.
#'
#' Omega_aragonite is the ratio of the ion product of calcium and carbonate
#' ions to the stoichiometric solubility product of aragonite:
#' \deqn{\Omega_{arag} = \frac{[Ca^{2+}][CO_3^{2-}]}{K_{sp,arag}}}
#'
#' Values:
#' - **> 1**: supersaturated -- shells grow/maintain normally.
#' - **~1**: marginal -- dissolution risk, especially for larvae and juveniles.
#' - **< 1**: undersaturated -- net shell dissolution occurs.
#'
#' @param temperature Numeric vector. Seawater temperature in degrees Celsius.
#' @param salinity Numeric vector. Practical salinity (PSU).
#' @param pH Numeric vector. Seawater pH on the **Total Scale** (recommended).
#'   If your sensor outputs NBS/free scale pH, convert before calling this
#'   function (typical NBS-to-Total correction is approximately -0.11 at 15 degC,
#'   35 PSU; see Dickson et al. 2007 Guide to Best Practices).
#' @param alkalinity Numeric vector. Total alkalinity in **micromol per kg
#'   seawater** (umol/kg-sw). Typical open-ocean value ~2300 umol/kg; typical
#'   shelf sea value 2100-2400 umol/kg. If your data are in umol/L, divide by
#'   seawater density (~1.025 kg/L) before passing in.
#'
#' @return Numeric vector of Omega_aragonite values (dimensionless).
#'   Returns `NA` where any input is `NA` or out of the valid temperature/
#'   salinity range for the equilibrium constants (2-35 degC, 19-43 PSU).
#'   A warning is issued if values outside this range are detected.
#'
#' @section Alkalinity assumptions:
#' The calculation uses the carbonate alkalinity approximation:
#' \deqn{TA_{carb} = TA - [B(OH)_4^-] - [OH^-] + [H^+]}
#' Minor alkalinity contributions (phosphate, silicate, fluoride) are omitted.
#' This is appropriate for typical coastal seawater. If your water has high
#' nutrient loading (>5 umol/kg phosphate), the result may be slightly
#' overestimated.
#'
#' @seealso [predict_oyster()] -- pass omega_aragonite as a column in your
#'   survey data for automatic inclusion in suitability scoring.
#'
#' @references
#' Lueker T.J., Dickson A.G. & Keeling C.D. (2000). Ocean pCO2 calculated
#'   from dissolved inorganic carbon, alkalinity, and equations for K1 and K2:
#'   validation based on laboratory measurements of CO2 in gas and seawater at
#'   equilibrium. Geochim Cosmochim Acta 64(4):575-590.
#'
#' Dickson A.G. (1990). Thermodynamics of the dissociation of boric acid in
#'   synthetic seawater. Deep-Sea Res 37(5):755-766.
#'
#' Millero F.J. (1995). Thermodynamics of the carbon dioxide system in the
#'   oceans. Geochim Cosmochim Acta 59(4):661-677.
#'
#' Mucci A. (1983). The solubility of calcite and aragonite in seawater at
#'   various salinities, temperatures, and one atmosphere total pressure.
#'   Am J Sci 283(7):780-799.
#'
#' @export
#' @examples
#' # Typical shelf sea conditions -- should return Omega ~2.5-3.0
#' calculate_aragonite(
#'   temperature = 12,
#'   salinity    = 35,
#'   pH          = 8.1,
#'   alkalinity  = 2300
#' )
#'
#' # Vectorised -- add omega_aragonite to a minimal survey dataframe
#' df <- data.frame(
#'   temperature = c(10, 12, 15),
#'   salinity    = c(34, 35, 33),
#'   ph          = c(8.05, 8.10, 8.15),
#'   alkalinity  = c(2280, 2300, 2310)
#' )
#' df$omega_aragonite <- calculate_aragonite(
#'   temperature = df$temperature,
#'   salinity    = df$salinity,
#'   pH          = df$ph,
#'   alkalinity  = df$alkalinity
#' )
#' df[, c("ph", "omega_aragonite")]
calculate_aragonite <- function(temperature, salinity, pH, alkalinity) {

  # ---- Input validation -------------------------------------------------------
  if (any(!is.na(temperature) & (temperature < 2 | temperature > 35))) {
    cli::cli_warn(c(
      "!" = "Some temperature values are outside the valid range (2-35 degC) for the Lueker et al. (2000) K1/K2 constants.",
      "i" = "Results outside this range should be treated with caution."
    ))
  }
  if (any(!is.na(salinity) & (salinity < 19 | salinity > 43))) {
    cli::cli_warn(c(
      "!" = "Some salinity values are outside the valid range (19-43 PSU) for the Lueker et al. (2000) K1/K2 constants.",
      "i" = "Results outside this range should be treated with caution."
    ))
  }

  TK <- temperature + 273.15   # Kelvin
  S  <- salinity
  h  <- 10^(-pH)               # [H+] mol/kg, Total scale
  TA <- alkalinity * 1e-6      # convert umol/kg -> mol/kg

  # ---- K1: first dissociation constant of carbonic acid ----------------------
  # Lueker et al. (2000) Eq. 1 -- Total pH scale
  lnK1 <- (
    -2307.1266 / TK +
     2.83655 -
     1.5529413 * log(TK) +
    (-4.0484 / TK - 0.20760841) * sqrt(S) +
     0.08468345 * S -
     0.00654208 * S^1.5 +
     log(1 - 0.001005 * S)
  )
  K1 <- exp(lnK1)

  # ---- K2: second dissociation constant of carbonic acid ---------------------
  # Lueker et al. (2000) Eq. 2 -- Total pH scale
  lnK2 <- (
    -3351.6106 / TK -
     9.226508 -
     0.2005743 * log(TK) +
    (-23.9722 / TK - 0.106901773) * sqrt(S) +
     0.1130822 * S -
     0.00846934 * S^1.5 +
     log(1 - 0.001005 * S)
  )
  K2 <- exp(lnK2)

  # ---- KB: dissociation constant of boric acid --------------------------------
  # Dickson (1990) -- Total pH scale
  KB <- exp(
    (
      -8966.90 - 2890.53 * sqrt(S) - 77.942 * S +
       1.728 * S^1.5 - 0.0996 * S^2
    ) / TK +
    148.0248 + 137.1942 * sqrt(S) + 1.62142 * S +
    (-24.4344 - 25.085 * sqrt(S) - 0.2474 * S) * log(TK) +
     0.053105 * sqrt(S) * TK
  )

  # ---- KW: water dissociation constant ----------------------------------------
  # Millero (1995)
  KW <- exp(
     148.9652 -
     13847.26 / TK -
     23.6521 * log(TK) +
    (-5.977 + 118.67 / TK + 1.0495 * log(TK)) * sqrt(S) -
     0.01615 * S
  )

  # ---- Ksp_aragonite: stoichiometric solubility product -----------------------
  # Mucci (1983) Table 7, aragonite
  log10_Ksp <- (
    -171.945 - 0.077993 * TK + 2903.293 / TK + 71.595 * log10(TK) +
    (-0.068393 + 0.0017276 * TK + 88.135 / TK) * sqrt(S) -
     0.10018 * S + 0.0059415 * S^1.5
  )
  Ksp_arag <- 10^log10_Ksp

  # ---- Total boron concentration ----------------------------------------------
  # Uppstrom (1974): B_T = 0.0004157 mol/kg at S = 35
  BT <- 4.157e-4 * S / 35   # mol/kg-sw

  # ---- Calcium concentration --------------------------------------------------
  # Riley & Tongudai (1967): proportional to salinity
  Ca <- 0.010282 * S / 35   # mol/kg-sw  (~10.28 mmol/kg at S=35)

  # ---- Carbonate alkalinity ---------------------------------------------------
  # Remove borate, hydroxide, and proton contributions from total alkalinity
  # to isolate the carbonate system
  OH       <- KW / h
  B_OH4    <- BT * KB / (h + KB)    # [B(OH)4-]
  TA_carb  <- TA - B_OH4 - OH + h   # mol/kg; should be positive

  # ---- Carbonate ion concentration --------------------------------------------
  # TA_carb = [HCO3-] + 2*[CO3(2-)]
  # [HCO3-] = [CO3(2-)] * h / K2
  # => TA_carb = [CO3(2-)] * (h/K2 + 2)
  CO3 <- TA_carb / (h / K2 + 2)
  CO3 <- pmax(CO3, 0)   # guard: very low pH can give negative TA_carb

  # ---- Aragonite saturation state --------------------------------------------
  omega_arag <- (Ca * CO3) / Ksp_arag

  # Set NA where inputs were NA
  omega_arag[is.na(temperature) | is.na(salinity) | is.na(pH) | is.na(alkalinity)] <- NA_real_

  omega_arag
}


#' Automatically calculate aragonite saturation if prerequisites are present
#'
#' @description
#' Called internally by [predict_oyster()] before scoring. If the survey
#' dataframe contains pH, alkalinity, temperature, and salinity columns but
#' lacks an omega_aragonite column, this function computes it in place.
#' The result column is named `omega_aragonite`.
#'
#' @param df Survey dataframe (post-standardisation).
#' @param verbose Logical. Print a message if aragonite is calculated (default TRUE).
#' @return The input dataframe, possibly with `omega_aragonite` added.
#' @keywords internal
.auto_calculate_aragonite <- function(df, verbose = TRUE) {

  col_lwr <- tolower(names(df))

  # Already present -- nothing to do
  if (any(col_lwr %in% c("omega_aragonite", "omega_arag", "aragonite_saturation"))) {
    return(df)
  }

  # Check prerequisites: pH + alkalinity + temperature + salinity
  ph_col   <- names(df)[col_lwr %in% c("ph", "seawater_ph", "sea_ph", "water_ph")][1]
  alk_col  <- names(df)[col_lwr %in% c("alkalinity", "total_alkalinity", "ta",
                                        "alk", "alkalinity_umol_kg")][1]
  temp_col <- names(df)[col_lwr %in% c("temperature", "temp", "temp_c")][1]
  sal_col  <- names(df)[col_lwr %in% c("salinity", "sal", "salinity_psu")][1]

  if (is.na(ph_col) || is.na(alk_col) || is.na(temp_col) || is.na(sal_col)) {
    return(df)   # prerequisites not available; silently skip
  }

  if (verbose) {
    cli::cli_inform(c(
      "i" = "pH and alkalinity columns detected. Calculating {.val omega_aragonite} in-house.",
      "i" = "Using Lueker et al. (2000) K1/K2, Mucci (1983) Ksp_arag."
    ))
  }

  df$omega_aragonite <- calculate_aragonite(
    temperature = as.numeric(df[[temp_col]]),
    salinity    = as.numeric(df[[sal_col]]),
    pH          = as.numeric(df[[ph_col]]),
    alkalinity  = as.numeric(df[[alk_col]])
  )

  df
}
