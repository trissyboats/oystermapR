# =============================================================================
# Sediment stability and mobility scoring
# =============================================================================
#
# A substrate that appears suitable from its static classification (e.g. "sand")
# may be highly mobile under combined wave and tidal forcing \u2014 producing
# migrating bedforms, sand wave fields, or storm-driven erosion events that
# bury or abrade juvenile oysters and destabilise restoration reef structures.
#
# The Shields parameter (\u03b8) is the standard dimensionless measure of bed
# shear stress relative to the critical stress for sediment entrainment:
#
#   \u03b8 = \u03c4_b / ((\u03c1_s - \u03c1_w) * g * d_50)
#
# where:
#   \u03c4_b  = bed shear stress (N/m\u00b2)
#   \u03c1_s  = sediment density (kg/m\u00b3; ~2650 for quartz, ~2700 for biogenic shell)
#   \u03c1_w  = seawater density (kg/m\u00b3; ~1025)
#   g    = 9.81 m/s\u00b2
#   d_50 = median grain diameter (m)
#
# Bed shear stress from current alone:
#   \u03c4_b = \u03c1_w * C_f * U_b\u00b2
#   C_f = quadratic drag coefficient = (\u03ba / ln(z_r/z_0))\u00b2
#         with \u03ba = 0.41 (von K\u00e1rm\u00e1n), z_r = 1 m (reference height), z_0 = 0.001 m
#         (roughness length for smooth mixed seabed)
#
# Wave orbital velocity contribution (Grant & Madsen 1979 simplified):
#   U_w = \u03c0 * Hs / (T_p * sinh(k*h))
#   with dispersion relation for k and user-supplied peak period T_p.
#   If wave data absent: U_w estimated from Hs alone assuming T_p = 8 s.
#
# Combined wave-current stress uses the simple vector addition approximation:
#   U_combined = sqrt(U_b^2 + U_w^2)  (conservative \u2014 assumes aligned)
#
# Critical Shields number:
#   \u03b8_cr \u2248 0.047 for coarse sand / gravel (Shields 1936; van Rijn 1984)
#   \u03b8_cr increases for very fine sediment (cohesion effects; Hjulstr\u00f6m curve)
#
# Oyster-specific context:
#  - Oyster shell (d_50 ~ 20-50 mm) has \u03b8_cr ~ 0.06-0.10 \u2014 very stable
#  - Gravel substratum (d_50 ~ 4-20 mm) \u03b8_cr ~ 0.05-0.08
#  - Coarse sand (d_50 ~ 0.5-2 mm) \u03b8_cr ~ 0.04-0.06; mobile in storms
#  - Fine sand / mud \u2014 mobile at minimal current speeds; unsuitable as primary
#    restoration substrate
#
# References:
#  Shields (1936): Anwendung der \u00c4hnlichkeitsmechanik. Mitt. Preuss. Versuchsanst.
#  van Rijn (1984): Sediment transport, Part I: Bed load transport. J Hydraul Eng.
#  Grant & Madsen (1979): Combined wave and current interaction with rough bottom.
#   JGR Oceans.
#  Whitehouse et al. (2000): Settling, Transport and Entrainment of Sediment. HR Wallingford.

#' Score sediment stability and mobility at survey locations
#'
#' @description
#' Calculates the Shields parameter and a sediment stability score \[0,1\] for
#' each survey location. High sediment mobility (theta >> theta_cr) indicates that
#' the seabed is frequently in motion, which is unfavourable for oyster
#' settlement, juvenile survival, and restoration reef persistence.
#'
#' @section Inputs required:
#' At minimum, the function needs current velocity to estimate bed shear
#' stress. Grain size improves accuracy but can be estimated from the
#' substrate type column if not directly measured.
#'
#' @param result Dataframe from [predict_oyster()]. Must contain `lat`, `lon`.
#'   Key optional columns (column names configurable via arguments):
#'   - `current_ms`: depth-averaged current speed (m/s)
#'   - `wave_hs_m`: significant wave height (m) — from `score_wave_exposure()`
#'     or direct measurement
#'   - `depth_m`: water depth (m) — needed for wave orbital velocity
#'   - `substrate`: substrate classification string — used to estimate d50
#'   - `d50_mm`: median grain diameter (mm) — overrides substrate-derived d50
#'
#' @param current_col Character. Column name for depth-averaged current speed
#'   (m/s). Default `"current_ms"`.
#' @param wave_hs_col Character. Column name for significant wave height (m).
#'   Default `"wave_hs_m"`.
#' @param depth_col Character. Column name for depth (m). Default `"depth_m"`.
#' @param substrate_col Character. Column name for substrate type. Default
#'   `"substrate"`.
#' @param d50_col Character. Column name for measured median grain size (mm).
#'   Default `"d50_mm"`. If present, overrides substrate-derived estimate.
#' @param wave_period_s Numeric. Peak wave period in seconds for orbital
#'   velocity calculation. Default 8 s (typical NW European sea state).
#' @param drag_coef Numeric. Quadratic bed drag coefficient C_f. Default
#'   0.003 (smooth mixed seabed; increase to 0.005-0.010 for rough rock/reef).
#' @param verbose Logical. Default TRUE.
#'
#' @return `result` with additional columns:
#'   - `shields_parameter`: Shields number theta at each site
#'   - `shields_critical`: critical Shields number theta_cr for the substrate d50
#'   - `mobility_ratio`: theta / theta_cr (> 1.0 = mobile; < 1.0 = stable)
#'   - `d50_mm_estimated`: grain size used (mm) — measured or substrate-derived
#'   - `sediment_stability_score` \[0,1\]: 1 = fully stable, 0 = highly mobile
#'   - `sediment_mobility_class`: "Stable" / "Marginally stable" / "Mobile" / "Highly mobile"
#'   - `sediment_stability_note`: management flag
#'
#' @export
#' @examples
#' \dontrun{
#' # With current velocity and substrate type columns
#' result <- score_sediment_stability(result, current_col="current_ms", substrate_col="substrate")
#'
#' # After score_wave_exposure() — wave_hs_m column already present
#' result <- score_wave_exposure(result, fetch_km=15)
#' result <- score_sediment_stability(result)
#'
#' # With measured grain size
#' result$d50_mm <- c(0.25, 2.5, 15.0, 0.08)
#' result <- score_sediment_stability(result)
#' }
score_sediment_stability <- function(result,
                                      current_col   = "current_ms",
                                      wave_hs_col   = "wave_hs_m",
                                      depth_col     = "depth_m",
                                      substrate_col = "substrate",
                                      d50_col       = "d50_mm",
                                      wave_period_s = 8,
                                      drag_coef     = 0.003,
                                      verbose       = TRUE) {

  if (!all(c("lat","lon") %in% names(result)))
    cli::cli_abort("result must contain 'lat' and 'lon' columns.")

  n <- nrow(result)

  # \u2500\u2500 Physical constants \u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500
  g     <- 9.81          # m/s\u00b2
  rho_w <- 1025          # kg/m\u00b3 seawater
  rho_s <- 2650          # kg/m\u00b3 quartz (biogenic shell ~2700, similar)
  R     <- (rho_s - rho_w) / rho_w  # submerged specific gravity ~1.585

  # \u2500\u2500 Substrate \u2192 d50 lookup \u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500
  # Median grain diameter in mm for each substrate class
  sub_to_d50 <- c(
    "very soft" = 0.010,   # fluid mud / cohesive
    "soft"      = 0.080,   # fine silt / mud
    "sand"      = 0.300,   # medium sand (0.2-0.5 mm typical)
    "mixed"     = 3.000,   # coarse sand to fine gravel
    "hard"      = 25.000,  # gravel / shell / rock (effectively immobile)
    "rock"      = 50.000,
    "gravel"    = 15.000,
    "shell"     = 30.000,
    "reef"      = 50.000
  )

  # \u2500\u2500 Determine d50 per row \u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500
  d50_mm <- rep(NA_real_, n)

  if (!is.null(d50_col) && d50_col %in% names(result)) {
    d50_mm <- as.numeric(result[[d50_col]])
  }

  if (substrate_col %in% names(result)) {
    sub_lower <- tolower(trimws(as.character(result[[substrate_col]])))
    for (i in seq_len(n)) {
      if (!is.na(d50_mm[i])) next   # already have measured value
      s <- sub_lower[i]
      # Match each lookup key as substring
      for (k in names(sub_to_d50)) {
        if (grepl(k, s, fixed = TRUE)) {
          d50_mm[i] <- sub_to_d50[k]
          break
        }
      }
    }
  }

  # Default for any still-NA
  if (any(is.na(d50_mm))) {
    if (verbose)
      cli::cli_warn(c(
        "{sum(is.na(d50_mm))} row(s) have no substrate or grain size data.",
        "i" = "Defaulting to d50 = 1 mm (coarse sand). Provide substrate or d50_mm column.",
        "i" = "Results for these rows are unreliable."
      ))
    d50_mm[is.na(d50_mm)] <- 1.0
  }

  result$d50_mm_estimated <- d50_mm
  d50_m <- d50_mm / 1000   # convert to metres

  # \u2500\u2500 Critical Shields number \u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500
  # van Rijn (1984) simplified: \u03b8_cr varies with Archimedes / D* parameter
  # D* = d_50 * (R*g / \u03bd^2)^(1/3)  with \u03bd = 1.2e-6 m\u00b2/s (seawater at 15\u00b0C)
  nu  <- 1.2e-6
  D_star <- d50_m * ((R * g) / nu^2)^(1/3)

  # Critical Shields number (Soulsby 1997 empirical fit):
  # \u03b8_cr = 0.30/(1 + 1.2*D*) + 0.055*(1 - exp(-0.020*D*))
  theta_cr <- 0.30 / (1 + 1.2 * D_star) + 0.055 * (1 - exp(-0.020 * D_star))
  # For cohesive very fine sediment (D* < 1): \u03b8_cr can be higher due to cohesion,
  # but entrainment is less mechanically relevant for oyster restoration
  theta_cr <- pmax(theta_cr, 0.020)  # floor

  # \u2500\u2500 Current velocity \u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500
  U_b <- rep(0.0, n)
  if (current_col %in% names(result)) {
    U_b <- pmax(as.numeric(result[[current_col]]), 0)
    U_b[is.na(U_b)] <- 0
  } else {
    if (verbose)
      cli::cli_warn(c(
        "No current velocity column '{current_col}' found.",
        "i" = "Wave orbital velocity only will be used where wave data is available.",
        "i" = "Stability estimates will be unreliable without current data."
      ))
  }

  # \u2500\u2500 Wave orbital velocity \u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500
  # Grant & Madsen (1979) simplified: U_w = \u03c0 * Hs / (Tp * sinh(kh))
  # Dispersion relation: \u03c9\u00b2 = g*k*tanh(k*h), solved iteratively
  U_w <- rep(0.0, n)

  has_hs <- wave_hs_col %in% names(result)
  has_d  <- depth_col %in% names(result)

  if (has_hs && has_d) {
    hs <- pmax(as.numeric(result[[wave_hs_col]]), 0)
    hs[is.na(hs)] <- 0
    h  <- pmax(abs(as.numeric(result[[depth_col]])), 0.1)
    h[is.na(h)] <- 10

    omega <- 2 * pi / wave_period_s

    for (i in seq_len(n)) {
      if (hs[i] <= 0) next
      # Iterative solution of dispersion relation \u03c9\u00b2 = g*k*tanh(k*h)
      k_est <- omega^2 / g  # deep water initial guess
      for (iter in 1:10) {
        k_est <- omega^2 / (g * tanh(k_est * h[i]))
      }
      kh <- k_est * h[i]
      sinh_kh <- sinh(kh)
      if (sinh_kh > 1e6) sinh_kh <- 1e6  # deep water limit
      U_w[i] <- pi * hs[i] / (wave_period_s * sinh_kh)
    }
  } else if (has_hs) {
    hs <- pmax(as.numeric(result[[wave_hs_col]]), 0)
    hs[is.na(hs)] <- 0
    # Approximate U_w without depth: deep water approximation
    U_w <- pi * hs / wave_period_s
  }

  # \u2500\u2500 Combined bed shear stress \u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500
  # Conservative: assume waves and currents aligned \u2192 vector addition
  U_combined <- sqrt(U_b^2 + U_w^2)

  tau_b   <- rho_w * drag_coef * U_combined^2
  theta   <- tau_b / ((rho_s - rho_w) * g * d50_m)

  result$shields_parameter <- round(theta, 4)
  result$shields_critical  <- round(theta_cr, 4)
  result$mobility_ratio    <- round(theta / theta_cr, 3)

  # \u2500\u2500 Stability score [0,1] \u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500
  # Score = 1.0 when \u03b8/\u03b8_cr = 0 (fully stable)
  # Score decays as mobility increases:
  #  \u03b8/\u03b8_cr = 0.5 \u2192 score 0.85  (rarely mobile)
  #  \u03b8/\u03b8_cr = 1.0 \u2192 score 0.50  (at entrainment threshold)
  #  \u03b8/\u03b8_cr = 2.0 \u2192 score 0.20  (mobile bedforms)
  #  \u03b8/\u03b8_cr >= 5.0 \u2192 score 0.0   (highly mobile / sandwaves)

  mr <- result$mobility_ratio
  result$sediment_stability_score <- pmax(
    ifelse(mr < 0.5,  1.0  - 0.15  * mr / 0.5,
    ifelse(mr < 1.0,  0.85 - 0.35  * (mr - 0.5) / 0.5,
    ifelse(mr < 2.0,  0.50 - 0.30  * (mr - 1.0) / 1.0,
    ifelse(mr < 5.0,  0.20 - 0.20  * (mr - 2.0) / 3.0,
                      0.0)))),
    0.0
  )

  result$sediment_mobility_class <- dplyr::case_when(
    result$mobility_ratio < 0.5  ~ "Stable",
    result$mobility_ratio < 1.0  ~ "Marginally stable",
    result$mobility_ratio < 2.0  ~ "Mobile",
    TRUE                         ~ "Highly mobile"
  )

  result$sediment_stability_note <- dplyr::case_when(
    result$sediment_mobility_class == "Stable" ~
      "Seabed stable under design conditions. Suitable for spat and reef deployment.",
    result$sediment_mobility_class == "Marginally stable" ~
      paste0("Near-threshold mobility (\u03b8/\u03b8_cr = ",
             round(result$mobility_ratio, 2),
             "). Reef may be stable in calm periods but monitor during storms."),
    result$sediment_mobility_class == "Mobile" ~
      paste0("Mobile seabed (\u03b8/\u03b8_cr = ",
             round(result$mobility_ratio, 2),
             "). Elevated platform or cage deployment required; spat mortality risk is high."),
    result$sediment_mobility_class == "Highly mobile" ~
      paste0("Highly mobile seabed (\u03b8/\u03b8_cr = ",
             round(result$mobility_ratio, 2),
             "). Unsuitable for any benthic restoration without sediment stabilisation works."),
    TRUE ~ NA_character_
  )

  if (verbose) {
    tab <- table(result$sediment_mobility_class)
    for (cl in names(tab)) {
      n_cl <- tab[cl]
      med_ratio <- round(median(result$mobility_ratio[
        result$sediment_mobility_class == cl], na.rm=TRUE), 2)
      cli::cli_inform("  Sediment {cl}: {n_cl} site{?s} (median \u03b8/\u03b8_cr = {med_ratio})")
    }
  }

  result
}
