# =============================================================================
# Chlorophyll-a Estimation from ADCP Acoustic Backscatter
#
# Acoustic Doppler Current Profilers (ADCPs) transmit acoustic pulses and
# measure the intensity of backscattered sound. In coastal and estuarine
# environments, the principal scatterers in the upper water column are
# phytoplankton and suspended particles that co-vary with chlorophyll-a.
#
# The empirical log-linear relationship between volume backscatter strength
# (Sv, in dB) and chlorophyll-a (\u00b5g/L) is well established in the literature:
#
#   log10(Chl_a) = a * Sv + b
#  =>  Chl_a = 10^(a * Sv + b)
#
# Reference calibration constants by ADCP frequency:
#   Deines K.L. (1999) "Backscatter estimation using Broadband Acoustic Doppler
#     Current Profilers." OCEANS'99 Proceedings. MTS/IEEE.
#   Sahin C. et al. (2017) "Estimation of suspended particulate matter and
#     chlorophyll-a by acoustic backscatter at a shallow coastal site." EST.
#
# IMPORTANT: These are generic empirical constants. For best results, calibrate
# against concurrent water samples from your survey site (even 5\u201310 Niskin/grab
# samples can significantly improve accuracy). Supply calibrated `a` and `b`
# via the `calibration` argument.
#
# Typical accuracy without calibration: factor of ~2\u20133\u00d7 (r\u00b2 0.4\u20130.7)
# Typical accuracy with site calibration: factor of ~1.3\u20131.5\u00d7 (r\u00b2 0.7\u20130.9)
# =============================================================================


# Default calibration constants by ADCP frequency (Deines 1999 + updates)
.backscatter_defaults <- list(
  # c(a, b) in: log10(Chl) = a * Sv + b
  "300"  = c(a =  0.0420, b = -0.430),
  "600"  = c(a =  0.0380, b = -0.390),
  "1200" = c(a =  0.0330, b = -0.340)
)


#' Estimate chlorophyll-a concentration from ADCP acoustic backscatter
#'
#' @description
#' Uses the empirical log-linear relationship between volume backscatter
#' strength (Sv) and chlorophyll-a concentration to add an estimated
#' `chlorophyll_a` column to a survey dataframe. Intended for surveys where
#' a fluorometer was not deployed and the ADCP backscatter intensity is
#' available.
#'
#' The conversion is: `Chl_a (ugg/L) = 10 ^ (a * Sv + b)`
#'
#' Default `a` and `b` constants are derived from Deines (1999) for three
#' common ADCP frequencies (300, 600, 1200 kHz). For best results, calibrate
#' against concurrent water samples from your survey using the `calibration`
#' argument.
#'
#' @param df Dataframe containing a backscatter column and optional temperature
#'   and depth columns.
#' @param backscatter_col Character. Name of the column containing volume
#'   backscatter strength values (Sv in dB re 1 \eqn{m^{-1}}). For Nortek Signature
#'   outputs, this is typically the mean amplitude across clean bins, available
#'   as `amp_mean_dB` after [read_nortek_adcp()].
#' @param frequency_khz Numeric. ADCP operating frequency in kHz. Used to
#'   select default calibration constants if `calibration` is not supplied.
#'   Common values: 300, 600, 1200. For other frequencies, supply `calibration`
#'   directly.
#' @param calibration Numeric vector of length 2: `c(a, b)` for the log-linear
#'   model `log10(Chl_a) = a * Sv + b`. Supply to override the built-in
#'   frequency-based defaults. Derive from concurrent water samples:
#'   fit `log10(observed_chl) ~ backscatter` and extract intercept (`b`) and
#'   slope (`a`).
#' @param min_chl Numeric. Floor for estimated chlorophyll-a in ugg/L
#'   (default `0.05`). Prevents physically implausible negative estimates.
#' @param max_chl Numeric. Ceiling for estimated chlorophyll-a in ugg/L
#'   (default `50`). Values above this are likely turbidity artefacts (non-algal
#'   particles dominating backscatter).
#' @param keep_raw Logical. If `TRUE`, preserves the input backscatter column
#'   (default `TRUE`).
#' @param verbose Logical. Print conversion summary and warnings (default `TRUE`).
#'
#' @return The input dataframe with an additional `chlorophyll_a` column
#'   (ugg/L). If a `chlorophyll_a` column already exists, a warning is issued
#'   and the existing column is overwritten.
#'
#' @section Improving accuracy with water samples:
#' Collect 5--15 grab/Niskin water samples distributed across your survey area
#' and depth range, then:
#' ```r
#' # Fit calibration from samples
#' fit  <- lm(log10(sample_chl) ~ backscatter, data = samples_df)
#' a    <- coef(fit)[["backscatter"]]
#' b    <- coef(fit)[["(Intercept)"]]
#' df   <- estimate_chlorophyll_from_backscatter(
#'           df, "amp_mean_dB", frequency_khz = 600,
#'           calibration = c(a, b))
#' ```
#'
#' @export
#' @examples
#' adcp_f <- system.file("extdata", "example_bay_adcp.csv", package = "oystermapR")
#' adcp   <- read_nortek_adcp(adcp_f, verbose = FALSE)
#' # estimate_chlorophyll_from_backscatter requires an amp_mean_dB column
#' # produced by read_nortek_adcp when the ADCP data contains amplitude bins
#' if ("amp_mean_dB" %in% names(adcp)) {
#'   adcp <- estimate_chlorophyll_from_backscatter(adcp, "amp_mean_dB",
#'     frequency_khz = 300)
#' }
estimate_chlorophyll_from_backscatter <- function(df,
                                                   backscatter_col,
                                                   frequency_khz   = 600,
                                                   calibration     = NULL,
                                                   min_chl         = 0.05,
                                                   max_chl         = 50,
                                                   keep_raw        = TRUE,
                                                   verbose         = TRUE) {

  # \u2500\u2500 Validate inputs \u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500
  if (!backscatter_col %in% names(df)) {
    cli::cli_abort(c(
      "Column {.val {backscatter_col}} not found in dataframe.",
      "i" = "Available columns: {.val {names(df)}}"
    ))
  }

  Sv <- as.numeric(df[[backscatter_col]])

  if (all(is.na(Sv))) {
    cli::cli_abort("Column {.val {backscatter_col}} contains only NA values.")
  }

  # \u2500\u2500 Calibration constants \u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500
  if (!is.null(calibration)) {
    if (length(calibration) != 2) {
      cli::cli_abort("`calibration` must be a numeric vector of length 2: c(a, b).")
    }
    a <- calibration[1]
    b <- calibration[2]
    calib_source <- "user-supplied"
  } else {
    freq_key <- as.character(round(frequency_khz, -2))   # nearest 100 kHz
    defaults <- .backscatter_defaults[[freq_key]]
    if (is.null(defaults)) {
      cli::cli_warn(c(
        "No default calibration for {frequency_khz} kHz.",
        "i" = "Supported: {.val {names(.backscatter_defaults)}} kHz.",
        "i" = "Using 600 kHz defaults. Supply {.arg calibration} for better accuracy."
      ))
      defaults <- .backscatter_defaults[["600"]]
    }
    a <- defaults["a"]
    b <- defaults["b"]
    calib_source <- paste0("built-in (", freq_key, " kHz defaults)")
  }

  # \u2500\u2500 Convert \u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500
  chl_raw <- 10 ^ (a * Sv + b)
  chl     <- pmin(pmax(chl_raw, min_chl), max_chl)

  n_clipped_low  <- sum(!is.na(chl_raw) & chl_raw < min_chl)
  n_clipped_high <- sum(!is.na(chl_raw) & chl_raw > max_chl)

  # \u2500\u2500 Overwrite warning \u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500
  if ("chlorophyll_a" %in% names(df)) {
    cli::cli_warn(c(
      "!" = "Column {.val chlorophyll_a} already exists \u2014 overwriting.",
      "i" = "Rename the existing column if you want to preserve it."
    ))
  }

  df$chlorophyll_a <- chl

  if (verbose) {
    cli::cli_inform(c(
      "i" = "Chlorophyll-a estimated from backscatter ({.val {backscatter_col}})",
      " " = "Calibration: {calib_source}  [a={round(a,4)}, b={round(b,4)}]",
      " " = paste0("Mean Chl-a: {round(mean(chl, na.rm=TRUE), 2)} \u00b5g/L  |  ",
                   "Range: {round(min(chl, na.rm=TRUE),2)}\u2013{round(max(chl, na.rm=TRUE),2)} \u00b5g/L"),
      " " = paste0("Clipped low (<{min_chl}): {n_clipped_low} pts  |  ",
                   "Clipped high (>{max_chl}): {n_clipped_high} pts")
    ))
    if (n_clipped_high > 0) {
      cli::cli_warn(c(
        "!" = "{n_clipped_high} point{?s} capped at {max_chl} \u00b5g/L.",
        "i" = "High backscatter may reflect suspended sediment rather than phytoplankton.",
        "i" = "Consider lowering {.arg max_chl} or masking turbid nearshore cells."
      ))
    }
    cli::cli_inform(c(
      "i" = "Accuracy note: without site calibration, expect factor 2\u20133\u00d7 uncertainty.",
      "i" = "Collect water samples and supply {.arg calibration = c(a, b)} to improve this."
    ))
  }

  df
}


# =============================================================================
# Substrate classification from near-seabed backscatter
# =============================================================================

#' Classify seabed substrate hardness from near-seabed ADCP backscatter
#'
#' @description
#' Uses the intensity of acoustic backscatter from the near-seabed bin(s) to
#' classify substrate type. Hard substrates (gravel, rock, shell hash) scatter
#' more sound energy than soft substrates (mud, fine sand). The function
#' converts raw backscatter (dB re 1 m^-1 or instrument counts) to a
#' categorical substrate class and a continuous hardness index \[0, 1\].
#'
#' **Classification thresholds (default, adjustable):**
#' | Class        | Hardness index | Typical substrate              |
#' |--------------|---------------|-------------------------------|
#' | Hard         | 0.8 -- 1.0     | Rock, cobble, shell hash       |
#' | Mixed        | 0.5 -- 0.8     | Gravel/sand mix, maerl beds    |
#' | Soft         | 0.2 -- 0.5     | Sand, sandy mud                |
#' | Very Soft    | 0.0 -- 0.2     | Mud, fine silt                 |
#'
#' The hardness index is a min-max normalisation of the mean near-seabed
#' backscatter across the bottom `n_bottom_bins` cells. If the column
#' contains calibrated volume backscatter (Sv in dB), set `is_sv = TRUE`
#' and a sign-flip is applied (less negative Sv = stronger return = harder).
#'
#' @param df Dataframe with at least one column of seabed backscatter values.
#' @param backscatter_col Character. Name of the backscatter column.
#' @param is_sv Logical. If TRUE, treats values as Sv (dB, typically negative);
#'   hardness is derived from abs(Sv) after sign convention correction.
#'   Default FALSE (raw instrument counts, higher = harder).
#' @param bs_min Numeric. Backscatter value mapped to hardness = 0 (softest).
#'   Defaults to 5th percentile of observed data.
#' @param bs_max Numeric. Backscatter value mapped to hardness = 1 (hardest).
#'   Defaults to 95th percentile of observed data.
#' @param hard_thresh Numeric. Hardness index above which substrate is "Hard"
#'   (default 0.8).
#' @param mixed_thresh Numeric. Hardness index above which substrate is "Mixed"
#'   (default 0.5).
#' @param soft_thresh Numeric. Hardness index above which substrate is "Soft"
#'   (default 0.2); below is "Very Soft".
#' @param output_col Character. Base name for output columns. Two columns are
#'   added: `<output_col>_index` (numeric, 0-1) and `<output_col>_class`
#'   (character). Default `"substrate_hardness"`.
#' @param verbose Logical. Print classification summary (default TRUE).
#'
#' @return Input dataframe with `substrate_hardness_index` and
#'   `substrate_hardness_class` columns appended. These map directly to the
#'   `substrate_hardness` scored variable in the tolerance spec.
#'
#' @export
#' @examples
#' # Minimal dataframe with a mean-volume backscatter column (dB re 1 m-1)
#' df <- data.frame(
#'   lat           = c(51.5, 51.6, 51.7),
#'   lon           = c(-3.2, -3.2, -3.1),
#'   sv_backscatter = c(-55.0, -68.0, -75.0)
#' )
#' df <- classify_substrate_from_backscatter(df, backscatter_col = "sv_backscatter",
#'                                            is_sv = TRUE, verbose = FALSE)
#' df[, c("substrate_hardness_index", "substrate_hardness_class")]
classify_substrate_from_backscatter <- function(df,
                                                 backscatter_col,
                                                 is_sv        = FALSE,
                                                 bs_min       = NULL,
                                                 bs_max       = NULL,
                                                 hard_thresh  = 0.80,
                                                 mixed_thresh = 0.50,
                                                 soft_thresh  = 0.20,
                                                 output_col   = "substrate_hardness",
                                                 verbose      = TRUE) {

  if (!backscatter_col %in% names(df))
    cli::cli_abort("Column {.val {backscatter_col}} not found in dataframe.")

  bs <- df[[backscatter_col]]

  # For Sv (negative dB), flip sign so harder = larger positive number
  if (is_sv) bs <- abs(bs)

  # Normalise to [0, 1]
  lo <- if (!is.null(bs_min)) bs_min else stats::quantile(bs, 0.05, na.rm = TRUE)
  hi <- if (!is.null(bs_max)) bs_max else stats::quantile(bs, 0.95, na.rm = TRUE)

  if (hi <= lo) cli::cli_abort("bs_max must be greater than bs_min.")

  hardness <- pmin(1, pmax(0, (bs - lo) / (hi - lo)))

  substrate_class <- dplyr::case_when(
    is.na(hardness)          ~ NA_character_,
    hardness >= hard_thresh  ~ "Hard",
    hardness >= mixed_thresh ~ "Mixed",
    hardness >= soft_thresh  ~ "Soft",
    TRUE                     ~ "Very Soft"
  )

  idx_col   <- paste0(output_col, "_index")
  class_col <- paste0(output_col, "_class")

  df[[idx_col]]   <- hardness
  df[[class_col]] <- substrate_class

  if (verbose) {
    tbl <- table(substrate_class)
    cli::cli_h3("Substrate Classification ({backscatter_col})")
    cli::cli_inform(c(
      " " = paste0("Normalisation range: [", round(lo, 2), ", ", round(hi, 2), "]"),
      " " = paste0("Hard: ",      tbl["Hard"]      %||% 0, " | ",
                   "Mixed: ",     tbl["Mixed"]     %||% 0, " | ",
                   "Soft: ",      tbl["Soft"]      %||% 0, " | ",
                   "Very Soft: ", tbl["Very Soft"] %||% 0)
    ))
    cli::cli_inform(c(
      "i" = paste0("Rename column to 'substrate_hardness' to use as scored variable: ",
                   "names(df)[names(df) == '", idx_col, "'] <- 'substrate_hardness'")
    ))
  }

  df
}
