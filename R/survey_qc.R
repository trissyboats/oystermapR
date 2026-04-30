# =============================================================================
# Automatic survey data quality control and outlier flagging
# =============================================================================

#' Run automated quality control on raw survey data
#'
#' @description
#' Detects and flags suspect sensor readings before they enter the suitability
#' model. Three complementary checks are applied to each numeric column:
#'
#' 1. **Range check** — values outside the physically plausible range for that
#'    variable (e.g. temperature > 35°C in temperate coastal water, salinity
#'    > 42 PSU) are flagged as `"range_fail"`.
#'
#' 2. **Statistical outlier** — values beyond `iqr_k` × IQR from the median
#'    are flagged as `"outlier"` (default k = 3, Tukey's outer fence).
#'
#' 3. **Temporal gradient** — when a `datetime` column is present and data is
#'    sorted chronologically, sequential differences exceeding the
#'    `max_gradient` threshold are flagged as `"gradient_fail"` (instrument
#'    spike or data entry error).
#'
#' Additionally, **cross-variable sanity checks** flag physically implausible
#' combinations:
#' - Dissolved oxygen > 20 mg/L with temperature < 5°C (likely sensor error)
#' - Salinity < 5 PSU with temperature > 25°C (likely freshwater intrusion
#'   instrument error, not a real coastal reading)
#' - Chlorophyll_a > 50 µg/L (extreme bloom or sensor fouling)
#'
#' Flagged values are **not removed** — they are marked in a `qc_flag_<col>`
#' column so the user can decide whether to replace with NA, correct, or
#' accept them. A summary `qc_n_flags` column counts total flags per row.
#'
#' @param df Dataframe of raw survey measurements.
#' @param datetime_col Character or NULL. Name of the datetime column for
#'   temporal gradient checks (default `"datetime"`). Set to NULL to skip.
#' @param iqr_k Numeric. IQR multiplier for outlier detection (default 3.0;
#'   use 1.5 for stricter QC of high-precision CTD data).
#' @param max_gradients Named numeric vector. Maximum allowed absolute change
#'   per unit time (seconds) per variable. Defaults are conservative for typical
#'   towed or lowered deployments.
#' @param apply_flags Logical. If TRUE, replace flagged values with NA in the
#'   returned dataframe. If FALSE (default), flags are added but values
#'   preserved for manual review.
#' @param verbose Logical. Print QC summary (default TRUE).
#'
#' @return Input dataframe with additional columns:
#'   `qc_flag_<varname>` for each checked variable (`"ok"`, `"range_fail"`,
#'   `"outlier"`, `"gradient_fail"`, `"cross_fail"`),
#'   `qc_n_flags` (integer count of flags per row),
#'   `qc_status` (`"pass"`, `"review"`, `"fail"` — fail = 3+ flags on one row).
#'
#' @export
#' @examples
#' \dontrun{
#' # Run QC before modelling
#' survey_qc <- qc_survey_data(survey, datetime_col = "timestamp")
#'
#' # Inspect flagged rows
#' flagged <- subset(survey_qc, qc_status %in% c("review","fail"))
#' View(flagged)
#'
#' # Apply flags (replace flagged values with NA) before scoring
#' survey_clean <- qc_survey_data(survey, apply_flags = TRUE)
#' result <- predict_oyster(survey_clean, "ostrea_edulis")
#' }
qc_survey_data <- function(df,
                             datetime_col    = "datetime",
                             iqr_k           = 3.0,
                             max_gradients   = NULL,
                             apply_flags     = FALSE,
                             verbose         = TRUE) {

  # \u2500\u2500 Physical plausibility ranges \u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500
  .phys_ranges <- list(
    temperature       = c(-2,   35),
    salinity          = c( 0,   42),
    depth             = c(-1,  500),
    dissolved_oxygen  = c( 0,   20),
    turbidity         = c( 0,  500),
    chlorophyll_a     = c( 0,   80),
    ph                = c( 6.5, 9.5),
    current_velocity  = c( 0,    5),
    substrate_hardness= c( 0,    1)
  )

  # \u2500\u2500 Default temporal gradient limits (change per second) \u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500
  .default_gradients <- list(
    temperature      = 0.05,   # 0.05\u00b0C/s = 3\u00b0C/min \u2014 plausible for fast tow
    salinity         = 0.10,
    depth            = 2.00,   # 2 m/s descent rate
    dissolved_oxygen = 0.20,
    turbidity        = 5.00,
    chlorophyll_a    = 0.50
  )

  if (!is.null(max_gradients))
    for (nm in names(max_gradients))
      .default_gradients[[nm]] <- max_gradients[[nm]]

  n    <- nrow(df)
  vars <- intersect(names(.phys_ranges), names(df))

  flag_cols <- list()
  for (v in vars) flag_cols[[paste0("qc_flag_", v)]] <- rep("ok", n)

  # \u2500\u2500 1. Range checks \u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500
  for (v in vars) {
    x   <- df[[v]]
    rng <- .phys_ranges[[v]]
    fc  <- paste0("qc_flag_", v)
    flag_cols[[fc]][!is.na(x) & (x < rng[1] | x > rng[2])] <- "range_fail"
  }

  # \u2500\u2500 2. Statistical outliers (Tukey outer fence) \u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500
  for (v in vars) {
    x  <- df[[v]]
    fc <- paste0("qc_flag_", v)
    q  <- stats::quantile(x, c(0.25, 0.75), na.rm = TRUE)
    iqr <- q[2] - q[1]
    lo  <- q[1] - iqr_k * iqr
    hi  <- q[2] + iqr_k * iqr
    is_out <- !is.na(x) & (x < lo | x > hi)
    flag_cols[[fc]][is_out & flag_cols[[fc]] == "ok"] <- "outlier"
  }

  # \u2500\u2500 3. Temporal gradient checks \u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500
  has_dt <- !is.null(datetime_col) && datetime_col %in% names(df)
  if (has_dt) {
    dt_raw <- df[[datetime_col]]
    dt     <- tryCatch(as.POSIXct(dt_raw), error = function(e) NULL)

    if (!is.null(dt)) {
      ord      <- order(dt)
      dt_sec   <- as.numeric(dt)
      dt_diff  <- c(NA, diff(dt_sec[ord]))

      grad_vars <- intersect(names(.default_gradients), vars)
      for (v in grad_vars) {
        x      <- df[[v]][ord]
        x_diff <- c(NA, abs(diff(x)))
        max_g  <- .default_gradients[[v]]
        fc     <- paste0("qc_flag_", v)
        # Rate = change / time_seconds; flag if > threshold
        rate   <- ifelse(!is.na(dt_diff) & dt_diff > 0, x_diff / dt_diff, 0)
        bad    <- !is.na(rate) & rate > max_g
        # Map back to original row order
        flag_vec <- flag_cols[[fc]]
        flag_vec[ord][bad & flag_vec[ord] == "ok"] <- "gradient_fail"
        flag_cols[[fc]] <- flag_vec
      }
    }
  }

  # \u2500\u2500 4. Cross-variable sanity checks \u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500
  has_do   <- "dissolved_oxygen" %in% names(df)
  has_temp <- "temperature"      %in% names(df)
  has_sal  <- "salinity"         %in% names(df)
  has_chl  <- "chlorophyll_a"    %in% names(df)

  if (has_do && has_temp) {
    do_vals   <- df$dissolved_oxygen
    temp_vals <- df$temperature
    cross_bad <- !is.na(do_vals) & !is.na(temp_vals) &
                 do_vals > 18 & temp_vals < 5
    flag_cols[["qc_flag_dissolved_oxygen"]][cross_bad] <- "cross_fail"
  }

  if (has_sal && has_temp) {
    sal_vals  <- df$salinity
    temp_vals <- df$temperature
    cross_bad <- !is.na(sal_vals) & !is.na(temp_vals) &
                 sal_vals < 5 & temp_vals > 25
    flag_cols[["qc_flag_salinity"]][cross_bad] <- "cross_fail"
  }

  if (has_chl) {
    chl_vals  <- df$chlorophyll_a
    cross_bad <- !is.na(chl_vals) & chl_vals > 50
    if ("qc_flag_chlorophyll_a" %in% names(flag_cols))
      flag_cols[["qc_flag_chlorophyll_a"]][cross_bad] <- "cross_fail"
  }

  # \u2500\u2500 Attach flag columns \u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500
  for (nm in names(flag_cols))
    df[[nm]] <- flag_cols[[nm]]

  # \u2500\u2500 Per-row flag count and status \u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500
  flag_mat  <- do.call(cbind, lapply(flag_cols, function(f) f != "ok"))
  n_flags   <- rowSums(flag_mat, na.rm = TRUE)
  df$qc_n_flags <- n_flags
  df$qc_status  <- dplyr::case_when(
    n_flags >= 3 ~ "fail",
    n_flags >= 1 ~ "review",
    TRUE          ~ "pass"
  )

  # \u2500\u2500 Optionally replace flagged values with NA \u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500
  if (apply_flags) {
    for (v in vars) {
      fc <- paste0("qc_flag_", v)
      df[[v]][df[[fc]] != "ok"] <- NA_real_
    }
    if (verbose)
      cli::cli_inform("i" = "Flagged values replaced with NA (apply_flags = TRUE).")
  }

  # \u2500\u2500 Summary \u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500
  if (verbose) {
    cli::cli_h2("Survey QC Summary")
    cli::cli_inform(c(
      " " = paste0("Rows: ", n,
                   " | Pass: ",   sum(df$qc_status == "pass"),
                   " | Review: ", sum(df$qc_status == "review"),
                   " | Fail: ",   sum(df$qc_status == "fail"))
    ))

    any_flagged <- FALSE
    for (v in vars) {
      fc   <- paste0("qc_flag_", v)
      n_fl <- sum(df[[fc]] != "ok")
      if (n_fl > 0) {
        types <- paste(names(table(df[[fc]][df[[fc]] != "ok"])), collapse = ", ")
        cli::cli_inform("  {v}: {n_fl} flag{?s} ({types})")
        any_flagged <- TRUE
      }
    }
    if (!any_flagged)
      cli::cli_inform("\u2713 All variables passed QC checks.")

    if (sum(df$qc_status == "fail") > 0)
      cli::cli_warn(c(
        "!" = paste0(sum(df$qc_status == "fail"),
                     " row{?s} flagged as 'fail' (3+ issues)."),
        "i" = "Inspect with: subset(df, qc_status == 'fail')",
        "i" = "Remove with:  df <- subset(df, qc_status != 'fail') or set apply_flags = TRUE"
      ))
  }

  df
}
