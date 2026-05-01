#' Check exclusion criteria for all rows in a dataset
#'
#' @description
#' Applies hard-stop exclusion criteria for a given species tolerance profile.
#' Any location that violates one or more exclusion rules receives a flag and
#' is removed from the suitability scoring pipeline.
#'
#' Exclusion factors checked (where data columns are present):
#' - General temperature range
#' - Season-specific temperature floors/ceilings (requires `season` column)
#' - Salinity (with temperature-dependent minimum)
#' - Dissolved oxygen (hypoxia threshold)
#'
#' @param df A dataframe. Must contain at minimum a subset of the standard
#'   column names (see **Column Naming** below). Any exclusion factor for
#'   which no column is present is silently skipped with a warning.
#' @param tolerances A species tolerance list as returned by
#'   [get_species_tolerances()].
#'
#' @section Column Naming:
#' The function recognises these column names (case-insensitive):
#' | Variable              | Expected column name(s)          |
#' |-----------------------|----------------------------------|
#' | Temperature           | `temperature`, `temp`            |
#' | Salinity              | `salinity`, `sal`                |
#' | Dissolved oxygen      | `dissolved_oxygen`, `do`, `do_mgl` |
#' | Season                | `season` (auto-added by [add_season_column()]) |
#'
#' @return The input dataframe with two additional columns:
#'   - `excluded`: logical — `TRUE` if the location fails any exclusion criterion.
#'   - `exclusion_reason`: character — semicolon-separated list of failed criteria,
#'     or `NA` if not excluded.
#'
#' @export
#' @examples
#' tol <- get_species_tolerances("ostrea_edulis")
#' df <- data.frame(
#'   lat = 51.5, lon = -2.5,
#'   temperature = 8, salinity = 28, dissolved_oxygen = 7
#' )
#' check_exclusions(df, tol)
check_exclusions <- function(df, tolerances) {

  excl  <- tolerances$exclusions
  n     <- nrow(df)

  excluded <- rep(FALSE, n)
  reasons  <- vector("list", n)

  # Helper: find a column by any of several candidate names (case-insensitive)
  .find_col <- function(candidates) {
    lower_names <- tolower(names(df))
    match_idx   <- which(lower_names %in% tolower(candidates))
    if (length(match_idx) == 0) return(NULL)
    names(df)[match_idx[1]]
  }

  temp_col <- .find_col(c("temperature", "temp", "temp_c"))
  sal_col  <- .find_col(c("salinity", "sal", "salinity_psu"))
  do_col   <- .find_col(c("dissolved_oxygen", "do", "do_mgl", "oxygen"))
  seas_col <- .find_col(c("season"))

  # ---- 1. General temperature range ------------------------------------------
  if (!is.null(temp_col)) {
    t <- df[[temp_col]]
    too_cold <- !is.na(t) & t < excl$temperature$min
    too_hot  <- !is.na(t) & t > excl$temperature$max

    excluded <- excluded | too_cold | too_hot
    reasons[too_cold] <- lapply(reasons[too_cold], function(r)
      c(r, sprintf("temp_too_cold (%.1f < %.1f\u00b0C)", t[too_cold], excl$temperature$min)))
    reasons[too_hot] <- lapply(reasons[too_hot], function(r)
      c(r, sprintf("temp_too_hot (%.1f > %.1f\u00b0C)", t[too_hot], excl$temperature$max)))
  } else {
    cli::cli_warn("No temperature column found; skipping general temperature exclusion.")
  }

  # ---- 2. Seasonal temperature checks -----------------------------------------
  if (!is.null(temp_col) && !is.null(seas_col)) {
    t    <- df[[temp_col]]
    seas <- tolower(df[[seas_col]])

    # Guard: only apply if the species actually defines a seasonal limit
    if (!is.null(excl$temperature_winter) && !is.null(excl$temperature_winter$min)) {
      winter_flag <- seas == "winter" & !is.na(t) & t < excl$temperature_winter$min
      excluded <- excluded | winter_flag
      reasons[winter_flag] <- lapply(reasons[winter_flag], function(r)
        c(r, sprintf("winter_temp_too_cold (%.1f < %.1f\u00b0C)", t[winter_flag], excl$temperature_winter$min)))
    }

    if (!is.null(excl$temperature_summer) && !is.null(excl$temperature_summer$max)) {
      summer_flag <- seas == "summer" & !is.na(t) & t > excl$temperature_summer$max
      excluded <- excluded | summer_flag
      reasons[summer_flag] <- lapply(reasons[summer_flag], function(r)
        c(r, sprintf("summer_temp_too_hot (%.1f > %.1f\u00b0C)", t[summer_flag], excl$temperature_summer$max)))
    }
  }

  # ---- 3. Salinity -----------------------------------------------------------
  if (!is.null(sal_col)) {
    s  <- df[[sal_col]]

    # Temperature-dependent minimum
    if (!is.null(temp_col)) {
      t       <- df[[temp_col]]
      pivot   <- excl$salinity$temp_pivot
      sal_min <- ifelse(!is.na(t) & t > pivot,
                        excl$salinity$min_warm,
                        excl$salinity$min_cold)
    } else {
      sal_min <- rep(excl$salinity$min_cold, n)
    }

    sal_low  <- !is.na(s) & s < sal_min
    sal_high <- !is.na(s) & s > excl$salinity$max

    excluded <- excluded | sal_low | sal_high
    reasons[sal_low] <- lapply(seq_len(n)[sal_low], function(i)
      c(reasons[[i]], sprintf("salinity_too_low (%.1f < %.1f PSU)", s[i], sal_min[i])))
    reasons[sal_high] <- lapply(seq_len(n)[sal_high], function(i)
      c(reasons[[i]], sprintf("salinity_too_high (%.1f > %.0f PSU)", s[i], excl$salinity$max)))
  } else {
    cli::cli_warn("No salinity column found; skipping salinity exclusion.")
  }

  # ---- 4. Dissolved oxygen ---------------------------------------------------
  if (!is.null(do_col)) {
    do_vals  <- df[[do_col]]
    hypoxia  <- !is.na(do_vals) & do_vals < excl$dissolved_oxygen$min

    excluded <- excluded | hypoxia
    reasons[hypoxia] <- lapply(reasons[hypoxia], function(r)
      c(r, sprintf("hypoxia (%.2f < %.1f mg/L)", do_vals[hypoxia], excl$dissolved_oxygen$min)))
  } else {
    cli::cli_warn("No dissolved oxygen column found; skipping oxygen exclusion.")
  }

  # ---- Compile results -------------------------------------------------------
  df$excluded <- excluded
  df$exclusion_reason <- vapply(reasons, function(r) {
    if (length(r) == 0) NA_character_ else paste(r, collapse = "; ")
  }, character(1))

  n_excl <- sum(excluded)
  if (n_excl > 0) {
    cli::cli_inform(c(
      "i" = "{n_excl} location{?s} excluded by hard-stop criteria.",
      " " = "Run {.code dplyr::filter(result, excluded)} to inspect them."
    ))
  }

  df
}
