# =============================================================================
# Seasonal composite suitability scoring
# =============================================================================

#' Combine multi-season survey results into a composite suitability score
#'
#' @description
#' Oysters are permanent residents; a location is only truly suitable if
#' conditions are adequate year-round. `composite_seasonal()` takes a named
#' list of `predict_oyster()` result dataframes (one per season or survey
#' visit) and produces a single composite dataframe with one row per spatial
#' cell, summarising suitability across all input surveys.
#'
#' **Composite methods:**
#' - `"min"` (default) — most conservative; a location must be suitable in
#'   *every* season to score well. Best for regulatory site selection.
#' - `"mean"` — average across seasons. Good for monitoring trend summaries.
#' - `"prob"` — fraction of seasons with suitability >= `prob_threshold`.
#'   Useful when surveys are irregularly spaced or some seasons are missing.
#'
#' Spatial matching uses a grid cell tolerance of `cell_deg` decimal degrees
#' (default 0.001 approx. 111 m). Cells not present in all seasons are flagged in
#' `n_seasons_present`.
#'
#' @param surveys Named list of dataframes from [predict_oyster()]. Each must
#'   have `lat`, `lon`, and `suitability` columns. Names are used as season
#'   labels (e.g. `list(spring = r1, summer = r2, autumn = r3)`).
#' @param method Character. One of `"min"`, `"mean"`, `"prob"` (default `"min"`).
#' @param prob_threshold Numeric \[0,1\]. Suitability threshold used when
#'   `method = "prob"` (default 0.5).
#' @param cell_deg Numeric. Spatial tolerance in decimal degrees for matching
#'   cells across surveys (default 0.001).
#' @param verbose Logical. Print summary (default TRUE).
#'
#' @return A dataframe with columns: `lat`, `lon`, `suitability_composite`,
#'   `suitability_class`, `n_seasons_present`, one `suit_<season>` column per
#'   input survey, and `suit_range` (max - min across seasons).
#'
#' @export
#' @examples
#' \dontrun{
#' spring <- predict_oyster(survey_spring, "ostrea_edulis")
#' summer <- predict_oyster(survey_summer, "ostrea_edulis")
#' autumn <- predict_oyster(survey_autumn, "ostrea_edulis")
#'
#' composite <- composite_seasonal(
#'   surveys = list(spring = spring, summer = summer, autumn = autumn),
#'   method  = "min"
#' )
#'
#' generate_report(composite, "composite_report.html",
#'                 title = "Year-round Suitability Assessment")
#' }
composite_seasonal <- function(surveys,
                                method          = c("min", "mean", "prob"),
                                prob_threshold  = 0.5,
                                cell_deg        = 0.001,
                                verbose         = TRUE) {

  method <- match.arg(method)

  if (length(surveys) < 2)
    cli::cli_abort("Supply at least 2 surveys in the list.")

  season_names <- names(surveys)
  if (is.null(season_names) || any(season_names == ""))
    cli::cli_abort("All list elements must be named (e.g. list(spring = r1, summer = r2)).")

  required_cols <- c("lat", "lon", "suitability")
  for (nm in season_names) {
    missing <- setdiff(required_cols, names(surveys[[nm]]))
    if (length(missing) > 0)
      cli::cli_abort("Survey '{nm}' is missing columns: {paste(missing, collapse=', ')}.")
  }

  # \u2500\u2500 Round all coordinates to grid resolution \u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500
  round_coord <- function(x) round(x / cell_deg) * cell_deg

  grids <- lapply(season_names, function(nm) {
    df <- surveys[[nm]]
    data.frame(
      lat_r = round_coord(df$lat),
      lon_r = round_coord(df$lon),
      suit  = df$suitability,
      stringsAsFactors = FALSE
    )
  })
  names(grids) <- season_names

  # \u2500\u2500 Build union of all cells \u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500
  all_cells <- unique(do.call(rbind, lapply(grids, function(g) g[, c("lat_r","lon_r")])))
  all_cells <- all_cells[order(all_cells$lat_r, all_cells$lon_r), ]

  n_cells   <- nrow(all_cells)
  suit_mat  <- matrix(NA_real_, nrow = n_cells, ncol = length(season_names))
  colnames(suit_mat) <- season_names

  for (j in seq_along(season_names)) {
    g   <- grids[[season_names[j]]]
    key_g    <- paste(g$lat_r, g$lon_r, sep = "_")
    key_all  <- paste(all_cells$lat_r, all_cells$lon_r, sep = "_")
    idx      <- match(key_all, key_g)
    suit_mat[, j] <- g$suit[idx]   # NA where cell absent in this season
  }

  # \u2500\u2500 Compute composite \u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500
  composite_suit <- switch(method,
    min  = apply(suit_mat, 1, min,  na.rm = FALSE),  # NA if any season missing
    mean = apply(suit_mat, 1, mean, na.rm = TRUE),
    prob = apply(suit_mat, 1, function(r) {
      present <- r[!is.na(r)]
      if (length(present) == 0) return(NA_real_)
      mean(present >= prob_threshold)
    })
  )

  # For min method, allow partial coverage with a warning
  n_present <- apply(suit_mat, 1, function(r) sum(!is.na(r)))
  if (method == "min") {
    n_incomplete <- sum(n_present < length(season_names))
    if (n_incomplete > 0) {
      cli::cli_warn(c(
        "!" = paste0("{n_incomplete} cell{?s} missing data in at least one season."),
        "i" = "These cells have composite suitability = NA (method = 'min').",
        "i" = "Use method = 'mean' or 'prob' to include partially covered cells."
      ))
    }
  }

  composite_suit <- pmin(1, pmax(0, composite_suit))

  suit_class <- cut(
    composite_suit,
    breaks = c(-Inf, 0.3, 0.5, 0.7, Inf),
    labels = c("Low", "Low", "Moderate", "High"),
    right  = TRUE
  )
  suit_class <- as.character(suit_class)
  suit_class[composite_suit < 0.3] <- "Very Low"
  suit_class[is.na(composite_suit)] <- "Excluded"

  out <- data.frame(
    lat                  = all_cells$lat_r,
    lon                  = all_cells$lon_r,
    suitability          = composite_suit,
    suitability_class    = suit_class,
    n_seasons_present    = n_present,
    suit_range           = apply(suit_mat, 1, function(r) {
      present <- r[!is.na(r)]
      if (length(present) < 2) return(NA_real_)
      max(present) - min(present)
    }),
    stringsAsFactors = FALSE
  )

  # Add per-season columns
  for (nm in season_names)
    out[[paste0("suit_", nm)]] <- suit_mat[, nm]

  if (verbose) {
    cli::cli_h2("Seasonal Composite \u2014 {method} method")
    cli::cli_inform(c(
      " " = "Seasons: {paste(season_names, collapse=', ')}",
      " " = "Cells: {n_cells} total | {sum(!is.na(composite_suit))} scored",
      " " = "High: {sum(suit_class=='High',na.rm=TRUE)} | Moderate: {sum(suit_class=='Moderate',na.rm=TRUE)} | Low/Very Low: {sum(suit_class %in% c('Low','Very Low'),na.rm=TRUE)}",
      " " = paste0("Mean composite suitability: ",
                   round(mean(composite_suit, na.rm = TRUE), 3))
    ))
    mean_range <- round(mean(out$suit_range, na.rm = TRUE), 3)
    cli::cli_inform(c(
      "i" = "Mean seasonal range: {mean_range} (0 = stable, 1 = highly variable)"
    ))
  }

  out
}
