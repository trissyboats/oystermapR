# =============================================================================
# Multi-survey comparison
# =============================================================================

#' Compare suitability scores across multiple surveys or monitoring years
#'
#' @description
#' Takes a named list of `predict_oyster()` result dataframes and produces a
#' single comparative summary suitable for trend monitoring, multi-site
#' selection, or regulatory reporting. Common spatial cells are matched across
#' surveys and a change analysis is run between consecutive surveys if ordered
#' chronologically.
#'
#' The function returns a list with three elements:
#' - `summary` — one row per survey with mean suitability, class breakdown,
#'   and data completeness stats.
#' - `spatial` — all surveys joined on shared lat/lon cells, with suitability
#'   columns per survey and `trend_slope` (linear regression of suitability
#'   over survey index for each cell).
#' - `change` — pairwise change table (only if surveys are in order; each
#'   consecutive pair shows mean score difference and fraction of cells
#'   that improved, degraded, or were stable).
#'
#' @param surveys Named list of dataframes from [predict_oyster()]. Names
#'   should be meaningful labels (e.g. survey year or site name).
#' @param cell_deg Numeric. Spatial rounding tolerance in decimal degrees for
#'   matching cells across surveys (default 0.001 approx. 111 m).
#' @param stable_threshold Numeric. Absolute suitability change below which a
#'   cell is considered "stable" rather than improved/degraded (default 0.05).
#' @param verbose Logical. Print comparison summary (default TRUE).
#'
#' @return Named list: `summary` (dataframe), `spatial` (dataframe),
#'   `change` (dataframe or NULL if only one survey).
#'
#' @export
#' @examples
#' \dontrun{
#' r2022 <- predict_oyster(survey_2022, "ostrea_edulis")
#' r2023 <- predict_oyster(survey_2023, "ostrea_edulis")
#' r2024 <- predict_oyster(survey_2024, "ostrea_edulis")
#'
#' comp <- compare_surveys(
#'   surveys = list("2022" = r2022, "2023" = r2023, "2024" = r2024)
#' )
#'
#' comp$summary        # mean scores by year
#' comp$change         # year-on-year change
#'
#' # Pass to generate_report() for a full comparative HTML report
#' generate_report(r2024, "monitoring_report.html",
#'                 title = "Kames Bay 3-Year Monitoring")
#' }
compare_surveys <- function(surveys,
                             cell_deg          = 0.001,
                             stable_threshold  = 0.05,
                             verbose           = TRUE) {

  if (length(surveys) < 1)
    cli::cli_abort("Supply at least one survey.")

  survey_names <- names(surveys)
  if (is.null(survey_names) || any(survey_names == ""))
    cli::cli_abort("All list elements must be named.")

  round_coord <- function(x) round(x / cell_deg) * cell_deg

  # \u2500\u2500 Per-survey summary \u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500
  summary_rows <- lapply(survey_names, function(nm) {
    df <- surveys[[nm]]
    if (!"excluded" %in% names(df)) df$excluded <- FALSE
    df$excluded <- as.logical(df$excluded)
    df$excluded[is.na(df$excluded)] <- FALSE

    scored <- df[!df$excluded, ]
    n      <- nrow(df)
    n_sc   <- nrow(scored)

    data.frame(
      survey          = nm,
      n_locations     = n,
      n_scored        = n_sc,
      n_excluded      = n - n_sc,
      mean_suitability = round(mean(scored$suitability, na.rm = TRUE), 4),
      sd_suitability   = round(stats::sd(scored$suitability,   na.rm = TRUE), 4),
      pct_high         = round(100 * mean(scored$suitability_class == "High",     na.rm = TRUE), 1),
      pct_moderate     = round(100 * mean(scored$suitability_class == "Moderate", na.rm = TRUE), 1),
      pct_low          = round(100 * mean(scored$suitability_class %in% c("Low","Very Low"), na.rm = TRUE), 1),
      mean_completeness = if ("data_completeness" %in% names(df))
        round(mean(df$data_completeness, na.rm = TRUE), 3) else NA_real_,
      stringsAsFactors = FALSE
    )
  })
  summary_df <- do.call(rbind, summary_rows)

  # \u2500\u2500 Spatial join on shared cells \u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500
  grids <- lapply(survey_names, function(nm) {
    df <- surveys[[nm]]
    data.frame(
      lat_r = round_coord(df$lat),
      lon_r = round_coord(df$lon),
      suit  = df$suitability,
      stringsAsFactors = FALSE
    )
  })
  names(grids) <- survey_names

  all_cells <- unique(do.call(rbind, lapply(grids, function(g) g[, c("lat_r","lon_r")])))
  all_cells <- all_cells[order(all_cells$lat_r, all_cells$lon_r), ]

  suit_mat <- matrix(NA_real_, nrow = nrow(all_cells), ncol = length(survey_names))
  colnames(suit_mat) <- survey_names

  for (j in seq_along(survey_names)) {
    g        <- grids[[survey_names[j]]]
    key_g    <- paste(g$lat_r, g$lon_r, sep = "_")
    key_all  <- paste(all_cells$lat_r, all_cells$lon_r, sep = "_")
    suit_mat[, j] <- g$suit[match(key_all, key_g)]
  }

  # Trend slope: linear regression of suitability over survey index per cell
  trend_slope <- apply(suit_mat, 1, function(r) {
    idx <- which(!is.na(r))
    if (length(idx) < 2) return(NA_real_)
    stats::coef(stats::lm(r[idx] ~ idx))[2]
  })

  spatial_df <- data.frame(
    lat         = all_cells$lat_r,
    lon         = all_cells$lon_r,
    n_surveys   = apply(suit_mat, 1, function(r) sum(!is.na(r))),
    mean_suit   = apply(suit_mat, 1, mean, na.rm = TRUE),
    trend_slope = round(trend_slope, 5),
    stringsAsFactors = FALSE
  )
  for (nm in survey_names)
    spatial_df[[paste0("suit_", nm)]] <- suit_mat[, nm]

  # \u2500\u2500 Pairwise change analysis \u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500
  change_df <- NULL
  if (length(survey_names) >= 2) {
    change_rows <- lapply(seq_len(length(survey_names) - 1), function(i) {
      a <- suit_mat[, i]
      b <- suit_mat[, i + 1]
      both <- !is.na(a) & !is.na(b)
      delta <- b[both] - a[both]
      data.frame(
        from          = survey_names[i],
        to            = survey_names[i + 1],
        n_shared_cells = sum(both),
        mean_change   = round(mean(delta), 4),
        pct_improved  = round(100 * mean(delta >  stable_threshold), 1),
        pct_stable    = round(100 * mean(abs(delta) <= stable_threshold), 1),
        pct_degraded  = round(100 * mean(delta < -stable_threshold), 1),
        stringsAsFactors = FALSE
      )
    })
    change_df <- do.call(rbind, change_rows)
  }

  if (verbose) {
    cli::cli_h2("Survey Comparison \u2014 {length(survey_names)} survey{?s}")
    for (i in seq_len(nrow(summary_df))) {
      r <- summary_df[i, ]
      cli::cli_inform(paste0(
        r$survey, ": mean=", r$mean_suitability,
        " | High=", r$pct_high, "% | Mod=", r$pct_moderate,
        "% | n=", r$n_scored, " scored"
      ))
    }
    if (!is.null(change_df)) {
      cli::cli_h3("Change Analysis")
      for (i in seq_len(nrow(change_df))) {
        r <- change_df[i, ]
        cli::cli_inform(paste0(
          r$from, " \u2192 ", r$to,
          ": mean \u0394=", r$mean_change,
          " | improved=", r$pct_improved,
          "% | stable=", r$pct_stable,
          "% | degraded=", r$pct_degraded, "%"
        ))
      }
    }
  }

  list(summary = summary_df, spatial = spatial_df, change = change_df)
}
