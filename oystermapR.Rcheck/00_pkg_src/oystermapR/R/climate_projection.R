# =============================================================================
# Climate change suitability projection
# =============================================================================
#
# References:
#  UKCP18 Marine projections: Met Office (2018) \u2014 SST/salinity deltas
#  IPCC AR6 WG1 (2021): SSP scenario temperature trajectories
#  Lemasson et al. (2017): Climate change impacts on bivalve aquaculture in UK
#  Thomas et al. (2016): O. edulis temperature sensitivity in climate scenarios

#' Project suitability under future climate scenarios
#'
#' @description
#' Re-scores survey locations under user-specified climate perturbations to
#' assess how habitat suitability may change under warming and related
#' oceanographic shifts. Perturbations are applied as additive deltas to
#' temperature and salinity, and multiplicative factors to other variables.
#'
#' Built-in scenarios align with UKCP18 marine projections for NW European
#' shelf seas:
#'
#' | Scenario    | SST delta | Salinity delta | Notes                        |
#' |-------------|-----------|----------------|------------------------------|
#' | RCP2.6/SSP1 | +0.5\u00b0C    | 0.0 PSU        | Low emissions, best case     |
#' | RCP4.5/SSP2 | +1.2\u00b0C    | -0.2 PSU       | Intermediate emissions       |
#' | RCP8.5/SSP5 | +2.5\u00b0C    | -0.5 PSU       | High emissions, worst case   |
#' | Custom      | user      | user           | Supply your own deltas       |
#'
#' The function runs `score_locations()` for each scenario and returns a list
#' with one result dataframe per scenario, plus a comparison summary and a
#' `suitability_delta` column (projected minus baseline) for each.
#'
#' @param result Dataframe from [predict_oyster()] (the baseline).
#' @param tolerances Species tolerance list from [get_species_tolerances()].
#' @param scenarios Character vector of built-in scenario names, or NULL to
#'   use `custom_deltas` only. Built-in: `"rcp26"`, `"rcp45"`, `"rcp85"`.
#'   Default: all three.
#' @param custom_deltas Named list of custom scenarios. Each element is a
#'   named numeric vector of deltas keyed to column names, e.g.
#'   `list(my_scenario = c(temperature = 1.5, salinity = -0.3))`.
#' @param horizon Character. Label for the time horizon, used in output names
#'   only (default `"2050s"`).
#' @param verbose Logical. Print scenario comparison table (default TRUE).
#'
#' @return Named list:
#'   - One dataframe per scenario (named by scenario key) with `suitability`,
#'     `suitability_class`, and `suitability_delta` columns.
#'   - `"summary"` \u2014 dataframe comparing mean suitability, class breakdown,
#'     and net change vs baseline across all scenarios.
#'
#' @export
#' @examples
#' \dontrun{
#' result <- predict_oyster(survey, "ostrea_edulis")
#' tol    <- get_species_tolerances("ostrea_edulis")
#'
#' # Project under all three UKCP18-aligned scenarios
#' proj <- project_suitability(result, tol)
#'
#' # Mean suitability change by scenario
#' proj$summary
#'
#' # High-risk cells: currently High but drops to Low under RCP8.5
#' vulnerable <- subset(proj$rcp85,
#'   result$suitability_class == "High" & suitability_class == "Low")
#'
#' # Custom scenario (e.g. local downscaled projection)
#' proj2 <- project_suitability(result, tol, scenarios = NULL,
#'   custom_deltas = list(
#'     ukcp18_p95 = c(temperature = 3.2, salinity = -0.8)))
#' }
project_suitability <- function(result,
                                 tolerances,
                                 scenarios      = c("rcp26", "rcp45", "rcp85"),
                                 custom_deltas  = NULL,
                                 horizon        = "2050s",
                                 verbose        = TRUE) {

  # \u2500\u2500 Built-in UKCP18-aligned scenario deltas \u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500
  .builtin <- list(
    rcp26 = list(
      label       = "RCP2.6 / SSP1",
      description = "Low emissions (+0.5\u00b0C SST, 2050s)",
      deltas      = c(temperature = 0.5, salinity = 0.0)
    ),
    rcp45 = list(
      label       = "RCP4.5 / SSP2",
      description = "Intermediate emissions (+1.2\u00b0C SST, 2050s)",
      deltas      = c(temperature = 1.2, salinity = -0.2)
    ),
    rcp85 = list(
      label       = "RCP8.5 / SSP5",
      description = "High emissions (+2.5\u00b0C SST, 2050s)",
      deltas      = c(temperature = 2.5, salinity = -0.5)
    )
  )

  # Compile scenario list
  all_scenarios <- list()
  for (nm in scenarios) {
    if (!nm %in% names(.builtin))
      cli::cli_abort("Unknown built-in scenario: {.val {nm}}. Choose from: rcp26, rcp45, rcp85.")
    all_scenarios[[nm]] <- .builtin[[nm]]
  }
  if (!is.null(custom_deltas)) {
    for (nm in names(custom_deltas)) {
      all_scenarios[[nm]] <- list(
        label       = nm,
        description = paste0("Custom scenario: ", nm),
        deltas      = custom_deltas[[nm]]
      )
    }
  }

  if (length(all_scenarios) == 0)
    cli::cli_abort("No scenarios specified. Use 'scenarios' or 'custom_deltas'.")

  baseline_suit <- result$suitability

  outputs <- list()

  for (nm in names(all_scenarios)) {
    sc      <- all_scenarios[[nm]]
    deltas  <- sc$deltas
    perturb <- result

    # Apply additive deltas to matching columns
    for (col in names(deltas)) {
      if (col %in% names(perturb) && is.numeric(perturb[[col]])) {
        perturb[[col]] <- perturb[[col]] + deltas[[col]]
      }
    }

    # Re-score with perturbed data
    scored <- score_locations(perturb, tolerances, verbose = FALSE)

    scored$suitability_delta   <- round(scored$suitability - baseline_suit, 4)
    scored$scenario            <- nm
    scored$scenario_label      <- sc$label
    scored$scenario_description <- sc$description

    outputs[[nm]] <- scored
  }

  # \u2500\u2500 Comparison summary \u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500
  baseline_mean <- round(mean(baseline_suit, na.rm = TRUE), 4)

  summary_rows <- lapply(names(outputs), function(nm) {
    sc_df <- outputs[[nm]]
    data.frame(
      scenario        = nm,
      label           = all_scenarios[[nm]]$label,
      horizon         = horizon,
      mean_suit       = round(mean(sc_df$suitability,       na.rm = TRUE), 4),
      mean_delta      = round(mean(sc_df$suitability_delta, na.rm = TRUE), 4),
      pct_high        = round(100 * mean(sc_df$suitability_class == "High",     na.rm = TRUE), 1),
      pct_moderate    = round(100 * mean(sc_df$suitability_class == "Moderate", na.rm = TRUE), 1),
      pct_declining   = round(100 * mean(sc_df$suitability_delta < -0.05,       na.rm = TRUE), 1),
      pct_improving   = round(100 * mean(sc_df$suitability_delta >  0.05,       na.rm = TRUE), 1),
      stringsAsFactors = FALSE
    )
  })
  summary_df <- rbind(
    data.frame(scenario = "baseline", label = "Baseline (observed)",
               horizon = "current",
               mean_suit = baseline_mean, mean_delta = 0,
               pct_high = round(100 * mean(result$suitability_class == "High",     na.rm = TRUE), 1),
               pct_moderate = round(100 * mean(result$suitability_class == "Moderate", na.rm = TRUE), 1),
               pct_declining = 0, pct_improving = 0,
               stringsAsFactors = FALSE),
    do.call(rbind, summary_rows)
  )

  if (verbose) {
    cli::cli_h2("Climate Projection \u2014 {length(all_scenarios)} scenario{?s} ({horizon})")
    for (i in seq_len(nrow(summary_df))) {
      r <- summary_df[i, ]
      sign <- if (r$mean_delta > 0) "+" else ""
      cli::cli_inform(paste0(
        r$label, ": mean=", r$mean_suit,
        " (", sign, r$mean_delta, " vs baseline)",
        " | High=", r$pct_high, "%",
        " | declining=", r$pct_declining, "%",
        " | improving=", r$pct_improving, "%"
      ))
    }

    # Warn about large losses
    worst_loss <- min(sapply(outputs, function(o)
      mean(o$suitability_delta, na.rm = TRUE)))
    if (worst_loss < -0.1)
      cli::cli_warn(c(
        "!" = paste0("Worst-case scenario shows mean suitability loss of ",
                     round(abs(worst_loss), 3), "."),
        "i" = "Consider identifying climate-resilient sites (stable across all scenarios)."
      ))
  }

  outputs[["summary"]] <- summary_df
  outputs
}


#' Identify climate-resilient locations
#'
#' @description
#' From a [project_suitability()] output, identifies locations that maintain
#' at least `min_class` suitability across all projected scenarios. These are
#' the most robust sites for long-term restoration or aquaculture investment.
#'
#' @param projections Named list returned by [project_suitability()].
#' @param baseline Dataframe from [predict_oyster()] (the baseline result).
#' @param min_class Character. Minimum suitability class required in all
#'   scenarios. One of `"High"`, `"Moderate"`, `"Low"` (default `"Moderate"`).
#'
#' @return Subset of `baseline` for locations that meet `min_class` in every
#'   scenario, with an added `n_scenarios_qualifying` column.
#'
#' @export
#' @examples
#' \dontrun{
#' proj      <- project_suitability(result, tol)
#' resilient <- identify_resilient_sites(proj, result, min_class = "Moderate")
#' nrow(resilient)  # sites that stay Moderate+ even under RCP8.5
#' }
identify_resilient_sites <- function(projections,
                                      baseline,
                                      min_class = "Moderate") {

  class_order <- c("Very Low" = 1, "Low" = 2, "Moderate" = 3,
                   "High" = 4, "Excluded" = 0)
  min_val     <- class_order[[min_class]]

  scenario_keys <- setdiff(names(projections), "summary")

  qualify_mat <- do.call(cbind, lapply(scenario_keys, function(nm) {
    sc_class <- projections[[nm]]$suitability_class
    as.integer(class_order[sc_class] >= min_val)
  }))

  n_qualifying <- rowSums(qualify_mat, na.rm = TRUE)
  all_qualify  <- n_qualifying == length(scenario_keys)

  out <- baseline[all_qualify, ]
  out$n_scenarios_qualifying <- n_qualifying[all_qualify]

  cli::cli_inform(c(
    "i" = paste0(sum(all_qualify), " of ", nrow(baseline),
                 " locations maintain ", min_class,
                 "+ suitability across all ", length(scenario_keys),
                 " scenario{?s}.")
  ))

  out
}
