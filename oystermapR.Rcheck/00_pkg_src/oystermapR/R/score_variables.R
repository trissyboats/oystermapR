#' @importFrom rlang `%||%`
#' @importFrom utils read.csv
NULL

#' Score a single numeric variable against an optimal-range definition
#'
#' @param x Numeric value to score.
#' @param params Named list of scoring parameters.
#' @return Numeric in [0, 1]. Returns `NA` if `x` is `NA`.
#' @keywords internal
.score_numeric <- function(x, params) {
  if (is.na(x)) return(NA_real_)

  type <- params$type

  if (type %in% c("optimal_range", "seasonal")) {
    opt_min  <- params$optimal_min
    opt_max  <- params$optimal_max
    lo       <- params$poor_min       %||% params$acceptable_min  %||% (opt_min - (opt_max - opt_min))
    hi       <- params$acceptable_max %||% params$poor_max        %||% (opt_max + (opt_max - opt_min))
    abs_hi   <- params$absolute_max   %||% hi

    if (x < lo || x > abs_hi) return(0)
    if (x >= opt_min && x <= opt_max) return(1)
    if (x < opt_min) return(max(0, (x - lo)     / (opt_min - lo)))
    return(          max(0, (abs_hi - x) / (abs_hi - opt_max)))

  } else if (type == "threshold_decay") {
    opt_max  <- params$optimal_max
    hard_max <- params$hard_max %||% params$poor_max %||% (opt_max * 2)
    min_val  <- params$min_for_food %||% params$optimal_min %||% NULL

    if (!is.null(min_val) && x < min_val) {
      floor_val <- params$poor_min %||% 0
      return(max(0, (x - floor_val) / (min_val - floor_val) * 0.5))
    }
    if (x <= opt_max) return(1)
    if (x >= hard_max) return(0)
    return(max(0, 1 - (x - opt_max) / (hard_max - opt_max)))
  }

  NA_real_
}


#' Score a categorical variable
#' @keywords internal
.score_categorical <- function(x, params) {
  if (is.na(x) || x == "") return(params$scores[["unknown"]] %||% 0.5)
  x_lower   <- tolower(trimws(x))
  score_map <- setNames(params$scores, tolower(names(params$scores)))
  if (x_lower %in% names(score_map)) return(unname(score_map[x_lower]))
  match_idx <- grep(x_lower, names(score_map), fixed = TRUE)
  if (length(match_idx) > 0) return(unname(score_map[match_idx[1]]))
  score_map[["unknown"]] %||% 0.5
}


#' Score all weighted factors for a single row (season-aware)
#'
#' @description
#' Computes a weighted mean suitability score. Variables of type `"seasonal"`
#' have their parameters swapped from `tolerances$seasonal_overrides` when a
#' `season` value is present in the row. Missing columns are skipped and their
#' weight is excluded from the denominator.
#'
#' @param row Named list or single-row dataframe slice.
#' @param tolerances Species tolerance list from [get_species_tolerances()].
#' @return List: `score`, `variable_scores`, `variable_weights`, `limiting_factors`.
#' @keywords internal
.score_row <- function(row, tolerances) {

  scored   <- tolerances$scored
  col_lwr  <- tolower(names(row))

  # Season detected for this row (NA if not available)
  season <- NULL
  s_col  <- names(row)[col_lwr %in% "season"]
  if (length(s_col) > 0 && !is.na(row[[s_col[1]]])) {
    season <- tolower(as.character(row[[s_col[1]]]))
  }

  # Seasonal overrides lookup
  seasonal_overrides <- tolerances$seasonal_overrides %||% list()

  # Column aliases
  col_aliases <- list(
    temperature         = c("temperature", "temp", "temp_c", "water_temp"),
    fishing_intensity   = c("fishing_intensity", "fishing", "fishing_observed"),
    shear_stress        = c("shear_stress", "tau", "bed_shear", "shear"),
    chlorophyll_a       = c("chlorophyll_a", "chla", "chl_a", "chlorophyll"),
    sediment_type       = c("sediment_type", "sediment", "substrate_type"),
    substrate_hardness  = c("substrate_hardness", "hardness", "bottom_hardness"),
    benthic_communities = c("benthic_communities", "benthic", "community"),
    depth               = c("depth", "depth_m"),
    biotope             = c("biotope", "biotopes", "habitat"),
    roughness           = c("roughness", "rugosity"),
    slope               = c("slope", "slope_deg"),
    turbidity           = c("turbidity", "ntu", "turb"),
    current_velocity    = c("current_velocity", "velocity", "current", "u_mean")
  )

  var_scores  <- numeric(0)
  var_weights <- numeric(0)

  for (var_name in names(scored)) {
    params <- scored[[var_name]]
    rank   <- params$rank

    # --- Find the data column --------------------------------------------------
    candidates <- col_aliases[[var_name]] %||% var_name
    col_match  <- names(row)[col_lwr %in% tolower(candidates)]
    if (length(col_match) == 0) next

    val <- row[[col_match[1]]]

    # --- Apply seasonal override if available ----------------------------------
    if (!is.null(season) && params$type == "seasonal") {
      season_params <- seasonal_overrides[[var_name]][[season]]
      if (!is.null(season_params)) params <- season_params
    }

    # --- fishing_intensity: binary penalty ------------------------------------
    if (var_name == "fishing_intensity") {
      depth_col <- names(row)[col_lwr %in% c("depth", "depth_m")]
      depth_val <- if (length(depth_col) > 0) row[[depth_col[1]]] else NA
      fishing_obs <- isTRUE(as.logical(val))
      trawl_max   <- params$trawl_depth_max

      raw_score <- if (fishing_obs && !is.na(depth_val) && depth_val <= trawl_max) {
        1 - params$penalty
      } else if (fishing_obs) {
        1 - (params$penalty / 2)
      } else {
        1.0
      }
      var_scores[var_name]  <- raw_score
      var_weights[var_name] <- rank
      next
    }

    # --- chlorophyll_a: only scored above feeding temperature -----------------
    if (var_name == "chlorophyll_a") {
      temp_col <- names(row)[col_lwr %in% c("temperature", "temp", "temp_c")]
      temp_val <- if (length(temp_col) > 0) row[[temp_col[1]]] else NA
      if (!is.na(temp_val) && !is.null(params$temp_threshold) &&
          temp_val <= params$temp_threshold) next
    }

    # --- M. gigas intertidal bonus: intertidal cells score depth = 1.0 --------
    # Pacific oyster is strongly intertidal in NW Europe; the depth scoring
    # curve (0-15 m optimal) would otherwise undervalue shallow/intertidal sites.
    if (var_name == "depth") {
      zone_col <- names(row)[tolower(names(row)) %in% "intertidal_zone"]
      if (length(zone_col) > 0 && !is.na(row[[zone_col[1]]])) {
        zone_val <- tolower(as.character(row[[zone_col[1]]]))
        # Only apply the bonus for species that explicitly favour intertidal
        # habitat (currently magallana_gigas). Identified by lower optimal depth.
        is_intertidal_species <- !is.null(params$optimal_max) &&
                                 !is.na(params$optimal_max) &&
                                 params$optimal_max <= 20   # edulis = 30, gigas = 15
        if (isTRUE(is_intertidal_species) && zone_val == "intertidal") {
          var_scores[var_name]  <- 1.0
          var_weights[var_name] <- rank
          next
        }
        # Supratidal = excluded from scoring (physically can't support oysters)
        if (zone_val == "supratidal") next
      }
    }

    # --- Categorical vs numeric -----------------------------------------------
    raw_score <- if (params$type == "categorical") {
      .score_categorical(as.character(val), params)
    } else {
      .score_numeric(as.numeric(val), params)
    }

    if (!is.na(raw_score)) {
      var_scores[var_name]  <- raw_score
      var_weights[var_name] <- rank
    }
  }

  # --- Data completeness: fraction of tolerance-spec variables that were scored
  # n_defined = number of variables in the tolerance spec
  # n_scored  = number that were actually present AND contributed a non-NA score
  n_defined   <- length(scored)
  n_scored    <- length(var_scores)
  completeness <- if (n_defined > 0) round(n_scored / n_defined, 3) else NA_real_

  if (length(var_scores) == 0) {
    return(list(score            = NA_real_,
                variable_scores  = var_scores,
                variable_weights = var_weights,
                limiting_factors = NA_character_,
                data_completeness = completeness))
  }

  # Weights: rank 1 = highest weight (1/rank, normalised)
  raw_weights  <- 1 / var_weights
  norm_weights <- raw_weights / sum(raw_weights)
  final_score  <- sum(var_scores * norm_weights)

  # --- Limiting factors: weighted-lowest-scoring variables -------------------
  # A variable is "limiting" if its score is meaningfully below the overall
  # score. Weight by importance so a rank-1 variable at 0.4 matters more than
  # a rank-6 variable at 0.3. Report top 3 worst weighted contributions.
  weighted_contributions <- var_scores * norm_weights
  sorted_idx <- order(weighted_contributions)
  top_limit  <- sorted_idx[seq_len(min(3, length(sorted_idx)))]
  # Only flag variables that are actually dragging the score (score < 0.65)
  top_limit  <- top_limit[var_scores[top_limit] < 0.65]
  limiting   <- if (length(top_limit) > 0) names(var_scores)[top_limit] else NA_character_

  list(
    score             = round(final_score, 4),
    variable_scores   = round(var_scores, 4),
    variable_weights  = round(norm_weights, 4),
    limiting_factors  = limiting,
    data_completeness = completeness
  )
}


#' Score all locations in a dataframe
#'
#' @description
#' Applies [.score_row()] across every non-excluded row. Excluded rows
#' receive `suitability = 0`. Adds per-variable score columns and a
#' `limiting_factors` column identifying the variables most constraining
#' the score at each location.
#'
#' @param df A dataframe processed by [check_exclusions()].
#' @param tolerances Species tolerance list from [get_species_tolerances()].
#' @param verbose Logical. Print per-variable scoring summary (default `FALSE`).
#'
#' @return The input dataframe with additional columns:
#'   - `suitability`: weighted score [0, 1]
#'   - `suitability_class`: "High" / "Moderate" / "Low" / "Very Low" / "Excluded"
#'   - `limiting_factors`: comma-separated names of the variables most limiting
#'     the score (NA if all variables score \u2265 0.65)
#'   - `score_<variable>`: one column per scored variable present in the data
#'
#' @export
score_locations <- function(df, tolerances, verbose = FALSE) {

  results <- vector("list", nrow(df))

  for (i in seq_len(nrow(df))) {
    if (isTRUE(df$excluded[i])) {
      results[[i]] <- list(score = 0, variable_scores = numeric(0),
                           variable_weights = numeric(0),
                           limiting_factors = NA_character_,
                           data_completeness = NA_real_)
    } else {
      results[[i]] <- .score_row(as.list(df[i, ]), tolerances)
    }
  }

  df$suitability <- vapply(results, `[[`, numeric(1), "score")

  df$suitability_class <- dplyr::case_when(
    df$excluded             ~ "Excluded",
    df$suitability >= 0.70  ~ "High",
    df$suitability >= 0.45  ~ "Moderate",
    df$suitability >= 0.20  ~ "Low",
    TRUE                    ~ "Very Low"
  )

  # Data completeness: fraction of tolerance-spec variables that had real data
  df$data_completeness <- vapply(results, function(r) {
    r$data_completeness %||% NA_real_
  }, numeric(1))

  # Limiting factors column
  df$limiting_factors <- vapply(results, function(r) {
    lf <- r$limiting_factors
    lf <- lf[!is.na(lf)]
    if (length(lf) == 0) NA_character_ else paste(lf, collapse = ", ")
  }, character(1))

  # Per-variable score columns
  all_var_names <- unique(unlist(lapply(results, function(r) names(r$variable_scores))))

  for (vn in all_var_names) {
    col_name <- paste0("score_", vn)
    df[[col_name]] <- vapply(results, function(r) {
      sc <- r$variable_scores
      if (vn %in% names(sc)) sc[[vn]] else NA_real_
    }, numeric(1))
  }

  if (verbose) .print_scoring_summary(df, all_var_names)

  df
}


#' @keywords internal
.print_scoring_summary <- function(df, var_names) {
  cli::cli_h2("Scoring Summary")
  cli::cli_inform(c(
    "i" = "Total: {nrow(df)} | Excluded: {sum(df$excluded, na.rm=TRUE)} | High: {sum(df$suitability_class=='High',na.rm=TRUE)} | Moderate: {sum(df$suitability_class=='Moderate',na.rm=TRUE)} | Low: {sum(df$suitability_class=='Low',na.rm=TRUE)} | Very Low: {sum(df$suitability_class=='Very Low',na.rm=TRUE)}"
  ))
  if (length(var_names) > 0) {
    cli::cli_h3("Mean score per variable (non-excluded rows)")
    non_excl <- df[!df$excluded, ]
    for (vn in var_names) {
      col_nm <- paste0("score_", vn)
      if (col_nm %in% names(non_excl)) {
        mn <- mean(non_excl[[col_nm]], na.rm = TRUE)
        cli::cli_inform("  {vn}: {round(mn, 3)}")
      }
    }
  }
}


# =============================================================================
# Uncertainty quantification via bootstrap
# =============================================================================

#' Add bootstrap confidence intervals to suitability scores
#'
#' @description
#' Re-scores each row of a survey dataframe `n_boot` times, each time adding
#' Gaussian noise to every numeric variable proportional to its measurement
#' uncertainty, and returns the 5th and 95th percentile suitability scores as
#' `suit_ci_lower` and `suit_ci_upper` columns (90% interval).
#'
#' Measurement uncertainty is specified via the `uncertainty` argument: a named
#' list where each name matches a variable name in `tolerances$scored_factors`
#' and the value is the 1-sigma absolute error (same units as the variable).
#' Any variable not listed defaults to 2% of its observed value.
#'
#' @param result Dataframe returned by [predict_oyster()].
#' @param tolerances Species tolerance list from [get_species_tolerances()].
#' @param n_boot Integer. Number of bootstrap resamples (default 200).
#' @param uncertainty Named numeric vector. 1-sigma measurement errors by
#'   variable name (e.g. `c(temperature = 0.2, salinity = 0.5)`).
#' @param seed Integer or NULL. Random seed for reproducibility (default 42).
#' @param verbose Logical. Print progress (default FALSE).
#'
#' @return The input dataframe with additional columns:
#'   `suit_ci_lower`, `suit_ci_upper`, `suit_ci_width`.
#'
#' @export
#' @examples
#' \dontrun{
#' result <- predict_oyster(survey, "ostrea_edulis")
#' tol    <- get_species_tolerances("ostrea_edulis")
#' result <- add_suitability_ci(result, tol,
#'                              uncertainty = c(temperature = 0.3,
#'                                              salinity    = 0.5))
#' # Columns suit_ci_lower, suit_ci_upper, suit_ci_width now present
#' generate_report(result, "report.html")   # map rings scale with CI width
#' }
add_suitability_ci <- function(result,
                                tolerances,
                                n_boot      = 200L,
                                uncertainty = NULL,
                                seed        = 42L,
                                verbose     = FALSE) {

  if (!is.null(seed)) set.seed(seed)

  sf   <- tolerances$scored_factors
  vars <- names(sf)

  # Identify numeric columns that correspond to scored variables
  numeric_vars <- vars[vapply(vars, function(v) {
    v %in% names(result) && is.numeric(result[[v]])
  }, logical(1))]

  if (length(numeric_vars) == 0) {
    cli::cli_warn("No numeric scored-variable columns found in result. CI not computed.")
    result$suit_ci_lower <- result$suitability
    result$suit_ci_upper <- result$suitability
    result$suit_ci_width <- 0
    return(result)
  }

  # Default uncertainty: 2% of column mean for each variable
  sigma <- setNames(
    vapply(numeric_vars, function(v) {
      if (!is.null(uncertainty) && v %in% names(uncertainty))
        uncertainty[[v]]
      else
        max(0.001, 0.02 * mean(result[[v]], na.rm = TRUE))
    }, numeric(1)),
    numeric_vars
  )

  if (verbose) {
    cli::cli_inform("Bootstrap CI: {n_boot} resamples over {length(numeric_vars)} variable{?s}.")
    for (v in numeric_vars)
      cli::cli_inform("  {v}: \u00b1{round(sigma[v], 4)}")
  }

  n_rows   <- nrow(result)
  boot_mat <- matrix(NA_real_, nrow = n_rows, ncol = n_boot)

  for (b in seq_len(n_boot)) {
    perturbed <- result
    for (v in numeric_vars) {
      noise <- stats::rnorm(n_rows, mean = 0, sd = sigma[v])
      perturbed[[v]] <- perturbed[[v]] + noise
    }
    scored_b <- score_locations(perturbed, tolerances, verbose = FALSE)
    boot_mat[, b] <- scored_b$suitability
  }

  result$suit_ci_lower <- apply(boot_mat, 1, stats::quantile, probs = 0.05, na.rm = TRUE)
  result$suit_ci_upper <- apply(boot_mat, 1, stats::quantile, probs = 0.95, na.rm = TRUE)
  result$suit_ci_width <- result$suit_ci_upper - result$suit_ci_lower

  if (verbose) {
    cli::cli_inform(c(
      "\u2713" = "CI complete.",
      " " = paste0("Mean 90% width: ",
                   round(mean(result$suit_ci_width, na.rm = TRUE), 3))
    ))
  }

  result
}
