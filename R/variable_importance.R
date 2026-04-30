# =============================================================================
# Variable Importance and Sensitivity Analysis
# =============================================================================
#
# Two complementary diagnostics for published species distribution models:
#
# 1. PERMUTATION VARIABLE IMPORTANCE (Breiman 2001; extended to SDMs)
#    For each scored variable, randomly shuffle its values across all locations
#    and re-compute suitability. The drop in AUC relative to baseline indicates
#    how much the model relies on that variable. High drop = high importance.
#    This is model-agnostic and works with any scoring function.
#
# 2. PARTIAL DEPENDENCE / SENSITIVITY ANALYSIS
#    Vary one variable across its full biological range while holding all other
#    variables at their observed median (or a specified value). Shows the
#    marginal response curve \u2014 how suitability changes as a single variable
#    changes. Used for:
#     - Verifying that scored response curves match ecological expectations
#     - Communicating model behaviour to stakeholders and referees
#     - Checking for artefacts at parameter boundaries
#
# References:
#  Breiman (2001): Random Forests. Machine Learning 45:5-32.
#  Thuiller et al. (2009): BIOMOD \u2014 a platform for ensemble forecasting of
#   species distributions. Ecography 32:369-373.
#  Elith et al. (2008): A working guide to boosted regression trees. J Anim
#   Ecology 77:802-813.
#  Zurell et al. (2020): A standard protocol for reporting species distribution
#   models. Ecography 43:1261-1277.  [ODMAP protocol]

#' Permutation variable importance for suitability model
#'
#' @description
#' Estimates the importance of each scored environmental variable by randomly
#' permuting its values and measuring the resulting drop in AUC against known
#' presence/absence records. A large drop indicates the model strongly depends
#' on that variable; a small drop indicates it could be removed with minimal
#' impact.
#'
#' This approach (Breiman 2001) is model-agnostic, requires no model refit,
#' and is directly interpretable: "how much does AUC fall if I destroy the
#' information in variable X?"
#'
#' @param predicted Dataframe from [predict_oyster()] with `lat`, `lon`,
#'   `suitability`, and per-variable score columns (e.g. `score_temperature`,
#'   `score_salinity`). The function uses the per-variable component scores
#'   already computed by [predict_oyster()].
#' @param records Dataframe of presence/absence records (same format as
#'   [validate_against_records()]).
#' @param presence_col Character. Name of presence/absence column (default
#'   `"presence"`).
#' @param n_permutations Integer. Number of permutation replicates per variable
#'   (default 50). More = more stable importance estimates; >= 100 recommended
#'   for publication.
#' @param match_radius_deg Numeric. Matching radius (default 0.002°).
#' @param seed Integer. Random seed (default 42).
#' @param verbose Logical. Default TRUE.
#'
#' @return A dataframe (sorted by importance, descending) with columns:
#'   - `variable`: scored variable name
#'   - `baseline_auc`: AUC of unperturbed model
#'   - `mean_permuted_auc`: mean AUC after permuting this variable
#'   - `importance`: baseline_auc - mean_permuted_auc (drop in AUC)
#'   - `importance_sd`: SD of AUC drop across permutation replicates
#'   - `importance_pct`: importance as percentage of baseline AUC
#'   - `rank`: rank by importance (1 = most important)
#'
#' @section Interpretation:
#' - importance > 0.10 (> 10% AUC drop): high importance — variable is load-
#'   bearing for model discrimination.
#' - importance 0.02–0.10: moderate importance.
#' - importance < 0.02: low importance — consider whether this variable adds
#'   value relative to its data collection cost.
#'
#' @export
#' @references
#' Breiman (2001) Machine Learning 45:5-32.
#' Zurell et al. (2020) Ecography 43:1261-1277.
#'
#' @examples
#' \dontrun{
#' records <- read.csv("nbn_ostrea_edulis.csv")
#' result  <- predict_oyster(survey, "ostrea_edulis")
#'
#' imp <- permutation_importance(result, records, n_permutations = 100)
#' print(imp)
#' # Variable        Importance  Rank
#' # temperature     0.183       1
#' # salinity        0.091       2
#' # depth           0.045       3
#' # ...
#' }
permutation_importance <- function(predicted,
                                    records,
                                    presence_col     = "presence",
                                    n_permutations   = 50L,
                                    match_radius_deg = 0.002,
                                    seed             = 42L,
                                    verbose          = TRUE) {

  set.seed(seed)

  # \u2500\u2500 Input checks \u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500
  if (!"suitability" %in% names(predicted))
    cli::cli_abort("predicted must contain a 'suitability' column.")

  # Identify per-variable score columns (created by score_locations())
  score_cols <- grep("^score_", names(predicted), value = TRUE)
  if (length(score_cols) == 0)
    cli::cli_abort(c(
      "No 'score_*' columns found in predicted.",
      "i" = paste0("predict_oyster() stores per-variable scores as 'score_temperature', ",
                   "'score_salinity' etc. Ensure your result comes from predict_oyster().")
    ))

  # \u2500\u2500 Match records to prediction cells \u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500
  records <- .standardise_coords(records)
  if (!presence_col %in% names(records))
    cli::cli_abort("Column '{presence_col}' not found in records.")

  obs <- as.numeric(as.logical(records[[presence_col]]))
  keep <- !is.na(obs)
  records <- records[keep, ]; obs <- obs[keep]

  n_rec     <- nrow(records)
  pred_lat  <- predicted$lat
  pred_lon  <- predicted$lon

  match_idx <- vapply(seq_len(n_rec), function(i) {
    d2 <- (pred_lat - records$lat[i])^2 + (pred_lon - records$lon[i])^2
    j  <- which.min(d2)
    if (sqrt(d2[j]) <= match_radius_deg) j else NA_integer_
  }, integer(1))

  keep2    <- !is.na(match_idx)
  obs      <- obs[keep2]
  match_idx <- match_idx[keep2]

  if (length(obs) < 10)
    cli::cli_abort("Too few matched records ({length(obs)}) for importance estimation.")

  if (verbose) {
    cli::cli_h2("Permutation Variable Importance")
    cli::cli_inform(c(
      " " = "Records: {length(obs)} ({sum(obs)} presences, {sum(obs==0)} absences)",
      " " = "Variables: {length(score_cols)}",
      " " = "Permutations per variable: {n_permutations}"
    ))
  }

  # \u2500\u2500 Compute baseline suitability for matched records \u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500
  base_suit <- predicted$suitability[match_idx]

  # Internal AUC function (trapezoidal; no external packages)
  .auc_from_suit <- function(suit, obs_vec) {
    thrs <- sort(unique(c(0, suit, 1)), decreasing = TRUE)
    roc  <- lapply(thrs, function(thr) {
      pp   <- suit >= thr
      tp   <- sum( pp & obs_vec == 1)
      fp   <- sum( pp & obs_vec == 0)
      tn   <- sum(!pp & obs_vec == 0)
      fn   <- sum(!pp & obs_vec == 1)
      sens <- if ((tp+fn)>0) tp/(tp+fn) else 0
      spec <- if ((tn+fp)>0) tn/(tn+fp) else 0
      c(fpr = 1-spec, tpr = sens)
    })
    fpr <- sapply(roc, `[`, "fpr")
    tpr <- sapply(roc, `[`, "tpr")
    ord <- order(fpr)
    abs(sum(diff(fpr[ord]) * (tpr[ord[-1]] + tpr[ord[-length(ord)]]) / 2))
  }

  baseline_auc <- .auc_from_suit(base_suit, obs)
  if (verbose) cli::cli_inform("Baseline AUC: {round(baseline_auc, 4)}")

  # \u2500\u2500 Permutation loop \u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500
  # Weights for each score_col in the final suitability (from predict_oyster).
  # We need to know the AHP weight of each variable. Try to recover from
  # weight_ columns; if absent, assume equal weights.
  weight_cols <- gsub("^score_", "weight_", score_cols)
  has_weights <- all(weight_cols %in% names(predicted))

  importance_rows <- vector("list", length(score_cols))

  for (sc_i in seq_along(score_cols)) {
    sc_col <- score_cols[sc_i]
    var    <- sub("^score_", "", sc_col)

    # Score matrix: all cells \u00d7 all score columns (values at matched indices)
    # We reconstruct suitability with variable sc_col permuted
    drop_v <- numeric(n_permutations)

    for (rep_i in seq_len(n_permutations)) {
      # Permute the score column across ALL prediction cells, then re-index
      perm_order  <- sample(nrow(predicted))
      perm_scores <- predicted[[sc_col]][perm_order]

      # Recompute suitability: replace sc_col with permuted version
      if (has_weights) {
        # Weighted mean reconstruction
        w_col <- weight_cols[sc_i]
        w_vals <- predicted[[w_col]]

        # Compute numerator and denominator across all score columns
        # Numerator = sum of w_j * score_j; denominator = sum of valid w_j
        numerator   <- rep(0, nrow(predicted))
        denominator <- rep(0, nrow(predicted))

        for (sc_j in seq_along(score_cols)) {
          sc_col_j <- score_cols[sc_j]
          w_col_j  <- weight_cols[sc_j]
          s_j <- if (sc_j == sc_i) perm_scores else predicted[[sc_col_j]]
          w_j <- predicted[[w_col_j]]
          valid_j <- !is.na(s_j) & !is.na(w_j)
          numerator[valid_j]   <- numerator[valid_j]   + w_j[valid_j] * s_j[valid_j]
          denominator[valid_j] <- denominator[valid_j] + w_j[valid_j]
        }
        perm_suit_all <- ifelse(denominator > 0, numerator / denominator, 0)
      } else {
        # Equal-weight reconstruction
        score_mat <- do.call(cbind, lapply(seq_along(score_cols), function(sc_j) {
          if (sc_j == sc_i) perm_scores else predicted[[score_cols[sc_j]]]
        }))
        perm_suit_all <- rowMeans(score_mat, na.rm = TRUE)
      }

      perm_suit <- perm_suit_all[match_idx]
      perm_auc  <- .auc_from_suit(perm_suit, obs)
      drop_v[rep_i] <- baseline_auc - perm_auc
    }

    importance_rows[[sc_i]] <- data.frame(
      variable          = var,
      baseline_auc      = round(baseline_auc, 4),
      mean_permuted_auc = round(baseline_auc - mean(drop_v), 4),
      importance        = round(mean(drop_v),  4),
      importance_sd     = round(sd(drop_v),    4),
      importance_pct    = round(100 * mean(drop_v) / max(baseline_auc, 0.01), 2),
      stringsAsFactors  = FALSE
    )

    if (verbose)
      cli::cli_inform(
        "  {var}: importance = {round(mean(drop_v),4)} (\u00b1{round(sd(drop_v),4)})"
      )
  }

  result_df <- do.call(rbind, importance_rows)
  result_df <- result_df[order(result_df$importance, decreasing = TRUE), ]
  result_df$rank <- seq_len(nrow(result_df))
  rownames(result_df) <- NULL

  if (verbose) {
    cli::cli_h3("Ranked variable importance")
    cli::cli_inform(c(
      " " = "Top variable: '{result_df$variable[1]}' ({result_df$importance_pct[1]}% AUC drop)",
      "i" = paste0("Variables with < 2% AUC drop may be candidates for removal ",
                   "to reduce data collection burden.")
    ))
  }

  result_df
}


#' Partial dependence / sensitivity analysis for a scored variable
#'
#' @description
#' Computes the marginal response curve for a single environmental variable:
#' suitability is predicted at a grid of values for the focal variable, while
#' all other variables are held at their observed median (or a specified
#' background value). This reveals the shape of the scoring function as
#' actually applied to a real dataset.
#'
#' Partial dependence is a standard diagnostic for SDM publication (Elith et al.
#' 2008; Zurell et al. 2020 ODMAP protocol). Use it to verify that predicted
#' responses match ecological expectations before submission.
#'
#' @param predicted Dataframe from [predict_oyster()] containing `lat`, `lon`,
#'   `suitability`, and per-variable score/weight columns.
#' @param species Character. Species key (e.g. `"ostrea_edulis"`). Used to
#'   retrieve tolerance parameters for the scoring function.
#' @param variable Character. Name of the focal variable to vary (e.g.
#'   `"temperature"`, `"salinity"`, `"depth"`).
#' @param n_steps Integer. Number of evenly spaced values across the variable's
#'   biological range (default 100).
#' @param background Named list. Fixed values for all other variables. If NULL
#'   (default), uses column medians from `predicted`.
#' @param season Character or NULL. Season to apply for seasonal variables
#'   (e.g. `"summer"`). NULL = no seasonal override.
#' @param verbose Logical. Default TRUE.
#'
#' @return A dataframe with columns:
#'   - `x`: focal variable value
#'   - `suitability`: predicted suitability at that value
#'   - `variable`: focal variable name
#'   - `species`: species key
#'
#' Suitable for plotting with `ggplot2` or base R `plot()`.
#'
#' @export
#' @references
#' Elith et al. (2008) J Animal Ecology 77:802-813.
#' Zurell et al. (2020) Ecography 43:1261-1277.
#'
#' @examples
#' \dontrun{
#' result <- predict_oyster(survey, "ostrea_edulis")
#'
#' # Temperature response curve
#' temp_pd <- sensitivity_analysis(result, "ostrea_edulis", "temperature")
#' plot(temp_pd$x, temp_pd$suitability, type="l",
#'      xlab="Temperature (°C)", ylab="Suitability",
#'      main="Partial dependence: temperature")
#'
#' # Salinity response (summer)
#' sal_pd <- sensitivity_analysis(result, "ostrea_edulis", "salinity",
#'                                season = "summer")
#'
#' # ggplot2 version
#' library(ggplot2)
#' ggplot(temp_pd, aes(x, suitability)) +
#'   geom_line(colour="steelblue", linewidth=1.2) +
#'   geom_ribbon(aes(ymin=0, ymax=suitability), alpha=0.2, fill="steelblue") +
#'   labs(x="Temperature (°C)", y="Suitability \[0-1\]") +
#'   theme_minimal()
#' }
sensitivity_analysis <- function(predicted,
                                  species,
                                  variable,
                                  n_steps    = 100L,
                                  background = NULL,
                                  season     = NULL,
                                  verbose    = TRUE) {

  # \u2500\u2500 Load species tolerances (with Bayesian updates) \u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500
  tols <- get_species_tolerances(species)
  tols <- .apply_bayesian_update(tols, species)

  if (!variable %in% names(tols$scored))
    cli::cli_abort(c(
      "Variable '{variable}' not found in scored parameters for '{species}'.",
      "i" = "Available: {paste(names(tols$scored), collapse=', ')}"
    ))

  params <- tols$scored[[variable]]

  # \u2500\u2500 Build range of focal variable \u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500
  # Use biological range from tolerance params + some margin
  opt_min <- params$optimal_min
  opt_max <- params$optimal_max
  lo      <- params$poor_min %||% params$acceptable_min %||% (opt_min - (opt_max - opt_min) * 1.5)
  hi      <- params$acceptable_max %||% params$poor_max %||% (opt_max + (opt_max - opt_min) * 1.5)
  abs_hi  <- params$absolute_max %||% hi

  # If variable exists in predicted, extend to observed range
  var_col <- names(predicted)[tolower(names(predicted)) == tolower(variable)]
  if (length(var_col) > 0) {
    obs_range <- range(as.numeric(predicted[[var_col[1]]]), na.rm = TRUE)
    lo     <- min(lo, obs_range[1])
    abs_hi <- max(abs_hi, obs_range[2])
  }

  x_seq <- seq(lo, abs_hi, length.out = n_steps)

  # \u2500\u2500 Build background row from medians \u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500
  bg_row <- list()
  if (is.null(background)) {
    for (v in names(tols$scored)) {
      # Find matching column
      col_lwr <- tolower(names(predicted))
      cands   <- c(v, tolower(v))
      col_hit <- names(predicted)[col_lwr %in% cands]
      if (length(col_hit) > 0 && v != variable) {
        vals <- as.numeric(predicted[[col_hit[1]]])
        bg_row[[v]] <- median(vals, na.rm = TRUE)
      }
    }
  } else {
    bg_row <- as.list(background)
  }
  if (!is.null(season)) bg_row[["season"]] <- season

  # \u2500\u2500 Compute suitability at each x \u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500
  suit_v <- numeric(n_steps)
  for (xi in seq_len(n_steps)) {
    row_i <- bg_row
    row_i[[variable]] <- x_seq[xi]
    scored_row <- .score_row(row_i, tols)
    suit_v[xi] <- scored_row$score
  }

  out_df <- data.frame(
    x           = x_seq,
    suitability = round(suit_v, 5),
    variable    = variable,
    species     = species,
    stringsAsFactors = FALSE
  )

  if (verbose) {
    cli::cli_inform(c(
      "i" = "Partial dependence: '{variable}' for '{species}'",
      " " = "Range evaluated: [{round(lo,3)}, {round(abs_hi,3)}]",
      " " = "Peak suitability {round(max(suit_v),3)} at {variable}={round(x_seq[which.max(suit_v)],3)}",
      " " = "Optimal range (prior): [{opt_min}, {opt_max}]"
    ))
  }

  out_df
}
