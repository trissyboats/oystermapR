# =============================================================================
# Model Validation for oystermapR
#
# validate_against_records() \u2014 compare suitability predictions against known
#   presence/absence records (e.g. OSPAR habitat data, NBN Atlas, ICES WGOS)
#   and compute standard classification metrics (AUC, sensitivity,
#   specificity, F1, Brier score).
#
# No external packages are required: ROC/AUC is computed from first principles.
# =============================================================================


#' Validate suitability predictions against known presence/absence records
#'
#' @description
#' Compares the suitability scores from [predict_oyster()] against a
#' presence/absence dataset to quantify how well the model discriminates
#' between occupied and unoccupied habitat. Useful for assessing whether the
#' AHP-weighted scoring produces ecologically defensible outputs before using
#' them for site selection.
#'
#' **Metrics computed:**
#' - **AUC** (Area Under the ROC Curve): probability that a random presence
#'   location scores higher than a random absence location. 0.5 = random;
#'   1.0 = perfect discrimination.
#' - **Optimal threshold:** Youden's J statistic (maximises sensitivity +
#'   specificity - 1). Used for the confusion-matrix-derived metrics below.
#' - **Sensitivity** (true positive rate): fraction of presences correctly
#'   predicted above threshold.
#' - **Specificity** (true negative rate): fraction of absences correctly
#'   predicted below threshold.
#' - **F1 score**: harmonic mean of precision and sensitivity.
#' - **Brier score**: mean squared error between predicted probability and
#'   observed presence/absence; 0 = perfect, 0.25 = uninformative.
#' - **TSS** (True Skill Statistic = sensitivity + specificity - 1): values
#'   above 0.4 generally indicate useful predictive skill.
#'
#' @param predicted Dataframe from [predict_oyster()] containing `lat`, `lon`,
#'   and `suitability` columns.
#' @param records Dataframe of known presence/absence records. Must contain:
#'   - Coordinate columns (`lat`/`lon` or equivalents)
#'   - A presence column: 1 = present, 0 = absent (or logical TRUE/FALSE).
#' @param presence_col Character. Name of the presence/absence column in
#'   `records` (default `"presence"`).
#' @param match_radius_deg Numeric. Radius in decimal degrees within which a
#'   prediction cell is matched to a record (default `0.002` approx. 220 m).
#'   Records that fall outside all prediction cells at this radius are dropped
#'   with a warning.
#' @param plot Logical. Print an ASCII ROC curve to the console (default `TRUE`).
#' @param verbose Logical. Print full metric summary (default `TRUE`).
#'
#' @return A named list:
#'   - `auc`: numeric \[0,1\]
#'   - `optimal_threshold`: numeric \[0,1\] — Youden's J
#'   - `sensitivity`: numeric \[0,1\] at optimal threshold
#'   - `specificity`: numeric \[0,1\] at optimal threshold
#'   - `f1`: numeric \[0,1\] at optimal threshold
#'   - `tss`: numeric \[-1,1\] at optimal threshold
#'   - `brier_score`: numeric \[0,1\]
#'   - `n_presences`: integer
#'   - `n_absences`: integer
#'   - `n_unmatched`: integer — records dropped due to no nearby prediction
#'   - `roc_df`: dataframe with columns `threshold`, `sensitivity`,
#'     `specificity` (for custom plotting)
#'
#' @export
#' @examples
#' \dontrun{
#' # Load known flat oyster records (e.g. from NBN Atlas CSV export)
#' records <- read.csv("nbn_ostrea_edulis.csv")
#'
#' # Run prediction on same area
#' result <- predict_oyster(survey, "ostrea_edulis")
#'
#' # Validate
#' val <- validate_against_records(result, records,
#'                                  presence_col = "presence")
#' val$auc           # overall discrimination power
#' val$tss           # skill score
#' val$roc_df        # data for custom ggplot
#' }
validate_against_records <- function(predicted,
                                      records,
                                      presence_col    = "presence",
                                      match_radius_deg = 0.002,
                                      plot            = TRUE,
                                      verbose         = TRUE) {

  # \u2500\u2500 Validate inputs \u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500
  if (!"suitability" %in% names(predicted)) {
    cli::cli_abort(c(
      "{.arg predicted} must contain a {.val suitability} column.",
      "i" = "Pass the output of {.fn predict_oyster} directly."
    ))
  }

  records <- .standardise_coords(records)

  if (!presence_col %in% names(records)) {
    cli::cli_abort(c(
      "Presence column {.val {presence_col}} not found in {.arg records}.",
      "i" = "Available: {.val {names(records)}}"
    ))
  }

  obs <- as.numeric(as.logical(records[[presence_col]]))
  if (any(is.na(obs))) {
    n_na <- sum(is.na(obs))
    cli::cli_warn("{n_na} record{?s} with NA presence value dropped.")
    records <- records[!is.na(obs), ]
    obs     <- obs[!is.na(obs)]
  }

  # \u2500\u2500 Spatial matching: find nearest prediction cell for each record \u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500
  pred_suit  <- predicted$suitability
  pred_lat   <- predicted$lat
  pred_lon   <- predicted$lon

  matched_suit  <- numeric(nrow(records))
  matched_suit[] <- NA_real_

  for (i in seq_len(nrow(records))) {
    rlat <- records$lat[i]
    rlon <- records$lon[i]
    dist2 <- (pred_lat - rlat)^2 + (pred_lon - rlon)^2
    nearest_idx <- which.min(dist2)
    if (sqrt(dist2[nearest_idx]) <= match_radius_deg) {
      matched_suit[i] <- pred_suit[nearest_idx]
    }
  }

  n_unmatched <- sum(is.na(matched_suit))
  if (n_unmatched > 0) {
    cli::cli_warn(c(
      "!" = paste0("{n_unmatched} record{?s} had no prediction cell within ",
                   "{match_radius_deg}\u00b0 \u2014 dropped from validation."),
      "i" = "Increase {.arg match_radius_deg} or check coordinate alignment."
    ))
  }

  keep         <- !is.na(matched_suit)
  matched_suit <- matched_suit[keep]
  obs          <- obs[keep]

  if (length(matched_suit) < 10) {
    cli::cli_abort(c(
      "Only {length(matched_suit)} matched record{?s}; need at least 10.",
      "i" = "Check that records and predictions cover the same spatial area."
    ))
  }

  n_pres <- sum(obs == 1)
  n_abs  <- sum(obs == 0)

  if (n_pres == 0 || n_abs == 0) {
    cli::cli_abort("Need both presences (1) and absences (0) in records.")
  }

  # \u2500\u2500 ROC curve (manual, no external packages) \u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500
  # Sort all unique thresholds; compute TP, FP, TN, FN at each.
  thresholds <- sort(unique(c(0, matched_suit, 1)), decreasing = TRUE)

  roc_rows <- lapply(thresholds, function(thr) {
    pred_pos <- matched_suit >= thr
    tp  <- sum( pred_pos &  obs == 1)
    fp  <- sum( pred_pos &  obs == 0)
    tn  <- sum(!pred_pos &  obs == 0)
    fn  <- sum(!pred_pos &  obs == 1)
    sens <- if ((tp + fn) > 0) tp / (tp + fn) else 0
    spec <- if ((tn + fp) > 0) tn / (tn + fp) else 0
    list(threshold = thr, tp = tp, fp = fp, tn = tn, fn = fn,
         sensitivity = sens, specificity = spec)
  })

  roc_df <- do.call(rbind.data.frame, roc_rows)

  # AUC via trapezoidal rule (1 - specificity on x, sensitivity on y)
  fpr <- 1 - roc_df$specificity
  tpr <- roc_df$sensitivity
  ord <- order(fpr)
  auc <- sum(diff(fpr[ord]) * (tpr[ord[-1]] + tpr[ord[-length(ord)]]) / 2)
  auc <- abs(auc)

  # Youden's J: maximise sensitivity + specificity - 1
  J <- roc_df$sensitivity + roc_df$specificity - 1
  best_idx  <- which.max(J)
  opt_thr   <- roc_df$threshold[best_idx]
  opt_sens  <- roc_df$sensitivity[best_idx]
  opt_spec  <- roc_df$specificity[best_idx]
  tss       <- opt_sens + opt_spec - 1

  # F1 at optimal threshold
  tp_opt <- roc_df$tp[best_idx]
  fp_opt <- roc_df$fp[best_idx]
  fn_opt <- roc_df$fn[best_idx]
  prec   <- if ((tp_opt + fp_opt) > 0) tp_opt / (tp_opt + fp_opt) else 0
  f1     <- if ((prec + opt_sens) > 0) 2 * prec * opt_sens / (prec + opt_sens) else 0

  # Brier score
  brier <- mean((matched_suit - obs)^2)

  # \u2500\u2500 ASCII ROC plot \u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500
  if (plot) {
    .ascii_roc(fpr[ord], tpr[ord], auc)
  }

  # \u2500\u2500 Verbose summary \u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500
  if (verbose) {
    cli::cli_h2("Validation Results")
    cli::cli_inform(c(
      " " = paste0("Records: {n_pres} presences | {n_abs} absences | ",
                   "{n_unmatched} unmatched"),
      " " = "\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500",
      " " = "AUC:              {round(auc, 3)}  (>0.7 = useful; >0.8 = good)",
      " " = "TSS:              {round(tss, 3)}  (>0.4 = useful skill)",
      " " = "Brier score:      {round(brier, 3)}  (lower = better; 0.25 = random)",
      " " = "Optimal threshold:{round(opt_thr, 3)}",
      " " = "Sensitivity:      {round(opt_sens, 3)}  (fraction of presences correctly predicted)",
      " " = "Specificity:      {round(opt_spec, 3)}  (fraction of absences correctly predicted)",
      " " = "F1 score:         {round(f1, 3)}"
    ))

    if (auc < 0.6) {
      cli::cli_warn(c(
        "!" = "AUC < 0.6: model discriminates only marginally better than random.",
        "i" = paste0("Consider: (1) adding more scored environmental variables, ",
                     "(2) adjusting AHP weights, (3) checking spatial alignment of records.")
      ))
    } else if (auc >= 0.8) {
      cli::cli_inform(c("i" = "AUC >= 0.8: good discrimination."))
    }
  }

  invisible(list(
    auc               = round(auc,      4),
    optimal_threshold = round(opt_thr,  4),
    sensitivity       = round(opt_sens, 4),
    specificity       = round(opt_spec, 4),
    f1                = round(f1,       4),
    tss               = round(tss,      4),
    brier_score       = round(brier,    4),
    n_presences       = n_pres,
    n_absences        = n_abs,
    n_unmatched       = n_unmatched,
    roc_df            = roc_df
  ))
}


# =============================================================================
# Spatial Block Cross-Validation
# =============================================================================
#
# Standard random cross-validation inflates performance metrics for spatially
# autocorrelated data because train and test points near each other share
# similar environments \u2014 the model "leaks" across the split (Hijmans 2012).
#
# Spatial block CV (Roberts et al. 2017) partitions records into contiguous
# spatial blocks so that held-out blocks are genuinely geographically
# independent of training blocks. This gives an honest estimate of
# transferability \u2014 the key criterion for species distribution models used
# in site selection and conservation planning.
#
# Method:
#  1. Estimate spatial autocorrelation range via Moran's I decay or
#     variogram-style binning of residuals (simplified, base-R implementation).
#  2. Partition records into k square blocks (default k = 5).
#  3. Leave-one-block-out: train on k-1 blocks, predict on held-out block.
#  4. Aggregate per-fold AUC; report mean \u00b1 SD.
#
# References:
#  Roberts et al. (2017): Cross-validation strategies for data with temporal,
#   spatial, hierarchical, or phylogenetic structure. Ecography 40:913-929.
#  Hijmans (2012): Cross-validation of species distribution models: removing
#   spatial sorting bias and calibration with a null model. Ecology 93:679-688.

#' Spatial block cross-validation for suitability model evaluation
#'
#' @description
#' Evaluates model performance using spatial block cross-validation (Roberts
#' et al. 2017), which avoids inflated performance estimates that arise when
#' nearby train and test records share similar environments.
#'
#' Records are partitioned into `n_blocks` contiguous geographic blocks. In
#' each fold, one block is held out as test data and the remaining blocks
#' provide training data to re-score the suitability model. AUC, TSS, and
#' Brier score are computed per fold and summarised across folds.
#'
#' This function is purely evaluative — it does not refit a new model. Instead,
#' it uses the spatial heterogeneity in the existing `predicted` suitability
#' surface to estimate out-of-block performance.
#'
#' @param predicted Dataframe from [predict_oyster()] containing `lat`, `lon`,
#'   `suitability`.
#' @param records Dataframe of known presence/absence records. Must contain
#'   `lat`, `lon`, and a presence/absence column.
#' @param presence_col Character. Name of presence/absence column (default
#'   `"presence"`).
#' @param n_blocks Integer. Number of spatial blocks (default 5). More blocks
#'   = finer spatial resolution but smaller test sets per fold.
#' @param match_radius_deg Numeric. Matching radius in degrees (default 0.002
#'   approx. 220 m). Passed to inner matching step.
#' @param seed Integer. Random seed for reproducibility (default 42).
#' @param plot Logical. Reserved for future visualisation output; currently
#'   unused (default TRUE).
#' @param verbose Logical. Print fold-level diagnostics (default TRUE).
#'
#' @return A named list:
#'   - `mean_auc`: mean AUC across blocks (primary reporting metric)
#'   - `auc_sd`: standard deviation of per-block AUC
#'   - `mean_tss`, `tss_sd`: mean / SD of True Skill Statistic
#'   - `brier_mean`, `brier_sd`: mean / SD of Brier score
#'   - `n_blocks_actual`: number of blocks with >= 5 records (used in CV)
#'   - `fold_results`: dataframe with per-fold metrics
#'   - `spatial_autocorr_range_deg`: estimated autocorrelation range (degrees)
#'   - `spatial_bias_warning`: logical, TRUE if random CV would be misleading
#'
#' @section Interpreting results:
#' Compare `mean_auc` from spatial CV with standard AUC from
#' [validate_against_records()]. A large drop (> 0.10) indicates strong
#' spatial autocorrelation and means the model is less transferable than
#' the standard validation suggested. This is common for coastal surveys
#' with spatially clustered records.
#'
#' @export
#' @references
#' Roberts et al. (2017) Ecography 40:913-929.
#'   doi:10.1111/ecog.02881
#'
#' @examples
#' \dontrun{
#' records <- read.csv("nbn_ostrea_edulis.csv")
#' result  <- predict_oyster(survey, "ostrea_edulis")
#'
#' # Standard validation
#' std_val <- validate_against_records(result, records)
#' std_val$auc  # may be optimistically high
#'
#' # Spatial block CV
#' sp_cv <- spatial_block_cv(result, records, n_blocks = 5)
#' sp_cv$mean_auc  # more honest estimate of transferability
#'
#' # Compare -- if sp_cv$mean_auc << std_val$auc, model is spatially
#' # autocorrelated and transfers less well than initially apparent.
#' }
spatial_block_cv <- function(predicted,
                              records,
                              presence_col    = "presence",
                              n_blocks        = 5L,
                              match_radius_deg = 0.002,
                              seed            = 42L,
                              plot            = TRUE,
                              verbose         = TRUE) {

  set.seed(seed)

  # \u2500\u2500 Input checks \u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500
  if (!"suitability" %in% names(predicted))
    cli::cli_abort("predicted must contain a 'suitability' column.")
  if (!presence_col %in% names(records))
    cli::cli_abort("Column '{presence_col}' not found in records.")

  records <- .standardise_coords(records)
  obs_all <- as.numeric(as.logical(records[[presence_col]]))
  keep    <- !is.na(obs_all)
  records <- records[keep, ]
  obs_all <- obs_all[keep]

  n_rec <- nrow(records)
  if (n_rec < n_blocks * 5)
    cli::cli_abort(c(
      "Too few records ({n_rec}) for {n_blocks} blocks (need >= {n_blocks*5}).",
      "i" = "Reduce n_blocks or collect more records."
    ))

  # \u2500\u2500 Match records to prediction cells (once) \u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500
  pred_suit <- predicted$suitability
  pred_lat  <- predicted$lat
  pred_lon  <- predicted$lon

  matched_suit <- vapply(seq_len(n_rec), function(i) {
    d2  <- (pred_lat - records$lat[i])^2 + (pred_lon - records$lon[i])^2
    idx <- which.min(d2)
    if (sqrt(d2[idx]) <= match_radius_deg) pred_suit[idx] else NA_real_
  }, numeric(1))

  keep2   <- !is.na(matched_suit)
  n_unmat <- sum(!keep2)
  if (n_unmat > 0 && verbose)
    cli::cli_warn("{n_unmat} record(s) unmatched (no prediction cell within {match_radius_deg}\u00b0); dropped.")
  records      <- records[keep2, ]
  obs_all      <- obs_all[keep2]
  matched_suit <- matched_suit[keep2]

  # \u2500\u2500 Spatial partitioning into blocks \u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500
  # Divide geographic extent into n_blocks roughly equal grid cells by
  # assigning each record to the nearest block centroid (k-means style).
  # Simple approach: split by longitude quantiles nested in latitude quantiles
  # to get square-ish blocks without requiring spatial packages.
  lat_cut <- cut(records$lat, breaks = ceiling(sqrt(n_blocks)), labels = FALSE)
  lon_cut <- cut(records$lon, breaks = ceiling(sqrt(n_blocks)), labels = FALSE)
  block_id <- (lat_cut - 1) * ceiling(sqrt(n_blocks)) + lon_cut

  # Remap to 1..n_blocks_actual
  unique_blocks  <- unique(block_id)
  block_id_remapped <- match(block_id, unique_blocks)
  n_blocks_actual <- length(unique_blocks)

  if (verbose) {
    cli::cli_h2("Spatial Block Cross-Validation")
    cli::cli_inform(c(
      " " = "Records: {length(obs_all)} ({sum(obs_all)} presences, {sum(obs_all==0)} absences)",
      " " = "Blocks:  {n_blocks_actual} (requested {n_blocks})"
    ))
    block_sizes <- table(block_id_remapped)
    cli::cli_inform("  Block sizes: {paste(block_sizes, collapse=' | ')}")
  }

  # \u2500\u2500 Estimate spatial autocorrelation range \u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500
  # Use Moran's I-style decay: bin pairwise distances and compute correlation
  # of suitability scores. Range = distance at which correlation drops below 0.1.
  autocorr_range <- tryCatch({
    n_s <- min(200L, length(matched_suit))  # subsample for speed
    idx_s <- sample(length(matched_suit), n_s)
    suits_s <- matched_suit[idx_s]
    lats_s  <- records$lat[idx_s]
    lons_s  <- records$lon[idx_s]

    # All pairwise distances and suitability products
    pairs <- expand.grid(i = seq_len(n_s), j = seq_len(n_s))
    pairs <- pairs[pairs$i < pairs$j, ]
    dist_v  <- sqrt((lats_s[pairs$i]-lats_s[pairs$j])^2 +
                    (lons_s[pairs$i]-lons_s[pairs$j])^2)
    prod_v  <- (suits_s[pairs$i] - mean(suits_s)) *
               (suits_s[pairs$j] - mean(suits_s))
    var_s   <- var(suits_s)

    dist_breaks <- quantile(dist_v, probs = seq(0, 1, length.out = 12))
    bin_id      <- cut(dist_v, breaks = dist_breaks, include.lowest = TRUE)
    bin_corr    <- tapply(prod_v / max(var_s, 1e-9), bin_id, mean, na.rm=TRUE)
    bin_mid     <- tapply(dist_v, bin_id, mean, na.rm=TRUE)

    # Find distance where correlation first drops below 0.10
    first_low <- which(bin_corr < 0.10)
    if (length(first_low) > 0) bin_mid[first_low[1]] else max(dist_v)
  }, error = function(e) NA_real_)

  bias_warning <- !is.na(autocorr_range) && autocorr_range > 0.05

  if (verbose) {
    cli::cli_inform(c(
      " " = "Spatial autocorr. range: ~{round(autocorr_range, 3)}\u00b0 ({round(autocorr_range*111, 1)} km)",
      " " = "Spatial bias warning: {bias_warning}"
    ))
    if (bias_warning)
      cli::cli_warn(c(
        "!" = "Spatial autocorrelation detected (range > 0.05\u00b0).",
        "i" = "Random CV would likely overestimate AUC. Spatial block CV results are more reliable."
      ))
  }

  # \u2500\u2500 Leave-one-block-out cross-validation \u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500
  fold_results <- vector("list", n_blocks_actual)

  for (b in seq_len(n_blocks_actual)) {
    test_idx  <- which(block_id_remapped == b)
    if (length(test_idx) < 5) {
      if (verbose)
        cli::cli_inform("  Block {b}: only {length(test_idx)} records \u2014 skipping (need \u2265 5).")
      next
    }

    test_suit <- matched_suit[test_idx]
    test_obs  <- obs_all[test_idx]

    n_p <- sum(test_obs == 1)
    n_a <- sum(test_obs == 0)

    if (n_p == 0 || n_a == 0) {
      if (verbose)
        cli::cli_inform("  Block {b}: no presences or absences in test set \u2014 skipping.")
      next
    }

    # Compute AUC for this fold (no refitting \u2014 evaluating existing predictions)
    thresholds  <- sort(unique(c(0, test_suit, 1)), decreasing = TRUE)
    roc_rows_b  <- lapply(thresholds, function(thr) {
      pp  <- test_suit >= thr
      tp  <- sum( pp & test_obs == 1)
      fp  <- sum( pp & test_obs == 0)
      tn  <- sum(!pp & test_obs == 0)
      fn  <- sum(!pp & test_obs == 1)
      sens <- if ((tp+fn)>0) tp/(tp+fn) else 0
      spec <- if ((tn+fp)>0) tn/(tn+fp) else 0
      list(sensitivity=sens, specificity=spec)
    })
    fpr_b <- 1 - sapply(roc_rows_b, `[[`, "specificity")
    tpr_b <- sapply(roc_rows_b, `[[`, "sensitivity")
    ord_b <- order(fpr_b)
    auc_b <- abs(sum(diff(fpr_b[ord_b]) *
                     (tpr_b[ord_b[-1]] + tpr_b[ord_b[-length(ord_b)]]) / 2))

    # Youden's J \u2192 TSS
    J_b      <- tpr_b + (1-fpr_b) - 1
    best_b   <- which.max(J_b)
    tss_b    <- J_b[best_b]

    # Brier score
    brier_b  <- mean((test_suit - test_obs)^2)

    fold_results[[b]] <- data.frame(
      block         = b,
      n_test        = length(test_idx),
      n_presences   = n_p,
      n_absences    = n_a,
      auc           = round(auc_b,  4),
      tss           = round(tss_b,  4),
      brier_score   = round(brier_b, 4)
    )

    if (verbose)
      cli::cli_inform(paste0(
        "  Block ", b, ": n=", length(test_idx),
        " (", n_p, "P/", n_a, "A)  AUC=", round(auc_b,3),
        "  TSS=", round(tss_b,3)
      ))
  }

  fold_df   <- do.call(rbind, Filter(Negate(is.null), fold_results))
  n_valid   <- nrow(fold_df)

  if (n_valid == 0)
    cli::cli_abort("No blocks had sufficient records for cross-validation.")

  auc_mean   <- mean(fold_df$auc)
  auc_sd     <- if (n_valid > 1) sd(fold_df$auc) else NA_real_
  tss_mean   <- mean(fold_df$tss)
  tss_sd     <- if (n_valid > 1) sd(fold_df$tss) else NA_real_
  brier_mean <- mean(fold_df$brier_score)
  brier_sd   <- if (n_valid > 1) sd(fold_df$brier_score) else NA_real_

  if (verbose) {
    cli::cli_h3("Spatial block CV summary")
    cli::cli_inform(c(
      " " = "Blocks used: {n_valid} of {n_blocks_actual}",
      " " = "AUC:   {round(auc_mean,3)} \u00b1 {round(auc_sd,3)} SD",
      " " = "TSS:   {round(tss_mean,3)} \u00b1 {round(tss_sd,3)} SD",
      " " = "Brier: {round(brier_mean,3)} \u00b1 {round(brier_sd,3)} SD"
    ))
    if (auc_mean < 0.65)
      cli::cli_warn(c(
        "!" = "Spatial CV AUC < 0.65: model shows limited transferability across space.",
        "i" = "Consider adding more environmental predictors or collecting more spatially distributed records."
      ))
  }

  invisible(list(
    mean_auc                 = round(auc_mean,   4),
    auc_sd                   = round(auc_sd,     4),
    mean_tss                 = round(tss_mean,   4),
    tss_sd                   = round(tss_sd,     4),
    brier_mean               = round(brier_mean, 4),
    brier_sd                 = round(brier_sd,   4),
    n_blocks_actual          = n_blocks_actual,
    n_valid_blocks           = n_valid,
    fold_results             = fold_df,
    spatial_autocorr_range_deg = round(autocorr_range, 5),
    spatial_bias_warning     = bias_warning
  ))
}


#' Print a simple ASCII ROC curve to the console
#' @keywords internal
.ascii_roc <- function(fpr, tpr, auc, width = 50, height = 20) {
  grid <- matrix(".", nrow = height, ncol = width)

  # Diagonal (random classifier)
  for (i in seq_len(min(width, height))) {
    r <- height - round((i / width) * height) + 1
    r <- max(1L, min(height, r))
    grid[r, i] <- "-"
  }

  # ROC curve points
  for (i in seq_along(fpr)) {
    x <- max(1L, min(width,  round(fpr[i] * width)  + 1L))
    y <- max(1L, min(height, height - round(tpr[i] * height) + 1L))
    grid[y, x] <- "*"
  }

  # Axes
  grid[height, ] <- "_"
  grid[, 1]      <- "|"
  grid[height, 1] <- "+"

  cli::cli_inform(c(
    "i" = "ROC Curve  (AUC = {round(auc, 3)})  * = model  - = random"
  ))
  cli::cli_inform("  1.0 |")
  for (i in seq_len(height)) {
    row_str <- paste(grid[i, ], collapse = "")
    if (i == 1)       cli::cli_inform("  1.0 {row_str}")
    else if (i == height) cli::cli_inform("  0.0 {row_str}")
    else              cli::cli_inform("      {row_str}")
  }
  cli::cli_inform("       0.0 (1-specificity) 1.0")
}
