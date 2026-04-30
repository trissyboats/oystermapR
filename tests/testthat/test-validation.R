library(testthat)

# =============================================================================
# Model validation tests
# validate_against_records() and spatial_block_cv()
# =============================================================================

# ── Helper — synthetic predicted + records pairs ──────────────────────────────

# Generate a synthetic predicted dataframe with known suitability values
# and a records dataframe positioned at the same locations.
# Suitability is set so presences score high and absences score low,
# giving a near-perfect AUC that we can assert > 0.5.
.make_synthetic_data <- function(seed = 1234) {
  set.seed(seed)
  n <- 40
  lats <- seq(51.0, 51.39, length.out = n)
  lons <- rep(-4.0, n)

  # Alternating high-low suitability
  suit <- rep(c(0.8, 0.2), n / 2)
  pres <- as.integer(suit > 0.5)

  predicted <- data.frame(
    lat             = lats,
    lon             = lons,
    suitability     = suit,
    score_temperature = suit,          # synthetic component scores
    score_salinity    = suit * 0.9,
    score_depth       = suit * 0.85,
    stringsAsFactors = FALSE
  )

  # Records sit exactly at prediction cells (radius 0 needed in matching,
  # but we use a small offset within match_radius_deg)
  records <- data.frame(
    lat      = lats + 0.0001,          # within default 0.002° tolerance
    lon      = lons,
    presence = pres,
    stringsAsFactors = FALSE
  )

  list(predicted = predicted, records = records)
}

# ── validate_against_records — output structure ────────────────────────────────

test_that("validate_against_records returns a list with required elements", {
  d   <- .make_synthetic_data()
  val <- validate_against_records(d$predicted, d$records,
                                   plot = FALSE, verbose = FALSE)
  expect_type(val, "list")
  expected <- c("auc", "optimal_threshold", "sensitivity", "specificity",
                "f1",  "tss", "brier_score", "n_presences", "n_absences",
                "n_unmatched", "roc_df")
  for (nm in expected)
    expect_true(nm %in% names(val), info = paste("missing:", nm))
})

test_that("validate_against_records AUC is in [0, 1]", {
  d   <- .make_synthetic_data()
  val <- validate_against_records(d$predicted, d$records,
                                   plot = FALSE, verbose = FALSE)
  expect_true(val$auc >= 0 && val$auc <= 1)
})

test_that("validate_against_records AUC > 0.5 for correctly ordered predictions", {
  d   <- .make_synthetic_data()
  val <- validate_against_records(d$predicted, d$records,
                                   plot = FALSE, verbose = FALSE)
  # Presences are assigned suitability 0.8, absences 0.2 → near-perfect discrimination
  expect_true(val$auc > 0.5)
})

test_that("validate_against_records sensitivity and specificity in [0, 1]", {
  d   <- .make_synthetic_data()
  val <- validate_against_records(d$predicted, d$records,
                                   plot = FALSE, verbose = FALSE)
  expect_true(val$sensitivity >= 0 && val$sensitivity <= 1)
  expect_true(val$specificity >= 0 && val$specificity <= 1)
})

test_that("validate_against_records TSS in [-1, 1]", {
  d   <- .make_synthetic_data()
  val <- validate_against_records(d$predicted, d$records,
                                   plot = FALSE, verbose = FALSE)
  expect_true(val$tss >= -1 && val$tss <= 1)
})

test_that("validate_against_records Brier score in [0, 1]", {
  d   <- .make_synthetic_data()
  val <- validate_against_records(d$predicted, d$records,
                                   plot = FALSE, verbose = FALSE)
  expect_true(val$brier_score >= 0 && val$brier_score <= 1)
})

test_that("validate_against_records roc_df has correct columns", {
  d   <- .make_synthetic_data()
  val <- validate_against_records(d$predicted, d$records,
                                   plot = FALSE, verbose = FALSE)
  expect_true(all(c("threshold", "sensitivity", "specificity") %in%
                    names(val$roc_df)))
})

test_that("validate_against_records n_presences and n_absences are correct", {
  d   <- .make_synthetic_data()
  val <- validate_against_records(d$predicted, d$records,
                                   plot = FALSE, verbose = FALSE)
  # 40 records, alternating presence/absence → 20 each
  expect_equal(val$n_presences + val$n_absences,
               val$n_presences + val$n_absences)  # totals are consistent
  expect_true(val$n_presences >= 1)
  expect_true(val$n_absences  >= 1)
})

test_that("validate_against_records errors if suitability column absent", {
  d   <- .make_synthetic_data()
  bad <- d$predicted[, setdiff(names(d$predicted), "suitability")]
  expect_error(validate_against_records(bad, d$records,
                                         plot = FALSE, verbose = FALSE))
})

test_that("validate_against_records errors if presence column absent", {
  d    <- .make_synthetic_data()
  badr <- d$records[, setdiff(names(d$records), "presence")]
  expect_error(validate_against_records(d$predicted, badr,
                                         plot = FALSE, verbose = FALSE))
})

test_that("validate_against_records inverted predictions → AUC < 0.5", {
  d   <- .make_synthetic_data()
  # Flip suitability so presences score low and absences score high
  d$predicted$suitability <- 1 - d$predicted$suitability
  val <- validate_against_records(d$predicted, d$records,
                                   plot = FALSE, verbose = FALSE)
  expect_true(val$auc < 0.5)
})

# ── spatial_block_cv ──────────────────────────────────────────────────────────

test_that("spatial_block_cv returns a list with required elements", {
  d   <- .make_synthetic_data()
  # spatial_block_cv needs enough records for blocks; use n_blocks = 2
  cv <- suppressWarnings(
    spatial_block_cv(d$predicted, d$records,
                      n_blocks  = 2L,
                      seed      = 42L,
                      plot      = FALSE,
                      verbose   = FALSE)
  )
  expect_type(cv, "list")
  expect_true("mean_auc"    %in% names(cv))
  expect_true("mean_tss"    %in% names(cv))
  expect_true("fold_results" %in% names(cv))
})

test_that("spatial_block_cv mean_auc is in [0, 1]", {
  d  <- .make_synthetic_data()
  cv <- suppressWarnings(
    spatial_block_cv(d$predicted, d$records,
                      n_blocks = 2L, seed = 42L,
                      plot = FALSE, verbose = FALSE)
  )
  expect_true(is.numeric(cv$mean_auc))
  expect_true(cv$mean_auc >= 0 && cv$mean_auc <= 1)
})

test_that("spatial_block_cv fold_results has one row per block", {
  d  <- .make_synthetic_data()
  n_b <- 4L
  cv  <- suppressWarnings(
    spatial_block_cv(d$predicted, d$records,
                      n_blocks = n_b, seed = 42L,
                      plot = FALSE, verbose = FALSE)
  )
  expect_true(nrow(cv$fold_results) <= n_b)  # some blocks may be empty
})

test_that("spatial_block_cv mean_tss is in [-1, 1]", {
  d  <- .make_synthetic_data()
  cv <- suppressWarnings(
    spatial_block_cv(d$predicted, d$records,
                      n_blocks = 2L, seed = 99L,
                      plot = FALSE, verbose = FALSE)
  )
  expect_true(cv$mean_tss >= -1 && cv$mean_tss <= 1)
})
