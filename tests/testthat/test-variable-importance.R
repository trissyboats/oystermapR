library(testthat)

# =============================================================================
# Variable importance and sensitivity analysis tests
# =============================================================================

# ── Shared helper — synthetic predicted + matched records ─────────────────────

# Build a synthetic predicted dataframe that:
#  - Has suitability and per-variable score_ columns
#  - Has records positioned at exactly the same lat/lon
#    (offset 0.0001° — well within default match_radius_deg 0.002°)
#  - High-suitability locations are assigned as presences
.make_imp_data <- function(seed = 1) {
  set.seed(seed)
  n    <- 30
  lats <- seq(51.0, 51.29, length.out = n)
  lons <- rep(-4.0, n)

  suit_temp <- pmin(1, pmax(0, rnorm(n, 0.7, 0.2)))
  suit_sal  <- pmin(1, pmax(0, rnorm(n, 0.6, 0.15)))
  suit_dep  <- pmin(1, pmax(0, rnorm(n, 0.5, 0.20)))

  # Weighted suitability (temperature is dominant)
  suit <- (suit_temp * 0.5 + suit_sal * 0.3 + suit_dep * 0.2)
  suit <- pmin(1, pmax(0, suit))

  predicted <- data.frame(
    lat                = lats,
    lon                = lons,
    suitability        = suit,
    score_temperature  = suit_temp,
    score_salinity     = suit_sal,
    score_depth        = suit_dep,
    stringsAsFactors   = FALSE
  )

  # Records at same locations ± small offset — presences where suit > 0.55
  records <- data.frame(
    lat      = lats + 0.0001,
    lon      = lons,
    presence = as.integer(suit > 0.55),
    stringsAsFactors = FALSE
  )

  list(predicted = predicted, records = records)
}

# ── permutation_importance ────────────────────────────────────────────────────

test_that("permutation_importance returns a dataframe", {
  d   <- .make_imp_data()
  imp <- permutation_importance(
    predicted      = d$predicted,
    records        = d$records,
    n_permutations = 5L,    # low for speed
    seed           = 42L,
    verbose        = FALSE
  )
  expect_s3_class(imp, "data.frame")
})

test_that("permutation_importance has required columns", {
  d   <- .make_imp_data()
  imp <- permutation_importance(
    predicted      = d$predicted,
    records        = d$records,
    n_permutations = 5L,
    seed           = 42L,
    verbose        = FALSE
  )
  for (nm in c("variable", "baseline_auc", "mean_permuted_auc",
               "importance", "importance_sd", "importance_pct", "rank")) {
    expect_true(nm %in% names(imp), info = paste("missing:", nm))
  }
})

test_that("permutation_importance has one row per scored variable", {
  d   <- .make_imp_data()
  imp <- permutation_importance(
    predicted      = d$predicted,
    records        = d$records,
    n_permutations = 5L,
    seed           = 42L,
    verbose        = FALSE
  )
  n_score_cols <- length(grep("^score_", names(d$predicted)))
  expect_equal(nrow(imp), n_score_cols)
})

test_that("permutation_importance importance values are numeric", {
  d   <- .make_imp_data()
  imp <- permutation_importance(
    predicted      = d$predicted,
    records        = d$records,
    n_permutations = 5L,
    seed           = 42L,
    verbose        = FALSE
  )
  expect_true(is.numeric(imp$importance))
})

test_that("permutation_importance baseline_auc is in [0, 1]", {
  d   <- .make_imp_data()
  imp <- permutation_importance(
    predicted      = d$predicted,
    records        = d$records,
    n_permutations = 5L,
    seed           = 42L,
    verbose        = FALSE
  )
  expect_true(all(imp$baseline_auc >= 0 & imp$baseline_auc <= 1))
})

test_that("permutation_importance rank column is 1..n with no duplicates", {
  d   <- .make_imp_data()
  imp <- permutation_importance(
    predicted      = d$predicted,
    records        = d$records,
    n_permutations = 5L,
    seed           = 42L,
    verbose        = FALSE
  )
  expect_equal(sort(imp$rank), seq_len(nrow(imp)))
})

test_that("permutation_importance errors when no score_ columns present", {
  d <- .make_imp_data()
  bad_pred <- d$predicted[, !grepl("^score_", names(d$predicted))]
  expect_error(
    permutation_importance(bad_pred, d$records, verbose = FALSE),
    regexp = "."
  )
})

test_that("permutation_importance errors when suitability column absent", {
  d <- .make_imp_data()
  bad_pred <- d$predicted[, setdiff(names(d$predicted), "suitability")]
  expect_error(
    permutation_importance(bad_pred, d$records, verbose = FALSE),
    regexp = "."
  )
})

test_that("permutation_importance is reproducible with same seed", {
  d    <- .make_imp_data()
  imp1 <- permutation_importance(d$predicted, d$records,
                                  n_permutations = 5L, seed = 99L,
                                  verbose = FALSE)
  imp2 <- permutation_importance(d$predicted, d$records,
                                  n_permutations = 5L, seed = 99L,
                                  verbose = FALSE)
  expect_equal(imp1$importance, imp2$importance)
})

# ── sensitivity_analysis ─────────────────────────────────────────────────────

test_that("sensitivity_analysis returns a dataframe", {
  d  <- .make_imp_data()
  pd <- sensitivity_analysis(
    predicted = d$predicted,
    species   = "ostrea_edulis",
    variable  = "temperature",
    n_steps   = 20L,
    verbose   = FALSE
  )
  expect_s3_class(pd, "data.frame")
})

test_that("sensitivity_analysis has correct columns", {
  d  <- .make_imp_data()
  pd <- sensitivity_analysis(
    predicted = d$predicted,
    species   = "ostrea_edulis",
    variable  = "temperature",
    n_steps   = 20L,
    verbose   = FALSE
  )
  for (nm in c("x", "suitability", "variable", "species")) {
    expect_true(nm %in% names(pd), info = paste("missing:", nm))
  }
})

test_that("sensitivity_analysis has n_steps rows", {
  d  <- .make_imp_data()
  pd <- sensitivity_analysis(
    predicted = d$predicted,
    species   = "ostrea_edulis",
    variable  = "temperature",
    n_steps   = 25L,
    verbose   = FALSE
  )
  expect_equal(nrow(pd), 25L)
})

test_that("sensitivity_analysis suitability values in [0, 1]", {
  d  <- .make_imp_data()
  pd <- sensitivity_analysis(
    predicted = d$predicted,
    species   = "ostrea_edulis",
    variable  = "temperature",
    n_steps   = 30L,
    verbose   = FALSE
  )
  expect_true(all(pd$suitability >= 0 & pd$suitability <= 1, na.rm = TRUE))
})

test_that("sensitivity_analysis peaks within optimal temperature range", {
  d  <- .make_imp_data()
  pd <- sensitivity_analysis(
    predicted = d$predicted,
    species   = "ostrea_edulis",
    variable  = "temperature",
    n_steps   = 50L,
    verbose   = FALSE
  )
  # O. edulis temperature optimum ~15-20°C — peak should be in that range
  peak_x <- pd$x[which.max(pd$suitability)]
  expect_true(peak_x >= 10 && peak_x <= 25,
              info = paste("Peak temperature:", peak_x))
})

test_that("sensitivity_analysis variable column matches requested variable", {
  d  <- .make_imp_data()
  pd <- sensitivity_analysis(
    predicted = d$predicted,
    species   = "ostrea_edulis",
    variable  = "depth",
    n_steps   = 20L,
    verbose   = FALSE
  )
  expect_true(all(pd$variable == "depth"))
})

test_that("sensitivity_analysis species column matches requested species", {
  d  <- .make_imp_data()
  pd <- sensitivity_analysis(
    predicted = d$predicted,
    species   = "ostrea_edulis",
    variable  = "temperature",
    n_steps   = 20L,
    verbose   = FALSE
  )
  expect_true(all(pd$species == "ostrea_edulis"))
})

test_that("sensitivity_analysis errors for unknown species", {
  d <- .make_imp_data()
  expect_error(
    sensitivity_analysis(d$predicted, "imaginary_sp", "temperature",
                          verbose = FALSE)
  )
})
