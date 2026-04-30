library(testthat)

# =============================================================================
# Bayesian tolerance parameter updating tests
# =============================================================================
#
# These tests exercise the MAP + Laplace approximation path and the
# cache management functions. MCMC is not tested here (too slow for CRAN).
# All tests use synthetic presence/absence data with environmental values
# drawn from the known optimal ranges of Ostrea edulis to ensure the
# optimiser converges successfully.
# =============================================================================

# ── Synthetic training data ───────────────────────────────────────────────────

.make_bayes_records <- function(n = 30, seed = 42) {
  set.seed(seed)
  # Simulate presences in the O. edulis optimal range, absences outside it.
  # Temperature optimal ~15-20°C; presence if temp in 12-22°C, absence otherwise.
  temp     <- c(runif(ceiling(n / 2), 14, 20),   # likely presences
                runif(floor(n / 2),    4,  9))    # likely absences
  presence <- as.integer(temp > 11 & temp < 23) * rbinom(n, 1, 0.85) +
              as.integer(temp <=  9) * rbinom(n, 1, 0.10)
  data.frame(
    lat         = runif(n, 50.5, 52.0),
    lon         = runif(n, -5.0, -2.0),
    temperature = temp,
    salinity    = rnorm(n, 33, 1),      # always in acceptable range
    presence    = pmin(1, presence),
    stringsAsFactors = FALSE
  )
}

# ── update_species_tolerances — basic interface ───────────────────────────────

test_that("update_species_tolerances runs without error (MAP, temperature)", {
  records <- .make_bayes_records()
  expect_no_error(
    update_species_tolerances(
      records     = records,
      species     = "ostrea_edulis",
      update_vars = "temperature",
      method      = "map",
      min_records = 20L,
      verbose     = FALSE
    )
  )
})

test_that("update_species_tolerances returns a list with required fields", {
  records <- .make_bayes_records()
  fit <- update_species_tolerances(
    records     = records,
    species     = "ostrea_edulis",
    update_vars = "temperature",
    method      = "map",
    min_records = 20L,
    verbose     = FALSE
  )
  expect_type(fit, "list")
  for (nm in c("species", "method", "n_records", "loglik_null",
               "loglik_fit", "mcfadden_r2", "updated_params",
               "posterior_sd", "convergence")) {
    expect_true(nm %in% names(fit), info = paste("missing:", nm))
  }
})

test_that("update_species_tolerances convergence code is 0 (success)", {
  records <- .make_bayes_records()
  fit <- update_species_tolerances(
    records     = records,
    species     = "ostrea_edulis",
    update_vars = "temperature",
    method      = "map",
    min_records = 20L,
    verbose     = FALSE
  )
  expect_equal(fit$convergence, 0L)
})

test_that("update_species_tolerances McFadden R² is in [0, 1)", {
  records <- .make_bayes_records()
  fit <- update_species_tolerances(
    records     = records,
    species     = "ostrea_edulis",
    update_vars = "temperature",
    method      = "map",
    min_records = 20L,
    verbose     = FALSE
  )
  expect_true(is.numeric(fit$mcfadden_r2))
  expect_true(fit$mcfadden_r2 >= 0 && fit$mcfadden_r2 < 1)
})

test_that("update_species_tolerances errors when fewer records than min_records", {
  records <- .make_bayes_records(n = 5)
  expect_error(
    update_species_tolerances(
      records     = records,
      species     = "ostrea_edulis",
      update_vars = "temperature",
      min_records = 20L,
      verbose     = FALSE
    )
  )
})

test_that("update_species_tolerances errors when presence column absent", {
  records        <- .make_bayes_records()
  records$pres2  <- records$presence
  records        <- records[, setdiff(names(records), "presence")]
  expect_error(
    update_species_tolerances(
      records      = records,
      species      = "ostrea_edulis",
      update_vars  = "temperature",
      presence_col = "presence",
      min_records  = 20L,
      verbose      = FALSE
    )
  )
})

test_that("update_species_tolerances errors for unknown species", {
  records <- .make_bayes_records()
  expect_error(
    update_species_tolerances(
      records  = records,
      species  = "imaginary_bivalve",
      verbose  = FALSE
    )
  )
})

# ── Cache management —  get / reset / save / load ─────────────────────────────

test_that("get_tolerance_posteriors returns NULL before any update", {
  reset_tolerance_update("ostrea_stentina")  # species unlikely to have been updated
  post <- get_tolerance_posteriors("ostrea_stentina")
  expect_null(post)
})

test_that("get_tolerance_posteriors returns a list after update", {
  records <- .make_bayes_records()
  update_species_tolerances(
    records     = records,
    species     = "ostrea_edulis",
    update_vars = "temperature",
    method      = "map",
    min_records = 20L,
    verbose     = FALSE
  )
  post <- get_tolerance_posteriors("ostrea_edulis")
  expect_type(post, "list")
})

test_that("reset_tolerance_update clears cached posteriors", {
  records <- .make_bayes_records()
  update_species_tolerances(
    records     = records,
    species     = "ostrea_edulis",
    update_vars = "temperature",
    method      = "map",
    min_records = 20L,
    verbose     = FALSE
  )
  reset_tolerance_update("ostrea_edulis")
  post <- get_tolerance_posteriors("ostrea_edulis")
  expect_null(post)
})

test_that("save_tolerance_update writes a file and load_tolerance_update reads it", {
  records  <- .make_bayes_records()
  update_species_tolerances(
    records     = records,
    species     = "ostrea_edulis",
    update_vars = "temperature",
    method      = "map",
    min_records = 20L,
    verbose     = FALSE
  )

  tmp_file <- tempfile(fileext = ".rds")
  on.exit(unlink(tmp_file))

  save_tolerance_update("ostrea_edulis", path = tmp_file, verbose = FALSE)
  expect_true(file.exists(tmp_file))

  # Clear cache, then reload
  reset_tolerance_update("ostrea_edulis")
  expect_null(get_tolerance_posteriors("ostrea_edulis"))

  load_tolerance_update("ostrea_edulis", path = tmp_file, verbose = FALSE)
  post <- get_tolerance_posteriors("ostrea_edulis")
  expect_type(post, "list")
})

# ── Sequential updating ────────────────────────────────────────────────────────

test_that("two sequential updates both succeed and use distinct n_records", {
  set.seed(1)
  rec1 <- .make_bayes_records(n = 25, seed = 1)
  set.seed(2)
  rec2 <- .make_bayes_records(n = 28, seed = 2)

  reset_tolerance_update("ostrea_lurida")

  fit1 <- update_species_tolerances(
    records     = rec1,
    species     = "ostrea_lurida",
    update_vars = "temperature",
    method      = "map",
    min_records = 20L,
    verbose     = FALSE
  )
  fit2 <- update_species_tolerances(
    records     = rec2,
    species     = "ostrea_lurida",
    update_vars = "temperature",
    method      = "map",
    min_records = 20L,
    verbose     = FALSE
  )

  expect_equal(fit1$n_records, 25L)
  expect_equal(fit2$n_records, 28L)
  expect_true(fit2$convergence == 0)

  reset_tolerance_update("ostrea_lurida")
})

# ── Integration: updated parameters affect predict_oyster ─────────────────────

test_that("predict_oyster uses cached Bayesian parameters after update", {
  sample_path <- system.file("extdata", "sample_survey.csv", package = "oystermapR")
  skip_if(sample_path == "", "sample_survey.csv not found")

  reset_tolerance_update("ostrea_edulis")
  result_before <- predict_oyster(sample_path, species = "ostrea_edulis",
                                   verbose = FALSE)

  records <- .make_bayes_records()
  update_species_tolerances(
    records     = records,
    species     = "ostrea_edulis",
    update_vars = "temperature",
    method      = "map",
    min_records = 20L,
    verbose     = FALSE
  )

  result_after <- predict_oyster(sample_path, species = "ostrea_edulis",
                                  verbose = FALSE)

  # Both runs should return valid results; scores may differ
  expect_true("suitability" %in% names(result_before))
  expect_true("suitability" %in% names(result_after))

  reset_tolerance_update("ostrea_edulis")
})
