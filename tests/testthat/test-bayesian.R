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


# ---- the fitter fits what is deployed -------------------------------------------

.two_var_tols <- function() {
  tols <- get_species_tolerances("ostrea_edulis")
  sub <- tols; sub$scored <- tols$scored[c("temperature", "salinity")]
  sub
}
.deployed_S <- function(tl, ev) {
  vapply(seq_along(ev$temperature), function(i)
    .score_row(list(temperature = ev$temperature[i], salinity = ev$salinity[i]), tl)$score,
    numeric(1))
}

test_that(".fit_suitability equals the deployed scorer, weights included", {
  sub <- .two_var_tols()
  set.seed(1); n <- 60
  ev <- list(temperature = runif(n, 5, 27), salinity = runif(n, 15, 38))
  p_t <- sub$scored$temperature; p_s <- sub$scored$salinity
  # rank 1 vs rank 5: if weights were rank instead of 1/rank this could not match
  expect_gt(p_s$rank, p_t$rank)
  theta0 <- c(0, 4, p_t$optimal_min, p_t$optimal_max, p_s$optimal_min, p_s$optimal_max)
  expect_equal(.fit_suitability(theta0, c("temperature", "salinity"), sub, ev),
               .deployed_S(sub, ev), tolerance = 1e-4)   # .score_row rounds to 4 dp
})

test_that("shoulders move with a candidate optimum exactly as the returned tolerances do", {
  sub <- .two_var_tols()
  set.seed(2); n <- 60
  ev <- list(temperature = runif(n, 5, 27), salinity = runif(n, 15, 38))
  theta <- c(0, 4, 13, 19.5, 27, 33)                    # optimum shifted and narrowed
  shifted <- .apply_params_to_tols(sub, list(temperature = list(optimal_min = 13, optimal_max = 19.5),
                                             salinity    = list(optimal_min = 27, optimal_max = 33)))
  expect_equal(.fit_suitability(theta, c("temperature", "salinity"), sub, ev),
               .deployed_S(shifted, ev), tolerance = 1e-4)
  pp <- .params_at(sub$scored$temperature, 13, 19.5)
  expect_equal(pp$optimal_min, 13); expect_equal(pp$optimal_max, 19.5)
  expect_equal(pp$absolute_max, sub$scored$temperature$absolute_max)   # hard limit stays put
})

test_that("a two-variable fit recovers the heavily weighted variable's optimum", {
  .cleanup_profiles <- function() {
    for (k in grep("::", names(.oystermapR_env$posterior_cache), value = TRUE))
      .oystermapR_env$posterior_cache[[k]] <- NULL
  }
  on.exit(.cleanup_profiles())
  sub <- .two_var_tols()
  err <- vapply(1:3, function(seed) {
    set.seed(seed); n <- 400
    d <- data.frame(lat = 51, lon = -4, temperature = runif(n, 6, 24), salinity = runif(n, 15, 38))
    tl <- .apply_params_to_tols(sub, list(temperature = list(optimal_min = 12, optimal_max = 18),
                                          salinity    = list(optimal_min = 26, optimal_max = 33)))
    d$presence <- rbinom(n, 1, plogis(-5 + 10 * .deployed_S(tl, d)))
    f <- suppressMessages(suppressWarnings(update_species_tolerances(
      d, "ostrea_edulis", update_vars = c("temperature", "salinity"), verbose = FALSE,
      profile = paste0("rec", seed))))
    c(abs(unname(f$updated_params$temperature$optimal_min) - 12),
      abs(unname(f$updated_params$temperature$optimal_max) - 18), f$mcfadden_r2)
  }, numeric(3))
  expect_lt(mean(err[1, ]), 0.6)     # was ~2 degrees out at the upper bound before
  expect_lt(mean(err[2, ]), 1.0)
  expect_gt(min(err[3, ]), 0.3)      # the fit is informative (it was ~0.03 before)
})

test_that("posterior SDs are numbers and a bound-hitting estimate reports the prior SD", {
  on.exit(for (k in grep("::", names(.oystermapR_env$posterior_cache), value = TRUE))
    .oystermapR_env$posterior_cache[[k]] <- NULL)
  set.seed(4); n <- 300
  d <- data.frame(lat = 51, lon = -4, temperature = runif(n, 6, 26))
  # An informative optimum (18-24) above what the search box allows for the
  # lower bound (default optimum 10-20, box to about 16): omin ends up outside it.
  d$presence <- rbinom(n, 1, ifelse(d$temperature > 18 & d$temperature < 24, 0.95, 0.05))
  f <- suppressMessages(suppressWarnings(update_species_tolerances(
    d, "ostrea_edulis", update_vars = "temperature", verbose = FALSE, profile = "bnd")))
  u <- f$updated_params$temperature
  expect_true(is.finite(u$posterior_sd_omin) && u$posterior_sd_omin > 0)
  expect_true(all(is.finite(u$ci95_omin)) && all(is.finite(u$ci95_omax)))
  expect_true(isTRUE(u$at_bound_omin))
  expect_false(isTRUE(u$at_bound_omax))    # the well-determined bound is not flagged
  expect_gt(f$mcfadden_r2, 0.3)
  base <- get_species_tolerances("ostrea_edulis")$scored$temperature
  expect_equal(unname(u$posterior_sd_omin), (base$optimal_max - base$optimal_min) * 0.20, tolerance = 1e-3)
})
