# Extracted from test-validation.R:186

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "oystermapR", path = "..")
attach(test_env, warn.conflicts = FALSE)

# prequel ----------------------------------------------------------------------
library(testthat)
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

# test -------------------------------------------------------------------------
d  <- .make_synthetic_data()
cv <- suppressWarnings(
    spatial_block_cv(d$predicted, d$records,
                      n_blocks = 2L, seed = 99L,
                      plot = FALSE, verbose = FALSE)
  )
expect_true(cv$mean_tss >= -1 && cv$mean_tss <= 1)
