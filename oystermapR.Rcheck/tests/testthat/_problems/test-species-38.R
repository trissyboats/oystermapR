# Extracted from test-species.R:38

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "oystermapR", path = "..")
attach(test_env, warn.conflicts = FALSE)

# prequel ----------------------------------------------------------------------
library(testthat)
.expect_valid_tol <- function(tol, key, expected_latin) {
  # Structure
  expect_type(tol, "list")
  expect_true("latin_name"  %in% names(tol), info = key)
  expect_true("exclusions"  %in% names(tol), info = key)
  expect_true("scored"      %in% names(tol), info = key)
  # Latin name
  expect_equal(tol$latin_name, expected_latin, info = key)
  # Scored variables are a named list
  expect_type(tol$scored, "list", info = key)
  expect_true(length(tol$scored) >= 3, info = key)
  # Each scored variable must have a type field
  for (v in names(tol$scored)) {
    expect_true("type" %in% names(tol$scored[[v]]),
                info = paste(key, v, "missing type"))
  }
  # Exclusions must have temperature limits
  expect_true("temperature" %in% names(tol$exclusions), info = key)
  temp_ex <- tol$exclusions$temperature
  expect_true(!is.null(temp_ex$hard_min) || !is.null(temp_ex$absolute_min),
              info = paste(key, "temperature exclusion min missing"))
}

# test -------------------------------------------------------------------------
tol <- get_species_tolerances("ostrea_edulis")
.expect_valid_tol(tol, "ostrea_edulis", "Ostrea edulis")
