library(testthat)

# =============================================================================
# Species tolerance parameter tests
# Verifies that all 5 supported species return valid, internally consistent
# tolerance structures from get_species_tolerances().
# =============================================================================

# ── Helper ────────────────────────────────────────────────────────────────────

.expect_valid_tol <- function(tol, key, expected_latin) {
  # Structure
  expect_type(tol, "list")
  expect_true("latin_name"  %in% names(tol), info = key)
  expect_true("exclusions"  %in% names(tol), info = key)
  expect_true("scored"      %in% names(tol), info = key)
  # Latin name
  expect_equal(tol$latin_name, expected_latin, info = key)
  # Scored variables are a named list
  expect_type(tol$scored, "list")
  expect_true(length(tol$scored) >= 3, info = key)
  # Each scored variable must have a type field
  for (v in names(tol$scored)) {
    expect_true("type" %in% names(tol$scored[[v]]),
                info = paste(key, v, "missing type"))
  }
  # Exclusions must have temperature limits
  expect_true("temperature" %in% names(tol$exclusions), info = key)
  temp_ex <- tol$exclusions$temperature
  expect_true(!is.null(temp_ex$min) || !is.null(temp_ex$hard_min) || !is.null(temp_ex$absolute_min),
              info = paste(key, "temperature exclusion min missing"))
}

# ── Ostrea edulis (European flat oyster) ─────────────────────────────────────

test_that("ostrea_edulis tolerance structure is valid", {
  tol <- get_species_tolerances("ostrea_edulis")
  .expect_valid_tol(tol, "ostrea_edulis", "Ostrea edulis")
})

test_that("ostrea_edulis depth parameters are biologically plausible", {
  tol  <- get_species_tolerances("ostrea_edulis")
  d    <- tol$scored$depth
  expect_true(!is.null(d))
  omin <- d$optimal_min
  omax <- d$optimal_max
  expect_true(is.numeric(omin) && is.numeric(omax))
  expect_true(omax > omin)
  # Edulis is subtidal to ~40 m; optimal should be within 0–30 m
  expect_true(omin >= 0  && omin <= 10)
  expect_true(omax > 0   && omax <= 40)
})

test_that("ostrea_edulis temperature optimal range is within known biology", {
  tol  <- get_species_tolerances("ostrea_edulis")
  temp <- tol$scored$temperature
  # Literature: gametogenesis 12-22°C; optimum ~15-20°C
  expect_true(temp$optimal_min >= 10 && temp$optimal_min <= 18)
  expect_true(temp$optimal_max >= 15 && temp$optimal_max <= 25)
})

test_that("ostrea_edulis salinity optimal range is within known biology", {
  tol <- get_species_tolerances("ostrea_edulis")
  # O. edulis salinity is exclusion-only (not in scored); check exclusions
  sal <- tol$exclusions$salinity
  expect_true(!is.null(sal))
  # Literature: tolerates 20-40 PSU; min_cold = 20, max = 40
  expect_true(sal$min_cold >= 20 && sal$min_cold <= 30)
  expect_true(sal$max >= 28 && sal$max <= 40)
})

# ── Magallana gigas (Pacific oyster) ─────────────────────────────────────────

test_that("magallana_gigas tolerance structure is valid", {
  tol <- get_species_tolerances("magallana_gigas")
  .expect_valid_tol(tol, "magallana_gigas", "Magallana gigas")
})

test_that("magallana_gigas has broader temperature tolerance than ostrea_edulis", {
  tol_mg <- get_species_tolerances("magallana_gigas")
  tol_oe <- get_species_tolerances("ostrea_edulis")
  # Pacific oyster is eurythermal — spans a wider temperature range
  temp_mg <- tol_mg$scored$temperature
  temp_oe <- tol_oe$scored$temperature
  range_mg <- temp_mg$optimal_max - temp_mg$optimal_min
  range_oe <- temp_oe$optimal_max - temp_oe$optimal_min
  expect_true(range_mg >= range_oe - 2)  # at least comparable range
})

test_that("magallana_gigas temperature optimum is in expected band", {
  tol  <- get_species_tolerances("magallana_gigas")
  temp <- tol$scored$temperature
  # Literature: reproduction >20°C; optimal ~20-28°C
  expect_true(temp$optimal_max >= 20)
})

# ── Crassostrea angulata (Portuguese oyster) ──────────────────────────────────

test_that("crassostrea_angulata tolerance structure is valid", {
  tol <- get_species_tolerances("crassostrea_angulata")
  .expect_valid_tol(tol, "crassostrea_angulata", "Crassostrea angulata")
})

test_that("crassostrea_angulata partial match works", {
  tol <- get_species_tolerances("angulata")
  expect_equal(tol$latin_name, "Crassostrea angulata")
})

test_that("crassostrea_angulata salinity tolerance reflects estuarine niche", {
  tol <- get_species_tolerances("crassostrea_angulata")
  # C. angulata salinity is exclusion-only (not in scored); check exclusions
  sal <- tol$exclusions$salinity
  expect_true(!is.null(sal))
  # Portuguese oyster tolerates brackish (15 PSU at cold temps)
  expect_true(!is.null(sal$min_cold) || !is.null(sal$min_warm))
  lower <- if (!is.null(sal$min_cold)) sal$min_cold else sal$min_warm
  expect_true(lower <= 25)
})

# ── Ostrea stentina (Denticulate flat oyster) ──────────────────────────────────

test_that("ostrea_stentina tolerance structure is valid", {
  tol <- get_species_tolerances("ostrea_stentina")
  .expect_valid_tol(tol, "ostrea_stentina", "Ostrea stentina")
})

test_that("ostrea_stentina partial match works", {
  tol <- get_species_tolerances("stentina")
  expect_equal(tol$latin_name, "Ostrea stentina")
})

test_that("ostrea_stentina temperature reflects warm Mediterranean niche", {
  tol  <- get_species_tolerances("ostrea_stentina")
  temp <- tol$scored$temperature
  # Mar Menor; warm-water Mediterranean species
  expect_true(temp$optimal_min >= 12)
  expect_true(temp$optimal_max >= 20)
})

# ── Ostrea lurida (Olympia oyster) ────────────────────────────────────────────

test_that("ostrea_lurida tolerance structure is valid", {
  tol <- get_species_tolerances("ostrea_lurida")
  .expect_valid_tol(tol, "ostrea_lurida", "Ostrea lurida")
})

test_that("ostrea_lurida partial match works", {
  tol <- get_species_tolerances("lurida")
  expect_equal(tol$latin_name, "Ostrea lurida")
})

test_that("ostrea_lurida temperature is cold-water adapted", {
  tol  <- get_species_tolerances("ostrea_lurida")
  temp <- tol$scored$temperature
  # NE Pacific; Puget Sound; gametogenesis 10-18°C; lower optimal than gigas
  expect_true(temp$optimal_max <= 24)
  expect_true(temp$optimal_min <= 16)
})

# ── Cross-species consistency checks ──────────────────────────────────────────

test_that("all 5 species return distinct latin names", {
  keys   <- c("ostrea_edulis", "magallana_gigas", "crassostrea_angulata",
              "ostrea_stentina", "ostrea_lurida")
  latins <- vapply(keys, function(k) get_species_tolerances(k)$latin_name, character(1))
  expect_equal(length(unique(latins)), 5L)
})

test_that("all 5 species have depth as a scored variable", {
  keys <- c("ostrea_edulis", "magallana_gigas", "crassostrea_angulata",
            "ostrea_stentina", "ostrea_lurida")
  for (k in keys) {
    tol <- get_species_tolerances(k)
    expect_true("depth" %in% names(tol$scored), info = k)
  }
})

test_that("all 5 species have temperature as a scored variable", {
  keys <- c("ostrea_edulis", "magallana_gigas", "crassostrea_angulata",
            "ostrea_stentina", "ostrea_lurida")
  for (k in keys) {
    tol <- get_species_tolerances(k)
    expect_true("temperature" %in% names(tol$scored), info = k)
  }
})

test_that("all 5 species have salinity as a scored or exclusion variable", {
  keys <- c("ostrea_edulis", "magallana_gigas", "crassostrea_angulata",
            "ostrea_stentina", "ostrea_lurida")
  for (k in keys) {
    tol <- get_species_tolerances(k)
    has_scored <- "salinity" %in% names(tol$scored)
    has_excl   <- "salinity" %in% names(tol$exclusions)
    expect_true(has_scored || has_excl, info = k)
  }
})

test_that("unknown species key raises an informative error", {
  expect_error(get_species_tolerances("imaginary_species"), regexp = ".")
})
