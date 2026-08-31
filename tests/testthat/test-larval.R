library(testthat)

# =============================================================================
# Larval dispersal connectivity tests
# =============================================================================

# ── Shared helpers ────────────────────────────────────────────────────────────

# A simple 20-location survey spread over ~50 km × 50 km
# Half the sites are high-suitability (potential sources), half are lower.
.make_larval_df <- function(n = 20) {
  set.seed(77)
  lats <- runif(n, 51.0, 51.45)
  lons <- runif(n, -4.5, -4.0)
  suit <- c(runif(n/2, 0.55, 0.90),    # sources
            runif(n/2, 0.10, 0.38))     # below min_suitability
  data.frame(
    lat         = lats,
    lon         = lons,
    suitability = suit,
    stringsAsFactors = FALSE
  )
}

# A geographically spread dataset for isolation tests
.make_isolated_df <- function() {
  data.frame(
    lat         = c(51.0, 51.01, 55.0),   # third site is ~400 km away
    lon         = c(-4.0, -4.01, -3.0),
    suitability = c(0.80, 0.75, 0.80),
    stringsAsFactors = FALSE
  )
}

# ── Output structure ──────────────────────────────────────────────────────────

test_that("score_larval_connectivity returns required columns (species route)", {
  df  <- .make_larval_df()
  out <- score_larval_connectivity(df, species = "ostrea_edulis", verbose = FALSE)
  required <- c("larval_dispersal_km", "larval_pld_days", "larval_type",
                "n_larval_sources", "source_quality_score",
                "larval_cluster_id", "larval_cluster_size",
                "nearest_source_km", "larval_connectivity_score",
                "larval_connectivity_class", "larval_connectivity_note",
                "larval_source")
  for (nm in required)
    expect_true(nm %in% names(out), info = paste("missing:", nm))
})

test_that("score_larval_connectivity returns required columns (dispersal_km route)", {
  df  <- .make_larval_df()
  out <- score_larval_connectivity(df, dispersal_km = 20, verbose = FALSE)
  expect_true("larval_connectivity_score" %in% names(out))
  expect_true("larval_dispersal_km" %in% names(out))
})

test_that("score_larval_connectivity scores are in [0, 1]", {
  df  <- .make_larval_df()
  out <- score_larval_connectivity(df, species = "ostrea_edulis", verbose = FALSE)
  expect_true(all(out$larval_connectivity_score >= 0 &
                  out$larval_connectivity_score <= 1, na.rm = TRUE))
})

test_that("score_larval_connectivity source_quality_score is in [0, 1]", {
  df  <- .make_larval_df()
  out <- score_larval_connectivity(df, species = "ostrea_edulis", verbose = FALSE)
  expect_true(all(out$source_quality_score >= 0 &
                  out$source_quality_score <= 1, na.rm = TRUE))
})

# ── Species PLD lookups ────────────────────────────────────────────────────────

test_that("ostrea_edulis yields shorter dispersal_km than magallana_gigas", {
  df       <- .make_larval_df()
  out_oe   <- score_larval_connectivity(df, species = "ostrea_edulis",   verbose = FALSE)
  out_mg   <- score_larval_connectivity(df, species = "magallana_gigas", verbose = FALSE)
  # O. edulis PLD ~4 days vs M. gigas ~21 days
  expect_true(out_oe$larval_dispersal_km[1] < out_mg$larval_dispersal_km[1])
})

test_that("larval_type is lecithotrophic for ostrea_edulis", {
  df  <- .make_larval_df()
  out <- score_larval_connectivity(df, species = "ostrea_edulis", verbose = FALSE)
  expect_true(all(out$larval_type == "lecithotrophic"))
})

test_that("larval_type is planktotrophic for magallana_gigas", {
  df  <- .make_larval_df()
  out <- score_larval_connectivity(df, species = "magallana_gigas", verbose = FALSE)
  expect_true(all(out$larval_type == "planktotrophic"))
})

test_that("all 5 species keys are resolved without error", {
  df   <- .make_larval_df()
  keys <- c("ostrea_edulis", "magallana_gigas", "crassostrea_angulata",
            "ostrea_stentina", "ostrea_lurida")
  for (k in keys)
    expect_no_error(
      score_larval_connectivity(df, species = k, verbose = FALSE),
      message = paste("Failed for", k)
    )
})

test_that("crassostrea_gigas alias resolves to magallana_gigas", {
  df  <- .make_larval_df()
  out <- score_larval_connectivity(df, species = "crassostrea_gigas", verbose = FALSE)
  expect_true(all(out$larval_type == "planktotrophic"))
})

# ── Dispersal kernel parameters ────────────────────────────────────────────────

test_that("dispersal_km override is respected", {
  df  <- .make_larval_df()
  out <- score_larval_connectivity(df, dispersal_km = 50, verbose = FALSE)
  expect_true(all(out$larval_dispersal_km == 50))
})

test_that("pld_days × tidal_excursion_km = larval_dispersal_km", {
  df  <- .make_larval_df()
  out <- score_larval_connectivity(df, pld_days = 7, tidal_excursion_km = 6,
                                    verbose = FALSE)
  expect_equal(out$larval_dispersal_km[1], 7 * 6)
})

test_that("larger tidal_excursion_km → larger dispersal_km", {
  df   <- .make_larval_df()
  out1 <- score_larval_connectivity(df, species = "ostrea_edulis",
                                     tidal_excursion_km = 3,  verbose = FALSE)
  out2 <- score_larval_connectivity(df, species = "ostrea_edulis",
                                     tidal_excursion_km = 12, verbose = FALSE)
  expect_true(out2$larval_dispersal_km[1] > out1$larval_dispersal_km[1])
})

# ── Spatial logic ─────────────────────────────────────────────────────────────

test_that("closely spaced high-suitability sites score higher than isolated ones", {
  df  <- .make_isolated_df()
  out <- score_larval_connectivity(df, dispersal_km = 50, verbose = FALSE)
  # Sites 1 & 2 are ~1 km apart; site 3 is ~400 km away — should be isolated
  score_near     <- mean(out$larval_connectivity_score[1:2])
  score_isolated <- out$larval_connectivity_score[3]
  expect_true(score_near > score_isolated)
})

test_that("geographically isolated site gets Isolated classification", {
  df  <- .make_isolated_df()
  out <- score_larval_connectivity(df, dispersal_km = 50, verbose = FALSE)
  expect_equal(out$larval_connectivity_class[3], "Isolated")
})

test_that("closely spaced suitable patches are in the same cluster", {
  df  <- .make_isolated_df()
  out <- score_larval_connectivity(df, dispersal_km = 50, verbose = FALSE)
  # Sites 1 & 2 should share a cluster ID
  expect_equal(out$larval_cluster_id[1], out$larval_cluster_id[2])
})

test_that("n_larval_sources is 0 for isolated sites", {
  df  <- .make_isolated_df()
  out <- score_larval_connectivity(df, dispersal_km = 50, verbose = FALSE)
  expect_equal(out$n_larval_sources[3], 0L)
})

test_that("larger dispersal kernel → more sources and higher scores", {
  df   <- .make_larval_df()
  out_small <- score_larval_connectivity(df, dispersal_km = 2,  verbose = FALSE)
  out_large <- score_larval_connectivity(df, dispersal_km = 80, verbose = FALSE)
  expect_true(mean(out_large$n_larval_sources) >= mean(out_small$n_larval_sources))
  expect_true(mean(out_large$larval_connectivity_score) >=
              mean(out_small$larval_connectivity_score))
})

# ── Connectivity matrix (Route 2) ─────────────────────────────────────────────

test_that("connectivity_matrix route scores rows and marks source as 'matrix'", {
  df <- .make_larval_df()

  # Build a synthetic matrix: site 1 is a source for site 10
  cm <- data.frame(
    source_lat = df$lat[1],
    source_lon = df$lon[1],
    dest_lat   = df$lat[10],
    dest_lon   = df$lon[10],
    weight     = 0.75,
    stringsAsFactors = FALSE
  )

  out <- score_larval_connectivity(df, species = "ostrea_edulis",
                                    connectivity_matrix = cm,
                                    matrix_match_radius_deg = 0.02,
                                    verbose = FALSE)
  expect_true("larval_connectivity_score" %in% names(out))
  # At least one row should be scored from matrix
  expect_true(any(out$larval_source %in% c("matrix", "matrix+union_find")))
})

test_that("connectivity_matrix errors with missing required columns", {
  df <- .make_larval_df()
  bad_cm <- data.frame(source_lat = 51, source_lon = -4, weight = 0.5)
  expect_error(
    score_larval_connectivity(df, species = "ostrea_edulis",
                               connectivity_matrix = bad_cm, verbose = FALSE)
  )
})

# ── Error handling ─────────────────────────────────────────────────────────────

test_that("score_larval_connectivity errors when no species/dispersal supplied", {
  df <- .make_larval_df()
  expect_error(score_larval_connectivity(df, verbose = FALSE))
})

test_that("score_larval_connectivity errors for unknown species", {
  df <- .make_larval_df()
  expect_error(
    score_larval_connectivity(df, species = "unicorn_oyster", verbose = FALSE)
  )
})

test_that("score_larval_connectivity errors when required columns absent", {
  df <- data.frame(lat = 51, lon = -4)  # no suitability column
  expect_error(
    score_larval_connectivity(df, dispersal_km = 20, verbose = FALSE)
  )
})

test_that("score_larval_connectivity class values are from valid set", {
  df  <- .make_larval_df()
  out <- score_larval_connectivity(df, species = "ostrea_edulis", verbose = FALSE)
  valid <- c("Highly connected", "Connected", "Low connectivity", "Isolated")
  expect_true(all(out$larval_connectivity_class %in% valid))
})

# ── PLD lookup for all 14 species (v1.3.0) ────────────────────────────────────

test_that("score_larval_connectivity works for all 14 species", {
  df <- .make_larval_df()
  new_species <- c(
    "crassostrea_virginica", "saccostrea_glomerata", "magallana_sikamea",
    "magallana_ariakensis",  "crassostrea_hongkongensis", "crassostrea_nippona",
    "crassostrea_belcheri",  "ostrea_chilensis", "ostrea_denselamellosa"
  )
  for (sp in new_species) {
    out <- score_larval_connectivity(df, species = sp, verbose = FALSE)
    expect_true("larval_connectivity_score" %in% names(out), info = sp)
    expect_true(all(is.finite(out$larval_connectivity_score) | is.na(out$larval_connectivity_score)),
                info = sp)
  }
})

test_that("lecithotrophic species have shorter dispersal than planktotrophic", {
  df <- .make_larval_df()
  # Lecithotrophic (short PLD): O. lurida, O. chilensis, O. denselamellosa
  # Planktotrophic (long PLD): M. gigas, C. virginica
  out_lurida   <- score_larval_connectivity(df, species = "ostrea_lurida",         verbose = FALSE)
  out_gigas    <- score_larval_connectivity(df, species = "magallana_gigas",       verbose = FALSE)
  # Record the PLD used — check metadata columns if available
  pld_lurida <- unique(out_lurida$larval_pld_days)
  pld_gigas  <- unique(out_gigas$larval_pld_days)
  if (!any(is.na(c(pld_lurida, pld_gigas)))) {
    expect_true(mean(pld_lurida, na.rm = TRUE) < mean(pld_gigas, na.rm = TRUE))
  }
})
