library(testthat)

# =============================================================================
# Wave exposure and sediment stability tests
# =============================================================================

# ── Shared test dataframe ─────────────────────────────────────────────────────

.make_site_df <- function(n = 4) {
  data.frame(
    lat     = c(51.0, 51.5, 52.0, 52.5)[seq_len(n)],
    lon     = c(-4.0, -4.1, -4.2, -4.3)[seq_len(n)],
    depth_m = c(5,    10,   15,   20   )[seq_len(n)],
    stringsAsFactors = FALSE
  )
}

# ── score_wave_exposure — output structure ─────────────────────────────────────

test_that("score_wave_exposure returns required columns (fetch scalar)", {
  df  <- .make_site_df()
  out <- score_wave_exposure(df, fetch_km = 10, verbose = FALSE)
  expect_true("wave_hs_m"           %in% names(out))
  expect_true("wave_exposure_score" %in% names(out))
  expect_true("wave_exposure_class" %in% names(out))
  expect_true("wave_exposure_note"  %in% names(out))
  expect_true("wave_source"         %in% names(out))
})

test_that("score_wave_exposure scores are in [0, 1]", {
  df  <- .make_site_df()
  out <- score_wave_exposure(df, fetch_km = 10, verbose = FALSE)
  expect_true(all(out$wave_exposure_score >= 0 & out$wave_exposure_score <= 1))
})

test_that("score_wave_exposure JONSWAP: larger fetch → larger Hs", {
  base <- .make_site_df(1)
  out_small <- score_wave_exposure(base, fetch_km = 2,   verbose = FALSE)
  out_large <- score_wave_exposure(base, fetch_km = 100, verbose = FALSE)
  expect_true(out_large$wave_hs_m > out_small$wave_hs_m)
})

test_that("score_wave_exposure JONSWAP: higher wind → larger Hs", {
  base <- .make_site_df(1)
  out_low  <- score_wave_exposure(base, fetch_km = 20, wind_speed_ms = 5,  verbose = FALSE)
  out_high <- score_wave_exposure(base, fetch_km = 20, wind_speed_ms = 20, verbose = FALSE)
  expect_true(out_high$wave_hs_m > out_low$wave_hs_m)
})

test_that("score_wave_exposure uses measured Hs when supplied as scalar", {
  df  <- .make_site_df(2)
  out <- score_wave_exposure(df, wave_height_m = 1.2, verbose = FALSE)
  expect_true(all(out$wave_hs_m == 1.2))
  expect_true(all(out$wave_source == "measured"))
})

test_that("score_wave_exposure uses measured Hs from column", {
  df             <- .make_site_df(2)
  df$hs_measured <- c(0.5, 2.5)
  out <- score_wave_exposure(df, wave_height_col = "hs_measured", verbose = FALSE)
  expect_equal(out$wave_hs_m, c(0.5, 2.5))
  expect_true(all(out$wave_source == "measured"))
})

test_that("score_wave_exposure fetch column overrides scalar fetch", {
  df            <- .make_site_df(2)
  df$fetch_dist <- c(5, 50)
  out <- score_wave_exposure(df, fetch_col = "fetch_dist", verbose = FALSE)
  # Larger fetch → larger Hs for row 2
  expect_true(out$wave_hs_m[2] > out$wave_hs_m[1])
})

test_that("score_wave_exposure depth-limiting caps Hs at ~0.6 * depth", {
  # Very shallow site with large fetch — Hs should be capped by depth
  df <- data.frame(lat = 51, lon = -4, depth_m = 2, stringsAsFactors = FALSE)
  out <- score_wave_exposure(df, fetch_km = 200, depth_limit = TRUE, verbose = FALSE)
  expect_true(out$wave_hs_m <= 0.6 * 2 + 0.01)  # allow tiny float tolerance
})

test_that("score_wave_exposure classes are consistent with Hs breakpoints", {
  df <- data.frame(
    lat     = rep(51, 4),
    lon     = rep(-4, 4),
    stringsAsFactors = FALSE
  )
  df <- score_wave_exposure(df,
         wave_height_m = c(0.2, 1.0, 2.0, 3.5),
         verbose = FALSE)
  expect_equal(df$wave_exposure_class[1], "Sheltered")
  expect_equal(df$wave_exposure_class[2], "Moderate")
  expect_equal(df$wave_exposure_class[3], "Exposed")
  expect_equal(df$wave_exposure_class[4], "Severe")
})

test_that("score_wave_exposure errors when lat/lon absent", {
  df <- data.frame(depth_m = 10)
  expect_error(score_wave_exposure(df, verbose = FALSE))
})

# ── score_sediment_stability — output structure ────────────────────────────────

test_that("score_sediment_stability returns required columns", {
  df          <- .make_site_df()
  df$current_ms <- c(0.1, 0.2, 0.3, 0.4)
  out <- score_sediment_stability(df, verbose = FALSE)
  expect_true("shields_parameter"       %in% names(out))
  expect_true("shields_critical"        %in% names(out))
  expect_true("mobility_ratio"          %in% names(out))
  expect_true("d50_mm_estimated"        %in% names(out))
  expect_true("sediment_stability_score" %in% names(out))
  expect_true("sediment_mobility_class" %in% names(out))
})

test_that("score_sediment_stability scores are in [0, 1]", {
  df            <- .make_site_df()
  df$current_ms <- c(0.1, 0.3, 0.6, 1.0)
  out <- score_sediment_stability(df, verbose = FALSE)
  expect_true(all(out$sediment_stability_score >= 0 &
                  out$sediment_stability_score <= 1))
})

test_that("score_sediment_stability: harder substrate → more stable", {
  # Sand vs gravel at same current speed
  df_sand   <- data.frame(lat = 51, lon = -4, current_ms = 0.3,
                           substrate = "sand",   stringsAsFactors = FALSE)
  df_gravel <- data.frame(lat = 51, lon = -4, current_ms = 0.3,
                           substrate = "gravel", stringsAsFactors = FALSE)
  out_sand   <- score_sediment_stability(df_sand,   verbose = FALSE)
  out_gravel <- score_sediment_stability(df_gravel, verbose = FALSE)
  expect_true(out_gravel$sediment_stability_score >
              out_sand$sediment_stability_score)
})

test_that("score_sediment_stability: faster current → less stable", {
  df_slow <- data.frame(lat = 51, lon = -4, current_ms = 0.1,
                         substrate = "sand", stringsAsFactors = FALSE)
  df_fast <- data.frame(lat = 51, lon = -4, current_ms = 0.8,
                         substrate = "sand", stringsAsFactors = FALSE)
  out_slow <- score_sediment_stability(df_slow, verbose = FALSE)
  out_fast <- score_sediment_stability(df_fast, verbose = FALSE)
  expect_true(out_fast$sediment_stability_score <
              out_slow$sediment_stability_score)
})

test_that("score_sediment_stability Shields parameter is positive", {
  df            <- .make_site_df(3)
  df$current_ms <- c(0.2, 0.5, 0.9)
  out <- score_sediment_stability(df, verbose = FALSE)
  expect_true(all(out$shields_parameter >= 0))
})

test_that("score_sediment_stability mobility_ratio > 1 → mobile class", {
  # Very fast current over fine sand → highly mobile
  df <- data.frame(lat = 51, lon = -4, current_ms = 2.0,
                   substrate = "soft", stringsAsFactors = FALSE)
  out <- score_sediment_stability(df, verbose = FALSE)
  expect_true(out$mobility_ratio >= 1)
  expect_true(out$sediment_mobility_class %in% c("Mobile", "Highly mobile"))
})

test_that("score_sediment_stability uses d50_mm column when supplied", {
  df         <- data.frame(lat = 51, lon = -4, current_ms = 0.2,
                            d50_mm = 30, stringsAsFactors = FALSE)
  out <- score_sediment_stability(df, verbose = FALSE)
  expect_equal(out$d50_mm_estimated[1], 30)
})

test_that("score_sediment_stability errors when lat/lon absent", {
  df <- data.frame(current_ms = 0.3, depth_m = 10)
  expect_error(score_sediment_stability(df, verbose = FALSE))
})

# ── Wave → Sediment pipeline ──────────────────────────────────────────────────

test_that("wave_hs_m from score_wave_exposure feeds sediment stability", {
  df            <- .make_site_df(3)
  df$current_ms <- c(0.2, 0.3, 0.4)
  df$substrate  <- "sand"
  df <- score_wave_exposure(df, fetch_km = 20, verbose = FALSE)
  df <- score_sediment_stability(df, verbose = FALSE)
  # wave_hs_m column is now present and used in orbital velocity calc
  expect_true("wave_hs_m"           %in% names(df))
  expect_true("shields_parameter"   %in% names(df))
  expect_true(nrow(df) == 3)
})
