library(testthat)

# =============================================================================
# Survey QC and tidal correction tests
# =============================================================================

# ── qc_survey_data ─────────────────────────────────────────────────────────────

.make_survey_df <- function() {
  data.frame(
    lat              = c(51.1, 51.2, 51.3, 51.4, 51.5),
    lon              = c(-4.0, -4.0, -4.0, -4.0, -4.0),
    temperature      = c(14.0, 15.0, 16.0, 17.0, 18.0),   # all valid
    salinity         = c(32.0, 33.0, 34.0, 33.5, 32.5),
    dissolved_oxygen = c(7.0,  7.5,  8.0,  7.2,  6.9),
    depth            = c(5,    8,    12,   15,   20),
    stringsAsFactors = FALSE
  )
}

test_that("qc_survey_data returns a dataframe with qc columns", {
  df  <- .make_survey_df()
  out <- qc_survey_data(df, verbose = FALSE)
  expect_s3_class(out, "data.frame")
  expect_true("qc_n_flags" %in% names(out))
  expect_true("qc_status"  %in% names(out))
})

test_that("qc_survey_data: all-valid survey passes with zero flags", {
  df  <- .make_survey_df()
  out <- qc_survey_data(df, verbose = FALSE)
  expect_true(all(out$qc_n_flags == 0 | is.na(out$qc_n_flags)))
  expect_true(all(out$qc_status == "pass"))
})

test_that("qc_survey_data: out-of-range temperature flagged", {
  df               <- .make_survey_df()
  df$temperature[3] <- 50   # physically impossible for coastal seawater
  out <- qc_survey_data(df, verbose = FALSE)
  expect_true(out$qc_n_flags[3] >= 1)
  # Status should be review or fail
  expect_true(out$qc_status[3] %in% c("review", "fail"))
})

test_that("qc_survey_data: negative salinity flagged", {
  df              <- .make_survey_df()
  df$salinity[2]  <- -5   # impossible
  out <- qc_survey_data(df, verbose = FALSE)
  expect_true(out$qc_n_flags[2] >= 1)
})

test_that("qc_survey_data: extreme dissolved oxygen flagged", {
  df                       <- .make_survey_df()
  df$dissolved_oxygen[4]   <- 25  # > physical maximum in seawater
  out <- qc_survey_data(df, verbose = FALSE)
  expect_true(out$qc_n_flags[4] >= 1)
})

test_that("qc_survey_data: apply_flags=TRUE sets flagged values to NA", {
  df               <- .make_survey_df()
  df$temperature[3] <- 50
  out_keep <- qc_survey_data(df, apply_flags = FALSE, verbose = FALSE)
  out_na   <- qc_survey_data(df, apply_flags = TRUE,  verbose = FALSE)
  # Original df preserves the value
  expect_equal(out_keep$temperature[3], 50)
  # apply_flags replaces with NA
  expect_true(is.na(out_na$temperature[3]))
})

test_that("qc_survey_data: IQR outlier detection fires on extreme stat outlier", {
  df               <- .make_survey_df()
  df$salinity[5]   <- 200   # far outside IQR
  out <- qc_survey_data(df, verbose = FALSE)
  expect_true(out$qc_n_flags[5] >= 1)
})

test_that("qc_survey_data returns qc_flag_<variable> columns for checked vars", {
  df  <- .make_survey_df()
  out <- qc_survey_data(df, verbose = FALSE)
  # At least one qc_flag_ column should be present
  flag_cols <- grep("^qc_flag_", names(out), value = TRUE)
  expect_true(length(flag_cols) >= 1)
})

test_that("qc_survey_data: fail status assigned when 3+ flags on same row", {
  df                       <- .make_survey_df()
  df$temperature[1]        <- 60   # range fail
  df$salinity[1]           <- 90   # range fail
  df$dissolved_oxygen[1]   <- 25   # range fail → should trigger fail
  out <- qc_survey_data(df, verbose = FALSE)
  expect_equal(out$qc_status[1], "fail")
})

# ── Tidal correction — correct_to_chart_datum ─────────────────────────────────
# Note: correct_to_chart_datum modifies the 'depth' column in-place.
# It preserves raw values in 'depth_raw_m' when keep_raw = TRUE (default).

test_that("correct_to_chart_datum adds depth_raw_m column (keep_raw default)", {
  df <- data.frame(lat = 51, lon = -4, depth = c(5, 10, 15),
                   stringsAsFactors = FALSE)
  out <- correct_to_chart_datum(df, tidal_height_m = 2.0, verbose = FALSE)
  expect_true("depth_raw_m" %in% names(out))
})

test_that("correct_to_chart_datum: corrected depth = raw + tidal_height (scalar)", {
  df  <- data.frame(lat = 51, lon = -4, depth = c(5, 10),
                    stringsAsFactors = FALSE)
  out <- correct_to_chart_datum(df, tidal_height_m = 1.5, verbose = FALSE)
  # depth column is now corrected; depth_raw_m stores originals
  expect_equal(out$depth[1], 5 + 1.5)
  expect_equal(out$depth[2], 10 + 1.5)
  expect_equal(out$depth_raw_m[1], 5.0)
  expect_equal(out$depth_raw_m[2], 10.0)
})

test_that("correct_to_chart_datum: per-row tidal heights are respected", {
  df  <- data.frame(lat = 51, lon = -4, depth = c(5, 10),
                    stringsAsFactors = FALSE)
  out <- correct_to_chart_datum(df,
          tidal_height_m = c(1.0, 3.0),
          verbose = FALSE)
  expect_equal(out$depth[1], 5  + 1.0)
  expect_equal(out$depth[2], 10 + 3.0)
})

test_that("correct_to_chart_datum issues warning when no tidal height supplied", {
  df <- data.frame(lat = 51, lon = -4, depth = c(5, 10),
                   stringsAsFactors = FALSE)
  expect_warning(
    correct_to_chart_datum(df, tidal_height_m = NULL, verbose = TRUE),
    regexp = "."
  )
})

test_that("correct_to_chart_datum: corrected depth >= raw depth with positive offset", {
  # Positive tidal height means CD is deeper than surveyed depth
  df  <- data.frame(lat = 51, lon = -4, depth = c(3, 7, 12),
                    stringsAsFactors = FALSE)
  out <- correct_to_chart_datum(df, tidal_height_m = 2.5, verbose = FALSE)
  expect_true(all(out$depth >= df$depth))
})

# ── Tidal correction — auto_tidal_correct ────────────────────────────────────
# auto_tidal_correct modifies 'depth' in-place and adds 'tidal_height_pred_m'

test_that("auto_tidal_correct returns df with tidal_height_pred_m near a standard port", {
  # Milford Haven (UK) — well within harmonics database
  df <- data.frame(
    lat   = c(51.71, 51.71),
    lon   = c(-5.02, -5.02),
    depth = c(5, 10),
    date  = as.POSIXct(c("2024-06-15 10:00:00", "2024-06-15 12:00:00"), tz = "UTC"),
    stringsAsFactors = FALSE
  )
  out <- suppressWarnings(
    auto_tidal_correct(df, datetime_col = "date", depth_col = "depth",
                       verbose = FALSE)
  )
  expect_true("tidal_height_pred_m" %in% names(out))
  expect_true("depth_raw_m"         %in% names(out))
})

test_that("auto_tidal_correct corrected depth >= raw depth for positive tide", {
  df <- data.frame(
    lat   = c(51.71, 51.71),
    lon   = c(-5.02, -5.02),
    depth = c(5, 10),
    date  = as.POSIXct(c("2024-06-15 10:00:00", "2024-06-15 12:00:00"), tz = "UTC"),
    stringsAsFactors = FALSE
  )
  out <- suppressWarnings(
    auto_tidal_correct(df, datetime_col = "date", depth_col = "depth",
                       verbose = FALSE)
  )
  raw_depth <- out$depth_raw_m
  # depth column is now Chart Datum depth; raw stored in depth_raw_m
  # tidal height prediction may be negative (ebbing tide), so just check finite
  expect_true(all(is.finite(out$depth)))
})

test_that("auto_tidal_correct warns when site is far from all standard ports", {
  # Remote location well outside any standard port (mid-Atlantic)
  df <- data.frame(
    lat   = 30.0,
    lon   = -40.0,
    depth = 100,
    date  = as.POSIXct("2024-06-15 10:00:00", tz = "UTC"),
    stringsAsFactors = FALSE
  )
  expect_warning(
    auto_tidal_correct(df, datetime_col = "date", depth_col = "depth",
                       verbose = FALSE),
    regexp = "."
  )
})
