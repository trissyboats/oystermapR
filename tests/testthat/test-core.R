library(testthat)

# ---- Season detection --------------------------------------------------------

test_that("detect_season works for Northern Hemisphere", {
  expect_equal(detect_season("2024-01-15", lat =  51.5), "winter")
  expect_equal(detect_season("2024-04-15", lat =  51.5), "spring")
  expect_equal(detect_season("2024-07-15", lat =  51.5), "summer")
  expect_equal(detect_season("2024-10-15", lat =  51.5), "autumn")
})

test_that("detect_season flips for Southern Hemisphere", {
  expect_equal(detect_season("2024-01-15", lat = -33.9), "summer")
  expect_equal(detect_season("2024-07-15", lat = -33.9), "winter")
})

test_that("detect_season handles equatorial sites (low abs latitude)", {
  # At equator (lat = 0) seasons are NH-convention
  expect_equal(detect_season("2024-07-15", lat = 0),   "summer")
  expect_equal(detect_season("2024-01-15", lat = 0),   "winter")
})

test_that("detect_season handles Date and character input consistently", {
  s_char <- detect_season("2024-06-21", lat = 51)
  s_date <- detect_season(as.Date("2024-06-21"), lat = 51)
  expect_equal(s_char, s_date)
  expect_equal(s_char, "summer")
})

# ---- Species lookup ----------------------------------------------------------

test_that("get_species_tolerances returns Ostrea edulis by key", {
  tol <- get_species_tolerances("ostrea_edulis")
  expect_equal(tol$latin_name, "Ostrea edulis")
  expect_true(!is.null(tol$exclusions$temperature))
  expect_true(!is.null(tol$scored$depth))
})

test_that("get_species_tolerances partial matches on latin name", {
  tol <- get_species_tolerances("edulis")
  expect_equal(tol$latin_name, "Ostrea edulis")
})

test_that("get_species_tolerances returns a list with required structure", {
  tol <- get_species_tolerances("ostrea_edulis")
  expect_type(tol, "list")
  expect_true("latin_name"  %in% names(tol))
  expect_true("exclusions"  %in% names(tol))
  expect_true("scored"      %in% names(tol))
})

test_that("list_species returns at least one entry", {
  sp <- list_species()
  expect_true(length(sp) >= 1)
})

# ---- Exclusion checks --------------------------------------------------------

test_that("check_exclusions flags hypoxic locations", {
  tol <- get_species_tolerances("ostrea_edulis")
  df  <- data.frame(lat = 51, lon = -4, temperature = 14, salinity = 32,
                    dissolved_oxygen = 2.5, stringsAsFactors = FALSE)
  out <- check_exclusions(df, tol)
  expect_true(out$excluded[1])
  expect_match(out$exclusion_reason[1], "hypoxia")
})

test_that("check_exclusions flags low salinity", {
  tol <- get_species_tolerances("ostrea_edulis")
  df  <- data.frame(lat = 51, lon = -4, temperature = 14, salinity = 15,
                    dissolved_oxygen = 7, stringsAsFactors = FALSE)
  out <- check_exclusions(df, tol)
  expect_true(out$excluded[1])
  expect_match(out$exclusion_reason[1], "salinity")
})

test_that("check_exclusions passes valid location", {
  tol <- get_species_tolerances("ostrea_edulis")
  df  <- data.frame(lat = 51, lon = -4, temperature = 14, salinity = 33,
                    dissolved_oxygen = 7.5, stringsAsFactors = FALSE)
  out <- check_exclusions(df, tol)
  expect_false(out$excluded[1])
  expect_true(is.na(out$exclusion_reason[1]))
})

test_that("check_exclusions flags winter temperature below minimum", {
  tol <- get_species_tolerances("ostrea_edulis")
  df  <- data.frame(lat = 51, lon = -4, date = "2024-01-15",
                    temperature = 0.5, salinity = 33,
                    dissolved_oxygen = 9, stringsAsFactors = FALSE)
  df  <- add_season_column(df)
  out <- check_exclusions(df, tol)
  expect_true(out$excluded[1])
  expect_match(out$exclusion_reason[1], "winter")
})

test_that("check_exclusions returns correct columns", {
  tol <- get_species_tolerances("ostrea_edulis")
  df  <- data.frame(lat = 51, lon = -4, temperature = 14, salinity = 32,
                    dissolved_oxygen = 7, stringsAsFactors = FALSE)
  out <- check_exclusions(df, tol)
  expect_true("excluded"         %in% names(out))
  expect_true("exclusion_reason" %in% names(out))
})

test_that("check_exclusions handles multi-row dataframe correctly", {
  tol  <- get_species_tolerances("ostrea_edulis")
  df   <- data.frame(
    lat              = c(51, 51, 51),
    lon              = c(-4, -4, -4),
    temperature      = c(14, 14, 14),
    salinity         = c(33, 15, 33),   # row 2 is hyposaline
    dissolved_oxygen = c(7,  7,  2.5)   # row 3 is hypoxic
  )
  out <- check_exclusions(df, tol)
  expect_false(out$excluded[1])
  expect_true(out$excluded[2])
  expect_true(out$excluded[3])
})

# ---- add_season_column -------------------------------------------------------

test_that("add_season_column creates a season column from a date column", {
  df  <- data.frame(lat = 51, lon = -4, date = "2024-07-15")
  out <- add_season_column(df)
  expect_true("season" %in% names(out))
  expect_equal(out$season[1], "summer")
})

test_that("add_season_column works with datetime column name", {
  df  <- data.frame(lat = 51, lon = -4, datetime = "2024-01-15 10:00:00")
  out <- add_season_column(df)
  expect_true("season" %in% names(out))
  expect_equal(out$season[1], "winter")
})

# ---- Full pipeline -----------------------------------------------------------

test_that("predict_oyster runs on sample data and returns suitability column", {
  sample_path <- system.file("extdata", "sample_survey.csv", package = "oystermapR")
  skip_if(sample_path == "", "sample_survey.csv not found")

  result <- predict_oyster(sample_path, species = "ostrea_edulis")
  expect_true("suitability"       %in% names(result))
  expect_true("suitability_class" %in% names(result))
  expect_true(all(result$suitability >= 0 & result$suitability <= 1, na.rm = TRUE))
})

test_that("predict_oyster returns correct suitability classes", {
  sample_path <- system.file("extdata", "sample_survey.csv", package = "oystermapR")
  skip_if(sample_path == "", "sample_survey.csv not found")

  result <- predict_oyster(sample_path, species = "ostrea_edulis")
  valid_classes <- c("High", "Moderate", "Low", "Very Low", "Excluded")
  expect_true(all(result$suitability_class %in% valid_classes))
})


# ---- PDF text is safe in any locale -----------------------------------------------

test_that(".g() leaves UTF-8 text alone and gives ASCII stand-ins otherwise", {
  txt <- "Ostrea edulis — Flat · 0.45–0.70 ≥ 8 °C"
  expect_identical(.g(txt, utf8 = TRUE), txt)
  out <- .g(txt, utf8 = FALSE)
  expect_true(all(utf8ToInt(out) < 128))
  expect_identical(out, "Ostrea edulis - Flat | 0.45-0.70 >= 8  degC")
  expect_true(grepl("?", .g("café", utf8 = FALSE), fixed = TRUE))
})

test_that("generate_summary_pdf() works in whatever locale the session has", {
  skip_if_not_installed("grid")
  res <- suppressMessages(suppressWarnings(
    predict_oyster(system.file("extdata", "sample_survey.csv", package = "oystermapR"),
                   "ostrea_edulis", verbose = FALSE)))
  out <- tempfile(fileext = ".pdf")
  suppressMessages(generate_summary_pdf(res, out, title = "Kames Bay — Spring",
                                        author = "T. Tucker", open = FALSE))
  expect_true(file.exists(out) && file.size(out) > 1000)
})


# ---- exclusion reasons and missing seasons ------------------------------------------------

test_that("each excluded row's reason names its own value, not every flagged row's", {
  tl <- get_species_tolerances("ostrea_edulis")
  df <- data.frame(lat = 51, lon = -4, temperature = c(2, 3, 15, 30),
                   dissolved_oxygen = c(0.5, 1, 8, 8), salinity = 33)
  out <- suppressMessages(suppressWarnings(check_exclusions(df, tl)))
  expect_match(out$exclusion_reason[1], "temp_too_cold \\(2\\.0 < ")
  expect_false(grepl("3\\.0", out$exclusion_reason[1]))
  expect_match(out$exclusion_reason[2], "temp_too_cold \\(3\\.0 < ")
  expect_false(grepl("2\\.0 <", out$exclusion_reason[2]))
  expect_match(out$exclusion_reason[1], "hypoxia \\(0\\.50 < ")
  expect_match(out$exclusion_reason[2], "hypoxia \\(1\\.00 < ")
  expect_equal(lengths(regmatches(out$exclusion_reason[1], gregexpr("temp_too_cold", out$exclusion_reason[1]))), 1L)
  expect_true(is.na(out$exclusion_reason[3]))
  expect_match(out$exclusion_reason[4], "temp_too_hot \\(30\\.0 > ")
})

test_that("an unknown season means no seasonal exclusion, for every species", {
  # Tropical species have a winter minimum above temperate temperatures; with no
  # date column season is NA, and NA & TRUE used to crash the update.
  d <- data.frame(lat = 51, lon = -4, temperature = c(12, 14, 16, 17),
                  salinity = 33, dissolved_oxygen = 7, season = NA_character_)
  for (sp in c("crassostrea_iredalei", "saccostrea_cucullata")) {
    tl <- get_species_tolerances(sp)
    out <- suppressMessages(suppressWarnings(check_exclusions(d, tl)))
    expect_false(anyNA(out$excluded), info = sp)
    expect_length(out$excluded, nrow(d))
  }
})

test_that("every species can be scored on an ordinary survey", {
  d <- utils::read.csv(system.file("extdata", "sample_survey.csv", package = "oystermapR"))
  d$date <- NULL                                   # no date: seasonal checks skipped
  for (sp in names(.species_tolerances)) {
    res <- tryCatch(suppressMessages(suppressWarnings(predict_oyster(d, sp, verbose = FALSE))),
                    error = function(e) e)
    expect_false(inherits(res, "error"), info = paste(sp, if (inherits(res, "error")) conditionMessage(res)))
  }
})
