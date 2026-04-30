library(testthat)

# =============================================================================
# Risk module tests — disease risk, shellfish classification
# =============================================================================

# ── Shared helper ─────────────────────────────────────────────────────────────

.make_result_df <- function(temps  = c(10, 15, 20, 25),
                             sals   = c(33, 33, 33, 33),
                             turbs  = c(5,  5,  5,  5)) {
  data.frame(
    lat         = seq(51.0, 51.0 + (length(temps) - 1) * 0.01, by = 0.01),
    lon         = rep(-4.0, length(temps)),
    temperature = temps,
    salinity    = sals,
    turbidity   = turbs,
    suitability = runif(length(temps), 0.3, 0.9),
    stringsAsFactors = FALSE
  )
}

# ── score_disease_risk — ostrea_edulis (Bonamia) ──────────────────────────────

test_that("score_disease_risk returns required columns for ostrea_edulis", {
  df  <- .make_result_df()
  out <- score_disease_risk(df, species = "ostrea_edulis", verbose = FALSE)
  expect_true("disease_risk_score" %in% names(out))
  expect_true("disease_risk_class" %in% names(out))
  expect_true("disease_agent"      %in% names(out))
  expect_true("disease_risk_note"  %in% names(out))
})

test_that("score_disease_risk scores are in [0, 1]", {
  df  <- .make_result_df()
  out <- score_disease_risk(df, species = "ostrea_edulis", verbose = FALSE)
  expect_true(all(out$disease_risk_score >= 0 & out$disease_risk_score <= 1,
                  na.rm = TRUE))
})

test_that("score_disease_risk Bonamia: cold sites score lower risk than warm", {
  # Bonamia transmission peaks 15-20°C; negligible below 8°C
  cold <- .make_result_df(temps = c(5, 6))
  warm <- .make_result_df(temps = c(17, 18))
  out_cold <- score_disease_risk(cold, "ostrea_edulis", verbose = FALSE)
  out_warm <- score_disease_risk(warm, "ostrea_edulis", verbose = FALSE)
  expect_true(mean(out_cold$disease_risk_score) <
              mean(out_warm$disease_risk_score))
})

test_that("score_disease_risk Bonamia: risk class labels are valid", {
  df  <- .make_result_df()
  out <- score_disease_risk(df, species = "ostrea_edulis", verbose = FALSE)
  valid_classes <- c("Low", "Moderate", "High", "Critical")
  expect_true(all(out$disease_risk_class %in% valid_classes))
})

test_that("score_disease_risk Bonamia: agent is correct", {
  df  <- .make_result_df()
  out <- score_disease_risk(df, species = "ostrea_edulis", verbose = FALSE)
  expect_true(all(out$disease_agent == "Bonamia ostreae"))
})

# ── score_disease_risk — magallana_gigas (OsHV-1) ────────────────────────────

test_that("score_disease_risk returns required columns for magallana_gigas", {
  df  <- .make_result_df()
  out <- score_disease_risk(df, species = "magallana_gigas", verbose = FALSE)
  expect_true("disease_risk_score" %in% names(out))
  expect_true("disease_risk_class" %in% names(out))
})

test_that("score_disease_risk OsHV-1: warm sites score higher risk", {
  # OsHV-1 mortality events at >16°C
  cool <- .make_result_df(temps = c(12, 13))
  warm <- .make_result_df(temps = c(20, 22))
  out_cool <- score_disease_risk(cool, "magallana_gigas", verbose = FALSE)
  out_warm <- score_disease_risk(warm, "magallana_gigas", verbose = FALSE)
  expect_true(mean(out_warm$disease_risk_score) >
              mean(out_cool$disease_risk_score))
})

test_that("score_disease_risk OsHV-1: agent label is correct", {
  df  <- .make_result_df()
  out <- score_disease_risk(df, species = "magallana_gigas", verbose = FALSE)
  expect_true(all(out$disease_agent == "OsHV-1 microvariant"))
})

test_that("score_disease_risk errors for unsupported species", {
  df <- .make_result_df()
  expect_error(score_disease_risk(df, species = "ostrea_lurida", verbose = FALSE))
})

test_that("score_disease_risk errors when temperature column absent", {
  df <- data.frame(lat = 51, lon = -4, salinity = 33)
  expect_error(score_disease_risk(df, species = "ostrea_edulis", verbose = FALSE))
})

test_that("score_disease_risk Bonamia proximity multiplier increases risk", {
  df    <- .make_result_df(temps = c(16, 16))  # peak transmission temperature
  # Known infected site very close to row 1 (lat 51.00)
  sites <- data.frame(lat = 51.001, lon = -4.001, stringsAsFactors = FALSE)
  out_no_sites <- score_disease_risk(df, "ostrea_edulis",
                                      known_sites = NULL,  verbose = FALSE)
  out_with_sites <- score_disease_risk(df, "ostrea_edulis",
                                        known_sites = sites, verbose = FALSE)
  # Row 1 should have higher risk with nearby infected site
  expect_true(out_with_sites$disease_risk_score[1] >=
              out_no_sites$disease_risk_score[1])
})

# ── add_shellfish_classification ──────────────────────────────────────────────

test_that("add_shellfish_classification works from existing column", {
  df              <- .make_result_df(2)
  df$water_class  <- c("A", "B")
  out <- add_shellfish_classification(df, class_col = "water_class",
                                       verbose = FALSE)
  expect_true("shellfish_class"         %in% names(out))
  expect_true("shellfish_class_penalty" %in% names(out))
  expect_equal(out$shellfish_class[1], "A")
  expect_equal(out$shellfish_class[2], "B")
})

test_that("add_shellfish_classification penalties follow EC regulation", {
  df             <- .make_result_df(4)
  df$water_class <- c("A", "B", "C", "Prohibited")
  out <- add_shellfish_classification(df, class_col = "water_class",
                                       verbose = FALSE)
  expect_equal(out$shellfish_class_penalty[out$shellfish_class == "A"],          1.00)
  expect_equal(out$shellfish_class_penalty[out$shellfish_class == "B"],          0.80)
  expect_equal(out$shellfish_class_penalty[out$shellfish_class == "C"],          0.60)
  expect_equal(out$shellfish_class_penalty[out$shellfish_class == "Prohibited"], 0.00)
})

test_that("add_shellfish_classification Unclassified penalty is precautionary", {
  df <- .make_result_df(1)
  # No class supplied → Unclassified
  out <- add_shellfish_classification(df, verbose = FALSE)
  expect_equal(out$shellfish_class[1], "Unclassified")
  # Precautionary penalty (0.70 per rationale)
  expect_equal(out$shellfish_class_penalty[1], 0.70)
})

test_that("add_shellfish_classification spatial matching works", {
  df    <- .make_result_df(2)  # lats 51.00, 51.01
  areas <- data.frame(lat = 51.00, lon = -4.00, shellfish_class = "A",
                       stringsAsFactors = FALSE)
  out <- add_shellfish_classification(df,
          classified_areas = areas,
          match_radius_deg = 0.05,
          verbose = FALSE)
  # Row 1 should be matched to "A"
  expect_equal(out$shellfish_class[1], "A")
})

test_that("add_shellfish_classification Prohibited sites get zero penalty", {
  df             <- .make_result_df(1)
  df$water_class <- "Prohibited"
  out <- add_shellfish_classification(df, class_col = "water_class",
                                       verbose = FALSE)
  expect_equal(out$shellfish_class_penalty[1], 0.00)
})
