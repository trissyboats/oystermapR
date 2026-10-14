# ---- helpers --------------------------------------------------------------

#' Ostrea edulis ranks, as used in the acoustic survey write-up.
.test_tolerances_oe <- function() {
  ranks <- c(temperature = 1, fishing_intensity = 1, shear_stress = 2,
             chlorophyll_a = 3, current_velocity = 3, sediment_type = 3,
             substrate_hardness = 3, benthic_communities = 3,
             depth = 4, biotope = 4, ph = 4, omega_aragonite = 4,
             turbidity = 5, roughness = 5, salinity = 5,
             dissolved_oxygen = 5, slope = 6)
  list(
    latin_name  = "Ostrea edulis",
    common_name = "European flat oyster",
    scored = lapply(stats::setNames(as.list(ranks), names(ranks)),
                    function(r) list(rank = r, type = "optimal_range"))
  )
}

.test_assigned <- function() {
  list(
    bpi = list(rank = 3, rationale = paste(
      "Bathymetric position is a proxy for the sediment accumulation regime.",
      "Ranked alongside sediment_type (3), the published variable it stands",
      "in for.")),
    shadow_density = list(rank = 3, rationale = paste(
      "Density of objects standing proud of the seabed, from acoustic shadow",
      "geometry; settlement substrate for a cementing bivalve, so this sits",
      "with substrate_hardness (3)."))
  )
}

#' Score columns whose population SD is exactly the value given.
.test_scores_with_spread <- function(spread, layers, n, seed) {
  set.seed(seed)
  out <- as.data.frame(lapply(spread, function(s) {
    if (s == 0) return(rep(1, n))
    v <- stats::rnorm(n)
    (v - mean(v)) / sqrt(sum((v - mean(v))^2) / n) * s + 0.5
  }))
  names(out) <- layers
  out
}


test_that("weights reproduce the published Kames Bay derivation", {

  # Target: the weight table published for the Loch Melfort / Kames Bay
  # acoustic survey, derived independently in Python. If this test fails the
  # R implementation has diverged from the numbers in the write-up.
  tol    <- .test_tolerances_oe()
  target <- data.frame(
    layer  = c("bpi", "slope", "hardness", "settlement_texture",
               "shadow_density", "depth"),
    rel    = c(0.896, 0.712, 0.402, 0.392, 0.208, 0.930),
    spread = c(0.300, 0.265, 0.180, 0.289, 0.289, 0.000),
    weight = c(0.477, 0.168, 0.129, 0.121, 0.107, 0.000),
    stringsAsFactors = FALSE
  )

  scores <- .test_scores_with_spread(target$spread, target$layer, n = 5000,
                                     seed = 11)
  ew <- evidence_weights(
    scores,
    reliability = stats::setNames(target$rel, target$layer),
    tolerances  = tol,
    assigned    = .test_assigned(),
    aliases     = c(hardness = "substrate_hardness",
                    settlement_texture = "roughness")
  )
  got <- ew$table[match(target$layer, ew$table$layer), ]

  # Absolute, not relative: the published table rounds reliability and spread
  # to 3 dp, so agreement is expected to 3 dp of the weight and no further.
  expect_true(all(abs(got$weight - target$weight) < 1e-3))
  expect_identical(got$provenance,
                   c("assigned", "published", "published", "published",
                     "assigned", "published"))
  expect_identical(as.integer(got$rank), c(3L, 6L, 3L, 5L, 3L, 4L))
  expect_equal(sum(ew$weights), 1)
})


test_that("a variable with no spread takes no weight", {
  # Depth scored 1.0 at every location of the survey: rank 4, reliability
  # 0.93, and no information about where conditions differ. Under rank
  # weighting it took a third of the weight.
  tol <- .test_tolerances_oe()
  scores <- data.frame(depth = rep(1, 500), slope = runif(500))
  ew <- evidence_weights(scores, c(depth = 0.93, slope = 0.7), tol)
  expect_equal(unname(ew$weights[["depth"]]), 0)
  expect_gt(ew$weights[["slope"]], 0)
})


test_that("a variable with no measured reliability takes no weight", {
  tol <- .test_tolerances_oe()
  scores <- data.frame(depth = runif(500), slope = runif(500))
  expect_equal(unname(evidence_weights(scores, c(slope = 0.7), tol)$weights[["depth"]]), 0)
  expect_equal(unname(evidence_weights(scores, c(depth = NA_real_, slope = 0.7),
                                       tol)$weights[["depth"]]), 0)
})


test_that("an unreliable variable is demoted rather than removed", {
  # The point of the method: a layer that does not reproduce demotes itself,
  # so it can be offered without a second judgement call about excluding it.
  tol <- .test_tolerances_oe()
  set.seed(3)
  scores <- data.frame(depth = runif(500), slope = runif(500))
  strong <- evidence_weights(scores, c(depth = 0.9, slope = 0.7), tol)
  weak   <- evidence_weights(scores, c(depth = 0.02, slope = 0.7), tol)
  expect_lt(weak$weights[["depth"]], strong$weights[["depth"]] / 10)
  expect_true("depth" %in% weak$table$layer)
})


test_that("assigned layers need an explicit rank and are flagged", {
  tol <- .test_tolerances_oe()
  scores <- data.frame(bpi = runif(500), slope = runif(500))

  expect_error(
    evidence_weights(scores, c(bpi = 0.8, slope = 0.7), tol,
                     assigned = list(bpi = list(rationale = "x"))),
    "rank"
  )
  expect_warning(
    evidence_weights(scores, c(bpi = 0.8, slope = 0.7), tol,
                     assigned = list(bpi = list(rank = 3))),
    "rationale"
  )
  ew <- evidence_weights(scores, c(bpi = 0.8, slope = 0.7), tol,
                         assigned = .test_assigned())
  expect_identical(ew$table$provenance[ew$table$layer == "bpi"], "assigned")
  expect_true(is.na(ew$table$prior_var[ew$table$layer == "bpi"]))
})


test_that("unmatched layers are dropped with a warning", {
  tol <- .test_tolerances_oe()
  scores <- data.frame(mystery = runif(100), slope = runif(100))
  expect_warning(
    ew <- evidence_weights(scores, c(mystery = 0.9, slope = 0.7), tol),
    "neither in the tolerance spec"
  )
  expect_false("mystery" %in% names(ew$weights))
})


test_that("coverage is weight-based and matches the published figures", {
  tol <- .test_tolerances_oe()
  cov <- evidence_coverage(
    tol,
    observed         = c("substrate_hardness", "roughness", "slope", "depth"),
    attempted_failed = c("current_velocity", "shear_stress")
  )
  expect_equal(cov$coverage, 0.155, tolerance = 1e-3)
  expect_equal(cov$prior_observed, 0.95, tolerance = 1e-3)
  expect_equal(cov$prior_total, 6.1333, tolerance = 1e-3)
  expect_equal(cov$coverage_if_recovered, 0.291, tolerance = 1e-3)
  expect_identical(nrow(cov$ledger), 17L)
  expect_identical(sum(cov$ledger$status == "present"), 4L)
})


test_that("coverage differs from count-based completeness", {
  # 4 of 17 variables is 0.235 by count but 0.155 by weight, because the
  # missing variables include both rank-1 terms. The difference is the point.
  tol <- .test_tolerances_oe()
  cov <- evidence_coverage(tol, c("substrate_hardness", "roughness",
                                  "slope", "depth"))
  expect_lt(cov$coverage, 4 / 17)
})


test_that("assigned layers cannot raise coverage", {
  tol <- .test_tolerances_oe()
  base <- evidence_coverage(tol, c("substrate_hardness", "roughness",
                                   "slope", "depth"))
  expect_warning(
    padded <- evidence_coverage(tol, c("substrate_hardness", "roughness",
                                       "slope", "depth", "bpi",
                                       "shadow_density")),
    "not in the tolerance spec"
  )
  expect_equal(padded$coverage, base$coverage)
})


test_that("reliability_from_replicates recovers the attenuation", {
  set.seed(7)
  truth <- stats::rnorm(4000)
  r <- reliability_from_replicates(truth + stats::rnorm(4000, 0, 0.6),
                                   truth + stats::rnorm(4000, 0, 0.6))
  expect_equal(as.numeric(r), 1 / 1.36, tolerance = 0.05)
  expect_identical(attr(r, "n"), 4000L)
  expect_true(attr(r, "established"))
})


test_that("too few overlapping observations returns NA, not a correlation", {
  set.seed(9)
  r <- reliability_from_replicates(stats::rnorm(10), stats::rnorm(10))
  expect_true(is.na(as.numeric(r)))
  expect_false(attr(r, "established"))
  expect_match(attr(r, "note"), "too few")
})


test_that("an interval crossing zero is not established", {
  set.seed(21)
  x1 <- stats::rnorm(60); x2 <- stats::rnorm(60)
  r <- reliability_from_replicates(x1, x2)
  expect_false(attr(r, "established"))
  expect_true(is.finite(as.numeric(r)))   # still returned, still weighted
})


test_that("reliability_from_perturbation matches the analytic attenuation", {
  # r = var(signal) / (var(signal) + sigma^2)
  set.seed(42)
  depth <- 8 + stats::rnorm(20000, 0, 1.2)
  rp <- reliability_from_perturbation(depth, sigma = 0.308 / sqrt(2),
                                      n_rep = 5, seed = 20250805)
  expect_equal(as.numeric(rp), 1.2^2 / (1.2^2 + (0.308 / sqrt(2))^2),
               tolerance = 0.02)
  expect_identical(attr(rp, "method"), "perturbation")
})


test_that("perturbation is reproducible and leaves the RNG stream alone", {
  x <- stats::rnorm(2000)
  set.seed(99); before <- stats::runif(1)
  a <- reliability_from_perturbation(x, sigma = 0.3, seed = 5)
  b <- reliability_from_perturbation(x, sigma = 0.3, seed = 5)
  expect_identical(as.numeric(a), as.numeric(b))
  set.seed(99); expect_identical(stats::runif(1), before)
})


test_that("derive propagates error through a derived layer", {
  # Slope inherits its error from the depth surface; perturbing depth then
  # deriving is not the same as perturbing slope.
  set.seed(4)
  z <- as.vector(stats::filter(stats::rnorm(3000), rep(1 / 20, 20),
                               sides = 2))
  z <- z[is.finite(z)]
  direct  <- reliability_from_perturbation(z, sigma = 0.2, seed = 1)
  derived <- reliability_from_perturbation(
    z, sigma = 0.2, seed = 1,
    derive = function(v) c(NA, diff(v))
  )
  expect_lt(as.numeric(derived), as.numeric(direct))
})


test_that("confidence_score weights and normalises components", {
  route  <- c(1.0, 0.6, 0.3, 1.0)
  passes <- pmin(pmax(c(3, 1, 1, 2) / 2, 0.4), 1)
  cf <- confidence_score(list(route = route, passes = passes), c(0.65, 0.35))
  expect_equal(cf[1], 1)
  expect_equal(cf[3], 0.65 * 0.3 + 0.35 * 0.5)
  # weights are normalised, so an unnormalised pair gives the same answer
  expect_equal(confidence_score(list(route = route, passes = passes),
                                c(6.5, 3.5)), cf)
  expect_error(confidence_score(list(a = 1:3, b = 1:2), c(1, 1)),
               "same length")
})


test_that("evidence_suitability scores, classifies and carries provenance", {
  tol <- .test_tolerances_oe()
  set.seed(15)
  n  <- 400
  df <- data.frame(
    lat = stats::runif(n), lon = stats::runif(n),
    score_depth    = stats::runif(n),
    score_slope    = stats::runif(n),
    score_roughness = stats::runif(n)
  )
  conf <- confidence_score(
    list(route  = sample(c(1, 0.6, 0.3), n, TRUE),
         passes = pmin(pmax(sample(1:3, n, TRUE) / 2, 0.4), 1)),
    c(0.65, 0.35)
  )
  out <- evidence_suitability(
    df, tol,
    reliability = c(depth = 0.93, slope = 0.71, roughness = 0.39),
    confidence  = conf,
    attempted_failed = c("current_velocity", "shear_stress")
  )

  expect_true(all(c("suitability_evidence", "suitability_class_evidence",
                    "confidence") %in% names(out)))
  expect_true(all(out$suitability_evidence >= 0 & out$suitability_evidence <= 1))
  expect_true(any(out$suitability_class_evidence == "Insufficient data"))
  expect_true(all(
    out$suitability_class_evidence[out$confidence < 0.5] == "Insufficient data"
  ))
  # the continuous score survives even where the class does not
  expect_true(all(is.finite(
    out$suitability_evidence[out$suitability_class_evidence == "Insufficient data"]
  )))

  ew <- attr(out, "evidence_weights")
  cv <- attr(out, "evidence_coverage")
  expect_s3_class(ew, "evidence_weights")
  expect_s3_class(cv, "evidence_coverage")
  expect_lt(cv$coverage, 1)
})


test_that("evidence_suitability warns on scores outside [0, 1]", {
  tol <- .test_tolerances_oe()
  df <- data.frame(score_depth = c(-0.2, 0.5, 1.4),
                   score_slope = c(0.1, 0.5, 0.9))
  expect_warning(
    evidence_suitability(df, tol, c(depth = 0.9, slope = 0.7)),
    "outside"
  )
})


test_that("evidence_suitability needs scores to work from", {
  tol <- .test_tolerances_oe()
  expect_error(
    evidence_suitability(data.frame(lat = 1, lon = 1), tol, c(depth = 0.9)),
    "score columns"
  )
})


test_that("weight_table_md renders both tables from the objects", {
  tol <- .test_tolerances_oe()
  set.seed(2)
  df <- data.frame(score_depth = stats::runif(200),
                   score_slope = stats::runif(200))
  out <- evidence_suitability(df, tol, c(depth = 0.93, slope = 0.71),
                              attempted_failed = "current_velocity")
  md <- weight_table_md(out)

  expect_type(md, "character")
  expect_true(any(grepl("Coverage: \\*\\*", md)))
  expect_true(any(grepl("renormalises this to 1.000", md)))
  expect_true(any(grepl("^\\| depth \\|", md)))
  expect_true(any(grepl("costs .* percentage points", md)))

  f <- tempfile(fileext = ".md")
  on.exit(unlink(f), add = TRUE)
  weight_table_md(out, file = f)
  expect_true(file.exists(f))
  expect_identical(readLines(f), md)
})

