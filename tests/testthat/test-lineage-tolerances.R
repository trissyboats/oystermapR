# Simulation harness. Occurrence data are generated from the *deployed* scorer
# (.score_numeric with shoulders scaled to the true optimum, as
# .apply_params_to_tols does) through a logistic link -- the same curve the
# fitter now fits, so "recovery" means recovering what the maps will use.
.sim_lineages <- function(seed, signal, n_each = c(rep(150, 7), rep(14, 3), 0, 0),
                          shuffle = TRUE) {
  base <- get_species_tolerances("ostrea_edulis")
  set.seed(seed)
  if (shuffle) n_each <- sample(n_each)   # thin lineages sit among sampled relatives
  n_lin <- length(n_each)
  x   <- sort(runif(n_lin)); ids <- paste0("L", seq_len(n_lin))
  D   <- as.matrix(dist(x)); dimnames(D) <- list(ids, ids)
  omin <- if (signal) 10 + 5 * x + rnorm(n_lin, 0, 0.2) else runif(n_lin, 10, 15)
  omax <- omin + 6
  recs <- lapply(seq_len(n_lin), function(i) {
    n <- n_each[i]; if (n == 0) return(NULL)
    d <- data.frame(lat = 51 + runif(n) / 10, lon = -4 + runif(n) / 10,
                    temperature = runif(n, 6, 24), lineage_id = ids[i])
    tl <- .apply_params_to_tols(base, list(temperature = list(optimal_min = omin[i],
                                                              optimal_max = omax[i])))
    S <- vapply(d$temperature, .score_numeric, numeric(1),
                params = tl$scored$temperature)
    d$presence <- rbinom(n, 1, plogis(-5 + 10 * S))
    d
  })
  list(records = do.call(rbind, recs), D = D,
       truth = data.frame(lineage = ids, omin = omin, n = n_each, x = x))
}
.quiet_fit <- function(s, ...) {
  fit_lineages(s$records, s$D, "ostrea_edulis", update_vars = "temperature",
               seed = 1, verbose = FALSE, ...)
}
.cleanup_cache <- function() {
  keys <- grep("::", names(.oystermapR_env$posterior_cache), value = TRUE)
  for (k in keys) .oystermapR_env$posterior_cache[[k]] <- NULL
}


# ---- distances ----------------------------------------------------------------

test_that("distance matrices are validated", {
  d <- matrix(c(0, .1, .4, .1, 0, .35, .4, .35, 0), 3,
              dimnames = list(c("A", "B", "C"), c("A", "B", "C")))
  expect_equal(as_lineage_distances(d), d)
  expect_equal(dim(as_lineage_distances(stats::as.dist(d))), c(3, 3))

  df <- data.frame(id = c("A", "B", "C"), A = d[, 1], B = d[, 2], C = d[, 3])
  expect_equal(unname(as_lineage_distances(df)), unname(d))

  f <- tempfile(fileext = ".csv"); utils::write.csv(df, f, row.names = FALSE)
  expect_equal(unname(as_lineage_distances(f)), unname(d))

  bad <- d; bad[1, 2] <- 0.5
  expect_error(as_lineage_distances(bad), "symmetric")
  bad <- d; diag(bad) <- 0.1
  expect_error(as_lineage_distances(bad), "diagonal")
  bad <- d; bad[1, 2] <- bad[2, 1] <- -1
  expect_error(as_lineage_distances(bad), "non-negative")
  bad <- d; bad[1, 2] <- bad[2, 1] <- NA
  expect_error(as_lineage_distances(bad), "missing")
  expect_error(as_lineage_distances(unname(d)), "row and column names")
  expect_error(as_lineage_distances(d[1:2, ]), "square")
})


# ---- signal ------------------------------------------------------------------

test_that("signal test detects structure and refuses when there is none", {
  set.seed(1)
  x <- sort(runif(10)); ids <- paste0("L", 1:10)
  D <- as.matrix(dist(x)); dimnames(D) <- list(ids, ids)
  strong <- cbind(a = 10 + 5 * x + rnorm(10, 0, .2), b = 16 + 5 * x + rnorm(10, 0, .2))
  rownames(strong) <- ids
  none <- matrix(rnorm(20), 10, 2, dimnames = list(ids, c("a", "b")))

  s1 <- lineage_signal(strong, D, n_perm = 499, seed = 1)
  expect_true(s1$usable); expect_gt(s1$r, 0.7); expect_lt(s1$p, 0.01)
  s2 <- lineage_signal(none, D, n_perm = 499, seed = 1)
  expect_false(s2$usable)
  expect_identical(lineage_signal(strong, D, n_perm = 99, seed = 4)$p,
                   lineage_signal(strong, D, n_perm = 99, seed = 4)$p)
})

test_that("too few lineages is not tested, and says why", {
  D <- matrix(c(0, 1, 2, 1, 0, 1, 2, 1, 0), 3, dimnames = list(LETTERS[1:3], LETTERS[1:3]))
  est <- matrix(c(1, 2, 3, 4, 5, 6), 3, dimnames = list(LETTERS[1:3], c("a", "b")))
  s <- lineage_signal(est, D)
  expect_true(is.na(s$r)); expect_false(s$usable); expect_match(s$note, "too few")
  expect_error(lineage_signal(est, D[1:2, 1:2]), "missing from the distance")
})


# ---- fitter hooks --------------------------------------------------------------

test_that("profile fits leave the species-level posterior alone", {
  .cleanup_cache(); on.exit(.cleanup_cache())
  s <- .sim_lineages(3, TRUE, n_each = rep(150, 12), shuffle = FALSE)
  d <- s$records[s$records$lineage_id == "L6", ]
  before <- .oystermapR_env$posterior_cache[["ostrea_edulis"]]
  f <- suppressMessages(suppressWarnings(update_species_tolerances(
    d, "ostrea_edulis", update_vars = "temperature", verbose = FALSE,
    profile = "L6")))
  expect_identical(.oystermapR_env$posterior_cache[["ostrea_edulis"]], before)
  expect_false(is.null(.oystermapR_env$posterior_cache[["ostrea_edulis::L6"]]))
  expect_identical(f$profile, "L6")
  expect_true(is.list(f$tolerances))
})

test_that("posterior SDs and intervals are no longer NA", {
  .cleanup_cache(); on.exit(.cleanup_cache())
  s <- .sim_lineages(3, TRUE, n_each = rep(150, 12), shuffle = FALSE)
  f <- suppressMessages(suppressWarnings(update_species_tolerances(
    s$records[s$records$lineage_id == "L6", ], "ostrea_edulis",
    update_vars = "temperature", verbose = FALSE, profile = "sd")))
  u <- f$updated_params$temperature
  expect_true(is.finite(u$posterior_sd_omin) && u$posterior_sd_omin > 0)
  expect_true(all(is.finite(u$ci95_omin)))
})

test_that("a supplied prior pulls a data-poor fit toward it", {
  .cleanup_cache(); on.exit(.cleanup_cache())
  s <- .sim_lineages(3, TRUE, n_each = rep(150, 12), shuffle = FALSE)
  d <- s$records[s$records$lineage_id == "L3", ][1:20, ]
  fit <- function(pr) suppressMessages(suppressWarnings(update_species_tolerances(
    d, "ostrea_edulis", update_vars = "temperature", min_records = 10,
    verbose = FALSE, profile = "p", prior = pr)))$updated_params$temperature$optimal_min
  default <- fit(NULL)
  .cleanup_cache()
  pulled  <- fit(list(temperature = list(optimal_min = c(13, 0.3), optimal_max = c(19, 0.3))))
  expect_gt(abs(pulled - 13) * -1 + abs(default - 13), 0)   # closer to 13 than the default fit
})


# ---- forward: fit ------------------------------------------------------------

test_that("input problems are reported plainly", {
  .cleanup_cache(); on.exit(.cleanup_cache())
  s <- .sim_lineages(2, TRUE)
  expect_error(fit_lineages(s$records, s$D, "ostrea_edulis", lineage_col = "nope"),
               "not found")
  r <- s$records; r$lineage_id[1] <- "ZZ"
  expect_error(fit_lineages(r, s$D, "ostrea_edulis", update_vars = "temperature"),
               "not in the distance matrix")
  r <- s$records; r$lineage_id[1] <- NA
  expect_error(fit_lineages(r, s$D, "ostrea_edulis", update_vars = "temperature"),
               "missing values")
  expect_error(fit_lineages(s$records, s$D, "ostrea_edulis", min_records = 5,
                            min_sparse = 10), "cannot exceed")
})

test_that("with real signal, pooling is used and helps the thin lineages", {
  .cleanup_cache(); on.exit(.cleanup_cache())
  err_pooled <- err_alone <- err_borrowed <- err_default <- numeric(0)
  rich_err <- numeric(0)
  for (seed in 2:4) {
    s <- .sim_lineages(seed, TRUE)
    f <- .quiet_fit(s)
    expect_true(f$pooled, info = paste("seed", seed))
    expect_lt(f$signal$p, 0.05)

    pr <- f$profiles[f$profiles$parameter == "optimal_min", ]
    pr <- merge(pr[, c("lineage", "source", "mean")], s$truth, by = "lineage")
    pr$err <- abs(pr$mean - pr$omin)
    thin <- pr$n == 14
    err_pooled <- c(err_pooled, pr$err[thin])
    err_alone  <- c(err_alone, vapply(pr$lineage[thin], function(k) {
      a <- suppressMessages(suppressWarnings(update_species_tolerances(
        s$records[s$records$lineage_id == k, ], "ostrea_edulis",
        update_vars = "temperature", min_records = 8, verbose = FALSE,
        profile = paste0(k, "_alone"))))
      abs(a$updated_params$temperature$optimal_min - pr$omin[pr$lineage == k])
    }, numeric(1)))
    none <- pr$n == 0
    expect_true(all(pr$source[none] == "borrowed"))
    err_borrowed <- c(err_borrowed, pr$err[none])
    err_default  <- c(err_default, abs(10 - pr$omin[none]))
    rich_err <- c(rich_err, pr$err[pr$n == 150])
    .cleanup_cache()
  }
  expect_lt(mean(err_pooled), mean(err_alone))       # thin lineages beat fitting alone
  expect_lt(mean(err_borrowed), mean(err_default))   # unsampled beat the species default
  expect_lt(mean(rich_err), 0.5)                     # well-sampled lineages recovered (to well under a degree)
})

test_that("with no signal, lineages are fitted alone and thin ones keep the default", {
  .cleanup_cache(); on.exit(.cleanup_cache())
  for (seed in 1:3) {
    s <- .sim_lineages(seed, FALSE)
    f <- .quiet_fit(s)
    expect_false(f$pooled, info = paste("seed", seed))
    expect_false(is.null(f$pool_reason))
    src <- unique(f$profiles[, c("lineage", "n_records", "source")])
    expect_true(all(src$source[src$n_records >= 20] == "own"))
    expect_true(all(src$source[src$n_records < 20] == "default"))
  }
})

test_that("pool = 'never' and too few lineages both refuse to pool", {
  .cleanup_cache(); on.exit(.cleanup_cache())
  s <- .sim_lineages(2, TRUE)
  expect_false(.quiet_fit(s, pool = "never")$pooled)
  few <- .sim_lineages(2, TRUE, n_each = c(150, 150, 150, 14, 0))
  f <- .quiet_fit(few)
  expect_false(f$pooled); expect_match(f$pool_reason, "min_lineages")
})

test_that("fitting leaves no lineage keys behind in the session cache", {
  .cleanup_cache()
  s <- .sim_lineages(2, TRUE)
  .quiet_fit(s)
  expect_length(grep("::", names(.oystermapR_env$posterior_cache)), 0)
})


# ---- inverse ---------------------------------------------------------------------

test_that("lineage_occurrence gives a proper posterior and honest labels", {
  .cleanup_cache(); on.exit(.cleanup_cache())
  s <- .sim_lineages(3, TRUE)
  f <- .quiet_fit(s)
  grid <- data.frame(lat = 51, lon = seq(-4, -3.9, length.out = 9),
                     temperature = c(4, 8, 10, 12, 14, 16, 18, 22, 30))
  occ <- lineage_occurrence(f, grid)

  post <- as.matrix(occ[, grep("^post_", names(occ))])
  expect_equal(unname(rowSums(post)[!occ$excluded]), rep(1, sum(!occ$excluded)))
  expect_true(all(is.na(post[occ$excluded, ])))
  expect_equal(occ$lineage_class[occ$temperature %in% c(4, 30)], c("Excluded", "Excluded"))
  expect_true(all(occ$lineage_ambiguity >= 0 & occ$lineage_ambiguity <= 1, na.rm = TRUE))
  expect_true(all(occ$lineage_prob >= 1 / nrow(f$calibration) - 1e-9, na.rm = TRUE))
  expect_true(all(occ$lineage_source[!occ$excluded] %in% c("own", "pooled", "borrowed", "default")))

  # A loose threshold names more cells than a strict one.
  loose  <- lineage_occurrence(f, grid, ambiguity_threshold = 0)
  strict <- lineage_occurrence(f, grid, ambiguity_threshold = 0.99)
  expect_gte(sum(loose$lineage_class %in% f$calibration$lineage),
             sum(strict$lineage_class %in% f$calibration$lineage))
  expect_true(all(strict$lineage_class[!strict$excluded] == "Ambiguous"))
})

test_that("cold-adapted and warm-adapted lineages win where they should", {
  .cleanup_cache(); on.exit(.cleanup_cache())
  s <- .sim_lineages(3, TRUE, n_each = rep(150, 12), shuffle = FALSE)
  f <- .quiet_fit(s)
  # Two sharply different lineages only: coldest and warmest.
  occ <- lineage_occurrence(f, data.frame(lat = 51, lon = -4:-3 / 10, temperature = c(7.5, 23)),
                            ambiguity_threshold = 0)
  expect_true(occ$lineage_best[1] %in% s$truth$lineage[order(s$truth$omin)][1:4])
  expect_true(occ$lineage_best[2] %in% s$truth$lineage[order(-s$truth$omin)][1:4])
})

test_that("shared calibration puts lineages on one curve; per-lineage does not", {
  .cleanup_cache(); on.exit(.cleanup_cache())
  s <- .sim_lineages(3, TRUE)
  shared <- .quiet_fit(s)
  expect_equal(length(unique(round(shared$calibration$alpha, 8))), 1L)
  expect_equal(length(unique(round(shared$calibration$beta, 8))), 1L)
  expect_gt(shared$calibration$beta[1], 0)
  per <- .quiet_fit(s, calibration = "lineage")
  expect_gt(length(unique(round(per$calibration$alpha, 4))), 1L)
})

test_that("argument errors are clear", {
  .cleanup_cache(); on.exit(.cleanup_cache())
  s <- .sim_lineages(2, TRUE); f <- .quiet_fit(s)
  g <- data.frame(lat = 51, lon = -4, temperature = 15)
  expect_error(lineage_occurrence(list(), g), "fit_lineages")
  expect_error(lineage_occurrence(f, g, prior_share = c(a = 1)), "named")
  expect_error(lineage_occurrence(f, g, prior_share = "bogus"), "prior_share")
})


# ---- source ranking ----------------------------------------------------------------

test_that("source ranking orders lineages and applies the translocation discount", {
  .cleanup_cache(); on.exit(.cleanup_cache())
  s <- .sim_lineages(3, TRUE); f <- .quiet_fit(s)
  sites <- data.frame(site_id = c("a", "b", "hot"), lat = 51, lon = -4,
                      temperature = c(11, 15, 28))
  L <- nrow(f$calibration)

  r0 <- rank_source_populations(f, sites)
  expect_equal(nrow(r0), 2 * L)                       # the hot site is excluded
  expect_identical(attr(r0, "excluded_sites"), "hot")
  for (id in c("a", "b")) {
    ri <- r0[r0$site_id == id, ]
    expect_equal(ri$rank, seq_len(L))
    expect_false(is.unsorted(rev(ri$score)))
    expect_equal(sum(ri$posterior_share), 1)
  }
  expect_true(all(r0$translocation_weight == 1))

  local <- f$calibration$lineage[1]
  r2 <- rank_source_populations(f, sites, local_lineage = local, distance_penalty = 3)
  ri <- r2[r2$site_id == "a", ]
  expect_equal(ri$translocation_weight[ri$lineage == local], 1)
  expect_true(all(ri$translocation_weight <= 1))
  far <- ri$lineage[which.max(ri$distance_to_local)]
  expect_lt(ri$translocation_weight[ri$lineage == far], 0.1)
  expect_equal(ri$score, ri$p_occurrence * ri$translocation_weight)

  expect_error(rank_source_populations(f, sites, distance_penalty = 1), "local_lineage")
  expect_error(rank_source_populations(f, sites, local_lineage = "nope"), "local_lineage")
  expect_error(rank_source_populations(f, sites, distance_penalty = -1), "non-negative")
})

test_that("a per-site local lineage column is honoured", {
  .cleanup_cache(); on.exit(.cleanup_cache())
  s <- .sim_lineages(3, TRUE); f <- .quiet_fit(s)
  ids <- f$calibration$lineage
  sites <- data.frame(site_id = c("a", "b"), lat = 51, lon = -4, temperature = c(12, 16),
                      native = ids[c(1, 5)])
  r <- rank_source_populations(f, sites, local_lineage = "native", distance_penalty = 1)
  expect_equal(r$translocation_weight[r$site_id == "a" & r$lineage == ids[1]], 1)
  expect_equal(r$translocation_weight[r$site_id == "b" & r$lineage == ids[5]], 1)
  expect_lt(r$translocation_weight[r$site_id == "a" & r$lineage == ids[5]], 1)
})

test_that("posterior draws add tie-aware top shares", {
  .cleanup_cache(); on.exit(.cleanup_cache())
  s <- .sim_lineages(3, TRUE); f <- .quiet_fit(s)
  sites <- data.frame(site_id = "a", lat = 51, lon = -4, temperature = 12)
  r <- rank_source_populations(f, sites, n_draws = 20, seed = 1)
  expect_true(all(c("p_top", "score_lo", "score_hi") %in% names(r)))
  expect_true(all(r$p_top >= 0 & r$p_top <= 1))
  expect_gte(sum(r$p_top), 1)                    # at least one lineage is always top
  expect_true(all(r$score_lo <= r$score_hi + 1e-12))
  expect_error(rank_source_populations(f, sites, n_draws = 1), "at least 2")
})


# ---- storing and restoring ----------------------------------------------------------

test_that("a model survives a JSON round trip and scores the same", {
  .cleanup_cache(); on.exit(.cleanup_cache())
  s <- .sim_lineages(3, TRUE); f <- .quiet_fit(s)
  json <- jsonlite::toJSON(lineage_fits_as_list(f), auto_unbox = TRUE, null = "null",
                           na = "null", digits = NA)
  back <- lineage_fits_from_list(jsonlite::fromJSON(json, simplifyVector = FALSE))

  expect_s3_class(back, "lineage_fits")
  expect_identical(names(back$tolerances), names(f$tolerances))
  expect_equal(back$distances, f$distances)
  expect_equal(back$calibration$alpha, f$calibration$alpha)
  expect_equal(unname(back$n_records), unname(f$n_records))
  expect_identical(back$pooled, f$pooled)

  grid <- data.frame(lat = 51, lon = seq(-4, -3.9, length.out = 6),
                     temperature = c(8, 10, 12, 15, 18, 22))
  a <- lineage_occurrence(f, grid); b <- lineage_occurrence(back, grid)
  expect_equal(b$lineage_best, a$lineage_best)
  expect_equal(b$lineage_prob, a$lineage_prob, tolerance = 1e-3)
  post <- grep("^post_", names(a), value = TRUE)
  expect_equal(as.matrix(b[post]), as.matrix(a[post]), tolerance = 1e-3)

  sites <- data.frame(site_id = c("a", "b"), lat = 51, lon = -4, temperature = c(12, 16))
  ra <- rank_source_populations(f, sites, local_lineage = names(f$tolerances)[1], distance_penalty = 2)
  rb <- rank_source_populations(back, sites, local_lineage = names(f$tolerances)[1], distance_penalty = 2)
  expect_equal(rb$lineage, ra$lineage)
  expect_equal(rb$score, ra$score, tolerance = 1e-3)
})

test_that("restoring a malformed model says what is missing", {
  expect_error(lineage_fits_from_list(list(species = "ostrea_edulis")), "missing")
  expect_error(lineage_fits_as_list(list()), "fit_lineages")
})
