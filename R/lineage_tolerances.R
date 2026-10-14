# =============================================================================
# Lineage tolerance profiles and inverse lineage mapping
#
# Forward:  one tolerance profile per population / lineage, with sparse
#           lineages borrowing from relatives -- but only as far as the data
#           show that tolerance tracks relatedness.
# Inverse:  given the profiles, where is each lineage more likely to occur
#           than the others, and which source population suits a given site.
#
# Everything here is bring-your-own-data: occurrence records that carry a
# lineage identifier, and a pairwise genetic distance matrix between lineages.
# Nothing is shipped or assumed about any particular species' population
# structure.
# =============================================================================


# ---- distances --------------------------------------------------------------

#' Validate a genetic distance matrix between lineages
#'
#' @description
#' Accepts a matrix, a `dist` object, a data frame (first column lineage ids,
#' or row names), or the path to a CSV in that layout, and returns a checked
#' symmetric numeric matrix with lineage ids as dimnames. Fst, p-distance,
#' patristic distance from a tree (`ape::cophenetic()`) -- anything that
#' is a non-negative pairwise dissimilarity works; the scale does not matter.
#'
#' @param x Matrix, `dist`, data frame or CSV path.
#'
#' @return A numeric matrix with identical row and column names.
#' @export
#' @examples
#' d <- matrix(c(0, .1, .4, .1, 0, .35, .4, .35, 0), 3,
#'             dimnames = list(c("A", "B", "C"), c("A", "B", "C")))
#' as_lineage_distances(d)
as_lineage_distances <- function(x) {

  if (is.character(x) && length(x) == 1L && file.exists(x)) {
    x <- utils::read.csv(x, check.names = FALSE, stringsAsFactors = FALSE)
  }
  if (inherits(x, "dist")) x <- as.matrix(x)
  if (is.data.frame(x)) {
    if (!is.numeric(x[[1]])) {
      ids <- as.character(x[[1]])
      x   <- x[-1]
    } else {
      ids <- rownames(x)
    }
    x <- as.matrix(x)
    rownames(x) <- ids
  }
  if (!is.matrix(x) || !is.numeric(x)) {
    cli::cli_abort("{.arg distances} must be a numeric matrix, {.cls dist}, data frame or CSV path.")
  }
  if (nrow(x) != ncol(x)) {
    cli::cli_abort("Distance matrix must be square; got {nrow(x)} x {ncol(x)}.")
  }
  if (is.null(rownames(x)) || is.null(colnames(x)) ||
      anyNA(rownames(x)) || any(!nzchar(rownames(x)))) {
    cli::cli_abort("Distance matrix needs lineage ids as row and column names.")
  }
  if (anyDuplicated(rownames(x))) {
    cli::cli_abort("Duplicate lineage ids in the distance matrix: {.val {unique(rownames(x)[duplicated(rownames(x))])}}.")
  }
  if (!identical(rownames(x), colnames(x))) {
    if (setequal(rownames(x), colnames(x))) {
      x <- x[rownames(x), rownames(x), drop = FALSE]
    } else {
      cli::cli_abort("Row and column names of the distance matrix differ.")
    }
  }
  if (anyNA(x) || any(!is.finite(x))) {
    cli::cli_abort("Distance matrix contains missing or non-finite values.")
  }
  if (any(x < 0)) cli::cli_abort("Distances must be non-negative.")
  scale <- max(x, 1e-12)
  if (max(abs(x - t(x))) > 1e-6 * scale) {
    cli::cli_abort("Distance matrix is not symmetric.")
  }
  if (max(abs(diag(x))) > 1e-6 * scale) {
    cli::cli_abort("Distance matrix diagonal must be zero.")
  }
  x <- (x + t(x)) / 2
  diag(x) <- 0
  x
}


# ---- signal -----------------------------------------------------------------

#' Does tolerance track relatedness?
#'
#' @description
#' A Mantel test between the pairwise distance in fitted tolerance parameters
#' and the pairwise genetic distance. Borrowing tolerance from relatives is
#' only justified if relatives really are more alike; this is the measurement.
#' A significant positive correlation supports pooling. No signal means the
#' lineages are best fitted alone: genetic similarity is not tolerance
#' similarity, and local adaptation can decouple them.
#'
#' @param estimates Numeric matrix, one row per lineage (named) and one column
#'   per fitted parameter (e.g. `temperature.optimal_min`).
#' @param distances Genetic distance matrix ([as_lineage_distances()]).
#' @param n_perm Number of permutations for the p-value.
#' @param min_lineages Fewer fitted lineages than this and the test is not run:
#'   with so few, it has little power (five lineages give ten pairs) and the
#'   answer would mean nothing. Default 6.
#' @param alpha Significance level used for `usable`.
#' @param seed Optional integer for reproducible permutations.
#'
#' @return An object of class `lineage_signal`: `r`, `p`, `n_lineages`,
#'   `n_perm`, `usable` (enough lineages, r > 0 and p < alpha) and `note`.
#' @export
lineage_signal <- function(estimates, distances, n_perm = 999L,
                           min_lineages = 6L, alpha = 0.05, seed = NULL) {

  distances <- as_lineage_distances(distances)
  estimates <- as.matrix(estimates)
  ids <- rownames(estimates)
  if (is.null(ids)) cli::cli_abort("{.arg estimates} needs lineage ids as row names.")
  miss <- setdiff(ids, rownames(distances))
  if (length(miss) > 0) {
    cli::cli_abort("Lineage{?s} missing from the distance matrix: {.val {miss}}.")
  }
  n <- length(ids)

  out <- list(r = NA_real_, p = NA_real_, n_lineages = n, n_perm = n_perm,
              usable = FALSE, note = NA_character_)
  class(out) <- "lineage_signal"

  if (n < min_lineages) {
    out$note <- sprintf(
      "only %d lineage%s with a fitted profile (min_lineages = %d): too few for the test to mean anything",
      n, if (n == 1) "" else "s", min_lineages)
    return(out)
  }

  keep <- apply(estimates, 2, function(v) stats::sd(v) > 0)
  if (!any(keep)) {
    out$note <- "fitted parameters do not differ between lineages"
    return(out)
  }
  z  <- scale(estimates[, keep, drop = FALSE])
  pd <- as.matrix(stats::dist(z))
  gd <- distances[ids, ids, drop = FALSE]
  lt <- lower.tri(pd)

  if (stats::sd(gd[lt]) == 0) {
    out$note <- "all genetic distances are equal"
    return(out)
  }
  r_obs <- suppressWarnings(stats::cor(pd[lt], gd[lt]))
  if (!is.finite(r_obs)) {
    out$note <- "correlation not defined"
    return(out)
  }

  if (!is.null(seed)) {
    old <- if (exists(".Random.seed", envir = globalenv())) get(".Random.seed", envir = globalenv()) else NULL
    on.exit(if (!is.null(old)) assign(".Random.seed", old, envir = globalenv()), add = TRUE)
    set.seed(seed)
  }
  r_perm <- vapply(seq_len(n_perm), function(i) {
    o <- sample.int(n)
    suppressWarnings(stats::cor(pd[lt], gd[o, o][lt]))
  }, numeric(1))

  out$r <- r_obs
  out$p <- (1 + sum(r_perm >= r_obs, na.rm = TRUE)) / (n_perm + 1)
  out$usable <- r_obs > 0 && out$p < alpha
  out$note <- if (out$usable) NA_character_ else
    "no detectable tendency for related lineages to have similar tolerances"
  out
}

#' @export
print.lineage_signal <- function(x, ...) {
  cli::cli_h3("Phylogenetic signal in tolerance")
  if (is.na(x$r)) {
    cli::cli_inform("  Not tested: {x$note}.")
  } else {
    cli::cli_inform(c(
      "  Mantel r = {sprintf('%+.3f', x$r)}, p = {sprintf('%.3f', x$p)} ({x$n_lineages} lineages, {x$n_perm} permutations)",
      "  {if (x$usable) 'Related lineages have more similar tolerances: pooling is supported.' else paste0('Pooling not supported: ', x$note, '.')}"
    ))
  }
  invisible(x)
}


# ---- pooling ----------------------------------------------------------------

# Gaussian-process hyperparameters shared across parameters:
#   f_p ~ N(m_p 1, tau_p^2 K(rho)),  K = exp(-D / rho),  y_p = f_p + N(0, s_p^2)
# Estimated by marginal likelihood over the fitted lineages. m_p is profiled
# out in closed form.
.gp_hyper <- function(y, s, D, floor_sd) {
  P <- ncol(y)
  dpos <- D[D > 0]
  dmed <- stats::median(dpos); dmax <- max(dpos)
  rho_lo <- log(0.05 * dmed); rho_hi <- log(20 * dmax)

  .nll_one <- function(yp, sp, rho, tau) {
    K <- exp(-D / rho)
    S <- tau^2 * K + diag(sp^2, nrow(D))
    ch <- tryCatch(chol(S + diag(1e-10, nrow(D))), error = function(e) NULL)
    if (is.null(ch)) return(list(nll = 1e10, m = mean(yp)))
    one <- rep(1, length(yp))
    Si1 <- backsolve(ch, forwardsolve(t(ch), one))
    Siy <- backsolve(ch, forwardsolve(t(ch), yp))
    m   <- sum(Siy) / sum(Si1)
    r   <- yp - m
    Sir <- backsolve(ch, forwardsolve(t(ch), r))
    list(nll = sum(log(diag(ch))) + 0.5 * sum(r * Sir), m = m)
  }

  f <- function(par) {
    rho <- exp(par[1])
    sum(vapply(seq_len(P), function(p)
      .nll_one(y[, p], s[, p], rho, exp(par[1 + p]))$nll, numeric(1)))
  }
  # tau is kept off zero: hyperparameters estimated from a handful of
  # lineages are imprecise, and an estimate on the boundary would otherwise
  # turn into a prior so tight it pins every lineage to the common mean.
  tau_lo <- log(0.1 * floor_sd)
  sd0  <- pmax(apply(y, 2, stats::sd), 0.2 * floor_sd)
  par0 <- c(log(dmed), log(sd0))
  lo   <- c(rho_lo, tau_lo)
  hi   <- c(rho_hi, log(pmax(sd0, floor_sd)) + 3)
  opt  <- stats::optim(par0, f, method = "L-BFGS-B", lower = lo, upper = hi)

  rho <- exp(opt$par[1]); tau <- exp(opt$par[-1])
  m <- vapply(seq_len(P), function(p) .nll_one(y[, p], s[, p], rho, tau[p])$m,
              numeric(1))
  list(rho = rho, tau = stats::setNames(tau, colnames(y)),
       m = stats::setNames(m, colnames(y)), converged = opt$convergence == 0)
}

# Prior for one lineage from the fitted lineages other than itself.
.gp_prior <- function(k, y, s, D, hyper, floor_sd) {
  others <- setdiff(rownames(y), k)
  K_all <- exp(-D / hyper$rho)
  out <- lapply(colnames(y), function(pn) {
    tau <- hyper$tau[[pn]]; m <- hyper$m[[pn]]
    S  <- tau^2 * K_all[others, others, drop = FALSE] +
      diag(s[others, pn]^2, length(others))
    cv <- tau^2 * K_all[k, others]
    w  <- solve(S + diag(1e-10, length(others)), cbind(cv, y[others, pn] - m))
    mu <- m + sum(w[, 2] * cv)
    v  <- tau^2 - sum(w[, 1] * cv)
    c(mean = mu, sd = max(sqrt(max(v, 0)), 0.25 * floor_sd[[pn]]))
  })
  stats::setNames(out, colnames(y))
}

# Apply new optimal bounds to a tolerance object, shoulders scaling with the
# range as in update_species_tolerances().
.apply_params_to_tols <- function(tols, params) {
  for (v in names(params)) {
    p <- tols$scored[[v]]
    if (is.null(p)) next
    new_min <- params[[v]][["optimal_min"]]
    new_max <- params[[v]][["optimal_max"]]
    if (!(new_max > new_min)) new_max <- new_min + 1e-3
    old_rng <- max(p$optimal_max - p$optimal_min, 1e-3)
    scale   <- max(new_max - new_min, 1e-3) / old_rng
    upd <- list(
      optimal_min    = new_min,
      optimal_max    = new_max,
      poor_min       = if (!is.null(p$poor_min))       new_min - (p$optimal_min - p$poor_min) * scale,
      acceptable_min = if (!is.null(p$acceptable_min)) new_min - (p$optimal_min - p$acceptable_min) * scale,
      acceptable_max = if (!is.null(p$acceptable_max)) new_max + (p$acceptable_max - p$optimal_max) * scale,
      poor_max       = if (!is.null(p$poor_max))       new_max + (p$poor_max - p$optimal_max) * scale
    )
    for (f in names(upd)) if (!is.null(upd[[f]])) tols$scored[[v]][[f]] <- upd[[f]]
  }
  tols
}


# ---- forward: fit -------------------------------------------------------------

#' Fit tolerance profiles for a set of related lineages
#'
#' @description
#' Fits one tolerance profile per lineage from occurrence records carrying a
#' lineage identifier, using the same Bayesian update as
#' [update_species_tolerances()]. Where the data show that tolerance tracks
#' genetic relatedness, sparse lineages borrow a prior from their relatives,
#' and lineages with no records at all get a profile predicted from genetic
#' distance alone.
#'
#' @section How pooling is decided:
#' 1. Lineages with at least `min_records` records are fitted alone.
#' 2. [lineage_signal()] tests whether their fitted parameters are more alike
#'    between close relatives. With `pool = "auto"` (default), pooling happens
#'    only if there are at least `min_lineages` fitted lineages **and** the
#'    signal is positive and significant. Otherwise every lineage is left on
#'    its own fit, and lineages too sparse to fit are returned with the
#'    species default, flagged as such.
#' 3. When pooling, a Gaussian-process prior with covariance decaying with
#'    genetic distance is estimated from the fitted lineages. Each lineage's
#'    prior is what its relatives (never itself) predict. Lineages with at
#'    least `min_sparse` records are refitted under that prior; lineages with
#'    fewer take the prior as their profile (`source = "borrowed"`), with the
#'    wide uncertainty that implies.
#'
#' @section Circularity:
#' Lineage identifiers must come from genetics alone. If lineages are defined
#' by where they were sampled, the inverse map recovers the sampling, not
#' anything about the populations.
#'
#' @section Limits:
#' Borrowing works by interpolation. A lineage further out than any of its
#' fitted relatives on the distance scale is predicted no better than the
#' species default, because a distance-decay prior cannot extrapolate a trend;
#' it is flagged `borrowed` and is a hypothesis. Below about six fitted
#' lineages the signal test has little power and pooling is refused. The
#' fitter itself is bounded: an optimum more than about three prior standard
#' deviations from the species default cannot be reached.
#'
#' @param records Data frame of occurrence records with `lat`, `lon`, a
#'   presence/absence column, the environmental columns to fit, and
#'   `lineage_col`.
#' @param distances Pairwise genetic distance matrix between lineages
#'   ([as_lineage_distances()]). Lineages in the matrix with no records are
#'   allowed and receive borrowed profiles when pooling is on.
#' @param species Species key, e.g. `"ostrea_edulis"`.
#' @param lineage_col Name of the lineage column.
#' @param presence_col Name of the presence/absence column.
#' @param update_vars Variables to fit; `NULL` fits every updatable variable
#'   present in `records`.
#' @param pool `"auto"` (default), `"always"` (skip the signal test) or
#'   `"never"`.
#' @param min_records Records needed to fit a lineage alone.
#' @param min_sparse Records needed to refit a lineage under a borrowed prior;
#'   below this it takes the prior itself.
#' @param min_lineages Fitted lineages needed before any pooling.
#' @param alpha Significance level for the signal test.
#' @param n_perm Permutations for the signal test.
#' @param calibration How suitability is turned into occurrence probability.
#'   `"shared"` (default) fits one logistic curve pooled over every lineage's
#'   records, each scored against its own profile, so lineages differ only in
#'   their tolerances. `"lineage"` fits one per lineage, which lets each
#'   lineage's sampling prevalence into the comparison: a lineage that
#'   happened to be sampled at good sites looks more widespread than one
#'   sampled at poor ones, and that is not a property of the populations.
#' @param prior_sd_fraction,method Passed to [update_species_tolerances()].
#' @param seed Optional integer for reproducible permutations.
#' @param verbose Print progress.
#'
#' @return An object of class `lineage_fits`: `profiles` (long table of every
#'   parameter with its mean, sd and `source`), `tolerances` (a scoring-ready
#'   tolerance object per lineage), `calibration` (per-lineage logistic
#'   intercept and gain), `signal`, `hyper`, `pooled`, `pool_reason`,
#'   `distances`, `n_records`. `source` is `"own"`, `"pooled"`, `"borrowed"` or
#'   `"default"`.
#'
#' @seealso [lineage_occurrence()], [rank_source_populations()],
#'   [lineage_signal()]
#' @export
fit_lineages <- function(records,
                         distances,
                         species,
                         lineage_col       = "lineage_id",
                         presence_col      = "presence",
                         update_vars       = NULL,
                         pool              = c("auto", "always", "never"),
                         min_records       = 20L,
                         min_sparse        = 8L,
                         min_lineages      = 6L,
                         alpha             = 0.05,
                         n_perm            = 999L,
                         calibration       = c("shared", "lineage"),
                         prior_sd_fraction = 0.20,
                         method            = "map",
                         seed              = NULL,
                         verbose           = TRUE) {

  pool <- match.arg(pool)
  calibration <- match.arg(calibration)
  distances <- as_lineage_distances(distances)
  records <- as.data.frame(records, stringsAsFactors = FALSE)
  if (!lineage_col %in% names(records)) {
    cli::cli_abort("Lineage column {.field {lineage_col}} not found in {.arg records}.")
  }
  if (min_sparse > min_records) {
    cli::cli_abort("{.arg min_sparse} ({min_sparse}) cannot exceed {.arg min_records} ({min_records}).")
  }
  lin_rec <- as.character(records[[lineage_col]])
  if (anyNA(lin_rec) || any(!nzchar(lin_rec))) {
    cli::cli_abort("{.field {lineage_col}} has missing values; every record needs a lineage.")
  }
  unknown <- setdiff(unique(lin_rec), rownames(distances))
  if (length(unknown) > 0) {
    cli::cli_abort(c(
      "Lineage{?s} in {.arg records} but not in the distance matrix: {.val {unknown}}.",
      "i" = "Every lineage with records needs a row and column in the distance matrix."
    ))
  }
  all_ids <- rownames(distances)
  n_rec <- vapply(all_ids, function(k) {
    sum(lin_rec == k & !is.na(records[[presence_col]]))
  }, numeric(1))
  names(n_rec) <- all_ids

  base_tols <- get_species_tolerances(species)
  cache_keys <- character(0)
  .fit <- function(k, prior = NULL, min_n = min_records, tag = "") {
    key <- paste0(k, tag)
    cache_keys <<- c(cache_keys, paste0(species, "::", key))
    suppressMessages(suppressWarnings(update_species_tolerances(
      records[lin_rec == k, , drop = FALSE], species,
      update_vars = update_vars, presence_col = presence_col, method = method,
      prior_sd_fraction = prior_sd_fraction, min_records = min_n,
      verbose = FALSE, profile = key, prior = prior)))
  }
  on.exit({
    for (key in unique(cache_keys)) .oystermapR_env$posterior_cache[[key]] <- NULL
  }, add = TRUE)

  # ---- 1. lineages with enough data are fitted alone -------------------------
  fit0 <- list(); failed <- character(0)
  for (k in all_ids[n_rec >= min_records]) {
    res <- tryCatch(.fit(k, tag = "__alone"), error = function(e) e)
    if (inherits(res, "error")) failed <- c(failed, k) else fit0[[k]] <- res
  }
  if (length(failed) > 0 && verbose) {
    cli::cli_warn("Could not fit {length(failed)} lineage{?s} alone: {.val {failed}}.")
  }
  if (length(fit0) == 0) {
    cli::cli_abort(c(
      "No lineage has enough usable records to fit ({.arg min_records} = {min_records}).",
      "i" = "Records per lineage: {paste(names(n_rec), n_rec, sep = '=', collapse = ', ')}."
    ))
  }

  vars <- Reduce(intersect, lapply(fit0, function(f) names(f$updated_params)))
  if (length(vars) == 0) cli::cli_abort("Fitted lineages share no updated variable.")
  pnames <- as.vector(rbind(paste0(vars, ".optimal_min"), paste0(vars, ".optimal_max")))

  est <- t(vapply(fit0, function(f) {
    unlist(lapply(vars, function(v) c(f$updated_params[[v]]$optimal_min,
                                      f$updated_params[[v]]$optimal_max)))
  }, numeric(2 * length(vars))))
  colnames(est) <- pnames
  se <- t(vapply(fit0, function(f) {
    unlist(lapply(vars, function(v) {
      d <- f$prior_params[[v]]
      rng <- max(d$optimal_max - d$optimal_min, 1e-3)
      pick <- function(x) if (is.finite(x) && x > 0) x else rng * prior_sd_fraction
      c(pick(f$updated_params[[v]]$posterior_sd_omin),
        pick(f$updated_params[[v]]$posterior_sd_omax))
    }))
  }, numeric(2 * length(vars))))
  colnames(se) <- pnames
  se <- pmax(se, 1e-3)
  # The species default prior's width per parameter: the yardstick for floors.
  floor_sd <- stats::setNames(rep(vapply(vars, function(v) {
    b <- base_tols$scored[[v]]
    max(b$optimal_max - b$optimal_min, 1e-3) * prior_sd_fraction
  }, numeric(1)), each = 2), pnames)

  # ---- 2. does tolerance track relatedness? ---------------------------------
  fitted_ids <- rownames(est)
  sig <- lineage_signal(est, distances, n_perm = n_perm,
                        min_lineages = min_lineages, alpha = alpha, seed = seed)

  do_pool <- FALSE
  reason  <- NULL
  if (pool == "never") {
    reason <- "pool = 'never'"
  } else if (length(fitted_ids) < min_lineages) {
    reason <- sprintf("only %d lineage%s with enough records (min_lineages = %d)",
                      length(fitted_ids), if (length(fitted_ids) == 1) "" else "s",
                      min_lineages)
  } else if (pool == "auto" && !isTRUE(sig$usable)) {
    reason <- sig$note
  } else {
    do_pool <- TRUE
  }

  # ---- 3. profiles -----------------------------------------------------------
  calib <- data.frame(lineage = all_ids, alpha = NA_real_, beta = NA_real_,
                      stringsAsFactors = FALSE)
  tols <- stats::setNames(vector("list", length(all_ids)), all_ids)
  prof <- list()
  add_prof <- function(k, source, means, sds, pm = NULL, ps = NULL) {
    for (v in vars) for (par in c("optimal_min", "optimal_max")) {
      pn <- paste0(v, ".", par)
      prof[[length(prof) + 1L]] <<- data.frame(
        lineage = k, n_records = n_rec[[k]], source = source, variable = v,
        parameter = par, mean = means[[pn]], sd = sds[[pn]],
        prior_mean = if (length(pm)) pm[[pn]] else NA_real_,
        prior_sd   = if (length(ps)) ps[[pn]] else NA_real_,
        stringsAsFactors = FALSE)
    }
  }
  fit_summary <- function(f) {
    m <- unlist(lapply(vars, function(v) c(f$updated_params[[v]]$optimal_min,
                                           f$updated_params[[v]]$optimal_max)))
    s <- unlist(lapply(vars, function(v) c(f$updated_params[[v]]$posterior_sd_omin,
                                           f$updated_params[[v]]$posterior_sd_omax)))
    names(m) <- names(s) <- pnames
    # A Hessian that failed leaves no posterior sd; fall back to the prior's.
    fallback <- vapply(pnames, function(p) {
      b <- base_tols$scored[[sub("\\..*$", "", p)]]
      max(b$optimal_max - b$optimal_min, 1e-3) * prior_sd_fraction
    }, numeric(1))
    bad <- !is.finite(s) | s <= 0
    s[bad] <- fallback[bad]
    list(m = m, s = s)
  }

  hyper <- NULL
  final <- list()
  if (do_pool) {
    hyper <- .gp_hyper(est, se, distances[fitted_ids, fitted_ids, drop = FALSE],
                       floor_sd = floor_sd)
    if (verbose) cli::cli_inform(
      "Pooling: relatedness scale rho = {signif(hyper$rho, 3)} (in the units of the distance matrix).")
    for (k in all_ids) {
      # Each lineage's prior is what the *other* fitted lineages predict; a
      # lineage with no fit of its own is predicted from all of them.
      pr <- .gp_prior(k, est, se, distances, hyper = hyper, floor_sd = floor_sd)
      pm <- vapply(pnames, function(p) pr[[p]][["mean"]], numeric(1))
      ps <- vapply(pnames, function(p) pr[[p]][["sd"]],   numeric(1))
      prior_list <- stats::setNames(lapply(vars, function(v) list(
        optimal_min = c(pm[[paste0(v, ".optimal_min")]], ps[[paste0(v, ".optimal_min")]]),
        optimal_max = c(pm[[paste0(v, ".optimal_max")]], ps[[paste0(v, ".optimal_max")]]))),
        vars)
      if (n_rec[[k]] >= min_sparse) {
        f <- tryCatch(.fit(k, prior = prior_list, min_n = min_sparse), error = function(e) NULL)
        if (!is.null(f)) {
          fs <- fit_summary(f)
          add_prof(k, "pooled", fs$m, fs$s, pm, ps)
          tols[[k]] <- f$tolerances
          next
        }
      }
      params <- stats::setNames(lapply(vars, function(v) list(
        optimal_min = pm[[paste0(v, ".optimal_min")]],
        optimal_max = pm[[paste0(v, ".optimal_max")]])), vars)
      tols[[k]] <- .apply_params_to_tols(base_tols, params)
      add_prof(k, "borrowed", pm, ps, pm, ps)
    }
  } else {
    if (verbose) cli::cli_inform("Not pooling: {reason}.")
    for (k in all_ids) {
      if (k %in% fitted_ids) {
        f <- fit0[[k]]
        fs <- fit_summary(f)
        add_prof(k, "own", fs$m, fs$s)
        tols[[k]] <- f$tolerances
      } else {
        def <- vapply(pnames, function(p) {
          v <- sub("\\..*$", "", p); par <- sub("^.*\\.", "", p)
          base_tols$scored[[v]][[par]] %||% NA_real_
        }, numeric(1))
        ds <- vapply(vars, function(v) {
          b <- base_tols$scored[[v]]
          max(b$optimal_max - b$optimal_min, 1e-3) * prior_sd_fraction
        }, numeric(1))
        add_prof(k, "default", def, stats::setNames(rep(ds, each = 2), pnames))
        tols[[k]] <- base_tols
      }
    }
  }

  # Calibration: presence ~ logistic(alpha + beta * suitability), against the
  # same scorer that draws the maps. Shared by default (see `calibration`).
  cal_data <- list()
  for (k in all_ids) {
    if (n_rec[[k]] < min_sparse || is.null(tols[[k]])) next
    d  <- records[lin_rec == k & !is.na(records[[presence_col]]), , drop = FALSE]
    y  <- as.numeric(as.logical(d[[presence_col]]))
    sc <- tryCatch(.lineage_score(.lineage_prepare_env(d), tols[[k]]),
                   error = function(e) NULL)
    if (is.null(sc)) next
    ok <- !sc$excluded & is.finite(sc$suitability)
    if (sum(ok) < min_sparse) next
    cal_data[[k]] <- data.frame(y = y[ok], s = sc$suitability[ok])
  }
  .glm_cal <- function(dd) {
    if (is.null(dd) || nrow(dd) < min_sparse || length(unique(dd$y)) < 2 ||
        stats::sd(dd$s) == 0) return(NULL)
    g <- tryCatch(suppressWarnings(stats::glm(y ~ s, family = stats::binomial(), data = dd)),
                  error = function(e) NULL)
    if (is.null(g)) return(NULL)
    cf <- unname(stats::coef(g))
    if (all(is.finite(cf)) && cf[2] > 0) cf else NULL
  }
  shared <- .glm_cal(do.call(rbind, cal_data))
  if (is.null(shared)) {
    cli::cli_abort("Could not calibrate suitability against presence: records show no usable relationship.")
  }
  calib$alpha <- shared[1]; calib$beta <- shared[2]
  if (calibration == "lineage") {
    for (k in names(cal_data)) {
      cf <- .glm_cal(cal_data[[k]])
      if (!is.null(cf)) calib[calib$lineage == k, c("alpha", "beta")] <- cf
    }
  }

  profiles <- do.call(rbind, prof)
  out <- structure(list(
    species = species, profiles = profiles, tolerances = tols,
    calibration = calib, signal = sig, hyper = hyper, pooled = do_pool,
    pool_reason = reason, distances = distances, n_records = n_rec,
    lineage_col = lineage_col, vars = vars),
    class = "lineage_fits")
  if (verbose) print(out)
  out
}

#' @export
print.lineage_fits <- function(x, ...) {
  cli::cli_h2("Lineage tolerance profiles - {x$species}")
  print(x$signal)
  src <- unique(x$profiles[, c("lineage", "n_records", "source")])
  cli::cli_inform("{nrow(src)} lineage{?s}: {paste(names(table(src$source)), table(src$source), sep = ' x ', collapse = ', ')}.")
  if (any(src$source %in% c("borrowed", "default"))) {
    cli::cli_inform(c("!" = "Profiles marked {.val borrowed} come from relatives alone; {.val default} ones carry the species default. Treat them as hypotheses."))
  }
  invisible(x)
}


# ---- scoring helpers ----------------------------------------------------------

.lineage_prepare_env <- function(env) {
  df <- .standardise_coords(as.data.frame(env, stringsAsFactors = FALSE))
  date_col <- .find_col_any(df, c("date", "datetime", "timestamp"))
  if (!is.null(date_col)) {
    df <- suppressMessages(add_season_column(df, date_col = date_col, lat_col = "lat"))
  } else {
    df$season <- NA_character_
  }
  suppressMessages(.auto_calculate_aragonite(df, verbose = FALSE))
}

.lineage_score <- function(df, tols) {
  d <- suppressMessages(suppressWarnings(check_exclusions(df, tols)))
  d <- suppressMessages(suppressWarnings(score_locations(d, tols, verbose = FALSE)))
  list(suitability = d$suitability, excluded = as.logical(d$excluded))
}

.lineage_tag <- function(ids) make.names(ids, unique = TRUE)


# ---- inverse: where does each lineage occur ---------------------------------

#' Where is each lineage more likely to occur than the others?
#'
#' @description
#' For every location, scores each lineage's tolerance profile, converts the
#' score to an occurrence probability with that lineage's fitted calibration,
#' and applies Bayes' rule across lineages:
#'
#' `P(lineage k | present, x) = pi_k * p_k(x) / sum_j pi_j * p_j(x)`
#'
#' where `pi_k` is a prior share (uniform by default). This is
#' **environmental correspondence**, not evidence of local adaptation:
#' telling adaptation from plasticity needs common-garden or transplant data.
#'
#' @section Ambiguity:
#' Where the environment does not discriminate between lineages, the result is
#' `"Ambiguous"` rather than a name. `lineage_ambiguity` is the normalised
#' entropy of the posterior (0 = one lineage certain, 1 = all equally likely).
#'
#' @param fits A `lineage_fits` object from [fit_lineages()].
#' @param env Data frame of locations with `lat`, `lon` and the environmental
#'   columns the profiles use.
#' @param prior_share `"uniform"` (default), `"records"` (proportional to
#'   records per lineage) or a named numeric vector.
#' @param ambiguity_threshold The best lineage must hold at least this share of
#'   the posterior to be named; default 0.5 (more likely than all others
#'   together). With many lineages whose tolerances overlap that is a high bar
#'   -- most locations will be `"Ambiguous"`, which is an accurate statement
#'   that the environment does not tell them apart. `lineage_best` and
#'   `lineage_prob` are always returned, so a most-likely map is available
#'   regardless; lower the threshold, or group lineages, to name more cells.
#'
#' @return `env` (standardised) with, per lineage, `suit_<id>`, `p_<id>` and
#'   `post_<id>`, plus `lineage_best`, `lineage_prob`, `lineage_ambiguity`,
#'   `lineage_class` and `lineage_source` (how the best lineage's profile was
#'   obtained), and `excluded`. Locations that fail a hard exclusion are
#'   `"Excluded"`.
#' @seealso [fit_lineages()], [rank_source_populations()]
#' @export
lineage_occurrence <- function(fits, env,
                               prior_share         = "uniform",
                               ambiguity_threshold = 0.5) {

  if (!inherits(fits, "lineage_fits")) {
    cli::cli_abort("{.arg fits} must come from {.fn fit_lineages}.")
  }
  ids <- names(fits$tolerances)
  L   <- length(ids)
  if (L < 2) cli::cli_abort("Need at least two lineages to compare.")
  tag <- .lineage_tag(ids)

  pi_k <- if (is.numeric(prior_share)) {
    if (is.null(names(prior_share)) || !all(ids %in% names(prior_share)))
      cli::cli_abort("A numeric {.arg prior_share} must be named for every lineage.")
    prior_share[ids]
  } else if (identical(prior_share, "records")) {
    fits$n_records[ids] + 1e-9
  } else if (identical(prior_share, "uniform")) {
    rep(1, L)
  } else cli::cli_abort("{.arg prior_share} must be 'uniform', 'records' or a named vector.")
  pi_k <- pi_k / sum(pi_k)

  df <- .lineage_prepare_env(env)
  n  <- nrow(df)
  P <- matrix(NA_real_, n, L, dimnames = list(NULL, ids))
  S <- P
  excluded <- rep(FALSE, n)
  for (i in seq_len(L)) {
    sc <- .lineage_score(df, fits$tolerances[[ids[i]]])
    S[, i] <- sc$suitability
    excluded <- excluded | sc$excluded
    cal <- fits$calibration[fits$calibration$lineage == ids[i], ]
    P[, i] <- stats::plogis(cal$alpha + cal$beta * sc$suitability)
  }
  P[excluded, ] <- NA_real_

  num <- sweep(P, 2, pi_k, `*`)
  den <- rowSums(num)
  post <- num / den
  post[!is.finite(den) | den <= 0, ] <- NA_real_

  best_i <- apply(post, 1, function(r) if (all(is.na(r))) NA_integer_ else which.max(r))
  best_p <- ifelse(is.na(best_i), NA_real_, post[cbind(seq_len(n), best_i)])
  ent <- apply(post, 1, function(r) {
    if (anyNA(r)) return(NA_real_)
    r <- r[r > 0]
    # Clamp: a near-uniform posterior can overshoot 1 by rounding error.
    min(max(-sum(r * log(r)) / log(L), 0), 1)
  })

  src <- unique(fits$profiles[, c("lineage", "source")])
  src <- stats::setNames(src$source, src$lineage)

  cls <- ifelse(excluded, "Excluded",
         ifelse(is.na(best_i), NA_character_,
         ifelse(best_p < ambiguity_threshold, "Ambiguous", ids[best_i])))

  df$excluded          <- excluded
  for (i in seq_len(L)) {
    df[[paste0("suit_", tag[i])]] <- S[, i]
    df[[paste0("p_",    tag[i])]] <- P[, i]
    df[[paste0("post_", tag[i])]] <- post[, i]
  }
  df$lineage_best      <- ifelse(is.na(best_i), NA_character_, ids[best_i])
  df$lineage_prob      <- best_p
  df$lineage_ambiguity <- ent
  df$lineage_class     <- cls
  df$lineage_source    <- ifelse(is.na(best_i), NA_character_, unname(src[df$lineage_best]))
  attr(df, "lineages") <- ids
  attr(df, "ambiguity_threshold") <- ambiguity_threshold
  df
}


# ---- inverse, management framing: which source for this site ----------------

#' Which source population suits each site?
#'
#' @description
#' Ranks lineages as candidate sources for each restoration site. The score
#' is the lineage's occurrence probability at the site, optionally discounted
#' by genetic distance from the population that is locally native, to reflect
#' the risk of introducing a distant lineage.
#'
#' `score = p_k(x) * exp(-distance_penalty * d(k, local) / max(d))`
#'
#' With `distance_penalty = 0` (default) genetic distance plays no part and the
#' ranking is purely environmental. Whether and how strongly to penalise
#' translocation distance is a management judgement, not something the data
#' decide; the penalty is stated in the output.
#'
#' @param fits A `lineage_fits` object.
#' @param sites Data frame, one row per site: `lat`, `lon`, the environmental
#'   columns, and optionally `site_id` and a local-lineage column.
#' @param local_lineage `NULL` (no discount), a lineage id applied to every
#'   site, or the name of a column of `sites` giving each site's native lineage.
#' @param distance_penalty Non-negative strength of the discount; the farthest
#'   pair of lineages is discounted by `exp(-distance_penalty)`.
#' @param n_draws If positive, profile parameters are drawn this many times
#'   from their posteriors and the ranking repeated, adding `p_top` (share of
#'   draws in which the lineage scores within `tie_tol` of the best) and a
#'   5-95% interval on the score. Calibration uncertainty is not propagated.
#'   Slow for many sites.
#' @param tie_tol Scores within this of the best count as tied for first in
#'   `p_top`. Where several lineages all suit a site (scores near 1) the winner
#'   is decided by differences in the third decimal, and calling one of them
#'   "best" would overstate what the model can tell apart. `p_top` values then
#'   sum to more than 1 across lineages, which is the point.
#' @param seed Optional integer for the draws.
#'
#' @return A data frame, one row per site x lineage, ordered by site then rank:
#'   `site_id`, `rank`, `lineage`, `suitability`, `p_occurrence`,
#'   `posterior_share`, `distance_to_local`, `translocation_weight`, `score`,
#'   `source`, `evidence` (`"low"` for borrowed/default profiles) and, with
#'   draws, `p_top`, `score_lo`, `score_hi`. Excluded sites return no rows and
#'   are listed in the `excluded_sites` attribute.
#' @seealso [lineage_occurrence()], [fit_lineages()]
#' @export
rank_source_populations <- function(fits, sites,
                                    local_lineage    = NULL,
                                    distance_penalty = 0,
                                    n_draws          = 0L,
                                    tie_tol          = 0.02,
                                    seed             = NULL) {

  if (!inherits(fits, "lineage_fits")) {
    cli::cli_abort("{.arg fits} must come from {.fn fit_lineages}.")
  }
  if (!is.numeric(distance_penalty) || distance_penalty < 0) {
    cli::cli_abort("{.arg distance_penalty} must be a non-negative number.")
  }
  ids <- names(fits$tolerances); L <- length(ids)
  D <- fits$distances
  dmax <- max(D)
  sites <- as.data.frame(sites, stringsAsFactors = FALSE)
  n <- nrow(sites)
  site_id <- if ("site_id" %in% names(sites)) as.character(sites$site_id) else as.character(seq_len(n))

  loc <- rep(NA_character_, n)
  if (!is.null(local_lineage)) {
    if (length(local_lineage) == 1L && local_lineage %in% ids) {
      loc[] <- local_lineage
    } else if (length(local_lineage) == 1L && local_lineage %in% names(sites)) {
      loc <- as.character(sites[[local_lineage]])
    } else {
      cli::cli_abort("{.arg local_lineage} must be a lineage id or the name of a column of {.arg sites}.")
    }
    bad <- setdiff(stats::na.omit(unique(loc)), ids)
    if (length(bad) > 0) cli::cli_abort("Unknown local lineage{?s}: {.val {bad}}.")
  }
  if (distance_penalty > 0 && all(is.na(loc))) {
    cli::cli_abort("{.arg distance_penalty} > 0 needs {.arg local_lineage}: a discount needs something to measure distance from.")
  }

  df <- .lineage_prepare_env(sites)
  src <- unique(fits$profiles[, c("lineage", "source")])
  src <- stats::setNames(src$source, src$lineage)

  # Sites x lineages at the posterior means.
  score_all <- function(tols_list) {
    S <- matrix(NA_real_, n, L, dimnames = list(NULL, ids)); ex <- rep(FALSE, n)
    for (i in seq_len(L)) {
      sc <- .lineage_score(df, tols_list[[i]])
      S[, i] <- sc$suitability; ex <- ex | sc$excluded
    }
    list(S = S, excluded = ex)
  }
  weight_mat <- matrix(1, n, L, dimnames = list(NULL, ids))
  if (distance_penalty > 0 && dmax > 0) {
    for (r in seq_len(n)) if (!is.na(loc[r])) {
      weight_mat[r, ] <- exp(-distance_penalty * D[loc[r], ids] / dmax)
    }
  }
  cal <- fits$calibration[match(ids, fits$calibration$lineage), ]
  to_p <- function(S) {
    stats::plogis(sweep(sweep(S, 2, cal$beta, `*`), 2, cal$alpha, `+`))
  }

  base <- score_all(fits$tolerances)
  Pm <- to_p(base$S)
  Pm[base$excluded, ] <- NA_real_
  Sc <- Pm * weight_mat
  post <- Pm / rowSums(Pm)

  p_top <- s_lo <- s_hi <- NULL
  if (n_draws > 0) {
    if (n_draws < 2) cli::cli_abort("{.arg n_draws} must be 0 (off) or at least 2.")
    if (!is.null(seed)) {
      old <- if (exists(".Random.seed", envir = globalenv())) get(".Random.seed", envir = globalenv()) else NULL
      on.exit(if (!is.null(old)) assign(".Random.seed", old, envir = globalenv()), add = TRUE)
      set.seed(seed)
    }
    base_tols <- get_species_tolerances(fits$species)
    prof <- fits$profiles
    draws <- array(NA_real_, c(n, L, n_draws))
    for (b in seq_len(n_draws)) {
      tl <- lapply(seq_len(L), function(i) {
        pk <- prof[prof$lineage == ids[i], ]
        params <- lapply(fits$vars, function(v) {
          a <- pk[pk$variable == v & pk$parameter == "optimal_min", ]
          z <- pk[pk$variable == v & pk$parameter == "optimal_max", ]
          list(optimal_min = stats::rnorm(1, a$mean, a$sd),
               optimal_max = stats::rnorm(1, z$mean, z$sd))
        })
        names(params) <- fits$vars
        .apply_params_to_tols(base_tols, params)
      })
      sc <- score_all(tl)
      Pb <- to_p(sc$S); Pb[sc$excluded, ] <- NA_real_
      draws[, , b] <- Pb * weight_mat
    }
    p_top <- matrix(NA_real_, n, L); s_lo <- s_hi <- p_top
    for (r in seq_len(n)) {
      if (base$excluded[r]) next
      dr <- matrix(draws[r, , ], nrow = L)
      top <- sweep(dr, 2, apply(dr, 2, max, na.rm = TRUE), `-`) >= -tie_tol
      p_top[r, ] <- rowMeans(top)
      q <- apply(dr, 1, stats::quantile, probs = c(0.05, 0.95), na.rm = TRUE)
      s_lo[r, ] <- q[1, ]; s_hi[r, ] <- q[2, ]
    }
  }

  rows <- lapply(seq_len(n), function(r) {
    if (base$excluded[r]) return(NULL)
    ord <- order(Sc[r, ], decreasing = TRUE)
    out <- data.frame(
      site_id = site_id[r], rank = seq_len(L), lineage = ids[ord],
      suitability = base$S[r, ord], p_occurrence = Pm[r, ord],
      posterior_share = post[r, ord],
      distance_to_local = if (is.na(loc[r])) NA_real_ else D[loc[r], ids[ord]],
      translocation_weight = weight_mat[r, ord], score = Sc[r, ord],
      source = unname(src[ids[ord]]),
      evidence = ifelse(src[ids[ord]] %in% c("borrowed", "default"), "low", "ok"),
      stringsAsFactors = FALSE, row.names = NULL)
    if (!is.null(p_top)) {
      out$p_top <- p_top[r, ord]; out$score_lo <- s_lo[r, ord]; out$score_hi <- s_hi[r, ord]
    }
    out
  })
  res <- do.call(rbind, rows)
  if (is.null(res)) res <- data.frame()
  attr(res, "excluded_sites") <- site_id[base$excluded]
  attr(res, "distance_penalty") <- distance_penalty
  attr(res, "local_lineage") <- if (all(is.na(loc))) NULL else loc
  res
}


# ---- storing and restoring a fitted model ------------------------------------

#' Turn a fitted lineage model into plain data, and back
#'
#' @description
#' `lineage_fits_as_list()` reduces a [fit_lineages()] result to plain lists and
#' numbers -- the profile table, calibration, distance matrix and the signal
#' test -- that survive a round trip through JSON. `lineage_fits_from_list()`
#' rebuilds a scoring-ready `lineage_fits` from that: the tolerance objects are
#' regenerated from the species defaults and the stored profile means, so the
#' numbers a user sees in the profile table are exactly the ones that score.
#'
#' Storing the parameters rather than serialised R objects keeps a saved model
#' inspectable and independent of the R version that fitted it.
#'
#' @param fits A `lineage_fits` object.
#' @param model A list as returned by `lineage_fits_as_list()` (or read back
#'   from its JSON).
#'
#' @return A list, or a `lineage_fits` object.
#' @export
lineage_fits_as_list <- function(fits) {
  if (!inherits(fits, "lineage_fits")) {
    cli::cli_abort("{.arg fits} must come from {.fn fit_lineages}.")
  }
  D <- fits$distances
  list(
    version      = 1L,
    species      = fits$species,
    lineage_col  = fits$lineage_col,
    vars         = as.list(fits$vars),
    pooled       = fits$pooled,
    pool_reason  = fits$pool_reason,
    signal       = list(r = fits$signal$r, p = fits$signal$p,
                        n_lineages = fits$signal$n_lineages,
                        n_perm = fits$signal$n_perm, usable = fits$signal$usable,
                        note = fits$signal$note),
    hyper        = if (is.null(fits$hyper)) NULL else
      list(rho = fits$hyper$rho, tau = as.list(fits$hyper$tau), m = as.list(fits$hyper$m)),
    n_records    = as.list(fits$n_records),
    calibration  = fits$calibration,
    profiles     = fits$profiles,
    distances    = list(ids = rownames(D), values = unname(split(unname(D), row(D))))
  )
}

#' @rdname lineage_fits_as_list
#' @export
lineage_fits_from_list <- function(model) {
  need <- c("species", "profiles", "calibration", "distances", "n_records", "vars")
  miss <- setdiff(need, names(model))
  if (length(miss) > 0) {
    cli::cli_abort("Stored lineage model is missing: {.val {miss}}.")
  }
  prof <- .rows_to_df(model$profiles)
  cal  <- .rows_to_df(model$calibration)

  ids <- as.character(unlist(model$distances$ids))
  vals <- model$distances$values
  Dm <- if (is.matrix(vals)) vals else do.call(rbind, lapply(vals, as.numeric))
  dimnames(Dm) <- list(ids, ids)
  D <- as_lineage_distances(Dm)

  base <- get_species_tolerances(model$species)
  vars <- as.character(unlist(model$vars))
  tols <- stats::setNames(lapply(ids, function(k) {
    pk <- prof[prof$lineage == k, , drop = FALSE]
    if (nrow(pk) == 0) cli::cli_abort("Stored model has no profile for lineage {.val {k}}.")
    params <- stats::setNames(lapply(vars, function(v) list(
      optimal_min = pk$mean[pk$variable == v & pk$parameter == "optimal_min"],
      optimal_max = pk$mean[pk$variable == v & pk$parameter == "optimal_max"])), vars)
    .apply_params_to_tols(base, params)
  }), ids)

  n_rec <- unlist(model$n_records)
  structure(list(
    species = model$species, profiles = prof, tolerances = tols,
    calibration = cal[match(ids, cal$lineage), , drop = FALSE],
    signal = structure(model$signal, class = "lineage_signal"),
    hyper = model$hyper, pooled = isTRUE(model$pooled),
    pool_reason = model$pool_reason, distances = D,
    n_records = n_rec[ids], lineage_col = model$lineage_col %||% "lineage_id",
    vars = vars), class = "lineage_fits")
}


# A table read back from JSON is a data frame (simplified) or a list of row
# lists (not simplified, with nulls as NULL). Accept both.
.rows_to_df <- function(x) {
  if (is.data.frame(x)) return(x)
  rows <- lapply(x, function(r) as.data.frame(
    lapply(r, function(v) if (is.null(v)) NA else v), stringsAsFactors = FALSE))
  do.call(rbind, rows)
}
