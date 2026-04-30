# =============================================================================
# Bayesian Tolerance Parameter Updating
# =============================================================================
#
# This module allows suitability scoring parameters (optimal range thresholds)
# to be updated from field presence/absence records using Bayesian inference.
#
# MOTIVATION
# ----------
# The AHP-weighted scoring in oystermapR uses expert-elicited tolerance ranges
# (e.g. "temperature optimal 15\u201320\u00b0C"). These priors are well-grounded in the
# literature but are inherently uncertain. When field survey data become
# available \u2014 especially presence/absence records tied to measured environmental
# variables \u2014 this uncertainty can be reduced using Bayes' rule:
#
#   p(\u03b8 | data) \u221d p(data | \u03b8) \u00b7 p(\u03b8)
#
# where \u03b8 is a vector of tolerance parameters (e.g. optimal_min, optimal_max
# for temperature), and the likelihood p(data | \u03b8) is a Bernoulli GLM linking
# predicted suitability to observed presence/absence.
#
# METHOD: MAP + LAPLACE APPROXIMATION
# ------------------------------------
# Default fast path (no MCMC chains required):
#  1. Find MAP estimate: \u03b8* = argmax log p(\u03b8|data) via optim() L-BFGS-B
#  2. Laplace approximation: compute numerical Hessian H at \u03b8*
#  3. Approximate posterior: \u03b8 | data \u2248 MVN(\u03b8*, -H\u207b\u00b9)
#  4. CIs from diagonal of -H\u207b\u00b9 (marginal posterior variance per parameter)
#
# This is the same approach used internally by brms, INLA, and glmer.
# It is exact when the posterior is unimodal and Gaussian; slightly
# over-confident when it is skewed (typical with small datasets).
#
# SEQUENTIAL UPDATING
# -------------------
# Each call to update_species_tolerances() stores updated posterior means as
# new prior means in the package-level cache (.oystermapR_env). Users can
# call the function again with new data: the posterior from the previous run
# automatically becomes the prior (Bayesian online learning). This is valid
# when observations are exchangeable (i.e. each survey is a fresh draw from
# the same environmental distribution).
#
# LIKELIHOOD MODEL
# ----------------
# Y_i ~ Bernoulli(p_i)
# logit(p_i) = \u03b1 + \u03b2 * S_i(\u03b8)
#
# where S_i(\u03b8) is the suitability score [0,1] computed for observation i
# using the current parameter vector \u03b8. \u03b1 is an intercept, \u03b2 is a gain
# parameter (both estimated jointly). The logistic link ensures p_i \u2208 (0,1).
#
# OPTIONAL: RANDOM-WALK METROPOLIS-HASTINGS MCMC
# -----------------------------------------------
# Set method = "mcmc" for full posterior samples. Use this when:
#  - n_records < 50 (Laplace may be poorly calibrated with small N)
#  - Posterior skewness is suspected (e.g. near-boundary parameters)
#  - You need formal credible intervals for publication
#
# References:
#  Gelman et al. (2013): Bayesian Data Analysis, 3rd ed. CRC Press.
#  Rue et al. (2009): Approximate Bayesian inference using INLA. JRSS-B.
#  Roberts et al. (2017): Cross-validation strategies for data with temporal,
#   spatial, hierarchical, or phylogenetic structure. Ecography.
#  Dorazio (2012): Predicting the geographic distribution of a species from
#   presence-only data subject to detection errors. Biometrics.

# Package-level environment for storing posterior updates
.oystermapR_env <- new.env(parent = emptyenv())
.oystermapR_env$posterior_cache <- list()

# \u2500\u2500 Main update function \u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500

#' Update species tolerance parameters from field observations (Bayesian)
#'
#' @description
#' Fits a Bayesian logistic regression linking AHP suitability scores to
#' observed presence/absence, then updates the optimal-range tolerance
#' parameters for one or more scored variables.
#'
#' **Default method (MAP + Laplace):** finds the Maximum A Posteriori estimate
#' via `optim()` (L-BFGS-B) and approximates the posterior as a multivariate
#' normal using the numerical Hessian. Fast; suitable for routine use.
#'
#' **MCMC method:** Random-Walk Metropolis-Hastings sampler for full posterior
#' samples. Recommended when n < 50 or when credible intervals are needed for
#' publication. May take several minutes for large parameter sets.
#'
#' Posteriors are stored in a package-level cache so that repeated calls
#' accumulate evidence (sequential Bayesian updating). Use
#' [save_tolerance_update()] to persist between sessions.
#'
#' @section What gets updated:
#' For each variable in `update_vars`, the function estimates shifts in:
#' - `optimal_min`, `optimal_max` \u2014 the sweet-spot bounds
#' - `acceptable_min` / `poor_min`, `acceptable_max` / `poor_max` \u2014 shoulder
#'   bounds (updated proportionally to maintain curve shape)
#'
#' Parameters are constrained to biologically plausible ranges (the prior
#' uncertainty bounds) via box constraints in the optimiser.
#'
#' @param records Dataframe of field observations. Must contain:
#'   - `lat`, `lon` \u2014 coordinates
#'   - `presence` (or name given in `presence_col`) \u2014 1 = present, 0 = absent
#'   - Columns matching the environmental variables to update (e.g. `temperature`,
#'     `salinity`, `depth`, etc.)
#' @param species Character. Species key (e.g. `"ostrea_edulis"`). See
#'   [list_species()].
#' @param update_vars Character vector. Variables to update parameters for.
#'   Defaults to all numeric scored variables present in `records`.
#' @param presence_col Character. Name of presence/absence column (default
#'   `"presence"`).
#' @param method Character. `"map"` (default, fast) or `"mcmc"` (full posterior).
#' @param n_iter Integer. MCMC iterations (default 5000; ignored for MAP).
#' @param n_warmup Integer. MCMC warm-up / burn-in (default 1000; ignored for MAP).
#' @param prior_sd_fraction Numeric. Prior SD as fraction of parameter range
#'   (default 0.20 = 20\%). Wider = weaker / more data-driven prior.
#' @param min_records Integer. Minimum matched records required (default 20).
#' @param verbose Logical. Print progress and diagnostics (default TRUE).
#'
#' @return Invisibly: a named list with elements:
#'   - `species`: species key
#'   - `method`: "map" or "mcmc"
#'   - `n_records`: number of matched records used
#'   - `loglik_null`: log-likelihood of intercept-only model
#'   - `loglik_fit`: log-likelihood at MAP/posterior mean
#'   - `mcfadden_r2`: 1 - loglik_fit / loglik_null (pseudo-R\u00b2)
#'   - `updated_params`: named list of updated parameter values per variable
#'   - `posterior_sd`: named list of posterior SD per parameter (from Laplace/MCMC)
#'   - `prior_params`: the parameter values before updating (for comparison)
#'   - `convergence`: optim() convergence code (0 = success; MAP only)
#'
#' Side effect: updates the in-session cache accessed by [score_locations()].
#'
#' @export
#' @seealso [get_tolerance_posteriors()], [reset_tolerance_update()],
#'   [save_tolerance_update()], [load_tolerance_update()]
#'
#' @examples
#' \dontrun{
#' # Field records with presence/absence + environmental measurements
#' records <- data.frame(
#'   lat      = c(51.5, 51.6, 51.7, 51.8, 51.4),
#'   lon      = c(-4.1, -4.2, -4.0, -4.3, -4.0),
#'   presence = c(1, 1, 0, 0, 1),
#'   temperature = c(16.2, 17.1, 12.3, 11.0, 15.8),
#'   salinity    = c(32, 33, 31, 30, 34),
#'   depth       = c(5, 8, 15, 20, 3)
#' )
#'
#' fit <- update_species_tolerances(
#'   records = records,
#'   species = "ostrea_edulis",
#'   update_vars = c("temperature", "salinity", "depth")
#' )
#'
#' # See updated parameters
#' get_tolerance_posteriors("ostrea_edulis")
#'
#' # Re-run predict_oyster \u2014 it will automatically use updated parameters
#' result <- predict_oyster(survey, "ostrea_edulis")
#'
#' # Save to disk for next session
#' save_tolerance_update("ostrea_edulis")
#' }
update_species_tolerances <- function(records,
                                       species,
                                       update_vars       = NULL,
                                       presence_col      = "presence",
                                       method            = c("map", "mcmc"),
                                       n_iter            = 5000L,
                                       n_warmup          = 1000L,
                                       prior_sd_fraction = 0.20,
                                       min_records       = 20L,
                                       verbose           = TRUE) {

  method <- match.arg(method)

  # \u2500\u2500 Load species tolerances (from cache if already updated) \u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500
  tols <- .get_cached_tolerances(species)
  if (is.null(tols)) {
    tols <- tryCatch(
      get_species_tolerances(species),
      error = function(e) cli::cli_abort(
        "Species '{species}' not found. Use list_species() to see available species."
      )
    )
  }

  # \u2500\u2500 Input validation \u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500
  if (!presence_col %in% names(records))
    cli::cli_abort("Column '{presence_col}' not found in records.")

  if (!all(c("lat","lon") %in% names(records)))
    cli::cli_abort("records must contain 'lat' and 'lon' columns.")

  obs <- as.numeric(as.logical(records[[presence_col]]))
  if (any(is.na(obs))) {
    n_na    <- sum(is.na(obs))
    records <- records[!is.na(obs), ]
    obs     <- obs[!is.na(obs)]
    if (verbose) cli::cli_warn("{n_na} record(s) with NA presence dropped.")
  }

  n_rec <- nrow(records)
  if (n_rec < min_records)
    cli::cli_abort(c(
      "Only {n_rec} records; need at least {min_records}.",
      "i" = "Lower min_records or collect more field observations."
    ))

  # \u2500\u2500 Identify which scored variables to update \u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500
  scored_vars <- names(tols$scored)[sapply(names(tols$scored), function(v) {
    p <- tols$scored[[v]]
    p$type %in% c("optimal_range","seasonal","threshold_decay") &&
      !is.null(p$optimal_min) && !is.null(p$optimal_max)
  })]

  # Restrict to variables actually present in records
  col_lwr   <- tolower(names(records))
  col_aliases <- list(
    temperature      = c("temperature","temp","temp_c","water_temp"),
    salinity         = c("salinity","sal","psu","salinity_psu"),
    depth            = c("depth","depth_m"),
    current_velocity = c("current_velocity","velocity","current","u_mean"),
    turbidity        = c("turbidity","ntu","turb"),
    dissolved_oxygen = c("dissolved_oxygen","do","do_mgl","oxygen"),
    chlorophyll_a    = c("chlorophyll_a","chla","chl_a","chlorophyll"),
    slope            = c("slope","slope_deg"),
    roughness        = c("roughness","rugosity")
  )

  vars_present <- Filter(function(v) {
    cands <- col_aliases[[v]] %||% v
    any(col_lwr %in% tolower(cands))
  }, scored_vars)

  if (!is.null(update_vars)) {
    not_found <- setdiff(update_vars, vars_present)
    if (length(not_found) > 0)
      cli::cli_warn("update_vars not found in records or not updatable: {not_found}")
    vars_present <- intersect(update_vars, vars_present)
  }

  if (length(vars_present) == 0)
    cli::cli_abort(c(
      "No updatable numeric scored variables found in records.",
      "i" = paste0("Need columns matching any of: ",
                   paste(scored_vars, collapse = ", "))
    ))

  if (verbose) {
    cli::cli_h2("Bayesian Tolerance Parameter Update")
    cli::cli_inform(c(
      " " = "Species:  {species}",
      " " = "Method:   {toupper(method)} + {'Laplace approximation'[method=='map']['RWMH MCMC'][method=='mcmc']}",
      " " = "Records:  {n_rec} ({sum(obs==1)} presences, {sum(obs==0)} absences)",
      " " = "Updating: {paste(vars_present, collapse=', ')}"
    ))
  }

  # \u2500\u2500 Extract environmental values for each variable \u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500
  env_vals <- lapply(vars_present, function(v) {
    cands   <- col_aliases[[v]] %||% v
    col_hit <- names(records)[col_lwr %in% tolower(cands)]
    as.numeric(records[[col_hit[1]]])
  })
  names(env_vals) <- vars_present

  # Drop rows with NA in any update variable
  valid_mask <- Reduce("&", lapply(env_vals, function(x) !is.na(x)))
  if (sum(valid_mask) < min_records)
    cli::cli_abort("After removing NA rows only {sum(valid_mask)} valid records remain.")
  obs      <- obs[valid_mask]
  env_vals <- lapply(env_vals, function(x) x[valid_mask])
  n_obs    <- length(obs)

  # \u2500\u2500 Build parameter vector and bounds \u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500
  # For each variable we estimate: optimal_min, optimal_max
  # We also optionally update shoulders but derive them from the optimal bounds
  # to keep dimensionality low (2 params per variable + 2 global: alpha, beta).

  prior_params <- list()
  theta_names  <- c("alpha", "beta")
  theta_init   <- c(0.0, 3.0)  # logistic intercept + gain
  lower_bounds <- c(-5.0, 0.1)
  upper_bounds <- c( 5.0, 15.0)

  for (v in vars_present) {
    p     <- tols$scored[[v]]
    omin  <- p$optimal_min
    omax  <- p$optimal_max
    # Prior SD = fraction of optimal range + some extra for boundary params
    rng   <- max(omax - omin, 1e-3)
    sd_v  <- rng * prior_sd_fraction

    prior_params[[v]] <- list(
      optimal_min   = omin,
      optimal_max   = omax,
      prior_sd_omin = sd_v,
      prior_sd_omax = sd_v
    )

    env_range   <- range(env_vals[[v]], na.rm = TRUE)
    global_rng  <- max(env_range[2] - env_range[1], rng)

    # Box constraints: can't move more than 2 SD from prior mean,
    # can't cross each other, must stay within observed data range
    lo_omin <- max(omin - 3*sd_v, env_range[1] - rng * 0.5)
    hi_omin <- omin + 3*sd_v
    lo_omax <- omax - 3*sd_v
    hi_omax <- min(omax + 3*sd_v, env_range[2] + rng * 0.5)

    theta_names  <- c(theta_names,  paste0(v, ".omin"), paste0(v, ".omax"))
    theta_init   <- c(theta_init,   omin,   omax)
    lower_bounds <- c(lower_bounds, lo_omin, lo_omax)
    upper_bounds <- c(upper_bounds, hi_omin, hi_omax)
  }

  n_theta <- length(theta_names)

  # \u2500\u2500 Log-posterior function \u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500
  .log_posterior <- function(theta) {
    alpha <- theta[1]
    beta  <- theta[2]
    if (beta <= 0) return(-Inf)

    # Compute suitability score for each observation under current theta
    S <- numeric(n_obs)
    for (k in seq_len(n_obs)) {
      s_sum <- 0; w_sum <- 0
      for (vi in seq_along(vars_present)) {
        v    <- vars_present[vi]
        p    <- tols$scored[[v]]
        omin <- theta[2 + 2*(vi-1) + 1]
        omax <- theta[2 + 2*(vi-1) + 2]
        if (omax <= omin) return(-Inf)

        # Rebuild params with updated optimal bounds; shoulders scale proportionally
        lo   <- p$poor_min %||% p$acceptable_min %||% (omin - (omax - omin))
        hi   <- p$acceptable_max %||% p$poor_max %||% (omax + (omax - omin))
        abs_hi <- p$absolute_max %||% hi

        x <- env_vals[[v]][k]
        if (is.na(x)) next
        if (x < lo || x > abs_hi) { raw <- 0 }
        else if (x >= omin && x <= omax) { raw <- 1 }
        else if (x < omin) { raw <- max(0, (x - lo) / (omin - lo)) }
        else { raw <- max(0, (abs_hi - x) / (abs_hi - omax)) }

        wt    <- p$rank
        s_sum <- s_sum + wt * raw
        w_sum <- w_sum + wt
      }
      S[k] <- if (w_sum > 0) s_sum / w_sum else 0.5
    }

    # Likelihood: Y_i ~ Bernoulli(logistic(alpha + beta * S_i))
    linpred <- alpha + beta * S
    loglik  <- sum(obs * linpred - log(1 + exp(linpred)))

    # Log-prior: independent Gaussian on optimal_min, optimal_max per variable
    log_prior <- 0
    for (vi in seq_along(vars_present)) {
      v     <- vars_present[vi]
      pp    <- prior_params[[v]]
      omin_ <- theta[2 + 2*(vi-1) + 1]
      omax_ <- theta[2 + 2*(vi-1) + 2]
      log_prior <- log_prior +
        dnorm(omin_, pp$optimal_min, pp$prior_sd_omin, log=TRUE) +
        dnorm(omax_, pp$optimal_max, pp$prior_sd_omax, log=TRUE)
    }
    # Weakly informative prior on alpha, beta
    log_prior <- log_prior +
      dnorm(alpha, 0, 2,  log=TRUE) +
      dnorm(beta,  4, 2,  log=TRUE)

    loglik + log_prior
  }

  # \u2500\u2500 MAP estimation \u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500
  # Enforce that lower_omin < upper_omin and lower_omax < upper_omax
  for (i in seq_along(theta_names)) {
    if (lower_bounds[i] >= upper_bounds[i]) {
      mid <- (lower_bounds[i] + upper_bounds[i]) / 2
      lower_bounds[i] <- mid - 1e-4
      upper_bounds[i] <- mid + 1e-4
    }
    # Clamp init to within bounds
    theta_init[i] <- max(lower_bounds[i], min(upper_bounds[i], theta_init[i]))
  }

  if (verbose) cli::cli_inform("Running MAP optimisation...")

  opt_result <- tryCatch(
    optim(
      par     = theta_init,
      fn      = function(th) -.log_posterior(th),  # minimise negative log-posterior
      method  = "L-BFGS-B",
      lower   = lower_bounds,
      upper   = upper_bounds,
      control = list(maxit = 2000L, factr = 1e7)
    ),
    error = function(e) {
      cli::cli_warn("L-BFGS-B failed ({conditionMessage(e)}); trying Nelder-Mead fallback.")
      optim(theta_init, function(th) -.log_posterior(th),
            method = "Nelder-Mead", control = list(maxit = 5000L))
    }
  )

  theta_map   <- opt_result$par
  names(theta_map) <- theta_names
  convergence_code <- opt_result$convergence

  if (verbose && convergence_code != 0)
    cli::cli_warn(
      "optim() convergence code = {convergence_code}. Interpret results cautiously.",
      "i" = "Try more data or broader prior_sd_fraction."
    )

  # \u2500\u2500 Laplace approximation for posterior SDs \u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500
  posterior_sd_map <- rep(NA_real_, n_theta)
  names(posterior_sd_map) <- theta_names

  if (method == "map") {
    if (verbose) cli::cli_inform("Computing Laplace approximation (numerical Hessian)...")
    hess <- tryCatch(
      .numerical_hessian(function(th) -.log_posterior(th), theta_map, h = 1e-4),
      error = function(e) {
        cli::cli_warn("Hessian computation failed: {conditionMessage(e)}")
        NULL
      }
    )
    if (!is.null(hess)) {
      cov_mat <- tryCatch(solve(hess), error = function(e) NULL)
      if (!is.null(cov_mat) && all(diag(cov_mat) > 0)) {
        posterior_sd_map <- sqrt(diag(cov_mat))
      } else {
        # Fallback: diagonal approximation
        diag_h <- diag(hess)
        diag_h[diag_h <= 0] <- 1  # guard against non-positive definite
        posterior_sd_map <- 1 / sqrt(diag_h)
        cli::cli_warn("Hessian was not positive-definite; using diagonal approximation.")
      }
    }
  }

  # \u2500\u2500 Optional MCMC \u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500
  mcmc_samples <- NULL
  if (method == "mcmc") {
    if (verbose) cli::cli_inform("Running RWMH MCMC ({n_iter} iterations, {n_warmup} warm-up)...")
    mcmc_samples <- .rwmh(
      log_post   = .log_posterior,
      theta_init = theta_map,   # start from MAP
      n_iter     = n_iter,
      n_warmup   = n_warmup,
      proposal_sd = posterior_sd_map * 0.5,  # adaptive-ish: scale to Laplace SD
      lower      = lower_bounds,
      upper      = upper_bounds,
      verbose    = verbose
    )
    # Posterior summaries from samples
    theta_map        <- colMeans(mcmc_samples)  # posterior mean replaces MAP
    posterior_sd_map <- apply(mcmc_samples, 2, sd)
    names(theta_map)        <- theta_names
    names(posterior_sd_map) <- theta_names
  }

  # \u2500\u2500 Extract updated parameters \u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500
  updated_params <- list()
  for (vi in seq_along(vars_present)) {
    v            <- vars_present[vi]
    new_omin     <- theta_map[paste0(v, ".omin")]
    new_omax     <- theta_map[paste0(v, ".omax")]
    sd_omin      <- posterior_sd_map[paste0(v, ".omin")]
    sd_omax      <- posterior_sd_map[paste0(v, ".omax")]

    # Maintain shoulder proportions relative to updated optimal range
    p   <- tols$scored[[v]]
    old_rng <- max(p$optimal_max - p$optimal_min, 1e-3)
    new_rng <- max(new_omax - new_omin, 1e-3)
    scale   <- new_rng / old_rng

    updated_params[[v]] <- list(
      optimal_min  = round(new_omin, 4),
      optimal_max  = round(new_omax, 4),
      posterior_sd_omin = round(sd_omin, 4),
      posterior_sd_omax = round(sd_omax, 4),
      # 95% credible intervals
      ci95_omin = round(c(new_omin - 1.96*sd_omin, new_omin + 1.96*sd_omin), 4),
      ci95_omax = round(c(new_omax - 1.96*sd_omax, new_omax + 1.96*sd_omax), 4),
      # Update shoulders proportionally
      poor_min      = if (!is.null(p$poor_min)) round(new_omin - (p$optimal_min - p$poor_min) * scale, 4) else NULL,
      acceptable_min = if (!is.null(p$acceptable_min)) round(new_omin - (p$optimal_min - p$acceptable_min) * scale, 4) else NULL,
      acceptable_max = if (!is.null(p$acceptable_max)) round(new_omax + (p$acceptable_max - p$optimal_max) * scale, 4) else NULL,
      poor_max      = if (!is.null(p$poor_max)) round(new_omax + (p$poor_max - p$optimal_max) * scale, 4) else NULL,
      absolute_max  = p$absolute_max  # hard limits stay fixed
    )
  }

  # \u2500\u2500 Log-likelihood metrics \u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500
  # Null model: intercept only (S_i irrelevant)
  p_null      <- mean(obs)
  loglik_null <- sum(obs * log(p_null + 1e-10) + (1-obs) * log(1 - p_null + 1e-10))

  # Fitted model at MAP
  linpred_fit <- theta_map["alpha"] + theta_map["beta"] *
    sapply(seq_len(n_obs), function(k) {
      s_sum <- 0; w_sum <- 0
      for (vi in seq_along(vars_present)) {
        v    <- vars_present[vi]
        up   <- updated_params[[v]]
        p    <- tols$scored[[v]]
        omin <- up$optimal_min
        omax <- up$optimal_max
        lo   <- up$poor_min %||% up$acceptable_min %||% (omin - (omax - omin))
        hi   <- up$acceptable_max %||% up$poor_max %||% (omax + (omax - omin))
        abs_hi <- up$absolute_max %||% hi
        x    <- env_vals[[v]][k]
        if (is.na(x)) next
        if (x < lo || x > abs_hi)  { raw <- 0 }
        else if (x >= omin && x <= omax) { raw <- 1 }
        else if (x < omin)         { raw <- max(0, (x - lo) / (omin - lo)) }
        else                       { raw <- max(0, (abs_hi - x) / (abs_hi - omax)) }
        wt    <- p$rank
        s_sum <- s_sum + wt * raw
        w_sum <- w_sum + wt
      }
      if (w_sum > 0) s_sum / w_sum else 0.5
    })

  p_fit      <- 1 / (1 + exp(-linpred_fit))
  loglik_fit <- sum(obs * log(p_fit + 1e-10) + (1-obs) * log(1 - p_fit + 1e-10))
  r2         <- round(1 - loglik_fit / loglik_null, 4)

  if (verbose) {
    cli::cli_h3("Model fit diagnostics")
    cli::cli_inform(c(
      " " = "McFadden pseudo-R\u00b2: {r2}  (>0.20 = good fit for SDMs)",
      " " = "Log-likelihood (null): {round(loglik_null, 2)}",
      " " = "Log-likelihood (fit):  {round(loglik_fit,  2)}",
      " " = "optim() convergence:   {convergence_code} (0 = OK)"
    ))
    cli::cli_h3("Updated parameters")
    for (v in vars_present) {
      up <- updated_params[[v]]
      pp <- prior_params[[v]]
      cli::cli_inform(c(
        " " = "{v}:",
        " " = "  optimal_min: {pp$optimal_min} -> {up$optimal_min}  (\u00b1{up$posterior_sd_omin})",
        " " = "  optimal_max: {pp$optimal_max} -> {up$optimal_max}  (\u00b1{up$posterior_sd_omax})"
      ))
    }
  }

  # \u2500\u2500 Store in session cache \u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500
  # Deep-copy tolerances and apply updated optimal bounds
  updated_tols <- tols
  for (vi in seq_along(vars_present)) {
    v  <- vars_present[vi]
    up <- updated_params[[v]]
    # Apply updates to cached tolerance object
    for (field in c("optimal_min","optimal_max","poor_min","acceptable_min",
                    "acceptable_max","poor_max")) {
      if (!is.null(up[[field]]))
        updated_tols$scored[[v]][[field]] <- up[[field]]
    }
  }
  # Sequential update: store updated tols as new prior for next call
  .oystermapR_env$posterior_cache[[species]] <- updated_tols

  # Return result
  result_out <- list(
    species       = species,
    method        = method,
    n_records     = n_obs,
    loglik_null   = round(loglik_null,  4),
    loglik_fit    = round(loglik_fit,   4),
    mcfadden_r2   = r2,
    updated_params = updated_params,
    posterior_sd  = setNames(
      lapply(vars_present, function(v) posterior_sd_map[grep(v, theta_names)]),
      vars_present
    ),
    prior_params  = lapply(prior_params, function(pp) list(
      optimal_min = pp$optimal_min,
      optimal_max = pp$optimal_max
    )),
    convergence   = convergence_code,
    mcmc_samples  = mcmc_samples,
    theta_map     = theta_map
  )
  invisible(result_out)
}


# \u2500\u2500 Accessors \u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500

#' Retrieve current posterior tolerance parameters for a species
#'
#' @description
#' Returns the tolerance parameter list currently held in the session cache,
#' after any [update_species_tolerances()] calls. If no update has been run,
#' returns the built-in prior parameters unchanged.
#'
#' @param species Character. Species key (e.g. `"ostrea_edulis"`).
#' @param var Character or NULL. If specified, returns parameters for that
#'   variable only; otherwise returns the full `$scored` list.
#'
#' @return Named list of tolerance parameters (same structure as
#'   [get_species_tolerances()]`$scored`).
#'
#' @export
#' @examples
#' \dontrun{
#' # After calling update_species_tolerances()
#' post <- get_tolerance_posteriors("ostrea_edulis")
#' post$temperature  # updated optimal_min, optimal_max with CIs
#' }
get_tolerance_posteriors <- function(species, var = NULL) {
  cached <- .get_cached_tolerances(species)
  if (is.null(cached)) return(NULL)
  if (!is.null(var)) {
    if (!var %in% names(cached$scored))
      cli::cli_abort("Variable '{var}' not found in scored parameters for '{species}'.")
    return(cached$scored[[var]])
  }
  cached$scored
}


#' Reset Bayesian tolerance updates for a species
#'
#' @description
#' Clears the session cache for a species, reverting to the built-in prior
#' parameters. Does not affect saved files (use file deletion for those).
#'
#' @param species Character. Species key, or `"all"` to clear all cached updates.
#' @param verbose Logical. Default TRUE.
#'
#' @return Invisibly NULL.
#' @export
#' @examples
#' \dontrun{
#' reset_tolerance_update("ostrea_edulis")
#' reset_tolerance_update("all")
#' }
reset_tolerance_update <- function(species = "all", verbose = TRUE) {
  if (species == "all") {
    n <- length(.oystermapR_env$posterior_cache)
    .oystermapR_env$posterior_cache <- list()
    if (verbose) cli::cli_inform("Cleared Bayesian updates for {n} species.")
  } else {
    if (!is.null(.oystermapR_env$posterior_cache[[species]])) {
      .oystermapR_env$posterior_cache[[species]] <- NULL
      if (verbose) cli::cli_inform("Reset '{species}' to built-in parameters.")
    } else {
      if (verbose) cli::cli_inform("No cached update found for '{species}'.")
    }
  }
  invisible(NULL)
}


#' Save Bayesian tolerance updates to disk
#'
#' @description
#' Serialises the current session cache for a species to an `.rds` file so
#' that updated parameters persist across R sessions. Load again with
#' [load_tolerance_update()].
#'
#' @param species Character. Species key or `"all"` to save all cached species.
#' @param path Character. Directory to save into (default: user cache directory
#'   `~/.oystermapR/`). Created if absent.
#' @param verbose Logical. Default TRUE.
#'
#' @return Invisibly: path(s) to saved file(s).
#' @export
#' @examples
#' \dontrun{
#' save_tolerance_update("ostrea_edulis")
#' save_tolerance_update("all", path = "data/bayes_updates/")
#' }
save_tolerance_update <- function(species = "all",
                                   path    = "~/.oystermapR",
                                   verbose = TRUE) {
  if (!dir.exists(path)) dir.create(path, recursive = TRUE)

  species_to_save <- if (species == "all") {
    names(.oystermapR_env$posterior_cache)
  } else {
    species
  }

  if (length(species_to_save) == 0) {
    if (verbose) cli::cli_warn("No cached updates to save.")
    return(invisible(character(0)))
  }

  saved_paths <- character(length(species_to_save))
  for (i in seq_along(species_to_save)) {
    sp   <- species_to_save[i]
    data <- .oystermapR_env$posterior_cache[[sp]]
    if (is.null(data)) {
      if (verbose) cli::cli_warn("No cached update for '{sp}'; skipping.")
      next
    }
    fpath <- file.path(path, paste0("bayes_update_", sp, ".rds"))
    saveRDS(data, fpath)
    saved_paths[i] <- fpath
    if (verbose) cli::cli_inform("Saved '{sp}' update -> {fpath}")
  }
  invisible(saved_paths)
}


#' Load saved Bayesian tolerance updates into session cache
#'
#' @description
#' Reads a previously saved `.rds` file (from [save_tolerance_update()]) and
#' restores it into the session cache. Subsequent calls to [predict_oyster()]
#' and [score_locations()] will automatically use the loaded parameters.
#'
#' @param species Character. Species key or `"all"` to load all `.rds` files
#'   in `path`.
#' @param path Character. Directory to load from (default: `~/.oystermapR/`).
#' @param verbose Logical. Default TRUE.
#'
#' @return Invisibly: the loaded tolerance list.
#' @export
#' @examples
#' \dontrun{
#' load_tolerance_update("ostrea_edulis")
#' # Now predict_oyster() uses the updated tolerances automatically
#' result <- predict_oyster(survey, "ostrea_edulis")
#' }
load_tolerance_update <- function(species = "all",
                                   path    = "~/.oystermapR",
                                   verbose = TRUE) {
  if (species == "all") {
    files <- list.files(path, pattern = "^bayes_update_.*\\.rds$", full.names = TRUE)
    if (length(files) == 0) {
      if (verbose) cli::cli_warn("No saved Bayesian updates found in '{path}'.")
      return(invisible(NULL))
    }
    for (f in files) {
      sp   <- sub("^bayes_update_", "", sub("\\.rds$", "", basename(f)))
      data <- readRDS(f)
      .oystermapR_env$posterior_cache[[sp]] <- data
      if (verbose) cli::cli_inform("Loaded '{sp}' from {f}")
    }
    return(invisible(NULL))
  }

  fpath <- file.path(path, paste0("bayes_update_", species, ".rds"))
  if (!file.exists(fpath))
    cli::cli_abort("File not found: {fpath}. Run save_tolerance_update() first.")

  data <- readRDS(fpath)
  .oystermapR_env$posterior_cache[[species]] <- data
  if (verbose) cli::cli_inform("Loaded '{species}' Bayesian update from {fpath}")
  invisible(data)
}


# \u2500\u2500 Internal helpers \u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500

#' Retrieve cached tolerance list (or NULL if not updated)
#' @keywords internal
.get_cached_tolerances <- function(species) {
  .oystermapR_env$posterior_cache[[species]]
}


#' Compute numerical Hessian by finite differences (central differences)
#' @keywords internal
.numerical_hessian <- function(f, theta, h = 1e-4) {
  n    <- length(theta)
  hess <- matrix(0, n, n)
  f0   <- f(theta)
  for (i in seq_len(n)) {
    for (j in seq_len(n)) {
      if (i == j) {
        tp <- theta; tp[i] <- tp[i] + h
        tm <- theta; tm[i] <- tm[i] - h
        hess[i,j] <- (f(tp) - 2*f0 + f(tm)) / h^2
      } else if (j > i) {
        tpp <- theta; tpp[i] <- tpp[i] + h; tpp[j] <- tpp[j] + h
        tpm <- theta; tpm[i] <- tpm[i] + h; tpm[j] <- tpm[j] - h
        tmp <- theta; tmp[i] <- tmp[i] - h; tmp[j] <- tmp[j] + h
        tmm <- theta; tmm[i] <- tmm[i] - h; tmm[j] <- tmm[j] - h
        hess[i,j] <- hess[j,i] <- (f(tpp) - f(tpm) - f(tmp) + f(tmm)) / (4*h^2)
      }
    }
  }
  hess
}


#' Random-Walk Metropolis-Hastings MCMC sampler
#' @keywords internal
.rwmh <- function(log_post, theta_init, n_iter, n_warmup,
                  proposal_sd, lower, upper, verbose = TRUE) {

  n_theta  <- length(theta_init)
  # Replace NA/zero proposal SDs with small default
  proposal_sd[is.na(proposal_sd) | proposal_sd <= 0] <- 0.05

  samples      <- matrix(NA_real_, nrow = n_iter, ncol = n_theta)
  colnames(samples) <- names(theta_init)
  theta_cur    <- theta_init
  lp_cur       <- log_post(theta_cur)
  n_accept     <- 0L

  # Adaptive scaling for acceptance rate targeting ~23-40%
  scale <- 1.0

  for (iter in seq_len(n_iter + n_warmup)) {
    # Proposal
    theta_prop <- theta_cur + rnorm(n_theta, 0, scale * proposal_sd)
    # Reflect off bounds
    theta_prop <- pmax(lower, pmin(upper, theta_prop))

    lp_prop <- tryCatch(log_post(theta_prop), error = function(e) -Inf)

    # Metropolis accept/reject
    log_ratio <- lp_prop - lp_cur
    if (is.finite(log_ratio) && log(runif(1)) < log_ratio) {
      theta_cur <- theta_prop
      lp_cur    <- lp_prop
      if (iter > n_warmup) n_accept <- n_accept + 1L
    }

    # Adaptive scaling during warm-up (every 100 steps)
    if (iter <= n_warmup && iter %% 100 == 0) {
      accept_rate <- n_accept / 100
      scale <- scale * exp(0.5 * (accept_rate - 0.25))
      scale <- max(0.01, min(scale, 5.0))
      n_accept <- 0L  # reset window counter
    }

    if (iter > n_warmup) {
      samples[iter - n_warmup, ] <- theta_cur
    }
  }

  accept_rate_final <- n_accept / n_iter
  if (verbose) {
    cli::cli_inform(c(
      " " = "MCMC acceptance rate: {round(accept_rate_final*100, 1)}%  (target: 23-40%)",
      " " = "Effective samples: ~{n_iter}  (check trace plots if using for publication)"
    ))
    if (accept_rate_final < 0.10 || accept_rate_final > 0.70)
      cli::cli_warn(
        "Acceptance rate outside optimal range. Adjust proposal_sd or prior_sd_fraction."
      )
  }
  samples
}


# \u2500\u2500 Hook into predict_oyster / score_locations \u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500
# These helpers are called by get_species_tolerances() to check for updates.

#' Apply any cached Bayesian updates to a tolerance list
#'
#' @description
#' Called internally by [get_species_tolerances()] and [predict_oyster()] to
#' transparently substitute updated parameters when a Bayesian update exists
#' for a species in the session cache. End users rarely need to call this
#' directly.
#'
#' @param tols Tolerance list from [get_species_tolerances()].
#' @param species Character. Species key.
#' @return Updated tolerance list (or unchanged if no cache entry found).
#' @keywords internal
.apply_bayesian_update <- function(tols, species) {
  cached <- .get_cached_tolerances(species)
  if (is.null(cached)) return(tols)
  # Merge: only overwrite $scored entries that exist in cached
  for (v in names(cached$scored)) {
    if (v %in% names(tols$scored)) {
      tols$scored[[v]] <- cached$scored[[v]]
    }
  }
  tols
}
