# =========================================================================
# Evidence-weighted suitability scoring
#
# VENDORED FROM seafloormapRcore
# ------------------------------
# oystermapR does not import seafloormapRcore -- it carries its own copy of
# the engine -- so this file is a vendored copy of
# `seafloormapRcore/R/evidence_weights.R`. Keep the two in step: fix bugs in
# the core copy first and re-vendor, rather than editing this one in place.
# Only the roxygen cross-references differ.
#
# WHY THIS EXISTS
# ---------------
# The default engine in `score_locations()` weights each scored variable by
# `1 / rank` and normalises those weights over whichever variables happen to
# be present in the data:
#
#     raw_weights  <- 1 / var_weights
#     norm_weights <- raw_weights / sum(raw_weights)
#
# That renormalisation makes ABSENCE INDISTINGUISHABLE FROM IRRELEVANCE. A
# survey that measures 4 of a species' 17 scored variables returns a
# confident-looking score on [0, 1] with nothing to indicate that most of the
# species' evidence base was never observed. Two further consequences follow,
# both of them measurable:
#
#   * A variable that does not vary still absorbs weight. Depth scored 1.0 at
#     every cell of a 6-10 m survey and still took a third of the weight,
#     diluting the layers that did discriminate.
#   * A variable that does not reproduce is weighted identically to one that
#     does. A layer whose two independent estimates correlate at r = 0.02
#     carries its full rank weight regardless.
#
# THE FIX
# -------
# Weight each variable by its prior biological importance MULTIPLIED BY its
# measured reliability and its realised spread:
#
#     w_i  proportional to  (1 / rank_i) * reliability_i * spread_i
#
# Unreliable layers fall out on their own; uninformative ones demote
# themselves. Reliability MULTIPLIES the biological prior and never replaces
# it: weighting on measured reliability alone would promote whatever the
# instrument happens to measure well, which is circular. Retaining the
# published ranks as the prior is what prevents that.
#
# Separately, the fraction of the species' total prior weight actually
# observed is reported as COVERAGE and carried on every output. That number is
# the honest headline, and it is what the renormalising model discards.
#
# This module is additive. `weighting = "rank"` remains the default everywhere
# and reproduces the existing behaviour exactly.
# =========================================================================


# ---- coverage -------------------------------------------------------------

#' Fraction of a species' evidence base observed by a survey
#'
#' @description
#' Coverage is the share of a species' **total prior weight** that a survey
#' actually observed, where prior weight is `1 / rank` summed over every
#' variable in the tolerance specification.
#'
#' It is deliberately weight-based rather than count-based. The
#' `data_completeness` column produced by [score_locations()] counts variables
#' (`n_scored / n_defined`) and so treats a rank-1 variable and a rank-6
#' variable as equal losses. Coverage does not: missing the single most
#' important variable costs six times what missing the least important one
#' does.
#'
#' Variables passed in `assigned` do **not** count toward coverage. Coverage
#' measures how much of the *species'* evidence base was observed, and
#' inventing a new layer cannot increase that. A tool that let you raise your
#' own coverage by adding layers would be measuring its own enthusiasm.
#'
#' @param tolerances A species tolerance list (see
#'   [get_species_tolerances()]).
#' @param observed Character vector of tolerance-spec variable names the
#'   survey observed. Names not present in the spec are ignored with a
#'   warning.
#' @param attempted_failed Optional character vector of variables that were
#'   instrumented but returned unusable data. Reported separately, and used to
#'   compute `coverage_if_recovered` — the coverage the survey would have had
#'   if that payload had worked. Quantifying the cost of an instrument failure
#'   in units of the species' evidence base is usually more informative than
#'   reporting the failure alone.
#'
#' @return An object of class `evidence_coverage`: a list with `coverage`,
#'   `prior_observed`, `prior_total`, `observed`, `missing`,
#'   `attempted_failed`, `coverage_if_recovered` and a `ledger` data frame
#'   giving rank, prior and status for every variable in the spec.
#'
#' @examples
#' \dontrun{
#' tol <- get_species_tolerances("ostrea_edulis")
#' evidence_coverage(
#'   tol,
#'   observed         = c("depth", "slope", "substrate_hardness", "roughness"),
#'   attempted_failed = c("current_velocity", "shear_stress")
#' )
#' }
#' @export
evidence_coverage <- function(tolerances,
                              observed,
                              attempted_failed = NULL) {

  scored <- tolerances$scored %||% list()
  if (length(scored) == 0) {
    cli::cli_abort("{.arg tolerances} has no {.field scored} variables.")
  }

  ranks <- vapply(scored, function(p) as.numeric(p$rank %||% NA_real_), numeric(1))
  names(ranks) <- names(scored)
  if (anyNA(ranks)) {
    cli::cli_abort(c(
      "Every scored variable needs a {.field rank} to compute coverage.",
      "x" = "Missing for: {.var {names(ranks)[is.na(ranks)]}}"
    ))
  }

  priors      <- 1 / ranks
  prior_total <- sum(priors)

  unknown <- setdiff(observed, names(ranks))
  if (length(unknown) > 0) {
    cli::cli_warn(c(
      "!" = "{length(unknown)} name{?s} in {.arg observed} {?is/are} not in the tolerance spec and {?does/do} not count toward coverage:",
      "*" = "{.var {unknown}}",
      "i" = "Layers with no published counterpart belong in {.arg assigned} of {.fn evidence_weights}."
    ))
  }
  observed <- intersect(observed, names(ranks))
  failed   <- intersect(attempted_failed %||% character(0), names(ranks))
  failed   <- setdiff(failed, observed)

  prior_observed <- sum(priors[observed])

  status <- rep("not_collected", length(ranks))
  names(status) <- names(ranks)
  status[observed] <- "present"
  status[failed]   <- "attempted_failed"

  ledger <- data.frame(
    variable = names(ranks),
    rank     = as.integer(ranks),
    prior    = round(unname(priors), 4),
    status   = unname(status),
    stringsAsFactors = FALSE
  )
  ledger <- ledger[order(ledger$rank, ledger$variable), ]
  rownames(ledger) <- NULL

  out <- list(
    coverage              = prior_observed / prior_total,
    prior_observed        = prior_observed,
    prior_total           = prior_total,
    observed              = observed,
    missing               = names(ranks)[status == "not_collected"],
    attempted_failed      = failed,
    coverage_if_recovered = (prior_observed + sum(priors[failed])) / prior_total,
    n_scored_spec         = length(ranks),
    species               = tolerances$latin_name %||% NA_character_,
    ledger                = ledger
  )
  class(out) <- "evidence_coverage"
  out
}

#' @export
print.evidence_coverage <- function(x, ...) {
  cli::cli_h3("Evidence coverage{if (!is.na(x$species)) paste0(' - ', x$species) else ''}")
  cli::cli_inform(c(
    "Coverage: {.strong {round(x$coverage, 3)}} ({round(x$prior_observed, 3)} of {round(x$prior_total, 3)} total prior weight)",
    "Observed: {length(x$observed)} of {x$n_scored_spec} scored variables"
  ))
  if (length(x$attempted_failed) > 0) {
    cost <- x$coverage_if_recovered - x$coverage
    cli::cli_inform(c(
      "!" = "{length(x$attempted_failed)} variable{?s} attempted but unusable: {.var {x$attempted_failed}}",
      " " = "Coverage would have been {round(x$coverage_if_recovered, 3)} - the failure costs {round(cost * 100, 1)} percentage points."
    ))
  }
  cli::cli_inform(
    "A rank-weighted model renormalises this coverage to 1.000."
  )
  invisible(x)
}


# ---- reliability ----------------------------------------------------------

#' Reliability of a layer from two independent estimates
#'
#' @description
#' Reliability is put on one scale for every layer: the correlation between
#' two independent estimates of the same quantity. Where a survey has repeat
#' coverage, that is measured directly — line against line, pass against
#' pass — and this function is how it is computed.
#'
#' @param x1,x2 Numeric vectors of equal length: two independent estimates of
#'   the same quantity at the same locations. Pairs where either is `NA` are
#'   dropped.
#' @param method Correlation method passed to [stats::cor()].
#' @param min_n Minimum number of complete pairs required. Below this the
#'   result is `NA`, which zeroes the layer's weight in
#'   [evidence_weights()] — a layer with too few overlapping observations to
#'   test has not earned any influence.
#' @param conf_level Confidence level for the returned interval.
#'
#' @return A single numeric, with attributes `n`, `ci` and `established`.
#'   `established` is `FALSE` when the interval crosses zero. Note that an
#'   unestablished layer is **not** removed: leaving it in and letting the
#'   weighting demote it is preferable to excluding it by hand, which would
#'   be a second judgement call.
#'
#' @examples
#' set.seed(1)
#' truth <- rnorm(500)
#' reliability_from_replicates(truth + rnorm(500, 0, 0.5),
#'                             truth + rnorm(500, 0, 0.5))
#' @export
reliability_from_replicates <- function(x1, x2,
                                        method     = "pearson",
                                        min_n      = 30,
                                        conf_level = 0.95) {

  if (length(x1) != length(x2)) {
    cli::cli_abort("{.arg x1} and {.arg x2} must be the same length.")
  }
  ok <- is.finite(x1) & is.finite(x2)
  n  <- sum(ok)

  if (n < min_n) {
    out <- NA_real_
    attr(out, "n")  <- n
    attr(out, "ci") <- c(NA_real_, NA_real_)
    attr(out, "established") <- FALSE
    attr(out, "note") <- sprintf(
      "only %d complete pair%s (min_n = %d): too few to test",
      n, if (n == 1) "" else "s", min_n
    )
    return(out)
  }

  ct <- stats::cor.test(x1[ok], x2[ok], method = method,
                        conf.level = conf_level)
  r  <- unname(ct$estimate)
  ci <- if (!is.null(ct$conf.int)) as.numeric(ct$conf.int) else c(NA_real_, NA_real_)

  attr(r, "n")  <- n
  attr(r, "ci") <- ci
  attr(r, "established") <-
    !anyNA(ci) && (ci[1] > 0 || ci[2] < 0)
  r
}


#' Reliability of a layer from a measured error model
#'
#' @description
#' For a layer with no repeat coverage to correlate, the same statistic is
#' obtained by injecting two independent realisations of the **measured**
#' per-observation error and recomputing. This keeps every layer's reliability
#' on one scale rather than mixing measured correlations with assumed ones.
#'
#' The error SD must be the honest one. For depth from a multi-pass survey
#' that is the between-pass disagreement over `sqrt(2)`, **not** the
#' within-cell standard error: the standard error describes scatter within a
#' cell and ignores errors that are systematic within a pass, so it
#' understates the uncertainty that matters here.
#'
#' `derive` handles quantities computed *from* the noisy layer rather than
#' measured directly. Slope, curvature and bathymetric position index all
#' inherit their error from the depth surface, and perturbing depth before
#' deriving them propagates it correctly — whereas perturbing the derived
#' layer directly would not.
#'
#' @param x Numeric vector (or matrix, for `derive` functions that need
#'   spatial structure) of the measured quantity.
#' @param sigma Numeric. SD of the per-observation error, in the units of `x`.
#' @param derive Optional function applied to each perturbed realisation
#'   before correlating, e.g. `function(z) terrain_slope(z, cell = 1)`. The
#'   default correlates `x` itself.
#' @param n_rep Number of perturbation pairs to average over. The default of
#'   1 matches a single measured comparison; raising it reduces Monte Carlo
#'   noise in the reliability estimate itself.
#' @param seed Optional integer for reproducibility. Reliability enters the
#'   weights, so a run that cannot be reproduced produces a weight table that
#'   cannot be reproduced.
#' @param ... Passed to [reliability_from_replicates()].
#'
#' @return As [reliability_from_replicates()], with attribute `method` set to
#'   `"perturbation"` and `sigma` recorded.
#'
#' @examples
#' set.seed(42)
#' depth <- 8 + rnorm(1000, 0, 1.2)
#' reliability_from_perturbation(depth, sigma = 0.308 / sqrt(2), seed = 1)
#' @export
reliability_from_perturbation <- function(x, sigma,
                                          derive = NULL,
                                          n_rep  = 1L,
                                          seed   = NULL,
                                          ...) {

  if (!is.numeric(sigma) || length(sigma) != 1 || !is.finite(sigma) || sigma < 0) {
    cli::cli_abort("{.arg sigma} must be a single non-negative finite number.")
  }
  if (!is.null(seed)) {
    old <- if (exists(".Random.seed", .GlobalEnv)) get(".Random.seed", .GlobalEnv) else NULL
    on.exit({
      if (!is.null(old)) assign(".Random.seed", old, .GlobalEnv)
    }, add = TRUE)
    set.seed(seed)
  }

  rs <- numeric(n_rep)
  ns <- integer(n_rep)
  last_ci <- c(NA_real_, NA_real_)
  for (i in seq_len(n_rep)) {
    e1 <- stats::rnorm(length(x), 0, sigma)
    e2 <- stats::rnorm(length(x), 0, sigma)
    a  <- x + array(e1, dim = dim(x) %||% length(x))
    b  <- x + array(e2, dim = dim(x) %||% length(x))
    if (!is.null(derive)) {
      a <- derive(a)
      b <- derive(b)
    }
    r     <- reliability_from_replicates(as.numeric(a), as.numeric(b), ...)
    rs[i] <- as.numeric(r)
    ns[i] <- attr(r, "n") %||% NA_integer_
    last_ci <- attr(r, "ci") %||% c(NA_real_, NA_real_)
  }

  out <- mean(rs, na.rm = TRUE)
  if (!is.finite(out)) out <- NA_real_
  attr(out, "n")           <- if (all(is.na(ns))) NA_integer_ else max(ns, na.rm = TRUE)
  attr(out, "ci")          <- if (n_rep > 1) {
    unname(stats::quantile(rs, c(0.025, 0.975), na.rm = TRUE))
  } else last_ci
  attr(out, "established") <- is.finite(out) && out > 0
  attr(out, "method")      <- "perturbation"
  attr(out, "sigma")       <- sigma
  attr(out, "n_rep")       <- n_rep
  out
}


# ---- weights --------------------------------------------------------------

#' Evidence-weighted variable weights
#'
#' @description
#' Computes `w = prior * reliability * spread`, normalised to sum to 1, and
#' returns the derivation as a table rather than only the weights. Every
#' factor traces to either the published tolerance profile (the prior) or a
#' measurement made on the survey itself (reliability, spread), so the weight
#' table is auditable line by line.
#'
#' A layer with no measured reliability receives **no weight**. That is the
#' rule that keeps the method honest: nothing enters the score on the strength
#' of an argument alone, however good the argument. It is also why every
#' candidate layer can be offered at once — the ones that do not reproduce
#' demote themselves, so inclusion costs nothing.
#'
#' @param scores A data frame of per-location scores on \[0, 1\], one column
#'   per layer, or a named list of numeric vectors. Column names are layer
#'   names. `score_` prefixes as produced by [score_locations()] are stripped.
#' @param reliability Named numeric vector of reliability per layer, from
#'   [reliability_from_replicates()] or [reliability_from_perturbation()].
#'   Layers absent from this vector, or carrying `NA`, get zero weight.
#' @param tolerances A species tolerance list from
#'   [get_species_tolerances()], used for the published ranks.
#' @param assigned Optional named list for layers with **no counterpart** in
#'   the published profile, each element a list with `rank` and `rationale`.
#'   Assigning a rank is a judgement call, and the only honest way to include
#'   such a layer is to make that call explicit rather than smuggle it in
#'   under a neighbouring variable's prior. Assigned layers are flagged as
#'   such in the weight table and excluded from coverage.
#' @param aliases Optional named character vector mapping layer names in
#'   `scores` to tolerance-spec variable names, e.g.
#'   `c(hardness = "substrate_hardness")`.
#'
#' @return An object of class `evidence_weights`: a list with `weights` (named
#'   numeric) and `table` (data frame with layer, prior variable, provenance,
#'   rank, prior, reliability, spread, raw and normalised weight).
#'
#' @details
#' `spread` is the population standard deviation of the layer's **score**, not
#' of its raw values. A variable whose every observation lands in the optimal
#' band scores 1.0 everywhere: it has spread 0 and takes weight 0, however
#' important the species finds it and however precisely it was measured. This
#' is the intended behaviour — such a variable carries no information about
#' *where within this survey* conditions differ — and it is why the method
#' reports coverage separately. A zero weight means "did not discriminate
#' here", never "does not matter".
#'
#' @export
evidence_weights <- function(scores,
                             reliability,
                             tolerances,
                             assigned = NULL,
                             aliases  = NULL) {

  if (is.data.frame(scores)) scores <- as.list(scores)
  if (!is.list(scores) || length(scores) == 0) {
    cli::cli_abort("{.arg scores} must be a non-empty data frame or named list.")
  }
  names(scores) <- sub("^score_", "", names(scores))
  scores <- scores[vapply(scores, is.numeric, logical(1))]
  if (length(scores) == 0) {
    cli::cli_abort("{.arg scores} contains no numeric columns.")
  }

  spec  <- tolerances$scored %||% list()
  ranks <- vapply(spec, function(p) as.numeric(p$rank %||% NA_real_), numeric(1))
  names(ranks) <- names(spec)

  assigned <- assigned %||% list()
  aliases  <- aliases  %||% character(0)

  rows <- list()
  for (layer in names(scores)) {

    spec_var   <- if (layer %in% names(aliases)) unname(aliases[[layer]]) else layer
    provenance <- NA_character_
    rank       <- NA_real_
    rationale  <- NA_character_

    if (spec_var %in% names(ranks)) {
      provenance <- "published"
      rank       <- ranks[[spec_var]]
    } else if (layer %in% names(assigned)) {
      provenance <- "assigned"
      rank       <- as.numeric(assigned[[layer]]$rank %||% NA_real_)
      rationale  <- assigned[[layer]]$rationale %||% NA_character_
      spec_var   <- NA_character_
      if (!is.finite(rank)) {
        cli::cli_abort(c(
          "{.arg assigned} entry {.var {layer}} needs a numeric {.field rank}.",
          "i" = "An assigned rank is a judgement call and must be stated, not defaulted."
        ))
      }
      if (is.na(rationale) || !nzchar(rationale)) {
        cli::cli_warn(c(
          "!" = "Assigned layer {.var {layer}} has no {.field rationale}.",
          "i" = "It will appear in the weight table without a justification a reader can check."
        ))
      }
    } else {
      cli::cli_warn(c(
        "!" = "Layer {.var {layer}} is neither in the tolerance spec nor in {.arg assigned}; dropped.",
        "i" = "Map it with {.arg aliases}, or declare a rank in {.arg assigned}."
      ))
      next
    }

    v      <- scores[[layer]]
    fin    <- is.finite(v)
    spread <- if (any(fin)) .pop_sd(v[fin]) else 0
    rel    <- if (layer %in% names(reliability)) {
      suppressWarnings(as.numeric(reliability[[layer]]))
    } else NA_real_
    raw    <- if (!is.finite(rel)) 0 else max((1 / rank) * rel * spread, 0)

    rows[[length(rows) + 1L]] <- data.frame(
      layer        = layer,
      prior_var    = spec_var,
      provenance   = provenance,
      rank         = as.integer(rank),
      prior        = 1 / rank,
      reliability  = rel,
      spread       = spread,
      raw          = raw,
      n_scored     = sum(fin),
      rationale    = rationale,
      stringsAsFactors = FALSE
    )
  }

  if (length(rows) == 0) {
    cli::cli_abort("No layer in {.arg scores} could be matched to a prior.")
  }

  tbl <- do.call(rbind, rows)
  tot <- sum(tbl$raw)
  tbl$weight <- if (tot > 0) tbl$raw / tot else 0
  tbl <- tbl[order(-tbl$weight, tbl$rank), ]
  rownames(tbl) <- NULL

  w <- stats::setNames(tbl$weight, tbl$layer)

  zeroed <- tbl$layer[tbl$weight == 0]
  if (length(zeroed) > 0) {
    why <- ifelse(!is.finite(tbl$reliability[tbl$weight == 0]),
                  "no measured reliability",
                  ifelse(tbl$spread[tbl$weight == 0] == 0,
                         "no spread in score", "reliability <= 0"))
    cli::cli_inform(c(
      "i" = "{length(zeroed)} layer{?s} carr{?ies/y} zero weight:",
      "*" = "{paste0(zeroed, ' (', why, ')')}"
    ))
  }

  out <- list(weights = w, table = tbl,
              species = tolerances$latin_name %||% NA_character_)
  class(out) <- "evidence_weights"
  out
}

#' @export
print.evidence_weights <- function(x, ...) {
  cli::cli_h3("Evidence weights{if (!is.na(x$species)) paste0(' - ', x$species) else ''}")
  show <- x$table[, c("layer", "prior_var", "provenance", "rank",
                      "prior", "reliability", "spread", "weight")]
  show$prior       <- round(show$prior, 3)
  show$reliability <- round(show$reliability, 3)
  show$spread      <- round(show$spread, 3)
  show$weight      <- round(show$weight, 3)
  print(show, row.names = FALSE)
  if (any(x$table$provenance == "assigned")) {
    cli::cli_inform(c(
      "i" = "Layers marked {.val assigned} have no counterpart in the published profile; their rank was chosen for this run and does not count toward coverage."
    ))
  }
  invisible(x)
}


#' @keywords internal
.pop_sd <- function(v) {
  n <- length(v)
  if (n < 1) return(0)
  sqrt(sum((v - mean(v))^2) / n)
}


# ---- per-location confidence ---------------------------------------------

#' Combine per-location confidence components
#'
#' @description
#' Per-location confidence **within the observed layers**, kept deliberately
#' separate from coverage. Multiplying the two would drive every location below
#' any usable class threshold on a low-coverage survey — technically honest,
#' useless as a product. So coverage is reported once, loudly, at survey level,
#' and this carries the location-to-location variation.
#'
#' @param components A named list of numeric vectors on \[0, 1\], each a
#'   confidence component — for example how a class was derived (a fitted
#'   physical model, a cluster assignment, an ordinal bin) or whether a
#'   location had an independent second observation.
#' @param weights Numeric vector of component weights, recycled by name if
#'   named. Normalised to sum to 1.
#'
#' @return Numeric vector of combined confidence.
#'
#' @examples
#' route  <- c(1.0, 0.6, 0.3, 1.0)
#' passes <- pmin(pmax(c(3, 1, 1, 2) / 2, 0.4), 1)
#' confidence_score(list(route = route, passes = passes), c(0.65, 0.35))
#' @export
confidence_score <- function(components, weights) {

  if (!is.list(components) || length(components) == 0) {
    cli::cli_abort("{.arg components} must be a non-empty named list.")
  }
  lens <- vapply(components, length, integer(1))
  if (length(unique(lens)) != 1) {
    cli::cli_abort("All {.arg components} must be the same length; got {.val {lens}}.")
  }
  if (!is.null(names(weights)) && !is.null(names(components))) {
    weights <- weights[names(components)]
  }
  if (length(weights) != length(components) || anyNA(weights)) {
    cli::cli_abort("{.arg weights} must give one finite weight per component.")
  }

  rng <- range(unlist(components), na.rm = TRUE)
  if (is.finite(rng[1]) && (rng[1] < 0 || rng[2] > 1)) {
    cli::cli_warn("Confidence components outside [0, 1]; result will be too.")
  }

  weights <- weights / sum(weights)
  Reduce(`+`, Map(function(v, w) w * v, components, weights))
}


# ---- the scoring mode -----------------------------------------------------

#' Evidence-weighted suitability
#'
#' @description
#' Re-weights scores already produced by [score_locations()] using
#' `prior * reliability * spread` instead of `1 / rank`, and attaches the
#' coverage of the species' evidence base to the result.
#'
#' The split of labour is deliberate: the existing scoring curves supply the
#' per-variable scores, and this supplies only the weights. Curve shapes are
#' where site-specific judgement legitimately lives and are unchanged.
#'
#' @param df A data frame already through [score_locations()], carrying
#'   `score_<variable>` columns.
#' @param tolerances A species tolerance list from
#'   [get_species_tolerances()].
#' @param reliability Named numeric vector of measured reliability per layer.
#' @param confidence Optional numeric vector of per-location confidence, e.g.
#'   from [confidence_score()]. Locations below `confidence_floor` are
#'   classified `"Insufficient data"` rather than given a class — an
#'   uncertain location should not be handed a label it cannot support. The
#'   continuous score is still returned for every location.
#' @param confidence_floor Numeric. Default 0.5.
#' @param assigned,aliases Passed to [evidence_weights()].
#' @param extra_scores Optional data frame of additional score columns on
#'   \[0, 1\] not produced by the standard curves (layers declared in
#'   `assigned`, typically). Must have `nrow(df)` rows.
#' @param observed Optional character vector of tolerance-spec variables the
#'   survey observed, for coverage. Defaults to the published layers found in
#'   `df`, which is right only if every observed variable was also scorable —
#'   pass it explicitly when a variable was measured but could not be scored.
#' @param attempted_failed Passed to [evidence_coverage()].
#'
#' @return `df` with added columns `suitability_evidence`,
#'   `suitability_class_evidence` and, if `confidence` was supplied,
#'   `confidence`. The weight table and coverage are attached as attributes
#'   `"evidence_weights"` and `"evidence_coverage"` and are what belongs in a
#'   methods section.
#'
#' @export
evidence_suitability <- function(df,
                                 tolerances,
                                 reliability,
                                 confidence       = NULL,
                                 confidence_floor = 0.5,
                                 assigned         = NULL,
                                 aliases          = NULL,
                                 extra_scores     = NULL,
                                 observed         = NULL,
                                 attempted_failed = NULL) {

  if (!is.data.frame(df)) cli::cli_abort("{.arg df} must be a data frame.")

  score_cols <- grep("^score_", names(df), value = TRUE)
  scores <- if (length(score_cols) > 0) df[, score_cols, drop = FALSE] else
    data.frame(row.names = seq_len(nrow(df)))
  names(scores) <- sub("^score_", "", names(scores))

  if (!is.null(extra_scores)) {
    if (nrow(extra_scores) != nrow(df)) {
      cli::cli_abort(c(
        "{.arg extra_scores} must have the same number of rows as {.arg df}.",
        "x" = "Got {nrow(extra_scores)} vs {nrow(df)}."
      ))
    }
    scores <- cbind(scores, extra_scores)
  }
  if (ncol(scores) == 0) {
    cli::cli_abort(c(
      "No score columns found.",
      "i" = "Run {.fn score_locations} first, or supply {.arg extra_scores}."
    ))
  }

  rng <- suppressWarnings(range(unlist(scores), na.rm = TRUE))
  if (all(is.finite(rng)) && (rng[1] < 0 || rng[2] > 1)) {
    cli::cli_warn(c(
      "!" = "Score columns fall outside [0, 1] (range {round(rng[1], 3)} to {round(rng[2], 3)}).",
      "i" = "The weighted mean inherits that range, so {.field suitability_evidence} will not be a suitability on [0, 1] and the class breaks will not mean what they say."
    ))
  }

  ew <- evidence_weights(scores, reliability, tolerances,
                         assigned = assigned, aliases = aliases)

  used <- names(ew$weights)
  num  <- rep(0, nrow(df))
  den  <- rep(0, nrow(df))
  for (layer in used) {
    v <- scores[[layer]]
    m <- is.finite(v)
    w <- ew$weights[[layer]]
    num[m] <- num[m] + w * v[m]
    den[m] <- den[m] + w
  }
  suit <- ifelse(den > 0, num / den, NA_real_)

  if (is.null(observed)) {
    pub      <- ew$table$prior_var[ew$table$provenance == "published"]
    observed <- unique(pub[!is.na(pub)])
  }
  cov <- evidence_coverage(tolerances, observed = observed,
                           attempted_failed = attempted_failed)

  cls <- cut(suit,
             breaks = c(-Inf, 0.20, 0.45, 0.70, Inf),
             labels = c("Very Low", "Low", "Moderate", "High"),
             right  = FALSE)
  cls <- as.character(cls)

  if (!is.null(confidence)) {
    if (length(confidence) != nrow(df)) {
      cli::cli_abort("{.arg confidence} must have one value per row of {.arg df}.")
    }
    df$confidence <- confidence
    cls[!is.finite(confidence) | confidence < confidence_floor] <- "Insufficient data"
  }
  cls[!is.finite(suit)] <- "Insufficient data"

  df$suitability_evidence       <- suit
  df$suitability_class_evidence <- cls

  attr(df, "evidence_weights")  <- ew
  attr(df, "evidence_coverage") <- cov

  cli::cli_h3("Evidence-weighted suitability")
  cli::cli_inform(c(
    "Coverage: {.strong {round(cov$coverage, 3)}} of the {.val {cov$species}} evidence base",
    "Mean suitability: {round(mean(suit, na.rm = TRUE), 3)} over {sum(is.finite(suit))} location{?s}"
  ))
  if ("suitability" %in% names(df)) {
    shift <- mean(suit, na.rm = TRUE) - mean(df$suitability, na.rm = TRUE)
    cli::cli_inform(
      "Shift from rank weighting: {sprintf('%+.3f', shift)}"
    )
  }
  # Hard-stop exclusions have no score, so they land in "Insufficient data"
  # here; the caller restores them to "Excluded". Tally only the rest so
  # this summary does not report excluded ground as a data shortfall.
  keep <- if ("excluded" %in% names(df)) !(df$excluded %in% TRUE) else rep(TRUE, length(cls))
  cls_k <- cls[keep]
  for (k in c("High", "Moderate", "Low", "Very Low", "Insufficient data")) {
    n <- sum(cls_k == k, na.rm = TRUE)
    if (n > 0) cli::cli_inform("  {k}: {n} ({round(100 * n / length(cls_k), 1)}%)")
  }

  df
}


# ---- reporting ------------------------------------------------------------

#' Weight derivation as a markdown table
#'
#' @description
#' Renders the weight derivation and the coverage ledger as markdown, for
#' pasting into a methods section or a supplementary file. Every number in the
#' output is read back from the objects rather than transcribed, so
#' re-rendering after a change refreshes the whole table.
#'
#' @param weights An `evidence_weights` object, or a data frame returned by
#'   [evidence_suitability()] carrying it as an attribute.
#' @param coverage An `evidence_coverage` object. Taken from the attribute if
#'   `weights` is a scored data frame.
#' @param title Optional heading.
#' @param file Optional path. Written there if given, and returned invisibly.
#'
#' @return A character vector of markdown lines.
#' @export
weight_table_md <- function(weights, coverage = NULL, title = NULL,
                            file = NULL) {

  if (is.data.frame(weights)) {
    coverage <- coverage %||% attr(weights, "evidence_coverage")
    weights  <- attr(weights, "evidence_weights")
  }
  if (!inherits(weights, "evidence_weights")) {
    cli::cli_abort("{.arg weights} must be an {.cls evidence_weights} object or a scored data frame.")
  }

  tbl <- weights$table
  L <- c(
    sprintf("### %s", title %||% sprintf("%s - weight derivation",
                                         weights$species %||% "Survey")),
    ""
  )

  if (!is.null(coverage)) {
    L <- c(L, sprintf(
      paste0("Coverage: **%.3f** (%.3f of %.3f total prior weight for *%s*). ",
             "A rank-weighted model renormalises this to 1.000."),
      coverage$coverage, coverage$prior_observed, coverage$prior_total,
      coverage$species %||% "the species"), "")
  }

  L <- c(L,
    "| Layer | Prior variable | Prior source | Rank | Prior 1/rank | Reliability | Spread | Weight |",
    "|---|---|---|---|---|---|---|---|")
  for (i in seq_len(nrow(tbl))) {
    r <- tbl[i, ]
    L <- c(L, sprintf("| %s | %s | %s | %d | %.3f | %s | %.3f | **%.3f** |",
      r$layer,
      if (is.na(r$prior_var)) "(none)" else r$prior_var,
      r$provenance, r$rank, r$prior,
      if (is.finite(r$reliability)) sprintf("%+.3f", r$reliability) else "not measured",
      r$spread, r$weight))
  }

  asg <- tbl[tbl$provenance == "assigned", ]
  if (nrow(asg) > 0) {
    L <- c(L, "",
      paste0("Layers marked *assigned* have no counterpart in the published ",
             "profile; their rank was chosen for this run and is stated below. ",
             "They do **not** count toward coverage."))
    for (i in seq_len(nrow(asg))) {
      L <- c(L, sprintf("- `%s` (rank %d): %s", asg$layer[i], asg$rank[i],
                        if (is.na(asg$rationale[i])) "no rationale supplied" else asg$rationale[i]))
    }
  }

  if (!is.null(coverage)) {
    n_status <- function(k) sum(coverage$ledger$status == k)
    L <- c(L, "", sprintf(
      "Of %d scored variables: %d present, %d attempted but unusable, %d not collected.",
      coverage$n_scored_spec, n_status("present"),
      n_status("attempted_failed"), n_status("not_collected")))
    if (length(coverage$attempted_failed) > 0) {
      L <- c(L, "", sprintf(
        paste0("Had the unusable payload returned data, coverage would have ",
               "been **%.3f** rather than %.3f - the instrument failure alone ",
               "costs %.1f percentage points of the species' evidence base."),
        coverage$coverage_if_recovered, coverage$coverage,
        100 * (coverage$coverage_if_recovered - coverage$coverage)))
    }
    L <- c(L, "",
      "| Variable | Rank | Prior | Status |", "|---|---|---|---|")
    for (i in seq_len(nrow(coverage$ledger))) {
      r <- coverage$ledger[i, ]
      L <- c(L, sprintf("| %s | %d | %.3f | %s |",
                        r$variable, r$rank, r$prior, r$status))
    }
  }

  if (!is.null(file)) {
    writeLines(L, file)
    cli::cli_inform("Weight table written to {.file {file}}")
    return(invisible(L))
  }
  L
}
