# =============================================================================
# Spatial interpolation of survey measurements
# =============================================================================
#
# Method selection logic:
#  - Ordinary kriging (via gstat) when n >= 50 observations \u2014 statistically
#    optimal (BLUE), returns prediction variance, handles irregular sampling.
#    Requires: gstat, sp packages.
#  - IDW fallback when n < 50, when gstat is unavailable, or when the
#    variogram cannot be fitted. IDW is implemented internally \u2014 no extra
#    packages required.
#  - Force method with method = "kriging" or method = "idw".
#
# References:
#  Cressie (1993): Statistics for Spatial Data \u2014 kriging theory
#  Shepard (1968): IDW \u2014 original formulation
#  Webster & Oliver (2007): Geostatistics for Environmental Scientists

#' Interpolate survey measurements to a regular grid before scoring
#'
#' @description
#' Converts sparse point observations (CTD casts, ADCP profiles) to a regular
#' grid by spatial interpolation. The interpolated grid is returned in the same
#' dataframe format as the raw survey, and can be passed directly to
#' [predict_oyster()].
#'
#' **Method selection** (applied per variable independently):
#' - **Ordinary kriging** is used when >= 50 non-NA observations are available
#'   for a variable, the `gstat` and `sp` packages are installed, and the
#'   variogram can be fitted without error. Kriging is the best linear
#'   unbiased estimator (BLUE) under the assumption of spatial stationarity —
#'   it minimises mean squared prediction error and returns a kriging variance
#'   for each cell.
#' - **IDW** (Inverse Distance Weighting, p = 2) is used as a fallback when
#'   kriging cannot be applied. It is fast, deterministic, and requires no
#'   model fitting, but produces no uncertainty estimate and can create
#'   bull's-eye artefacts around isolated observations.
#'
#' Kriging variograms are fitted automatically using `gstat::fit.variogram()`
#' with a Matérn model (default) — the most flexible and statistically
#' justified model for environmental data. If automatic fitting fails, a
#' spherical model is tried; if that also fails, IDW is used.
#'
#' @param survey Dataframe with `lat`, `lon`, and measurement columns.
#' @param vars Character vector of column names to interpolate. Default: all
#'   numeric columns except `lat` and `lon`.
#' @param resolution_m Numeric. Grid cell size in metres (default 100 m).
#'   Coarsen for sparse surveys (> 500 m) or refine for dense ADCP grids (50 m).
#' @param method Character. `"auto"` (default — kriging if possible, IDW
#'   fallback), `"kriging"` (force kriging; error if insufficient data), or
#'   `"idw"` (always use IDW).
#' @param kriging_min_n Integer. Minimum observations required to attempt
#'   kriging (default 50).
#' @param idw_power Numeric. IDW distance decay exponent (default 2).
#' @param idw_max_radius_m Numeric. Maximum search radius for IDW neighbours
#'   in metres (default 5 * resolution_m).
#' @param verbose Logical. Print per-variable method used and diagnostics
#'   (default TRUE).
#'
#' @return A dataframe on a regular grid with interpolated values, a `method`
#'   column per variable (`interp_method_<var>`), and kriging variance columns
#'   (`krige_var_<var>`) where kriging was used.
#'
#' @export
#' @examples
#' \dontrun{
#' # Auto method — kriging for dense variables, IDW for sparse
#' grid <- interpolate_survey(survey, resolution_m = 100)
#' result <- predict_oyster(grid, "ostrea_edulis")
#'
#' # Force IDW (fast, no gstat needed)
#' grid <- interpolate_survey(survey, resolution_m = 200, method = "idw")
#'
#' # Inspect which method was used per variable
#' unique(grid$interp_method_temperature)
#'
#' # Kriging variance — high values = uncertain interpolation
#' hist(grid$krige_var_temperature)
#' }
interpolate_survey <- function(survey,
                                vars           = NULL,
                                resolution_m   = 100,
                                method         = c("auto","kriging","idw"),
                                kriging_min_n  = 50L,
                                idw_power      = 2,
                                idw_max_radius_m = NULL,
                                verbose        = TRUE) {

  method <- match.arg(method)

  required <- c("lat","lon")
  if (!all(required %in% names(survey)))
    cli::cli_abort("survey must contain 'lat' and 'lon' columns.")

  if (is.null(vars)) {
    vars <- setdiff(
      names(survey)[vapply(survey, is.numeric, logical(1))],
      c("lat","lon")
    )
  }
  vars <- intersect(vars, names(survey))
  if (length(vars) == 0)
    cli::cli_abort("No numeric columns found to interpolate.")

  if (is.null(idw_max_radius_m))
    idw_max_radius_m <- 5 * resolution_m

  # \u2500\u2500 Build regular grid \u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500
  lat_mid   <- mean(survey$lat, na.rm = TRUE)
  m_per_lon <- 111320 * cos(lat_mid * pi / 180)
  m_per_lat <- 111320

  lon_range <- range(survey$lon, na.rm = TRUE)
  lat_range <- range(survey$lat, na.rm = TRUE)

  # Pad by one cell on each side
  pad_lon <- resolution_m / m_per_lon
  pad_lat <- resolution_m / m_per_lat

  grid_lon <- seq(lon_range[1] - pad_lon, lon_range[2] + pad_lon,
                  by = resolution_m / m_per_lon)
  grid_lat <- seq(lat_range[1] - pad_lat, lat_range[2] + pad_lat,
                  by = resolution_m / m_per_lat)

  grid <- expand.grid(lon = grid_lon, lat = grid_lat)
  gx   <- grid$lon * m_per_lon
  gy   <- grid$lat * m_per_lat
  sx   <- survey$lon * m_per_lon
  sy   <- survey$lat * m_per_lat

  # Check for gstat availability (only needed for kriging)
  has_gstat <- requireNamespace("gstat", quietly = TRUE) &&
               requireNamespace("sp",    quietly = TRUE)

  if (method == "kriging" && !has_gstat)
    cli::cli_abort(c(
      "method = 'kriging' requires packages gstat and sp.",
      "i" = "Install with: install.packages(c('gstat','sp'))",
      "i" = "Or use method = 'auto' to fall back to IDW automatically."
    ))

  if (verbose)
    cli::cli_h2("Spatial Interpolation \u2014 {length(vars)} variable{?s}, {nrow(grid)} grid cells")

  # \u2500\u2500 Interpolate each variable \u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500
  for (v in vars) {
    obs      <- survey[[v]]
    valid    <- !is.na(obs)
    n_valid  <- sum(valid)

    use_kriging <- FALSE
    if (method == "kriging") {
      if (n_valid < kriging_min_n)
        cli::cli_abort(c(
          "Insufficient observations for kriging of {.val {v}}: {n_valid} < {kriging_min_n}.",
          "i" = "Use method = 'auto' to fall back to IDW."
        ))
      use_kriging <- TRUE
    } else if (method == "auto") {
      use_kriging <- n_valid >= kriging_min_n && has_gstat
    }

    if (use_kriging) {
      result_v <- tryCatch(
        .krige_variable(survey, v, grid, valid, m_per_lon, m_per_lat, verbose),
        error = function(e) {
          if (verbose)
            cli::cli_warn("Kriging failed for {.val {v}} ({conditionMessage(e)}). Using IDW.")
          NULL
        }
      )
      if (!is.null(result_v)) {
        grid[[v]]                            <- result_v$pred
        grid[[paste0("krige_var_", v)]]      <- result_v$var
        grid[[paste0("interp_method_", v)]]  <- "kriging"
        next
      }
    }

    # IDW fallback
    grid[[v]]                           <- .idw_variable(
      obs[valid], sx[valid], sy[valid], gx, gy,
      power = idw_power, max_radius = idw_max_radius_m
    )
    grid[[paste0("interp_method_", v)]] <- "idw"

    if (verbose) {
      reason <- if (n_valid < kriging_min_n) paste0("n=", n_valid, " < ", kriging_min_n)
                else if (!has_gstat) "gstat not available"
                else "IDW selected"
      cli::cli_inform("  {v}: IDW ({reason})")
    }
  }

  if (verbose) {
    n_krig <- sum(vapply(vars, function(v) {
      col <- paste0("interp_method_", v)
      col %in% names(grid) && any(grid[[col]] == "kriging", na.rm = TRUE)
    }, logical(1)))
    cli::cli_inform(c(
      "\u2713" = paste0("Done. Grid: ", nrow(grid), " cells | ",
                        n_krig, " variable{?s} kriged, ",
                        length(vars) - n_krig, " IDW")
    ))
  }

  grid
}


# \u2500\u2500 Internal: ordinary kriging via gstat \u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500

.krige_variable <- function(survey, v, grid, valid, m_per_lon, m_per_lat,
                             verbose) {

  obs <- survey[[v]][valid]
  sx  <- survey$lon[valid] * m_per_lon
  sy  <- survey$lat[valid] * m_per_lat
  gx  <- grid$lon * m_per_lon
  gy  <- grid$lat * m_per_lat

  # Build sp objects
  sp_data <- sp::SpatialPointsDataFrame(
    coords = cbind(sx, sy),
    data   = data.frame(z = obs)
  )
  sp_grid <- sp::SpatialPoints(cbind(gx, gy))

  # Fit variogram \u2014 try Matern first, fall back to spherical
  vgm_emp <- gstat::variogram(z ~ 1, data = sp_data)

  fit_v <- tryCatch(
    gstat::fit.variogram(vgm_emp,
      gstat::vgm(psill = stats::var(obs),
                 model = "Mat",
                 range = diff(range(sx)) / 3,
                 nugget = stats::var(obs) * 0.1,
                 kappa = 1.5)),   # Matern smoothness
    error = function(e) NULL
  )

  if (is.null(fit_v) || inherits(fit_v, "try-error")) {
    fit_v <- tryCatch(
      gstat::fit.variogram(vgm_emp,
        gstat::vgm(psill = stats::var(obs),
                   model = "Sph",
                   range = diff(range(sx)) / 3,
                   nugget = stats::var(obs) * 0.1)),
      error = function(e) stop(paste0("Variogram fitting failed: ", e$message))
    )
  }

  if (verbose) {
    model_used <- as.character(fit_v$model[2])
    cli::cli_inform(paste0("  ", v, ": kriging (", model_used, " variogram)"))
  }

  # Kriging prediction
  g    <- gstat::gstat(formula = z ~ 1, data = sp_data, model = fit_v)
  pred <- predict(g, sp_grid, debug.level = 0)

  list(
    pred = pred$var1.pred,
    var  = pred$var1.var
  )
}


# \u2500\u2500 Internal: IDW (no external dependencies) \u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500

.idw_variable <- function(obs, sx, sy, gx, gy, power = 2, max_radius) {

  n_grid <- length(gx)
  result <- numeric(n_grid)

  for (i in seq_len(n_grid)) {
    dx   <- sx - gx[i]
    dy   <- sy - gy[i]
    dist <- sqrt(dx^2 + dy^2)

    in_r <- dist <= max_radius
    if (!any(in_r)) {
      # No neighbours in radius \u2014 use nearest single point
      in_r[which.min(dist)] <- TRUE
    }

    d_sub  <- dist[in_r]
    o_sub  <- obs[in_r]

    # Exact hits (distance = 0) \u2014 return observed value
    exact <- d_sub == 0
    if (any(exact)) {
      result[i] <- mean(o_sub[exact])
      next
    }

    w      <- 1 / (d_sub^power)
    result[i] <- sum(w * o_sub) / sum(w)
  }

  result
}
