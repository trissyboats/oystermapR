# ---- survey diagnostics: reliability measured from the survey itself --------

#' Reliability of each layer from overlapping survey lines
#'
#' @description
#' Measures, layer by layer, how well two independent passes over the same
#' ground agree. This is the `reliability` that
#' `predict_oyster(weighting = "evidence")` needs, taken from the survey
#' rather than typed in.
#'
#' The survey is binned to a grid. In every cell crossed by two or more lines
#' each line's mean is taken, the lines are paired, and the pairs go to
#' [reliability_from_replicates()]. Nothing is assumed about the instrument:
#' a layer that does not reproduce between lines scores near zero, and a layer
#' with too few overlapping cells to test returns `NA`, which gives it no
#' weight in [evidence_weights()].
#'
#' @section What it does not tell you:
#' Agreement between lines is reproducibility, not accuracy. A bias shared by
#' every line (a mis-calibrated sensor) reproduces perfectly. A large-scale
#' gradient across the site also raises the correlation, because cells differ
#' from one another more than lines differ within a cell; for a layer where
#' that matters, detrend first.
#'
#' @param df A survey data frame with coordinates (`lat`/`lon` or synonyms), a
#'   column identifying the survey line or pass, and the layers to test.
#'   Rows should be in acquisition order within each line: the automatic cell
#'   size is taken from the spacing between consecutive rows of a line.
#' @param layers Character vector of survey column names to test. `NULL`
#'   (default) tests every numeric column recognised as a scored layer.
#' @param line_col Name of the line/pass column. `NULL` looks for `line_id`,
#'   `line`, `survey_line`, `pass`, `pass_id`, `transect` or `track`.
#' @param cell_size_m Grid cell size in metres. `NULL` uses three times the
#'   median spacing between consecutive points along a line, so that
#'   neighbouring lines land in a common cell.
#' @param min_n Minimum overlapping pairs needed; passed to
#'   [reliability_from_replicates()].
#' @param method Correlation method, `"pearson"` (default) or `"spearman"`.
#' @param conf_level Confidence level for the intervals.
#'
#' @return An object of class `layer_reproducibility`, a list with
#'   * `reliability`: named numeric, one entry per tested layer, named as the
#'     scored layer (`depth`, not `depth_m`), `NA` where untestable. Pass this
#'     to `predict_oyster(reliability = )`.
#'   * `table`: data frame with `layer`, `column`, `r`, `ci_low`, `ci_high`,
#'     `n_pairs`, `n_cells`, `established` and `note`.
#'   * `cell_size_m`, `n_lines`, `line_col`.
#'
#' @seealso [reliability_from_replicates()], [evidence_weights()]
#' @export
#' @examples
#' set.seed(1)
#' g  <- expand.grid(lon = seq(0, 0.01, length.out = 40),
#'                   lat = seq(50, 50.01, length.out = 40))
#' truth <- sin(g$lon * 900) + cos(g$lat * 700)
#' pass <- function(id, sd) transform(g, line_id = id,
#'   depth_m = truth + rnorm(nrow(g), 0, sd),
#'   turbidity = rnorm(nrow(g)))
#' survey <- rbind(pass("A", 0.2), pass("B", 0.2))
#' layer_reproducibility(survey, cell_size_m = 5)
layer_reproducibility <- function(df,
                                  layers      = NULL,
                                  line_col    = NULL,
                                  cell_size_m = NULL,
                                  min_n       = 30,
                                  method      = c("pearson", "spearman"),
                                  conf_level  = 0.95) {

  method <- match.arg(method)
  df <- as.data.frame(df, stringsAsFactors = FALSE)
  lwr <- tolower(names(df))

  lon_i <- which(lwr %in% c("lon", "lng", "longitude", "x"))[1]
  lat_i <- which(lwr %in% c("lat", "latitude", "y"))[1]
  if (is.na(lon_i) || is.na(lat_i)) {
    cli::cli_abort("Could not find coordinate columns ({.val lat}, {.val lon}).")
  }

  if (is.null(line_col)) {
    line_i <- which(lwr %in% .line_col_names())[1]
    if (is.na(line_i)) {
      cli::cli_abort(c(
        "No line or pass column found.",
        "i" = "Add a column identifying the survey line, named one of {.val {(.line_col_names())}}, or pass {.arg line_col}.",
        "i" = "Reliability is measured from lines that cross the same ground, so the survey needs at least two."
      ))
    }
    line_col <- names(df)[line_i]
  } else if (!line_col %in% names(df)) {
    cli::cli_abort("{.arg line_col} {.val {line_col}} is not a column of {.arg df}.")
  }

  line <- as.character(df[[line_col]])
  n_lines <- length(unique(stats::na.omit(line)))
  if (n_lines < 2) {
    cli::cli_abort("Only {n_lines} line found in {.field {line_col}}: reproducibility needs at least two.")
  }

  aliases <- .layer_aliases()
  if (is.null(layers)) {
    layers <- names(df)[lwr %in% unlist(aliases) &
                        vapply(df, is.numeric, logical(1))]
    layers <- setdiff(layers, names(df)[c(lon_i, lat_i)])
    if (length(layers) == 0) {
      cli::cli_abort("No numeric scored-layer columns found. Name them with {.arg layers}.")
    }
  }
  missing_layers <- setdiff(layers, names(df))
  if (length(missing_layers) > 0) {
    cli::cli_abort("Layer column{?s} not in {.arg df}: {.val {missing_layers}}.")
  }

  # ---- project to metres, then to a grid -----------------------------------
  lon <- as.numeric(df[[lon_i]]); lat <- as.numeric(df[[lat_i]])
  lat0 <- mean(lat, na.rm = TRUE)
  x <- (lon - mean(lon, na.rm = TRUE)) * 111320 * cos(lat0 * pi / 180)
  y <- (lat - lat0) * 110540

  if (is.null(cell_size_m)) {
    step <- unlist(lapply(split(seq_along(x), line), function(ix) {
      if (length(ix) < 2) return(numeric(0))
      sqrt(diff(x[ix])^2 + diff(y[ix])^2)
    }))
    step <- step[is.finite(step) & step > 0]
    cell_size_m <- if (length(step) > 0) 3 * stats::median(step) else NA_real_
    if (!is.finite(cell_size_m) || cell_size_m < 1) cell_size_m <- 1
  }
  if (!is.numeric(cell_size_m) || length(cell_size_m) != 1 || cell_size_m <= 0) {
    cli::cli_abort("{.arg cell_size_m} must be a single positive number of metres.")
  }
  cell <- paste(floor(x / cell_size_m), floor(y / cell_size_m))

  # ---- per layer -------------------------------------------------------------
  rows <- lapply(layers, function(col) {
    v  <- as.numeric(df[[col]])
    ok <- is.finite(v) & is.finite(x) & is.finite(y) & !is.na(line)
    key <- paste(cell[ok], line[ok], sep = "\r")
    sums <- rowsum(v[ok], key, reorder = FALSE)
    cnt  <- rowsum(rep(1, sum(ok)), key, reorder = FALSE)
    kk   <- rownames(sums)
    mean_v  <- as.numeric(sums) / as.numeric(cnt)
    cell_of <- sub("\r.*$", "", kk)

    by_cell <- split(seq_along(kk), cell_of)
    by_cell <- by_cell[lengths(by_cell) >= 2]

    a <- numeric(0); b <- numeric(0)
    if (length(by_cell) > 0) {
      pairs <- lapply(by_cell, function(ix) {
        ix <- ix[seq_len(min(length(ix), 5L))]   # cap lines per cell
        utils::combn(ix, 2)
      })
      pm <- do.call(cbind, pairs)
      # Alternate orientation so the pooled correlation does not depend on
      # which line happened to be listed first.
      flip <- (seq_len(ncol(pm)) %% 2L) == 0L
      first  <- ifelse(flip, pm[2, ], pm[1, ])
      second <- ifelse(flip, pm[1, ], pm[2, ])
      a <- mean_v[first]; b <- mean_v[second]
    }

    r <- reliability_from_replicates(a, b, method = method, min_n = min_n,
                                     conf_level = conf_level)
    ci <- attr(r, "ci")
    layer_name <- .resolve_layer_name(col, aliases)
    data.frame(layer = layer_name, column = col,
               r = as.numeric(r), ci_low = ci[1], ci_high = ci[2],
               n_pairs = attr(r, "n"), n_cells = length(by_cell),
               established = isTRUE(attr(r, "established")),
               note = attr(r, "note") %||% NA_character_,
               stringsAsFactors = FALSE)
  })
  tbl <- do.call(rbind, rows)

  dup <- tbl$layer[duplicated(tbl$layer)]
  if (length(dup) > 0) {
    cli::cli_abort("Two columns map to the same layer: {.val {unique(dup)}}. Test one.")
  }

  structure(
    list(reliability = stats::setNames(tbl$r, tbl$layer),
         table       = tbl,
         cell_size_m = cell_size_m,
         n_lines     = n_lines,
         line_col    = line_col),
    class = "layer_reproducibility"
  )
}


#' @export
print.layer_reproducibility <- function(x, ...) {
  cli::cli_h3("Layer reproducibility")
  cli::cli_inform(
    "{x$n_lines} lines ({.field {x$line_col}}), grid cell {round(x$cell_size_m, 1)} m")
  t <- x$table
  for (i in seq_len(nrow(t))) {
    if (is.na(t$r[i])) {
      cli::cli_inform("  {t$layer[i]}: not testable ({t$note[i]})")
    } else {
      cli::cli_inform(
        "  {t$layer[i]}: r = {sprintf('%+.3f', t$r[i])} [{sprintf('%+.3f', t$ci_low[i])}, {sprintf('%+.3f', t$ci_high[i])}], {t$n_pairs[i]} pairs in {t$n_cells[i]} cells{if (!t$established[i]) ' -- NOT established' else ''}")
    }
  }
  invisible(x)
}


# Names recognised as the survey-line column. Mirrored in the SaaS API
# (api/runs.py _LINE_COLUMN_NAMES); keep the two in step.
.line_col_names <- function() {
  c("line_id", "line", "survey_line", "pass", "pass_id", "transect", "track")
}

# Survey column synonyms -> scored layer name. Same table as .score_row();
# duplicated rather than shared so the scoring path stays untouched.
.layer_aliases <- function() {
  list(
    temperature         = c("temperature", "temp", "temp_c", "water_temp"),
    fishing_intensity   = c("fishing_intensity", "fishing", "fishing_observed"),
    shear_stress        = c("shear_stress", "tau", "bed_shear", "shear"),
    chlorophyll_a       = c("chlorophyll_a", "chla", "chl_a", "chlorophyll"),
    sediment_type       = c("sediment_type", "sediment", "substrate_type"),
    substrate_hardness  = c("substrate_hardness", "hardness", "bottom_hardness"),
    benthic_communities = c("benthic_communities", "benthic", "community"),
    depth               = c("depth", "depth_m"),
    biotope             = c("biotope", "biotopes", "habitat"),
    roughness           = c("roughness", "rugosity"),
    slope               = c("slope", "slope_deg"),
    turbidity           = c("turbidity", "ntu", "turb"),
    current_velocity    = c("current_velocity", "velocity", "current", "u_mean"),
    ph                  = c("ph", "seawater_ph", "sea_ph", "water_ph"),
    omega_aragonite     = c("omega_aragonite", "omega_arag", "aragonite_saturation",
                            "aragonite"),
    dissolved_oxygen    = c("dissolved_oxygen", "do", "do_mgl", "oxygen",
                            "dissolved_o2", "do_mg_l"),
    salinity            = c("salinity", "sal", "salinity_psu", "psu", "sal_psu")
  )
}

.resolve_layer_name <- function(col, aliases) {
  hit <- names(aliases)[vapply(aliases, function(a) tolower(col) %in% a, logical(1))]
  if (length(hit) > 0) hit[1] else tolower(col)
}
