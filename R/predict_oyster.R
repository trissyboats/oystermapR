#' Predict oyster growth suitability from environmental survey data
#'
#' @description
#' The primary user-facing function of `oystermapR`. Accepts a tabular dataset
#' (CSV file path or dataframe) containing spatial coordinates and environmental
#' measurements, applies species-specific exclusion and weighted scoring rules,
#' and returns a scored dataframe alongside an optional GeoTIFF heatmap for
#' QGIS visualisation.
#'
#' @param data A dataframe or a file path to a CSV file. Must contain at minimum:
#'   - **`lon`** / **`lng`** / **`longitude`** -- longitude in decimal degrees
#'   - **`lat`** / **`latitude`** -- latitude in decimal degrees
#'   - **`date`** -- observation date (any format coercible by `as.Date()`)
#'   Additional environmental columns are matched automatically; see
#'   **Column Naming** below.
#'
#'   **Aragonite auto-calculation:** if the data contains `ph` (or a recognised
#'   synonym) and `alkalinity` columns alongside `temperature` and `salinity`,
#'   `predict_oyster()` will automatically compute `omega_aragonite` using the
#'   in-house [calculate_aragonite()] function before scoring. You can also
#'   pre-compute it and supply `omega_aragonite` directly.
#'
#' @param species Character string identifying the target oyster species.
#'   Accepts the key (`"ostrea_edulis"`), latin name, or common name.
#'   Use [list_species()] to see all available options.
#'
#' @param output_geotiff Logical or character. If `TRUE`, exports a GeoTIFF to
#'   the current working directory as `<species>_suitability.tif`. If a
#'   character string, it is used as the file path. If `FALSE` (default), no
#'   raster file is written.
#'
#' @param resolution Numeric. Raster cell size in degrees for GeoTIFF output
#'   (default `0.001`, approximately 100 m at mid-latitudes). Ignored when
#'   `output_geotiff = FALSE`.
#'
#' @param contours Logical. If `TRUE` (default), contour lines are computed
#'   and embedded in the GeoTIFF export. Ignored when `output_geotiff = FALSE`.
#'
#' @param verbose Logical. If `TRUE`, prints a per-variable scoring summary
#'   after processing (default `FALSE`).
#'
#' @param weighting Character. How the per-variable scores are combined.
#'   `"rank"` (default) is the 1.5.0 behaviour: weights are `1 / rank`,
#'   normalised over the variables present in the data. `"evidence"` weights
#'   each variable by `prior * reliability * spread` (see
#'   [evidence_weights()]) and attaches the coverage of the species' evidence
#'   base to the result. The default is unchanged, so existing calls return
#'   exactly what they returned in 1.5.0.
#'
#'   Note what renormalisation costs you under `"rank"`: a survey measuring
#'   four of a species' seventeen scored variables still returns a confident
#'   number on \[0, 1\], because the weights are renormalised over whatever
#'   happens to be present. Absence becomes indistinguishable from
#'   irrelevance. `"evidence"` is the mode that says so.
#'
#' @param reliability Named numeric vector, one entry per scored layer,
#'   giving the correlation between two independent estimates of that layer --
#'   from [reliability_from_replicates()] where the survey has repeat
#'   coverage, or [reliability_from_perturbation()] where it does not.
#'   Required when `weighting = "evidence"`. A layer with no measured
#'   reliability receives no weight rather than a default one.
#'
#' @param confidence Optional numeric vector of per-location confidence on
#'   \[0, 1\], one value per row, typically from [confidence_score()].
#'   When `NULL` (the default) and `weighting = "evidence"`, confidence is
#'   taken to be the share of the evidence weight actually observed at that
#'   location -- which falls straight out of the weighting and needs no
#'   further assumption. Either way a `confidence` column is returned.
#'
#' @param confidence_floor Numeric, default `0.5`. Locations below this are
#'   classified `"Insufficient data"` instead of being given a suitability
#'   class. The continuous score is still returned for them. Only used when
#'   `weighting = "evidence"`.
#'
#' @param assigned Optional named list for layers with no counterpart in the
#'   published tolerance profile, each element a list with `rank` and
#'   `rationale`. Passed to [evidence_weights()]. Assigned layers are flagged
#'   in the weight table and do not count toward coverage.
#'
#' @param aliases Optional named character vector mapping score column names
#'   to tolerance-spec variable names, e.g.
#'   `c(hardness = "substrate_hardness")`.
#'
#' @param observed Optional character vector of tolerance-spec variables the
#'   survey observed, used for coverage. Defaults to the published layers
#'   found in the data -- pass it explicitly when a variable was measured but
#'   could not be scored.
#'
#' @param attempted_failed Optional character vector of variables that were
#'   instrumented but returned unusable data. Reported separately and used for
#'   `coverage_if_recovered`, which prices an instrument failure in units of
#'   the species' evidence base.
#'
#' @section Column Naming:
#' Column names are matched case-insensitively. Recognised synonyms:
#'
#' | Variable              | Recognised column names                              |
#' |-----------------------|------------------------------------------------------|
#' | Longitude             | `lon`, `lng`, `longitude`, `x`                       |
#' | Latitude              | `lat`, `latitude`, `y`                               |
#' | Date                  | `date`, `datetime`, `timestamp`                      |
#' | Temperature (degrees C)      | `temperature`, `temp`, `temp_c`                      |
#' | Salinity (PSU)        | `salinity`, `sal`, `salinity_psu`                    |
#' | Dissolved oxygen (mg/L)| `dissolved_oxygen`, `do`, `do_mgl`, `oxygen`        |
#' | Depth (m)             | `depth`, `depth_m`                                   |
#' | Current velocity (m/s)| `current_velocity`, `velocity`, `current`, `u_mean`  |
#' | Shear stress (N/m^2)   | `shear_stress`, `tau`, `bed_shear`, `shear`          |
#' | Chlorophyll-a (ugg/L)  | `chlorophyll_a`, `chla`, `chl_a`, `chlorophyll`      |
#' | Turbidity (NTU)       | `turbidity`, `ntu`, `turb`                           |
#' | Slope (degrees)       | `slope`, `slope_deg`                                 |
#' | Roughness / rugosity  | `roughness`, `rugosity`                              |
#' | Substrate hardness    | `substrate_hardness`, `hardness`, `bottom_hardness`  |
#' | Sediment type         | `sediment_type`, `sediment`, `substrate_type`        |
#' | Benthic communities   | `benthic_communities`, `benthic`, `community`        |
#' | Biotope               | `biotope`, `biotopes`, `habitat`                     |
#' | Fishing intensity     | `fishing_intensity`, `fishing`, `fishing_observed`   |
#' | pH                    | `ph`, `pH`, `seawater_ph`, `sea_ph`, `water_ph`      |
#' | Total alkalinity (umol/kg) | `alkalinity`, `total_alkalinity`, `ta`, `alk`, `alkalinity_umol_kg` |
#' | Aragonite saturation  | `omega_aragonite`, `omega_arag`, `aragonite_saturation` |
#'
#' @return A dataframe (invisibly) with all original columns plus:
#'   - **`season`**: detected season at each location/date.
#'   - **`excluded`**: logical, `TRUE` if the location fails a hard exclusion.
#'   - **`exclusion_reason`**: character, reason(s) for exclusion or `NA`.
#'   - **`suitability`**: numeric \[0, 1\], overall weighted suitability score.
#'   - **`suitability_class`**: factor, one of "High", "Moderate", "Low",
#'     "Very Low", "Excluded".
#'   - **`score_<variable>`**: per-variable component scores.
#'
#' With `weighting = "evidence"` the result carries, in addition:
#'   - **`suitability_rank`** / **`suitability_class_rank`**: what rank
#'     weighting would have returned, kept for comparison.
#'   - **`confidence`**: per-location confidence \[0, 1\].
#'   - **`suitability_class`** may take the further value
#'     `"Insufficient data"` for locations below `confidence_floor`.
#'   - attributes `"evidence_weights"` and `"evidence_coverage"`, holding the
#'     weight derivation and the coverage ledger. These are what belongs in a
#'     methods section; render them with [weight_table_md()].
#'
#' If `output_geotiff` is set, a GeoTIFF is written to disk and its path is
#' printed to the console.
#'
#' @seealso [evidence_weights()], [evidence_coverage()],
#'   [confidence_score()], [weight_table_md()]
#' @export
#' @examples
#' sample_csv <- system.file("extdata", "sample_survey.csv", package = "oystermapR")
#' result <- predict_oyster(sample_csv, "ostrea_edulis", verbose = FALSE)
#' table(result$suitability_class)
#'
#' \dontrun{
#' # Evidence weighting: reliability measured per layer, not assumed.
#' rel <- c(depth = 0.930, slope = 0.712, substrate_hardness = 0.402)
#' ev  <- predict_oyster(sample_csv, "ostrea_edulis",
#'                       weighting   = "evidence",
#'                       reliability = rel)
#' attr(ev, "evidence_coverage")
#' cat(weight_table_md(ev), sep = "\n")
#' }
#'
#' \dontrun{
#' # Full pipeline from raw sensor files
#' adcp_f  <- system.file("extdata", "example_bay_adcp.csv",      package = "oystermapR")
#' bathy_f <- system.file("extdata", "example_bay_soundings.xyz", package = "oystermapR")
#' ctd_f   <- system.file("extdata", "example_bay_ctd.csv",       package = "oystermapR")
#' adcp   <- read_nortek_adcp(adcp_f,    verbose = FALSE)
#' bathy  <- read_soundings_xyz(bathy_f, verbose = FALSE)
#' ctd    <- read_generic_csv(ctd_f,     verbose = FALSE)
#' survey <- merge_sensor_data(adcp = adcp, bathy = bathy, ctd = ctd)
#' predict_oyster(survey, "ostrea_edulis", verbose = FALSE)
#' }
predict_oyster <- function(data,
                           species,
                           output_geotiff = FALSE,
                           resolution     = 0.0002,
                           contours       = TRUE,
                           verbose        = FALSE,
                           weighting        = c("rank", "evidence"),
                           reliability      = NULL,
                           confidence       = NULL,
                           confidence_floor = 0.5,
                           assigned         = NULL,
                           aliases          = NULL,
                           observed         = NULL,
                           attempted_failed = NULL) {

  weighting <- match.arg(weighting)

  # ---- 1. Load data ----------------------------------------------------------
  if (is.character(data) && length(data) == 1 && file.exists(data)) {
    cli::cli_inform("Reading data from {.file {data}}")
    df <- utils::read.csv(data, stringsAsFactors = FALSE)
  } else if (is.data.frame(data)) {
    df <- data
  } else {
    cli::cli_abort(c(
      "{.arg data} must be a dataframe or a valid path to a CSV file.",
      "x" = "Got: {.cls {class(data)}}"
    ))
  }

  cli::cli_inform("Processing {nrow(df)} location{?s} for {.val {species}}...")

  # ---- 2. Get species tolerances (with Bayesian updates if available) --------
  tolerances <- get_species_tolerances(species)
  tolerances <- .apply_bayesian_update(tolerances, species)
  cli::cli_inform("Species: {tolerances$latin_name} ({tolerances$common_name})")

  # ---- 3. Standardise coordinate column names --------------------------------
  df <- .standardise_coords(df)

  # ---- 3b. Drop rows with missing coordinates ---------------------------------
  na_coords <- is.na(df$lat) | is.na(df$lon)
  if (any(na_coords)) {
    cli::cli_warn(c(
      "!" = "{sum(na_coords)} row{?s} dropped: missing lat/lon after spatial merge.",
      "i" = "This is normal if sensor datasets covered slightly different areas."
    ))
    df <- df[!na_coords, ]
  }

  # ---- 4. Detect season ------------------------------------------------------
  date_col <- .find_col_any(df, c("date", "datetime", "timestamp"))
  lat_col  <- "lat"  # standardised by .standardise_coords

  if (!is.null(date_col)) {
    df <- add_season_column(df, date_col = date_col, lat_col = lat_col)
    cli::cli_inform("Season detected from {.val {date_col}} + latitude.")
  } else {
    cli::cli_warn(c(
      "No date column found.",
      "i" = "Seasonal temperature checks (winter min, summer max) will be skipped.",
      "i" = "Add a column named {.val date} to enable seasonal exclusions."
    ))
    df$season <- NA_character_
  }

  # ---- 5. Auto-calculate aragonite saturation if pH + alkalinity present -----
  df <- .auto_calculate_aragonite(df, verbose = verbose)

  # ---- 6. Apply exclusion criteria -------------------------------------------
  df <- check_exclusions(df, tolerances)

  # ---- 7. Score non-excluded locations ---------------------------------------
  df <- score_locations(df, tolerances, verbose = verbose)

  # ---- 7b. Re-weight on measured evidence, if asked --------------------------
  # The curves above are untouched. Only the weights change: 1/rank becomes
  # prior x reliability x spread, and the share of the species' evidence base
  # the survey actually observed is carried on the result instead of being
  # renormalised away.
  if (weighting == "evidence") {
    df <- .apply_evidence_weighting(
      df,
      tolerances       = tolerances,
      reliability      = reliability,
      confidence       = confidence,
      confidence_floor = confidence_floor,
      assigned         = assigned,
      aliases          = aliases,
      observed         = observed,
      attempted_failed = attempted_failed
    )
  }

  # ---- 8. Export GeoTIFF if requested ----------------------------------------
  if (!isFALSE(output_geotiff)) {
    tif_path <- if (isTRUE(output_geotiff)) {
      file.path(getwd(), paste0(gsub(" ", "_", tolower(tolerances$latin_name)),
                                "_suitability.tif"))
    } else {
      as.character(output_geotiff)
    }
    export_geotiff(df, tif_path, resolution = resolution, contours = contours)
  }

  # ---- 8. Summary ------------------------------------------------------------
  cli::cli_h2("oystermapR \u2014 Results Summary")
  cli::cli_inform(c(
    "Species:   {tolerances$latin_name}",
    "Locations: {nrow(df)}",
    "Excluded:  {sum(df$excluded, na.rm = TRUE)} ({round(mean(df$excluded, na.rm=TRUE)*100,1)}%)",
    "Mean suitability (non-excluded): {round(mean(df$suitability[!df$excluded], na.rm=TRUE), 3)}"
  ))

  class_tbl <- table(df$suitability_class)
  for (cls in c("High", "Moderate", "Low", "Very Low", "Excluded",
                "Insufficient data")) {
    n <- class_tbl[cls]
    n <- if (length(n) == 0L || is.na(n)) 0L else as.integer(n)
    if (cls == "Insufficient data" && n == 0L) next
    cli::cli_inform("  {cls}: {n}")
  }

  # ---- 9. Top introduction sites ---------------------------------------------
  cli::cli_h3("Top Introduction Sites")
  top_sites <- .top_introduction_sites(df)

  if (is.null(top_sites) || nrow(top_sites) == 0) {
    cli::cli_inform("  No sites above suitability threshold found.")
  } else {
    cli::cli_inform(c(
      "i" = "Top {nrow(top_sites)} spatially distinct location{?s} for oyster introduction.",
      "i" = "Patch radius = equivalent circular radius of the surrounding suitable area.",
      "i" = "Ensure local regulatory and seabed lease requirements are met before stocking."
    ))
    for (i in seq_len(nrow(top_sites))) {
      s    <- top_sites[i, ]
      lat5 <- round(s$lat, 5)
      lon5 <- round(s$lon, 5)
      suit <- round(s$suitability, 3)
      cls  <- s$suitability_class
      rad  <- s$patch_radius_m

      # Build optional context string from available variables
      ctx_parts <- character(0)
      if (!is.null(s$depth)              && !is.na(s$depth))
        ctx_parts <- c(ctx_parts, sprintf("depth %.1f m", s$depth))
      if (!is.null(s$temperature)        && !is.na(s$temperature))
        ctx_parts <- c(ctx_parts, sprintf("temp %.1f\u00b0C", s$temperature))
      if (!is.null(s$salinity)           && !is.na(s$salinity))
        ctx_parts <- c(ctx_parts, sprintf("sal %.1f PSU", s$salinity))
      if (!is.null(s$current_velocity)   && !is.na(s$current_velocity))
        ctx_parts <- c(ctx_parts, sprintf("current %.2f m/s", s$current_velocity))
      if (!is.null(s$substrate_hardness) && !is.na(s$substrate_hardness))
        ctx_parts <- c(ctx_parts, sprintf("hardness %.2f", s$substrate_hardness))

      ctx_str <- if (length(ctx_parts) > 0) {
        paste0("  [", paste(ctx_parts, collapse = ", "), "]")
      } else {
        ""
      }

      cli::cli_inform(
        "  {i}. ({lat5}, {lon5})  score={suit} ({cls})  patch ~{rad} m radius{ctx_str}"
      )
    }
    cli::cli_inform(c(
      "i" = "To extract these in R: top5 <- oystermapR:::.top_introduction_sites(result)"
    ))
  }

  invisible(df)
}


# ---- Evidence weighting ------------------------------------------------------

#' Apply evidence weighting to an already-scored dataframe
#'
#' @description
#' Internal worker behind `predict_oyster(weighting = "evidence")`. Keeps the
#' rank-weighted result alongside the evidence-weighted one, promotes the
#' evidence result into the canonical `suitability` / `suitability_class`
#' columns so that every downstream consumer (GeoTIFF export, `area_summary()`,
#' reports) sees the mode that was asked for, and re-attaches the weight table
#' and coverage ledger.
#'
#' Excluded locations keep their exclusion: a hard threshold breach is not
#' something a change of weighting can rescue.
#'
#' @param df Dataframe from [score_locations()].
#' @param tolerances Species tolerance list.
#' @param reliability,confidence,confidence_floor,assigned,aliases,observed,attempted_failed
#'   As documented in [predict_oyster()].
#' @return `df` with evidence columns and the two evidence attributes.
#' @keywords internal
.apply_evidence_weighting <- function(df,
                                      tolerances,
                                      reliability      = NULL,
                                      confidence       = NULL,
                                      confidence_floor = 0.5,
                                      assigned         = NULL,
                                      aliases          = NULL,
                                      observed         = NULL,
                                      attempted_failed = NULL) {

  if (is.null(reliability) || length(reliability) == 0) {
    cli::cli_abort(c(
      "{.arg reliability} is required when {.code weighting = \"evidence\"}.",
      "i" = "Measure it with {.fn reliability_from_replicates} where the survey has repeat coverage, or {.fn reliability_from_perturbation} where it does not.",
      "i" = "Nothing enters the score on the strength of an argument alone, so there is no default to fall back on."
    ))
  }
  if (is.null(names(reliability)) || any(!nzchar(names(reliability)))) {
    cli::cli_abort("{.arg reliability} must be a {.emph named} numeric vector, one entry per layer.")
  }

  # Keep what rank weighting said, so the two can be compared directly.
  df$suitability_rank       <- df$suitability
  df$suitability_class_rank <- df$suitability_class

  df <- evidence_suitability(
    df,
    tolerances       = tolerances,
    reliability      = reliability,
    confidence       = confidence,
    confidence_floor = confidence_floor,
    assigned         = assigned,
    aliases          = aliases,
    observed         = observed,
    attempted_failed = attempted_failed
  )

  ew  <- attr(df, "evidence_weights")
  cov <- attr(df, "evidence_coverage")

  excl <- as.logical(df$excluded)
  excl[is.na(excl)] <- FALSE

  # Default confidence: the share of the evidence weight actually present at
  # each location. It needs no assumption beyond the weights themselves --
  # it is already what evidence_suitability() divided by. Hard-excluded
  # locations have no scores to support, so they get no confidence figure and
  # are not counted against the floor; their exclusion stands on its own.
  if (is.null(confidence)) {
    df$confidence <- .evidence_row_support(df, ew$weights)
    df$confidence[excl] <- NA_real_

    cls    <- df$suitability_class_evidence
    below  <- !excl & (!is.finite(df$confidence) |
                         df$confidence < confidence_floor)
    cls[below] <- "Insufficient data"
    df$suitability_class_evidence <- cls

    n_ins <- sum(below, na.rm = TRUE)
    if (n_ins > 0) {
      cli::cli_inform(c(
        "i" = "{n_ins} location{?s} below the confidence floor of {confidence_floor} {?is/are} returned as {.val Insufficient data} rather than given a class.",
        " " = "Confidence here is the share of the evidence weight observed at that location; supply {.arg confidence} to use a measured one instead."
      ))
    }
  }

  # Promote the evidence result into the canonical columns.
  df$suitability       <- df$suitability_evidence
  df$suitability_class <- df$suitability_class_evidence

  df$suitability[excl]       <- 0
  df$suitability_class[excl] <- "Excluded"

  attr(df, "evidence_weights")  <- ew
  attr(df, "evidence_coverage") <- cov
  attr(df, "weighting")         <- "evidence"
  df
}


#' Share of the evidence weight observed at each location
#'
#' @param df Dataframe carrying `score_<layer>` columns.
#' @param weights Named numeric vector of evidence weights.
#' @return Numeric vector on \[0, 1\], one value per row.
#' @keywords internal
.evidence_row_support <- function(df, weights) {
  support <- rep(0, nrow(df))
  total   <- 0
  for (layer in names(weights)) {
    w <- weights[[layer]]
    total <- total + w
    col <- paste0("score_", layer)
    if (!col %in% names(df)) next
    support <- support + w * is.finite(df[[col]])
  }
  if (total <= 0) return(rep(NA_real_, nrow(df)))
  support / total
}


# ---- Internal helpers --------------------------------------------------------

#' Find top N spatially distinct introduction sites with patch radius
#'
#' @description
#' Identifies the highest-scoring, spatially spread-out locations for oyster
#' introduction. Uses a greedy selection that prevents clustering -- once a site
#' is chosen, no other site within `min_spacing_deg` is considered, so the
#' result always represents distinct areas of the survey.
#'
#' Patch radius is the equivalent circular radius of all suitable cells
#' (suitability >= `min_suitability`) within the search neighbourhood. This
#' gives a practical sense of how large the suitable area is around each site.
#'
#' @param df Scored dataframe from predict_oyster().
#' @param n Integer. Number of sites to return (default 5).
#' @param min_suitability Numeric. Minimum score to consider suitable \[0,1\].
#' @param min_spacing_deg Numeric. Minimum separation between selected sites
#'   in decimal degrees (default 0.002 approx. 220 m). Prevents adjacent cells all
#'   being reported as separate "sites".
#' @param patch_search_deg Numeric. Radius in degrees to count nearby suitable
#'   cells when computing patch size (default 0.005 approx. 550 m).
#' @param spatial_res Integer. Survey grid resolution in decimal places (default 4).
#' @return Dataframe of top sites, or NULL if none qualify.
#' @keywords internal
.top_introduction_sites <- function(df,
                                    n                = 5L,
                                    min_suitability  = 0.40,
                                    min_spacing_deg  = 0.002,
                                    patch_search_deg = 0.005,
                                    spatial_res      = 4L) {

  # Guard: ensure 'excluded' exists and is logical
  if (!"excluded" %in% names(df)) df$excluded <- FALSE
  df$excluded <- as.logical(df$excluded)
  df$excluded[is.na(df$excluded)] <- FALSE

  # Pool of candidate sites: non-excluded, scored, above threshold
  # A location the model declined to classify cannot be recommended as a site.
  insufficient <- rep(FALSE, nrow(df))
  if ("suitability_class" %in% names(df)) {
    insufficient <- !is.na(df$suitability_class) &
      df$suitability_class == "Insufficient data"
  }

  pool <- df[!df$excluded & !insufficient & !is.na(df$suitability) &
               df$suitability >= min_suitability, ]

  if (nrow(pool) == 0) return(NULL)

  pool <- pool[order(-pool$suitability), ]

  # ---- Greedy spatially-deduped selection ------------------------------------
  selected  <- vector("list", n)
  remaining <- pool
  n_found   <- 0L

  while (n_found < n && nrow(remaining) > 0) {
    top          <- remaining[1L, , drop = FALSE]
    n_found      <- n_found + 1L
    selected[[n_found]] <- top

    dx   <- remaining$lon - top$lon
    dy   <- remaining$lat - top$lat
    far  <- sqrt(dx^2 + dy^2) > min_spacing_deg
    remaining <- remaining[far, ]
  }

  if (n_found == 0L) return(NULL)
  sites <- do.call(rbind, selected[seq_len(n_found)])

  # ---- Patch radius -----------------------------------------------------------
  # Count suitable cells within patch_search_deg of each site; convert to
  # equivalent circular radius so the farmer knows how large the patch is.
  cell_m       <- (10^(-spatial_res)) * 111000   # ~11 m per cell at 4 d.p.
  cell_area_m2 <- cell_m^2

  sites$patch_radius_m <- vapply(seq_len(nrow(sites)), function(i) {
    dx       <- pool$lon - sites$lon[i]
    dy       <- pool$lat - sites$lat[i]
    n_nearby <- sum(sqrt(dx^2 + dy^2) <= patch_search_deg)
    round(sqrt(n_nearby * cell_area_m2 / pi))
  }, numeric(1L))

  # ---- Return clean table -----------------------------------------------------
  keep <- intersect(c("lat", "lon", "suitability", "suitability_class",
                       "patch_radius_m", "depth", "temperature", "salinity",
                       "current_velocity", "substrate_hardness"),
                    names(sites))
  sites[, keep, drop = FALSE]
}


#' Standardise coordinate column names to lat/lon
#' @keywords internal
.standardise_coords <- function(df) {
  lwr <- tolower(names(df))

  lon_idx <- which(lwr %in% c("lon", "lng", "longitude", "x"))[1]
  lat_idx <- which(lwr %in% c("lat", "latitude", "y"))[1]

  if (is.na(lat_idx) || is.na(lon_idx)) {
    cli::cli_abort(c(
      "Could not find coordinate columns.",
      "i" = "Please include columns named {.val lat} (or latitude/y) and {.val lon} (or lng/longitude/x)."
    ))
  }

  names(df)[lon_idx] <- "lon"
  names(df)[lat_idx] <- "lat"
  df
}

#' Find first matching column (case-insensitive)
#' @keywords internal
.find_col_any <- function(df, candidates) {
  lwr <- tolower(names(df))
  idx <- which(lwr %in% tolower(candidates))
  if (length(idx) == 0) return(NULL)
  names(df)[idx[1]]
}
