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
#'   - **`lon`** / **`lng`** / **`longitude`** \u2014 longitude in decimal degrees
#'   - **`lat`** / **`latitude`** \u2014 latitude in decimal degrees
#'   - **`date`** \u2014 observation date (any format coercible by `as.Date()`)
#'   Additional environmental columns are matched automatically; see
#'   **Column Naming** below.
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
#' @param verbose Logical. If `TRUE`, prints a per-variable scoring summary
#'   after processing (default `FALSE`).
#'
#' @section Column Naming:
#' Column names are matched case-insensitively. Recognised synonyms:
#'
#' | Variable              | Recognised column names                              |
#' |-----------------------|------------------------------------------------------|
#' | Longitude             | `lon`, `lng`, `longitude`, `x`                       |
#' | Latitude              | `lat`, `latitude`, `y`                               |
#' | Date                  | `date`, `datetime`, `timestamp`                      |
#' | Temperature (\u00b0C)      | `temperature`, `temp`, `temp_c`                      |
#' | Salinity (PSU)        | `salinity`, `sal`, `salinity_psu`                    |
#' | Dissolved oxygen (mg/L)| `dissolved_oxygen`, `do`, `do_mgl`, `oxygen`        |
#' | Depth (m)             | `depth`, `depth_m`                                   |
#' | Current velocity (m/s)| `current_velocity`, `velocity`, `current`, `u_mean`  |
#' | Shear stress (N/m\u00b2)   | `shear_stress`, `tau`, `bed_shear`, `shear`          |
#' | Chlorophyll-a (\u00b5g/L)  | `chlorophyll_a`, `chla`, `chl_a`, `chlorophyll`      |
#' | Turbidity (NTU)       | `turbidity`, `ntu`, `turb`                           |
#' | Slope (degrees)       | `slope`, `slope_deg`                                 |
#' | Roughness / rugosity  | `roughness`, `rugosity`                              |
#' | Substrate hardness    | `substrate_hardness`, `hardness`, `bottom_hardness`  |
#' | Sediment type         | `sediment_type`, `sediment`, `substrate_type`        |
#' | Benthic communities   | `benthic_communities`, `benthic`, `community`        |
#' | Biotope               | `biotope`, `biotopes`, `habitat`                     |
#' | Fishing intensity     | `fishing_intensity`, `fishing`, `fishing_observed`   |
#'
#' @return A dataframe (invisibly) with all original columns plus:
#'   - **`season`**: detected season at each location/date.
#'   - **`excluded`**: logical, `TRUE` if the location fails a hard exclusion.
#'   - **`exclusion_reason`**: character, reason(s) for exclusion or `NA`.
#'   - **`suitability`**: numeric [0, 1], overall weighted suitability score.
#'   - **`suitability_class`**: factor, one of "High", "Moderate", "Low",
#'     "Very Low", "Excluded".
#'   - **`score_<variable>`**: per-variable component scores.
#'
#' If `output_geotiff` is set, a GeoTIFF is written to disk and its path is
#' printed to the console.
#'
#' @export
#' @examples
#' \dontrun{
#' # Using a CSV file
#' result <- predict_oyster(
#'   data    = "my_survey.csv",
#'   species = "ostrea_edulis",
#'   output_geotiff = "oyster_suitability.tif"
#' )
#'
#' # Using a dataframe
#' df <- read.csv("survey_data.csv")
#' result <- predict_oyster(df, species = "ostrea_edulis", verbose = TRUE)
#'
#' # View top locations
#' subset(result, suitability_class == "High")
#' }
predict_oyster <- function(data,
                           species,
                           output_geotiff = FALSE,
                           resolution     = 0.0002,
                           contours       = TRUE,
                           verbose        = FALSE) {

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

  # ---- 5. Apply exclusion criteria -------------------------------------------
  df <- check_exclusions(df, tolerances)

  # ---- 6. Score non-excluded locations ---------------------------------------
  df <- score_locations(df, tolerances, verbose = verbose)

  # ---- 7. Export GeoTIFF if requested ----------------------------------------
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
  for (cls in c("High", "Moderate", "Low", "Very Low", "Excluded")) {
    n <- class_tbl[cls] %||% 0
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


# ---- Internal helpers --------------------------------------------------------

#' Find top N spatially distinct introduction sites with patch radius
#'
#' @description
#' Identifies the highest-scoring, spatially spread-out locations for oyster
#' introduction. Uses a greedy selection that prevents clustering \u2014 once a site
#' is chosen, no other site within `min_spacing_deg` is considered, so the
#' result always represents distinct areas of the survey.
#'
#' Patch radius is the equivalent circular radius of all suitable cells
#' (suitability >= `min_suitability`) within the search neighbourhood. This
#' gives a practical sense of how large the suitable area is around each site.
#'
#' @param df Scored dataframe from predict_oyster().
#' @param n Integer. Number of sites to return (default 5).
#' @param min_suitability Numeric. Minimum score to consider suitable [0,1].
#' @param min_spacing_deg Numeric. Minimum separation between selected sites
#'   in decimal degrees (default 0.002 \u2248 220 m). Prevents adjacent cells all
#'   being reported as separate "sites".
#' @param patch_search_deg Numeric. Radius in degrees to count nearby suitable
#'   cells when computing patch size (default 0.005 \u2248 550 m).
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
  pool <- df[!df$excluded & !is.na(df$suitability) &
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
