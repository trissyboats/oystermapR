# =============================================================================
# Shellfish water quality / classification compliance module
# =============================================================================
#
# EC Regulation 854/2004 (retained in UK law) classifies shellfish harvesting
# areas as:
#   Class A  \u2014 direct harvest for immediate human consumption permitted
#   Class B  \u2014 must pass through an approved purification centre (depuration)
#   Class C  \u2014 must be relaid in open sea for >= 2 months (or heat-treated)
#   Prohibited \u2014 harvesting not permitted under any circumstances
#
# For native oyster restoration, class has little direct relevance (restoration
# reefs are not harvested) but for the aquaculture use case it is a hard
# regulatory constraint that overrides ecological suitability.
#
# This module:
#  1. Accepts a manually supplied classification column OR polygon/area file
#  2. Optionally fetches live FSA/DAERA classification data (fetch_live=TRUE)
#  3. Adds shellfish_class and shellfish_class_penalty columns to a result df
#  4. score_economic_viability() respects the penalty as a multiplicative gate
#
# References:
#  EC (2004): Regulation 854/2004 \u2014 hygiene of food of animal origin
#  FSA (2024): Classified shellfish harvesting areas \u2014 food.gov.uk
#  DAERA (2024): Northern Ireland shellfish classification areas

#' Add shellfish water quality classification to scored result
#'
#' @description
#' Attaches a regulatory shellfish harvesting classification (A/B/C/Prohibited)
#' to each row of a scored result dataframe. This classification affects the
#' economic viability score for aquaculture applications: Prohibited sites
#' receive viability = 0; Class C sites receive a 0.60 multiplier; Class B a
#' 0.80 multiplier; Class A is unpenalised.
#'
#' Classification can be supplied three ways (in priority order):
#' 1. **`class_col`** \u2014 name of an existing column in `result` already
#'    containing classification values ("A", "B", "C", "Prohibited", or NA).
#' 2. **`classified_areas`** \u2014 a dataframe with `lat`, `lon`, and
#'    `shellfish_class` columns, spatially matched to result rows within
#'    `match_radius_deg` degrees.
#' 3. **`fetch_live = TRUE`** \u2014 queries the FSA England/Wales open data API
#'    (requires internet). Requires `httr` package. Works only for sites
#'    within the FSA/DAERA coverage area (England, Wales, Northern Ireland).
#'
#' Unclassified or unmatched sites receive `shellfish_class = "Unclassified"`
#' and a penalty of 0.70 (precautionary principle).
#'
#' @param result Dataframe from [predict_oyster()] or [score_locations()].
#'   Must contain `lat` and `lon`.
#' @param class_col Character. Name of an existing column in `result` containing
#'   class values. If supplied, `classified_areas` and `fetch_live` are ignored.
#' @param classified_areas Dataframe with `lat`, `lon`, `shellfish_class`
#'   columns. Used when `class_col` is NULL and `fetch_live = FALSE`.
#' @param fetch_live Logical. Query live FSA/DAERA API (default FALSE). Requires
#'   internet access and the `httr` package. If fetch fails, falls back to
#'   "Unclassified".
#' @param match_radius_deg Numeric. Spatial matching tolerance in decimal
#'   degrees when using `classified_areas` (default 0.05 = ~5 km).
#' @param verbose Logical. Print matching summary (default TRUE).
#'
#' @return `result` with two additional columns:
#'   - `shellfish_class`: "A", "B", "C", "Prohibited", or "Unclassified"
#'   - `shellfish_class_penalty`: multiplier applied to economic viability
#'     (1.0 for A, 0.80 for B, 0.60 for C, 0.0 for Prohibited, 0.70 for
#'     Unclassified)
#'
#' @export
#' @examples
#' \dontrun{
#' # Option 1: manual column already in data
#' result <- add_shellfish_classification(result, class_col = "water_class")
#'
#' # Option 2: separate classified areas dataframe
#' areas <- data.frame(lat = c(51.5), lon = c(-4.2), shellfish_class = c("B"))
#' result <- add_shellfish_classification(result, classified_areas = areas)
#'
#' # Option 3: live fetch (internet required)
#' result <- add_shellfish_classification(result, fetch_live = TRUE)
#' }
add_shellfish_classification <- function(result,
                                          class_col        = NULL,
                                          classified_areas = NULL,
                                          fetch_live       = FALSE,
                                          match_radius_deg = 0.05,
                                          verbose          = TRUE) {

  VALID_CLASSES <- c("A","B","C","Prohibited","Unclassified")

  CLASS_PENALTY <- c(
    "A"            = 1.00,
    "B"            = 0.80,
    "C"            = 0.60,
    "Prohibited"   = 0.00,
    "Unclassified" = 0.70
  )

  if (!all(c("lat","lon") %in% names(result)))
    cli::cli_abort("result must contain 'lat' and 'lon' columns.")

  # \u2500\u2500 Source 1: existing column \u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500
  if (!is.null(class_col)) {
    if (!class_col %in% names(result))
      cli::cli_abort("Column '{class_col}' not found in result.")

    result$shellfish_class <- as.character(result[[class_col]])
    result$shellfish_class[is.na(result$shellfish_class)] <- "Unclassified"
    # Normalise case
    result$shellfish_class <- .normalise_shellfish_class(result$shellfish_class)
    result$shellfish_class_penalty <- CLASS_PENALTY[result$shellfish_class]

    if (verbose)
      .shellfish_summary(result$shellfish_class)
    return(result)
  }

  # \u2500\u2500 Source 2: live fetch \u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500
  if (fetch_live && is.null(classified_areas)) {
    if (verbose) cli::cli_inform("Fetching live FSA shellfish classification data...")
    bbox <- c(
      lon_min = min(result$lon, na.rm = TRUE),
      lon_max = max(result$lon, na.rm = TRUE),
      lat_min = min(result$lat, na.rm = TRUE),
      lat_max = max(result$lat, na.rm = TRUE)
    )
    classified_areas <- tryCatch(
      .fetch_fsa_shellfish(bbox, verbose),
      error = function(e) {
        cli::cli_warn("Live shellfish classification fetch failed: {conditionMessage(e)}. Using Unclassified.")
        NULL
      }
    )
    if (!is.null(classified_areas)) {
      # Normalise FSA field names \u2014 FSA returns 'Classification', 'lat', 'lon'
      # or centroid-derived coordinates
      if (!"shellfish_class" %in% names(classified_areas)) {
        # Try common FSA field names
        cname <- intersect(
          c("Classification","CLASS","class","classification","CLASSIFICATION"),
          names(classified_areas)
        )
        if (length(cname) > 0) {
          classified_areas$shellfish_class <- classified_areas[[cname[1]]]
        } else {
          classified_areas <- NULL
        }
      }
    }
  }

  # \u2500\u2500 Source 3: manual classified_areas dataframe \u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500
  if (!is.null(classified_areas)) {
    required <- c("lat","lon","shellfish_class")
    if (!all(required %in% names(classified_areas)))
      cli::cli_abort(paste0(
        "classified_areas must contain columns: ",
        paste(required, collapse=", ")
      ))

    classified_areas$shellfish_class <- .normalise_shellfish_class(
      as.character(classified_areas$shellfish_class)
    )

    # Nearest-neighbour spatial match within radius
    result$shellfish_class         <- "Unclassified"
    result$shellfish_class_penalty <- CLASS_PENALTY["Unclassified"]

    for (i in seq_len(nrow(result))) {
      dlat <- classified_areas$lat - result$lat[i]
      dlon <- classified_areas$lon - result$lon[i]
      dist <- sqrt(dlat^2 + dlon^2)
      j <- which.min(dist)
      if (dist[j] <= match_radius_deg) {
        result$shellfish_class[i]         <- classified_areas$shellfish_class[j]
        result$shellfish_class_penalty[i] <- CLASS_PENALTY[classified_areas$shellfish_class[j]]
      }
    }

    if (verbose)
      .shellfish_summary(result$shellfish_class)
    return(result)
  }

  # \u2500\u2500 No source available \u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500
  cli::cli_warn(c(
    "No shellfish classification data supplied.",
    "i" = "Set class_col, supply classified_areas, or use fetch_live = TRUE.",
    "i" = "All sites will be marked 'Unclassified' (penalty = 0.70)."
  ))
  result$shellfish_class         <- "Unclassified"
  result$shellfish_class_penalty <- CLASS_PENALTY["Unclassified"]
  result
}


# \u2500\u2500 Internal helpers \u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500

.normalise_shellfish_class <- function(x) {
  x <- trimws(toupper(x))
  x[x %in% c("A","CLASS A","CLASS_A","1")] <- "A"
  x[x %in% c("B","CLASS B","CLASS_B","2")] <- "B"
  x[x %in% c("C","CLASS C","CLASS_C","3")] <- "C"
  x[x %in% c("PROHIBITED","PROHIB","CLOSED","X","0","NO HARVEST")] <- "Prohibited"
  x[!x %in% c("A","B","C","Prohibited")] <- "Unclassified"
  x
}

.shellfish_summary <- function(classes) {
  tab <- table(classes)
  cli::cli_h3("Shellfish classification summary")
  for (cl in names(tab))
    cli::cli_inform("  {cl}: {tab[cl]} site{?s}")
  n_prohib <- sum(classes == "Prohibited", na.rm = TRUE)
  if (n_prohib > 0)
    cli::cli_warn("{n_prohib} Prohibited site{?s} will receive economic viability = 0.")
}
