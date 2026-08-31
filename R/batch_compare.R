# =============================================================================
# Batch Species Comparison for oystermapR
# =============================================================================

#' Compare suitability across multiple oyster species
#'
#' @description
#' Runs [predict_oyster()] for every specified species against the same survey
#' dataset and returns a combined comparison table -- one row per survey
#' location, one suitability column per species. An optional summary prints
#' which species is best-suited to each location.
#'
#' This is the tool to use when you haven't yet decided which species to stock,
#' or when you want to identify areas where multiple species overlap in
#' suitability (robust site selection).
#'
#' @param data A dataframe or CSV path accepted by [predict_oyster()].
#' @param species Character vector of species keys to compare. Defaults to all
#'   species currently in the oystermapR database (see [list_species()]).
#'   Passing `NULL` uses all available species.
#' @param output_dir Character or `NULL`. If a directory path is supplied,
#'   individual GeoTIFF heatmaps are exported for each species into that folder.
#'   Default `NULL` (no rasters written).
#' @param min_data_quality Character. Minimum data quality tier to include:
#'   `"low"`, `"medium"`, or `"high"` (default `"low"` -- include all species).
#'   Set to `"high"` to restrict to well-characterised species only.
#' @param verbose Logical. Print a comparison summary per species (default `TRUE`).
#'
#' @return A dataframe with all original survey columns plus:
#'   - `suit_<species_key>`: suitability score \[0, 1\] per species
#'   - `class_<species_key>`: suitability class per species
#'   - `best_species`: key of the species with the highest suitability at each
#'     location (NA if all excluded)
#'   - `best_suitability`: the highest suitability score across all species
#'   - `n_species_high`: number of species scoring "High" at each location
#'     (useful for identifying robust multi-species sites)
#'
#' @export
#' @examples
#' sample_csv <- system.file("extdata", "sample_survey.csv", package = "oystermapR")
#' comparison <- compare_species(
#'   sample_csv,
#'   species = c("ostrea_edulis", "magallana_gigas"),
#'   verbose = FALSE
#' )
#' # Locations suitable for both species
#' both_high <- subset(comparison, n_species_high >= 2)
#' nrow(both_high)
compare_species <- function(data,
                            species          = NULL,
                            output_dir       = NULL,
                            min_data_quality = "low",
                            verbose          = TRUE) {

  # ---- Resolve species list --------------------------------------------------
  all_keys <- names(.species_tolerances)

  quality_order <- c("low" = 1L, "medium" = 2L, "high" = 3L)
  min_q <- quality_order[tolower(min_data_quality)] %||% 1L

  all_keys <- all_keys[vapply(all_keys, function(k) {
    dq <- .species_tolerances[[k]]$data_quality %||% "low"
    quality_order[dq] %||% 1L >= min_q
  }, logical(1))]

  if (is.null(species)) {
    species <- all_keys
  } else {
    # Resolve each species name to a key
    species <- vapply(species, function(s) {
      tol <- tryCatch(get_species_tolerances(s), error = function(e) NULL)
      if (is.null(tol)) {
        cli::cli_warn("Species {.val {s}} not found; skipped.")
        return(NA_character_)
      }
      tolower(gsub("[ .]", "_", tol$latin_name))
    }, character(1))
    species <- species[!is.na(species)]
  }

  if (length(species) == 0) {
    cli::cli_abort("No valid species to compare. Check {.fn list_species}.")
  }

  if (verbose) {
    cli::cli_h2("Batch Species Comparison")
    cli::cli_inform("Comparing {length(species)} species: {.val {species}}")
  }

  # ---- Load data once --------------------------------------------------------
  if (is.character(data) && length(data) == 1 && file.exists(data)) {
    df_base <- utils::read.csv(data, stringsAsFactors = FALSE)
  } else if (is.data.frame(data)) {
    df_base <- data
  } else {
    cli::cli_abort("{.arg data} must be a dataframe or a valid CSV file path.")
  }

  # ---- Run predict_oyster for each species -----------------------------------
  results_list <- vector("list", length(species))
  names(results_list) <- species

  for (sp in species) {
    if (verbose) cli::cli_inform("  Running: {sp}...")

    tif_path <- if (!is.null(output_dir)) {
      if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
      file.path(output_dir, paste0(sp, "_suitability.tif"))
    } else {
      FALSE
    }

    result_sp <- tryCatch(
      predict_oyster(data           = df_base,
                     species        = sp,
                     output_geotiff = tif_path,
                     contours       = !isFALSE(tif_path),
                     verbose        = FALSE),
      error = function(e) {
        cli::cli_warn("Species {.val {sp}} failed: {conditionMessage(e)}")
        NULL
      }
    )
    results_list[[sp]] <- result_sp
  }

  # ---- Combine into comparison table -----------------------------------------
  # Start with coordinate/date columns from the base data
  tol_first     <- get_species_tolerances(species[1])
  first_result  <- results_list[[species[1]]]

  if (is.null(first_result)) {
    cli::cli_abort("All species failed. Cannot build comparison table.")
  }

  # Key columns to preserve from the first result
  base_cols <- intersect(c("lat", "lon", "date", "season", "depth",
                            "temperature", "salinity", "dissolved_oxygen",
                            "current_velocity", "substrate_hardness",
                            "sediment_type", "slope", "roughness"),
                         names(first_result))
  comparison <- first_result[, base_cols, drop = FALSE]

  # Append per-species suitability and class columns
  for (sp in species) {
    res <- results_list[[sp]]
    if (is.null(res)) {
      comparison[[paste0("suit_",  sp)]] <- NA_real_
      comparison[[paste0("class_", sp)]] <- NA_character_
      next
    }
    # Align rows by lat/lon key (in case rows differ after exclusion drops)
    comparison[[paste0("suit_",  sp)]] <- res$suitability[
      match(paste(comparison$lat, comparison$lon),
            paste(res$lat,         res$lon))]
    comparison[[paste0("class_", sp)]] <- res$suitability_class[
      match(paste(comparison$lat, comparison$lon),
            paste(res$lat,         res$lon))]
  }

  # ---- Derived summary columns -----------------------------------------------
  suit_cols  <- paste0("suit_",  species)
  class_cols <- paste0("class_", species)

  suit_mat <- as.matrix(comparison[, intersect(suit_cols, names(comparison)),
                                   drop = FALSE])

  # Best species at each location
  best_idx <- apply(suit_mat, 1, which.max)
  best_idx[apply(is.na(suit_mat), 1, all)] <- NA_integer_
  comparison$best_species    <- ifelse(is.na(best_idx), NA_character_,
                                       species[best_idx])
  comparison$best_suitability <- apply(suit_mat, 1, max, na.rm = TRUE)
  comparison$best_suitability[apply(is.na(suit_mat), 1, all)] <- NA_real_

  # Number of species scoring "High"
  class_mat <- as.matrix(comparison[, intersect(class_cols, names(comparison)),
                                    drop = FALSE])
  comparison$n_species_high <- rowSums(class_mat == "High", na.rm = TRUE)

  # ---- Species competition penalty -------------------------------------------
  # In NW Europe, established Magallana gigas beds directly compete with
  # Ostrea edulis for settling space and food. Apply a post-hoc downward
  # correction to O. edulis suitability scores at cells where M. gigas scores
  # High or Moderate, reflecting competitive exclusion pressure.
  #
  # Penalty matrix (competitor \u2192 target):
  #   gigas High     \u2192 edulis \u00d70.80  (strong competitive pressure)
  #   gigas Moderate \u2192 edulis \u00d70.92  (moderate pressure)
  #
  # This is implemented only when both species are being compared.

  .competition_pairs <- list(
    list(competitor = "magallana_gigas",
         target     = "ostrea_edulis",
         penalties  = c(High = 0.80, Moderate = 0.92))
  )

  for (pair in .competition_pairs) {
    comp_class_col  <- paste0("class_", pair$competitor)
    tgt_suit_col    <- paste0("suit_",  pair$target)
    adj_col         <- paste0("suit_",  pair$target, "_competition_adj")

    if (comp_class_col %in% names(comparison) &&
        tgt_suit_col   %in% names(comparison)) {

      comp_class <- comparison[[comp_class_col]]
      tgt_suit   <- comparison[[tgt_suit_col]]

      penalty <- rep(1.0, nrow(comparison))
      for (cls in names(pair$penalties)) {
        penalty[!is.na(comp_class) & comp_class == cls] <- pair$penalties[cls]
      }

      comparison[[adj_col]] <- round(tgt_suit * penalty, 4)

      if (verbose) {
        n_adj <- sum(penalty < 1.0, na.rm = TRUE)
        cli::cli_inform(c(
          "i" = "Competition adjustment: {pair$competitor} vs {pair$target}",
          " " = "{n_adj} location{?s} downscored for competitive pressure.",
          " " = "Adjusted scores in column {.val {adj_col}}."
        ))
      }
    }
  }

  # ---- Summary ---------------------------------------------------------------
  if (verbose) {
    cli::cli_h3("Comparison Summary")
    for (sp in species) {
      dq  <- .species_tolerances[[sp]]$data_quality %||% "?"
      lat <- .species_tolerances[[sp]]$latin_name
      sc  <- comparison[[paste0("suit_", sp)]]
      n_high <- sum(comparison[[paste0("class_", sp)]] == "High", na.rm = TRUE)
      cli::cli_inform(
        "  {sp} ({lat}) [{dq}]: mean={round(mean(sc,na.rm=TRUE),3)}, High={n_high} cells"
      )
    }
    n_robust <- sum(comparison$n_species_high >= 2, na.rm = TRUE)
    cli::cli_inform(c(
      " ",
      "i" = "{n_robust} location{?s} score 'High' for 2+ species (robust multi-species sites).",
      "i" = "Use {.code subset(result, n_species_high >= 2)} to extract these."
    ))
    if (!is.null(output_dir)) {
      cli::cli_inform(c(
        "i" = "Per-species heatmaps written to: {.file {output_dir}}"
      ))
    }
  }

  invisible(comparison)
}
