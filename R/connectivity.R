# =============================================================================
# Habitat connectivity and isolation analysis
# =============================================================================

#' Analyse spatial connectivity of suitable habitat cells
#'
#' @description
#' Builds a spatial graph of cells above a suitability threshold and identifies
#' connected components — contiguous patches of suitable habitat. Isolated
#' cells or small patches are flagged because they are at higher risk of:
#'
#' - **Recruitment failure** — larvae from isolated populations may be
#'   flushed away before settling back in the patch.
#' - **Genetic bottlenecks** — small isolated populations have lower allelic
#'   diversity and reduced adaptive capacity.
#' - **Local extinction without recolonisation** — if a patch is lost, there
#'   are no connected source populations to recolonise.
#'
#' Two cells are considered connected if they are within `gap_m` metres of
#' each other (default 500 m — approximately the scale of larval dispersal
#' during a tidal cycle at moderate current speeds). Connectivity is computed
#' using a fast union-find (disjoint set) algorithm — no external graph
#' packages required.
#'
#' @param result Dataframe from [predict_oyster()] with `lat`, `lon`,
#'   `suitability`, and `suitability_class` columns.
#' @param min_suitability Numeric. Minimum suitability score for a cell to be
#'   included in the connectivity analysis (default 0.5, i.e. Moderate+).
#' @param gap_m Numeric. Maximum distance in metres between two suitable cells
#'   for them to be considered connected (default 500 m).
#' @param min_patch_cells Integer. Patches smaller than this are flagged as
#'   isolated (default 3 cells).
#' @param verbose Logical. Print connectivity summary (default TRUE).
#'
#' @return Input dataframe with additional columns:
#'   `patch_id` (integer — unique patch identifier; NA for non-suitable cells),
#'   `patch_size` (number of cells in the patch),
#'   `patch_area_km2` (approximate area based on cell density),
#'   `connectivity_class` (`"isolated"`, `"small"`, `"moderate"`, `"large"`),
#'   `is_hub` (logical — TRUE for the highest-scoring cell in each patch,
#'   useful as candidate introduction points).
#'
#' @export
#' @examples
#' \dontrun{
#' result <- predict_oyster(survey, "ostrea_edulis")
#' result <- analyse_connectivity(result, gap_m = 500)
#'
#' # Large well-connected patches are the best restoration targets
#' good_patches <- subset(result,
#'   connectivity_class == "large" & suitability_class == "High")
#'
#' # Count patches
#' table(result$connectivity_class)
#'
#' # Visualise — patch_id maps to distinct colours in QGIS
#' export_geotiff(result, "connectivity.tif")
#' }
analyse_connectivity <- function(result,
                                  min_suitability = 0.5,
                                  gap_m           = 500,
                                  min_patch_cells = 3L,
                                  verbose         = TRUE) {

  required <- c("lat","lon","suitability")
  missing  <- setdiff(required, names(result))
  if (length(missing) > 0)
    cli::cli_abort("result missing columns: {paste(missing, collapse=', ')}.")

  n <- nrow(result)
  result$patch_id         <- NA_integer_
  result$patch_size       <- NA_integer_
  result$patch_area_km2   <- NA_real_
  result$connectivity_class <- NA_character_
  result$is_hub           <- FALSE

  # Suitable cells only
  suitable <- !is.na(result$suitability) & result$suitability >= min_suitability
  idx      <- which(suitable)
  n_suit   <- length(idx)

  if (n_suit == 0) {
    cli::cli_warn("No cells above suitability threshold {min_suitability}. No connectivity computed.")
    return(result)
  }

  # Convert to metres (equirectangular)
  lat_mid   <- mean(result$lat[idx], na.rm = TRUE)
  m_per_lon <- 111320 * cos(lat_mid * pi / 180)
  m_per_lat <- 111320

  x <- result$lon[idx] * m_per_lon
  y <- result$lat[idx] * m_per_lat

  # \u2500\u2500 Union-Find \u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500
  parent <- seq_len(n_suit)
  rank_v <- integer(n_suit)

  find <- function(i) {
    while (parent[i] != i) {
      parent[i] <<- parent[parent[i]]  # path compression
      i <- parent[i]
    }
    i
  }

  union <- function(i, j) {
    ri <- find(i); rj <- find(j)
    if (ri == rj) return(invisible(NULL))
    if (rank_v[ri] < rank_v[rj]) { parent[ri] <<- rj }
    else if (rank_v[ri] > rank_v[rj]) { parent[rj] <<- ri }
    else { parent[rj] <<- ri; rank_v[ri] <<- rank_v[ri] + 1L }
  }

  # Connect cells within gap_m
  # Use a simple O(n^2) approach \u2014 for large surveys warn and suggest gap reduction
  if (n_suit > 2000)
    cli::cli_warn(c(
      "!" = paste0(n_suit, " suitable cells \u2014 connectivity search may be slow."),
      "i" = "Consider increasing min_suitability or reducing gap_m."
    ))

  gap2 <- gap_m^2
  for (i in seq_len(n_suit - 1)) {
    dx2 <- (x[i] - x[(i+1):n_suit])^2
    # Quick x-axis pre-filter to avoid full sqrt for distant cells
    close_x <- dx2 <= gap2
    jj      <- which(close_x) + i
    for (j in jj) {
      dist2 <- dx2[j - i] + (y[i] - y[j])^2
      if (dist2 <= gap2) union(i, j)
    }
  }

  # Resolve all roots
  roots <- vapply(seq_len(n_suit), find, integer(1))

  # Assign sequential patch IDs
  unique_roots <- unique(roots)
  patch_map    <- setNames(seq_along(unique_roots), unique_roots)
  patch_ids    <- patch_map[as.character(roots)]

  # Patch sizes
  patch_sizes  <- table(patch_ids)

  # Approximate cell area: median nearest-neighbour distance squared
  # (assumes roughly regular spacing)
  if (n_suit >= 4) {
    sample_idx  <- sample(n_suit, min(n_suit, 100))
    nn_dists    <- vapply(sample_idx, function(i) {
      d2 <- (x[i] - x[-i])^2 + (y[i] - y[-i])^2
      sqrt(min(d2))
    }, numeric(1))
    cell_side_m <- stats::median(nn_dists, na.rm = TRUE)
  } else {
    cell_side_m <- gap_m / 2
  }
  cell_area_km2 <- (cell_side_m / 1000)^2

  # \u2500\u2500 Assign results back \u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500
  result$patch_id[idx]   <- patch_ids
  patch_size_vec         <- as.integer(patch_sizes[as.character(patch_ids)])
  result$patch_size[idx] <- patch_size_vec
  result$patch_area_km2[idx] <- round(patch_size_vec * cell_area_km2, 4)

  result$connectivity_class[idx] <- dplyr::case_when(
    patch_size_vec == 1                 ~ "isolated",
    patch_size_vec < min_patch_cells    ~ "isolated",
    patch_size_vec < 10                 ~ "small",
    patch_size_vec < 50                 ~ "moderate",
    TRUE                                ~ "large"
  )

  # Hub = highest-suitability cell per patch
  for (pid in unique(patch_ids)) {
    cells <- idx[patch_ids == pid]
    hub   <- cells[which.max(result$suitability[cells])]
    result$is_hub[hub] <- TRUE
  }

  if (verbose) {
    n_patches <- length(unique(patch_ids))
    tbl       <- table(result$connectivity_class[idx])
    cli::cli_h2("Connectivity Analysis")
    cli::cli_inform(c(
      " " = paste0("Suitable cells: ", n_suit,
                   " | Patches found: ", n_patches,
                   " | Gap threshold: ", gap_m, " m"),
      " " = paste0("Isolated: ", tbl["isolated"] %||% 0,
                   " | Small: ",    tbl["small"]    %||% 0,
                   " | Moderate: ", tbl["moderate"] %||% 0,
                   " | Large: ",    tbl["large"]    %||% 0)
    ))
    largest <- max(patch_sizes)
    cli::cli_inform(c(
      "i" = paste0("Largest patch: ", largest, " cells (",
                   round(largest * cell_area_km2, 3), " km\u00b2)")
    ))
    n_iso <- sum(result$connectivity_class == "isolated", na.rm = TRUE)
    if (n_iso > 0)
      cli::cli_inform(c(
        "i" = paste0(n_iso, " isolated cell{?s} \u2014 suitable but unconnected. ",
                     "Restoration value is limited without connectivity to source populations.")
      ))
  }

  result
}
