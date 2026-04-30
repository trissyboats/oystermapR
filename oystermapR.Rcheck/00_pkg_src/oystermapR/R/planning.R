# =============================================================================
# Economics and aquaculture planning module
# =============================================================================
# This module is DISABLED by default. It is aimed at commercial aquaculture
# operators assessing site feasibility, and is deliberately kept separate from
# the core restoration-focused workflow to avoid cluttering outputs for
# conservation users.
#
# Enable with: predict_oyster(..., enable_planning = TRUE)
# Or call functions directly after predict_oyster().
#
# References:
#  SAMS / Marine Scotland: Scottish shellfish farm economics benchmarking (2022)
#  Falconer et al. (2013): Site selection criteria for suspended mussel culture
#  FAO (2004): Oyster Aquaculture: site selection and management guidelines
#  MMO (2021): Marine licensing \u2014 shellfish aquaculture guidance

# \u2500\u2500 Gear specification lookup \u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500

.gear_specs <- list(
  longline = list(
    name          = "Longline (suspended)",
    depth_min     = 4,    depth_max  = 25,   # metres, CD
    current_min   = 0.05, current_max = 0.60, # m/s
    substrate     = c("any"),                  # anchors can go anywhere
    wave_max      = 2.5,                       # Hs metres (if available)
    turbidity_max = 30,                        # NTU \u2014 fouling threshold
    notes = "Most common offshore method. Requires vessel access, mooring consent."
  ),
  bottom_culture = list(
    name          = "Bottom culture (bags/cages on seabed)",
    depth_min     = 1,    depth_max  = 12,
    current_min   = 0.02, current_max = 0.40,
    substrate     = c("hard","mixed"),
    wave_max      = 1.5,
    turbidity_max = 20,
    notes = "Best for intertidal and shallow subtidal. Needs hard substrate."
  ),
  intertidal_rack = list(
    name          = "Intertidal rack/trestle",
    depth_min     = -2,   depth_max  = 2,    # relative to MHWS
    current_min   = 0.0,  current_max = 0.80,
    substrate     = c("hard","mixed","sand"),
    wave_max      = 1.0,
    turbidity_max = 40,
    notes = "Low capital cost. Requires foreshore lease. Hand-harvested."
  ),
  raft = list(
    name          = "Raft / pontoon culture",
    depth_min     = 5,    depth_max  = 20,
    current_min   = 0.02, current_max = 0.30,
    substrate     = c("any"),
    wave_max      = 1.0,
    turbidity_max = 25,
    notes = "High yield per unit area but needs sheltered water (Hs < 1 m)."
  ),
  restoration_reef = list(
    name          = "Restoration reef (no harvest)",
    depth_min     = 0,    depth_max  = 20,
    current_min   = 0.02, current_max = 0.80,
    substrate     = c("hard","mixed","shell_hash"),
    wave_max      = 3.0,
    turbidity_max = 50,
    notes = "Conservation objective. No commercial licence required in many areas."
  )
)


#' Assess gear deployment feasibility at survey locations
#'
#' @description
#' Evaluates which aquaculture and restoration gear types are physically viable
#' at each survey location based on depth, current velocity, substrate, and
#' (optionally) wave height. Returns a feasibility score and binary flag for
#' each gear type.
#'
#' This function is part of the **planning module** aimed at commercial
#' operators. For pure restoration work, the `restoration_reef` gear type is
#' always included and is the most relevant output.
#'
#' @param result Dataframe from [predict_oyster()] with environmental columns.
#' @param gear_types Character vector of gear types to assess. Options:
#'   `"longline"`, `"bottom_culture"`, `"intertidal_rack"`, `"raft"`,
#'   `"restoration_reef"`. Default: all five.
#' @param depth_col Character. Column name for depth below CD in metres
#'   (default `"depth"`; positive = subtidal).
#' @param current_col Character. Column name for current velocity in m/s
#'   (default `"current_velocity"`).
#' @param substrate_col Character. Column for substrate class
#'   (default `"substrate_hardness_class"`). Can be the output of
#'   [classify_substrate_from_backscatter()].
#' @param wave_col Character or NULL. Column for significant wave height Hs
#'   in metres (default NULL \u2014 wave check skipped if absent).
#' @param verbose Logical. Print feasibility summary (default TRUE).
#'
#' @return Input dataframe with added columns per gear type:
#'   `gear_<type>_feasible` (logical),
#'   `gear_<type>_score` [0-1],
#'   plus `best_gear` (most feasible option) and `n_gear_options` (count).
#'
#' @export
#' @examples
#' \dontrun{
#' result <- predict_oyster(survey, "ostrea_edulis")
#' result <- assess_gear_feasibility(result)
#'
#' # Sites where longline is feasible AND suitability is High
#' longline_sites <- subset(result,
#'   gear_longline_feasible & suitability_class == "High")
#'
#' # Restoration reef sites (no farming licence needed)
#' reef_sites <- subset(result, gear_restoration_reef_feasible)
#' }
assess_gear_feasibility <- function(result,
                                     gear_types   = names(.gear_specs),
                                     depth_col    = "depth",
                                     current_col  = "current_velocity",
                                     substrate_col = "substrate_hardness_class",
                                     wave_col     = NULL,
                                     verbose      = TRUE) {

  gear_types <- intersect(gear_types, names(.gear_specs))
  if (length(gear_types) == 0)
    cli::cli_abort("No valid gear types specified. Choose from: {paste(names(.gear_specs), collapse=', ')}.")

  has_depth     <- depth_col     %in% names(result)
  has_current   <- current_col   %in% names(result)
  has_substrate <- substrate_col %in% names(result)
  has_wave      <- !is.null(wave_col) && wave_col %in% names(result)

  n <- nrow(result)

  for (gt in gear_types) {
    spec  <- .gear_specs[[gt]]
    score <- rep(1.0, n)
    feasible <- rep(TRUE, n)

    # Depth check
    if (has_depth) {
      d <- result[[depth_col]]
      d_adj <- -d  # depth column: positive = below surface; convert to depth below CD
      depth_ok <- !is.na(d_adj) & d_adj >= spec$depth_min & d_adj <= spec$depth_max
      depth_score <- ifelse(
        is.na(d_adj), 0.5,
        ifelse(depth_ok, 1.0,
               pmax(0, 1 - pmin(
                 abs(d_adj - spec$depth_min) / max(1, spec$depth_min),
                 abs(d_adj - spec$depth_max) / max(1, spec$depth_max)
               ) * 2))
      )
      score    <- score * depth_score
      feasible <- feasible & (is.na(d_adj) | depth_ok)
    }

    # Current check
    if (has_current) {
      cv <- result[[current_col]]
      curr_ok <- !is.na(cv) & cv >= spec$current_min & cv <= spec$current_max
      curr_score <- ifelse(
        is.na(cv), 0.5,
        ifelse(curr_ok, 1.0,
               pmax(0, 1 - pmin(
                 pmax(0, spec$current_min - cv) / max(0.01, spec$current_min),
                 pmax(0, cv - spec$current_max) / max(0.01, spec$current_max)
               ) * 2))
      )
      score    <- score * curr_score
      feasible <- feasible & (is.na(cv) | curr_ok)
    }

    # Substrate check (only if gear requires specific substrate)
    if (has_substrate && !"any" %in% spec$substrate) {
      sub <- tolower(as.character(result[[substrate_col]]))
      sub_ok <- sub %in% spec$substrate | is.na(sub)
      score    <- score * ifelse(sub_ok, 1.0, 0.3)
      feasible <- feasible & sub_ok
    }

    # Wave height check
    if (has_wave) {
      hs <- result[[wave_col]]
      wave_ok <- !is.na(hs) & hs <= spec$wave_max
      wave_score <- ifelse(is.na(hs), 0.8,
                           pmax(0, 1 - pmax(0, hs - spec$wave_max) / spec$wave_max))
      score    <- score * wave_score
      feasible <- feasible & (is.na(hs) | wave_ok)
    }

    result[[paste0("gear_", gt, "_feasible")]] <- feasible
    result[[paste0("gear_", gt, "_score")]]    <- round(pmin(1, pmax(0, score)), 3)
  }

  # Best gear and count
  gear_score_cols  <- paste0("gear_", gear_types, "_score")
  gear_feat_cols   <- paste0("gear_", gear_types, "_feasible")

  result$n_gear_options <- rowSums(
    as.data.frame(result[, gear_feat_cols, drop = FALSE]), na.rm = TRUE)

  best_gear_idx <- apply(
    as.matrix(result[, gear_score_cols, drop = FALSE]), 1, which.max)
  result$best_gear <- gear_types[best_gear_idx]

  if (verbose) {
    cli::cli_h2("Gear Feasibility Assessment")
    for (gt in gear_types) {
      n_feas <- sum(result[[paste0("gear_", gt, "_feasible")]], na.rm = TRUE)
      cli::cli_inform("  {.gear_specs[[gt]]$name}: {n_feas} / {n} locations feasible")
    }
    cli::cli_inform(c(
      "i" = paste0("Mean gear options per location: ",
                   round(mean(result$n_gear_options, na.rm = TRUE), 1))
    ))
  }

  result
}


#' Score economic viability for aquaculture or restoration
#'
#' @description
#' Combines ecological suitability, gear feasibility, site accessibility, and
#' patch size into a composite **economic viability index** [0-1]. This is a
#' heuristic scoring \u2014 not a financial model \u2014 but it ranks sites consistently
#' and can be used to shortlist candidates for detailed business case
#' development.
#'
#' The index weighs four components:
#' 1. **Ecological suitability** (40%) \u2014 from [predict_oyster()]
#' 2. **Gear feasibility** (25%) \u2014 best gear score from [assess_gear_feasibility()]
#' 3. **Accessible patch area** (20%) \u2014 log-scaled area of connected suitable
#'    habitat (from [analyse_connectivity()] if available)
#' 4. **Access score** (15%) \u2014 distance to nearest harbour/port (if
#'    `harbours` table supplied) or flat 0.5 if unknown
#'
#' @param result Dataframe from [assess_gear_feasibility()] (which itself
#'   requires [predict_oyster()] output). Optionally also run
#'   [analyse_connectivity()] first to enable area scoring.
#' @param harbours Dataframe or NULL. Known harbour/landing locations with
#'   `lat` and `lon` columns. Used to score site accessibility.
#' @param target Character. `"restoration"` (default) weights ecological
#'   suitability and connectivity higher. `"aquaculture"` weights gear
#'   feasibility and economic access higher.
#' @param verbose Logical. Print viability summary (default TRUE).
#'
#' @return Input dataframe with `viability_score` [0-1],
#'   `viability_class` (Poor/Fair/Good/Excellent), and `viability_notes`.
#'
#' @export
#' @examples
#' \dontrun{
#' result <- predict_oyster(survey, "ostrea_edulis")
#' result <- assess_gear_feasibility(result)
#' result <- analyse_connectivity(result)
#'
#' harbours <- data.frame(
#'   name = c("Tarbert","Portavadie"),
#'   lat  = c(55.865, 55.875),
#'   lon  = c(-5.425, -5.300)
#' )
#'
#' result <- score_economic_viability(result, harbours = harbours,
#'                                     target = "aquaculture")
#'
#' # Best aquaculture candidates
#' subset(result, viability_class %in% c("Good","Excellent"))
#' }
score_economic_viability <- function(result,
                                      harbours = NULL,
                                      target   = c("restoration","aquaculture"),
                                      verbose  = TRUE) {

  target <- match.arg(target)

  weights <- if (target == "restoration") {
    list(ecology = 0.45, gear = 0.15, area = 0.25, access = 0.15)
  } else {
    list(ecology = 0.30, gear = 0.30, area = 0.20, access = 0.20)
  }

  n <- nrow(result)

  # \u2500\u2500 Component 1: Ecological suitability \u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500
  eco_score <- if ("suitability" %in% names(result))
    result$suitability else rep(0.5, n)

  # \u2500\u2500 Component 2: Best gear score \u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500
  gear_col  <- "best_gear"
  gear_score_cols <- grep("^gear_.*_score$", names(result), value = TRUE)
  if (length(gear_score_cols) > 0 && gear_col %in% names(result)) {
    best_scores <- vapply(seq_len(n), function(i) {
      bg  <- result$best_gear[i]
      col <- paste0("gear_", bg, "_score")
      if (col %in% names(result)) result[[col]][i] else 0.5
    }, numeric(1))
  } else {
    best_scores <- rep(0.5, n)
  }

  # \u2500\u2500 Component 3: Patch area (log-scaled) \u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500
  area_score <- rep(0.5, n)
  if ("patch_area_km2" %in% names(result)) {
    pa <- result$patch_area_km2
    pa[is.na(pa)] <- 0
    # Log scale: 0.01 km2 -> 0, 1 km2 -> 0.5, 10 km2 -> 1.0
    area_score <- pmin(1, pmax(0,
      log10(pa + 0.001) / log10(10) + 0.5
    ))
  }

  # \u2500\u2500 Component 4: Harbour access \u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500
  access_score <- rep(0.5, n)  # unknown = neutral
  if (!is.null(harbours) && nrow(harbours) > 0 &&
      all(c("lat","lon") %in% names(harbours))) {
    lat_mid   <- mean(result$lat, na.rm = TRUE)
    m_per_lon <- 111320 * cos(lat_mid * pi / 180)
    m_per_lat <- 111320

    access_score <- vapply(seq_len(n), function(i) {
      dx   <- (result$lon[i] - harbours$lon) * m_per_lon
      dy   <- (result$lat[i] - harbours$lat) * m_per_lat
      dkm  <- min(sqrt(dx^2 + dy^2)) / 1000
      # 0 km -> 1.0; 5 km -> 0.75; 20 km -> 0.3; 50 km -> 0
      pmax(0, 1 - (dkm / 50)^0.7)
    }, numeric(1))
  }

  # \u2500\u2500 Composite index \u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500
  viability <- (weights$ecology * eco_score  +
                weights$gear    * best_scores +
                weights$area    * area_score  +
                weights$access  * access_score)

  viability <- round(pmin(1, pmax(0, viability)), 4)

  viability_class <- dplyr::case_when(
    viability >= 0.75 ~ "Excellent",
    viability >= 0.55 ~ "Good",
    viability >= 0.35 ~ "Fair",
    TRUE              ~ "Poor"
  )

  viability_notes <- dplyr::case_when(
    viability_class == "Excellent" ~ paste0(
      "High ecological suitability, feasible gear options, and good site access. ",
      "Strong candidate for ", target, " development."),
    viability_class == "Good" ~ paste0(
      "Good overall viability for ", target, ". ",
      "Minor constraints exist \u2014 review gear and access details."),
    viability_class == "Fair" ~ paste0(
      "Moderate viability. Constraints in one or more components. ",
      "Detailed feasibility study recommended before investment."),
    TRUE ~ paste0(
      "Low viability for ", target, ". ",
      "Ecological, gear, or access constraints make this site challenging.")
  )

  result$viability_score  <- viability
  result$viability_class  <- viability_class
  result$viability_notes  <- viability_notes
  result$viability_target <- target

  if (verbose) {
    tbl <- table(viability_class)
    cli::cli_h2("Economic Viability \u2014 {target} mode")
    cli::cli_inform(c(
      " " = paste0("Excellent: ", tbl["Excellent"] %||% 0,
                   " | Good: ",  tbl["Good"]      %||% 0,
                   " | Fair: ",  tbl["Fair"]      %||% 0,
                   " | Poor: ",  tbl["Poor"]      %||% 0),
      "i" = paste0("Mean viability: ",
                   round(mean(viability, na.rm = TRUE), 3),
                   " | Component weights: ecology=", weights$ecology,
                   " gear=", weights$gear,
                   " area=", weights$area,
                   " access=", weights$access)
    ))
  }

  result
}
