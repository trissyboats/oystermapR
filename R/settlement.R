# =============================================================================
# Spat settlement suitability layer
# =============================================================================

# Settlement tolerance specs differ from adult specs in four key ways:
#  1. Turbidity tolerance is much lower (larvae are killed by silt smothering)
#  2. Current velocity has a minimum requirement (delivers larvae to site)
#  3. Substrate must be hard (larvae need biofilm-covered hard surface to set)
#  4. Temperature window for settlement is narrower (16-22\u00b0C optimal)
#
# References:
#  Bayne (1965): O. edulis larval settlement requirements
#  Helm & Bourne (2004): FAO Hatchery Manual \u2014 settlement substrate criteria
#  Wouters et al. (2012): M. gigas spat settlement temperature thresholds
#  Robert et al. (2017): Turbidity effects on bivalve larval settlement

.settlement_tolerances <- list(

  ostrea_edulis = list(
    latin_name   = "Ostrea edulis",
    common_name  = "European flat oyster (spat settlement)",
    exclusions   = list(
      temperature      = list(min = 14.0, max = 28.0,  unit = "\u00b0C",
                              note = "Below 14\u00b0C larvae do not complete metamorphosis"),
      salinity         = list(min = 25.0, max = 40.0,  unit = "PSU",
                              note = "Larvae more sensitive than adults; <25 PSU = failure"),
      dissolved_oxygen = list(min = 6.0,               unit = "mg/L")
    ),
    scored_factors = list(
      temperature = list(
        rank = 1, type = "optimal_range",
        optimal_min = 16.0, optimal_max = 22.0,
        acceptable_min = 14.0, acceptable_max = 26.0, unit = "\u00b0C",
        note = "Settlement peak 18-20\u00b0C; drops sharply outside 16-22\u00b0C"
      ),
      salinity = list(
        rank = 2, type = "optimal_range",
        optimal_min = 30.0, optimal_max = 36.0,
        acceptable_min = 25.0, acceptable_max = 40.0, unit = "PSU"
      ),
      turbidity = list(
        rank = 3, type = "threshold_decay",
        optimal_max = 5.0, hard_max = 20.0, unit = "NTU",
        note = "Silt smothering is the primary cause of spat mortality; hard limit <20 NTU"
      ),
      current_velocity = list(
        rank = 4, type = "optimal_range",
        optimal_min = 0.05, optimal_max = 0.30,
        acceptable_min = 0.02, acceptable_max = 0.60, unit = "m/s",
        note = "Minimum flow needed for larval delivery; >0.6 m/s prevents attachment"
      ),
      substrate_hardness = list(
        rank = 5, type = "categorical",
        scores = list(hard = 1.0, mixed = 0.6, soft = 0.1, very_soft = 0.0,
                      shell_hash = 1.0, rock = 1.0, gravel = 0.8, sand = 0.15,
                      mud = 0.0, unknown = 0.4),
        note = "Larvae require biofilm-covered hard substrate; soft sediment = near-zero settlement"
      ),
      chlorophyll_a = list(
        rank = 6, type = "optimal_range",
        optimal_min = 2.0, optimal_max = 8.0,
        acceptable_min = 0.5, acceptable_max = 20.0, unit = "ug/L",
        note = "Larvae need phytoplankton for energy during metamorphosis"
      ),
      dissolved_oxygen = list(
        rank = 7, type = "optimal_range",
        optimal_min = 7.0, optimal_max = 12.0,
        acceptable_min = 6.0, acceptable_max = 14.0, unit = "mg/L"
      )
    )
  ),

  magallana_gigas = list(
    latin_name   = "Magallana gigas",
    common_name  = "Pacific oyster (spat settlement)",
    exclusions   = list(
      temperature      = list(min = 18.0, max = 32.0,  unit = "\u00b0C",
                              note = "Pacific oyster requires warmer water for successful metamorphosis"),
      salinity         = list(min = 20.0, max = 40.0,  unit = "PSU"),
      dissolved_oxygen = list(min = 5.5,               unit = "mg/L")
    ),
    scored_factors = list(
      temperature = list(
        rank = 1, type = "optimal_range",
        optimal_min = 20.0, optimal_max = 26.0,
        acceptable_min = 18.0, acceptable_max = 30.0, unit = "\u00b0C",
        note = "Settlement requires sustained >18\u00b0C; UK waters marginal in most years"
      ),
      salinity = list(
        rank = 2, type = "optimal_range",
        optimal_min = 25.0, optimal_max = 35.0,
        acceptable_min = 20.0, acceptable_max = 40.0, unit = "PSU"
      ),
      turbidity = list(
        rank = 3, type = "threshold_decay",
        optimal_max = 8.0, hard_max = 30.0, unit = "NTU",
        note = "More turbidity-tolerant than O. edulis at settlement stage"
      ),
      current_velocity = list(
        rank = 4, type = "optimal_range",
        optimal_min = 0.05, optimal_max = 0.40,
        acceptable_min = 0.01, acceptable_max = 0.80, unit = "m/s"
      ),
      substrate_hardness = list(
        rank = 5, type = "categorical",
        scores = list(hard = 1.0, mixed = 0.7, soft = 0.2, very_soft = 0.0,
                      shell_hash = 1.0, rock = 0.9, gravel = 0.75, sand = 0.2,
                      mud = 0.0, unknown = 0.4),
        note = "Can settle on conspecific shell; somewhat more flexible than O. edulis"
      ),
      chlorophyll_a = list(
        rank = 6, type = "optimal_range",
        optimal_min = 2.0, optimal_max = 10.0,
        acceptable_min = 0.5, acceptable_max = 25.0, unit = "ug/L"
      ),
      dissolved_oxygen = list(
        rank = 7, type = "optimal_range",
        optimal_min = 6.5, optimal_max = 12.0,
        acceptable_min = 5.5, acceptable_max = 14.0, unit = "mg/L"
      )
    )
  )
)


#' Score locations for spat settlement suitability
#'
#' @description
#' Assesses whether survey locations meet the conditions required for bivalve
#' larval settlement and metamorphosis. Settlement scoring uses a separate,
#' tighter tolerance specification than adult habitat scoring — larvae are more
#' sensitive to turbidity, require minimum current flow for delivery, and need
#' hard substrate for attachment.
#'
#' The output can be compared directly against the adult suitability score from
#' [predict_oyster()] to distinguish "good adult habitat" from "likely natural
#' recruitment site."
#'
#' @param survey Dataframe of survey measurements. Same format as [predict_oyster()].
#' @param species Character. One of `"ostrea_edulis"` or `"magallana_gigas"`.
#' @param verbose Logical. Print summary (default TRUE).
#'
#' @return Dataframe with `settlement_suitability`, `settlement_class`, and
#'   per-variable `settle_score_*` columns. Combine with adult result via
#'   `cbind()` or merge on `lat`/`lon`.
#'
#' @export
#' @examples
#' \dontrun{
#' adult_result     <- predict_oyster(survey, "ostrea_edulis")
#' settlement_result <- score_settlement(survey, "ostrea_edulis")
#'
#' # Identify cells suitable for both adult survival AND natural recruitment
#' combined <- merge(adult_result, settlement_result[, c("lat","lon",
#'               "settlement_suitability","settlement_class")],
#'               by = c("lat","lon"))
#' combined$dual_suitable <- combined$suitability >= 0.6 &
#'                           combined$settlement_suitability >= 0.6
#' }
score_settlement <- function(survey, species = "ostrea_edulis", verbose = TRUE) {

  species <- tolower(species)
  # Normalise common aliases
  species <- gsub("crassostrea_gigas", "magallana_gigas", species)

  if (!species %in% names(.settlement_tolerances))
    cli::cli_abort(c(
      "No settlement tolerance spec for species {.val {species}}.",
      "i" = "Available: {paste(names(.settlement_tolerances), collapse=', ')}"
    ))

  tol <- .settlement_tolerances[[species]]

  if (verbose)
    cli::cli_h2("Settlement Scoring: {tol$latin_name}")

  scored <- score_locations(survey, tol, verbose = verbose)

  # Rename suitability columns to settlement-specific names
  names(scored)[names(scored) == "suitability"]       <- "settlement_suitability"
  names(scored)[names(scored) == "suitability_class"] <- "settlement_class"
  names(scored)[names(scored) == "excluded"]          <- "settlement_excluded"
  names(scored)[names(scored) == "data_completeness"] <- "settlement_completeness"

  score_cols <- grep("^score_", names(scored), value = TRUE)
  names(scored)[names(scored) %in% score_cols] <-
    sub("^score_", "settle_score_", score_cols)

  scored
}
