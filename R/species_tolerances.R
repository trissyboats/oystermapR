#' Species Tolerance Data for oystermapR
#'
#' @description
#' Internal lookup tables defining exclusion thresholds, weighted scoring
#' parameters, and seasonal scoring overrides for each supported oyster species.
#'
#' **Structure per species entry:**
#' - `exclusions`         — hard-stop limits; location is excluded if breached
#' - `scored`             — weighted variables contributing to the 0-1 score
#' - `seasonal_overrides` — season-specific parameter swaps for scored variables
#'
#' **Adding a new species:** append a named list entry following the same
#' structure. Use `"seasonal_overrides" = list()` if no seasonal data exists.
#'
#' @format A named list, one entry per species keyed by lowercase Latin name
#'   with underscores (e.g. `"ostrea_edulis"`).
#' @keywords internal
.species_tolerances <- list(

  # ===========================================================================
  # Ostrea edulis \u2014 European Flat Oyster
  #
  # Primary source: Pogoda B. et al. (2023) 'Assessment of European flat oyster
  #   Ostrea edulis (Linnaeus, 1758) habitat suitability in the North Sea and
  #   adjacent waters.' Aquatic Conservation 33(9):940-960.
  #   DOI: 10.1002/aqc.3928
  #
  # Seasonal temperature bounds: Helm M.M. et al. (2004) 'Hatchery Culture of
  #   Bivalves.' FAO Fisheries Technical Paper No. 471. Rome: FAO.
  #
  # Current velocity ranking: synthesised from C. gigas AHP literature and
  #   general bivalve trophodynamics; see package documentation.
  # ===========================================================================
  "ostrea_edulis" = list(

    common_name  = "European Flat Oyster",
    latin_name   = "Ostrea edulis",
    region       = "NE Atlantic, North Sea, Mediterranean",
    data_quality = "high",   # high / medium / low \u2014 confidence in tolerance values

    # -------------------------------------------------------------------------
    # EXCLUSION FACTORS
    # -------------------------------------------------------------------------
    exclusions = list(

      temperature = list(
        min = 7,     max = 25,
        unit = "celsius",
        note = "Feeding begins 7-9\u00b0C; upper sustained tolerance 25\u00b0C."
      ),
      temperature_winter = list(
        min = 1.5,
        unit = "celsius",
        note = "Short-term cold tolerance; prolonged below 1.5\u00b0C fatal."
      ),
      temperature_summer = list(
        max = 30,
        unit = "celsius",
        note = "Mortality rises sharply above 30\u00b0C."
      ),
      salinity = list(
        min_cold   = 20,   max = 40,
        min_warm   = 30,   temp_pivot = 20,
        unit = "psu",
        note = "Min 20 PSU at <=20\u00b0C; optimal >30 PSU at >20\u00b0C."
      ),
      dissolved_oxygen = list(
        min = 3.5,   optimal_min = 6.0,
        unit = "mg/L",
        note = "Hypoxia threshold 3.5 mg/L (short-term); growth impaired below 6 mg/L."
      )
    ),

    # -------------------------------------------------------------------------
    # SCORED FACTORS  (rank 1 = highest weight)
    # -------------------------------------------------------------------------
    scored = list(

      temperature = list(
        rank = 1,
        type = "seasonal",   # uses seasonal_overrides below; year-round fallback:
        optimal_min = 10,  optimal_max = 20,
        poor_min    = 5,   acceptable_max = 23,  absolute_max = 26,
        unit = "celsius",
        note = "Season-aware scoring; see seasonal_overrides$temperature."
      ),

      fishing_intensity = list(
        rank = 1,
        type = "binary_penalty",
        trawl_depth_max = 30,
        penalty = 0.60,
        unit = "logical",
        note = "60% penalty if commercial fishing observed at trawlable depth."
      ),

      shear_stress = list(
        rank = 2,
        type = "threshold_decay",
        optimal_max = 0.3,  hard_max = 0.6,
        unit = "N/m^2",
        note = "tau = rho * Cd * U^2."
      ),

      chlorophyll_a = list(
        rank = 3,
        type = "optimal_range",
        temp_threshold = 7,
        optimal_min = 2.0,  optimal_max = 3.0,
        acceptable_min = 0.5, acceptable_max = 10.0,
        unit = "ug/L",
        note = "Only scored when temperature > 7\u00b0C (feeding active)."
      ),

      current_velocity = list(
        rank = 3,
        type = "optimal_range",
        min_for_food = 0.05,  optimal_min = 0.10,  optimal_max = 0.30,
        tidal_max    = 0.45,  hard_max    = 0.80,
        unit = "m/s",
        note = "Food delivery mechanism; ranks alongside chlorophyll-a."
      ),

      sediment_type = list(
        rank = 3,
        type = "categorical",
        scores = c(
          "hard_rock" = 1.00, "bedrock" = 1.00, "shell_gravel" = 1.00,
          "coarse_gravel" = 0.95, "gravel" = 0.90, "coarse_sand" = 0.85,
          "medium_sand" = 0.75, "mixed_sediment" = 0.60,
          "sandy_mud" = 0.35, "muddy_sand" = 0.35,
          "fine_sand" = 0.20, "mud" = 0.10, "silt" = 0.05,
          "unknown" = 0.50
        ),
        unit = "category"
      ),

      substrate_hardness = list(
        rank = 3,
        type = "optimal_range",
        optimal_min = 0.50, optimal_max = 1.00,
        poor_min = 0.00,    poor_max    = 0.25,
        unit = "hardness_index (0-1)"
      ),

      benthic_communities = list(
        rank = 3,
        type = "categorical",
        scores = c(
          "quiniadella_spisula" = 1.00, "chlamys_opercularis" = 0.90,
          "mixed_bivalve" = 0.75, "polychaete_dominated" = 0.50,
          "sparse" = 0.40, "macroalgae" = 0.35, "seagrass" = 0.30,
          "none_recorded" = 0.50, "unknown" = 0.50
        ),
        unit = "category"
      ),

      depth = list(
        rank = 4,
        type = "optimal_range",
        optimal_min = 0,   optimal_max = 30,
        acceptable_max = 50, absolute_max = 80,
        unit = "metres"
      ),

      biotope = list(
        rank = 4,
        type = "categorical",
        scores = c(
          "CGS" = 1.00, "coarse_sand" = 0.85, "mixed_coarse" = 0.80,
          "rocky_reef" = 0.70, "sandy_gravel" = 0.70,
          "mixed_sediment" = 0.50, "fine_sand_biotope" = 0.20,
          "muddy_biotope" = 0.10, "unknown" = 0.50
        ),
        unit = "category"
      ),

      turbidity = list(
        rank = 5,
        type = "threshold_decay",
        optimal_max = 5, acceptable_max = 15, poor_max = 30,
        unit = "NTU"
      ),

      roughness = list(
        rank = 5,
        type = "optimal_range",
        optimal_min = 1.05, optimal_max = 1.50,
        acceptable_max = 2.50, poor_max = 5.00,
        unit = "rugosity_index"
      ),

      slope = list(
        rank = 6,
        type = "threshold_decay",
        optimal_max = 5, acceptable_max = 15, poor_max = 30,
        unit = "degrees"
      )
    ),

    # -------------------------------------------------------------------------
    # SEASONAL OVERRIDES
    # Provide season-specific parameter sets for any scored variable.
    # When a row's `season` column matches a key here, those parameters
    # replace the base scored parameters for that variable only.
    # Keys: "winter" | "spring" | "summer" | "autumn"
    #
    # Source: Helm et al. (2004) FAO Tech. Paper 471; Pogoda et al. (2023)
    # -------------------------------------------------------------------------
    seasonal_overrides = list(
      temperature = list(
        winter = list(
          type = "optimal_range",
          optimal_min = 4,   optimal_max = 12,
          poor_min    = 1.5, acceptable_max = 15, absolute_max = 18,
          note = "Winter: near-dormant; scores highest in cool but not cold conditions."
        ),
        spring = list(
          type = "optimal_range",
          optimal_min = 10,  optimal_max = 16,
          poor_min    = 6,   acceptable_max = 20, absolute_max = 24,
          note = "Spring: gonad development begins; warming waters preferred."
        ),
        summer = list(
          type = "optimal_range",
          optimal_min = 15,  optimal_max = 22,
          poor_min    = 10,  acceptable_max = 25, absolute_max = 28,
          note = "Summer: spawning and peak growth; 15-22\u00b0C optimal for NE Atlantic."
        ),
        autumn = list(
          type = "optimal_range",
          optimal_min = 10,  optimal_max = 18,
          poor_min    = 6,   acceptable_max = 22, absolute_max = 25,
          note = "Autumn: post-spawn recovery; cooling tolerance improves."
        )
      )
    )
  ),  # end ostrea_edulis


  # ===========================================================================
  # Magallana gigas (= Crassostrea gigas) \u2014 Pacific Oyster
  #
  # Primary sources:
  #   FAO (2004) 'Crassostrea gigas.' Cultured Aquatic Species Fact Sheets.
  #   Rome: FAO Fisheries and Aquaculture Department.
  #   URL: https://www.fao.org/fishery/docs/CDrom/aquaculture/I1129m/file/en/en_pacificcuppedoyster.htm
  #
  #   Bayne B.L. et al. (2017) 'The Physiology of Oysters of the Genus
  #   Crassostrea and Ostrea.' Annual Review of Marine Science 9:503-531.
  #   DOI: 10.1146/annurev-marine-122414-034127
  #
  #   Rico-Villa B. et al. (2009) 'Influence of phytoplankton diet mixtures
  #   on microalgae consumption, larval development, and settlement of the
  #   Pacific oyster Crassostrea gigas.' Aquaculture 289:164-173.
  #
  # Data quality: HIGH \u2014 well-studied commercial species.
  #
  # NOTE: C. gigas is a non-native invasive species in parts of Europe.
  # Before using this package output for C. gigas stocking, check current
  # ICES and national regulations regarding invasive species status in your
  # jurisdiction. It is listed on the IUCN invasive species database for
  # several European regions.
  # ===========================================================================
  "magallana_gigas" = list(

    common_name  = "Pacific Oyster",
    latin_name   = "Magallana gigas",
    region       = "Global; widely farmed in NW Europe",
    data_quality = "high",

    exclusions = list(
      temperature = list(
        min = 5,   max = 33,
        unit = "celsius",
        note = "Growth ceases below 5\u00b0C; mortality risk above 33\u00b0C sustained."
      ),
      temperature_summer = list(
        max = 35,
        unit = "celsius",
        note = "Short-term survival to 35\u00b0C; spawning threshold ~18-20\u00b0C."
      ),
      salinity = list(
        min_cold  = 12,  max = 43,
        min_warm  = 15,  temp_pivot = 15,
        unit = "psu",
        note = "Tolerates 12-43 PSU; growth optimal 20-30 PSU."
      ),
      dissolved_oxygen = list(
        min = 2.0,  optimal_min = 5.0,
        unit = "mg/L",
        note = "Survival minimum 2 mg/L; growth requires >5 mg/L."
      )
    ),

    scored = list(

      temperature = list(
        rank = 1,
        type = "seasonal",
        optimal_min = 15, optimal_max = 25,
        poor_min = 5,     acceptable_max = 30, absolute_max = 33,
        unit = "celsius",
        note = "Season-aware; see seasonal_overrides$temperature."
      ),

      fishing_intensity = list(
        rank = 1,
        type = "binary_penalty",
        trawl_depth_max = 30,
        penalty = 0.60,
        unit = "logical"
      ),

      shear_stress = list(
        rank = 2,
        type = "threshold_decay",
        optimal_max = 0.35, hard_max = 0.70,
        unit = "N/m^2",
        note = "Slightly higher tolerance than O. edulis (larger, heavier shell)."
      ),

      chlorophyll_a = list(
        rank = 3,
        type = "optimal_range",
        temp_threshold = 5,
        optimal_min = 1.5,  optimal_max = 4.0,
        acceptable_min = 0.5, acceptable_max = 12.0,
        unit = "ug/L"
      ),

      current_velocity = list(
        rank = 3,
        type = "optimal_range",
        min_for_food = 0.05, optimal_min = 0.10, optimal_max = 0.35,
        tidal_max    = 0.50, hard_max    = 0.90,
        unit = "m/s",
        note = "Broader flow tolerance than O. edulis due to reef-forming habit."
      ),

      sediment_type = list(
        rank = 3,
        type = "categorical",
        scores = c(
          "hard_rock" = 1.00, "bedrock" = 0.95, "shell_gravel" = 1.00,
          "coarse_gravel" = 0.95, "gravel" = 0.90, "coarse_sand" = 0.85,
          "medium_sand" = 0.80, "mixed_sediment" = 0.70,
          "sandy_mud" = 0.50, "muddy_sand" = 0.45,
          "fine_sand" = 0.30, "mud" = 0.15, "silt" = 0.10,
          "unknown" = 0.50
        ),
        unit = "category",
        note = "More tolerant of softer substrates than O. edulis (intertidal-capable)."
      ),

      substrate_hardness = list(
        rank = 3,
        type = "optimal_range",
        optimal_min = 0.40, optimal_max = 1.00,
        poor_min = 0.00,    poor_max    = 0.20,
        unit = "hardness_index (0-1)",
        note = "Slightly lower minimum than O. edulis."
      ),

      benthic_communities = list(
        rank = 3,
        type = "categorical",
        scores = c(
          "mixed_bivalve" = 0.90, "chlamys_opercularis" = 0.80,
          "polychaete_dominated" = 0.60, "sparse" = 0.50,
          "macroalgae" = 0.40, "seagrass" = 0.35,
          "none_recorded" = 0.50, "unknown" = 0.50
        ),
        unit = "category"
      ),

      depth = list(
        rank = 4,
        type = "optimal_range",
        optimal_min = 0,   optimal_max = 15,
        acceptable_max = 30, absolute_max = 50,
        unit = "metres",
        note = "Primarily intertidal to 15 m; deeper populations less productive."
      ),

      turbidity = list(
        rank = 5,
        type = "threshold_decay",
        optimal_max = 8,  acceptable_max = 20, poor_max = 50,
        unit = "NTU",
        note = "Higher turbidity tolerance than O. edulis. Optimal <8 NTU."
      ),

      roughness = list(
        rank = 5,
        type = "optimal_range",
        optimal_min = 1.02, optimal_max = 2.00,
        acceptable_max = 3.50, poor_max = 6.00,
        unit = "rugosity_index"
      ),

      slope = list(
        rank = 6,
        type = "threshold_decay",
        optimal_max = 8,  acceptable_max = 20, poor_max = 35,
        unit = "degrees",
        note = "More tolerant of slope than O. edulis due to cemented habit."
      )
    ),

    seasonal_overrides = list(
      temperature = list(
        winter = list(
          type = "optimal_range",
          optimal_min = 5,   optimal_max = 14,
          poor_min    = 2,   acceptable_max = 18, absolute_max = 20,
          note = "Winter: near-dormant below 5\u00b0C; broader cold tolerance than O. edulis."
        ),
        spring = list(
          type = "optimal_range",
          optimal_min = 12,  optimal_max = 20,
          poor_min    = 6,   acceptable_max = 24, absolute_max = 28,
          note = "Spring: feeding resumes; gametogenesis begins ~10\u00b0C."
        ),
        summer = list(
          type = "optimal_range",
          optimal_min = 18,  optimal_max = 26,
          poor_min    = 12,  acceptable_max = 30, absolute_max = 33,
          note = "Summer: spawning at 18-20\u00b0C; peak growth 20-26\u00b0C."
        ),
        autumn = list(
          type = "optimal_range",
          optimal_min = 12,  optimal_max = 22,
          poor_min    = 7,   acceptable_max = 26, absolute_max = 30,
          note = "Autumn: post-spawn recovery; wide temperature acceptance."
        )
      )
    )
  ),  # end magallana_gigas


  # ===========================================================================
  # Crassostrea angulata \u2014 Portuguese Oyster
  #
  # Primary sources:
  #   Huvet A. et al. (2018) 'Comparative sensitivity of diploid and triploid
  #   Pacific oyster Crassostrea gigas and Portuguese oyster Crassostrea angulata
  #   larvae to temperature and salinity.' Aquatic Living Resources 31:23.
  #   DOI: 10.1051/alr/2018013
  #
  #   Flores-Vergara C. et al. (2004) 'Larval and postlarval morphology,
  #   settlement, and growth of the Portuguese oyster Crassostrea angulata
  #   (Lamarck, 1819) reared in the laboratory.' J Shellfish Res 23(3):841-848.
  #
  #   Ojea J. et al. (2011) 'Seasonal variation in biochemical composition
  #   of the gonad of Crassostrea angulata in relation to gametogenesis.'
  #   J Shellfish Res 30(3):713-722. DOI: 10.2983/035.030.0307
  #
  #   Royer J. et al. (2004) 'Comparative biological performances of a
  #   tetraploid line of the Portuguese oyster Crassostrea angulata.'
  #   Aquaculture 237:285-296.
  #
  # Data quality: MEDIUM \u2014 estuarine populations in the Sado/Tagus (Portugal)
  #   and Gironde/Arcachon (France) best characterised. Temperature, salinity
  #   and depth are well-constrained by field studies. Current velocity and
  #   roughness remain estimated by analogy with C. gigas.
  #
  # DISEASE NOTE: Marteilia refringens (haplosporidian paramyxean) caused
  #   significant C. angulata mortality in Iberian and French populations from
  #   the 1970s onward, contributing to range collapse (Poder & Dynamic 1986;
  #   Carrasco et al. 2012). OsHV-1 was confirmed in French C. angulata
  #   populations during 2008-2010 mass mortality events (Segarra et al. 2010).
  #   The score_disease_risk() function currently models Bonamia/OsHV-1 only \u2014
  #   users deploying C. angulata should supplement with Marteilia risk assessment.
  #
  # CONSERVATION NOTE: C. angulata wild populations are critically reduced
  #   due to disease and hybridisation with C. gigas. Check current OSPAR and
  #   national regulations regarding non-native status before planning introduction.
  # ===========================================================================
  "crassostrea_angulata" = list(

    common_name  = "Portuguese Oyster",
    latin_name   = "Crassostrea angulata",
    region       = "Iberian Peninsula, SW France; historically to English Channel",
    data_quality = "medium",

    exclusions = list(
      temperature = list(
        min = 8,   max = 30,
        unit = "celsius",
        note = "Larval survival tested 16-28\u00b0C (Huvet 2018); adult tolerance broader. Cold min from Royer 2004."
      ),
      temperature_summer = list(
        max = 33,
        unit = "celsius",
        note = "Short-term upper tolerance; Tagus/Sado adults survive summer peaks."
      ),
      salinity = list(
        min_cold = 15,  max = 40,
        min_warm = 20,  temp_pivot = 18,
        unit = "psu",
        note = "Larval development tested 15-38 PSU, optimal 22-31 PSU (Flores-Vergara 2004). Max 40 PSU from Iberian lagoon records."
      ),
      dissolved_oxygen = list(
        min = 2.5,  optimal_min = 5.0,
        unit = "mg/L",
        note = "Estimated from C. gigas analogy; direct C. angulata hypoxia thresholds not published."
      )
    ),

    scored = list(

      temperature = list(
        rank = 1,
        type = "seasonal",
        optimal_min = 18, optimal_max = 26,
        poor_min = 8,     acceptable_max = 29, absolute_max = 32,
        unit = "celsius",
        note = "Warmer optimum than C. gigas; Iberian origin. Gametogenesis initiation ~12\u00b0C (Ojea 2011), peak spawning 20-22\u00b0C."
      ),

      fishing_intensity = list(
        rank = 1,  type = "binary_penalty",
        trawl_depth_max = 30,  penalty = 0.60,
        unit = "logical"
      ),

      shear_stress = list(
        rank = 2,  type = "threshold_decay",
        optimal_max = 0.35,  hard_max = 0.65,
        unit = "N/m^2",
        note = "Estuarine origin; similar tolerance to C. gigas."
      ),

      current_velocity = list(
        rank = 3,  type = "optimal_range",
        min_for_food = 0.05,  optimal_min = 0.10,  optimal_max = 0.35,
        tidal_max    = 0.50,  hard_max    = 0.85,
        unit = "m/s",
        note = "Estimated by analogy with C. gigas. Tagus estuary populations experience 0.10-0.40 m/s mean tidal currents; no direct flume data published."
      ),

      sediment_type = list(
        rank = 3,  type = "categorical",
        scores = c(
          "hard_rock" = 0.95, "shell_gravel" = 1.00, "coarse_gravel" = 0.95,
          "gravel" = 0.90, "coarse_sand" = 0.85, "medium_sand" = 0.80,
          "mixed_sediment" = 0.70, "sandy_mud" = 0.60, "muddy_sand" = 0.55,
          "fine_sand" = 0.35, "mud" = 0.25, "silt" = 0.10,
          "unknown" = 0.50
        ),
        unit = "category",
        note = "Estuarine-adapted; tolerates softer substrates than O. edulis. Mud score elevated relative to Atlantic species (Sado/Tagus estuarine origin)."
      ),

      substrate_hardness = list(
        rank = 3,  type = "optimal_range",
        optimal_min = 0.35, optimal_max = 1.00,
        poor_min = 0.00,    poor_max    = 0.15,
        unit = "hardness_index (0-1)"
      ),

      depth = list(
        rank = 4,  type = "optimal_range",
        optimal_min = 0,   optimal_max = 10,
        acceptable_max = 25, absolute_max = 40,
        unit = "metres",
        note = "Primarily intertidal to 10 m; Tagus/Arcachon farm records confirm 0-8 m peak productivity."
      ),

      biotope = list(
        rank = 4,
        type = "categorical",
        scores = c(
          "estuarine_shell"  = 1.00, "CGS" = 0.90, "coarse_sand" = 0.85,
          "mixed_coarse"     = 0.80, "rocky_reef" = 0.75,
          "sandy_gravel"     = 0.75, "intertidal_mixed" = 0.85,
          "mixed_sediment"   = 0.60, "fine_sand_biotope" = 0.30,
          "muddy_biotope"    = 0.25, "unknown" = 0.50
        ),
        unit = "category",
        note = "Estuarine shell/gravel elevated; intertidal mixed also high given Tagus estuary natural range."
      ),

      turbidity = list(
        rank = 5,  type = "threshold_decay",
        optimal_max = 10, acceptable_max = 25, poor_max = 60,
        unit = "NTU",
        note = "Estuarine origin; higher turbidity tolerance than O. edulis. Tagus estuary median turbidity ~8-15 NTU."
      ),

      roughness = list(
        rank = 5,  type = "optimal_range",
        optimal_min = 1.02, optimal_max = 2.00,
        acceptable_max = 3.50, poor_max = 6.00,
        unit = "rugosity_index",
        note = "Estimated by analogy with C. gigas."
      ),

      chlorophyll_a = list(
        rank = 3,
        type = "optimal_range",
        temp_threshold = 8,
        optimal_min = 1.5,  optimal_max = 4.5,
        acceptable_min = 0.5, acceptable_max = 14.0,
        unit = "ug/L",
        note = "Estimated from C. gigas; closely related with similar filtration physiology. Acceptable_max raised slightly for estuarine conditions with episodic phytoplankton blooms."
      ),

      benthic_communities = list(
        rank = 3,
        type = "categorical",
        scores = c(
          "mixed_bivalve"     = 0.90,
          "estuarine_bivalve" = 0.95,   # native Iberian bivalve communities
          "chlamys_opercularis" = 0.70,
          "polychaete_dominated" = 0.65,
          "sabellaria"        = 0.60,   # common estuarine associate
          "sparse"            = 0.55,
          "macroalgae"        = 0.45,
          "seagrass"          = 0.35,
          "none_recorded"     = 0.50,
          "unknown"           = 0.50
        ),
        unit = "category",
        note = "Estuarine origin; higher tolerance of polychaete-dominated communities than O. edulis. Sabellaria worm reef added as documented estuarine associate."
      ),

      slope = list(
        rank = 6,  type = "threshold_decay",
        optimal_max = 6,  acceptable_max = 18,  poor_max = 35,
        unit = "degrees"
      )
    ),

    seasonal_overrides = list(
      temperature = list(
        winter = list(
          type = "optimal_range",
          optimal_min = 8,   optimal_max = 16,
          poor_min    = 4,   acceptable_max = 20, absolute_max = 24,
          note = "Winter: limited cold tolerance vs C. gigas; Iberian coastal winters 10-15\u00b0C typical. Ojea (2011) shows gonad regression continues at 10\u00b0C."
        ),
        spring = list(
          type = "optimal_range",
          optimal_min = 14,  optimal_max = 22,
          poor_min    = 8,   acceptable_max = 26, absolute_max = 30,
          note = "Spring: rapid warming response; gametogenesis onset ~12\u00b0C (Ojea 2011)."
        ),
        summer = list(
          type = "optimal_range",
          optimal_min = 20,  optimal_max = 28,
          poor_min    = 14,  acceptable_max = 31, absolute_max = 33,
          note = "Summer: warm-water optimum; peak spawning 20-22\u00b0C (Ojea 2011). Tagus surface temperatures regularly 22-28\u00b0C."
        ),
        autumn = list(
          type = "optimal_range",
          optimal_min = 14,  optimal_max = 24,
          poor_min    = 9,   acceptable_max = 28, absolute_max = 31,
          note = "Autumn: post-spawn recovery; Iberian autumn mild \u2014 extended feeding season vs Atlantic species."
        )
      )
    )
  ),  # end crassostrea_angulata


  # ===========================================================================
  # Ostrea stentina \u2014 Denticulate Flat Oyster
  #
  # Primary sources:
  #   Sendra M. et al. (2022) 'First assessment of Ostrea stentina (Payraudeau,
  #   1826) spatfall recruitment in Mar Menor coastal lagoon, SE Spain.'
  #   Regional Studies in Marine Science 53:102434.
  #   DOI: 10.1016/j.rsma.2022.102434
  #
  #   Gonzalez-Wanguemert M. et al. (2019) 'Genetic connectivity and
  #   differentiation of Ostrea stentina populations across the Mediterranean
  #   and Eastern Atlantic.' J Sea Research 145:40-50.
  #
  #   Ramos-Espla A.A. et al. (2022) 'Decline of Cymodocea nodosa meadows
  #   and associated epibiont species in the Mar Menor lagoon.'
  #   Marine Pollution Bulletin 174:113282.
  #
  #   Reece K.S. et al. (2008) phylogeographic/taxonomic work distinguishing
  #   O. stentina from O. edulis. J Shellfish Res 27(4).
  #
  # Data quality: LOW-MEDIUM \u2014 direct settlement and recruitment data from
  #   Mar Menor (Sendra 2022). Temperature, salinity, and depth parameters
  #   now backed by field records. Current velocity, dissolved oxygen, and
  #   chlorophyll-a remain estimated from ecological context. DO NOT use
  #   these values for regulatory decisions without independent validation.
  #
  # ECOLOGY NOTE: O. stentina has two distinct habitat associations:
  #   (1) Lagoon/lagoon-margin populations in hypersaline conditions
  #       (Mar Menor, \u00c9tang de Thau, Mersin Lagoon)
  #   (2) Open-coast shallow rocky reef populations in Atlantic (Azores, Iberia)
  #   The tolerance parameters below represent the lagoonal type, which has
  #   broader salinity and temperature tolerance than open-coast populations.
  #
  # CONSERVATION NOTE: O. stentina is a candidate for Mediterranean restoration
  #   as the native flat oyster surrogate in environments too warm for O. edulis.
  #   It is protected in several Spanish Marine Protected Areas. IUCN assessment
  #   not completed; check national regulations before any translocation.
  # ===========================================================================
  "ostrea_stentina" = list(

    common_name  = "Denticulate Flat Oyster",
    latin_name   = "Ostrea stentina",
    region       = "Mediterranean, NE Atlantic (Azores to Bay of Biscay)",
    data_quality = "low",  # upgrading fields individually below

    exclusions = list(
      temperature = list(
        min = 8,   max = 32,
        unit = "celsius",
        note = "Cold min estimated from O. edulis (closely related). Upper limit from Mar Menor field records: populations survive summer peaks of 30-32\u00b0C (Sendra 2022)."
      ),
      temperature_summer = list(
        max = 35,
        unit = "celsius",
        note = "Mar Menor lagoon surface temperatures reach 32-34\u00b0C in exceptional years; populations persist, though reproduction is suppressed above ~32\u00b0C."
      ),
      salinity = list(
        min_cold = 18,  max = 47,
        min_warm = 22,  temp_pivot = 20,
        unit = "psu",
        note = "Upper limit from historical Mar Menor records (max ~47 PSU pre-2021 channel opening; Ramos-Espla 2022). Lower bound from open-coast Atlantic populations."
      ),
      dissolved_oxygen = list(
        min = 2.5,  optimal_min = 5.0,
        unit = "mg/L",
        note = "Lagoonal populations routinely experience seasonal hypoxia in Mar Menor (DO < 4 mg/L documented in bottom waters Aug-Sep). Lower threshold estimated but likely more tolerant than O. edulis."
      )
    ),

    scored = list(

      temperature = list(
        rank = 1,
        type = "seasonal",
        optimal_min = 14, optimal_max = 26,
        poor_min = 8,     acceptable_max = 30, absolute_max = 33,
        unit = "celsius",
        note = "Based on Sendra (2022) Mar Menor recruitment data: peak spatfall associated with 18-26\u00b0C. Upper end expanded to 30\u00b0C for acceptable range given confirmed persistence at these temperatures."
      ),

      fishing_intensity = list(
        rank = 1,  type = "binary_penalty",
        trawl_depth_max = 20,  penalty = 0.60,
        unit = "logical",
        note = "Shallow coastal/lagoonal species; 20 m trawl depth threshold appropriate."
      ),

      shear_stress = list(
        rank = 2,  type = "threshold_decay",
        optimal_max = 0.25,  hard_max = 0.50,
        unit = "N/m^2",
        note = "Small body size (typically 20-50 mm); lower shear tolerance than O. edulis. Lagoonal habitat typical of low-energy conditions."
      ),

      current_velocity = list(
        rank = 3,  type = "optimal_range",
        min_for_food = 0.02, optimal_min = 0.05, optimal_max = 0.20,
        tidal_max    = 0.30, hard_max    = 0.50,
        unit = "m/s",
        note = "Lagoonal species. Mar Menor currents typically 0.01-0.15 m/s (wind-driven, no significant tidal component). Hard max lowered vs Atlantic species."
      ),

      sediment_type = list(
        rank = 3,  type = "categorical",
        scores = c(
          "hard_rock"      = 0.90,
          "shell_gravel"   = 1.00,
          "coarse_gravel"  = 0.90,
          "gravel"         = 0.85,
          "coarse_sand"    = 0.80,
          "medium_sand"    = 0.75,
          "mixed_sediment" = 0.65,
          "sandy_mud"      = 0.60,
          "muddy_sand"     = 0.55,
          "seagrass_bed"   = 0.85,  # documented association with Cymodocea nodosa (Sendra 2022)
          "posidonia"      = 0.70,  # Posidonia oceanica matte also a substrate
          "pinna_shell"    = 0.90,  # documented settlement on Pinna nobilis shell
          "fine_sand"      = 0.35,
          "mud"            = 0.20,
          "silt"           = 0.10,
          "unknown"        = 0.50
        ),
        unit = "category",
        note = "Sendra (2022): spatfall documented on Cymodocea nodosa rhizomes, shell gravel, and Pinna nobilis valves. Posidonia oceanica matte also used."
      ),

      substrate_hardness = list(
        rank = 3,  type = "optimal_range",
        optimal_min = 0.30, optimal_max = 0.90,
        poor_min = 0.00,    poor_max    = 0.10,
        unit = "hardness_index (0-1)"
      ),

      depth = list(
        rank = 4,  type = "optimal_range",
        optimal_min = 0,   optimal_max = 12,
        acceptable_max = 25, absolute_max = 40,
        unit = "metres",
        note = "Sendra (2022): Mar Menor recruitment concentrated 1-8 m. Open-coast Atlantic populations recorded to 30 m (Gonzalez-Wanguemert 2019). Absolute max retained conservatively at 40 m."
      ),

      biotope = list(
        rank = 4,
        type = "categorical",
        scores = c(
          "lagoonal_shell"    = 1.00,  # hypersaline lagoon with shell substrate
          "cymodocea_meadow"  = 0.90,  # Cymodocea nodosa seagrass (documented)
          "posidonia_matte"   = 0.75,  # Posidonia oceanica matte
          "CGS"               = 0.85,
          "coarse_sand"       = 0.80,
          "mixed_coarse"      = 0.75,
          "rocky_reef"        = 0.70,
          "mixed_sediment"    = 0.55,
          "fine_sand_biotope" = 0.25,
          "muddy_biotope"     = 0.15,
          "unknown"           = 0.50
        ),
        unit = "category",
        note = "Lagoonal shell and Cymodocea meadow elevated based on documented Mar Menor habitat associations."
      ),

      turbidity = list(
        rank = 5,  type = "threshold_decay",
        optimal_max = 10, acceptable_max = 25, poor_max = 55,
        unit = "NTU",
        note = "Mar Menor episodic turbidity events documented (algal blooms, wind resuspension). Upper acceptable limit raised to 25 NTU vs O. edulis given lagoonal adaptation. Poor max 55 NTU from lagoon bloom records."
      ),

      roughness = list(
        rank = 5,  type = "optimal_range",
        optimal_min = 1.02, optimal_max = 1.80,
        acceptable_max = 3.00, poor_max = 5.00,
        unit = "rugosity_index"
      ),

      chlorophyll_a = list(
        rank = 3,
        type = "optimal_range",
        temp_threshold = 10,
        optimal_min = 0.8,  optimal_max = 3.0,
        acceptable_min = 0.2, acceptable_max = 8.0,
        unit = "ug/L",
        note = "Lagoonal origin; likely adapted to lower ambient chlorophyll than open-coast Atlantic species (Mar Menor historically oligotrophic; ~0.5-2.0 ug/L background). Optimal lower bound reduced vs O. edulis."
      ),

      benthic_communities = list(
        rank = 3,
        type = "categorical",
        scores = c(
          "mixed_bivalve"        = 0.85,
          "chlamys_opercularis"  = 0.65,
          "polychaete_dominated" = 0.55,
          "sparse"               = 0.50,
          "macroalgae"           = 0.40,
          "seagrass"             = 0.85,   # documented settlement habitat
          "cymodocea"            = 0.90,   # Cymodocea nodosa \u2014 primary documented associate
          "posidonia"            = 0.75,   # Posidonia oceanica
          "caulerpa"             = 0.30,   # invasive Caulerpa spp. \u2014 low due to competition
          "none_recorded"        = 0.50,
          "unknown"              = 0.50
        ),
        unit = "category",
        note = "Seagrass communities elevated based on documented Mar Menor habitat use. Caulerpa penalised as invasive species colonising similar habitats in Mediterranean."
      ),

      slope = list(
        rank = 6,  type = "threshold_decay",
        optimal_max = 5,  acceptable_max = 12,  poor_max = 25,
        unit = "degrees",
        note = "Lagoonal origin; small body size requires stable low-slope substrate."
      )
    ),

    seasonal_overrides = list(
      temperature = list(
        winter = list(
          type = "optimal_range",
          optimal_min = 10,  optimal_max = 18,
          poor_min    = 6,   acceptable_max = 22, absolute_max = 26,
          note = "Mediterranean lagoon winters: typically 12-16\u00b0C. Sendra (2022): no recruitment in winter surveys consistent with feeding/reproduction reduction below 12\u00b0C."
        ),
        spring = list(
          type = "optimal_range",
          optimal_min = 14,  optimal_max = 22,
          poor_min    = 9,   acceptable_max = 26, absolute_max = 30,
          note = "Sendra (2022): spring spatfall begins as temperatures exceed 16\u00b0C in Mar Menor (April-May)."
        ),
        summer = list(
          type = "optimal_range",
          optimal_min = 20,  optimal_max = 28,
          poor_min    = 14,  acceptable_max = 32, absolute_max = 35,
          note = "Sendra (2022): peak spatfall June-August in Mar Menor, water temperature 22-30\u00b0C. Populations persist at 30-32\u00b0C surface temperatures."
        ),
        autumn = list(
          type = "optimal_range",
          optimal_min = 14,  optimal_max = 24,
          poor_min    = 9,   acceptable_max = 28, absolute_max = 32,
          note = "Secondary spatfall peak September-October documented by Sendra (2022) as temperatures decline from summer peak."
        )
      )
    )
  ),  # end ostrea_stentina


  # ===========================================================================
  # Ostrea lurida \u2014 Olympia Oyster
  #
  # Primary sources:
  #   Trimble A.C. et al. (2009) 'Factors structuring the distribution of the
  #   Olympia oyster (Ostrea lurida) across a salinity gradient.'
  #   J Shellfish Res 28(1):69-78. DOI: 10.2983/035.028.0114
  #
  #   Kimbro D.L. et al. (2019) 'Salinity and temperature effects on Olympia
  #   oyster growth.' Ecology 100(8):e02759. DOI: 10.1002/ecy.2759
  #
  #   Wasson K. et al. (2015) 'Moving beyond the blame game: embracing complexity
  #   in Olympia oyster restoration.' J Shellfish Res 34(2):535-550.
  #
  #   Henderson J. & Manley A. (2012) 'Multiple stressors on Olympia oysters.'
  #   Marine Ecology Progress Series 458:109-122.
  #
  #   Polson M.P. & Zacherl D.C. (2009) 'Geographic distribution and intertidal
  #   population status for the Olympia oyster, Ostrea lurida, from Alaska to Baja.'
  #   J Shellfish Res 28(1):69-78.
  #
  # Data quality: MEDIUM \u2014 restoration programmes on the US West Coast since 2000
  #   have generated substantial field data. Temperature and salinity thresholds
  #   are well-constrained. Turbidity, roughness, and biotope are estimated from
  #   ecological context and San Francisco Bay monitoring data.
  #
  # GEOGRAPHIC RANGE NOTE: O. lurida is native to the NE Pacific from
  #   Sitka, Alaska (57\u00b0N) to Baja California, Mexico (28\u00b0N). Historically
  #   the dominant intertidal oyster on the US West Coast; severely depleted
  #   by overharvest and habitat loss in the 19th-20th centuries. Major
  #   restoration programmes active in San Francisco Bay, Puget Sound, Humboldt
  #   Bay, Willapa Bay, and Tomales Bay. Not established in Europe.
  #
  # REPRODUCTION NOTE: O. lurida is protandric hermaphrodite (like O. edulis),
  #   brooding larvae internally for 10-14 days. Spawning triggered at 14-16\u00b0C
  #   (Kimbro 2019), substantially cooler than M. gigas (~18-20\u00b0C). This means
  #   it reproduces successfully in cool Pacific coastal systems where C. gigas
  #   may fail to spawn.
  # ===========================================================================
  "ostrea_lurida" = list(

    common_name  = "Olympia Oyster",
    latin_name   = "Ostrea lurida",
    region       = "NE Pacific: Sitka AK to Baja California MX",
    data_quality = "medium",

    exclusions = list(
      temperature = list(
        min = 2,   max = 26,
        unit = "celsius",
        note = "Polson & Zacherl (2009): range extends to 57\u00b0N (Alaska); cold min from northern populations. Upper sustained limit ~26\u00b0C; growth ceases above 24\u00b0C (Kimbro 2019)."
      ),
      temperature_summer = list(
        max = 28,
        unit = "celsius",
        note = "Short-term heat tolerance from intertidal exposure; sustained above 28\u00b0C lethal."
      ),
      salinity = list(
        min_cold = 10,  max = 38,
        min_warm = 15,  temp_pivot = 12,
        unit = "psu",
        note = "Trimble (2009): found 10-35 PSU across distribution; optimal 20-32 PSU (Kimbro 2019). More euryhaline than O. edulis; tolerates estuarine dilution."
      ),
      dissolved_oxygen = list(
        min = 3.5,  optimal_min = 6.0,
        unit = "mg/L",
        note = "Henderson & Manley (2012): sensitive to hypoxia; 3.5 mg/L threshold from San Francisco Bay monitoring."
      )
    ),

    scored = list(

      temperature = list(
        rank = 1,
        type = "seasonal",
        optimal_min = 12, optimal_max = 20,
        poor_min = 4,     acceptable_max = 24, absolute_max = 26,
        unit = "celsius",
        note = "Kimbro (2019): growth rate maximum 12-20\u00b0C. Spawning triggered at 14-16\u00b0C. Significantly cooler optimum than any Crassostrea/Magallana species."
      ),

      fishing_intensity = list(
        rank = 1,  type = "binary_penalty",
        trawl_depth_max = 15,  penalty = 0.60,
        unit = "logical",
        note = "Intertidal to shallow subtidal species; trawl depth threshold set at 15 m."
      ),

      shear_stress = list(
        rank = 2,  type = "threshold_decay",
        optimal_max = 0.30,  hard_max = 0.55,
        unit = "N/m^2",
        note = "Small body size (typically 30-80 mm); lower shear tolerance than M. gigas. Intertidal populations on wave-sheltered shores."
      ),

      chlorophyll_a = list(
        rank = 3,
        type = "optimal_range",
        temp_threshold = 6,
        optimal_min = 1.5,  optimal_max = 4.0,
        acceptable_min = 0.5, acceptable_max = 10.0,
        unit = "ug/L",
        note = "Estimated from Pacific coastal productivity data (MBARI monitoring). Temperature threshold 6\u00b0C: feeding active in cool Pacific winters."
      ),

      current_velocity = list(
        rank = 3,  type = "optimal_range",
        min_for_food = 0.05, optimal_min = 0.10, optimal_max = 0.30,
        tidal_max    = 0.45, hard_max    = 0.75,
        unit = "m/s",
        note = "Trimble (2009): highest densities in sites with moderate tidal exchange (0.10-0.35 m/s). San Francisco Bay restoration: poor performance in very low flow areas."
      ),

      sediment_type = list(
        rank = 3,  type = "categorical",
        scores = c(
          "hard_rock"      = 1.00,
          "bedrock"        = 1.00,
          "shell_gravel"   = 1.00,  # native shell hash critical for recruitment
          "coarse_gravel"  = 0.90,
          "gravel"         = 0.85,
          "coarse_sand"    = 0.75,
          "medium_sand"    = 0.60,
          "mixed_sediment" = 0.50,
          "sandy_mud"      = 0.30,
          "muddy_sand"     = 0.25,
          "fine_sand"      = 0.15,
          "mud"            = 0.05,
          "silt"           = 0.05,
          "unknown"        = 0.50
        ),
        unit = "category",
        note = "Wasson (2015): settlement strongly biased to native shell hash (cultch) and hard substrate. Soft sediment restoration has very low success rate."
      ),

      substrate_hardness = list(
        rank = 3,  type = "optimal_range",
        optimal_min = 0.55, optimal_max = 1.00,
        poor_min = 0.00,    poor_max    = 0.20,
        unit = "hardness_index (0-1)",
        note = "Higher minimum than O. edulis \u2014 O. lurida restoration requires hard substrate or shell cultch deployment."
      ),

      benthic_communities = list(
        rank = 3,
        type = "categorical",
        scores = c(
          "mixed_bivalve"        = 0.90,
          "olympia_reef"         = 1.00,  # established O. lurida reef
          "mussel_bed"           = 0.75,  # Mytilus californianus/trossulus coexist
          "chlamys_opercularis"  = 0.60,
          "polychaete_dominated" = 0.45,
          "sparse"               = 0.40,
          "macroalgae"           = 0.35,
          "seagrass"             = 0.40,  # Zostera marina \u2014 indirect association
          "none_recorded"        = 0.50,
          "unknown"              = 0.50
        ),
        unit = "category",
        note = "Established olympia reef scored highest. Mussel beds elevated \u2014 M. californianus provides structural complexity and substrate used by O. lurida."
      ),

      depth = list(
        rank = 4,  type = "optimal_range",
        optimal_min = -1,  optimal_max = 8,
        acceptable_max = 20, absolute_max = 30,
        unit = "metres",
        note = "Polson & Zacherl (2009): intertidal (-1 m MLLW) to 8 m subtidal is primary range. Wasson (2015): deeper sites (>8 m) have lower recruitment but adults viable."
      ),

      biotope = list(
        rank = 4,
        type = "categorical",
        scores = c(
          "intertidal_shell"  = 1.00,  # intertidal shell hash \u2014 optimal restoration substrate
          "CGS"               = 0.85,
          "rocky_reef"        = 0.90,
          "coarse_sand"       = 0.75,
          "mixed_coarse"      = 0.80,
          "sandy_gravel"      = 0.75,
          "mixed_sediment"    = 0.45,
          "fine_sand_biotope" = 0.20,
          "muddy_biotope"     = 0.10,
          "unknown"           = 0.50
        ),
        unit = "category"
      ),

      turbidity = list(
        rank = 5,  type = "threshold_decay",
        optimal_max = 8,  acceptable_max = 20, poor_max = 45,
        unit = "NTU",
        note = "Estimated from San Francisco Bay/Tomales Bay monitoring context. Tolerates moderate estuarine turbidity; optimal turbidity similar to O. edulis."
      ),

      roughness = list(
        rank = 5,  type = "optimal_range",
        optimal_min = 1.05, optimal_max = 1.60,
        acceptable_max = 2.50, poor_max = 5.00,
        unit = "rugosity_index",
        note = "Intertidal rocky habitat; moderate rugosity beneficial for refuge from desiccation and wave stress."
      ),

      slope = list(
        rank = 6,  type = "threshold_decay",
        optimal_max = 8,  acceptable_max = 20, poor_max = 35,
        unit = "degrees",
        note = "Intertidal species; moderate slope tolerance. Steep rocky intertidal is natural habitat."
      )
    ),

    seasonal_overrides = list(
      temperature = list(
        winter = list(
          type = "optimal_range",
          optimal_min = 4,   optimal_max = 12,
          poor_min    = 1,   acceptable_max = 15, absolute_max = 18,
          note = "Pacific coast winters: 6-12\u00b0C typical in central California; 2-8\u00b0C in Puget Sound. O. lurida survives cold winters better than M. gigas at equivalent temperatures."
        ),
        spring = list(
          type = "optimal_range",
          optimal_min = 10,  optimal_max = 18,
          poor_min    = 5,   acceptable_max = 22, absolute_max = 24,
          note = "Kimbro (2019): growth rate increases sharply as temperatures exceed 10\u00b0C. Gametogenesis onset ~12\u00b0C."
        ),
        summer = list(
          type = "optimal_range",
          optimal_min = 14,  optimal_max = 20,
          poor_min    = 10,  acceptable_max = 24, absolute_max = 27,
          note = "Kimbro (2019): spawning triggered at 14-16\u00b0C; peak growth 14-20\u00b0C. Upwelling zones (Oregon, N. California) maintain cool summer temperatures favourable for reproduction."
        ),
        autumn = list(
          type = "optimal_range",
          optimal_min = 10,  optimal_max = 18,
          poor_min    = 5,   acceptable_max = 22, absolute_max = 25,
          note = "Autumn cooling along Pacific coast; secondary settlement peak documented in some years (Wasson 2015)."
        )
      )
    )
  )  # end ostrea_lurida

)  # end .species_tolerances


#' Get species tolerance parameters
#'
#' @param species Character. Species key (e.g. `"ostrea_edulis"`), latin name,
#'   or common name (partial, case-insensitive).
#' @return Named list of tolerance parameters for the species.
#' @export
#' @examples
#' tol <- get_species_tolerances("ostrea_edulis")
#' tol$exclusions$temperature
get_species_tolerances <- function(species) {
  key <- tolower(gsub("[ .]", "_", trimws(species)))

  if (key %in% names(.species_tolerances)) return(.species_tolerances[[key]])

  for (nm in names(.species_tolerances)) {
    entry <- .species_tolerances[[nm]]
    if (grepl(key, tolower(entry$latin_name),   fixed = TRUE) ||
        grepl(key, tolower(entry$common_name),  fixed = TRUE)) {
      return(entry)
    }
  }

  cli::cli_abort(c(
    "Species {.val {species}} not found.",
    "i" = "Available: {.val {list_species()}}",
    "i" = "Use {.fn get_species_tolerances} with the key, latin name, or common name."
  ))
}


#' List all supported species
#'
#' @return Named character vector: names = latin names, values = species keys.
#' @export
#' @examples
#' list_species()
list_species <- function() {
  keys <- names(.species_tolerances)
  nms  <- vapply(.species_tolerances, `[[`, character(1), "latin_name")
  dq   <- vapply(.species_tolerances, `[[`, character(1), "data_quality")
  cli::cli_inform(c(
    "i" = "Supported species ({length(keys)} total):"
  ))
  for (i in seq_along(keys)) {
    flag <- switch(dq[i], high = "\u2713", medium = "~", low = "! (limited data)", "\u2713")
    cli::cli_inform("  {flag} {.val {keys[i]}} \u2014 {nms[i]}")
  }
  invisible(setNames(keys, nms))
}
