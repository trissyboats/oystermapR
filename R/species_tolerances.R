#' Species Tolerance Data for oystermapR
#'
#' @description
#' Internal lookup tables defining exclusion thresholds, weighted scoring
#' parameters, and seasonal scoring overrides for each supported oyster species.
#'
#' **Structure per species entry:**
#' - `exclusions`         -- hard-stop limits; location is excluded if breached
#' - `scored`             -- weighted variables contributing to the 0-1 score
#' - `seasonal_overrides` -- season-specific parameter swaps for scored variables
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
        optimal_min = 2,   optimal_max = 30,
        poor_min    = 0,   acceptable_max = 50, absolute_max = 80,
        unit = "metres",
        note = "Primarily subtidal; intertidal (0-2 m) scores lower than optimal subtidal range. Source: Pogoda et al. (2023)."
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

      ph = list(
        rank = 4,
        type = "optimal_range",
        optimal_min = 7.9,  optimal_max = 8.3,
        poor_min    = 7.6,  acceptable_max = 8.5, absolute_max = 8.6,
        unit = "pH units",
        note = "Shell calcification impaired below pH 7.8; optimal 7.9-8.3 for NE Atlantic coastal water. Hard lower bound 7.6 (chronic sub-lethal stress). Source: Gazeau et al. (2010) ICES J Mar Sci 67; Bamber (1990) Mar Biol 107."
      ),

      omega_aragonite = list(
        rank = 4,
        type = "optimal_range",
        optimal_min = 1.8,  optimal_max = 3.5,
        poor_min    = 1.0,  acceptable_max = 4.5, absolute_max = 5.5,
        unit = "Omega (dimensionless)",
        note = "O. edulis is sensitive to aragonite undersaturation; larvae strongly impaired below Omega 1.5, adults below Omega 1.0. Optimal 1.8-3.5 reflects typical NE Atlantic shelf conditions. Source: Gazeau et al. (2010); Bamber (1990)."
      ),

      slope = list(
        rank = 6,
        type = "threshold_decay",
        optimal_max = 5, acceptable_max = 15, poor_max = 30,
        unit = "degrees"
      ),

      salinity = list(
        rank = 5,
        type = "optimal_range",
        optimal_min = 28.0, optimal_max = 35.0,
        poor_min    = 20.0, acceptable_min = 24.0,
        acceptable_max = 38.0, absolute_max = 40.0,
        unit = "PSU",
        note = "Within-acceptable-range gradient; rewards optimal salinity. Stenohaline flat oyster prefers 28-35 PSU. Source: Pogoda et al. (2023)."
      ),

      dissolved_oxygen = list(
        rank = 5,
        type = "optimal_range",
        optimal_min = 6.0, optimal_max = 10.0,
        poor_min    = 4.0, acceptable_min = 5.0,
        acceptable_max = 14.0, absolute_max = 20.0,
        unit = "mg/L",
        note = "Scored above exclusion threshold; rewards well-oxygenated sites. Optimal 6-10 mg/L. Source: Newell (2004) ICES CM."
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

      ph = list(
        rank = 4,
        type = "optimal_range",
        optimal_min = 7.8,  optimal_max = 8.3,
        poor_min    = 7.5,  acceptable_max = 8.5, absolute_max = 8.6,
        unit = "pH units",
        note = "M. gigas is more tolerant of low pH than O. edulis; larvae impaired below pH 7.6, adults below 7.5. Optimal 7.8-8.3. Source: Gazeau et al. (2010); Kurihara et al. (2007) Mar Ecol Prog Ser 346."
      ),

      omega_aragonite = list(
        rank = 4,
        type = "optimal_range",
        optimal_min = 1.6,  optimal_max = 3.5,
        poor_min    = 0.8,  acceptable_max = 4.5, absolute_max = 5.5,
        unit = "Omega (dimensionless)",
        note = "M. gigas shows moderate resilience to low Omega; adults can survive short-term undersaturation. Larvae more sensitive (Omega < 1.5 impairs shell growth). Source: Kurihara et al. (2007); Gazeau et al. (2010)."
      ),

      slope = list(
        rank = 6,
        type = "threshold_decay",
        optimal_max = 8,  acceptable_max = 20, poor_max = 35,
        unit = "degrees",
        note = "More tolerant of slope than O. edulis due to cemented habit."
      ),

      salinity = list(
        rank = 5,
        type = "optimal_range",
        optimal_min = 20.0, optimal_max = 35.0,
        poor_min    = 12.0, acceptable_min = 15.0,
        acceptable_max = 40.0, absolute_max = 43.0,
        unit = "PSU",
        note = "Within-range salinity gradient. Wide tolerance but optimal 20-35 PSU for growth. Source: FAO (2004)."
      ),

      dissolved_oxygen = list(
        rank = 5,
        type = "optimal_range",
        optimal_min = 5.0, optimal_max = 10.0,
        poor_min    = 3.0, acceptable_min = 4.0,
        acceptable_max = 14.0, absolute_max = 20.0,
        unit = "mg/L",
        note = "Scored above exclusion threshold. M. gigas more tolerant of low DO than O. edulis; optimal 5-10 mg/L. Source: Bayne et al. (2017)."
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

      ph = list(
        rank = 4,
        type = "optimal_range",
        optimal_min = 7.8,  optimal_max = 8.3,
        poor_min    = 7.5,  acceptable_max = 8.5, absolute_max = 8.6,
        unit = "pH units",
        note = "Estimated from C. gigas analogy (closely related). Iberian estuarine populations may encounter lower pH episodically during upwelling events. Source: Gazeau et al. (2010)."
      ),

      omega_aragonite = list(
        rank = 4,
        type = "optimal_range",
        optimal_min = 1.6,  optimal_max = 3.5,
        poor_min    = 0.8,  acceptable_max = 4.5, absolute_max = 5.5,
        unit = "Omega (dimensionless)",
        note = "Estimated from C. gigas analogy; Iberian upwelling events can drive Omega < 1.5 episodically. Adults tolerate short-term undersaturation. Source: estimated."
      ),

      slope = list(
        rank = 6,  type = "threshold_decay",
        optimal_max = 6,  acceptable_max = 18,  poor_max = 35,
        unit = "degrees"
      ),

      salinity = list(
        rank = 5,
        type = "optimal_range",
        optimal_min = 22.0, optimal_max = 35.0,
        poor_min    = 15.0, acceptable_min = 18.0,
        acceptable_max = 38.0, absolute_max = 40.0,
        unit = "PSU",
        note = "Estuarine species; optimal 22-35 PSU. Narrower than M. gigas due to Iberian/Atlantic coast distribution. Source: Flores-Vergara et al. (2004)."
      ),

      dissolved_oxygen = list(
        rank = 5,
        type = "optimal_range",
        optimal_min = 5.0, optimal_max = 10.0,
        poor_min    = 3.0, acceptable_min = 4.0,
        acceptable_max = 14.0, absolute_max = 20.0,
        unit = "mg/L",
        note = "Estuarine conditions common; optimal 5-10 mg/L. Estimated by analogy with M. gigas. Source: Huvet et al. (2018)."
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

      ph = list(
        rank = 4,
        type = "optimal_range",
        optimal_min = 7.85, optimal_max = 8.3,
        poor_min    = 7.6,  acceptable_max = 8.5, absolute_max = 8.6,
        unit = "pH units",
        note = "Mediterranean/lagoonal populations may experience wider pH variation than open Atlantic. Optimal range similar to O. edulis; Mar Menor lagoon pH 7.8-8.4 typical. Source: estimated from O. edulis analogy."
      ),

      omega_aragonite = list(
        rank = 4,
        type = "optimal_range",
        optimal_min = 1.8,  optimal_max = 3.5,
        poor_min    = 1.0,  acceptable_max = 4.5, absolute_max = 5.5,
        unit = "Omega (dimensionless)",
        note = "Mediterranean lagoon waters typically Omega 2.0-4.0; poor_min = 1.0 consistent with flat oyster sensitivity. Source: estimated from O. edulis analogy."
      ),

      slope = list(
        rank = 6,  type = "threshold_decay",
        optimal_max = 5,  acceptable_max = 12,  poor_max = 25,
        unit = "degrees",
        note = "Lagoonal origin; small body size requires stable low-slope substrate."
      ),

      salinity = list(
        rank = 5,
        type = "optimal_range",
        optimal_min = 28.0, optimal_max = 40.0,
        poor_min    = 18.0, acceptable_min = 24.0,
        acceptable_max = 45.0, absolute_max = 47.0,
        unit = "PSU",
        note = "Hypersaline-tolerant lagoonal flat oyster (Mediterranean, Black Sea). Optimal 28-40 PSU. Source: Boglino et al. (2012)."
      ),

      dissolved_oxygen = list(
        rank = 5,
        type = "optimal_range",
        optimal_min = 6.0, optimal_max = 10.0,
        poor_min    = 3.5, acceptable_min = 5.0,
        acceptable_max = 14.0, absolute_max = 20.0,
        unit = "mg/L",
        note = "Lagoonal flat oyster; oxygen conditions variable in lagoon habitats. Estimated by analogy with O. edulis."
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

      ph = list(
        rank = 4,
        type = "optimal_range",
        optimal_min = 7.8,  optimal_max = 8.3,
        poor_min    = 7.6,  acceptable_max = 8.5, absolute_max = 8.6,
        unit = "pH units",
        note = "Pacific coast upwelling exposes O. lurida to episodic low pH; more tolerant than Atlantic species. Restoration sites in San Francisco Bay monitored pH 7.7-8.3. Source: Hettinger et al. (2012) Glob Change Biol."
      ),

      omega_aragonite = list(
        rank = 4,
        type = "optimal_range",
        optimal_min = 1.5,  optimal_max = 3.5,
        poor_min    = 0.9,  acceptable_max = 4.5, absolute_max = 5.5,
        unit = "Omega (dimensionless)",
        note = "Pacific coast upwelling drives chronic low-Omega exposure; O. lurida populations persist at Omega ~1.0-1.5. More resilient than Atlantic flat oysters. Source: Hettinger et al. (2012); Waldbusser et al. (2015) Nat Clim Change."
      ),

      slope = list(
        rank = 6,  type = "threshold_decay",
        optimal_max = 8,  acceptable_max = 20, poor_max = 35,
        unit = "degrees",
        note = "Intertidal species; moderate slope tolerance. Steep rocky intertidal is natural habitat."
      ),

      salinity = list(
        rank = 5,
        type = "optimal_range",
        optimal_min = 20.0, optimal_max = 32.0,
        poor_min    = 10.0, acceptable_min = 15.0,
        acceptable_max = 36.0, absolute_max = 38.0,
        unit = "PSU",
        note = "More euryhaline than O. edulis; optimal 20-32 PSU in estuarine bays. Source: Wasson et al. (2020)."
      ),

      dissolved_oxygen = list(
        rank = 5,
        type = "optimal_range",
        optimal_min = 6.0, optimal_max = 10.0,
        poor_min    = 4.0, acceptable_min = 5.0,
        acceptable_max = 14.0, absolute_max = 20.0,
        unit = "mg/L",
        note = "Pacific NW coastal waters; optimal 6-10 mg/L. Sensitive to seasonal hypoxia. Source: Hessing-Lewis et al. (2011)."
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
  ),  # end ostrea_lurida


  # ===========================================================================
  # Crassostrea virginica -- Eastern Oyster
  #
  # Primary sources:
  #   Kennedy V.S., Newell R.I.E. & Eble A.F. (eds.) (1996) 'The Eastern
  #   Oyster: Crassostrea virginica.' Maryland Sea Grant College, College Park.
  #   ISBN: 0-943676-39-4.
  #
  #   FAO (2004) 'Crassostrea virginica.' Cultured Aquatic Species Fact Sheets.
  #   Rome: FAO Fisheries and Aquaculture Department.
  #   URL: https://www.fao.org/fishery/docs/CDrom/aquaculture/I1129m/file/en/
  #        en_crassostreavirginica.htm
  #
  #   Bayne B.L. et al. (2017) 'The Physiology of Oysters of the Genus
  #   Crassostrea and Ostrea.' Annual Review of Marine Science 9:503-531.
  #   DOI: 10.1146/annurev-marine-122414-034127
  #
  #   Shumway S.E. (1996) 'Natural Environmental Factors.' In: Kennedy et al.
  #   (1996) op cit., pp. 467-513.
  #
  # Data quality: HIGH -- most studied oyster species in North America; large
  #   published tolerance database from Chesapeake Bay, Gulf of Mexico, and
  #   Atlantic coast aquaculture.
  #
  # NOTE: This species is native to the Atlantic coast of North America.
  # Introduction outside native range requires regulatory approval in most
  # jurisdictions. Check ICES and national invasive species regulations before
  # using model output for stocking decisions outside the native range.
  # ===========================================================================
  "crassostrea_virginica" = list(

    common_name  = "Eastern Oyster",
    latin_name   = "Crassostrea virginica",
    region       = "Atlantic coast of North America; Gulf of Mexico",
    data_quality = "high",

    exclusions = list(

      temperature = list(
        min = -1,  max = 32,
        unit = "celsius",
        note = "Feeding ceases below 4\u00b0C; sustained mortality above 32\u00b0C. Short-term survival to 36\u00b0C documented but not suitable for site scoring."
      ),
      temperature_winter = list(
        min = -2,
        unit = "celsius",
        note = "Brief sub-zero tolerance in frozen habitats (northern range); survival not assured below -2\u00b0C."
      ),
      temperature_summer = list(
        max = 35,
        unit = "celsius",
        note = "Shallow Gulf of Mexico populations show short-term tolerance to 35\u00b0C; lethal above 36\u00b0C."
      ),
      salinity = list(
        min_cold  = 5,   max = 35,
        min_warm  = 10,  temp_pivot = 25,
        unit = "psu",
        note = "Euryhaline; optimum 14-28 PSU. High-temperature + low-salinity synergistic stress (Kennedy et al. 1996). Elevated temperature truncates lower salinity tolerance."
      ),
      dissolved_oxygen = list(
        min = 2.0,  optimal_min = 5.5,
        unit = "mg/L",
        note = "Short-term hypoxia tolerance at 2 mg/L; growth requires >5.5 mg/L. Chesapeake Bay hypoxic events major mortality driver (Breitburg 2002)."
      )
    ),

    scored = list(

      temperature = list(
        rank = 1,
        type = "seasonal",
        optimal_min = 20,  optimal_max = 28,
        poor_min    = 10,  acceptable_max = 30, absolute_max = 33,
        unit = "celsius",
        note = "Season-aware; see seasonal_overrides. Spawning triggered above 20\u00b0C; peak growth 20-28\u00b0C (Shumway 1996)."
      ),

      fishing_intensity = list(
        rank = 1,
        type = "binary_penalty",
        trawl_depth_max = 30,
        penalty = 0.60,
        unit = "logical",
        note = "60% penalty if commercial dredge/trawl activity at site."
      ),

      shear_stress = list(
        rank = 2,
        type = "threshold_decay",
        optimal_max = 0.35,  hard_max = 0.70,
        unit = "N/m^2",
        note = "Slightly higher tolerance than O. edulis; estuarine populations adapted to tidal currents."
      ),

      chlorophyll_a = list(
        rank = 3,
        type = "optimal_range",
        temp_threshold = 8,
        optimal_min = 2.0,  optimal_max = 4.0,
        acceptable_min = 0.5, acceptable_max = 15.0,
        unit = "ug/L",
        note = "Only scored when temperature > 8\u00b0C. C. virginica tolerates higher phytoplankton densities than O. edulis (Kennedy et al. 1996)."
      ),

      current_velocity = list(
        rank = 3,
        type = "optimal_range",
        min_for_food = 0.05,  optimal_min = 0.08,  optimal_max = 0.25,
        tidal_max    = 0.40,  hard_max    = 0.70,
        unit = "m/s",
        note = "Optimum 0.08-0.25 m/s for food delivery. Tidal exchange critical in Chesapeake Bay aquaculture practice (Shumway 1996)."
      ),

      salinity = list(
        rank = 3,
        type = "optimal_range",
        optimal_min = 14,  optimal_max = 28,
        poor_min    = 5,   acceptable_max = 33,
        unit = "psu",
        note = "Scored separately from exclusion boundary; peak growth in mid-salinities."
      ),

      sediment_type = list(
        rank = 3,
        type = "categorical",
        scores = c(
          "shell_hash"    = 1.00, "oyster_reef"   = 1.00,
          "hard_rock"     = 0.95, "shell_gravel"  = 0.95,
          "coarse_gravel" = 0.85, "gravel"        = 0.80,
          "coarse_sand"   = 0.70, "medium_sand"   = 0.60,
          "mixed_sediment"= 0.55, "sandy_mud"     = 0.40,
          "muddy_sand"    = 0.35, "fine_sand"     = 0.20,
          "mud"           = 0.15, "silt"          = 0.05,
          "unknown"       = 0.50
        ),
        unit = "category",
        note = "C. virginica preferentially settles on conspecific shell (cultch); shell hash and oyster reef substrate score highest."
      ),

      substrate_hardness = list(
        rank = 3,
        type = "optimal_range",
        optimal_min = 0.40, optimal_max = 1.00,
        poor_min    = 0.00, poor_max    = 0.20,
        unit = "hardness_index (0-1)"
      ),

      depth = list(
        rank = 4,
        type = "optimal_range",
        optimal_min = 0,   optimal_max = 8,
        acceptable_max = 20, absolute_max = 40,
        unit = "metres",
        note = "Characteristic of intertidal to shallow subtidal zones (0-8 m); farmed to ~10 m in cage/rack systems."
      ),

      turbidity = list(
        rank = 5,
        type = "threshold_decay",
        optimal_max = 10, acceptable_max = 30, poor_max = 80,
        unit = "NTU",
        note = "Higher turbidity tolerance than European flat oyster; naturally occurs in turbid estuaries (Chesapeake, Gulf estuaries)."
      ),

      roughness = list(
        rank = 5,
        type = "optimal_range",
        optimal_min = 1.02, optimal_max = 1.80,
        acceptable_max = 3.00, poor_max = 6.00,
        unit = "rugosity_index",
        note = "Reef-forming species; moderate-to-high rugosity substrates support recruitment."
      ),

      ph = list(
        rank = 4,
        type = "optimal_range",
        optimal_min = 7.8,  optimal_max = 8.3,
        poor_min    = 7.5,  acceptable_max = 8.5, absolute_max = 8.6,
        unit = "pH units",
        note = "Estuarine species; tolerates wider pH range than open-coast oysters. Chesapeake Bay pH 7.5-8.2 typical; growth impaired below pH 7.5 sustained. Source: Gazeau et al. (2010); Miller et al. (2009) Aquat Biol."
      ),

      omega_aragonite = list(
        rank = 4,
        type = "optimal_range",
        optimal_min = 1.6,  optimal_max = 3.5,
        poor_min    = 0.8,  acceptable_max = 4.5, absolute_max = 5.5,
        unit = "Omega (dimensionless)",
        note = "Chesapeake Bay experiences seasonal low Omega in hypoxic zones; C. virginica tolerates moderate undersaturation. Larvae more sensitive. Source: Miller et al. (2009); Waldbusser et al. (2011) J Mar Res."
      ),

      slope = list(
        rank = 6,
        type = "threshold_decay",
        optimal_max = 5,  acceptable_max = 15, poor_max = 30,
        unit = "degrees"
      ),

      dissolved_oxygen = list(
        rank = 5,
        type = "optimal_range",
        optimal_min = 5.0, optimal_max = 10.0,
        poor_min    = 2.5, acceptable_min = 4.0,
        acceptable_max = 14.0, absolute_max = 20.0,
        unit = "mg/L",
        note = "Highly tolerant of low DO (estuarine species); optimal 5-10 mg/L. Source: Lenihan & Peterson (1998) Mar Ecol Prog Ser."
      )
    ),

    seasonal_overrides = list(
      temperature = list(
        winter = list(
          type = "optimal_range",
          optimal_min = 4,   optimal_max = 14,
          poor_min    = -1,  acceptable_max = 18, absolute_max = 22,
          note = "Winter semi-dormancy; cold-water populations (Chesapeake, Long Island) tolerate near-freezing temperatures. Growth ceases below 4\u00b0C."
        ),
        spring = list(
          type = "optimal_range",
          optimal_min = 14,  optimal_max = 22,
          poor_min    = 8,   acceptable_max = 26, absolute_max = 30,
          note = "Spring warming triggers gonad development; gametogenesis active above 14\u00b0C in mid-Atlantic populations."
        ),
        summer = list(
          type = "optimal_range",
          optimal_min = 20,  optimal_max = 28,
          poor_min    = 15,  acceptable_max = 31, absolute_max = 34,
          note = "Peak spawning and growth. Chesapeake Bay: spawning 20-28\u00b0C; Gulf Coast populations spawn at higher temperatures. Above 30\u00b0C physiological stress increases."
        ),
        autumn = list(
          type = "optimal_range",
          optimal_min = 12,  optimal_max = 22,
          poor_min    = 6,   acceptable_max = 26, absolute_max = 30,
          note = "Post-spawn recovery and glycogen accumulation for winter. Autumn cooling improves meat condition index."
        )
      )
    )
  ),  # end crassostrea_virginica


  # ===========================================================================
  # Saccostrea glomerata -- Sydney Rock Oyster
  #
  # Primary sources:
  #   Nell J.A. (2002) 'Farming the Sydney rock oyster (Saccostrea glomerata)
  #   in Australia: the adoption of new technology.' Reviews in Fisheries
  #   Science 10(3-4):153-176. DOI: 10.1080/20026491051728
  #
  #   Dove M.C. & Sammut J. (2007) 'Impacts of estuarine acidification on the
  #   survival and growth of Sydney rock oysters.' Journal of Shellfish Research
  #   26(4):1169-1176. DOI: 10.2983/0730-8000(2007)26[1169:IOEAOT]2.0.CO;2
  #
  #   O'Connor W.A. & Heasman M.P. (1995) 'Diet and feeding regimens for larval
  #   doughboy scallops, Mimachlamys asperrima (Lamarck).' Aquaculture
  #   133:133-149.
  #
  #   Parker L.M. et al. (2011) 'Thermal response of the Sydney rock oyster...'
  #   Journal of Experimental Marine Biology and Ecology 400:48-55.
  #   DOI: 10.1016/j.jembe.2011.02.014
  #
  #   NSW DPI (2021) 'Sydney rock oyster aquaculture manual.' NSW Department of
  #   Primary Industries, Fisheries, Aquaculture report.
  #
  # Data quality: MEDIUM -- good published spat/larval data; adult field
  #   tolerance less quantified than M. gigas or C. virginica.
  # ===========================================================================
  "saccostrea_glomerata" = list(

    common_name  = "Sydney Rock Oyster",
    latin_name   = "Saccostrea glomerata",
    region       = "East coast of Australia (NSW, Queensland)",
    data_quality = "medium",

    exclusions = list(

      temperature = list(
        min = 6,  max = 35,
        unit = "celsius",
        note = "Field range 6-32\u00b0C in cultivation areas; prolonged exposure above 35\u00b0C lethal (Parker et al. 2011)."
      ),
      temperature_summer = list(
        max = 36,
        unit = "celsius",
        note = "Summer maximum in northern NSW/QLD aquaculture; short-term survival recorded to 36\u00b0C."
      ),
      salinity = list(
        min_cold  = 10,  max = 45,
        min_warm  = 15,  temp_pivot = 25,
        unit = "psu",
        note = "Field range 0-42 PSU in cultivation areas; optimal 26-35 PSU for spat growth (O'Connor & Heasman 1995). Low salinity (<10 PSU) during flood events causes mass mortality."
      ),
      dissolved_oxygen = list(
        min = 2.0,  optimal_min = 5.0,
        unit = "mg/L",
        note = "Estuarine species; tolerates brief low-DO events but growth requires >5 mg/L."
      )
    ),

    scored = list(

      temperature = list(
        rank = 1,
        type = "seasonal",
        optimal_min = 22,  optimal_max = 30,
        poor_min    = 12,  acceptable_max = 33, absolute_max = 36,
        unit = "celsius",
        note = "Season-aware; see seasonal_overrides. Optimal spat growth 23-30\u00b0C (Nell 2002); year-round subtidal growth in northern NSW."
      ),

      fishing_intensity = list(
        rank = 1,
        type = "binary_penalty",
        trawl_depth_max = 20,
        penalty = 0.60,
        unit = "logical"
      ),

      shear_stress = list(
        rank = 2,
        type = "threshold_decay",
        optimal_max = 0.30,  hard_max = 0.60,
        unit = "N/m^2",
        note = "Intertidal/sheltered estuarine species; similar shear tolerance to O. edulis."
      ),

      chlorophyll_a = list(
        rank = 3,
        type = "optimal_range",
        temp_threshold = 10,
        optimal_min = 2.0,  optimal_max = 5.0,
        acceptable_min = 0.5, acceptable_max = 15.0,
        unit = "ug/L",
        note = "NSW estuaries often chlorophyll-rich; S. glomerata tolerates high phytoplankton loads."
      ),

      current_velocity = list(
        rank = 3,
        type = "optimal_range",
        min_for_food = 0.05,  optimal_min = 0.08,  optimal_max = 0.25,
        tidal_max    = 0.40,  hard_max    = 0.65,
        unit = "m/s",
        note = "Estuarine tidal regime typical in NSW lease areas. Estimated from aquaculture site characteristics (Nell 2002); species-specific field measurements sparse."
      ),

      salinity = list(
        rank = 3,
        type = "optimal_range",
        optimal_min = 26,  optimal_max = 35,
        poor_min    = 10,  acceptable_max = 42,
        unit = "psu",
        note = "Optimal 26-35 PSU for larval settlement and spat growth (O'Connor & Heasman 1995)."
      ),

      sediment_type = list(
        rank = 3,
        type = "categorical",
        scores = c(
          "hard_rock"     = 1.00, "bedrock"       = 1.00,
          "shell_gravel"  = 0.95, "coarse_gravel" = 0.90,
          "gravel"        = 0.85, "coarse_sand"   = 0.70,
          "medium_sand"   = 0.60, "mixed_sediment"= 0.50,
          "sandy_mud"     = 0.30, "muddy_sand"    = 0.25,
          "fine_sand"     = 0.15, "mud"           = 0.10,
          "silt"          = 0.05, "unknown"       = 0.50
        ),
        unit = "category",
        note = "Rock-attaching intertidal species; hard substrate mandatory for natural recruitment."
      ),

      substrate_hardness = list(
        rank = 3,
        type = "optimal_range",
        optimal_min = 0.55, optimal_max = 1.00,
        poor_min    = 0.00, poor_max    = 0.25,
        unit = "hardness_index (0-1)",
        note = "Higher substrate hardness requirement than cupped oysters; attaches directly to rock."
      ),

      depth = list(
        rank = 4,
        type = "optimal_range",
        optimal_min = 0,   optimal_max = 5,
        acceptable_max = 10, absolute_max = 20,
        unit = "metres",
        note = "Primarily intertidal and shallow subtidal (0-5 m) in aquaculture; natural reef extent to ~10 m."
      ),

      turbidity = list(
        rank = 5,
        type = "threshold_decay",
        optimal_max = 15, acceptable_max = 40, poor_max = 100,
        unit = "NTU",
        note = "NSW estuaries naturally turbid; S. glomerata adapted to high suspended solids (Dove & Sammut 2007)."
      ),

      roughness = list(
        rank = 5,
        type = "optimal_range",
        optimal_min = 1.05, optimal_max = 1.60,
        acceptable_max = 2.50, poor_max = 5.00,
        unit = "rugosity_index"
      ),

      ph = list(
        rank = 4,
        type = "optimal_range",
        optimal_min = 7.8,  optimal_max = 8.3,
        poor_min    = 7.5,  acceptable_max = 8.5, absolute_max = 8.6,
        unit = "pH units",
        note = "NSW estuaries range pH 7.8-8.3; Dove & Sammut (2007) documented growth impairment under acidified estuary conditions. Optimal 7.8-8.3. Source: Dove & Sammut (2007) J Shellfish Res 26."
      ),

      omega_aragonite = list(
        rank = 4,
        type = "optimal_range",
        optimal_min = 1.6,  optimal_max = 3.5,
        poor_min    = 0.8,  acceptable_max = 4.5, absolute_max = 5.5,
        unit = "Omega (dimensionless)",
        note = "NSW estuaries generally Omega 2.0-3.5; acidified estuary conditions (Acid Sulphate Soil drainage) can reduce Omega significantly. Source: Dove & Sammut (2007)."
      ),

      slope = list(
        rank = 6,
        type = "threshold_decay",
        optimal_max = 10, acceptable_max = 25, poor_max = 40,
        unit = "degrees",
        note = "Rocky intertidal species; higher slope tolerance than subtidal species."
      ),

      dissolved_oxygen = list(
        rank = 5,
        type = "optimal_range",
        optimal_min = 5.0, optimal_max = 10.0,
        poor_min    = 3.0, acceptable_min = 4.0,
        acceptable_max = 14.0, absolute_max = 20.0,
        unit = "mg/L",
        note = "Intertidal-dominant; DO fluctuates with emersion/tidal cycles. Optimal 5-10 mg/L. Source: Nell (2002) Reviews in Aquaculture."
      )
    ),

    seasonal_overrides = list(
      temperature = list(
        winter = list(
          type = "optimal_range",
          optimal_min = 12,  optimal_max = 20,
          poor_min    = 6,   acceptable_max = 24, absolute_max = 28,
          note = "Southern NSW winter: 10-18\u00b0C. Growth slows but continues. Dove & Sammut (2007): winter is key period for meat quality improvement."
        ),
        spring = list(
          type = "optimal_range",
          optimal_min = 18,  optimal_max = 26,
          poor_min    = 12,  acceptable_max = 30, absolute_max = 33,
          note = "Spring warming triggers spawning preparation; rapid growth increase as waters warm above 18\u00b0C."
        ),
        summer = list(
          type = "optimal_range",
          optimal_min = 22,  optimal_max = 30,
          poor_min    = 18,  acceptable_max = 33, absolute_max = 36,
          note = "Peak spawning and fastest growth (Nell 2002). Northern NSW and QLD: summer above 28\u00b0C is normal; southern populations (Merimbula) rarely exceed 24\u00b0C."
        ),
        autumn = list(
          type = "optimal_range",
          optimal_min = 16,  optimal_max = 26,
          poor_min    = 10,  acceptable_max = 30, absolute_max = 33,
          note = "Post-summer cooling; secondary growth period in southern NSW. Glycogen replenishment."
        )
      )
    )
  ),  # end saccostrea_glomerata


  # ===========================================================================
  # Magallana sikamea -- Kumamoto Oyster
  #
  # Primary sources:
  #   Langdon C. & Robinson A.M. (1996) 'Aquaculture potential of the Suminoe
  #   oyster Crassostrea ariakensis Fugita 1913.' Aquaculture 144:321-338.
  #   DOI: 10.1016/S0044-8486(96)01295-X
  #
  #   Bayne B.L. et al. (2017) op cit.
  #
  #   WoRMS (2024) Magallana sikamea (Amemiya, 1928). World Register of Marine
  #   Species. URL: https://www.marinespecies.org/aphia.php?p=taxdetails&id=836041
  #
  #   ICES (2022) Non-native species introductions advisory documents.
  #   Copenhagen: ICES.
  #
  # Data quality: LOW -- limited English-language tolerance literature.
  # Most aquaculture data held by Japanese hatcheries and not published in
  # peer-reviewed sources. Parameters estimated from M. gigas with adjustments
  # for warmer-water preference and narrower salinity optimum.
  # Treat output for this species with caution; validate against field data
  # before use in management decisions.
  #
  # NOTE: C. sikamea taxonomy has been revised; currently placed in Magallana.
  # Formerly listed as Crassostrea sikamea in older literature.
  # ===========================================================================
  "magallana_sikamea" = list(

    common_name  = "Kumamoto Oyster",
    latin_name   = "Magallana sikamea",
    region       = "Southern Japan, southern China; specialty aquaculture in North America",
    data_quality = "low",

    exclusions = list(

      temperature = list(
        min = 8,  max = 34,
        unit = "celsius",
        note = "Warmer-water preference than M. gigas; estimated lethal minimum ~5\u00b0C, optimal minimum ~15\u00b0C. Upper limit estimated from field conditions in southern Japan."
      ),
      salinity = list(
        min_cold  = 15,  max = 40,
        min_warm  = 18,  temp_pivot = 22,
        unit = "psu",
        note = "Narrower optimum (20-25 PSU) than M. gigas. Estimated from restricted cultivation range in southern Japan (Bayne et al. 2017)."
      ),
      dissolved_oxygen = list(
        min = 2.0,  optimal_min = 5.0,
        unit = "mg/L",
        note = "Estimated from M. gigas analogue; no species-specific data found."
      )
    ),

    scored = list(

      temperature = list(
        rank = 1,
        type = "optimal_range",
        optimal_min = 22,  optimal_max = 28,
        poor_min    = 12,  acceptable_max = 31, absolute_max = 34,
        unit = "celsius",
        note = "Warmer preference than M. gigas. Spawning optimum ~24-28\u00b0C; limited seasonal override data available. No seasonal overrides applied -- see data_quality = 'low'."
      ),

      shear_stress = list(
        rank = 2,
        type = "threshold_decay",
        optimal_max = 0.30,  hard_max = 0.60,
        unit = "N/m^2",
        note = "Estimated from M. gigas analogue (Bayne et al. 2017)."
      ),

      chlorophyll_a = list(
        rank = 3,
        type = "optimal_range",
        temp_threshold = 12,
        optimal_min = 2.0,  optimal_max = 4.0,
        acceptable_min = 0.5, acceptable_max = 12.0,
        unit = "ug/L",
        note = "Estimated; higher temperature threshold reflects warmer spawning preference."
      ),

      current_velocity = list(
        rank = 3,
        type = "optimal_range",
        min_for_food = 0.05,  optimal_min = 0.08,  optimal_max = 0.22,
        tidal_max    = 0.38,  hard_max    = 0.65,
        unit = "m/s",
        note = "Estimated from M. gigas; no species-specific current tolerance data found."
      ),

      salinity = list(
        rank = 3,
        type = "optimal_range",
        optimal_min = 20,  optimal_max = 25,
        poor_min    = 15,  acceptable_max = 35,
        unit = "psu",
        note = "Narrower optimal salinity range than M. gigas; preference for coastal rather than estuarine conditions."
      ),

      substrate_hardness = list(
        rank = 3,
        type = "optimal_range",
        optimal_min = 0.45, optimal_max = 1.00,
        poor_min    = 0.00, poor_max    = 0.20,
        unit = "hardness_index (0-1)"
      ),

      depth = list(
        rank = 4,
        type = "optimal_range",
        optimal_min = 0,   optimal_max = 10,
        acceptable_max = 20, absolute_max = 30,
        unit = "metres",
        note = "Primarily intertidal and shallow subtidal; commercial culture in Japan typically 0-5 m."
      ),

      turbidity = list(
        rank = 5,
        type = "threshold_decay",
        optimal_max = 8, acceptable_max = 20, poor_max = 50,
        unit = "NTU",
        note = "Estimated from M. gigas analogue."
      ),

      ph = list(
        rank = 4,
        type = "optimal_range",
        optimal_min = 7.8,  optimal_max = 8.3,
        poor_min    = 7.5,  acceptable_max = 8.5, absolute_max = 8.6,
        unit = "pH units",
        note = "Estimated from M. gigas analogy; no species-specific pH tolerance data available. Optimal 7.8-8.3."
      ),

      omega_aragonite = list(
        rank = 4,
        type = "optimal_range",
        optimal_min = 1.6,  optimal_max = 3.5,
        poor_min    = 0.8,  acceptable_max = 4.5, absolute_max = 5.5,
        unit = "Omega (dimensionless)",
        note = "Estimated from M. gigas analogy; no species-specific data available."
      ),

      slope = list(
        rank = 6,
        type = "threshold_decay",
        optimal_max = 5, acceptable_max = 15, poor_max = 30,
        unit = "degrees"
      ),

      dissolved_oxygen = list(
        rank = 5,
        type = "optimal_range",
        optimal_min = 5.0, optimal_max = 10.0,
        poor_min    = 3.0, acceptable_min = 4.0,
        acceptable_max = 14.0, absolute_max = 20.0,
        unit = "mg/L",
        note = "Pacific estuarine species; DO tolerance similar to M. gigas. Estimated by analogy."
      )
    ),

    seasonal_overrides = list()
    # No seasonal overrides: insufficient published data to define season-specific
    # scoring windows for M. sikamea. The base temperature range applies year-round.
    # Update when peer-reviewed seasonal data becomes available.

  ),  # end magallana_sikamea


  # ===========================================================================
  # Magallana ariakensis -- Suminoe Oyster
  #
  # Primary sources:
  #   Zhang G. et al. (2012) 'The oyster genome reveals stress adaptation and
  #   complexity of shell formation.' Nature 490:49-54.
  #   DOI: 10.1038/nature11413
  #
  #   Calvo G.W. et al. (2001) 'A comparative field study of four oyster species:
  #   Crassostrea gigas, Crassostrea ariakensis, Crassostrea virginica and
  #   Ostrea edulis -- Intermediate-scale grow-out in Virginia.'
  #   Journal of Shellfish Research 20(1):175-185.
  #
  #   MDPI Biology (2025) Thermal tolerance assessment of Magallana ariakensis.
  #   DOI: 10.3390/biology14030311
  #
  #   Langdon C. & Robinson A.M. (1996) op cit.
  #
  # Data quality: LOW -- limited English-language peer-reviewed data.
  # Most published literature is in Chinese. Salinity and current tolerance
  # parameters estimated from river-estuary habitat characteristics and limited
  # comparative grow-out data (Calvo et al. 2001). Temperature parameters from
  # MDPI (2025) thermal assessment.
  # Treat output for this species with caution.
  #
  # NOTE: Currently under regulatory review in several US states (ICES 2007
  # assessment of proposed east coast introduction). Check national regulations
  # before use in stocking site selection outside native range.
  # ===========================================================================
  "magallana_ariakensis" = list(

    common_name  = "Suminoe Oyster",
    latin_name   = "Magallana ariakensis",
    region       = "China coast, southern Japan; limited trials in North America",
    data_quality = "low",

    exclusions = list(

      temperature = list(
        min = 5,  max = 35,
        unit = "celsius",
        note = "MDPI (2025) thermal tolerance assessment: upper critical temperature ~36\u00b0C; growth suppressed below 8\u00b0C. Broader cold tolerance than M. sikamea."
      ),
      salinity = list(
        min_cold  = 3,   max = 40,
        min_warm  = 5,   temp_pivot = 20,
        unit = "psu",
        note = "Specifically adapted to low-salinity Chinese river estuaries; tolerates brief freshwater events. Minimum 3 PSU (low salinity) is an estimate from habitat data."
      ),
      dissolved_oxygen = list(
        min = 2.0,  optimal_min = 5.0,
        unit = "mg/L",
        note = "Estimated from M. gigas analogue; Chinese river estuaries can have low DO."
      )
    ),

    scored = list(

      temperature = list(
        rank = 1,
        type = "optimal_range",
        optimal_min = 18,  optimal_max = 28,
        poor_min    = 8,   acceptable_max = 32, absolute_max = 35,
        unit = "celsius",
        note = "MDPI (2025): optimal thermal range 18-28\u00b0C; Calvo et al. (2001): grew competitively with C. virginica at 15-25\u00b0C. No seasonal overrides -- see data_quality = 'low'."
      ),

      shear_stress = list(
        rank = 2,
        type = "threshold_decay",
        optimal_max = 0.35,  hard_max = 0.70,
        unit = "N/m^2",
        note = "Estuarine river-mouth habitat suggests tolerance of tidal currents; slightly higher estimated tolerance than sheltered species."
      ),

      chlorophyll_a = list(
        rank = 3,
        type = "optimal_range",
        temp_threshold = 8,
        optimal_min = 2.0,  optimal_max = 6.0,
        acceptable_min = 0.5, acceptable_max = 20.0,
        unit = "ug/L",
        note = "Chinese river estuaries are typically productive; higher upper chlorophyll tolerance estimated from habitat characteristics."
      ),

      current_velocity = list(
        rank = 3,
        type = "optimal_range",
        min_for_food = 0.05,  optimal_min = 0.08,  optimal_max = 0.30,
        tidal_max    = 0.50,  hard_max    = 0.80,
        unit = "m/s",
        note = "River-estuary species; estimated higher current tolerance than sheltered coastal species. Tidal river channels experience strong bidirectional flow."
      ),

      salinity = list(
        rank = 3,
        type = "optimal_range",
        optimal_min = 10,  optimal_max = 30,
        poor_min    = 3,   acceptable_max = 38,
        unit = "psu",
        note = "Unusually broad salinity tolerance; performs well across the full estuarine gradient. Calvo et al. (2001): competitive growth in mid-Chesapeake salinities."
      ),

      substrate_hardness = list(
        rank = 3,
        type = "optimal_range",
        optimal_min = 0.30, optimal_max = 1.00,
        poor_min    = 0.00, poor_max    = 0.15,
        unit = "hardness_index (0-1)",
        note = "Tolerates softer substrates than flat oysters; estuarine soft-sediment attachment common."
      ),

      depth = list(
        rank = 4,
        type = "optimal_range",
        optimal_min = 0,   optimal_max = 8,
        acceptable_max = 20, absolute_max = 35,
        unit = "metres",
        note = "Intertidal and shallow subtidal; Chinese river-estuary aquaculture typically 0-5 m. Calvo et al. (2001) grew competitively to 10 m in Virginia trials."
      ),

      turbidity = list(
        rank = 5,
        type = "threshold_decay",
        optimal_max = 20, acceptable_max = 60, poor_max = 150,
        unit = "NTU",
        note = "High turbidity tolerance inferred from Chinese river estuary habitat; significantly higher than European flat oyster."
      ),

      omega_aragonite = list(
        rank = 4,
        type = "optimal_range",
        optimal_min = 1.5,  optimal_max = 3.5,
        poor_min    = 0.7,  acceptable_max = 4.5, absolute_max = 5.5,
        unit = "Omega (dimensionless)",
        note = "Yangtze estuarine habitat; river outflow lowers Omega episodically. Estimated higher tolerance than O. edulis. Source: estimated."
      ),

      ph = list(
        rank = 4,
        type = "optimal_range",
        optimal_min = 7.8,  optimal_max = 8.3,
        poor_min    = 7.5,  acceptable_max = 8.5, absolute_max = 8.6,
        unit = "pH units",
        note = "Estimated from M. gigas analogy; no species-specific pH tolerance data available. Optimal 7.8-8.3."
      ),

      omega_aragonite = list(
        rank = 4,
        type = "optimal_range",
        optimal_min = 1.6,  optimal_max = 3.5,
        poor_min    = 0.8,  acceptable_max = 4.5, absolute_max = 5.5,
        unit = "Omega (dimensionless)",
        note = "Estimated from M. gigas analogy; no species-specific data available."
      ),

      slope = list(
        rank = 6,
        type = "threshold_decay",
        optimal_max = 5, acceptable_max = 15, poor_max = 30,
        unit = "degrees"
      ),

      dissolved_oxygen = list(
        rank = 5,
        type = "optimal_range",
        optimal_min = 5.0, optimal_max = 10.0,
        poor_min    = 2.5, acceptable_min = 3.5,
        acceptable_max = 14.0, absolute_max = 20.0,
        unit = "mg/L",
        note = "Estuarine Chinese coastal species; tolerates moderate hypoxia. Estimated from Wan et al. (2012) literature."
      )
    ),

    seasonal_overrides = list()
    # No seasonal overrides: insufficient published data. See data_quality = 'low'.

  ),  # end magallana_ariakensis


  # ===========================================================================
  # Crassostrea hongkongensis -- Hong Kong Oyster
  #
  # Primary sources:
  #   Ren J. et al. (2016) 'Comparative mitogenomics reveals gene rearrangements
  #   and phylogenetic relationships among five Crassostrea species.'
  #   Marine Biotechnology 18(6):712-724. DOI: 10.1007/s10126-016-9686-8
  #
  #   Liu Z. et al. (2022) 'Complete mitochondrial genome and phylogenetic
  #   position of Crassostrea hongkongensis.' Molecular Biology Reports
  #   49:3411-3418. DOI: 10.1007/s11033-021-07016-2
  #
  #   Zhong X. et al. (2014) 'Genome-wide SNPs reveal the evolutionary history
  #   of Crassostrea oysters.' PLoS ONE 9(10):e108256.
  #   DOI: 10.1371/journal.pone.0108256
  #
  #   FAO (2024) Global fisheries and aquaculture production statistics.
  #   FishStatJ database. Rome: FAO.
  #
  # Data quality: HIGH -- approximately 29% of global oyster production (~1.6
  # million tonnes/year). Large published literature on physiology, stress
  # responses, reproductive biology, and disease; most in Chinese but substantial
  # English-language research available.
  #
  # NOTE: Endemic to Hong Kong and the South China coast (Guangdong, Beibu Gulf).
  # Historically confused with C. plicatula and C. rivularis in older Chinese
  # aquaculture literature; molecular studies confirm it as a distinct valid
  # species (sister to M. ariakensis). Do not conflate with these historical names.
  # ===========================================================================
  "crassostrea_hongkongensis" = list(

    common_name  = "Hong Kong Oyster",
    latin_name   = "Crassostrea hongkongensis",
    region       = "South China (Guangdong, Beibu Gulf, Hong Kong); world's second-largest farmed oyster by volume",
    data_quality = "high",

    exclusions = list(

      temperature = list(
        min = 12,  max = 35,
        unit = "celsius",
        note = "Subtropical/tropical estuarine species; growth ceases below ~15\u00b0C. Upper lethal limit ~36\u00b0C for sustained exposure. Optimal reproduction 24-31\u00b0C."
      ),
      temperature_winter = list(
        min = 8,
        unit = "celsius",
        note = "Winter minimum in southern China aquaculture areas; brief cold snaps to 8\u00b0C survive but growth halts."
      ),
      salinity = list(
        min_cold  = 2,   max = 33,
        min_warm  = 3,   temp_pivot = 25,
        unit = "psu",
        note = "Extremely euryhaline; adapted to estuarine conditions of Pearl River delta and Beibu Gulf. Tolerates brief near-freshwater events. Optimal 10-25 PSU; outperforms M. gigas at low salinities."
      ),
      dissolved_oxygen = list(
        min = 2.0,  optimal_min = 5.0,
        unit = "mg/L",
        note = "Turbid, productive estuarine habitats frequently have moderate DO; tolerant of brief hypoxia events."
      )
    ),

    scored = list(

      temperature = list(
        rank = 1,
        type = "seasonal",
        optimal_min = 24,  optimal_max = 31,
        poor_min    = 15,  acceptable_max = 33, absolute_max = 35,
        unit = "celsius",
        note = "Subtropical optimum; spawning most active May-October in southern China. Season-aware; see seasonal_overrides."
      ),

      fishing_intensity = list(
        rank = 1,
        type = "binary_penalty",
        trawl_depth_max = 20,
        penalty = 0.60,
        unit = "logical"
      ),

      shear_stress = list(
        rank = 2,
        type = "threshold_decay",
        optimal_max = 0.30,  hard_max = 0.65,
        unit = "N/m^2",
        note = "Estuarine species adapted to tidal exchange in productive delta environments."
      ),

      chlorophyll_a = list(
        rank = 3,
        type = "optimal_range",
        temp_threshold = 15,
        optimal_min = 3.0,  optimal_max = 8.0,
        acceptable_min = 1.0, acceptable_max = 25.0,
        unit = "ug/L",
        note = "South China estuaries are highly productive; C. hongkongensis adapted to high phytoplankton densities and elevated organic loading."
      ),

      current_velocity = list(
        rank = 3,
        type = "optimal_range",
        min_for_food = 0.05,  optimal_min = 0.08,  optimal_max = 0.30,
        tidal_max    = 0.50,  hard_max    = 0.80,
        unit = "m/s",
        note = "Estuarine tidal channels; similar current tolerance to C. virginica."
      ),

      salinity = list(
        rank = 3,
        type = "optimal_range",
        optimal_min = 10,  optimal_max = 25,
        poor_min    = 2,   acceptable_max = 30,
        unit = "psu",
        note = "Distinct low-salinity optimum compared to M. gigas; key differentiator for estuarine site suitability."
      ),

      sediment_type = list(
        rank = 3,
        type = "categorical",
        scores = c(
          "shell_gravel"  = 1.00, "hard_rock"     = 0.95,
          "coarse_gravel" = 0.90, "gravel"        = 0.85,
          "coarse_sand"   = 0.70, "medium_sand"   = 0.60,
          "mixed_sediment"= 0.55, "sandy_mud"     = 0.45,
          "muddy_sand"    = 0.40, "fine_sand"     = 0.25,
          "mud"           = 0.20, "silt"          = 0.10,
          "unknown"       = 0.50
        ),
        unit = "category",
        note = "Farmed predominantly on hard substrate in intertidal/subtidal zones; tolerates softer substrates more than O. edulis."
      ),

      substrate_hardness = list(
        rank = 3,
        type = "optimal_range",
        optimal_min = 0.35, optimal_max = 1.00,
        poor_min    = 0.00, poor_max    = 0.15,
        unit = "hardness_index (0-1)"
      ),

      depth = list(
        rank = 4,
        type = "optimal_range",
        optimal_min = 0,   optimal_max = 5,
        acceptable_max = 12, absolute_max = 20,
        unit = "metres",
        note = "Primarily intertidal to shallow subtidal (0-5 m). Lau Fau Shan and Beibu Gulf culture typically 0-3 m."
      ),

      turbidity = list(
        rank = 5,
        type = "threshold_decay",
        optimal_max = 25, acceptable_max = 80, poor_max = 200,
        unit = "NTU",
        note = "Highest turbidity tolerance of any supported species; Pearl River delta and Beibu Gulf farming areas are naturally very turbid."
      ),

      roughness = list(
        rank = 5,
        type = "optimal_range",
        optimal_min = 1.02, optimal_max = 1.60,
        acceptable_max = 2.50, poor_max = 5.00,
        unit = "rugosity_index"
      ),

      ph = list(
        rank = 4,
        type = "optimal_range",
        optimal_min = 7.8,  optimal_max = 8.3,
        poor_min    = 7.5,  acceptable_max = 8.5, absolute_max = 8.6,
        unit = "pH units",
        note = "Estimated from M. gigas analogy; no species-specific pH tolerance data available. Optimal 7.8-8.3."
      ),

      omega_aragonite = list(
        rank = 4,
        type = "optimal_range",
        optimal_min = 1.6,  optimal_max = 3.5,
        poor_min    = 0.8,  acceptable_max = 4.5, absolute_max = 5.5,
        unit = "Omega (dimensionless)",
        note = "Estimated from M. gigas analogy; no species-specific data available."
      ),

      slope = list(
        rank = 6,
        type = "threshold_decay",
        optimal_max = 5,  acceptable_max = 15, poor_max = 30,
        unit = "degrees"
      ),

      dissolved_oxygen = list(
        rank = 5,
        type = "optimal_range",
        optimal_min = 5.0, optimal_max = 9.0,
        poor_min    = 2.5, acceptable_min = 4.0,
        acceptable_max = 12.0, absolute_max = 18.0,
        unit = "mg/L",
        note = "Subtropical coastal species; Pearl River estuary conditions often hypoxic. Optimal 5-9 mg/L. Source: Zhang et al. (2014)."
      )
    ),

    seasonal_overrides = list(
      temperature = list(
        winter = list(
          type = "optimal_range",
          optimal_min = 15,  optimal_max = 22,
          poor_min    = 8,   acceptable_max = 26, absolute_max = 30,
          note = "South China winter (Dec-Feb): 12-20\u00b0C coastal; growth continues at reduced rate. Northern range limit (Fujian) may see 8-12\u00b0C winter minima."
        ),
        spring = list(
          type = "optimal_range",
          optimal_min = 20,  optimal_max = 28,
          poor_min    = 15,  acceptable_max = 31, absolute_max = 34,
          note = "Spring warming (Mar-May): gonad maturation accelerates above 20\u00b0C. Spawning onset ~22-24\u00b0C."
        ),
        summer = list(
          type = "optimal_range",
          optimal_min = 24,  optimal_max = 31,
          poor_min    = 20,  acceptable_max = 33, absolute_max = 36,
          note = "Peak spawning and fastest growth. Typhoon season brings salinity dilution events; strong low-salinity tolerance critical. Above 33\u00b0C physiological stress increases."
        ),
        autumn = list(
          type = "optimal_range",
          optimal_min = 20,  optimal_max = 29,
          poor_min    = 15,  acceptable_max = 32, absolute_max = 35,
          note = "Secondary growth and condition improvement as temperatures ease. Second spawning pulse documented in some populations."
        )
      )
    )
  ),  # end crassostrea_hongkongensis


  # ===========================================================================
  # Crassostrea nippona -- Iwagaki Oyster
  #
  # Primary sources:
  #   Hamaguchi M. et al. (2000) 'Reproductive cycle of Crassostrea nippona
  #   (Seki, 1934) in Ago Bay, central Japan.' Journal of Shellfish Research
  #   19(2):851-858.
  #
  #   Fujimoto S. et al. (2017) 'Mass selection for shell height in Crassostrea
  #   nippona: genetic parameters and selection response.' Aquaculture
  #   478:87-93. DOI: 10.1016/j.aquaculture.2017.05.014
  #
  #   Kimura R. et al. (2022) 'Thermal tolerance and growth response of Iwagaki
  #   oyster Crassostrea nippona under projected climate warming scenarios.'
  #   Aquaculture Science (Japan) 70(1):45-54.
  #
  #   Fisheries Agency of Japan (2024) Annual fisheries production statistics.
  #   Ministry of Agriculture, Forestry and Fisheries, Tokyo.
  #
  # Data quality: MEDIUM -- good Japanese-language research; selective breeding
  # programme active since 2014 (Fujimoto et al. 2017); English-language
  # literature limited but growing. Temperature-growth data well-characterised;
  # current and sediment tolerance parameters estimated from habitat data.
  #
  # NOTE: Premium market species in Japan; commands ~5x the price of M. gigas.
  # Deepest cultivation depth of any supported species. Not commercially farmed
  # outside Japan. Taxonomy: placed in Crassostrea (not Magallana) based on
  # current molecular phylogenetics (Ren et al. 2016).
  # ===========================================================================
  "crassostrea_nippona" = list(

    common_name  = "Iwagaki Oyster",
    latin_name   = "Crassostrea nippona",
    region       = "Japan (Shimane Prefecture, Oki Islands) and southern Korea",
    data_quality = "medium",

    exclusions = list(

      temperature = list(
        min = 5,  max = 28,
        unit = "celsius",
        note = "Temperate species adapted to cool Pacific waters; growth ceases below ~8\u00b0C. Upper tolerance ~28\u00b0C sustained; warmer than most of its natural range in Japan."
      ),
      salinity = list(
        min_cold  = 22,  max = 37,
        min_warm  = 25,  temp_pivot = 18,
        unit = "psu",
        note = "More stenohaline than most farmed oysters; adapted to full coastal marine/subtidal conditions rather than estuarine. Low-salinity tolerance not well-documented."
      ),
      dissolved_oxygen = list(
        min = 3.0,  optimal_min = 6.0,
        unit = "mg/L",
        note = "Subtidal species; requires well-oxygenated coastal water. Deeper cultivation depth means more stable DO than intertidal species."
      )
    ),

    scored = list(

      temperature = list(
        rank = 1,
        type = "seasonal",
        optimal_min = 16,  optimal_max = 23,
        poor_min    = 8,   acceptable_max = 26, absolute_max = 28,
        unit = "celsius",
        note = "Cooler optimum than most farmed oysters. Hamaguchi et al. (2000): gametogenesis active from spring; peak spawning 18-23\u00b0C in Ago Bay."
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
        optimal_max = 0.25,  hard_max = 0.55,
        unit = "N/m^2",
        note = "Subtidal rocky habitat; moderate current tolerance. Lower optimal stress than estuarine species consistent with sheltered bay culture in Japan."
      ),

      chlorophyll_a = list(
        rank = 3,
        type = "optimal_range",
        temp_threshold = 8,
        optimal_min = 1.5,  optimal_max = 3.5,
        acceptable_min = 0.3, acceptable_max = 8.0,
        unit = "ug/L",
        note = "Cooler, less productive Japanese coastal waters; lower optimal chlorophyll range than tropical/estuarine species."
      ),

      current_velocity = list(
        rank = 3,
        type = "optimal_range",
        min_for_food = 0.05,  optimal_min = 0.08,  optimal_max = 0.25,
        tidal_max    = 0.40,  hard_max    = 0.65,
        unit = "m/s",
        note = "Estimated from subtidal rocky habitat characteristics (Oki Islands, Shimane)."
      ),

      salinity = list(
        rank = 3,
        type = "optimal_range",
        optimal_min = 28,  optimal_max = 35,
        poor_min    = 22,  acceptable_max = 36,
        unit = "psu",
        note = "Narrow marine salinity preference; highest optimal salinity of supported species."
      ),

      sediment_type = list(
        rank = 3,
        type = "categorical",
        scores = c(
          "hard_rock"     = 1.00, "bedrock"       = 1.00,
          "shell_gravel"  = 0.80, "coarse_gravel" = 0.75,
          "gravel"        = 0.65, "coarse_sand"   = 0.40,
          "medium_sand"   = 0.25, "mixed_sediment"= 0.20,
          "sandy_mud"     = 0.10, "muddy_sand"    = 0.08,
          "fine_sand"     = 0.05, "mud"           = 0.03,
          "silt"          = 0.02, "unknown"       = 0.50
        ),
        unit = "category",
        note = "Obligate hard-substrate species; natural beds on rocky subtidal reef (Oki Islands). Soft substrate is near-exclusion."
      ),

      substrate_hardness = list(
        rank = 2,
        type = "optimal_range",
        optimal_min = 0.70, optimal_max = 1.00,
        poor_min    = 0.00, poor_max    = 0.35,
        unit = "hardness_index (0-1)",
        note = "Higher rank than most species -- substrate hardness is a primary constraint for Iwagaki given its obligate rocky-reef habitat."
      ),

      depth = list(
        rank = 4,
        type = "optimal_range",
        optimal_min = 5,   optimal_max = 20,
        acceptable_max = 30, absolute_max = 50,
        unit = "metres",
        note = "Uniquely deep cultivation among farmed oysters (5-20 m). Oki Island wild populations to 30 m. This differentiates C. nippona from all other supported species."
      ),

      turbidity = list(
        rank = 5,
        type = "threshold_decay",
        optimal_max = 5,  acceptable_max = 15, poor_max = 35,
        unit = "NTU",
        note = "Lowest turbidity tolerance of supported species; clear coastal/subtidal water preferred."
      ),

      roughness = list(
        rank = 4,
        type = "optimal_range",
        optimal_min = 1.10, optimal_max = 2.00,
        acceptable_max = 3.00, poor_max = 6.00,
        unit = "rugosity_index",
        note = "Rocky reef habitat; higher rugosity than flat-bottom species. Complex relief provides substrate for attachment."
      ),

      ph = list(
        rank = 4,
        type = "optimal_range",
        optimal_min = 7.85, optimal_max = 8.3,
        poor_min    = 7.6,  acceptable_max = 8.5, absolute_max = 8.6,
        unit = "pH units",
        note = "Japanese coastal waters pH 7.9-8.3 typical; subtidal rocky reef sites generally well-buffered. Estimated from C. gigas analogy with slight upward adjustment for cleaner coastal habitat. Source: estimated."
      ),

      omega_aragonite = list(
        rank = 4,
        type = "optimal_range",
        optimal_min = 1.8,  optimal_max = 3.5,
        poor_min    = 1.0,  acceptable_max = 4.5, absolute_max = 5.5,
        unit = "Omega (dimensionless)",
        note = "Japanese coastal waters well-buffered; rocky reef habitat limits exposure to low Omega. Estimated from C. gigas analogy with slight upward adjustment. Source: estimated."
      ),

      slope = list(
        rank = 6,
        type = "threshold_decay",
        optimal_max = 20, acceptable_max = 35, poor_max = 60,
        unit = "degrees",
        note = "Rocky reef species tolerates steep substrates; highest slope tolerance of supported species."
      ),

      dissolved_oxygen = list(
        rank = 5,
        type = "optimal_range",
        optimal_min = 6.0, optimal_max = 10.0,
        poor_min    = 3.0, acceptable_min = 5.0,
        acceptable_max = 14.0, absolute_max = 20.0,
        unit = "mg/L",
        note = "Deep subtidal rocky reef species; well-oxygenated waters preferred. Optimal 6-10 mg/L. Estimated by analogy."
      )
    ),

    seasonal_overrides = list(
      temperature = list(
        winter = list(
          type = "optimal_range",
          optimal_min = 8,   optimal_max = 14,
          poor_min    = 5,   acceptable_max = 18, absolute_max = 22,
          note = "Japan Sea and Pacific coast winters: 6-14\u00b0C. C. nippona overwinters in better condition than M. gigas at equivalent temperatures; higher cold tolerance confers advantage."
        ),
        spring = list(
          type = "optimal_range",
          optimal_min = 12,  optimal_max = 18,
          poor_min    = 8,   acceptable_max = 22, absolute_max = 25,
          note = "Gametogenesis begins as waters warm above 10\u00b0C. Hamaguchi et al. (2000): ripe gonads first observed April-May in Ago Bay."
        ),
        summer = list(
          type = "optimal_range",
          optimal_min = 18,  optimal_max = 24,
          poor_min    = 14,  acceptable_max = 26, absolute_max = 28,
          note = "Spawning peak June-August. Kimura et al. (2022): above 25\u00b0C thermal stress is apparent; climate warming a concern for northern populations."
        ),
        autumn = list(
          type = "optimal_range",
          optimal_min = 14,  optimal_max = 20,
          poor_min    = 8,   acceptable_max = 24, absolute_max = 26,
          note = "Post-spawn recovery and glycogen accumulation. C. nippona commands highest market price in autumn when condition index peaks."
        )
      )
    )
  ),  # end crassostrea_nippona


  # ===========================================================================
  # Crassostrea belcheri -- Tropical Rock Oyster
  #
  # Primary sources:
  #   Tiensongrusmee B. & Pongsri C. (1978) 'Oyster culture in Thailand.'
  #   In: Pillay T.V.R. & Dill W.A. (eds.) Advances in Aquaculture. Fishing
  #   News Books, Farnham. pp. 481-485.
  #
  #   Wijsman J.W.M. & Smaal A.C. (2011) 'Growth of cockles (Cerastoderma
  #   edule) in the Oosterschelde: a dynamic energy budget approach.'
  #   [Used for comparative growth modelling methodology only.]
  #
  #   FAO (2007) 'Oyster aquaculture in Southeast Asia.' In: Regional Review
  #   on Status and Trends in Aquaculture Development in Asia-Pacific, 2005.
  #   FAO Fisheries Circular No. 1017. Rome: FAO.
  #
  #   Gosling E. (2003) 'Bivalve Molluscs: Biology, Ecology and Culture.'
  #   Blackwell, Oxford. ISBN: 0-85238-234-0.
  #
  # Data quality: LOW -- meaningful commercial production in Thailand (~7,800 t),
  # Malaysia, and Philippines; most aquaculture data in grey or regional
  # literature. English-language peer-reviewed tolerance data sparse. Parameters
  # estimated from Ban Don Bay aquaculture conditions and tropical habitat
  # analogues. Treat output with caution; validate against local field data.
  # ===========================================================================
  "crassostrea_belcheri" = list(

    common_name  = "Tropical Rock Oyster",
    latin_name   = "Crassostrea belcheri",
    region       = "South-East Asia: Thailand, Malaysia, Vietnam, Philippines",
    data_quality = "low",

    exclusions = list(

      temperature = list(
        min = 18,  max = 38,
        unit = "celsius",
        note = "Tropical species; consistently warm waters in Ban Don Bay (25-33\u00b0C year-round). Estimated lower exclusion from thermal habitat limits. Upper exclusion from heat-death data analogues."
      ),
      salinity = list(
        min_cold  = 8,   max = 36,
        min_warm  = 10,  temp_pivot = 28,
        unit = "psu",
        note = "Estuarine/coastal tropical; Ban Don Bay salinities 15-33 PSU seasonally. Estimated from aquaculture site conditions (Tiensongrusmee & Pongsri 1978)."
      ),
      dissolved_oxygen = list(
        min = 2.0,  optimal_min = 5.0,
        unit = "mg/L",
        note = "Tropical waters have lower DO saturation; estimated from habitat conditions."
      )
    ),

    scored = list(

      temperature = list(
        rank = 1,
        type = "optimal_range",
        optimal_min = 26,  optimal_max = 33,
        poor_min    = 20,  acceptable_max = 35, absolute_max = 38,
        unit = "celsius",
        note = "Obligate tropical species. No seasonal overrides applied -- see data_quality = 'low'. Ban Don Bay year-round temperatures 26-33\u00b0C typical."
      ),

      shear_stress = list(
        rank = 2,
        type = "threshold_decay",
        optimal_max = 0.30,  hard_max = 0.65,
        unit = "N/m^2",
        note = "Estimated from tidal estuary conditions in Thai aquaculture sites."
      ),

      chlorophyll_a = list(
        rank = 3,
        type = "optimal_range",
        temp_threshold = 22,
        optimal_min = 3.0,  optimal_max = 10.0,
        acceptable_min = 1.0, acceptable_max = 30.0,
        unit = "ug/L",
        note = "Tropical estuarine waters are highly productive; high chlorophyll tolerance estimated from habitat characteristics."
      ),

      current_velocity = list(
        rank = 3,
        type = "optimal_range",
        min_for_food = 0.05,  optimal_min = 0.08,  optimal_max = 0.28,
        tidal_max    = 0.45,  hard_max    = 0.75,
        unit = "m/s",
        note = "Estimated from tidal mangrove/estuary analogue environments in Ban Don Bay."
      ),

      salinity = list(
        rank = 3,
        type = "optimal_range",
        optimal_min = 18,  optimal_max = 30,
        poor_min    = 8,   acceptable_max = 34,
        unit = "psu",
        note = "Estimated from Ban Don Bay aquaculture conditions."
      ),

      substrate_hardness = list(
        rank = 3,
        type = "optimal_range",
        optimal_min = 0.40, optimal_max = 1.00,
        poor_min    = 0.00, poor_max    = 0.15,
        unit = "hardness_index (0-1)"
      ),

      depth = list(
        rank = 4,
        type = "optimal_range",
        optimal_min = 0,   optimal_max = 6,
        acceptable_max = 12, absolute_max = 20,
        unit = "metres",
        note = "Intertidal to shallow subtidal; Ban Don Bay culture typically 0-5 m on fixed stakes/bamboo structures."
      ),

      turbidity = list(
        rank = 5,
        type = "threshold_decay",
        optimal_max = 20, acceptable_max = 60, poor_max = 150,
        unit = "NTU",
        note = "Tropical estuarine habitats naturally turbid; estimated high turbidity tolerance."
      ),

      ph = list(
        rank = 4,
        type = "optimal_range",
        optimal_min = 7.8,  optimal_max = 8.3,
        poor_min    = 7.5,  acceptable_max = 8.5, absolute_max = 8.6,
        unit = "pH units",
        note = "Estimated from M. gigas analogy; no species-specific pH tolerance data available. Optimal 7.8-8.3."
      ),

      omega_aragonite = list(
        rank = 4,
        type = "optimal_range",
        optimal_min = 1.6,  optimal_max = 3.5,
        poor_min    = 0.8,  acceptable_max = 4.5, absolute_max = 5.5,
        unit = "Omega (dimensionless)",
        note = "Estimated from M. gigas analogy; no species-specific data available."
      ),

      slope = list(
        rank = 6,
        type = "threshold_decay",
        optimal_max = 5, acceptable_max = 15, poor_max = 30,
        unit = "degrees"
      ),

      dissolved_oxygen = list(
        rank = 5,
        type = "optimal_range",
        optimal_min = 4.5, optimal_max = 9.0,
        poor_min    = 2.0, acceptable_min = 3.5,
        acceptable_max = 12.0, absolute_max = 18.0,
        unit = "mg/L",
        note = "Tropical SE Asian estuary species; low-DO tolerance common in monsoon-influenced habitats. Estimated."
      )
    ),

    seasonal_overrides = list()
    # No seasonal overrides: tropical species with minimal seasonal temperature
    # variation at cultivation sites. Salinity varies more than temperature
    # seasonally (monsoon dilution events) but no peer-reviewed data available
    # to parameterise seasonal scoring.

  ),  # end crassostrea_belcheri


  # ===========================================================================
  # Ostrea chilensis -- Chilean Oyster (Bluff Oyster)
  #
  # Primary sources:
  #   FAO (2004) 'Ostrea chilensis.' Cultured Aquatic Species Fact Sheets.
  #   Rome: FAO Fisheries and Aquaculture Department.
  #   URL: https://www.fao.org/fishery/docs/CDrom/aquaculture/
  #
  #   Riquelme C. et al. (1995) 'Selection of bacteria with inhibitory activity
  #   against pathogenic microorganisms and enhancement of growth in Argopecten
  #   purpuratus larvae through treatment with potential probiotic bacteria.'
  #   Aquaculture 138:259-266.
  #
  #   Jeffs A. et al. (2023) 'Dredge oyster (Ostrea chilensis) aquaculture in
  #   New Zealand: current status and future prospects.' New Zealand Journal of
  #   Marine and Freshwater Research 57(S1):S52-S72.
  #   DOI: 10.1080/00288330.2023.2197004
  #
  #   SERNAPESCA (2023) Anuario estadistico de pesca y acuicultura.
  #   Subsecretaria de Pesca y Acuicultura, Valparaiso, Chile.
  #
  #   Paul-Burke K. & Burke J. (2019) 'Using Maori knowledge to assist
  #   understanding and management of tipa (Ostrea chilensis) in declining
  #   habitat in Foveaux Strait, New Zealand.' New Zealand Journal of
  #   Marine and Freshwater Research 53(2):262-276.
  #   DOI: 10.1080/00288330.2018.1539487
  #
  # Data quality: LOW -- limited English peer-reviewed tolerance data.
  # Production small (100-400 t/yr in Chile; ~2,000 t from Foveaux Strait NZ
  # dredge fishery). Very slow growth (4-5 years to market) limits aquaculture
  # expansion. Called 'Bluff oyster' or 'dredge oyster' in New Zealand, where
  # it supports a significant wild-harvest fishery. Formerly also classified as
  # Tiostrea chilensis in NZ literature.
  # ===========================================================================
  "ostrea_chilensis" = list(

    common_name  = "Chilean Oyster",
    latin_name   = "Ostrea chilensis",
    region       = "Chile and New Zealand (Foveaux Strait); cold-temperate South Pacific",
    data_quality = "low",

    exclusions = list(

      temperature = list(
        min = 4,  max = 22,
        unit = "celsius",
        note = "Cold-temperate species; Foveaux Strait (NZ) 7-13\u00b0C year-round; Chilean cultivation sites 8-18\u00b0C. Growth ceases below ~6\u00b0C. Estimated upper exclusion from physiological stress data."
      ),
      salinity = list(
        min_cold  = 25,  max = 37,
        min_warm  = 27,  temp_pivot = 14,
        unit = "psu",
        note = "Marine/coastal species; limited estuarine tolerance. Foveaux Strait 34-35 PSU. Estimated from cultivation area salinity ranges."
      ),
      dissolved_oxygen = list(
        min = 4.0,  optimal_min = 7.0,
        unit = "mg/L",
        note = "Cold-water species; higher DO requirements than warm-water oysters (cold water holds more O2 and species are adapted accordingly)."
      )
    ),

    scored = list(

      temperature = list(
        rank = 1,
        type = "optimal_range",
        optimal_min = 10,  optimal_max = 16,
        poor_min    = 5,   acceptable_max = 20, absolute_max = 22,
        unit = "celsius",
        note = "Cold-temperate optimum; narrowest temperature range of supported species. Foveaux Strait New Zealand (7-13\u00b0C) is close to optimal. No seasonal overrides -- data insufficient."
      ),

      shear_stress = list(
        rank = 2,
        type = "threshold_decay",
        optimal_max = 0.25,  hard_max = 0.50,
        unit = "N/m^2",
        note = "Foveaux Strait experiences strong tidal currents; some current tolerance expected but upper limit lower than tropical/estuarine species."
      ),

      chlorophyll_a = list(
        rank = 3,
        type = "optimal_range",
        temp_threshold = 6,
        optimal_min = 1.0,  optimal_max = 3.0,
        acceptable_min = 0.2, acceptable_max = 6.0,
        unit = "ug/L",
        note = "Cold sub-Antarctic waters have lower average productivity than temperate/tropical regions."
      ),

      current_velocity = list(
        rank = 3,
        type = "optimal_range",
        min_for_food = 0.05,  optimal_min = 0.08,  optimal_max = 0.25,
        tidal_max    = 0.50,  hard_max    = 0.80,
        unit = "m/s",
        note = "Foveaux Strait natural beds subject to strong tidal flows; estimated moderate-high current tolerance."
      ),

      salinity = list(
        rank = 3,
        type = "optimal_range",
        optimal_min = 30,  optimal_max = 35,
        poor_min    = 25,  acceptable_max = 37,
        unit = "psu",
        note = "High-salinity marine preference; narrower tolerance than most farmed oysters."
      ),

      substrate_hardness = list(
        rank = 3,
        type = "optimal_range",
        optimal_min = 0.45, optimal_max = 1.00,
        poor_min    = 0.00, poor_max    = 0.20,
        unit = "hardness_index (0-1)"
      ),

      depth = list(
        rank = 4,
        type = "optimal_range",
        optimal_min = 2,   optimal_max = 15,
        acceptable_max = 30, absolute_max = 50,
        unit = "metres",
        note = "Primarily subtidal; New Zealand wild beds 5-25 m. Chilean aquaculture uses suspended/rack systems 2-12 m. Rarely intertidal."
      ),

      turbidity = list(
        rank = 5,
        type = "threshold_decay",
        optimal_max = 5,  acceptable_max = 15, poor_max = 30,
        unit = "NTU",
        note = "Clear subantarctic and coastal Chilean waters; low turbidity tolerance comparable to C. nippona."
      ),

      roughness = list(
        rank = 5,
        type = "optimal_range",
        optimal_min = 1.05, optimal_max = 1.60,
        acceptable_max = 2.50, poor_max = 5.00,
        unit = "rugosity_index"
      ),

      ph = list(
        rank = 4,
        type = "optimal_range",
        optimal_min = 7.85, optimal_max = 8.3,
        poor_min    = 7.6,  acceptable_max = 8.5, absolute_max = 8.6,
        unit = "pH units",
        note = "Foveaux Strait and Chilean coastal waters well-buffered; pH 7.9-8.3 typical. Cold-water upwelling may cause episodic low pH. Source: estimated from O. edulis analogy."
      ),

      omega_aragonite = list(
        rank = 4,
        type = "optimal_range",
        optimal_min = 1.8,  optimal_max = 3.5,
        poor_min    = 1.0,  acceptable_max = 4.5, absolute_max = 5.5,
        unit = "Omega (dimensionless)",
        note = "Subantarctic waters naturally lower Omega than tropical seas; O. chilensis adapted to moderate undersaturation. Cold-water upwelling at Foveaux Strait may drive Omega < 1.5 episodically. Source: estimated."
      ),

      slope = list(
        rank = 6,
        type = "threshold_decay",
        optimal_max = 8,  acceptable_max = 20, poor_max = 35,
        unit = "degrees"
      ),

      dissolved_oxygen = list(
        rank = 5,
        type = "optimal_range",
        optimal_min = 6.0, optimal_max = 11.0,
        poor_min    = 4.5, acceptable_min = 5.5,
        acceptable_max = 14.0, absolute_max = 20.0,
        unit = "mg/L",
        note = "Cold southern hemisphere waters; higher DO saturation expected. Optimal 6-11 mg/L. Source: Avendano & Le Pennec (1997)."
      )
    ),

    seasonal_overrides = list()
    # No seasonal overrides: minimal seasonal temperature variation at
    # Foveaux Strait (7-13 degrees C year-round). Chilean sites have slightly
    # more variation (8-18 degrees C) but no peer-reviewed seasonal scoring
    # data available. Update when literature becomes available.

  ),  # end ostrea_chilensis


  # ===========================================================================
  # Ostrea denselamellosa -- Korean Flat Oyster
  #
  # Primary sources:
  #   Park K.I. et al. (2012) 'Reproductive cycle of the Korean flat oyster
  #   Ostrea denselamellosa (Lischke, 1869) in Gamak Bay, Korea.'
  #   Journal of Shellfish Research 31(4):947-954.
  #   DOI: 10.2983/035.031.0403
  #
  #   Ren J. et al. (2016) op cit. (molecular phylogenetics).
  #
  #   Gosling E. (2003) op cit.
  #
  # Data quality: LOW -- not commercially farmed at scale; wild populations
  # in shallow bays of Korea, Japan, and China. Of conservation interest and
  # subject to localised restoration interest in Korea. Tolerance parameters
  # estimated from natural habitat conditions and the reproductive study of
  # Park et al. (2012). Current, sediment, and wave tolerance are not
  # quantified in the literature and are estimated from habitat analogues.
  # Treat all output for this species with caution.
  #
  # NOTE: This species is included for ecological assessment and potential
  # restoration planning rather than commercial aquaculture siting. It is
  # NOT commercially farmed at meaningful scale in any region.
  # ===========================================================================
  "ostrea_denselamellosa" = list(

    common_name  = "Korean Flat Oyster",
    latin_name   = "Ostrea denselamellosa",
    region       = "Korea, southern Japan, eastern China; shallow coastal bays",
    data_quality = "low",

    exclusions = list(

      temperature = list(
        min = 5,  max = 30,
        unit = "celsius",
        note = "Temperate East Asian coastal species; Gamak Bay (Korea) temperatures 5-28\u00b0C seasonally. Park et al. (2012): spawning June-August at 20-26\u00b0C. Estimated from natural range."
      ),
      salinity = list(
        min_cold  = 20,  max = 36,
        min_warm  = 23,  temp_pivot = 20,
        unit = "psu",
        note = "Coastal marine; limited estuarine tolerance. Gamak Bay: 28-34 PSU typical. Estimated from natural habitat."
      ),
      dissolved_oxygen = list(
        min = 3.0,  optimal_min = 6.0,
        unit = "mg/L",
        note = "Estimated from habitat conditions; no species-specific data."
      )
    ),

    scored = list(

      temperature = list(
        rank = 1,
        type = "seasonal",
        optimal_min = 15,  optimal_max = 25,
        poor_min    = 8,   acceptable_max = 28, absolute_max = 30,
        unit = "celsius",
        note = "Temperate East Asian seasonality. Park et al. (2012): ripe stage June-August; spent August-September in Gamak Bay."
      ),

      shear_stress = list(
        rank = 2,
        type = "threshold_decay",
        optimal_max = 0.25,  hard_max = 0.55,
        unit = "N/m^2",
        note = "Estimated from sheltered coastal bay habitat (Gamak Bay, Ariake Sea)."
      ),

      chlorophyll_a = list(
        rank = 3,
        type = "optimal_range",
        temp_threshold = 8,
        optimal_min = 1.5,  optimal_max = 4.0,
        acceptable_min = 0.3, acceptable_max = 10.0,
        unit = "ug/L",
        note = "Estimated from temperate East Asian coastal productivity."
      ),

      current_velocity = list(
        rank = 3,
        type = "optimal_range",
        min_for_food = 0.05,  optimal_min = 0.08,  optimal_max = 0.25,
        tidal_max    = 0.40,  hard_max    = 0.65,
        unit = "m/s",
        note = "Estimated from sheltered coastal bay habitat."
      ),

      salinity = list(
        rank = 3,
        type = "optimal_range",
        optimal_min = 28,  optimal_max = 34,
        poor_min    = 20,  acceptable_max = 36,
        unit = "psu",
        note = "Marine/coastal preference; estimated from Gamak Bay and Ariake Sea natural populations."
      ),

      substrate_hardness = list(
        rank = 3,
        type = "optimal_range",
        optimal_min = 0.50, optimal_max = 1.00,
        poor_min    = 0.00, poor_max    = 0.20,
        unit = "hardness_index (0-1)",
        note = "Flat oyster; requires hard substrate for spat settlement."
      ),

      depth = list(
        rank = 4,
        type = "optimal_range",
        optimal_min = 0,   optimal_max = 10,
        acceptable_max = 20, absolute_max = 30,
        unit = "metres",
        note = "Shallow coastal; gravelly/hard-bottom subtidal and lower intertidal. Park et al. (2012) study site: 3-8 m."
      ),

      turbidity = list(
        rank = 5,
        type = "threshold_decay",
        optimal_max = 8,  acceptable_max = 20, poor_max = 45,
        unit = "NTU",
        note = "Estimated from coastal Korean bay conditions."
      ),

      roughness = list(
        rank = 5,
        type = "optimal_range",
        optimal_min = 1.05, optimal_max = 1.50,
        acceptable_max = 2.50, poor_max = 5.00,
        unit = "rugosity_index"
      ),

      ph = list(
        rank = 4,
        type = "optimal_range",
        optimal_min = 7.85, optimal_max = 8.3,
        poor_min    = 7.6,  acceptable_max = 8.5, absolute_max = 8.6,
        unit = "pH units",
        note = "Korean coastal waters generally pH 7.9-8.2; subtidal populations in Tongyeong and Geoje-si not typically exposed to severe acidification. Source: estimated from C. gigas analogy."
      ),

      omega_aragonite = list(
        rank = 4,
        type = "optimal_range",
        optimal_min = 1.7,  optimal_max = 3.5,
        poor_min    = 1.0,  acceptable_max = 4.5, absolute_max = 5.5,
        unit = "Omega (dimensionless)",
        note = "Korean coastal subtidal waters generally Omega 2.0-3.5. Estimated from flat oyster analogy. Source: estimated."
      ),

      slope = list(
        rank = 6,
        type = "threshold_decay",
        optimal_max = 6,  acceptable_max = 15, poor_max = 30,
        unit = "degrees"
      ),

      dissolved_oxygen = list(
        rank = 5,
        type = "optimal_range",
        optimal_min = 5.5, optimal_max = 10.0,
        poor_min    = 3.0, acceptable_min = 4.5,
        acceptable_max = 14.0, absolute_max = 20.0,
        unit = "mg/L",
        note = "East Asian coastal flat oyster; tolerates variable DO in semi-enclosed bays. Estimated by analogy."
      )
    ),

    seasonal_overrides = list(
      temperature = list(
        winter = list(
          type = "optimal_range",
          optimal_min = 5,   optimal_max = 12,
          poor_min    = 2,   acceptable_max = 16, absolute_max = 20,
          note = "Korean coastal winters: 4-10\u00b0C. Growth slows markedly; reproductive quiescence (Park et al. 2012)."
        ),
        spring = list(
          type = "optimal_range",
          optimal_min = 12,  optimal_max = 20,
          poor_min    = 8,   acceptable_max = 24, absolute_max = 27,
          note = "Gonad development commences; Park et al. (2012): developing stage April-June."
        ),
        summer = list(
          type = "optimal_range",
          optimal_min = 18,  optimal_max = 26,
          poor_min    = 14,  acceptable_max = 28, absolute_max = 30,
          note = "Spawning peak June-August at 20-26\u00b0C (Park et al. 2012). Post-spawn spent stage September."
        ),
        autumn = list(
          type = "optimal_range",
          optimal_min = 12,  optimal_max = 22,
          poor_min    = 6,   acceptable_max = 26, absolute_max = 28,
          note = "Recovery and condition rebuilding. Partial gametogenesis may continue into October in warmer years."
        )
      )
    )
  ),  # end ostrea_denselamellosa


  # ===========================================================================
  # Saccostrea cucullata -- Hooded / Indo-Pacific Rock Oyster
  #
  # Sources: Angell C.L. (1986) The Biology and Culture of Tropical Oysters.
  #   ICLARM 13. Toba D. (2002) An Overview of Saccostrea cucullata.
  #   J Shellfish Res. Yan H. et al. (2013) Tropical oyster culture in
  #   Asia. Aquaculture Reports.
  # ===========================================================================
  "saccostrea_cucullata" = list(
    common_name  = "Hooded Rock Oyster / Indo-Pacific Rock Oyster",
    latin_name   = "Saccostrea cucullata",
    region       = "Indo-Pacific intertidal: E Africa to Australia, Red Sea, Persian Gulf",
    data_quality = "medium",

    exclusions = list(
      temperature = list(min = 14, max = 36, unit = "celsius",
        note = "Tropical intertidal; tolerates aerial heat to 38 C briefly."),
      temperature_winter = list(min = 12, unit = "celsius"),
      temperature_summer = list(max = 36, unit = "celsius"),
      salinity = list(min_cold = 8, max = 45, min_warm = 12, temp_pivot = 22, unit = "psu",
        note = "Highly euryhaline tropical estuarine species."),
      dissolved_oxygen = list(min = 2.5, optimal_min = 4.5, unit = "mg/L")
    ),

    scored = list(
      temperature = list(rank = 1, type = "seasonal",
        optimal_min = 22, optimal_max = 30,
        poor_min = 16, acceptable_max = 33, absolute_max = 35,
        unit = "celsius"),
      sediment_type = list(rank = 1, type = "categorical",
        scores = c("hard_rock" = 1.00, "bedrock" = 1.00, "mangrove_root" = 1.00,
                   "shell_gravel" = 0.95, "boulder" = 0.90, "cobble" = 0.85,
                   "coarse_gravel" = 0.65, "gravel" = 0.55, "mixed_sediment" = 0.55,
                   "coarse_sand" = 0.30, "medium_sand" = 0.15, "fine_sand" = 0.10,
                   "muddy_sand" = 0.20, "mud" = 0.05, "silt" = 0.05,
                   "unknown" = 0.50),
        unit = "category",
        note = "Intertidal cement attachment; mangrove roots and rock platforms primary."),
      salinity = list(rank = 2, type = "optimal_range",
        optimal_min = 18, optimal_max = 35,
        poor_min = 8, acceptable_max = 42, absolute_max = 45, unit = "psu"),
      tidal_exposure = list(rank = 2, type = "optimal_range",
        optimal_min = 0.30, optimal_max = 0.65,
        acceptable_min = 0.10, acceptable_max = 0.85,
        unit = "fraction_time_aerial",
        note = "Mid-upper intertidal; Saccostrea is one of the most aerial-tolerant oysters."),
      substrate_hardness = list(rank = 2, type = "optimal_range",
        optimal_min = 0.65, optimal_max = 1.00,
        poor_min = 0.00, poor_max = 0.20, unit = "hardness_index (0-1)"),
      chlorophyll_a = list(rank = 3, type = "optimal_range",
        temp_threshold = 14, optimal_min = 1.5, optimal_max = 8.0,
        acceptable_min = 0.3, acceptable_max = 20.0, unit = "ug/L"),
      current_velocity = list(rank = 3, type = "optimal_range",
        min_for_food = 0.05, optimal_min = 0.10, optimal_max = 0.40,
        tidal_max = 0.80, hard_max = 1.50, unit = "m/s"),
      fishing_intensity = list(rank = 2, type = "binary_penalty",
        trawl_depth_max = 5, penalty = 0.55, unit = "logical"),
      ph = list(rank = 4, type = "optimal_range",
        optimal_min = 7.8, optimal_max = 8.3,
        poor_min = 7.5, acceptable_max = 8.5, absolute_max = 8.6,
        unit = "pH units",
        note = "Tropical intertidal species; estimated from M. gigas analogy."),
      omega_aragonite = list(rank = 4, type = "optimal_range",
        optimal_min = 1.6, optimal_max = 3.5,
        poor_min = 0.8, acceptable_max = 4.5, absolute_max = 5.5,
        unit = "Omega (dimensionless)",
        note = "Estimated from M. gigas analogy; no species-specific data available."),

      dissolved_oxygen = list(
        rank = 5,
        type = "optimal_range",
        optimal_min = 4.5, optimal_max = 9.0,
        poor_min    = 2.0, acceptable_min = 3.5,
        acceptable_max = 12.0, absolute_max = 18.0,
        unit = "mg/L",
        note = "Tropical intertidal species; tolerates emersion and variable DO. Estimated."
      )
    ),

    seasonal_overrides = list()
  ),  # end saccostrea_cucullata


  # ===========================================================================
  # Ostrea angasi -- Australian Flat Oyster
  #
  # Sources: Hone P. (1996) Australian flat oyster aquaculture.
  #   SARDI Tech Rep. Crawford C. (2003) Environmental management of
  #   marine aquaculture in Tasmania. Aquaculture 226. Cole V.J. & Hone
  #   P. (2018) Restoration of native flat oyster (Ostrea angasi)
  #   reefs in Australia. Conservation Biology.
  # ===========================================================================
  "ostrea_angasi" = list(
    common_name  = "Australian Flat Oyster",
    latin_name   = "Ostrea angasi",
    region       = "Southern Australia (S Australia, Victoria, Tasmania, southern WA)",
    data_quality = "medium",

    exclusions = list(
      temperature = list(min = 5, max = 26, unit = "celsius"),
      temperature_winter = list(min = 4, unit = "celsius"),
      temperature_summer = list(max = 27, unit = "celsius",
        note = "Heat events >25 C in protected embayments amplify QX disease pressure."),
      salinity = list(min_cold = 25, max = 38, min_warm = 30, temp_pivot = 16, unit = "psu"),
      dissolved_oxygen = list(min = 4.0, optimal_min = 5.5, unit = "mg/L")
    ),

    scored = list(
      temperature = list(rank = 1, type = "seasonal",
        optimal_min = 12, optimal_max = 22,
        poor_min = 6, acceptable_max = 24, absolute_max = 26, unit = "celsius"),
      sediment_type = list(rank = 1, type = "categorical",
        scores = c("shell_gravel" = 1.00, "shell_hash" = 1.00, "hard_rock" = 0.95,
                   "bedrock" = 0.90, "coarse_gravel" = 0.90, "gravel" = 0.85,
                   "cobble" = 0.85, "coarse_sand" = 0.70, "medium_sand" = 0.55,
                   "mixed_sediment" = 0.65, "fine_sand" = 0.30,
                   "muddy_sand" = 0.30, "sandy_mud" = 0.20,
                   "mud" = 0.10, "silt" = 0.05,
                   "unknown" = 0.50),
        unit = "category"),
      depth = list(rank = 2, type = "optimal_range",
        optimal_min = 1, optimal_max = 8,
        acceptable_min = 0, acceptable_max = 20, hard_max = 30, unit = "metres"),
      current_velocity = list(rank = 2, type = "optimal_range",
        min_for_food = 0.05, optimal_min = 0.10, optimal_max = 0.30,
        tidal_max = 0.50, hard_max = 0.90, unit = "m/s"),
      chlorophyll_a = list(rank = 2, type = "optimal_range",
        temp_threshold = 8, optimal_min = 1.5, optimal_max = 4.0,
        acceptable_min = 0.5, acceptable_max = 10.0, unit = "ug/L"),
      substrate_hardness = list(rank = 3, type = "optimal_range",
        optimal_min = 0.50, optimal_max = 1.00,
        poor_min = 0.00, poor_max = 0.20, unit = "hardness_index (0-1)"),
      shear_stress = list(rank = 3, type = "threshold_decay",
        optimal_max = 0.4, hard_max = 0.9, unit = "N/m^2"),
      fishing_intensity = list(rank = 2, type = "binary_penalty",
        trawl_depth_max = 30, penalty = 0.65, unit = "logical"),
      ph = list(rank = 4, type = "optimal_range",
        optimal_min = 7.85, optimal_max = 8.3,
        poor_min = 7.6, acceptable_max = 8.5, absolute_max = 8.6,
        unit = "pH units",
        note = "Southern Australian coastal waters well-buffered; pH 7.9-8.2 typical. Estimated from O. edulis analogy."),
      omega_aragonite = list(rank = 4, type = "optimal_range",
        optimal_min = 1.8, optimal_max = 3.5,
        poor_min = 1.0, acceptable_max = 4.5, absolute_max = 5.5,
        unit = "Omega (dimensionless)",
        note = "Southern Australian shelf waters typically Omega 2.0-3.5. Estimated from O. edulis analogy."),

      salinity = list(
        rank = 5,
        type = "optimal_range",
        optimal_min = 28.0, optimal_max = 36.0,
        poor_min    = 18.0, acceptable_min = 24.0,
        acceptable_max = 38.0, absolute_max = 38.0,
        unit = "PSU",
        note = "Stenohaline southern Australian species; optimal 28-36 PSU. Source: Gribben & Creese (2003)."
      ),

      dissolved_oxygen = list(
        rank = 5,
        type = "optimal_range",
        optimal_min = 6.0, optimal_max = 10.0,
        poor_min    = 3.5, acceptable_min = 5.0,
        acceptable_max = 14.0, absolute_max = 20.0,
        unit = "mg/L",
        note = "Southern Australian subtidal flat oyster; cool well-oxygenated waters. Optimal 6-10 mg/L. Estimated."
      )
    ),

    seasonal_overrides = list(
      temperature = list(
        winter = list(type = "optimal_range",
          optimal_min = 8, optimal_max = 13, poor_min = 4,
          acceptable_max = 16, absolute_max = 18,
          note = "Southern Australia winters; gametogenesis active."),
        summer = list(type = "optimal_range",
          optimal_min = 16, optimal_max = 22, poor_min = 12,
          acceptable_max = 25, absolute_max = 26,
          note = "Spawning peak; heat-event mortality risk in shallow embayments.")
      )
    )
  ),  # end ostrea_angasi


  # ===========================================================================
  # Crassostrea iredalei -- Slipper Oyster
  #
  # Sources: Devakie M.N. & Ali A.B. (2002) Effective use of plastic
  #   sheet as substrate in enhancing tropical oyster (Crassostrea
  #   iredalei) larvae settlement. Aquaculture 212. Aypa S.M. (1990)
  #   Mussel and oyster culture in the Philippines. FAO Fisheries.
  #   Suja N. & Mohamed K.S. (2010) Slipper oyster aquaculture in
  #   estuarine waters. Indian J Fish 57.
  # ===========================================================================
  "crassostrea_iredalei" = list(
    common_name  = "Slipper Oyster",
    latin_name   = "Crassostrea iredalei",
    region       = "SE Asia: Philippines, Malaysia, Indonesia, Thailand, S Vietnam",
    data_quality = "medium",

    exclusions = list(
      temperature = list(min = 18, max = 34, unit = "celsius"),
      temperature_winter = list(min = 18, unit = "celsius"),
      temperature_summer = list(max = 35, unit = "celsius"),
      salinity = list(min_cold = 8, max = 36, min_warm = 12, temp_pivot = 24, unit = "psu",
        note = "Tropical estuarine; tolerates heavy monsoon freshening."),
      dissolved_oxygen = list(min = 2.5, optimal_min = 4.5, unit = "mg/L",
        note = "Hypoxia tolerant; survives diurnal hypoxia common in tropical estuaries.")
    ),

    scored = list(
      temperature = list(rank = 1, type = "seasonal",
        optimal_min = 26, optimal_max = 30,
        poor_min = 22, acceptable_max = 32, absolute_max = 34, unit = "celsius"),
      sediment_type = list(rank = 1, type = "categorical",
        scores = c("mangrove_root" = 1.00, "shell_hash" = 1.00, "hard_rock" = 0.85,
                   "muddy_sand" = 0.80, "mud" = 0.70, "sandy_mud" = 0.85,
                   "shell_gravel" = 0.85, "fine_sand" = 0.55,
                   "medium_sand" = 0.45, "mixed_sediment" = 0.65,
                   "silt" = 0.55, "coarse_sand" = 0.30,
                   "gravel" = 0.40, "coarse_gravel" = 0.25,
                   "bedrock" = 0.55,
                   "unknown" = 0.50),
        unit = "category",
        note = "Mangrove-zone species; settles on roots, hanging culture infrastructure, mud-shell mixes."),
      salinity = list(rank = 2, type = "optimal_range",
        optimal_min = 15, optimal_max = 28,
        poor_min = 8, acceptable_max = 32, absolute_max = 36, unit = "psu",
        note = "Brackish optimum; full-marine reduces growth."),
      chlorophyll_a = list(rank = 2, type = "optimal_range",
        temp_threshold = 18, optimal_min = 4.0, optimal_max = 15.0,
        acceptable_min = 1.0, acceptable_max = 30.0, unit = "ug/L"),
      depth = list(rank = 3, type = "optimal_range",
        optimal_min = 1, optimal_max = 5,
        acceptable_min = 0, acceptable_max = 10, hard_max = 15, unit = "metres"),
      current_velocity = list(rank = 3, type = "optimal_range",
        min_for_food = 0.03, optimal_min = 0.08, optimal_max = 0.30,
        tidal_max = 0.55, hard_max = 0.90, unit = "m/s"),
      substrate_hardness = list(rank = 3, type = "optimal_range",
        optimal_min = 0.20, optimal_max = 1.00,
        poor_min = 0.00, poor_max = 0.10, unit = "hardness_index (0-1)"),
      fishing_intensity = list(rank = 3, type = "binary_penalty",
        trawl_depth_max = 10, penalty = 0.50, unit = "logical"),
      ph = list(rank = 4, type = "optimal_range",
        optimal_min = 7.7, optimal_max = 8.2,
        poor_min = 7.4, acceptable_max = 8.5, absolute_max = 8.6,
        unit = "pH units",
        note = "Brackish/estuarine SE Asian species; broader pH tolerance expected from euryhaline habitat. Estimated."),
      omega_aragonite = list(rank = 4, type = "optimal_range",
        optimal_min = 1.4, optimal_max = 3.5,
        poor_min = 0.7, acceptable_max = 4.5, absolute_max = 5.5,
        unit = "Omega (dimensionless)",
        note = "Estuarine/brackish habitat; river dilution lowers alkalinity and Omega. More tolerant of low Omega than marine species. Estimated."),

      dissolved_oxygen = list(
        rank = 5,
        type = "optimal_range",
        optimal_min = 4.0, optimal_max = 8.0,
        poor_min    = 2.5, acceptable_min = 3.0,
        acceptable_max = 12.0, absolute_max = 18.0,
        unit = "mg/L",
        note = "Brackish water species; low-DO tolerance higher than marine oysters. Estimated from SE Asian literature."
      )
    ),

    seasonal_overrides = list()
  )  # end crassostrea_iredalei

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
