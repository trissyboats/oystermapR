# =============================================================================
# ADCP Data Converter for oystermapR
# Nortek Signature 500 — merged CSV format
#
# Input:  Raw merged ADCP CSV (adcp_time_utc, gps_lat, gps_lon,
#                               VelE_bin1..N, VelN_bin1..N, adcp_heading_deg)
# Output: Spatially averaged CSV ready for predict_oyster()
#
# Key processing decisions
# ------------------------
# 1. ONLY bin1 (nearest to transducer) is used for velocity.
#    Bins 2+ show sidelobe contamination against the seabed in shallow water
#    (confirmed: 75% of bin2 readings exceed 1.5 m/s at Melfort survey).
#
# 2. Velocity magnitude: speed = sqrt(VelE^2 + VelN^2) in m/s.
#
# 3. Shear stress estimated from near-surface bin1:
#    tau = rho * Cd * U^2  (rho=1025 kg/m3, Cd=0.002)
#    NOTE: This is an APPROXIMATION. True bed shear stress requires near-bed
#    velocity which is not recoverable from this dataset due to sidelobe
#    contamination. Treat shear_stress output as a lower-bound estimate.
#
# 4. Spatial averaging at 4 decimal place resolution (~11 m cells at 56°N).
#    All ensembles within a cell are averaged. Cells with fewer than
#    min_obs ensembles are dropped (configurable below).
#
# 5. Date extracted from adcp_time_utc (UTC). One date per output row.
# =============================================================================

library(dplyr)

# ---- Configuration -----------------------------------------------------------
input_file  <- "path/to/your/input.csv"  # <- UPDATE THIS
output_file <- "path/to/output/desired output.csv"         # <- UPDATE THIS

spatial_res  <- 4        # decimal places for lat/lon binning (~11 m at 56°N)
min_obs      <- 5        # minimum ensembles per cell to include in output
spike_thresh <- 1.5      # m/s — bin1 readings above this are treated as spikes
rho          <- 1025     # kg/m3 — seawater density
Cd           <- 0.002    # drag coefficient (dimensionless, typical coastal)


# ---- Load raw data -----------------------------------------------------------
message("Loading ADCP data...")
raw <- read.csv(input_file, stringsAsFactors = FALSE)
message(sprintf("  %d ensembles loaded.", nrow(raw)))


# ---- Compute bin1 speed magnitude --------------------------------------------
raw$speed_bin1 <- sqrt(raw$VelE_bin1^2 + raw$VelN_bin1^2)

# Remove obvious spikes (sidelobe breakthrough into bin1)
n_spikes <- sum(raw$speed_bin1 > spike_thresh, na.rm = TRUE)
if (n_spikes > 0) {
  message(sprintf("  Removing %d bin1 spike(s) (> %.1f m/s)", n_spikes, spike_thresh))
  raw <- raw[raw$speed_bin1 <= spike_thresh, ]
}


# ---- Extract date from UTC timestamp ----------------------------------------
# adcp_time_utc format: "2025-08-05T09:14:46.999Z"
raw$date <- as.Date(substr(raw$adcp_time_utc, 1, 10))


# ---- Spatial binning ---------------------------------------------------------
raw$lat_bin <- round(raw$gps_lat, spatial_res)
raw$lon_bin <- round(raw$gps_lon, spatial_res)

message(sprintf("Spatially averaging into %.0f-decimal-place grid (~%d m cells)...",
                spatial_res, round(10^(-spatial_res) * 111000)))

averaged <- raw %>%
  group_by(lat_bin, lon_bin) %>%
  summarise(
    lat              = mean(gps_lat,    na.rm = TRUE),
    lon              = mean(gps_lon,    na.rm = TRUE),
    date             = as.character(min(date)),          # earliest date in cell
    current_velocity = mean(speed_bin1, na.rm = TRUE),   # m/s (bin1 only)
    current_velocity_sd = sd(speed_bin1, na.rm = TRUE),  # variability diagnostic
    current_velocity_p95 = quantile(speed_bin1, 0.95, na.rm = TRUE),
    shear_stress     = mean(rho * Cd * speed_bin1^2, na.rm = TRUE),  # N/m2 (approx)
    heading_deg      = mean(adcp_heading_deg, na.rm = TRUE),
    n_ensembles      = n(),
    .groups = "drop"
  ) %>%
  filter(n_ensembles >= min_obs) %>%
  rename(lat = lat, lon = lon) %>%
  select(-lat_bin, -lon_bin)

message(sprintf("  %d spatial cells retained (>= %d ensembles each).", nrow(averaged), min_obs))
message(sprintf("  Current velocity: mean=%.3f m/s, p95=%.3f m/s, max=%.3f m/s",
                mean(averaged$current_velocity),
                quantile(averaged$current_velocity, 0.95),
                max(averaged$current_velocity)))
message(sprintf("  Shear stress:     mean=%.4f N/m2 (approx, lower bound)",
                mean(averaged$shear_stress)))


# ---- Add placeholder columns for other oystermapR variables -----------------
# Fill these in from your other sensors (Lowrance BioBase, probe data, etc.)
averaged$temperature        <- NA_real_   # °C        — from probe
averaged$salinity           <- NA_real_   # PSU       — from probe
averaged$dissolved_oxygen   <- NA_real_   # mg/L      — from probe/sampler
averaged$depth              <- NA_real_   # m         — from bathymetric sonar
averaged$slope              <- NA_real_   # degrees   — derived from DEM
averaged$roughness          <- NA_real_   # rugosity  — derived from DEM
averaged$turbidity          <- NA_real_   # NTU       — from ADCP backscatter (future)
averaged$chlorophyll_a      <- NA_real_   # ug/L      — from probe/lab
averaged$substrate_hardness <- NA_real_   # 0-1 index — from Lowrance BioBase
averaged$sediment_type      <- NA_character_  # category  — from BioBase/sidescan
averaged$benthic_communities<- NA_character_  # category  — from BioBase
averaged$biotope            <- NA_character_  # category  — from sidescan
averaged$fishing_intensity  <- FALSE          # logical   — field observation


# ---- Write output ------------------------------------------------------------
write.csv(averaged, output_file, row.names = FALSE)

message(sprintf("\nDone. Converted file written to: %s", output_file))
message("Next: fill in the NA columns from your other sensor datasets,")
message("then run predict_oyster() on the completed CSV.")
message("")
message("Columns ready for oystermapR:")
message("  current_velocity, shear_stress, lat, lon, date")
message("Columns still needed from other sensors:")
message("  temperature, salinity, dissolved_oxygen, depth, slope, roughness,")
message("  substrate_hardness, sediment_type, benthic_communities, biotope")
