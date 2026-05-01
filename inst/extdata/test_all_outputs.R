# =============================================================================
# oystermapR — copy-paste demo / smoke test
# Tests every major output using the bundled sample_survey.csv
#
# After installing the package:
#   devtools::install("~/Documents/Claude/Projects/oystermapR")
#   library(oystermapR)
#   source(system.file("extdata", "test_all_outputs.R", package = "oystermapR"))
# =============================================================================

library(oystermapR)

out <- file.path(path.expand("~"), "Desktop", "oystermapR_test")
dir.create(out, recursive = TRUE, showWarnings = FALSE)
cat("Outputs going to:", out, "\n\n")

csv <- system.file("extdata", "sample_survey.csv", package = "oystermapR")
df  <- read.csv(csv)

# ── 1. Basic prediction ───────────────────────────────────────────────────────
result <- predict_oyster(df, species = "ostrea_edulis", verbose = TRUE)
print(result[, c("lat", "lon", "suitability", "suitability_class")])
table(result$suitability_class)

# ── 2. GeoTIFF + QML style ────────────────────────────────────────────────────
tif <- file.path(out, "edulis_suitability.tif")
qml <- file.path(out, "edulis_suitability.qml")
predict_oyster(df, "ostrea_edulis", output_geotiff = tif, verbose = FALSE)
cat("GeoTIFF written:", file.exists(tif), "\n")
cat("QML written:    ", file.exists(qml), "\n")
# In QGIS: Layer > Add Raster Layer → .tif, then drag .qml onto the layer

# ── 3. All five species ───────────────────────────────────────────────────────
for (sp in c("ostrea_edulis", "magallana_gigas", "crassostrea_angulata",
             "ostrea_stentina", "ostrea_lurida")) {
  r <- predict_oyster(df, sp, verbose = FALSE)
  cat(sp, "— High:", sum(r$suitability_class == "High"),
      " Moderate:", sum(r$suitability_class == "Moderate"), "\n")
}

# ── 4. Species comparison ─────────────────────────────────────────────────────
comparison <- compare_species(
  data    = df,
  species = c("ostrea_edulis", "magallana_gigas", "ostrea_lurida"),
  verbose = TRUE
)
print(comparison[, c("lat", "lon", "best_species", "best_suitability")])

# ── 5. Seasonal composite ─────────────────────────────────────────────────────
df_summer <- df[as.Date(df$date) > as.Date("2024-03-01"), ]
df_winter <- df[as.Date(df$date) < as.Date("2024-03-01"), ]
if (nrow(df_winter) < 2) df_winter <- df_summer
r_summer  <- predict_oyster(df_summer, "ostrea_edulis", verbose = FALSE)
r_winter  <- predict_oyster(df_winter, "ostrea_edulis", verbose = FALSE)
composite <- composite_seasonal(
  surveys = list(summer = r_summer, winter = r_winter),
  method  = "mean",   # "min" gives all-NA when any season is fully excluded
  verbose = TRUE
)
print(composite[, c("lat", "lon", "suitability_composite", "suitability_class")])

# ── 6. Validation ─────────────────────────────────────────────────────────────
records <- data.frame(
  lat         = result$lat,
  lon         = result$lon,
  presence    = as.integer(result$suitability >= 0.5),
  temperature = result$temperature,
  salinity    = result$salinity,
  depth       = result$depth
)
val <- validate_against_records(result, records, presence_col = "presence",
                                plot = TRUE, verbose = TRUE)
cat("AUC:", val$auc, " TSS:", val$tss, " F1:", val$f1, "\n")

# ── 7. Spatial block cross-validation ────────────────────────────────────────
cv <- spatial_block_cv(result, records, presence_col = "presence",
                       n_blocks = 3, verbose = TRUE)
cat("mean AUC:", cv$mean_auc, " mean TSS:", cv$mean_tss, "\n")
print(cv$fold_results)

# ── 8. Permutation variable importance ───────────────────────────────────────
imp <- permutation_importance(result, records, presence_col = "presence",
                              n_perm = 20, verbose = TRUE)
print(imp)

# ── 9. Sensitivity analysis (temperature response curve) ─────────────────────
sens <- sensitivity_analysis(result, "ostrea_edulis", "temperature",
                             n_steps = 30, verbose = TRUE)
print(sens)

# ── 10. Wave exposure ─────────────────────────────────────────────────────────
result_wave <- score_wave_exposure(result, fetch_km = 2, wind_speed_ms = 8,
                                   verbose = TRUE)
print(result_wave[, c("lat", "lon", "wave_hs_m", "wave_exposure_class")])

# ── 11. Sediment stability ────────────────────────────────────────────────────
result_sed <- score_sediment_stability(result, verbose = TRUE)
cat("New cols:", paste(setdiff(names(result_sed), names(result)), collapse = ", "), "\n")

# ── 12. Larval connectivity ───────────────────────────────────────────────────
result_larv <- score_larval_connectivity(result, species = "ostrea_edulis",
                                         verbose = TRUE)
cat("New cols:", paste(setdiff(names(result_larv), names(result)), collapse = ", "), "\n")

# ── 13. Predation risk ────────────────────────────────────────────────────────
result_pred <- score_predation_risk(result, verbose = TRUE)
cat("New cols:", paste(setdiff(names(result_pred), names(result)), collapse = ", "), "\n")

# ── 14. HAB risk (offline) ────────────────────────────────────────────────────
result_hab <- score_hab_risk(result, fetch_live = FALSE, verbose = TRUE)
cat("New cols:", paste(setdiff(names(result_hab), names(result)), collapse = ", "), "\n")

# ── 15. Anthropogenic disturbance (offline) ───────────────────────────────────
result_anth <- score_anthropogenic_disturbance(result, fetch_live = FALSE,
                                               verbose = TRUE)
cat("New cols:", paste(setdiff(names(result_anth), names(result)), collapse = ", "), "\n")

# ── 16. Shellfish classification ──────────────────────────────────────────────
result_class <- add_shellfish_classification(result)
cat("New cols:", paste(setdiff(names(result_class), names(result)), collapse = ", "), "\n")

# ── 17. Bayesian tolerance update ─────────────────────────────────────────────
# Sample data only has 18 rows; lower min_records to test
fit <- update_species_tolerances(
  records     = records,
  species     = "ostrea_edulis",
  update_vars = c("temperature", "salinity"),
  min_records = 10,
  verbose     = TRUE
)
result_updated <- predict_oyster(df, "ostrea_edulis", verbose = FALSE)
cat("Mean suitability shift after Bayesian update:",
    mean(result_updated$suitability - result$suitability, na.rm = TRUE), "\n")

# ── 18. HTML report ───────────────────────────────────────────────────────────
report_path <- file.path(out, "oystermapR_report.html")
generate_report(result, species = "ostrea_edulis",
                output = report_path, open = FALSE)
cat("Report:", report_path, "\n")

# ── 19. Print summary PDF (no LaTeX required) ─────────────────────────────────
pdf_path <- file.path(out, "edulis_print_summary.pdf")
generate_summary_pdf(
  result  = result,
  output  = pdf_path,
  species = "ostrea_edulis",
  title   = "Kames Bay — Oyster Suitability Summary",
  author  = "T. Tucker",
  open    = FALSE,
  verbose = TRUE
)
cat("Summary PDF:", file.exists(pdf_path), "\n")

# ── Done ──────────────────────────────────────────────────────────────────────
cat("\nFiles on Desktop/oystermapR_test:\n")
for (f in list.files(out)) cat(" ", f, "\n")
