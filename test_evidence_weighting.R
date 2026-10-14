# =============================================================================
# test_evidence_weighting.R -- smoke test for oystermapR 1.6.0 evidence mode
#
# Run from the oystermapR package directory:
#   Rscript test_evidence_weighting.R
#
# Checks:
#   1. Default weighting is unchanged (rank == 1.5.0 behaviour)
#   2. weighting = "evidence" errors clearly without reliability
#   3. Evidence mode returns coverage, weights, confidence, comparison columns
#   4. A zero-spread layer receives zero weight; weights sum to 1
#   5. "Insufficient data" cells are kept out of the suitable area
#   6. Exports (GeoTIFF bands, QML) still write
# =============================================================================

if (!requireNamespace("devtools", quietly = TRUE)) {
  install.packages("devtools")
}
suppressMessages(devtools::load_all(".", quiet = TRUE))

pass <- 0L; fail <- 0L
check <- function(label, ok) {
  if (isTRUE(ok)) { pass <<- pass + 1L; cat(sprintf("  PASS  %s\n", label)) }
  else            { fail <<- fail + 1L; cat(sprintf("  FAIL  %s\n", label)) }
}

survey <- utils::read.csv("sample_galway_bay_realistic.csv", stringsAsFactors = FALSE)
# Every 10th cell keeps the run to seconds; the weighting logic is
# per-cell, so nothing here depends on the full 40k grid.
survey <- survey[seq(1, nrow(survey), by = 10), ]
cat(sprintf("Survey: %d rows, %d columns\n\n", nrow(survey), ncol(survey)))

# Per-layer reliability (0-1, from replicate coverage). Illustrative values --
# replace with reliability_from_replicates() output on real repeat surveys.
rel <- c(temperature = 0.95, salinity = 0.90, dissolved_oxygen = 0.80,
         current_velocity = 0.55, chlorophyll_a = 0.60, turbidity = 0.50,
         depth = 0.93, fishing_intensity = 0.70, omega_aragonite = 0.65)

# ---- 1. Rank mode is the default and is unchanged ---------------------------
cat("1. Rank mode (default)\n")
r_default <- predict_oyster(survey, "ostrea_edulis", verbose = FALSE)
r_rank    <- predict_oyster(survey, "ostrea_edulis", weighting = "rank",
                            verbose = FALSE)
check("default == weighting='rank'",
      isTRUE(all.equal(r_default$suitability, r_rank$suitability)))
check("no evidence attributes in rank mode",
      is.null(attr(r_rank, "evidence_coverage")))
check("no 'Insufficient data' in rank mode",
      !"Insufficient data" %in% as.character(r_rank$suitability_class))

# ---- 2. Evidence mode needs reliability -------------------------------------
cat("\n2. Evidence mode without reliability\n")
err <- tryCatch(predict_oyster(survey, "ostrea_edulis", weighting = "evidence",
                               verbose = FALSE),
                error = function(e) conditionMessage(e))
check("errors when reliability is missing", is.character(err))
if (is.character(err)) cat("       message:", substr(err, 1, 100), "\n")

# ---- 3. Evidence mode outputs ------------------------------------------------
cat("\n3. Evidence mode outputs\n")
r_ev <- predict_oyster(survey, "ostrea_edulis", weighting = "evidence",
                       reliability = rel, verbose = FALSE)
cov <- attr(r_ev, "evidence_coverage")
ew  <- attr(r_ev, "evidence_weights")
check("evidence_coverage attached", !is.null(cov))
check("evidence_weights attached",  !is.null(ew))
check("confidence column present",  "confidence" %in% names(r_ev))
check("suitability_rank kept for comparison",
      all(c("suitability_rank", "suitability_class_rank") %in% names(r_ev)))
check("suitability within [0, 1]",
      all(r_ev$suitability >= 0 & r_ev$suitability <= 1, na.rm = TRUE))
check("rank-mode score reproduced in suitability_rank",
      isTRUE(all.equal(r_ev$suitability_rank, r_rank$suitability)))

cat("\n   Coverage:\n"); print(cov)
cat("\n   Weight table:\n"); cat(weight_table_md(r_ev), sep = "\n")

# ---- 4. Weight properties ----------------------------------------------------
cat("\n4. Weight properties\n")
wt <- ew$table
check("weights sum to 1", abs(sum(wt$weight) - 1) < 1e-6)
check("no layer without reliability carries weight",
      all(wt$weight[!is.finite(wt$reliability)] == 0))

# Constant layer => zero spread => zero weight
flat <- survey; flat$salinity <- 33
r_flat <- predict_oyster(flat, "ostrea_edulis", weighting = "evidence",
                         reliability = rel, verbose = FALSE)
wf <- attr(r_flat, "evidence_weights")$table
check("constant layer (salinity) gets ~0 weight",
      wf$weight[wf$layer == "salinity"] < 1e-6)

# ---- 5. Insufficient data stays out of suitable area -------------------------
cat("\n5. Insufficient-data handling\n")
# Blank two layers over the first half of the cells: those cells lose part of
# the evidence base, so confidence drops below a strict floor.
patchy <- survey
half   <- seq_len(nrow(patchy) %/% 2)
patchy$current_velocity[half] <- NA
patchy$chlorophyll_a[half]    <- NA
patchy$turbidity[half]        <- NA
r_patchy <- predict_oyster(patchy, "ostrea_edulis", weighting = "evidence",
                           reliability = rel, confidence_floor = 0.9,
                           verbose = FALSE)
n_insuff <- sum(r_patchy$suitability_class == "Insufficient data", na.rm = TRUE)
check("cells missing layers decline to classify", n_insuff > 0)
check("complete cells are still classified",
      any(r_patchy$suitability_class %in% c("High", "Moderate", "Low")))
cat(sprintf("       %d of %d cells declined to classify\n", n_insuff, nrow(r_patchy)))

a_p <- suppressMessages(area_summary(r_patchy))
a_r <- suppressMessages(area_summary(r_rank))
cs  <- a_p$class_summary
hm  <- sum(cs$area_m2[cs$class %in% c("High", "Moderate")])
check("suitable area is High + Moderate only",
      isTRUE(all.equal(unname(a_p$total["suitable_area_m2"]), hm)))
check("'Insufficient data' counted in surveyed area, not suitable area",
      cs$area_m2[cs$class == "Insufficient data"] > 0 &&
      a_p$total["surveyed_area_m2"] > a_p$total["suitable_area_m2"])
cat(sprintf("       suitable area: rank %.0f ha, evidence (patchy) %.0f ha\n",
            a_r$total["suitable_area_ha"], a_p$total["suitable_area_ha"]))

# ---- 6. Exports --------------------------------------------------------------
cat("\n6. Exports\n")
tif <- tempfile(fileext = ".tif")
ok_tif <- tryCatch({
  # Coarse grid, no contours: this checks band layout, not interpolation.
  export_geotiff(r_patchy, tif, resolution = 0.002, contours = FALSE)
  file.exists(tif)
}, error = function(e) { cat("       ", conditionMessage(e), "\n"); FALSE })
check("export_geotiff writes", ok_tif)
if (ok_tif) {
  check("GeoTIFF has 7 bands in evidence mode", terra::nlyr(terra::rast(tif)) == 7)
  ok_qml <- tryCatch({ export_qml_style(tif); TRUE },
                     error = function(e) { cat("       ", conditionMessage(e), "\n"); FALSE })
  check("export_qml_style writes both style files", ok_qml &&
        file.exists(sub("\\.tif$", ".qml", tif)) &&
        file.exists(sub("\\.tif$", "_insufficient.qml", tif)))
}

# ---- 7. Reliability measured from overlapping survey lines -------------------
cat("\n7. layer_reproducibility() -> evidence weighting\n")
set.seed(42)
lineA <- survey; lineA$line_id <- "A"
lineB <- survey; lineB$line_id <- "B"
# Line B re-measures the same ground: depth and salinity reproduce, turbidity
# is pure noise on each pass.
lineB$depth_m   <- lineB$depth_m   + rnorm(nrow(lineB), 0, 0.05)
lineB$salinity  <- lineB$salinity  + rnorm(nrow(lineB), 0, 0.10)
lineA$turbidity <- rnorm(nrow(lineA), mean(survey$turbidity), sd(survey$turbidity))
lineB$turbidity <- rnorm(nrow(lineB), mean(survey$turbidity), sd(survey$turbidity))
two_lines <- rbind(lineA, lineB)

repro <- layer_reproducibility(two_lines, cell_size_m = 30)
print(repro)
check("reproducible layers score high",
      repro$reliability[["depth"]] > 0.9 && repro$reliability[["salinity"]] > 0.9)
check("noise layer scores near zero", abs(repro$reliability[["turbidity"]]) < 0.3)

rel_measured <- repro$reliability[is.finite(repro$reliability)]
r_meas <- predict_oyster(survey, "ostrea_edulis", weighting = "evidence",
                         reliability = rel_measured, verbose = FALSE)
wm <- attr(r_meas, "evidence_weights")$table
check("measured reliability flows into the weight table",
      isTRUE(all.equal(wm$reliability[wm$layer == "depth"],
                       unname(rel_measured[["depth"]]))))
check("noise layer is demoted below reproducible ones",
      wm$weight[wm$layer == "turbidity"] < wm$weight[wm$layer == "salinity"])

# ---- Rank vs evidence comparison --------------------------------------------
cat("\n\nClass share (%), rank vs evidence:\n")
lv <- c("High", "Moderate", "Low", "Very Low", "Excluded", "Insufficient data")
share <- function(x) round(100 * prop.table(table(factor(x, levels = lv))), 1)
print(rbind(rank = share(r_rank$suitability_class),
            evidence = share(r_ev$suitability_class)))

cat(sprintf("\n%d passed, %d failed\n", pass, fail))
if (fail > 0) quit(status = 1)
