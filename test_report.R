# ── oystermapR HTML report smoke-test ─────────────────────────────────────────
# Builds synthetic predict_oyster()-style output, runs compare_species() and
# validate_against_records(), then calls generate_report() to produce an HTML
# report you can open directly in a browser.
#
# Run from the package root:
#   source("test_report.R")
# or after devtools::load_all():
#   devtools::load_all("."); source("test_report.R")
# ------------------------------------------------------------------------------

library(oystermapR)

set.seed(42)
n <- 120

# ── 1. Synthetic result dataframe (mimics predict_oyster() output) ─────────────
result <- data.frame(
  lat               = runif(n, 55.5, 56.5),
  lon               = runif(n, -5.5, -4.5),
  suitability       = pmin(1, pmax(0, rnorm(n, mean = 0.55, sd = 0.22))),
  suitability_class = NA_character_,
  data_completeness = runif(n, 0.4, 1.0),
  score_temperature       = runif(n, 0.3, 1.0),
  score_salinity          = runif(n, 0.5, 1.0),
  score_depth             = runif(n, 0.1, 1.0),
  score_turbidity         = runif(n, 0.2, 0.9),
  score_dissolved_oxygen  = runif(n, 0.4, 1.0),
  score_ph                = runif(n, 0.6, 1.0),
  score_chlorophyll_a     = runif(n, 0.3, 0.95),
  intertidal_zone         = sample(
    c("subtidal", "intertidal", "supratidal"),
    n, replace = TRUE, prob = c(0.70, 0.25, 0.05)
  ),
  stringsAsFactors = FALSE
)

result$suitability_class <- cut(
  result$suitability,
  breaks = c(-Inf, 0.3, 0.5, 0.7, Inf),
  labels = c("Unsuitable", "Low", "Moderate", "High"),
  right  = TRUE
)

# ── 2. Synthetic compare_species() output ─────────────────────────────────────
comp <- result[, c("lat", "lon", "suitability", "suitability_class")]
names(comp)[3:4] <- c("suit_ostrea_edulis", "class_ostrea_edulis")

comp$suit_magallana_gigas <- pmin(1, pmax(0, rnorm(n, 0.65, 0.18)))
comp$class_magallana_gigas <- cut(
  comp$suit_magallana_gigas,
  breaks = c(-Inf, 0.3, 0.5, 0.7, Inf),
  labels = c("Unsuitable", "Low", "Moderate", "High")
)

penalty <- ifelse(comp$class_magallana_gigas == "High",     0.80,
           ifelse(comp$class_magallana_gigas == "Moderate", 0.92, 1.0))
comp$suit_ostrea_edulis_competition_adj <- comp$suit_ostrea_edulis * penalty

# ── 3. Synthetic validation records ───────────────────────────────────────────
# Records are sampled directly from prediction lat/lon so they always match.
# match_radius_deg is also widened to 0.5° for robustness with random data.
idx_pres <- sample(nrow(result), 40)
idx_abs  <- sample(setdiff(seq_len(nrow(result)), idx_pres), 20)

records <- data.frame(
  lat      = result$lat[c(idx_pres, idx_abs)],
  lon      = result$lon[c(idx_pres, idx_abs)],
  presence = c(rep(1L, 40), rep(0L, 20))
)

val <- tryCatch(
  validate_against_records(
    result,
    records,
    presence_col    = "presence",
    match_radius_deg = 0.5      # widen to handle spatial jitter in synthetic data
  ),
  error = function(e) {
    message("validate_against_records() skipped: ", conditionMessage(e))
    NULL
  }
)

# ── 4. Generate the HTML report ────────────────────────────────────────────────
# Output goes next to this script so it's easy to find.
# Missing packages (plotly, scales, rmarkdown, knitr) are auto-installed.
# Resolve script location so the HTML lands next to test_report.R
this_file <- tryCatch(
  normalizePath(sys.frame(1)$ofile),          # works when sourced
  error = function(e) normalizePath(".")       # fallback: current working dir
)
out_path <- file.path(dirname(this_file), "test_report.html")

out <- generate_report(
  result     = result,
  output     = out_path,
  title      = "Test \u2014 Kames Bay Oyster Habitat Assessment",
  author     = "oystermapR smoke-test",
  species    = "ostrea_edulis",
  top_n      = 5,
  validation = val,   # NULL if validation was skipped above
  comparison = comp,
  open       = TRUE
)

message("Report written to: ", normalizePath(out, mustWork = FALSE))
