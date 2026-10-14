# =============================================================================
# lineage_walkthrough.R -- tolerance profiles for related lineages, and the
# inverse maps, on data you supply.
#
#   Rscript lineage_walkthrough.R
#
# YOU SUPPLY TWO THINGS
#
#   1. occurrence.csv  one row per sampling location, with presence AND absence:
#        lat, lon, presence (0/1), lineage_id, temperature, salinity, ...
#      lineage_id must come from the genetics alone (haplotype group, cluster,
#      population). If you define lineages by where you sampled them, the
#      inverse map will just recover your sampling.
#
#   2. distances.csv   pairwise genetic distance between lineages (Fst,
#      p-distance, patristic distance from a tree -- any dissimilarity):
#        first column = lineage id, then one column per lineage, same order.
#
# This script writes simulated versions of both, so it runs as-is. Swap the two
# read.csv() paths for your own files.
# =============================================================================

suppressMessages(devtools::load_all(".", quiet = TRUE))
species <- "ostrea_edulis"

# ---- simulated stand-in data (delete this block when you have your own) -----
set.seed(11)
tp <- get_species_tolerances(species)$scored$temperature
n_each <- sample(c(rep(150, 7), rep(14, 3), 0, 0))       # 12 lineages, some thin, some unsampled
x   <- sort(runif(12)); ids <- paste0("pop", 1:12)
D   <- as.matrix(dist(x)); dimnames(D) <- list(ids, ids)
omin <- 10 + 5 * x + rnorm(12, 0, 0.2)                    # relatives have similar tolerances
recs <- do.call(rbind, lapply(seq_along(ids), function(i) {
  n <- n_each[i]; if (n == 0) return(NULL)
  t <- runif(n, 6, 24)
  S <- vapply(t, function(v) if (v < tp$poor_min || v > tp$absolute_max) 0 else
    if (v >= omin[i] && v <= omin[i] + 6) 1 else if (v < omin[i]) (v - tp$poor_min) / (omin[i] - tp$poor_min) else
      (tp$absolute_max - v) / (tp$absolute_max - omin[i] - 6), numeric(1))
  data.frame(lat = 51 + runif(n) / 10, lon = -4 + runif(n) / 10, presence = rbinom(n, 1, plogis(-5 + 10 * S)),
             lineage_id = ids[i], temperature = t)
}))
write.csv(recs, "occurrence.csv", row.names = FALSE)
write.csv(cbind(lineage_id = ids, as.data.frame(D)), "distances.csv", row.names = FALSE)
# -----------------------------------------------------------------------------

occurrence <- read.csv("occurrence.csv")
distances  <- as_lineage_distances("distances.csv")      # checks symmetry, ids, etc.
cat("Records per lineage:\n"); print(table(occurrence$lineage_id))

# ---- 1. Forward: one profile per lineage --------------------------------------
# Pooling happens only if tolerance is shown to track relatedness (Mantel test).
fits <- fit_lineages(occurrence, distances, species, update_vars = "temperature", seed = 1)

cat("\nProfiles (optimal_min), with how each was obtained:\n")
print(head(fits$profiles[fits$profiles$parameter == "optimal_min",
                         c("lineage", "n_records", "source", "mean", "sd")], 12), digits = 3)
#   source: own = fitted alone | pooled = own data + prior from relatives
#           borrowed = relatives only (no/too few records) | default = species default

# ---- 2. Inverse, ecological: which lineage occurs here? -----------------------
grid <- data.frame(lat = 51, lon = seq(-4, -3.9, length.out = 7),
                   temperature = c(4, 8, 11, 14, 17, 22, 30))
occ <- lineage_occurrence(fits, grid, ambiguity_threshold = 0.2)
cat("\nMost likely lineage per location:\n")
print(occ[, c("temperature", "lineage_best", "lineage_prob", "lineage_ambiguity", "lineage_class")], digits = 2)
#   "Ambiguous" = the environment does not tell the lineages apart.
#   "Excluded"  = outside a hard limit for the species.

# ---- 3. Inverse, management: which source population for this site? -----------
sites <- data.frame(site_id = c("Bay A", "Bay B"), lat = 51, lon = -4,
                    temperature = c(11, 15), native = c("pop3", "pop9"))
src <- rank_source_populations(fits, sites, local_lineage = "native",
                               distance_penalty = 2, n_draws = 50, seed = 1)
cat("\nTop 3 source populations per site (discounted by genetic distance from the native one):\n")
print(do.call(rbind, lapply(split(src, src$site_id), head, 3))[, c("site_id", "rank", "lineage", "p_occurrence",
      "distance_to_local", "score", "p_top", "source", "evidence")], digits = 2, row.names = FALSE)
#   p_top: share of posterior draws in which the lineage is within 0.02 of the best.
#   evidence "low": the profile came from relatives alone -- treat as a hypothesis.
#   distance_penalty is a management choice: 0 ignores genetic distance entirely.

unlink(c("occurrence.csv", "distances.csv"))
