################################################################################
# Lab 4 TAKE TWO!: Neutral Landscape Models as Null Models (REVISED — no NLMR required)
# 
#   Part 1 – Explore NLMs using interactive widgets (no R code needed)
#   Part 2 – Load pre-generated NLM rasters; analyze metrics vs. p
#   Part 3 – Use NLMs as null models: randomization test for significance
#   Part 4 – Apply to a real landscape (take-home extension)
#
# Prerequisites: landscapemetrics (Lab 3) 
# NOTE: NLMR is NOT required for today's lab.
#       Pre-generated rasters have been provided in your data/ folder.
#
# R version: 4.x+  [if things aren't working, check your version of R!]
################################################################################

# ── Package installation (run once) ──────────────────────────────────────────
# you shouldn't need to install these again, but here's the code just in case:
# install.packages("landscapemetrics")
# install.packages("tidyverse")
# install.packages("terra")

# ── Load libraries ─────────────────────────────────────────────────────────
# These are the ONLY packages you need today:
library(landscapemetrics)
library(tidyverse)
library(terra)

# ── Project setup ─────────────────────────────────────────────────────────────
getwd()   # confirm your working directory is the Lab 4 folder

# Create an outputs folder if it doesn't exist
if (!dir.exists("outputs")) dir.create("outputs")


# ══════════════════════════════════════════════════════════════════════════════
#  PART 1: Understanding Neutral Landscape Models (Concept Exploration)
#  NO R CODE NEEDED — Open the HTML widget files 
# ══════════════════════════════════════════════════════════════════════════════

# Open this files from your Lab 4 folder in a web browser:
#   → nlm_explorer.html       (main widget: all 3 NLM types with sliders)
#   or you can pull up the individual ones from Tuesday:
#   → nlm_randomcluster_explainer.html (this one lets you go through the algorithm step by step)
#   → nlm_mpd _explainer.html (this runs the midpoint displacement algo step by step)
#   → rfractal_nlm_parameter_explorer.html (this explores the relationship between habitat prop and H (roughness))
#
# Work through the three tabs in the widget:
#
# TAB 1 — Simple Random Maps
#   Move the "habitat proportion (p)" slider and observe the binary map.
#   Q1: At which value of p does habitat appear to first "connect" across
#       the map? This is the percolation threshold (p_crit). Theory predicts
#       p_crit ≈ 0.593 for a 4-neighbor simple random map.
#
# TAB 2 — Random Cluster Maps
#   This algorithm adds SPATIAL AUTOCORRELATION via a clustering parameter.
#   NOTE: The clustering parameter here is confusingly also called "p" in the
#   original NLMR package — but it is a DIFFERENT p than habitat proportion.
#   Here it controls what fraction of cells "seed" new clusters.
#   Adjust clustering and habitat proportion independently.
#   Q2: Compare a clumped vs. fragmented random-cluster map at the same
#       habitat proportion. What is the key visual difference?
#
# TAB 3 — Fractal / Midpoint Displacement Maps
#   The "roughness" parameter (H) controls spatial autocorrelation.
#   Low roughness → smooth gradients. High roughness → patchy, random-looking.
#   Q3: How does roughness relate to the concept of spatial autocorrelation?
#
#   Q4: Which of these three NLM types would you choose as a null model for a question about
#       whether topographic gradients drive land-cover pattern? Why?


# ══════════════════════════════════════════════════════════════════════════════
#  PART 2: NLMs × Landscape Metrics
#  Load pre-generated rasters; calculate metrics across p values
# ══════════════════════════════════════════════════════════════════════════════

# ── 2a. Load and examine pre-generated rasters ────────────────────────────────
# I pre-generated binary random maps at p = 0.1 to 0.9.
# These are the SAME maps that NLMR::nlm_random() + util_classify() produces.
# Files are in: data/random_maps/

# Load all nine binary random maps into a list
p_vals <- seq(0.1, 0.9, by = 0.1)

# this code generates random maps at each step of p=0.1-0.9 and puts them
# all into one object as a list:
random_maps <- lapply(p_vals, function(p) {
  fname <- sprintf("data/random_maps/random_p%.1f.tif", p)
  r <- terra::rast(fname)
  as.factor(r)
})
names(random_maps) <- paste0("p=", p_vals)

# Quick check: plot the maps
par(mfrow = c(3, 3), mar = c(2, 2, 2, 1))
for (i in seq_along(random_maps)) {
  plot(random_maps[[i]], main = names(random_maps)[i],
       col = c("white", "forestgreen"), legend = FALSE)
}
par(mfrow = c(1, 1))

# You can also load individual maps and inspect them:
# Just remember the rasters are all in one object so you have to specify which one you want:
r_mid <- random_maps[["p=0.5"]]    # just change the "p=0.5" if you want to pull up a diff one individually
plot(r_mid, col = c("white", "forestgreen"),
     main = "Binary random map, p = 0.5")


# ── 2b. Calculate landscape metrics across all the values of p ──────────────────────────────────
# this is generating metrics for our RANDOMLY generated landscapes
# We'll calculate the same metrics as in Lab 3: 
    # NP = number of patches
    # MPS = ??    <- look up the function in the help to get the name & insert it here.
    # CONTAG = ??  <- look up the fucntion & insert name here
    # SHEI =  ??     <- look up the function & insert name here

# First let's do it just for one of them, so we get the basics:
  # I'm calling this np_p01 because i'm calculating the Number of Patches in the 
  # prop habitat = 0.1 raster that I generated above
np_p01 <- lsm_l_np(random_maps[["p=0.1"]], directions = 4 )
# then examine the output:
np_p01   # should give you a tibble with the metric value

# But for efficiency's sake we can calculate them all at once in a loop.
results <- vector("list", length(p_vals))

for (i in seq_along(p_vals)) {
  p_i <- p_vals[i]
  r   <- random_maps[[i]]

  np     <- lsm_l_np(r,    directions = 4)$value
  mps    <- lsm_l_area_mn(r, directions = 4)$value
  contag <- tryCatch(lsm_l_contag(r)$value, error = function(e) NA)
  shei   <- tryCatch(lsm_l_shei(r)$value,  error = function(e) NA)

  results[[i]] <- tibble(p = p_i, NP = np, MPS = mps,
                         CONTAG = contag, SHEI = shei)
  message("Finished p = ", p_i)
}

metrics_df <- bind_rows(results)
metrics_df   # inspect

# Q: Do you notice any patterns in how these metrics change across the range of p?


# ── 2c. Visualize metrics vs. p ───────────────────────────────────────────────
metrics_long <- metrics_df %>%
  pivot_longer(cols = c(NP, MPS, CONTAG, SHEI),
               names_to = "metric", values_to = "value")

ggplot(metrics_long, aes(x = p, y = value)) +
  geom_line(color = "darkgreen", linewidth = 1) +
  geom_point(color = "darkgreen", size = 2) +
  facet_wrap(~ metric, scales = "free_y") +
  geom_vline(xintercept = 0.5928, linetype = "dashed", color = "red") +
  labs(title    = "Landscape Metrics across p values (simple random maps)",
       subtitle = "Red dashed line = theoretical percolation threshold (p_crit ≈ 0.593)",
       x = "p (proportion habitat)", y = "Metric value") +
  theme_bw()

ggsave("outputs/part2_metrics_vs_p.jpg", width = 8, height = 6)

# Q5: Which metric shows the sharpest change near p_crit?
#     Why would a sudden change matter for a species that depends
#     on connected habitat?


# ── 2d. Compare other NLM types at the same p ───────────────────────────────────────
#  Remember, we don't always want to use a simple random NLM!
#  I have also provided autocorrelated (random cluster) maps.
#  Load one clumped and one fragmented map at p_habitat = 0.5.

r_random   <- random_maps[["p=0.5"]]                  # simple random
r_clumped  <- as.factor(terra::rast("data/cluster_maps/cluster_clumped_p0.5.tif"))
r_fragmented <- as.factor(terra::rast("data/cluster_maps/cluster_frag_p0.5.tif"))

# Plot all three side by side
par(mfrow = c(1, 3), mar = c(2, 2, 3, 1))
plot(r_random,     col = c("white","forestgreen"), main = "Random (p=0.5)",     legend=FALSE)
plot(r_clumped,    col = c("white","forestgreen"), main = "Clustered (low p_c)", legend=FALSE)
plot(r_fragmented, col = c("white","forestgreen"), main = "Fragmented (high p_c)", legend=FALSE)
par(mfrow = c(1, 1))

# Verify that all three maps actually do have p ≈ 0.5
# (You may find that the clustered maps aren't exactly 0.5 — why might that be?)
p_check <- function(r) {
  ft <- table(values(r))
  round(as.numeric(ft["1"]) / sum(ft), 3)
}
cat("Actual p — Random:    ", p_check(r_random), "\n")
cat("Actual p — Clumped:   ", p_check(r_clumped), "\n")
cat("Actual p — Fragmented:", p_check(r_fragmented), "\n")


# Calculate CONTAG for each and compare
contag_random <- lsm_l_contag(r_random)$value
contag_clumped <- lsm_l_contag(r_clumped)$value
contag_frag   <- lsm_l_contag(r_fragmented)$value

cat("CONTAG — Random:",    round(contag_random, 2), "\n")
cat("CONTAG — Clumped:",   round(contag_clumped, 2), "\n")
cat("CONTAG — Fragmented:", round(contag_frag, 2), "\n")

# Q6: All three maps have were generated with the same habitat proportion (p = 0.5).
#     CONTAG differs among them — why? 
#     

write_csv(metrics_df, "outputs/part2_metrics_vs_p.csv")




################################################################################
##                                                                            ##
##  ...this is where we were going to start today...                          ##
##                                                                            ##
################################################################################

# ══════════════════════════════════════════════════════════════════════════════
#  PART 3: NLMs as Null Models — Testing for Significant Landscape Structure
#  Core concept from LLE Ch. 7 (Remmel & Fortin):
#    One landscape = one replicate → can't use standard parametric tests.
#    Solution: generate many NLMs that share properties with the observed
#    landscape (same p, same size); compute their metric distributions, then
#    ask: does the observed value fall in the tails?
# ══════════════════════════════════════════════════════════════════════════════

# ── 3a. Load the "observed" landscape ─────────────────────────────────────────
# In a real study this would be your actual landscape raster.
# Here we will use a pre-generated spatially autocorrelated landscape to represent
# the observed landscape, so we can see how it differs from random expectation.

obs_raw <- terra::rast("data/observed_landscape.tif")
obs_r   <- as.factor(obs_raw)

plot(obs_r, col = c("white", "forestgreen"),
     main = "Observed (autocorrelated) landscape")

# Extract the proportion of 'observed' habitat (p_obs) — needed to parameterize null models
freq_tab <- table(values(obs_r))
 # we'll put the proportion value into this object called : p_obs
p_obs    <- as.numeric(freq_tab["1"]) / sum(freq_tab)
cat("Observed habitat proportion (p_obs):", round(p_obs, 3), "\n")

# Metric of interest: Contagion (measures clumping / spatial structure)
obs_contag <- lsm_l_contag(obs_r)$value
cat("Observed CONTAG:", round(obs_contag, 3), "\n")

# Q7 Part 1: Before running the null model, predict whether you think
#   the observed CONTAG will be HIGHER or LOWER than random expectation.
#   Explain your reasoning based on what you saw in the widget (Part 1).


# ── 3b. Helper function for null maps ─────────────────────────────────────────
# This function generates a binary random map with a given habitat proportion.
# It uses terra directly — no NLMR needed!
# Under the hood this is exactly what NLMR::nlm_random + util_classify() does.

make_binary_random <- function(p_hab, nrow = 100, ncol = 100) {
  # Generate a matrix of uniform random values [0,1]
  m <- matrix(runif(nrow * ncol), nrow = nrow, ncol = ncol)
  # Threshold at (1 - p_hab): cells above threshold = habitat (class 1)
  m_bin <- ifelse(m >= (1 - p_hab), 1, 0)
  # Convert to terra SpatRaster and return as factor
  r <- terra::rast(m_bin)
  as.factor(r)
}

# Test: make sure it works and gives roughly the right p
test_r <- make_binary_random(p_obs)
freq_test <- table(values(test_r))
cat("Test map habitat proportion:", round(as.numeric(freq_test["1"]) / sum(freq_test), 3), "\n")
# (Should be close to p_obs — won't be exact due to randomness)


# ── 3c. Generate the null distribution ───────────────────────────────────────
# We create n_iter SIMPLE RANDOM maps with the SAME p as the observed landscape.
# These are the null — pattern arising from habitat amount alone, no process.
# If the observed metric falls outside the 95% null envelope, the pattern
# differs significantly from random expectation.

n_iter      <- 99   # 99 + 1 observed = 100 total; gives α = 0.05 cutoffs
null_contag <- numeric(n_iter)

message("Running null model simulations (", n_iter, " iterations)...")
for (i in seq_len(n_iter)) {
  null_map        <- make_binary_random(p_obs, nrow = 100, ncol = 100)
  null_contag[i]  <- tryCatch(lsm_l_contag(null_map)$value, error = function(e) NA)
  if (i %% 10 == 0) message("  Iteration ", i, " / ", n_iter)
}


# ── 3d. Compare observed to null distribution ─────────────────────────────────
null_df <- tibble(contag = null_contag)
head(null_df)

# 95% confidence envelope from null
ci_lo <- quantile(null_contag, 0.025, na.rm = TRUE)
ci_hi <- quantile(null_contag, 0.975, na.rm = TRUE)

cat("\nNull distribution summary:\n")
cat("  Mean CONTAG:", round(mean(null_contag, na.rm = TRUE), 3), "\n")
cat("  95% CI: [", round(ci_lo, 3), ",", round(ci_hi, 3), "]\n")
cat("  Observed CONTAG:", round(obs_contag, 3), "\n")
cat("  Significantly different from random?",
    ifelse(obs_contag < ci_lo | obs_contag > ci_hi, "YES", "NO"), "\n")


# ── 3e. Visualize the test ────────────────────────────────────────────────────
# Null distribution only:
ggplot(null_df, aes(x = contag)) +
  geom_histogram(fill = "steelblue", color = "white", bins = 25) +
  labs(title = "Null distribution of CONTAG",
       x = "Contagion (CONTAG, %)", y = "Count") +
  theme_bw()

# Add observed value and CI lines:
ggplot(null_df, aes(x = contag)) +
  geom_histogram(fill = "steelblue", color = "white", bins = 25) +
  geom_vline(xintercept = obs_contag, color = "red",
             linetype = "solid", linewidth = 1.2) +
  geom_vline(xintercept = c(ci_lo, ci_hi), color = "black",
             linetype = "dashed", linewidth = 0.9) +
  annotate("text", x = obs_contag, y = Inf,
           label = " Observed", color = "red", hjust = 0, vjust = 1.5) +
  annotate("text", x = ci_lo, y = Inf,
           label = "2.5%",  color = "black", hjust = 1.1, vjust = 1.5, size = 3) +
  annotate("text", x = ci_hi, y = Inf,
           label = "97.5%", color = "black", hjust = -0.1, vjust = 1.5, size = 3) +
  labs(title    = "Randomization Test: Is observed CONTAG significantly non-random?",
       subtitle  = paste0("n = ", n_iter, " simple random null maps | p_obs = ",
                           round(p_obs, 3)),
       x = "Contagion (CONTAG, %)", y = "Count") +
  theme_bw()

ggsave("outputs/part3_null_model_test.jpg", width = 7, height = 4)

# Q7 Part 2: Was your prediction correct? What does it mean ecologically
#   if the observed landscape has significantly HIGHER contagion than random?
#   What ecological process might explain that pattern?

# Q8: What would change if you used a spatially autocorrelated (random cluster)
#   NLM as the null instead of a simple random map? When would that be more
#   appropriate?


# ── 3f. Extend to multiple metrics ────────────────────────────────────────────
  # observed number of patches
obs_np  <- lsm_l_np(obs_r,    directions = 4)$value
  # observed mean patch size
obs_mps <- lsm_l_area_mn(obs_r, directions = 4)$value

null_np  <- numeric(n_iter)
null_mps <- numeric(n_iter)

message("Running null simulations for NP and MPS...")
for (i in seq_len(n_iter)) {
  null_map   <- make_binary_random(p_obs, nrow = 100, ncol = 100)
  null_np[i]  <- tryCatch(lsm_l_np(null_map, directions = 4)$value,    error = function(e) NA)
  null_mps[i] <- tryCatch(lsm_l_area_mn(null_map, directions = 4)$value, error = function(e) NA)
}

# Summary table
multi_results <- tibble(
  Metric    = c("CONTAG", "NP", "MPS"),
  Observed  = c(obs_contag, obs_np, obs_mps),
  Null_Mean = c(mean(null_contag, na.rm = TRUE),
                mean(null_np,    na.rm = TRUE),
                mean(null_mps,   na.rm = TRUE)),
  CI_lo     = c(quantile(null_contag, 0.025, na.rm = TRUE),
                quantile(null_np,    0.025, na.rm = TRUE),
                quantile(null_mps,   0.025, na.rm = TRUE)),
  CI_hi     = c(quantile(null_contag, 0.975, na.rm = TRUE),
                quantile(null_np,    0.975, na.rm = TRUE),
                quantile(null_mps,   0.975, na.rm = TRUE))
) %>%
  mutate(Significant = Observed < CI_lo | Observed > CI_hi)

print(multi_results)
write_csv(multi_results, "outputs/part3_multi_metric_null_test.csv")

# DISCUSSION: Do all three metrics reach the same conclusion?
#   What would it mean if some metrics are significant and others are not?
#   (Hint: think back to metric correlation analysis from Lab 3.)

# THIS IS NOW ALL YOU NEED TO TURN IN:
################################################################################
##                                                                            ##
##  PART 4: TAKE-HOME – Apply to a Real Landscape  (LLE Ch. 7, Exercise 4)   ##
##                                                                            ##
##  Instructions:                                                              ##
##  Load one of the NE landscapes from Lab 3 (e.g., ne1.tif) as your         ##
##  "observed" landscape. Adapt the code from Part 3 to:                     ##
##    (a) Estimate p_obs from the real raster for your focal cover class.    ##
##    (b) Run n_iter = 99 simple random null maps with make_binary_random().  ##
##    (c) Test whether CONTAG and at least one other metric of your choice   ##
##        differ significantly from the null.                                 ##
##    (d) Write 3–4 sentences interpreting your results ecologically:         ##
##        What does it tell you about the landscape-forming process?          ##
##                                                                            ##
##  Bonus: generate an autocorrelated null map manually (see below) and       ##
##    repeat. Does the conclusion change? Why or why not?                     ##
################################################################################

# ── Starter code for take-home ──────────────────────────────────────
# (Fill in the blanks indicated by !!!)

# real_r <- terra::rast("data/ne1.tif")   # !!! load your landscape
# real_r <- as.factor(real_r)
#
# freq_real   <- freq(real_r)
# focal_class <- 41   # !!! change to your class of interest (e.g., 41 = Deciduous Forest)
# p_real      <- freq_real$count[freq_real$value == focal_class] / ncell(real_r)
# cat("p_real:", round(p_real, 3))
#
# obs_contag_real <- lsm_l_contag(real_r)$value
#
# n_rows <- nrow(real_r)
# n_cols <- ncol(real_r)
#
# null_contag_real <- numeric(99)
# for (i in 1:99) {
#   nm <- make_binary_random(p_real, nrow = n_rows, ncol = n_cols)
#   null_contag_real[i] <- tryCatch(lsm_l_contag(nm)$value, error = function(e) NA)
# }
#
# # !!! adapt the visualization and significance test from Part 3d–3e here

################################################################################
#  END OF SCRIPT
################################################################################
