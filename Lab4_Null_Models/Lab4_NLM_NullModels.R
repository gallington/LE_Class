################################################################################
# Lab 4: Neutral Landscape Models as Null Models
# Two-session landscape ecology lab
#
# Session 1 (Tues):
#   Part 1 – Generate and visualize neutral landscape models (NLMs)
#   Part 2 – Explore how p and spatial autocorrelation drive landscape metrics
#
# Session 2 (Thurs):
#   Part 3 – Use NLMs as null models: randomization test for significance
#   Part 4 – Apply to a real landscape (take-home extension)
#
# Prerequisites: landscapemetrics (Lab 3), tidyverse, terra
# New packages: NLMR, landscapetools
#
# R version: 4.x+
# Authors: adapted from LLE Ch. 6–7 (Gardner; Remmel & Fortin) and nlm_lab.R
################################################################################

##### FOR NEXT TIME!!! MAKE SURE EVERYONE HAS RTools installed/updated in order to install NLMR!!!

# ── Package installation (run once) ──────────────────────────────────────────
# install.packages("remotes")
# remotes::install_github("ropensci/NLMR")          # NLM generation
# install.packages("landscapetools")                 # visualization helpers
# install.packages("landscapemetrics")               # you used this in Lab 3
# install.packages("tidyverse")
# install.packages("terra")


# ── Load libraries ────────────────────────────────────────────────────────────
library(NLMR)             # neutral landscape model generators
library(landscapetools)   # show_landscape(), util_classify()
library(landscapemetrics) # lsm_* functions (same as Lab 3)
library(tidyverse)
library(terra)

# ── Project setup ─────────────────────────────────────────────────────────────
# Use an RProject file for Lab 4 (same approach as Lab 3)
getwd()   # confirm working directory


################################################################################
##                                                                            ##
##  SESSION 1 – DAY 1                                                         ##
##                                                                            ##
################################################################################

# ══════════════════════════════════════════════════════════════════════════════
#  PART 1: Generating Neutral Landscape Models
#  Concepts: what is a NLM? random vs. autocorrelated maps; the p parameter
# ══════════════════════════════════════════════════════════════════════════════

# ── 1a. Simple random map ─────────────────────────────────────────────────────
# Each cell gets a uniformly random value [0, 1]; no spatial structure.
# This is the simplest neutral model — a null baseline with NO autocorrelation.

rmap <- NLMR::nlm_random(nrow = 100,
                         ncol = 100,
                         resolution = 1,
                         rescale = TRUE)   # values rescaled to [0,1]

show_landscape(rmap)   # continuous heat map
plot(rmap)
#


# Classify into binary habitat / non-habitat at p = 0.5
 # using the 'util_classify()' command.
# (weighting = proportion of cells designated as "habitat")
rmap_bin <- util_classify(rmap,
                          weighting = c(0.5, 0.5),          # p = 0.5
                          level_names = c("Non-habitat", "Habitat"))
show_landscape(rmap_bin, discrete = TRUE)

#    Does this look like any real landscape you know?
#   Why or why not? What process would produce a pattern like this?


# ── 1b. Vary p: how habitat amount alone changes spatial pattern ──────────────
# We'll generate random maps at three values of p and compare them visually.

# this code here is generating a function so that you don't have to copy and 
# paste this three times to generate maps at diff values of p.
# this function combines the commands we used above:
    # 'nlm_random()' and 'util_classify()' into one call 
make_binary_random <- function(p_hab, nrow = 100, ncol = 100) {
  r <- NLMR::nlm_random(nrow = nrow, ncol = ncol, rescale = TRUE)
  util_classify(r,
                weighting = c(1 - p_hab, p_hab),
                level_names = c("Non-habitat", "Habitat"))
}

# here we will run the function we created above for three diff values of p
  # you could change the values in the parentheses below to see what you would get at diff values.
maps_p <- list(
  "p = 0.2" = make_binary_random(0.2),  # change these if you want to see diff values
  "p = 0.5" = make_binary_random(0.5),  # or add more lines to get more maps
  "p = 0.8" = make_binary_random(0.8)
)

show_landscape(maps_p)   # side-by-side panel

#  Q1: At which value of p does habitat appear to first "connect"
#   across the map? This threshold is called the percolation threshold (p_crit).
#   For a simple random map with 4-neighbor rule, theory predicts p_crit ≈ 0.593.


# ── 1c. Spatially autocorrelated maps (random cluster algorithm) ──────────────
# Real landscapes are NOT random — similar land covers cluster together.
# The random cluster algorithm adds spatial autocorrelation via the parameter p
# THIS IS VERY CONFUSING, BC THIS IS A ***DIFFERENT*** p than we used above.
# here, p= the prop of elements randomly selected to form clusters; 
# higher p → more even coverage, less fragmented.
# Don't blame me. I did not develop this package. 

# FIRST: open the randomcluster_explainer.html file in your folder

rcmap_clumped <- NLMR::nlm_randomcluster(
  nrow          = 100,
  ncol          = 100,
  p             = 0.3,          # lower p → more clumped
# this next line is where you specify the proportions of the diff classes
  ai            = c(0.5, 0.5),  # this means equal proportions of two classes, but you could make it something like (0.35, 0.15, 0.5) or whatever.
  neighbourhood = 4,            # neighborhood rule!!!
  rescale       = FALSE
)
show_landscape(rcmap_clumped, discrete = TRUE)

#### PLEASE play around with different values of p, going up and down at increments
      # to see how it changes the configuration
      # then you can try to adjust ai 

rcmap_frag <- NLMR::nlm_randomcluster(
  nrow          = 100,
  ncol          = 100,
  p             = 0.7,          # higher p → more fragmented
  ai            = c(0.5, 0.5),
  neighbourhood = 4,
  #rescale       = FALSE
)
show_landscape(rcmap_frag, discrete = TRUE)

# do you notice that this looks different than what you generated in the widget?
# that's because nlm_randomcluster uses a slightly different algorithm
# open the nlm_randomcluster_explainer.html file to see 


# Q2: Compare the clumped and fragmented random-cluster maps to the simple 
# random maps at the same habitat proportion. What is the key visual difference? 
#Which parameter in nlm_randomcluster() controls the degree of clumping, 
# and what does it do?


# ── 1d. Fractal / midpoint displacement NLM ───────────────────────────────────
# The midpoint displacement algorithm produces a gradient surface with
# user-controlled roughness (H parameter; 0 = rough/random, 1 = smooth).
# After classification it looks more like natural landscapes.

# FIRST: open the .html file in your folder to access the fractal widget

# Then generate some yourself here:
frac_smooth <- NLMR::nlm_mpd(nrow = 100, ncol = 100,
                              roughness = 0.2,   # low roughness → smooth
                              rescale   = TRUE)
show_landscape(frac_smooth)

frac_rough <- NLMR::nlm_mpd(nrow = 100, ncol = 100,
                             roughness = 0.8,    # high roughness → patchy
                             rescale   = TRUE)
show_landscape(frac_rough)

# Classify the smooth fractal landscape into 3 cover types
frac_class <- util_classify(frac_smooth,
                             weighting    = c(0.3, 0.4, 0.3),
                             level_names  = c("Type A", "Type B", "Type C"))
show_landscape(frac_class, discrete = TRUE)

# Play around with diff settings for roughness and weighting (basic proportions of cover)

# Q3: How does roughness in the fractal NLM relate to the concept
#   of spatial autocorrelation? 

# Q4. Given what you observed in Parts 1a–1d, which NLM type would you choose 
# as a null model for a question about whether topographic gradients drive 
# land-cover pattern? 


# ══════════════════════════════════════════════════════════════════════════════
#  PART 2: NLMs × Landscape Metrics  
#  Key idea: metrics vary predictably with p and autocorrelation structure
# ══════════════════════════════════════════════════════════════════════════════

# We'll sweep p from 0.1 to 0.9 across 9 random maps and calculate the same
# metrics we used in Lab 3 (NP, MPS, CONTAG, SHEI).
# This takes ~1-2 min

p_vals   <- seq(0.1, 0.9, by = 0.1)
results  <- vector("list", length(p_vals))
# this for loop is subdivided below with comments to help
  # you understand what is going on. But you need to select the 
  # entire this to Run it! 
  # select from here down to the closing squiggle bracket
for (i in seq_along(p_vals)) {
  p_i <- p_vals[i]
  
  # generate binary random map and convert to terra SpatRaster
  m <- make_binary_random(p_i)
  r <- terra::rast(m)   # NLMR returns a RasterLayer; convert for landscapemetrics
  r <- as.factor(r)

  # calculate landscape-level metrics
  np    <- lsm_l_np(r,    directions = 4)$value
  mps   <- lsm_l_area_mn(r, directions = 4)$value
  contag <- tryCatch(lsm_l_contag(r)$value, error = function(e) NA)
  shei  <- tryCatch(lsm_l_shei(r)$value,  error = function(e) NA)
  
  results[[i]] <- tibble(p = p_i, NP = np, MPS = mps,
                         CONTAG = contag, SHEI = shei)
  message("Finished p = ", p_i)
}  # select all the way down to here to execute this code chunk.

metrics_df <- bind_rows(results)
metrics_df   # have a look

# Visualize: facet plot of each metric vs. p
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

# Q5: Which metric shows the sharpest change near p_crit?
#   Why would a sudden change that matter for a species that depends
#   on connected habitat?

# Save results for reference
write_csv(metrics_df, "outputs/part2_metrics_vs_p.csv")

################## STOP HERE ON DAY 1 ########################
##############################################################


################################################################################
##                                                                            ##
##  SESSION 2 – DAY 2                                                         ##
##                                                                            ##
################################################################################

# ══════════════════════════════════════════════════════════════════════════════
#  PART 3: NLMs as Null Models — Testing for Significant Landscape Differences
#  Core concept from LLE Ch. 7 (Remmel & Fortin):
#    One landscape = one replicate → can't use standard parametric tests.
#    Solution: generate many NLMs that share properties with the observed
#    landscape (same p, same size), compute their metric distributions, then
#    ask: does the observed value fall in the tails?
# ══════════════════════════════════════════════════════════════════════════════

# ── 3a. Build a simulated "observed" landscape ────────────────────────────────
# In a real study this would be your actual landscape raster.
# Here we use a spatially autocorrelated NLM to represent an observed landscape,
# so we can see it differ from random expectation.

set.seed(42)   # reproducibility
obs_raw <- NLMR::nlm_randomcluster(nrow = 100, ncol = 100,
                                    p    = 0.4,
                                    ai   = c(0.55, 0.45),
                                    neighbourhood = 4,
                                    rescale = FALSE)
#obs_r <- as.factor(terra::rast(obs_raw))
show_landscape(obs_r, discrete = TRUE)

# Extract the proportion of habitat (p_obs) from this landscape
# (we need this to parameterize the null model)
freq_tab    <- table(values(obs_r))
p_obs       <- as.numeric(freq_tab["1"]) / sum(freq_tab)   # class 1 = Habitat
cat("Observed habitat proportion (p_obs):", round(p_obs, 3), "\n")

# Metric of interest: Contagion (a measure of clumping / spatial structure)
obs_contag <- lsm_l_contag(obs_r)$value
# Q7 Part 1.
cat("Observed CONTAG:", round(obs_contag, 3), "\n")


# ── 3b. Generate the null distribution ───────────────────────────────────────
# We create n_iter SIMPLE RANDOM maps with the SAME p as the observed landscape.
# These are the null — pattern arising from habitat amount alone, no process.
# If the observed metric falls outside the 95% null envelope, the pattern is
# significantly different from random expectation.

n_iter    <- 99    # 99 + 1 observed = 100 total; gives clean α = 0.05 cutoffs
null_contag <- numeric(n_iter)

# you don't have to understand everything that is happening here, but before
# you Run this code chunk, look through it to see where the landscape metric for
# contagion is being calulated....
message("Running null model simulations (", n_iter, " iterations)...")
for (i in seq_len(n_iter)) {
  null_map <- make_binary_random(p_obs, nrow = 100, ncol = 100) # NOTE: BINARY random
  null_r   <- as.factor(terra::rast(null_map))
  null_contag[i] <- tryCatch(lsm_l_contag(null_r)$value, error = function(e) NA)
  if (i %% 10 == 0) message("  Iteration ", i, " / ", n_iter)
}

# ── 3c. Compare observed to null distribution ─────────────────────────────────
null_df <- tibble(contag = null_contag)
# check out what you just generated-- this gives you a preview
head(null_df)

# Q7 part 2:
# 95% confidence envelope from null
ci_lo <- quantile(null_contag, 0.025, na.rm = TRUE)
ci_hi <- quantile(null_contag, 0.975, na.rm = TRUE)

cat("\nNull distribution summary:\n")
cat("  Mean CONTAG:", round(mean(null_contag, na.rm = TRUE), 3), "\n")
cat("  95% CI: [", round(ci_lo, 3), ",", round(ci_hi, 3), "]\n")
cat("  Observed CONTAG:", round(obs_contag, 3), "\n")
cat("  Significantly different from random?",
    ifelse(obs_contag < ci_lo | obs_contag > ci_hi, "YES", "NO"), "\n")

# ── 3d. Visualize the test ────────────────────────────────────────────────────
# first just a histogram of the distribution of values from the null maps
ggplot(null_df, aes(x = contag)) +
  geom_histogram(fill = "steelblue", color = "white", bins = 25)

# now let's add the 'observed' landscape. the axis has to change to fit it on!
# so the null distibution gets squished into one bin!
ggplot(null_df, aes(x = contag)) +
  geom_histogram(fill = "steelblue", color = "white", bins = 25) +
  geom_vline(xintercept = obs_contag, color = "red",
             linetype = "solid", linewidth = 1.2) +
  geom_vline(xintercept = c(ci_lo, ci_hi), color = "black",
             linetype = "dashed", linewidth = 0.9) +
  annotate("text", x = obs_contag, y = Inf,
           label = " Observed", color = "red", hjust = 0, vjust = 1.5) +
  annotate("text", x = ci_lo,     y = Inf,
           label = "2.5%", color = "black", hjust = 1.1, vjust = 1.5, size = 3) +
  annotate("text", x = ci_hi,     y = Inf,
           label = "97.5%", color = "black", hjust = -0.1, vjust = 1.5, size = 3) +
  labs(title    = "Randomization Test: Is observed contagion significantly non-random?",
       subtitle  = paste0("n = ", n_iter, " simple random null maps | p_obs = ",
                           round(p_obs, 3)),
       x = "Contagion (CONTAG, %)", y = "Count") +
  theme_bw()

ggsave("outputs/part3_null_model_test.jpg", width = 7, height = 4)

# DISCUSSION : What does it mean ecologically if the observed landscape has
#   significantly HIGHER contagion than the null distribution?
#   What process might explain that?

# Q8: What would change if you used a fractal NLM as the null
#   instead of a simple random map? When would that be more appropriate?


# ── 3e. Extend to multiple metrics ────────────────────────────────────────────
# A real analysis would test several metrics simultaneously.
# Here we add Number of Patches (NP) and Mean Patch Size (MPS).

obs_np  <- lsm_l_np(obs_r,    directions = 4)$value
obs_mps <- lsm_l_area_mn(obs_r, directions = 4)$value

null_np  <- numeric(n_iter)
null_mps <- numeric(n_iter)

message("Re-using null maps for NP and MPS (re-running simulations)...")
for (i in seq_len(n_iter)) {
  null_map   <- make_binary_random(p_obs, nrow = 100, ncol = 100)
  null_r     <- as.factor(terra::rast(null_map))
  null_np[i]  <- tryCatch(lsm_l_np(null_r, directions = 4)$value,    error = function(e) NA)
  null_mps[i] <- tryCatch(lsm_l_area_mn(null_r, directions = 4)$value, error = function(e) NA)
}

# Summary table
multi_results <- tibble(
  Metric   = c("CONTAG", "NP", "MPS"),
  Observed = c(obs_contag, obs_np, obs_mps),
  Null_Mean = c(mean(null_contag, na.rm=TRUE),
                mean(null_np,    na.rm=TRUE),
                mean(null_mps,   na.rm=TRUE)),
  CI_lo    = c(quantile(null_contag, 0.025, na.rm=TRUE),
               quantile(null_np,    0.025, na.rm=TRUE),
               quantile(null_mps,   0.025, na.rm=TRUE)),
  CI_hi    = c(quantile(null_contag, 0.975, na.rm=TRUE),
               quantile(null_np,    0.975, na.rm=TRUE),
               quantile(null_mps,   0.975, na.rm=TRUE))
) %>%
  mutate(Significant = Observed < CI_lo | Observed > CI_hi)

print(multi_results)
write_csv(multi_results, "outputs/part3_multi_metric_null_test.csv")

# DISCUSSION: Do all three metrics reach the same conclusion?
#   What would it mean if some metrics are significant and others are not?
#   (Hint: think back to the metric correlation analysis from Lab 3.)


################################################################################
##                                                                            ##
##  PART 4: TAKE-HOME – Apply to a Real Landscape  (LLE Ch. 7, Exercise 4)   ##
##                                                                            ##
##  Instructions:                                                              ##
##  Load one of the NE landscapes from Lab 3 (e.g., ne1.tif) as your         ##
##  "observed" landscape. Adapt the code from Part 3 to:                     ##
##    (a) Estimate p_obs from the real raster for your focal cover class.    ##
##    (b) Run n_iter = 99 simple random null maps with the same p.            ##
##    (c) Test whether CONTAG and at least one other metric of your choice   ##
##        differ significantly from the null.                                 ##
##    (d) Write 3–4 sentences interpreting your results ecologically:         ##
##        What does it tell you about the landscape-forming process?          ##
##                                                                            ##
##  Bonus: swap in nlm_randomcluster (autocorrelated null) and repeat.        ##
##    Does the conclusion change? Why or why not?                             ##
################################################################################

# ── Starter code for take-home ──────────────────────────────────────
# (Fill in the blanks indicated by !!!)

# real.R <- terra::rast("data/ne1.tif")   # !!! load your landscape
# real.R <- as.factor(real.R)
#
# # !!! estimate p for your focal class (e.g., class value 41 = Deciduous Forest)
# freq_real  <- freq(real.R)
# focal_class <- 41   # !!! change this to your class of interest
# p_real <- freq_real$count[freq_real$value == focal_class] / ncell(real.R)
# cat("p_real:", round(p_real, 3))
#
# obs_contag_real <- lsm_l_contag(real.R)$value
#
# # !!! set nrow/ncol to match real.R dimensions
# you can check it out with 
# dim(real.R) # and then use the actual numbers below w/ "n_rows <- [the actual #]"
# or just automate it 
# this way: 
# n_rows <- nrow(real.R) # this just calcs the number automatically and assigns it
# n_cols <- ncol(real.R)
#
# null_contag_real <- numeric(99)
# for (i in 1:99) {
#   nm <- make_binary_random(p_real, nrow = n_rows, ncol = n_cols)
#   nr <- as.factor(terra::rast(nm))
#   null_contag_real[i] <- tryCatch(lsm_l_contag(nr)$value, error = function(e) NA)
# }
#
# # !!! adapt the visualization and significance test from Part 3c–3d here


################################################################################
#  END OF SCRIPT
################################################################################
