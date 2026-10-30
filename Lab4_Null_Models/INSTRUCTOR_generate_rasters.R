################################################################################
# INSTRUCTOR SCRIPT: Pre-generate rasters for Lab 4 (NLMR workaround)
#
# Run this on YOUR machine (which has NLMR installed) to produce all the
# raster files students need. Distribute the data/ folder to students.
#
# Output structure (put in student Lab 4 folder):
#   data/
#     random_maps/
#       random_p0.1.tif  ... random_p0.9.tif   (9 binary random maps)
#     cluster_maps/
#       cluster_clumped_p0.5.tif
#       cluster_frag_p0.5.tif
#     observed_landscape.tif
#
# Estimated run time: ~2–3 minutes on a modern laptop.
################################################################################

library(NLMR)
library(terra)
# Note: landscapetools is NOT used — util_classify() breaks on SpatRaster.
# We threshold the continuous NLMR output directly with terra instead.

# Create output directories
dir.create("data/random_maps",  showWarnings = FALSE, recursive = TRUE)
dir.create("data/cluster_maps", showWarnings = FALSE, recursive = TRUE)

set.seed(42)   # reproducibility — all students get the same maps

# ── Helper: binary random map (terra-native, no util_classify) ────────────────
# nlm_random() returns a SpatRaster with values in [0, 1].
# Cells with value >= (1 - p_hab) become habitat (1); the rest are non-habitat (0).
# This is mathematically identical to what util_classify() does.
make_binary_random <- function(p_hab, nrow = 100, ncol = 100) {
  r     <- NLMR::nlm_random(nrow = nrow, ncol = ncol, rescale = TRUE)
  r_bin <- terra::ifel(r >= (1 - p_hab), 1L, 0L)
  r_bin
}

# ── 1. Simple random maps at p = 0.1 → 0.9 ────────────────────────────────────
p_vals <- seq(0.1, 0.9, by = 0.1)

message("Generating simple random maps...")
for (p in p_vals) {
  r_bin <- make_binary_random(p)
  fname <- sprintf("data/random_maps/random_p%.1f.tif", p)
  terra::writeRaster(r_bin, fname, overwrite = TRUE, datatype = "INT1U")
  message("  Saved: ", fname)
}

# Quick sanity check after generating the first map:
# test <- terra::rast("data/random_maps/random_p0.5.tif")
# table(values(test))   # should show values 0 and 1 only


# ── 2. Random cluster maps at p_habitat = 0.5 ─────────────────────────────────
# nlm_randomcluster() returns a SpatRaster directly in current NLMR — no
# terra::rast() conversion needed, but wrapping it is harmless if unsure.
message("Generating random cluster maps...")

# Clumped (cluster algorithm p = 0.3 → large, contiguous patches)
rc_clumped <- NLMR::nlm_randomcluster(
  nrow = 100, ncol = 100,
  p    = 0.3,
  ai   = c(0.5, 0.5),
  neighbourhood = 4,
  rescale = FALSE
)
terra::writeRaster(rc_clumped,
                   "data/cluster_maps/cluster_clumped_p0.5.tif",
                   overwrite = TRUE, datatype = "INT1U")
message("  Saved: data/cluster_maps/cluster_clumped_p0.5.tif")

# Fragmented (cluster algorithm p = 0.7 → small, scattered patches)
rc_frag <- NLMR::nlm_randomcluster(
  nrow = 100, ncol = 100,
  p    = 0.7,
  ai   = c(0.5, 0.5),
  neighbourhood = 4,
  rescale = FALSE
)
terra::writeRaster(rc_frag,
                   "data/cluster_maps/cluster_frag_p0.5.tif",
                   overwrite = TRUE, datatype = "INT1U")
message("  Saved: data/cluster_maps/cluster_frag_p0.5.tif")


# ── 3. "Observed" landscape for Part 3 ────────────────────────────────────────
# A strongly autocorrelated map (acts as "real" landscape in the null model test)
message("Generating observed landscape...")
obs_raw <- NLMR::nlm_randomcluster(
  nrow = 100, ncol = 100,
  p    = 0.4,            # produces large, clumped patches
  ai   = c(0.55, 0.45),
  neighbourhood = 4,
  rescale = FALSE
)
terra::writeRaster(obs_raw,
                   "data/observed_landscape.tif",
                   overwrite = TRUE, datatype = "INT1U")
message("  Saved: data/observed_landscape.tif")

plot(obs_raw, col = c("white","forestgreen"),
     main = paste0("Observed Landscape"), legend = FALSE)
# ── 4. Quick verification plots ───────────────────────────────────────────────
message("Creating verification plots...")

par(mfrow = c(3, 3), mar = c(2, 2, 2, 1))
for (p in p_vals) {
  r <- terra::rast(sprintf("data/random_maps/random_p%.1f.tif", p))
  r <- as.factor(r)
  plot(r, col = c("white","forestgreen"),
       main = paste0("Random, p=", p), legend = FALSE)
}
par(mfrow = c(1, 1))

# ── 5. Summary ────────────────────────────────────────────────────────────────
message("\n=== ALL RASTERS GENERATED SUCCESSFULLY ===")
message("Distribute the entire 'data/' folder to students.")
message("\nFiles created:")
message(paste(" ", list.files("data", recursive = TRUE, full.names = TRUE), collapse = "\n"))

# UPDATE: to generate a fragmented landscape that still have p=0.5
# because the settings above result in very few pixels 
# Fragmented cluster map — with verification loop
message("Generating fragmented cluster map (verifying p ≈ 0.5)...")
repeat {
  rc_frag <- NLMR::nlm_randomcluster(
    nrow = 100, ncol = 100,
    p    = 0.45,
    ai   = c(0.5, 0.5),
    neighbourhood = 4,
    rescale = FALSE
  )
  actual_p <- mean(values(rc_frag) == 1, na.rm = TRUE)
  message("  actual p = ", round(actual_p, 3))
  if (abs(actual_p - 0.5) < 0.05) break   # accept if within ±5%
}
terra::writeRaster(rc_frag,
                   "data/cluster_maps/cluster_frag_p0.5.tif",
                   overwrite = TRUE, datatype = "INT1U")
message("  Saved with actual p = ", round(actual_p, 3))