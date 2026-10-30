# Title:  MarkovLab_student_script.R
# Landscape Ecology – Lab 6
# Based on LLE Ch. 8: Projecting Landscape Change with Markov Models
#
# OVERVIEW:
# In this lab you will learn how to use Markov chain models to project future
# landscape composition based on observed patterns of past change. The core idea
# is straightforward: if we know the rate at which the landscape transitions
# between cover types (e.g., old-growth forest → clear-cut), we can project
# those transitions forward in time and ask: what will this landscape look like
# in 50, 200, or 1000 years?
#
# We use a Pacific Northwest conifer forest dataset (1972, 1984, 1991) that
# captures a period of intense timber harvesting and early-seral regrowth.
# The six cover classes range from Non-forest/Clear-cut through Old Growth.
#
# WHAT YOU WILL DO (step by step):
#   PART 1  – Load and visualize the classified landscape maps
#   PART 2  – Characterize landscape composition through time (observed data)
#   PART 3  – Hand-calculation exercise: understand the transition tally
#   PART 4  – Build the transition probability matrix in R and adjust time steps
#   PART 5  – Run and verify the baseline Markov projection
#   PART 6  – Scenario analysis: what happens if harvest rates change?
#
# FILES YOU NEED (should be in your working directory or a sub-folder):
#   images/pnw72.gif, images/pnw84.gif, images/pnw91.gif   (classified rasters)
#   sample200.csv      (200 random sample points, classified for each year)
#   rel.R              (helper function: relativize a matrix by row)
#   fix_tm.R           (helper function: convert multi-year matrix to annual steps)
#
# I suggest you make a copy of this script so you will have the original
# after you make changes to the scenario analysis section.
#
# ============================================================================


# Load packages ---------------------------------------------------------------
library(terra)       # spatial raster operations (replaces raster)
library(tidyverse)   # data wrangling & plotting utilities


# ============================================================================
##  PART 1 – Explore the Landscapes
# ============================================================================

# Set your working directory to wherever you stored the lab data files.
# If you set up an .RProj file (recommended!), this should already be set.
getwd()   # confirm your working directory

# Load the three classified raster maps
pnw72 <- terra::rast("images/pnw72.gif")
pnw84 <- terra::rast("images/pnw84.gif")
pnw91 <- terra::rast("images/pnw91.gif")

# Create a class legend so we always know which number = which cover type
class.legend <- c("Non-forest", "ClearCut", "EarlySeral", "MidSeral", "Mature", "OldGrowth")
pnw.classes  <- as.data.frame(cbind(0:5, class.legend))
names(pnw.classes) <- c("ClassNum", "ClassName")
pnw.classes   # look at it

# ---- Plot the 1972 landscape -----------------------------------------------
plot(pnw72,
     main   = "1972",
     col    = terrain.colors(n = 6, rev = TRUE),
     legend = TRUE,
     )
legend("topright",
       legend = class.legend,
       fill   = rev(terrain.colors(6)),
       bty    = "n", 
       xpd = TRUE)

# !!! PASTE PLOT HERE IN YOUR REPORT DOC !!!

# ---- Plot 1984 and 1991 ----------------------------------------------------
plot(pnw84, main = "1984", col = rev(terrain.colors(6)))
legend("topright",
       legend = class.legend,
       fill   = rev(terrain.colors(6)),
       bty    = "n", 
       xpd = TRUE)
# !!! PASTE PLOT HERE IN YOUR REPORT DOC !!!

plot(pnw91, main = "1991", col = rev(terrain.colors(6)))

# !!! PASTE PLOT HERE IN YOUR REPORT DOC !!!

# -----------------------------------------------------------------------------
# Q1. Look at the three maps. Qualitatively, what changes do you notice in
#     landscape composition from 1972 to 1991? Which cover types appear to
#     increase and which appear to decrease?
# -----------------------------------------------------------------------------


# ============================================================================
##  PART 2 – Characterize Landscape Composition Over Time (Observed Data)
# ============================================================================

# Load the sample data: 200 random points classified for each year.
# Each row is one sample point; columns are its class in 1972, 1984, and 1991.
pnw.data <- read.csv("sample200.csv")
names(pnw.data)
dim(pnw.data)

# Store the number of sample points (we will need this for proportions)
sample.n <- nrow(pnw.data)   # should be 200

# --- Tally counts and convert to proportions for each year ------------------

# 1972
t1972 <- table(as.factor(pnw.data$X1972))   # count of points in each class
p1972 <- t1972 / sample.n                    # proportion of landscape in each class
inits <- as.vector(p1972)                    # initial conditions vector (used later)

# 1984
t1984 <- table(as.factor(pnw.data$X1984))
p1984 <- t1984 / sample.n

# 1991
t1991 <- table(as.factor(pnw.data$X1991))
p1991 <- t1991 / sample.n

# Combine into a single data frame for plotting
years   <- c(1972, 1984, 1991)
pnw.d2  <- rbind(as.vector(p1972), as.vector(p1984), as.vector(p1991))
pnw.d2  <- data.frame(cbind(years, pnw.d2))
names(pnw.d2) <- c("Year", "ClearCut", "EarlySeral", "MidSeral", "Mature", "OldGrowth")

# Preview the proportions table
pnw.d2

# --- Plot observed landscape composition over time --------------------------
attach(pnw.d2)
plot(Year, ClearCut,
     xlim = c(1970, 1995), ylim = c(0, 1),
     xlab = "Year",        ylab = "Proportion",
     type = "b", pch = 19, col = "brown",      lwd = 2,
     main = "Observed Landscape Composition 1972–1991")
lines(Year, EarlySeral, type = "b", pch = 19, col = "gold",        lwd = 2)
lines(Year, MidSeral,   type = "b", pch = 19, col = "light green", lwd = 2)
lines(Year, Mature,     type = "b", pch = 19, col = "green",       lwd = 2)
lines(Year, OldGrowth,  type = "b", pch = 19, col = "dark green",  lwd = 2)
legend("topright",
       legend = c("ClearCut", "EarlySeral", "MidSeral", "Mature", "OldGrowth"),
       lwd = 2, pch = 19,
       col = c("brown", "gold", "light green", "green", "dark green"),
       bty = "n")
detach(pnw.d2)

# !!! PASTE PLOT HERE IN YOUR REPORT DOC !!!

# -----------------------------------------------------------------------------
# Q2. Using the observed proportions table (pnw.d2) and the plot:
#     Which cover type showed the LARGEST proportional increase from 1972–1991?
#     Which showed the largest decrease? Do the patterns make ecological sense
#     given what you know about Pacific Northwest timber harvesting?
# -----------------------------------------------------------------------------


# ============================================================================
##  PART 3 – HAND-CALCULATION EXERCISE: Understanding the Transition Tally
# ============================================================================
#
# Before letting R build the full transition matrix, you will manually
# construct ONE ROW of the transition tally to make sure you understand
# what the matrix represents.
#
# BACKGROUND (review LLE Ch. 8 before doing this):
#   A transition tally matrix records how many sample points that were in
#   class i at time t1 ended up in class j at time t2. Each row corresponds
#   to a starting class; each column to an ending class.
#   Row sums = total number of points that were in that starting class.
#   Dividing each row by its row sum converts tallies → probabilities.
#
# ----- YOUR TASK -------------------------------------------------------
#
# STEP A. Open sample200.csv in a spreadsheet or use R to filter the data.
#         For the OLD GROWTH class (Class 5) at 1972:
#
#           Filter your data so you see ONLY rows where X1972 == 5
#           Then count how many of those points ended up in each class by 1984:
#             Class 0 (Non-forest):   _____
#             Class 1 (ClearCut):     _____
#             Class 2 (EarlySeral):   _____
#             Class 3 (MidSeral):     _____
#             Class 4 (Mature):       _____
#             Class 5 (OldGrowth):    _____
#             Row total:              _____   (should match count of OG points in 1972)
#
# Use R to help you filter and count (hint: you can use filter() or subset()):
og.1972 <- pnw.data[pnw.data$X1972 == 5, ]  # filter to OldGrowth points in 1972
nrow(og.1972)                                # how many were Old Growth in 1972?
table(og.1972$X1984)                         # how did they transition by 1984?

# STEP B. Convert those raw counts to proportions (divide each count by the row total).
#         Record the proportions for each destination class here:
#           Class 0: _____   Class 1: _____   Class 2: _____
#           Class 3: _____   Class 4: _____   Class 5: _____
#           (Check: they should sum to ~1.0)

# STEP C. In Part 4 below, R will build the full transition matrix automatically.
#         After you run Part 4, come back and compare: does the Old Growth row
#         of your R matrix match what you calculated by hand here?

# -----------------------------------------------------------------------------
# Q3. In your own words, what does a high value in cell [5, 1] of the
#     transition probability matrix (row = OldGrowth, column = ClearCut) mean
#     ecologically? What landscape process does it represent?
# -----------------------------------------------------------------------------

# -----------------------------------------------------------------------------
# Q4. What assumption does a Markov model make about HOW transitions occur?
#     (Hint: re-read the first paragraph of Ch. 8 and think about what
#      "memoryless" means – does it matter what the forest was 20 years ago?)
# -----------------------------------------------------------------------------


# ============================================================================
##  PART 4 – Build the Transition Probability Matrix
# ============================================================================

# Load the two helper functions included in the lab folder:
source("rel.R")      # rel()       – relativizes a matrix by row or column sums
source("fix_tm.R")   # fix.steps() – converts a multi-year matrix to annual steps

# ----- Build the transition TALLY for 1972 → 1984 --------------------------
# Each cell [i, j] = number of points in class i at 1972 AND class j at 1984
t.72.84 <- table(pnw.data$X1972, pnw.data$X1984)
t.72.84   # look at it: this is the raw transition tally (counts)

# ----- Convert tally to transition PROBABILITY matrix -----------------------
# Divide each row by its row sum so rows sum to 1.0
tr.72.84 <- rel(t.72.84, byrow = TRUE, bycol = FALSE)
tr.72.84   # this is the 12-year transition probability matrix

# ----- Adjust for time step -------------------------------------------------
# tr.72.84 spans 12 years, but we want ANNUAL transition probabilities.
# fix.steps() scales the off-diagonal probabilities down proportionally.
P <- fix.steps(tr.72.84, 12)
P   # this is the annual transition matrix (P) — the core of the Markov model

# Look up the class legend again if needed:
pnw.classes

# -----------------------------------------------------------------------------
# Q5. What was the annual probability of Old Growth converting to Clear-Cut
#     between 1972 and 1984? (Look at row 5 ["OldGrowth"], column 1 ["ClearCut"]
#     of matrix P.) Express this as both a probability and as a percentage.
#     Does this match the hand-calculation row you computed in Part 3?
# -----------------------------------------------------------------------------

# ----- Build the 1984–1991 matrix for comparison ----------------------------
t.84.91  <- table(pnw.data$X1984, pnw.data$X1991)
tr.84.91 <- rel(t.84.91, byrow = TRUE, bycol = FALSE)
P.84.91  <- fix.steps(tr.84.91, 7)   # 7-year time step
P.84.91

# -----------------------------------------------------------------------------
# Q6. Compare P (1972–84) and P.84.91 (1984–91).
#     Did the annual harvest rate of Old Growth forests INCREASE or DECREASE
#     in the second period? What might explain the difference?
#     (Hint: think about what was happening to old growth availability
#      and timber policy in the Pacific Northwest during this era.)
# -----------------------------------------------------------------------------


# ============================================================================
##  PART 5 – Markov Projections
# ============================================================================

# We will use the 1972–1984 transition matrix (P) and the 1972 initial
# conditions (inits) to project landscape composition forward 1000 years.

# Set up some parameters
ntypes <- length(inits)   # number of cover types (5, excluding non-forest)
ntimes <- 999             # number of years to project

# Create empty objects to store the output
t         <- vector(length = ntimes)            # year labels
transient <- matrix(nrow = ntimes, ncol = ntypes)  # proportions at each time step

# Peek at what we just created (empty containers)
str(t)
str(transient)

# Set starting conditions
tmp  <- inits        # working copy of initial proportions
t1   <- 1972         # start year

# ----- RUN THE PROJECTION ---------------------------------------------------
# Each step: multiply the current proportion vector by the transition matrix P
# This is the heart of the Markov model: x(t+1) = x(t) %*% P

for (i in 1:ntimes) {
  t[i]          <- t1 + i
  transient[i,] <- tmp %*% P    # matrix multiplication
  tmp           <- transient[i,]
}

# Wrap the output into a nice data frame
out <- data.frame(cbind(t, transient))
names(out) <- c("Year", "ClearCut", "EarlySeral", "MidSeral", "Mature", "OldGrowth")

# Save output to CSV (useful to examine in Excel)
write.csv(out, "markov_out.csv", row.names = FALSE)

# ----- Model Verification: plot projections for first ~25 years only --------
# We will overlay the actual observed data points to see how well the model
# reproduces the changes that actually happened in this landscape.

plot(out[,1], out[,2],
     type = "l", col = "brown",      lwd = 2,
     xlim = c(1970, 1995), ylim = c(0, 0.5),
     xlab = "Year",        ylab = "Proportion",
     main = "Model Verification: Projections vs. Observed Data")
lines(out[,1], out[,3], col = "gold",        lwd = 2)
lines(out[,1], out[,4], col = "light green", lwd = 2)
lines(out[,1], out[,5], col = "green",       lwd = 2)
lines(out[,1], out[,6], col = "dark green",  lwd = 2)

# Overlay the actual observed data points
points(rep(years[1], ntypes), p1972,
       pch = 19, col = c("brown", "gold", "light green", "green", "dark green"))
points(rep(years[2], ntypes), p1984,
       pch = 19, col = c("brown", "gold", "light green", "green", "dark green"))
points(rep(years[3], ntypes), p1991,
       pch = 19, col = c("brown", "gold", "light green", "green", "dark green"))
legend("topright",
       legend = names(out[2:6]), lwd = 2, pch = 19,
       col    = c("brown", "gold", "light green", "green", "dark green"),
       bty    = "n")

# !!! PASTE PLOT HERE IN YOUR REPORT DOC !!!

# -----------------------------------------------------------------------------
# Q7. Does the model projection match the observed values (the dots)?
#     Which cover types fit well and which do not?
#     Suggest at least some reasons why a Markov model might not perfectly
#     reproduce observed landscape change.
# -----------------------------------------------------------------------------

# ----- Long-term projection: full 1000 years --------------------------------
plot(out[,1], out[,2],
     type = "l", col = "brown",      lwd = 2,
     xlim = c(1972, 2972), ylim = c(0, 0.5),
     xlab = "Year",        ylab = "Proportion",
     main = "Baseline Projection – 1000 Years")
lines(out[,1], out[,3], col = "gold",        lwd = 2)
lines(out[,1], out[,4], col = "light green", lwd = 2)
lines(out[,1], out[,5], col = "green",       lwd = 2)
lines(out[,1], out[,6], col = "dark green",  lwd = 2)
legend("topright",
       legend = names(out[2:6]),
       col    = c("brown", "gold", "light green", "green", "dark green"),
       lwd = 2, bty = "n")

# !!! PASTE PLOT HERE IN YOUR REPORT DOC !!!

# -----------------------------------------------------------------------------
# Q8. Does the model ever reach a steady state (stable proportions)?
#     Approximately what year does Old Growth forest fall below 10%?
#     What does the steady state of this landscape tell you about the long-term
#     trajectory under the observed 1972–84 harvest regime?
# -----------------------------------------------------------------------------

# Optional: find the exact year OG drops below 10%
out[out$OldGrowth < 0.10, ][1, ]   # first year below 10%


# ============================================================================
##  PART 6 – Scenario Analysis: What If Harvest Rates Change?
# ============================================================================
#
# One of the most powerful features of Markov models is the ability to run
# "what if" scenarios by adjusting transition probabilities.
# Here we will cut the harvest rates of Mature and Old Growth forests in half
# and re-run the projection to see how the landscape responds.

# Copy P so we don't overwrite the original
P2 <- P

# View the current harvest rates
P[4, 1]   # annual probability: Mature    → ClearCut (row 4, col 1)
P[5, 1]   # annual probability: OldGrowth → ClearCut (row 5, col 1)

# Halve the harvest rate for Mature forest
h41      <- P[4, 1] / 2          # new (halved) rate
P2[4, 1] <- h41                  # replace in matrix
P2[4, 4] <- P[4, 4] + h41        # add the "saved" probability back to staying Mature

# Halve the harvest rate for Old Growth
h51      <- P[5, 1] / 2
P2[5, 1] <- h51
P2[5, 5] <- P2[5, 5] + h51       # add back to staying OldGrowth

# Inspect the adjusted matrix
P2

# ----- Re-run the projection with P2 ----------------------------------------
trans2    <- matrix(nrow = ntimes, ncol = ntypes)
t1        <- 1972
tmp       <- inits

for (i in 1:ntimes) {
  t[i]       <- t1 + i
  trans2[i,] <- tmp %*% P2
  tmp        <- trans2[i,]
}

out2          <- data.frame(cbind(t, trans2))
out2          <- rbind(c(t1, inits), out2)
names(out2)   <- c("Year", "ClearCut", "EarlySeral", "MidSeral", "Mature", "OldGrowth")

# Plot the 50% harvest scenario
plot(out2[,1], out2[,2],
     type = "l", col = "brown",      lwd = 2,
     xlim = c(1972, 2972), ylim = c(0, 0.5),
     xlab = "Year",        ylab = "Proportion",
     main = "Projection: 50% Harvest Rate")
lines(out2[,1], out2[,3], col = "gold",        lwd = 2)
lines(out2[,1], out2[,4], col = "light green", lwd = 2)
lines(out2[,1], out2[,5], col = "green",       lwd = 2)
lines(out2[,1], out2[,6], col = "dark green",  lwd = 2)
legend("topright",
       legend = names(out2[2:6]),
       col    = c("brown", "gold", "light green", "green", "dark green"),
       lwd = 2, bty = "n")

# !!! PASTE PLOT HERE IN YOUR REPORT DOC !!!

# -----------------------------------------------------------------------------
# Q9. Compare the 50% harvest rate scenario to the baseline.
#     At what year does OldGrowth fall below 10% under the 50% scenario?
#     How much extra time did halving the harvest rate buy for old growth forests?
# -----------------------------------------------------------------------------

# Use this to find the year:
out2[out2$OldGrowth < 0.10, ][1, ]

# ============================================================================
## CHALLENGE – Find a Sustainable Harvest Rate
# ============================================================================
#
# Your goal: find the harvest rate that keeps Old Growth above 30% in the
# long run. You can modify the code block below and try different fractions.
# You may also explore changing other cells in the matrix (e.g., what if
# early-seral recovery rates also changed?).
#
# Hint: you can modify the fraction applied to P[4,1] and P[5,1] freely.
#       fractions less than 1 reduce harvest; fractions greater than 1 increase it.

# ---- Copy the baseline P again and adjust as you like ----------------------
P3 <- P

# YOUR CODE HERE: adjust harvest rates as desired
# Example: cut to 10% of original rate
# P3[4,1] <- P[4,1] * 0.10
# P3[4,4] <- P[4,4] + (P[4,1] - P3[4,1])
# P3[5,1] <- P[5,1] * 0.10
# P3[5,5] <- P[5,5] + (P[5,1] - P3[5,1])

# Re-run the projection with P3 (copy/paste the loop from Part 6 and change names)
trans3  <- matrix(nrow = ntimes, ncol = ntypes)
t1      <- 1972
tmp     <- inits

for (i in 1:ntimes) {
  t[i]       <- t1 + i
  trans3[i,] <- tmp %*% P3
  tmp        <- trans3[i,]
}

out3        <- data.frame(cbind(t, trans3))
out3        <- rbind(c(t1, inits), out3)
names(out3) <- c("Year", "ClearCut", "EarlySeral", "MidSeral", "Mature", "OldGrowth")

# Compare all three scenarios for Old Growth only:
plot(out[,1],  out[,6],
     type = "l", col = 1, lwd = 2,
     xlim = c(1972, 2075), ylim = c(0, 0.5),
     xlab = "Year",        ylab = "Proportion Old Growth",
     main = "Old Growth Trajectories Under Three Harvest Scenarios")
lines(out2[,1], out2[,6], col = 2, lwd = 2)
lines(out3[,1], out3[,6], col = 3, lwd = 2)
abline(h = 0.30, lty = 2, col = "grey50")   # 30% sustainability threshold
legend("topright",
       legend = c("Baseline (1972–84 rate)", "50% rate", "Your scenario"),
       col = 1:3, lwd = 2, bty = "n")

# !!! PASTE PLOT HERE IN YOUR REPORT DOC !!!

# -----------------------------------------------------------------------------
# Q10. What harvest rate fraction did you find that keeps Old Growth above 30%?
#      Is this ecologically or economically realistic?
#      What OTHER landscape transitions (besides harvest rates) might you
#      adjust to improve old growth sustainability without reducing harvest
#      as drastically? (Think about regeneration rates and disturbance regimes.)
# -----------------------------------------------------------------------------

# =============================================================================
## SYNTHESIS QUESTIONS
# =============================================================================

# -----------------------------------------------------------------------------
# Q11. A Markov model assumes that transition probabilities are CONSTANT
#      through time (i.e., the 1972–84 rates will hold forever). Name at least
#      TWO real-world factors that might cause those rates to change over time,
#      and explain how each would affect the reliability of a 1000-year projection.
# -----------------------------------------------------------------------------

# -----------------------------------------------------------------------------
# Q12. Compare this Markov modeling approach to the neutral landscape model
#      (NLM) approach from Lab 4. Both are modeling tools.
#      What different kinds of ecological QUESTIONS does each one address?
#      When would you choose a Markov model vs. an NLM, and why?
# -----------------------------------------------------------------------------

# =============================================================================
## END OF SCRIPT
# =============================================================================
