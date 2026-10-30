install.packages("remotes")
remotes::install_github("ropensci/NLMR")
install.packages("landscapetools")
install.packages("igraph")
library(NLMR)
library(landscapetools)
library(igraph)
# maps created in this lab:
# 1. simple random maps
# 2. multi-fractal maps
# This lab will be working with grid (raster) data 
# so for each map we need to specify the number of rows & columns in the grid

# A central concept to emerge from neutral landscape models (which were themselves 
# derived initially from a branch of physics called percolation theory; Stauffer and Aharony 1992) 
# is that of critical thresholds. In short, small changes in p can result in sudden 
# changes in spatial patterns, and in particular, whether habitat is connected from 
# one edge of the map or not. The value of p at which spatial patterns on a random 
# map change qualitatively is called the percolation threshold, typically abbreviated as pc
# or pcrit. In this exercise, we use R to generate and analyze a series
# of simple random maps as a function of p, which will range from 0.1 to 0.9. 
# 
# The results of four landscape metrics (total number of patches, total edge, 
# area-weighted mean patch size, and frequency of percolation among the map replicates) 
# will be examined to determine if and at what values of p a percolation threshold may exist. 
# 1. You and/or your team will generate a series of random maps using a specific neighborhood rule. 
# If your last name begins with a letter in the range A–L then use neighborhood rule 1 (nearest-neighbor); 
# M–O then use neighborhood rule 2 (next nearest-neighbor); 
# P–W then use neighborhood rule 3 (third nearest- neighbor).
# 
# 
# 
# https://github.com/ropensci/NLMR
# https://github.com/ropensci/landscapetools/
# https://r-spatialecology.github.io/landscapemetrics/
#   
#   



# Playing with simple random maps:
rmap<- NLMR::nlm_random(nrow= 128,
                        ncol = 128,
                        resolution = 1,
                        rescale = TRUE) #Each raster cell is randomly assigned a value between 0-1
raster::plot(random)
#show_landscape(rmap, discrete = TRUE)


show_landscape(rcmap, discrete = TRUE)
# Random maps with specified clustering parameters
rcmap <- NLMR::nlm_randomcluster(nrow = 100, # set the number of columns
                                 ncol= 100,  # set the number of rows
                                 # Proportion of elements randomly selected to form clusters.
                                 p =  0.5,  
                                 # Specify a vector with the cluster type distribution (percentages of occupancy). 
                                 ai = c(0.3, 0.6, 0.1) , 
                                 # Set the neighborhood rule: 4 or 8
                                 neighbourhood = 4,
                                 rescale = FALSE)
# extract the values:
rc.dat<- rcmap@data@values
show_landscape(rcmap, discrete = TRUE)


# First, change the two probabilities (i.e., habitat, non-habitat) for land- cover generation from 0.5 to 0.1 and 0.9 (but leave p0 unchanged). 
# Copy and edit again, changing probabilities to 0.2 and 0.8. 
# And a third time with probabilities of 0.3 and 0.7
# One last time changing to 0.4 and 0.6





# play around with different settings to see how the output map changes based on these assumptions.
rcmap2 <- NLMR::nlm_randomcluster(nrow = 100,
                                  ncol= 100,
                                  # Proportion of elements randomly selected to form clusters.
                                  p =  0.25,  
                                  #Vector with the cluster type distribution (percentages of occupancy). 
                                  ai = c(0.5, 0.5) ,
                                  
                                  rescale = FALSE)

rc2.dat <- rcmap2@data@values
show_landscape(rcmap2)

# plot the cumulative distribution functions
plot(ecdf(rc.dat))
plot(ecdf(rc2.dat))

# summary statistics
Ustats <- function (x)        
{
  value <- c(n = length(x),
             mean = mean(x),
             med = median(x),
             var = var(x),
             min = min(x),
             max = max(x))
  return (value)
}  # end of function indat

# classify:
classified_landscape <- util_classify(rcmap,
                                      n = 3,
                                      level_names = c("Land Use 1", 
                                                      "Land Use 2",
                                                      "Land Use 3"))



# Merge all landscapes into one
merged_landscape <- util_merge(fractal_landscape,
                               c(gradient_landscape, random_landscape),
                               scalingfactor = 1)

# Plot an overview
merge_vis <- list(
  "1) Primary" = fractal_landscape,
  "2) Secondary 1" = gradient_landscape,
  "3) Secondary 2" = random_landscape,
  "4) Result" = merged_landscape
)

show_landscape(merge_vis)









