library(NLMR)
library(landscapetools)

par(mfrow = c(1,3))

# simulate midpoint displacement NLM:
show_landscape(util_classify(nlm_random(100,100),
                             weighting = c(0.6,0.4)))

show_landscape(util_classify(nlm_random(100,100),
                             weighting = c(0.4,0.6)))

show_landscape(util_classify(nlm_mpd(100,100,
                             roughness = 0.8),
                             weighting = c(0.4,0.6)))