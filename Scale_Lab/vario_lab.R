# This code was modified from the original which was developed in
# Scale Detection Using Semivariograms and Autocorrelograms 
# by M.W. Palmer, D.J. McGlinn
# it has been updated to make use of updated packages and syntax
# this code makes use of the data provided on vegetation height:
# veght_long.csv

# Work through the following code chunks. Save all of your plots
# Then answer the questions in Lab1_SpatialDependence.doc


################################################################################
# PACKAGES
################################################################################
# install these packages -- you will need to use functions from each of them
install.packages("tidyverse")
install.packages("sf") 
install.packages("gstat")

# then load the libraries
library(tidyverse)
library(sf)
library(gstat)

################################################################################
# DATA IMPORT
################################################################################
# Set R's working directory to the folder where you have stored the relevant data files

setwd("./Scale_Lab/")

# read in the .csv files you have downloaded in the working directory
# this dataframe contains veg height data from three sites
# for the third site there are data from five different pastures within that site.

veght <- read_csv("veght_long.csv")

##############################
#Column descriptions for veght
# These vegetation height data were collected along 200m transects in different sites
#"Site": TP = Tallgrass Prairie (Osage County, Oklahoma)
#        MF = Mowed field (Oklahoma State Univ. Campus)
#        Pasture.A to Pasture.E: Five savanna pastures in Payne County, Oklahoma
#"Pos": Transect position (m)
#"VegHt": Height of vegetation (cm) 
#"
##############################


################################################################################
# DATA EXPLORATION
################################################################################
# begin with some exploration of the data 
# NOTE: the following data exploration is not described in the chapter text
# but is recommended when starting with any new dataset

str(veght) # examine the structure of the data

head(veght) # the first six rows of the data

# Summary by site

veght %>%
  group_by(Site) %>%
  summarise(
    n = n(),
    mean_height = mean(VegHt),
    sd_height = sd(VegHt),
    min_height = min(VegHt),
    max_height = max(VegHt)
  )

################################################################################
# VEGETATION HEIGHT PLOTS
################################################################################
# Examine the data visually for one of the transects (e.g., Tallgrass Prairie (TP) dataset)
# Tallgrass Prairie

veght %>%
  filter(Site == "TP") %>% #change Site == to a diff site code if you want to visualize a diff site
  ggplot(aes(Pos, VegHt)) +
  geom_line() +
  geom_point() +
  labs(
    x = "Transect Position (m)",
    y = "Vegetation Height (cm)",
    title = "Tallgrass Prairie" # if you change the site above, be sure to change the axis label!
  ) +
  theme_bw()

# Save your plot using the Export tab in the Plots pane over there --->

# All sites viz at once

veght %>%
  ggplot(aes(Pos, VegHt)) +
  geom_line() +
  geom_point(size = 0.75) +
  facet_wrap(~Site, scales = "free_y") +
  labs(
    x = "Transect Position (m)",
    y = "Vegetation Height (cm)"
  ) +
  theme_bw()

# Save your plot using the Export tab in the Plots pane over there --->

################################################################################
# OPTION 1: calculate semivariograms and autocorrellograms using 
# functions within the gstat package

# If you want to get a better sense of what is actually being calculated and how
# skip down to OPTION 2: 

################################################################################
# SEMIVARIOGRAM USING gstat
################################################################################

# first have to make a spatial object out of our dataframe
# for this versio we're just looking at Tallgrass Prairie (TP)

tp_sf <-
  veght %>%
  filter(Site == "TP") %>% #change the site code if you want to plot a diff one
  mutate(y = 0) %>%
  st_as_sf(
    coords = c("Pos", "y")# we just have a single transect of data -- length along one line (x)
    # st_as_sf() requires 2 coordinates, so create a dummy y coordinate
  )

# empirical variogram
################################################################################
# TALLGRASS PRAIRIE SEMIVARIOGRAM
################################################################################

tp_vario_gstat <-  # saves the calculate semivariance values at all lag distances into an object
  variogram(
    VegHt ~ 1,
    data = tp_sf, # note this is just using the datafram you created above that just has TP data in it
    cutoff = 100,  # because we only are interested in spatial lags less than half of the max distance we will only plot lags from 1 to 100 m
    width = 1
  )

# Examine the results:
tp_vario_gstat #gamma = the semivariance

# Now compute for all sites:
################################################################################
# ALL SITES SEMIVARIOGRAMS
################################################################################
# select this entire block of code and Run the entire thing at once.
all_gstat_variograms <-
  veght %>%
  mutate(y = 0) %>%
  group_by(Site) %>%
  nest() %>%
  mutate(
    variogram =
      map(
        data,
        ~ {
          sf_dat <-
            st_as_sf(
              .x,
              coords = c("Pos", "y")
            )
          variogram(
            VegHt ~ 1,
            sf_dat,
            cutoff = 100, # because we only are interested in spatial lags less than half of the max distance we will only plot lags from 1 to 100 m
            width = 1
          ) %>%
            as_tibble()
        }
      )
  ) %>%
  select(-data) %>%
  unnest(variogram)

# NOTE: What you did above just put all of the calculations into an 
# Object called: all_gstat_variograms <- 
# We'll use that below to plot the results.

################################################################################
# SPATIAL AUTOCORRELOGRAMS FOR ALL SITES
################################################################################

# Function to calculate autocorrelation at each lag distance

calc_acf <- function(df, max_lag = 100){
  
  lags <- 1:max_lag
  
  tibble(
    lag = lags,
    autocorrelation = map_dbl(
      lags,
      ~ cor(
        df$VegHt[1:(nrow(df) - .x)],
        df$VegHt[(1 + .x):nrow(df)],
        use = "complete.obs"
      )
    )
  )
}

all_autocorrelograms <-
  veght %>%
  arrange(Site, Pos) %>%
  group_by(Site) %>%
  nest() %>%
  mutate(
    correlogram = map(data, calc_acf)
  ) %>%
  select(-data) %>%
  unnest(correlogram)

################################################################################
# PLOT RESULTS
################################################################################

# Here we plot side-by-side plots of vegetation height as a function of spatial position 
# and the semivariogram and correlogram as a function of spatial lag
# Because we only are interested in spatial lags less than half of the max distance we
# will only plot lags from 1 to 100 m


all_gstat_variograms %>%
  ggplot(
    aes(
      dist,
      gamma
    )
  ) +
  geom_line() +
  geom_point(size = 0.8) +
  facet_wrap(~Site, scales = "free_y") +
  labs(
    x = "Spatial Lag (m)",
    y = "Semivariance"
  ) +
  theme_bw()


################################################################################
# AUTOCORRELOGRAM PLOTS
################################################################################

all_autocorrelograms %>%
  ggplot(
    aes(
      lag,
      autocorrelation
    )
  ) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed",
    color = "grey50"
  ) +
  geom_line() +
  geom_point(size = 0.8) +
  facet_wrap(~Site, scales = "free_y") +
  labs(
    x = "Spatial Lag (m)",
    y = "Autocorrelation (r)"
  ) +
  theme_bw()

# DONE! NOW MOVE ON TO ANSWERING THE QUESTIONS (separate doc).
# Be sure to save your figure!







################################################################################
# Option 2: Calculate the semivariance and autocorrellograms 
# "by hand", by specifying some functions, which generate tables of pairwise semivariance
# I'm giving you this option in case you want to look inside the box a bit more
################################################################################
# SEMIVARIOGRAM FUNCTION
################################################################################
# select this entire block of code and Run all at once
semivariogram_tbl <- function(pos, value){
  lag <- as.vector(dist(pos))
  semivar <- 0.5 * as.vector(dist(value))^2
  tibble(
    lag = lag,
    semivariance = semivar
  ) %>%
    group_by(lag) %>%
    summarise(
      gamma = mean(semivariance),
      .groups = "drop"
    )
}

################################################################################
# AUTOCORRELOGRAM FUNCTION
################################################################################
# select this entire block of code and Run all at once
auto_cor_tbl <- function(x){
  map_dfr(
    1:(length(x)-2),
    function(h){
  tibble(
        lag = h,
        autocorrelation =
          cor(
            x[1:(length(x)-h)],
            x[(h+1):length(x)]
          ))})}

################################################################################
# TALLGRASS PRAIRIE SEMIVARIOGRAM
################################################################################

tp_vario <-
  
  veght_long %>%
  filter(Site == "TP") %>%
  summarise(
    data = list(
      semivariogram_tbl(Pos, VegHt)
    )
  ) %>%
  pull(data) %>%
  .[[1]]

tp_vario %>%
  ggplot(aes(lag, gamma)) +
  geom_line() +
  geom_point() +
  geom_vline(
    xintercept = max(tp_vario$lag)/2,
    linetype = 2,
    colour = "dodgerblue"
  ) +
  labs(
    x = "Spatial Lag (m)",
    y = expression(gamma(h)),
    title = "Tallgrass Prairie Semivariogram"
  ) +
  theme_bw()

################################################################################
# TALLGRASS PRAIRIE AUTOCORRELOGRAM
################################################################################

tp_cor <-
  
  veght_long %>%
  filter(Site == "TP") %>%
  summarise(
    data = list(
      auto_cor_tbl(VegHt)
    )
  ) %>%
  pull(data) %>%
  .[[1]]

tp_cor %>%
  ggplot(aes(lag, autocorrelation)) +
  geom_line() +
  geom_point() +
  geom_hline(yintercept = 0) +
  geom_vline(
    xintercept = 100,
    linetype = 2,
    colour = "dodgerblue"
  ) +
  labs(
    x = "Spatial Lag (m)",
    y = "Autocorrelation",
    title = "Tallgrass Prairie Autocorrelogram"
  ) +
  theme_bw()

################################################################################
# SEMIVARIOGRAMS FOR ALL SITES
################################################################################

all_variograms <-
  
  veght_long %>%
  group_by(Site) %>%
  nest() %>%
  mutate(
    variogram =
      map(
        data,
        ~ semivariogram_tbl(
          pos = .x$Pos,
          value = .x$VegHt
        )
      )
  ) %>%
  select(-data) %>%
  unnest(variogram)

all_variograms %>%
  filter(lag <= 100) %>%
  ggplot(
    aes(
      lag,
      gamma
    )
  ) +
  geom_line() +
  facet_wrap(~Site, scales = "free_y") +
  labs(
    x = "Spatial Lag (m)",
    y = "Semivariance"
  ) +
  theme_bw()

################################################################################
# AUTOCORRELOGRAMS FOR ALL SITES
################################################################################

all_autocor <-
  
  veght_long %>%
  group_by(Site) %>%
  nest() %>%
  mutate(
    autocor =
      map(
        data,
        ~ auto_cor_tbl(.x$VegHt)
      )
  ) %>%
  select(-data) %>%
  unnest(autocor)

all_autocor %>%
  filter(lag <= 100) %>%
  ggplot(
    aes(
      lag,
      autocorrelation
    )
  ) +
  geom_line() +
  geom_hline(yintercept = 0) +
  facet_wrap(~Site, scales = "free_y") +
  labs(
    x = "Spatial Lag (m)",
    y = "Autocorrelation"
  ) +
  theme_bw()

################################################################################
# COMBINE RESULTS INTO SINGLE FIGURE
################################################################################

veg_panel <-
  
  veght_long %>%
  transmute(
    Site,
    metric = "Vegetation Height",
    x = Pos,
    y = VegHt
  )

vario_panel <-
  
  all_variograms %>%
  filter(lag <= 100) %>%
  transmute(
    Site,
    metric = "Semivariance",
    x = lag,
    y = gamma
  )

cor_panel <-
  
  all_autocor %>%
  filter(lag <= 100) %>%
  transmute(
    Site,
    metric = "Autocorrelation",
    x = lag,
    y = autocorrelation
  )

plot_data <-
  
  bind_rows(
    veg_panel,
    vario_panel,
    cor_panel
  )

ggplot(
  plot_data,
  aes(
    x,
    y
  )
) +
  geom_line() +
  facet_grid(
    metric ~ Site,
    scales = "free_y"
  ) +
  labs(
    x = NULL,
    y = NULL
  ) +
  theme_bw()

################################################################################
# OPTIONAL: IDENTIFY CHARACTERISTIC SCALE
################################################################################

# First lag where autocorrelation becomes negative

scale_estimates <-
  
  all_autocor %>%
  filter(lag <= 100) %>%
  group_by(Site) %>%
  summarise(
    decorrelation_scale =
      min(
        lag[autocorrelation < 0],
        na.rm = TRUE
      )
  )

scale_estimates


