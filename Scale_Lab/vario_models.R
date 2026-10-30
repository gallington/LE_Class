# modeled semivariogram:

library(gstat)
library(sf)
library(tidyverse)

# Create spatial object

tp_sf <-
  veght_long %>%
  filter(Site == "TP") %>%
  mutate(y = 0) %>%
  st_as_sf(coords = c("Pos", "y"))

# Empirical variogram

vg_emp <-
   variogram(
    VegHt ~ 1,
    tp_sf,
    cutoff = 100,
    width = 2
  )

# Initial model guesses

m_sph <- vgm(psill = 1000, model = "Sph", range = 25, nugget = 0)
m_exp <- vgm(psill = 1000, model = "Exp", range = 25, nugget = 0)
m_gau <- vgm(psill = 1000, model = "Gau", range = 25, nugget = 0)

# Fit models

fit_sph <- fit.variogram(vg_emp, m_sph)
fit_exp <- fit.variogram(vg_emp, m_exp)
fit_gau <- fit.variogram(vg_emp, m_gau)

fit_sph
fit_exp
fit_gau

# Generate predictions from fitted models:
lag_seq <- seq(0, 100, by = 1)

sph_line <-
  variogramLine(
    fit_sph,
    maxdist = 100,
    n = length(lag_seq)
  ) %>%
  as_tibble() %>%
  mutate(Model = "Spherical")

exp_line <-
  variogramLine(
    fit_exp,
    maxdist = 100,
    n = length(lag_seq)
  ) %>%
  as_tibble() %>%
  mutate(Model = "Exponential")

gau_line <-
  variogramLine(
    fit_gau,
    maxdist = 100,
    n = length(lag_seq)
  ) %>%
  as_tibble() %>%
  mutate(Model = "Gaussian")

model_lines <-
  bind_rows(
    sph_line,
    exp_line,
    gau_line
  )

# Plot empirical and fitted semivariograms:
ggplot() +

  geom_point(
    data = vg_emp,
    aes(dist, gamma),
    size = 2
  ) +

  geom_line(
    data = model_lines,
    aes(dist, gamma, color = Model),
    linewidth = 1
  ) +

  labs(
    x = "Spatial Lag (m)",
    y = "Semivariance",
    title = "Tallgrass Prairie Semivariogram",
    subtitle = "Empirical semivariogram with fitted models"
  ) +

  theme_bw()

# Compare teh diff model fits:
tibble(
  Model = c(
    "Spherical",
    "Exponential",
    "Gaussian"
  ),
  SSErr = c(
    attr(fit_sph, "SSErr"),
    attr(fit_exp, "SSErr"),
    attr(fit_gau, "SSErr")
  )
) %>%
  arrange(SSErr)
# the model with the smallest SSErr provides the best fit to the empirical semivariogram.

# You do it now:
# Compute for all sites.  Determine best model fit for each:
site_variograms <-
  
  veght_long %>%
  mutate(y = 0) %>%
  group_by(Site) %>%
  nest() %>%
  mutate(
    
    sf_data =
      map(
        data,
        ~ st_as_sf(.x, coords = c("Pos", "y"))
      ),
    
    empirical =
      map(
        sf_data,
        ~ variogram(
          VegHt ~ 1,
          .x,
          cutoff = 100,
          width = 2
        )
      )
  )