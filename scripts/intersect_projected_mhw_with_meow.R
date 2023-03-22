################################################################################
# title
################################################################################
#
# Juan Carlos Villaseñor-Derbez
# juancvd@stanford.edu
# date
#
# Description
#
################################################################################

## SET UP ######################################################################

# Load packages ----------------------------------------------------------------
library(here)
library(sf)
library(tidyverse)

# Load data --------------------------------------------------------------------
meow <- st_read(here("data", "raw", "clean_meow.gpkg")) %>% 
  select(-a)

obs <- read_csv(here("data", "output", "MHW", "MHW_stats_OISST_OBSERVED_19820101-20211231.csv")) %>% 
  set_names(c("lon", "lat", 1982:2021)) %>% 
  pivot_longer(cols = c(3:42),
               names_to = "year",
               values_to = "obs") %>% 
  mutate(year = as.numeric(year))

ssp126 <- read_csv(here("data", "output", "MHW", "MHW_stats_ensemble_ssp126_19820101-21001231.csv")) %>% 
  set_names(c("lon", "lat", 1982:2100)) %>% 
  pivot_longer(cols = c(3:121),
               names_to = "year",
               values_to = "ssp126")

ssp245 <- read_csv(here("data", "output", "MHW", "MHW_stats_ensemble_ssp245_19820101-21001231.csv")) %>% 
  set_names(c("lon", "lat", 1982:2100)) %>% 
  pivot_longer(cols = c(3:121),
               names_to = "year",
               values_to = "ssp245")

ssp585 <- read_csv(here("data", "output", "MHW", "MHW_stats_ensemble_ssp585_19820101-21001231.csv")) %>% 
  set_names(c("lon", "lat", 1982:2100)) %>% 
  pivot_longer(cols = c(3:121),
               names_to = "year",
               values_to = "ssp585")

## PROCESSING ##################################################################

# X ----------------------------------------------------------------------------
grid <- ssp126 %>% 
  select(lon, lat) %>% 
  distinct() %>% 
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>% 
  st_join(meow) %>% 
  bind_cols(st_coordinates(.)) %>% 
  st_drop_geometry() %>% 
  select(lon = X, lat = Y, everything())

# X ----------------------------------------------------------------------------
data <- ssp126 %>% 
  left_join(ssp245, by = c("lat", "lon", "year")) %>% 
  left_join(ssp585, by = c("lat", "lon", "year")) %>% 
  mutate(year = as.numeric(year)) %>% 
  filter(year >= 2021) %>% 
  pivot_longer(cols = 4:6,
               names_to = "ssp",
               values_to = "MHW") %>% 
  left_join(grid, by = c("lat", "lon"))

## VISUALIZE ###################################################################

# X ----------------------------------------------------------------------------

## EXPORT ######################################################################

# X ----------------------------------------------------------------------------

ggplot(data = data,
       mapping = aes(x = year, y = MHW, fill = ssp, color = ssp)) +
  stat_summary(data = obs,
               mapping = aes(x = year, y = obs),
               geom = "line",
               fun = "mean",
               inherit.aes = F) +
  # stat_summary(geom = "ribbon", fun.data = "mean_cl_normal") +
  stat_summary(geom = "line", fun = "mean") +
  facet_wrap(~realm, ncol = 2) +
  theme_bw()

ggplot(data = data,
       mapping = aes(x = year, y = MHW, fill = ssp, color = ssp)) +
  stat_summary(data = obs,
               mapping = aes(x = year, y = obs),
               geom = "line",
               fun = "mean",
               inherit.aes = F) +
  # stat_summary(geom = "ribbon", fun.data = "mean_cl_normal") +
  stat_summary(geom = "line", fun = "mean") +
  facet_wrap(~province) +
  theme_bw()

ggplot(data = data,
       mapping = aes(x = year, y = MHW, fill = ssp, color = ssp)) +
  stat_summary(data = obs,
               mapping = aes(x = year, y = obs),
               geom = "line",
               fun = "mean",
               inherit.aes = F) +
  # stat_summary(geom = "ribbon", fun.data = "mean_cl_normal") +
  stat_summary(geom = "line", fun = "mean") +
  facet_wrap(~ecoregion) +
  theme_bw()
