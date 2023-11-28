################################################################################
# title
################################################################################
#
# Juan Carlos Villaseñor-Derbez
# juancvd@stanford.edu
# date
#
# This is the prefered one for analysis
#
################################################################################

## SET UP ######################################################################

# Load packages ----------------------------------------------------------------
pacman::p_load(
  here,
  sf,
  furrr,
  future,
  tidyverse)


# Turn off spatial sphere
# sf_use_s2(TRUE)

# Load data --------------------------------------------------------------------
kelp <- st_read(here("data", "processed", "clean_kelp.gpkg"))

mpas <- st_read(here("data", "processed", "clean_mpas.gpkg"))

meow <- st_read(here("data", "raw", "clean_meow.gpkg")) %>% 
  select(-a)

# Define some functions --------------------------------------------------------
fast_intersect <- function(x, y) {
  # browser()
  # Step 1 - make polygons valid and throw away otherwise
  # For kelp polygons
  x <- x %>% 
    st_make_valid() %>%
    filter(st_is_valid(.)) %>%
    mutate(x_id = 1:nrow(.)) # make a unique identifier
  
  # For MPA polygons
  y <- y %>% 
    st_make_valid() %>%
    filter(st_is_valid(.)) %>%
    mutate(y_id = 1:nrow(.)) %>% 
    select(-country)
  
  # Step 2 - Join kelp and MPA polygons (defaults to intersection)
  join_coverage <- st_join(x, y)
  
  # Identify the kelp polygons that touch an MPA
  kelp_inside <- join_coverage %>%
    st_drop_geometry() %>%
    filter(!is.na(y_id)) %>%
    pull(x_id)
  
  # Kelp outside MPAs
  kelp_outside <- join_coverage %>%
    filter(!x_id %in% kelp_inside)  # Remove the ones that lie inside MPA
  
  # Identify MPAs that touch kelp
  mpa_with_kelp <- join_coverage %>%
    st_drop_geometry() %>%
    filter(!is.na(y_id)) %>%
    pull(y_id)
  
  # Retain only polygons touching an MPA
  xx <- x %>%
    filter(x_id %in% kelp_inside)
  
  # Retain only MPAs touching kelp (i.e. removes MPAs not relevant to our analysis)
  yy <- y %>%
    filter(y_id %in% mpa_with_kelp) 
  
  # Step 3 - Intersect polygons that we know touch in case we need to brake them down
  int_coverage <- st_intersection(xx, yy) %>%
    ungroup() %>%
    st_make_valid() %>%
    # filter(st_is_valid(.)) %>%
    filter(!st_is_empty(.)) #%>% 
    # mutate(kelp_area = as.numeric(st_area(.)))
  
  # Step4 - Bring it all together
  # Combine with the previous ones
  combined <- bind_rows(kelp_outside, int_coverage) %>% 
    replace_na(replace = list(lfp_cat = "None",
                              lfp_group = "None",
                              lfp = 0)) %>% 
    select(country, x_id, y_id, mpa_id, lfp, lfp_cat, lfp_group)
  
  return(combined)
}

## PROCESSING ##################################################################

# Assemble nested tibbles ------------------------------------------------------
nested_kelp <- kelp %>%
  group_by(country) %>% 
  group_split()
# nest() %>%
# rename(k = data)

nested_mpas <- mpas %>%
  group_by(country) %>% 
  group_split()
# nest() %>%
# rename(m = data)

# Begin intersection -----------------------------------------------------------
future::plan("multisession",
             workers = 12)

kelp_and_mpa <- future_map2_dfr(nested_kelp,
                                nested_mpas,
                                fast_intersect)
beepr::beep(2)

plan(sequential)

# kelp_and_mpa_unnested <- kelp_and_mpa %>% 
#   select(country, data) %>% 
#   unnest(data) %>% 
#   ungroup() %>% 
#   st_sf() 

sf_use_s2(FALSE) # Turn off to intersect faster
intersect_kelp_mpa_and_ecoregion <- st_intersection(kelp_and_mpa, meow)

sf_use_s2(TRUE)  # Turn on to calculate areas
kelp_mpa_and_ecoregion <- intersect_kelp_mpa_and_ecoregion %>% 
  mutate(kelp_area_km2 = as.numeric(st_area(.)) / 1e6)

# EXPORT #######################################################################
saveRDS(kelp_mpa_and_ecoregion,
        file = here("data", "output", "intersected_kelp_mpa_ecoregion.rds"))

st_write(obj = kelp_mpa_and_ecoregion,
         dsn = here("data", "output", "intersected_kelp_mpa_ecoregion.gpkg"),
         delete_dsn = T)
