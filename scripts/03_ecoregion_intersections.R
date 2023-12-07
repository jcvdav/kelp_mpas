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
  tictoc,
  tidyverse)

# Load data --------------------------------------------------------------------
kelp <- st_read(here("data", "processed", "clean_kelp.gpkg"))

mpas <- st_read(here("data", "processed", "clean_mpas.gpkg"))

meow <- st_read(here("data", "raw", "clean_meow.gpkg")) %>% 
  select(-a)

# Define some functions --------------------------------------------------------
st_erase <- function(x, y) {
  # Errase all of y from x
  st_difference(x, st_union(st_make_valid(st_combine(y))))
}

fast_intersect <- function(x, y) {
  # browser()
  # Step 1 - make polygons valid and add unique identifiers
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
  # This is the first step in identifying kelp patches that toruch / dont touch an mpa
  # and MPAs that touch / don't touch kelp. There is no point in doing the intersection between all
  join_coverage <- st_join(x, y)
  
  # Identify the kelp polygons that touch an MPA
  kelp_inside <- join_coverage %>%
    st_drop_geometry() %>%
    filter(!is.na(y_id)) %>%
    pull(x_id)
  
  # Identify lelp patches that don't touch any MPA
  kelp_outside <- join_coverage %>%
    filter(!x_id %in% kelp_inside) %>% # Remove the ones that lie inside MPA
    # Assign categories, so null sicne it's outside
    mutate(lfp_cat = "None",
           lfp_group = "None",
           lfp = 0)
  
  # Identify MPAs that touch some kelp
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
  
  # Step 3 - Intersect kelp polygons known to touch an MPA with MPAs known to
  # touch a keelp polygon. This brakes up kelp polygons, so we'll then need to
  # find any kelp patches that were only partially protected
  int_coverage <- st_intersection(xx, yy) %>%
    st_make_valid() %>%
    filter(st_is_valid(.))
    # filter(!st_is_empty(.)) #%>% 
    # mutate(kelp_area = as.numeric(st_area(.)))
  
  # Ste p 4 - The anti-intersection part. This identifies kelp patches that were
  # partially protected. That is, that part of it actually falls inside an MPA
  # (identified in the intersection above) but part of it doesnt
  not_int_coverage <- st_erase(xx, yy) %>%
    st_make_valid() %>%
    filter(st_is_valid(.)) %>% 
    # Assign categories, so null since it's outside
    mutate(lfp_cat = "None",
           lfp_group = "None",
           lfp = 0)
  
  # Step 5 - Bring it all together
  # Combine with the previous ones
  combined <- bind_rows(
    kelp_outside, # Kelp known to not touch an MPA
    int_coverage, # Kelp that intersects with an MPA
    not_int_coverage # Kelp that was part of a patch that touched an MPA, but that does not directly fall within the MPA
    ) %>% 
    select(country, x_id, y_id, mpa_id, lfp, lfp_cat, lfp_group)
  
  return(combined)
}

## PROCESSING ##################################################################

# Assemble nested tibbles ------------------------------------------------------
nested_kelp <- kelp %>%
  group_by(country) %>% 
  group_split()

nested_mpas <- mpas %>%
  group_by(country) %>% 
  group_split()

# Begin intersection -----------------------------------------------------------
future::plan("multisession",
             workers = 12)
tic()
kelp_and_mpa <- future_map2_dfr(nested_kelp,
                                nested_mpas,
                                fast_intersect)
toc()
beepr::beep(2)

plan(sequential)


sf_use_s2(FALSE) # Turn off to intersect faster
intersect_kelp_mpa_and_ecoregion <- st_intersection(st_make_valid(kelp_and_mpa), meow)

sf_use_s2(TRUE)  # Turn on to calculate areas
kelp_mpa_and_ecoregion <- intersect_kelp_mpa_and_ecoregion %>% 
  mutate(kelp_area_km2 = units::set_units(st_area(.), "km^2"))

# EXPORT #######################################################################
saveRDS(kelp_mpa_and_ecoregion,
        file = here("data", "output", "intersected_kelp_mpa_ecoregion.rds"))

st_write(obj = kelp_mpa_and_ecoregion,
         dsn = here("data", "output", "intersected_kelp_mpa_ecoregion.gpkg"),
         delete_dsn = T)
