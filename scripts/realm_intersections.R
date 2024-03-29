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
pacman::p_load(
  here,
  sf,
  future,
  furrr,
  tidyverse
)

# Turn off spatial sphere
# sf_use_s2(FALSE) 

# Define some functions --------------------------------------------------------
fast_intersect <- function(x, y) {
  x <- x %>% 
    st_make_valid() %>% 
    filter(st_is_valid(.)) %>% 
    mutate(x_id = 1:nrow(.))
  
  y <- y %>% 
    st_make_valid() %>% 
    filter(st_is_valid(.)) %>% 
    mutate(y_id = 1:nrow(.))
  
  join_coverage <- st_join(x, y)
  
  kelp_needs_intersection <- join_coverage %>%
    st_drop_geometry() %>%
    filter(!is.na(y_id)) %>%
    pull(x_id)

  mpa_needs_intersection <- join_coverage %>%
    st_drop_geometry() %>%
    filter(!is.na(y_id)) %>%
    pull(y_id)

  xx <- x %>%
    filter(x_id %in% kelp_needs_intersection)

  yy <- y %>%
    filter(y_id %in% mpa_needs_intersection) 

  int_coverage <- st_intersection(xx, yy) %>%
    ungroup() %>%
    st_make_valid() %>%
    # filter(st_is_valid(.)) %>%
    filter(!st_is_empty(.))

  combined <- join_coverage %>%
    filter(!x_id %in% kelp_needs_intersection) %>%
    bind_rows(int_coverage) %>% 
    replace_na(replace = list(status = "Outside MPA"))

  return(combined)
}

# Load data --------------------------------------------------------------------

kelp <- st_read(here("data", "processed", "clean_kelp.gpkg"))

mpas <- st_read(here("data", "processed", "clean_mpas.gpkg"))

meow <- st_read(here("data", "raw", "clean_meow.gpkg")) %>% 
  group_by(realm) %>%
  summarize(a = 1) %>%
  ungroup() %>%
  select(-a)


## PROCESSING ##################################################################

# Assamble nested tibbles ------------------------------------------------------
nested_kelp <- kelp %>%
  group_by(country) %>% 
  nest() %>% 
  rename(k = data)

nested_mpas <- mpas %>%
  group_by(country) %>% 
  nest() %>% 
  rename(m = data)

# Begin intersection -----------------------------------------------------------
plan(multisession, workers = 12)

kelp_and_mpa <- left_join(nested_kelp,
                     nested_mpas, by = "country") %>%
  mutate(data = future_map2(.x = k, .y = m, .f = fast_intersect,
                            .options = furrr_options(seed = T),
                            .progress = TRUE))

plan(sequential)

kelp_and_mpa_unnested <- kelp_and_mpa %>% 
  select(country, data) %>% 
  unnest(data) %>% 
  ungroup() %>% 
  st_sf() 

kelp_mpa_and_realm <- kelp_and_mpa_unnested %>% 
  st_intersection(meow) %>% 
  mutate(kelp_area = as.numeric(st_area(.)))

# EXPORT #######################################################################
saveRDS(kelp_mpa_and_realm,
        file = here("data", "output", "intersected_kelp_mpa_realm.rds"))

st_write(obj = kelp_mpa_and_realm,
         dsn = here("data", "output", "intersected_kelp_mpa_realm.gpkg"),
         delete_dsn = T)