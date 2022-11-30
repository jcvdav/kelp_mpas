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
library(furrr)
library(tidyverse)

# Load data --------------------------------------------------------------------

kelp <- st_read(here("data", "processed", "clean_kelp.gpkg"))

mpas <- st_read(here("data", "processed", "clean_mpas.gpkg"))

meow <- st_read(here("data", "raw", "clean_meow.gpkg")) %>% 
  select(-a)

## PROCESSING ##################################################################

# X ----------------------------------------------------------------------------


fast_intersect <- function(x, y) {
  x <- x %>% 
    mutate(x_id = 1:nrow(.))
  y <- y %>% 
    mutate(y_id = 1:nrow(.))
  
  join_coverage <- st_join(x, y)
  
  needs_intersection <- join_coverage %>% 
    st_drop_geometry() %>% 
    filter(!is.na(y_id)) %>% 
    pull(x_id)
  
  int_coverage <- x %>% 
    filter(x_id %in% needs_intersection) %>% 
    st_intersection(y) %>% 
    ungroup() %>% 
    st_make_valid() %>% 
    filter(st_is_valid(.)) %>% 
    mutate(kelp_area = as.numeric(st_area(.)))
  
  combined <- join_coverage %>% 
    filter(!x_id %in% needs_intersection) %>% 
    bind_rows(int_coverage)
  
  return(combined)
}

get_summary <- function(x){
  x %>% 
    st_drop_geometry() %>% 
    ungroup() %>% 
    replace_na(replace = list(status = "Outside MPA")) %>% 
    group_by(status) %>% 
    summarize(kelp_area = sum(kelp_area, na.rm = T)) %>% 
    ungroup() %>% 
    mutate(pct_area = as.numeric(kelp_area / sum(kelp_area, na.rm = T)))
}

get_summary_byyear <- function(x){
  x %>% 
    st_drop_geometry() %>% 
    ungroup() %>% 
    replace_na(replace = list(status = "Outside MPA")) %>% 
    group_by(year, status) %>% 
    summarize(kelp_area = sum(kelp_area, na.rm = T)) %>% 
    ungroup() %>% 
    mutate(pct_area = as.numeric(kelp_area / sum(kelp_area, na.rm = T)))
}

k <- kelp %>%
  group_by(country) %>% 
  nest() %>% 
  rename(k = data)

m <- mpas %>%
  group_by(country) %>% 
  nest() %>% 
  rename(m = data)

plan(multisession, workers = 12)
tic()
results <- left_join(k, m, by = "country") %>%
  mutate(data = future_map2(.x = k, .y = m, .f = fast_intersect),
         summary = future_map(data, get_summary),
         summary_by_year = future_map(data, get_summary_byyear))
beep()
toc()
plan(sequential)


## VISUALIZE ###################################################################

# X ----------------------------------------------------------------------------

pal <- c("gray90", "orange", "cadetblue", "steelblue")

labs <- tibble(x = "ARG",
               y = c(0.1, 0.3) + 0.05,
               label = c("10%", "30%"))

results %>%
  select(country, summary) %>%
  unnest(summary) %>% 
  ungroup() %>% 
  mutate(status = fct_relevel(status, c("Outside MPA", "None", "Partial", "Full"))) %>% 
  ggplot() + 
  geom_col(aes(x = country, y = pct_area, fill = status),
           color = "black", size = 0.1) +
  geom_hline(yintercept = 0, size = 0.3) +
  geom_hline(yintercept = 0.1, linetype = "dotted", size = 0.3) +
  geom_hline(yintercept = 0.3, linetype = "dashed", size = 0.3) +
  geom_hline(yintercept = 1, size = 0.3) +
  scale_fill_manual(values = pal) +
  scale_y_continuous(limits = c(-0.5, NA),
                     expand = c(0, 0),
                     labels = NULL) +
  geom_text(data = labs, aes(x = x, y = y, label = label),
            size = 3,
            inherit.aes = F) +
  coord_polar() +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        axis.text = element_text(size = 7)) +
  labs(x = "",
       y = "",
       fill = "Protection status")

results %>%
  select(country, summary) %>%
  unnest(summary) %>% 
  ungroup() %>% 
  select(country, status, pct_area) %>% 
  pivot_wider(names_from = status, values_from = pct_area)


# Errors

true_area <- kelp %>%
  st_drop_geometry() %>%
  group_by(country) %>%
  summarize(true_area = sum(kelp_area))

other_area <- results %>%
  select(country, summary) %>%
  unnest(summary) %>% 
  ungroup() %>% 
  group_by(country) %>%
  summarize(other_area = sum(kelp_area))

left_join(true_area, other_area, by = "country") %>% 
  filter(true_area < other_area) %>% 
  pull(country)

## EXPORT ######################################################################

# X ----------------------------------------------------------------------------

saveRDS(results,
        file = here("data", "output", "kelp_mpa_intersected.rds"))




# Ejemplo de duplicados en Mexico
results %>% 
  filter(country == "MEX") %>% 
  select(country, data) %>% 
  unnest(data) %>% 
  # filter(x_id %in% 2353:2359) %>% 
  st_as_sf() %>% 
  mapview(zcol = "status")

# Diagnósitco
mpas %>%
  filter(country == "MEX") %>%
  mutate(p_id = 1:nrow(.)) %>%
  mapview(zcol = "p_id")

mpas %>%
  filter(country == "MEX") %>%
  mutate(p_id = 1:nrow(.),
         status = fct_relevel(status, c("None", "Partial", "Full"))) %>%
  arrange(desc(status)) %>% 
  st_intersection() %>%
  select(-origins) %>%
  mapview(zcol = "p_id")


################################
# Ejemplo de terrestres en USA
mpas %>% 
  filter(country == "ZAF") %>% 
  mutate(p_id = 1:nrow(.)) %>%
  mapview(zcol = "status")

mpas %>% 
  filter(country == "NZL") %>% 
  mutate(p_id = 1:nrow(.)) %>%
  arrange(status) %>% 
  # st_set_precision(1000) %>%
  st_make_valid() %>%
  st_intersection() %>%
  select(-origins) %>%
  filter(!st_geometry_type(.) == "POINT") %>% 
  mapview(zcol = "p_id")
