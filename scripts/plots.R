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
library(cowplot)
library(tidyverse)


# Load data --------------------------------------------------------------------
kelp_mpa_and_realm <-
  readRDS(file = here("data", "output", "intersected_kelp_mpa_realm.rds"))

meow <- st_read(here("data", "raw", "clean_meow.gpkg")) %>% 
  group_by(realm, rlm_code) %>% 
  summarize(a = 1) %>% 
  ungroup() %>% 
  select(-a)


# Define palette colors --------------------------------------------------------
pal <- c("gray90", "orange", "cadetblue", "steelblue")


## PROCESSING ##################################################################

# Build data at the realm-level ------------------------------------------------
realm_data <- kelp_mpa_and_realm %>%
  st_drop_geometry() %>%
  group_by(realm, status) %>%
  summarize(kelp_area = sum(kelp_area)) %>%
  ungroup() %>%
  group_by(realm) %>%
  mutate(pct_area = kelp_area / sum(kelp_area)) %>%
  ungroup() %>%
  replace_na(replace = list(status = "Outside MPA")) %>%
  mutate(status = fct_relevel(status, c("Outside MPA", "None", "Partial", "Full")))

# Build data at the country-level
country_data <- kelp_mpa_and_realm %>%
  st_drop_geometry() %>%
  group_by(country, status) %>%
  summarize(kelp_area = sum(kelp_area)) %>%
  ungroup() %>%
  group_by(country) %>%
  mutate(pct_area = kelp_area / sum(kelp_area)) %>%
  ungroup() %>%
  replace_na(replace = list(status = "Outside MPA")) %>%
  mutate(status = fct_relevel(status, c("Outside MPA", "None", "Partial", "Full")))

cr_data <- kelp_mpa_and_realm %>%
  st_drop_geometry() %>%
  group_by(realm, rlm_code, country, status) %>%
  summarize(kelp_area = sum(kelp_area)) %>%
  ungroup() %>%
  mutate(pct_area = kelp_area / sum(kelp_area)) %>%
  ungroup() %>%
  replace_na(replace = list(status = "Outside MPA")) %>%
  group_by(country) %>% 
  mutate(pct_country_fully_protected = sum(pct_area)) %>% 
  ungroup() %>% 
  group_by(realm) %>% 
  mutate(pct_realm_fully_protected = sum(pct_area)) %>% 
  ungroup() %>% 
  mutate(status = fct_relevel(status, c("Outside MPA", "None", "Partial", "Full")),
         country = fct_reorder(country, pct_country_fully_protected),
         realm = str_replace_all(realm, " ", "\n"),
         realm = fct_reorder(realm, pct_realm_fully_protected))

## VISUALIZE ###################################################################

# X ----------------------------------------------------------------------------

## EXPORT ######################################################################
labs <- tibble(
  x = "ARG",
  y = c(0.1, 0.3) + 0.05,
  label = c("10%", "30%")
)

labs <- tibble(
  x = 1,
  y = c(0.1, 0.3) + 0.05,
  label = c("10%", "30%")
)

realm_plot <- ggplot(data = realm_data) +
  geom_col(aes(x = realm, y = pct_area, fill = status),
           color = "black",
           size = 0.1) +
  geom_hline(yintercept = 0, size = 0.3) +
  geom_hline(yintercept = 0.1,
             linetype = "dotted",
             size = 0.3) +
  geom_hline(yintercept = 0.3,
             linetype = "dashed",
             size = 0.3) +
  geom_hline(yintercept = 1, size = 0.3) +
  scale_fill_manual(values = pal) +
  scale_y_continuous(limits = c(-0.25, NA),
                     expand = c(0, 0),
                     labels = NULL) +
  geom_text(
    data = labs,
    aes(x = x, y = y, label = label),
    size = 3,
    inherit.aes = F
  ) +
  coord_polar() +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        axis.text = element_text(size = 7),
        legend.position = "None") +
  labs(x = "",
       y = "",
       fill = "Fishing restrictions")

country_plot <- ggplot(data = country_data) +
  geom_col(aes(x = country, y = pct_area, fill = status),
           color = "black",
           size = 0.1) +
  geom_hline(yintercept = 0, size = 0.3) +
  geom_hline(yintercept = 0.1,
             linetype = "dotted",
             size = 0.3) +
  geom_hline(yintercept = 0.3,
             linetype = "dashed",
             size = 0.3) +
  geom_hline(yintercept = 1, size = 0.3) +
  scale_fill_manual(values = pal) +
  scale_y_continuous(limits = c(-0.25, NA),
                     expand = c(0, 0),
                     labels = NULL) +
  geom_text(
    data = labs,
    aes(x = x, y = y, label = label),
    size = 3,
    inherit.aes = F
  ) +
  coord_polar() +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        axis.text = element_text(size = 7),
        legend.position = "None") +
  labs(x = "",
       y = "",
       fill = "Fishing restrictions")

alluvial <- ggplot(data = cr_data,
       aes(axis1 = country, axis2 = realm,
           y = pct_area)) +
  geom_alluvium(aes(fill = status), color = "black", size = 0.1, alpha = 0.75) +
  geom_stratum(width = 1/3, size = 0.3) +
  geom_text(stat = "stratum",
            aes(label = after_stat(stratum)),
            parse = F,
            size = 3) +
  scale_x_discrete(limits = c("Country", "Realm"), expand = c(0, 0)) +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal() +
  scale_fill_manual(values = pal) +
  theme(legend.position = "top") +
  coord_flip() +
  labs(y = "% Area",
       fill = "Fishing restriction\nof MPA")


props <- plot_grid(realm_plot,
                   country_plot,
                   ncol = 1)

plot_grid(alluvial,
          props,
          ncol = 2,
          rel_widths = c(2, 1))


#
#
# # Errors
#
# true_area <- kelp %>%
#   st_drop_geometry() %>%
#   group_by(country) %>%
#   summarize(true_area = sum(kelp_area))
#
# other_area <- results %>%
#   select(country, summary) %>%
#   unnest(summary) %>%
#   ungroup() %>%
#   group_by(country) %>%
#   summarize(other_area = sum(kelp_area))
#
# left_join(true_area, other_area, by = "country") %>%
#   filter(true_area < other_area) %>%
#   pull(country)
#
# ## EXPORT ######################################################################
#
# # X ----------------------------------------------------------------------------
# saveRDS(results,
#         file = here("data", "output", "kelp_mpa_intersected.rds"))
#
#
#
#
# # Ejemplo de duplicados en Mexico
# results %>%
#   filter(country == "MEX") %>%
#   select(country, data) %>%
#   unnest(data) %>%
#   # filter(x_id %in% 2353:2359) %>%
#   st_as_sf() %>%
#   mapview(zcol = "status")
#
# # Diagnósitco
# mpas %>%
#   filter(country == "MEX") %>%
#   mutate(p_id = 1:nrow(.)) %>%
#   mapview(zcol = "p_id")
#
# mpas %>%
#   filter(country == "MEX") %>%
#   mutate(p_id = 1:nrow(.),
#          status = fct_relevel(status, c("None", "Partial", "Full"))) %>%
#   arrange(desc(status)) %>%
#   st_intersection() %>%
#   select(-origins) %>%
#   mapview(zcol = "p_id")
#
#
# ################################
# # Ejemplo de terrestres en USA
# mpas %>%
#   filter(country == "ZAF") %>%
#   mutate(p_id = 1:nrow(.)) %>%
#   mapview(zcol = "status")
#
# mpas %>%
#   filter(country == "NZL") %>%
#   mutate(p_id = 1:nrow(.)) %>%
#   arrange(status) %>%
#   # st_set_precision(1000) %>%
#   st_make_valid() %>%
#   st_intersection() %>%
#   select(-origins) %>%
#   filter(!st_geometry_type(.) == "POINT") %>%
#   mapview(zcol = "p_id")
