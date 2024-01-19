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
  cowplot,
  ggrepel,
  sf,
  ggalluvial,
  tidyverse 
)


# Load data --------------------------------------------------------------------
kelp_mpa_and_ecoregion <-
  readRDS(file = here("data", "output", "intersected_kelp_mpa_ecoregion.rds"))

# Define palette colors --------------------------------------------------------
pal <- c("None" = "gray90",
         "Least"  = "#1e699d",
         "Less"  = "#0d9948",
         "Moderately"  = "#faec27",
         "Heavily"  = "#eca929",
         "Most"  = "#dd3c40")


## PROCESSING ##################################################################

# format data ------------------------------------------------------------------
kelp_mpa_and_ecoregion_data <- kelp_mpa_and_ecoregion %>% 
  st_drop_geometry() %>%
  mutate(lfp_cat = fct_relevel(lfp_cat, c("None", "Least", "Less", "Moderately", "Heavily", "Most")),
         lfp_group = fct_relevel(lfp_group, c("None", "Less", "Moderately", "Highly")),
         realm = str_replace_all(realm, " ", "\n"),
         kelp_area_km2 = as.numeric(kelp_area_km2))

# Build data at the realm-level ------------------------------------------------
realm_data <- kelp_mpa_and_ecoregion_data %>%
  group_by(realm, lfp_cat) %>%
  summarize(kelp_area_km2 = sum(kelp_area_km2)) %>%
  ungroup() %>%
  group_by(realm) %>%
  mutate(pct_area = kelp_area_km2 / sum(kelp_area_km2)) %>%
  ungroup()

# Build data at the ecoregion-level
ecoregion_data <- kelp_mpa_and_ecoregion_data %>%
  group_by(ecoregion, lfp_cat) %>%
  summarize(kelp_area_km2 = sum(kelp_area_km2)) %>%
  ungroup() %>%
  group_by(ecoregion) %>%
  mutate(pct_area = kelp_area_km2 / sum(kelp_area_km2)) %>%
  ungroup()

# Build data at the country-level
country_data <- kelp_mpa_and_ecoregion_data %>%
  group_by(country, lfp_cat) %>%
  summarize(kelp_area_km2 = sum(kelp_area_km2)) %>%
  ungroup() %>%
  group_by(country) %>%
  mutate(pct_area = kelp_area_km2 / sum(kelp_area_km2)) %>%
  ungroup()

cr_data <- kelp_mpa_and_ecoregion_data %>%
  group_by(ecoregion,
           province,
           realm, country, lfp_group, lfp_cat) %>%
  summarize(kelp_area_km2 = sum(kelp_area_km2), .groups = "drop") %>%
  ungroup() %>%
  mutate(pct_area = kelp_area_km2 / sum(kelp_area_km2)) %>%
  ungroup() %>%
  group_by(ecoregion) %>%
  mutate(pct_eco = sum(pct_area)) %>%
  ungroup() %>%
  group_by(province) %>%
  mutate(pct_pro = sum(pct_area)) %>%
  ungroup() %>%
  group_by(realm) %>% 
  mutate(pct_realm = sum(pct_area)) %>% 
  ungroup() %>% 
  group_by(country) %>% 
  mutate(pct_country = sum(pct_area)) %>% 
  ungroup() %>% 
  mutate(country = fct_reorder(country, pct_country),
         realm = fct_reorder(realm, pct_realm),
         province = fct_reorder(province, pct_pro),
         ecoregion = fct_reorder(ecoregion, pct_eco))

## VISUALIZE ###################################################################

# X ----------------------------------------------------------------------------
labs <- tibble(
  x = 1,
  y = c(0.1, 0.3) + 0.05,
  label = c("10%", "30%")
)

realm_plot <- ggplot(data = realm_data) +
  geom_col(aes(x = realm, y = pct_area, fill = lfp_cat),
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
  geom_text(data = realm_data %>%
              select(realm) %>%
              distinct(),
            mapping = aes(x = realm, y = 0.75, label = realm),
            inherit.aes = F,
            size = 2
            ) +
  scale_x_discrete(labels = NULL) +
  scale_y_continuous(limits = c(-0.25, NA),
                     expand = c(0, 0),
                     labels = NULL) +
  scale_fill_manual(values = pal) +
  geom_text(
    data = labs,
    aes(x = x, y = y, label = label),
    size = 2,
    inherit.aes = F
  ) +
  coord_polar() +
  theme_void() +
  theme(legend.position = "None") +
  labs(x = "",
       y = "",
       fill = "Fishing restrictions")

country_plot <- ggplot(data = country_data) +
  geom_col(aes(x = country, y = pct_area, fill = lfp_cat),
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
  geom_text(data = country_data %>%
              select(country) %>%
              distinct(),
            mapping = aes(x = country, y = 0.75, label = country),
            inherit.aes = F,
            size = 2
  ) +
  scale_x_discrete(labels = NULL) +
  scale_y_continuous(limits = c(-0.25, NA),
                     expand = c(0, 0),
                     labels = NULL) +
  scale_fill_manual(values = pal) +
  geom_text(
    data = labs,
    aes(x = x, y = y, label = label),
    size = 2,
    inherit.aes = F
  ) +
  coord_polar() +
  theme_void() +
  theme(axis.text = element_text(size = 7),
        legend.position = "None") +
  labs(x = "",
       y = "",
       fill = "Fishing restrictions")

alluvial_plot <- cr_data %>% 
  select(realm, country, lfp_group, lfp_cat, pct_area) %>%
  distinct() %>%
  group_by(realm, country, lfp_group, lfp_cat) %>%
  summarize(pct_area = sum(pct_area), .groups = "drop") %>%
  ungroup() %>%
  ggplot(aes(axis1 = country,
             axis2 = lfp_group,
             axis3 = realm,
             y = pct_area)) +
  geom_alluvium(aes(fill = lfp_cat),
                color = "black", size = 0.1, alpha = 1) +
  geom_stratum(width = 0.3,
               size = 0.3) +
  geom_text(stat = "stratum",
            aes(label = after_stat(stratum)),
            size = 2) +
  scale_x_discrete(limits = c("Country (ISO3)",
                              "LFP Category",
                              "Realm"),
                   expand = c(0, 0)) +
  scale_y_continuous(breaks = seq(0, 1, by = 0.1),
                     labels = scales::percent,
                     expand = c(0,0)) +
  theme_minimal() +
  theme(panel.grid = element_blank()) +
  scale_fill_manual(values = pal) +
  theme(legend.position = "left", legend.box.spacing = unit(x = 0, units = "mm")) +
  labs(y = "% Area with kelp",
       fill = "LFP score")

props <- plot_grid(realm_plot,
                   country_plot,
                   ncol = 1, 
                   align = "hv",
                   labels = "AUTO")

p <- plot_grid(props,
               alluvial_plot,
               ncol = 2,
               rel_widths = c(1, 4),
               align = "h",
               labels = c("", "C"))

# Export #######################################################################
ggsave(plot = p,
       filename = here("img", "kelp_protection_realm_country.pdf"),
       width = 12,
       height = 8)


write_csv(realm_data,
          file = here("data", "output", "protection_status_by_realm.csv"))

write_csv(ecoregion_data,
          file = here("data", "output", "protection_status_by_ecoregion.csv"))

write_csv(country_data,
          file = here("data", "output", "protection_status_by_country.csv"))

write_csv(cr_data,
          file = here("data", "output", "protection_status_by_country_realm_province_ecoregion.csv"))
