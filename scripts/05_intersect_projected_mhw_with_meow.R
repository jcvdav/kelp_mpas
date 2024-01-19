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
  cowplot,
  tidyverse
)

# From https://www.ipcc.ch/site/assets/uploads/2022/09/IPCC_AR6_WGI_VisualStyleGuide_2022.pdf
# Page 9
ssp_palette <- c("SSP1-2.6" = "#173c66",
                 "SSP2-4.5" = "#f79420", 
                 "SSP5-8.5" = "#951b1e")

# Set a global theme -----------------------------------------------------------
ggplot2::theme_set(
  ggplot2::theme_bw()
)
ggplot2::theme_update(
  axis.title.y = ggplot2::element_text(size = 10),
  axis.title.x = ggplot2::element_text(size = 10),
  axis.text.y = ggplot2::element_text(size = 8),
  axis.text.x = ggplot2::element_text(size = 8),
  panel.background = ggplot2::element_blank(),
  plot.background = ggplot2::element_blank(),
  legend.background = ggplot2::element_blank(),
  legend.key = ggplot2::element_blank(),
  panel.border = ggplot2::element_blank(),
  panel.grid.minor = ggplot2::element_blank(),
  panel.grid.major.x = ggplot2::element_blank(),
  panel.grid.major.y = ggplot2::element_line(colour = "black",
                                             linewidth = 0.1),
  axis.line = element_line(color = "black",
                           linewidth = 0.5),
  strip.background = ggplot2::element_blank()
)


# Load data --------------------------------------------------------------------
meow <- st_read(here("data", "raw", "clean_meow.gpkg")) %>% 
  select(-a)

ssp126 <- read_csv(here("data", "output", "MHW", "ssp126_median_01_24.csv")) %>% 
  rename(lon = x, lat = y) %>% 
  pivot_longer(cols = c(3:121),
               names_to = "year",
               values_to = "SSP1-2.6") %>% 
  mutate(year = as.numeric(year))

ssp245 <- read_csv(here("data", "output", "MHW", "ssp245_median_01_24.csv")) %>% 
  rename(lon = x, lat = y) %>% 
  pivot_longer(cols = c(3:121),
               names_to = "year",
               values_to = "SSP2-4.5") %>% 
  mutate(year = as.numeric(year))

ssp585 <- read_csv(here("data", "output", "MHW", "ssp585_median_01_24.csv")) %>% 
  rename(lon = x, lat = y) %>% 
  pivot_longer(cols = c(3:121),
               names_to = "year",
               values_to = "SSP5-8.5") %>% 
  mutate(year = as.numeric(year))

## PROCESSING ##################################################################

# Get unique combinations-------------------------------------------------------
# This means I do a single join, instead of n_years * n_scenarios
grid <- ssp126 %>% 
  select(lon, lat) %>% 
  distinct() %>% 
  st_as_sf(coords = c("lon", "lat"),
           crs = 4326) %>% 
  st_join(meow) %>% 
  mutate(realm = fct_relevel(realm, 
                             "Arctic",
                             "Temperate Northern Pacific",
                             "Temperate South America",
                             "Southern Ocean",
                             "Temperate Southern Africa",
                             "Temperate Australasia"))

# X ----------------------------------------------------------------------------
data <- ssp126 %>% 
  left_join(ssp245, by = c("lat", "lon", "year")) %>% 
  left_join(ssp585, by = c("lat", "lon", "year")) %>% 
  pivot_longer(cols = 4:6,
               names_to = "ssp",
               values_to = "MHW") %>% 
  left_join(grid %>% 
              bind_cols(st_coordinates(.)) %>% 
              st_drop_geometry() %>% 
              select(lon = X, lat = Y, everything()), by = c("lat", "lon"))   # add ecoregion info

# Define a function to calculate percentiles
pct_range <- function(x, pct = 0.95) {
  l_lim <- (1 - pct) / 2
  u_lim <- pct + l_lim
  
  pcts <- quantile(x, probs = c(l_lim, u_lim))
  
  tibble(ymin = min(pcts),
         ymax = max(pcts))
}

## VISUALIZE ###################################################################

# Build the main plot ----------------------------------------------------------
plot <- ggplot(data = data,
       mapping = aes(x = year, y = MHW, color = ssp, fill = ssp, group = ssp)) +
  stat_summary(geom = "ribbon",
               fun.data = pct_range,
               fun.args = list(pct = 0.90),
               color = "transparent",
               alpha = 0.25) +
  stat_summary(geom = "line", fun = "median") +
  scale_color_manual(values = ssp_palette, aesthetics = c("fill", "color")) +
  guides(color = guide_legend(title.position = "top", title.hjust = 0.5,
                              title = "SSP"),
         fill = "none") +
  theme(legend.position = "bottom") +
  labs(x = "Year",
       y = "Median Cumulative Marine Heatwave Intensity (°C days)")

# And the sub-plots
realm <- plot + 
  facet_wrap(~realm,
             ncol = 3, dir = "v")

province <- plot + 
  facet_wrap(~province,
             ncol = 3)

ecoregion <- plot + 
  facet_wrap(~ecoregion,
             ncol = 5)

# MAP
realms <- meow %>% 
  filter(ecoregion %in% unique(data$ecoregion)) %>% 
  group_by(realm, ecoregion) %>% 
  summarise(a = 1, .groups = "drop") %>% 
  select(realm, ecoregion) %>% 
  mutate(realm = fct_relevel(realm, 
                             "Arctic",
                             "Temperate Northern Pacific",
                             "Temperate South America",
                             "Southern Ocean",
                             "Temperate Southern Africa",
                             "Temperate Australasia"))

world <- rnaturalearth::ne_countries(returnclass = "sf")

map <- ggplot() +
  geom_sf(data = realms,
          mapping = aes(fill = realm),
          color = "black",
          size = 0.1,
          alpha = 0.75) +
  geom_sf(data = grid,
          color = "black",
          size = 1) +
  geom_sf(data = world,
          color = "black",
          fill = "gray",
          linewidth = 0.5) +
  geom_sf(data = world,
          color = "gray",
          fill = "gray",
          linewidth = 0.1) +
  scale_fill_brewer(palette = "Set3") +
  guides(fill = guide_legend(title.position = "top",
                             title.hjust = 0.5,
                             title = "Realm")) +
  theme_void() +
  theme(legend.position = "bottom")

fig <- plot_grid(map, realm,
                 ncol = 1,
                 labels = "auto")


## EXPORT ######################################################################

# X ----------------------------------------------------------------------------
ggsave(plot = plot,
       filename = here("img", "MHW_global.pdf"),
       width = 12,
       height = 6)

ggsave(plot = fig,
       filename = here("img", "Map_and_mhw_by_realm.pdf"),
       width = 8,
       height = 10)

ggsave(plot = realm,
       filename = here("img", "MHW_realm.pdf"),
       width = 12,
       height = 6)

ggsave(plot = province,
       filename = here("img", "MHW_province.pdf"),
       width = 12,
       height = 12)

ggsave(plot = ecoregion,
       filename = here("img", "MHW_ecoregion.pdf"),
       width = 12,
       height = 12)
