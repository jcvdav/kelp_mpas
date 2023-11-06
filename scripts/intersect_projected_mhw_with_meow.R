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

ssp126 <- read_csv(here("data", "output", "MHW", "ensmedian_ssp126_Cum_MHW_Int_1982-2100.csv")) %>% 
  rename(lon = x, lat = y) %>% 
  pivot_longer(cols = c(3:121),
               names_to = "year",
               values_to = "SSP1-2.6") %>% 
  mutate(year = ymd(year))

ssp126 <- read_csv(here("data", "output", "MHW", "ens10th_ssp126_Cum_MHW_Int_1982-2100.csv")) %>% 
  rename(lon = x, lat = y) %>% 
  pivot_longer(cols = c(3:121),
               names_to = "year",
               values_to = "SSP1-2.6") %>% 
  mutate(year = ymd(year))

ssp245 <- read_csv(here("data", "output", "MHW", "ensmedian_ssp245_Cum_MHW_Int_1982-2100.csv")) %>% 
  rename(lon = x, lat = y) %>% 
  pivot_longer(cols = c(3:121),
               names_to = "year",
               values_to = "SSP2-4.5") %>% 
  mutate(year = ymd(year))

ssp585 <- read_csv(here("data", "output", "MHW", "ensmedian_ssp585_Cum_MHW_Int_1982-2100.csv")) %>% 
  rename(lon = x, lat = y) %>% 
  pivot_longer(cols = c(3:121),
               names_to = "year",
               values_to = "SSP5-8.5") %>% 
  mutate(year = ymd(year))

## PROCESSING ##################################################################

# X ----------------------------------------------------------------------------
grid <- ssp126 %>% 
  select(lon, lat) %>% 
  distinct() %>% 
  st_as_sf(coords = c("lon", "lat"),
           crs = 4326) %>% 
  st_join(meow) %>% 
  bind_cols(st_coordinates(.)) %>% 
  st_drop_geometry() %>% 
  select(lon = X, lat = Y, everything())

# X ----------------------------------------------------------------------------
data <- ssp126 %>% 
  left_join(ssp245, by = c("lat", "lon", "year")) %>% 
  left_join(ssp585, by = c("lat", "lon", "year")) %>% 
  pivot_longer(cols = 4:6,
               names_to = "ssp",
               values_to = "MHW") %>% 
  left_join(grid, by = c("lat", "lon"))

pct_range <- function(x, pct = 0.95) {
  l_lim <- (1 - pct) / 2
  u_lim <- pct + l_lim
  
  pcts <- quantile(x, probs = c(l_lim, u_lim))
  
  tibble(ymin = min(pcts),
         ymax = max(pcts))
}

## VISUALIZE ###################################################################

# X ----------------------------------------------------------------------------
plot <- ggplot(data = data,
       mapping = aes(x = year, y = MHW, color = ssp, fill = ssp, group = ssp)) +
  stat_summary(geom = "ribbon",
               fun.data = pct_range,
               fun.args = list(pct = 0.90),
               color = "transparent",
               alpha = 0.25) +
  stat_summary(geom = "line", fun = "mean") +
  scale_color_manual(values = ssp_palette, aesthetics = c("fill", "color")) +
  guides(color = guide_legend(title.position = "top", title.hjust = 0.5,
                              title = "SSP")) +
  theme(legend.position = "bottom") +
  labs(x = "Year",
       y = "Median Cumulative Marine Heatwave Intensity (°C days)")

realm <- plot + 
  facet_wrap(~realm,
             ncol = 3)

province <- plot + 
  facet_wrap(~province,
             ncol = 3)

ecoregion <- plot + 
  facet_wrap(~ecoregion,
             ncol = 5)


## EXPORT ######################################################################

# X ----------------------------------------------------------------------------
ggsave(plot = realm,
       filename = here("img", "MHW_realm.png"),
       width = 12,
       height = 6)

ggsave(plot = province,
       filename = here("img", "MHW_province.png"),
       width = 12,
       height = 12)

ggsave(plot = ecoregion,
       filename = here("img", "MHW_ecoregion.png"),
       width = 12,
       height = 12)
