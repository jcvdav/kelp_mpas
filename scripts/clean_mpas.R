################################################################################
# title
################################################################################
#
# Juan Carlos Villaseñor-Derbez
# juancvd@stanford.edu
# date
#
# Find overlaps
#
################################################################################

## SET UP ######################################################################

# Load packages ----------------------------------------------------------------
library(here)
library(sf)
library(tidyverse)

# Find files --------------------------------------------------------------------
files <- list.files(path = here("data", "raw", "Final_MPAs"),
                    pattern = "shp$",
                    full.names = T)

## PROCESSING ##################################################################


# Notas
# Hay algunas de Mexico y NZ sin fecha


# Combine all shapefiles -------------------------------------------------------
clean_mpas <- tibble(file = files,
                     basename = basename(file)) %>% 
  mutate(data = map(file, st_read),
         data = map(data, st_zm, drop = T),
         data = map(data, st_transform, crs = "EPSG:4326")) %>% 
  unnest(data, names_repair = "universal") %>% 
  ungroup() %>% 
  st_as_sf() %>% 
  mutate(ANO = as.numeric(ANO),
         GAZ_DATE = as.numeric(str_extract(GAZ_DATE, pattern = "[:digit:]{4}")),
         anp_felec = as.numeric(str_extract(anp_felec, pattern = "[:digit:]{4}")),
         F_DCTOAPRU = as.numeric(str_extract(F_DCTOAPRU, pattern = "[:digit:]{4}")),
         Establishe = as.numeric(str_extract(Establishe, "[:digit:]{4}")),
         D_DCLAR = as.numeric(str_extract(D_DCLAR, "[:digit:]{4}")),
         year = coalesce(ANO,
                         GAZ_DATE,
                         anp_felec,
                         F_DCTOAPRU,
                         Establishe,
                         D_DCLAR,
                         PROTDATE,
                         Estab_Yr,
                         Estab_Yr_1,
                         status_yea),
         Status = coalesce(Status, Status_1)) %>% 
  select(basename, year, status = Status) %>% 
  st_make_valid() %>% 
  mutate(status = fct_relevel(status, c("None", "Partial", "Full"))) %>%
  arrange(desc(status)) %>% 
  st_set_precision(10000) %>%
  st_make_valid() %>% 
  st_intersection() %>%
  st_make_valid() %>% 
  filter(st_is_valid(.)) %>% 
  mutate(year_missing = is.na(year),
         country = str_remove_all(basename, "MPAs_|.shp|[:digit:]+"),
         mpa_area = st_area(.)) %>% 
  mutate(country = case_when(
    country == "Argentina" ~ "ARG",
    country == "Australia" ~ "AUS",
    country == "Canada" ~ "CAN",
    country == "Chile" ~ "CHL",
    country == "French" ~ "FRA",
    country == "Mexico" ~ "MEX",
    country == "Namibia" ~ "NAM",
    country == "NewZealand" ~ "NZL",
    country == "Peru" ~ "PER",
    country == "South_Africa" ~ "ZAF", 
    country == "UK" ~ "GBR",
    country == "USA" ~ "USA"))%>% 
  replace_na(replace = list(year = 2022)) %>% 
  select(basename, country, year, year_missing, status, mpa_area)

## EXPORT ######################################################################

# X ----------------------------------------------------------------------------
st_write(obj = clean_mpas,
         dsn = here("data", "processed", "clean_mpas.gpkg"),
         delete_dsn = T)

