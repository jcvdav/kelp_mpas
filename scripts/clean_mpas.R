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

sf_use_s2(FALSE)

# Find files -------------------------------------------------------------------
files <- list.files(
  path = here("data", "raw", "Final_MPAs"),
  pattern = "shp$",
  full.names = T
)

# Read all files into a single sf object ---------------------------------------
raw_mpas <- tibble(file = files,
                   basename = basename(file)) %>%
  mutate(
    data = map(file, st_read),
    data = map(data, st_zm, drop = T),
    data = map(data, st_transform, crs = "EPSG:4326")
  ) %>%
  unnest(data, names_repair = "universal") %>%
  ungroup() %>%
  st_as_sf()

## PROCESSING ##################################################################
# Notas
# Hay algunas de Mexico y NZ sin fecha


# Clean data -------------------------------------------------------------------
clean_mpas <- raw_mpas %>%
  mutate(
    ANO = as.numeric(ANO),
    GAZ_DATE = as.numeric(str_extract(GAZ_DATE, pattern = "[:digit:]{4}")),
    anp_felec = as.numeric(str_extract(anp_felec, pattern = "[:digit:]{4}")),
    F_DCTOAPRU = as.numeric(str_extract(F_DCTOAPRU, pattern = "[:digit:]{4}")),
    Establishe = as.numeric(str_extract(Establishe, "[:digit:]{4}")),
    D_DCLAR = as.numeric(str_extract(D_DCLAR, "[:digit:]{4}")),
    year = coalesce(
      ANO,
      GAZ_DATE,
      anp_felec,
      F_DCTOAPRU,
      Establishe,
      D_DCLAR,
      PROTDATE,
      Estab_Yr,
      Estab_Yr_1,
      status_yea
    ),
    Status = coalesce(Status, Status_1)
  ) %>%
  select(basename, year, status = Status) %>%
  mutate(
    status = fct_relevel(status, c("None", "Partial", "Full")),
    country = str_remove_all(basename, "MPAs_|.shp|[:digit:]+"),
    year_missing = is.na(year),
    year = coalesce(year, 2022)
  ) %>%
  arrange(desc(status)) %>%
  st_make_valid() %>%
  # st_set_precision(10000) %>%
  # st_make_valid() %>%
  st_difference() %>%
  st_make_valid() %>%
  filter(!st_geometry_type(.) == "GEOMETRYCOLLECTION") %>%
  mutate(
    country = case_when(
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
      country == "USA" ~ "USA"
    )
  ) %>%
  select(basename, country, year, year_missing, status)

## EXPORT ######################################################################

# X ----------------------------------------------------------------------------
st_write(
  obj = clean_mpas,
  dsn = here("data", "processed", "clean_mpas.gpkg"),
  delete_dsn = T
)
