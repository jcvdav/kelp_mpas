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
  tidyverse
)

# sf_use_s2(FALSE)

# Load data --------------------------------------------------------------------
raw_kelp <- st_read(here("data", "raw", "kelp", "07-27-23"))

## PROCESSING ##################################################################

# X ----------------------------------------------------------------------------

clean_kelp <- raw_kelp %>%
  st_zm(drop = T) %>%
  st_make_valid() %>%
  filter(!st_is_empty(.)) %>%
  mutate(kelp_area = st_area(.)) %>%
  select(country = Country, kelp_area) %>%
  mutate(
    country = case_when(
      country == "Argentina" ~ "ARG",
      country == "Australia" ~ "AUS",
      country == "British Overseas Territories" ~ "GBR",
      country == "Canda" ~ "CAN",
      country == "Chile" ~ "CHL",
      country == "Mexico" ~ "MEX",
      country == "Namibia" ~ "NAM",
      country == "New Zealand" ~ "NZL",
      country == "Peru" ~ "PER",
      country == "South Africa" ~ "ZAF",
      country == "Southern French Territories" ~ "FRA",
      country == "United States" ~ "USA",
      country == "United States of America" ~ "USA"
    )
  )

## EXPORT ######################################################################

# X ----------------------------------------------------------------------------
st_write(
  clean_kelp,
  dsn = here("data", "processed", "clean_kelp.gpkg"),
  delete_dsn = T
)
