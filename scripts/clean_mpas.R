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
pacman::p_load(
  here,
  janitor,
  sf,
  tidyverse
)

# Read files -------------------------------------------------------------------

raw_mpas <- st_read(dsn = here("data", "raw", "mpas", "Final_MPAs_07-23"),
                    layer = "MPA_All_Final_07-23")

## PROCESSING ##################################################################

# Clean data -------------------------------------------------------------------
clean_mpas <- raw_mpas %>%
  st_zm(drop = T) %>% 
  clean_names() %>% 
  select(mpa_id, country, lfp = new_pro_se) %>%
  mutate(
    lfp = as.numeric(lfp),
    lfp_cat = case_when(lfp == 1 ~ "Least",
                        lfp == 2 ~ "Less",
                        lfp == 3 ~ "Moderately",
                        lfp == 4 ~ "Heavily",
                        lfp == 5 ~ "Most"),
    lfp_group = case_when(lfp <= 2 ~ "Less",
                          lfp == 3 ~ "Moderately",
                          lfp >= 4 ~ "Highly")) %>%
  arrange(desc(lfp)) %>%
  st_make_valid() %>%
  st_difference() %>%
  st_make_valid() %>%
  mutate(
    country = case_when(
      country == "Argentina" ~ "ARG",
      country == "Australia" ~ "AUS",
      country == "Canada" ~ "CAN",
      country == "Chile" ~ "CHL",
      country == "France" ~ "FRA",
      country == "Mexico" ~ "MEX",
      country == "Namibia" ~ "NAM",
      country == "New Zealand" ~ "NZL",
      country == "Peru" ~ "PER",
      country == "South Africa" ~ "ZAF",
      country == "United Kingdom" ~ "GBR",
      country == "United States" ~ "USA"
    )
  )

## EXPORT ######################################################################

# X ----------------------------------------------------------------------------
st_write(
  obj = clean_mpas,
  dsn = here("data", "processed", "clean_mpas.gpkg"),
  delete_dsn = T
)
