# This code reads sf files for Iowa counties and aggregates 
# them using grouping from county_crosswalk file. 
# Resulting sf data is saved to `common` directory in IDD repo.

library(tidyverse)
library(sf)


# Read sf data for mapping Counties of Iowa
ia_county_map <- 
  read_rds("Data/IA-county-sf.rds")
# Read Iowa county crosswalk for all service geographies
ia_county_crosswalk <-
  read_csv("Data/county_crosswalk.csv", col_types = cols(.default = "c")) %>% 
  janitor::clean_names()

df <- 
  ia_county_crosswalk %>%
  mutate(across(contains("_number"), as.integer),
         fips = as.integer(fips))

# IA COUNTY
ia_county_map %>%
  write_rds("../i2d2_dashboards/common/Data/map_IA-county.rds")


# MIECHV
ia_county_map %>%
  left_join(df, by = "fips") %>%
  group_by(fips = miechv_number_26, name = miechv_26) %>%
  summarise() %>%
  mutate(long = st_centroid(geom) %>% st_coordinates() %>% .[1],
         lat = st_centroid(geom) %>% st_coordinates() %>% .[2]) %>%
  ungroup() %>%
  mutate(geography = "MIECHV Service Areas") %>%
  select(fips, name, geography, long, lat, geom) %>%
  as.data.frame() %>%
  sf::st_as_sf() %>%
  write_rds("../i2d2_dashboards/common/Data/map_MIECHV-areas.rds")
# Read sf data for mapping MIECHV Service Areas
miechv_area_map <- read_rds("../i2d2_dashboards/common/Data/map_MIECHV-areas.rds")


# HHS 
ia_county_map %>%
  left_join(df, by = "fips") %>%
  group_by(fips = hhs_number, name = hhs_regions) %>%
  summarise() %>%
  mutate(long = st_centroid(geom) %>% st_coordinates() %>% .[1],
         lat = st_centroid(geom) %>% st_coordinates() %>% .[2]) %>%
  ungroup() %>%
  mutate(geography = "HHS Regions") %>%
  select(fips, name, geography, long, lat, geom) %>%
  as.data.frame() %>%
  sf::st_as_sf() %>%
  write_rds("../i2d2_dashboards/common/Data/map_HHS-regions.rds")
# Read sf data for mapping HHS Regions
hhs_area_map <- read_rds("../i2d2_dashboards/common/Data/map_HHS-regions.rds")


# HS 
ia_county_map %>%
  left_join(df, by = "fips") %>%
  group_by(fips = hs_number, name = head_start_grantees) %>%
  summarise() %>%
  mutate(long = st_centroid(geom) %>% st_coordinates() %>% .[1],
         lat = st_centroid(geom) %>% st_coordinates() %>% .[2]) %>%
  ungroup() %>%
  mutate(geography = "Head Start Grantees") %>%
  select(fips, name, geography, long, lat, geom) %>%
  as.data.frame() %>%
  sf::st_as_sf() %>%
  write_rds("../i2d2_dashboards/common/Data/map_HS-grantess.rds")
# Read sf data for mapping Head Start Grantees
hs_area_map <- read_rds("../i2d2_dashboards/common/Data/map_HS-grantess.rds")
  
# ECI
ia_county_map %>%
  left_join(df, by = "fips") %>%
  group_by(fips = eci_number, name = eci_area_name) %>%
  summarise() %>%
  mutate(long = st_centroid(geom) %>% st_coordinates() %>% .[1],
         lat = st_centroid(geom) %>% st_coordinates() %>% .[2]) %>%
  ungroup() %>%
  mutate(geography = "ECI Areas") %>%
  select(fips, name, geography, long, lat, geom) %>%
  as.data.frame() %>%
  sf::st_as_sf() %>%
  write_rds("../i2d2_dashboards/common/Data/map_ECI-areas.rds")
# Read sf data for mapping ECI areas
eci_area_map <- read_rds("../i2d2_dashboards/common/Data/map_ECI-areas.rds")

