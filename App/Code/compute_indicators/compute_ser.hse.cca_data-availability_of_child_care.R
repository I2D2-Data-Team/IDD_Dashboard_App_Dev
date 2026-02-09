# Compute Indicator
# Aggregate Availability of Child Care indicator for different geos

library(tidyverse)



# Functions ---------------------------------------------------------------

# Function to compute statewide value (aggregate by statewide)
compute_statewide <- function(df) {
  df %>%
    group_by(year, dimension, measure, indicator, subset_level, group_level) %>%
    summarise(count = sum(count, na.rm = TRUE),
              total = sum(total, na.rm =  TRUE)) %>%
    ungroup() %>%
    mutate(index = count/total,
           county_name = "Statewide",
           eci_area_name = "Statewide",
           region = "Statewide",
           miechv = "Statewide",
           hs = "Statewide",
           fips = '19',
           eci_number = '19',
           region_number = '19',
           miechv_number = '19',
           hs_number = '19')
}

# Function to aggregate indicators by geography
# as an input need to provide numeric code and name of the geography
aggregate_by_geo <- function(df, geo_code, geo_name) {
  df %>% 
    compute_statewide() %>%
    bind_rows(df) %>%
    group_by({{geo_code}}, {{geo_name}}, year, dimension, measure, indicator, subset_level, group_level) %>%
    summarise(total = sum(total, na.rm = TRUE),
              count = sum(count, na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(index = count/total) %>%
    select({{geo_code}}, {{geo_name}}, {my.cols}) %>%
    arrange(as.numeric({{geo_code}}), year)
}



# Read Data ---------------------------------------------------------------

# Set paths to CyBox
cy_path <- file.path(Sys.getenv("USERPROFILE"), "Box")
cy_idd_path <- "Iowa IDS/Projects/_IA Data Drive/Indicators/I2D2 Data/ACS INDICATORS/Indicators_GioUpload"
cy_my_path <- "My IDS/_DATA/IDD"

# Read crosswalk for aggregation
ia_counties <- read_csv("Data/county_crosswalk.csv", col_types = cols(.default = "c")) %>%
  janitor::clean_names() %>% 
  select(fips, county_name, 
         eci_number, eci_area_name,
         hs_number, hs = head_start_grantees,
         miechv_number = miechv_number_26, miechv = miechv_26,
         region_number = hhs_number, region = hhs_regions)

# Read indicator data files
data.ia.county <- read_csv(file.path(cy_path, cy_my_path, "hss-child-care-capacity.csv"))



# Prepare Data ------------------------------------------------------------

# Set column names
my.cols <- c("year", "dimension","measure","indicator", "subset_level", "group_level", "index", "count")

# Get all data organized for computing individual data sets
data <- 
  data.ia.county %>%
  mutate(index = count/total) %>%
  mutate(
    county_name = county,
    subset_level = recode(type,
                          "center" = "HHS Licensed Centers and Preschools",
                          "home" = "Registered Child Development Homes"),
    group_level = "none",
    dimension = "Services",
    measure = "Human Services", 
    indicator = "Availability of Child Care") %>%
  left_join(ia_counties, by = "county_name")



# Aggregate Data by Geographies -------------------------------------------

# Compute Iowa County level data for Fig 1, 2 and 3
data %>% 
  compute_statewide() %>%
  bind_rows(data) %>%
  select(fips, county_name, {my.cols}) %>%
  arrange(fips, year) %T>%
  # write_csv("Data/Indicators/IA-county/ser_cca_fig1_2_3_co.csv") %>%
  write_csv(file.path(cy_path, cy_idd_path, "services indicators", "ser_cca_fig1_2_3_co.csv")) %>%
  write_csv(file.path(cy_path, cy_my_path, "Services/IA-county", "ser_cca_fig1_2_3_co.csv"))


# Compute ECI Area level data for Fig 1, 2 and 3
data %>%
  aggregate_by_geo(eci_number, eci_area_name) %T>%
  # write_csv("Data/Indicators/ECI-area/ser_cca_fig1_2_3_eci.csv") %>%
  write_csv(file.path(cy_path, cy_idd_path, "services indicators", "ser_cca_fig1_2_3_eci.csv")) %>%
  write_csv(file.path(cy_path, cy_my_path, "Services/ECI-area", "ser_cca_fig1_2_3_eci.csv"))


# Compute HSS Region level data for Fig 1, 2 and 3
data %>%
  aggregate_by_geo(region_number, region) %T>%
  # write_csv("Data/Indicators/HHS-region/ser_cca_fig1_2_3_rg.csv") %>%
  write_csv(file.path(cy_path, cy_idd_path, "services indicators", "ser_cca_fig1_2_3_rg.csv")) %>%
  write_csv(file.path(cy_path, cy_my_path, "Services/HHS-region", "ser_cca_fig1_2_3_rg.csv"))


# Compute HS Grantees level data for Fig 1, 2 and 3
data %>%
  aggregate_by_geo(hs_number, hs) %T>%
  # write_csv("Data/Indicators/HS-grantee/ser_cca_fig1_2_3_hs.csv") %>%
  write_csv(file.path(cy_path, cy_idd_path, "services indicators", "ser_cca_fig1_2_3_hs.csv")) %>%
  write_csv(file.path(cy_path, cy_my_path, "Services/HS-grantee", "ser_cca_fig1_2_3_hs.csv"))


# Compute MIECHV Service Area level data for Fig 1, 2 and 3
data %>%
  aggregate_by_geo(miechv_number, miechv) %T>%
  # write_csv("Data/Indicators/MIECHV-area/ser_cca_fig1_2_3_mv.csv") %>%
  write_csv(file.path(cy_path, cy_idd_path, "services indicators", "ser_cca_fig1_2_3_mv.csv")) %>%
  write_csv(file.path(cy_path, cy_my_path, "Services/MIECHV-area", "ser_cca_fig1_2_3_mv.csv"))


