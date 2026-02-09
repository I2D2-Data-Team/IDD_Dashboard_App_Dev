# Compute Indicator
# Extract data for mothers receiving WIC prenatally to be used with 
# WIC indicator (under Human Services measure in Services domain)
# This is originally going to be used for FIg 4 in that dashboard

library(tidyverse)



# Read Data ---------------------------------------------------------------

# Set paths to CyBox
cy_path <- file.path(Sys.getenv("USERPROFILE"), "Box")
cy_idd_path <- "Iowa IDS/Projects/_IA Data Drive/Indicators/I2D2 Data/ACS INDICATORS/Indicators_GioUpload"
cy_my_path <- "My IDS/_DATA/IDD"


# Read the birth risk data (that contains WIC)
data.rsk <- 
  read_csv(list.files(file.path(cy_path, cy_my_path), pattern = "^idd-birth-risk", full.names = TRUE),
           col_types = cols(.default = "c", year = "i", index = "d", count = "d"))

# Read the WIC data for figures 1, 2, and 3 
wic.123 <- 
  read_csv(file.path(cy_path, cy_my_path, "Services/IA-county/ser_wic_fig1_2_3_co.csv"), 
           col_types = cols(.default = "c"))



# Prepare Data ------------------------------------------------------------

# Set column names
my.cols <- c("year", "dimension","measure","indicator", "subset_level", "group_level", "index", "count")

# Get meta data about indicator
wic_dimenstion <- unique(wic.123$dimension)
wic_measure <- unique(wic.123$measure)
wic_indicator <- unique(wic.123$indicator)
wic_subset_level <- "Prenatal"
wic_group_level <- "none"
wic_years <- range(wic.123$year)

# prepare data for WIC fig 4
wic.4 <- 
  data.rsk %>%
  # select indicator of interest
  filter(indicator == "Medicaid or WIC Receipt at Birth", 
         subset == 'WIC Receipt Only') %>%
  # select indicator for all races 
  filter(is.na(group)) %>%
  # select years that are available/computed for ACS 
  filter(between(year, as.integer(wic_years[1]), as.integer(wic_years[2]))) %>%
  mutate(dimension = wic_dimenstion,
         measure = wic_measure,
         indicator = wic_indicator,
         subset_level = wic_subset_level,
         group_level = wic_group_level) %>%
  select(geo, code, geoname, {my.cols}) %>%
  arrange(geo, code, year)



# Save Data ---------------------------------------------------------------

# Rename code and geoname for each geography before saving data
rename_geocode_and_geoname <- function(df, geo) {
  my_com_cols <- c("year", "dimension", "measure", "indicator", "subset_level", "group_level", "index", "count")
  switch (geo,
          "data_ia" = df %>% select(geo, fips = code, county_name = geoname, {my_com_cols}),
          "data_eci" = df %>% select(geo, eci_number = code, eci_area_name = geoname, {my_com_cols}),
          "data_hhs" = df %>% select(geo, region_number = code, region = geoname, {my_com_cols}),
          "data_hs" = df %>% select(geo, hs_number = code, hs = geoname, {my_com_cols}),
          "data_miechv_26" = df %>% select(geo, miechv_number = code, miechv = geoname, {my_com_cols}),
          "No geography matched"
  )
}

# Save data separately for each geography 
for (i in unique(wic.4$geo)) {
  # set file name for saving data
  file_name <-
    switch (i,
            "data_ia"        = paste0("ser_wic_fig4_", "co", ".csv"),
            "data_eci"       = paste0("ser_wic_fig4_", "eci", ".csv"),
            "data_hhs"       = paste0("ser_wic_fig4_", "rg", ".csv"),
            "data_hs"        = paste0("ser_wic_fig4_", "hs", ".csv"),
            "data_miechv_26" = paste0("ser_wic_fig4_", "mv", ".csv")
    )
  # set directory name to save data in my CyBox
  dir_name <- 
    switch (i,
            "data_ia"        = "IA-county",
            "data_eci"       = "ECI-area",
            "data_hhs"       = "HHS-region",
            "data_hs"        = "HS-grantee",
            "data_miechv_26" = "MIECHV-area"
    )
  # subset data by geographies and save in corresponding locations
  wic.4 %>%
    rename_geocode_and_geoname(i) %>%
    filter(geo == i) %>%
    select(-geo) %T>%
    write_csv(file.path(cy_path, cy_idd_path, "services indicators", file_name)) %>%
    write_csv(file.path(cy_path, cy_my_path, "Services", dir_name, file_name))
}

