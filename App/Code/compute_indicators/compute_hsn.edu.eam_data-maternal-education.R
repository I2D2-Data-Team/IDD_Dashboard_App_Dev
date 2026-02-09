# Compute Indicator
# Split data for Maternal Education by geographies and save 

library(tidyverse)



# Read Data ---------------------------------------------------------------

# Set paths to CyBox
cy_path <- file.path(Sys.getenv("USERPROFILE"), "Box")
cy_idd_path <- "Iowa IDS/Projects/_IA Data Drive/Indicators/I2D2 Data/ACS INDICATORS/Indicators_GioUpload"
cy_my_path <- "My IDS/_DATA/IDD"


# Read the maternal education data
data.eam <- 
  read_csv(list.files(file.path(cy_path, cy_my_path), pattern = "^idd-educational-attainment", full.names = TRUE),
           col_types = cols(.default = "c", year = "i", index = "d", count = "d"))



# Prepare Data ------------------------------------------------------------

# Set column names
my.cols <- c("year", "dimension","measure","indicator", "subset_level", "group_level", "index", "count")


# Transform data
my.data <- 
  data.eam %>%
  filter(is.na(group)) %>%
  mutate(dimension = "Health and Social Needs",
         measure = "Education",
         indicator = "Maternal Education",
         subset_level = subset,
         group_level = "none") %>%
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
for (i in unique(my.data$geo)) {
  # set file name for saving data
  file_name <-
    switch (i,
            "data_ia" =  paste0("edu_eam_fig1_2_3_", "co", ".csv"),
            "data_eci" = paste0("edu_eam_fig1_2_3_", "eci", ".csv") ,
            "data_hhs" = paste0("edu_eam_fig1_2_3_", "rg", ".csv"),
            "data_hs" =  paste0("edu_eam_fig1_2_3_", "hs", ".csv"),
            "data_miechv_26" = paste0("edu_eam_fig1_2_3_", "mv", ".csv")
    )
  # set directory name to save data in my CyBox
  dir_name <- 
    switch (i,
            "data_ia" = "IA-county",
            "data_eci" = "ECI-area",
            "data_hhs" = "HHS-region",
            "data_hs" = "HS-grantee",
            "data_miechv_26" = "MIECHV-area"
    )
  # subset data by geographies and save in corresponding locations
  my.data %>%
    rename_geocode_and_geoname(i) %>%
    filter(geo == i) %>%
    select(-geo) %T>%
    write_csv(file.path(cy_path, cy_idd_path, "health and social indicators", file_name)) %>%
    write_csv(file.path(cy_path, cy_my_path, "Health", dir_name, file_name))
}


