# Compute Indicator
# Extract data for Birth Risk indicators and save individually

library(tidyverse)



# Read Data ---------------------------------------------------------------

# Set paths to CyBox
cy_path <- file.path(Sys.getenv("USERPROFILE"), "Box")
cy_idd_path <- "Iowa IDS/Projects/_IA Data Drive/Indicators/I2D2 Data/ACS INDICATORS/Indicators_GioUpload"
cy_my_path <- "My IDS/_DATA/IDD"


# Read the birth risk data (that contains Medicaid or WIC)
data.rsk <- 
  read_csv(list.files(file.path(cy_path, cy_my_path), pattern = "^idd-birth-risk", full.names = TRUE),
           col_types = cols(.default = "c", year = "i", index = "d", count = "d"))



# Prepare Data ------------------------------------------------------------

# Set column names
my.cols <- c("year", "dimension","measure","indicator", "subset_level", "group_level", "index", "count")

# Set indicator names
my_indicators_abbr <- tibble::tribble(
  ~indicator_vm,                        ~indicator_name,                      ~abbr,
  "Teenage Mother",                     "Birth to Teenage Mother",            "btm",
  "Unmarried Mother",                   "Birth to Unmarried Mother",          "bum",
  "Inadequate Prenatal Care",           "Inadequate Prenatal Care",           "ipc",
  "Preterm or Low Birth Weight",        "Preterm or Low Birth Weight",        "plw",
  "Tobacco Exposure",                   "Prenatal Tobacco Exposure",          "pte",
  "Low Maternal Education",             "Low Maternal Education at Birth",    "lme",
  "Medicaid or WIC Receipt at Birth",   "Medicaid or WIC Receipt at Birth",   "mow",
  "Cummulative Birth Reisk",            "Cumulative Birth Risks",             "cum"
)

# Function to extract data for Fig 1 and 2 and Fig 3
compute_rsk_fig1 <- function(data, indicator_vm, indicator_idd) {
  data %>%
    filter(indicator == indicator_vm,
           is.na(group)) %>%
    mutate(dimension = "Health and Social Needs",
           measure = "Birth Risks",
           indicator = indicator_idd,
           subset_level = subset,
           group_level = "none") 
}

# Function to extract data for Fig 3 and 4
compute_rsk_fig3 <- function(data, indicator_vm, indicator_idd) {
  data %>%
    filter(indicator == indicator_vm,
           !is.na(group)) %>%
    mutate(dimension = "Health and Social Needs",
           measure = "Birth Risks",
           indicator = indicator_idd,
           subset_level = subset,
           group_level = ifelse(is.na(group), "none", group)) 
}

# Function to rename code and geoname for each geography before saving data
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


# Save Data ---------------------------------------------------------------

# Save data separately for each indicator and geography for Fig 1 and 2
for (j in 1:nrow(my_indicators_abbr)) {
  cat(crayon::blue("Extracting data for", my_indicators_abbr$indicator_vm[j], "\n"))
  
  # Extract data for each indicator
  my.data.1 <- 
    data.rsk %>% 
    compute_rsk_fig1(my_indicators_abbr$indicator_vm[j], my_indicators_abbr$indicator_name[j]) %>%
    select(geo, code, geoname, {my.cols}) %>%
    arrange(geo, code, year)
  
  # Extract data for each geography
  for (i in unique(my.data.1$geo)) {
    # set file name for saving data
    file_name <-
      switch (i,
              "data_ia"        = paste0("rsk_", my_indicators_abbr$abbr[j], "_fig1_2_", "co", ".csv"),
              "data_eci"       = paste0("rsk_", my_indicators_abbr$abbr[j], "_fig1_2_", "eci", ".csv") ,
              "data_hhs"       = paste0("rsk_", my_indicators_abbr$abbr[j], "_fig1_2_", "rg", ".csv"),
              "data_hs"        = paste0("rsk_", my_indicators_abbr$abbr[j], "_fig1_2_", "hs", ".csv"),
              "data_miechv_26" = paste0("rsk_", my_indicators_abbr$abbr[j], "_fig1_2_", "mv", ".csv")
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
    my.data.1 %>%
      rename_geocode_and_geoname(i) %>%
      filter(geo == i) %>%
      select(-geo) %T>%
      write_csv(file.path(cy_path, cy_idd_path, "health and social indicators", file_name)) %>%
      write_csv(file.path(cy_path, cy_my_path, "Health", dir_name, file_name))
  }
  
}


# Save data separately for each indicator and geography for Fig 3 and 4
for (j in 1:nrow(my_indicators_abbr)) {
  cat(crayon::blue("Extracting data for", my_indicators_abbr$indicator_vm[j], "\n"))
  
  # Extract data for each indicator
  my.data.3 <- 
    data.rsk %>% 
    compute_rsk_fig3(my_indicators_abbr$indicator_vm[j], my_indicators_abbr$indicator_name[j]) %>%
    select(geo, code, geoname, {my.cols}) %>%
    arrange(geo, code, year)
  
  # Extract data for each geography
  for (i in unique(my.data.3$geo)) {
    # set file name for saving data
    file_name <-
      switch (i,
              "data_ia"        = paste0("rsk_", my_indicators_abbr$abbr[j], "_fig3_4_", "co", ".csv"),
              "data_eci"       = paste0("rsk_", my_indicators_abbr$abbr[j], "_fig3_4_", "eci", ".csv") ,
              "data_hhs"       = paste0("rsk_", my_indicators_abbr$abbr[j], "_fig3_4_", "rg", ".csv"),
              "data_hs"        = paste0("rsk_", my_indicators_abbr$abbr[j], "_fig3_4_", "hs", ".csv"),
              "data_miechv_26" = paste0("rsk_", my_indicators_abbr$abbr[j], "_fig3_4_", "mv", ".csv")
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
    my.data.3 %>%
      rename_geocode_and_geoname(i) %>%
      filter(geo == i) %>%
      select(-geo) %T>%
      write_csv(file.path(cy_path, cy_idd_path, "health and social indicators", file_name)) %>%
      write_csv(file.path(cy_path, cy_my_path, "Health", dir_name, file_name))
  }
  
}

