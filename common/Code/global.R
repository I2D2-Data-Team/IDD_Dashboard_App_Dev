library(tidyverse)

# READ Data --------------------------------------------------------------------

# Read sf data for mapping State of Iowa
ia_state_map <- read_rds("../common/Data/map_IA-state.rds")
# # Read sf data for mapping Counties of Iowa
# ia_county_map <- read_rds("../common/Data/map_IA-county.rds")
# # Read sf data for mapping ECI areas
# eci_area_map <- read_rds("../common/Data/map_ECI-areas.rds")
# # Read sf data for mapping HHS Regions
# hhs_area_map <- read_rds("../common/Data/map_HHS-regions.rds")
# # Read sf data for mapping Head Start Grantees
# hs_area_map <- read_rds("../common/Data/map_HS-grantess.rds")
# # Read sf data for mapping MIECHV Service Areas
# miechv_area_map <- read_rds("../common/Data/map_MIECHV-areas.rds")


# Read Iowa county crosswalk for all service geographies
ia_county_crosswalk <- read_csv("../common/Data/county_crosswalk.csv", col_types = cols(.default = "c")) %>% janitor::clean_names()


# Geographies -------------------------------------------------------------

# Function to create named list
# it assume that the first column of input dataframe is a numeric code 
# assigned to geography and the second column is a name to be displayed  
create_named_list <- function(df) {
  my_list <- as.list(as.numeric(df[[1]]))
  names(my_list) <- df[[2]]
  return(my_list)
}

# Create named list for Location drop-down selection
# Named list of 99 Iowa counties
ia_county_droplist <- ia_county_crosswalk %>% count(fips, county_name) %>% create_named_list()
# Named list of 34 ECI Areas
eci_area_droplist <- ia_county_crosswalk %>% count(eci_number, eci_area_name) %>% create_named_list()
# Named list of 7 HHS Regions
hhs_region_droplist <- ia_county_crosswalk %>% count(hhs_number, hhs_regions) %>% create_named_list()
# Named list of 17 Head Start Grantees
hs_grantee_droplist <- ia_county_crosswalk %>% count(hs_number, head_start_grantees) %>% create_named_list()
# Named list of 8 MIECHV Service Areas (based on 2026)
miechv_droplist <- ia_county_crosswalk %>% count(miechv_number_26, miechv_26) %>% create_named_list()


# Color Palettes ----------------------------------------------------------

# Set main colors
# all_8_colors <- c("#1B9E77", "#D95F02", "#7570B3", "#E7298A", "#66A61E", "#E6AB02", "#A6761D", "#666666")
all_8_colors <- c("#D0202E", "#001B71", "#76777A", "#0097CE", "#863399", "#D6A415", "#0B5715", "#666666")

# Set colors for heat map
map_colors <- list(
  # colors_yellow_green  = c("#fff999", "#f1d581", "#b6c45c", "#7db257", "#4c9c53", "#34834b", "#146c37"),
  # colors_yellow_red    = c("#FFFFB2", "#FED976", "#FEB24C", "#FD8D3C", "#FC4E2A", "#E31A1C", "#B10026"),   # RColorBrewer::brewer.pal(7, "YlOrRd")
  # colors_purple_red    = c("#FEEBE2", "#FCC5C0", "#FA9FB5", "#F768A1", "#DD3497", "#AE017E", "#7A0177"),   # RColorBrewer::brewer.pal(7, "RdPu")
  # colors_blue_purple   = c("#EDF8FB", "#BFD3E6", "#9EBCDA", "#8C96C6", "#8C6BB1", "#88419D", "#6E016B"),   # RColorBrewer::brewer.pal(7, "BuPu")
  # colors_yellow_blue   = c("#FFFFBF", "#E6F598", "#ABDDA4", "#66C2A5", "#3288BD", "#5E4FA2", "#473d8b"),   # c(RColorBrewer::brewer.pal(11, "Spectral")[6:11], "#darkslateblue")
  # colors_yellow_purple = c("#FDE2A3", "#FEAA74", "#F7725C", "#D9466B", "#AA337D", "#792282", "#4A1079"),    # viridis::magma(18)[seq(17, 5, -2)]
  colors_red_gradient     = rev(c("#d0202e", "#d84342", "#df5d57", "#e5736d", "#eb8984", "#ef9e9b", "#f1b3b3", "#f3c7cb")),
  colors_blue_gradient    = rev(c("#001b71", "#2d2f80", "#48458f", "#615b9e", "#7973ad", "#918bbc", "#aaa4cb", "#c2beda")),
  colors_purple_gradient  = rev(c("#863399", "#944aa4", "#a160af", "#ae75ba", "#bb8ac4", "#c89fcf", "#d5b4da", "#e1cae5"))
)

# Create named list for Color drop-down selection
map_color_choices <- 
  list(
    # "Yellow to Green"  = "colors_yellow_green",
    # "Yellow to Red"    = "colors_yellow_red",
    # "Yellow to Blue"   = "colors_yellow_blue",
    # "Yellow to Purple" = "colors_yellow_purple",
    # "Blue to Purple"   = "colors_blue_purple",
    # "Purple to Red"    = "colors_purple_red",
    "Red" = "colors_red_gradient",
    "Blue" = "colors_blue_gradient",
    "Purple" = "colors_purple_gradient"
  )

