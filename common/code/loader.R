# Load required packages -------------------------------------------------------
# Only load tidyverse packages that is used by shiny, 
# those that are not used comment out.
# Packages directly related to shiny are loaded with app.R script 

library(AzureStor)
library(readr)
library(dplyr)
library(stringr)
library(ggplot2)
library(purrr)
# library(forcats)
# library(lubridate)
# library(tibble)
# library(tidyr)
library(ggrepel)
library(ggtext)
library(sf)
library(leaflet)
library(leaflegend)


# Load R scripts ---------------------------------------------------------------

# Use system2() to capture command output into an R variable
# 'stdout' argument is set to TRUE to capture the standard output
current_branch <- system2("git", args = c("rev-parse", "--abbrev-ref", "HEAD"), stdout = TRUE)

if (Sys.getenv("USERNAME") == "gio" && current_branch != "main") {
  cat("Reading code from local directory.\n")
  common.function.path <- ".."
} else {
  cat("You are on main branch! Reading code from github.\n")
  common.function.path <- "https://raw.githubusercontent.com/I2D2-Data-Team/IDD_Dashboard_App_Dev/refs/heads/main"
}

# Read common functions
source(file.path(common.function.path, "common/code/functions.R"))
# Read geospatial data and color pallets
source(file.path(common.function.path, "common/code/global.R"))
# Read modules
source(file.path(common.function.path, "common/code/modules.R"))
# Load shiny settings
source(file.path(common.function.path, "common/code/settings.R"))
# Load shiny sidebar
source(file.path(common.function.path, "common/code/sidebar.R"))


