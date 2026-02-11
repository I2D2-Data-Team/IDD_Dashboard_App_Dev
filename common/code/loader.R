# Load required packages -------------------------------------------------------
# Only load tidyverse packages that is used by shiny, 
# those that are not used comment out.
# Packages directly related to shiny are loaded with app.R script 

library(AzureStor)
library(readr)
library(dplyr)
library(stringr)
library(ggplot2)
# library(forcats)
# library(lubridate)
# library(purrr)
# library(tibble)
# library(tidyr)
library(ggrepel)
library(ggtext)
library(magick)
library(sf)


# Load R scripts ---------------------------------------------------------------

if (Sys.getenv("USERNAME") == "gio" & exists("check.this.to.run.local")) {
  print(cat("Reading code from local dir"))
  common.function.path <- ".."
} else {
  print(cat("Reading code from remote dir"))
  common.function.path <- "https://raw.githubusercontent.com/I2D2-Data-Team/IDD_Dashboard_App_Dev/refs/heads/main"
}

# Read common functions
source(file.path(common.function.path, "common/code/functions.R"))
# Read geospatial data and color pallets
source(file.path(common.function.path, "common/code/global.R"))
# Read modules
source(file.path(common.function.path, "common/code/modules.R"))
