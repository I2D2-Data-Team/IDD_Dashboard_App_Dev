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

# Read common functions
source("https://raw.githubusercontent.com/I2D2-Data-Team/IDD_Dashboard_App_Dev/refs/heads/main/common/code/functions.R")
# Read geospatial data and color pallets
source("https://raw.githubusercontent.com/I2D2-Data-Team/IDD_Dashboard_App_Dev/refs/heads/main/common/code/global.R")
# Read modules
source("https://raw.githubusercontent.com/I2D2-Data-Team/IDD_Dashboard_App_Dev/refs/heads/main/common/code/modules.R")
