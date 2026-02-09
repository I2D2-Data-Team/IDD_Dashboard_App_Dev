# library(tidyverse)
# library(ggplot2)
# library(ggrepel)
# library(AzureStor)

# Read common functions
source("Code/functions.R")
# Read geospatial data and color pallets 
source("Code/global.R")
# Read modules
source("Code/modules.R")


dim_dir <- "Demographic"


# READ METADATA -----------------------------------------------------------

# Read indicator type
metadata.fig.types <-
  readxl::read_xlsx("../common/Data/figure_titles.xlsx", sheet = "type", .name_repair = janitor::make_clean_names) %>%
  filter(dimension == "Demographic")

# Read figure titles and tooltips
metadata.fig.titles <-
  readxl::read_xlsx("../common/Data/figure_titles.xlsx", sheet = "titles", .name_repair = janitor::make_clean_names) %>%
  filter(dimension == "Demographic") %>%
  select(measure, indicator, subsets, figure, title, tool_tip_text, numerator, num_source, denominator, den_source) %>%
  mutate(num_source = paste0("(", num_source, ")"),
         den_source = paste0("(", den_source, ")"))


# READ DATA ---------------------------------------------------------------

# Set subsets for each indicator
subset.dem.age <- c("0-2", "3-4", "5", "6-8", "9-11", "12-14", "15-17")
subset.dem.rac <- c("White", "Black", "Asian", "Other", "Hispanic")
subset.hse.lng <- c("English Only", "Spanish", "Other Language")
subset.hse.typ <- c("Married", "Female No Spouse", "Male No Spouse")
subset.hse.wrk <- c("All Parents Working", "No Parent Working", "Single Working Parent", "Two Parents, One Working", "Two Parents, Both Working")
subset.hse.tch <- c("Computing Device", "Internet Access")
subset.hse.trn <- c("Drove Alone", "Carpooled", "Public Transportation", "Walked", "Other", "Not Applicable")
subset.sch.wrk <- c("none")


# Set groups for indicator
group.hse.typ      <- c("0-5", "0-2", "3-4", "5")
group.hse.lng      <- c("< 100% FPL", "≥ 100% FPL")
group.hse.tch.cmp  <- c("Computer Only", "Mobile Device Only", "Both Computer and Mobile Device", "None")
group.hse.tch.int  <- c("Dial-up", "Broadband", "Cellular Data Plan", "Broadband and Cellular", "No Internet")
group.hse.trn      <- c("< 100% FPL", "100% - 149% FPL", "≥ 150% FPL")


# # Read county data to check SUBSET and GROUP levels
# data.county.dem.age.1 <- read_my_csv(dim_dir, "IA-county", "dem_age_fig1_2_3")
# data.county.dem.rac.1 <- read_my_csv(dim_dir, "IA-county", "dem_rac_fig1_2_3")
# data.county.hse.ftm.1 <- read_my_csv(dim_dir, "IA-county", "hse_ftm_fig1_2")
# data.county.hse.ftm.3 <- read_my_csv(dim_dir, "IA-county", "hse_ftm_fig3_4")
# data.county.hse.lng.1 <- read_my_csv(dim_dir, "IA-county", "hse_lng_fig1_2")
# data.county.hse.lng.3 <- read_my_csv(dim_dir, "IA-county", "hse_lng_fig3")
# data.county.hse.typ.1 <- read_my_csv(dim_dir, "IA-county", "hse_typ_fig1_2")
# data.county.hse.typ.3 <- read_my_csv(dim_dir, "IA-county", "hse_typ_fig3")
# data.county.hse.wrk.1 <- read_my_csv(dim_dir, "IA-county", "hse_wrk_fig1_2_3")
# data.county.hse.tch.1 <- read_my_csv(dim_dir, "IA-county", "hse_tch_fig1_2_3")
# data.county.hse.trn.1 <- read_my_csv(dim_dir, "IA-county", "hse_trn_fig1_2")
# data.county.hse.trn.3 <- read_my_csv(dim_dir, "IA-county", "hse_trn_fig3")
# data.county.hse.sch.1 <- read_my_csv(dim_dir, "IA-county", "hse_sch_fig1_2_3")


