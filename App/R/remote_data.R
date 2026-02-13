# source("../common/code/loader.R")
source("https://raw.githubusercontent.com/I2D2-Data-Team/IDD_Dashboard_App_Dev/refs/heads/main/common/code/loader.R")

# Set dimension name for pulling data for correct indicators OR from corresponding directories
dim_dir <- "Demographic"


# # READ METADATA -----------------------------------------------------------
# 
# # Read indicator types
# metadata.fig.types <-
#   read_csv("https://raw.githubusercontent.com/I2D2-Data-Team/IDD_Dashboard_App_Dev/refs/heads/main/common/data/idd_data_type.csv")
# 
# # Read indicator sources
# metadata.fig.sources <-
#   read_csv("https://raw.githubusercontent.com/I2D2-Data-Team/IDD_Dashboard_App_Dev/refs/heads/main/common/data/idd_data_source.csv")
#   
# # Read figure titles and tooltips
# metadata.fig.titles <-
#   read_csv("https://raw.githubusercontent.com/I2D2-Data-Team/IDD_Dashboard_App_Dev/refs/heads/main/common/data/idd_tooltip.csv")


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


