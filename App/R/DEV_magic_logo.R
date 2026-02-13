# Adding I2D2 logo to figures ---------------------------------------------

# FROM loader.R -----------------------------------------------------------
library(magick)

## FROM function.R ---------------------------------------------------------

# Load I2D2 logo for plots
i2d2_logo <- 
  magick::image_read("https://raw.githubusercontent.com/I2D2-Data-Team/IDD_Dashboard_App_Dev/main/common/www/I2D2_Logo_short.png") %>% 
  grid::rasterGrob(interpolate = TRUE)

# Make I2D2 logo with fixed size and position (for Cartesian figures)
i2d2_logo_fixed <- 
  magick::image_read("https://raw.githubusercontent.com/I2D2-Data-Team/IDD_Dashboard_App_Dev/main/common/www/I2D2_Logo_short.png") %>% 
  grid::rasterGrob(x = unit(0.98, "npc"), y = unit(-0.2, "npc"),
                   just = c("right", "bottom"), 
                   width = unit(2.2, "cm"))

# The part that was remove from the plotting functions
# annotation_custom was removed from maps
format_map_download <- function(fig, fig_title_data, fig_source_data) {
  
  download_fig <- 
    fig +
    annotation_custom(grob = i2d2_logo,
                      xmin = -90.0, xmax = -90.65, 
                      ymin =  38.4, ymax =  41.2)
  }

# annotation_custom was removed from other figs
format_figs_download <- function(fig, fig_title, fig_source_data, figure_number) {
  download_fig <- 
    fig +
    annotation_custom(i2d2_logo_fixed)
}


