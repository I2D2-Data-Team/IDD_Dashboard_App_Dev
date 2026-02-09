
i2d2_image <- magick::image_read("../common/Code/www/I2D2_Logo_short.png") %>% grid::rasterGrob(interpolate = TRUE)
data.county.dem.age.1 <- read_my_csv(dim_dir, "IA-county", "dem_age_fig1_2_3")
ia_county_map <- read_rds("../common/Data/map_IA-county.rds")
fig_titles <- metadata.fig.titles %>%
  filter(indicator == "Child Age", figure == 1)


data.county.dem.age.1 %>%
  filter(year == 2023, group_level == "none", subset_level == "0-2", fips != 19) %>%
  mutate(fips = as.numeric(fips)) %>%
  plot_map_view(ia_county_map, LOCATIONS = c(19003, 19005),
                DATA_TYPE = "percent", OUTLINES = TRUE, LABELS = FALSE,
                COL = "colors_red_gradient") +
  # theme_view_map DASHBOARD
  theme_void() +
  theme(
    legend.position = "bottom",
    legend.key.height = unit(4, "mm"),
    legend.key.width = unit(16, "mm"),
    legend.justification = c(0.45, 0.9),
    legend.text = element_text(size = 16, face = "bold")
    ) +
  
  # theme for output map DOWNLOAD
  annotation_custom(grob = i2d2_image,
           xmin = -90.0, xmax = -90.65, 
           ymin =  38.4, ymax =  41.2) +   
  labs(title = paste(fig_titles$title[1], fig_titles$tool_tip_text, fig_titles$denominator),
       # subtitle = "subtitle goes here",
       caption = sprintf(
         "**Source:** IA Data Drive.<br>**Data:** %s.<br>**Year:** %s.<br>**Downloaded on:** %s",
         fig_titles$tool_tip_text, 2022,
         format(Sys.Date(), "%B %d, %Y")),
       tag = "Developed by Giorgi Chighladze",
       alt = "Iowa heatmap") +
  theme(
    plot.title = element_textbox_simple(size = 33, face = "bold", halign = 0.45, vjust = 0.5, lineheight = 1.5),
    plot.caption = element_markdown(size = 10, hjust = 0, margin = margin(l = 20), lineheight = 1.3),
    plot.tag.position = c(0.99, 0.19),
    plot.tag = element_text(hjust = 1, vjust = 1, size = 9, face = "bold.italic", color = "grey99"),
    plot.margin = margin(t = 5, b = 5, unit = "pt")
  ) +
  # allign legend bar and its text
  guides(
    fill = guide_colorbar(
      barwidth = 20, barheight = 1.2,
      label.theme = element_text(hjust = c(1.1, -0.1), vjust = 7))
  ) +
  # # to see outline of the plot and panel
  # theme(panel.border = element_rect(color = "red", fill = NA, linewidth = 1),
  #       plot.background = element_rect(color = "blue", fill = NA, linewidth = 1)) +
  coord_sf(clip = "off")
  
ggsave("../../../../../../Downloads/TEST1.png", width = 10, height = 8, scale = 1.25, dpi = 150, bg = "white")


# fluidRow(
#   column(
#     width = 12,
#     wellPanel(
#       h1(strong("Data")),
#       tags$hr(style = "margin: 0; padding: 0; height: 10px; border-top: 2px solid;"),
#       p(strong("Source: "), "SENCUS ACS", a("Table 123", href = "https://www.census.gov/programs-surveys/acs/data.html")),
#       p(strong("Available Years: "), "2018-2025"),
#       p(strong("Date Obtained: "), "January 29, 2026"),
#       p(strong("Note: "), "Some notes about indicator"),
#       
#       # p(strong("DESCRIPTION: "), "INDICATOR_DESCRIPTION"),
#       # p(strong("NUMERATOR: "), "INDICATOR_NUMERATOR", "INDICATOR_DATA_AGENCY_NUMERATOR", a("INDICATOR_DATA_SOURCE_NUMERATOR", href = "INDICATOR_DATA_URL_NUMERATOR")),
#       # p(strong("DENOMINATOR: "), "INDICATOR_DENOMINATOR", "INDICATOR_DATA_AGENCY_DENOMINATOR", "INDICATOR_DATA_SOURCE_DENOMINATOR"),
#       # p(strong("AVAILABLE YEARS: "), "INDICATOR_YEAR_FIRST", " - ", "INDICATOR_YEAR_LAST"),
#       # p(strong("UPDATED: "), "INDICATOR_FREQUENCY"),
#       # p(strong("DATA OBTAINED: "), "INDICATOR_LAST_OBTAINED"),
#       # p(strong("NOTE: "), "INDICATOR_NOTES"),
#       # downloadButton(ns("DOWNLOAD_CSV"), "Download CSV"),
#       # downloadButton(ns("DOWNLOAD_XLSX"), "Download Excel"),
#       br(),
#       p("For additional info, see the ", a("IA Data Drive Manual", href = "https://i2d2.iastate.edu/wp-content/uploads/2025/04/IA-Data-Drive-Manual-v3.3.pdf"))
#     )
#   )
# ),