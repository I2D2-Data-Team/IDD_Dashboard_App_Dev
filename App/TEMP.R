# Data to be used for figures
metadata.fig.sources <- read_csv("https://raw.githubusercontent.com/I2D2-Data-Team/IDD_Dashboard_App_Dev/refs/heads/main/common/data/idd_data_source.csv")

### ··· Get source for active indicator 
current_indicator_source <- reactive({
  req(current_indicator())
  source <-
    metadata.fig.sources %>%
    filter(measure == input$MEASURE,
           indicator == current_indicator())
  return(source)
})

### ... Render indicator data source info 
build_data_source_container_server("DEM_DATA_SOURCE", current_indicator_source)

my_df <- 
  metadata.fig.sources %>%
  filter(indicator == "Child Age")

dim_dir <- "Demographic"
## -----------------------------------------------------------------------------
## -----------------------------------------------------------------------------
## -----------------------------------------------------------------------------


# i2d2_image <- magick::image_read("../common/Code/www/I2D2_Logo_short.png") %>% grid::rasterGrob(interpolate = TRUE)
i2d2_logo <- magick::image_read("https://raw.githubusercontent.com/I2D2-Data-Team/IDD_Dashboard_App_Dev/main/common/www/I2D2_Logo_short.png") %>% grid::rasterGrob(interpolate = TRUE)
data.county.dem.age.1 <- read_my_csv(dim_dir, "IA-county", "dem_age_fig1_2_3")
ia_county_map <- read_rds("../common/Data/map_IA-county.rds")
# fig_titles <- metadata.fig.titles %>% filter(indicator == "Child Age", figure == 1)


if (nrow(my_df) > 1) {
  my_data_source_list <- "<br><span style='color:white'>data:</span>- "
} else {
  my_data_source_list <- ""
}

fig_titles <-
  my_df %>%
  mutate(data = "Table C09001", source = "CENSUS") %>%
  bind_rows(my_df) %>%
  bind_rows(my_df) %>%
  mutate(source = paste0(my_data_source_list, source, ", ", data)) %>%
  group_by(measure, indicator) %>%
  summarise(source = str_flatten(unique(source), collapse = ";"),
            # date2 = str_flatten(unique(date_obtained), collapse = "; "),
            date = format(max(as.Date(date_obtained, "%m/%d/%y")), "%B %d, %Y"),
            year = max(max_year),
            years = paste0(max(min_year), "-", max(max_year))
            ) %>%
  ungroup()


url <- "https:// iadatadrive.i2d2.iastate.edu"

p1 <- 
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
    )  +
  # align legend bar and its text
  guides(
    fill = guide_colorbar(
      label.theme = element_text(hjust = c(1.1, -0.1), vjust = 7))
  )
  
  
# theme for output map DOWNLOAD
p1 +
  annotation_custom(grob = i2d2_logo,
           xmin = -90.0, xmax = -90.65, 
           ymin =  38.4, ymax =  41.2) +   
  labs(title = paste(fig_titles$measure, fig_titles$indicator),
       # subtitle = "subtitle goes here",
       caption = sprintf(
         "**Source:** I2D2, IA Data Drive, %s<br>**Data:** %s.<br>**Year:** %s<br>**Downloaded on:** %s",
         url, fig_titles$source, fig_titles$year, fig_titles$date
         # format(Sys.Date(), "%B %d, %Y")
         ),
       tag = "Developed by Giorgi Chighladze",
       alt = "Iowa heatmap") +
  theme(
    plot.title = element_textbox_simple(size = 33, face = "bold", halign = 0.45, vjust = 0.5, lineheight = 1.5),
    plot.caption = element_markdown(size = 10, hjust = 0, margin = margin(l = 20), lineheight = 1.3),
    plot.tag.position = c(0.99, 0.19),
    plot.tag = element_text(hjust = 1, vjust = 1, size = 9, face = "bold.italic", color = "grey99"),
    plot.margin = margin(t = 5, b = 5, unit = "pt")
  ) +
  # align legend bar and its text
  guides(fill = guide_colorbar(barwidth = 20, barheight = 1.2)) +
  # # to see outline of the plot and panel
  # theme(panel.border = element_rect(color = "red", fill = NA, linewidth = 1),
  #       plot.background = element_rect(color = "blue", fill = NA, linewidth = 1)) +
  coord_sf(clip = "off")
  

map.dem() +
  labs(title = paste(fig_titles()$title[1]),
       # subtitle = "subtitle goes here", caption = "this is caption", tag = "ECI Indicators from IDD",
       caption = sprintf(#"**Source:** IA Data Drive.\n**Data:** %s.\n**Year:** %s.\n**Downloaded on:** %s",
         "**Source:** IA Data Drive.<br>**Data:** %s.<br>**Year:** %s.<br>**Downloaded on:** %s",
         "INDICATOR_SOURCE", 2022,
         format(Sys.Date(), "%B %d, %Y")),
       alt = "map") +
  theme(
    plot.title = element_text(size = 20, face = "bold", hjust = 0.45, vjust = 0.5),
    plot.caption = element_markdown(size = 9, hjust = 0, margin = margin(l = 10), lineheight = 1)
  )

theme_download_map <- function(fig, fig_title_data, fig_source_data) {
  my_title <- fig_title_data#()
  my_source <- fig_source_data#()
  
  download_fig <- 
    fig +
    annotation_custom(grob = i2d2_logo,
                      xmin = -90.0, xmax = -90.65, 
                      ymin =  38.4, ymax =  41.2) +   
    labs(title = my_title$title[1],
         # subtitle = "subtitle goes here",
         caption = sprintf(
           "**Source:** I2D2, IA Data Drive, %s<br>**Data:** %s.<br>**Year:** %s<br>**Downloaded on:** %s",
           url, my_source$source, my_source$year, my_source$date
         ),
         tag = "Developed by Giorgi Chighladze",
         alt = "Iowa heatmap") +
    theme(
      plot.title = element_textbox_simple(size = 33, face = "bold", halign = 0.45, vjust = 0.5, lineheight = 1.5),
      plot.caption = element_markdown(size = 10, hjust = 0, margin = margin(l = 20), lineheight = 1.3),
      plot.tag.position = c(0.99, 0.19),
      plot.tag = element_text(hjust = 1, vjust = 1, size = 9, face = "bold.italic", color = "grey99"),
      plot.margin = margin(t = 5, b = 5, unit = "pt")
      ) +
    # align legend bar and its text
    guides(fill = guide_colorbar(barwidth = 20, barheight = 1.2)) +
    coord_sf(clip = "off")
  
  return(download_fig)
}
fig_title <- tibble(title = "This is titile")
theme_download_map(p1, fig_title, fig_titles)

  
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