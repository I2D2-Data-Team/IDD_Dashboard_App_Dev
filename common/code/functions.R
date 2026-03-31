# Read sf data for mapping State of Iowa
ia_state_map <- read_rds("../common/data/map_IA-state.rds")


# Function to Read Indicator Data Files ----------------------------------------
if (is.na(Sys.getenv("SHINY_SERVER_VERSION", unset = NA))) {
  message("Reading data from local CyBox...")
  
  # Read csv files from CyBox and transform
  read_my_csv <- function(dir, geo, file_name) {
    read_csv(file.path(Sys.getenv("USERPROFILE"), "Box/My IDS/_DATA/IDD", dir, geo, data_file_suffix(geo, file_name)),
             col_types = cols(.default = "c", year = "i", index = "d", count = "d")) %>%
      rename_geography_columns_to_fips(geo_name = geo) %>% 
      filter(!fips %in% 699)
  }
  
} else {
  message("Reading data from Azure Storage...")
  
  # Read environmental variables from local env file
  readRenviron("../.Renviron")
  
  # Set endpoint for azure container 
  azure_container_con <- function(container_name = "iowa-data-drive"){
    # Get variables from local environment
    account_url <- Sys.getenv("AZURE_STORAGE_URL") # WA_AZURE_DATA_URL
    access_sas <- Sys.getenv("AZURE_STORAGE_SAS") # WA_AZURE_STORAGE_URL https://isuaabiddstg.blob.core.windows.net/iowa-data-drive
    # Create a storage endpoint object
    endp_sas <- storage_endpoint(account_url, sas = access_sas)
    container <- storage_container(endpoint = endp_sas, name = container_name)
    return(container)
  }
  
  # Read csv files from Azure Blob and transform
  read_my_csv <- function(dir, geo, file_name) {
    storage_read_csv(azure_container_con(), file.path("Dashboards", dir, geo, data_file_suffix(geo, file_name)), 
                     col_types = cols(.default = "c", year = "i", index = "d", count = "d")) %>%
      rename_geography_columns_to_fips(geo_name = geo) %>% 
      filter(!fips %in% 699)
  }
}


# Rename geo codes for indicator data ------------------------------------------
# Data files for different indicators have a different names for assigned 
# three-digit codes (e.g. eci_number). This code replace those with fips
rename_geography_columns_to_fips <- function(df, geo_name) {
  my_com_cols <- c("year", "dimension", "measure", "indicator", "subset_level", "group_level", "index", "count")
  switch (geo_name,
          "IA-county" = df %>% select(fips, {my_com_cols}),
          "ECI-area" = df %>% select(fips = eci_number, {my_com_cols}),
          "HHS-region" = df %>% select(fips = region_number, {my_com_cols}),
          "HS-grantee" = df %>% select(fips = hs_number, {my_com_cols}),
          "MIECHV-area" = df %>% select(fips = miechv_number, {my_com_cols}),
          "No geography matched"
  )
}


# Select file name suffix based on selected geographies ------------------------ 
data_file_suffix <- function(geo_name, file_name) {
  # select correct suffix
  suf <-
    switch(
      geo_name,
      "IA-county"   = "_co.csv",
      "ECI-area"    = "_eci.csv",
      "HHS-region"  = "_rg.csv",
      "HS-grantee"  = "_hs.csv",
      "MIECHV-area" = "_mv.csv"
    )
  # combine file name with suffix
  name <- paste0(file_name, suf)
  return(name)
}



# Function to Create Suppress Labels -------------------------------------------
suppress_value_labels <- function(DATA) {
  DATA %>%
    mutate(sup_label = 
             case_when(
               index == -9999 ~ "- Suppressed", 
               is.na(index) ~ "- Not Available/Not Applicable",
               TRUE ~ ""),
           value =
             case_when(
               index == -9999 ~ 0, 
               is.na(index) ~ 0,
               TRUE ~ index),
           index = ifelse(index == -9999, NA_real_, index))
}


# Function for limiting year range selection -----------------------------------
select_my_year_range <- function(range, years = 5){
  if(range[[1]] > range[[2]] - years) {
    range} 
  else {
    c(range[[2]] - years, range[[2]])
  }
}


# Function to set numeric values (e.g. ages of group) in order -----------------
order_age <- function(x) {
  x[order(parse_number(x))]
}


# Function to format Y axis labels ---------------------------------------------
set_y_axis_label_format <- function(df, data_type = "percent", space_above_bar = 1.25) {
  my_max_value <- max(df$index, na.rm = TRUE)
  # my_min_value <- min(df$index, na.rm = TRUE)
  # Y_RANGE <- pretty(c(my_min_value, my_max_value), n = 5, shrink.sml = .01)
  if(my_max_value < 0.2) { ROUND <- 1 } else { ROUND <- 0 }

  # define max limit for Y axis 
  if (data_type == "percent") {
    Y_MAX <- ceiling(my_max_value * 100 * space_above_bar) / 100
    # fix max values to 5% if all values are suppressed or absence
    if (Y_MAX == 0) Y_MAX <- 0.05
  } else {
    Y_MAX <- ceiling(my_max_value * space_above_bar/10) * 10 
    # fix max values to 5 if all values are suppressed or absence
    if (Y_MAX == 0) Y_MAX <- 5
  }
  
  # show decimal point on Y axis if values range from 0 to 10
  if (my_max_value < .09) {
    Y_ACCURACY <- 1/10
  } else {
    Y_ACCURACY <- 1
  }
  
  # select format for Y scales
  Y_FORMAT <- if (data_type == "percent") {
    scales::percent
  } else {
    scales::comma
  }
  
  # determine length of legend text for geogrpahies
  my_geos <- unique(df$fips)
  G_LENGTH <- length(my_geos)
  G_TEXT_LENGTH <- str_length(paste(my_geos, collapse = ""))
  G_NROW <- G_TEXT_LENGTH %/% 70 + 1
  
  my_output <- list('ROUND' = ROUND, 'Y_MAX' = Y_MAX, 'Y_ACCURACY' = Y_ACCURACY, 'Y_FORMAT' = Y_FORMAT,
                    'G_LENGTH' = G_LENGTH, 'G_TEXT_LENGTH' = G_TEXT_LENGTH, 'G_NROW' = G_NROW)
  
  return(my_output)
}


# Function to adjust y axis of bar charts --------------------------------------
adjust_bar_chart_y_axis <- function(plot, y_format, data_type = "percent", stacked = FALSE) {
  if (stacked) {
    LIM <- c(NA, NA)
    EXP <- c(0.05, 0)
  } else {
    LIM <- c(NA, y_format$Y_MAX)
    EXP <- c(0, 0)
  }
  # adjust labels of the axis
  if (data_type == "percent") {
    BAR <-
      plot +
      scale_y_continuous(labels = scales::percent_format(accuracy = y_format$Y_ACCURACY),
                         limits = LIM,
                         expand = EXP)
  } else {
    BAR <-
      plot +
      scale_y_continuous(labels = scales::comma,
                         limits = LIM,
                         expand = EXP)
  }
  
  return(BAR)
}


# Function to format bar chart value labels ------------------------------------
adjust_bar_chart_v_labels <- function(plot, bar_spacing = 0.8, data_type = "percent", stacked = FALSE) {
  if (data_type == "percent") {
    BAR <-
      plot +
      geom_text(aes(label = scales::percent(index, accuracy = .1)),
                size = 4, vjust = 0.5, hjust = -0.2, angle = 90,
                position = position_dodge(width = bar_spacing))
  } else {
    BAR <-
      plot +
      geom_text(aes(label = scales::comma(index, accuracy = 1)),
                size = 4, vjust = 0.5, hjust = -0.2, angle = 90,
                position = position_dodge(width = bar_spacing))
  }
  
  if (stacked) {
    if (data_type == "percent") {
      BAR <- 
        plot +
        geom_text(aes(label = scales::percent(index, accuracy = .1)),
                  size = 4, color = "white",
                  position = position_stack(vjust = 0.5, reverse = TRUE))
    } else {
      BAR <- 
        plot +
        geom_text(aes(label = scales::comma(index, accuracy = 1)),
                  size = 4, color = "white",
                  position = position_stack(vjust = 0.5, reverse = TRUE))
    }
  }
  
  return(BAR)
}


# Function to adjust legend for rendered figures in browser --------------------
format_figure_legend_fit <- function(width, locations,
                                     geocodes = input$LOCATION_SELECT, 
                                     statewide = input$ADD_STATEWIDE) {
  # get current width (in pixels) of the plot output in the user's browser
  plot.width <- width
  # get codes of currently selected geographies (does not include statewide)
  geo_codes <- locations()[locations() %in% geocodes]
  # get names of those geographies
  geo_names <- names(geo_codes)
  # compute the length of all geographies including statewide (if selected)
  if (statewide) {
    length <- str_length(paste(geo_names, collapse = "")) + 10
  } else {
    length <- str_length(paste(geo_names, collapse = ""))
  }
  # compute length of two longest strings
  length2 <- geo_names %>% str_length() %>% sort(decreasing = TRUE)  %>% .[1:2] %>% sum(na.rm = TRUE)
  # set number of columns for legend based on comparison of string length with plot width
  if (plot.width < 10 * length) {
    if (plot.width < length2 *10) {
      n = 1
    } else {
      n = 2
    }
    font.size = 16 - n
  } else {
    n = NULL
    font.size = 16
  }
  # set font size for legend
  if (plot.width < 350 && length2 > 50) {font.size = font.size - 3}
  
  output <- list('font.size' = font.size, 'n.col' = n)
  
  return(output)
}



# Set themes for plots ---------------------------------------------------------

# theme for maps
theme_view_map <- 
  theme_void() +
  theme(legend.position = "bottom",
        legend.key.height = unit(4, "mm"),
        legend.key.width = unit(16, "mm"),
        legend.justification = c(0.45, 0.9),
        legend.text = element_text(size = 16, face = "bold"),
        text = element_text(family = 'Arial')
  ) 

# theme for trend lines
theme_view_trend <- 
  theme_minimal() +
  theme(panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_line(colour = "grey80"),
        text = element_text(size = 18, lineheight = 1, family = 'Arial'),
        legend.position = "bottom",
        legend.justification = "top",
        legend.spacing.x = unit(2, "mm"),
        legend.text = element_text(size = 16, face = "bold"),
        plot.background = element_rect(fill = 'white', colour = "white")
  )

# theme for bar charts
theme_view_bar <-
  theme(panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_line(colour = "grey80"),
        panel.spacing.x = unit(1, 'lines'),
        panel.background = element_rect(fill = "white"),
        strip.background = element_rect(fill = "gray90", color = "black"),
        text = element_text(size = 18, lineheight = 1, family = 'Arial'),
        legend.position = "bottom",
        legend.justification = "top",
        legend.spacing.x = unit(2, "mm"),
        legend.text = element_text(size = 16, face = "bold"),
        axis.text.x = element_text(vjust = -2),
        axis.ticks = element_blank(),
        plot.background = element_rect(fill = 'white', colour = "white"),
        plot.margin = margin(t = 5, b = 5, r = 20, l = 20,  unit = "mm"))


# Functions for making plots ---------------------------------------------------

## > DATA for MAP
compute_map_data <- function(DATA, YEAR_RANGE, LOCATIONS, DATA_TYPE){
  my_data <- 
    DATA %>%
    filter(year == YEAR_RANGE) %>%
    filter(fips != "19") %>%
    mutate(fips = as.integer(fips)) %>%
    select(-dimension, -count)
  
  # Define rounding for value labels
  if(max(my_data$index, na.rm = TRUE) < 0.2) {
    ROUND <- 1
  } else {
    ROUND <- 0
  }
  
  # prepare input data for leaflet maps
  map_data <-
    # get names of locations
    data.frame(location = names(LOCATIONS),
               fips = unlist(LOCATIONS, use.names = FALSE)) %>%
    # combine location names and data
    left_join(my_data, by = "fips") %>%
    mutate(variable_type = DATA_TYPE) %>%
    mutate(label = ifelse(variable_type == "percent", 
                          sprintf(paste0("%.", ROUND, "f%s"), index*100, "%"),
                          format(round(index), big.mark = ',', trim = TRUE)),
           # show suppressed instead of NA on popup/tooltip box
           label =
             case_when(
               index == -9999 ~ "Suppressed", 
               is.na(index) ~ "Not Available/Not Applicable",
               TRUE ~ label),
           index = ifelse(index == -9999, NA_real_, index)
    ) %>%
    rowwise() %>%
    mutate(popup_label =
             htmltools::HTML(
               sprintf('<b>%s</b>',
                       # htmltools::HTML(sprintf('<b>%s</b>
                       # <br><span style="padding-left: 10px;">Year: <b>%s</b>
                       # <br><span style="padding-left: 10px;">%s: <b>%s</b>',
                       location, year, "value", label))) %>%
    ungroup()
  
  
  return(map_data)
}

## > MAP plot for LEAFLET ------------------------------------------------------
plot_map_leaflet <- function(DATA, BASE_MAP, LOCATIONS,
                             DATA_TYPE = "percent",
                             OUTLINES = FALSE, LABELS = FALSE,
                             COL = "colors_red_gradient") {
  
  # set colors
  pal <- leaflet::colorNumeric(
    palette = map_colors[[COL]], 
    na.color = "whitesmoke",
    domain = na.exclude(DATA$index)
  )
  
  # Plot Iowa map
  my_leaflet_map <-
    BASE_MAP %>%
    left_join(DATA, c("fips")) %>%
    leaflet(options = leafletOptions(zoomControl = FALSE,
                                     minZoom = 6, maxZoom = 8,
                                     dragging = TRUE)) %>% 
    addTiles() %>%
    addPolygons(stroke = TRUE, 
                weight = 1,
                color = "grey",
                smoothFactor = 0.3, 
                fillOpacity = 0.975,
                fillColor = ~pal(index), 
                label = ~popup_label,
                labelOptions = labelOptions(
                  direction = "auto", 
                  textsize = "15px",
                  style = list(
                    "border-color" = "rgba(0,0,0,0.5)",
                    padding = "4px 8px"
                  )),
                highlightOptions = highlightOptions(
                  weight = 5,
                  color = "#666",
                  fillOpacity = 0.905,
                  bringToFront = FALSE)
    ) %>%
    addProviderTiles(providers$CartoDB.Positron)
  
  # set default text for NAs
  my_na_label <- "Not Available"
  
    # add continuous legend
    if (DATA_TYPE == "percent") {
      my_leaflet_map <-  
        my_leaflet_map %>%
        addLegendNumeric(pal = pal, values = na.omit(DATA$index), orientation = 'horizontal',
                         naLabel = my_na_label,
                         numberFormat = function(x) {
                           paste0(prettyNum(x*100, format = "f", big.mark = ",", digits = 3, scientific = FALSE), "%")
                         },
                         width = 120, height = 20, position = 'bottomright')
    } else {
      my_leaflet_map <-
        my_leaflet_map %>%
        addLegendNumeric(pal = pal, values = na.omit(DATA$index), orientation = 'horizontal',
                         naLabel = my_na_label,
                         numberFormat = function(x) {
                           prettyNum(x, format = "f", big.mark = ",", digits = 3, scientific = FALSE)
                         },
                         width = 120, height = 20, position = 'bottomright')
    }
  
    
  # exclude statewide from the list
  SELECTED_LOCS <- LOCATIONS[LOCATIONS != 19]
    
  # add names of selected locations
  if(LABELS & length(SELECTED_LOCS) > 0) {
    my_leaflet_map <- 
      my_leaflet_map %>%
      addLabelOnlyMarkers(
        data = BASE_MAP %>% filter(fips %in% SELECTED_LOCS),
        lng = ~long, lat = ~lat, label = ~name,
        labelOptions = labelOptions(noHide = TRUE, 
                                    direction = 'center',
                                    textOnly = TRUE
        ))
  } 
  
  # add outline of selected locations
  if(OUTLINES) {
    my_leaflet_map <- 
      my_leaflet_map %>%
      addPolylines(
        data = BASE_MAP %>% filter(fips %in% SELECTED_LOCS),
        stroke = TRUE,
        weight = 3,
        opacity = 0.75,
        color = "black") 
  } 
  
  # add outline of state
  my_leaflet_map <- 
    my_leaflet_map %>% 
    addPolylines(data = ia_state_map, highlightOptions = FALSE,
                 weight = 3,
                 opacity = 0.75,
                 color = "black")
  
  return(my_leaflet_map)
}


## > MAP plot for VIEW ---------------------------------------------------------
plot_map_view <- function(DATA, BASE_MAP, LOCATIONS,
                          DATA_TYPE = "percent",
                          OUTLINES = FALSE, LABELS = FALSE,
                          COL = "colors_red_gradient"){
  my_map <-
    BASE_MAP %>%
    left_join(DATA, by = "fips") %>%
    ggplot() +
    geom_sf(aes(fill = index), linewidth = 0.2) +
    geom_sf(data = ia_state_map, fill = NA, linewidth = 1.2, color = "black") +
    scale_fill_continuous(
      labels = ~ (if (DATA_TYPE == "percent") scales::percent(.x) else scales::comma(.x)), 
      breaks = c(min(DATA$index, na.rm = TRUE), max(DATA$index, na.rm = TRUE)),
      na.value = "grey90",
      name = NULL,
      palette = map_colors[[COL]]) +
    theme_view_map +
    # align legend bar and its text and remove ticks
    guides(
      fill = guide_colorbar(
        label.theme = element_text(hjust = c(1.1, -0.1), vjust = 5),
        ticks = FALSE)
    )
  
  # add names of the counties
  if(LABELS & !is.null(LOCATIONS)) {
    my_map <- 
      my_map +
      geom_text(data = BASE_MAP %>% filter(fips %in% LOCATIONS),
                aes(long, lat, label = str_wrap(name, 16)),
                lineheight = 1.1, size = 4)
  } 
  
  # outline selected counties
  if(OUTLINES & !is.null(LOCATIONS)) {
    my_map <- 
      my_map +
      geom_sf(data = BASE_MAP %>% filter(fips %in% LOCATIONS),
              fill = NA, color = "black", linewidth = 1)
  } 
  
  return(my_map)
}


## > MAP plot for DOWNLOAD -----------------------------------------------------
format_map_download <- function(fig, fig_title_data, fig_source_data) {
  my_title <- fig_title_data()
  my_source <- fig_source_data()
  url <- "https:// iadatadrive.i2d2.iastate.edu"
  
  download_fig <- 
    fig +
    labs(title = my_title$title[1],
         subtitle = " ",
         caption = sprintf(
           "**Source:** I2D2, IA Data Drive, %s<br>**Data:** %s.<br>**Year:** %s<br>**Downloaded on:** %s",
           url, my_source$source, my_source$year, my_source$date
         ),
         # tag = "Designed by Giorgi Chighladze",
         alt = "Iowa heatmap") +
    theme(
      plot.title = element_textbox_simple(size = 30, face = "bold", halign = 0.45, vjust = 0.5, lineheight = 1.5,  maxwidth = unit(9, "in")),
      plot.caption = element_markdown(size = 10, hjust = 0, margin = margin(l = 20), lineheight = 1.3),
      plot.tag.position = c(0.99, 0.19),
      plot.tag = element_text(hjust = 1, vjust = 1, size = 9, face = "bold.italic", color = "grey99"),
      plot.margin = margin(t = 15, b = 15, l = 10, r = 10, unit = "pt")
    ) +
    # align legend bar and its text and remove ticks
    guides(
      fill = guide_colorbar(
        barwidth = 20, 
        barheight = 1.2,
        label.theme = element_text(hjust = c(1.1, -0.1), vjust = 5, size = 20),
        ticks = FALSE)
    ) +
    coord_sf(clip = "off")
  
  return(download_fig)
}


## > TREND plot for VIEW -------------------------------------------------------
plot_trend_view <- function(DATA, DATA_TYPE = "percent", LOCATIONS, STATEWIDE = TRUE, LABELS = FALSE){
  
  # make statewide black
  if (STATEWIDE) {
    my_color_palette <- c("black", all_8_colors[1:length(LOCATIONS)])
  } else {
    my_color_palette <- all_8_colors[1:length(LOCATIONS)]
  }
  
  # Set Y axis label format
  Y <- set_y_axis_label_format(DATA, DATA_TYPE)
  
  # Define steps for x label increments
  x_steps <- ceiling(length(unique(DATA$year)) / 7)
  
  my_trend <-
    DATA %>%
    mutate(indicator_type = DATA_TYPE) %>%
    mutate(label = ifelse(indicator_type == "percent", 
                          round(index * 100, Y$ROUND) %>%
                            format(nsmall = Y$ROUND) %>%
                            paste0("%"),
                          round(index) %>%
                            format(big.mark = ",")),
           label = str_squish(label)) %>%
    ggplot(aes(year, index, color = fips)) +
    geom_point(size = 1.2) +
    geom_line(linewidth = 0.8) +
    scale_x_continuous(name = NULL, breaks = seq(2000, 2100, x_steps)) +
    scale_y_continuous(labels = Y$Y_FORMAT) +
    labs(y = NULL,
         color = NULL, 
         fill = NULL,
         alt = "Select County to Desplay Plot") +
    scale_color_manual(name = NULL, values = my_color_palette) +
    theme_view_trend +
    theme(legend.text = element_text(size = 16 - Y$G_NROW)) +
    guides(color = guide_legend(nrow = Y$G_NROW))
  
  # add value labels
  if(LABELS) {
    my_trend <- 
      my_trend +
      geom_point(size = 3) +
      geom_line(linewidth = 1.2) +
      geom_text_repel(aes(label = label), direction = 'y', size = 4, show.legend = FALSE)
  } 
  
  return(my_trend)
}


## > LINE plot for VIEW --------------------------------------------------------
plot_line_view <- function(DATA, DATA_TYPE = "percent", LOCATIONS, STATEWIDE = TRUE, LABELS = FALSE,
                           GROUPS = NULL){
  
  my_color_palette <- all_8_colors
  # my_color_palette <- hcl.colors(length(GROUPS), "Zissou 1") # check more hcl colors here https://colorspace.r-forge.r-project.org/articles/hcl_palettes.html
  
  # Set Y axis label format
  Y <- set_y_axis_label_format(DATA, DATA_TYPE)
  
  # Define steps for x label increments
  x_steps <- ceiling(length(unique(DATA$year)) / 7)
  
  my_trend <-
    DATA %>%
    mutate(indicator_type = DATA_TYPE) %>%
    mutate(label = ifelse(indicator_type == "percent", 
                          round(index * 100, Y$ROUND) %>%
                            format(nsmall = Y$ROUND) %>%
                            paste0("%"),
                          round(index) %>%
                            format(big.mark = ",")),
           label = str_squish(label)) %>%
    ggplot(aes(year, index, color = group_level)) +
    geom_point(size = 1.2) +
    geom_line(linewidth = 0.8) +
    scale_x_continuous(name = NULL, breaks = seq(2000, 2100, x_steps)) +
    scale_y_continuous(labels = Y$Y_FORMAT) +
    labs(y = NULL,
         color = NULL, 
         fill = NULL) +
    scale_color_manual(name = NULL, values = my_color_palette) +
    theme_view_trend +
    guides(color = guide_legend(title.position = "top", nrow = 1))
  
  # add value labels
  if(LABELS) {
    my_trend <- 
      my_trend +
      geom_point(size = 3) +
      geom_line(linewidth = 1.2) +
      geom_text_repel(aes(label = label), direction = 'y', size = 4, show.legend = FALSE)
  } 
  
  return(my_trend)
}


## > BAR plot for VIEW ---------------------------------------------------------
plot_bar_view <- function(DATA, DATA_TYPE = "percent", LOCATIONS, 
                          STATEWIDE = TRUE, LABELS = FALSE, FACET = FALSE,
                          SPECIAL_CASE = FALSE){
  
  # make statewide black
  if (STATEWIDE) {
    my_color_palette <- c("black", all_8_colors[1:length(LOCATIONS)])
  } else {
    my_color_palette <- all_8_colors[1:length(LOCATIONS)]
  }
  
  if (SPECIAL_CASE) my_color_palette <- "black"
  
  # create value and sup_label for handling suppressed and not available values
  my_data <- suppress_value_labels(DATA)
  
  # Set Y axis label format
  Y <- set_y_axis_label_format(DATA, DATA_TYPE)
  
  # set spacing between groups of bars
  my_sbg <- 0.8
  # adjust column width to spacing between bars
  my_clw <- my_sbg*0.95
  
  my_bar <-
    my_data %>% 
    ggplot(aes(x = group_level, 
               y = value,
               fill = fips)) +
    geom_col(width = my_clw, position = position_dodge(width = my_sbg, preserve = "single")) +
    geom_text(aes(label = sup_label), size = 5, angle = 90, hjust = 0, vjust = 0.5,
              position = position_dodge(width = my_sbg)) +
    scale_fill_manual(values = my_color_palette) +
    labs(x = NULL, y = NULL, fill = NULL) +
    # facet_grid(~ group, scales = 'free', space = 'free') +
    theme_view_bar +
    theme(legend.text = element_text(size = 16 - Y$G_NROW)) +
    guides(fill = guide_legend(nrow = Y$G_NROW))
  
  # adjust labels of the axis
  my_bar <- adjust_bar_chart_y_axis(my_bar, Y, DATA_TYPE)
  
  # add value labels
  if(LABELS) {
    my_bar <- adjust_bar_chart_v_labels(my_bar, my_sbg, DATA_TYPE)
  } 
  
  # add faceting 
  if (FACET == TRUE) {
    my_bar <-
      my_bar +
      facet_grid(~ group, scales = 'free', space = 'free') +
      theme(panel.border = element_rect(color = "black", fill = NA))
  }
  
  return(my_bar)
}


## > BAR plot for VIEW 2 -------------------------------------------------------
plot_bar_view2 <- function(DATA, DATA_TYPE = "percent", LOCATIONS, 
                          STATEWIDE = TRUE, LABELS = FALSE, FACET = FALSE,
                          SPECIAL_CASE = FALSE){
  GROUPS <- unique(DATA$group_level)
  
  # assign colors
  my_color_palette <- all_8_colors[1:length(GROUPS)]
  
  # create value and sup_label for handling suppressed and not available values
  my_data <- suppress_value_labels(DATA)
  
  # Set Y axis label format
  Y <- set_y_axis_label_format(DATA, DATA_TYPE)
  
  # set spacing between groups of bars
  my_sbg <- 0.8
  # adjust column width to spacing between bars
  my_clw <- my_sbg*0.95
  
  my_bar <-
    my_data %>% 
    ggplot(aes(x = fips, 
               y = value,
               fill = group_level)) +
    geom_col(width = my_clw, position = position_dodge(width = my_sbg, preserve = "single")) +
    geom_text(aes(label = sup_label), size = 5, angle = 90, hjust = 0, vjust = 0.5,
              position = position_dodge(width = my_sbg)) +
    scale_fill_manual(values = my_color_palette) +
    scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
    labs(x = NULL, y = NULL, fill = NULL) +
    # facet_grid(~ group, scales = 'free', space = 'free') +
    theme_view_bar +
    theme(axis.text.x = element_text(vjust = 1))
  
  # adjust labels of the axis
  my_bar <- adjust_bar_chart_y_axis(my_bar, Y, DATA_TYPE)
  
  # add value labels
  if(LABELS) {
    my_bar <- adjust_bar_chart_v_labels(my_bar, my_sbg, DATA_TYPE)
  } 
  
  # add faceting 
  if (FACET == TRUE) {
    my_bar <-
      my_bar +
      facet_grid(~ group, scales = 'free', space = 'free') +
      theme(panel.border = element_rect(color = "black", fill = NA))
  }
  
  return(my_bar)
}


## > STACKED BAR plot for VIEW -------------------------------------------------
plot_bar_stacked_view <- function(DATA, DATA_TYPE = "percent",
                                  LABELS = FALSE, FACET = FALSE){
  
  my_color_palette <- all_8_colors[1:3]
  # my_color_palette <- c("green4", "lightblue3", "salmon")
  
  # create value and sup_label for handling suppressed and not available values
  my_data <- suppress_value_labels(DATA)
  
  # Set Y axis label format
  Y <- set_y_axis_label_format(DATA, DATA_TYPE)
  
  # set spacing between groups of bars
  my_sbg <- 0.8
  # adjust column width to spacing between bars
  my_clw <- my_sbg*0.95
  
  my_bar <-
    my_data %>% 
    ggplot(aes(x = group_level, 
               y = value,
               fill = subset_level)) +
    geom_col(width = my_clw, position = position_stack(vjust = 0, reverse = TRUE)) +
    scale_fill_manual(values = my_color_palette) +
    labs(x = NULL, y = NULL, fill = NULL) +
    theme_view_bar +
    theme(plot.margin = margin(t = 5, b = 5, r = 4, l = 2,  unit = "mm"))
  
  # adjust labels of the axis
  my_bar <- adjust_bar_chart_y_axis(my_bar, Y, DATA_TYPE, stacked = TRUE)
  
  # add value labels
  if(LABELS) {
    my_bar <- adjust_bar_chart_v_labels(my_bar, my_sbg, DATA_TYPE, stacked = TRUE)
  } 
  
  # add faceting 
  if (FACET == TRUE) {
    my_bar <-
      my_bar +
      facet_grid(~ group, scales = 'free', space = 'free') +
      theme(panel.border = element_rect(color = "black", fill = NA))
  }
  
  return(my_bar)
}


## > PIE chart for VIEW --------------------------------------------------------
plot_pie_view <- function(DATA, LABELS = FALSE){
  
  my_color_palette <- all_8_colors[1:3]
  # my_color_palette <- c("green4", "lightblue3", "salmon")
  
  my_bar <- 
    DATA %>% 
    filter(group_level != "Hispanic") %>%
    group_by(subset_level) %>%
    summarise(count = sum(count)) %>%
    ungroup() %>%
    mutate(index = count/sum(count)) %>%
    mutate(y_pos = cumsum(index) - 0.5 * index) %>%
    ggplot(aes(x = "", y = index, fill= subset_level)) +
    geom_bar(stat = "identity", colour = "white", linewidth = 0.5) +
    coord_polar("y", start = 0) +
    labs(fill = NULL, x = NULL, y = NULL) +
    scale_fill_manual(values = my_color_palette) +
    theme(panel.grid.minor.x = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.spacing.x = unit(1, 'lines'),
          panel.background = element_rect(fill = "white"),
          strip.background = element_rect(fill = "gray90", color = "black"),
          text = element_text(size = 18, lineheight = 1, family = 'Arial'),
          legend.position = "bottom",
          legend.justification = "top",
          legend.spacing.x = unit(2, "mm"),
          legend.text = element_text(size = 16, face = "bold"),
          axis.ticks = element_blank(),
          axis.text = element_blank(),
          plot.background = element_rect(fill = 'white', colour = "white"),
          plot.margin = margin(t = 5, b = 5, r = 4, l = 2,  unit = "mm"))
  
  # add value labels
  if(LABELS) {
    my_bar <- 
      my_bar +
      geom_text(aes(y = y_pos, label = paste0(round(index*100), "%")), 
                size = 7, fontface = "bold", color = "white")
  } else {
    my_bar
  }
  
  return(my_bar)
}


## > CARTESIAN plot for DOWNLOAD -----------------------------------------------------
format_figs_download <- function(fig, fig_title, fig_source_data, figure_number) {
  my_title <- fig_title() %>% filter(figure == figure_number) %>% pull(title)
  my_source <- fig_source_data()
  url <- "https:// iadatadrive.i2d2.iastate.edu"
  
  download_fig <- 
    fig +
    labs(title = my_title,
         subtitle = " ",
         caption = sprintf(
           "<br>**Source:** I2D2, IA Data Drive, %s<br>**Data:** %s.<br>**Year:** %s<br>**Downloaded on:** %s",
           url, my_source$source, my_source$year, my_source$date
         ),
         # tag = "Designed by Giorgi Chighladze",
         alt = "Iowa figures") +
    theme(
      plot.title = element_textbox_simple(size = 33, face = "bold", halign = 0.45, vjust = 0.5, lineheight = 1.5,  maxwidth = unit(9, "in")),
      plot.caption = element_markdown(size = 10, hjust = 0, margin = margin(l = 20), lineheight = 1.3),
      plot.tag.position = c(0.99, 0.19),
      plot.tag = element_text(hjust = 1, vjust = 1, size = 9, face = "bold.italic", color = "grey99"),
      plot.margin = margin(t = 15, b = 15, l = 10, r = 10, unit = "pt")
    ) +
    coord_cartesian(clip = "off")
  
  return(download_fig)
}


