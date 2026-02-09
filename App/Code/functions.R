library(tidyverse)
library(ggplot2)
library(ggrepel)
library(ggtext)
library(magick)
library(AzureStor)


# Function to Read Indicator Data Files ----------------------------------------
if (is.na(Sys.getenv("SHINY_SERVER_VERSION", unset = NA))) {
  message("Reading data from local CyBox...")
  
  # Read csv files from CyBox and transform
  read_my_csv <- function(dir, geo, file_name) {
    read_csv(file.path(Sys.getenv("USERPROFILE"), "Box/My IDS/_DATA/IDD", dir, geo, data_file_suffix(geo, file_name)),
             col_types = cols(.default = "c", year = "i", index = "d", count = "d")) %>%
      rename_geography_columns_to_fips(geo_name = geo)
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
      rename_geography_columns_to_fips(geo_name = geo)
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


# Set themes for plots ---------------------------------------------------------

# theme for maps
theme_view_map <- 
  theme_void() +
  theme(legend.position = "bottom",
        legend.key.height = unit(4, "mm"),
        legend.key.width = unit(16, "mm"),
        legend.justification = c(0.45, 0.9),
        legend.text = element_text(size = 16, face = "bold", hjust = 1.5, vjust = 7) # hjust = c(1.5, -0.5)
  )

# theme for trend lines
theme_view_trend <- 
  theme_minimal() +
  theme(panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_line(colour = "grey80"),
        text = element_text(size = 18, lineheight = 1),
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
        text = element_text(size = 18, lineheight = 1),
        legend.position = "bottom",
        legend.justification = "top",
        legend.spacing.x = unit(2, "mm"),
        legend.text = element_text(size = 16, face = "bold"),
        axis.text.x = element_text(vjust = -2),
        axis.ticks = element_blank(),
        plot.background = element_rect(fill = 'white', colour = "white"),
        plot.margin = margin(t = 5, b = 5, r = 20, l = 20,  unit = "mm"))


# Functions for making plots ---------------------------------------------------

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
    guides(fill = guide_colorbar(ticks = FALSE))
  
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


## > TREND plot for VIEW -------------------------------------------------------
plot_trend_view <- function(DATA, DATA_TYPE = "percent", LOCATIONS, STATEWIDE = TRUE, LABELS = FALSE){
  
  # make statewide black
  if (STATEWIDE) {
    my_color_palette <- c("black", all_8_colors[1:length(LOCATIONS)])
  } else {
    my_color_palette <- all_8_colors[1:length(LOCATIONS)]
  }
  
  # Define value labels
  my_max_value <- max(DATA$index, na.rm = TRUE)
  my_min_value <- min(DATA$index, na.rm = TRUE)
  Y_RANGE <- pretty(c(my_min_value, my_max_value), n = 5, shrink.sml = .01)
  if(my_max_value < 0.2) { ROUND <- 1 } else { ROUND <- 0 }
  
  # select format for Y scales
  y_label_format <- if (DATA_TYPE == "percent") {
    scales::percent
  } else {
    scales::comma
  }
  
  # Define steps for x label increments
  x_steps <- ceiling(length(unique(DATA$year)) / 7)
  
  my_trend <-
    DATA %>%
    mutate(indicator_type = DATA_TYPE) %>%
    mutate(label = ifelse(indicator_type == "percent", 
                          round(index * 100, ROUND) %>%
                            format(nsmall = ROUND) %>%
                            paste0("%"),
                          round(index) %>%
                            format(big.mark = ",")),
           label = str_squish(label)) %>%
    ggplot(aes(year, index, color = fips)) +
    geom_point(size = 1.2) +
    geom_line(linewidth = 0.8) +
    scale_x_continuous(name = NULL, breaks = seq(2000, 2100, x_steps)) +
    scale_y_continuous(labels = y_label_format) +
    labs(y = NULL,
         color = NULL, 
         fill = NULL,
         alt = "Select County to Desplay Plot") +
    scale_color_manual(name = NULL, values = my_color_palette) +
    theme_view_trend
  
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
  
  # Define value labels
  my_max_value <- max(DATA$index, na.rm = TRUE)
  my_min_value <- min(DATA$index, na.rm = TRUE)
  Y_RANGE <- pretty(c(my_min_value, my_max_value), n = 5, shrink.sml = .01)
  if(my_max_value < 0.2) { ROUND <- 1 } else { ROUND <- 0 }
  
  # select format for Y scales
  y_label_format <- if (DATA_TYPE == "percent") {
    scales::percent
  } else {
    scales::comma
  }
  
  # Define steps for x label increments
  x_steps <- ceiling(length(unique(DATA$year)) / 7)
  
  my_trend <-
    DATA %>%
    mutate(indicator_type = DATA_TYPE) %>%
    mutate(label = ifelse(indicator_type == "percent", 
                          round(index * 100, ROUND) %>%
                            format(nsmall = ROUND) %>%
                            paste0("%"),
                          round(index) %>%
                            format(big.mark = ",")),
           label = str_squish(label)) %>%
    ggplot(aes(year, index, color = group_level)) +
    geom_point(size = 1.2) +
    geom_line(linewidth = 0.8) +
    scale_x_continuous(name = NULL, breaks = seq(2000, 2100, x_steps)) +
    scale_y_continuous(labels = y_label_format) +
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
  
  # Define value labels
  my_max_value <- max(my_data$value, na.rm = TRUE)
  my_min_value <- min(my_data$value, na.rm = TRUE)
  Y_RANGE <- pretty(c(my_min_value, my_max_value), n = 5, shrink.sml = .01)
  if(my_max_value < 0.2) { ROUND <- 1 } else { ROUND <- 0 }
  
  # show decimal point on Y axis if values range from 0 to 10
  if (my_max_value < .09) {
    Y_ACCURACY <- 1/10
  } else {
    Y_ACCURACY <- 1
  }
  
  # define max limit for Y axis
  if (DATA_TYPE == "percent") {
    Y_MAX <- ceiling(my_max_value * 100 * 1.15) / 100
    # fix max values to 5% if all values are suppressed or absence
    if (Y_MAX == 0) Y_MAX <- 0.05
  } else {
    Y_MAX <- ceiling(my_max_value * 1.15/10) * 10 
    # fix max values to 5 if all values are suppressed or absence
    if (Y_MAX == 0) Y_MAX <- 5
  }
  
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
    theme_view_bar
  
  # add value labels
  if(LABELS) {
    if (DATA_TYPE == "percent") {
      my_bar <- 
        my_bar +
        geom_text(aes(label = scales::percent(index, accuracy = .1)),
                  size = 4, vjust = -1, position = position_dodge(width = my_sbg))
    } else {
      my_bar <- 
        my_bar +
        geom_text(aes(label = scales::comma(index, accuracy = 1)),
                  size = 4, vjust = -1, position = position_dodge(width = my_sbg))
    }
  } 
  
  # adjust labels of the axis
  if (DATA_TYPE == "percent") {
    my_bar <-
      my_bar +
      scale_y_continuous(labels = scales::percent_format(accuracy = Y_ACCURACY),
                         limits = c(NA, Y_MAX),
                         expand = c(0, 0))
  } else {
    my_bar <-
      my_bar +
      scale_y_continuous(labels = scales::comma,
                         limits = c(NA, Y_MAX),
                         expand = c(0, 0))
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
  
  # Define value labels
  my_max_value <- max(my_data$value, na.rm = TRUE)
  my_min_value <- min(my_data$value, na.rm = TRUE)
  Y_RANGE <- pretty(c(my_min_value, my_max_value), n = 5, shrink.sml = .01)
  if(my_max_value < 0.2) { ROUND <- 1 } else { ROUND <- 0 }
  
  # show decimal point on Y axis if values range from 0 to 10
  if (my_max_value < .09) {
    Y_ACCURACY <- 1/10
  } else {
    Y_ACCURACY <- 1
  }
  
  # define max limit for Y axis
  if (DATA_TYPE == "percent") {
    Y_MAX <- ceiling(my_max_value * 100 * 1.15) / 100
    # fix max values to 5% if all values are suppressed or absence
    if (Y_MAX == 0) Y_MAX <- 0.05
  } else {
    Y_MAX <- ceiling(my_max_value * 1.15/10) * 10 
    # fix max values to 5 if all values are suppressed or absence
    if (Y_MAX == 0) Y_MAX <- 5
  }
  
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
    labs(x = NULL, y = NULL, fill = NULL) +
    # facet_grid(~ group, scales = 'free', space = 'free') +
    theme_view_bar
  
  # add value labels
  if(LABELS) {
    if (DATA_TYPE == "percent") {
      my_bar <- 
        my_bar +
        geom_text(aes(label = scales::percent(index, accuracy = .1)),
                  size = 4, vjust = -1, position = position_dodge(width = my_sbg))
    } else {
      my_bar <- 
        my_bar +
        geom_text(aes(label = scales::comma(index, accuracy = 1)),
                  size = 4, vjust = -1, position = position_dodge(width = my_sbg))
    }
  } 
  
  # adjust labels of the axis
  if (DATA_TYPE == "percent") {
    my_bar <-
      my_bar +
      scale_y_continuous(labels = scales::percent_format(accuracy = Y_ACCURACY),
                         limits = c(NA, Y_MAX),
                         expand = c(0, 0))
  } else {
    my_bar <-
      my_bar +
      scale_y_continuous(labels = scales::comma,
                         limits = c(NA, Y_MAX),
                         expand = c(0, 0))
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
  
  # Define value labels
  my_max_value <- max(my_data$value, na.rm = TRUE)
  my_min_value <- min(my_data$value, na.rm = TRUE)
  Y_RANGE <- pretty(c(my_min_value, my_max_value), n = 5, shrink.sml = .01)
  if(my_max_value < 0.2) { ROUND <- 1 } else { ROUND <- 0 }
  
  # show decimal point on Y axis if values range from 0 to 10
  if (my_max_value < .09) {
    Y_ACCURACY <- 1/10
  } else {
    Y_ACCURACY <- 1
  }
  
  # define max limit for Y axis
  if (DATA_TYPE == "percent") {
    Y_MAX <- ceiling(my_max_value * 100 * 1.15) / 100
    # fix max values to 5% if all values are suppressed or absence
    if (Y_MAX == 0) Y_MAX <- 0.05
  } else {
    Y_MAX <- ceiling(my_max_value * 1.15/10) * 10 
    # fix max values to 5 if all values are suppressed or absence
    if (Y_MAX == 0) Y_MAX <- 5
  }
  
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
  
  # add value labels
  if(LABELS) {
    if (DATA_TYPE == "percent") {
      my_bar <- 
        my_bar +
        geom_text(aes(label = scales::percent(index, accuracy = .1)),
                  size = 4, position = position_stack(vjust = 0.5, reverse = TRUE))
    } else {
      my_bar <- 
        my_bar +
        geom_text(aes(label = scales::comma(index, accuracy = 1)),
                  size = 4, position = position_stack(vjust = 0.5, reverse = TRUE))
    }
  } 
  
  # adjust labels of the axis
  if (DATA_TYPE == "percent") {
    my_bar <-
      my_bar +
      scale_y_continuous(labels = scales::percent_format(accuracy = Y_ACCURACY))
  } else {
    my_bar <-
      my_bar +
      scale_y_continuous(labels = scales::comma)
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
          text = element_text(size = 18, lineheight = 1),
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


