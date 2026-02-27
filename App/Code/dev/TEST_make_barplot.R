################################################################################
## TESTING Technology ----------------------------------------------------------
################################################################################
library(tidyverse)
source("R/remote_data.R")

data <- read_my_csv(dim_dir, "HS-grantee", "hse_tch_fig1_2_3")
my_locations <- sort(c(211, 216, 217, 205, 203, 210, 213))
my_locations_names <- hs_grantee_droplist[hs_grantee_droplist %in% my_locations]

df <- 
  data %>%
  filter(year == 2023) %>%
  filter(fips %in% c(my_locations, 19)) %>%
  filter(subset_level == "Computing Device") %>%
  # mutate(group = group_level, group_level = subset_level) %>%
  mutate(fips = factor(fips,
                       levels = c("Statewide" = 19, my_locations),
                       labels = names(c("Statewide" = 19, my_locations_names))),
         group = "none",
         group_level = factor(group_level, levels = group.hse.tch.cmp),
         indicator = 'Technology Access')

fig_titles <-
  metadata.fig.titles %>% 
  filter(measure == unique(df$measure), indicator == unique(df$indicator)) %>% 
  filter(figure == 3) %>% pull(title)

current_indicator_source_fig <- 
  metadata.fig.sources  %>% 
  filter(measure == unique(df$measure), indicator == unique(df$indicator)) 

# select special string for concatenating source when more than one source
if (nrow(current_indicator_source_fig) > 1) {
  my_data_source_list <- "<br><span style='color:white'>data:</span>- "
} else {
  my_data_source_list <- ""
}
# generate a table with source info for figures
my_source <-
  current_indicator_source_fig %>%
  mutate(source = paste0(my_data_source_list, source, ", ", data)) %>%
  group_by(measure, indicator) %>%
  summarise(source = str_flatten(unique(source), collapse = ";"),
            date = format(max(as.Date(date_obtained, "%m/%d/%y")), "%B %d, %Y"),
            year = as.integer(max(max_year)),
            years = paste0(max(min_year), "-", max(max_year))
  ) %>%
  ungroup()

DATA <- df
GROUPS <- unique(DATA$group_level)
DATA_TYPE <- "percent"
my_color_palette <- all_8_colors[1:length(GROUPS)]
my_data <- suppress_value_labels(DATA)
Y <- set_y_axis_label_format(DATA, DATA_TYPE)
my_sbg <- 0.8
my_clw <- my_sbg*0.95
my_bar <-
  my_data %>% 
  ggplot(aes(x = fips, 
             y = value,
             fill = group_level)) +
  geom_col(width = my_clw, position = position_dodge(width = my_sbg, preserve = "single")) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
  geom_text(aes(label = sup_label), size = 5, angle = 90, hjust = 0, vjust = 0.5,
            position = position_dodge(width = my_sbg)) +
  scale_fill_manual(values = my_color_palette) +
  labs(x = NULL, y = NULL, fill = NULL) +
  # theme_view_bar
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
        # axis.text.x = element_text(vjust = -2),
        axis.text.x = element_text(vjust = 1),
        axis.ticks = element_blank(),
        plot.background = element_rect(fill = 'white', colour = "white"),
        plot.margin = margin(t = 5, b = 5, r = 20, l = 20,  unit = "mm"))

# plot the barchart
df %>%
  plot_bar_view2(DATA_TYPE =  "percent",
                 LOCATIONS = my_locations,
                 STATEWIDE = TRUE,
                 LABELS = TRUE,
                 FACET = FALSE) +
  # scale_x_discrete(labels = scales::label_wrap(width = 10)) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
  labs(title = fig_titles,
       subtitle = " ",
       caption = sprintf(
         "<br>**Source:** I2D2, IA Data Drive, %s<br>**Data:** %s.<br>**Year:** %s<br>**Downloaded on:** %s",
         "https:// iadatadrive.i2d2.iastate.edu", my_source$source, my_source$year, my_source$date
       ),
       # tag = "Designed by Giorgi Chighladze",
       alt = "Iowa figures") +
  theme(
    # plot.title = ggtext::element_textbox_simple(size = 33, face = "bold", halign = 0.45, vjust = 0.5, lineheight = 1.5),
    plot.caption = element_markdown(size = 10, hjust = 0, margin = margin(l = 20), lineheight = 1.3),
    plot.tag.position = c(0.99, 0.19),
    plot.tag = element_text(hjust = 1, vjust = 1, size = 9, face = "bold.italic", color = "grey99"),
    plot.margin = margin(t = 15, b = 15, l = 10, r = 10, unit = "pt")
  ) +
  theme(axis.text.x = element_text(vjust = 1)) +
  coord_cartesian(clip = "off")

ggsave(filename = "TEST.png",
       width = 10, height = 8, scale = 1.25, dpi = 150, bg = "white")
#






################################################################################
## TESTING Languages ----------------------------------------------------------- 
################################################################################

# County Data
data <- read_my_csv(dim_dir, "IA-county", "hse_lng_fig3")
my_locations <- sort(c(19057, 19023, 19143, 19113))
my_locations_names <- ia_county_droplist[ia_county_droplist %in% my_locations]
# HS Grantee Data
data <- read_my_csv(dim_dir, "HS-grantee", "hse_lng_fig3")
my_locations <- sort(c(211, 216, 217, 205, 203, 210, 213))
# my_locations <- sort(c(211, 216, 217))
my_locations_names <- hs_grantee_droplist[hs_grantee_droplist %in% my_locations]


df <- 
  data %>%
  mutate(group = group_level, group_level = subset_level) %>%
  filter(year == 2023) %>%
  filter(fips %in% c(my_locations, 19)) %>%
  mutate(fips = factor(fips,
                       levels = c("Statewide" = 19, my_locations),
                       labels = names(c("Statewide" = 19, my_locations_names))),
         
         group = factor(group, levels = group.hse.lng),
         group_level = factor(group_level, levels = subset.hse.lng))

fig_titles <-
  metadata.fig.titles %>% 
  filter(measure == unique(df$measure), indicator == unique(df$indicator)) %>% 
  filter(figure == 3) %>% pull(title)

current_indicator_source_fig <- 
  metadata.fig.sources  %>% 
  filter(measure == unique(df$measure), indicator == unique(df$indicator)) 

# select special string for concatenating source when more than one source
if (nrow(current_indicator_source_fig) > 1) {
  my_data_source_list <- "<br><span style='color:white'>data:</span>- "
} else {
  my_data_source_list <- ""
}
# generate a table with source info for figures
my_source <-
  current_indicator_source_fig %>%
  mutate(source = paste0(my_data_source_list, source, ", ", data)) %>%
  group_by(measure, indicator) %>%
  summarise(source = str_flatten(unique(source), collapse = ";"),
            date = format(max(as.Date(date_obtained, "%m/%d/%y")), "%B %d, %Y"),
            year = as.integer(max(max_year)),
            years = paste0(max(min_year), "-", max(max_year))
  ) %>%
  ungroup()


source("Code/dev/TEST_barplot.R")

if (length(my_locations) > 3) {
  width_scale <- 30 / (str_length(paste(unique(df$fips), collapse = "")) / length(my_locations))
} else {
  width_scale <- 1
}
# plot the barchart
p3 <- df %>%
  mutate(index = count) %>%
  plot_bar_view2("percentNO", my_locations, FACET = TRUE, LABELS =  TRUE) +
  # geom_text(aes(label = scales::percent(index, accuracy = .1)), 
  #           size = 4, 
  #           # THE IS NEW
  #           vjust = 0.5, hjust = -0.2, angle = 90,
  #           
  #           position = position_dodge(width = my_sbg)) + 
  labs(title = fig_titles,
       subtitle = " ",
       caption = sprintf(
         "<br>**Source:** I2D2, IA Data Drive, %s<br>**Data:** %s.<br>**Year:** %s<br>**Downloaded on:** %s",
         "https:// iadatadrive.i2d2.iastate.edu", my_source$source, my_source$year, my_source$date
       ),
       # tag = "Designed by Giorgi Chighladze",
       alt = "Iowa figures") +
  theme(
    plot.title = element_textbox_simple(size = 33, face = "bold", halign = 0.45, vjust = 0.5, lineheight = 1.5),
    plot.caption = element_markdown(size = 10, hjust = 0, margin = margin(l = 20), lineheight = 1.3),
    plot.tag.position = c(0.99, 0.19),
    plot.tag = element_text(hjust = 1, vjust = 1, size = 9, face = "bold.italic", color = "grey99"),
    plot.margin = margin(t = 15, b = 15, l = 10, r = 10, unit = "pt")
  ) 
p3 +
  scale_fill_discrete(labels = function(x) stringr::str_wrap(x, width = 20)) +
  theme(legend.text=ggplot2::element_text(size = 16 * width_scale)) + 
  coord_cartesian(clip = "off")

ggsave(filename = "TEST.png",
       width = 10, height = 8, scale = 1.25, dpi = 150, bg = "white")
#

scg <- str_length(paste(unique(df$fips), collapse = "")) %/% 70 + 1
p3 +
  guides(fill = guide_legend(nrow = scg)) +
  # scale_fill_discrete(labels = function(x) stringr::str_wrap(x, width = 20)) +
  theme(legend.text=ggplot2::element_text(size = 17 - scg)) +
  coord_cartesian(clip = "off")
ggsave(filename = "TEST.png",
       width = 10, height = 8, scale = 1.25, dpi = 150, bg = "white")

