data <- read_my_csv(dim_dir, "IA-county", "hse_lng_fig3")

my_locations <- sort(c(19057, 19023, 19143, 19113))
my_locations_names <- ia_county_droplist[ia_county_droplist %in% my_locations]

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

# plot the barchart
df %>%
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
  ) +
  coord_cartesian(clip = "off")
  
ggsave(filename = "TEST.png",
       width = 10, height = 8, scale = 1.25, dpi = 150, bg = "white")
#



