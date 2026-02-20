

plot_bar_view2 <- function(DATA, DATA_TYPE = "percent", LOCATIONS, 
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
    Y_MAX <- ceiling(my_max_value * 100 * 1.25) / 100
    # fix max values to 5% if all values are suppressed or absence
    if (Y_MAX == 0) Y_MAX <- 0.05
  } else {
    Y_MAX <- ceiling(my_max_value * 1.25/10) * 10 
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
                  size = 4, #vjust = -1, 
                  vjust = 0.5, hjust = -0.2, angle = 90,
                  position = position_dodge(width = my_sbg))
    } else {
      my_bar <- 
        my_bar +
        geom_text(aes(label = scales::comma(index, accuracy = 1)),
                  size = 4, #vjust = -1, 
                  vjust = 0.5, hjust = -0.2, angle = 90,
                  position = position_dodge(width = my_sbg))
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
