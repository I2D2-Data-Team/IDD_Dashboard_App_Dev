# This code cleans the tooltip data and prepares for use by IDD
library(tidyverse)


# Save a Clean Version of Tooltip -----------------------------------------

# Read figure titles and tooltips
metadata.fig.titles <-
  readxl::read_xlsx("data/figure_titles.xlsx", sheet = "titles", .name_repair = janitor::make_clean_names, col_types = "text") %>%
  select(figure_id, dimension, measure, indicator, subsets, figure, title, tool_tip_text, numerator, num_source, denominator, den_source) %>%
  mutate(num_source = paste0("(", num_source, ")"),
         den_source = paste0("(", den_source, ")"))

# Read tooltip data from Jamy
tooltip <- 
  # read_csv("data/tooltip.csv", col_types = cols(.default = "c")) %>% 
  read_csv("C://Users/gio/Box/Iowa IDS/Projects/_IA Data Drive/Indicators/I2D2 Data/ACS INDICATORS/Indicators_GioUpload/tooltip.csv", col_types = cols(.default = "c")) %>% 
  janitor::remove_empty(which = "cols") %>%
  janitor::clean_names() %>%
  rename(subsets = subset_if_diff) 

# Check indicator that are not matching
metadata.fig.titles %>% distinct(figure_id, dimension, measure, indicator, subsets, figure) %>%
  full_join(tooltip) %>%
  filter(is.na(figure_id) | is.na(id)) %>%
  select(1:7) %>%
  count(dimension, measure, indicator, subsets)

# # Check values in my metadata
# metadata.fig.titles %>% filter(indicator == "First Time Mother")
# metadata.fig.titles %>% filter(indicator == "Immunizations")
# tooltip %>% filter(indicator == "Immunizations")
# metadata.fig.titles %>% filter(indicator == "Household Type")
# tooltip %>% filter(indicator == "Household Type")

# Clean and combine data
tooltip_new <-
  tooltip %>% 
  mutate(
    # fix dimension, measure, and indicator name for First Time Mother
    dimension = ifelse(indicator == "Birth to First Time Mother", "Demographic", dimension),
    measure = ifelse(indicator == "Birth to First Time Mother", "Household Characteristics", measure),
    indicator = ifelse(indicator == "Birth to First Time Mother", "First Time Mother", indicator), 
    # add subsets to Immunizations
    subsets = 
      case_when(
        indicator == "Immunizations" & str_detect(tool_tip_desc, "2-year") ~ "2-year-old Children",
        indicator == "Immunizations" & str_detect(tool_tip_desc, "childcare") ~ "Childcare",
        indicator == "Immunizations" & str_detect(tool_tip_desc, "school") ~ "School Age",
        TRUE ~ subsets)
    ) %>%
  full_join(metadata.fig.titles %>% distinct(figure_id, dimension, measure, indicator, subsets, figure)) %>%
  select(figure_id, dimension, measure, indicator, subsets, figure, title:notes) %>%
  mutate(num_source = ifelse(is.na(num_source), NA_character_, paste0("(", num_source, ")")),
         den_source = ifelse(is.na(den_source), NA_character_, paste0("(", den_source, ")")))

# save data
tooltip_new %>% 
  mutate(id = str_sub(figure_id, 1, 11)) %>%
  rename(tool_tip_text = tool_tip_desc) %>%
  select(id, everything()) %>%
  write_csv("../common/data/idd_tooltip.csv")
# read_csv("../common/data/idd_tooltip.csv")


# Save a Clean Version of Data Source -------------------------------------

# Copy data to local file
file.copy(from = "C://Users/gio/Box/Iowa IDS/Projects/_IA Data Drive/Indicators/I2D2 Data/ACS INDICATORS/Indicators_GioUpload/data_source.csv",
          to = "data/data_source.csv", overwrite = TRUE)

# Read data source file from Jamy
datasource <- 
  read_csv("data/data_source.csv", col_types = cols(.default = "c")) %>%
  janitor::remove_empty(which = c("rows")) %>%
  janitor::clean_names()

# Read my data source for the rest of the indicators
datasource_rest <- read_csv("data/data_source_rest.csv", col_types = cols(.default = "c")) %>%
  janitor::clean_names()

# Check
datasource %>%
  count(dimension, measure, indicator) %>%
  anti_join(metadata.fig.titles %>% distinct(dimension, measure, indicator))

metadata.fig.titles %>% 
  count(dimension, measure, indicator) %>%
  anti_join(datasource %>% distinct(dimension, measure, indicator))

# Clean and combine data
datasource_new <- 
  datasource %>%
  filter(indicator != "Maternal Education") %>%
  mutate(
    indicator = 
      case_when(
        indicator == "Arrests" ~ "Arrest",
        indicator == "Child Abuse Or Neglect" ~ "Child Abuse or Neglect", 
        indicator == "Domestic Violence Children Present" ~ "Domestic Violence", 
        str_detect(str_to_lower(data), "free or reduced-price") ~ "Free or Reduced Price Lunch (FRPL)",
        TRUE ~ indicator)
    ) %>%
  distinct(dimension, measure, indicator, source, data, min_year, max_year, date_obtained, link, notes) %>%
  bind_rows(datasource_rest %>% select(-id))

# save data
tooltip_new %>%
  mutate(id = str_remove(figure_id, ".{4}$")) %>%
  distinct(id, dimension, measure, indicator) %>%
  full_join(datasource_new) %>%
  write_csv("../common/data/idd_data_source.csv")



# Save Data Type ----------------------------------------------------------

# Read indicator type
metadata.fig.types <-
  readxl::read_xlsx("data/figure_titles.xlsx", sheet = "type", .name_repair = janitor::make_clean_names)

# save data
metadata.fig.types %>% 
  select(id = indicator_id, dimension, measure, indicator, data_type) %>%
  write_csv("../common/data/idd_data_type.csv")
