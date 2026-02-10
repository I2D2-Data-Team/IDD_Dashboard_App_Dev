# Read indicator type
metadata.fig.types <-
  readxl::read_xlsx("data/figure_titles.xlsx", sheet = "type", .name_repair = janitor::make_clean_names)

# Read figure titles and tooltips
metadata.fig.titles <-
  readxl::read_xlsx("data/figure_titles.xlsx", sheet = "titles", .name_repair = janitor::make_clean_names, col_types = "text") %>%
  select(figure_id, dimension, measure, indicator, subsets, figure, title, tool_tip_text, numerator, num_source, denominator, den_source) %>%
  mutate(num_source = paste0("(", num_source, ")"),
         den_source = paste0("(", den_source, ")"))


tooltip <- read_csv("../common/data/tooltip.csv", col_types = cols(.default = "c")) %>% 
  janitor::remove_empty(which = "cols") %>%
  rename(subsets = subset_if_diff) 

metadata.fig.titles %>% distinct(figure_id, dimension, measure, indicator, subsets, figure) %>%
  full_join(tooltip) %>%
  # add_count(figure_id) %>%
  filter(is.na(figure_id)) %>%
  select(1:7) %>%
  count(dimension, measure, indicator, subsets)

a %>% select(-subsets) %>% left_join(metadata.fig.titles)

metadata.fig.titles %>% filter(indicator == "First Time Mother")
