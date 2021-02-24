library("knitr")
library("dplyr")
library("tidyverse")
library("ggplot2")
library("maps")

# NYT Mask-Wearing Survey data set
mask_use_by_county_df <- read.csv("data/NYT Mask-Wearing Survey/mask-use-by-county.csv")

mask_use_by_county_sum_df <- mask_use_by_county_df %>% 
  mutate(sum = NEVER + RARELY + SOMETIMES + FREQUENTLY + ALWAYS)

mask_use_by_county_summary_df <- mask_use_by_county_sum_df %>%
  select(-COUNTYFP) %>%
  summary()

mask_use_violin_plot_data_df <- mask_use_by_county_df %>% pivot_longer(c(NEVER, RARELY, SOMETIMES, FREQUENTLY, ALWAYS))

# Make `name` a factor to order it manually
mask_use_violin_plot_data_df$name <- factor(mask_use_violin_plot_data_df$name, levels = c("NEVER", "RARELY", "SOMETIMES", "FREQUENTLY", "ALWAYS"))

mask_use_violin_plot <- ggplot(data = mask_use_violin_plot_data_df, mapping = aes(x = name, y = value)) +
  geom_violin() +
  labs(title = "Distribution of Reported Mask-Wearing for Each County", x = "Response", y = "Estimated percent of responses") +
  scale_y_continuous(labels = scales::percent)

mask_use_map_data_df <- map_data("county") %>% 
  mutate(polyname = paste(region, subregion, sep = ",")) %>%
  left_join(county.fips, by = "polyname") %>% 
  left_join(mask_use_by_county_df, by = c("fips" = "COUNTYFP"))

mask_use_always_map <- ggplot(data = mask_use_map_data_df, mapping = aes(fill = ALWAYS)) +
  geom_polygon(mapping = aes(x = long, y = lat, group = group)) +
  scale_fill_distiller(name = "Percent of\n\"Always\"\nResponses", palette = "YlOrRd", labels = scales::percent) +
  coord_map() +
  theme_void() +
  labs(title = "\"Always\" Responses per County")

mask_use_never_map <- ggplot(data = mask_use_map_data_df, mapping = aes(fill = NEVER)) +
  geom_polygon(mapping = aes(x = long, y = lat, group = group)) +
  scale_fill_distiller(name = "Percent of\n\"Never\"\nResponses", palette = "YlOrRd", labels = scales::percent) +
  coord_map() +
  theme_void() +
  labs(title = "\"Never\" Responses per County")
  