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



# CDC Vaccination by State data set
vaccination_by_state_df <- read.csv("data/COVID-19 Vaccinations in the US/covid19_vaccinations_in_the_united_states.csv")

# replace "N/A" string to NA value and convert the column into numeric
vaccination_by_state_df[vaccination_by_state_df=="N/A"] <- NA
vaccination_by_state_df[, c(2:5)] <- sapply(vaccination_by_state_df[, c(2:5)], as.integer)

# make summary of data set
vaccination_by_state_summary_df <- vaccination_by_state_df %>%
  na.omit() %>% 
  select(-State) %>%
  summary()

# add a column of ratio of population that are given doses for each state
vaccination_by_state_df <- vaccination_by_state_df %>% 
  mutate(Ratio_Doses_Administered = Doses_Administered_per_100k /100000)

# calculate sum/mean/min/max
sum_vaccination <- sum(vaccination_by_state_df$Total_Doses_Administered)
mean_vaccination_per_100k <- as.integer(round(mean(vaccination_by_state_df$Doses_Administered_per_100k, na.rm = T)))
min_vaccination_per_100k <- min(vaccination_by_state_df$Doses_Administered_per_100k, na.rm = T)
max_vaccination_per_100k <- max(vaccination_by_state_df$Doses_Administered_per_100k, na.rm = T)
mean_ratio_per_100k <- mean(vaccination_by_state_df$Ratio_Doses_Administered, na.rm = T)

# create map data
vaccination_map_data_df <- map_data("state")
vaccination_map_data_df$region <- str_to_title(vaccination_map_data_df$region)
vaccination_map_data_df[vaccination_map_data_df=="New York"] <- "New York State"
vaccination_map_data_df <- left_join(vaccination_map_data_df, vaccination_by_state_df, by = c("region" = "State"))

# map for total vaccination
map_total_vaccination <- ggplot(data = vaccination_map_data_df, mapping = aes(fill = Total_Doses_Administered)) +
  geom_polygon(mapping = aes(x = long, y = lat, group = group)) +
  scale_fill_distiller(name = "Number of\nVaccination population", palette = "YlOrRd", labels = scales::comma_format(),direction = "horizontal") +
  coord_map() +
  theme_void() +
  labs(title = "Total Vaccination Population per State")

# map for vaccination percentage
map_percent_vaccination <- ggplot(data = vaccination_map_data_df, mapping = aes(fill = Ratio_Doses_Administered)) +
  geom_polygon(mapping = aes(x = long, y = lat, group = group)) +
  scale_fill_distiller(name = "Percentage of\nPopulation\nthat got Vaccine", palette = "YlOrRd", labels = scales::percent,direction = "horizontal") +
  coord_map() +
  theme_void() +
  labs(title = "Percentage of Vaccination Population per State")

# top 5 percent vaccination column graph
top_5_percent_vaccination_df <- vaccination_map_data_df %>% 
  select(region,Ratio_Doses_Administered) %>% 
  distinct() %>% 
  slice_max(Ratio_Doses_Administered, n = 5)

list_state <- rev(top_5_percent_vaccination_df$region)

top_5_percent_vaccination_plot <- top_5_percent_vaccination_df %>% 
  mutate(region=factor(region, levels = list_state)) %>% 
  ggplot() +
  geom_col(mapping = aes(x = region, 
                         y = Ratio_Doses_Administered),
           position = position_dodge2(reverse = T)) + 
  coord_flip() +
  scale_y_continuous(labels = scales::percent) +
  labs(title="Top 5 States with Highest Percentage of Vaccination",
       x ="State", y = "Percentage of Vaccination")
