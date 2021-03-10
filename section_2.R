library("knitr")
library("dplyr")
library("tidyverse")
library("ggplot2")
library("maps")
library("stringr")

united_states_state_map_data <- map_data("state") %>%
  left_join(maps::state.fips, by = c("region" = "polyname"))
united_states_state_map_data$abb[united_states_state_map_data$region == "massachusetts"] <- "MA"
united_states_state_map_data$abb[united_states_state_map_data$region == "michigan"] <- "MI"
united_states_state_map_data$abb[united_states_state_map_data$region == "north carolina"] <- "NC"
united_states_state_map_data$abb[united_states_state_map_data$region == "new york"] <- "NY"
united_states_state_map_data$abb[united_states_state_map_data$region == "virginia"] <- "VA"
united_states_state_map_data$abb[united_states_state_map_data$region == "washington"] <- "WA"

# ***** JHU CSSE COVID-19 Data *****
jhu_cases_time_series_raw_df <- read.csv("./data/JHU CSSE COVID-19/time_series_covid19_confirmed_US.csv")
jhu_cases_time_series_sample_df <- jhu_cases_time_series_raw_df %>%
  select(1:12)
jhu_cases_time_series_df <- jhu_cases_time_series_raw_df %>%
  group_by(Province_State) %>%
  summarize(across(starts_with("X"), sum)) %>%
  gather(
    key = date,
    value = total_cases,
    starts_with("X")
  ) %>%
  mutate(
    state_territory = tolower(Province_State),
    date = as.Date(date, format = "X%m.%d.%y")
  ) %>%
  select(-Province_State)

most_recent_total_cases_with_state_map_data_df <- jhu_cases_time_series_df %>%
  filter(date == max(date)) %>%
  left_join(united_states_state_map_data, by = c("state_territory" = "region"))

total_cases_state_distribution_plot <- ggplot(data = most_recent_total_cases_with_state_map_data_df, mapping = aes(fill = total_cases)) +
  geom_polygon(
    mapping = aes(x = long, y = lat, group = group),
    color = "black",
    size = 0.1
  ) +
  coord_map() +
  scale_fill_distiller(
    palette = "YlOrRd",
    direction = "horizontal",
    labels = scales::label_comma()
  ) +
  theme_void() +
  labs(
    title = "Distribution of COVID-19 Cases by State",
    fill = "Cases"
  )

jhu_deaths_time_series_raw_df <- read.csv("./data/JHU CSSE COVID-19/time_series_covid19_deaths_US.csv")
jhu_deaths_time_series_sample_df <- jhu_deaths_time_series_raw_df %>%
  select(1:12)
jhu_deaths_time_series_df <- jhu_deaths_time_series_raw_df %>%
  group_by(Province_State) %>%
  summarize(across(starts_with("X"), sum)) %>%
  gather(
    key = date,
    value = total_deaths,
    starts_with("X")
  ) %>%
  mutate(
    state_territory = tolower(Province_State),
    date = as.Date(date, format = "X%m.%d.%y")
  ) %>%
  select(-Province_State)

most_recent_total_deaths_with_state_map_data_df <- jhu_deaths_time_series_df %>%
  filter(date == max(date)) %>%
  left_join(united_states_state_map_data, by = c("state_territory" = "region"))

total_deaths_state_distribution_plot <- ggplot(data = most_recent_total_deaths_with_state_map_data_df, mapping = aes(fill = total_deaths)) +
  geom_polygon(
    mapping = aes(x = long, y = lat, group = group),
    color = "black",
    size = 0.1
  ) +
  coord_map() +
  scale_fill_distiller(
    palette = "YlOrRd",
    direction = "horizontal",
    labels = scales::label_comma()
  ) +
  theme_void() +
  labs(
    title = "Distribution of COVID-19 Deaths by State",
    fill = "Deaths"
  )

total_cases_by_date_df <- jhu_cases_time_series_df %>%
  group_by(date) %>%
  summarize(total_cases = sum(total_cases))

total_deaths_by_date_df <- jhu_deaths_time_series_df %>%
  group_by(date) %>%
  summarize(total_deaths = sum(total_deaths))


jhu_cases_deaths_time_series_summary_df <- total_cases_by_date_df %>% 
  left_join(total_deaths_by_date_df, by = "date") %>% 
  summary() 

jhu_cases_deaths_time_series_summary_df <- total_cases_by_date_df %>%
  left_join(total_deaths_by_date_df, by = "date") %>%
  summary()


jhu_cases_recorded <- mean(
  total_cases_by_date_df %>%
    filter(date == max(date)) %>%
    pull(total_cases)
)

jhu_deaths_recorded <- mean(
  total_deaths_by_date_df %>%
    filter(date == max(date)) %>%
    pull(total_deaths)
)

jhu_date_range <- range(
  jhu_cases_time_series_df %>%
    pull(date)
)

total_cases_deaths_by_date_df <- total_cases_by_date_df %>%
  left_join(total_deaths_by_date_df, by = "date") %>%
  gather(
    key = count_type,
    value = count,
    -date
  )

total_cases_deaths_by_date_plot <- ggplot(data = total_cases_deaths_by_date_df) +
  geom_path(mapping = aes(x = date, y = count, color = count_type), alpha = 0.5, size = 1.5) +
  scale_color_brewer(palette = "Dark2", labels = c("total_cases" = "Cases", "total_deaths" = "Deaths")) +
  scale_y_continuous(labels = scales::label_comma()) +
  labs(
    title = "Distribution of COVID-19 Cases & Deaths Over Time",
    x = "Date",
    y = "Count",
    color = "Count Totals"
  )

# ***** HIFLD Hospitals Data Set *****
standardize_trauma_levels <- function(unstandard_levels) {
  # Removes pediatrics
  unstandard_levels[grepl("^[^,]+PEDIATRIC", unstandard_levels)] <- NA
  unstandard_levels[unstandard_levels == "RPTC"] <- NA

  # Removes non-ACS standard centers
  unstandard_levels[unstandard_levels == "PARC"] <- NA

  unstandard_levels[startsWith(unstandard_levels, "LEVEL V")] <- "V"
  unstandard_levels[startsWith(unstandard_levels, "LEVEL IV")] <- "IV"
  unstandard_levels[startsWith(unstandard_levels, "LEVEL III")] <- "III"
  unstandard_levels[startsWith(unstandard_levels, "LEVEL II")] <- "II"
  unstandard_levels[startsWith(unstandard_levels, "LEVEL I")] <- "I"
  unstandard_levels[unstandard_levels == "ATH"] <- "III"
  unstandard_levels[unstandard_levels == "CTH"] <- "IV"
  unstandard_levels[unstandard_levels == "RTC" | unstandard_levels == "RTH"] <- "II"
  unstandard_levels[unstandard_levels == "TRF" | unstandard_levels == "TRH"] <- "V"

  unstandard_levels
}

hifld_hospitals_df <- read.csv("./data/HIFLD Hospitals/Hospitals.csv", na.strings = c("NOT AVAILABLE"))
hifld_hospitals_df$BEDS <- na_if(hifld_hospitals_df$BEDS, -999)
hifld_hospitals_sample_df <- hifld_hospitals_df %>%
  select(c(NAME, STATE, TYPE, BEDS, TRAUMA))

covid_related_hospitals_df <- hifld_hospitals_df %>%
  filter(
    STATUS == "OPEN",
    TYPE %in% c("GENERAL ACUTE CARE", "CRITICAL ACCESS", "LONG TERM CARE", "MILITARY", "WOMEN")
  )

hifld_hospitals_summary_df <- covid_related_hospitals_df %>%
  select(BEDS) %>%
  summary()

covid_related_hospitals_contiguous_df <- covid_related_hospitals_df %>%
  filter(!STATE %in% c("AK", "AS", "GU", "HI", "MP", "PR", "PW", "VI"))

covid_related_hospitals_state_distribution_plot <- ggplot(data = united_states_state_map_data) +
  geom_polygon(
    mapping = aes(x = long, y = lat, group = group),
    color = "black",
    size = 0.1
  ) +
  geom_point(
    data = covid_related_hospitals_contiguous_df,
    mapping = aes(x = LONGITUDE, y = LATITUDE),
    color = "red",
    alpha = 0.5
  ) +
  coord_map() +
  theme_void() +
  theme(plot.caption = element_text(hjust = 0.5)) +
  labs(
    title = "Distribution of Hospitals in the United States",
    caption = "Each dot represents an open general acute care, critical access, long term care, military, children's, or women's hospital."
  )

trauma_centers_contiguous_df <- covid_related_hospitals_contiguous_df %>%
  mutate(trauma_level = standardize_trauma_levels(TRAUMA)) %>%
  filter(!is.na(trauma_level))

trauma_centers_state_distribution_plot <- ggplot(data = united_states_state_map_data) +
  geom_polygon(
    mapping = aes(x = long, y = lat, group = group),
    color = "black",
    size = 0.1
  ) +
  geom_point(
    data = trauma_centers_contiguous_df,
    mapping = aes(x = LONGITUDE, y = LATITUDE, color = trauma_level),
    alpha = 0.5
  ) +
  coord_map() +
  scale_color_brewer(palette = "Dark2") +
  theme_void() +
  labs(
    title = "Distribution of Trauma Centers by Level in the United States",
    color = "Level"
  )

trauma_centers_df <- covid_related_hospitals_df %>%
  mutate(trauma_level = standardize_trauma_levels(TRAUMA)) %>%
  filter(!is.na(trauma_level)) %>%
  filter(!is.na(BEDS))

trauma_centers_beds_distribution_plot <- ggplot(data = trauma_centers_df) +
  geom_boxplot(mapping = aes(x = BEDS, y = trauma_level)) +
  scale_x_continuous(labels = scales::label_comma()) +
  labs(
    title = "Distribution of Beds by Trauma Center Level",
    x = "Beds",
    y = "Level"
  )

# ***** NYT Mask-Wearing Survey data set *****
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


# ***** CDC Vaccination by State data set *****
vaccination_by_state_df <- read.csv("data/COVID-19 Vaccinations in the US/covid19_vaccinations_in_the_united_states.csv")

# replace "N/A" string to NA value and convert the column into numeric
vaccination_by_state_df[vaccination_by_state_df=="N/A"] <- NA
vaccination_by_state_df[, c(2:5)] <- sapply(vaccination_by_state_df[, c(2:5)], as.integer)
vaccination_by_state_sample_df <- vaccination_by_state_df
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

# Stay at home orders for each state
state_stay_at_home_order_raw_data_df <- read.csv("data/State Stay At Home Orders/state_stay_at_home_order_data.csv", 
                                                 stringsAsFactors = FALSE)
state_stay_at_home_order_data_df <- state_stay_at_home_order_raw_data_df
stay_at_home_order_data_sample_df <- state_stay_at_home_order_raw_data_df[1:10, ] %>%
  select(State, 
         Order.date, 
         Infection.rate.and.confidence.interval..before.order.,
         Infection.rate.and.confidence.interval..after.order.)

state_stay_at_home_order_data_df <- state_stay_at_home_order_data_df %>%
  mutate(infection.rate.before.order = 
           as.numeric(sub("\\ .*", "", 
                         Infection.rate.and.confidence.interval..before.order.)),
         infection.rate.after.order = 
           as.numeric(sub("\\ .*", "", 
                         Infection.rate.and.confidence.interval..after.order.)),
         infection.rate.change = 
           infection.rate.before.order - infection.rate.after.order,
         Order.date = 
           as.Date(Order.date, format = "%m/%d/%y")) %>%
  select(State, 
         Order.date, 
         Number.of.days.before.order,
         Number.of.days.after.order,
         infection.rate.before.order,
         infection.rate.after.order,
         infection.rate.change) 

state_stay_at_home_order_summary_df <- state_stay_at_home_order_data_df %>%
  select(-State) %>%
  summary()

date_range <- 
  range(state_stay_at_home_order_data_df$Order.date)
average.infection.rate.before.order <- 
  mean(state_stay_at_home_order_data_df$infection.rate.before.order)
average.infection.rate.after.order <- 
  mean(state_stay_at_home_order_data_df$infection.rate.after.order)
average.infection.rate.change <- 
  mean(state_stay_at_home_order_data_df$infection.rate.change)
min.infection.rate.change <- 
  min(state_stay_at_home_order_data_df$infection.rate.change)
max.infection.range <-
  max(state_stay_at_home_order_data_df$infection.rate.change)
num_orders <- 
  length(state_stay_at_home_order_data_df$State)

stay_at_home_order_map_df <- map_data("state")
stay_at_home_order_map_df$region <- str_to_title(stay_at_home_order_map_df$region)
stay_at_home_order_map_df <- stay_at_home_order_map_df %>% 
  left_join(state_stay_at_home_order_data_df, by = c("region" = "State")) %>% 
  rename("Before Order" = infection.rate.before.order,
         "After Order" = infection.rate.after.order) %>%
  pivot_longer(c("Before Order", "After Order"), 
                 "When_Recorded") %>%
  mutate(When_Recorded = factor(When_Recorded, levels = c("Before Order", "After Order")))
            
map_infection_rate_decrease_per_state_plot <- ggplot(data = stay_at_home_order_map_df, 
                                                     mapping = aes(fill = value)) +
  geom_polygon(mapping = aes(x = long, y = lat, group = group)) +
  scale_fill_distiller(name = "Infection Rate \n(Grey = No Data)", 
                       palette = "RdYlGn",
                       na.value = "grey",
                       direction = -1) +
  coord_map() +
  theme_void() + 
  facet_wrap(~When_Recorded) +
  labs(title = "Infection Rate Before and After Stay At Home Order")

top_5_infection_rate_decrease_states_df <- state_stay_at_home_order_data_df %>%
  select(State, infection.rate.before.order, 
         infection.rate.after.order, 
         infection.rate.change) %>%
  arrange(infection.rate.change) %>%
  top_n(5) %>%
  rename("Before Order" = "infection.rate.before.order", 
         "After Order" = "infection.rate.after.order")

top_5_state_list <- c(top_5_infection_rate_decrease_states_df$State)

top_5_infection_rate_decrease_states_df <- top_5_infection_rate_decrease_states_df %>%
  pivot_longer("Before Order":"After Order", 
               "when_recorded") 

top_5_infection_rate_decrease_states_df <- top_5_infection_rate_decrease_states_df %>%
  mutate(State = factor(State, levels = top_5_state_list), 
         when_recorded = factor(when_recorded, levels = c("Before Order", 
                                                          "Difference",
                                                          "After Order")))

top_5_infection_rate_decrease_states_plot <- ggplot(data = top_5_infection_rate_decrease_states_df) + 
  geom_col(mapping = aes(x = State, 
                         y = value,
                         fill = when_recorded), 
           position = "dodge") + 
  labs(title = "Top 5 State Infection Rate Decreases After Stay At Home Order", 
       y = "Infection Rate") +
  theme(legend.title = element_blank())

infection_rate_decrease_distribution_df <- state_stay_at_home_order_data_df %>%
  select(infection.rate.before.order, infection.rate.after.order, infection.rate.change) %>%
  rename("Before Order" = infection.rate.before.order, 
         "After Order" = infection.rate.after.order,
         "Difference"  = infection.rate.change) %>%
  pivot_longer(c("Before Order", "After Order", "Difference"), "Infection_Rate") %>%
  mutate(Infection_Rate = factor(Infection_Rate, levels = c("Before Order", 
                                                            "After Order", 
                                                            "Difference")))
  
infection_rate_distribution_plot <- 
  ggplot(data = infection_rate_decrease_distribution_df) + 
  geom_boxplot(mapping = aes(x = value, y = Infection_Rate)) + 
  labs(title = "Distribution of Pre and Post Order Infection Rates and Difference", 
       y = "Rate Source", 
       x = "Infection Rate") 

infection.rate.before.order.outliers <- 
  c(arrange(state_stay_at_home_order_data_df, 
            infection.rate.before.order))[["infection.rate.before.order"]][1:3]
infection.rate.before.order.outlier.states <- 
  c(state_stay_at_home_order_data_df[["State"]][1:3])
infection.rate.change.outliers <- 
  c(arrange(state_stay_at_home_order_data_df, infection.rate.change))[["infection.rate.change"]][1]
infection.rate.change.outlier.states <- 
  c(arrange(state_stay_at_home_order_data_df, infection.rate.change))[["State"]][1]
           
  




           

  

