source("section_2.R")

population_state_df <- read.csv("./data/US Population/population.csv") %>%
  select(State, Pop)

# ***** Does the number and types of hospitals affect the death rate in each state, and if so, how? *****
hospital_type_counts_by_state_df <- covid_related_hospitals_df %>%
  mutate(trauma_level = standardize_trauma_levels(TRAUMA)) %>%
  rename(State = STATE) %>%
  group_by(State) %>%
  summarize(
    non_trauma = length(which(is.na(trauma_level))),
    level_i = length(which(trauma_level == "I")),
    level_ii = length(which(trauma_level == "II")),
    level_iii = length(which(trauma_level == "III")),
    level_iv = length(which(trauma_level == "IV")),
    level_v = length(which(trauma_level == "V"))
  )

hospital_counts_by_state_summary_df <- hospital_type_counts_by_state_df %>%
  rename(
    "Level I" = level_i,
    "Level II" = level_ii,
    "Level III" = level_iii,
    "Level IV" = level_iv,
    "Level V" = level_v,
    "Non-trauma Hospitals" = non_trauma
  )

hospital_counts_by_state_df <- hospital_type_counts_by_state_df %>%
  gather(
    key = type,
    value = count,
    -State
  )

death_rate_hospitals_df <- most_recent_total_deaths_with_state_map_data_df %>%
  select(-c(lat, long, order)) %>%
  unique() %>%
  left_join(population_state_df %>% mutate(state_territory = tolower(State))) %>%
  mutate(death_rate = (total_deaths * 100000) / Pop) %>%
  left_join(hospital_counts_by_state_df, by = c("abb" = "State")) %>%
  filter(!is.na(type))

healthcare_facility_count_death_rate_plot <- ggplot(data = death_rate_hospitals_df) +
  geom_point(mapping = aes(x = count, y = death_rate)) +
  geom_smooth(mapping = aes(x = count, y = death_rate), method = "lm") +
  facet_wrap(
    ~type,
    scales = "free_x",
    labeller = labeller(
      type = c("level_i" = "Level I", "level_ii" = "Level II", "level_iii" = "Level III",
               "level_iv" = "Level IV", "level_v" = "Level V", "non_trauma" = "Non-trauma Hospitals")
    )
  ) +
  labs(
    title = "Healthcare Facility Count vs. Deaths per 100,000 from COVID-19",
    x = "Facility Count",
    y = "Deaths per 100,000"
  )

# ***** Does the percent of vaccination population in each state affect the rate of cases? If so, how? *****
new_case_df <- jhu_cases_time_series_df %>%
  group_by(Province_State) %>%
  mutate(new_case = total_cases - lag(total_cases)) %>%
  rename("State" = "Province_State") %>%
  select(State, date, new_case)

# join population df and summarize case rate
rate_case_df <- new_case_df %>%
  left_join(population_state_df) %>%
  mutate(case_rate = (new_case * 100000)/ Pop) %>%
  select(State, date, case_rate)

rate_case_df$date <- as.character.Date(rate_case_df$date)

# wrangling percent of vaccination
percent_vaccination_df <- vaccination_map_data_df %>%
  select(date, region,ratio_people_vaccinated) %>%
  distinct() %>%
  na.omit()

## sort by ratio of doses administered
percent_vaccination_df <- percent_vaccination_df[order(-percent_vaccination_df$ratio_people_vaccinated),] %>%
  rename(State = region)
percent_vaccination_df[percent_vaccination_df=="New York State"] <- "New York"

vaccine_vs_rate_case <- left_join(percent_vaccination_df, rate_case_df, by = c("date", "State")) %>%
  na.omit()

# list of state for selectbox
states <- vaccine_vs_rate_case$State %>% 
  unique() %>% 
  sort()

# plot the graph
vaccine_vs_rate_case_scatter_plot <- ggplot(data = vaccine_vs_rate_case, mapping =
                                              aes(x = ratio_people_vaccinated,
                                                  y = case_rate)) +
  geom_point(size = 1) +
  geom_smooth(mapping = aes(x = ratio_people_vaccinated, y = case_rate), method = "lm", formula = y ~ x) +
  scale_x_continuous(labels = scales::percent) +
  scale_y_continuous() +
  labs(title = "The Daily Rate of Cases per 100k vs Percent of Vaccination Population\n in each state Over Time",
       x = "Percent of Vaccination Population", y = "Daily Cases Rate per 100k Population")


# ***** How is self-reported mask-wearing related to the number of cases for each county in the United States? *****
cases_july_14_df <- jhu_cases_time_series_raw_df %>%
  mutate(cases = X7.14.20) %>%
  select(FIPS, cases)

mask_use_vs_cases_df <- mask_use_by_county_df %>%
  left_join(cases_july_14_df, by = c("COUNTYFP" = "FIPS"))

mask_wearing_results_df <- data.frame(
  always = c(
    mask_use_vs_cases_df %>%
      top_frac(0.5, ALWAYS) %>%
      summarize(mean = mean(cases)) %>%
      pull(mean),
    mask_use_vs_cases_df %>%
      top_frac(0.5, -ALWAYS) %>%
      summarize(mean = mean(cases)) %>%
      pull(mean)
  ),
  never = c(
    mask_use_vs_cases_df %>%
      top_frac(0.5, NEVER) %>%
      summarize(mean = mean(cases)) %>%
      pull(mean),
    mask_use_vs_cases_df %>%
      top_frac(0.5, -NEVER) %>%
      summarize(mean = mean(cases)) %>%
      pull(mean)
  ),
  row.names = c(
    "Top 50%",
    "Bottom 50%"
  )
)

always_plot <- ggplot(data = mask_use_vs_cases_df, mapping = aes(x = ALWAYS, y = cases)) +
  geom_point(size = 0.8) +
  scale_x_continuous(labels = scales::percent) +
  labs(title = "Mask-wearing vs COVID cases on July 14, 2020", x = "Percent of people in a county who say they \"Always\" wear a mask", y = "COVID cases")

always_plot_log_scale <- always_plot + scale_y_log10(labels = scales::label_comma())
always_plot <- always_plot + scale_y_continuous(labels = scales::label_comma())

never_plot <- ggplot(data = mask_use_vs_cases_df, mapping = aes(x = NEVER, y = cases)) +
  geom_point(size = 0.8) +
  scale_x_continuous(labels = scales::percent) +
  labs(title = "Mask-wearing vs COVID cases on July 14, 2020", x = "Percent of people in a county who say they \"Never\" wear a mask", y = "COVID cases")

never_plot_log_scale <- never_plot + scale_y_log10(labels = scales::label_comma())
never_plot <- never_plot + scale_y_continuous(labels = scales::label_comma())

# *** Do state stay at home orders really work?

time_vs_infection_rate_change_df <- state_stay_at_home_order_data_df %>%
  mutate(ratio.between.days.after.and.before.order =
           (Number.of.days.after.order / Number.of.days.before.order),
         average.daily.infection.rate.decrease =
           (infection.rate.change) / (Number.of.days.after.order + Number.of.days.before.order)) %>%
  arrange(ratio.between.days.after.and.before.order)  %>%
  filter(ratio.between.days.after.and.before.order < 3) %>%
  select(State, Order.date, ratio.between.days.after.and.before.order, average.daily.infection.rate.decrease)


time_vs_infection_rate_change_plot <- ggplot(data = time_vs_infection_rate_change_df) +
  geom_point(mapping = aes(x = ratio.between.days.after.and.before.order,
                           y = average.daily.infection.rate.decrease)) +
  geom_smooth(mapping = aes(x = ratio.between.days.after.and.before.order,
                            y = average.daily.infection.rate.decrease),
                            method = 'lm' ,
                            formula = 'y ~ x') +
  labs(title = "Relationship Between Stay At Home Order Duration and Decrease In Infection Rate",
       x = "Ratio of Time In vs. Out of Lockdown Between Infection Rate Measurements \n(Per State)",
       y = "State Average Daily Infection Rate Decrease") +
  scale_x_continuous(labels = scales::label_number())

stay_home_order_analysis_df <- jhu_cases_time_series_df %>%
  select(-Province_State) %>%
  mutate(State = str_to_title(state_territory)) %>%
  left_join(state_stay_at_home_order_data_df, by = "State") %>%
  filter(!(is.na(Order.date))) %>%
  select(date, Order.date, State, total_cases) %>%
  filter(date >= Order.date - 2) %>%
  filter(date <= Order.date + 90) %>%
  arrange(State) %>%
  mutate(num_days_since_order = as.numeric(date - Order.date, units = "days"),
         num_new_cases = total_cases - lag(total_cases),
         change_in_num_new_cases = num_new_cases - lag(num_new_cases))

get_initial_new_cases <- stay_home_order_analysis_df %>%
  filter(num_days_since_order == 0) %>%
  select(State, num_new_cases) %>%
  rename(initial_new_cases = num_new_cases)

stay_home_order_analysis_df <- stay_home_order_analysis_df %>%
  left_join(get_initial_new_cases, by = "State") %>%
  mutate(initial_vs_current_new_case_difference = num_new_cases - initial_new_cases) %>%
  filter(num_days_since_order >= 0)

country_average_df <- stay_home_order_analysis_df %>%
  group_by(num_days_since_order) %>%
  summarise(across(-State, mean)) %>% 
  mutate(State = "Country Average")

stay_home_order_analysis_df <- stay_home_order_analysis_df %>%
  merge(country_average_df, all = TRUE) %>%
  arrange(State) %>% 
  rename("New Daily Cases" = num_new_cases, 
         "Daily Change In Number of New Cases" = change_in_num_new_cases,
         "Cumulative Difference Between Initial and Current Daily Change" = initial_vs_current_new_case_difference) %>%
  select(-c(Order.date, date, initial_new_cases, total_cases)) %>%
  gather(key = "Category", value = "Values", -c(State, num_days_since_order)) %>%
  spread(key = State, value = Values)
