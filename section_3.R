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
new_case_most_recent_df <- jhu_cases_time_series_df %>% 
  filter(date == max(date) | date == max(date) - 1) %>% 
  group_by(state_territory) %>% 
  summarize(new_case_most_recent = max(total_cases) - min(total_cases)) %>% 
  rename(State = state_territory)

# join population df and summarize case rate
new_case_most_recent_df$State <- str_to_title(new_case_most_recent_df$State) 
rate_case_df <- new_case_most_recent_df %>% 
  left_join(population_state_df) %>% 
  mutate(case_rate_most_recent = new_case_most_recent / Pop) %>% 
  select(State, case_rate_most_recent)

# wrangling percent ofvaccination 
percent_vaccination_df <- vaccination_map_data_df %>% 
  select(region,Ratio_Doses_Administered) %>% 
  distinct() %>% 
  na.omit()
## sort by ratio of doses administered
percent_vaccination_df <- percent_vaccination_df[order(-percent_vaccination_df$Ratio_Doses_Administered),] %>% 
  rename(State = region)
percent_vaccination_df[percent_vaccination_df=="New York State"] <- "New York"

vaccine_vs_rate_case <- left_join(percent_vaccination_df, rate_case_df)

# plot the graph
vaccine_vs_rate_case_scatter_plot <- ggplot(data = vaccine_vs_rate_case, mapping = 
                                              aes(x = Ratio_Doses_Administered, 
                                                  y = case_rate_most_recent)) + 
  geom_point(size = 1) +
  scale_x_continuous(labels = scales::percent) +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "The Rate of Cases vs Percent of Vaccination population in each state\n   on most recent date", 
       x = "Percent of Vaccination Population", y = "COVID Cases Rate")


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

