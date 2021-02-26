# Does the percent of vaccination population in each state affect the rate of cases? If so, how?
source("section_2.R")
population_state_df <- read.csv("./data/US Population/population.csv") %>% 
  select(State, Pop)
# create new case data frame
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


