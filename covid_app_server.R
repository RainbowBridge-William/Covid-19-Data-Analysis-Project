library("knitr")
library("dplyr")
library("tidyverse")
library("ggplot2")
library("maps")
library("stringr")



source("section_3.R")

app_server <- function(input, output) {
<<<<<<< HEAD
  #*******Vaccine**********
  output$vaccine_question <- renderText({
    question <- paste("Does the vaccination population affect the rate of cases in", input$vaccine_state_select ,"? If so, how?")
    return(question)
    })
=======

  output$hospitalsVisualization <- renderPlotly({

    filtered_death_rate_hospitals_df <- death_rate_hospitals_df %>%
      filter(type == input$hospitalsTypeSelectize) %>%
      filter(case_when(
        is.null(input$hospitalsStateSelectize) ~ TRUE,
        TRUE ~ abb %in% input$hospitalsStateSelectize
      ))

    filtered_healthcare_facility_count_death_rate_plot <- ggplot(data = filtered_death_rate_hospitals_df) +
      geom_point(mapping = aes(x = count, y = death_rate)) +
      geom_smooth(mapping = aes(x = count, y = death_rate), method = "lm") +
      labs(
        x = "Facility Count",
        y = "Deaths per 100,000"
      )

    ggplotly(filtered_healthcare_facility_count_death_rate_plot)
  })

>>>>>>> 4e33dd3b37c6f71b46dad1ec8030c66f5a5af651
}
