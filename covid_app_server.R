library("knitr")
library("dplyr")
library("tidyverse")
library("ggplot2")
library("maps")
library("stringr")
library("plotly")


source("section_3.R")

app_server <- function(input, output) {
  
  
  
  #*******Vaccine**********
  output$vaccine_question <- renderText({
    question <- paste("Does the vaccination population affect the rate of cases? If so, how?")
    return(question)
    })
  

  
  output$vaccination_plot <- renderPlotly({
    # filter the data frame based on input
    filtered_vaccine_vs_rate_case <- vaccine_vs_rate_case
    if (input$vaccine_state_select != "the Entire US") {
      filtered_vaccine_vs_rate_case <- filter(filtered_vaccine_vs_rate_case,
                                              State == input$vaccine_state_select)
      }
    vaccine_plot_title <- paste("The Daily Rate of Cases per 100k vs Percent of Vaccination Population in",
                                input$vaccine_state_select)

    
    # plot the graph
    filtered_vaccine_vs_rate_case_scatter_plot <- ggplot(data = filtered_vaccine_vs_rate_case, mapping =
                                                  aes(x = ratio_people_vaccinated,
                                                      y = case_rate,
                                                      )) +
      geom_point(size = 1, aes(text = paste0('Percent of Vaccination Population:',
                                        paste(ratio_people_vaccinated*100,"%"),
                                        '<br>Cases Rate per 100k:',
                                        round(case_rate)
      ))) +
      geom_smooth(mapping = aes(x = ratio_people_vaccinated, y = case_rate), method = "lm", formula = y ~ x) +
      scale_x_continuous(labels = scales::percent) +
      scale_y_continuous() +
      labs(title = vaccine_plot_title,
           x = "Percent of Vaccination Population", y = "Daily Cases Rate per 100k Population")
    
    ggplotly(filtered_vaccine_vs_rate_case_scatter_plot, tooltip = "text")
    

  })
  
  
  output$vaccination_description <- renderText({
    
    filtered_vaccine_vs_rate_case <- vaccine_vs_rate_case
    if (input$vaccine_state_select != "the Entire US") {
      filtered_vaccine_vs_rate_case <- filter(filtered_vaccine_vs_rate_case,
                                              State == input$vaccine_state_select)
    }
    
    # print the slope of smooth line
    round(coef(lm(ratio_people_vaccinated * 100 ~ case_rate, data = filtered_vaccine_vs_rate_case))[2],2)
  })
  
  #*********Hospital************
  output$hospitalsVisualization <- renderPlotly({

    filtered_death_rate_hospitals_df <- death_rate_hospitals_df %>%
      filter(type == input$hospitalsTypeSelectize) %>%
      filter(case_when(
        is.null(input$hospitalsStateSelectize) ~ TRUE,
        TRUE ~ abb %in% input$hospitalsStateSelectize
      ))

    filtered_healthcare_facility_count_death_rate_plot <- ggplotly(data = filtered_death_rate_hospitals_df) +
      geom_point(mapping = aes(x = count, y = death_rate)) +
      geom_smooth(mapping = aes(x = count, y = death_rate), method = "lm") +
      labs(
        x = "Facility Count",
        y = "Deaths per 100,000"
      )

    return(filtered_healthcare_facility_count_death_rate_plot)
  })
}