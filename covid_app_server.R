library("knitr")
library("dplyr")
library("tidyverse")
library("ggplot2")
library("maps")
library("stringr")

source("section_2.R")
source("section_3.R")

<<<<<<< HEAD
create_stay_home_order_visualization <- function(input, output) {
  output$stay_home_order_analysis_visual <- renderPlot({
    stay_home_order_plot_df <- stay_home_order_analysis_df %>%
      filter(num_days_since_order <= input$num_days_since_order) %>%
      filter(Category == input$filter) %>%
      select(num_days_since_order,
             Category,
             input$state) %>%
      pivot_longer(!c(Category, num_days_since_order), 
                   names_to = "State", 
                   values_to = "Data")
    
    stay_home_order_analysis_visualization <- ggplot(stay_home_order_plot_df) +
      geom_line(mapping = aes(x = num_days_since_order,
                              y = Data,
                              color = State)) + 
      labs(title = "",
           x = "Number Of Days After Stay Home Order Start",
           y = input$filter)
    
    stay_home_order_analysis_visualization                            
  })
}

app_server <- function(input, output) {
  create_stay_home_order_visualization(input, output)
  # create_stay_home_order_visualization_description(input, output)
}
=======

source("section_3.R")

app_server <- function(input, output) {

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
  
  output$mask_use_plot <- renderPlot({
    x_label <- paste0("Percent of people in a county who say they \"", names(mask_survey_answers)[mask_survey_answers == input$survey_answer], "\" wear a mask")
    
    plot <- ggplot(data = mask_use_vs_cases_df, mapping = aes_string(x = input$survey_answer, y = "cases")) +
      geom_point(size = 0.8) +
      scale_x_continuous(labels = scales::percent) +
      labs(title = "Mask-wearing vs COVID cases on July 14, 2020", x = x_label, y = "COVID cases")
    
    if (input$use_log_scale) {
      plot <- plot + scale_y_log10(labels = scales::label_comma())
    } else {
      plot <- plot + scale_y_continuous(labels = scales::label_comma())
    }
    
    return(plot)
  })
  
}
>>>>>>> 1ef5d6625af8e272155afdfdffa5677ce212a4dd
