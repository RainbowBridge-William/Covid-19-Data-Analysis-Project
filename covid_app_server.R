library("knitr")
library("dplyr")
library("tidyverse")
library("ggplot2")
library("maps")
library("stringr")



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
