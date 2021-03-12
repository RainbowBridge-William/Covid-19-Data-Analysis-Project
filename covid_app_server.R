library("knitr")
library("dplyr")
library("tidyverse")
library("ggplot2")
library("maps")
library("stringr")
library("plotly")


source("section_3.R")


app_server <- function(input, output) {
  
  #********stay home order*************
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
    
    # get the slope of smooth line
    slope <- round(coef(lm(ratio_people_vaccinated * 100 ~ case_rate, data = filtered_vaccine_vs_rate_case))[2],2)

    paste("The following scatter plot shows the correlation of the daily rate of cases per 100k and percent of vaccined population in ",
          input$vaccine_state_select," after the distribution of vaccine across the US. Each dot representes a state at a specific date and 
          we could observe the general relationship from the blue linear regression fit of the scatter points.","\n", "\n","In current location(",
          input$vaccine_state_select,"), the slope of regression line is near ",slope, ", which means a weak negative correlation. Overall, it 
          clearly shows that the higher percentage of vaccined population, the lower rate of cases increase.",sep = "")
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

  #************mask use*****************
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
  
  output$mask_use_text <- renderText({
    average_percent <- mean(mask_use_vs_cases_df[[input$survey_answer]]) * 100
    survey_answer <- names(mask_survey_answers)[mask_survey_answers == input$survey_answer]
    
    top_50_percent_mean <- mask_use_vs_cases_df %>%
      top_frac(0.5, .data[[input$survey_answer]]) %>%
      summarize(mean = mean(cases)) %>%
      pull(mean)
    bottom_50_percent_mean <- mask_use_vs_cases_df %>%
      top_frac(-0.5, .data[[input$survey_answer]]) %>%
      summarize(mean = mean(cases)) %>%
      pull(mean)
    
    return(paste0("For each county, an average of ", formatC(average_percent, 2, format = "f"),
                  "% of people reported that they \"", survey_answer, "\" wear a mask.
                  Sorted by the percent of people who reported \"", survey_answer,
                  "\" wearing a mask, the top 50% of counties had an average of ",
                  round(top_50_percent_mean), " COVID cases, and the bottom 50% had an average of ",
                  round(bottom_50_percent_mean), " cases."))
  })
  
}

