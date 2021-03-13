library("knitr")
library("dplyr")
library("tidyverse")
library("ggplot2")
library("maps")
library("stringr")

source("section_2.R")
source("section_3.R")

create_stay_home_order_visualization <- function(input, output) {
  output$stay_home_order_analysis_visual <- renderPlot({
    stay_home_order_plot_df <- stay_home_order_analysis_df %>%
      select(State, 
             input$filter, 
             Date) %>%
      pivot_longer(input$filter, 
                   "Category") %>%
      filter(value > 0) %>%
      pivot_wider(id_cols = c(Date, Category), 
                  names_from = State, 
                  values_from = "value") %>%
      select(Date, Category, c(input$state)) %>%
      pivot_longer(c(input$state), 
                   "State") 
    
    stay_home_order_analysis_visualization <- ggplot(stay_home_order_plot_df) +
      geom_line(mapping = aes(x = Date,
                              y = value,
                              color = State)) + 
      labs(title = paste("Date vs.", input$filter),
           x = "Date",
           y = input$filter) + 
      scale_x_date(limits = c(as.Date("2020-03-19", "%Y-%m-%d"), input$max_date)) +
      if(input$filter == "Number of New Cases") {
        ylim(c(NA, 5000))
      } else if (input$filter == "Number of Contagious Cases") {
        ylim(c(NA, 100000))
      } else if (input$filter == "State Contagious Population Percentage") {
        ylim(c(NA, 1))
      } else {
        ylim(c(NA, 15))
      }
    
    stay_home_order_analysis_visualization                            
  })
}

app_server <- function(input, output) {
  create_stay_home_order_visualization(input, output)
  # create_stay_home_order_visualization_description(input, output)
}