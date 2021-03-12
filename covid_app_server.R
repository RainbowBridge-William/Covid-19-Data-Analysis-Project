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