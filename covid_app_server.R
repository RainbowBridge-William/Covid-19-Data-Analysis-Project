library("knitr")
library("dplyr")
library("tidyverse")
library("ggplot2")
library("maps")
library("stringr")





app_server <- function(input, output) {
  #*******Vaccine**********
  output$vaccine_question <- renderText({
    question <- paste("Does the vaccination population affect the rate of cases in", input$vaccine_state_select ,"? If so, how?")
    return(question)
    })
}
