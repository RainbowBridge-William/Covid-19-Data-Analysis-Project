library("knitr")
library("dplyr")
library("tidyverse")
library("ggplot2")
library("maps")
library("stringr")


source("covid_app_server.R")
source("covid_app_UI.R")

shinyApp(ui = app_UI, server = app_server)



