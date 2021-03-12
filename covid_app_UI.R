library("knitr")
library("dplyr")
library("tidyverse")
library("ggplot2")
library("maps")
library("stringr")
library("shiny")
library("shinythemes")

source("section_3.R")

domain_description <-
  "COVID-19, which is short for coronavirus disease
   of 2019, is the illness caused by the SARS-CoV-2
   virus first identified in Wuhan, China in December
   of 2019. Since then, the virus has rapidly spread
   across the world, leading the World Health Organization
   to declare a global pandemic. Millions of Americans
   have been infected by the virus, and hundreds of
   thousands have died due to the disease with those
   numbers only continuing to grow each day. A global
   race to develop a vaccine in record-breaking time
   ensued, with over 100 different candidates being
   tested across the globe. Despite multiple vaccines
   receiving emergency authorizations from multiple
   different nations, the situation is worsening daily
   as new mutant strains are being identified such as
   those identified in the United Kingdom. In the United
   States, public health officials are struggling to convince
   the populous that the vaccines are safe and effective,
   leading to widespread anti-vaccine protests seeking to
   slow the vaccination efforts, which only lends itself to
   give the virus more time to develop a mutation to defeat
   the current vaccine formulations."

domain_description_p2 <-
  "Thus, analyzing data related to COVID-19 is worthwhile
  since it will help people understand the overall situation
  and severity of the pandemic and arouse their interest in
  adopting protective measures like mask-wearing, social-distancing,
  and vaccination. In addition, analyzing this data may expose
  differences in the ability of different regulations between
  states to contain the virus, which may prove beneficial in
  helping state governments are only utilizing restrictions
  that truly work to contain this pathogen."

hospital_data_description <-
  "This dataset published by the United States Department of Homeland
  Security and compiled from sources from the United States Department
  of Health & Human Services and Centers for Disease Control and Prevention
  provides a list of all hospitals in the United States and their associated
  trauma level. It will help in identifying how many hospitals and of
  what type exist in each state, whichis needed to understand if hospital
  count and type has an effect on death ratesfrom COVID-19."

vaccination_data_description <-
  "The data set includes the overall US COVID vaccine distribution and
   administration. It was collected by each state government and gathered
   by the Centers for Disease Control and Prevention and these data will
   support answering the relationship between vaccination and change in the
   rate of cases & deaths of each state."

cases_and_deaths_data_description <-
  "This data set includes the COVID-19 cases and deaths of each state over
   time. It was collected by each state and gathered by the Centers for Disease
   Control and Prevention and it will support answering the relationship between
   vaccination and change in the rate of cases & deaths of each state."

mask_survey_data_description <-
  "This data set contains estimates of mask-usage from 250,000 survey responses
   for each county in the US. It was assembled by The New York Times and Dynata.
   Each participant was asked \"How often do you wear a mask in public when you
   expect to be within six feet of another person?\" and given the choices of never,
   rarely, sometimes, frequently, or always. This will be used to answer the question
   about mask-wearing affecting the number of cases."

stay_home_order_data_description <-
  "This data set gives information on each U.S. state's first stay at home home order
   at the beginning of the COVID-19 pandemic. Data includes the date each state's order
   went into effect, and the infection rates on dates before and after the order. This
   data will be used to answer the question regarding the effectiveness of stay at home
   orders in slowing the spread of COVID-19 in the U.S."

introduction_panel <- tabPanel(("Background"),
                               h3(strong("Group A6")),
                               hr(),
                               h4(" - Patrick Abboud"),
                               h4(" - Jeffrey Kaufman"),
                               h4(" - Kyler Smith"),
                               h4(" - William Wang"),
                               a("Exploratory Report", href = "https://info201b-wi21.github.io/project-group-a6/"),
                               h3(strong("Problem Domain")),
                               hr(),
                               p(domain_description),
                               p(domain_description_p2),
                               h3(strong("Datasets")),
                               hr(),
                               a("- Homeland Infrastructure Foundation-Level Data Hospitals:",
                                 href = "https://hifld-geoplatform.opendata.arcgis.com/datasets/hospitals"),
                               p(hospital_data_description),
                               a("- Covid-19 Vaccinations in the United States:",
                                 href = "https://covid.cdc.gov/covid-data-tracker/#vaccinations"),
                               p(vaccination_data_description),
                               a("- U.S. Covid-19 Cases and Deaths By State Over Time:",
                                 href = "https://data.cdc.gov/Case-Surveillance/United-States-COVID-19-Cases-and-Deaths-by-State-o/9mfq-cb36"),
                               p(cases_and_deaths_data_description),
                               a("- New York Times Mask Wearing Survey:",
                                 href = "https://github.com/nytimes/covid-19-data/tree/master/mask-use"),
                               p(mask_survey_data_description),
                               a("- Stay At Home Order Date and Infection Rate Data:",
                                 href = "https://www.ncbi.nlm.nih.gov/pmc/articles/PMC7246016/table/tbl0001/?report=objectonly"),
                               p(stay_home_order_data_description)
                               )

hospital_data_panel <- tabPanel("Hospital Data")

#****************** Vaccination Panel********************
vaccination_controls_panel <- sidebarPanel(
   #select box for selecting states
   selectInput("vaccine_state_select", label = h4("Select which state to visualize data for:"), 
               choices = list("Overall (Entire US)" = "the Entire US", states = states), 
               selected = "the Entire US"),
   h4("**Hover on the dots to see more information**")
)

vaccination_analysis_panel <- mainPanel(
   h2(textOutput("vaccine_question")),
   plotlyOutput("vaccination_plot"),
   br(),
   p(textOutput("vaccination_description"))
   )


vaccination_data_panel <- tabPanel("Vaccination Data",
                                   sidebarLayout(
                                      vaccination_controls_panel,
                                      vaccination_analysis_panel
                                   ))


#*********************************************************

mask_survey_answers <- c(
   "Never" = "NEVER",
   "Rarely" = "RARELY",
   "Sometimes" = "SOMETIMES",
   "Frequently" = "FREQUENTLY",
   "Always" = "ALWAYS"
)

mask_data_panel <- tabPanel(
   "Mask Use Data",
   h2("How is self-reported mask-wearing related to the number of cases for each county in the United States?"),
   p("Given a survey asking \"How often do you wear a mask in public when you
    expect to be within six feet of another person?\", with 5 different levels of
    responses, is there any correlation between counties that have more COVID
    cases and counties with people who answer in a particular way?"),
   p("The survey was done in 2020 from July 2 to July 14, and the number of
     COVID cases in each county was recorded on July 14, 2020."),
   sidebarLayout(
      sidebarPanel(
         selectInput("survey_answer", "Survey answer", mask_survey_answers),
         checkboxInput("use_log_scale", "Use log scale", FALSE)
      ),
      mainPanel(
         plotOutput("mask_use_plot"),
         br(),
         textOutput("mask_use_text"),
         br(),
         p("Counties with more people who report that they wear a mask in
           public, also have more COVID cases. This unexpected, because masks
           are supposed to slow the spread of COVID. Instead, this is probably the result of people wearing a mask", em("because"), "their county has a large number of cases.")
      )
   )
)


stay_home_order_time_slider <- sliderInput(inputId = "num_days_since_order",
                                              label = h5("Days Since Order: "),
                                              value = 30, 
                                              min = 0, 
                                              max = 90)

stay_home_order_filter_buttons <- radioButtons(inputId = "filter", 
                                                  label = h5("Time vs.: "),
                                                  choices = c("New Daily Cases",
                                                              "Daily Change In Number of New Cases",
                                                              "Cumulative Difference Between Initial and Current Daily Change"
                                                              )
                                                  )

stay_home_order_state_selector_choice_list <- as.list(append("Country Average", 
                                                               state_stay_at_home_order_data_df$State, 
                                                               0
                                                               )
                                                      )

stay_home_order_state_selector <- 
  selectizeInput(inputId = "state", 
                 label = h5("State:"),
                 choices = stay_home_order_state_selector_choice_list,
                 options = list(
                   highlight = FALSE,
                   minItems = 1,
                   maxItems = 10,
                   placeholder = NULL),
                 selected = "Country Average",
                 multiple = TRUE
                 )

stay_home_order_controls_panel <- sidebarPanel(h4(strong("Filters:")),
                                          stay_home_order_time_slider,
                                          stay_home_order_filter_buttons,
                                          stay_home_order_state_selector)

year_slider_control <- sliderInput(inputId = "days_after_order",
                                   label = h5("Days After Order: "),
                                   value = 30, 
                                   min = 1, 
                                   max = 90)

button_filter_control <- radioButtons(inputId = "filter", 
                                      label = "Filter",
                                      choices = c("Decrease In Cases After Order",
                                                  "Daily Cases After Order")
                                      )

menu_control <- selectizeInput(inputId = "state", 
                            label = h5("State:"),
                            list(choices = append("Country Average", 
                                                  state_stay_at_home_order_data_df$State, 
                                                  0)
                                 ),
                            options = list(
                              highlight = FALSE,
                              maxOptions = 3,
                              placeholder = "Search for state and/or country average"),
                            selected = "Country Average",
                            multiple = TRUE
                            )

home_order_controls_panel <- sidebarPanel(h4(strong("Filters:")),
                                          year_slider_control,
                                          button_filter_control,
                                          menu_control
                                          )

stay_home_order_analysis_panel <- mainPanel(h2("How Well Do Stay-At-Home Orders Work?"),
                                       hr(),
                                       p(plotOutput("stay_home_order_analysis_visual"))
                                       #p(textOutput("stay_home_order_analysis_plot_description"))
                                       )

stay_home_order_data_panel <- tabPanel("Stay At Home Order Data",
                                          sidebarLayout(
                                            stay_home_order_controls_panel,
                                            stay_home_order_analysis_panel
                                            )
                                          )


app_UI <- fluidPage(theme = shinytheme("slate"), 
                    titlePanel("COVID-19 Data Analysis"), 
                    navbarPage(title = strong("Menu"), 
                               introduction_panel, 
                               hospital_data_panel, 
                               vaccination_data_panel, 
                               mask_data_panel, 
                               stay_home_order_data_panel)
                     )
