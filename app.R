library(shiny)
library(shinycssloaders)
library(shinyjs)
library(leaflet)
library(htmltools)
library(sf)
library(ggplot2)
library(plotly)
library(ncdf4)
library(reshape)
library(sortable)
library(slickR)
library(tinytex)
library(lubridate)
library(shinyWidgets)
library(shinydashboard)
library(tidyverse)
library(matrixStats)


# Options for Spinner
options(spinner.color="#0275D8", spinner.color.background="#ffffff", spinner.size=2)

# colors for plots
cols <- RColorBrewer::brewer.pal(8, "Dark2")
l.cols <- RColorBrewer::brewer.pal(8, "Set2")[-c(1, 2)]
pair.l.cols <- RColorBrewer::brewer.pal(8, "Paired")

# colors for theme
obj_bg <- "#D4ECE1"
ques_bg <- "#B8E0CD"

# Load app input
module_text <- read.csv("data/module_text.csv", row.names = 1, header = FALSE)
EF_links <- read.csv("data/eco_forecast_examples.csv")
forecast_dates <- read.csv("data/forecast_dates.csv")
stakeholder_info <- read.csv("data/stakeholders.csv")

mock_data <- read.csv('data/wq_forecasts/microcystin_mock_data.csv')
mock_data$date_forecast_made <- as.Date(mock_data$date_forecast_made)
mock_data$date_of_forecast <- as.Date(mock_data$date_of_forecast)

# Define vectors
forecast_descriptions <- c("", 'There is no chance of water quality degradation on June 6',
  'There is a chance that the water quality will be dangerous to swimmers (>35 ug/L) on June 6',
  'The algal concentration will be below the water quality treatment threshold (25 ug/L)',
  'The algal concentration will be below the dangerous swimming threshold (35 ug/L)')
decision_options <- c('', 'low stakes', 'general assessor', 'decision theorist')

proact_answers <- c('There is an increased risk of algal blooms due to the time of year and you must make sure that swimmers in the reservoir
   are safe, the city benefits economically from the event, and that residents continue to receive safe drinking water in their taps',
  'Provide safe drinking water quality for the city',
  'Ensure safe water quality for swimmers',
  'Cancel the event',
  'Continue with the event',
  'Treat the water with a chemical to decrease algae concentrations',
  'Loss of money due to canceling the event',
  'Loss of aquatic life (e.g., aquatic plants, insects, fish) due to chemical treatment',
  'Decreased water quality due to lack of treatment',
  'Loss of money due to cost of water treatment, but increased economic activity to the city from the swimming competition',
  'Death of aquatic organisms, but safe water quality for swimmers and city residents',
  'Occurrence of human health risks due to swimming in unsafe water, but money is saved and aquatic organisms are not affected due to
  avoiding chemical treatment')

# define the date of the swimming event (Activity B)
date_of_event <- as.Date('2021-06-06')

#user interface
ui <- tagList(
  navbarPage(title = "Module 8",
             position = "fixed-top",
             
             #useShinydashboard(),
             
             # Tab1: Macrosystems Overview ----
             tabPanel(title = "Macrosystems Overview",
                      tags$style(type="text/css", "body {padding-top: 65px;}"),
                      img(src = "project-eddie-banner-2020_green.png", height = 100, 
                          width = 1544, top = 5),
                      useShinyjs(),
                      column(5,
                             h3("Project EDDIE"),
                             p(module_text["EDDIE", ]),
                             h3("Macrosystems Ecology"),
                             p(module_text["Macro", ]),
                             h3("Macrosystems EDDIE and Ecological Forecasting"),
                             p(module_text["macro_EF",]),
                             tags$ul(
                               tags$li("Module 5: Introduction to Ecological Forecasting"),
                               tags$li("Module 6: Forecast Uncertainty"),
                               tags$li("Module 7: Confronting Forecasts with Data"),
                               tags$li("Module 8: Using Ecological Forecasts to Guide Decision Making")
                             ),
                             
                      ),
                      column(7,h2("Ecological Forecasting as a Tool for Macrosystems Ecology"),
                             #HTML('<center><img src="TFC_v1.png"></center>'),  #ask TM diff btw HTML and img
                             img(src = "ecoforecast_v3.png",
                                 width = 800,
                                 height = 500)),
                      br(),
                      br())
             ,
             
             # Tab2: Module 8 Introduction ---- ## want to make this the default landing page, which shows up under 'Module 8: Using Ecological...'
             tabPanel(title = "Introduction",
                      tags$style(type="text/css", "body {padding-top: 65px;}"),
                      img(src = "project-eddie-banner-2020_green.png", height = 100, 
                          width = 1544, top = 5),
                      #* Intro text ====
                      h2("Module 8: Using Ecological Forecasts to Guide Decision Making"),
                      br(),
                      h2("Today's focal question:", align = 'center'),
                      h3("How can ecological forecasts and their visualizations aid in decision making?", align = 'center'),
                      p(module_text["to_address_question",]),
                      column(6, 
                             h3("Overview of Activities"),
                             tags$ul(
                               tags$li("Introduction to Ecological Forecasting - Pre-readings and PowerPoint in class"),
                               tags$li("Activity A - Explore an existing ecological forecast"),
                               tags$li("Activity B - Make decisions using a real ecological forecast"),
                               tags$li("Activity C - Create a customized visualization for a specific stakeholder")
                             ),
                             br(),
                             h3("Learning Objectives"),
                             h4("By the end of this module, you will be able to:"),
                             tags$ul(
                               tags$li(module_text["LO1",]),
                               tags$li(module_text["LO2",]),
                               tags$li(module_text["LO3",]),
                               tags$li(module_text["LO4",]),
                               tags$li(module_text["LO5",]),
                               tags$li(module_text["LO6",]),
                               
                             )),
                      column(6, 
                             h3("Ecological Forecasting"),
                             p(module_text["eco_forecast", ]),
                             p(module_text["theme_mod8", ])),
                      br()
                      ),
             # Tab3: Activity A ----
             tabPanel(title = "Activity A: Explore",
                      tags$style(type="text/css", "body {padding-top: 65px;}"),
                      img(src = "project-eddie-banner-2020_green.png", height = 100, 
                          width = 1544, top = 5),
                      h2("Activity A: Explore ecological forecast visualizations and decision-use"),
                      h4("Many of us use various types of forecasts in our daily life to make decisions (e.g., weather forecasts). However, we often take for granted the way in
                         which the forecast is presented to us. In this activity, you will examine several ecological forecasts and analyze the visualizations they provide
                         as decision-support tools for their users."),
                     br(),
                      tabsetPanel(selected = 'Objective 1',
                       tabPanel(title = 'List of Ecological Forecasts',
                                h3("List of Ecological Forecasts"),
                                tags$ul(
                                  tags$li(a(href = EF_links$webpage[1], EF_links$Forecast[1]), br(), p(EF_links$About[1]), tags$b(p(EF_links$hint[1])), img(src = EF_links$logo_file[1], height = '20%', width = '10%')),
                                  br(),
                                  tags$li(a(href = EF_links$webpage[2], EF_links$Forecast[2]), br(), p(EF_links$About[2]), tags$b(p(EF_links$hint[2])), img(src = EF_links$logo_file[2], height = '30%', width = '20%')),
                                  br(),
                                  tags$li(a(href = EF_links$webpage[3], EF_links$Forecast[3]), br(), p(EF_links$About[3]), tags$b(p(EF_links$hint[3])), img(src = EF_links$logo_file[3], height = '20%', width = '10%')),
                                  br(),
                                  tags$li(a(href = EF_links$webpage[4], EF_links$Forecast[4]), br(), p(EF_links$About[4]), tags$b(p(EF_links$hint[4])), img(src = EF_links$logo_file[4], height = '30%', width = '50%')),
                                  br(),
                                  tags$li(a(href = EF_links$webpage[5], EF_links$Forecast[5]), br(), p(EF_links$About[5]), tags$b(p(EF_links$hint[5])), img(src = EF_links$logo_file[5], height = '20%', width = '10%')),
                                  br(),
                                  tags$li(a(href = EF_links$webpage[6], EF_links$Forecast[6]), br(), p(EF_links$About[6]), tags$b(p(EF_links$hint[6])), img(src = EF_links$logo_file[6], height = '30%', width = '20%')),
                                  br(),
                                  tags$li(a(href = EF_links$webpage[7], EF_links$Forecast[7]), br(), p(EF_links$About[7]), tags$b(p(EF_links$hint[7])), img(src = EF_links$logo_file[7], height = '20%', width = '10%')), 
                                  br(),
                                  tags$li(a(href = EF_links$webpage[8], EF_links$Forecast[8]), br(), p(EF_links$About[8]), tags$b(p(EF_links$hint[8])), img(src = EF_links$logo_file[8], height = '20%', width = '10%')), 
                                  br(),
                                  tags$li(a(href = EF_links$webpage[9], EF_links$Forecast[9]), br(), p(EF_links$About[9]), tags$b(p(EF_links$hint[9])), img(src = EF_links$logo_file[9], height = '20%', width = '10%')) 
                                )),
                       tabPanel('Objective 1',
                                h4(tags$b("Objective 1: Explore how uncertainty is visualized in an ecological forecast")),
                                br(),
                                h4("Choose an ecological forecast from the 'List of Ecological Forecasts' panel. Spend a few minutes exploring their website to learn about 
                                   the ecological forecast. Select a forecast visualization file to download. (You can do this by right-clicking
                                   on the file on the webpage and selecting 'Save image as...'. Then upload this file into the app by selecting 'Browse' below.)"),
                                br(),
                                fluidRow(column(6, fileInput(inputId = 'forecast_file', label = 'Upload a file of a visualization from the forecasting system you have chosen', width = '75%')),
                                column(6, imageOutput('forecast_image'))),
                                h4('Using the image you have uploaded, answer the following questions'),
                                wellPanel(style = paste0("background: ", ques_bg),
                                  fluidRow(tags$ul(
                                  textInput(inputId = "q1", label = paste0('Q1. ', module_text["activityA_Q1",]),
                                            placeholder = "", width = "80%"),
                                  textInput(inputId = "q1", label = paste0('Q2. ', module_text["activityA_Q2",]),
                                            placeholder = "", width = "80%"),
                                  textInput(inputId = "q1", label = paste0('Q3. ', module_text["activityA_Q3",]),
                                            placeholder = "", width = "80%"),
                                  textInput(inputId = "q1", label = paste0('Q4. ', module_text["activityA_Q4",]),
                                            placeholder = "", width = "80%"),
                                  textInput(inputId = "q1", label = paste0('Q5. ', module_text["activityA_Q5",]),
                                            placeholder = "", width = "80%"))),
                                  fluidRow(tags$ul(
                                    column(4, textInput(inputId = "q6_A", label = paste0("Q6. ", module_text["activityA_Q6",]),
                                              placeholder = "", width = "80%"),
                                           textInput(inputId = "q6_B", label ="",
                                                     placeholder = "", width = "80%"),
                                           textInput(inputId = "q6_C", label = "",
                                                     placeholder = "", width = "80%")),
                                    column(4, selectInput(inputId = "q7_A", label = paste0("Q7. ", module_text["activityA_Q7",]),
                                              choices = decision_options, width = "80%"),
                                           selectInput(inputId = "q7_B", label = "",
                                                       width = "80%", choices = decision_options),
                                           selectInput(inputId = "q7_C", label = "",
                                                       width = "80%", choices = decision_options)),
                                    column(4, textInput(inputId = "q8_A", label = paste0("Q8. ", module_text["activityA_Q8",]),
                                                        placeholder = "", width = "80%"),
                                           textInput(inputId = "q8_B", label = "",
                                                     placeholder = "", width = "80%"),
                                           textInput(inputId = "q8_C", label = "",
                                                     placeholder = "", width = "80%"))
                                    
                                  ))),
                                  
                                
                       ),
                       tabPanel('Objective 2',
                                h4(tags$b("Objective 2: Compare forecast visualizations and answer the following questions.")),
                                br(),
                                h4("With another team, compare forecasting systems and visualizations. 
                                Discuss the following questions regarding the ecological forecasting systems you explored."),
                                h5("Upload your partner's forecast image to see the two displayed here. They can either email you their visualization file
                                   or you can navigate to their website, download the image, and upload here."),
                                fluidRow(column(4, fileInput(inputId = 'forecast_file_2', label = "Upload a file of the visualization from the forecasting system your partner has chosen", width = '75%')),
                                         column(4, imageOutput('forecast_image_second_time')),
                                         column(4,imageOutput('forecast_image_2'))),
                                h4('Using the image you have uploaded, answer the following questions'),
                                br(),
                                wellPanel(style = paste0("background: ", ques_bg),
                                  tags$ul(
                                  textInput(inputId = "q_obj2_1", label = paste0("Q9. ", module_text["activityA_obj2_Q9",]),
                                            placeholder = "", width = "80%"),
                                  textInput(inputId = "q_obj2_2", label = paste0("Q10. ",module_text["activityA_obj2_Q10",]),
                                            placeholder = "", width = "80%"),
                                  textInput(inputId = "q_obj2_3", label = paste0("Q11. ",module_text["activityA_obj2_Q11",]),
                                            placeholder = "", width = "80%"),
                                  textInput(inputId = "q_obj2_4", label = paste0("Q12. ",module_text["activityA_obj2_Q12",]),
                                            placeholder = "", width = "80%"),
                                  textInput(inputId = "q_obj2_5", label = paste0("Q13. ",module_text["activityA_obj2_Q13",]),
                                            placeholder = "", width = "80%"),
                                  textInput(inputId = "q_obj2_6", label = paste0("Q14. ",module_text["activityA_obj2_Q14",]),
                                            placeholder = "", width = "80%")))
                         
                               
                       
                       )
                     ),
                    ),
                            
                    
             # Tab4: Activity B ----
             tabPanel(title = "Activity B: Decide",
                      tags$style(type="text/css", "body {padding-top: 65px;}"),
                      img(src = "project-eddie-banner-2020_green.png", height = 100, 
                          width = 1544, top = 5),
                      h2("Activity B: Make decisions informed by a real water quality forecast"),
                      h4("Ecological forecasts have vast potential for aiding decision-making for range of different stakeholders, 
                         yet forecast results may be challenging to understand because they inherently are associated with uncertainty 
                         in alternate future outcomes which have not yet occurred.This activity will allow you to make decisions and alter future scenarios 
                         to optimize future drinking water quality. Forecasts will update through time, allowing you to see how forecast uncertainty 
                         changes over time, and how management decisions can impact water quality."),
                      tabsetPanel(
                        tabPanel('Scenario',
                                 h4(tags$b('Read the following scenario and use it to complete Objectives 3-5:')),
                                 img(src = 'CCR.jfif',
                                     height = '25%',
                                     width = '65%'),
                                 br(),
                                 br(),
                                 h4(tags$b('Scenario:')),
                                 p(module_text["activityB_scenario1",]),
                                 p(module_text["activityB_scenario2",]),
                                 p(module_text["activityB_scenario3",]),
                                 h4(tags$b('Each day as you look at the forecast you must decide to continue with the swimming event
                                           as planned or cancel the event.')),
                               #  tags$ol(tags$li('Continue with the swimming event as planned'),
                                #         tags$li('Cancel the swimming event'),
                                #         tags$li('Perform a low cost treatment in the treatment plant after the water is extracted from the reservoir. This would make the water safe for drinking but does not alter the water quality in the reservoir'),
                                #         tags$li('Perform a high cost water treatment action by adding chemicals directly into the reservoir. This would make the reservoir safe for both swimming and drinking, but would have negative ecological effects on the aquatic life in the reservoir')),
                                h3('Use these decision options to guide you in answering the questions in Objectives 3-5')
                                 ),
                        tabPanel('Objective 3',
                                 h4(tags$b("Objective 3: Identify the components of the decision you need to make a drinking water manager (PrOACT):")),
                                 br(),
                                 p("As a drinking water manager, you need to balance many different objectives. Your actions can influence the health of the 
                                   reservoir ecosystem, costs to the water utility your work for, drinking water quality for thousands of residents,
                                   and economic impact on your city based on the major swimming event. Forecasts can help in balancing these different
                                   decision objectives by facilitating structured decision-making. One type of structured decision-making tool is called PrOACT. 
                                   Scroll through the presentation below to learn more about PrOACT and to help you answer the question below."),
                                 slickROutput('PrOACT', width = '50%', height = '50%'),
                               h4('Use the definitions and examples in the slides to help you answer the following question. Drag and drop
                                  the answers from the answer bank to the appropriate category. There may be more than one answer for a 
                                  given category.'),  
                              wellPanel(style = paste0("background: ", ques_bg),
                                        fluidRow(  
                                          column(12, bucket_list(
                                            header = "",
                                            group_name = "bucket_list_group",
                                            orientation = "horizontal",
                                            add_rank_list(
                                              text = tags$b("Drag from here"),
                                              labels = sample(c(proact_answers)),
                                              input_id = "word_bank"
                                            ),
                                            add_rank_list(
                                              text = tags$b("Problem"),
                                              labels = NULL,
                                              input_id = "problem"
                                            ),
                                            add_rank_list(
                                              text = tags$b("Objective"),
                                              labels = NULL,
                                              input_id = "objective"
                                            ),
                                            add_rank_list(
                                              text = tags$b("Alternatives"),
                                              labels = NULL,
                                              input_id = "alternatives"
                                            ),
                                            add_rank_list(
                                              text = tags$b("Consequences"),
                                              labels = NULL,
                                              input_id = "consequences"
                                            ),
                                            add_rank_list(
                                              text = tags$b("Trade-Offs"),
                                              labels = NULL,
                                              input_id = "tradeoffs"
                                            )
                                          )))) ,
                                 
                          #      textInput(inputId = "activityb_obj3_q1", label = module_text["activityB_obj3_Q1",],
                          #                placeholder = "", width = "80%"),
                          #      textInput(inputId = "activityb_obj3_q2", label = module_text["activityB_obj3_Q2",],
                          #                placeholder = "", width = "80%")
                                 
                        ),
                        tabPanel('Objective 4',
                                 h4(tags$b('Objective 4: Decide how to manage a drinking water reservoir as forecast uncertainty changes through time')),
                                 p("Between your partner, choose one of you to be in Group A and one to be in Group B. Both of you will have to decide whether to proceed with the swimming event based on
                                 the water quality forecast. However, students in Group A will see different visualizations than students in Group B. 
                                 You will then discuss your choices and how they were influenced by the visualizations in Objective 5."),
                                 p(tags$b("You will be unable to change your selection after you pick one below, so make sure you discuss with your partner
                                          who will chose what!")), 
                                 br(),
                                 #radioButtons('student_group', label = 'Are you in Group A or B?', choices = c('A', 'B'), selected = character(0)),
                                # actionButton('choose_group', 'Submit Group Choice'),
                                 h4('Examine the 14-day water quality forecast as you approach the day of the swimming event, June 06 at Carvins Cove. 
                                 The forecasts will update over time, allowing you to update your decision as the day gets closer. 
                                 On each of the designated days, make a decision about how to manage the reservoir on each day of the forecast and 
                                 submit your answers below.'),
                                h5("Remember that water becomes dangerous for drinking when the chlorophyll-a concentration goes above 25 ug/L
                                  and dangerous for swimming when the chlorophyll-a concentration goes above 35 ug/L. You can display these thresholds
                                  dynamically on the figures by changing the 'Display threshold line' value."),
                                h5("The black dotted line represents the day on which the forecast is made and the solid grey line represents the
                                   day of the swimming event, June 06"),
                                 
                                 
                 # Day 14 decision
                                fluidRow(style = "border: 4px double black;",
                                  column(5,
                                                h4(tags$b('Days Before the Event: 14')),
                                                wellPanel(style = paste0("background: ", ques_bg),
                                                          numericInput('add_threshold_14', 'Display threshold line', value = 35),
                                                textInput('day14_forecast_value', 'What is the mean forecasted concentration for June 6 in the 14-day forecast?', placeholder = 'enter answer here', width = '100%'),
                                                textInput('day14_descibe_forecast', 'In your own words, describe the forecast over the next 14 days leading up to June 6', width = '100%'),
                                                radioButtons(inputId = "Decision_Day14", label = 'Decision 14 days before the event', selected = character(0),
                                                            choices = c('Continue with the swimming event as planned', 
                                                                        'Cancel the event'),  
                                                                         width = "100%"))),
                                         column(7,
                                                br(),
                                                plotlyOutput('forecast_plot_14'))),     
                                br(),
                                br(),
                  # Day 7 decision               
                                fluidRow(style = "border: 4px double black;",
                                         column(5,
                                                h4(tags$b('Days Before the Event: 7')),
                                                wellPanel(style = paste0("background: ", ques_bg),
                                                  numericInput('add_threshold_7', 'Change the threshold line', value = 35),
                                                  textInput('day7_forecast_value', 'What is the mean forecasted concentration for June 6 in the 7-day forecast?', placeholder = 'enter answer here'),
                                                  textInput('day7_descibe_forecast', 'In your own words, describe the forecast over the next 7 days leading up to June 6', width = '100%'),
                                                  radioButtons(inputId = "Decision_Day7", label = 'Decision 7 days before the event',
                                                                             choices = c('Continue with the swimming event as planned', 
                                                                                         'Cancel the event' ),  
                                                                             width = "100%", selected = character(0)))),
                                         column(7,
                                                plotlyOutput('forecast_plot_7')  
                                         )
                                        ),
                 br(),
                # Day 2 decision
                                fluidRow(style = "border: 4px double black;",
                                  column(5,
                                         h4(tags$b('Days Before the Event: 2')),
                                         wellPanel(style = paste0("background: ", ques_bg),
                                                   numericInput('add_threshold_2', 'Change the threshold line', value = 35),
                                                   textInput('day2_forecast_value', 'What is the mean forecasted concentration for June 6 in the 2-day forecast?', placeholder = 'enter answer here'),
                                                   selectInput('day2_forecast_multiple_choice', label = 'Choose the best description of the forecast on June 6 from the following options',
                                                      choices = forecast_descriptions,
                                                      selected = "", width = '100%'),
                                                   textInput('day2_descibe_forecast', 'In your own words, describe the forecast over the next 2 days leading up to June 6', width = '100%'),
                                                   radioButtons(inputId = "Decision_Day2", label = 'Decision 2 days before the event',
                                                                      choices = c('Continue with the swimming event as planned', 
                                                                                  'Cancel the event' ),  
                                                                      width = "100%", selected = character(0)))),
                                         column(7,
                                                conditionalPanel("input.Decision_Day7!==''",
                                                                 plotlyOutput('forecast_plot_2'))
                                         )
                                ),
                                        
                                h3("Once you've made your decisions, continue to Objective 5 to answer questions about your decision-making process and 
                                   compare your answers with your partner")
                                        
                                 ),
                        tabPanel('Objective X: decisions with UC',
                                 fluidRow(style = "border: 4px double black;",
                                          column(5,
                                                 h4(tags$b('Days Before the Event: 14')),
                                                 wellPanel(style = paste0("background: ", ques_bg),
                                                           numericInput('add_threshold_14_UC', 'Display threshold line', value = 35),
                                                           selectInput('day14_forecast_multiple_choice_UC', label = 'Choose the best description of the forecast on June 6 from the following options',
                                                                       choices = forecast_descriptions,
                                                                       selected = "", width = '100%'),
                                                           radioButtons(inputId = "Decision_Day14_UC", label = 'Decision 14 days before the event', selected = character(0),
                                                                        choices = c('Continue with the swimming event as planned', 
                                                                                    'Cancel the event'),  
                                                                        width = "100%"))),
                                          column(7,
                                                 br(),
                                                 plotlyOutput('forecast_plot_14_withUC'))),
                                 fluidRow(style = "border: 4px double black;",
                                          column(5,
                                                 h4(tags$b('Days Before the Event: 7')),
                                                 wellPanel(style = paste0("background: ", ques_bg),
                                                           numericInput('add_threshold_7_UC', 'Display threshold line', value = 35),
                                                           selectInput('day7_forecast_multiple_choice_UC', label = 'Choose the best description of the forecast on June 6 from the following options',
                                                                       choices = forecast_descriptions,
                                                                       selected = "", width = '100%'),
                                                           radioButtons(inputId = "Decision_Day7_UC", label = 'Decision 7 days before the event', selected = character(0),
                                                                        choices = c('Continue with the swimming event as planned', 
                                                                                    'Cancel the event'),  
                                                                        width = "100%"))),
                                          column(7,
                                                 br(),
                                                 plotlyOutput('forecast_plot_7_withUC'))),
                                 fluidRow(style = "border: 4px double black;",
                                          column(5,
                                                 h4(tags$b('Days Before the Event: 2')),
                                                 wellPanel(style = paste0("background: ", ques_bg),
                                                           numericInput('add_threshold_2_UC', 'Display threshold line', value = 35),
                                                           selectInput('day2_forecast_multiple_choice_UC', label = 'Choose the best description of the forecast on June 6 from the following options',
                                                                       choices = forecast_descriptions,
                                                                       selected = "", width = '100%'),
                                                           radioButtons(inputId = "Decision_Day2_UC", label = 'Decision 2 days before the event', selected = character(0),
                                                                        choices = c('Continue with the swimming event as planned', 
                                                                                    'Cancel the event'),  
                                                                        width = "100%"))),
                                          column(7,
                                                 br(),
                                                 plotlyOutput('forecast_plot_2_withUC')))                                 
                                 ),

                        tabPanel('Objective 5',
                                 h4(tags$b('Objective 5: Assess the impact of the forecast visualization on your decision-making')),
                                 br(),
                                 column(5,                                 
                                        plotlyOutput('WQ_decisions')),
                                 column(7,
                                        plotlyOutput('forecast_final')),
                                 p('Look at the observed water quality on the day of the swimming competition. Answer the following questions about your experience as a manager using the water quality forecast.'),
                                 wellPanel(style = paste0("background: ", ques_bg),
                                 textInput(inputId = "activityb_obj5_q1", label = module_text["activityB_obj5_Q1",],
                                           placeholder = "", width = "80%"),
                                 textInput(inputId = "activityb_obj5_q2", label = module_text["activityB_obj5_Q2",],
                                           placeholder = "", width = "80%"),
                                 textInput(inputId = "activityb_obj5_q3", label = module_text["activityB_obj5_Q3",],
                                           placeholder = "", width = "80%"),     
                                 textInput(inputId = "activityb_obj5_q4", label = module_text["activityB_obj5_Q4",],
                                           placeholder = "", width = "80%"),
                                 textInput(inputId = "activityb_obj5_q5", label = module_text["activityB_obj5_Q5",],
                                           placeholder = "Refer to the figure above to answer this question.", width = "80%"), 
                                 textInput(inputId = "activityb_obj5_q6", label = module_text["activityB_obj5_Q6",],
                                           placeholder = "Refer to the figure above to answer this question.", width = "80%"), 
                                 textInput(inputId = "activityb_obj5_q7", label = module_text["activityB_obj5_Q7",],
                                           placeholder = "", width = "80%"),
                                 textInput(inputId = "activityb_obj5_q8", label = module_text["activityB_obj5_Q8",],
                                           placeholder = "", width = "80%"),
                                 textInput(inputId = "activityb_obj5_q9", label = module_text["activityB_obj5_Q9",],
                                           placeholder = "", width = "80%"))
                        )
                      ),
                      
                        
             ),

             
             # Tab5: Activity C ----
             tabPanel(title = "Activity C: Customize",
                      tags$style(type="text/css", "body {padding-top: 65px;}"),
                      img(src = "project-eddie-banner-2020_green.png", height = 100, 
                          width = 1544, top = 5),
                      h2("Activity C: Explore different ways of visualizing ecological forecasts for specific stakeholder"),
                      h4("Uncertainty is an inherently difficult concept to understand, and especially difficult to represent visually. 
                      There are many ways to represent uncertainty visually and it has been shown that different representations
                      can lead to different levels of comprehension of the actual scenario. Further, the way that uncertainty is visualized is likely to
                      vary between stakeholders, with some stakeholders needing more information than others in order to facilitate quick and accurate
                      decision-making. This activity will allow you to role-play as a specific stakeholder, identify that stakeholder's decision needs,
                      and create a forecast visualization of uncertainty tailored to that stakeholder. Lastly, you will compare different visualizations 
                      of the same dataset and answer questions on how it impacts your comprehension of the scenario."),
                      tabsetPanel(tabPanel('Objective 6',
                                           h4(tags$b("Objective 6: Explore different ways to represent uncertainty and discuss how visualizations can be suited for stakeholder needs")),
                                           br(),
                                           p('Choose a stakeholder from the drop-down menu and answer the questions below'),
                                           wellPanel(style = paste0("background: ", ques_bg),
                                            fluidRow(
   
                                             column(8,
                                                    selectInput('stakeholder', 'Choose a stakeholder', choices = c('swimmer', 'fisher', 'dog owner', 'parent', 'drinking water manager')), #'water scientist', 
                                                    textInput(inputId = 'activityC_obj6_q1', label = module_text["activityC_obj6_Q1",],
                                                              width = '80%'),
                                                    br(),
                                                    h4(tags$b('Identify the PrOACT components of the stakeholder decision you identified above')),
                                                    textInput(inputId = "Problem_3", label = 'Problem(s)',
                                                              placeholder = "Enter problem(s) here", width = "80%"),
                                                    textInput(inputId = "Objective_3", label = 'Objective(s)',
                                                              placeholder = "Enter objective(s) here", width = "80%"),
                                                    textInput(inputId = "Alternative_3", label = 'Alternative(s)',
                                                              placeholder = "Enter alternative(s) here", width = "80%"),
                                                    textInput(inputId = "Consequence_3", label = 'Consequence(s)',
                                                              placeholder = "Enter consequence(s) here", width = "80%"),
                                                    textInput(inputId = "TradeOff_3", label = 'Trade Off(s)',
                                                              placeholder = "Enter trade off(s) here", width = "80%"),                        ),
                                             column(4,
                                                    htmlOutput('stakeholder_name'),
                                                    br(),
                                                    textOutput('stakeholder_text'),
                                                    br(),
                                                    imageOutput('stakeholder_pic')
                                                    
                                             )))),
                                  tabPanel('Objective 7',
                                           h4(tags$b('Objective 7: Create a customized a forecast visualization for your stakeholder using the questions you answered in Objective 6 to guide your decisions')),
                                           textInput('stakehold_name', 'Which stakeholder did you choose in Objective 6?', placeholder = 'Enter stakeholder name', width = '80%'),
                                           h4("Forecast data are inherently difficult to visualize because they represent alternate future scenarios which have not yet occurred.
                                              Below you will see a data table of forecast output. In this activity, you will explore multiple ways of communicating this same data
                                              in order to create a customized forecast visualization for your stakeholder."),
                                           h5("First, you should get to know your data. Use the 'Calculate Statistics' button to calculate various statistics for
                                              one day of the forecast and input them into QX."),
                                          fluidRow(
                                           column(6, DT::dataTableOutput('fcast_table')),
                                           column(6, h3("Calculate statistics"),
                                                  selectInput('forecast_viz_date', label = 'Select a date', choices = seq.Date(as.Date('2021-06-05'), as.Date('2021-06-18'), by = 'day')),
                                                  selectInput("stat_calc", label = "Select calculation:", choices = c("Pick a summary statistic", 'mean', 'median', 'max', 'min', 'standard deviation')),
                                                  textOutput("out_stats"),
                                                  h3('Choose one day and answer the following questions'),
                                                  wellPanel( style = paste0("background: ", ques_bg),
                                                  textInput('mean_ens', label = 'What is the mean concentration of all the ensembles?',
                                                            placeholder = 'Enter answer here', width = "60%"),
                                                  textInput('median_ens', label = 'What is the median concentration of all the ensembles?',
                                                            placeholder = 'Enter answer here', width = "60%"),
                                                  textInput('min_ens', label = 'What is the minimum concentration of all the ensembles?',
                                                            placeholder = 'Enter answer here', width = "60%"),
                                                  textInput('max_ens', label = 'What is the maximum concentration of all the ensembles?',
                                                            placeholder = 'Enter answer here', width = "60%"),
                                                  textInput('sd_ens', label = 'What is the standard deviation of all the ensembles?',
                                                            placeholder = 'Enter answer here', width = "60%")),
                                                  
                                                  
                                                  )),
                                    
                                           br(),
                                          br(),
                                          br(),
                                          h4(tags$b("Now that you are familiar with your data, explore the following visualization options to make
                                             a customized visualization for your stakeholder. Remember to consider the decision needs of your stakeholder
                                                    as you choose from among the visualization options.")),
                                           fluidRow(column(5,
                                                          wellPanel(style = paste0("background: ", ques_bg),
                                                                    radioButtons('metric_raw', 'Select whether to represent uncertainty as a summarized value based on a metric or as the actual forecasted data', 
                                                                                 choices = c('metric', 'raw forecast output'), selected = character(0)),
                                                                    conditionalPanel("input.metric_raw=='metric'",
                                                                                     radioButtons('summ_comm_type', 'Select a communication type to represent your summarized uncertainty',
                                                                                                  choices = c('word', 'number', 'icon', 'figure'), selected = character(0))),
                                                                    conditionalPanel("input.metric_raw=='raw forecast output'",
                                                                                     radioButtons('raw_comm_type', 'Select a communication type to represent uncertainty in your raw forecast output',
                                                                                                  choices = c('number', 'figure'), selected = character(0))),
                                                                    conditionalPanel("input.metric_raw=='metric' && input.summ_comm_type=='figure'",
                                                                                     radioButtons('summ_plot_options', 'Select the plot type for a summarized metric', choices = c('pie', 'time series', 'bar graph'), selected = character(0))),
                                                                    conditionalPanel("input.metric_raw=='raw forecast output' && input.raw_comm_type=='figure'", radioButtons('raw_plot_options', 'Select the plot type for raw forecast output', choices = c('pie', 'time series', 'bar graph'), selected = character(0))),
                                                                    conditionalPanel("input.metric_raw=='raw forecast output' && input.raw_comm_type=='figure' && input.raw_plot_options=='time series'",
                                                                                     radioButtons('ts_line_type', 'Select how you want to visualize the forecast ensembles',
                                                                                                  choices = c('Line', 'Distribution'),
                                                                                                  selected = character(0))),
                                                                    actionButton('create_plot', 'Create Custom Plot'),
                                                                    textInput('figure_title', 'Give your figure a title', placeholder = 'Enter title here', width = '80%'),
                                                                    textInput('figure_caption', 'Give your figure a caption to help your stakeholder understand it', placeholder = 'Enter caption here', width = '80%')
                                                                    #radioButtons('static_interactive', 'Select whether you want a static or interactive plot', choices = c('static', 'interactive'), selected = character(0)),
                                                                    
                                                          )),
                                                   column(7,
                                                          conditionalPanel("input.summ_comm_type=='icon'",
                                                                           plotlyOutput('custom_plotly')),
                                                          conditionalPanel("input.summ_comm_type!=='icon'",
                                                                           plotOutput('custom_plot'))
                                                          
                                                   )),
                                           h4('Once you are satisfied with your forecast visualization, continue to Objective 8.'),
                                       ),
                                  tabPanel('Objective 8',
                                           h4(tags$b('Objective 8: Examine how different uncertainty visualizations impact your comprehension and decision-making')),
                                           br(),
                                           h4('Using your completed, customized visualization, answer the follow questions'),  
                                           fluidRow(column(3,),
                                           column(6,
                                                  conditionalPanel("input.summ_comm_type=='icon'",
                                                                    plotlyOutput('custom_plotly_second_time')),
                                                  conditionalPanel("input.summ_comm_type!=='icon'",
                                                                   plotOutput('custom_plot_second_time'))
                                                  ),
                                           column(3,)),
                                           
                                           wellPanel(style = paste0("background: ", ques_bg),
                                             fluidRow(
                                             textInput('activityC_obj8_Q1', label = module_text["activityC_obj8_Q1",], placeholder = 'Enter answer here', width = '100%'),
                                             textInput('activityC_obj8_Q2', label = module_text["activityC_obj8_Q2",], placeholder = 'Enter answer here', width = '100%'),
                                             textInput('activityC_obj8_Q3', label = module_text["activityC_obj8_Q3",], placeholder = 'If you chose a word or number communication type, skip this question.', width = '100%'),
                                             textInput('activityC_obj8_Q4', label = module_text["activityC_obj8_Q4",], placeholder = 'Enter answer here', width = '100%'),
                                             textInput('activityC_obj8_Q5', label = module_text["activityC_obj8_Q5",], placeholder = 'Enter answer here', width = '100%'),
                                             textInput('activityC_obj8_Q6', label = module_text["activityC_obj8_Q6",], placeholder = 'Enter answer here', width = '100%'),
                                             textInput('activityC_obj8_Q7', label = module_text["activityC_obj8_Q7",], placeholder = 'Enter answer here', width = '100%')
                                             
                                             
                                           ))
                                           
                                           )
                                           
              
              
        )
    
  )
 )
)

server <- function(input, output){

  file <- reactive({gsub("\\\\", "/", input$forecast_file$datapath)})
  
  output$forecast_image <- renderImage({
    req(file())
    list(src = file(), height = '70%') #width = '70%', based on TM recommendation 30Jan
  }, deleteFile = FALSE)
  
  output$forecast_image_second_time <- renderImage({
    req(file())
    list(src = file(),height = '70%') # width = '70%', 
  }, deleteFile = FALSE)
  
  file_2 <- reactive({gsub("\\\\", "/", input$forecast_file_2$datapath)})
  
  output$forecast_image_2 <- renderImage({
    req(file_2())
    list(src = file_2(), height = '70%') #width = '70%', 
  }, deleteFile = FALSE)
  
  output$PrOACT <- renderSlickR({
    imgs <- list.files("www", pattern = "PrOACT", full.names = TRUE)
    slickR(imgs)
  })  

#observeEvent(input$student_group, {
#  disable("student_group", !is.na(input$student_group))
#})

fc_plots <- reactiveValues(day14 = NULL, day7 = NULL, day2 = NULL)

observe({
  fcast <- read.csv("data/wq_forecasts/forecast_day14.csv")
  fcast$date <- as.Date(fcast$date)
  data <- read.csv("data/wq_forecasts/mock_chl_obs.csv")
  data$date <- as.Date(data$date)
  
  fc_plots$day14 <- ggplot()+
    geom_line(data = fcast, aes(date, mean, color = "Forecast Mean")) +
    scale_y_continuous(breaks = seq(0, 100, 10))+
    xlim(min(fcast$date)-7, max(fcast$date)) +
    geom_point(data = data[data$date<=min(fcast$date),], aes(date, obs_chl_ugl, color = "Obs"), size = 4) +
    geom_vline(xintercept = as.numeric(min(fcast$date)), linetype = "dashed") +
    geom_vline(xintercept = as.numeric(date_of_event), color = 'grey44', size = 1.2) +
    #geom_label(data = day14, aes(Past, y, label = 'Past'), size = 12) +
    scale_color_manual(name = "", values = c("Obs" = l.cols[2], 'Forecast Mean' = 'black'))+
    ylab("Chlorophyll-a (ug/L)") +
    xlab("Date") +
    theme_classic(base_size = 15) +
    theme(panel.border = element_rect(fill = NA, colour = "black"), 
          axis.text.x = element_text(size = 15),
          legend.text = element_text(size = 8),
          legend.title = element_text(size = 10))
  
  
})

output$forecast_plot_14 <- renderPlotly({
  p <- fc_plots$day14 
  if(!is.na(input$add_threshold_14)){
    p <- fc_plots$day14 +  geom_hline(yintercept = input$add_threshold_14, col = 'red', size = 1.1)
  }
  return(ggplotly(p))
})


 output$forecast_plot_14_withUC <- renderPlotly({
   fcast <- read.csv("data/wq_forecasts/forecast_day14.csv")
   fcast$date <- as.Date(fcast$date)
   p <- fc_plots$day14 + geom_ribbon(data = fcast, aes(date, ymin = min, ymax = max, fill = "95% Confidence Interval"), alpha = 0.3) +
     scale_color_manual(name = "", values = c("Obs" = l.cols[2], 'Forecast Mean' = 'black', "95% Confidence Interval" = l.cols[4]))
     
   
   if(!is.na(input$add_threshold_14)){
     p <- p +  geom_hline(yintercept = input$add_threshold_14, col = 'red', size = 1.1)
   }
   p <- ggplotly(p)
   
   for (i in 1:length(p$x$data)){
     if (!is.null(p$x$data[[i]]$name)){
       p$x$data[[i]]$name =  gsub("\\(","",str_split(p$x$data[[i]]$name,",")[[1]][1])
     }
   }
   
   return(p)
   })
  
 
 observe({
   fcast <- read.csv("data/wq_forecasts/forecast_day7.csv")
   fcast$date <- as.Date(fcast$date)
   data <- read.csv("data/wq_forecasts/mock_chl_obs.csv")
   data$date <- as.Date(data$date)
   
   fc_plots$day7 <-    ggplot()+
     geom_line(data = fcast, aes(date, mean, color = "Forecast Mean")) +
     scale_y_continuous(breaks = seq(0, 100, 10))+
     xlim(min(fcast$date)-7, max(fcast$date)) +
     geom_point(data = data[data$date<=min(fcast$date),], aes(date, obs_chl_ugl, color = "Obs"), size = 4) +
     geom_vline(xintercept = as.numeric(min(fcast$date)), linetype = "dashed") +
     geom_vline(xintercept = as.numeric(date_of_event), color = 'grey44', size = 2) +
     scale_color_manual(name = "", values = c("Obs" = l.cols[2], 'Forecast Mean' = 'black'))+
     #geom_label(data = day14, aes(Past, y, label = 'Past'), size = 12) +
     ylab("Chlorophyll-a (ug/L)") +
     xlab("Date") +
     theme_classic(base_size = 15) +
     theme(panel.border = element_rect(fill = NA, colour = "black"), 
           axis.text.x = element_text(size = 15),
           legend.text = element_text(size = 8),
           legend.title = element_text(size = 10))
   
 })
 
 output$forecast_plot_7 <- renderPlotly({
   req(input$Decision_Day14)
   p <- fc_plots$day7 
   if(!is.na(input$add_threshold_7)){
     p <- fc_plots$day7 +  geom_hline(yintercept = input$add_threshold_7, col = 'red', size = 1.1)
   }
   return(ggplotly(p))
 })
 
 
 output$forecast_plot_7_withUC <- renderPlotly({
   req(input$Decision_Day14_UC)
   fcast <- read.csv("data/wq_forecasts/forecast_day7.csv")
   fcast$date <- as.Date(fcast$date)
   p <- fc_plots$day7 + geom_ribbon(data = fcast, aes(date, ymin = min, ymax = max, fill = "95% Confidence Interval"), alpha = 0.3) +
     scale_color_manual(name = "", values = c("Obs" = l.cols[2], 'Forecast Mean' = 'black', "95% Confidence Interval" = l.cols[4]))
   
   
   if(!is.na(input$add_threshold_7)){
     p <- p +  geom_hline(yintercept = input$add_threshold_7, col = 'red', size = 1.1)
   }
   
  p <- ggplotly(p)
  
    for (i in 1:length(p$x$data)){
     if (!is.null(p$x$data[[i]]$name)){
       p$x$data[[i]]$name =  gsub("\\(","",str_split(p$x$data[[i]]$name,",")[[1]][1])
     }
    }
  
   return(p)
 })
 
 
 observe({
   fcast <- read.csv("data/wq_forecasts/forecast_day2.csv")
   fcast$date <- as.Date(fcast$date)
   data <- read.csv("data/wq_forecasts/mock_chl_obs.csv")
   data$date <- as.Date(data$date)
   
   fc_plots$day2 <-     ggplot()+
     geom_line(data = fcast, aes(date, mean, color = "Forecast Mean")) +
     scale_y_continuous(breaks = seq(0, 100, 10))+
     xlim(min(fcast$date)-7, max(fcast$date)) +
     geom_point(data = data[data$date<=min(fcast$date),], aes(date, obs_chl_ugl, color = "Obs"), size = 4) +
     geom_vline(xintercept = as.numeric(min(fcast$date)), linetype = "dashed") +
     geom_vline(xintercept = as.numeric(date_of_event), color = 'grey44', size = 2) +
     scale_color_manual(name = "", values = c("Obs" = l.cols[2], 'Forecast Mean' = 'black'))+
     #geom_label(data = day14, aes(Past, y, label = 'Past'), size = 12) +
     ylab("Chlorophyll-a (ug/L)") +
     xlab("Date") +
     theme_classic(base_size = 15) +
     theme(panel.border = element_rect(fill = NA, colour = "black"), 
           axis.text.x = element_text(size = 15),
           legend.text = element_text(size = 8),
           legend.title = element_text(size = 10))
   
 })
 
 output$forecast_plot_2 <- renderPlotly({
   req(input$Decision_Day7)
   p <- fc_plots$day2 
   if(!is.na(input$add_threshold_2)){
     p <- fc_plots$day2 +  geom_hline(yintercept = input$add_threshold_2, col = 'red', size = 1.1)
   }
   return(ggplotly(p))
 })
 
 
 output$forecast_plot_2_withUC <- renderPlotly({
   req(input$Decision_Day7_UC)
   fcast <- read.csv("data/wq_forecasts/forecast_day2.csv")
   fcast$date <- as.Date(fcast$date)
   p <- fc_plots$day2 + geom_ribbon(data = fcast, aes(date, ymin = min, ymax = max, fill = "95% Confidence Interval"), alpha = 0.3) +
     scale_color_manual(name = "", values = c("Obs" = l.cols[2], 'Forecast Mean' = 'black', "95% Confidence Interval" = l.cols[4])) 
   
   if(!is.na(input$add_threshold_2)){
     p <- p +  geom_hline(yintercept = input$add_threshold_2, col = 'red', size = 1.1)
   }
   p <- ggplotly(p)
   
   for (i in 1:length(p$x$data)){
     if (!is.null(p$x$data[[i]]$name)){
       p$x$data[[i]]$name =  gsub("\\(","",str_split(p$x$data[[i]]$name,",")[[1]][1])
     }
   }
   
   return(p)
   })
 
 
decision_data <- reactive({
  data <- data.frame(
    day = c(as.Date('2021-05-23'), as.Date('2021-05-30'), as.Date('2021-06-04')),
    choice_noUC = NA,
    choice_withUC = NA,
    binary_noUC = NA,
    binary_withUC = NA)
  
  data$choice_noUC <- c(input$Decision_Day14,
                        input$Decision_Day7,
                        input$Decision_Day2)
  data$choice_withUC <- c(input$Decision_Day14_UC,
                          input$Decision_Day7_UC,
                          input$Decision_Day2_UC)
  
  for (i in 1:nrow(data)) {
    data$binary_noUC[i] <- ifelse(data$choice_noUC[i]=='Cancel the event', 0, 1)
    data$binary_withUC[i] <- ifelse(data$choice_withUC[i]=='Cancel the event', 0.1, 0.9)
  }
  
  return(data)
})
 
output$WQ_decisions <- renderPlotly({
  req(input$Decision_Day2_UC)
  
  decisions <- ggplot(data = decision_data()) +
    geom_point(aes(x = day, y = binary_noUC, color = "Without Uncertainty", position = 'jitter'), size = 4) +
    geom_point(aes(x = day, y = binary_withUC, color = "With Uncertainty", position = 'jitter'), size = 4) +
    scale_y_continuous(breaks = c(0, 1), labels = c('Cancel', 'Continue')) +
    ylab("Decision") +
    xlab("Date") +
    scale_x_date(breaks = c(as.Date('2021-05-23'), as.Date('2021-05-27'), as.Date('2021-05-30'), as.Date('2021-06-04')), date_labels = '%b %d') +
    scale_color_manual(name = "", values = c("Without Uncertainty" = cols[5], "With Uncertainty" = cols[3]))+
    theme_classic(base_size = 15) +
    theme(panel.border = element_rect(fill = NA, colour = "black"), 
          axis.text = element_text(size = 10),
          axis.text.y = element_text(angle = 90, hjust = 0.7),
          legend.text = element_text(size = 8),
          legend.title = element_text(size = 10))
  
  return(ggplotly(decisions))
})
  
output$forecast_final <- renderPlotly({
  data <- read.csv("data/wq_forecasts/mock_chl_obs.csv")
  data$date <- as.Date(data$date)
  data <- data[data$date<date_of_event,]
  
  fcast_14 <- read.csv("data/wq_forecasts/forecast_day14.csv")
  fcast_14$date <- as.Date(fcast_14$date)
  fcast_10 <- read.csv("data/wq_forecasts/forecast_day10.csv")
  fcast_10$date <- as.Date(fcast_10$date)
  fcast_10 <- fcast_10[fcast_10$date<=date_of_event+2,]
  fcast_7 <- read.csv("data/wq_forecasts/forecast_day7.csv")
  fcast_7$date <- as.Date(fcast_7$date)
  fcast_7 <- fcast_7[fcast_7$date<=date_of_event+2,]
  fcast_2 <- read.csv("data/wq_forecasts/forecast_day2.csv")
  fcast_2$date <- as.Date(fcast_2$date)
  fcast_2 <- fcast_2[fcast_2$date<=date_of_event+2,]
  data <- read.csv("data/wq_forecasts/mock_chl_obs.csv")
  data$date <- as.Date(data$date)
  data <- data[data$date<=date_of_event+2,]
  
  final_plot <- ggplot() +
    xlim(min(data$date), date_of_event + 2) +
    geom_ribbon(data = fcast_14, aes(date, ymin = min, ymax = max, fill = "14-day"), alpha = 0.8) +
    geom_line(data = fcast_14, aes(date, mean, color = "14-day mean")) + #B2DF8A
    geom_ribbon(data = fcast_10, aes(date, ymin = min, ymax = max, fill = "10-day"), alpha = 0.7) +
    geom_line(data = fcast_10, aes(date, mean,  color = "10-day mean")) + #A6CEE3
    geom_ribbon(data = fcast_7, aes(date, ymin = min, ymax = max, fill = "7-day"), alpha = 0.7) +
    geom_line(data = fcast_7, aes(date, mean, color = "7-day mean")) + # 33A02C
    geom_ribbon(data = fcast_2, aes(date, ymin = min, ymax = max, fill = "2-day"), alpha = 0.6) +
    geom_line(data = fcast_2, aes(date, mean, color = "2-day mean")) + # FB9A99
    scale_y_continuous(breaks = seq(0, 100, 10))+
    scale_color_manual(values = c("14-day mean" = cols[1], "10-day mean" = cols[5], "7-day mean" = cols[3], "2-day mean" = cols[4], "Obs" = cols[6])) +
    scale_fill_manual(values = c("14-day" = cols[1], "10-day" = cols[5], "7-day" = cols[3], "2-day" = cols[4])) +
    geom_point(data = data, aes(date, obs_chl_ugl, color = "Obs"), size = 2.5) +
    geom_vline(xintercept = as.numeric(date_of_event), color = 'grey44', size = 1.3) +
    ylab("Chlorophyll-a (ug/L)") +
    xlab("Date") +
    theme_classic(base_size = 15) +
    theme(panel.border = element_rect(fill = NA, colour = "black"), 
          axis.text.x = element_text(size = 15),
          legend.text = element_text(size = 8),
          legend.title = element_text(size = 10))
  
  final_plot <- ggplotly(final_plot)
  
  for (i in 1:length(final_plot$x$data)){
    if (!is.null(final_plot$x$data[[i]]$name)){
      final_plot$x$data[[i]]$name =  gsub("\\(","",str_split(final_plot$x$data[[i]]$name,",")[[1]][1])
    }
  }
  
  
  
  return(final_plot)
})
  
output$PlotID <- renderImage({
    idx <- which(plot_types == input$plot_type)
    filename <-  normalizePath(file.path('./www', paste0(plot_files[idx])))
      
    list(src = filename,
         width = 400,
         height = 600,
         alt = "Alt text")
    
  }, deleteFile = FALSE) 
  plot_type <- reactive({input$plot_type})
  
  
output$stakeholder_pic <- renderImage({
    stakeholder_id <-  which(stakeholder_info$stakeholder_selected == input$stakeholder)
         filename <- normalizePath(file.path('./www', paste0(stakeholder_info[stakeholder_id,2])))
         print(filename)
         list(src = filename,
              width = '70%',
              height = '50%',
              alt = 'error loading file')
    
  }, deleteFile = FALSE)
    
output$stakeholder_name <- renderUI({
  stakeholder_id <-  which(stakeholder_info$stakeholder_selected == input$stakeholder)
  HTML(paste0("<b>", stakeholder_info[stakeholder_id,6], "<b>"))
})
output$stakeholder_text <- renderText({
  stakeholder_id <-  which(stakeholder_info$stakeholder_selected == input$stakeholder)
  stakeholder_info[stakeholder_id,4]   #4th column holds the text
})
  
fcast <- reactive({
  fcast <- read.csv("data/wq_forecasts/forecast_day2.csv")
  fcast$date <- as.Date(fcast$date)
  fcast[,2:30] <- round(fcast[,2:30], digits = 2)
  fcast <- fcast[,-c(31, 32, 33)]
  return(fcast)
})

output$fcast_table <- DT::renderDataTable({
  fcast()[-1,-c(2, 3, 4, 5)]}, 
  options = list(scrollX = TRUE))
 
output$out_stats <- renderText({
if(input$stat_calc=='Pick a summary statistic'){
  return("")
}
  
  fcast_stats <- fcast()[fcast()$date == as.Date(input$forecast_viz_date), ]
  fcast_stats <- fcast_stats[,-1]
  fcast_stats <- as.matrix(fcast_stats)
  if(input$stat_calc=='mean'){
    out_stat <- rowMeans(fcast_stats)
    out_stat <- paste0('Mean: ', signif(out_stat, 3))
  }
  if(input$stat_calc=='min'){
    out_stat <- rowMins(fcast_stats)
    out_stat <- paste0('Minimum: ', signif(out_stat, 3))
  }
  if(input$stat_calc=='max'){
    out_stat <- rowMaxs(fcast_stats)
    out_stat <- paste0('Maximum: ', signif(out_stat, 3))
  }
  if(input$stat_calc=='standard deviation'){
    out_stat <- rowSds(fcast_stats)
    out_stat <- paste0('Standard Deviation: ', signif(out_stat, 3))
  }
  if(input$stat_calc=='median'){
    out_stat <- rowMedians(fcast_stats)
    out_stat <- paste0('Median: ', signif(out_stat, 3))
  }
  return(out_stat)  
  })

output$custom_plotly <- renderPlotly({
  cust_plot$plot

 
})
  

   cust_plot <- reactiveValues(plot = NULL)
   
   observe({
     if(input$create_plot){ # should be yes or no based on whether or not the button is clicked
       if(input$metric_raw=='metric'){
         req(input$summ_comm_type)
         if(input$summ_comm_type=='word'){
           p1 <- ggplot(data = mock_data, aes(x = date_of_forecast[16], y = forecast_ugL[16])) +
             geom_label(aes(label = 'High Chance of \n Algal Bloom', x = date_of_forecast[16] + 0.5), size = 20) +
             labs(title = input$figure_title, caption = input$figure_caption) +
             theme(legend.position = 'none',
                   panel.background = element_rect(fill = NA, color = 'black'),
                   panel.border = element_rect(color = 'black', fill = NA),
                   axis.text = element_blank(),
                   axis.title = element_blank(),
                   axis.ticks = element_blank(),
                   plot.title = element_text(size = 25, hjust = 0.5),
                   plot.caption = element_text(size = 15, hjust = 0))
           cust_plot$plot <- p1
         }
         if(input$summ_comm_type=='number'){
           p2 <-  ggplot(data = mock_data, aes(x = date_of_forecast[16], y = forecast_ugL[16])) +
             geom_label(aes(label = '>75% chance of \n Algal Bloom', x = date_of_forecast[16] + 0.5), size = 20) +
             labs(title = input$figure_title, caption = input$figure_caption) +
             theme(legend.position = 'none',
                   panel.background = element_rect(fill = NA, color = 'black'),
                   panel.border = element_rect(color = 'black', fill = NA),
                   axis.text = element_blank(),
                   axis.title = element_blank(),
                   axis.ticks = element_blank(),
                   plot.title = element_text(size = 25, hjust = 0.5),
                   plot.caption = element_text(size = 15, hjust = 0))
           cust_plot$plot <- p2
         }
         if(input$summ_comm_type=='icon'){
           dial <- plot_ly(
             domain = list(x = c(0, 1), y = c(0, 1)),
             value = 75,
             title = list(text = "Likelihood of Algal Bloom"),
             type = "indicator",
             mode = "gauge+number+delta",
             gauge = list(
               axis =list(range = list(NULL, 100)),
               steps = list(
                 list(range = c(0, 100), color = "lightgray"),
                 list(range = c(50, 100), color = "red"))))   
           cust_plot$plot <- dial
         }
         if(input$summ_comm_type=='figure'){
           req(input$summ_plot_options)
           if(input$summ_plot_options=='pie'){
             data <- data.frame(
               group=c('0-10%', '10-30%', '30-60%', '60-90%', '90-100%'),
               value=c(13,7,9,21,2)
             )
             p_pie <-  ggplot(data, aes(x="", y=value, fill=group)) +
               geom_bar(stat="identity", width=1, color="white") +
               coord_polar("y", start=0) +
               labs(title = input$figure_title, caption = input$figure_caption) +
               theme_void() # remove background, grid, numeric labels
             cust_plot$plot <- p_pie
           }
           if(input$summ_plot_options=='time series'){
             fcast <- read.csv("data/wq_forecasts/forecast_day2.csv")
             fcast$date <- as.Date(fcast$date)
             fcast$percent_over_35 <- NA
             
             for (i in 2:nrow(fcast)) {
               number <-   length(which(fcast[i,6:30] > 35))
               fcast$percent_over_35[i] <- number/25*100
             }
             
             p_metric_ts <- ggplot()+
               geom_line(data = fcast, aes(date, percent_over_35), size = 2) +
               scale_y_continuous(breaks = seq(0, 100, 10))+
               ylab("% Likelihood of Algal Bloom") +
               xlab("Date") +
               labs(title = paste0("Time Series leading up to June 18 Forecast \n", input$figure_title), caption = input$figure_caption) +
               theme_classic(base_size = 24) +
               theme(panel.border = element_rect(fill = NA, colour = "black"), 
                     axis.text.x = element_text(size = 24),
                     legend.position = 'none')
             cust_plot$plot <- p_metric_ts
           } # this one is messed up
           if(input$summ_plot_options=='bar graph'){
             data <- data.frame(
               group=c('0-10%', '10-30%', '30-60%', '60-90%', '90-100%'),
               value=c(13,7,9,21,2)
             )
             p_metric_bar <- ggplot(data = data, aes(group, value, fill = group)) +
               geom_bar(stat = 'identity') +
               labs(title = input$figure_title, caption = input$figure_caption) +
               ylab('Number of Simulations') +
               xlab('% Likelihood of Algal Bloom') +
               theme(legend.position = 'none',
                     panel.background = element_rect(fill = NA, color = 'black'),
                     panel.border = element_rect(color = 'black', fill = NA),
                     plot.title = element_text(size = 25, hjust = 0.5),
                     plot.caption = element_text(size = 15, hjust = 0))
             cust_plot$plot <- p_metric_bar
           }
         }
       }
       if(input$metric_raw=='raw forecast output'){
         req(input$raw_comm_type)
         if(input$raw_comm_type=='number'){
           fcast <- read.csv("data/wq_forecasts/forecast_day2.csv")
           fcast$date <- as.Date(fcast$date)
           fcast <- fcast[15,]
           
           p_raw_number <- ggplot(data = fcast, aes(x = date, y = mean)) +
             geom_label(aes(label = paste0("The forecasted \n algal concentration is \n ", round(mean, 1), ' +/-', round(min, 1), ' ug/L'), x =date+ 0.5), size = 12) +
             labs(title = input$figure_title, caption = input$figure_caption) +
             theme(legend.position = 'none',
                   panel.background = element_rect(fill = NA, color = 'black'),
                   panel.border = element_rect(color = 'black', fill = NA),
                   axis.text = element_blank(),
                   axis.title = element_blank(),
                   axis.ticks = element_blank(),
                   plot.title = element_text(size = 25, hjust = 0.5),
                   plot.caption = element_text(size = 15, hjust = 0))
           cust_plot$plot <- p_raw_number
         }
         if(input$raw_comm_type=='figure'){
           req(input$raw_plot_options)
           if(input$raw_plot_options=='pie'){
             fcast <- read.csv("data/wq_forecasts/forecast_day2.csv")
             fcast$date <- as.Date(fcast$date)
             fcast <- fcast[15,]
             fcast <- fcast %>% select(date, ens_1:ens_25) %>% 
               gather(key = ensemble, value = forecast, ens_1:ens_25)
             
             info <- hist(fcast$forecast)
             
             data <- data.frame(
               breaks = info$breaks[1:length(info$breaks)-1],
               counts = as.vector(info$counts)
             ) 
             data$counts <- as.factor(data$counts)
             data$breaks <- as.factor(data$breaks)
             p_pie_raw <- ggplot(data, aes(x="", y=counts, fill=breaks)) +
               scale_fill_brewer(palette = 'Set2', name = 'Range of Predicted Chl Concentration', 
                                 label = c('0-15', '15-20', '20-25', '25-30', '30-35', '35-40', '40-45', '45-50')) +
               geom_bar(stat="identity", width=1) +
               coord_polar("y", start=0) +
               labs(title = input$figure_title, caption = input$figure_caption) +
               theme_void() # remove background, grid, numeric labels
             cust_plot$plot <- p_pie_raw
           }
           if(input$raw_plot_options=='bar graph'){
             # visualizing just the last horizon of the forecast
             fcast <- read.csv("data/wq_forecasts/forecast_day2.csv")
             fcast$date <- as.Date(fcast$date)
             fcast <- fcast[15,]
             fcast <- fcast %>% select(date, ens_1:ens_25) %>% 
               gather(key = ensemble, value = forecast, ens_1:ens_25)
             
             info <- hist(fcast$forecast)
             
             data <- data.frame(
               breaks = info$breaks[1:length(info$breaks)-1],
               counts = as.vector(info$counts)
             )
             data$breaks <- as.factor(data$breaks)
             
             
             p_bar_raw <-  ggplot(data = data, aes(breaks, counts, fill = breaks)) +
               geom_bar(stat = 'identity') +
               scale_fill_brewer(palette = 'Dark2', name = 'Range of Predicted Chl Concentration', 
                                 label = c('0-15', '15-20', '20-25', '25-30', '30-35', '35-40', '40-45', '45-50')) +
               ylab('Frequency of Prediction') +
               xlab('Predicted Algal Concentration (ug/L)') +
               labs(title = paste0("June 18 Forecast \n", input$figure_title), caption = input$figure_caption) +
               theme(
                 panel.background = element_rect(fill = NA, color = 'black'),
                 panel.border = element_rect(color = 'black', fill = NA),
                 plot.title = element_text(size = 25, hjust = 0.5),
                 plot.caption = element_text(size = 15, hjust = 0))
             cust_plot$plot <- p_bar_raw
           }
           if(input$raw_plot_options=='time series'){
             req(input$ts_line_type)
             fcast <- read.csv("data/wq_forecasts/forecast_day2.csv")
             fcast$date <- as.Date(fcast$date)
             data <- read.csv("data/wq_forecasts/mock_chl_obs.csv")
             data$date <- as.Date(data$date)
             
             p_raw_ts_distribution <- ggplot()+
               geom_line(data = fcast(), aes(date, mean)) +
               scale_y_continuous(breaks = seq(0, 100, 10))+
               xlim(min(fcast()$date)-7, max(fcast()$date)) +
               geom_point(data = data[data$date<=min(fcast()$date),], aes(date, obs_chl_ugl), color = l.cols[3], size = 4) +
               geom_ribbon(data = fcast(), aes(date, ymin = min, ymax = max), fill = l.cols[3], alpha = 0.3) +
               geom_vline(xintercept = as.Date(min(fcast()$date)), linetype = "dashed") +
               geom_vline(xintercept = as.Date(date_of_event), color = 'grey44', size = 2) +
               ylab("Chlorophyll-a (ug/L)") +
               xlab("Date") +
               labs(title = paste0("Time Series leading up to June 18 Forecast \n", input$figure_title), caption = input$figure_caption) +
               theme_classic(base_size = 24) +
               theme(panel.border = element_rect(fill = NA, colour = "black"), 
                     axis.text.x = element_text(size = 24),
                     legend.position = 'none')
             
             fcast<- fcast %>% select(date, ens_1:ens_25) %>% 
               gather(key = ensemble, value = forecast, ens_1:ens_25, -date)
             
             p_raw_ts_ens <- ggplot()+
               geom_line(data = fcast, aes(date, forecast, group = ensemble), color = l.cols[3], size = 0.8) +
               scale_y_continuous(breaks = seq(0, 100, 10))+
               xlim(min(fcast()$date)-7, max(fcast$date)) +
               geom_point(data = data[data$date<=min(fcast$date),], aes(date, obs_chl_ugl), color = l.cols[3], size = 4) +
               geom_vline(xintercept = as.Date(min(fcast$date)), linetype = "dashed") +
               geom_vline(xintercept = as.Date(date_of_event), color = 'grey44', size = 2) +
               ylab("Chlorophyll-a (ug/L)") +
               xlab("Date") +
               labs(title = paste0("Time Series leading up to June 18 Forecast \n", input$figure_title), caption = input$figure_caption) +
               theme_classic(base_size = 24) +
               theme(panel.border = element_rect(fill = NA, colour = "black"), 
                     axis.text.x = element_text(size = 24),
                     legend.position = 'none')
             
             
             if(input$ts_line_type=='Line'){
               cust_plot$plot <- p_raw_ts_ens
               
             }
             if(input$ts_line_type=='Distribution'){
               cust_plot$plot <- p_raw_ts_distribution
               
             }
           }
         }
       }
     }
     
     
   })
  # cust_plot_2 <- cust_plot #this one shows up on the next tab
  
  output$custom_plot <- renderPlot({
    cust_plot$plot
      
  })
  
  output$custom_plot_second_time <- renderPlot({
    cust_plot$plot
  })
  
  output$custom_plotly_second_time <- renderPlotly({
    dial <- plot_ly(
      domain = list(x = c(0, 1), y = c(0, 1)),
      value = 75,
      title = list(text = "Likelihood of Algal Bloom"),
      type = "indicator",
      mode = "gauge+number+delta",
      gauge = list(
        axis =list(range = list(NULL, 100)),
        steps = list(
          list(range = c(0, 100), color = "lightgray"),
          list(range = c(50, 100), color = "red"))))   
    return(ggplotly(dial))
  })
  
}





shinyApp(ui = ui, server = server)
