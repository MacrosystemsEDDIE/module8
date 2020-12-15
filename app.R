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

# Options for Spinner
options(spinner.color="#0275D8", spinner.color.background="#ffffff", spinner.size=2)

# colors for plots
cols <- RColorBrewer::brewer.pal(8, "Dark2")
l.cols <- RColorBrewer::brewer.pal(8, "Set2")


# Load app input
module_text <- read.csv("data/module_text.csv", row.names = 1, header = FALSE)
EF_links <- read.csv("data/eco_forecast_examples.csv")
forecast_dates <- read.csv("data/forecast_dates.csv")
stakeholder_info <- read.csv("data/stakeholders.csv")

mock_data <- read.csv('data/wq_forecasts/microcystin_mock_data.csv')
mock_data$date_forecast_made <- as.Date(mock_data$date_forecast_made)
mock_data$date_of_forecast <- as.Date(mock_data$date_of_forecast)

# Define plot types
plot_types <- c("binary", "likelihood", "confidence interval", "all ensembles")
plot_files <- as.vector(c("binary.png", "likelihood.png", "confidence_interval.png", "all_ensembles.png"))

# define the date of the swimming event (Activity B)
date_of_event <- as.Date('2019-10-08')

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
                               tags$li("Activity C - Discuss how forecast visualizations impact decision-making")
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
                      h2("Activity A: Explore an ecological forecast"),
                      h4("Many of us use various types of forecasts in our daily life to make decisions (e.g., weather forecasts). However, because producing ecological forecasts
                                  is still a relatively new practice, many people are unaware of the availability of ecological forecasts. This activity will
                                  introduce you to several existing ecological forecasts, and guide you through learning what they predict, why they are made,
                                  and how they are used."),
                     br(),
                      tabsetPanel(
                       tabPanel(title = 'List of Ecological Forecasts',
                                h3("List of Ecological Forecasts (more forecasts and logos coming soon)"),
                                tags$ul(
                                  tags$li(a(href = EF_links$webpage[1], EF_links$Forecast[1]), br(), p(EF_links$About[1]), img(src = EF_links$logo_file[1], height = '10%', width = '5%')),
                                  tags$li(a(href = EF_links$webpage[2], EF_links$Forecast[2]), br(), p(EF_links$About[2]), img(src = EF_links$logo_file[2], height = '10%', width = '5%')),
                                  tags$li(a(href = EF_links$webpage[3], EF_links$Forecast[3]), br(), p(EF_links$About[3])),
                                  tags$li(a(href = EF_links$webpage[4], EF_links$Forecast[4]), br(), p(EF_links$About[4])),
                                  tags$li(a(href = EF_links$webpage[5], EF_links$Forecast[5]), br(), p(EF_links$About[5])),
                                  tags$li(a(href = EF_links$webpage[6], EF_links$Forecast[6]), br(), p(EF_links$About[6]))
                                )),
                       tabPanel('Objective 1',
                                h4(tags$b("Objective 1: Familiarize yourself with an ecological forecast by identifying the basic components of a forecast, 
                                   forecast stakeholders, stakeholder usage, and how forecasts are visualized")),
                                br(),
                                h4("Choose an ecological forecast from the previous panel and answer the following questions."),
                                br(),
                                tags$ul(
                                  textInput(inputId = "q1", label = "What is the name of the forecasting system you chose?",
                                            placeholder = "", width = "80%"),
                                  textInput(inputId = "q1", label = module_text["activityA_Q1",],
                                            placeholder = "", width = "80%"),
                                  selectInput(inputId = "q2", label = module_text["activityA_Q2",],
                                              choices = c("", 'Forest', 'Freshwater', 'Marine', 'Agricultural', 'Urban', 'Desert', 'Grassland', 'Global', 'Other'),  width = "80%"),
                                  textInput(inputId = "q3", label = module_text["activityA_Q3",],
                                            placeholder = "", width = "80%"),
                                  textInput(inputId = "q4", label = module_text["activityA_Q4",],
                                            placeholder = "", width = "80%"),
                                  textInput(inputId = "q5", label = module_text["activityA_Q5",],
                                            placeholder = "", width = "80%"),
                                  textInput(inputId = "q6", label = module_text["activityA_Q6",],
                                            placeholder = "", width = "80%"),
                                  textInput(inputId = "q7", label = module_text["activityA_Q7",],
                                            placeholder = "", width = "80%"),
                                  textInput(inputId = "q8", label = module_text["activityA_Q8",],
                                            placeholder = "", width = "80%"),
                                  textInput(inputId = "q9", label = module_text["activityA_Q9",],
                                            placeholder = "", width = "80%"),
                                  textInput(inputId = "q10", label = module_text["activityA_Q10",],
                                            placeholder = "", width = "80%"),
                                  textInput(inputId = "q11", label = module_text["activityA_Q11",],
                                            placeholder = "", width = "80%"),
                                  textInput(inputId = "q12", label = module_text["activityA_Q12",],
                                            placeholder = "", width = "80%"),
                                  
                                  #tags$li(module_text["activityA_Q2",]),
                                  #tags$li(module_text["activityA_Q3",]),
                                  #tags$li(module_text["activityA_Q4",]),
                                  #tags$li(module_text["activityA_Q5",]),
                                  #tags$li(module_text["activityA_Q6",]),
                                  #tags$li(module_text["activityA_Q7",]),
                                  #tags$li(module_text["activityA_Q8",]),
                                  #tags$li(module_text["activityA_Q9",]),
                                  #tags$li(module_text["activityA_Q10",]),
                                  #tags$li(module_text["activityA_Q11",]),
                                  #tags$li(module_text["activityA_Q12",])
                                  
                                ),
                                
                       ),
                       tabPanel('Objective 2',
                                h4(tags$b("Objective 2: With another team, compare forecasting systems and your answers above. 
                                   Answer the questions below regarding how the forecasts compare")),
                                br(),
                                h4("With another team, compare forecasting systems and your answers above. 
                                Discuss the following questions regarding the ecological forecasting systems you explored."),
                                br(),
                                tags$ul(
                                  textInput(inputId = "q_obj2_1", label = module_text["activityA_obj2_Q1",],
                                            placeholder = "", width = "80%"),
                                  textInput(inputId = "q_obj2_2", label = module_text["activityA_obj2_Q2",],
                                            placeholder = "", width = "80%"),
                                  textInput(inputId = "q_obj2_3", label = module_text["activityA_obj2_Q3",],
                                            placeholder = "", width = "80%"),
                                  textInput(inputId = "q_obj2_4", label = module_text["activityA_obj2_Q4",],
                                            placeholder = "", width = "80%"),
                                  textInput(inputId = "q_obj2_5", label = module_text["activityA_obj2_Q5",],
                                            placeholder = "", width = "80%"),
                                  textInput(inputId = "q_obj2_6", label = module_text["activityA_obj2_Q6",],
                                            placeholder = "", width = "80%"),)
                                
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
                                 p(paste0('Scenario: ', module_text["activityB_scenario1",])),
                                 p(module_text["activityB_scenario2",]),
                                 p(module_text["activityB_scenario3",]),
                                 br(),
                                 h4(tags$b('Each day as you look at the forecast you must decide between the following options')),
                                 tags$ol(tags$li('Continue with the swimming event as planned'),
                                         tags$li('Cancel the swimming event'),
                                         tags$li('Perform a low cost treatment in the treatment plant after the water is extracted from the reservoir. This would make the water safe for drinking but does not alter the water quality in the reservoir'),
                                         tags$li('Perform a high cost water treatment action by adding chemicals directly into the reservoir. This would make the reservoir safe for both swimming and drinking, but would have negative ecological effects on the aquatic life in the reservoir')),
                                h3('Use this decision options to guide you in answers the questions in Objectives 3-5')
                                 ),
                        tabPanel('Objective 3',
                                 h4(tags$b("Objective 3: Identify the components of the decision you need to make a drinking water manager (PrOACT):")),
                                 br(),
                                 p("As a drinking water manager, you need to balance many different objectives. Your actions can influence the health of the 
                                   reservoir ecosystem, costs to the water utility your work for, drinking water quality for thousands of residents,
                                   and economic impact on your city based on the major swimming event. Please consider all of these components
                                   when answering the following questions."),
                                 textInput(inputId = "Problem", label = 'Problem(s)',
                                           placeholder = "What is the problem you are faced with?", width = "80%"),
                                 textInput(inputId = "Objective", label = 'Objective(s)',
                                           placeholder = "There are many consequences of a decision. 
                                           What is the ultimate objective you are trying to achieve?", width = "80%"),
                                 textInput(inputId = "Alternative", label = 'Alternative(s)',
                                           placeholder = "What alternative decisions can you make?", width = "80%"),
                                 textInput(inputId = "Consequence", label = 'Consequence(s)',
                                           placeholder = "What are the consequences of each of the alternatives identified above?", width = "80%"),
                                 textInput(inputId = "TradeOff", label = 'Trade Off(s)',
                                           placeholder = "What trade-offs are you making given each alternative decision?", width = "80%"),
                                 
                        ),
                        tabPanel('Objective 4',
                                 h4(tags$b('Objective 4: Decide how to manage a drinking water reservoir as forecast uncertainty changes through time')),
                                 p('In Objective 4, students in Group A will see different visualizations than students in Group B. You will then discuss
                                 your choices in Objective 5. Within your pairs, decide which of you will be in each group.'),
                                 br(),
                                 radioButtons('student_group', label = 'Are you in Group A or B', choices = c('A', 'B'), selected = character(0)),
                                # actionButton('choose_group', 'Submit Group Choice'),
                                 p('Examine the water quality forecast for the day of the swimming event at Carvins Cove as it updates over time. 
                                           On each of the designated days, make a decision about how to manage the reservoir on each day of the forecast and 
                                           submit your answers below.'),
                                 
                                 
                 # Day 16 decision
                                fluidRow(style = "border: 4px double black;",
                                  column(5,
                                                h4(tags$b('Days Before the Event: 16')),
                                                wellPanel(numericInput('add_threshold_16', 'Change the threshold line', value = 35)),
                                                textInput('day16_forecast_value', 'What is the mean forecasted concentration 16 days before the event?', placeholder = 'enter answer here'),
                                                conditionalPanel("input.day16_forecast_value!==''",
                                                                 selectInput(inputId = "Decision_Day16", label = 'Decision 16 days before the event',
                                                                             choices = c("",'Continue with the swimming event as planned', 
                                                                                         'Cancel the event', 
                                                                                         'Perform a low cost treatment in the treatment plant after the water is extracted from the reservoir', 
                                                                                         'Perform a high cost water treatment action by adding chemicals directly into the reservoir' ),  
                                                                             width = "100%"))),
                                         column(7,
                                                plotlyOutput('forecast_plot_16'))),     
                                br(),
                  # Day 10 decision
                                fluidRow(style = "border: 4px double black;",
                                  column(5,
                                         h4(tags$b('Days Before the Event: 10')),
                                         conditionalPanel("input.Decision_Day16!==''", wellPanel(numericInput('add_threshold_10', 'Change the threshold line', value = 35))),
                                         conditionalPanel("input.Decision_Day16!==''",
                                                          textInput('day10_forecast_value', 'What is the forecasted concentration 10 days before the event?', placeholder = 'enter answer here')
                                         ),
                                         conditionalPanel("input.day10_forecast_value!==''",
                                                          selectInput(inputId = "Decision_Day10", label = 'Decision 10 days before the event',
                                                                      choices = c("",'Continue with the swimming event as planned', 
                                                                                  'Cancel the event', 
                                                                                  'Perform a low cost treatment in the treatment plant after the water is extracted from the reservoir', 
                                                                                  'Perform a high cost water treatment action by adding chemicals directly into the reservoir' ),  
                                                                      width = "100%")),
                                         
                                           ),
                                  column(7,
                                        conditionalPanel("input.Decision_Day16!==''" ,plotlyOutput('forecast_plot_10'))  )),
                             
                                         #valueBox('low' , 'Cost of Treatment', icon = icon("dollar-sign"))
                                         #valueBox('Poor', 'Outgoing Drinking Water Quality', icon = icon("faucet"), color = 'magenta')
                br(),
                  # Day 7 decision               
                                fluidRow(style = "border: 4px double black;",
                                         column(5,
                                                h4(tags$b('Days Before the Event: 7')),
                                                conditionalPanel("input.Decision_Day10!==''", wellPanel(numericInput('add_threshold_7', 'Change the threshold line', value = 35))),
                                                conditionalPanel("input.Decision_Day10!==''", 
                                                                 textInput('day7_forecast_value', 'What is the forecasted concentration 7 days before the event?', placeholder = 'enter answer here')
                                                ),
                                                conditionalPanel("input.day7_forecast_value!==''", 
                                                                 selectInput(inputId = "Decision_Day7", label = 'Decision 7 days before the event',
                                                                             choices = c("",'Continue with the swimming event as planned', 
                                                                                         'Cancel the event', 
                                                                                         'Perform a low cost treatment in the treatment plant after the water is extracted from the reservoir', 
                                                                                         'Perform a high cost water treatment action by adding chemicals directly into the reservoir' ),  
                                                                             width = "100%"))
                                                ),
                                         column(7,
                                            conditionalPanel("input.Decision_Day10!==''",
                                                             plotlyOutput('forecast_plot_7'))  
                                         )
                                        ),
                 br(),
                # Day 2 decision
                                fluidRow(style = "border: 4px double black;",
                                  column(5,
                                         h4(tags$b('Days Before the Event: 2')),
                                         conditionalPanel("input.Decision_Day7!==''", wellPanel(numericInput('add_threshold_2', 'Change the threshold line', value = 35))),
                                         conditionalPanel("input.Decision_Day7!==''",
                                                          textInput('day2_forecast_value', 'What is the forecasted concentration 2 days before the event?', placeholder = 'enter answer here')
                                                ),
                                         
                                         conditionalPanel("input.day2_forecast_value!==''",
                                                          selectInput(inputId = "Decision_Day2", label = 'Decision 2 days before the event',
                                                                      choices = c("",'Continue with the swimming event as planned', 
                                                                                  'Cancel the event', 
                                                                                  'Perform a low cost treatment in the treatment plant after the water is extracted from the reservoir', 
                                                                                  'Perform a high cost water treatment action by adding chemicals directly into the reservoir' ),  
                                                                      width = "100%"))
                                                ),
                                         column(7,
                                                conditionalPanel("input.Decision_Day7!==''",
                                                                 plotlyOutput('forecast_plot_2'))
                                         )
                                ),
                                        
                                h3("Once you've your decisions, continue to Objective 5 to answer questions about your decision-making process")
                                        
                                 ), 
                                 
# some ideas for things to include
# how uncertain is your model? toggle bar
# update forecast after making a management action
# need to build in data based on the scenarios, e.g. if perform treatment within reservoir, chla goes down
                        tabPanel('Objective 5',
                                 h4(tags$b('Objective 5: Assess the impact of the forecast uncertainty on your comprehension and decision-making
                                           regarding managing drinking water quality')),
                                 br(),
                                 p('Look at the observed water quality on the day of the swimming competition. Answer the following questions about your experience as a manager using the water quality forecast.'),
                                 textInput(inputId = "activity_b_assign_3_q_1", label = 'What was the actual algal concentration on the day of the swimming competition?',
                                           placeholder = "", width = "80%"),
                                 textInput(inputId = "activity_b_assign_3_q_2", label = 'What was your final decision on managing the water quality of the reservoir two days ahead of the swimming competition?',
                                           placeholder = "", width = "80%"),
                                 textInput(inputId = "activity_b_assign_3_q_3", label = 'How did you decision change over time?',
                                           placeholder = "", width = "80%"),     
                                 textInput(inputId = "activity_b_assign_3_q_4", label = 'How did the uncertainty around the water quality change as you moved through time?',
                                           placeholder = "", width = "80%"),
                                 textInput(inputId = "activity_b_assign_3_q_5", label = 'What was the range of uncertainty around the forecast on the day of the event in the 16-day forecast?',
                                           placeholder = "", width = "80%"), 
                                 textInput(inputId = "activity_b_assign_3_q_6", label = 'What was the range of uncertainty around the forecast on the day of the event in the 2-day forecast?',
                                           placeholder = "", width = "80%"), 
                                 textInput(inputId = "activity_b_assign_3_q_5", label = 'How did the forecast visualization help you or hurt you in making decisions?',
                                           placeholder = "", width = "80%"),
                                 textInput(inputId = "activity_b_obj_5_q_6", label = 'What other information would you have liked to know in order to inform your decision',
                                           placeholder = "", width = "80%")
                        )
                      ),
                      
                        
             ),

             
             # Tab5: Activity C ----
             tabPanel(title = "Activity C: Customize",
                      tags$style(type="text/css", "body {padding-top: 65px;}"),
                      img(src = "project-eddie-banner-2020_green.png", height = 100, 
                          width = 1544, top = 5),
                      h2("Activity C: Explore different ways of visualizing ecological forecasts for specific stakeholder"),
                      h4("Descriptive motivating text for this activity. something about how uncertainty is a notoriously difficult concept to understand
                      and represent visually. There are many ways to represent uncertainty visually and it has been shown that different representations
                      can lead to different levels of comprehension of the actual scenario. Further, the way that uncertainty is visualized is likely to
                      vary between stakeholders, with some stakeholders needing more information than others in order to facilitate quick and accurate
                      decision-making. This activity will allow you to role-play as a specific stakeholder, identify that stakeholder's decision needs,
                      and create a forecast visualization of uncertainty tailored to that stakeholder. Lastly, you will compare different visualizations 
                      of the same dataset and answer questions on how it impacts your comprehension of the scenario."),
                      tabsetPanel(tabPanel('Objective 6',
                                           h4(tags$b("Objective 6: Explore different ways to represent uncertainty and discuss how visualizations can be suited for stakeholder needs")),
                                           br(),
                                           p('Choose a stakeholder from the drop-down menu and answer the questions below'),
                                           fluidRow(
                                             
                                             column(8,
                                                    selectInput('stakeholder', 'Choose a stakeholder', choices = c('swimmer', 'fisher', 'dog owner', 'parent', 'water scientist', 'drinking water manager')),
                                                    textInput(inputId = 'activity_c_q_1', label = 'Name one decision that your stakeholder could make using the forecast',
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
                                                    imageOutput('stakeholder_pic'),
                                                    textOutput('stakeholder_text')
                                             ))),
                                  tabPanel('Objective 7',
                                           h4(tags$b('Objective 7: Create a customized a forecast visualization for your stakeholder using the questions you answered in Objective 6 to guide your decisions')),
                                           p('NOTE: Still brainstorming viz options, suggestions welcome. Functionality is not yet built in. Some of these will be hierarchical (i.e., cant have pie chart which uses shapes)'),
                                           textInput('stakehold_name', 'Which stakeholder did you choose in Objective 6?', placeholder = 'Enter stakeholder name', width = '80%'),
                                          fluidRow(column(5,
                                                          wellPanel(radioButtons('metric_raw', 'Select whether to represent uncertainty as a summarized value based on a metric or as the actual forecasted data', 
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
                                   fluidRow(
                                      h4('Once you are satisfied with your forecast visualization, justify your choices via the questions below'),
                                      textInput('C_Obj7_Q1', 'Why did you choose a metric or raw forecast output?', placeholder = 'Enter answer here', width = '100%'),
                                      textInput('C_Obj7_Q2', 'Why did you choose a the communication type that you did (e.g., word, number, icon, or figure)?', placeholder = 'Enter answer here', width = '100%'),
                                      textInput('C_Obj7_Q3', 'If you chose a figure representation, why did you choose a the plot type that you did (e.g., pie, time series, bar graph)?', placeholder = 'Enter answer here', width = '100%'),
                                      textInput('C_Obj7_Q4', 'What other improvements would you make to customize for your stakeholder?', placeholder = 'Enter answer here', width = '100%'),
                                      textInput('C_Obj7_Q5', 'What other stakeholders could use your forecast?', placeholder = 'Enter answer here', width = '100%'),
                                      textInput('C_Obj7_Q6', 'Pick one of the stakeholders from the above question.Is their use case different from your original stakeholder?', placeholder = 'Enter answer here', width = '100%'),
                                      textInput('C_Obj7_Q7', 'If you were customizing a visualization for this stakeholder, how might you alter your current visualization?', placeholder = 'Enter answer here', width = '100%'),
                                      
                                    )
                                           
                                           ),
                                  tabPanel('Objective 8',
                                           h4(tags$b('Objective 8: Examine how different uncertainty visualizations impact your comprehension and decision-making')),
                                           br(),
                                           p('Explore the four visualizations below and answer the follow questions'),        
                                           fluidRow(
                                             column(7,
                                                    selectInput("plot_type", "Plot Types", plot_types, selected = plot_types[1]), #for the drop-down options
                                                    br(),
                                                    textInput(inputId = 'activity_c_assign_2_q_1', label = 'Which visualizations represent uncertainty and which do not?',
                                                              width = '80%'),
                                                    textInput(inputId = 'activity_c_assign_2_q_2', label = 'How is uncertainty represented in each visualization? (e.g., using color, shapes, summarized data, etc.)',
                                                              width = '80%'),
                                                    textInput(inputId = 'activity_c_assign_2_q_3', label = 'Which visualization contains the most explicit representation of uncertainty?',
                                                              width = '80%'),
                                                    textInput(inputId = 'activity_c_assign_2_q_4', label = 'Based on the forecast visualizations, what is the maximum possbible value forecasted?',
                                                              width = '80%'),
                                                    textInput(inputId = 'activity_c_assign_2_q_5', label = 'Which visualization do you prefer for your stakeholder? Why?',
                                                              width = '80%')
                                             ),
                                             column(5,
                                                    imageOutput("PlotID", width = "20%",height = "10%", inline = T) #for the image to be displayed
                                             )
                                             
                                           )
                                           
                                           )
                                           
              
              
        )
    
  )
 )
)

server <- function(input, output){


  
 output$forecast_plot_16 <- renderPlotly({
   p <- ggplot(data = mock_data, aes(date_of_forecast[16], forecast_ugL[16])) +
     geom_point(size = 5) +
     ylim(0, 50) +
     ggtitle(paste0('Forecasted Microcystin for 2020-07-19 made on ', mock_data$date_forecast_made[16]))+
     xlim((mock_data$date_of_forecast[16]-1), (mock_data$date_of_forecast[16]+1)) +
     ylab('Forecasted Microsystin (ug/L)') +
     xlab('Forecast Date') +
     theme(legend.position = 'none',
           panel.background = element_rect(fill = NA, color = 'black'),
           panel.border = element_rect(color = 'black', fill = NA),
           axis.text = element_text(size = 10),
           axis.title = element_text(size = 15),
           plot.title = element_text(size = 15, hjust = 0.5))
  if(input$student_group=='B'){
    p <- p + geom_errorbar(aes(ymin = upper_CI[16], ymax = lower_CI[16]), width = 0.5) +
      geom_label(aes(label = paste0(forecast_ugL[16], '+/-', CI_boundary[16], 'ug/L'), x = date_forecast_made[i] + 0.5), size = 10) 

  }
   
    if(!is.na(input$add_threshold_16)){
     p <- p +  geom_hline(yintercept = input$add_threshold_16, col = 'red', size = 1.1)
       
    }
   print(mock_data$date_of_forecast[16])
      return(ggplotly(p))

 })
  
 output$forecast_plot_10 <- renderPlotly({
   p_10 <- ggplot(data = mock_data, aes(date_of_forecast[10], forecast_ugL[10])) +
     geom_point(size = 5) +
     ylim(0, 50) +
     ggtitle(paste0('Forecasted Microcystin for 2020-07-19 made on ', mock_data$date_forecast_made[10]))+
     xlim((mock_data$date_of_forecast[10]-1), (mock_data$date_of_forecast[10]+1)) +
     ylab('Forecasted Microsystin (ug/L)') +
     xlab('Forecast Date') +
     theme(legend.position = 'none',
           panel.background = element_rect(fill = NA, color = 'black'),
           panel.border = element_rect(color = 'black', fill = NA),
           axis.text = element_text(size = 10),
           axis.title = element_text(size = 15),
           plot.title = element_text(size = 15, hjust = 0.5))
   if(input$student_group=='B'){
     p_10 <- p_10 + geom_errorbar(aes(ymin = upper_CI[10], ymax = lower_CI[10]), width = 0.5) +
       geom_label(aes(label = paste0(forecast_ugL[10], '+/-', CI_boundary[10], 'ug/L'), x = date_forecast_made[i] + 0.5), size = 10) 
     
   }
   
   if(!is.na(input$add_threshold_10)){
     p_10 <- p_10 +  geom_hline(yintercept = input$add_threshold_10, col = 'red', size = 1.1)
     
   }
   return(ggplotly(p_10))
   
 })
 
 output$forecast_plot_7 <- renderPlotly({
   p_7 <- ggplot(data = mock_data, aes(date_of_forecast[7], forecast_ugL[7])) +
     geom_point(size = 5) +
     ylim(0, 50) +
     ggtitle(paste0('Forecasted Microcystin for 2020-07-19 made on ', mock_data$date_forecast_made[7]))+
     xlim((mock_data$date_of_forecast[7]-1), (mock_data$date_of_forecast[7]+1)) +
     ylab('Forecasted Microsystin (ug/L)') +
     xlab('Forecast Date') +
     theme(legend.position = 'none',
           panel.background = element_rect(fill = NA, color = 'black'),
           panel.border = element_rect(color = 'black', fill = NA),
           axis.text = element_text(size = 10),
           axis.title = element_text(size = 15),
           plot.title = element_text(size = 15, hjust = 0.5))
   if(input$student_group=='B'){
     p_7 <- p_7 + geom_errorbar(aes(ymin = upper_CI[7], ymax = lower_CI[7]), width = 0.5) +
       geom_label(aes(label = paste0(forecast_ugL[7], '+/-', CI_boundary[7], 'ug/L'), x = date_forecast_made[7] + 0.5), size = 10) 
     
   }
   
   if(!is.na(input$add_threshold_7)){
     p_7 <- p_7 +  geom_hline(yintercept = input$add_threshold_7, col = 'red', size = 1.1)
     
   }
   return(ggplotly(p_7))
   
 })
 
 output$forecast_plot_2 <- renderPlotly({
   p_2 <- ggplot(data = mock_data, aes(date_of_forecast[2], forecast_ugL[2])) +
     geom_point(size = 5) +
     ylim(0, 50) +
     ggtitle(paste0('Forecasted Microcystin for 2020-07-19 made on ', mock_data$date_forecast_made[2]))+
     xlim((mock_data$date_of_forecast[2]-1), (mock_data$date_of_forecast[2]+1)) +
     ylab('Forecasted Microsystin (ug/L)') +
     xlab('Forecast Date') +
     theme(legend.position = 'none',
           panel.background = element_rect(fill = NA, color = 'black'),
           panel.border = element_rect(color = 'black', fill = NA),
           axis.text = element_text(size = 10),
           axis.title = element_text(size = 15),
           plot.title = element_text(size = 15, hjust = 0.5))
   if(input$student_group=='B'){
     p_2 <- p_2 + geom_errorbar(aes(ymin = upper_CI[2], ymax = lower_CI[2]), width = 0.5) +
       geom_label(aes(label = paste0(forecast_ugL[2], '+/-', CI_boundary[2], 'ug/L'), x = date_forecast_made[7] + 0.5), size = 10) 
     
   }
   
   if(!is.na(input$add_threshold_2)){
     p_2 <- p_2 +  geom_hline(yintercept = input$add_threshold_2, col = 'red', size = 1.1)
     
   }
   return(ggplotly(p_2))
   
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
    
output$stakeholder_text <- renderText({
  stakeholder_id <-  which(stakeholder_info$stakeholder_selected == input$stakeholder)
  stakeholder_info[stakeholder_id,4] #4th column holds the text
})
  
output$custom_plotly <- renderPlotly({
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
  
  output$custom_plot <- renderPlot({
    if(input$create_plot){
      if(input$metric_raw=='metric' && input$summ_comm_type=='word'){
        p1 <- ggplot(data = mock_data, aes(x = date_of_forecast[16], y = forecast_ugL[16])) +
          geom_label(aes(label = 'High Chance of \n Algal Bloom', x = mock_data$date_of_forecast[16] + 0.5), size = 20) +
          labs(title = input$figure_title, caption = input$figure_caption) +
          theme(legend.position = 'none',
                panel.background = element_rect(fill = NA, color = 'black'),
                panel.border = element_rect(color = 'black', fill = NA),
                axis.text = element_blank(),
                axis.title = element_blank(),
                axis.ticks = element_blank(),
                plot.title = element_text(size = 25, hjust = 0.5),
                plot.caption = element_text(size = 15, hjust = 0))
        print(p1)
      }
      if(input$metric_raw=='metric' && input$summ_comm_type=='number'){
       p2 <-  ggplot(data = mock_data, aes(x = date_of_forecast[16], y = forecast_ugL[16])) +
          geom_label(aes(label = '>75% chance of \n Algal Bloom', x = mock_data$date_of_forecast[16] + 0.5), size = 20) +
          labs(title = input$figure_title, caption = input$figure_caption) +
          theme(legend.position = 'none',
                panel.background = element_rect(fill = NA, color = 'black'),
                panel.border = element_rect(color = 'black', fill = NA),
                axis.text = element_blank(),
                axis.title = element_blank(),
                axis.ticks = element_blank(),
                plot.title = element_text(size = 25, hjust = 0.5),
                plot.caption = element_text(size = 15, hjust = 0))
       print(p2)
      }
      if(input$metric_raw=='metric' && input$summ_comm_type=='icon'){
        plot(4,6, main = 'metric icon placeholder')
      }
      if(input$metric_raw=='metric' && input$summ_comm_type=='figure' && input$summ_plot_options=='pie'){
        data <- data.frame(
          group=c('0-10%', '10-30%', '30-60%', '60-90%', '90-100%'),
          value=c(13,7,9,21,2)
        )
        
        # Basic piechart
       p_pie <-  ggplot(data, aes(x="", y=value, fill=group)) +
          geom_bar(stat="identity", width=1, color="white") +
          coord_polar("y", start=0) +
         labs(title = input$figure_title, caption = input$figure_caption) +
         theme_void() # remove background, grid, numeric labels
       return(p_pie)
      }
      if(input$metric_raw=='metric' && input$summ_comm_type=='figure' && input$summ_plot_options=='time series'){
        print(hist(rnorm(7), main = 'metric time series'))
      }
      if(input$metric_raw=='metric' && input$summ_comm_type=='figure' && input$summ_plot_options=='bar graph'){
        data <- data.frame(
          group=c('0-10%', '10-30%', '30-60%', '60-90%', '90-100%'),
          value=c(13,7,9,21,2)
        )
        p_bar <- ggplot(data = data, aes(group, value, fill = group)) +
          geom_bar(stat = 'identity') +
          labs(title = input$figure_title, caption = input$figure_caption) +
          ylab('Number of Simulations') +
          xlab('% Likelihood of Algal Bloom') +
          theme(legend.position = 'none',
                panel.background = element_rect(fill = NA, color = 'black'),
                panel.border = element_rect(color = 'black', fill = NA),
                plot.title = element_text(size = 25, hjust = 0.5),
                plot.caption = element_text(size = 15, hjust = 0))
        return(p_bar)
        }
      if(input$metric_raw=='raw forecast output' && input$raw_comm_type=='number'){
        p3 <-  ggplot(data = mock_data, aes(x = date_of_forecast[16], y = forecast_ugL[16])) +
          geom_label(aes(label = paste0(mock_data$forecast_ugL[16], ' +/-', mock_data$CI_boundary[16], ' ug/L'), x = mock_data$date_of_forecast[16] + 0.5), size = 20) +
          labs(title = input$figure_title, caption = input$figure_caption) +
          theme(legend.position = 'none',
                panel.background = element_rect(fill = NA, color = 'black'),
                panel.border = element_rect(color = 'black', fill = NA),
                axis.text = element_blank(),
                axis.title = element_blank(),
                axis.ticks = element_blank(),
                plot.title = element_text(size = 25, hjust = 0.5),
                plot.caption = element_text(size = 15, hjust = 0))
        print(p3)
      }
      if(input$metric_raw=='raw forecast output' && input$raw_comm_type=='figure' && input$raw_plot_options=='pie'){
        data <- data.frame(
          group=c('0-5', '5-10', '10-20', '20-30', '30+'),
          value=c(7,13,9,21,2)
        )
        p_pie_raw <-  ggplot(data, aes(x="", y=value, fill=group)) +
          geom_bar(stat="identity", width=1, color="white") +
          coord_polar("y", start=0) +
          labs(title = input$figure_title, caption = input$figure_caption) +
          theme_void() # remove background, grid, numeric labels
        return(p_pie_raw)
        } 
      if(input$metric_raw=='raw forecast output' && input$raw_comm_type=='figure' && input$raw_plot_options=='time series'){
        print(hist(rnorm(32), main = 'raw time series viz'))
      }
      if(input$metric_raw=='raw forecast output' && input$raw_comm_type=='figure' && input$raw_plot_options=='bar graph'){
        data <- data.frame(
          group=c('0-5', '5-10', '10-20', '20-30', '30+'),
          value=c(7,13,9,21,2)
        )
        p_bar_raw <- ggplot(data = data, aes(group, value, fill = group)) +
          geom_bar(stat = 'identity') +
          labs(title = input$figure_title, caption = input$figure_caption) +
          ylab('Number of Simulations') +
          xlab('Predicted Algal Concentration') +
          theme(legend.position = 'none',
                panel.background = element_rect(fill = NA, color = 'black'),
                panel.border = element_rect(color = 'black', fill = NA),
                plot.title = element_text(size = 25, hjust = 0.5),
                plot.caption = element_text(size = 15, hjust = 0))
        return(p_bar_raw)       }
      
    }
      
  })
  

}





shinyApp(ui = ui, server = server)
