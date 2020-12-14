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
                                 br(),
                                 p('Examine the water quality forecast for the day of the swimming event at Carvins Cove as it updates over time. 
                                           On each of the designated days, make a decision about how to manage the reservoir on each day of the forecast and 
                                           submit your answers below.'),
                                fluidRow(column(3,
                                                wellPanel(selectInput('forecast_day', 'Day in future', choices = forecast_dates$day_in_future),
                                                          checkboxInput('show_obs', 'Show Past Observations', value = FALSE),
                                                          numericInput('add_threshold', 'Add a threshold line to display', value = character(0)))),
                                         column(1),
                                         column(8,
                                                plotlyOutput('forecast_plot'))
                                         
                                  
                                ),     
                                br(),
                                fluidRow(
                                  h4('It is 16 days before the swimming event. Look at the forecasted water quality for the reservoir in the panel above and make a decision below'),
                                  column(6,
                                         textInput('day16_forecast_value', 'What is the mean forecasted concentration 16 days before the event?', placeholder = 'enter answer here')
                                                ),
                                  column(6,
                                         br(),
                                         conditionalPanel("input.day16_forecast_value!==''",
                                                          selectInput(inputId = "Decision_Day16", label = 'Decision 16 days before the event',
                                                                      choices = c("",'Continue with the swimming event as planned', 
                                                                                  'Cancel the event', 
                                                                                  'Perform a low cost treatment in the treatment plant after the water is extracted from the reservoir', 
                                                                                  'Perform a high cost water treatment action by adding chemicals directly into the reservoir' ),  
                                                                      width = "75%"))
                                         #valueBox('low' , 'Cost of Treatment', icon = icon("dollar-sign"))
                                         #valueBox('Poor', 'Outgoing Drinking Water Quality', icon = icon("faucet"), color = 'magenta')
                                         
                                         )
                                        
                                ),
                                fluidRow(column(6,
                                                conditionalPanel("input.Decision_Day16!==''",
                                                                 textInput('day10_forecast_value', 'What is the forecasted concentration 10 days before the event?', placeholder = 'enter answer here')
                                                )
                                                ),
                                         column(6,
                                                br(),
                                                conditionalPanel("input.day10_forecast_value!==''",
                                                                 selectInput(inputId = "Decision_Day10", label = 'Decision 10 days before the event',
                                                                             choices = c("",'Continue with the swimming event as planned', 
                                                                                         'Cancel the event', 
                                                                                         'Perform a low cost treatment in the treatment plant after the water is extracted from the reservoir', 
                                                                                         'Perform a high cost water treatment action by adding chemicals directly into the reservoir' ),  
                                                                             width = "100%"),
                                                                 conditionalPanel("input.Decision_Day10=='Perform a high cost water treatment action by adding chemicals directly into the reservoir'",
                                                                                  actionButton('update_forecast_day10', 'Update Forecast'))
                                                )
                                                )),
                                fluidRow(column(6,
                                                conditionalPanel("input.Decision_Day10!==''", 
                                                                 textInput('day7_forecast_value', 'What is the forecasted concentration 7 days before the event?', placeholder = 'enter answer here')
                                                )),
                                         column(6,
                                                br(),
                                                conditionalPanel("input.day7_forecast_value!==''", 
                                                                 selectInput(inputId = "Decision_Day7", label = 'Decision 7 days before the event',
                                                                             choices = c("",'Continue with the swimming event as planned', 
                                                                                         'Cancel the event', 
                                                                                         'Perform a low cost treatment in the treatment plant after the water is extracted from the reservoir', 
                                                                                         'Perform a high cost water treatment action by adding chemicals directly into the reservoir' ),  
                                                                             width = "100%"))
                                                )),
                                fluidRow(column(6,
                                                conditionalPanel("input.Decision_Day7!==''",
                                                                 textInput('day2_forecast_value', 'What is the forecasted concentration 2 days before the event?', placeholder = 'enter answer here')
                                                )
                                                ),
                                         column(6,
                                                br(),
                                                conditionalPanel("input.day2_forecast_value!==''",
                                                                 selectInput(inputId = "Decision_Day2", label = 'Decision 2 days before the event',
                                                                             choices = c("",'Continue with the swimming event as planned', 
                                                                                         'Cancel the event', 
                                                                                         'Perform a low cost treatment in the treatment plant after the water is extracted from the reservoir', 
                                                                                         'Perform a high cost water treatment action by adding chemicals directly into the reservoir' ),  
                                                                             width = "100%"))
                                         )
                                ),
                                        
                                h3("Once you're satisfied with your decisions, continue to Objective 5 to answer questions about your decision-making process")
                                        
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
             tabPanel(title = "Activity C: Discuss",
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
                                           column(5,
                                                  wellPanel(radioButtons('metric_raw', 'Select whether to represent uncertainty as a summarized value based on a metric or as the actual forecasted data', 
                                                               choices = c('metric', 'raw forecast output'), selected = character(0)),
                                                  conditionalPanel("input.metric_raw=='metric'",
                                                                   radioButtons('summ_comm_type', 'Select a communication type to represent your summarized uncertainty',
                                                                                choices = c('word', 'number', 'icon', 'figure'), selected = character(0))),
                                                  conditionalPanel("input.metric_raw=='raw forecast output'",
                                                                   radioButtons('raw_comm_type', 'Select a communication type to represent uncertainty in your raw forecast output',
                                                                                choices = c('word', 'number', 'figure'), selected = character(0))),
                                                  conditionalPanel("input.metric_raw=='metric' && input.summ_comm_type=='figure'",
                                                                   radioButtons('summ_plot_options', 'Select the plot type for a summarized metric', choices = c('pie', 'icon', 'time series', 'bar graph'), selected = character(0))),
                                                  conditionalPanel("input.metric_raw=='raw forecast output' && input.raw_comm_type=='figure'", radioButtons('raw_plot_options', 'Select the plot type for raw forecast output', choices = c('pie', 'time series', 'bar graph'), selected = character(0))),
                                                  actionButton('create_plot', 'Create Custom Plot'),
                                                  textInput('figure_title', 'Give your figure a title', placeholder = 'Enter title here', width = '80%'),
                                                  textInput('figure_caption', 'Give your figure a caption to help your stakeholder understand it', placeholder = 'Enter caption here', width = '80%')
                                                  #radioButtons('static_interactive', 'Select whether you want a static or interactive plot', choices = c('static', 'interactive'), selected = character(0)),
                                                  
                                           )),
                                           column(7,
                                                  plotOutput('custom_plot')
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

  forecast_data <- eventReactive(input$forecast_day, {
    forecast_id <- which(forecast_dates$day_in_future == input$forecast_day)
    forecast_file <- file.path("data", "wq_forecasts", forecast_dates[forecast_id,3])
    nc <- nc_open(forecast_file)
    t <- ncvar_get(nc,'time')
    local_tzone <- ncatt_get(nc, 0)$time_zone_of_simulation
    full_time_local <- as.POSIXct(t, origin = '1970-01-01 00:00.00 UTC', tz = local_tzone)
    full_time_day_local <- as_date(full_time_local)
    temp_mean <- ncvar_get(nc,'temp_mean') # rows are days, columns are depths
    temp_mean_1.6 <- temp_mean[,6]
    temp <- ncvar_get(nc,'temp')
    temp_upper <- ncvar_get(nc,'temp_upperCI')
    temp_lower  <- ncvar_get(nc,'temp_lowerCI')
    temp_upper_1.6 <- temp_upper[,6]
    temp_lower_1.6  <- temp_lower[,6]
    obs <- ncvar_get(nc, 'obs')
    obs_1.6 <- obs[,6]
    nc_close(nc)
    
    forecast <- data.frame('date' = full_time_day_local, 'temp_mean' = temp_mean_1.6, 'upper_CI' = temp_upper_1.6, 'lower_CI' = temp_lower_1.6, 
                           'obs' = obs_1.6, 'mean_post_sulfate' = temp_mean_1.6*0.1, 'upper_CI_post_sulfate' = temp_upper_1.6*0.1, 'lower_CI_post_sulfate' = temp_lower_1.6*0.1)
    forecast[forecast$date>=forecast_dates[forecast_id+1,2],5] <- 'NA'
    forecast$obs <- as.numeric(forecast$obs)
    return(forecast)
  })
  
  forecast_ens_data <- eventReactive(input$forecast_day,{
    forecast_id <- which(forecast_dates$day_in_future == input$forecast_day)
    forecast_file <- file.path("data", "wq_forecasts", forecast_dates[forecast_id,3])
    nc <- nc_open(forecast_file)
    t <- ncvar_get(nc,'time')
    local_tzone <- ncatt_get(nc, 0)$time_zone_of_simulation
    full_time_local <- as.POSIXct(t, origin = '1970-01-01 00:00.00 UTC', tz = local_tzone)
    full_time_day_local <- as_date(full_time_local)
    temp <- ncvar_get(nc,'temp')
    temp_1.6 <- temp[,6]
    obs <- ncvar_get(nc, 'obs')
    obs_1.6 <- obs[,6]
    nc_close(nc)
    
    #finish up by making a dataframe with the forecast ensembles by day
  })
  
#  forecast_plot <- eventReactive(input$forecast_day, {
#    forecast_id <- which(forecast_dates$day_in_future == input$forecast_day)
#    p <- ggplot(data = forecast_data(), aes(x = date, y = temp_mean)) + 
#      geom_line() +
#      geom_ribbon(aes(date, ymin = lower_CI, ymax = upper_CI, fill = '95th', alpha = 0.4)) +
#      geom_vline(xintercept = as.Date(forecast_dates[forecast_id,2])) +
#      geom_text(aes(as.Date(forecast_dates[forecast_id,2])-1, y = 27.5), label = 'past') +
#      geom_text(aes(as.Date(forecast_dates[forecast_id,2])+1, y = 27.5), label = 'future') +
#      geom_vline(xintercept = as.Date(date_of_event), color = 'red') +
#      geom_text(aes(as.Date(date_of_event)-1.1, y = 27.5), color = 'red', label = 'Day of Event') +
#      ylab('Chlorophyll-a (µg/L)') + 
#      xlab("Date") +
#      theme(panel.grid.major = element_blank(),
#            legend.position = 'none') 
#    if(input$show_obs){
#      p <- p + geom_point(aes(x = date, y = obs, color = 'red'), na.rm = TRUE) 
#
#    }
#    if(!is.na(input$add_threshold)){
#      p <- p + geom_hline(yintercept = input$add_threshold)
#    }
#    return(p)
#  })
#  

    output$forecast_plot <- renderPlotly({ 
      forecast_id <- which(forecast_dates$day_in_future == input$forecast_day)
      print(str(as.Date(forecast_dates[forecast_id, 2])))
      print(str(forecast_data()))
      print(str(forecast_dates))
      p <- ggplot(data = forecast_data(), aes(x = as.Date(date), y = temp_mean)) + 
        geom_line() +
        geom_ribbon(aes(date, ymin = lower_CI, ymax = upper_CI, fill = '95th', alpha = 0.4)) +
        geom_vline(xintercept = as.Date(forecast_dates[forecast_id, 2])) +
        geom_text(aes(as.Date(forecast_dates[forecast_id,2])-1, y = 27.5), label = 'past') +
        geom_text(aes(as.Date(forecast_dates[forecast_id,2])+1, y = 27.5), label = 'future') +
        geom_vline(xintercept = as.Date(date_of_event), color = 'red') +
        geom_text(aes(as.Date(date_of_event)-1.1, y = 27.5), color = 'red', label = 'Day of Event') +
        ylab('Chlorophyll-a (µg/L)') + 
        xlab("Date") +
        theme_minimal(base_size = 16) +
        theme(panel.background = element_rect(fill = NA, color = 'black'),
              panel.border = element_rect(color = 'black', fill = NA),
              legend.position = 'none')
        #theme(panel.grid.major = element_blank(),
        #      legend.position = 'none') 
      if(input$show_obs){
        p <- p + geom_point(aes(x = date, y = obs, color = 'red'), na.rm = TRUE) 
        
      }
      if(!is.na(input$add_threshold)){
        p <- p + geom_hline(yintercept = input$add_threshold)
      }
      return(ggplotly(p))
   })
    
output$forecast_plot_updated <- renderPlotly({
  input$update_forecast
  forecast_id <- which(forecast_dates$day_in_future == input$forecast_day)
  j <- ggplot(data = forecast_data(), aes(x = as.Date(date), y = temp_mean)) + 
    geom_line() +
    geom_ribbon(aes(date, ymin = lower_CI, ymax = upper_CI, fill = '95th', alpha = 0.4)) +
    geom_line(aes(x = as.Date(date), y = mean_post_sulfate)) +
    geom_ribbon(aes(x = date, ymin = lower_CI_post_sulfate, ymax = upper_CI_post_sulfate, alpha = 0.4)) +
    geom_vline(xintercept = as.Date(forecast_dates[forecast_id, 2])) +
    geom_text(aes(as.Date(forecast_dates[forecast_id,2])-1, y = 27.5), label = 'past') +
    geom_text(aes(as.Date(forecast_dates[forecast_id,2])+1, y = 27.5), label = 'future') +
    geom_vline(xintercept = as.Date(date_of_event), color = 'red') +
    geom_text(aes(as.Date(date_of_event)-1.1, y = 27.5), color = 'red', label = 'Day of Event') +
    ylab('Chlorophyll-a (µg/L)') + 
    xlab("Date") +
    theme_minimal(base_size = 16) +
    theme(panel.background = element_rect(fill = NA, color = 'black'),
          panel.border = element_rect(color = 'black', fill = NA),
          legend.position = 'none')
  return(ggplotly(j))
  
  
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
  
  
  output$custom_plot <- renderPlot({
    if(input$create_plot){
      if(input$summ_plot_options=='pie'){
        hist(rnorm(100), main = 'pie')
      }else if(input$summ_plot_options=='icon'){
        hist(rnorm(7), main = 'icon')
      }else if(input$summ_plot_options=='time series'){
        hist(rnorm(7), main = 'time series')
      }else if(input$summ_plot_options=='bar graph'){
        hist(rnorm(7), main = 'bar graph')
      }else if(input$raw_plot_options=='pie'){
        hist(rnorm(3), main = 'pie')
      }else if(input$raw_plot_options=='time series'){
        hist(rnorm(32), main = 'time series')
      }else if(input$raw_plot_options=='bar graph'){
        hist(rnorm(3), main = 'bar graph')
      }
      
    }
      
  })
  

}





shinyApp(ui = ui, server = server)
