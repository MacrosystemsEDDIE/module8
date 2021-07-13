library(shiny)
library(shinycssloaders)
library(shinyjs)
library(leaflet)
library(htmltools)
library(ggplot2)
library(plotly)
library(reshape)
library(sortable)
library(slickR)
library(tinytex)
library(lubridate)
library(shinyWidgets)
library(shinydashboard)
library(tidyverse)
library(matrixStats)
#install.packages('googlesheets4')
library(googlesheets4)
#install.packages('rintrojs')
library(rintrojs)
#install.packages('shinyBS')
library(shinyBS)
#install.packages('shinyalert')
library(shinyalert)
library(webshot)
library(htmlwidgets)

if (is.null(suppressMessages(webshot:::find_phantom()))) { webshot::install_phantomjs() }
#if (!webshot::is_phantomjs_installed()) webshot::install_phantomjs()


source("textAreaInput2.R")


## googlesheets authentication
#options(gargle_oauth_cache = ".secrets")
## check the value of the option, if you like
#gargle::gargle_oauth_cache()
## trigger auth on purpose to store a token in the specified cache
## a broswer will be opened
#googlesheets4::gs4_auth()
## see your token file in the cache, if you like
#list.files(".secrets/")
## sheets reauth with specified token and email address
#sheets_auth(
#  cache = ".secrets",
#  email = "wwoelmer@vt.edu"
#)
#
# Options for Spinner
options(spinner.color="#0275D8", spinner.color.background="#ffffff", spinner.size=2)

# colors 
cols <- RColorBrewer::brewer.pal(8, "Dark2")
l.cols <- RColorBrewer::brewer.pal(8, "Set2")[-c(1, 2)]
pair.l.cols <- RColorBrewer::brewer.pal(8, "Paired")
nav_butt <- "#63BB92"
nav_txt <- "#000000"

# add last update time
app_time <- format(file.info("app.R")$mtime, "%Y-%m-%d")
app_update_txt <- paste0("This app was last updated on: ", app_time)

# colors for theme
obj_bg <- "#D4ECE1"
ques_bg <- "#B8E0CD"

# Load app input
module_text <- read.csv("data/module_text.csv", row.names = 1, header = FALSE)
EF_links <- read.csv("data/eco_forecast_examples.csv")
forecast_dates <- read.csv("data/forecast_dates.csv")
stakeholder_info <- read.csv("data/stakeholders.csv")
tab_names <- read.csv("data/tab_names.csv")
proact_answers <- read.csv("data/proact_answers.csv")
problem_answers <- "You must optimize multiple objectives when managing the reservoirs at a time when algal blooms are likely"
objective_answers <- c("Provide safe drinking water",
                       "Ensure swimmer safety",
                       "Maximize economic benefit",
                       "Protect ecological health")
alternative_answers <- c("Cancel the event",
                         "Continue with the event",
                         "Treat the reservoir with an algaecide")
consequence_answers <- c("Economic benefit is heavily decreased due to canceling the event",
                         "Decreased ecological health (e.g., death of aquatic organisms) due to algaecide treatment",
                         "Compromised drinking water quality due to lack of treatment during an algal bloom")
tradeoffs_answers <- c("Small loss of money due to cost of algaecide, but increased economic benefit to the city from the event",
               "Decrease in ecological health, but safe drinking water is ensured",
               "Swimmer safety is compromised, but economic benefit and ecological health remain high due to avoiding algaecide treatment")


mock_data <- read.csv('data/wq_forecasts/microcystin_mock_data.csv')
mock_data$date_forecast_made <- as.Date(mock_data$date_forecast_made)
mock_data$date_of_forecast <- as.Date(mock_data$date_of_forecast)

# Define vectors
forecast_descriptions <- c("", 'There is no chance of water quality degradation on June 6',
  'There is a chance that the water quality will be dangerous to swimmers (>35 \U00B5g/L) on June 6',
  'It is more likely that the algal concentration will be below 25 \U00B5g/L than it is that it will be above 25 \U00B5g/L',
  'The likelihood of an algal bloom (>25 \U00B5g/L) on June 6 is low')
decision_options <- c('', 'Casual user', "Practitioner", 'Decision analyst')
decision_objectives <- c('Drinking water quality', 'Ecological health', 'Economic benefit', 'Swimmer safety')
objective_colors <- c("#335AA6", "#84B082", "#E75A7C","#F6BD60")
mgmt_choices <- c('A) Continue with the swimming event as planned', 
                  'B) Cancel the event', 
                  'C) Treat the reservoir with an algaecide')

# define the date of the swimming event (Activity B)
date_of_event <- as.Date('2021-06-06')

# load forecast data
# data for obj 4a
source('create_forecast_data.R')
day14_orig <-  read.csv("data/wq_forecasts/forecast_day14.csv") 
day14_orig$date <- as.Date(day14_orig$date)
day10_orig <-  read.csv("data/wq_forecasts/forecast_day10.csv") 
day10_orig$date <- as.Date(day10_orig$date)
day7_orig <-  read.csv("data/wq_forecasts/forecast_day7.csv") 
day7_orig$date <- as.Date(day7_orig$date)
day2_orig <-  read.csv("data/wq_forecasts/forecast_day2.csv")
day2_orig$date <- as.Date(day2_orig$date)
obs_data <- read.csv("data/wq_forecasts/mock_chl_obs.csv")
obs_data$date <- as.Date(obs_data$date)
treat_data <- read.csv("data/wq_forecasts/mock_chl_obs.csv")
treat_data$date <- as.Date(treat_data$date)

# and for dataset for obj 4b with uncertainty
source('create_forecast_data_UC.R')
day14_orig_UC <-  read.csv("data/wq_forecasts/forecast_day14_UC.csv") 
day14_orig_UC$date <- as.Date(day14_orig_UC$date)
day10_orig_UC <-  read.csv("data/wq_forecasts/forecast_day10_UC.csv") 
day10_orig_UC$date <- as.Date(day10_orig_UC$date)
day7_orig_UC <-  read.csv("data/wq_forecasts/forecast_day7_UC.csv") 
day7_orig_UC$date <- as.Date(day7_orig_UC$date)
day2_orig_UC <-  read.csv("data/wq_forecasts/forecast_day2_UC.csv")
day2_orig_UC$date <- as.Date(day2_orig_UC$date)
obs_data_UC <- read.csv("data/wq_forecasts/mock_chl_obs_UC.csv")
obs_data_UC$date <- as.Date(obs_data_UC$date)
treat_data_UC <- read.csv("data/wq_forecasts/mock_chl_obs_UC.csv")
treat_data_UC$date <- as.Date(treat_data_UC$date)

# set treatment decrease factors
decrease_14 = sample(seq(0.6, 0.99, by = 0.01), 1)
decrease_10 = sample(seq(0.6, 0.99, by = 0.01), 1)
decrease_7 = sample(seq(0.6, 0.99, by = 0.01), 1)
decrease_2 = sample(seq(0.6, 0.99, by = 0.01), 1)

decrease_14_UC = sample(seq(0.6, 0.99, by = 0.01), 1)
decrease_10_UC = sample(seq(0.6, 0.99, by = 0.01), 1)
decrease_7_UC = sample(seq(0.6, 0.99, by = 0.01), 1)
decrease_2_UC = sample(seq(0.6, 0.99, by = 0.01), 1)

# decision dates
date_14 <- as.Date('2021-05-23')
date_10 <- as.Date('2021-05-27')
date_7 <- as.Date('2021-05-30')
date_2 <- as.Date('2021-06-04')

# objective dataframes
decision14 <- read.csv('data/scenario_objectives.csv')
decision10 <- read.csv('data/scenario_objectives.csv')
decision7 <- read.csv('data/scenario_objectives.csv')
decision2 <- read.csv('data/scenario_objectives.csv')



#user interface
ui <- tagList(
  tags$head(tags$link(rel = "shortcut icon", href = "macroeddi_ico_green.ico")), # Add icon for web bookmarks
  tags$head(includeHTML(("google-analytics.html"))),
  navbarPage(title = "Module 8: Using Ecological Forecasts to Guide Decision Making",
             position = "static-top",
             id = 'maintab',
             
             #useShinydashboard(),
             
             # Tab1: Module 8 Overview and Summary
             tabPanel(title = "Module Overview",
                      tags$style(type="text/css", "body {padding-top: 65px;}"),
                      value = 'mtab1',
                      img(src = "project-eddie-banner-2020_green.png", height = 100, 
                          width = 1544, top = 5),
                      #* Intro text ====
                      h2("Using Ecological Forecasts to Guide Decision Making", align = 'center'),
                      br(),
                      fluidRow(
                        column(6,
                          h2("Today's focal question:", align = 'center'),
                          h3("How can ecological forecasts and their visualizations aid in decision making?", align = 'center'),
                          h4('To answer this question, you will complete three activities:'),
                          br(),
                          tags$ul(
                             h4(tags$li("Activity A - Explore ecological forecast visualizations")),
                             tags$ul(style = "list-style-type: lower alpha;", 
                                     tags$li("Identify different ways to visualize a forecast"),
                                     tags$li("Recognize how uncertainty is represented (or not!) in forecast visualizations"),
                                     tags$li("Pair forecast visualizations with a stakeholder decision")),
                             h4(tags$li("Activity B - Make decisions using an ecological forecast")),
                             tags$ul(style = "list-style-type: lower alpha;", 
                                     tags$li("Match PrOACT components with a decision-making scenario"),
                                     tags$li("Make decisions using a forecast and balance multiple decision trade-offs"),
                                     tags$li("Discuss the implications of forecast visualizations on decision-making")),
                             h4(tags$li("Activity C - Create a customized visualization for a specific stakeholder")),
                             tags$ul(style = "list-style-type: lower alpha;", 
                                     tags$li("Explore forecast output which includes uncertainty"),
                                     tags$li("Create a customized visualization for a specific stakeholder based on their decision needs"),
                                     tags$li("Explain how your visualization choices match a specific stakeholder's decision needs")),
                        
                          ),
                          #h2("For more information about how to navigate the module activites, please proceed to the 'Workflow' tab.")
                          ),
                      column(6,
                             h2('Ecological Forecasting Cycle'),
                             img(src = "mod8_viz_v2_resize.png", 
                                 height = "80%",
                                 width = '80%',
                                 #align = 'center',
                                 #tags$style("border: solid 2px black;")
                                 )
                        
                      )),
                      hr(),
                      fluidRow(column(6, 
                             h3("Background on Ecological Forecasting and Decision-Making"),
                             p(module_text["eco_forecast", ]),
                             p(module_text["theme_mod8",])
                             ),
                      column(6,
                             h3("Learning Objectives"),
                             h4("By the end of this module, you will be able to:"),
                             tags$ul(
                               tags$li(module_text["LO1",]),
                               tags$li(module_text["LO2",]),
                               tags$li(module_text["LO3",]),
                               tags$li(module_text["LO4",]),
                               tags$li(module_text["LO5",]),
                               tags$li(module_text["LO6",]),
                               
                             )
                             )),
                      br(),
                      fluidRow(
                        column(4,
                          h3("Macrosystems EDDIE"),
                          p(module_text["Macro", ]),
                          p(HTML(paste0("For more information see the website ",a(href = "https://serc.carleton.edu/eddie/macrosystems/index.html", "here", target = "_blank"), ".")))
                        ),
                        column(4,
                               h3('Privacy Policy'),
                               p(id = "txt_j", module_text["privacy_policy", ], HTML(paste0("For information regarding assessment data, please visit our website ", a(href = "https://serc.carleton.edu/eddie/macrosystems/assessment", "here", target = "_blank"), "."))),
                        ),
                        column(4,
                               img(src = "MacroEDDIE Logo.png", height = "70%", 
                                   width = "70%", align = "center")
                               ))
                      ),
             # Tab2: Presentation
             tabPanel(title = 'Presentation',
                      tags$style(type="text/css", "body {padding-top: 65px;}"),
                      value = 'mtab2',
                      img(src = "project-eddie-banner-2020_green.png", height = 100, 
                          width = 1544, top = 5),
                      column(4,
                             br(),
                             br(),
                             p("The presentation accompanying this module covers an introduction to ecological forecasting, stakeholder decision
                               support, and uncertainty visualization."),
                             p("What is a forecast?"),
                             tags$ul(
                               tags$li("A forecast is a prediction of future conditions with uncertainty.")
                             ),
                             p("How can we include uncertainty in a forecast?"),
                             tags$ul(
                               tags$li("Forecast uncertainty is calculated by running many different forecasts
                                       with slightly different conditions.")
                             ),
                             p("Who uses a forecast?"),
                             tags$ul(
                               tags$li("Stakeholders can use a forecast. A stakeholder is anyone who can use a forecast to gain understanding or to make a decision.")
                             ),
                             
                             p("How can we communicate uncertainty in a forecast?"),
                             tags$ul(
                               tags$li("Forecasts can be communicated using forecast model output or translated into an index."),
                               tags$li("Forecast results can be communicated using words, numbers, icons, or figures.")
                               
                             ),
                             p('Click through the slides to recap some of the main points from the lecture.')
                             ),
                      column(8,
                             h2('Key Slides', align = 'center'),
                             wellPanel(slickROutput('Mod8_slides', width = "600px", height = "450px"))
                             )

                      ),
             # Tab3: Module Navigation ----
             tabPanel(title = 'Introduction',
                      tags$style(type="text/css", "body {padding-top: 65px;}"),
                      value = 'mtab3',
                      img(src = "project-eddie-banner-2020_green.png", height = 100, 
                          width = 1544, top = 5),
                      fluidRow(column(6,
                        h2('Workflow for this module'),
                        tags$ol(
                          tags$li(id = "txt_j", module_text["workflow1", ]),
                          tags$li(id = "txt_j", module_text["workflow2", ]),
                          tags$li(id = "txt_j", module_text["workflow3", ]),
                          tags$li(id = "txt_j", module_text["workflow4", ]),
                          tags$li(id = "txt_j", module_text["workflow5", ])
                          )
                        ),
                      column(6,
                             h2('Ecological Forecasting Cycle'),
                             img(src = "mod8_viz_v2_resize.png", 
                                 height = "80%",
                                 width = '80%',
                                 #align = 'center',
                                 tags$style("border: solid 2px black;")))),
                      hr(),
                      fluidRow(
                        column(6,
                          h3("Save your progress"),
                                  wellPanel(
                                    p("If you run out of time to finish all the activities you can save your progress and 
                                      return to it at a later date. Click the 'Download user input' button below and a file 
                                      'module8_answers_ID_number.eddie' will download. Store this file in a safe place locally
                                      on your computer."),
                                    tags$style(type="text/css", "#download_answers {background-color:#579277;color: white}"),
                                   # downloadButton("download_answers", label = "Download user input", class = "butt1"),
                                    
                                  ),
                            
                        ),
                        column(6,
                               h3("Generate Report"),
                               wellPanel(
                                 p("This will take the answers you have input into this app and generate a Microsoft Word document (.docx) document with your answers which you can download and make further edits before submitting. Return here when you have completed the module."),
                                 actionButton("generate", "Generate Report (.docx)", icon = icon("file"), width = "190px", class = "btn-primary"
                                              # id = "dl_btn", # This is the only button that shows up when the app is loaded
                                              # style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"
                                 ),
                                 br(),
                                 tags$style(type="text/css", "#download {background-color:#579277;color: white}"),
                                 conditionalPanel(condition = "output.reportbuilt", # This button appears after the report has been generated and is ready for download.
                                                  downloadButton("download", "Download Report", width = "60px", style = "width:190px;"
                                                                 # style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"
                                                  ))
                                 
                               ), 
                               br(), br(), br(),
                               )),
                      fluidRow(column(6,
                                      h3('Resume your progress'),
                                      wellPanel(
                                        p("To reload the app input from a previous session,
                                 you can upload the downloaded '.eddie' file below and it will populate your answers into the Shiny app."),
                                        fileInput("upload_answers", "Upload data", accept = c(".rds", ".eddie")), # B77C2C
                                        p(HTML(paste0(tags$b("Note:"), " You will need to remember which visualization you chose in Activity A,
                               Objective 1 and reselect this image. Additionally, your answers to Q14 in Objective 3 (PrOACT) will not reload into the app from 
                                             the '.eddie' file. You will need to re-answer this question."))),
                                        p("For the custom plot in Activity C, Objective 7, you will simply need to navigate to that objective and hit 
                               'Create custom Plot'. You should then see your plot reappear in Objective 7 and Objective 8"),
                                        p("Check the 'Questions still to be completed' section at right to see if any questions were not uploaded properly.")
                                      ),
                                      wellPanel(style = paste0("background: ", ques_bg),
                                                h2('Before you start...'),
                                                p('Input your name and Student ID. This information will be added to your final report.'),
                                                textInput(inputId = 'name', placeholder = "", label = 'Name', width = '40%'),
                                                textInput(inputId = 'id_number', placeholder = "", label = 'ID Number:', width = '40%'),
                                                #actionButton('submit', 'Submit')
                                      )),
                               column(6,
                                      h3('Questions still to be completed:'),
                                      wellPanel(
                                        h4('The questions listed here have not been completed within the app'),
                                        htmlOutput('check_list')
                                      ))),
                      ),
             
              # Tab3: Activity A ----
             tabPanel(title = "Activity A: Explore",
                      value = 'mtab4',
                      tags$style(type="text/css", "body {padding-top: 65px;}"),
                      img(src = "project-eddie-banner-2020_green.png", height = 100, 
                          width = 1544, top = 5),
                      h2("Activity A: Explore ecological forecast visualizations and decision-use"),
                      h4("Many of us use various types of forecasts in our daily lives to make decisions (e.g., weather forecasts). However, we often take for granted the way in
                         which the forecast is presented to us. In this activity, you will examine several ecological forecasts and analyze the visualizations they provide
                         as decision-support tools for their users."),
                     br(),
                      tabsetPanel(id = 'tabseries1',
                     
                       tabPanel('Objective 1',
                                value = 'taba1',
                                h4(tags$b("Objective 1: Explore how uncertainty is visualized in an ecological forecast")),
                                h4("Choose an ecological forecast visualization from the list of visualizations below. 
                                Spend a few minutes looking through all of the visualizations and then select one by clicking on
                                   the image. You should answer the questions below based on the image alone, but you can visit
                                   the website if you would like to learn more about the forecast."),
                                h4(tags$b("Make sure to coordinate with your partner so you select different forecast visualizations!")),
                                hr(),
                                h3("List of Ecological Forecast Visualizations"),
                                fluidRow(
                                  column(5,
                                       a(href = EF_links$webpage[1], EF_links$Forecast[1], target = "_blank", style = "font-size: 20px"), 
                                       br(),
                                       p(EF_links$About[1]), 
                                       #tags$b(p(EF_links$hint[1])),
                                       useShinyjs(),
                                       uiOutput('EF_1', click = 'EF_1_click'),
                                       tags$style('div#EF_1:hover {transform: scale(2);
                                                  transform-origin: top left;}'),
                                       br(),
                                       br(),
                                       a(href = EF_links$webpage[2], EF_links$Forecast[2], target = "_blank", style = "font-size: 20px"), 
                                       br(), 
                                       p(EF_links$About[2]), 
                                       #tags$b(p(EF_links$hint[2])), 
                                       useShinyjs(),
                                       uiOutput('EF_2'),
                                       tags$style('div#EF_2:hover {transform: scale(1.5);
                                                  transform-origin: top left;}'),
                                       br(),
                                       br(),
                                       a(href = EF_links$webpage[3], EF_links$Forecast[3], target = "_blank", style = "font-size: 20px"), 
                                       br(), 
                                       p(EF_links$About[3]), 
                                       #tags$b(p(EF_links$hint[3])), 
                                       uiOutput('EF_3'),
                                       tags$style('div#EF_3:hover {transform: scale(1.7);
                                                  transform-origin: top left;}'),
                                       br(),
                                       br(),
                                       a(href = EF_links$webpage[4], EF_links$Forecast[4], target = "_blank", style = "font-size: 20px"), 
                                       br(), 
                                       p(EF_links$About[4]), 
                                       #tags$b(p(EF_links$hint[4])), 
                                       uiOutput('EF_4'),
                                       tags$style('div#EF_4:hover {transform: scale(2);
                                                  transform-origin: top left;}'),
                                       br(),
                                ),
                                column(2,
                                ),
                                column(5,
                                       # removing EcoCaster for now bc images are not well suited to integration into app currently
                                       #a(href = EF_links$webpage[5], EF_links$Forecast[5], target = "_blank", style = "font-size: 20px"), 
                                       #br(), 
                                       #p(EF_links$About[5]), 
                                       #tags$b(p(EF_links$hint[5])), 
                                       #img(src = EF_links$logo_file[5], height = '20%', width = '10%'),
                                       #br(),
                                       #br(),
                                       a(href = EF_links$webpage[6], EF_links$Forecast[6], target = "_blank", style = "font-size: 20px"), 
                                       br(), 
                                       p(EF_links$About[6]), 
                                       #tags$b(p(EF_links$hint[6])), 
                                       uiOutput('EF_6'),
                                       tags$style('div#EF_6:hover {transform: scale(2);
                                                  transform-origin: top left;}'),
                                       br(),
                                       br(),
                                       a(href = EF_links$webpage[7], EF_links$Forecast[7], target = "_blank", style = "font-size: 20px"), 
                                       br(), p(EF_links$About[7]), 
                                       #tags$b(p(EF_links$hint[7])), 
                                       uiOutput('EF_7'),
                                       tags$style('div#EF_7:hover {transform: scale(1.4);
                                                  transform-origin: top left;}'),
                                       br(),
                                       br(),
                                       a(href = EF_links$webpage[8], EF_links$Forecast[8], target = "_blank", style = "font-size: 20px"), 
                                       br(), 
                                       p(EF_links$About[8]), 
                                       #tags$b(p(EF_links$hint[8])), 
                                       uiOutput('EF_8'),
                                       tags$style('div#EF_8:hover {transform: scale(2);
                                                  transform-origin: top left;}'),
                                       br(),
                                       br(),
                                       a(href = EF_links$webpage[9], EF_links$Forecast[9], target = "_blank", style = "font-size: 20px"), 
                                       br(), 
                                       p(EF_links$About[9]), 
                                       #tags$b(p(EF_links$hint[9])), 
                                       uiOutput('EF_9'),
                                       tags$style('div#EF_9:hover {transform: scale(2);
                                                  transform-origin: top left;}'),
                                       br(),
                                       br(),
                                       a(href = EF_links$webpage[10], EF_links$Forecast[10], target = "_blank", style = "font-size: 20px"), 
                                       br(), 
                                       p(EF_links$About[10]), 
                                       #tags$b(p(EF_links$hint[9])), 
                                       uiOutput('EF_10'),
                                       tags$style('div#EF_10:hover {transform: scale(2);
                                                  transform-origin: top left;}'),
                                )),
                                hr(),
                                fluidRow(h3('Using the image you have selected, answer the following questions')),
                                fluidRow(
                                  column(6,
                                         h4("Your visualization:"),
                                         wellPanel(imageOutput('forecast_image'),
                                                   p("If the image is still too small, right click to open it in a new window."),
                                                   style = "border: 1px double black;")
                                         ),
                                  column(6,
                                         )
                                  ),
                                wellPanel(style = paste0("background: ", ques_bg),
                                          h4(tags$b("NOTE: You should answer the following questions using your selected visualization,
                                          rather than visiting the website.")),
                                          hr(),
                                  fluidRow(tags$ul(column(6,
                                  textInput(inputId = "q1", label = paste0('Q1. ', module_text["activityA_Q1",]),
                                            placeholder = "", width = "60%"),
                                  textInput(inputId = "q2", label = paste0('Q2. ', module_text["activityA_Q2",]),
                                            placeholder = "", width = "60%"),
                                  radioButtons(inputId = "q3", label = paste0('Q3. ', module_text["activityA_Q3",]),
                                            choices = c("Yes", "No"), width = "60%", selected =character(0)),
                                  radioButtons(inputId = "q4", label = paste0('Q4. ', module_text["activityA_Q4",]),
                                               choices = c('Forecast output', 'Forecast index'), selected = character(0))
                                  ),
                                  column(6,
                                         textAreaInput2(inputId = "q5", label = paste0('Q5. ', module_text["activityA_Q5",]),
                                                   placeholder = "", width = "60%"),
                                         textAreaInput2(inputId = "q6", label = paste0("Q6. ", module_text["activityA_Q6",]),
                                                   placeholder = "", width = "60%"),
                                         selectInput(inputId = "q7", label = paste0("Q7. ", module_text["activityA_Q7",]),
                                                     choices = decision_options, width = "60%"))
                                  ))

                                    #column(4, textInput(inputId = "q8_A", label = paste0("Q8. ", module_text["activityA_Q8",]),
                                    #                    placeholder = "", width = "80%"))
                                    #   
                                  ),
                                  
                                
                       ),
                       tabPanel('Objective 2',
                                value = 'taba2',
                                h4(tags$b("Objective 2: Compare forecast visualizations across forecasting systems")),
                                br(),
                                h4("With your partner, compare forecasting systems and visualizations. 
                                Discuss the following questions regarding the ecological forecasting systems you explored."),
                                fluidRow(
                                  column(6,
                                         h4("Your visualization:"),
                                         wellPanel(imageOutput('forecast_image_second_time'),
                                                   style = "border: 1px double black;")
                                  ),
                                  column(6,
                                         h4("Your partner's visualization:"),
                                         wellPanel(
                                           selectInput(inputId = 'partner_image', 'Select the forecasting system your partner analyzed:',
                                                       choices = c("",
                                                                   'USA-NPN Pheno Forecast',
                                                                   'Smart & Connected Water Systems',
                                                                   'EcoCast',
                                                                   'Atlantic Sturgeon Risk of Encounter',
                                                                   'Naturecast Phenology Forecasts',
                                                                   'Portal Forecast',
                                                                   'Coral Reef Watch',
                                                                   'GrassCast',
                                                                   'Phenology Monitoring at the Morton Aboretum')
                                                       ),
                                           imageOutput('forecast_image_2'),
                                                   style = "border: 1px double black;")
                                  )
                                ),
                                hr(),
                                h4('Comparing the two visualizations, answer the following questions.'),
                                br(),
                                wellPanel(style = paste0("background: ", ques_bg),
                                  fluidRow(tags$ul(column(6,
                                                 textAreaInput2(inputId = "q8", label = paste0("Q8. ", module_text["activityA_obj2_Q9",]),
                                                           placeholder = "", width = "60%"),
                                                 textInput(inputId = "q9", label = paste0("Q9. ",module_text["activityA_obj2_Q10",]),
                                                           placeholder = "", width = "60%"),
                                                 radioButtons(inputId = "q10", label = paste0("Q10. ",module_text["activityA_obj2_Q11",]),
                                                              choices = c('Mine', "My partner's", 'Both', 'Neither'), selected = character(0), width = "60%")
                                                 ),
                                          column(6,
                                                 radioButtons(inputId = "q11", label = paste0("Q11. ",module_text["activityA_obj2_Q12",]),
                                                              choices = c('Forecast output', 'Forecast index'), selected = character(0),  width = "60%"),
                                                 textInput(inputId = "q12", label = paste0("Q12. ",module_text["activityA_obj2_Q13",]),
                                                           placeholder = "", width = "60%"),
                                                 textAreaInput2(inputId = "q13", label = paste0("Q13. ",module_text["activityA_obj2_Q14",]),
                                                          placeholder = "", width = "60%")
                                                 )
                                  
                                 )))
                         
                               
                       
                       )
                     ),
                    ),
                            
                    
             # Tab4: Activity B ----
             tabPanel(title = "Activity B: Decide",
                      value = 'mtab5',
                      tags$style(type="text/css", "body {padding-top: 65px;}"),
                      img(src = "project-eddie-banner-2020_green.png", height = 100, 
                          width = 1544, top = 5),
                      h2("Activity B: Make decisions using an ecological forecast"),
                      h4("Ecological forecasts have vast potential for aiding decision-making for a range of different stakeholders, 
                         yet forecast results may be challenging to understand because they inherently are associated with uncertainty 
                         in alternate future outcomes which have not yet occurred. This activity will ask you to make multiple decisions 
                         to optimize future drinking water quality. Forecasts will update through time, allowing you to see how forecast uncertainty 
                         changes over time, and how management decisions can impact water quality."),
                      tabsetPanel(id = 'tabseries2',
                        tabPanel('Scenario',
                                 value = 'tabb1',
                                 br(),
                                 fluidRow(align = 'center',
                                          img(src = 'ccr2.png', #
                                              width = '75%')
                                 ),
                                 br(),
                                 br(),
                                 h4(tags$b('Read the following scenario and use it to complete Objectives 3-5:'), align = 'center'),
                                 fluidRow(column(2,
                                                 ),
                                          column(8,
                                                 h4(tags$b('Scenario:')),
                                                 p(module_text["activityB_scenario1",]),
                                                 br(),
                                                 p(module_text["activityB_scenario2",]),
                                                 br(),
                                                 p(module_text["activityB_scenario3",]),
                                                 hr(),
                                                 h4(tags$b('Each day as you look at the forecast you must look at your Objectives Monitor, which shows 
                                                 the relative trade-offs between your four objectives on the day you are making your decision.
                                                 While making your decisions, you should try to keep all of them as high as possible.')),
                                                 h4('Examples of Objective Monitors with different decision alternatives and 
                                                    corresponding trade-offs are shown below:'),
                                                 ),
                                          column(2,
                                                 )
                                          ),
                                 fluidRow(column(4,
                                                 h4('A) Continue with the swimming event as planned'),
                                                 h5('If you choose this option, drinking water quality, ecological health, and swimmer safety
                                                 may decrease if there is an algal bloom, but economic benefit to the water utility is optimized.'),
                                                 plotOutput('decision_a')

                                                 #  tags$ol(tags$li('Continue with the swimming event as planned'),
                                                 #         tags$li('Cancel the swimming event'),
                                                 #         tags$li('Perform a low cost treatment in the treatment plant after the water is extracted from the reservoir. This would make the water safe for drinking but does not alter the water quality in the reservoir'),
                                                 #         tags$li('Perform a high cost water treatment action by adding chemicals directly into the reservoir. This would make the reservoir safe for both swimming and drinking, but would have negative ecological effects on the aquatic life in the reservoir')),
                                                 
                                                 ),
                                          column(4,
                                                 h4('B) Cancel the event'),
                                                 h5('If you choose this option, drinking water and ecological health may decrease 
                                                 if there is an algal bloom, and economic benefit is highly decreased due to canceling 
                                                 the event, but swimmer safety is not compromised.'),
                                                 br(),
                                                 plotOutput('decision_b')
                                                 ),
                                          column(4,
                                                 h4('C) Treat the reservoir with an algaecide'),
                                                 h5('If you choose this option, you will ensure good drinking water quality,
                                                 but economic benefit will decrease due to purchasing chemicals, and
                                                 ecological health and swimmer safety may be decreased due to exposure to the algaecide.'),
                                                 plotOutput('decision_c')
                                                 )
                                          ),
                                 h3('Use these decision options to guide you in answering the questions in Objectives 3-5', align = 'center')
                                 
                                 ),
                        tabPanel('Objective 3',
                                 value = 'tabb2',
                                 h4(tags$b("Objective 3: Identify the components of the decision you need to make as a drinking water manager (PrOACT):")),
                                 br(),
                                 p(module_text["proact_intro",]),
                                 slickROutput('PrOACT', width = '50%', height = '50%'),
                               h4('Use the definitions and examples in the slides to help you answer the following question. Drag and drop
                                  the answers from the answer bank to the appropriate category. In this example, we present only one objective, but there
                                  are many objectives to balance in reality.'),
                               fluidRow(
                                 wellPanel(style = paste0("background: ", ques_bg),
                                        h4("Q14. Drag the definitions from the box on the left to the corresponding boxes on the right. There may be more than
                                           one answer for some categories."),
                                        fluidRow(  
                                          column(12, bucket_list(
                                            header = "",
                                            group_name = "bucket_list_group",
                                            orientation = "horizontal",
                                            add_rank_list(
                                              text = tags$b("Drag from here"),
                                              labels = sample(proact_answers[,"answers_all"]),
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
                                              text = tags$b("Alternative Decisions"),
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
                                          ))),
                                        actionButton('ans_btn', 'Check answers'),
                                        textOutput("pr_ans"),
                                        textOutput('obj_ans'),
                                        textOutput('alt_ans'),
                                        textOutput('cons_ans'),
                                        textOutput('trof_ans'))
                               ),
                               
                              
                                 
                          #      textInput(inputId = "activityb_obj3_q1", label = module_text["activityB_obj3_Q1",],
                          #                placeholder = "", width = "80%"),
                          #      textInput(inputId = "activityb_obj3_q2", label = module_text["activityB_obj3_Q2",],
                          #                placeholder = "", width = "80%")
                                 
                        ),
                        tabPanel('Objective 4a',
                                 value = 'tabb3',
                                   h4(tags$b('Objective 4a: Decide how to manage a drinking water reservoir using an ecological forecast')),
                                # p("Between your partner, choose one of you to be in Group A and one to be in Group B. Both of you will have to decide whether to proceed with the swimming event based on
                                # the water quality forecast. However, students in Group A will see different visualizations than students in Group B. 
                                # You will then discuss your choices and how they were influenced by the visualizations in Objective 5."),
                                # p(tags$b("You will be unable to change your selection after you pick one below, so make sure you discuss with your partner
                                #          who will chose what!")), 
                                # br(),
                                 #radioButtons('student_group', label = 'Are you in Group A or B?', choices = c('A', 'B'), selected = character(0)),
                                # actionButton('choose_group', 'Submit Group Choice'),
                                 fluidRow(
                                   column(1,
                                          ),
                                   column(6,
                                          h4('You now have access to the 14-day water quality forecast leading up to the day of the swimming event, June 6. 
                                 Every day as time gets closer to the swimming competition, the forecast will update with new data, 
                                 allowing you to update your decision. On each of the designated days, you must make  a decision 
                                 about whether to A) Continue with the swimming event as planned, B) Cancel the event, or C) Treat the reservoir with an algaecide.
                                 Submit your answers below. Remember that the forecast includes 25 different ensemble members, 
                                 which are different forecast estimates, and what you are seeing here is the mean and 95% confidence interval
                                             of those ensembles.'),
                                          br(),
                                          h4("As you make your decisions, remember that water becomes dangerous for drinking when the chlorophyll-a concentration goes above 25 \U00B5g/L
                                  and dangerous for swimming when the chlorophyll-a concentration goes above 35 \U00B5g/L. "),   #You can display these thresholds dynamically on the figures by changing the 'Display threshold line' value.
                                          br(),
                                          br(),
                                          br(),
                                          br(),
                                        
                                          ),
                                   column(1,
                                          ),
                                   column(4,
                                          h4('This is your Objective Monitor. Each day as you make a decision, this plot will show
                                             the relative trade-offs between your four objectives on the day you are making your decision.
                                             You want to keep your all objectives 
                                             as close to 100% as possible.'),
                                                        h4("Today's Objectives", align = 'center'),
                                                        plotOutput('tradeoff_plot_optim'),
                                      
                                      
                                         
                                 )
                                 ),
                 # Day 14 decision
                 ## forecast output
                 
                 fluidRow(style = "border: 4px double black;",
                          column(3,
                                 h4(tags$b('Days Before the Event: 14')),
                                 wellPanel(style = paste0("background: ", ques_bg),
                                           textInput('day14_forecast_value', 'What is the mean forecasted concentration for June 6 in the 14-day forecast?', placeholder = 'Hover mouse over figure to answer questions', width = '100%'),
                                           selectInput('day14_obj4a_choose', label = 'Choose the best description of the forecast on June 6 from the following options',
                                                       choices = forecast_descriptions,
                                                       selected = "", width = '100%'),
                                           radioButtons(inputId = "Decision_Day14_UC", label = 'Decision 14 days before the event', selected = character(0),
                                                        choices = mgmt_choices,  
                                                        width = "100%"))),
                          column(6,
                                 br(),
                                 h4('Forecast'),
                                 plotlyOutput('forecast_plot_14_withUC')),
                          column(3,
                                 h4("Today's Objectives", align = 'center'),
                                 plotOutput('tradeoff_plot_14_withUC'))
                 ),
                 fluidRow(style = "border: 4px double black;",
                          column(3,
                                 h4(tags$b('Days Before the Event: 10')),
                                 wellPanel(style = paste0("background: ", ques_bg),
                                           textInput('day10_forecast_value', 'What is the mean forecasted concentration for June 6 in the 10-day forecast?', placeholder = 'Hover mouse over figure to answer questions', width = '100%'),
                                           radioButtons(inputId = "Decision_Day10_UC", label = 'Decision 10 days before the event', selected = character(0),
                                                        choices = mgmt_choices,  
                                                        width = "100%"))),
                          column(6,
                                 br(),
                                 h4('Forecast'),
                                 plotlyOutput('forecast_plot_10_withUC')),
                          column(3,
                                 h4("Today's Objectives", align = 'center'),
                                 plotOutput('tradeoff_plot_10_withUC'))
                 ),
                 fluidRow(style = "border: 4px double black;",
                          column(3,
                                 h4(tags$b('Days Before the Event: 7')),
                                 wellPanel(style = paste0("background: ", ques_bg),
                                           textInput('day7_forecast_value', 'What is the mean forecasted concentration for June 6 in the 7-day forecast?', placeholder = 'Hover mouse over figure to answer questions'),
                                           #numericInput('add_threshold_7_UC', 'Display threshold line', value = 35),
                                           #selectInput('day7_forecast_multiple_choice_UC', label = 'Choose the best description of the forecast on June 6 from the following options',
                                           #             choices = forecast_descriptions,
                                           #             selected = "", width = '100%'),
                                           radioButtons(inputId = "Decision_Day7_UC", label = 'Decision 7 days before the event', selected = character(0),
                                                        choices = mgmt_choices,  
                                                        width = "100%"))),
                          column(6,
                                 br(),
                                 h4('Forecast'),
                                 plotlyOutput('forecast_plot_7_withUC')),
                          column(3,
                                 h4("Today's Objectives", align = 'center'),
                                 plotOutput('tradeoff_plot_7_withUC'))
                 ),
                 fluidRow(style = "border: 4px double black;",
                          column(3,
                                 h4(tags$b('Days Before the Event: 2')),
                                 wellPanel(style = paste0("background: ", ques_bg),
                                           textInput('day2_forecast_value', 'What is the mean forecasted concentration for June 6 in the 2-day forecast?', placeholder = 'Hover mouse over figure to answer questions'),
                                           #numericInput('add_threshold_2_UC', 'Display threshold line', value = 35),
                                           #selectInput('day2_forecast_multiple_choice_UC', label = 'Choose the best description of the forecast on June 6 from the following options',
                                           #           choices = forecast_descriptions,
                                           #           selected = "", width = '100%'),
                                           radioButtons(inputId = "Decision_Day2_UC", label = 'Decision 2 days before the event', selected = character(0),
                                                        choices = mgmt_choices,  
                                                        width = "100%"),
                                           p("Once you've made your decisions, please select 'Save plot' under your objectives monitor
                                                   at right before proceeding to the next objective.")
                                 )),
                          column(6,
                                 br(),
                                 h4('Forecast'),
                                 plotlyOutput('forecast_plot_2_withUC')),
                          column(3,
                                 h4("Today's Objectives", align = 'center'),
                                 plotOutput('tradeoff_plot_2_withUC'),
                                 tags$style(type="text/css", "#save_obj4b_objectives {background-color:#63BB92;color: black}"),
                                 actionButton('save_obj4b_objectives', 'Save plot', icon = icon("save")),
                                 br(),
                                 br())
                 ), 
                                        
                                h3("Once you've made your decisions, continue to Objective 4b.")
                                        
                                 ),
                        tabPanel('Objective 4b',
                                 value = 'tabb4',
                                 h4(tags$b('Objective 4b: Decide how to manage a drinking water reservoir using an ecological forecast which shows uncertainty')),
                                 h4("Now, you will again make decisions about managing the reservoir over time, but this time you
                                             will use a different forecast visualization to make your decisions."),
                                 h4('Examine the 14-day water quality forecast as you approach the day of the swimming event, June 6. 
                                 The forecasts will update over time, allowing you to update your decision as the day gets closer. 
                                 On each of the designated days, make a decision about whether to cancel the swimming event or not and 
                                 submit your answers below.'),
                                 h5("Remember that water becomes dangerous for drinking when the chlorophyll-a concentration goes above 25 \U00B5g/L
                                  and dangerous for swimming when the chlorophyll-a concentration goes above 35 \U00B5g/L. "), #You can display these thresholds dynamically on the figures by changing the 'Display threshold line' value.
                                 ## forecast index
                                 
                                 fluidRow(style = "border: 4px double black;",
                                          column(3,
                                                 h4(tags$b('Days Before the Event: 14')),
                                                 wellPanel(style = paste0("background: ", ques_bg),
                                                           selectInput('day14_obj4b_choose', label = 'Choose the best description of the forecast on June 6 from the following options',
                                                                       choices = forecast_descriptions,
                                                                       selected = "", width = '100%'),
                                                           radioButtons(inputId = "Decision_Day14", label = 'Decision 14 days before the event', selected = character(0),
                                                                        choices = mgmt_choices,  
                                                                        width = "100%"))),
                                          useShinyalert(),
                                          column(6,
                                                 br(),
                                                 h4('Forecast'),
                                                 plotlyOutput('forecast_plot_14'),
                                                 
                                          ),
                                          column(3,
                                                 h4("Today's Objectives", align = 'center'),
                                                 plotOutput('tradeoff_plot_14'))
                                          
                                 )
                                 ,     
                                 br(),
                                 br(),
                                 # Day 10 decision
                                 fluidRow(style = "border: 4px double black;",
                                          column(3,
                                                 h4(tags$b('Days Before the Event: 10')),
                                                 wellPanel(style = paste0("background: ", ques_bg),
                                                           #numericInput('add_threshold_10', 'Display threshold line', value = 35),
                                                           radioButtons(inputId = "Decision_Day10", label = 'Decision 10 days before the event', selected = character(0),
                                                                        choices = mgmt_choices,  
                                                                        width = "100%"))),
                                          column(6,
                                                 br(),
                                                 h4('Forecast'),
                                                 plotlyOutput('forecast_plot_10')),
                                          column(3,
                                                 h4("Today's Objectives", align = 'center'),
                                                 plotOutput('tradeoff_plot_10'))
                                 ),     
                                 br(),
                                 br(),
                                 # Day 7 decision               
                                 fluidRow(style = "border: 4px double black;",
                                          column(3,
                                                 h4(tags$b('Days Before the Event: 7')),
                                                 wellPanel(style = paste0("background: ", ques_bg),
                                                           #numericInput('add_threshold_7', 'Change the threshold line', value = 35),
                                                           radioButtons(inputId = "Decision_Day7", label = 'Decision 7 days before the event',
                                                                        choices = mgmt_choices,  
                                                                        width = "100%", selected = character(0)))),
                                          column(6,
                                                 h4('Forecast'),
                                                 plotlyOutput('forecast_plot_7')  
                                          ),
                                          column(3,
                                                 h4("Today's Objectives", align = 'center'),
                                                 plotOutput('tradeoff_plot_7'))
                                 ),
                                 br(),
                                 # Day 2 decision
                                 fluidRow(style = "border: 4px double black;",
                                          column(3,
                                                 h4(tags$b('Days Before the Event: 2')),
                                                 wellPanel(style = paste0("background: ", ques_bg),
                                                           #numericInput('add_threshold_2', 'Change the threshold line', value = 35),
                                                           radioButtons(inputId = "Decision_Day2", label = 'Decision 2 days before the event',
                                                                        choices = mgmt_choices,  
                                                                        width = "100%", selected = character(0)),
                                                           p("Once you've made your decisions, please select 'Save plot' under your objectives monitor
                                                   at right before proceeding to the next objective.")
                                                 )),
                                          column(6,
                                                 conditionalPanel("input.Decision_Day7!==''",
                                                                  h4('Forecast'),
                                                                  plotlyOutput('forecast_plot_2'))
                                          ),
                                          column(3,
                                                 h4("Today's Objectives", align = 'center'),
                                                 plotOutput('tradeoff_plot_2'),
                                                 tags$style(type="text/css", "#save_obj4a_objectives {background-color:#63BB92;color: black}"),
                                                 actionButton('save_obj4a_objectives', 'Save plot', icon = icon("save")),
                                                 br(),
                                                 br())
                                 )
                              
                                                           
                                 ),

                        tabPanel('Objective 5',
                                 value = 'tabb5',
                                 h4(tags$b('Objective 5: Assess the impact of the forecast visualization on your decision-making')),
                                 h5('Use the plots below to answer the following questions about your experience making management decisions with different visualizations.'),
                                 br(),
                                fluidRow(
                                  column(5,  
                                        h4("Plot showing decisions over time", align = 'center'),
                                        plotlyOutput('WQ_decisions'),
                                        tags$style(type="text/css", "#save_decision_plot {background-color:#63BB92;color: black}"),
                                        actionButton("save_decision_plot", "Save plot", icon = icon("save"))),
                                 column(7,
                                        h4("Plot showing original forecasts from Objective 4a", align = 'center'),
                                        p("Vertical gray line indicates the day of the swimming event", align = 'center', style = "color:grey"),
                                        p('NOTE: You can add/remove items from the figure by clicking on them in the figure legend. Try it!', align = 'center'),
                                        plotlyOutput('forecast_final'))),
                                 wellPanel(style = paste0("background: ", ques_bg),
                                           fluidRow(
                                              column(6,
                                                     textInput(inputId = "q15", label = paste0("Q15. ", module_text["activityB_obj5_Q1",]),
                                                               placeholder = "Hover your mouse over the figure above to answer this question.", width = "80%"),     
                                                     # textInput(inputId = "activityb_obj5_q4", label = module_text["activityB_obj5_Q2",],
                                                     #                    placeholder = "", width = "80%"),
                                                     textAreaInput2(inputId = "q16", label = paste0("Q16. ", module_text["activityB_obj5_Q3",]),
                                                               placeholder = "Hover your mouse over the figure above to answer this question.", width = "80%"),     
                                                     textInput(inputId = "q17", label = paste0("Q17. ", module_text["activityB_obj5_Q4",]),
                                                               placeholder = "", width = "80%")     
                                                     # textInput(inputId = "activityb_obj5_q4", label = module_text["activityB_obj5_Q4",],
                                                     #          placeholder = "", width = "80%"),
                                                    
                                                     
                                                     ),
                                              column(6,
                                                     textAreaInput2(inputId = "q18", label = paste0("Q18. ", module_text["activityB_obj5_Q5",]),
                                                                    placeholder = "", width = "80%"),
                                                     textAreaInput2(inputId = "q19", label = paste0("Q19. ", module_text["activityB_obj5_Q10",]),
                                                                    placeholder = "", width = "80%"),
                                                     radioButtons(inputId = "q20", label = paste0("Q20. ", module_text["activityB_obj5_Q6",]),
                                                               choices = decision_objectives, selected = character(0), width = "80%"),
                                                     #textInput(inputId = "activityb_obj5_q7", label = paste0("Q19. ", module_text["activityB_obj5_Q7",]),
                                                    #           placeholder = "", width = "80%"),
                                                     radioButtons(inputId = 'q21', label = "Q21. Which visualization did you prefer as a drinking water manager?",
                                                                  choices = c('Objective 4a', 'Objective 4b'), selected = character(0))
                                                     #textInput(inputId = "activityb_obj5_q9", label = module_text["activityB_obj5_Q9",],
                                                     #          placeholder = "", width = "80%")
                                                     )
                                           )
                                 
                                 
                                 )
                        )
                      ),
                      
                        
             ),

             
             # Tab5: Activity C ----
             tabPanel(title = "Activity C: Customize",
                      value = 'mtab6',
                      tags$style(type="text/css", "body {padding-top: 65px;}"),
                      img(src = "project-eddie-banner-2020_green.png", height = 100, 
                          width = 1544, top = 5),
                      h2("Activity C: Create a customized visualization of an ecological forecast for a specific stakeholder"),
                      h4("Uncertainty is an inherently difficult concept to understand, and especially difficult to represent visually. 
                      There are many ways to represent uncertainty visually and it has been shown that different representations
                      can lead to different levels of comprehension of the actual scenario. Further, the best way to visualize uncertainty is likely to
                      vary between stakeholders, with some stakeholders needing more information than others in order to facilitate quick and accurate
                      decision-making. This activity will allow you to role-play as a specific stakeholder, identify that stakeholder's decision needs,
                      and create a forecast visualization of uncertainty tailored to that stakeholder."),
                      tabsetPanel(id = 'tabseries3',
                        tabPanel('Objective 6',
                                           value = 'tabc1',
                                           h4(tags$b("Objective 6: Identify a stakeholder and how they could use a water quality forecast for decision-making")),
                                           h4('We will now customize the water quality forecast from Activity B to inform other stakeholder decisions. 
                                           It is important to consider who will be using your forecast to make decisions, as this can impact they way in which you visualize uncertainty.'),
                                           br(),
                                           h4('Choose a stakeholder from the drop-down menu and answer the questions below.'),
                                           wellPanel(style = paste0("background: ", ques_bg),
                                            fluidRow(
   
                                             column(8,
                                                    selectInput('stakeholder', 'Choose a stakeholder', 
                                                                choices = c("", 'Swimmer', 'Fisher', 'Dog owner', 'Parent', 'Water scientist', 
                                                                'Local Policymaker', 'State Department of Environmental Quality Employee',  'Other'),# , 'drinking water manager'
                                                                            width = '40%'), #, 
                                                    conditionalPanel("input.stakeholder=='Other'",
                                                                     textInput(inputId = 'stakeholder_other', "Please provide a name and brief description of your stakeholder. Be creative!",
                                                                               width = '60%')),
                                                    textAreaInput2(inputId = 'q22', label = paste0("Q22. ", module_text["activityC_obj6_Q1",]),
                                                              width = '60%'),
                                                    selectInput("q23", label = paste0("Q23. ", module_text["activityC_obj6_Q23",]),
                                                                choices = decision_options, width = "60%" ),
                                                    #h5(tags$b('Q22. Identify the PrOACT components of the stakeholder decision you identified above')),
                                                    #textInput(inputId = "Problem_3", label = 'Problem(s)',
                                                    #          placeholder = "Enter a problem statement here", width = "60%"),
                                                    #textInput(inputId = "Objective_3", label = 'Objective(s)',
                                                    #          placeholder = "Enter objective(s) here", width = "60%"),
                                                    #textInput(inputId = "Alternative_3", label = 'Alternative(s)',
                                                    #          placeholder = "Enter alternative(s) here", width = "60%"),
                                                    #textInput(inputId = "Consequence_3", label = 'Consequence(s)',
                                                    #          placeholder = "Enter consequence(s) here", width = "60%"),
                                                    #textInput(inputId = "TradeOff_3", label = 'Trade Off(s)',
                                                    #          placeholder = "Enter trade off(s) here", width = "60%")
                                                    ),
                                             column(4,
                                                    htmlOutput('stakeholder_name'),
                                                    br(),
                                                    textOutput('stakeholder_text'),
                                                    br(),
                                                    imageOutput('stakeholder_pic')
                                                    
                                             )))),
                                  tabPanel('Objective 7',
                                           value = 'tabc2',
                                           h4(tags$b('Objective 7: Explore variability in the forecast output')),
                                           h4("Below is a data table of forecast output of algal concentrations which you used in Activity B. 
                                              In this activity, you will explore multiple ways of communicating this same forecast to create a customized 
                                              forecast visualization for your chosen stakeholder."),
                                           br(),
                                           h4(tags$b("First, let's explore the forecast output."),
                                              h4("Use the 'Select Calculation' button to calculate various statistics for
                                              one day of the forecast and input them into Q24-25.")),
                                          fluidRow(
                                           column(6, DT::dataTableOutput('fcast_table')),
                                           column(6, h3("Calculate statistics"),
                                                  selectInput('forecast_viz_date', label = 'Select a date', choices = seq.Date(as.Date('2021-05-24'), as.Date('2021-06-06'), by = 'day')),
                                                  selectInput("stat_calc", label = "Select calculation:", choices = c("Pick a summary statistic", 'mean', 'median', 'max', 'min', 'standard deviation')),
                                                  wellPanel(
                                                    span(textOutput('date_selected_calcs'), style = "font-size:20px"),
                                                    span(textOutput("out_stats"), style = "font-size:20px")),
                                                  h3('Choose one day and answer the following questions'),
                                                  wellPanel( style = paste0("background: ", ques_bg),
                                                             br(),
                                                  textInput('mean_ens', label = 'Q24. What is the mean concentration of all the forecasts?',
                                                            placeholder = 'Enter answer here', width = "60%"),
                                                  #textInput('median_ens', label = 'What is the median concentration of all the forecasts?',
                                                  #          placeholder = 'Enter answer here', width = "60%"),
                                                  textInput('min_ens', label = 'Q25. What is the minimum concentration of all the forecasts?',
                                                            placeholder = 'Enter answer here', width = "60%"),
                                                  textInput('max_ens', label = 'Q26. What is the maximum concentration of all the forecasts?',
                                                            placeholder = 'Enter answer here', width = "60%"),
                                                  textInput('q27', label = paste0('Q27. ', module_text["activityC_obj7_Q27",]),
                                                            width = '60%')
                                                  #textInput('sd_ens', label = 'What is the standard deviation of all the forecasts?',
                                                #            placeholder = 'Enter answer here', width = "60%")
                                                  ),
                                                  
                                                  
                                                  )),
                                    
                                       ),
                                  tabPanel('Objective 8',
                                           value = 'tabc3',
                                           h4(tags$b('Objective 8: Create a customized forecast visualization for your chosen stakeholder')),
                                           br(),
                                           h4(tags$b("Now that you are familiar with the forecast output from Objective 7, explore the following visualization options to make
                                             a customized visualization for your stakeholder. ")),
                                           h4("Remember to consider the decision needs of your stakeholder
                                                    as you choose from among the visualization options."),
                                           br(),
                                           br(),
                                           #imageOutput('stakeholder_pic_2'), 
                                           fluidRow(column(5,
                                                           wellPanel(style = paste0("background:", obj_bg), 
                                                                     htmlOutput('stakeholder_name_2'),
                                                                     textOutput('stakeholder_decision')),
                                                           wellPanel(style = paste0("background: ", ques_bg),
                                                                     radioButtons('index_raw', 'Select whether to represent uncertainty as a forecast index or as forecast output', 
                                                                                  choices = c('Forecast output', 'Forecast index'), selected = character(0)),
                                                                     conditionalPanel("input.index_raw=='Forecast index'",
                                                                                      radioButtons('summ_comm_type', 'Select a communication type to represent your summarized uncertainty',
                                                                                                   choices = c('Word', 'Number', 'Icon', 'Figure'), selected = character(0))),
                                                                     conditionalPanel("input.index_raw=='Forecast output'",
                                                                                      radioButtons('raw_comm_type', 'Select a communication type to represent uncertainty in your forecast output',
                                                                                                   choices = c('Number', 'Figure'), selected = character(0))),
                                                                     conditionalPanel("input.index_raw=='Forecast index' && input.summ_comm_type=='Figure'",
                                                                                      radioButtons('summ_plot_type', 'Select the plot type for a summarized index', 
                                                                                                   choices = c('Pie', 'Bar graph', 'Time series'), selected = character(0))),
                                                                     conditionalPanel("input.index_raw=='Forecast output' && input.raw_comm_type=='Figure'", 
                                                                                      radioButtons('raw_plot_type', 'Select the plot type for forecast output', 
                                                                                                   choices = c( 'Time series', 'Bar graph'), selected = character(0))),
                                                                     conditionalPanel("input.index_raw=='Forecast output' && input.raw_comm_type=='Figure' && input.raw_plot_type=='Time series'",
                                                                                      radioButtons('ts_line_type', 'Select how you want to visualize the forecast ensembles',
                                                                                                   choices = c('Line', 'Confidence Interval', 'Boxplot'), #
                                                                                                   selected = character(0))),
                                                                     textAreaInput2('figure_title', 'Give your figure a title', placeholder = 'Enter title here', width = '80%'),
                                                                     textAreaInput2('figure_caption', 'Give your figure a caption to help your stakeholder understand it', placeholder = 'Enter caption here', width = '80%'),
                                                                     actionButton('create_plot', 'Create Custom Plot'),
                                                                     tags$style(type="text/css", "#save_custom_plot {background-color:#63BB92;color: black}"),
                                                                     actionButton("save_custom_plot", "Save plot", icon = icon("save"))
                                                                     
                                                           )),
                                                    column(7,
                                                           conditionalPanel("input.summ_comm_type=='Icon'",
                                                                            plotlyOutput('custom_plotly')),
                                                           conditionalPanel("input.summ_comm_type!=='Icon'",
                                                                            plotOutput('custom_plot'))
                                                           
                                                    )),
                                           
                                           
                                           h4('Using your completed, customized visualization, answer the follow questions'),  
                                          # fluidRow(column(3,
                                          #                 p('You chose to use:'),
                                          #                 textOutput('raw_or_index'),
                                          #                 textOutput('raw_comm_out'),
                                          #                 textOutput('index_comm_out'),
                                          #                 textOutput('raw_plot_out'),
                                          #                 textOutput('index_plot_out'),
                                          #                 textOutput('raw_ts_out'),
                                          #                 textOutput('index_ts_out')),
                                          # column(6,
                                          #        conditionalPanel("input.summ_comm_type=='icon'",
                                          #                          plotlyOutput('custom_plotly_second_time')),
                                          #        conditionalPanel("input.summ_comm_type!=='icon'",
                                          #                         plotOutput('custom_plot_second_time')
                                          #                         #renderImage('custom_plot_img')
                                          #                         )
                                          #        ),
                                          # column(3,)),
                                          # 
                                           wellPanel(style = paste0("background: ", ques_bg),
                                             fluidRow(
                                                column(6,
                                                       textAreaInput2('q28', label = paste0("Q28. ", module_text["activityC_obj8_Q1",]), placeholder = 'Enter answer here', width = '60%'),
                                                       textAreaInput2('q29', label = paste0("Q29. ", module_text["activityC_obj8_Q2",]), placeholder = 'Enter answer here', width = '60%'),
                                                       textAreaInput2('q30', label = paste0("Q30. ", module_text["activityC_obj8_Q3",]), 
                                                                 placeholder = 'If you chose a word or number communication type, skip this question.', width = '60%'),
                                                ),
                                                column(6,
                                                       textAreaInput2('q31', label = paste0("Q31. ", module_text["activityC_obj8_Q5",]), placeholder = 'Enter answer here', width = '60%'),
                                                       selectInput('q32', label = paste0("Q32. ", module_text["activityC_obj8_Q6",]), 
                                                                   choices = decision_options, width = '60%'),
                                                       textAreaInput2('q33', label = paste0("Q33. ", module_text["activityC_obj8_Q8",]), placeholder = 'Enter answer here', width = '60%'))
                                             
                                             
                                           )),
                                           column(2),
                                           column(8,
                                                  conditionalPanel("input.q33!==''",
                                                                   wellPanel(h3("Well done! You have finished Module 8. Please return to the 'Introduction' tab, check the 'Questions still to be completed' list for any unanswered questions,
                                                                                and download your report!", align = 'center'),
                                                                             actionButton("return_intro", "Return to Introduction", icon = icon("home"))
                                                                   )
                                                                   
                                                                   )
                                                  
                                           ),
                                           column(2)
                                           )
                        
                                           
              
              
        )
    
  )
 ),
 # Tab navigation buttons ----
 br(), hr(),
 introBox(
   h4("Use the buttons below to navigate through the tabs", align = "center"),
   fluidRow(
     column(4, align = "center", 
            # wellPanel(
            style = paste0("background: ", obj_bg),
            br(),
            actionButton("prevBtn1", "< Previous", 
                         style = paste0("color: ", nav_txt, "; background-color: ", nav_butt, "; border-color: #00664B; padding:15px; font-size:22px;")),
            bsTooltip("prevBtn1", title = "Navigate to previous tab", placement = "left", trigger = "hover"),
            br(), br()
            # )
            
     ),
     column(4, align = 'center',
            style = paste0("background: ", obj_bg),
            br(),
            #tags$style(type="text/css", "#download_answers {background-color:#579277;color: white}"),
            downloadButton("download_answers", label = "Download user input", class = "butt1",
                           style = paste0("color: ", nav_txt, "; background-color: #68a388", "; border-color: #00664B; padding:20px; font-size:15px;")),
            br(), br()

            ),
     column(4, align = "center",
            # wellPanel(
            style = paste0("background: ", obj_bg),
            br(),
            actionButton("nextBtn1", "Next >",
                         style = paste0("color: ", nav_txt, "; background-color: ", nav_butt, "; border-color: #00664B; padding:15px; font-size:22px;")),
            bsTooltip("nextBtn1", title = "Navigate to next tab", placement = "right", trigger = "hover"),
            br(), br()
            # )
     )
   ), data.step = 3, data.intro = "	Or you can use these buttons at the bottom of the page to also navigate through the tabs.", 
   data.position = "right"
 ),
 hr(), 
 fluidRow(
   column(8, offset = 1,
          #p(module_text["acknowledgement", ], id = "ackn"),
          p(app_update_txt, id = "ackn")
   ),
 )
 
)

server <- function(input, output, session){
   
   mod8_slides <- list.files("www/Mod8_Slides_Shiny", pattern = "Slide", full.names = TRUE)
   
   output$Mod8_slides <- renderSlickR({
      slickR(mod8_slides)
   }) 
   
   output$EF_1 <- renderUI({
      tags$img(src = EF_links$logo_file[1], height = '80%', width = '50%')
      
   })
   output$EF_2 <- renderUI({
      tags$img(src = EF_links$logo_file[2], height = '80%', width = '50%')
      
   })
   output$EF_3 <- renderUI({
      tags$img(src = EF_links$logo_file[3], height = '60%', width = '30%')
      
   })
   output$EF_4 <- renderUI({
      tags$img(src = EF_links$logo_file[4], height = '80%', width = '50%')
      
   })
   #output$EF_5 <- renderUI({
   #   tags$img(src = EF_links$logo_file[5], height = '80%', width = '50%')
   #   
   #})
   output$EF_6 <- renderUI({
      tags$img(src = EF_links$logo_file[6], height = '80%', width = '50%')
      
   })
   output$EF_7 <- renderUI({
      tags$img(src = EF_links$logo_file[7], height = '80%', width = '50%')
      
   })
   output$EF_8 <- renderUI({
      tags$img(src = EF_links$logo_file[8], height = '80%', width = '50%')
      
   })
   output$EF_9 <- renderUI({
      tags$img(src = EF_links$logo_file[9], height = '80%', width = '50%')
      
   })
   output$EF_10 <- renderUI({
      tags$img(src = EF_links$logo_file[10], height = '80%', width = '50%')
      
   })
  
  image_selected_path <- reactiveValues(img = NA)
  shinyjs::onclick("EF_1",  image_selected_path$img <- 'USA-NPN Pheno Forecast')
  shinyjs::onclick("EF_2",  image_selected_path$img <- 'Smart & Connected Water Systems')
  shinyjs::onclick("EF_3",  image_selected_path$img <- 'EcoCast')
  shinyjs::onclick("EF_4",  image_selected_path$img <- 'Atlantic Sturgeon Risk of Encounter')
  shinyjs::onclick("EF_6",  image_selected_path$img <- 'Naturecast Phenology Forecasts')
  shinyjs::onclick("EF_7",  image_selected_path$img <- 'Portal Forecast')
  shinyjs::onclick("EF_8",  image_selected_path$img <- 'Coral Reef Watch')
  shinyjs::onclick("EF_9",  image_selected_path$img <- 'GrassCast')
  shinyjs::onclick("EF_10", image_selected_path$img <- 'Phenology Monitoring at the Morton Aboretum')

  observeEvent(image_selected_path$img, {
    req(!is.na(image_selected_path$img))
    # Show a modal when the button is pressed
    shinyalert("Congrats!", "You've selected a forecast image")
  })
  
  
  output$forecast_image <- renderImage({
    validate(need(!is.na(image_selected_path$img), "Please select a visualization by clicking one above"))
    #req(!is.na(image_selected_path$img))
     #print(image_selected_path$img)
     id_image <- which(EF_links$Forecast==image_selected_path$img)
     filename <-  file.path('www', EF_links$logo_file[id_image])
     list(src = filename, height = EF_links$height[id_image], width = EF_links$width[id_image]) #
  }, deleteFile = FALSE)
  
  file <- reactive({gsub("\\\\", "/", input$forecast_file$datapath)})
  
  output$forecast_image_second_time <- renderImage({
    req(!is.na(image_selected_path$img))
    #print(image_selected_path$img)
    id_image <- which(EF_links$Forecast==image_selected_path$img)
    filename <-  file.path('www', EF_links$logo_file[id_image])
    list(src = filename, height = EF_links$height[id_image], width = EF_links$width[id_image]) #
  }, deleteFile = FALSE)
  
  file_2 <- reactive({gsub("\\\\", "/", input$forecast_file_2$datapath)})
  
  output$forecast_image_2 <- renderImage({
    req(input$partner_image)
    id_image <- which(EF_links$Forecast==input$partner_image)
    filename <-  file.path('www', EF_links$logo_file[id_image])
    list(src = filename, height = EF_links$height[id_image], width = EF_links$width[id_image]) #
  }, deleteFile = FALSE)
  
  
  output$optimized_obj <- renderPlot({
    guage <- read.csv('data/scenario_objectives.csv')
    guage$objective <- factor(guage$objective, levels = decision_objectives)
    ggplot(data = guage[guage$decision=='none',], aes(objective, quantity, fill = objective)) + 
      geom_bar(stat = 'identity') +
      labs(title = 'Optimized Objectives') +
      scale_fill_manual(name = 'legend', 
                        values = c('Drinking water quality' = objective_colors[1], 
                                   'Ecological health' = objective_colors[2], 
                                   'Economic benefit' = objective_colors[3],
                                   'Swimmer safety' = objective_colors[4])) + ##FFB86F
      theme(legend.position = 'none',
            panel.background = element_rect(fill = NA, color = 'black'),
            panel.border = element_rect(color = 'black', fill = NA),
            plot.title = element_text(size = 15, hjust = 0.5),
            plot.caption = element_text(size = 15, hjust = 0))
    
    
  })
  
  output$decision_a <- renderPlot({
    guage <- read.csv('data/scenario_objectives_multi.csv')
    guage$objective <- factor(guage$objective, levels = decision_objectives)
    ggplot(data = guage[guage$decision=='a',], aes(objective, quantity, fill = objective)) + 
      geom_bar(stat = 'identity') +
      labs(title = 'Decision A') +
      scale_fill_manual(name = 'legend', 
                        values = c('Drinking water quality' = objective_colors[1], 
                                   'Ecological health' = objective_colors[2], 
                                   'Economic benefit' = objective_colors[3],
                                   'Swimmer safety' = objective_colors[4])) + ##FFB86F
      theme(legend.position = 'none',
            panel.background = element_rect(fill = NA, color = 'black'),
            panel.border = element_rect(color = 'black', fill = NA),
            plot.title = element_text(size = 15, hjust = 0.5),
            plot.caption = element_text(size = 15, hjust = 0),
            axis.text.x = element_text(angle = 25, size = 10, hjust = 1))
    
    
  })
  
  output$decision_b <- renderPlot({
    guage <- read.csv('data/scenario_objectives_multi.csv')
    guage$objective <- factor(guage$objective, levels = decision_objectives)
    ggplot(data = guage[guage$decision=='b',], aes(objective, quantity, fill = objective)) + 
      geom_bar(stat = 'identity') +
      labs(title = 'Decision B') +
      scale_fill_manual(name = 'legend', 
                        values = c('Drinking water quality' = objective_colors[1], 
                                   'Ecological health' = objective_colors[2], 
                                   'Economic benefit' = objective_colors[3],
                                   'Swimmer safety' = objective_colors[4])) + ##FFB86F
      theme(legend.position = 'none',
            panel.background = element_rect(fill = NA, color = 'black'),
            panel.border = element_rect(color = 'black', fill = NA),
            plot.title = element_text(size = 15, hjust = 0.5),
            plot.caption = element_text(size = 15, hjust = 0),
            axis.text.x = element_text(angle = 25, size = 10, hjust = 1))
    
    
  })
  
  output$decision_c <- renderPlot({
    guage <- read.csv('data/scenario_objectives_multi.csv')
    guage$objective <- factor(guage$objective, levels = decision_objectives)
    ggplot(data = guage[guage$decision=='c',], aes(objective, quantity, fill = objective)) + 
      geom_bar(stat = 'identity') +
      labs(title = 'Decision C') +
      scale_fill_manual(name = 'legend', 
                        values = c('Drinking water quality' = objective_colors[1], 
                                   'Ecological health' = objective_colors[2], 
                                   'Economic benefit' = objective_colors[3],
                                   'Swimmer safety' = objective_colors[4])) + ##FFB86F
      theme(legend.position = 'none',
            panel.background = element_rect(fill = NA, color = 'black'),
            panel.border = element_rect(color = 'black', fill = NA),
            plot.title = element_text(size = 15, hjust = 0.5),
            plot.caption = element_text(size = 15, hjust = 0),
            axis.text.x = element_text(angle = 25, size = 10, hjust = 1))
    
    
  })
  
  output$tradeoff_plot_optim <- renderPlot({
    guage <- read.csv('data/scenario_objectives.csv')
    guage$objective <- factor(guage$objective, levels = decision_objectives)
    ggplot(data = guage[guage$decision=='none',], aes(objective, quantity, fill = objective)) + 
      geom_bar(stat = 'identity') +
      scale_fill_manual(name = 'legend', 
                        values = c('Drinking water quality' = objective_colors[1], 
                                   'Ecological health' = objective_colors[2], 
                                   'Economic benefit' = objective_colors[3],
                                   'Swimmer safety' = objective_colors[4])) + ##FFB86F
      theme(legend.position = 'none',
            panel.background = element_rect(fill = NA, color = 'black'),
            panel.border = element_rect(color = 'black', fill = NA),
            plot.title = element_text(size = 15, hjust = 0.5),
            plot.caption = element_text(size = 15, hjust = 0),
            axis.text.x = element_text(angle = 25, size = 10, hjust = 1))
    
    
  })

  
  reactive_tradeoff_plot <- reactiveValues(plot14 = NULL, plot10 = NULL, plot7 = NULL, plot2 = NULL)
  objective_data <- reactiveValues(decision14 = decision14,
                                   decision10 = decision10,
                                   decision7 = decision7,
                                   decision2 = decision2 
                                   ) # previously set as NULL after day 14 and inherited from the previous day's objectives
  
  # disable subsequent radio buttons if a decision hasn't been made for previous days
  # objective 4a
  observe({
    if(is.null(input$Decision_Day14_UC)){
      disable("Decision_Day10_UC")
    }
  })
  
  observe({
    if(is.null(input$Decision_Day10_UC)){
      disable("Decision_Day7_UC")
    }
  })
  
  observe({
    if(is.null(input$Decision_Day7_UC)){
      disable("Decision_Day2_UC")
    }
  })
  
  
  # objective 4b
  observe({
    if(is.null(input$Decision_Day14)){
      disable("Decision_Day10")

    }
  })
  observe({
    if(is.null(input$Decision_Day10)) disable("Decision_Day7")

  })
  
  observe({
    if(is.null(input$Decision_Day7)){
      disable("Decision_Day2")
    }
  })
  
  observe({
    req(input$Decision_Day14)
    #guage <- read.csv('data/scenario_objectives.csv')
    guage <- objective_data$decision14
    guage$objective <- factor(guage$objective, levels = decision_objectives)
    treat_data <- fcast_data$data_treat
    obs_data <- fcast_data$data
    
    decision_date <- date_14
    idx <- which(obs_data$date == decision_date)
    
    wq_decrease <- NA
    # based on the obs chl
    if(treat_data$obs_chl_ugl[idx] > 25){
      wq_decrease <- 0.5
    }else if(treat_data$obs_chl_ugl[idx] > 35){
      wq_decrease <- 0.25
    }else if(treat_data$obs_chl_ugl[idx] < 25){
      wq_decrease <- 1
    }
    
    eco_decrease <- NA
    # based only on treatment
    if(input$Decision_Day14==mgmt_choices[3]){
      eco_decrease <- 0.5
    }else{
      eco_decrease <- 1
    }
    
    money_decrease <- NA
    # based on treatment or cancel
    if(input$Decision_Day14==mgmt_choices[3]){
      money_decrease <- 0.8
    }else if(input$Decision_Day14==mgmt_choices[2]){
      money_decrease <- 0.1
    }else{
      money_decrease <- 1
    }
    
    swim_treat_decrease <- NA
    # based on treatment and algal concentration
    if(input$Decision_Day14==mgmt_choices[3]){
      swim_treat_decrease <- 0.8
    }else{
      swim_treat_decrease <- 1
    }
    
    swim_algae_decrease <- NA
    # based on algal concentration
    if(obs_data$obs_chl_ugl[idx] > 25){
      swim_algae_decrease <- 0.3
    }else if(obs_data$obs_chl_ugl[idx] > 35){
      swim_algae_decrease <- 0.05
    }else if(obs_data$obs_chl_ugl[idx] < 25){
      swim_algae_decrease <- 1
    }
    
    #WQ
    guage[1,2] <- guage[1,2]*wq_decrease
    #eco health
    guage[2,2] <- guage[2,2]*eco_decrease
    #money
    guage[3,2] <- guage[3,2]*money_decrease
    #swimmers
    guage[4,2] <- guage[4,2]*swim_treat_decrease*swim_algae_decrease
    
    objective_data$decision10 <- guage
    
    
    #progress$set(message = "Updating Today's Objectives", 
    #             detail = "This may take a while. This window will disappear  
    #                 when it is downloaded.", value = 0.5)
    
    
    reactive_tradeoff_plot$plot14 <- ggplot(data = guage, aes(objective, quantity, fill = objective)) + 
      geom_bar(stat = 'identity') +
      # labs(title = 'Decision C') +
      ylim(0, 100) +
      xlab('Objectives') +
      ylab('Percent Optimized') +
      ylim(0, 100) +
      scale_fill_manual(name = 'legend', 
                      values = c('Drinking water quality' = objective_colors[1], 
                                 'Ecological health' = objective_colors[2], 
                                 'Economic benefit' = objective_colors[3],
                                 'Swimmer safety' = objective_colors[4])) + ##FFB86F
      theme(legend.position = 'none',
            panel.background = element_rect(fill = NA, color = 'black'),
            panel.border = element_rect(color = 'black', fill = NA),
            plot.title = element_text(size = 15, hjust = 0.5),
            plot.caption = element_text(size = 15, hjust = 0),
            axis.text.x = element_text(angle = 45, size = 10, hjust = 1))
  })
  
    observeEvent(input$Decision_Day14, {
      # Show a modal when the button is pressed
      if(input$Decision_Day14==mgmt_choices[2]){
        shinyalert("You've chosen to cancel the swimming event!", "You cannot undo this decision. Please scroll to the bottom of this page to save your Objectives Plot and proceed to the next objective.")
        
      }
      if(input$Decision_Day14==mgmt_choices[3]){
        shinyalert("You've chosen to treat the reservoir with an algaecide!", "This will decrease the algal concentration in the forecast, but it is not 100% effective")
        
      }
 
       })
  
  observe({
    req(input$Decision_Day10)
    guage <- objective_data$decision10
    guage[1,2] <- 100 # set drinking water to 100% to start, not dependent on previous decisions
    guage[4,2] <- 100 # set swimmer safety to 100% to start, will write in treatment dependencies
    guage$objective <- factor(guage$objective, levels = decision_objectives)
    treat_data <- fcast_data$data_treat
    obs_data <- fcast_data$data
    
    decision_date <- date_10
    idx <- which(obs_data$date == decision_date)
    
    wq_decrease <- NA
    # based on the obs chl
    if(input$Decision_Day10==mgmt_choices[3]){
      wq_decrease <- 1
    }else{
      if(treat_data$obs_chl_ugl[idx] > 25){
        wq_decrease <- 0.5
      }else if(treat_data$obs_chl_ugl[idx] > 35){
        wq_decrease <- 0.25
      }else if(treat_data$obs_chl_ugl[idx] < 25){
        wq_decrease <- 1
      }
      
    }
    
    eco_decrease <- NA
    # based only on treatment
    if(input$Decision_Day10==mgmt_choices[3]){
      eco_decrease <- 0.5
    }else{
      eco_decrease <- 1
    }
    
    money_decrease <- NA
    # based on treatment or cancel
    if(input$Decision_Day10==mgmt_choices[3]){
      money_decrease <- 0.8
    }else if(input$Decision_Day10==mgmt_choices[2]){
      money_decrease <- 0.1
    }else{
      money_decrease <- 1
    }
    
    
    
    swim_treat_decrease <- NA
    # based on treatment and algal concentration
    if(input$Decision_Day10==mgmt_choices[3]){
      swim_treat_decrease <- 0.8
    }else{
      swim_treat_decrease <- 1
    }
    
    swim_algae_decrease <- NA
    # based on algal concentration
    if(treat_data$obs_chl_ugl[idx] > 25){
      swim_algae_decrease <- 0.5
    }else if(treat_data$obs_chl_ugl[idx] > 35){
      swim_algae_decrease <- 0.05
    }else if(treat_data$obs_chl_ugl[idx] < 25){
      swim_algae_decrease <- 1
    }
    
    #WQ
    guage[1,2] <- guage[1,2]*wq_decrease
    #eco health
    guage[2,2] <- guage[2,2]*eco_decrease
    #money
    guage[3,2] <- guage[3,2]*money_decrease
    #swimmers
    guage[4,2] <- guage[4,2]*swim_treat_decrease*swim_algae_decrease
    
    objective_data$decision7 <- guage
    print(guage)
    print(swim_treat_decrease)
    print(swim_algae_decrease)
    print(money_decrease)
    print(eco_decrease)
    print(wq_decrease)
    print(treat_data)
    print(obs_data)

    
    reactive_tradeoff_plot$plot10 <- ggplot(data = guage, aes(objective, quantity, fill = objective)) + 
      geom_bar(stat = 'identity') +
      # labs(title = 'Decision C') +
      xlab('Objectives') +
      ylab('Percent Optimized') +
      ylim(0, 100) +
      scale_fill_manual(name = 'legend', 
                        values = c('Drinking water quality' = objective_colors[1], 
                                   'Ecological health' = objective_colors[2], 
                                   'Economic benefit' = objective_colors[3],
                                   'Swimmer safety' = objective_colors[4])) + ##FFB86F
      theme(legend.position = 'none',
            panel.background = element_rect(fill = NA, color = 'black'),
            panel.border = element_rect(color = 'black', fill = NA),
            plot.title = element_text(size = 15, hjust = 0.5),
            plot.caption = element_text(size = 15, hjust = 0),
            axis.text.x = element_text(angle = 45, size = 10, hjust = 1))
  })
  
  observeEvent(input$Decision_Day10, {
    # Show a modal when the button is pressed
    if(input$Decision_Day14!=mgmt_choices[2] &
      input$Decision_Day10==mgmt_choices[2]){
      shinyalert("You've chosen to cancel the swimming event!", "You cannot undo this decision. Please proceed to the next objective.")
      
    }
    if(input$Decision_Day14!=mgmt_choices[2] &
       input$Decision_Day10==mgmt_choices[3]){
      shinyalert("You've chosen to treat the reservoir with an algaecide!", "This will decrease the algal concentration in the forecast, but it is not 100% effective")
      
    }
    
  })
  
  observe({
    req(input$Decision_Day7)
    
    guage <- objective_data$decision7
    guage[1,2] <- 100 # set drinking water to 100% to start, not dependent on previous decisions
    guage[4,2] <- 100 # set swimmer safety to 100% to start, will write in treatment dependencies
    guage$objective <- factor(guage$objective, levels = decision_objectives)
    treat_data <- fcast_data$data_treat
    obs_data <- fcast_data$data
    
    decision_date <- date_7
    idx <- which(obs_data$date == decision_date)
    
    wq_decrease <- NA
    # based on the obs chl
    if(input$Decision_Day7==mgmt_choices[3]){
      wq_decrease <- 1
    }else{
      if(treat_data$obs_chl_ugl[idx] > 25){
        wq_decrease <- 0.5
      }else if(treat_data$obs_chl_ugl[idx] > 35){
        wq_decrease <- 0.25
      }else if(treat_data$obs_chl_ugl[idx] < 25){
        wq_decrease <- 1
      }
      
    }
    
    eco_decrease <- NA
    # based only on treatment
    if(input$Decision_Day7==mgmt_choices[3]){
      eco_decrease <- 0.5
    }else{
      eco_decrease <- 1
    }
    
    money_decrease <- NA
    # based on treatment or cancel
    if(input$Decision_Day7==mgmt_choices[3]){
      money_decrease <- 0.8
    }else if(input$Decision_Day7==mgmt_choices[2]){
      money_decrease <- 0.1
    }else{
      money_decrease <- 1
    }
    
    
    
    swim_treat_decrease <- NA
    # based on treatment and algal concentration
    if(input$Decision_Day7==mgmt_choices[3]){
      swim_treat_decrease <- 0.8
    }else{
      swim_treat_decrease <- 1
    }
    
    swim_algae_decrease <- NA
    # based on algal concentration
    if(treat_data$obs_chl_ugl[idx] > 25){
      swim_algae_decrease <- 0.5
    }else if(treat_data$obs_chl_ugl[idx] > 35){
      swim_algae_decrease <- 0.05
    }else if(treat_data$obs_chl_ugl[idx] < 25){
      swim_algae_decrease <- 1
    }
    
    #WQ
    guage[1,2] <- guage[1,2]*wq_decrease
    #eco health
    guage[2,2] <- guage[2,2]*eco_decrease
    #money
    guage[3,2] <- guage[3,2]*money_decrease
    #swimmers
    guage[4,2] <- guage[4,2]*swim_treat_decrease*swim_algae_decrease
    
    objective_data$decision2 <- guage
  
    
    reactive_tradeoff_plot$plot7 <- ggplot(data = guage, aes(objective, quantity, fill = objective)) + 
      geom_bar(stat = 'identity') +
      # labs(title = 'Decision C') +
      xlab('Objectives') +
      ylab('Percent Optimized') +
      ylim(0, 100) +
      scale_fill_manual(name = 'legend', 
                        values = c('Drinking water quality' = objective_colors[1], 
                                   'Ecological health' = objective_colors[2], 
                                   'Economic benefit' = objective_colors[3],
                                   'Swimmer safety' = objective_colors[4])) + ##FFB86F
      theme(legend.position = 'none',
            panel.background = element_rect(fill = NA, color = 'black'),
            panel.border = element_rect(color = 'black', fill = NA),
            plot.title = element_text(size = 15, hjust = 0.5),
            plot.caption = element_text(size = 15, hjust = 0),
            axis.text.x = element_text(angle = 45, size = 10, hjust = 1))
  })
  
  observeEvent(input$Decision_Day7, {
    # Show a modal when the button is pressed
    if(input$Decision_Day14!=mgmt_choices[2] &
       input$Decision_Day10!=mgmt_choices[2] &
       input$Decision_Day7==mgmt_choices[2]){
      shinyalert("You've chosen to cancel the swimming event!", "You cannot undo this decision. Please proceed to the next objective.") #, type = 'info'
      
    }
    if(input$Decision_Day14!=mgmt_choices[2] &
       input$Decision_Day10!=mgmt_choices[2] &
       input$Decision_Day7==mgmt_choices[3]){
      shinyalert("You've chosen to treat the reservoir with an algaecide!", "This will decrease the algal concentration in the forecast, but it is not 100% effective") #, type = 'info'
      
    }
    
  })
  
  observe({
    req(input$Decision_Day2)
    
    guage <- objective_data$decision2
    guage$objective <- factor(guage$objective, levels = decision_objectives)
    guage[1,2] <- 100 # set drinking water to 100% to start, not dependent on previous decisions
    guage[4,2] <- 100 # set swimmer safety to 100% to start, will write in treatment dependencies
    treat_data <- fcast_data$data_treat
    obs_data <- fcast_data$data
    
    decision_date <- date_2
    idx <- which(obs_data$date == decision_date)
    
    wq_decrease <- NA
    # based on the obs chl
    if(input$Decision_Day2==mgmt_choices[3]){
      wq_decrease <- 1
    }else{
      if(treat_data$obs_chl_ugl[idx] > 25){
        wq_decrease <- 0.5
      }else if(treat_data$obs_chl_ugl[idx] > 35){
        wq_decrease <- 0.25
      }else if(treat_data$obs_chl_ugl[idx] < 25){
        wq_decrease <- 1
      }
      
    }
    
    eco_decrease <- NA
    # based only on treatment
    if(input$Decision_Day2==mgmt_choices[3]){
      eco_decrease <- 0.5
    }else{
      eco_decrease <- 1
    }
    
    money_decrease <- NA
    # based on treatment or cancel
    if(input$Decision_Day2==mgmt_choices[3]){
      money_decrease <- 0.8
    }else if(input$Decision_Day2==mgmt_choices[2]){
      money_decrease <- 0.1
    }else{
      money_decrease <- 1
    }
    
    
    
    swim_treat_decrease <- NA
    # based on treatment and algal concentration
    if(input$Decision_Day2==mgmt_choices[3]){
      swim_treat_decrease <- 0.8
    }else{
      swim_treat_decrease <- 1
    }
    
    swim_algae_decrease <- NA
    # based on algal concentration
    if(treat_data$obs_chl_ugl[idx] > 25){
      swim_algae_decrease <- 0.5
    }else if(treat_data$obs_chl_ugl[idx] > 35){
      swim_algae_decrease <- 0.05
    }else if(treat_data$obs_chl_ugl[idx] < 25){
      swim_algae_decrease <- 1
    }
    
    #WQ
    guage[1,2] <- guage[1,2]*wq_decrease
    #eco health
    guage[2,2] <- guage[2,2]*eco_decrease
    #money
    guage[3,2] <- guage[3,2]*money_decrease
    #swimmers
    guage[4,2] <- guage[4,2]*swim_treat_decrease*swim_algae_decrease

    #objective_data$decision2 <- guage
    
  
    reactive_tradeoff_plot$plot2 <- ggplot(data = guage, aes(objective, quantity, fill = objective)) + 
      geom_bar(stat = 'identity') +
      # labs(title = 'Decision C') +
      xlab('Objectives') +
      ylab('Percent Optimized') +
      ylim(0, 100) +
      scale_fill_manual(name = 'legend', 
                        values = c('Drinking water quality' = objective_colors[1], 
                                   'Ecological health' = objective_colors[2], 
                                   'Economic benefit' = objective_colors[3],
                                   'Swimmer safety' = objective_colors[4])) + ##FFB86F
      theme(legend.position = 'none',
            panel.background = element_rect(fill = NA, color = 'black'),
            panel.border = element_rect(color = 'black', fill = NA),
            plot.title = element_text(size = 15, hjust = 0.5),
            plot.caption = element_text(size = 15, hjust = 0),
            axis.text.x = element_text(angle = 45, size = 10, hjust = 1))
  })
  
  observeEvent(input$Decision_Day2, {
    # Show a modal when the button is pressed
    if(input$Decision_Day14!=mgmt_choices[2] &
       input$Decision_Day10!=mgmt_choices[2] &
       input$Decision_Day7!=mgmt_choices[2] &
       input$Decision_Day2==mgmt_choices[2]){
      shinyalert("You've chosen to cancel the swimming event!", "You cannot undo this decision. Please proceed to the next objective.")
      
    }
    if(input$Decision_Day14!=mgmt_choices[2] &
       input$Decision_Day10!=mgmt_choices[2] &
       input$Decision_Day7!=mgmt_choices[2] &
       input$Decision_Day2==mgmt_choices[3]){
      shinyalert("You've chosen to treat the reservoir with an algaecide!", "This will decrease the algal concentration in the forecast, but it is not 100% effective")
      
    }
    
  })
  
  output$tradeoff_plot_14 <- renderPlot({
    p <- reactive_tradeoff_plot$plot14
    return(p)
  })
  
 output$tradeoff_plot_10 <- renderPlot({
   p <- reactive_tradeoff_plot$plot10
   return(p)
 })
 
 output$tradeoff_plot_7 <- renderPlot({
   p <- reactive_tradeoff_plot$plot7
   return(p)
 })
 
 output$tradeoff_plot_2 <- renderPlot({
   p <- reactive_tradeoff_plot$plot2
   return(p)
 })
 
 
 observeEvent(input$save_obj4a_objectives, {
   validate(
     need(input$Decision_Day2!="",
          message = "Please make your decisions.")
   )
   
   # Progress bar
   progress <- shiny::Progress$new()
   on.exit(progress$close())
   progress$set(message = "Saving plot as image file for the report.", 
                detail = "This may take a while. This window will disappear  
                     when it is downloaded.", value = 0.5)
   
   p <- reactive_tradeoff_plot$plot2 +
     theme_classic(base_size = 35) +
     
   theme(legend.position = 'none',
         panel.background = element_rect(fill = NA, color = 'black'),
         panel.border = element_rect(color = 'black', fill = NA),
         plot.title = element_text(size = 15, hjust = 0.5),
         plot.caption = element_text(size = 15, hjust = 0),
         axis.text.x = element_text(angle = 45, size = 25, hjust = 1))
   img_file <- "www/obj4a_objectives.png"
   ggsave(img_file, p, dpi = 300, width = 320, height = 380, units = "mm")
   progress$set(value = 1)
   
   
 })
  
 reactive_tradeoff_plot_UC <- reactiveValues(plot14 = NULL, plot10 = NULL, plot7 = NULL, plot2 = NULL)
 
 observe({
   req(input$Decision_Day14_UC)
   #guage <- read.csv('data/scenario_objectives.csv')
   guage <- objective_data$decision14
   guage$objective <- factor(guage$objective, levels = decision_objectives)
   guage[1,2] <- 100 # set drinking water to 100% to start, not dependent on previous decisions
   guage[4,2] <- 100 # set swimmer safety to 100% to start, will write in treatment dependencies
   treat_data <- fcast_data$data_treat
   obs_data <- fcast_data$data
   
   decision_date <- date_14
   idx <- which(obs_data$date == decision_date)
   
   wq_decrease <- NA
   # based on the obs chl
   if(obs_data$obs_chl_ugl[idx] > 25){
     wq_decrease <- 0.5
   }else if(obs_data$obs_chl_ugl[idx] > 35){
     wq_decrease <- 0.25
   }else if(obs_data$obs_chl_ugl[idx] < 25){
     wq_decrease <- 1
   }
   
   eco_decrease <- NA
   # based only on treatment
   if(input$Decision_Day14_UC==mgmt_choices[3]){
     eco_decrease <- 0.5
   }else{
     eco_decrease <- 1
   }
   
   money_decrease <- NA
   # based on treatment or cancel
   if(input$Decision_Day14_UC==mgmt_choices[3]){
     money_decrease <- 0.8
   }else if(input$Decision_Day14_UC==mgmt_choices[2]){
     money_decrease <- 0.1
   }else{
     money_decrease <- 1
   }
   
   swim_treat_decrease <- NA
   # based on treatment and algal concentration
   if(input$Decision_Day14_UC==mgmt_choices[3]){
     swim_treat_decrease <- 0.8
   }else{
     swim_treat_decrease <- 1
   }
   
   swim_algae_decrease <- NA
   # based on algal concentration
   if(obs_data$obs_chl_ugl[idx] > 25){
     swim_algae_decrease <- 0.5
   }else if(obs_data$obs_chl_ugl[idx] > 35){
     swim_algae_decrease <- 0.05
   }else if(obs_data$obs_chl_ugl[idx] < 25){
     swim_algae_decrease <- 1
   }
   
   #WQ
   guage[1,2] <- guage[1,2]*wq_decrease
   #eco health
   guage[2,2] <- guage[2,2]*eco_decrease
   #money
   guage[3,2] <- guage[3,2]*money_decrease
   #swimmers
   guage[4,2] <- guage[4,2]*swim_treat_decrease*swim_algae_decrease
   
   objective_data$decision10 <- guage
   
   reactive_tradeoff_plot_UC$plot14 <- ggplot(data = guage, aes(objective, quantity, fill = objective)) + 
     geom_bar(stat = 'identity') +
     # labs(title = 'Decision C') +
     xlab('Objectives') +
     ylab('Percent Optimized') +
     ylim(0, 100) +
     scale_fill_manual(name = 'legend', 
                       values = c('Drinking water quality' = objective_colors[1], 
                                  'Ecological health' = objective_colors[2], 
                                  'Economic benefit' = objective_colors[3],
                                  'Swimmer safety' = objective_colors[4])) + ##FFB86F
     theme(legend.position = 'none',
           panel.background = element_rect(fill = NA, color = 'black'),
           panel.border = element_rect(color = 'black', fill = NA),
           plot.title = element_text(size = 15, hjust = 0.5),
           plot.caption = element_text(size = 15, hjust = 0),
           axis.text.x = element_text(angle = 45, size = 10, hjust = 1))
   })
 
 observeEvent(input$Decision_Day14_UC, {
   # Show a modal when the button is pressed
   if(input$Decision_Day14_UC==mgmt_choices[2]){
     shinyalert("You've chosen to cancel the swimming event!", "You cannot undo this decision. Please proceed to the next objective.")
     
   }
   if(input$Decision_Day14_UC==mgmt_choices[3]){
     shinyalert("You've chosen to treat the reservoir with an algaecide!", "This will decrease the algal concentration in the forecast, but it is not 100% effective")
     
   }
   
 })
 
 observeEvent(input$Decision_Day14_UC, {
   toggleState("Decision_Day10_UC", !is.null(input$Decision_Day14_UC))
 })
 observe({
   req(input$Decision_Day10_UC)
   
   guage <- objective_data$decision10
   guage$objective <- factor(guage$objective, levels = decision_objectives)
   guage[1,2] <- 100 # set drinking water to 100% to start, not dependent on previous decisions
   guage[4,2] <- 100 # set swimmer safety to 100% to start, will write in treatment dependencies
   treat_data <- fcast_data$data_treat
   obs_data <- fcast_data$data
   
   decision_date <- date_10
   idx <- which(obs_data$date == decision_date)
   
   wq_decrease <- NA
   # based on the obs chl
   if(input$Decision_Day10_UC==mgmt_choices[3]){
     wq_decrease <- 1
   }else{
     if(treat_data$obs_chl_ugl[idx] > 25){
       wq_decrease <- 0.5
     }else if(treat_data$obs_chl_ugl[idx] > 35){
       wq_decrease <- 0.25
     }else if(treat_data$obs_chl_ugl[idx] < 25){
       wq_decrease <- 1
     }
     
   }
   
   eco_decrease <- NA
   # based only on treatment
   if(input$Decision_Day10_UC==mgmt_choices[3]){
     eco_decrease <- 0.5
   }else{
     eco_decrease <- 1
   }
   
   money_decrease <- NA
   # based on treatment or cancel
   if(input$Decision_Day10_UC==mgmt_choices[3]){
     money_decrease <- 0.8
   }else if(input$Decision_Day10_UC==mgmt_choices[2]){
     money_decrease <- 0.1
   }else{
     money_decrease <- 1
   }
   
   swim_treat_decrease <- NA
   # based on treatment and algal concentration
   if(input$Decision_Day10_UC==mgmt_choices[3]){
     swim_treat_decrease <- 0.8
   }else{
     swim_treat_decrease <- 1
   }
   
   swim_algae_decrease <- NA
   # based on algal concentration
   if(treat_data$obs_chl_ugl[idx] > 25){
     swim_algae_decrease <- 0.5
   }else if(treat_data$obs_chl_ugl[idx] > 35){
     swim_algae_decrease <- 0.05
   }else if(treat_data$obs_chl_ugl[idx] < 25){
     swim_algae_decrease <- 1
   }
   
   #WQ
   guage[1,2] <- guage[1,2]*wq_decrease
   #eco health
   guage[2,2] <- guage[2,2]*eco_decrease
   #money
   guage[3,2] <- guage[3,2]*money_decrease
   #swimmers
   guage[4,2] <- guage[4,2]*swim_treat_decrease*swim_algae_decrease
   
   objective_data$decision7 <- guage
  
   
   reactive_tradeoff_plot_UC$plot10 <- ggplot(data = guage, aes(objective, quantity, fill = objective)) + 
     geom_bar(stat = 'identity') +
     # labs(title = 'Decision C') +
     xlab('Objectives') +
     ylab('Percent Optimized') +
     ylim(0, 100) +
     scale_fill_manual(name = 'legend', 
                       values = c('Drinking water quality' = objective_colors[1], 
                                  'Ecological health' = objective_colors[2], 
                                  'Economic benefit' = objective_colors[3],
                                  'Swimmer safety' = objective_colors[4])) + ##FFB86F
     theme(legend.position = 'none',
           panel.background = element_rect(fill = NA, color = 'black'),
           panel.border = element_rect(color = 'black', fill = NA),
           plot.title = element_text(size = 15, hjust = 0.5),
           plot.caption = element_text(size = 15, hjust = 0),
           axis.text.x = element_text(angle = 45, size = 10, hjust = 1)) })
 
 observeEvent(input$Decision_Day10_UC, {
   # Show a modal when the button is pressed
   if(input$Decision_Day14_UC!=mgmt_choices[2] & 
      input$Decision_Day10_UC==mgmt_choices[2]){
     shinyalert("You've chosen to cancel the swimming event!", "You cannot undo this decision. Please proceed to the next objective.")
     
   }
   if(input$Decision_Day14_UC!=mgmt_choices[2] &
      input$Decision_Day10_UC==mgmt_choices[3]){
     shinyalert("You've chosen to treat the reservoir with an algaecide!", "This will decrease the algal concentration in the forecast, but it is not 100% effective")
     
   }
   
 })
 
 observe({
   req(input$Decision_Day7_UC)
   
   guage <- objective_data$decision7
   guage$objective <- factor(guage$objective, levels = decision_objectives)
   guage[1,2] <- 100 # set drinking water to 100% to start, not dependent on previous decisions
   guage[4,2] <- 100 # set swimmer safety to 100% to start, will write in treatment dependencies
   treat_data <- fcast_data$data_treat
   obs_data <- fcast_data$data
   
   decision_date <- date_7
   idx <- which(obs_data$date == decision_date)
   
   wq_decrease <- NA
   # based on the obs chl
   if(input$Decision_Day7_UC==mgmt_choices[3]){
     wq_decrease <- 1
   }else{
     if(treat_data$obs_chl_ugl[idx] > 25){
       wq_decrease <- 0.5
     }else if(treat_data$obs_chl_ugl[idx] > 35){
       wq_decrease <- 0.25
     }else if(treat_data$obs_chl_ugl[idx] < 25){
       wq_decrease <- 1
     }
     
   }
   
   
   eco_decrease <- NA
   # based only on treatment
   if(input$Decision_Day7_UC==mgmt_choices[3]){
     eco_decrease <- 0.5
   }else{
     eco_decrease <- 1
   }
   
   money_decrease <- NA
   # based on treatment or cancel
   if(input$Decision_Day7_UC==mgmt_choices[3]){
     money_decrease <- 0.8
   }else if(input$Decision_Day7_UC==mgmt_choices[2]){
     money_decrease <- 0.1
   }else{
     money_decrease <- 1
   }
   
   swim_treat_decrease <- NA
   # based on treatment and algal concentration
   if(input$Decision_Day7_UC==mgmt_choices[3]){
     swim_treat_decrease <- 0.8
   }else{
     swim_treat_decrease <- 1
   }
   
   swim_algae_decrease <- NA
   # based on algal concentration
   if(treat_data$obs_chl_ugl[idx] > 25){
     swim_algae_decrease <- 0.5
   }else if(treat_data$obs_chl_ugl[idx] > 35){
     swim_algae_decrease <- 0.05
   }else if(treat_data$obs_chl_ugl[idx] < 25){
     swim_algae_decrease <- 1
   }
   
   #WQ
   guage[1,2] <- guage[1,2]*wq_decrease
   #eco health
   guage[2,2] <- guage[2,2]*eco_decrease
   #money
   guage[3,2] <- guage[3,2]*money_decrease
   #swimmers
   guage[4,2] <- guage[4,2]*swim_treat_decrease*swim_algae_decrease
   
   objective_data$decision2 <- guage
  
   
   reactive_tradeoff_plot_UC$plot7 <- ggplot(data = guage, aes(objective, quantity, fill = objective)) + 
     geom_bar(stat = 'identity') +
     # labs(title = 'Decision C') +
     xlab('Objectives') +
     ylab('Percent Optimized') +
     ylim(0, 100) +
     scale_fill_manual(name = 'legend', 
                       values = c('Drinking water quality' = objective_colors[1], 
                                  'Ecological health' = objective_colors[2], 
                                  'Economic benefit' = objective_colors[3],
                                  'Swimmer safety' = objective_colors[4])) + ##FFB86F
     theme(legend.position = 'none',
           panel.background = element_rect(fill = NA, color = 'black'),
           panel.border = element_rect(color = 'black', fill = NA),
           plot.title = element_text(size = 15, hjust = 0.5),
           plot.caption = element_text(size = 15, hjust = 0),
           axis.text.x = element_text(angle = 45, size = 10, hjust = 1)) })
 
 observeEvent(input$Decision_Day7_UC, {
   # Show a modal when the button is pressed
   if(input$Decision_Day14_UC!=mgmt_choices[2] & input$Decision_Day10_UC!=mgmt_choices[2] & input$Decision_Day7_UC==mgmt_choices[2]){
     shinyalert("You've chosen to cancel the swimming event!", "You cannot undo this decision. Please proceed to the next objective.")
     
   }
   if(input$Decision_Day14_UC!=mgmt_choices[2] &
      input$Decision_Day10_UC!=mgmt_choices[2] &
      input$Decision_Day7_UC==mgmt_choices[3]){
     shinyalert("You've chosen to treat the reservoir with an algaecide!", "This will decrease the algal concentration in the forecast, but it is not 100% effective")
     
   }
   
 })
 
 observe({
   req(input$Decision_Day2_UC)
   
   guage <- objective_data$decision2
   guage$objective <- factor(guage$objective, levels = decision_objectives)
   guage[1,2] <- 100 # set drinking water to 100% to start, not dependent on previous decisions
   guage[4,2] <- 100 # set swimmer safety to 100% to start, will write in treatment dependencies
   treat_data <- fcast_data$data_treat
   obs_data <- fcast_data$data
   
   decision_date <- date_2
   idx <- which(obs_data$date == decision_date)
   
   wq_decrease <- NA
   # based on the obs chl
   if(input$Decision_Day2_UC==mgmt_choices[3]){
     wq_decrease <- 1
   }else{
     if(treat_data$obs_chl_ugl[idx] > 25){
       wq_decrease <- 0.5
     }else if(treat_data$obs_chl_ugl[idx] > 35){
       wq_decrease <- 0.25
     }else if(treat_data$obs_chl_ugl[idx] < 25){
       wq_decrease <- 1
     }
     
   }
   
   
   eco_decrease <- NA
   # based only on treatment
   if(input$Decision_Day2_UC==mgmt_choices[3]){
     eco_decrease <- 0.5
   }else{
     eco_decrease <- 1
   }
   
   money_decrease <- NA
   # based on treatment or cancel
   if(input$Decision_Day2_UC==mgmt_choices[3]){
     money_decrease <- 0.8
   }else if(input$Decision_Day2_UC==mgmt_choices[2]){
     money_decrease <- 0.1
   }else{
     money_decrease <- 1
   }
   
   swim_treat_decrease <- NA
   # based on treatment and algal concentration
   if(input$Decision_Day2_UC==mgmt_choices[3]){
     swim_treat_decrease <- 0.8
   }else{
     swim_treat_decrease <- 1
   }
   
   swim_algae_decrease <- NA
   # based on algal concentration
   if(treat_data$obs_chl_ugl[idx] > 25){
     swim_algae_decrease <- 0.5
   }else if(treat_data$obs_chl_ugl[idx] > 35){
     swim_algae_decrease <- 0.05
   }else if(treat_data$obs_chl_ugl[idx] < 25){
     swim_algae_decrease <- 1
   }
   
   #WQ
   guage[1,2] <- guage[1,2]*wq_decrease
   #eco health
   guage[2,2] <- guage[2,2]*eco_decrease
   #money
   guage[3,2] <- guage[3,2]*money_decrease
   #swimmers
   guage[4,2] <- guage[4,2]*swim_treat_decrease*swim_algae_decrease
   
#   objective_data$decision10 <- guage

   
   reactive_tradeoff_plot_UC$plot2 <- ggplot(data = guage, aes(objective, quantity, fill = objective)) + 
     geom_bar(stat = 'identity') +
     # labs(title = 'Decision C') +
     xlab('Objectives') +
     ylab('Percent Optimized') +
     ylim(0, 100) +
     scale_fill_manual(name = 'legend', 
                       values = c('Drinking water quality' = objective_colors[1], 
                                  'Ecological health' = objective_colors[2], 
                                  'Economic benefit' = objective_colors[3],
                                  'Swimmer safety' = objective_colors[4])) + ##FFB86F
     theme(legend.position = 'none',
           panel.background = element_rect(fill = NA, color = 'black'),
           panel.border = element_rect(color = 'black', fill = NA),
           plot.title = element_text(size = 15, hjust = 0.5),
           plot.caption = element_text(size = 15, hjust = 0),
           axis.text.x = element_text(angle = 45, size = 10, hjust = 1)) })
 
 observeEvent(input$Decision_Day2_UC, {
   # Show a modal when the button is pressed
   if(input$Decision_Day14_UC!=mgmt_choices[2] & 
      input$Decision_Day10_UC!=mgmt_choices[2] & 
      input$Decision_Day7_UC!=mgmt_choices[2] & 
      input$Decision_Day2_UC==mgmt_choices[2]){
     shinyalert("You've chosen to cancel the swimming event!", "You cannot undo this decision. Please proceed to the next objective.")
     
   }
   if(input$Decision_Day14_UC!=mgmt_choices[2] &
      input$Decision_Day10_UC!=mgmt_choices[2] &
      input$Decision_Day7_UC!=mgmt_choices[2] &
      input$Decision_Day2_UC==mgmt_choices[3]){
     shinyalert("You've chosen to treat the reservoir with an algaecide!", "This will decrease the algal concentration in the forecast, but it is not 100% effective")
     
   }
   
 })
 
 output$tradeoff_plot_14_withUC <- renderPlot({
   p <- reactive_tradeoff_plot_UC$plot14
   return(p)
 })
 
 output$tradeoff_plot_10_withUC <- renderPlot({
   p <- reactive_tradeoff_plot_UC$plot10
   return(p)
 })
 
 output$tradeoff_plot_7_withUC <- renderPlot({
   p <- reactive_tradeoff_plot_UC$plot7
   return(p)
 })
 
 output$tradeoff_plot_2_withUC <- renderPlot({
   p <- reactive_tradeoff_plot_UC$plot2
   return(p)
 }) 
 
 
 observeEvent(input$save_obj4b_objectives, {
   validate(
     need(input$Decision_Day2_UC!="",
          message = "Please make your decisions.")
   )
   
   # Progress bar
   progress <- shiny::Progress$new()
   on.exit(progress$close())
   progress$set(message = "Saving plot as image file for the report.", 
                detail = "This may take a while. This window will disappear  
                     when it is downloaded.", value = 0.5)
   
   p <- reactive_tradeoff_plot_UC$plot2 +
   theme_classic(base_size = 35) +
     theme(legend.position = 'none',
           panel.background = element_rect(fill = NA, color = 'black'),
           panel.border = element_rect(color = 'black', fill = NA),
           plot.title = element_text(size = 15, hjust = 0.5),
           plot.caption = element_text(size = 15, hjust = 0),
           axis.text.x = element_text(angle = 45, size = 25, hjust = 1))
   img_file <- "www/obj4b_objectives.png"
   ggsave(img_file, p, dpi = 300, width = 320, height = 380, units = "mm")
   progress$set(value = 1)
   
 })
 
  
  output$PrOACT <- renderSlickR({
    imgs <- list.files("www", pattern = "PrOACT", full.names = TRUE)
    slickR(imgs)
  })  


observeEvent(input$ans_btn, {
  if(length(input$problem) == 0) {
    res <- "Drag answers into Problem box!"
  } else if(all(ifelse(base::setequal(input$problem, problem_answers), TRUE, FALSE))) {
    res <- "Problem answers are correct!"
  } else {
    res <- "Incorrect or incomplete answers in Problem box"
  }
  
  if(length(input$objective) == 0) {
    res2 <- "Drag answers into Objectives box!"
  } else if(all(ifelse(base::setequal(input$objective, objective_answers), TRUE, FALSE))) {
    res2 <- "Objective answers are correct!"
  } else {
    res2 <- "Incorrect or incomplete answers in Objectives box"
  }
  
  if(length(input$alternatives) == 0) {
    res3 <- "Drag answers into Alternatives box!"
  } else if(all(ifelse(base::setequal(input$alternatives, alternative_answers), TRUE, FALSE))) {
    res3 <- "Alternative answers are correct!"
  } else {
    res3 <- "Incorrect or incomplete answers in Alternative box"
  }
  
  if(length(input$consequences) == 0) {
    res4 <- "Drag answers into Consequences box!"
  } else if(all(ifelse(base::setequal(input$consequences, consequence_answers), TRUE, FALSE))) {
    res4 <- "Consequence answers are correct!"
  } else {
    res4 <- "Incorrect or incomplete answers in Consequence box"
  }
  
  if(length(input$tradeoffs) == 0) {
    res5 <- "Drag answers into Trade-Offs box!"
    
  } else if(all(ifelse(base::setequal(input$tradeoffs, tradeoffs_answers), TRUE, FALSE))) {
    res5 <- "Tradeoff answers are correct!"
  } else {
    res5 <- "Incorrect or incomplete answers in Tradeoff box"
  }
  output$pr_ans <- renderText({
    res
  })
  output$obj_ans <- renderText({
    res2
  })
  output$alt_ans <- renderText({
    res3
  })
  output$cons_ans <- renderText({
    res4
  })
  output$trof_ans <- renderText({
    res5
  })
}) 




fc_plots <- reactiveValues(day14 = NULL, day7 = NULL, day2 = NULL)
# **Decision if statements ----
fcast_data <- reactiveValues(day14 = day14_orig, 
                             day10 = day10_orig, 
                             day7 =  day7_orig, 
                             day2 =  day2_orig,
                             data = obs_data,
                             data_treat = treat_data,
                             day14_UC = day14_orig_UC, 
                             day10_UC = day10_orig_UC, 
                             day7_UC =  day7_orig_UC, 
                             day2_UC =  day2_orig_UC,
                             data_UC = obs_data_UC,
                             data_treat_UC = treat_data_UC)


# set up dataframes based on decisions for objective 4a
observeEvent(input$Decision_Day14, {
  if(input$Decision_Day14==mgmt_choices[3]){
    fcast_data$day10[,3:28] <- fcast_data$day10[,3:28]*decrease_14
    fcast_data$day7[,3:28] <- fcast_data$day7[,3:28]*decrease_14
    fcast_data$day2[,3:28] <- fcast_data$day2[,3:28]*decrease_14
    
    treat_data[8:35,2] <- treat_data[8:35,2]*decrease_14
    fcast_data$data_treat <- treat_data

  }
  if(input$Decision_Day14==mgmt_choices[1]){  # if they change their answer, it goes back to original values
    fcast_data$day10 <- day10_orig
    fcast_data$day7  <- day7_orig
    fcast_data$day2  <- day2_orig
    fcast_data$data <- obs_data
    
  }

})


observeEvent(input$Decision_Day10, {
  if(input$Decision_Day14==mgmt_choices[3] & input$Decision_Day10==mgmt_choices[3]){
    fcast_data$day7[,3:28] <- day7_orig[,3:28]*decrease_10*decrease_14
    fcast_data$day2[,3:28] <- day2_orig[,3:28]*decrease_10*decrease_14
    
    treat_data[8:35,2] <- treat_data[8:35,2]*decrease_10*decrease_14
    fcast_data$data_treat <- treat_data

    
  }
  if(input$Decision_Day14==mgmt_choices[3] & input$Decision_Day10==mgmt_choices[1]){ 
    fcast_data$day7[,3:28]  <- day7_orig[,3:28]*decrease_14
    fcast_data$day2[,3:28]  <- day2_orig[,3:28]*decrease_14
    
    treat_data[8:35,2] <- treat_data[8:35,2]*decrease_14
    fcast_data$data_treat <- treat_data
    
    
  }
  if(input$Decision_Day14==mgmt_choices[1] & input$Decision_Day10==mgmt_choices[3]){
    fcast_data$day7[,3:28]  <- day7_orig[,3:28]*decrease_10
    fcast_data$day2[,3:28]  <- day2_orig[,3:28]*decrease_10
  
    treat_data[12:35,2] <- treat_data[12:35,2]*decrease_10 
    fcast_data$data_treat <- treat_data
    
    # treatment date indices 
    # day 14: 8
    # day 10: 12
    # day 7 : 15
    # day 2 : 20
    
  }
  if(input$Decision_Day14==mgmt_choices[1] & input$Decision_Day10==mgmt_choices[1]){ 
    fcast_data$day10 <- day10_orig
    fcast_data$day7  <- day7_orig
    fcast_data$day2  <- day2_orig
    fcast_data$data <- obs_data
    
  }
  
  
})

observeEvent(input$Decision_Day7, {
  # 3 3 3
  # 3 3 1
  # 3 1 3
  # 1 3 3
  # 1 1 3
  # 1 1 1
  # 1 3 1 

  
  
  if(input$Decision_Day14==mgmt_choices[3] & input$Decision_Day10==mgmt_choices[3] & input$Decision_Day7==mgmt_choices[3]){
    
    fcast_data$day2[,3:28] <- day2_orig[,3:28]*decrease_10*decrease_14*decrease_7
    treat_data[8:35,2] <- treat_data[8:35,2]*decrease_10*decrease_14*decrease_7
    fcast_data$data_treat <- treat_data
    
  }
  if(input$Decision_Day14==mgmt_choices[3] & input$Decision_Day10==mgmt_choices[3] & input$Decision_Day7==mgmt_choices[1]){
    fcast_data$day2[,3:28] <- day2_orig[,3:28]*decrease_10*decrease_14
    treat_data[8:35,2] <- treat_data[8:35,2]*decrease_10*decrease_14
    fcast_data$data_treat <- treat_data
  }
  if(input$Decision_Day14==mgmt_choices[3] & input$Decision_Day10==mgmt_choices[1] & input$Decision_Day7==mgmt_choices[3]){ 
    
    fcast_data$day2[,3:28] <- day2_orig[,3:28]*decrease_14*decrease_7
    treat_data[8:35,2] <- treat_data[8:35,2]*decrease_14*decrease_7
    fcast_data$data_treat <- treat_data
  }
  if(input$Decision_Day14==mgmt_choices[1] & input$Decision_Day10==mgmt_choices[3] & input$Decision_Day7==mgmt_choices[3]){ 
    
    fcast_data$day2[,3:28] <- day2_orig[,3:28]*decrease_10*decrease_7
    treat_data[8:35,2] <- treat_data[8:35,2]*decrease_10*decrease_7
    fcast_data$data_treat <- treat_data
  }
  if(input$Decision_Day14==mgmt_choices[1] & input$Decision_Day10==mgmt_choices[1] & input$Decision_Day7==mgmt_choices[3]){
    fcast_data$day2[,3:28] <- day2_orig[,3:28]*decrease_7
    treat_data[8:35,2] <- treat_data[8:35,2]*decrease_7
    fcast_data$data_treat <- treat_data
  } 
  if(input$Decision_Day14==mgmt_choices[1] & input$Decision_Day10==mgmt_choices[3] & input$Decision_Day7==mgmt_choices[1]){
    fcast_data$day2[,3:28] <- day2_orig[,3:28]*decrease_10
    treat_data[8:35,2] <- treat_data[8:35,2]*decrease_10
    fcast_data$data_treat <- treat_data
  }
  
  if(input$Decision_Day14==mgmt_choices[1] & input$Decision_Day10==mgmt_choices[1] & input$Decision_Day7==mgmt_choices[1]){
    
    fcast_data$day10 <- day10_orig
    fcast_data$day7  <- day7_orig
    fcast_data$day2  <- day2_orig
    print(fcast_data$day10$min)
    print(fcast_data$day10$max)
  }
  
  print(decrease_14)
  print(decrease_10)
  print(decrease_7)
})


observeEvent(input$Decision_Day2, {
  if(input$Decision_Day2==mgmt_choices[3]){
    fcast_data$data_treat[19:35, 2] <-  fcast_data$data_treat[19:35, 2]*decrease_2
    print(fcast_data$data_treat)
    print(decrease_2)
    
  }
})

# set up dataframes based on decisions for objective 4b
observeEvent(input$Decision_Day14_UC, {
  if(input$Decision_Day14_UC==mgmt_choices[3]){
    fcast_data$day10_UC[,3:28] <- day10_orig_UC[,3:28]*decrease_14_UC
    fcast_data$day7_UC[,3:28] <-  day7_orig_UC[,3:28]*decrease_14_UC
    fcast_data$day2_UC[,3:28] <-  day2_orig_UC[,3:28]*decrease_14_UC
    
    treat_data_UC[8:35,2] <- treat_data_UC[8:35,2]*decrease_14_UC
    fcast_data$data_treat_UC <- treat_data_UC

  }
  if(input$Decision_Day14_UC==mgmt_choices[1]){  # if they change their answer, it goes back to original values
    fcast_data$day10_UC <- day10_orig_UC
    fcast_data$day7_UC  <- day7_orig_UC
    fcast_data$day2_UC  <- day2_orig_UC
    fcast_data$data_UC <- obs_data_UC
    
  }
  
})


observeEvent(input$Decision_Day10_UC, {
  if(input$Decision_Day14_UC==mgmt_choices[3] & input$Decision_Day10_UC==mgmt_choices[3]){
    fcast_data$day7_UC[,3:28] <- day7_orig_UC[,3:28]*decrease_10_UC*decrease_14_UC
    fcast_data$day2_UC[,3:28] <- day2_orig_UC[,3:28]*decrease_10_UC*decrease_14_UC
    
    treat_data_UC[8:35,2] <- treat_data_UC[8:35,2]*decrease_10_UC*decrease_14_UC
    fcast_data$data_treat_UC <- treat_data_UC
    
    
  }
  if(input$Decision_Day14_UC==mgmt_choices[3] & input$Decision_Day10_UC==mgmt_choices[1]){ 
    fcast_data$day7_UC[,3:28]  <- day7_orig_UC[,3:28]*decrease_14_UC
    fcast_data$day2_UC[,3:28]  <- day2_orig_UC[,3:28]*decrease_14_UC

    treat_data_UC[8:35,2] <- treat_data_UC[8:35,2]*decrease_14_UC
    fcast_data$data_treat_UC <- treat_data_UC
    
    
  }
  if(input$Decision_Day14_UC==mgmt_choices[1] & input$Decision_Day10_UC==mgmt_choices[3]){
    fcast_data$day7_UC[,3:28]  <- day7_orig_UC[,3:28]*decrease_10_UC
    fcast_data$day2_UC[,3:28]  <- day2_orig_UC[,3:28]*decrease_10_UC

    treat_data_UC[8:35,2] <- treat_data_UC[8:35,2]*decrease_10_UC
    fcast_data$data_treat_UC <- treat_data_UC
    
    
  }
  if(input$Decision_Day14_UC==mgmt_choices[1] & input$Decision_Day10_UC==mgmt_choices[1]){ 
    fcast_data$day10_UC <- day10_orig_UC
    fcast_data$day7_UC  <- day7_orig_UC
    fcast_data$day2_UC  <- day2_orig_UC
    fcast_data$data_UC <- obs_data_UC
    
  }
  
  
})

observeEvent(input$Decision_Day7_UC, {
  # 3 3 3
  # 3 3 1
  # 3 1 3
  # 1 3 3
  # 1 1 3
  # 1 1 1
  
  
  
  if(input$Decision_Day14_UC==mgmt_choices[3] & input$Decision_Day10_UC==mgmt_choices[3] & input$Decision_Day7_UC==mgmt_choices[3]){
    
    fcast_data$day2_UC[,3:28] <- day2_orig_UC[,3:28]*decrease_10_UC*decrease_14_UC*decrease_7_UC
  
    treat_data_UC[8:35,2] <- treat_data_UC[8:35,2]*decrease_10_UC*decrease_14_UC*decrease_7_UC
    fcast_data$data_treat_UC <- treat_data_UC
    
  }
  if(input$Decision_Day14_UC==mgmt_choices[3] & input$Decision_Day10_UC==mgmt_choices[3] & input$Decision_Day7_UC==mgmt_choices[1]){
    fcast_data$day2_UC[,3:28] <- day2_orig_UC[,3:28]*decrease_10_UC*decrease_14_UC
   
    treat_data_UC[8:35,2] <- treat_data_UC[8:35,2]*decrease_10_UC*decrease_14_UC
    fcast_data$data_treat_UC <- treat_data_UC
  }
  if(input$Decision_Day14_UC==mgmt_choices[3] & input$Decision_Day10_UC==mgmt_choices[1] & input$Decision_Day7_UC==mgmt_choices[3]){ 
    fcast_data$day2_UC[,3:28] <- day2_orig_UC[,3:28]*decrease_14_UC*decrease_7_UC
  
    treat_data_UC[8:35,2] <- treat_data_UC[8:35,2]*decrease_14_UC*decrease_7_UC
    fcast_data$data_treat_UC <- treat_data_UC
  }
  if(input$Decision_Day14_UC==mgmt_choices[1] & input$Decision_Day10_UC==mgmt_choices[3] & input$Decision_Day7_UC==mgmt_choices[3]){ 
    fcast_data$day2_UC[,3:28] <- day2_orig_UC[,3:28]*decrease_10_UC*decrease_7_UC
    
    treat_data_UC[8:35,2] <- treat_data_UC[8:35,2]*decrease_10_UC*decrease_7_UC
    fcast_data$data_treat_UC <- treat_data_UC
  }
  if(input$Decision_Day14_UC==mgmt_choices[1] & input$Decision_Day10_UC==mgmt_choices[1] & input$Decision_Day7_UC==mgmt_choices[3]){
    fcast_data$day2_UC[,3:28] <- day2_orig_UC[,3:28]*decrease_7_UC
    
    treat_data_UC[8:35,2] <- treat_data_UC[8:35,2]*decrease_7_UC
    fcast_data$data_treat_UC <- treat_data_UC
  }
  if(input$Decision_Day14_UC==mgmt_choices[1] & input$Decision_Day10_UC==mgmt_choices[3] & input$Decision_Day7_UC==mgmt_choices[1]){
    fcast_data$day2[,3:28] <- day2_orig[,3:28]*decrease_10
    
    treat_data[8:35,2] <- treat_data[8:35,2]*decrease_10
    fcast_data$data_treat <- treat_data
  }
  if(input$Decision_Day14_UC==mgmt_choices[1] & input$Decision_Day10_UC==mgmt_choices[1] & input$Decision_Day7_UC==mgmt_choices[1]){
    
    fcast_data$day10_UC <- day10_orig_UC
    fcast_data$day7_UC  <- day7_orig_UC
    fcast_data$day2_UC  <- day2_orig_UC
  }
  
})


observeEvent(input$Decision_Day2_UC, {
  if(input$Decision_Day2_UC==mgmt_choices[3]){
    fcast_data$data_treat[19:35, 2] <-  fcast_data$data_treat[19:35, 2]*decrease_2
    print(fcast_data$data_treat)
    print(decrease_2)
    
  }
})

# activity B forecast plots----
observe({
  fcast <- fcast_data$day14
  data <- fcast_data$data
  fcast$percent_over_35 <- NA
  fcast$percent_over_25 <- NA
  
  for (i in 2:nrow(fcast)) {
    number <-   length(which(fcast[i,6:30] > 35))
    fcast$percent_over_35[i] <- number/25*100
  }
  
  for (i in 2:nrow(fcast)) {
    number <-   length(which(fcast[i,6:30] > 25))
    fcast$percent_over_25[i] <- number/25*100
  }
  
  fc_plots$day14 <- ggplot()+
    geom_line(data = fcast, aes(date, percent_over_35, color = 'Forecast: Swimming Threshold')) +
    geom_line(data = fcast, aes(date, percent_over_25, color = 'Forecast: Drinking Threshold')) +
    scale_y_continuous(breaks = seq(0, 100, 10))+
    ylab("% Likelihood of Exceeding Threshold") +
    xlab("Date") +
    ylim(0, 100) +
    geom_vline(aes(xintercept = as.numeric(date_of_event), color = 'Day of Event'), size = 1.2) +
    #geom_vline(aes(xintercept = as.numeric(min(fcast$date)), col = 'Day of Forecast'), linetype = "dashed") +
    scale_color_manual(name = "", values = c( 'Forecast: Drinking Threshold' = objective_colors[1], 
                                              'Forecast: Swimming Threshold' = objective_colors[4],
                                             'Day of Event' = 'grey44'
                                             #'Day of Forecast' = 'black'
                                             )) +
    scale_x_date(breaks = c(as.Date('2021-05-24'), as.Date('2021-05-31'), as.Date('2021-06-06')), date_labels = '%b %d') +
    theme_classic(base_size = 15) +
    theme(panel.border = element_rect(fill = NA, colour = "black"), 
          axis.text.x = element_text(size = 15),
          plot.title = element_text(size = 20, hjust = 0.5),
          plot.caption = element_text(size = 15, hjust = 0),
          legend.text = element_text(size = 6))
  
 
  
  
})



output$forecast_plot_14 <- renderPlotly({
  p <- fc_plots$day14 
  return(ggplotly(p))
})


 output$forecast_plot_14_withUC <- renderPlotly({

   fcast <- fcast_data$day14_UC
   data <- fcast_data$data_UC
   
   p <- fc_plots$day14_UC <- ggplot()+
     geom_line(data = fcast, aes(date, mean, color = "Forecast Mean")) +
     #scale_y_continuous(breaks = seq(0, 100, 10))+
     #ylim(0, max(fcast$max) + 5) +
     xlim(min(fcast$date)-7, max(fcast$date)) +
     geom_point(data = data[data$date<=min(fcast$date),], aes(date, obs_chl_ugl, color = "Obs"), size = 4) +
     geom_hline(aes(yintercept = 35, col = 'Swimming Threshold'), size = 1.2) +
     geom_hline(aes(yintercept = 25, col = 'Drinking Threshold')) +
     geom_vline(aes(xintercept = as.numeric(min(fcast$date)), col = 'Day of Forecast'), linetype = "dashed") +
     geom_vline(aes(xintercept = as.numeric(date_of_event), color = 'Day of Event'), size = 1.2) +
     geom_ribbon(data = fcast_data$day14_UC, aes(date, ymin = min, ymax = max, fill = "95% Conf. Int."), alpha = 0.3) +
     scale_color_manual(name = "", values = c("Obs" ="#DAD4EF",  #"#721121",  #"#C3C3E6", BAD7F2
                                              'Forecast Mean' = 'black', 
                                              'Drinking Threshold' = objective_colors[1], 
                                              'Swimming Threshold' = objective_colors[4],
                                              'Day of Forecast' = 'black',
                                              'Day of Event' = 'grey44')) +
     scale_fill_manual(name = "", values = c("95% Conf. Int." = "#BFB5E3")) +     ylab("Chlorophyll-a (\U00B5g/L)") +
     xlab("Date") +
     theme_classic(base_size = 15) +
     theme(panel.border = element_rect(fill = NA, colour = "black"), 
           axis.text.x = element_text(size = 15),
           legend.text = element_text(size = 8),
           legend.title = element_text(size = 10))
   
   
   p <- ggplotly(p)
   
   for (i in 1:length(p$x$data)){
     if (!is.null(p$x$data[[i]]$name)){
       p$x$data[[i]]$name =  gsub("\\(","",str_split(p$x$data[[i]]$name,",")[[1]][1])
     }
   }
   
   return(p)
   })
 
 
 observe({
   fcast <- fcast_data$day10
   data <- fcast_data$data
   fcast$percent_over_35 <- NA
   fcast$percent_over_25 <- NA
   
   for (i in 2:nrow(fcast)) {
     number <-   length(which(fcast[i,6:30] > 35))
     fcast$percent_over_35[i] <- number/25*100
   }
   
   for (i in 2:nrow(fcast)) {
     number <-   length(which(fcast[i,6:30] > 25))
     fcast$percent_over_25[i] <- number/25*100
   }
   
   
   fc_plots$day10 <- ggplot()+
     geom_line(data = fcast, aes(date, percent_over_35, color = 'Forecast: Swimming Threshold')) +
     geom_line(data = fcast, aes(date, percent_over_25, color = 'Forecast: Drinking Threshold')) +
     scale_y_continuous(breaks = seq(0, 100, 10))+
     ylab("% Likelihood of Exceeding Threshold") +
     xlab("Date") +
     ylim(0, 100) +
     geom_vline(aes(xintercept = as.numeric(date_of_event), color = 'Day of Event'), size = 1.2) +
     #geom_vline(aes(xintercept = as.numeric(min(fcast$date)), col = 'Day of Forecast'), linetype = "dashed") +
     scale_color_manual(name = "", values = c( 'Forecast: Drinking Threshold' = objective_colors[1], 
                                               'Forecast: Swimming Threshold' = objective_colors[4],
                                               'Day of Event' = 'grey44'
                                               #'Day of Forecast' = 'black'
     )) +
     scale_x_date(breaks = c(as.Date('2021-05-28'), as.Date('2021-06-04'),  as.Date('2021-06-10')), date_labels = '%b %d') +
     theme_classic(base_size = 15) +
     theme(panel.border = element_rect(fill = NA, colour = "black"), 
           axis.text.x = element_text(size = 15),
           plot.title = element_text(size = 20, hjust = 0.5),
           plot.caption = element_text(size = 15, hjust = 0),
           legend.text = element_text(size = 6))
 


 })
 
 output$forecast_plot_10 <- renderPlotly({
   req(input$Decision_Day14)
   fcast <- fcast_data$day10

   p <- fc_plots$day10 

   return(ggplotly(p))
 })  
 
 
 
 output$forecast_plot_10_withUC <- renderPlotly({
   req(input$Decision_Day14_UC)

   fcast <- fcast_data$day10_UC
   data <- fcast_data$data_UC
   
   p <- fc_plots$day10_UC <- ggplot()+
     geom_line(data = fcast, aes(date, mean, color = "Forecast Mean")) +
     #scale_y_continuous(breaks = seq(0, 100, 10))+
     #ylim(0, max(fcast$max) + 5) +
     xlim(min(fcast$date)-7, max(fcast$date)) +
     geom_point(data = data[data$date<=min(fcast$date),], aes(date, obs_chl_ugl, color = "Obs"), size = 4) +
     geom_hline(aes(yintercept = 35, col = 'Swimming Threshold'), size = 1.2) +
     geom_hline(aes(yintercept = 25, col = 'Drinking Threshold')) +
     geom_vline(aes(xintercept = as.numeric(min(fcast$date)), col = 'Day of Forecast'), linetype = "dashed") +
     geom_vline(aes(xintercept = as.numeric(date_of_event), color = 'Day of Event'), size = 1.2) +
     geom_ribbon(data = fcast_data$day10_UC, aes(date, ymin = min, ymax = max, fill = "95% Conf. Int."), alpha = 0.3) +
     scale_color_manual(name = "", values = c("Obs" ="#DAD4EF",  #"#721121",  #"#C3C3E6", BAD7F2
                                              'Forecast Mean' = 'black', 
                                              'Drinking Threshold' = objective_colors[1], 
                                              'Swimming Threshold' = objective_colors[4],
                                              'Day of Forecast' = 'black',
                                              'Day of Event' = 'grey44')) +
     scale_fill_manual(name = "", values = c("95% Conf. Int." = "#BFB5E3")) +     ylab("Chlorophyll-a (\U00B5g/L)") +
     xlab("Date") +
     theme_classic(base_size = 15) +
     theme(panel.border = element_rect(fill = NA, colour = "black"), 
           axis.text.x = element_text(size = 15),
           legend.text = element_text(size = 8),
           legend.title = element_text(size = 10))
 
   
   
   if(input$Decision_Day14_UC==mgmt_choices[3]){
     p <- p +     
       geom_point(data = fcast_data$data_treat_UC[fcast_data$data_treat_UC$date==min(fcast$date),], aes(date, obs_chl_ugl, color = "Obs after treatment"), size = 4) +
       scale_color_manual(name = "", values = c("Obs" ="#DAD4EF",
                                                "Obs after treatment" = l.cols[3],
                                                'Forecast Mean' = 'black', 
                                                'Drinking Threshold' = objective_colors[1], 
                                                'Swimming Threshold' = objective_colors[4],
                                                'Day of Forecast' = 'black',
                                                'Day of Event' = 'grey44'))
     
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
   fcast <- fcast_data$day7
   data <- fcast_data$data
   fcast$percent_over_35 <- NA
   fcast$percent_over_25 <- NA
   
   for (i in 2:nrow(fcast)) {
     number <-   length(which(fcast[i,6:30] > 35))
     fcast$percent_over_35[i] <- number/25*100
   }
   
   for (i in 2:nrow(fcast)) {
     number <-   length(which(fcast[i,6:30] > 25))
     fcast$percent_over_25[i] <- number/25*100
   }
   
   fc_plots$day7 <- ggplot()+
     geom_line(data = fcast, aes(date, percent_over_35, color = 'Forecast: Swimming Threshold')) +
     geom_line(data = fcast, aes(date, percent_over_25, color = 'Forecast: Drinking Threshold')) +
     scale_y_continuous(breaks = seq(0, 100, 10))+
     ylab("% Likelihood of Exceeding Threshold") +
     xlab("Date") +
     ylim(0, 100) +
     geom_vline(aes(xintercept = as.numeric(date_of_event), color = 'Day of Event'), size = 1.2) +
     #geom_vline(aes(xintercept = as.numeric(min(fcast$date)), col = 'Day of Forecast'), linetype = "dashed") +
     scale_color_manual(name = "", values = c( 'Forecast: Drinking Threshold' = objective_colors[1], 
                                               'Forecast: Swimming Threshold' = objective_colors[4],
                                               'Day of Event' = 'grey44'
                                               #'Day of Forecast' = 'black'
     )) +
     scale_x_date(breaks = c(as.Date('2021-05-31'), as.Date('2021-06-06'), as.Date('2021-06-11')), date_labels = '%b %d') +
     theme_classic(base_size = 15) +
     theme(panel.border = element_rect(fill = NA, colour = "black"), 
           axis.text.x = element_text(size = 15),
           plot.title = element_text(size = 20, hjust = 0.5),
           plot.caption = element_text(size = 15, hjust = 0),
           legend.text = element_text(size = 6))
   
 })
 
 output$forecast_plot_7 <- renderPlotly({
   req(input$Decision_Day10)
   p <- fc_plots$day7 
   return(ggplotly(p))
 })
 
 
 output$forecast_plot_7_withUC <- renderPlotly({
   req(input$Decision_Day10_UC)
  # fcast <- fcast_data$day7
  # p <- fc_plots$day7 + geom_ribbon(data = fcast, aes(date, ymin = min, ymax = max, fill = "95% Conf. Int."), alpha = 0.3) +
  #   scale_color_manual(name = "", values = c("Obs" ="#DAD4EF", 
  #                                            'Forecast Mean' = 'black', 
  #                                            'Drinking Threshold' = objective_colors[1], 
  #                                            'Swimming Threshold' = objective_colors[4],
  #                                            'Day of Forecast' = 'black',
  #                                            'Day of Event' = 'grey44'))+     
  #   scale_fill_manual(name = "", values = c("95% Conf. Int." = "#DAD4EF")) +
  #   theme(legend.title = element_blank())
   
   fcast <- fcast_data$day7_UC
   data <- fcast_data$data_UC
   
   p <- fc_plots$day7_UC <- ggplot()+
     geom_line(data = fcast, aes(date, mean, color = "Forecast Mean")) +
     #scale_y_continuous(breaks = seq(0, 100, 10))+
     #ylim(0, max(fcast$max) + 5) +
     xlim(min(fcast$date)-7, max(fcast$date)) +
     geom_point(data = data[data$date<=min(fcast$date),], aes(date, obs_chl_ugl, color = "Obs"), size = 4) +
     geom_hline(aes(yintercept = 35, col = 'Swimming Threshold'), size = 1.2) +
     geom_hline(aes(yintercept = 25, col = 'Drinking Threshold')) +
     geom_vline(aes(xintercept = as.numeric(min(fcast$date)), col = 'Day of Forecast'), linetype = "dashed") +
     geom_vline(aes(xintercept = as.numeric(date_of_event), color = 'Day of Event'), size = 1.2) +
     geom_ribbon(data = fcast_data$day7_UC, aes(date, ymin = min, ymax = max, fill = "95% Conf. Int."), alpha = 0.3) +
     scale_color_manual(name = "", values = c("Obs" ="#DAD4EF",  #"#721121",  #"#C3C3E6", BAD7F2
                                              'Forecast Mean' = 'black', 
                                              'Drinking Threshold' = objective_colors[1], 
                                              'Swimming Threshold' = objective_colors[4],
                                              'Day of Forecast' = 'black',
                                              'Day of Event' = 'grey44')) +
     scale_fill_manual(name = "", values = c("95% Conf. Int." = "#BFB5E3")) +     ylab("Chlorophyll-a (\U00B5g/L)") +
     xlab("Date") +
     theme_classic(base_size = 15) +
     theme(panel.border = element_rect(fill = NA, colour = "black"), 
           axis.text.x = element_text(size = 15),
           legend.text = element_text(size = 8),
           legend.title = element_text(size = 10))
   
   if(input$Decision_Day14_UC==mgmt_choices[3] | input$Decision_Day10_UC==mgmt_choices[3]){
     p <- p +     
       geom_point(data = fcast_data$data_treat_UC[fcast_data$data_treat_UC$date==min(fcast$date),], aes(date, obs_chl_ugl, color = "Obs after treatment"), size = 4) +
       scale_color_manual(name = "", values = c("Obs" ="#DAD4EF",
                                                "Obs after treatment" = l.cols[3],
                                                'Forecast Mean' = 'black', 
                                                'Drinking Threshold' = objective_colors[1], 
                                                'Swimming Threshold' = objective_colors[4],
                                                'Day of Forecast' = 'black',
                                                'Day of Event' = 'grey44'))
     
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
   fcast <- fcast_data$day2
   data <- fcast_data$data
   fcast$percent_over_35 <- NA
   fcast$percent_over_25 <- NA
   
   for (i in 2:nrow(fcast)) {
     number <-   length(which(fcast[i,6:30] > 35))
     fcast$percent_over_35[i] <- number/25*100
   }
   
   for (i in 2:nrow(fcast)) {
     number <-   length(which(fcast[i,6:30] > 25))
     fcast$percent_over_25[i] <- number/25*100
   }
   
   
   fc_plots$day2 <- ggplot()+
     geom_line(data = fcast, aes(date, percent_over_35, color = 'Forecast: Swimming Threshold')) +
     geom_line(data = fcast, aes(date, percent_over_25, color = 'Forecast: Drinking Threshold')) +
     scale_y_continuous(breaks = seq(0, 100, 10))+
     ylab("% Likelihood of Exceeding Threshold") +
     xlab("Date") +
     ylim(0, 100) +
     geom_vline(aes(xintercept = as.numeric(date_of_event), color = 'Day of Event'), size = 1.2) +
     #geom_vline(aes(xintercept = as.numeric(min(fcast$date)), col = 'Day of Forecast'), linetype = "dashed") +
     scale_color_manual(name = "", values = c( 'Forecast: Drinking Threshold' = objective_colors[1], 
                                               'Forecast: Swimming Threshold' = objective_colors[4],
                                               'Day of Event' = 'grey44'
                                               #'Day of Forecast' = 'black'
     )) +
     scale_x_date(breaks = c(as.Date('2021-06-04'), as.Date('2021-06-11'), as.Date('2021-06-18')), date_labels = '%b %d') +
     theme_classic(base_size = 15) +
     theme(panel.border = element_rect(fill = NA, colour = "black"), 
           axis.text.x = element_text(size = 15),
           plot.title = element_text(size = 20, hjust = 0.5),
           plot.caption = element_text(size = 15, hjust = 0),
           legend.text = element_text(size = 6))
   
 })
 
 output$forecast_plot_2 <- renderPlotly({
   req(input$Decision_Day7)
   fcast <- fcast_data$day2
   
    p <- fc_plots$day2 

   return(ggplotly(p))
 })
 
 
 output$forecast_plot_2_withUC <- renderPlotly({
   req(input$Decision_Day7_UC)
   
   fcast <- fcast_data$day2_UC
   data <- fcast_data$data_UC
   
   p <- fc_plots$day2_UC <- ggplot()+
     geom_line(data = fcast, aes(date, mean, color = "Forecast Mean")) +
     #scale_y_continuous(breaks = seq(0, 100, 10))+
     #ylim(0, max(fcast$max) + 5) +
     xlim(min(fcast$date)-7, max(fcast$date)) +
     geom_point(data = data[data$date<=min(fcast$date),], aes(date, obs_chl_ugl, color = "Obs"), size = 4) +
     geom_hline(aes(yintercept = 35, col = 'Swimming Threshold'), size = 1.2) +
     geom_hline(aes(yintercept = 25, col = 'Drinking Threshold')) +
     geom_vline(aes(xintercept = as.numeric(min(fcast$date)), col = 'Day of Forecast'), linetype = "dashed") +
     geom_vline(aes(xintercept = as.numeric(date_of_event), color = 'Day of Event'), size = 1.2) +
     geom_ribbon(data = fcast_data$day2_UC, aes(date, ymin = min, ymax = max, fill = "95% Conf. Int."), alpha = 0.3) +
     scale_color_manual(name = "", values = c("Obs" ="#DAD4EF",  #"#721121",  #"#C3C3E6", BAD7F2
                                              'Forecast Mean' = 'black', 
                                              'Drinking Threshold' = objective_colors[1], 
                                              'Swimming Threshold' = objective_colors[4],
                                              'Day of Forecast' = 'black',
                                              'Day of Event' = 'grey44')) +
     scale_fill_manual(name = "", values = c("95% Conf. Int." = "#BFB5E3")) +     ylab("Chlorophyll-a (\U00B5g/L)") +
     xlab("Date") +
     theme_classic(base_size = 15) +
     theme(panel.border = element_rect(fill = NA, colour = "black"), 
           axis.text.x = element_text(size = 15),
           legend.text = element_text(size = 8),
           legend.title = element_text(size = 10))
   
   if(input$Decision_Day14_UC==mgmt_choices[3] | input$Decision_Day10_UC==mgmt_choices[3] | input$Decision_Day7_UC==mgmt_choices[3]){
     p <- p +     
       geom_point(data = fcast_data$data_treat_UC[fcast_data$data_treat_UC$date==min(fcast$date),], aes(date, obs_chl_ugl, color = "Obs after treatment"), size = 4) +
       scale_color_manual(name = "", values = c("Obs" ="#DAD4EF",
                                                "Obs after treatment" = l.cols[3],
                                                'Forecast Mean' = 'black', 
                                                'Drinking Threshold' = objective_colors[1], 
                                                'Swimming Threshold' = objective_colors[4],
                                                'Day of Forecast' = 'black',
                                                'Day of Event' = 'grey44'))
     
   }
   p <- ggplotly(p)
   
   for (i in 1:length(p$x$data)){
     if (!is.null(p$x$data[[i]]$name)){
       p$x$data[[i]]$name =  gsub("\\(","",str_split(p$x$data[[i]]$name,",")[[1]][1])
     }
   }
   
   return(p)
   })
 
 
 
 observeEvent(input$Decision_Day14, {
   
   if(input$Decision_Day14 == "B) Cancel the event"){
     updateRadioButtons(session, "Decision_Day10", selected = mgmt_choices[2])
     updateRadioButtons(session, "Decision_Day7", selected =  mgmt_choices[2])
     updateRadioButtons(session, "Decision_Day2", selected =  mgmt_choices[2])
     
     disable("Decision_Day10")
     disable("Decision_Day7")
     disable("Decision_Day2")
   }else{
     enable("Decision_Day10")
     enable("Decision_Day7")
     enable("Decision_Day2")
     
   }

 })
 
 observeEvent(input$Decision_Day10, {
   
   if(input$Decision_Day10 == "B) Cancel the event"){
     updateRadioButtons(session, "Decision_Day7", selected =  mgmt_choices[2])
     updateRadioButtons(session, "Decision_Day2", selected =  mgmt_choices[2])
     
     disable("Decision_Day7")
     disable("Decision_Day2")
   }else{
     enable("Decision_Day7")
     enable("Decision_Day2")
     
   }
   
 })
 
 observeEvent(input$Decision_Day7, {
   
   if(input$Decision_Day7 == "B) Cancel the event"){
     updateRadioButtons(session, "Decision_Day2", selected =  mgmt_choices[2])
     
     disable("Decision_Day2")
   }else{
     enable("Decision_Day2")
     
   }
   
 })
 
 observeEvent(input$Decision_Day14_UC, {
   
   if(input$Decision_Day14_UC == "B) Cancel the event"){
     updateRadioButtons(session, "Decision_Day10_UC", selected = mgmt_choices[2])
     updateRadioButtons(session, "Decision_Day7_UC", selected =  mgmt_choices[2])
     updateRadioButtons(session, "Decision_Day2_UC", selected =  mgmt_choices[2])
     
     disable("Decision_Day10_UC")
     disable("Decision_Day7_UC")
     disable("Decision_Day2_UC")
   }else{
     enable("Decision_Day10_UC")
     enable("Decision_Day7_UC")
     enable("Decision_Day2_UC")
     
   }
   
 })
 
 observeEvent(input$Decision_Day10_UC, {
   
   if(input$Decision_Day10_UC == "B) Cancel the event"){
     updateRadioButtons(session, "Decision_Day7_UC", selected =  mgmt_choices[2])
     updateRadioButtons(session, "Decision_Day2_UC", selected =  mgmt_choices[2])
     
     disable("Decision_Day7_UC")
     disable("Decision_Day2_UC")
   }else{
     enable("Decision_Day7_UC")
     enable("Decision_Day2_UC")
     
   }
   
 })
 
 observeEvent(input$Decision_Day7_UC, {
   
   if(input$Decision_Day7_UC == "B) Cancel the event"){
     updateRadioButtons(session, "Decision_Day2_UC", selected =  mgmt_choices[2])
     
     disable("Decision_Day2_UC")
   }else{
     enable("Decision_Day2_UC")
     
   }
   
 })
 
 
 decision_data <- reactive({
  data <- data.frame(
    day = c(as.Date('2021-05-23'), as.Date('2021-05-27'), as.Date('2021-05-30'), as.Date('2021-06-04')),
    choice_noUC = NA,
    choice_withUC = NA,
    binary_noUC = NA,
    binary_withUC = NA)
  
  data$choice_noUC <- c(input$Decision_Day14,
                        input$Decision_Day10,
                        input$Decision_Day7,
                        input$Decision_Day2)
  data$choice_withUC <- c(input$Decision_Day14_UC,
                          input$Decision_Day10_UC,
                          input$Decision_Day7_UC,
                          input$Decision_Day2_UC)
  
  for (i in 1:nrow(data)) {
    if(data$choice_withUC[i]==mgmt_choices[1]){
      data$binary_withUC[i] <- 0
    }else if(data$choice_withUC[i]==mgmt_choices[2]){
      data$binary_withUC[i] <- 1
    }else if(data$choice_withUC[i]==mgmt_choices[3]){
      data$binary_withUC[i] <- 0.5
    }
  }
    
  for (i in 1:nrow(data)) {
    if(data$choice_noUC[i]==mgmt_choices[1]){
      data$binary_noUC[i] <- 0.02
    }else if(data$choice_noUC[i]==mgmt_choices[2]){
      data$binary_noUC[i] <- 0.98
    }else if(data$choice_noUC[i]==mgmt_choices[3]){
      data$binary_noUC[i] <- 0.62
    }
  }
  

  return(data)
})
 
 decisions <- reactiveValues(plot = NULL)
 
output$WQ_decisions <- renderPlotly({
  validate(
    need(input$Decision_Day2_UC!="", "Please complete your decisions in Objective 4a and 4b"))
  
  #req(input$Decision_Day2_UC)
  
  decisions$plot <- ggplot(data = decision_data()) +
    geom_hline(yintercept = c(0, 0.5, 1), color = 'white') +
    geom_point(aes(x = day, y = binary_noUC, color = "Objective 4a", position = 'jitter'), size = 4) +
    geom_point(aes(x = day, y = binary_withUC, color = "Objective 4b", position = 'jitter'), size = 4) +
    scale_y_continuous(breaks = c(0,0.5, 1), labels = c('Continue', 'Treat', 'Cancel')) +
    ylab("Decision") +
    xlab("Date") +
    scale_x_date(breaks = c(as.Date('2021-05-23'), as.Date('2021-05-27'), as.Date('2021-05-30'), as.Date('2021-06-04')), date_labels = '%b %d') +
    scale_color_manual(name = "", values = c("Objective 4a" = cols[5], "Objective 4b" = cols[3]))+
    theme_classic(base_size = 15) +
    theme(panel.border = element_rect(fill = NA, colour = "black"), 
          axis.text = element_text(size = 10),
          axis.text.y = element_text(angle = 90, hjust = 0.7),
          legend.text = element_text(size = 8),
          legend.title = element_text(size = 10))
  
  return(ggplotly(decisions$plot))
})

observeEvent(input$save_decision_plot, {
 # validate(
#    need(input$create_plot > 0, "Please click 'Create custom plot'")
#  )
  
  # Progress bar
  progress <- shiny::Progress$new()
  on.exit(progress$close())
  progress$set(message = "Saving plot as image file for the report.", 
               detail = "This may take a while. This window will disappear  
                     when it is downloaded.", value = 0.5)
  
  p <-    ggplot(data = decision_data()) +
    geom_hline(yintercept = c(0, 0.5, 1), color = 'white') +
    geom_point(aes(x = day, y = binary_noUC, color = "Without Uncertainty", position = 'jitter'), size = 12) +
    geom_point(aes(x = day, y = binary_withUC, color = "With Uncertainty", position = 'jitter'), size = 12) +
    scale_y_continuous(breaks = c(0,0.5, 1), labels = c('Continue', 'Treat', 'Cancel')) +
    ylab("Decision") +
    xlab("Date") +
    scale_x_date(breaks = c(as.Date('2021-05-23'), as.Date('2021-05-27'), as.Date('2021-05-30'), as.Date('2021-06-04')), date_labels = '%b %d') +
    scale_color_manual(name = "", values = c("Without Uncertainty" = cols[5], "With Uncertainty" = cols[3]))+
    theme_classic(base_size = 35) +
    theme(panel.border = element_rect(fill = NA, colour = "black"), 
          axis.text = element_text(size = 30),
          axis.text.y = element_text(angle = 90, hjust = 0.7),
          legend.text = element_text(size = 30),
          legend.title = element_text(size = 30))
  img_file <- "www/decision_plot.png"
  ggsave(img_file, p, dpi = 300, width = 520, height = 380, units = "mm")
  progress$set(value = 1)
  
  
  
})

  
output$forecast_final <- renderPlotly({
  validate(need(input$Decision_Day2!="", 'Please finish your decisions in Objective 4a'))
 
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
  
#fcast_14 <- fcast_data$day14
#fcast_10 <- fcast_data$day10
#fcast_10 <- fcast_10[fcast_10$date<=date_of_event+2,]
#fcast_7 <- fcast_data$day7
#fcast_7 <- fcast_7[fcast_7$date<=date_of_event+2,]
#fcast_2 <- fcast_data$day2
#fcast_2 <- fcast_2[fcast_2$date<=date_of_event+2,]
#data <- fcast_data$data
#data <- data[data$date<=date_of_event+2,]
data_treat <- fcast_data$data_treat
data_treat <- data_treat[data_treat$date==date_of_event,]
##data_treat_UC <- fcast_data$data_treat_UC
#print(fcast_7$min)
#print(fcast_7$mean)
#print(fcast_7$max)

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
    scale_color_manual(values = c("14-day mean" = cols[1], 
                                  "10-day mean" = cols[5], 
                                  "7-day mean" = cols[3], 
                                  "2-day mean" = cols[4], 
                                  "Obs" = cols[6])) +
    scale_fill_manual(values = c("14-day" = cols[1], 
                                 "10-day" = cols[5], 
                                 "7-day" = cols[3], 
                                 "2-day" = cols[4])) +
    geom_point(data = data, aes(date, obs_chl_ugl, color = "Obs"), size = 2.5) +
    geom_vline(xintercept = as.numeric(date_of_event), color = 'grey44', size = 1.3) +
    ylab("Chlorophyll-a (\U00B5g/L)") +
    xlab("Date") +
    theme_classic(base_size = 15) +
    theme(panel.border = element_rect(fill = NA, colour = "black"), 
          axis.text.x = element_text(size = 15),
          legend.text = element_text(size = 8),
          legend.title = element_blank())
  
  
 if(input$Decision_Day14==mgmt_choices[3] | input$Decision_Day10==mgmt_choices[3] | input$Decision_Day7==mgmt_choices[3] | input$Decision_Day2==mgmt_choices[3]){
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
     geom_vline(xintercept = as.numeric(date_of_event), color = 'grey44', size = 1.3) +
     geom_point(data = data, aes(date, obs_chl_ugl, color = "Obs"), size = 2.5) +
     geom_point(data = data_treat, aes(date, obs_chl_ugl, color = "Obs on June 6 after Treatment"), size = 2.5) +
     scale_color_manual(values = c("14-day mean" = cols[1], 
                                   "10-day mean" = cols[5], 
                                   "7-day mean" = cols[3], 
                                   "2-day mean" = cols[4], 
                                   "Obs" = cols[6],
                                   "Obs on June 6 after Treatment" = l.cols[3])) +
     scale_fill_manual(values = c("14-day" = cols[1], 
                                  "10-day" = cols[5], 
                                  "7-day" = cols[3], 
                                  "2-day" = cols[4])) +
      ylab("Chlorophyll-a (\U00B5g/L)") +
     xlab("Date") +
     theme_classic(base_size = 15) +
     theme(panel.border = element_rect(fill = NA, colour = "black"), 
           axis.text.x = element_text(size = 15),
           legend.text = element_text(size = 8),
           legend.title = element_blank())
   
   
    #geom_point(data = data_treat, aes(date, obs_chl_ugl, color = "Obs after Treatment (Obj4b)"), size = 2.5) 
 }else{
   final_plot <- final_plot
 }
  
  final_plot <- ggplotly(final_plot)
  
  for (i in 1:length(final_plot$x$data)){
    if (!is.null(final_plot$x$data[[i]]$name)){
      final_plot$x$data[[i]]$name =  gsub("\\(","",str_split(final_plot$x$data[[i]]$name,",")[[1]][1])
    }
  }
  
  
  
  return(final_plot)
})
  

#output activity c---- 
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
   validate(need(input$stakeholder!="", "Please select a stakeholder"))
   #req(input$stakeholder!="") 
   stakeholder_id <-  which(stakeholder_info$stakeholder_selected == input$stakeholder)
         filename <- normalizePath(file.path('./www', paste0(stakeholder_info[stakeholder_id,2])))
         print(filename)
         list(src = filename,
              width = '100%',
              height = '70%',
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

output$stakeholder_name_2 <- renderUI({
  validate(need(input$stakeholder!="", "Please select a stakeholder in Objective 6"))
   stakeholder_id <-  which(stakeholder_info$stakeholder_selected == input$stakeholder)
   HTML(paste0("<b>", stakeholder_info[stakeholder_id,6], "<b>"))
})

output$stakeholder_decision <- renderText({
  validate(need(input$q22!="", "Please identify a decision for your stakeholder in Objective 6"))
  paste0('Your stakeholder decision is: ', input$q22)
})


output$stakeholder_pic_2 <- renderImage({
   stakeholder_id <-  which(stakeholder_info$stakeholder_selected == input$stakeholder)
   filename <- normalizePath(file.path('./www', paste0(stakeholder_info[stakeholder_id,2])))
   print(filename)
   list(src = filename,
        width = '70%',
        height = '50%',
        alt = 'error loading file')
   
}, deleteFile = FALSE)

fcast <- reactive({
  fcast <- read.csv("data/wq_forecasts/forecast_day14.csv")
  fcast$date <- as.Date(fcast$date)
  fcast[,2:30] <- round(fcast[,2:30], digits = 2)
  fcast <- fcast[,-c(31, 32, 33)]
  return(fcast)
})

output$fcast_table <- DT::renderDataTable({
  fcast()[-1,-c(2, 3, 4, 5)]}, 
  options = list(scrollX = TRUE))
 
date_reactive <- reactiveValues(date = NULL)

observe({
  date_reactive$date <- input$forecast_viz_date
})

output$date_selected_calcs <- renderText({
  paste0("You have selected: ", date_reactive$date)
})

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


  

   cust_plot <- reactiveValues(plot = NULL)
   
   observeEvent(input$index_raw, {
     cust_plot$plot <- NULL
   })
   observeEvent(input$summ_comm_type, {
     cust_plot$plot <- NULL
   })
   observeEvent(input$summ_plot_type, {
     cust_plot$plot <- NULL
   })
   observeEvent(input$raw_comm_type, {
     cust_plot$plot <- NULL
   })
   observeEvent(input$raw_plot_type, {
     cust_plot$plot <- NULL
   })
   observeEvent(input$ts_line_type, {
     cust_plot$plot <- NULL
   })
   
   wrapper <- function(label, dev_width = dev.size("in")[1], dev_scaler = 7)  {   
     paste(strwrap(label, dev_width * dev_scaler), collapse = "\n") 
   }
   
   observeEvent(input$create_plot, {
     req(input$index_raw != "")
       if(input$index_raw=='Forecast index'){
         req(input$summ_comm_type)
         if(input$summ_comm_type=='Word'){
           fcast <- read.csv("data/wq_forecasts/forecast_day14.csv")
           fcast$date <- as.Date(fcast$date)
           
           # index, word
           fcast$percent_over_35 <- NA
           
           for (i in 2:nrow(fcast)) {
             number <-   length(which(fcast[i,6:30] > 35))
             fcast$percent_over_35[i] <- number/25*100
           }
           
           # define low, medium, and high risk categories
           # low = 0-30
           # medium = 31-60
           # high = >60
           
           fcast$word <- NA
           for (i in 2:nrow(fcast)) {
             if(fcast$percent_over_35[i]<=30){
               fcast$word[i] <- 'Low'
             }else if(fcast$percent_over_35[i]>31){
               fcast$word[i] <- 'Medium'
             }else if(fcast$percent_over_35[i]>=61){
               fcast$word[i] <- 'High'
             }
             
           }
           p1 <- ggplot(data = fcast, aes(x = date[1], y = obs_chl_ugl[1])) +
             geom_label(aes(label = paste0(fcast[15, ncol(fcast)], ' Chance of \n Algal Bloom'), x = date[1] + 0.5), size = 20) +
             labs(title = wrapper(input$figure_title), caption = wrapper(input$figure_caption)) +
             theme(legend.position = 'none',
                   panel.background = element_rect(fill = NA, color = 'black'),
                   panel.border = element_rect(color = 'black', fill = NA),
                   axis.text = element_blank(),
                   axis.title = element_blank(),
                   axis.ticks = element_blank(),
                   plot.title = element_text(size = 30, hjust = 0.5),
                   plot.caption = element_text(size = 15, hjust = 0))
           cust_plot$plot <- p1
         }
         if(input$summ_comm_type=='Number'){
           fcast <- read.csv("data/wq_forecasts/forecast_day14.csv")
           fcast$date <- as.Date(fcast$date)
           fcast$percent_over_35 <- NA
           
           for (i in 2:nrow(fcast)) {
             number <-   length(which(fcast[i,6:30] > 35))
             fcast$percent_over_35[i] <- number/25*100
           }
           
           p2 <-  ggplot(data = fcast, aes(x = date[1], y = obs_chl_ugl[1])) +
             geom_label(aes(label = paste0(fcast[15,ncol(fcast)], '% chance of \n Algal Bloom'), x = date[1] + 0.5), size = 20) +
             labs(title = wrapper(input$figure_title), caption = wrapper(input$figure_caption)) +
             theme(legend.position = 'none',
                   panel.background = element_rect(fill = NA, color = 'black'),
                   panel.border = element_rect(color = 'black', fill = NA),
                   axis.text = element_blank(),
                   axis.title = element_blank(),
                   axis.ticks = element_blank(),
                   plot.title = element_text(size = 30, hjust = 0.5),
                   plot.caption = element_text(size = 15, hjust = 0))
           cust_plot$plot <- p2
         }
         if(input$summ_comm_type=='Icon'){
           fcast <- read.csv("data/wq_forecasts/forecast_day14.csv")
           fcast$date <- as.Date(fcast$date)
           fcast$percent_over_35 <- NA
           
           for (i in 2:nrow(fcast)) {
             number <-   length(which(fcast[i,6:30] > 35))
             fcast$percent_over_35[i] <- number/25*100
           }
           
           dial <- plot_ly(
             domain = list(x = c(0, 1), y = c(0, 1)),
             value = fcast[15, ncol(fcast)],
             title = list(text = wrapper(paste0("Likelihood of Algal Bloom ", input$figure_title))),
             type = "indicator",
             mode = "gauge+number+delta",
             gauge = list(
               axis =list(range = list(NULL, 100)),
               bar = list(color = 'black'),
               steps = list(
                 list(range = c(0, 30), color = "green"),
                 list(range = c(30, 60), color = "yellow"),
                 list(range = c(60, 100), color = "red"))))    
           dial <- dial %>% layout(margin = list(l=20,r=30, t = 100))
           cust_plot$plot <- dial
         }
         if(input$summ_comm_type=='Figure'){
           req(input$summ_plot_type)
           if(input$summ_plot_type=='Pie'){
             fcast <- read.csv("data/wq_forecasts/forecast_day14.csv")
             fcast$date <- as.Date(fcast$date)
             fcast <- fcast[15,]
             fcast <- fcast %>% select(date, ens_1:ens_25) %>% 
               gather(key = ensemble, value = forecast, ens_1:ens_25)
             # calculate percent that are 0-25 ugL, 25-35 ugL, and >35ugL
             percents <- data.frame(range = c('0-25 \U00B5g/L', '25-35 \U00B5g/L', '>35 \U00B5g/L'),
                                    percent = NA)
             
             percents[1,2] = mean(fcast$forecast <25)*100
             percents[2,2] <-  mean(fcast$forecast >25 & fcast$forecast<35)*100
             percents[3,2] <-  mean(fcast$forecast >35)*100
             percents$range <- as.factor(percents$range)
             
             p_pie <-  ggplot(percents, aes(x="", y=percent, fill=range)) +
               geom_bar(stat="identity", width=1, color="white") +
                scale_fill_manual(name = 'legend', values = c('0-25 \U00B5g/L' = 'forestgreen', '25-35 \U00B5g/L' = 'goldenrod2', '>35 \U00B5g/L' = 'red3')) +
               coord_polar("y", start=0) +
               labs(title = wrapper(paste0("Percent Likelihood of Algal Concentrations in Each Category \n", input$figure_title)), 
                    caption = wrapper(input$figure_caption)) +
               theme_void() # remove background, grid, numeric labels
             
             cust_plot$plot <- p_pie
           }
           if(input$summ_plot_type=='Time series'){
             fcast <- read.csv("data/wq_forecasts/forecast_day14.csv")
             fcast$date <- as.Date(fcast$date)
             fcast$percent_over_35 <- NA
             
             for (i in 2:nrow(fcast)) {
               number <-   length(which(fcast[i,6:30] > 35))
               fcast$percent_over_35[i] <- number/25*100
             }
             
             p_index_ts <- ggplot()+
               geom_line(data = fcast, aes(date, percent_over_35), size = 2) +
               scale_y_continuous(breaks = seq(0, 100, 10))+
               ylab("% Likelihood of Algal Bloom") +
               xlab("Date") +
               labs(title = wrapper(paste0("Time Series leading up to June 6 Forecast \n", input$figure_title)), 
                    caption = wrapper(input$figure_caption)) +
               theme_classic(base_size = 24) +
               theme(panel.border = element_rect(fill = NA, colour = "black"), 
                     axis.text.x = element_text(size = 24),
                     legend.position = 'none',
                     plot.title = element_text(size = 30, hjust = 0.5),
                     plot.caption = element_text(size = 15, hjust = 0))
             cust_plot$plot <- p_index_ts
           } # this one is messed up
           if(input$summ_plot_type=='Bar graph'){
             fcast <- read.csv("data/wq_forecasts/forecast_day14.csv")
             fcast$date <- as.Date(fcast$date)
             fcast <- fcast[15,]
             fcast <- fcast %>% select(date, ens_1:ens_25) %>% 
               gather(key = ensemble, value = forecast, ens_1:ens_25)
             
             # calculate percent that are 0-25 ugL, 25-35 ugL, and >35ugL
             percents <- data.frame(range = c('0-25 \U00B5g/L', '25-35 \U00B5g/L', '>35 \U00B5g/L'),
                                    percent = NA)
             percents[1,2] <-  mean(fcast$forecast <25)*100
             percents[2,2] <-  mean(fcast$forecast >25 & fcast$forecast<35)*100
             percents[3,2] <-  mean(fcast$forecast >35)*100
             
             order <-  c('0-25 \U00B5g/L', '25-35 \U00B5g/L', '>35 \U00B5g/L')
            
             p_index_bar <- ggplot(data = percents, aes(range, percent, fill = range)) +
               geom_bar(stat = 'identity') +
               scale_x_discrete(limits = order) +
               labs(title = wrapper(input$figure_title), caption = wrapper(input$figure_caption)) +
               scale_fill_manual(name = 'legend', values = c('0-25 \U00B5g/L' = 'forestgreen', '25-35 \U00B5g/L' = 'goldenrod2', '>35 \U00B5g/L' = 'red3')) +
               ylab('% Likelihood of Algal Concentration') +
               xlab('Range of Algal Concentration') +
               theme(legend.position = 'none',
                     panel.background = element_rect(fill = NA, color = 'black'),
                     panel.border = element_rect(color = 'black', fill = NA),
                     plot.title = element_text(size = 30, hjust = 0.5),
                     plot.caption = element_text(size = 15, hjust = 0))
             
             cust_plot$plot <- p_index_bar
           }
         }
       }
       if(input$index_raw=='Forecast output'){
         req(input$raw_comm_type)
         if(input$raw_comm_type=='Number'){
           fcast <- read.csv("data/wq_forecasts/forecast_day14.csv")
           fcast$date <- as.Date(fcast$date)
           fcast <- fcast[15,]
           
           p_raw_number <- ggplot(data = fcast, aes(x = date, y = mean)) +
             geom_label(aes(label = paste0("The forecasted \n algal concentration is \n ", round(mean, 1), ' +/-', round(min, 2), ' \U00B5g/L'), x =date+ 0.5), size = 12) +
             labs(title = wrapper(input$figure_title), caption = wrapper(input$figure_caption)) +
             theme(legend.position = 'none',
                   panel.background = element_rect(fill = NA, color = 'black'),
                   panel.border = element_rect(color = 'black', fill = NA),
                   axis.text = element_blank(),
                   axis.title = element_blank(),
                   axis.ticks = element_blank(),
                   plot.title = element_text(size = 30, hjust = 0.5),
                   plot.caption = element_text(size = 15, hjust = 0))
           cust_plot$plot <- p_raw_number
         }
         if(input$raw_comm_type=='Figure'){
           req(input$raw_plot_type)

           if(input$raw_plot_type=='Bar graph'){
             # visualizing just the last horizon of the forecast
             fcast <- read.csv("data/wq_forecasts/forecast_day14.csv")
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
               xlab('Predicted Algal Concentration (\U00B5g/L)') +
               labs(title = wrapper(paste0("June 6 Forecast \n", input$figure_title)), caption = wrapper(input$figure_caption)) +
               theme(
                 panel.background = element_rect(fill = NA, color = 'black'),
                 panel.border = element_rect(color = 'black', fill = NA),
                 plot.title = element_text(size = 30, hjust = 0.5),
                 plot.caption = element_text(size = 15, hjust = 0))
             cust_plot$plot <- p_bar_raw
           }
           if(input$raw_plot_type=='Time series'){
             req(input$ts_line_type)
             fcast <- read.csv("data/wq_forecasts/forecast_day14.csv")
             fcast$date <- as.Date(fcast$date)
             data <- read.csv("data/wq_forecasts/mock_chl_obs.csv")
             data$date <- as.Date(data$date)
             
             p_raw_ts_confidence_interval <- ggplot()+
               geom_line(data = fcast(), aes(date, mean)) +
               scale_y_continuous(breaks = seq(0, 100, 10))+
               xlim(min(fcast()$date)-7, max(fcast()$date)) +
               geom_point(data = data[data$date<=min(fcast()$date),], aes(date, obs_chl_ugl), color = l.cols[3], size = 4) +
               geom_ribbon(data = fcast(), aes(date, ymin = min, ymax = max), fill = l.cols[3], alpha = 0.3) +
               geom_vline(xintercept = as.Date(min(fcast()$date)), linetype = "dashed") +
               geom_vline(xintercept = as.Date(date_of_event), color = 'grey44', size = 2) +
               ylab("Chlorophyll-a (\U00B5g/L)") +
               xlab("Date") +
               labs(title = wrapper(paste0("Time series leading up to June 6 Forecast \n", input$figure_title)), 
                    caption = wrapper(input$figure_caption)) +
               theme_classic(base_size = 24) +
               theme(panel.border = element_rect(fill = NA, colour = "black"), 
                     axis.text.x = element_text(size = 22),
                     legend.position = 'none',
                     plot.margin = margin(t = 0, r = 3, b = 0, l = 0, unit = 'cm'))
             
             fcast<- fcast %>% select(date, ens_1:ens_25) %>% 
               gather(key = ensemble, value = forecast, ens_1:ens_25, -date)
             
             p_raw_ts_ens <- ggplot()+
               geom_line(data = fcast, aes(date, forecast, group = ensemble), color = l.cols[3], size = 0.8) +
               scale_y_continuous(breaks = seq(0, 100, 10))+
               xlim(min(fcast()$date)-7, max(fcast$date)) +
               geom_point(data = data[data$date<=min(fcast$date),], aes(date, obs_chl_ugl), color = l.cols[3], size = 4) +
               geom_vline(xintercept = as.Date(min(fcast$date)), linetype = "dashed") +
               geom_vline(xintercept = as.Date(date_of_event), color = 'grey44', size = 2) +
               ylab("Chlorophyll-a (\U00B5g/L)") +
               xlab("Date") +
               labs(title = wrapper(paste0("Time series leading up to June 6 Forecast \n", input$figure_title)), 
                    caption = wrapper(input$figure_caption)) +
               theme_classic(base_size = 22) +
               theme(panel.border = element_rect(fill = NA, colour = "black"), 
                     axis.text.x = element_text(size = 20),
                     legend.position = 'none',
                     plot.title = element_text(size = 30, hjust = 0.5),
                     plot.caption = element_text(size = 15, hjust = 0),
                     plot.margin = margin(t = 0, r = 3, b = 0, l = 0, unit = 'cm'))
             
            p_raw_ts_boxplot <-   ggplot(data = fcast) +
              geom_boxplot(aes(x = as.factor(date), y = forecast)) +
              ylab("Chlorophyll-a (\U00B5g/L)") +
              xlab("Date") +
              labs(title = wrapper(paste0("Time series leading up to June 6 Forecast \n", input$figure_title)), 
                   caption = wrapper(input$figure_caption)) +
              theme_classic(base_size = 24) +
              theme(panel.border = element_rect(fill = NA, colour = "black"), 
                    axis.text.x = element_text(size = 22),
                    legend.position = 'none',
                    plot.title = element_text(size = 30, hjust = 0.5),
                    plot.caption = element_text(size = 15, hjust = 0)) +
              scale_x_discrete(breaks = c('2021-05-24', '2021-05-29', '2021-06-02', '2021-06-06'),
                               labels = c('2021-05-24' = 'May 24', '2021-05-29' = 'May 29',  '2021-06-02' = 'Jun 02', '2021-06-06' = 'Jun 06'))
            
             if(input$ts_line_type=='Line'){
               cust_plot$plot <- p_raw_ts_ens
               
             }
             if(input$ts_line_type=='Confidence Interval'){
               cust_plot$plot <- p_raw_ts_confidence_interval
               
             }
             if(input$ts_line_type=='Boxplot'){
               cust_plot$plot <- p_raw_ts_boxplot
             }
           }
         }
       }
     
     
     
   })
  # cust_plot_2 <- cust_plot #this one shows up on the next tab
  
  output$custom_plot <- renderPlot({
    validate(
      need(input$index_raw != "", "Please select 'Forecast index' or 'Forecast output'")
    ) 
    if(input$index_raw == "Forecast index") {
      validate(
        need(input$summ_comm_type != "", "Please select a communication type")
      )
      if(input$summ_comm_type=='Figure'){
        validate(
          need(input$summ_plot_type != "", "Please select a plot type")
        )
      }
      
    }
   if(input$index_raw=='Forecast output'){
     validate(
       need(input$raw_comm_type != "", "Please select a communication type")
     ) 
     if(input$raw_comm_type=='Figure'){
       validate(
         need(input$raw_plot_type!= "", 'Please select a plot type')
       )
       if(input$index_raw == "Forecast output" & input$raw_comm_type=='Figure' & input$raw_plot_type=='Time series'){
         validate(
           need(input$ts_line_type !="", 'Please select a time series plot type')
         )
       }
       
     }
     
   }
 
    validate(
      need(!is.null(cust_plot$plot), " Click 'Create Custom Plot'.")
    )
    
    cust_plot$plot
      
  })
  
  output$custom_plotly <- renderPlotly({
    validate(
      need(input$index_raw != "", "Please select 'Forecast index' or 'Forecast output'")
    )
    if(input$summ_comm_type=='Icon'){
      validate(
        need(!is.null(cust_plot$plot), " Click 'Create Custom Plot'.")
      ) 
    }
    cust_plot$plot
    
  })
  
   output$custom_plot_second_time <- renderPlot({
   cust_plot$plot
 })
 
   output$raw_or_index <- renderText({
     req(input$index_raw != "")
     paste0('Forecast index or Forecast output? ', input$index_raw)
     
    })
   
   output$raw_comm_out <- renderText({
     req(input$raw_comm_type)
     paste0('Communication type? ', input$raw_comm_type)
   })
   
   output$index_comm_out <- renderText({
     req(input$summ_comm_type)
     paste0('Communication type? ', input$summ_comm_type)
   })
   
   output$raw_plot_out <- renderText({
     req(input$raw_plot_type)
     paste0('Plot type? ', input$raw_plot_type)
   })
   
   
   output$index_plot_out <- renderText({
     req(input$summ_plot_type)
     paste0('Plot type? ', input$summ_plot_type)
   })
   
   output$raw_ts_out <- renderText({
     req(input$ts_line_type)
     paste0('Time series plot type? ', input$ts_line_type)
     
   })
   #textOutput('raw_ts_out')
   #textOutput('index_ts_out')
  
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
  
  
 custom_plot_file <-  reactiveValues(file = NULL)
  
  observeEvent(input$save_custom_plot, {
    validate(
      need(input$create_plot > 0, "Please click 'Create custom plot'")
    )
    
    # Progress bar
    progress <- shiny::Progress$new()
    on.exit(progress$close())
    progress$set(message = "Saving plot as image file for the report.", 
                 detail = "This may take a while. This window will disappear  
                     when it is downloaded.", value = 0.5)
    
    p <-    cust_plot$plot +
      theme_classic(base_size = 35) 
    if(input$index_raw=='Forecast index'){
      if(input$summ_comm_type=='Word'){
        p <- cust_plot$plot +
          theme(legend.position = 'none',
                panel.background = element_rect(fill = NA, color = 'black'),
                panel.border = element_rect(color = 'black', fill = NA),
                axis.text = element_blank(),
                axis.title = element_blank(),
                axis.ticks = element_blank(),
                plot.title = element_text(size = 40, hjust = 0.5),
                plot.caption = element_text(size = 30, hjust = 0))
      }
      if(input$summ_comm_type=='Number'){
        p <- cust_plot$plot + 
          theme(legend.position = 'none',
                panel.background = element_rect(fill = NA, color = 'black'),
                panel.border = element_rect(color = 'black', fill = NA),
                axis.text = element_blank(),
                axis.title = element_blank(),
                axis.ticks = element_blank(),
                plot.title = element_text(size = 30, hjust = 0.5),
                plot.caption = element_text(size = 40, hjust = 0))
      }
      if(input$summ_comm_type=='Icon'){
        htmlwidgets::saveWidget(cust_plot$plot, file = "www/custom_plot.html")
        webshot::webshot("www/custom_plot.html", "www/custom_plot.png")
        if(input$summ_comm_type=='Icon'){
          custom_plot_file$file <- "www/custom_plot.png"
        }else{
          custom_plot_file$file <- "www/custom_plot.png"
          img_file <- "www/custom_plot.png"
          ggsave(img_file, p, dpi = 300, width = 520, height = 380, units = "mm")
        }
        
        #p <- ggplotly(cust_plot$plot)
        #plotly_IMAGE(p,  "www/custom_plot.png")
      }
      if(input$summ_comm_type=='Figure'){
        if(input$summ_plot_type=='Pie'){
          p <- cust_plot$plot
          # make the legend and the title bigger? not sure how with theme_void() being necessary
        }
        if(input$summ_plot_type=='Time series'){
          p <- cust_plot$plot + 
            theme_classic(base_size = 40) +
            theme(panel.border = element_rect(fill = NA, colour = "black"), 
                  axis.text.x = element_text(size = 40),
                  legend.position = 'none',
                  plot.title = element_text(size = 40, hjust = 0.5),
                  plot.caption = element_text(size = 30, hjust = 0))
        } 
        if(input$summ_plot_type=='Bar graph'){
          
          p <- cust_plot$plot +
            theme(legend.position = 'none',
                  panel.background = element_rect(fill = NA, color = 'black'),
                  panel.border = element_rect(color = 'black', fill = NA),
                  plot.title = element_text(size = 40, hjust = 0.5),
                  axis.text = element_text(size = 30),
                  axis.title = element_text(size = 30),
                  plot.caption = element_text(size = 30, hjust = 0))
        }
      }
    }
    if(input$index_raw=='Forecast output'){
      if(input$raw_comm_type=='Number'){
        p <-  cust_plot$plot +
          theme(legend.position = 'none',
                panel.background = element_rect(fill = NA, color = 'black'),
                panel.border = element_rect(color = 'black', fill = NA),
                axis.text = element_blank(),
                axis.title = element_blank(),
                axis.ticks = element_blank(),
                plot.title = element_text(size = 40, hjust = 0.5),
                plot.caption = element_text(size = 30, hjust = 0))
      }
      if(input$raw_comm_type=='Figure'){
        if(input$raw_plot_type=='Bar graph'){
          p <-  cust_plot$plot +
            theme(
              panel.background = element_rect(fill = NA, color = 'black'),
              panel.border = element_rect(color = 'black', fill = NA),
              plot.title = element_text(size = 40, hjust = 0.5),
              axis.text = element_text(size = 30),
              axis.title = element_text(size = 30),
              plot.caption = element_text(size = 30, hjust = 0))
        }
        if(input$raw_plot_type=='Time series'){
          if(input$ts_line_type=='Line'){
            p <- cust_plot$plot +
              theme_classic(base_size = 40) +
              theme(panel.border = element_rect(fill = NA, colour = "black"), 
                    axis.text.x = element_text(size = 40),
                    legend.position = 'none',
                    plot.caption = element_text(size = 30, hjust = 0))
            
          }
          if(input$ts_line_type=='Confidence Interval'){
            p <- cust_plot$plot +
              theme_classic(base_size = 40) +
              theme(panel.border = element_rect(fill = NA, colour = "black"), 
                    axis.text.x = element_text(size = 40),
                    legend.position = 'none',
                    plot.title = element_text(size = 40, hjust = 0.5),
                    plot.caption = element_text(size = 30, hjust = 0))
            
          }
          if(input$ts_line_type=='Boxplot'){
            p <- cust_plot$plot +
              theme_classic(base_size = 40) +
              theme(panel.border = element_rect(fill = NA, colour = "black"), 
                    axis.text.x = element_text(size = 40),
                    legend.position = 'none',
                    plot.title = element_text(size = 40, hjust = 0.5),
                    plot.caption = element_text(size = 30, hjust = 0))
          }
        }
      }
    }
    
    
   
    progress$set(value = 1)
    
    

  })
  
  ID_input <- reactive({
    data.frame(name = input$name,
               studentID = input$studentID,
               q1 = input$q1,
               q2 = input$q2,
               activityb_obj5_q3 = input$activityb_obj5_q3
               )
  })

  
  #observeEvent(input$submit, {
  #  sheet_file <- gs4_get('https://docs.google.com/spreadsheets/d/1eoLJI_pr281ujcTiZXn2iqPc_Tp0d_LbmFiZXdlPwbA/edit#gid=0')
  #  sheet_append(sheet_file, data = ID_input())
  #})
  

  # Next button
  observe({
    curr_tab1 <- input$maintab
    idx <- which(tab_names$tab_id == curr_tab1)
    new_nam <- tab_names$name[idx + 1]
    if (curr_tab1 == "mtab4") {
      curr_obj <- input$tabseries1
      idx2 <- which(tab_names$tab_id == curr_obj)
      new_nam <- tab_names$name[idx2 + 1]
    }
    if (curr_tab1 == "mtab5") {
      curr_obj <- input$tabseries2
      idx2 <- which(tab_names$tab_id == curr_obj)
      new_nam <- tab_names$name[idx2 + 1]
    } 
    if (curr_tab1 == "mtab6") {
      curr_obj <- input$tabseries3
      idx2 <- which(tab_names$tab_id == curr_obj)
      new_nam <- tab_names$name[idx2 + 1]
      if(curr_obj == "tabc3") {
      new_nam <- "Next"
      }
    }
    updateActionButton(session, inputId = "nextBtn1", label = paste(new_nam, ">"))
  })
  
  # Previous button
  observe({
    curr_tab1 <- input$maintab
    idx <- which(tab_names$tab_id == curr_tab1)
    new_nam <- tab_names$name[idx - 1]
    if(curr_tab1 == "mtab1") {
      new_nam <- "Previous"
    }
    if (curr_tab1 == "mtab4") {
      curr_obj <- input$tabseries1
      idx2 <- which(tab_names$tab_id == curr_obj)
      if(curr_obj == "taba1") {
        new_nam <- tab_names$name[idx2 - 2]
      } else {
        new_nam <- tab_names$name[idx2 - 1]
      }

    }
    if (curr_tab1 == "mtab5") {
      curr_obj <- input$tabseries2
      idx2 <- which(tab_names$tab_id == curr_obj)
      if(curr_obj == "tabb1") {
        new_nam <- tab_names$name[idx2 - 2]
      } else {
        new_nam <- tab_names$name[idx2 - 1]
      }
    }
    if (curr_tab1 == "mtab6") {
      curr_obj <- input$tabseries3
      idx2 <- which(tab_names$tab_id == curr_obj)
      if(curr_obj == "tabc1") {
        new_nam <- tab_names$name[idx2 - 2]
      } else {
        new_nam <- tab_names$name[idx2 - 1]
      }

    }
    updateActionButton(session, inputId = "prevBtn1", label = paste("<", new_nam))
  })
  
  # Advancing Tabs
  observeEvent(input$nextBtn1, {

    curr_tab1 <- input$maintab
    idx <- which(tab_names$tab_id == curr_tab1)
    if (curr_tab1 == "mtab4" & rv1a$nxt < 3) {
      curr_obj <- input$tabseries1

      updateTabsetPanel(session, "tabseries1",
                        selected = paste0("taba", rv1a$nxt))

    } else if (curr_tab1 == "mtab5" & rv2a$nxt < 6) {
      curr_obj <- input$tabseries2
      updateTabsetPanel(session, "tabseries2",
                        selected = paste0("tabb", rv2a$nxt))
    }else if (curr_tab1 == "mtab6" & rv3a$nxt < 4) {
      curr_obj <- input$tabseries2
      updateTabsetPanel(session, "tabseries3",
                        selected = paste0("tabc", rv3a$nxt))
    } else {
      updateTabsetPanel(session, "tabseries1",
                        selected = "taba1")
      updateTabsetPanel(session, "tabseries2",
                        selected = "tabb1")
      updateTabsetPanel(session, "tabseries3",
                        selected = "tabc1")
      updateTabsetPanel(session, "maintab",
                        selected = paste0("mtab", rv1$nxt))
    }
    shinyjs::runjs("window.scrollTo(0, 0)") # scroll to top of page
})
  
    # Previous Tabs
  observeEvent(input$prevBtn1, {

    curr_tab1 <- input$maintab
    idx <- which(tab_names$tab_id == curr_tab1)
    if (curr_tab1 == "mtab4" & rv1a$prev > 0) {
      curr_obj <- input$tabseries1

      updateTabsetPanel(session, "tabseries1",
                        selected = paste0("taba", rv1a$prev))

    } else if (curr_tab1 == "mtab5" & rv2a$prev > 0) {
      curr_obj <- input$tabseries2
      updateTabsetPanel(session, "tabseries2",
                        selected = paste0("tabb", rv2a$prev))
    }else if (curr_tab1 == "mtab6" & rv3a$prev > 0) {
      curr_obj <- input$tabseries2
      updateTabsetPanel(session, "tabseries3",
                        selected = paste0("tabc", rv3a$prev))
    } else {
      updateTabsetPanel(session, "tabseries1",
                        selected = "taba2")
      updateTabsetPanel(session, "tabseries2",
                        selected = "tabb5")
      updateTabsetPanel(session, "tabseries3",
                        selected = "tabc3")
      updateTabsetPanel(session, "maintab",
                        selected = paste0("mtab", rv1$prev))
    }
    shinyjs::runjs("window.scrollTo(0, 0)") # scroll to top of page
})

  rv1 <- reactiveValues(prev = 0, nxt = 2)
  observeEvent(input$maintab, {
    curr_tab1 <- input$maintab
    rv1$prev <- readr::parse_number(curr_tab1) - 1
    rv1$nxt <- readr::parse_number(curr_tab1) + 1
  })

  #* Tab 1a ----
  rv1a <- reactiveValues(prev = 0, nxt = 2)
  observeEvent(input$tabseries1, {
    curr_tab1 <- input$tabseries1
    rv1a$prev <- readr::parse_number(curr_tab1) - 1
    rv1a$nxt <- readr::parse_number(curr_tab1) + 1
  })

  #* Tab 2a ----
  rv2a <- reactiveValues(prev = 0, nxt = 2)
  observeEvent(input$tabseries2, {
    curr_tab1 <- input$tabseries2
    rv2a$prev <- readr::parse_number(curr_tab1) - 1
    rv2a$nxt <- readr::parse_number(curr_tab1) + 1
  })

  #* Tab 3a ----
  rv3a <- reactiveValues(prev = 0, nxt = 2)
  observeEvent(input$tabseries3, {
    curr_tab1 <- input$tabseries3
    rv3a$prev <- readr::parse_number(curr_tab1) - 1
    rv3a$nxt <- readr::parse_number(curr_tab1) + 1
  })
  
  #Activate/Deactivate buttons
  observe({
    if( input$maintab == "mtab1" ) {
      shinyjs::disable("prevBtn1")
    } else {
      shinyjs::enable("prevBtn1")
    }
    if( input$maintab == 'mtab6' & input$tabseries3 == "tabc3") {
      shinyjs::disable("nextBtn1")
    } else {
      shinyjs::enable("nextBtn1")
    }
  })

  
  # Return to Introduction tab
  observeEvent(input$return_intro, {
    updateTabsetPanel(session, "maintab",
                      selected = "mtab3")
    shinyjs::runjs("window.scrollTo(0, 600)") # scroll to top of page
  })
  
  
  # Save answers in .eddie file
  ans_list <- reactiveValues()
  observe({
    ans_list <<- list(
      name = input$name,
      id_number = input$id_number,
      a1 = input$q1,
      a2 = input$q2,
      a3 = input$q3,
      a4 = input$q4,
      a5 = input$q5,
      a6 = input$q6,
      a7 = input$q7,
      viz_p = input$partner_image,
      a8 = input$q8,
      a9 = input$q9,
      a10 = input$q10,
      a11 = input$q11,
      a12 = input$q12,
      a13 = input$q13,
      a14_pr = input$problem,
      a14_obj = input$objective,
      a14_alt = input$alternatives,
      a14_con = input$consequences,
      a14_tro = input$tradeoffs,
      aobj4a_day14_mean = input$day14_forecast_value,
      aobj4a_choose = input$day14_obj4a_choose,
      aobj4a_day14_decision = input$Decision_Day14,
      aobj4a_day10_mean = input$day10_forecast_value,
      aobj4a_day10_decision = input$Decision_Day10,
      aobj4a_day7_mean = input$day7_forecast_value,
      aobj4a_day7_decision = input$Decision_Day7,
      aobj4a_day2_mean = input$day2_forecast_value,
      aobj4a_day2_decision = input$Decision_Day2,
      aobj4b_choose = input$day14_obj4b_choose,
      aobj4b_day14_decision = input$Decision_Day14_UC,
      aobj4b_day10_decision = input$Decision_Day10_UC,
      aobj4b_day7_decision = input$Decision_Day7_UC,
      aobj4b_day2_decision = input$Decision_Day2_UC,
      # save decision plot
      a15 = input$q15,
      a16 = input$q16,
      a17 = input$q17,
      a18 = input$q18,
      a19 = input$q19,
      a20 = input$q20,
      a21 = input$q21,
      aobj6_stakeholder = input$stakeholder,
      aobj6_stakeholder_other = input$stakeholder_other,
      a22 = input$q22,
      aobj7_date_selected = input$forecast_viz_date,
      a23 = input$q23, 
      a24 = input$mean_ens,
      a25 = input$min_ens,
      a26 = input$max_ens,
      a_index_raw = input$index_raw,
      a_summ_comm_type = input$summ_comm_type,
      a_summ_plot_type = input$summ_plot_type,
      a_raw_comm_type = input$raw_comm_type,
      a_raw_plot_type = input$raw_plot_type,
      a_ts_line_type = input$ts_line_type,
      a_title = input$figure_title,
      a_caption = input$figure_caption,
      # save custom plot
      a27 = input$q27,
      a28 = input$q28,
      a29 = input$q29,
      a30 = input$q30,
      a31 = input$q31,
      a32 = input$q32,
      a33 = input$q33
    )
    # ans_list <- data.frame(matrix(unlist(ans_list), nrow=length(ans_list), byrow = TRUE))
    # print(ans_list)
  })
  
  output$download_answers <- downloadHandler(
    
    # This function returns a string which tells the client
    # browser what name to use when saving the file.
    filename = function() {
      paste0("module8_answers_", input$id_number, ".eddie") %>%
        gsub(" ", "_", .)
    },
    
    # This function should write data to a file given to it by
    # the argument 'file'.
    content = function(file) {
      # write.csv(ans_list, file)
      saveRDS(ans_list, file = file)
    }
  )
  
  # Checklist for user inputs
  output$check_list <- renderUI({
    chk_list()
  })
  
  chk_list <- reactive({
    out_chk <- c(
      if(input$name == "") {"Introduction: Name"},
      if(input$id_number == "") "Introduction: ID number",
      if(input$q1 == "")"Activity A, Objective 1: Q. 1" ,
      if(input$q2 == "")"Activity A, Objective 1: Q. 2" ,
      if(is.null(input$q3))"Activity A, Objective 1: Q. 3" ,
      if(is.null(input$q4))"Activity A, Objective 1: Q. 4" ,
      if(input$q5 == "")"Activity A, Objective 1: Q. 5" ,
      if(input$q6 == "")"Activity A, Objective 1: Q. 6" ,
      if(input$q7 == "")"Activity A, Objective 1: Q. 7" ,
      if(input$partner_image =="")"Activity A, Objective 2: Select your partner's image",
      if(input$q8 == "")"Activity A, Objective 2: Q. 8",
      if(input$q9 == "")"Activity A, Objective 2: Q. 9",
      if(is.null(input$q10))"Activity A, Objective 2: Q. 10",
      if(is.null(input$q11))"Activity A, Objective 2: Q. 11",
      if(input$q12 == "")"Activity A, Objective 2: Q. 12",
      if(input$q13 == "")"Activity A, Objective 2: Q. 13",
      if(length(input$problem) == 0) "Activity B: Objective 3: Q14, Problem",
      if(length(input$objective) == 0) "Activity B: Objective 3: Q14, Objectives",
      if(length(input$alternatives) == 0) "Activity B: Objective 3: Q14, Alternatives",
      if(length(input$consequences) == 0) "Activity B: Objective 3: Q14, Consequences",
      if(length(input$tradeoffs) == 0) "Activity B: Objective 3: Q14, Trade-offs",
      if(input$day14_forecast_value == "" | input$day14_obj4a_choose == "" | is.null(input$Decision_Day14_UC))"Activity B, Objective 4a: Decision Day 14",
      if(input$day10_forecast_value == "" | is.null(input$Decision_Day10_UC))"Activity B, Objective 4a: Decision Day 10",
      if(input$day7_forecast_value == "" | is.null(input$Decision_Day7_UC))"Activity B, Objective 4a: Decision Day 7",
      if(input$day2_forecast_value == "" | is.null(input$Decision_Day2_UC))"Activity B, Objective 4a: Decision Day 2",
      if(input$save_obj4a_objectives==0)"Activity B, Objective 4a: Save objectives plot",
      if(input$day14_obj4b_choose == "" | is.null(input$Decision_Day14))"Activity B, Objective 4b: Decision Day 14",
      if(is.null(input$Decision_Day10))"Activity B, Objective 4b: Decision Day 10 ",
      if(is.null(input$Decision_Day7 ))"Activity B, Objective 4b: Decision Day 7",
      if(is.null(input$Decision_Day2 ))"Activity B, Objective 4b: Decision Day 2",
      if(input$save_obj4b_objectives==0)"Activity B, Objective 4b: Save objectives plot",
      if(input$save_decision_plot==0)"Activity B, Objective 5: Save decision plot",
      if(input$q15 == "")"Activity B, Objective 5: Q. 15",
      if(input$q16 == "")"Activity B, Objective 5: Q. 16",
      if(input$q17 == "")"Activity B, Objective 5: Q. 17",
      if(input$q18 == "")"Activity B, Objective 5: Q. 18",
      if(input$q19 == "")"Activity B, Objective 5: Q. 19",
      if(is.null(input$q20)) "Activity B, Objective 5: Q. 20",
      if(is.null(input$q21))"Activity B, Objective 5: Q. 21",
      if(input$q22 == "") "Activity C, Objective 6: Q. 22",
      if(input$q23 == "") "Activity C, Objective 6: Q. 23", # this is a select input
      if(input$mean_ens == "") "Activity C, Objective 7: Q. 24",
      if(input$min_ens == "") "Activity C, Objective 7: Q. 25",
      if(input$max_ens == "") "Activity C, Objective 7: Q. 26",
      if(input$q27 == "") "Activity C, Objective 7: Q. 27",
      if(input$save_custom_plot==0)"Activity C, Objective 8: Save custom plot",
      if(input$q28 == "") "Activity C, Objective 8: Q. 28",
      if(input$q29 == "") "Activity C, Objective 8: Q. 29",
      if(input$q30 == "") "Activity C, Objective 8: Q. 30",
      if(input$q31 == "") "Activity C, Objective 8: Q. 31",
      if(input$q32 == "") "Activity C, Objective 8: Q. 32",
      if(input$q33 == "") "Activity C, Objective 8: Q. 33"
    #  
      
    )
    
    if(length(out_chk) == 0) {
      out_chk <- "Finished! All answers have been input into the app."
    }
    
    HTML(
      paste(
        out_chk,
        collapse = "<br/>"
      )
    )
    
    
  })
  
  observeEvent(input$upload_answers, {
    
    up_answers <<- readRDS(input$upload_answers$datapath)
    updateTextAreaInput(session, "name", value = up_answers$name)
    updateTextAreaInput(session, "id_number", value = up_answers$id_number)
    updateTextAreaInput(session, "q1", value = up_answers$a1)
    updateTextAreaInput(session, "q2", value = up_answers$a2)
    updateRadioButtons(session, "q3", selected = up_answers$a3)
    updateRadioButtons(session, "q4", selected = up_answers$a4)
    updateTextAreaInput(session, "q5", value = up_answers$a5)
    updateTextAreaInput(session, "q6", value = up_answers$a6)
    updateSelectInput(session, "q7", selected = up_answers$a7)
    updateSelectInput(session, "partner_image", selected = up_answers$viz_p)
    updateTextAreaInput(session, "q8", value = up_answers$a8)
    updateTextAreaInput(session, "q9", value = up_answers$a9)
    updateRadioButtons(session, "q10", selected = up_answers$a10)
    updateRadioButtons(session, "q11", selected = up_answers$a11)
    updateTextAreaInput(session, "q12", value = up_answers$a12)
    updateTextAreaInput(session, "q13", value = up_answers$a13)
    #updateRadioButtons(session, "problem", selected = up_answers$a14_pro)
    #updateRadioButtons(session, "objective", selected = up_answers$a14_obj)
    #updateRadioButtons(session, "alternatives", selected = up_answers$a14_alt)
    #updateRadioButtons(session, "consequences", selected = up_answers$a14_con)
    #updateRadioButtons(session, "tradeoffs", selected = up_answers$a14_tro)
    updateTextAreaInput(session, "day14_forecast_value", value = up_answers$aobj4a_day14_mean)                   
    #updateTextAreaInput(session, "day14_descibe_forecast", value = up_answers$aobj4a_describe)          
    updateRadioButtons(session, "Decision_Day14", selected = up_answers$aobj4a_day14_decision)        
    updateTextAreaInput(session, "day10_forecast_value", value = up_answers$aobj4a_day10_mean)      
    updateRadioButtons(session, "Decision_Day10", selected = up_answers$aobj4a_day10_decision)        
    updateTextAreaInput(session, "day7_forecast_value", value = up_answers$aobj4a_day7_mean)        
    updateRadioButtons(session, "Decision_Day7", selected = up_answers$aobj4a_day7_decision)         
    updateTextAreaInput(session, "day2_forecast_value", value = up_answers$aobj4a_day2_mean)       
    updateRadioButtons(session, "Decision_Day2", selected = up_answers$aobj4a_day2_decision)         
    updateSelectInput(session, "day14_obj4a_choose", selected = up_answers$aobj4a_choose)                
    updateSelectInput(session, "day14_obj4b_choose", selected = up_answers$aobj4b_choose)                
    updateRadioButtons(session, "Decision_Day14_UC", selected = up_answers$aobj4b_day14_decision)    
    updateRadioButtons(session, "Decision_Day10_UC", selected = up_answers$aobj4b_day10_decision)    
    updateRadioButtons(session, "Decision_Day7_UC",  selected = up_answers$aobj4b_day7_decision)      
    updateRadioButtons(session, "Decision_Day2_UC",  selected = up_answers$aobj4b_day2_decision)      
    updateTextAreaInput(session, "q15", value = up_answers$a15)        
    updateTextAreaInput(session, "q16", value = up_answers$a16)        
    updateTextAreaInput(session, "q17", value = up_answers$a17)        
    updateTextAreaInput(session, "q18", value = up_answers$a18)       
    updateRadioButtons(session, "q19", selected = up_answers$a19)       
    updateTextAreaInput(session, "q20", value = up_answers$a20)        
    updateRadioButtons(session, "q21", selected = up_answers$a21)       
    updateSelectInput(session, "stakeholder", selected = up_answers$aobj6_stakeholder)
    #updateTextAreaInput(session, "stakeholder_other", selected = up_answers$aobj6_stakeholder_other)
    updateSelectInput(session, "forecast_viz_date", selected = up_answers$aobj7_date_selected)        
    
    updateTextAreaInput(session, "q22", value = up_answers$a22)  
   # updateSelectInput(session, "q23", value = up_answers$a23) 
    
    updateTextAreaInput(session, "mean_ens", value = up_answers$a24)       
    updateTextAreaInput(session, "min_ens", value = up_answers$a25) 
    updateTextAreaInput(session, "max_ens", value = up_answers$a26)                    
    updateRadioButtons(session, "index_raw", selected = up_answers$a_index_raw)
    updateRadioButtons(session, "summ_plot_type", selected = up_answers$a_summ_comm_type)
    updateRadioButtons(session, "index_raw", selected = up_answers$a_summ_plot_type)
    updateRadioButtons(session, "raw_comm_type", selected = up_answers$a_raw_comm_type)
    updateRadioButtons(session, "raw_plot_type", selected = up_answers$a_raw_plot_type)
    updateRadioButtons(session, "ts_line_type", selected = up_answers$a_ts_line_type)
    updateTextAreaInput(session, "figure_title", value = up_answers$a_title)                    
    updateTextAreaInput(session, "figure_caption", value = up_answers$a_caption)                     
    updateTextAreaInput(session, "q27", value = up_answers$a27)                 
    updateTextAreaInput(session, "q28", value = up_answers$a28)                    
    updateTextAreaInput(session, "q29", value = up_answers$a29)                    
    updateTextAreaInput(session, "q30", value = up_answers$a30)                    
    updateTextInput(session, "q31", value = up_answers$a31)                    
    #updateSelectInput(session, "q32", value = up_answers$a32)                    
    updateTextAreaInput(session, "q33", value = up_answers$a33)                    
    
    
    
  }) 
  
  #** Render Report ----
  report <- reactiveValues(filepath = NULL) #This creates a short-term storage location for a filepath
  
  observeEvent(input$generate, {
    
    progress <- shiny::Progress$new()
    # Make sure it closes when we exit this reactive, even if there's an error
    on.exit(progress$close())
    progress$set(message = "Gathering data and building report.", 
                 detail = "This may take a while. This window will disappear  
                     when the report is ready.", value = 1)

    # Set up parameters to pass to Rmd document
    params <- list(name = input$name,
                   id_number = input$id_number,
                   a1 = input$q1,
                   a2 = input$q2,
                   a3 = input$q3,
                   a4 = input$q4,
                   a5 = input$q5,
                   a6 = input$q6,
                   a7 = input$q7,
                   viz_p = input$partner_image,
                   a8 = input$q8,
                   a9 = input$q9,
                   a10 = input$q10,
                   a11 = input$q11,
                   a12 = input$q12,
                   a13 = input$q13,
                   a14_pr = input$problem,
                   a14_obj = input$objective,
                   a14_alt = input$alternatives,
                   a14_con = input$consequences,
                   a14_tro = input$tradeoffs,
                   aobj4a_day14_mean = input$day14_forecast_value,
                   #aobj4a_describe = input$day14_descibe_forecast,
                   aobj4a_day14_decision = input$Decision_Day14,
                   aobj4a_day10_mean = input$day10_forecast_value,
                   aobj4a_day10_decision = input$Decision_Day10,
                   aobj4a_day7_mean = input$day7_forecast_value,
                   aobj4a_day7_decision = input$Decision_Day7,
                   aobj4a_day2_mean = input$day2_forecast_value,
                   aobj4a_day2_decision = input$Decision_Day2,
                   obj4a_plot = "www/obj4a_objectives.png",
                   aobj4a_choose = input$day14_obj4a_choose,
                   aobj4b_choose = input$day14_obj4b_choose,
                   aobj4b_day14_decision = input$Decision_Day14_UC,
                   aobj4b_day10_decision = input$Decision_Day10_UC,
                   aobj4b_day7_decision = input$Decision_Day7_UC,
                   aobj4b_day2_decision = input$Decision_Day2_UC,
                   obj4b_plot = "www/obj4b_objectives.png",
                   decision_plot = "www/decision_plot.png",
                   a15 = input$q15,
                   a16 = input$q16,
                   a17 = input$q17,
                   a18 = input$q18,
                   a19 = input$q19,
                   a20 = input$q20,
                   aobj6_stakeholder = input$stakeholder,
                   aobj6_stakeholder_other = input$stakeholder_other,
                   a21 = input$q21,
                   aobj7_date_selected = input$forecast_viz_date,
                   a22 = input$q22,
                   a23 = input$q23,
                   a24 = input$mean_ens,
                   a25 = input$min_ens,
                   a26 = input$max_ens,
                   a27 = input$q27,
                   a_index_raw = input$index_raw,
                   a_summ_comm_type = input$summ_comm_type,
                   a_summ_plot_type = input$summ_plot_type,
                   a_raw_comm_type = input$raw_comm_type,
                   a_raw_plot_type = input$raw_plot_type,
                   a_ts_line_type = input$ts_line_type,
                   a_title = input$figure_title,
                   a_caption = input$figure_caption,
                   custom_plot ="www/custom_plot.png",
                   a28 = input$q28,
                   a29 = input$q29,
                   a30 = input$q30,
                   a31 = input$q31,
                   a32 = input$q32,
                   a33 = input$q33
    )
    
   # print(params)
    tmp_file <- paste0(tempfile(), ".docx") #Creating the temp where the .pdf is going to be stored
    
    rmarkdown::render("report.Rmd", 
                      output_format = "all", 
                      output_file = tmp_file,
                      params = params, 
                      envir = new.env(parent = globalenv()))
    progress$set(value = 1)
    report$filepath <- tmp_file #Assigning in the temp file where the .pdf is located to the reactive file created above
    
  })
  
  # Hide download button until report is generated
  output$reportbuilt <- reactive({
    return(!is.null(report$filepath))
  })
  outputOptions(output, 'reportbuilt', suspendWhenHidden= FALSE)
  
  
  #** Download Report ----
  
  #Download report  
  output$download <- downloadHandler(
    
    # This function returns a string which tells the client
    # browser what name to use when saving the file.
    filename = function() {
      paste0("report_", input$id_number, ".docx") %>%
        gsub(" ", "_", .)
    },
    
    # This function should write data to a file given to it by
    # the argument 'file'.
    content = function(file) {
      
      file.copy(report$filepath, file)
      
    }
  )
   
  
}

shinyApp(ui = ui, server = server)

# end
