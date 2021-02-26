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
                         "Treat the reservoir with an algicide")
consequence_answers <- c("Economic benefit is heavily decreased due to canceling the event",
                         "Decreased ecological health (e.g., death of aquatic organisms) due to algicide treatment",
                         "Compromised drinking water quality due to lack of treatment during an algal bloom")
tradeoffs_answers <- c("Small loss of money due to cost of algicide, but increased economic benefit to the city from the event",
               "Decrease in ecological health, but safe drinking water is ensured",
               "Swimmer safety is compromised, but economic benefit and ecological health remain high due to avoiding algicide treatment")


mock_data <- read.csv('data/wq_forecasts/microcystin_mock_data.csv')
mock_data$date_forecast_made <- as.Date(mock_data$date_forecast_made)
mock_data$date_of_forecast <- as.Date(mock_data$date_of_forecast)

# Define vectors
forecast_descriptions <- c("", 'There is no chance of water quality degradation on June 6',
  'There is a chance that the water quality will be dangerous to swimmers (>35 ug/L) on June 6',
  'It is more likely that the algal concentration will be below 25 ug/L than it is that it will be above 25 ug/L',
  'The likelihood of an algal bloom (>25 ug/L) on June 6 is low')
decision_options <- c('', 'low stakes', 'general assessor', 'decision theorist')
decision_objectives <- c('drinking water quality', 'ecological health', 'economic benefit', 'swimmer safety')
objective_colors <- c("#335AA6", "#84B082", "#E75A7C","#F6BD60")
mgmt_choices <- c('A) Continue with the swimming event as planned', 
                  'B) Cancel the event', 
                  'C) Treat the reservoir with an algicide')

# define the date of the swimming event (Activity B)
date_of_event <- as.Date('2021-06-06')

#user interface
ui <- tagList(
  tags$head(tags$link(rel = "shortcut icon", href = "macroeddi_ico_green.ico")), # Add icon for web bookmarks
  navbarPage(title = "Module 8",
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
                             h4(tags$li("Activity A - Explore an ecological forecast visualizations")),
                             tags$ul(style = "list-style-type: lower alpha;", 
                                     tags$li("Identify different ways to visualize forecast output"),
                                     tags$li("Recognize visualizations which do or not represent uncertainty"),
                                     tags$li("Pair forecast visualizations with a stakeholder decision")),
                             h4(tags$li("Activity B - Make decisions using an ecological forecast")),
                             tags$ul(style = "list-style-type: lower alpha;", 
                                     tags$li("Match PrOACT decision-making components with an actual decision scenario"),
                                     tags$li("Make decisions using a forecast and balance multiple decision trade-offs"),
                                     tags$li("Discuss the implications of forecast visualizations on decision-making")),
                             h4(tags$li("Activity C - Create a customized visualization for a specific stakeholder")),
                             tags$ul(style = "list-style-type: lower alpha;", 
                                     tags$li("Connect decision-needs to a specific stakeholder"),
                                     tags$li("Create a customized visualization for a specific stakeholder based on their decision needs"),
                                     tags$li("Defend visualization choices with a specific stakeholder's decision needs")),
                        
                          ),
                          h2("For more information about how to navigate the module activites, please proceed to the 'Module Workflow' tab.")),
                      column(6,
                             h2('Ecological Forecasting Cycle', align = 'center'),
                             img(src = "mod8_viz_v2_resize.png", tags$style("border: solid 2px black;"))
                        
                      )),
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
                        h2('Presentation Slides', align = 'center'),
                           wellPanel(slickROutput('Mod8_slides', width = '50%', height = '50%'))
                      ),
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
             # Tab2: Module Navigation ----
             tabPanel(title = 'Module Workflow',
                      tags$style(type="text/css", "body {padding-top: 65px;}"),
                      value = 'mtab2',
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
                      column(6,)),
                      fluidRow(
                        column(6,
                          h2("Save your progress"),
                                  wellPanel('Coming soon!'),
                             h2('Resume your progress'),
                             wellPanel('Coming soon!')
                        ),
                        column(6,
                               h2('Generate Report',),
                               wellPanel('Coming soon!'),
                               h2('Questions still to be completed:'),
                               wellPanel('Coming soon!')
                               )),
                      fluidRow(
                        column(6,
                               wellPanel(style = paste0("background: ", ques_bg),
                                         h2('Before you start...'),
                                         p('Input your name and Student ID. This information will be added to your final report.'),
                                         textInput(inputId = 'name', placeholder = "", label = 'Name', width = '40%'),
                                         textInput(inputId = 'studentID', placeholder = "", label = 'ID Number:', width = '40%'),
                                         actionButton('submit', 'Submit'))
                               
                               ),
                        column(6,
                               )
                      )),
             
              # Tab3: Activity A ----
             tabPanel(title = "Activity A: Explore",
                      value = 'mtab3',
                      tags$style(type="text/css", "body {padding-top: 65px;}"),
                      img(src = "project-eddie-banner-2020_green.png", height = 100, 
                          width = 1544, top = 5),
                      h2("Activity A: Explore ecological forecast visualizations and decision-use"),
                      h4("Many of us use various types of forecasts in our daily life to make decisions (e.g., weather forecasts). However, we often take for granted the way in
                         which the forecast is presented to us. In this activity, you will examine several ecological forecasts and analyze the visualizations they provide
                         as decision-support tools for their users."),
                     br(),
                      tabsetPanel(id = 'tabseries1',
                     
                       tabPanel('Objective 1',
                                value = 'taba1',
                                h4(tags$b("Objective 1: Explore how uncertainty is visualized in an ecological forecast")),
                                h4("Choose an ecological forecast visualization from the list of visualizations below. 
                                Spend a few minutes looking through all of the visualizations and then select one by clicking on
                                   the image."),
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
                                                   style = "border: 1px double black;")
                                         ),
                                  column(6,
                                         )
                                  ),
                                wellPanel(style = paste0("background: ", ques_bg),
                                  fluidRow(tags$ul(column(6,
                                  textInput(inputId = "q1", label = paste0('Q1. ', module_text["activityA_Q1",]),
                                            placeholder = "", width = "60%"),
                                  textInput(inputId = "q2", label = paste0('Q2. ', module_text["activityA_Q2",]),
                                            placeholder = "", width = "60%"),
                                  radioButtons(inputId = "q3", label = paste0('Q3. ', module_text["activityA_Q3",]),
                                            choices = c("yes", "no"), width = "60%", selected =character(0)),
                                  radioButtons(inputId = "q4", label = paste0('Q4. ', module_text["activityA_Q4",]),
                                               choices = c('raw forecast output', 'metric'), selected = character(0))
                                  ),
                                  column(6,
                                         textInput(inputId = "q5", label = paste0('Q5. ', module_text["activityA_Q5",]),
                                                   placeholder = "", width = "60%"),
                                         textInput(inputId = "q6", label = paste0("Q6. ", module_text["activityA_Q6",]),
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
                                h4(tags$b("Objective 2: Compare forecast visualizations and answer the following questions.")),
                                br(),
                                h4("With another team, compare forecasting systems and visualizations. 
                                Discuss the following questions regarding the ecological forecasting systems you explored."),
                                h5("Upload your partner's forecast image to see the two displayed here. They can either email you their visualization file
                                   or you can navigate to their website, download the image, and upload here."),
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
                                h4('Comparing the two visualizations, answer the following questions'),
                                br(),
                                wellPanel(style = paste0("background: ", ques_bg),
                                  fluidRow(tags$ul(column(6,
                                                 textInput(inputId = "q_obj2_1", label = paste0("Q8. ", module_text["activityA_obj2_Q9",]),
                                                           placeholder = "", width = "60%"),
                                                 textInput(inputId = "q_obj2_2", label = paste0("Q9. ",module_text["activityA_obj2_Q10",]),
                                                           placeholder = "", width = "60%"),
                                                 radioButtons(inputId = "q_obj2_3", label = paste0("Q10. ",module_text["activityA_obj2_Q11",]),
                                                              choices = c('Mine', "My partner's", 'Both'), selected = character(0), width = "60%")
                                                 ),
                                          column(6,
                                                 radioButtons(inputId = "q_obj2_4", label = paste0("Q11. ",module_text["activityA_obj2_Q12",]),
                                                              choices = c('raw forecast output', 'metric'), selected = character(0),  width = "60%"),
                                                 textInput(inputId = "q_obj2_5", label = paste0("Q12. ",module_text["activityA_obj2_Q13",]),
                                                           placeholder = "", width = "60%"),
                                                 textInput(inputId = "q_obj2_6", label = paste0("Q13. ",module_text["activityA_obj2_Q14",]),
                                                          placeholder = "", width = "60%")
                                                 )
                                  
                                 )))
                         
                               
                       
                       )
                     ),
                    ),
                            
                    
             # Tab4: Activity B ----
             tabPanel(title = "Activity B: Decide",
                      value = 'mtab4',
                      tags$style(type="text/css", "body {padding-top: 65px;}"),
                      img(src = "project-eddie-banner-2020_green.png", height = 100, 
                          width = 1544, top = 5),
                      h2("Activity B: Make decisions informed by a real water quality forecast"),
                      h4("Ecological forecasts have vast potential for aiding decision-making for range of different stakeholders, 
                         yet forecast results may be challenging to understand because they inherently are associated with uncertainty 
                         in alternate future outcomes which have not yet occurred.This activity will allow you to make decisions and alter future scenarios 
                         to optimize future drinking water quality. Forecasts will update through time, allowing you to see how forecast uncertainty 
                         changes over time, and how management decisions can impact water quality."),
                      tabsetPanel(id = 'tabseries2',
                        tabPanel('Scenario',
                                 value = 'tabb1',
                                 br(),
                                 fluidRow(align = 'center',
                                          img(src = 'CCR.jfif',
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
                                                 h4(tags$b('Each day as you look at the forecast you must optimize your three objectives,
                                                 trying to keep all of them as high as possible.
                                                 Your decision alternatives and corresponding trade-offs are as follows:')),
                                                 ),
                                          column(2,
                                                 )
                                          ),
                                 fluidRow(column(4,
                                                 h4('A) Continue with the swimming event as planned.'),
                                                 h5('If you choose this option, drinking water quality, ecological health, and swimmer safety
                                                 may decrease if there is an algal bloom, but economic benefit to the city is optimized.'),
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
                                                 the event, but swimmer safety is not compromised'),
                                                 plotOutput('decision_b')
                                                 ),
                                          column(4,
                                                 h4('C) Treat the reservoir with an algicide'),
                                                 h5('If you choose this option, you will assure good drinking water quality,
                                                 but economic benefit will decrease slightly due to purchasing chemicals, and
                                                 ecological health and swimmer safety may be decreased due to exposure to the algicide.'),
                                                 plotOutput('decision_c')
                                                 )
                                          ),
                                 h3('Use these decision options to guide you in answering the questions in Objectives 3-5', align = 'center')
                                 
                                 ),
                        tabPanel('Objective 3',
                                 value = 'tabb2',
                                 h4(tags$b("Objective 3: Identify the components of the decision you need to make a drinking water manager (PrOACT):")),
                                 br(),
                                 p(module_text["proact_intro",]),
                                 slickROutput('PrOACT', width = '50%', height = '50%'),
                               h4('Use the definitions and examples in the slides to help you answer the following question. Drag and drop
                                  the answers from the answer bank to the appropriate category. There may be more than one answer for a 
                                  given category.'),
                               fluidRow(
                                 wellPanel(style = paste0("background: ", ques_bg),
                                        h4("Q14. Drag the definitions from the box on the right to the corresponding PrOACT boxes. There may be more than
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
                                 fluidRow(
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
                                 about whether to A) Continue with the swimming event as planned, B) Cancel the event, or C) Treat the reservoir with an algicide.
                                 submit your answers below. Remember that the forecast includes 25 different ensemble members, 
                                 which are different forecast estimates, and what you are seeing here is the mean of those ensembles.'),
                                          br(),
                                          h4("As you make your decisions, remember that water becomes dangerous for drinking when the chlorophyll-a concentration goes above 25 ug/L
                                  and dangerous for swimming when the chlorophyll-a concentration goes above 35 ug/L. "),   #You can display these thresholds dynamically on the figures by changing the 'Display threshold line' value.
                                          h5("The black dotted line represents the day on which the forecast is made and the solid grey line represents the
                                   day of the swimming event, June 06"),
                                          br(),
                                          br(),
                                          br(),
                                          br(),
                                        
                                          ),
                                   column(1,
                                          ),
                                   column(4,
                                          h4('This is your Objective Monitor. Each day as you make a decision, this plot will show
                                             the relative trade-offs between your four objectives. You want to keep your all objectives 
                                             as close to 100% as possible.'),
                                                        h4('Objectives on June 6', align = 'center'),
                                                        plotOutput('tradeoff_plot_optim'),
                                          br(),
                                          br(),
                                          br(),
                                          br(),
                                      
                                         
                                 )
                                 ),
                 # Day 14 decision
                                fluidRow(style = "border: 4px double black;",
                                  column(3,
                                                h4(tags$b('Days Before the Event: 14')),
                                                wellPanel(style = paste0("background: ", ques_bg),
                                                          #numericInput('add_threshold_14', 'Display threshold line', value = 35),
                                                textInput('day14_forecast_value', 'What is the mean forecasted concentration for June 6 in the 14-day forecast?', placeholder = 'enter answer here', width = '100%'),
                                                textInput('day14_descibe_forecast', 'In your own words, describe the forecast over the next 14 days leading up to June 6', width = '100%'),
                                                radioButtons(inputId = "Decision_Day14", label = 'Decision 14 days before the event', selected = character(0),
                                                            choices = mgmt_choices,  
                                                                         width = "100%"))),
                                         column(6,
                                                br(),
                                                plotlyOutput('forecast_plot_14'),
                                              
                                                ),
                                  column(3,
                                         h4('Objectives on June 6', align = 'center'),
                                         plotOutput('tradeoff_plot_14'))
                                     
                                         )
                                  ),     
                                br(),
                                br(),
                 # Day 10 decision
                 fluidRow(style = "border: 4px double black;",
                 column(3,
                        h4(tags$b('Days Before the Event: 10')),
                        wellPanel(style = paste0("background: ", ques_bg),
                                  #numericInput('add_threshold_10', 'Display threshold line', value = 35),
                                  textInput('day10_forecast_value', 'What is the mean forecasted concentration for June 6 in the 14-day forecast?', placeholder = 'enter answer here', width = '100%'),
                                  radioButtons(inputId = "Decision_Day10", label = 'Decision 10 days before the event', selected = character(0),
                                               choices = mgmt_choices,  
                                               width = "100%"))),
                 column(6,
                        br(),
                        plotlyOutput('forecast_plot_10')),
                 column(3,
                        h4('Objectives on June 6', align = 'center'),
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
                                                  textInput('day7_forecast_value', 'What is the mean forecasted concentration for June 6 in the 7-day forecast?', placeholder = 'enter answer here'),
                                                  radioButtons(inputId = "Decision_Day7", label = 'Decision 7 days before the event',
                                                                             choices = mgmt_choices,  
                                                                             width = "100%", selected = character(0)))),
                                         column(6,
                                                plotlyOutput('forecast_plot_7')  
                                         ),
                                         column(3,
                                                h4('Objectives on June 6', align = 'center'),
                                                plotOutput('tradeoff_plot_7'))
                                        ),
                 br(),
                # Day 2 decision
                                fluidRow(style = "border: 4px double black;",
                                  column(3,
                                         h4(tags$b('Days Before the Event: 2')),
                                         wellPanel(style = paste0("background: ", ques_bg),
                                                   #numericInput('add_threshold_2', 'Change the threshold line', value = 35),
                                                   textInput('day2_forecast_value', 'What is the mean forecasted concentration for June 6 in the 2-day forecast?', placeholder = 'enter answer here'),
                                                   radioButtons(inputId = "Decision_Day2", label = 'Decision 2 days before the event',
                                                                      choices = mgmt_choices,  
                                                                      width = "100%", selected = character(0)))),
                                         column(6,
                                                conditionalPanel("input.Decision_Day7!==''",
                                                                 plotlyOutput('forecast_plot_2'))
                                         ),
                                  column(3,
                                         h4('Objectives on June 6', align = 'center'),
                                         plotOutput('tradeoff_plot_2'))
                                ),
                                        
                                h3("Once you've made your decisions, continue to Objective 4b.")
                                        
                                 ),
                        tabPanel('Objective 4b',
                                 value = 'tabb4',
                                 h4(tags$b('Objective 4b: Decide how to manage a drinking water reservoir using an ecological forecast')),
                                 h4("Now, you will again make decisions about managing the reservoir over time, but this time you
                                             will use a different forecast visualization ot make your decisions."),
                                 h4('Examine the 14-day water quality forecast as you approach the day of the swimming event, June 06. 
                                 The forecasts will update over time, allowing you to update your decision as the day gets closer. 
                                 On each of the designated days, make a decision about whether to cancel the swimming event or not and 
                                 submit your answers below.'),
                                 h5("Remember that water becomes dangerous for drinking when the chlorophyll-a concentration goes above 25 ug/L
                                  and dangerous for swimming when the chlorophyll-a concentration goes above 35 ug/L. "), #You can display these thresholds dynamically on the figures by changing the 'Display threshold line' value.

                                 h5("The black dotted line represents the day on which the forecast is made and the solid grey line represents the
                                   day of the swimming event, June 06"),
                                 fluidRow(style = "border: 4px double black;",
                                          column(3,
                                                 h4(tags$b('Days Before the Event: 14')),
                                                 wellPanel(style = paste0("background: ", ques_bg),
                                                           #numericInput('add_threshold_14_UC', 'Display threshold line', value = 35),
                                                           selectInput('day14_forecast_multiple_choice_UC', label = 'Choose the best description of the forecast on June 6 from the following options',
                                                                       choices = forecast_descriptions,
                                                                       selected = "", width = '100%'),
                                                           radioButtons(inputId = "Decision_Day14_UC", label = 'Decision 14 days before the event', selected = character(0),
                                                                        choices = mgmt_choices,  
                                                                        width = "100%"))),
                                          column(6,
                                                 br(),
                                                 plotlyOutput('forecast_plot_14_withUC')),
                                          column(3,
                                                 h4('Objectives on June 6', align = 'center'),
                                                 plotOutput('tradeoff_plot_14_withUC'))
                                          ),
                                 fluidRow(style = "border: 4px double black;",
                                          column(3,
                                                 h4(tags$b('Days Before the Event: 10')),
                                                 wellPanel(style = paste0("background: ", ques_bg),
                                                           #numericInput('add_threshold_10_UC', 'Display threshold line', value = 35),
                                                          # selectInput('day10_forecast_multiple_choice_UC', label = 'Choose the best description of the forecast on June 6 from the following options',
                                                          #             choices = forecast_descriptions,
                                                          #             selected = "", width = '100%'),
                                                           radioButtons(inputId = "Decision_Day10_UC", label = 'Decision 10 days before the event', selected = character(0),
                                                                        choices = mgmt_choices,  
                                                                        width = "100%"))),
                                          column(6,
                                                 br(),
                                                 plotlyOutput('forecast_plot_10_withUC')),
                                          column(3,
                                                 h4('Objectives on June 6', align = 'center'),
                                                 plotOutput('tradeoff_plot_10_withUC'))
                                          ),
                                 fluidRow(style = "border: 4px double black;",
                                          column(3,
                                                 h4(tags$b('Days Before the Event: 7')),
                                                 wellPanel(style = paste0("background: ", ques_bg),
                                                           #numericInput('add_threshold_7_UC', 'Display threshold line', value = 35),
                                                           #selectInput('day7_forecast_multiple_choice_UC', label = 'Choose the best description of the forecast on June 6 from the following options',
                                                          #             choices = forecast_descriptions,
                                                          #             selected = "", width = '100%'),
                                                           radioButtons(inputId = "Decision_Day7_UC", label = 'Decision 7 days before the event', selected = character(0),
                                                                        choices = mgmt_choices,  
                                                                        width = "100%"))),
                                          column(6,
                                                 br(),
                                                 plotlyOutput('forecast_plot_7_withUC')),
                                          column(3,
                                                 h4('Objectives on June 6', align = 'center'),
                                                 plotOutput('tradeoff_plot_7_withUC'))
                                          ),
                                 fluidRow(style = "border: 4px double black;",
                                          column(3,
                                                 h4(tags$b('Days Before the Event: 2')),
                                                 wellPanel(style = paste0("background: ", ques_bg),
                                                           #numericInput('add_threshold_2_UC', 'Display threshold line', value = 35),
                                                           #selectInput('day2_forecast_multiple_choice_UC', label = 'Choose the best description of the forecast on June 6 from the following options',
                                                            #           choices = forecast_descriptions,
                                                            #           selected = "", width = '100%'),
                                                           radioButtons(inputId = "Decision_Day2_UC", label = 'Decision 2 days before the event', selected = character(0),
                                                                        choices = mgmt_choices,  
                                                                        width = "100%"))),
                                          column(6,
                                                 br(),
                                                 plotlyOutput('forecast_plot_2_withUC')),
                                          column(3,
                                                 h4('Objectives on June 6', align = 'center'),
                                                 plotOutput('tradeoff_plot_2_withUC'))
                                          )                                 
                                 ),

                        tabPanel('Objective 5',
                                 value = 'tabb5',
                                 h4(tags$b('Objective 5: Assess the impact of the forecast visualization on your decision-making')),
                                 p(tags$b('NOTE: You can add/remove items from being displayed in the figures by clicking on them in the figure legend. Try it! 
                                          This will help you answer some of the questions below.')),
                                 br(),
                                 column(5,                                 
                                        plotlyOutput('WQ_decisions')),
                                 column(7,
                                        plotlyOutput('forecast_final')),
                                 p('Look at the observed water quality on the day of the swimming competition. Answer the following questions about your experience as a manager using the water quality forecast.'),
                                 wellPanel(style = paste0("background: ", ques_bg),
                                           fluidRow(
                                              column(6,
                                                     textInput(inputId = "activityb_obj5_q3", label = paste0("Q15. ", module_text["activityB_obj5_Q1",]),
                                                               placeholder = "Hover your mouse over the figure above to answer this question.", width = "80%"),     
                                                     # textInput(inputId = "activityb_obj5_q4", label = module_text["activityB_obj5_Q2",],
                                                     #                    placeholder = "", width = "80%"),
                                                     textInput(inputId = "activityb_obj5_q3", label = paste0("Q16. ", module_text["activityB_obj5_Q3",]),
                                                               placeholder = "Hover your mouse over the figure above to answer this question.", width = "80%"),     
                                                     # textInput(inputId = "activityb_obj5_q4", label = module_text["activityB_obj5_Q4",],
                                                     #          placeholder = "", width = "80%"),
                                                     textInput(inputId = "activityb_obj5_q5", label = paste0("Q17. ", module_text["activityB_obj5_Q5",]),
                                                               placeholder = "Hover your mouse over the figure above to answer this question.", width = "80%"),
                                                     
                                                     ),
                                              column(6,
                                                     radioButtons(inputId = "activityb_obj5_q6", label = paste0("Q18. ", module_text["activityB_obj5_Q6",]),
                                                               choices = decision_objectives, selected = character(0), width = "80%"),
                                                     #textInput(inputId = "activityb_obj5_q7", label = paste0("Q19. ", module_text["activityB_obj5_Q7",]),
                                                    #           placeholder = "", width = "80%"),
                                                     textInput(inputId = "activityb_obj5_q8", label = paste0("Q19. ", module_text["activityB_obj5_Q8",]),
                                                               placeholder = "", width = "80%"),
                                                     radioButtons(inputId = 'viz_preference', label = "Q20. Which visualization did you prefer?",
                                                                  choices = c('Without Uncertainty', 'With Uncertainty'), selected = character(0))
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
                      value = 'mtab5',
                      tags$style(type="text/css", "body {padding-top: 65px;}"),
                      img(src = "project-eddie-banner-2020_green.png", height = 100, 
                          width = 1544, top = 5),
                      h2("Activity C: Create a customized visualization of an ecological forecast for specific stakeholder"),
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
                                           h4('Using the same forecast as we used in Activity B to make decisions as a water quality manager, 
                                              we will now customize the forecast visualization. It is important to consider who will be using your forecast
                                              to make decisions, as this can impact they way in which you visualize uncertainty.'),
                                           br(),
                                           h4('Choose a stakeholder from the drop-down menu and answer the questions below'),
                                           wellPanel(style = paste0("background: ", ques_bg),
                                            fluidRow(
   
                                             column(8,
                                                    selectInput('stakeholder', 'Choose a stakeholder', 
                                                                choices = c("", 'swimmer', 'fisher', 'dog owner', 'parent', 'drinking water manager'),# 
                                                                            width = '40%'), #'water scientist', 
                                                    textInput(inputId = 'activityC_obj6_q1', label = paste0("Q21. ", module_text["activityC_obj6_Q1",]),
                                                              width = '60%'),
                                                    br(),
                                                    h5(tags$b('Q22. Identify the PrOACT components of the stakeholder decision you identified above')),
                                                    textInput(inputId = "Problem_3", label = 'Problem(s)',
                                                              placeholder = "Enter a problem statement here", width = "60%"),
                                                    textInput(inputId = "Objective_3", label = 'Objective(s)',
                                                              placeholder = "Enter objective(s) here", width = "60%"),
                                                    textInput(inputId = "Alternative_3", label = 'Alternative(s)',
                                                              placeholder = "Enter alternative(s) here", width = "60%"),
                                                    textInput(inputId = "Consequence_3", label = 'Consequence(s)',
                                                              placeholder = "Enter consequence(s) here", width = "60%"),
                                                    textInput(inputId = "TradeOff_3", label = 'Trade Off(s)',
                                                              placeholder = "Enter trade off(s) here", width = "60%")),
                                             column(4,
                                                    htmlOutput('stakeholder_name'),
                                                    br(),
                                                    textOutput('stakeholder_text'),
                                                    br(),
                                                    imageOutput('stakeholder_pic')
                                                    
                                             )))),
                                  tabPanel('Objective 7',
                                           value = 'tabc2',
                                           h4(tags$b('Objective 7: Explore the forecast output and create a customized forecast visualization for your stakeholder')),
                                           h4("Below is a data table of forecast output, the same forecast you used to make decisions in Activity B. 
                                           In this activity, you will explore multiple ways of communicating this same forecast in order to create a 
                                              customized forecast visualization for your stakeholder."),
                                           br(),
                                           h4(tags$b("First, you should get to know your data. Use the 'Calculate Statistics' button to calculate various statistics for
                                              one day of the forecast and input them into Q23-25.")),
                                          fluidRow(
                                           column(6, DT::dataTableOutput('fcast_table')),
                                           column(6, h3("Calculate statistics"),
                                                  selectInput('forecast_viz_date', label = 'Select a date', choices = seq.Date(as.Date('2021-05-24'), as.Date('2021-06-06'), by = 'day')),
                                                  selectInput("stat_calc", label = "Select calculation:", choices = c("Pick a summary statistic", 'mean', 'median', 'max', 'min', 'standard deviation')),
                                                  textOutput("out_stats"),
                                                  h3('Choose one day and answer the following questions'),
                                                  wellPanel( style = paste0("background: ", ques_bg),
                                                             textOutput('date_selected_calcs'),
                                                             br(),
                                                  textInput('mean_ens', label = 'Q23. What is the mean concentration of all the ensembles?',
                                                            placeholder = 'Enter answer here', width = "60%"),
                                                  #textInput('median_ens', label = 'What is the median concentration of all the ensembles?',
                                                  #          placeholder = 'Enter answer here', width = "60%"),
                                                  textInput('min_ens', label = 'Q24. What is the minimum concentration of all the ensembles?',
                                                            placeholder = 'Enter answer here', width = "60%"),
                                                  textInput('max_ens', label = 'Q25. What is the maximum concentration of all the ensembles?',
                                                            placeholder = 'Enter answer here', width = "60%")
                                                  #textInput('sd_ens', label = 'What is the standard deviation of all the ensembles?',
                                                #            placeholder = 'Enter answer here', width = "60%")
                                                  ),
                                                  
                                                  
                                                  )),
                                    
                                           br(),
                                          br(),
                                          br(),
                                          h4(tags$b("Now that you are familiar with your data, explore the following visualization options to make
                                             a customized visualization for your stakeholder. Remember to consider the decision needs of your stakeholder
                                                    as you choose from among the visualization options.")),
                                          br(),
                                          br(),
                                          #imageOutput('stakeholder_pic_2'), 
                                           fluidRow(column(5,
                                                          wellPanel(style = paste0("background:", obj_bg), htmlOutput('stakeholder_name_2')),
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
                                                                                     radioButtons('summ_plot_options', 'Select the plot type for a summarized metric', choices = c('pie', 'bar graph', 'time series'), selected = character(0))),
                                                                    conditionalPanel("input.metric_raw=='raw forecast output' && input.raw_comm_type=='figure'", radioButtons('raw_plot_options', 'Select the plot type for raw forecast output', choices = c('pie', 'time series', 'bar graph'), selected = character(0))),
                                                                    conditionalPanel("input.metric_raw=='raw forecast output' && input.raw_comm_type=='figure' && input.raw_plot_options=='time series'",
                                                                                     radioButtons('ts_line_type', 'Select how you want to visualize the forecast ensembles',
                                                                                                  choices = c('line', 'distribution', 'boxplot'), #
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
                                           value = 'tabc3',
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
                                                column(6,
                                                       textInput('activityC_obj8_Q1', label = paste0("Q26. ", module_text["activityC_obj8_Q1",]), placeholder = 'Enter answer here', width = '60%'),
                                                       textInput('activityC_obj8_Q2', label = paste0("Q27. ", module_text["activityC_obj8_Q2",]), placeholder = 'Enter answer here', width = '60%'),
                                                       textInput('activityC_obj8_Q3', label = paste0("Q28. ", module_text["activityC_obj8_Q3",]), 
                                                                 placeholder = 'If you chose a word or number communication type, skip this question.', width = '60%'),
                                                       textInput('activityC_obj8_Q4', label = paste0("Q29. ", module_text["activityC_obj8_Q4",]), placeholder = 'Enter answer here', width = '60%')
                                                ),
                                                column(6,
                                                       textInput('activityC_obj8_Q5', label = paste0("Q30. ", module_text["activityC_obj8_Q5",]), placeholder = 'Enter answer here', width = '60%'),
                                                       textInput('activityC_obj8_Q6', label = paste0("Q31. ", module_text["activityC_obj8_Q6",]), placeholder = 'Enter answer here', width = '60%'),
                                                       textInput('activityC_obj8_Q7', label = paste0("Q32. ", module_text["activityC_obj8_Q7",]), placeholder = 'Enter answer here', width = '60%'))
                                             
                                             
                                           ))
                                           
                                           )
                                           
              
              
        )
    
  )
 ),
 # Tab navigation buttons ----
 br(), hr(),
 introBox(
   h4("Use the buttons below to navigate through the tabs", align = "center"),
   fluidRow(
     column(6, align = "center", 
            # wellPanel(
            style = paste0("background: ", obj_bg),
            br(),
            actionButton("prevBtn1", "< Previous", 
                         style = paste0("color: ", nav_txt, "; background-color: ", nav_butt, "; border-color: #00664B; padding:15px; font-size:22px;")),
            bsTooltip("prevBtn1", title = "Navigate to previous tab", placement = "left", trigger = "hover"),
            br(), br()
            # )
            
     ),
     column(6, align = "center",
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
  shinyjs::onclick("EF_10",  image_selected_path$img <- 'Phenology Monitoring at the Morton Aboretum')
  
  
  output$forecast_image <- renderImage({
    req(!is.na(image_selected_path$img))
     print(image_selected_path$img)
     id_image <- which(EF_links$Forecast==image_selected_path$img)
     filename <-  file.path('www', EF_links$logo_file[id_image])
     list(src = filename, height = '75%')
  }, deleteFile = FALSE)
  
  file <- reactive({gsub("\\\\", "/", input$forecast_file$datapath)})
  
  output$forecast_image_second_time <- renderImage({
    req(!is.na(image_selected_path$img))
    print(image_selected_path$img)
    id_image <- which(EF_links$Forecast==image_selected_path$img)
    filename <-  file.path('www', EF_links$logo_file[id_image])
    list(src = filename, height = '75%') 
  }, deleteFile = FALSE)
  
  file_2 <- reactive({gsub("\\\\", "/", input$forecast_file_2$datapath)})
  
  output$forecast_image_2 <- renderImage({
    req(input$partner_image)
    id_image <- which(EF_links$Forecast==input$partner_image)
    filename <-  file.path('www', EF_links$logo_file[id_image])
    list(src = filename, height = '75%') 
  }, deleteFile = FALSE)
  
  
  output$optimized_obj <- renderPlot({
    guage <- read.csv('data/scenario_objectives.csv')
    guage$objective <- factor(guage$objective, levels = decision_objectives)
    ggplot(data = guage[guage$decision=='none',], aes(objective, quantity, fill = objective)) + 
      geom_bar(stat = 'identity') +
      labs(title = 'Optimized Objectives') +
      scale_fill_manual(name = 'legend', 
                        values = c('drinking water quality' = objective_colors[1], 
                                   'ecological health' = objective_colors[2], 
                                   'economic benefit' = objective_colors[3],
                                   'swimmer safety' = objective_colors[4])) + ##FFB86F
      theme(legend.position = 'none',
            panel.background = element_rect(fill = NA, color = 'black'),
            panel.border = element_rect(color = 'black', fill = NA),
            plot.title = element_text(size = 15, hjust = 0.5),
            plot.caption = element_text(size = 15, hjust = 0))
    
    
  })
  
  output$decision_a <- renderPlot({
    guage <- read.csv('data/scenario_objectives.csv')
    guage$objective <- factor(guage$objective, levels = decision_objectives)
    ggplot(data = guage[guage$decision=='a',], aes(objective, quantity, fill = objective)) + 
      geom_bar(stat = 'identity') +
      labs(title = 'Decision A') +
      scale_fill_manual(name = 'legend', 
                        values = c('drinking water quality' = objective_colors[1], 
                                   'ecological health' = objective_colors[2], 
                                   'economic benefit' = objective_colors[3],
                                   'swimmer safety' = objective_colors[4])) + ##FFB86F
      theme(legend.position = 'none',
            panel.background = element_rect(fill = NA, color = 'black'),
            panel.border = element_rect(color = 'black', fill = NA),
            plot.title = element_text(size = 15, hjust = 0.5),
            plot.caption = element_text(size = 15, hjust = 0),
            axis.text.x = element_text(angle = 25, size = 10, hjust = 1))
    
    
  })
  
  output$decision_b <- renderPlot({
    guage <- read.csv('data/scenario_objectives.csv')
    guage$objective <- factor(guage$objective, levels = decision_objectives)
    ggplot(data = guage[guage$decision=='b',], aes(objective, quantity, fill = objective)) + 
      geom_bar(stat = 'identity') +
      labs(title = 'Decision B') +
      scale_fill_manual(name = 'legend', 
                        values = c('drinking water quality' = objective_colors[1], 
                                   'ecological health' = objective_colors[2], 
                                   'economic benefit' = objective_colors[3],
                                   'swimmer safety' = objective_colors[4])) + ##FFB86F
      theme(legend.position = 'none',
            panel.background = element_rect(fill = NA, color = 'black'),
            panel.border = element_rect(color = 'black', fill = NA),
            plot.title = element_text(size = 15, hjust = 0.5),
            plot.caption = element_text(size = 15, hjust = 0),
            axis.text.x = element_text(angle = 25, size = 10, hjust = 1))
    
    
  })
  
  output$decision_c <- renderPlot({
    guage <- read.csv('data/scenario_objectives.csv')
    guage$objective <- factor(guage$objective, levels = decision_objectives)
    ggplot(data = guage[guage$decision=='c',], aes(objective, quantity, fill = objective)) + 
      geom_bar(stat = 'identity') +
      labs(title = 'Decision C') +
      scale_fill_manual(name = 'legend', 
                        values = c('drinking water quality' = objective_colors[1], 
                                   'ecological health' = objective_colors[2], 
                                   'economic benefit' = objective_colors[3],
                                   'swimmer safety' = objective_colors[4])) + ##FFB86F
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
                        values = c('drinking water quality' = objective_colors[1], 
                                   'ecological health' = objective_colors[2], 
                                   'economic benefit' = objective_colors[3],
                                   'swimmer safety' = objective_colors[4])) + ##FFB86F
      theme(legend.position = 'none',
            panel.background = element_rect(fill = NA, color = 'black'),
            panel.border = element_rect(color = 'black', fill = NA),
            plot.title = element_text(size = 15, hjust = 0.5),
            plot.caption = element_text(size = 15, hjust = 0),
            axis.text.x = element_text(angle = 25, size = 10, hjust = 1))
    
    
  })

  
  reactive_tradeoff_plot <- reactiveValues(plot14 = NULL, plot10 = NULL, plot7 = NULL, plot2 = NULL)
  
  observe({
    req(input$Decision_Day14)
    guage <- read.csv('data/scenario_objectives.csv')
    guage$objective <- factor(guage$objective, levels = decision_objectives)
    selected <- input$Decision_Day14
    if(selected==mgmt_choices[1]){
      decision <- 'a'
    }else if(selected==mgmt_choices[2]){
      decision <- 'b'
    }else{
      decision <- 'c'
    }
    reactive_tradeoff_plot$plot14 <- ggplot(data = guage[guage$decision==decision,], aes(objective, quantity, fill = objective)) + 
      geom_bar(stat = 'identity') +
      # labs(title = 'Decision C') +
      xlab('Objectives') +
      ylab('Percent Optimized') +
    scale_fill_manual(name = 'legend', 
                      values = c('drinking water quality' = objective_colors[1], 
                                 'ecological health' = objective_colors[2], 
                                 'economic benefit' = objective_colors[3],
                                 'swimmer safety' = objective_colors[4])) + ##FFB86F
      theme(legend.position = 'none',
            panel.background = element_rect(fill = NA, color = 'black'),
            panel.border = element_rect(color = 'black', fill = NA),
            plot.title = element_text(size = 15, hjust = 0.5),
            plot.caption = element_text(size = 15, hjust = 0),
            axis.text.x = element_text(angle = 45, size = 10, hjust = 1))
  })
  
  observe({
    req(input$Decision_Day10)
    guage <- read.csv('data/scenario_objectives.csv')
    guage$objective <- factor(guage$objective, levels = decision_objectives)
    selected <- input$Decision_Day10
    if(selected==mgmt_choices[1]){
      decision <- 'a'
    }else if(selected==mgmt_choices[2]){
      decision <- 'b'
    }else{
      decision <- 'c'
    }
    reactive_tradeoff_plot$plot10 <- ggplot(data = guage[guage$decision==decision,], aes(objective, quantity, fill = objective)) + 
      geom_bar(stat = 'identity') +
      # labs(title = 'Decision C') +
      xlab('Objectives') +
      ylab('Percent Optimized') +
    scale_fill_manual(name = 'legend', 
                      values = c('drinking water quality' = objective_colors[1], 
                                 'ecological health' = objective_colors[2], 
                                 'economic benefit' = objective_colors[3],
                                 'swimmer safety' = objective_colors[4])) + ##FFB86F
      theme(legend.position = 'none',
            panel.background = element_rect(fill = NA, color = 'black'),
            panel.border = element_rect(color = 'black', fill = NA),
            plot.title = element_text(size = 15, hjust = 0.5),
            plot.caption = element_text(size = 15, hjust = 0),
            axis.text.x = element_text(angle = 45, size = 10, hjust = 1))
  })
  
  observe({
    req(input$Decision_Day7)
    guage <- read.csv('data/scenario_objectives.csv')
    guage$objective <- factor(guage$objective, levels = decision_objectives)
    selected <- input$Decision_Day7
    if(selected==mgmt_choices[1]){
      decision <- 'a'
    }else if(selected==mgmt_choices[2]){
      decision <- 'b'
    }else{
      decision <- 'c'
    }
    reactive_tradeoff_plot$plot7 <- ggplot(data = guage[guage$decision==decision,], aes(objective, quantity, fill = objective)) + 
      geom_bar(stat = 'identity') +
      # labs(title = 'Decision C') +
      xlab('Objectives') +
      ylab('Percent Optimized') +
    scale_fill_manual(name = 'legend', 
                      values = c('drinking water quality' = objective_colors[1], 
                                 'ecological health' = objective_colors[2], 
                                 'economic benefit' = objective_colors[3],
                                 'swimmer safety' = objective_colors[4])) + ##FFB86F
      theme(legend.position = 'none',
            panel.background = element_rect(fill = NA, color = 'black'),
            panel.border = element_rect(color = 'black', fill = NA),
            plot.title = element_text(size = 15, hjust = 0.5),
            plot.caption = element_text(size = 15, hjust = 0),
            axis.text.x = element_text(angle = 45, size = 10, hjust = 1))
  })
  
  observe({
    req(input$Decision_Day2)
    guage <- read.csv('data/scenario_objectives.csv')
    guage$objective <- factor(guage$objective, levels = decision_objectives)
    selected <- input$Decision_Day2
    if(selected==mgmt_choices[1]){
      decision <- 'a'
    }else if(selected==mgmt_choices[2]){
      decision <- 'b'
    }else{
      decision <- 'c'
    }
    reactive_tradeoff_plot$plot2 <- ggplot(data = guage[guage$decision==decision,], aes(objective, quantity, fill = objective)) + 
      geom_bar(stat = 'identity') +
      # labs(title = 'Decision C') +
      xlab('Objectives') +
      ylab('Percent Optimized') +
    scale_fill_manual(name = 'legend', 
                      values = c('drinking water quality' = objective_colors[1], 
                                 'ecological health' = objective_colors[2], 
                                 'economic benefit' = objective_colors[3],
                                 'swimmer safety' = objective_colors[4])) + ##FFB86F
      theme(legend.position = 'none',
            panel.background = element_rect(fill = NA, color = 'black'),
            panel.border = element_rect(color = 'black', fill = NA),
            plot.title = element_text(size = 15, hjust = 0.5),
            plot.caption = element_text(size = 15, hjust = 0),
            axis.text.x = element_text(angle = 45, size = 10, hjust = 1))
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
  
 reactive_tradeoff_plot_UC <- reactiveValues(plot14 = NULL, plot10 = NULL, plot7 = NULL, plot2 = NULL)
 
 observe({
   req(input$Decision_Day14_UC)
   guage <- read.csv('data/scenario_objectives.csv')
   guage$objective <- factor(guage$objective, levels = decision_objectives)
   selected <- input$Decision_Day14_UC
   if(selected==mgmt_choices[1]){
     decision <- 'a'
   }else if(selected==mgmt_choices[2]){
     decision <- 'b'
   }else{
     decision <- 'c'
   }
   reactive_tradeoff_plot_UC$plot14 <- ggplot(data = guage[guage$decision==decision,], aes(objective, quantity, fill = objective)) + 
     geom_bar(stat = 'identity') +
     # labs(title = 'Decision C') +
     xlab('Objectives') +
     ylab('Percent Optimized') +
     scale_fill_manual(name = 'legend', 
                       values = c('drinking water quality' = objective_colors[1], 
                                  'ecological health' = objective_colors[2], 
                                  'economic benefit' = objective_colors[3],
                                  'swimmer safety' = objective_colors[4])) + ##FFB86F
     theme(legend.position = 'none',
           panel.background = element_rect(fill = NA, color = 'black'),
           panel.border = element_rect(color = 'black', fill = NA),
           plot.title = element_text(size = 15, hjust = 0.5),
           plot.caption = element_text(size = 15, hjust = 0),
           axis.text.x = element_text(angle = 45, size = 10, hjust = 1))
 })
 
 observe({
   req(input$Decision_Day10_UC)
   guage <- read.csv('data/scenario_objectives.csv')
   guage$objective <- factor(guage$objective, levels = decision_objectives)
   selected <- input$Decision_Day10_UC
   if(selected==mgmt_choices[1]){
     decision <- 'a'
   }else if(selected==mgmt_choices[2]){
     decision <- 'b'
   }else{
     decision <- 'c'
   }
   reactive_tradeoff_plot_UC$plot10 <- ggplot(data = guage[guage$decision==decision,], aes(objective, quantity, fill = objective)) + 
     geom_bar(stat = 'identity') +
     # labs(title = 'Decision C') +
     xlab('Objectives') +
     ylab('Percent Optimized') +
     scale_fill_manual(name = 'legend', 
                       values = c('drinking water quality' = objective_colors[1], 
                                  'ecological health' = objective_colors[2], 
                                  'economic benefit' = objective_colors[3],
                                  'swimmer safety' = objective_colors[4])) + ##FFB86F
     theme(legend.position = 'none',
           panel.background = element_rect(fill = NA, color = 'black'),
           panel.border = element_rect(color = 'black', fill = NA),
           plot.title = element_text(size = 15, hjust = 0.5),
           plot.caption = element_text(size = 15, hjust = 0),
           axis.text.x = element_text(angle = 45, size = 10, hjust = 1))
 })
 
 observe({
   req(input$Decision_Day7_UC)
   guage <- read.csv('data/scenario_objectives.csv')
   guage$objective <- factor(guage$objective, levels = decision_objectives)
   selected <- input$Decision_Day7_UC
   if(selected==mgmt_choices[1]){
     decision <- 'a'
   }else if(selected==mgmt_choices[2]){
     decision <- 'b'
   }else{
     decision <- 'c'
   }
   reactive_tradeoff_plot_UC$plot7 <- ggplot(data = guage[guage$decision==decision,], aes(objective, quantity, fill = objective)) + 
     geom_bar(stat = 'identity') +
     # labs(title = 'Decision C') +
     xlab('Objectives') +
     ylab('Percent Optimized') +
     scale_fill_manual(name = 'legend', 
                       values = c('drinking water quality' = objective_colors[1], 
                                  'ecological health' = objective_colors[2], 
                                  'economic benefit' = objective_colors[3],
                                  'swimmer safety' = objective_colors[4])) + ##FFB86F
     theme(legend.position = 'none',
           panel.background = element_rect(fill = NA, color = 'black'),
           panel.border = element_rect(color = 'black', fill = NA),
           plot.title = element_text(size = 15, hjust = 0.5),
           plot.caption = element_text(size = 15, hjust = 0),
           axis.text.x = element_text(angle = 45, size = 10, hjust = 1))
 })
 
 observe({
   req(input$Decision_Day2_UC)
   guage <- read.csv('data/scenario_objectives.csv')
   guage$objective <- factor(guage$objective, levels = decision_objectives)
   selected <- input$Decision_Day2_UC
   if(selected==mgmt_choices[1]){
     decision <- 'a'
   }else if(selected==mgmt_choices[2]){
     decision <- 'b'
   }else{
     decision <- 'c'
   }
   reactive_tradeoff_plot_UC$plot2 <- ggplot(data = guage[guage$decision==decision,], aes(objective, quantity, fill = objective)) + 
     geom_bar(stat = 'identity') +
     # labs(title = 'Decision C') +
     xlab('Objectives') +
     ylab('Percent Optimized') +
     scale_fill_manual(name = 'legend', 
                       values = c('drinking water quality' = objective_colors[1], 
                                  'ecological health' = objective_colors[2], 
                                  'economic benefit' = objective_colors[3],
                                  'swimmer safety' = objective_colors[4])) + ##FFB86F
     theme(legend.position = 'none',
           panel.background = element_rect(fill = NA, color = 'black'),
           panel.border = element_rect(color = 'black', fill = NA),
           plot.title = element_text(size = 15, hjust = 0.5),
           plot.caption = element_text(size = 15, hjust = 0),
           axis.text.x = element_text(angle = 45, size = 10, hjust = 1))
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
  
  output$PrOACT <- renderSlickR({
    imgs <- list.files("www", pattern = "PrOACT", full.names = TRUE)
    slickR(imgs)
  })  

  
#  #* Variables answer table ----
#output$ans_vars <- renderTable({
#  data.frame("Problem" = proact_answers[,"problem"],
#             "Objectives" = proact_answers[,"objective"],
#             "Alternatives" = proact_answers[,"alternatives"],
#             "Consequences" = proact_answers[,"consequences"],
#             "Trade-offs" = proact_answers[,"tradeoffs"])
#}) 
#
##* Toggle for dataframe answers
#observeEvent(input$ans_btn, {
#  # if(input$ans_btn %% 2 != 1 |){
#  #   hide(id = "ans_vars")
#  # }else{
#  show(id = "ans_vars")
#  # }
#  # toggle("ans_vars")
#})
#


observeEvent(input$ans_btn, {
  if(length(input$problem) == 0) {
    res <- "Drag answers into Problem box!"
    print(length(input$problem))
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
    print(input$tradeoffs)
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
  #if(!is.na(input$add_threshold_14)){
    p <- fc_plots$day14 #+  geom_hline(yintercept = input$add_threshold_14, col = 'red', size = 1.1)
  #}
  return(ggplotly(p))
})


 output$forecast_plot_14_withUC <- renderPlotly({
   fcast <- read.csv("data/wq_forecasts/forecast_day14.csv")
   fcast$date <- as.Date(fcast$date)
   p <- fc_plots$day14 + geom_ribbon(data = fcast, aes(date, ymin = min, ymax = max, fill = "95% Conf. Int."), alpha = 0.3) +
     scale_color_manual(name = "", values = c("Obs" = l.cols[2], 'Forecast Mean' = 'black', "95% Conf. Int." = l.cols[4])) +
     theme(legend.title = element_blank())
     
   
   #if(!is.na(input$add_threshold_14_UC)){
     p <- p #+  geom_hline(yintercept = input$add_threshold_14_UC, col = 'red', size = 1.1)
  # }
   p <- ggplotly(p)
   
   for (i in 1:length(p$x$data)){
     if (!is.null(p$x$data[[i]]$name)){
       p$x$data[[i]]$name =  gsub("\\(","",str_split(p$x$data[[i]]$name,",")[[1]][1])
     }
   }
   
   return(p)
   })
 
 observe({
   fcast <- read.csv("data/wq_forecasts/forecast_day10.csv")
   fcast$date <- as.Date(fcast$date)
   data <- read.csv("data/wq_forecasts/mock_chl_obs.csv")
   data$date <- as.Date(data$date)
   
   fc_plots$day10 <-    ggplot()+
     geom_line(data = fcast, aes(date, mean, color = "Forecast Mean")) +
     scale_y_continuous(breaks = seq(0, 100, 10))+
     xlim(min(data$date), max(fcast$date)) +
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
 
 output$forecast_plot_10 <- renderPlotly({
   req(input$Decision_Day14)
   p <- fc_plots$day10 
  # if(!is.na(input$add_threshold_10)){
     p <- fc_plots$day10 #+  geom_hline(yintercept = input$add_threshold_10, col = 'red', size = 1.1)
   #}
   return(ggplotly(p))
 })  
 
 #observeEvent(input$Decision_Day14, {
 #  disable("Decision_Day14", !is.na(input$Decision_Day14))
 #})
 
 
 output$forecast_plot_10_withUC <- renderPlotly({
   req(input$Decision_Day14_UC)
   fcast <- read.csv("data/wq_forecasts/forecast_day10.csv")
   fcast$date <- as.Date(fcast$date)
   p <- fc_plots$day10 + geom_ribbon(data = fcast, aes(date, ymin = min, ymax = max, fill = "95% Conf. Int."), alpha = 0.3) +
     scale_color_manual(name = "", values = c("Obs" = l.cols[2], 'Forecast Mean' = 'black', "95% Conf. Int." = l.cols[4]))+
     theme(legend.title = element_blank())
   
   
   #if(!is.na(input$add_threshold_10_UC)){
     p <- p #+  geom_hline(yintercept = input$add_threshold_10_UC, col = 'red', size = 1.1)
   #}
   
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
     xlim(min(data$date), max(fcast$date)) +
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
   req(input$Decision_Day10)
   p <- fc_plots$day7 
   #if(!is.na(input$add_threshold_7)){
     p <- fc_plots$day7 #+  geom_hline(yintercept = input$add_threshold_7, col = 'red', size = 1.1)
   #}
   return(ggplotly(p))
 })
 
 
 output$forecast_plot_7_withUC <- renderPlotly({
   req(input$Decision_Day10_UC)
   fcast <- read.csv("data/wq_forecasts/forecast_day7.csv")
   fcast$date <- as.Date(fcast$date)
   p <- fc_plots$day7 + geom_ribbon(data = fcast, aes(date, ymin = min, ymax = max, fill = "95% Conf. Int."), alpha = 0.3) +
     scale_color_manual(name = "", values = c("Obs" = l.cols[2], 'Forecast Mean' = 'black', "95% Conf. Int." = l.cols[4]))+
     theme(legend.title = element_blank())
   
   
   #if(!is.na(input$add_threshold_7_UC)){
     p <- p #+  geom_hline(yintercept = input$add_threshold_7_UC, col = 'red', size = 1.1)
  # }
   
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
     xlim(min(data$date), max(fcast$date)) +
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
   #if(!is.na(input$add_threshold_2)){
     p <- fc_plots$day2 #+  geom_hline(yintercept = input$add_threshold_2, col = 'red', size = 1.1)
   #}
   return(ggplotly(p))
 })
 
 
 output$forecast_plot_2_withUC <- renderPlotly({
   req(input$Decision_Day7_UC)
   fcast <- read.csv("data/wq_forecasts/forecast_day2.csv")
   fcast$date <- as.Date(fcast$date)
   p <- fc_plots$day2 + geom_ribbon(data = fcast, aes(date, ymin = min, ymax = max, fill = "95% Conf. Int."), alpha = 0.3) +
     scale_color_manual(name = "", values = c("Obs" = l.cols[2], 'Forecast Mean' = 'black', "95% Conf. Int." = l.cols[4]))+
     theme(legend.title = element_blank()) 
   
   #if(!is.na(input$add_threshold_2_UC)){
     p <- p #+  geom_hline(yintercept = input$add_threshold_2_UC, col = 'red', size = 1.1)
   #}
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
 
output$WQ_decisions <- renderPlotly({
  req(input$Decision_Day2_UC)
  
  decisions <- ggplot(data = data) +
    geom_point(aes(x = day, y = binary_noUC, color = "Without Uncertainty", position = 'jitter'), size = 4) +
    geom_point(aes(x = day, y = binary_withUC, color = "With Uncertainty", position = 'jitter'), size = 4) +
    scale_y_continuous(breaks = c(0,0.5, 1), labels = c('Cancel', 'Treat', 'Continue')) +
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
          legend.title = element_blank())
  
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
   validate(need(input$stakeholder!="", "Please select a stakeholder"))
   #req(input$stakeholder!="") 
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

output$stakeholder_name_2 <- renderUI({
   stakeholder_id <-  which(stakeholder_info$stakeholder_selected == input$stakeholder)
   HTML(paste0("<b>", stakeholder_info[stakeholder_id,6], "<b>"))
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

output$custom_plotly <- renderPlotly({
  cust_plot$plot

 
})
  

   cust_plot <- reactiveValues(plot = NULL)
   
   observe({
     if(input$create_plot){ # should be yes or no based on whether or not the button is clicked
       if(input$metric_raw=='metric'){
         req(input$summ_comm_type)
         if(input$summ_comm_type=='word'){
           fcast <- read.csv("data/wq_forecasts/forecast_day14.csv")
           fcast$date <- as.Date(fcast$date)
           
           # metric, word
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
             labs(title = input$figure_title, caption = input$figure_caption) +
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
         if(input$summ_comm_type=='number'){
           fcast <- read.csv("data/wq_forecasts/forecast_day14.csv")
           fcast$date <- as.Date(fcast$date)
           fcast$percent_over_35 <- NA
           
           for (i in 2:nrow(fcast)) {
             number <-   length(which(fcast[i,6:30] > 35))
             fcast$percent_over_35[i] <- number/25*100
           }
           
           p2 <-  ggplot(data = fcast, aes(x = date[1], y = obs_chl_ugl[1])) +
             geom_label(aes(label = paste0(fcast[15,ncol(fcast)], '% chance of \n Algal Bloom'), x = date[1] + 0.5), size = 20) +
             labs(title = input$figure_title, caption = input$figure_caption) +
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
         if(input$summ_comm_type=='icon'){
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
             title = list(text = "Likelihood of Algal Bloom"),
             type = "indicator",
             mode = "gauge+number+delta",
             gauge = list(
               axis =list(range = list(NULL, 100)),
               bar = list(color = 'black'),
               steps = list(
                 list(range = c(0, 30), color = "green"),
                 list(range = c(30, 60), color = "yellow"),
                 list(range = c(60, 100), color = "red"))))    
           cust_plot$plot <- dial
         }
         if(input$summ_comm_type=='figure'){
           req(input$summ_plot_options)
           if(input$summ_plot_options=='pie'){
             fcast <- read.csv("data/wq_forecasts/forecast_day14.csv")
             fcast$date <- as.Date(fcast$date)
             fcast <- fcast[15,]
             fcast <- fcast %>% select(date, ens_1:ens_25) %>% 
               gather(key = ensemble, value = forecast, ens_1:ens_25)
             
             # calculate percent that are 0-25 ugL, 25-35 ugL, and >35ugL
             percents <- data.frame(range = c('0-25 ug/L', '25-35 ug/L', '>35 ug/L'),
                                    percent = NA)
             percents[1,2] <-  mean(fcast$forecast <25)*100
             percents[2,2] <-  mean(fcast$forecast >25 & fcast$forecast<35)*100
             percents[3,2] <-  mean(fcast$forecast >35)*100
             percents$range <- as.factor(percents$range)
             p_pie <-  ggplot(percents, aes(x="", y=percent, fill=range)) +
               geom_bar(stat="identity", width=1, color="white") +
               scale_fill_manual(name = 'legend', values = c('0-25 ug/L' = 'forestgreen', '25-35 ug/L' = 'goldenrod2', '>35 ug/L' = 'red3')) +
               coord_polar("y", start=0) +
               labs(title = paste0("Percent Likelihood of Algal Concentrations \n", input$figure_title), caption = input$figure_caption) +
               theme_void() # remove background, grid, numeric labels
             
             cust_plot$plot <- p_pie
           }
           if(input$summ_plot_options=='time series'){
             fcast <- read.csv("data/wq_forecasts/forecast_day14.csv")
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
               labs(title = paste0("Time Series leading up to June 6 Forecast \n", input$figure_title), caption = input$figure_caption) +
               theme_classic(base_size = 24) +
               theme(panel.border = element_rect(fill = NA, colour = "black"), 
                     axis.text.x = element_text(size = 24),
                     legend.position = 'none',
                     plot.title = element_text(size = 30, hjust = 0.5),
                     plot.caption = element_text(size = 15, hjust = 0))
             cust_plot$plot <- p_metric_ts
           } # this one is messed up
           if(input$summ_plot_options=='bar graph'){
             fcast <- read.csv("data/wq_forecasts/forecast_day14.csv")
             fcast$date <- as.Date(fcast$date)
             fcast <- fcast[15,]
             fcast <- fcast %>% select(date, ens_1:ens_25) %>% 
               gather(key = ensemble, value = forecast, ens_1:ens_25)
             
             # calculate percent that are 0-25 ugL, 25-35 ugL, and >35ugL
             percents <- data.frame(range = c('0-25 ug/L', '25-35 ug/L', '>35 ug/L'),
                                    percent = NA)
             percents[1,2] <-  mean(fcast$forecast <25)*100
             percents[2,2] <-  mean(fcast$forecast >25 & fcast$forecast<35)*100
             percents[3,2] <-  mean(fcast$forecast >35)*100
             
             order <-  c('0-25 ug/L', '25-35 ug/L', '>35 ug/L')
            
             p_metric_bar <- ggplot(data = percents, aes(range, percent, fill = range)) +
               geom_bar(stat = 'identity') +
               scale_x_discrete(limits = order) +
               labs(title = input$figure_title, caption = input$figure_caption) +
               scale_fill_manual(name = 'legend', values = c('0-25 ug/L' = 'forestgreen', '25-35 ug/L' = 'goldenrod2', '>35 ug/L' = 'red3')) +
               ylab('% Likelihood of Algal Concentration') +
               xlab('Range of Algal Concentration') +
               theme(legend.position = 'none',
                     panel.background = element_rect(fill = NA, color = 'black'),
                     panel.border = element_rect(color = 'black', fill = NA),
                     plot.title = element_text(size = 30, hjust = 0.5),
                     plot.caption = element_text(size = 15, hjust = 0))
             
             cust_plot$plot <- p_metric_bar
           }
         }
       }
       if(input$metric_raw=='raw forecast output'){
         req(input$raw_comm_type)
         if(input$raw_comm_type=='number'){
           fcast <- read.csv("data/wq_forecasts/forecast_day14.csv")
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
                   plot.title = element_text(size = 30, hjust = 0.5),
                   plot.caption = element_text(size = 15, hjust = 0))
           cust_plot$plot <- p_raw_number
         }
         if(input$raw_comm_type=='figure'){
           req(input$raw_plot_options)
           if(input$raw_plot_options=='pie'){
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
             data$counts <- as.factor(data$counts)
             data$breaks <- as.factor(data$breaks)
             p_pie_raw <- ggplot(data, aes(x="", y=counts, fill=breaks)) +
               scale_fill_brewer(palette = 'Dark2', name = 'Range of Predicted Chl Concentration', 
                                 label = c('0-15', '15-20', '20-25', '25-30', '30-35', '35-40', '40-45', '45-50')) +
               geom_bar(stat="identity", width=1) +
               coord_polar("y", start=0) +
               labs(title = input$figure_title, caption = input$figure_caption) +
               theme_void() # remove background, grid, numeric labels
             cust_plot$plot <- p_pie_raw
           }
           if(input$raw_plot_options=='bar graph'){
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
               xlab('Predicted Algal Concentration (ug/L)') +
               labs(title = paste0("June 6 Forecast \n", input$figure_title), caption = input$figure_caption) +
               theme(
                 panel.background = element_rect(fill = NA, color = 'black'),
                 panel.border = element_rect(color = 'black', fill = NA),
                 plot.title = element_text(size = 30, hjust = 0.5),
                 plot.caption = element_text(size = 15, hjust = 0))
             cust_plot$plot <- p_bar_raw
           }
           if(input$raw_plot_options=='time series'){
             req(input$ts_line_type)
             fcast <- read.csv("data/wq_forecasts/forecast_day14.csv")
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
               labs(title = paste0("Time Series leading up to June 6 Forecast \n", input$figure_title), caption = input$figure_caption) +
               theme_classic(base_size = 24) +
               theme(panel.border = element_rect(fill = NA, colour = "black"), 
                     axis.text.x = element_text(size = 24),
                     legend.position = 'none',
                     plot.title = element_text(size = 30, hjust = 0.5),
                     plot.caption = element_text(size = 15, hjust = 0))
             
            p_raw_ts_boxplot <-   ggplot(data = fcast) +
              geom_boxplot(aes(x = as.factor(date), y = forecast)) +
              ylab("Chlorophyll-a (ug/L)") +
              xlab("Date") +
              labs(title = paste0("Time Series leading up to June 18 Forecast \n", input$figure_title), caption = input$figure_caption) +
              theme_classic(base_size = 24) +
              theme(panel.border = element_rect(fill = NA, colour = "black"), 
                    axis.text.x = element_text(size = 24),
                    legend.position = 'none',
                    plot.title = element_text(size = 30, hjust = 0.5),
                    plot.caption = element_text(size = 15, hjust = 0)) +
              scale_x_discrete(breaks = c('2021-05-24', '2021-05-29', '2021-06-02', '2021-06-06'),
                               labels = c('2021-05-24' = 'May 24', '2021-05-29' = 'May 29',  '2021-06-02' = 'Jun 02', '2021-06-06' = 'Jun 06'))
            
             if(input$ts_line_type=='line'){
               cust_plot$plot <- p_raw_ts_ens
               
             }
             if(input$ts_line_type=='distribution'){
               cust_plot$plot <- p_raw_ts_distribution
               
             }
             if(input$ts_line_type=='boxplot'){
               cust_plot$plot <- p_raw_ts_boxplot
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
  
  
  ID_input <- reactive({
    data.frame(name = input$name,
               studentID = input$studentID,
               q1 = input$q1,
               q2 = input$q2,
               activityb_obj5_q3 = input$activityb_obj5_q3
               )
  })

  
  observeEvent(input$submit, {
    sheet_file <- gs4_get('https://docs.google.com/spreadsheets/d/1eoLJI_pr281ujcTiZXn2iqPc_Tp0d_LbmFiZXdlPwbA/edit#gid=0')
    sheet_append(sheet_file, data = ID_input())
  })
  

  # Next button
  observe({
    curr_tab1 <- input$maintab
    idx <- which(tab_names$tab_id == curr_tab1)
    new_nam <- tab_names$name[idx + 1]
    if (curr_tab1 == "mtab3") {
      curr_obj <- input$tabseries1
      idx2 <- which(tab_names$tab_id == curr_obj)
      new_nam <- tab_names$name[idx2 + 1]
    }
    if (curr_tab1 == "mtab4") {
      curr_obj <- input$tabseries2
      idx2 <- which(tab_names$tab_id == curr_obj)
      new_nam <- tab_names$name[idx2 + 1]
    } 
    if (curr_tab1 == "mtab5") {
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
    if (curr_tab1 == "mtab3") {
      curr_obj <- input$tabseries1
      idx2 <- which(tab_names$tab_id == curr_obj)
      if(curr_obj == "taba1") {
        new_nam <- tab_names$name[idx2 - 2]
      } else {
        new_nam <- tab_names$name[idx2 - 1]
      }

    }
    if (curr_tab1 == "mtab4") {
      curr_obj <- input$tabseries2
      idx2 <- which(tab_names$tab_id == curr_obj)
      if(curr_obj == "tabb1") {
        new_nam <- tab_names$name[idx2 - 2]
      } else {
        new_nam <- tab_names$name[idx2 - 1]
      }
    }
    if (curr_tab1 == "mtab5") {
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
    if (curr_tab1 == "mtab3" & rv1a$nxt < 3) {
      curr_obj <- input$tabseries1

      updateTabsetPanel(session, "tabseries1",
                        selected = paste0("taba", rv1a$nxt))

    } else if (curr_tab1 == "mtab4" & rv2a$nxt < 6) {
      curr_obj <- input$tabseries2
      updateTabsetPanel(session, "tabseries2",
                        selected = paste0("tabb", rv2a$nxt))
    }else if (curr_tab1 == "mtab5" & rv3a$nxt < 4) {
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
    if (curr_tab1 == "mtab3" & rv1a$prev > 0) {
      curr_obj <- input$tabseries1

      updateTabsetPanel(session, "tabseries1",
                        selected = paste0("taba", rv1a$prev))

    } else if (curr_tab1 == "mtab4" & rv2a$prev > 0) {
      curr_obj <- input$tabseries2
      updateTabsetPanel(session, "tabseries2",
                        selected = paste0("tabb", rv2a$prev))
    }else if (curr_tab1 == "mtab5" & rv3a$prev > 0) {
      curr_obj <- input$tabseries2
      updateTabsetPanel(session, "tabseries3",
                        selected = paste0("tabc", rv2a$prev))
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
    if( input$maintab == 'mtab5' & input$tabseries3 == "tabc3") {
      shinyjs::disable("nextBtn1")
    } else {
      shinyjs::enable("nextBtn1")
    }
  })

  
  
}

shinyApp(ui = ui, server = server)

# end
