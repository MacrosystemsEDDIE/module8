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

# Options for Spinner
options(spinner.color="#0275D8", spinner.color.background="#ffffff", spinner.size=2)

# colors for plots
cols <- RColorBrewer::brewer.pal(8, "Dark2")
l.cols <- RColorBrewer::brewer.pal(8, "Set2")


# Load text input
module_text <- read.csv("data/module_text.csv", row.names = 1, header = FALSE)
EF_links <- read.csv("data/eco_forecast_examples.csv")
forecast_dates <- read.csv("data/forecast_dates.csv")

# Define plot types
plot_types <- c("binary", "likelihood", "confidence interval", "all ensembles")
plot_files <- as.vector(c("binary.png", "likelihood.png", "confidence_interval.png", "all_ensembles.png"))

#user interface
ui <- tagList(
  navbarPage(title = "Module 8",
             position = "fixed-top",
             
             # Tab1: Macrosystems Overview ----
             tabPanel(title = "Macrosystems Overview",
                      tags$style(type="text/css", "body {padding-top: 65px;}"),
                      img(src = "eddie_banner_2018.v5.jpg", height = 100, 
                          width = 1544, top = 5),
                      useShinyjs(),
                      column(4,
                             h3("Project EDDIE"),
                             p(module_text["EDDIE", ]),
                             h3("Macrosystems Ecology"),
                             p(module_text["Macro", ]),
                             h3("EDDIE Modules 5-8"),
                             tags$ul(
                               tags$li("Module 5: Introduction to Ecological Forecasting"),
                               tags$li("Module 6: Forecast Uncertainty"),
                               tags$li("Module 7: Confronting Forecasts with Data"),
                               tags$li( tags$b("Module 8: Using Ecological Forecasts to Guide Decision Making"))
                             ),
                      ),
                      column(8,h2("Ecological Forecasting as a Tool for Macrosystems Ecology"),
                             #HTML('<center><img src="TFC_v1.png"></center>'),  #ask TM diff btw HTML and img
                             img(src = "ecoforecast_v2.png",
                                 width = 800,
                                 height = 500)),
                      br(),
                      br())
             ,
             
             # Tab2: Module 8 Introduction ---- ## want to make this the default landing page, which shows up under 'Module 8: Using Ecological...'
             tabPanel(title = "Introduction",
                      tags$style(type="text/css", "body {padding-top: 65px;}"),
                      img(src = "eddie_banner_2018.v5.jpg", height = 100, 
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
                      img(src = "eddie_banner_2018.v5.jpg", height = 100, 
                          width = 1544, top = 5),
                      h2("Activity A: Explore an existing ecological forecast"),
                      h3("Objective 1: Choose an ecological forecast from the list below and visit their website. With a partner, answer the following questions"),
                      br(),
                      h3("Assignment Questions"),
                      tags$ul(
                        tags$li(module_text["activityA_Q1",]),
                        tags$li(module_text["activityA_Q2",]),
                        tags$li(module_text["activityA_Q3",]),
                        tags$li(module_text["activityA_Q4",]),
                        tags$li(module_text["activityA_Q5",]),
                        tags$li(module_text["activityA_Q6",]),
                        tags$li(module_text["activityA_Q7",]),
                        tags$li(module_text["activityA_Q8",]),
                        tags$li(module_text["activityA_Q9",]),
                        tags$li(module_text["activityA_Q10",]),
                        tags$li(module_text["activityA_Q11",]),
                        tags$li(module_text["activityA_Q12",])
                        
                      ),
                      h3("Objective 2: With another team, compare forecasting systems and your answers above. Discuss the following questions regarding the ecological forecasting system you explored"),
                      br(),
                      
                      tags$ul(
                        tags$li(a(href = EF_links$webpage[1], EF_links$Forecast[1]), br(), p(EF_links$About[1])),
                        tags$li(a(href = EF_links$webpage[2], EF_links$Forecast[2]), br(), p(EF_links$About[2])),
                        tags$li(a(href = EF_links$webpage[3], EF_links$Forecast[3]), br(), p(EF_links$About[3])),
                        tags$li(a(href = EF_links$webpage[4], EF_links$Forecast[4]), br(), p(EF_links$About[4]))
                      )
                      ),
             
             # Tab4: Activity B ----
             tabPanel(title = "Activity B: Decide",
                      tags$style(type="text/css", "body {padding-top: 65px;}"),
                      img(src = "eddie_banner_2018.v5.jpg", height = 100, 
                          width = 1544, top = 5),
                      h2("Activity B: Make decisions informed by a real water quality forecast"),
                      h3("Objective 3: Interact with real forecasts of water quality for the main drinking water source for a small city and make a decision about managing the reservoir"),
                      br(),
                      h4(module_text["activityB_scenario1",]),
                      h4(module_text["activityB_scenario2",]),
                      h4(module_text["activityB_scenario3",]),
                      column(3,
                             selectInput('forecast_day', 'Forecast Day', choices = forecast_dates$forecast_horizon),
                             submitButton("Show Forecast")# drop down to choose the day of the forecast
                             ),
                      column(1),
                      column(8,
                             plotlyOutput('forecast_plot')
                      )
                      ),

             
             # Tab5: Activity C ----
             tabPanel(title = "Activity C: Discuss",
                      h2("Activity C: Explore different ways of visualizing ecological forecasts"),
                      h3("Objective 4: Discuss different ways to represent uncertainty and how different visualizations can be suited for stakeholder needs"),
                      fluidRow(
                        column(4,
                               selectInput("plot_type", "Plot Types", plot_types, selected = plot_types[1]), #for the drop-down options
                               submitButton("Show Plot")),
                        column(6,
                               imageOutput("PlotID", width = "20%",height = "20%", inline = T) #for the image to be displayed
                               )
                        
                      ),
                      br())
    
  )
  
)

server <- function(input, output){

  #observeEvent(input$forecast_day, {
  #})
  
  
    output$forecast_plot <- renderPlotly({
    forecast_id <- which(forecast_dates$forecast_horizon == input$forecast_day)
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
    nc_close(nc)
    
    forecast <- data.frame('date' = full_time_day_local, 'temp_mean' = temp_mean_1.6, 'upper_CI' = temp_upper_1.6, 'lower_CI' = temp_lower_1.6)
    p <- ggplot(data = forecast, aes(x = date, y = temp_mean)) + 
      geom_line() +
      geom_line(aes(date, upper_CI), linetype = 'dashed') +
      geom_line(aes(date, lower_CI), linetype = 'dashed') +
      geom_vline(xintercept = as.Date(forecast_dates[forecast_id,2])) +
      geom_text(aes(as.Date(forecast_dates[forecast_id,2])-1, y = 27.5), label = 'past') +
      geom_text(aes(as.Date(forecast_dates[forecast_id,2])+1, y = 27.5), label = 'future') +
      ylab('Chlorophyll-a (µg/L)') + 
      xlab("Date") +
      theme(panel.grid.major = element_blank()) 
    
    return(ggplotly(p, dynamicTicks = TRUE))})
  
  output$PlotID <- renderImage({
    idx <- which(plot_types == input$plot_type)
    filename <-  normalizePath(file.path('./www', paste0(plot_files[idx])))
      
    list(src = filename,
         width = 400,
         height = 600,
         alt = "Alt text")
    
  }, deleteFile = FALSE) 
  plot_type <- reactive({input$plot_type})
  

}





shinyApp(ui = ui, server = server)
